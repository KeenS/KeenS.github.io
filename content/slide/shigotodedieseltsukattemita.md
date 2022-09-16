---
categories: [Rust]
date: 2017-03-01T10:57:38+09:00
description: null
title: 仕事でdiesel使ってみた
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# 仕事でdiesel使ってみた
----------------------
[RustのLT会！ Rust入門者の集い #2 - connpass](https://rust.connpass.com/event/48826/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# [diesel.rs](diesel.rs)

![dieselのwebページ](/images/diesel/diesel-web.png)

===

# diesel
---------


* RustのORM
  * スキーマからコード自動生成
  * diesel_codegen
* クエリビルダ
* マイグレーション管理
* PostgreSQL, MySQL, SQLiteサポート

===

# 準備
-------

* [docker-composeでmysql & postgreSQL をサクッと起動 - Qiita](http://qiita.com/astrsk_hori/items/1e683a7a2f2b7189cb6e)

```
cargo install diesel_cli
echo DATABASE_URL=postgres://username:password@localhost/hoge > .env
diesel setup
diesel migration generate create_posts
```

===

# 準備
-------

``` toml
[dependencies]
chrono = "0.3.0"
diesel = {version = "0.11.4", features = ["chrono", "postgres", "serde_json"]}
dotenv = "0.8.0"
serde_json = "0.9.6"
serde_derive = "0.9.7"
serde = "0.9.7"

[dependencies.diesel_codegen]
features = ["postgres"]
version = "0.11.0"
```

===
# プロジェクト構成
-----------------

* モデルのコンパイルが遅いので分離したい
* Cargoの[ワークスペース](http://doc.crates.io/manifest.html#the-workspace-section)機能を使う

===
# プロジェクト構成
-----------------

```
$ ls
Cargo.lock
Cargo.toml
hoge_model <- diesel_derive使う
hoge_XXX   <- diesel使う
hoge_YYY   <- diesel使う
docker-compose.yml
migrations
...
```
===

# テーブル
---------

* up.sqlとdown.sqlを書く

``` sql
CREATE TABLE hoge_suites (
  id SERIAL PRIMARY KEY,
  hoge_entry_id INT NOT NULL,
  hostname VARCHAR NOT NULL,
  status INT NOT NULL DEFAULT 0,
  CONSTRAINT unique_hoge_entry_id_hostname
             UNIQUE (
                      hoge_entry_id,
                      hostname
                      -- and other matrix
                     ),
  CONSTRAINT fkey_hoge_entry_id
             FOREIGN KEY(hoge_entry_id)
             REFERENCES hoge_entries(id) MATCH SIMPLE
             ON UPDATE NO ACTION
             ON DELETE CASCADE
);

```

===
# モデル
--------

``` rust
// なんかいっぱいderive
// QueryableとInsertableは分ける
#[derive(Default, Queryable, Associations, Identifiable, Debug, Clone)]
// belongs_to関係のstruct
#[belongs_to(HogeEntry)]
// has_many関係のテーブル
#[has_many(hoge_results)]
#[table_name="hoge_suites"]
pub struct HogeSuite {
    pub id: i32,
    pub bench_entry_id: i32,
    // String
    pub hostname: String,
    pub status: HogeStatus,
}

```

===

# モデル
--------

```
// Insertableの方
#[derive(Default, Insertable, Debug, Clone)]
#[table_name="hoge_suites"]
pub struct NewHogeSuite<'a> {
    pub hoge_entry_id: i32,
    // Stringではなく&str
    pub hostname: &'a str,
}
```

===

# enumの扱い
-----------

* dieselはenumのcodegenをサポートしてない
* 気合

===

# enumの扱い
-----------

```rust
// Queryableとかはderive出来ない
#[derive(Debug, Clone)]
pub enum HogeStatus {
    Waiting,
    Running,
    Finished,
    Failed,
    Canceled,
}

// 手で実装
impl<BE, QB, BC> FromSql<Integer, BE> for HogeStatus
    where QB: QueryBuilder<BE>,
          BC: BindCollector<BE>,
          BE: Backend<RawValue = [u8], QueryBuilder = QB, BindCollector = BC>
{
    fn from_sql(bytes: Option<&<Pg as Backend>::RawValue>)
                -> Result<Self, Box<Error + Send + Sync>> {
        use self::HogeStatus::*;
        match <i32 as FromSql<Integer, Pg>>::from_sql(bytes)? {
            0 => Ok(Waiting),
            1 => Ok(Running),
            2 => Ok(Finished),
            3 => Ok(Failed),
            4 => Ok(Canceled),
            n => Err(format!("invalid status: {}", n).into()),
        }
    }
}

impl<BE, QB, BC> FromSqlRow<Integer, BE> for HogeStatus
    where QB: QueryBuilder<BE>,
          BC: BindCollector<BE>,
          BE: Backend<RawValue = [u8], QueryBuilder = QB, BindCollector = BC>
{
    fn build_from_row<T: Row<BE>>(row: &mut T) -> Result<Self, Box<Error + Send + Sync>> {
        use self::HogeStatus::*;
        match <i32 as FromSqlRow<Integer, BE>>::build_from_row(row)? {
            0 => Ok(Waiting),
            1 => Ok(Running),
            2 => Ok(Finished),
            3 => Ok(Failed),
            4 => Ok(Canceled),
            n => Err(format!("invalid status: {}", n).into()),
        }
    }
}
```

===
# クエリ
--------

* モデルプロジェクトとは別
* 各プロジェクトのdb.rsで関数を定義
* SQLよりRsutのイテレータを意識した書き方

===
# クエリ
--------

```rust
pub fn is_hoge_finished<'a, Cn>(conn: &Cn, entry_id: i32) -> Result<bool, Error>
    where Cn: Connection<Backend = Pg>
{
    use self::schema::hoge_suites::dsl::*;
    // SELECT ... じゃなくてsourceからfileterして最後にselect
    let ret = hoge_suites
        .filter(hoge_entry_id.eq(entry_id))
        .select(status)
        // ここまでクエリビルダ
        .load::<i32>(conn)?
        // ここからイテレータ
        .into_iter()
        .all(|i| [HogeStatus::Finished as i32, HogeStatus::Failed as i32].contains(&i));
    Ok(ret)
}

```

===
# 複雑なクエリ
-------------

* トランザクション
* join
* order by

===

# 複雑なクエリ
-------------

``` rust
pub fn pic_suite<Cn>(conn: &Cn,
                     query: &api::worker_poll::Request)
                     -> Result<Option<(HogeEntry, HogeSuite)>, Error>
    where Cn: Connection<Backend = Pg>
{
    use self::schema::hoge_entries::dsl::*;
    use self::schema::hoge_suites::dsl::*;
    use self::schema::hoge_suites::dsl::id;
    // transaction
    conn.transaction(|| {
        let suite = hoge_suites
             // join
            .inner_join(hoge_entries)
            .filter(status.eq(HogeStatus::Waiting as i32)
                .and(hostname.eq(&query.hostname)))
            // order by
            .order(created_at.asc())
            .select((id, hoge_entry_id, hostname, status))
            .first::<HogeSuite>(conn)
            // NotFoundErrorをOptionにできる
            .optional()?;
        // transactionの中で普通にrustの式が書ける
        if let Some(s) = suite {
            let entry = hoge_entries.find(s.hoge_entry_id)
                .first::<HogeEntry>(conn)?;
            let suite =
                diesel::update(hoge_suites.find(s.id)).set(status.eq(HogeStatus::Running as i32))
                    .get_result::<HogeSuite>(conn)?;
            Ok(Some((entry, suite)))

        } else {
            Ok(None)
        }
    })
}
```

===

# もっと複雑なクエリ
-------------------

* 2重join
* dieselは2重joinを扱えない…

===

# もっと複雑なクエリ
-------------------


``` rust
pub fn load_all_data<'a, Cn>(conn: &Cn)
                             -> Result<Vec<(HogeEntry, Vec<((HogeSuite, HogeResult), Vec<HogeProfile>)>)>>
    where Cn: Connection<Backend = Pg>
{
    use diesel::types::{Integer, Timestamp, VarChar, Double};
    use diesel::pg::types::sql_types::{Array, Jsonb};
    use diesel::expression::dsl::*;
    use diesel::select;

    let status = sql::<Integer>("s.status");
    let ret = select(sql::<(
        (Integer, VarChar, VarChar, Array<VarChar>,Timestamp, VarChar, Integer, Jsonb),
        (Integer, Integer, VarChar, Integer),
        (Integer, Integer, VarChar, Integer, Integer, VarChar),
        (Integer, Integer, VarChar, Integer, Double))>(
        "
e.id, e.repo, e.committish, e.files, e.created_at, e.command, e.format, e.format_data,
s.id, s.hoge_entry_id, s.hostname, s.status,
r.id, r.hoge_suite_id, r.hoge_name, r.score, r.status, r.result,
p.id, p.hoge_result_id, p.function_name, p.score, p.percent
FROM hoge_profiles p
INNER JOIN hoge_results r on r.id = p.hoge_result_id
INNER JOIN hoge_suites s ON s.id = r.hoge_suite_id
INNER JOIN hoge_entries e ON e.id = s.hoge_entry_id"))
    .filter(status.eq(HogeStatus::Finished as i32))
    .load::<(HogeEntry, HogeSuite, HogeResult, HogeProfile)>(conn)?;
    let mut hash = HashMap::new();
    for (e, s, r, p) in ret {
        let mut hash_entry1 = hash         .entry(e.id).or_insert((e, HashMap::new()));
        let mut hash_entry2 = hash_entry1.1.entry(r.id).or_insert(((s, r), Vec::new()));
        hash_entry2.1.push(p);
    }
    let ret = hash.into_iter().map(|(_, v)| (v.0,  v.1.into_iter().map(|(_, p)| p).collect())).collect();
    Ok(ret)
}

```

===

# テスト
-------

* ￣\＿(ツ)＿/￣

===

# まとめ
--------

* 案外普通に使えるよ
* モデルはちょっと面倒かもね
* クエリは困ったらSQL生牡蠣
* マイグレーションとかはまた今度

</textarea>
