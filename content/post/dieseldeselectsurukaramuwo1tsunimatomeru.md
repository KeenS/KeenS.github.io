---
categories: ["Rust", "Advent Calendar", "Rust Advent Calendar", "diesel", "小ネタ"]
date: 2019-12-07T01:59:08+09:00
title: "dieselでselectするカラムを1箇所にまとめる"
---
κeenです。このエントリは[Rustその3 Advent Calendar 2019 - Qiita](https://qiita.com/advent-calendar/2019/rust3)の7日目の記事です。空いてたので飛び入り参加しました。

軽い小ネタです。Dieselで `select` する時にいちいちカラム名書くの面倒だよねって話です。

<!--more-->

長い前置きは端折って、以下のようなコードを考えます。

```toml
[dependencies]
chrono = "0.4.10"
diesel = { version = "1.4.3", features = ["chrono"] }
```


```rust
#[macro_use]
extern crate diesel;

use chrono::prelude::*;
use diesel::prelude::*;
use diesel::result::Error;
use diesel::table;

table! {
    users {
        id -> Integer,
        name -> Text,
        email -> Text,
        created_at -> Timestamp,
    }
}
```

ひとまず `created_at` を使わないとして、 `User` を定義しておきましょう。

```rust
#[derive(Queryable, Identifiable)]
struct User {
    id: i32,
    name: String,
    email: String,
}
```

即座にこれを `find` するコードが書けるはずです。

``` rust
// 具体的なDBに依存するのが面倒なのでジェネリクスで書いたが、
// 実用上は `PgConnection` など具体的なDBのコネクションを指定した方が楽
fn find_user<Cn, B>(cn: &Cn, id: i32) -> Result<Option<User>, Error>
where
    Cn: Connection<Backend = B>,
    B: diesel::backend::Backend<RawValue = [u8]>,
{
    use self::users::dsl::*;
    users
        .find(id)
        .select((self::users::id, name, email))
        .get_result(cn)
        .optional()
}

```

あるいは、検索するコードも書けますね

```rust
use diesel::sql_types::Timestamp;
use diesel::types::ToSql;
fn load_recent_users<Cn, B>(cn: &Cn, threshold: DateTime<Utc>) -> Result<Vec<User>, Error>
where
    Cn: Connection<Backend = B>,
    B: diesel::backend::Backend<RawValue = [u8]>,
    NaiveDateTime: ToSql<Timestamp, B>,
{
    use self::users::dsl::*;
    users
        .filter(created_at.ge(threshold.naive_utc()))
        .select((id, name, email))
        .load(cn)
}
```

さて、ここで問題になるのがdieselの良いところでも悪いところでもあるselectするカラムについてです。
Dieselはモデルがテーブルとは直接関係を持たないORMなので `users` テーブルから `User` を取得するには毎度カラムを指定する必要があります。
このおかげでRustのコードとSQLのインタフェースを綺麗に分けることができますし、例えば `deleted_users` のように別のテーブルからも `User` を取得できます。
代わりにロードするカラムは手で指定しないといけません。規模が小さいうちはそれでもいいのですが規模が大きくなるとフィールドを追加するたびにあちこち変更して周らないといけなくなり、とても手に負えなくなります。

そこで部分的にですがRustのデータ型とデータベースのテーブルを関連付けてカラム名の取得を省略できる方法を紹介します。

まあ、話は単純でこういうトレイトを用意してあげて、

```rust
trait Selectable {
    type Columns;
    fn columns() -> Self::Columns;
}

```

こういう実装を与えるだけです。

```rust
impl Selectable for User {
    type Columns = (users::id, users::name, users::email);

    fn columns() -> Self::Columns {
        (users::id, users::name, users::email)
    }
}
```

そうすればクエリの `select` 部分にカラム名を書かなくてよくなります。

```rust
fn find_user<Cn, B>(cn: &Cn, id: i32) -> Result<Option<User>, Error>
where
    Cn: Connection<Backend = B>,
    B: diesel::backend::Backend<RawValue = [u8]>,
{
    use self::users::dsl::*;
    users
        .find(id)
        // カラム名を直接書かなくてよくなった
        .select(User::columns())
        .get_result(cn)
        .optional()
}
```


この手法のいいところは合成可能な点です。

例えば `users` と関係のある `crates` というテーブルを考えてみましょう。

``` rust
table! {
    crates {
        id -> Integer,
        name -> Text,
        version -> Text,
        author_id -> Integer,
        created_at -> Timestamp,
    }
}

joinable!(crates -> users (author_id));
allow_tables_to_appear_in_same_query!(users, crates);
```

これに対応するデータ型をこう定義したとします。

``` rust
#[derive(Queryable, Identifiable)]
struct Crate {
    id: i32,
    name: String,
    version: String,
    // Users構造体を保持
    author: User,
}
```

`author_id` ではなく `author` と `User` 構造体そのまま保持してることに注意して下さい。


これには以下のように `Selectable` を実装できます。


``` rust
impl Selectable for Crate {
    type Columns = (
        crates::id,
        crates::name,
        crates::version,
        // UserのColumnsをそのまま使える
        <User as Selectable>::Columns,
    );

    fn columns() -> Self::Columns {
        // Userのcolumnsをそのまま使える
        (crates::id, crates::name, crates::version, User::columns())
    }
}
```

これはこのままクエリに使えます。

``` rust
fn load_create_of<Cn, B>(cn: &Cn, author_id: i32) -> Result<Vec<Crate>, Error>
where
    Cn: Connection<Backend = B>,
    B: diesel::backend::Backend<RawValue = [u8]>,
{
    use self::crates::dsl;
    use self::users::dsl::users;

    dsl::crates
        .filter(dsl::author_id.eq(author_id))
        .inner_join(users)
        .select(Crate::columns())
        .load(cn)
}
```

便利ですね。

注意点としてはテーブルとカラム名を指定してるので例えば `User` を `deleted_users` から取得するときは手でカラム名を書く必要があります。
`Selectable` にパラメータを持たせて `SelectableFrom<users>` とか `SelectableFrom<deleted_users>` とか書けるようにする手もありますが手間の割にあんまり便利にならなそうですね。実用の観点ではバランスを考えて導入しましょう。


ということでdisel小ネタでした。是非お試しあれ。
