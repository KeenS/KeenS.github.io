---
categories: [Rust, Diesel, 小ネタ, Advent Calendar, Advent Calendar 2017, Rust Advent Calendar]
date: 2017-12-16T16:40:36+09:00
title: "diesel小ネタ集"
---

κeenです。これは[Rust Advent Calendar 2017](https://qiita.com/advent-calendar/2017/rust-lang)の記事です。
RustのORM、クエリビルダ、マイグレーションツールのdieselについて。
dieselについては[公式チュートリアル](http://diesel.rs/guides/getting-started/)を読めばだいたい使い始められるのですが、それだけでは足りないのでいくつか持ちネタを書きます。
<!--more-->
diesel-1.0.0-beta1での情報です。

# `table!` マクロ
チュートリアルには`infer_schema!`を使ったDSLの自動生成が書かれてますが、`table!`マクロを使って自分で書くこともできます。

``` rust
table! {
  posts {
    id -> Integer,
    Title -> Text,
    body -> Text,
    published -> Bool
  }
}
```

このときの型は[dieselで定義されているSQL型](http://docs.diesel.rs/diesel/types/index.html)を書きます。
このSQL型は特定のDBMにあるものも用意されていて、例えばPostgreSQLなら[この辺](http://docs.diesel.rs/diesel/pg/types/sql_types/index.html)や[この辺](http://docs.diesel.rs/diesel/pg/data_types/index.html)にあります。

## viewの扱い
dieselは基本的にはviewをサポートしてません。
もちろん`infer_schema!`はviewは見てくれないのでどうしても使いたい場合は上記`table!`マクロで生成します。
実際にSQLを発行してDBMに怒られるまでtypoとかに気づかないのでつらいですね。

あとdieselの型的にはINSERTが書けてしまいます。もちろん実行時にはエラーになります。気をつけましょう。

# NewType Pattern
0.99.0からQueryableがネスト可能に[なった](https://github.com/diesel-rs/diesel/pull/1184)ので

``` rust
#[derive(Queryable)]
pub struct PostId(pub i32);

#[derive(Queryable)]
pub struct Post {
    pub id: PostId,
    pub title: String,
    pub body: String,
    pub published: bool,
}
```
のように`id`に個別の型を与えることができるようになりました。

しかしちょっと注意が必要です。

まずクエリが`id`のためにタプルを作らないといけなくなります。

``` rust
    dsl::posts
        ...
        // idを`(dsl::id,)` のようにしていしないといけない
        .select(((dsl::id,), dsl::title, dsl::body, dsl::published))
        .get_result(conn)
```

また、`find(id)`のようにクエリビルダの引数に渡そうと思うともうちょっと工夫が必要です。
dieselのクエリビルダのDSLに混ぜるには`Expression`や`ToSql`のトレイトを実装しないといけません。

とはいえ、どのidでも中身は同型なのでワンパターンです。これはマクロを用意すれば解決できて、


``` rust
use diesel::types::{Integer, ToSql, ToSqlOutput, IsNull};
use diesel::backend::Backend;
use std::io::Write;
use std::error::Error;
macro_rules! derive_for_ids {
    ($ty: ty) => {
        // `Expression`系を自動定義。
        // このマクロはdieselで定義されている。
        expression_impls!(BigInt -> $ty);

        // `ToSql`を定義
        impl<DB: Backend> ToSql<BigInt, DB> for $ty {
            fn to_sql<W: Write>(
                &self,
                out: &mut ToSqlOutput<W, DB>,
            ) -> Result<IsNull, Box<Error + Send + Sync>> {
                ToSql::<BigInt, DB>::to_sql(&self.0, out)
            }
        }
    }
}

```

というマクロで一撃です。

このマクロは以下のように使います。

```rust

#[derive(Queryable, Debug)]
pub struct PostId(i32);
derive_for_ids!(PostId);

```


このようにしてあげれば

``` rust
pub fn find_post<'a>(conn: &SqliteConnection, id: PostId) -> Option<Post> {
    use schema::posts::dsl;

    dsl::posts
        // findの引数に`PostId`の値を渡せる
        .find(id)
        .select(((dsl::id,), dsl::title, dsl::body, dsl::published))
        .get_result(conn)
        .optional()
        .expect("Error finding")
}
```

のように`PostId`のままDSLが使えます。

# enum
dieselはSQLのenumをサポートしていません。なのでDB側では`Integer`で定義して、Rustの`enum`にマッピングしてあげることを考えます。

因みに、復習も兼ねて、Rustの`enum`は全ての列挙子が引数を取らなければ(いわゆるC-like enumであれば)

``` rust
pub enum Visibility {
    Public = 0,
    Limited = 1,
    Private = 2,
}
```

のように数値を割り当てられますね(何も指定しなければ0始まりの連番です)。
そして`Visibility::Public as i32`のように数値にキャスト可能です(逆はできません)。

これを用いて諸々のDSL化に使われているトレイトを実装します。先程と同じく`Expression`、`ToSql`、`Queryable`ですが`derive(Queryable)`が使えないのでちょっと大変です。

同じくワンパターンなのでマクロを定義してあげます。
`enum`の列挙子を全て捕捉しないといけないので先程とちょっと変わって`enum`の定義をラップする形にします。

``` rust
use diesel::types::{Integer, ToSql, ToSqlOutput, IsNull, FromSql, FromSqlRow, HasSqlType};
use diesel::row::Row;
use diesel::Queryable;
use diesel::backend::Backend;
use std::io::Write;
use std::error::Error;

macro_rules! define_enum {
    ($(#[$meta:meta])*
     pub enum $name: ident { $($variant: ident = $val: expr,)*}) => {
        // 元のenumを定義
        $(#[$meta])*
        pub enum $name {
            $($variant = $val,)*
        }

        // `ToSql`を定義
        impl<DB: Backend> ToSql<Integer, DB> for $name {
            fn to_sql<W: Write>(
                &self,
                out: &mut ToSqlOutput<W, DB>,
            ) -> Result<IsNull, Box<Error + Send + Sync>> {
                ToSql::<Integer, DB>::to_sql(&(*self as i32), out)
            }
        }

        // `Expression`系を自動定義
        expression_impls!(Integer -> $name);

        // `Queryable`のために`FromSql`と`FromSqlRow`を定義
        impl<DB: Backend> FromSql<Integer, DB> for $name
            where i32: FromSql<Integer, DB>  {
            fn from_sql(bytes: Option<&DB::RawValue>) -> Result<Self, Box<Error + Send + Sync>> {
                use self::$name::*;
                match <i32 as FromSql<Integer, DB>>::from_sql(bytes)? {
                    $($val => Ok($variant),)*
                    s => Err(format!("invalid {} value: {}", stringify!($name), s).into()),
                }
            }
        }

        impl<DB: Backend> FromSqlRow<Integer, DB> for $name
            where i32: FromSql<Integer, DB> {
            fn build_from_row<T: Row<DB>>(row: &mut T) -> Result<Self, Box<Error + Send + Sync>> {
                use self::$name::*;
                match <i32 as FromSqlRow<Integer, DB>>::build_from_row(row)? {
                    $($val => Ok($variant),)*
                    s => Err(format!("invalid {} value: {}", stringify!($name), s).into()),
                }
            }
        }

        // `Queryable`を定義
        impl<DB> Queryable<Integer, DB> for $name
            where
            DB: Backend + HasSqlType<Integer>,
            $name: FromSqlRow<Integer, DB>,
        {
            type Row = Self;

            fn build(row: Self::Row) -> Self {
                row
            }
        }

    }
}

```

というマクロを用意してあげます。

このマクロは

``` rust
define_enum! {
    #[derive(Debug, Clone, Copy)]
    pub enum Visibility {
        Public = 0,
        Limited = 1,
        Private = 2,
    }
}
```

のように使います。

そしたら以下のように構造体に含められますし、

``` rust

#[derive(Queryable)]
pub struct Post {
    pub id: PostId,
    pub title: String,
    pub body: String,
    // `Visibility`が`Queryable`の中に
    pub published: Visibility,
}


#[derive(Insertable)]
#[table_name = "posts"]
pub struct NewPost<'a> {
    pub title: &'a str,
    pub body: &'a str,
    // `Visibility`が`Insertable`の中に
    pub visibility: Visibility,
}
```

あるいはクエリビルダの中で

``` rust
    dsl::posts
        .find(id)
        // `visibility`をselect
        .select(((dsl::id,), dsl::title, dsl::body, dsl::visibility))
        .get_result(conn)

```

や

``` rust
    let new_post = NewPost {
        title: title,
        body: body,
        visibility: Visibility::Private,
    };

    insert_into(posts::table)
        // `visibility`をinsert
        .values(&new_post)
        .execute(conn)
```

のように使えます。

# 終わりに

小ネタ集といいつつ3つくらいしか出てきませんでした。
まあ、その代わり内容が濃いめだったのでバランスが取れたかな？

また思いついたら随時投稿しようと思います。
