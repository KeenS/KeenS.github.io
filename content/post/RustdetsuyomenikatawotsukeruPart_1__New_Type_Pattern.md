---
categories: ["Rust", "AdventCalendar", "Advent Calendar 2018", "Rustで強めに型をつける"]
date: 2018-12-15T04:15:08+09:00
title: "Rustで強めに型をつけるPart 1: New Type Pattern"
---
このエントリは[Rustその2 Advent Calendar 2018](https://qiita.com/advent-calendar/2018/rust2) 6日目の記事を時空を遡って書いています。

κeenです。頭痛い。寝れない。寝れないので空いてる日の分を埋めに行きます。
「Rustで強めに型をつける」シリーズではある程度有名なテクニックを紹介しようかと思います。
みんな当たり前のように書くと知らない人はいつまで経っても知る機会が無いですからね。まずはNew Type Patternから。

<!--more-->

# 基本

例えば[crates.io](https://crates.io)を作るとしましょう。エンティティがいくつかありますね。クレートやユーザなど。

``` rust
struct Crate {
    id: u64,
    authors: Vec<User>,
    // ...
}

struct Release {
    id: u64,
    crate_id: u64,
}


struct User {
    id: u64,
    // ...
}

```

どのエンティティも大体 `id` フィールドを持つと思います。このような状況で以下の関数を見て下さい。

``` rust
fn list_releases(id: u64) -> Vec<Release> {
    unimplemented!()
}
```

この `id` には何を渡すべきでしょうか。クレートのIDを渡してそのリリース一覧を取得する関数？それともユーザIDを渡してそのユーザがauthorのリリース一覧？
パッとはわからないですね。
この問題への対処は関数がおかしいとか仮引数の名前をちゃんとつけろだとかドキュメントを書けとか色々意見があるかと思いますが、我々の頼れるコンパイラに任せてしまうのがNew Type Patternです。
まず、IDを表す型を導入します。

``` rust
struct CrateId(u64);
struct ReleaseId(u64);
struct UserId(u64);
```

そしてそれらを使います。


``` rust
struct Crate {
    id: CrateId,
    authors: Vec<User>,
    // ...
}

struct Release {
    id: ReleaseId,
    crate_id: CrateId,
}


struct User {
    id: UserId,
    // ...
}

```

これで型からどのエンティティのIDを意図していたかが分かります。
これで先程の関数を書き直すと、意図がはっきりします。

``` rust
fn list_releases(id: CrateId) -> Vec<Release> {
    unimplemented!()
}
```

ドキュメントや変数名など人に頼るのではなくコンパイラの型チェックに乗せた自動化された仕組みで正しさを保証できるようになりました。


ところでこのバターン、別の見方もできます。IDの実装の隠蔽です。
IDとは別に `u64` である必要はなくて、 `u32` かもしれませんし UUIDかもしれません。
ユーザIDはGitHubのIDで持ってるかもしれません。 それを隠蔽するために新しい型を導入したとも見れます。


# 幽霊型

ところで先程の `XxxId` 、エンティティが増えたら `XxxId` も増えるのでしょうか。増えます。大体のID型に求められる実装は同じ(`Copy` を導出するなど)なのでコピペが捗りますね。
これはマクロで対処することもできるのですが、もう少し楽に出来る方法があります。幽霊型(Phantom Type)を使う方法です。
まず、IDの実体は以下のように定義されます。


``` rust
use std::marker::PhantomData;

#[derive(Copy, Clone, Debug, Hash, Default, PartialEq, Eq, Ord, PartialOrd)]
struct Id<T> {
    id: u64,
    _phantom: PhantomData<T>
}

impl<T> Id<T> {
    pub fn new(id: u64) -> Self {
        id,
        _phantom: PhantomData,
    }

    pub fn get(&self) -> u64 {
        self.id
    }
}

```

本来ならIDに必要ない `T` というパラメータが増えています。
Rustは使われていない型パラメータを許さないので適当にダミーの値を作って型パラメータを消費します。そのためのデータ型が `PhantomData` です。
これは型パラメータを消費するためだけの存在で、サイズを持ちません。
これを使って先程の例はこう書けます。


``` rust
struct Crate {
    id: Id<Crate>,
    authors: Vec<User>,
    // ...
}

struct Release {
    id: Id<Release>,
    crate_id: CrateId,
}


struct User {
    id: Id<User>,
    // ...
}

```

内部実装は `u64` に固定されてしまいますが、実装を繰り返すことなくID型を定義できました。


# ノート

* New Type Pattternは公式のスタイルガイドにも[載っています](https://doc.rust-lang.org/1.0.0/style/features/types/newtype.html)
* IDに幽霊型を付けるアイディアはScala文化から借りました。元になったスライドはあるのですが今探したら見当たりませんでした。
* Rustの `PhantomData` の扱いは罠があるのですが詳しくは[qnighyさんのブログ](https://qnighy.hatenablog.com/entry/2018/01/14/220000)を参考にして下さい。
  + 因みにここの例は説明を省くために罠を踏んだ形になってます。注意。
