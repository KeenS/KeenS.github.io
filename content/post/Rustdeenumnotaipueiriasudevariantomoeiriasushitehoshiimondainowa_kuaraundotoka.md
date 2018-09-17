---
categories: ["Rust"]
date: 2018-09-17T16:16:34+09:00
title: "Rustでenumのタイプエイリアスでヴァリアントもエイリアスして欲しい問題のワークアラウンドとか"
---
κeenです。何度か「Rustでenumのタイプエイリアスしたときにヴァリアントもエイリアスして欲しい」という話を聞くので自分なりにどうにか出来ないかなと考えたやつをまとめます。
<!--more-->

# モチベーション

やりたいことはこんなの。
enumに`type`で別名を付けたときに `別名::ヴァリアント名` でアクセスしたいというもの。
特にジェネリクスを使ってるときにやりたくなる。


```rust
enum Foo<V> {
    Hoge,
    Piyo(V),
}

type IntFoo = Foo<i64>;

// ここで `IntFoo::Piyo` とアクセスしたい。

```

一応[RFC](https://github.com/rust-lang/rfcs/blob/master/text/2338-type-alias-enum-variants.md)にもなっているのでいつかは解決される。
しかし[Tracking issue](https://github.com/rust-lang/rust/issues/49683)はOpenでありいつ解決されるかは分からない。

そこでワークアラウンドを考えてみた。

# コード
要は型だけでなく名前空間もエイリアスしたのでイメージとしてはやりたいことはこうなる。
単相化する必要があるのでシンプルに `use Foo::*` するのはできなくて、真心込めて関数やConstを書いていく必要がある。

```rust
enum Foo<V> {
    Hoge,
    Fuga { x: i64, y: i64 },
    Piyo(V),
}

type IntFoo = Foo<i64>;

mod IntFoo {
    use super::{Foo, IntFoo};

    // Fooがpubでないので可視性を`pub(super)`にしてある。この辺は臨機応変に。
    pub(super) const Hoge: IntFoo = Foo::Hoge;
    pub(super) fn Piyo(v: i64) -> IntFoo {
        Foo::Piyo(v)
    }
}
```


ただし問題が2つある。1つ目はシンプルにコンパイルエラーになる点。型名とモジュール名は被ってはいけないらしい。というかモジュールって型名空間に入るんだ。

<code class="nohighlight">
<pre><font color="#EF2929"><b>error[E0428]</b></font><b>: the name \`IntFoo\` is defined multiple times</b>
 <font color="#729FCF"><b>--&gt; </b></font>enum_typename.rs:9:1
  <font color="#729FCF"><b>|</b></font>
<font color="#729FCF"><b>7</b></font> <font color="#729FCF"><b>| </b></font>type IntFoo = Foo&lt;i64&gt;;
  <font color="#729FCF"><b>| -----------------------</b></font> <font color="#729FCF"><b>previous definition of the type \`IntFoo\` here</b></font>
<font color="#729FCF"><b>8</b></font> <font color="#729FCF"><b>| </b></font>
<font color="#729FCF"><b>9</b></font> <font color="#729FCF"><b>| </b></font>mod IntFoo {
  <font color="#729FCF"><b>| </b></font><font color="#EF2929"><b>^^^^^^^^^^</b></font> <font color="#EF2929"><b>\`IntFoo\` redefined here</b></font>
  <font color="#729FCF"><b>|</b></font>
  <font color="#729FCF"><b>= </b></font><b>note</b>: \`IntFoo\` must be defined only once in the type namespace of this module
</pre>
</code>

もう一つは構造体型のヴァリアントをエイリアス出来ない点。上の例でも `Fuga` だけエイリアスがない。


これを回避したワークアラウンドがいくつかある。

1つ目の問題の解決策は2つある。
1つはモジュール名を変える。`IntFoo_` とか。ダサいけど。
もう一つは`type IntFoo`の方を変える。モジュール内に入れてしまって名前を `T` とかにする。

```type
mod IntFoo {
    use super::Foo;

    pub(super) type T = Foo<i64>;
}
```

RustっぽくはないけどMLのモジュールっぽい使い方。
残念ながらどちらの方法もRustにヴァリアントのエイリアスが実装されたら既存コードを変更して回らないといけない。


2つめの問題はシンプルに単相化を諦めるしかなさそう。

```rust
mod IntFoo {
    use super::Foo;

    pub(super) const Hoge: T = Foo::Hoge;
    // FooのFugaをそのまま使う
    pub(super) use super::Foo::Fuga;
    pub(super) fn Piyo(v: i64) -> T {
        Foo::Piyo(v)
    }
}
```

`Fuga` だけ多相のままにすることであるいは諦めて全部多相のままにすれば幾分がシンプルになる。

```
mod IntFoo {
    use super::Foo;

    pub(super) use super::Foo::*;
}
```

これも状況に応じて。



# まとめ


「Rustでenumのタイプエイリアスしたときにヴァリアントもエイリアスして欲しい」という欲求にはには言語側で対応中だが今すぐには使えない。
そこでいくつかワークアラウンドの案を挙げたがどれも言語側の対応が済むとコードの変更が必要になり中途半端。
また、実現したいことを全て出来るわけではない。
