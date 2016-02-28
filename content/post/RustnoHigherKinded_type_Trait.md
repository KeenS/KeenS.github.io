---
categories: [Rust]
date: 2016-02-28T14:40:59+09:00
title: RustのHigher-Kinded type Trait
---

κeenです。少し前の話になりますがRustの関連型を駆使してHigher Kinded type Trait（高階型トレイト）を実現してしまったものが出回ってたのでそれについて。
本来RustはHKTをサポートしていませんが不思議なことに実装出来てしまっているのです。

HKTについて微塵も知らない方にも分かるように解説していきます。
<!--more-->
# `map` 可能なトレイト

[`Option`](https://doc.rust-lang.org/core/option/enum.Option.html#method.map)、[`Result`](https://doc.rust-lang.org/core/result/enum.Result.html#method.map)、etc 色々なトレイトが `map` メソッドを実装しています。

それぞれ型シグネチャを抜き出してみましょう。

* Option: `map<U, F: FnOnce(T) -> U>(self, f: F) -> Option<U>`
* Result: `map<U, F: FnOnce(T) -> U>(self, op: F) -> Result<U, E>`

驚く程似てますね。これを抽象化して「`map`メソッドを持つ」トレイトを定義したくなるかもしれません。
しかしそれは簡単には書けません。何故なら`map`はコンテナ型、つまり「型引数を取る型」に定義される型だからです。
現在のRustは「型引数を取る型」に対するジェネリクスはサポートしていません。もし書こうとするとこのようになるでしょう。

``` rust
trait Mappable<M<_>, T> where Self: M<T> {
  fn map<U, F: FunOnce(T) -> U>(self, f: F) -> M<U>;
}
```

`M<_>` なんて型パラメータを書くことは出来ませんし`M<T>`や`M<U>`も無理があります。残念ですね。

# `Iterator` との違い

さて、`Iterator`にも[`map`](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.map)はあります。
これこそが求めている「`.map` 可能なトレイト」ではないのでしょうか。
これはおおよそは合っているのですがやや期待とは違う動作をします。

`Option`の`map`は不自由なく使えます。

``` rust
let o = Some(1);
let o = o.map(|i| i + 1);
println!("{:?}", o);
```

一方、`Vec`は全く同じような書き方は出来ません。

``` rust
let v = vec![1];
let v = v.map(|i| i + 1);
println!("{:?}", v);
```

```
<anon>:3:11: 3:25 error: no method named `map` found for type `collections::vec::Vec<_>` in the current scope
<anon>:3 let v = v.map(|i| i + 1);
                   ^~~~~~~~~~~~~~
<anon>:3:11: 3:25 note: the method `map` exists but the following trait bounds were not satisfied: `collections::vec::Vec<_> : core::iter::Iterator`, `[_] : core::iter::Iterator`
error: aborting due to previous error
playpen: application terminated with error code 101
```

正しくは、こうです。

``` rust
let v = vec![1];
let v: Vec<i32> = v.iter().map(|i| i + 1).collect();
println!("{:?}", v);
```

色々実装上のノイズが乗っているのでやや例として不適切だったかもしれませんが、注目して欲しい部分はここです。

``` rust
let v: Vec<i32> = ...
```

`Vec`から取り出したイテレータなのに`Vec`に戻すためにわざわざ型アノテーションを書く必要があります。

どうしてこうなるかというとイテレータを取り出した時点で元のコンテナ型が`Vec<>`であるという情報が失われているからです。
これはRustの型システム上どうしても仕方のないことで、なので恐らくRustは「`map`可能なトレイト」などを定義せず、コレクションの操作を`Iterator`に集約しているんだと思います。
この方式はある程度は上手くいきますが`Option`や`Result`はIteratorになれはしてもIteratorからは作れない(2要素のイテレータからOptionは作れない)ので独自でmapメソッドを持つことになってしまいます。

# `map` 可能なトレイト again

先程そういうのは作れないと言ったばかりですが実はどうにかする方法がなくもないです。先程の(仮想の)定義を思い出して下さい。


``` rust
trait Mappable<M<_>, T> where Self: M<T> {
  fn map<U, F: FunOnce(T) -> U>(self, f: F) -> M<U>;
}
```

ジェネリクスになっている登場人物は`M<_>`、`U`、`T` ですね。しかし`M<_>`は実際にはそのまま使われいる訳ではなく`M<T>`または`M<U>`の形でのみ使われています。
では、`M<T>`、`M<U>`をパラメータに取るようにしてはどうでしょうか。


``` rust
trait Mappable<T, MT> where Self: MT {
  fn map<U, MU, F: FunOnce(T) -> U>(self, f: F) -> MU;
}
```

これで出来…てません。これは欲しい、正しいもの *も* 作れてはいるのですが正しくないものも含んでいます。

``` rust
impl <T, MT: Option<T> Mappable<MT, T>  for Option<T> {
  fn map<U, MU, F: FunOnce(T) -> U>(self, f: F) -> MU {...}
}

let r: Result<String, ()> = Some(1).map(|i| i.to_string());
```

`Option` に対する`map`を`Result`で受けようとしています。
これはもちろんおかしなことですが上記の型定義に従うとこういうことが出来てしまいます。
もうちょっというと上のトレイトの定義はこういう「おかしなこと」が起こるような実装をしろ、と言っている訳で、実際には実装出来ません。

本来なら自由パラメータが3つなのに4つの自由パラメータを取ってしまったので不整合が起きる訳です。
では、自由パラメータを3つにして1つを従属パラメータにしてはどうでしょうか。それは[関連型](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/associated-types.html)を使えばRustでも出来ます。


``` rust
trait Mappable<T, U, MT> where Self: MT {
  type MU;
  fn map<F: FunOnce(T) -> U>(self, f: F) -> MU;
}
```

実は、これである程度上手くいきます。
もちろん、実際に`M<_>`を取れるのに比べると不自由で下手をするとバグる(MUを無関係な型に定義できてしまう)可能性があるのですが、先程の4パラメータの時と比べて
トレイトが定義された時点で返す型は決まっているのでちゃんと実装出来ますし、使う時に不整合が起きることもありません。

# HKT

さて、先の`Mappable`の仮想定義、`map`関数を除いて汎用的にするとこうなりますね。

``` rust
trait HKT<T, U, MT> where Self: MT {
  type MU;
}
```

`Self: MT` というアノテーションをつけてますが実際には使えません。`impl`を書くときに気をつけて実装するしかないです。なのでこうなります。

``` rust
trait HKT<T, U> {
  type MU;
}

impl <T, U> HKT<T, U> for Option<T> {
  type MU = Option<U>;
}
```

さて、こう書くとするとこういうことも出来てしまいます。

``` rust
trait HKT<T, U> {
  type MU;
}

impl <S, T, U> HKT<T, U> for Option<S> {
  type MU = Option<U>;
}
```

`T`とは独立な`S`に対するコンテナに対して実装できてしまうのです。「実装するときに気をつける」とはいいましたが制約を書けるなら書いた方がいいです。こうしましょう。

``` rust
trait HKT<U> {
  type T;
  type MU;
}

impl <T, U> HKT<U> for Option<T> {
  type T = T;
  type MU = Option<U>;
}
```

これで冒頭に紹介したHKTの実装になりました。

逆にこのHKTの実装からMappableを定義するには、こうです。

``` rust
trait Mappable<U>: HKT<U> {
  fn map<F: FunOnce(Self::T) -> U>(self, f: F) -> Self::MU;
}
```

# HKTについてもっと

ようやく話題のHKTの元実装です。こちら。

[Higher-kinded type trait](https://gist.github.com/14427/af90a21b917d2892eace)


ずいぶんとモナモナしいですがやはり高階型を使った一番実績のある構造はこのあたりでしょう。

こういうのを扱えるとプログラミングの幅が広がりますね。

# ノート
* モナドの話題が出ましたがRustでモナドを実現するには高階型だけでなく複数ある関数トレイトのどれを選ぶかだとかその他諸々の問題を扱う必要があるようです。
  [Rust and the Monad trait - Not just higher kinded types · m4rw3r](https://m4rw3r.github.io/rust-and-monad-trait/)


