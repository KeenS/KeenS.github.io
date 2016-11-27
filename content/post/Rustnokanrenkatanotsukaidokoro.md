---
categories: [Rust, 型]
date: 2016-11-22T18:26:47+09:00
title: Rustの関連型の使いどころ
---

κeenです。昨日は[RustのLT会！ Rust入門者の集い - connpass](https://rust.connpass.com/event/41467/)に参加してきました。
そこで[関連型に関する発表](http://qiita.com/tacke_jp/items/9c7617971dc341146c0f)があったので感化されて私も一筆。

<!--more-->
馴れないと関連型はジェネリクスでいいじゃんと思えますが、両者は別の機能を提供するものです。
設計による使い分けではなくて実現したいことに応じた機能による使い分けをするので馴れてしまえば迷うことなくどちらを使うか判断出来ます。

ということで関連型のパターンをいくつか。
もちろん、根底にある関連型という機能は共通なのでほとんど同じようなことを言ってますが気持としてパターンを知っておくと便利です。

# 型レベルの関数として
「関連」型なのである型に関連する型を表現します。ちょっと見方を変えると型から型への写像、つまり関数になります。
例えばこういうのです。

``` rust
trait ToUnsigned {
    type Counterpart;
    fn to_unsigned(self) -> Self::Counterpart;
}

impl ToUnsigned for i32 {
    type Counterpart = u32;
    fn to_unsigned(self) -> Self::Counterpart {
        self as u32
    }
}
```

`i32` から `u32` への関数になってそうなのが見えますかね？もちろん、 `i64` から `u64` などへの対応も作れます。
こういうのは例えば符号無し数にのみ演算が定義されている場合とかに便利ですね。

``` rust
fn write_bigendian_signed<I, U>(i: I) -> ()
    where I: ToUnsigned<Counterpart = U>,
          U: ...,  {
}
```

# トレイト内で使う型を固定するため

これが一番目にするやつじゃないでしょうか。

``` rust
trait Handler {
    type Request;
    type Response;
    fn handle(req: Self::Request, res: Self::Response) -> io::Result<()>;
}

struct HTTPHandler;

impl Handler for HTTPHandler {
    type Request = HTTPRequest;
    type Response = HTTPResponse;
    fn handle(req: Self::Request, res: Self::Response) -> io::Result<()>;
}
```

固定というか特殊化というか、トレイト自体は汎用的に作られていて、それを実装する型が特定の処理に特化するパターンです。
ジェネリクスで受け取る訳にはいかなくて、実装すべき型をトレイトの中で定義してあげる必要があるのは分かると思います。

# 関数の返り値を一般化するため

これは現在のRust(rust-1.13.0)が [`impl Trait`](https://github.com/rust-lang/rfcs/pull/1522)をサポートしていないために必要になるテクニックです。
関連型とトレイト境界を組み合わせて使います。

``` rust
trait ReverseIter {
    type Item;
    type Iter: Iterator<Item = Self::Item>;
    fn rev_iter(&self) -> Iter;
}
```

これの `type Iter: Iterator<Item = Self::Item>;` の方です。
返り値のIteratorを抽象化したいのですが、現在のRustでは`fn rev_iter(&self) -> Iterator<Item = Self::Item>;`のような書き方が出来ないので仕方なく関連型を使ってあげる必要があります。

ちょっと踏み込んだ話をすると、関数の引数の多相性は∀の量化、返り値の多相は∃の量化です。そしてトレイトのジェネリクスも∀の量化で関連型が∃の量化なのでそういう対応がある訳です。

# 最後に
結構理論的にも色々あるようなので調べてみると様々なブログポストが見付かると思います。

ぱぱっと思いついたパターンを3つ挙げました。もしかしたら他にもパターンがあるかもしれません。

これを知っておけば[tokio-service](https://github.com/tokio-rs/tokio-service)の[Service](https://tokio-rs.github.io/tokio-service/tokio_service/trait.Service.html)みたいな関連型を多用するパターンでもひるまなくなります。

# 参考

## 関連型とimpl Traitに関して

* [関連型](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/associated-types.html)
* [関連型が必要になる状況 | Rust by Example](http://rust-lang-ja.org/rust-by-example/generics/assoc_items/the_problem.html)
* [Abstract return types, aka `impl Trait`](http://www.ncameron.org/blog/abstract-return-types-aka-%60impl-trait%60/)

## 応用

* [RustのHigher-Kinded type Trait | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2016/02/28/rustnohigherkinded_type_trait/)

## 発展的な話題

* [Associated type constructors, part 1: basic concepts and introduction](http://smallcultfollowing.com/babysteps/blog/2016/11/02/associated-type-constructors-part-1-basic-concepts-and-introduction/)
* [Associated type constructors, part 2: family traits](http://smallcultfollowing.com/babysteps/blog/2016/11/03/associated-type-constructors-part-2-family-traits/)
* [Associated type constructors, part 3: What higher-kinded types might look like](http://smallcultfollowing.com/babysteps/blog/2016/11/04/associated-type-constructors-part-3-what-higher-kinded-types-might-look-like/)
* [Associated type constructors, part 4: Unifying ATC and HKT](http://smallcultfollowing.com/babysteps/blog/2016/11/09/associated-type-constructors-part-4-unifying-atc-and-hkt/)
* [Associated type constructors (a form of higher-kinded polymorphism). by withoutboats · Pull Request #1598 · rust-lang/rfcs](https://github.com/rust-lang/rfcs/pull/1598)
