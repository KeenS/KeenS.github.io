---
categories: ["Rust"]
date: 2019-01-16T08:35:35+09:00
description:
title: "void"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# void
----------------------
[Shinjuku.rs #2 @FORCIA](https://forcia.connpass.com/event/110888/)
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
# 発散型
--------

* `!`
* [ドキュメント](https://doc.rust-jp.rs/the-rust-programming-language-ja/1.6/book/functions.html#%E7%99%BA%E6%95%A3%E3%81%99%E3%82%8B%E9%96%A2%E6%95%B0)
  + 標準ライブラリだと [exit](https://doc.rust-lang.org/std/process/fn.exit.html) とか
  + 他には無限ループ、パニック、 `return`/`continue` など
* 呼び出し元に制御が返らない
* どんな型にもなれる

===
# 発散型
--------

``` rust
use std::process::exit;
// pub fn exit(code: i32) -> !

let hoge: i32 = exit(0);
let fuga: () = exit(0);
```

===
# `match` と型
--------------

* `match` の返り値は腕の型で決まる
* 発散型と他の型の組み合わせだと他の型になる

``` rust
let piyo: i32 = match 0 {
    // !
    0 => exit(0),
    // i32
    n => 1i32,
};
```

===
# `Void`
--------

``` rust
enum Void {}
```

===
# `Void`
--------

* バリアント(=コンストラクタ)がない
  + 値を作れない
* もしその値を受け取れるとしたらどうなる？

===
# `Void` の作り方
-----------------

* 発散型を使うと(コード上は)作れる
* 発散型なので実行時にはこのコードに到達しない

``` rust
let void: Void = unimplemented!();
//...
```

===
# `Void` の作り方2
-----------------

* 関数を使うと(コード上は)受け取れる
* 実際にはこの関数は呼べない

``` rust
fn take_void(void: Void) {
    // ...
}
```

===
# `Void` にマッチ
---------------

* Q: これはコンパイルが通る？
* A: 通る<!-- .element: class="fragment" data-fragment-index="1" -->

``` rust
fn absurd(void: Void) {
    match void {};
}
```

===
# `Void` にマッチ
---------------

* 返り値型はなんでもいい

``` rust
fn absurd(void: Void) -> () {
    match void {}
}
```

===
# `Void` にマッチ
---------------

* 返り値を発散型にもできる!

``` rust
fn absurd(void: Void) -> ! {
    match void {}
}
```

===
# `Void` と列挙型
-----------------

* `Result<T, Void>` という型を考える
* `Void` は作れないので `Err` はありえない
* `Ok` しか存在しない `Result` 型に

===
# `Void` と列挙型
-----------------

* Q: これはコンパイルが通る？
* A: 通らない<!-- .element: class="fragment" data-fragment-index="1" -->

``` rust
let result: Result<(), Void> = Ok(());
match result {
  Ok(()) => //
}

```

===
# `absurd` の使いどころ
-----------------------

* これはコンパイルが通る

``` rust
let result: Result<(), Void> = Ok(());
match result {
  Ok(()) => //
  Err(void) => absurd(void),
}

```

===
# `absurd` の使いどころ
-----------------------

* こういうのも作れる

``` rust
fn safe_unwrap<T>(result: Result<T, Void>) -> T {
    match result {
        Ok(t) => t,
        Err(void) => absurd(void),
    }
}
```

===
# まとめ
--------

* `!` は特殊な型だよ
* 空の列挙型 (`Void`) から `!` が作れるよ
* `Void` を使うと面白いことができるよ
* See also [voidクレート](https://crates.io/crates/void) - 今回の内容を実装してるやつ
* See also [never type](https://doc.rust-lang.org/std/primitive.never.html) と [RFC](https://github.com/rust-lang/rfcs/blob/master/text/1216-bang-type.md) - `!` を空の列挙型にする提案


</script>
</section>
