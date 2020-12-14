---
categories: [Rust]
date: 2020-12-15T04:26:34+09:00
title: "Rustのconst fnって何？"
---

このエントリは[Rust 2 Advent Calendar 2020](https://qiita.com/advent-calendar/2020/rust2)の10日目の記事です。
前は mocyutoさんで[RustでEC2検索を簡単にするCLIの作り方 - Screaming Loud](https://yuutookun.hatenablog.com/entry/rust_ec2_search)、  
後は mas-yoさんで[RustでstaticなEntity Component System - Qiita](https://qiita.com/mas-yo/items/e966e2429b142bfc2f20)でした

空いてる日を埋める担当のκeenです。
気付いたら空きができてたので埋めます。
Rustのリリースノートなどでよくみる `const fn` についてです。

<!--more-->

# const文脈

Rustにはグローバルに値に名前をつける手段として `const` と `static` がありますね。

```rust
static VERSION: u64 = 130;
const PAGE_SIZE: usize = 4096;
```

これらの右辺の値に注目しましょう。
これらはコンパイル時に計算されて、生成されたバイナリの中に埋め込まれます。
となるとこの中に書ける式には制約がつきます。IOなどはできないのはもちろんのこと、ヒープにアクセスするコードも書けません。

``` rust
const VEC: Vec<i32> = vec![1];
```

```text
error[E0010]: allocations are not allowed in constants
 --> const.rs:1:23
  |
1 | const VEC: Vec<i32> = vec![1];
  |                       ^^^^^^^ allocation not allowed in constants
  |
  = note: this error originates in a macro (in Nightly builds, run with -Z macro-backtrace for more info)
```

この制限された `static` や `const` の右辺に書くときの文脈を [**const文脈**](https://doc.rust-lang.org/reference/const_eval.html#const-context) と呼びます。
この他には配列初期化構文 `[init; size]` の `size` の部分やその型 `[Type; Size]` の `Size` 、あとはC-like列挙型の判別子の設定に書ける式もconst文脈で評価されます。

昔はconst文脈に書ける式の制限がかなり強かったです。具体的にはタプルやデータ型のコンストラクタなどに制限されていました。
これで困るのが一部のデータ型です。例えば `static` 変数に `Vec` を持たせようにも、 `Vec` のフィールドは公開されていないので `Vec::new()` などの関数を呼ないとなりません。でも関数呼び出しは `const` 文脈じゃ書けないので八方塞がりです。

ということである程度条件を満たした関数をconst文脈で書けるようにしよう、というのがconst fnです。

# const fn

[const fn](https://doc.rust-lang.org/reference/const_eval.html#const-functions)はRust 1.31.0で導入された機能です。[そのときのリリースブログ](https://blog.rust-lang.org/2018/12/06/Rust-1.31-and-rust-2018.html)。

普通の関数定義に `const` を前置して  `const fn name() {}` の構文で定義します。

`const fn` はざっくり言うと以下の2つの機能を持ちます。

1. const文脈で呼べる（そのときはコンパイル時に評価される）
2. 関数本体で呼べる機能に制約がある
  + const文脈とは微妙に違う制約

1は分かりやすいですね。const文脈でconst fnを呼べます。例えば [`Vec::new`](https://doc.rust-lang.org/std/vec/struct.Vec.html#method.new)は `const fn` なのでconst文脈で呼べます。

``` rust
const VEC: Vec<i32> = Vec::new();
```

const文脈で呼べる関数は増えるに越したことはないのでconst fnにできそうな関数は順次const fnにされていってます。

あるいは、const文脈でできることが増えたので昔はハックが必要だったことも簡単にできるようになりました
cf: [lazy_static はもう古い！？ once_cell を使おう](https://zenn.dev/frozenlib/articles/lazy_static_to_once_cell)。

2は、おおむねconst文脈といっしょです。
ですが浮動小数点数の演算ができないなど、いくつか異なる制約があります。

``` rust
const FLT: f64 = 1.0 + 2.0; // OK


const fn flt() -> f64 {
    1.0 + 2.0
    // error[E0658]: floating point arithmetic is not allowed in constant functions
    //  --> const.rs:6:5
    //   |
    // 6 |     1.0 + 2.0
    //   |     ^^^^^^^^^
    //   |
    //   = note: see issue #57241 <https://github.com/rust-lang/rust/issues/57241> for more information
}
```

まあ、これは細かい話なのでエラーになったらはじめて調べればいいでしょう。

# const fnでできること

`const fn` に限らずconst文脈でできることも多いですが、意外と表現力があります。
例えば変数の破壊的代入と `while` ループが書けるのでこういうことも書けます。

``` rust
// const文脈
const SUM: i32 = {
    let v = &[1, 2, 3];
    let mut i = 0;
    let len = v.len();
    let mut result = 0;
    while i < len {
        result += v[i];
        i += 1;
    }
    result
};

// const fn
const fn sum(v: &[i32]) -> i32 {
    let mut i = 0;
    let len = v.len();
    let mut result = 0;
    while i < len {
        result += v[i];
        i += 1;
    }
    result
}
```

`for` 式は内部で `Iterator::next` を呼び出しているのですが、これが `const fn` でないので使えません。

あるいは `if` と再帰呼出もできるので `n` 番目のフィボナッチ数を求めるコードも簡単に書けます。

``` rust
const fn fib(n: u32) -> u64 {
    if n < 2 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}
```

おもしろいですね。

# まとめ

const文脈についてと、const文脈で関数を呼べるようになる機能const fnについて紹介しました。
