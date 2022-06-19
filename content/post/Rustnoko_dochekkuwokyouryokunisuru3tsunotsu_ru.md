---
categories: [Rust, 形式検証, 抽象解釈]
date: 2022-06-20T03:53:40+09:00
title: "Rustのコードチェックを強力にする3つのツール"
---

κeenです。
Rustaceanのみなさんは普段書いてるRustのコードを検証するのに `cargo check` や `cargo test` などのツールを使っているかと思いますが、それらを強力にするツールの [`miri`](https://github.com/rust-lang/miri) 、 [`MIRAI`](https://github.com/facebookexperimental/MIRAI) 、 [`kani`](https://github.com/model-checking/kani) をそれぞれ紹介します。

<!--more-->

Rustにはいくつかコードの正しさや行儀のよさを検査する方法がありますね。
`cargo check` や `cargo test` 、 あるいはビルドしたバイナリを走らせて検証したり色々手を尽くしているでしょう。
ですがこれらだけだとあらゆる種類のバグを拾いきれる訳ではないのもみなさん承知の通りだと思います。
既存の方法だと手が回らない所があるので他のアプローチでコードを検証してくれるツールがあると嬉しいですよね。
そういったツールを3つ紹介します。

先に注意しておくと、これらのツールはまだ成熟しきってはおらず使いづらい点、使い物にならない点も多々あるでしょう。
仕事で使うというよりは趣味プロジェクトで興味本位で導入してみるか、くらいになるんじゃないかと思います。

## miri
### 概要
[miri](https://github.com/rust-lang/miri)はRustのインタプリタです。`cargo run` や `cargo test` の代わりに `cargo miri run` や `cargo miri test` とするとRustのコードをインタプリタ実行してくれます。

このmiriの特徴の1つに、いくつかの種類の未定義動作が起きてないか検証してくれる点があります。要は `unsafe` を正しく使えているかのチェックに使える訳です。普通は未定義動作を踏むと特にエラーは起きずにプログラム実行した結果が壊れるのでみつけるのが難しいです。そこにきて未定義動作が起きている場所を特定してくれる `miri` は重宝します。

### 使い方
miriを使って簡単なコードを検証してみましょう。

インストールは詳しくはガイドに従って欲しいのですが、rustupで入ります。

```console
$ rustup +nightly-2022-06-14 component add miri
```

以下のコードを考えましょう。

```rust
fn main() {
    let arr = [0; 3];
    unsafe {
        println!("got {}", arr.get_unchecked(3));
    }
}
```

このコードは長さが3しかない配列の(0-indexedで)3番目の要素にアクセスしようとしているので存在しない要素へのアクセスとなり、未定義動作になります。しかしコンパイルしても特にエラーにならずに実行した結果が壊れるだけになってしまいます。

```console
$ cargo run
   Compiling miri-test v0.1.0 (/home/shun/Rust/miri-test)
    Finished dev [unoptimized + debuginfo] target(s) in 0.43s
     Running `target/debug/miri-test`
got 1278960184
```

しかし `miri` を使って実行するとちゃんとエラーになり、ソースコードに問題のある箇所を教えてくれます。

```console
$ cargo +nightly-2022-06-14 miri run
   Compiling miri-test v0.1.0 (/home/shun/Rust/miri-test)
    Finished dev [unoptimized + debuginfo] target(s) in 0.02s
     Running `/home/shun/.rustup/toolchains/nightly-2022-06-14-x86_64-unknown-linux-gnu/bin/cargo-miri target/miri/x86_64-unknown-linux-gnu/debug/miri-test`
error: Undefined Behavior: dereferencing pointer failed: alloc1733 has size 12, so pointer to 4 bytes starting at offset 12 is out-of-bounds
   --> /home/shun/.rustup/toolchains/nightly-2022-06-14-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/slice/mod.rs:405:18
    |
405 |         unsafe { &*index.get_unchecked(self) }
    |                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ dereferencing pointer failed: alloc1733 has size 12, so pointer to 4 bytes starting at offset 12 is out-of-bounds
    |
    = help: this indicates a bug in the program: it performed an invalid operation, and caused Undefined Behavior
    = help: see https://doc.rust-lang.org/nightly/reference/behavior-considered-undefined.html for further information

    = note: inside `core::slice::<impl [i32]>::get_unchecked::<usize>` at /home/shun/.rustup/toolchains/nightly-2022-06-14-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/slice/mod.rs:405:18
note: inside `main` at src/main.rs:4:28
   --> src/main.rs:4:28
    |
4   |         println!("got {}", arr.get_unchecked(3));
    |                            ^^^^^^^^^^^^^^^^^^^^

note: some details are omitted, run with `MIRIFLAGS=-Zmiri-backtrace=full` for a verbose backtrace

error: aborting due to previous error
```

便利ですね。

### 注意点

インストールにnightlyを使ってることから察した方もいるかと思いますが、まだ実験的な存在です。色々至らぬ点はあるでしょう。
また、設計上の制約もいくつかあります。インタプリタとして実行しているので実際に実行されなかった箇所での未定義動作は判別できません。エッジケースで未定義動作になる場合はちゃんとそこを通るような実行の仕方をしないといけない訳です。
またプラットフォーム依存のコードは動きませんしFFIも今のところサポートしていません。一番unsafeを使いたいところなので歯痒いですね。

これらを踏まえてバグが見付かればラッキーくらいの気持で使っていくといいんじゃないですかね。

## MIRAI
### 概要

[MIRAI](https://github.com/facebookexperimental/MIRAI)はMIRの抽象解釈によってパニックが起きる箇所をみつけてくれるツールです。要は `cargo check` の意図しないパニックをみつけてくれるバージョンです。さらに言えば `assert!` を上手く使うとある種のコードの正しさも検査できます。

抽象解釈というのはコードを近似的に解釈してある種の性質が成り立つかを検査する手法です。型が具体的な値を計算せずにある種の正しさを検査しているのに似ていますね。

### 使い方
インストールは公式のガイドを読んで下さい。自前でビルドすることになります。

先程と同様に配列の3番目にアクセスしてみましょう。ただし、今回は `get` を使っているので `Option` が返ります。そこに対して `unwrap` を呼んでいます。


```rust
fn main() {
    let arr = [0; 3];
    println!("{}", arr.get(3).unwrap());
}
```

これを `cargo mirai` にかけるとパニックするかもよ、と教えてくれます。


```console
$ cargo  mirai
    Checking miri-test v0.1.0 (/home/shun/Rust/miri-test)
    Finished dev [unoptimized + debuginfo] target(s) in 0.32s
    Checking miri-test v0.1.0 (/home/shun/Rust/miri-test)
warning: called `Option::unwrap()` on a `None` value
 --> src/main.rs:3:20
  |
3 |     println!("{}", arr.get(3).unwrap());
  |                    ^^^^^^^^^^^^^^^^^^^

warning: `miri-test` (bin "miri-test") generated 1 warning
    Finished dev [unoptimized + debuginfo] target(s) in 0.16s
```

MIRAIは抽象解釈をする、つまりある程度Rustのコードを理解しているので2番目の要素へのアクセスだと警告が出ません。

```rust
fn main() {
    let arr = [0; 3];
    println!("{}", arr.get(2).unwrap());
}

```

```console
$ cargo  mirai
    Checking miri-test v0.1.0 (/home/shun/Rust/miri-test)
    Finished dev [unoptimized + debuginfo] target(s) in 0.33s
    Checking miri-test v0.1.0 (/home/shun/Rust/miri-test)
    Finished dev [unoptimized + debuginfo] target(s) in 0.16s
```

あるいは、以下のように明示的に `assert_eq!` を使えばテストのような使い方もできます。

```rust
fn flatten<T>(e: Option<Option<T>>) -> Option<T> {
    match e {
        Some(Some(e)) => Some(e),
        _ => None,
    }
}

fn main() {
    let v = Some(Some(1));
    assert_eq!(flatten(v), Some(1));

    let v = Some(None::<bool>);
    assert_eq!(flatten(v), None);

    let v = None::<Option<f32>>;
    assert_eq!(flatten(v), None);
}
```

これは関数を呼び出した後に `assert` をつけていますが関数内に `assert` を置けばコードの正しさの確認になりますし、読む人へのドキュメントにもなります。

### 注意点

MIRAIは必ずパニックをみつけてくれる訳でもMIRAIが警告したら必ずダメなコードとも限りません。例えば上記の `flatten` の確認は真ん中の `assert` で常に成り立つにも関わらず警告が出ます。

```console
$ cargo mirai
    Checking miri-test v0.1.0 (/home/shun/Rust/miri-test)
    Finished dev [unoptimized + debuginfo] target(s) in 0.32s
    Checking miri-test v0.1.0 (/home/shun/Rust/miri-test)
warning: assertion failed
  --> src/main.rs:13:5
   |
13 |     assert_eq!(flatten(v), None);
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

warning: `miri-test` (bin "miri-test") generated 1 warning
    Finished dev [unoptimized + debuginfo] target(s) in 0.13s
```

因みに `Option(None::<bool>)` ではなく `Option(None::<i32>)` にすると検査が通ります。ちょっとナイーブなところがあるようです。

また、コードを実行するかのように検査をするので `main` 関数かテスト関数を起点としないと検査できません。ライブラリを書くときにちょっと注意ですね。

## kani
[kani](https://github.com/model-checking/kani)はモデル検査によりパニックが起きないか(+色々)検査するツールです。Quickcheckなどのプロパティベーステストを御存じの方はそれを強力にやるツールだと思えばいいでしょう。まあ、MIRAIと同様にpanicが起きないことを検査するツールとしても使えます。

ちょっとプロパティベーステストについて例を出します。例えば数値の絶対値のマイナスを返す関数 `nabs` を考えます。

```rust
pub fn nabs(i: i32) -> i32 {
    if i > 0 {
        -i
    } else {
        i
    }
}
```

この関数は必ず0以下の値を返すはずですね。こういう関数の性質のようなものを検査するがプロパティベーステストです。従来の方法だと [`proptest`](https://crates.io/crates/proptest) のようなライブラリを使っていくつか乱数で生成した値を使ってテストする方法が知られています。

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn i64_nabs_is_never_positive(a: i64) {
        assert!(nabs(a) <= 0);
    }
}
```

これはある程度は上手くいくのですが、下手鉄砲方式なのでほとんど正しくて一部のエッジケースで正しくないようなものをみつけるのが難しいという欠点がありました。

kaniは値を与えて結果を見るのではなく、プログラムを見て振る舞いを解析することで性質が成り立つかを検査するのでエッジケースにも上手く対処できます。

### 使い方
`cargo install` でインストールしたあとにセットアップすると使えます。

```console
cargo install --lock kani-verifier --version <VERSION>
cargo-kani setup
```

プロパティテストでは「任意の値に対して~」のような書き方をするのですが、 `kani` では `kani::any()` を使って任意の値を作れます。先程の `nabs` の例でいくと、 以下のようなテストが書けます。


```rust
#[cfg(kani)]
#[kani::proof]
fn test_nabs() {
    let i = kani::any();
    assert!(nabs(i) <= 0);
}
```

以下のように `cargo kani --tests` でテストの検査をしてくれます。ほとんど `cargo test` を置き換えるような形の使い方ですね。
ちょっと出力が長いですがちゃんと検査して正しいことを確認できます。

```console
$ cargo kani --tests
   Compiling miri-test v0.1.0 (/home/shun/Rust/miri-test)
    Finished test [unoptimized + debuginfo] target(s) in 0.04s
  Executable unittests src/lib.rs (target/x86_64-unknown-linux-gnu/debug/deps/miri_test-d3cc2857db208f27)
  Executable unittests src/main.rs (target/x86_64-unknown-linux-gnu/debug/deps/miri_test-b3499c7eb629943c)
Checking harness test_abs...
CBMC 5.59.0 (cbmc-5.59.0)
CBMC version 5.59.0 (cbmc-5.59.0) 64-bit x86_64 linux
Reading GOTO program from file target/x86_64-unknown-linux-gnu/debug/deps/cbmc-for-test_abs.out
Generating GOTO Program
Adding CPROVER library (x86_64)
Removal of function pointers and virtual functions
Generic Property Instrumentation
Running with 16 object bits, 48 offset bits (user-specified)
Starting Bounded Model Checking
Runtime Symex: 0.0110323s
size of program expression: 959 steps
slicing removed 546 assignments
Generated 6 VCC(s), 4 remaining after simplification
Runtime Postprocess Equation: 6.9162e-05s
Passing problem to propositional reduction
converting SSA
Runtime Convert SSA: 0.000723774s
Running propositional reduction
Post-processing
Runtime Post-process: 5.42e-06s
Solving with MiniSAT 2.2.1 with simplifier
297 variables, 780 clauses
SAT checker: instance is SATISFIABLE
Runtime Solver: 0.000465309s
Runtime decision procedure: 0.00123455s

RESULTS:
Check 1: test_abs.assertion.1
         - Status: SUCCESS
         - Description: "assertion failed: nabs(i) <= 0"
         - Location: src/lib.rs:13:5 in function test_abs

Check 2: <T as kani::Arbitrary>::any.unsupported_construct.1
         - Status: SUCCESS
         - Description: "resume instruction"
         - Location: ~/.kani/kani-0.4.0/library/kani/src/arbitrary.rs:17:5 in function <T as kani::Arbitrary>::any

Check 3: nabs.assertion.1
         - Status: SUCCESS
         - Description: "attempt to negate with overflow"
         - Location: src/lib.rs:3:9 in function nabs


SUMMARY:
 ** 0 of 3 failed

VERIFICATION:- SUCCESSFUL


Verification Time: 0.033749647s
Checking harness test_abs...
CBMC 5.59.0 (cbmc-5.59.0)
CBMC version 5.59.0 (cbmc-5.59.0) 64-bit x86_64 linux
Reading GOTO program from file target/x86_64-unknown-linux-gnu/debug/deps/cbmc-for-test_abs.out
Generating GOTO Program
Adding CPROVER library (x86_64)
Removal of function pointers and virtual functions
Generic Property Instrumentation
Running with 16 object bits, 48 offset bits (user-specified)
Starting Bounded Model Checking
Runtime Symex: 0.0109523s
size of program expression: 959 steps
slicing removed 546 assignments
Generated 6 VCC(s), 4 remaining after simplification
Runtime Postprocess Equation: 7.6222e-05s
Passing problem to propositional reduction
converting SSA
Runtime Convert SSA: 0.000724784s
Running propositional reduction
Post-processing
Runtime Post-process: 5.7e-06s
Solving with MiniSAT 2.2.1 with simplifier
297 variables, 780 clauses
SAT checker: instance is SATISFIABLE
Runtime Solver: 0.000470079s
Runtime decision procedure: 0.00124127s

RESULTS:
Check 1: test_abs.assertion.1
         - Status: SUCCESS
         - Description: "assertion failed: nabs(i) <= 0"
         - Location: src/lib.rs:13:5 in function test_abs

Check 2: <T as kani::Arbitrary>::any.unsupported_construct.1
         - Status: SUCCESS
         - Description: "resume instruction"
         - Location: ~/.kani/kani-0.4.0/library/kani/src/arbitrary.rs:17:5 in function <T as kani::Arbitrary>::any

Check 3: nabs.assertion.1
         - Status: SUCCESS
         - Description: "attempt to negate with overflow"
         - Location: src/lib.rs:3:9 in function nabs


SUMMARY:
 ** 0 of 3 failed

VERIFICATION:- SUCCESSFUL


Verification Time: 0.03509131s
Complete - 2 successfully verified harnesses, 0 failures, 2 total.
```


`VERIFICATION:- SUCCESSFUL` で終わっているので検査に通っています。


因みに勘のいい方は気付いたかもしれませんが、 `nabs` ではなくただの絶対値が非負なことを検査するテストだと検査に通りません。

```rust
#[cfg(kani)]
#[kani::proof]
fn test_abs() {
    let i = kani::any::<i32>();
    assert!(i.abs() >= 0);
}
```

```console
$ cargo kani --tests

...

RESULTS:
Check 1: test_abs.assertion.1
         - Status: FAILURE
         - Description: "assertion failed: i.abs() >= 0"
         - Location: src/lib.rs:13:5 in function test_abs

Check 2: core::num::<impl i32>::abs.assertion.1
         - Status: FAILURE
         - Description: "attempt to negate with overflow"
         - Location: /rustc/7466d5492b2d28d2ba5114dbe71511a6502ac822/library/core/src/num/int_macros.rs:2423:17 in function core::num::<impl i32>::abs

Check 3: <T as kani::Arbitrary>::any.unsupported_construct.1
         - Status: SUCCESS
         - Description: "resume instruction"
         - Location: ~/.kani/kani-0.4.0/library/kani/src/arbitrary.rs:17:5 in function <T as kani::Arbitrary>::any


SUMMARY:
 ** 2 of 3 failed
Failed Checks: assertion failed: i.abs() >= 0
 File: "/home/shun/Rust/miri-test/src/lib.rs", line 13, in test_abs
Failed Checks: attempt to negate with overflow
 File: "/rustc/7466d5492b2d28d2ba5114dbe71511a6502ac822/library/core/src/num/int_macros.rs", line 2423, in core::num::<impl i32>::abs

VERIFICATION:- FAILED


Summary:
Verification failed for - test_abs
Verification failed for - test_abs
Complete - 0 successfully verified harnesses, 2 failures, 2 total.
```

これは[2の補数](https://ja.wikipedia.org/wiki/2の補数)表現の限界で、 `i32::MIN` のマイナスを取っても対応する正の値が存在しないからです。
既存のランダムな値を入れてみるテストはこういうエッジケースを見付けるのが苦手ですが、kaniはちゃんとみつけてくれます。
これはこれで嬉しいのですが、既に分かってる限界については「`i32::MIN` 以外の入力では非負になる」のように入力に条件をつけて書けると嬉しいですよね。こういうときは `kani::assume` を使ってあげるとそこを考慮して検査してくれます。

```rust
#[cfg(kani)]
#[kani::proof]
fn test_abs() {
    let i = kani::any::<i32>();
    kani::assume(i != i32::MIN);
    assert!(i.abs() >= 0);
}
```


このコードなら検査に通ります。一般にテストは事前条件、操作、事後条件を使って記述しますが、事前条件に `kani::any` と `kani::assume` を使うイメージですね。

他にも検査に通らなかった場合にどういう実行系列で失敗したのか表示する機能などもあるので詳しくはドキュメントを読んでみて下さい。


### 注意点

`kani::any` であらゆる入力への検査をしている関係かは知りませんが、文字列のように無限の空間を持つ値の検査はできなようです。

```console
$ cargo kani --tests
   Compiling miri-test v0.1.0 (/home/shun/Rust/miri-test)
error[E0277]: the trait bound `&str: kani::Invariant` is not satisfied
  --> src/lib.rs:4:13
   |
4  |     let s = kani::any::<&str>();
   |             ^^^^^^^^^^^^^^^^^ the trait `kani::Invariant` is not implemented for `&str`
   |
   = help: the following other types implement trait `kani::Invariant`:
             ()
             [T; N]
             bool
             char
             f32
             f64
             i128
             i16
           and 24 others
   = note: required because of the requirements on the impl of `kani::Arbitrary` for `&str`
note: required by a bound in `kani::any`
  --> /home/shun/.kani/kani-0.4.0/library/kani/src/lib.rs:70:15
   |
70 | pub fn any<T: Arbitrary>() -> T {
   |               ^^^^^^^^^ required by this bound in `kani::any`

For more information about this error, try `rustc --explain E0277`.
Error: "Failed to compile crate."
Error: "Failed to compile crate."
error: could not compile `miri-test` due to previous error
warning: build failed, waiting for other jobs to finish...
error: could not compile `miri-test` due to previous error
Error: cargo exited with status exit status: 101
```

モデル検査はあらゆる可能性を探索するので大きなコードだと時間がかかりすぎる可能性があります。また、あまり複雑なロジックはそもそも成り立つ性質を言いづらいことがあります。小さな関数からはじめることになるでしょう。

因みに結果については信頼できるはずで、検査に通れば望んだ性質が必ず成り立つことが、検査に通らなければ少くとも1つ失敗する入力があることが言えるはずです。ただし検査がSUCCESSにもFAILUREにもならずに検査をあきらめてしまうこともあります。詳しくはモデル検査で調べてみて下さい。

## まとめ

Rustのコードを検査するツールをいくつか紹介しました。
どれも問題へのアプローチは優れているのですが、ツールの出来栄えの面でやや不安が残るかなというのが個人的感想です。
そういう意味では試してみる人が増えた方が改善がはかどるはずなので少し紹介してみました。
みなさんも興味が湧いたら使ってみて下さい。
