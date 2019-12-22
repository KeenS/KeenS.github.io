---
categories: [Rust, 言語実装]
date: 2016-10-10T16:48:43+09:00
title: Rustのクロージャ3種を作って理解する
---

κeenです。Rustのクロージャ、3種類もあって複雑ですよね。
こういう複雑なものはRustの気持になって考えれば理解出来たりします。ということで手でクロージャを作って理解してみましょう。

尚、これはRustの1.12.0-stableと1.14.0-nightly (6e8f92f11 2016-10-07)で実験しています。
<!--more-->
# 用語

* 関数 - 関数
* 無名関数 - 名前のついていない関数
* 自由変数 - そのスコープの中で束縛されていない変数
* クロージャ - 関数に、自由変数の値(環境)もくっつけてそれだけで閉じるようにしたもの

クロージャについては関数が閉じ込める環境だけを指してクロージャとする人もいますが、ここでは関数本体と環境を合わせてクロージャとします。

よく、無名関数とクロージャ(関数閉包)を一緒くたにすることがありますが明確に区別しましょう。まあ、大抵の無名関数はクロージャになりますが、動的スコープの言語みたいにそうならないケースもあります。

余談ですが「ラムダ式」は言語がよく無名関数に付ける名前です。「関数」も言語によって「手続き」だったり「メソッド」だったりしますね。

# `FnOnce` `FnMut` `Fn`


さて、「作って理解する」なので例を出します。なんかよく分からないけど0から9の値に1を足して印字するプログラムです。

``` rust
fn main() {
    let x = 1;
    for i in 0..10 {
        // 無名関数を作る
        let cls = |arg|{ i + arg };
        println!("{}", cls(x));
    }
}
```

無名関数を作って `cls` という変数に束縛しています。 `cls` は自由変数 `i` を含みますので、 `cls` のクロージャを取ると 「`cls` の定義 + `i` の定義」になります。

では `i` という変数を保持したオブジェクトを作りましょう。

``` rust
struct Closure {
    i: isize,
}


fn main() {
    let x = 1;
    for i in 0..10 {
        // クロージャを作る
        let cls = Closure {i: i};
        // もちろん、Closureは関数でないので呼び出せない
        // println!("{}", cls(x));
    }
}
```

さて、このままだと `cls` は呼び出せません。しかし `+` なんかのオーバーロードを許しているRustはなんと関数呼び出し構文 `name(arg, ...)` もオーバーロード出来ます。実装すべきトレイトは `std::ops::{FnOnce, FnMut, Fn}` です。これがクロージャ3種です。

これらは継承関係にあって、上から順に `FnOnce`  `FnMut` `Fn` です。

``` rust
pub trait FnOnce<Args> {
    type Output;
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

pub trait FnMut<Args>: FnOnce<Args> {
    extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output;
}

pub trait Fn<Args>: FnMut<Args> {
    extern "rust-call" fn call(&self, args: Args) -> Self::Output;
}

```

これらの違いは `self` をムーブして受け取るか `&mut` で受けるか `&` で受けるかです。

## `FnOnce`

とりあえず先程の `Closure` にこれを実装してみましょう。ちょいとunstableな機能なようなので `feature` を使って、nightlyでコンパイルするようにしてみます。

``` rust
#![feature(unboxed_closures)]
#![feature(fn_traits)]

use std::ops::{FnOnce, FnMut, Fn};

// 再掲
struct Closure {
    i: isize,
}

impl FnOnce<(isize,)> for Closure {
    type Output = isize;

    extern "rust-call" fn call_once(self, (arg, ): (isize,)) -> Self::Output {
        self.i + arg
    }
}

```

これで呼び出しが出来る筈です。

``` rust
fn main() {
    let x = 1;
    for i in 0..10 {
        // 無名関数を作る
        let cls = Closure {i: i};
        // 呼出せるようになる
        println!("{}", cls(x));
    }
}

```

勿論、これは以下のように明示的にメソッドを呼んでも同じです。

``` rust
fn main() {
    let x = 1;
    for i in 0..10 {
        // クロージャを作る
        let cls = Closure {i: i};
        // メソッドで呼ぶ
        println!("{}", cls.call_once((x, )));
    }
}

```


## `FnMut` ~~`FnMut`~~ `Fn`

`FnOnce` で出来るんならなんで `FnMut` と ~~`FnMut`~~ `Fn`  が存在するかというと、クロージャが閉じ込めた環境の所有権の問題ですね。

2018-02-26: 上記2打ち消し線部分修正しました

例えば以下のコードを手で実装してみます。

```rust
fn main() {
    let x = 1;
    // ループの外でクロージャを作る
    let cls = |arg| { x + arg};
    for i in 0..10 {
        // 複数回クロージャを呼ぶ
        println!("{}", cls(i));
    }
}

```

先程と同じ `Closure` ではコンパイル出来ません（統一感のために `Closure` のメンバ名を変えました）。

``` rust
#![feature(unboxed_closures)]
#![feature(fn_traits)]

use std::ops::{FnOnce, FnMut, Fn};

struct Closure {
    x: isize,
}

impl FnOnce<(isize,)> for Closure {
  // ...略
}


fn main() {
    let x = 1;
    // ループの外でクロージャを作る
    let cls = Closure {x: x};
    for i in 0..10 {
        // 複数回クロージャを呼ぶ
        println!("{}", cls(i));
    }
}
```

```
error[E0382]: use of moved value: `cls`
  --> closure.rs:45:28
   |
45 |             println!("{}", cls(i));
   |                            ^^^ value moved here in previous iteration of loop
   |
   = note: move occurs because `cls` has type `Closure`, which does not implement the `Copy` trait

error: aborting due to previous error
```

1回目のループでは問題ないのですが `call_once(self, (arg, ): (isize,))` の呼び出しでムーブしてしまうので2回目以降は呼べません。ということで1つ下のトレイト、 `FnMut` も実装してみましょう。


``` rust
// ...略

impl FnMut<(isize,)> for Closure {

    extern "rust-call" fn call_mut(&mut self, (arg, ): (isize,)) -> Self::Output {
        self.x + arg
    }
}

fn main() {
    let x = 1;
    let cls = Closure {x: x};
    for i in 0..10 {
        println!("{}", cls(i));
    }
}
```

実はこれでも怒られます。

```
error: cannot borrow immutable local variable `cls` as mutable
  --> closure.rs:45:28
   |
43 |         let cls = Closure {x: x};
   |             --- use `mut cls` here to make mutable
44 |         for i in 0..10 {
45 |             println!("{}", cls(i));
   |                            ^^^ cannot borrow mutably

error: aborting due to previous error
```

何故なら `mut` を要求しているから。正確にはこうです。

``` rust
// ...略


fn main() {
    let x = 1;
    // `mut` を付ける
    let mut cls = Closure {x: x};
    for i in 0..10 {
        println!("{}", cls(i));
    }
}
```

これで通ります。しかしまあ、何か書き換えてる訳でもないのに `mut` が付いているのは嫌ですね。`mut` を要求しないように定義してあげようと思ったら `Fn` が必要になります。

``` rust
// ...略

impl Fn<(isize,)> for Closure {
    extern "rust-call" fn call(&self, (arg,): (isize, )) -> Self::Output {
        self.x + arg
    }
}

fn main() {
    let x = 1;
    let cls = Closure {x: x};
    for i in 0..10 {
        println!("{}", cls(i));
    }
}
```

これで通ります。

## 整理

* クロージャとは関数とその環境のセットだよ
* 環境にも所有権はあるよ
* Rustは所有権でクロージャの呼び出しを使い分けるよ

# 構文
じゃあ、 `FnOnce` `FnMut` `Fn` に対応するRustの構文を探っていきます。

## `Fn`

先程 `Fn` まで実装しないとコンパイルが通らなかった

``` rust
fn main() {
    let x = 1;
    let cls = |arg| { x + arg};
    for i in 0..10 {
        println!("{}", cls(i));
    }
}
```


は `||{}` でコンパイルが通っているので `||{}` は `Fn` を作るようです。


## `FnMut`

じゃあ、 `call_mut` を要求しそうにちょっと変えてみます。

``` rust
fn main() {
    let mut x = 1;
    let cls = |arg|{ x += arg };
    for i in 0..10 {
        cls(i);
    }
    println!("{}", x);
}
```

はい、予想通りエラーが出ます。

```
error: cannot borrow immutable local variable `cls` as mutable
  --> closure.rs:37:13
   |
35 |         let cls = |arg|{ x += arg };
   |             --- use `mut cls` here to make mutable
36 |         for i in 0..10 {
37 |             cls(i);
   |             ^^^ cannot borrow mutably

error[E0502]: cannot borrow `x` as immutable because it is also borrowed as mutable
  --> closure.rs:39:24
   |
35 |         let cls = |arg|{ x += arg };
   |                   -----  - previous borrow occurs due to use of `x` in closure
   |                   |
   |                   mutable borrow occurs here
...
39 |         println!("{}", x);
   |                        ^ immutable borrow occurs here
40 |     }
   |     - mutable borrow ends here

error: aborting due to 2 previous errors

```

先程の `FnMut` の例のように `cls` に `mut` を付けてみます。


``` rust
fn main() {
    let mut x = 1;
    let mut cls = |arg|{ x += arg };
    for i in 0..10 {
        cls(i);
    }
    println!("{}", x);
}
```

実は、これでもエラーです。


``` rust
error[E0502]: cannot borrow `x` as immutable because it is also borrowed as mutable
  --> closure.rs:39:24
   |
35 |         let mut cls = |arg|{ x += arg };
   |                       -----  - previous borrow occurs due to use of `x` in closure
   |                       |
   |                       mutable borrow occurs here
...
39 |         println!("{}", x);
   |                        ^ immutable borrow occurs here
40 |     }
   |     - mutable borrow ends here

error: aborting due to previous error
```

何故なら `cls` が `x` を捕捉し続けているから。これは `cls` のライフタイムを `println!` の前で終わらせてあげると解決します。

``` rust
fn main() {
    let mut x = 1;
    {
        // `{}` でスコープを作ることでライフタイムをコントロールする
        let mut cls = |arg|{ x += arg };
        for i in 0..10 {
            cls(i);
        }
    }
    println!("{}", x);
}
```

`FnMut` は変数を `mut` にする無名関数と同じなようです。

## `FnOnce`

じゃあ、 `FnOnce` は？ `std::mem::drop` を使って所有権を無理矢理奪うクロージャを作ってみましょう。

``` rust
use std::mem::drop;

fn main() {
    let x = String::new();
    let cls = || {drop(x)};
    cls();
}
```

はい、実はこれでOKです。冷静に考えたら `FnOnce` を継承している `Fn` を `||{}` で作れた時点で問題ありませんでしたね。

# 落穂拾い
## `move` クロージャ

じゃあ、Rustが良い感じに使い分けてくれるなら `move` クロージャって何のためにあるの？と思いますが、これは環境を捕捉する際に `Copy` なんかが起こらないようにするためのものです。

* `move` しない例

``` rust

let mut num = 5;

{
    let mut add_num = |x: i32| num += x;

    add_num(5);
}

assert_eq!(10, num);
```

* `move` する例

``` rust
let mut num = 5;

{
    let mut add_num = move |x: i32| num += x;

    add_num(5);
}

assert_eq!(5, num);
```

`move` は環境 **へ** 所有権を移すかどうか、 `FnOnce` などは環境 **から** 所有権を移すかどうかです。

## 関数ポインタ

クロージャを持たない関数は、 `fn(i32) -> i32` のような型を持ちます。これは空の環境を持つクロージャと同じですね。空の環境に所有権も何もないのでプリミティブの `fn` は `FnOnce` `FnMut` `Fn` のいずれとしても振舞います。

# 参考
ここまで書いて普通にドキュメントに詳しく載っているのに気付きました（汗

* [関数](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/functions.html)
* [クロージャ](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/closures.html)
