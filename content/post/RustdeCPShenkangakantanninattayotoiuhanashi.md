---
categories: ["Rust", "Advent Calendar", "Rust Advent Calendar", "Advent Calendar 2019", "継続", "CPS"]
date: 2019-12-07T17:59:51+09:00
title: "RustでCPS変換が簡単になったよという話"
---

κeenです。このエントリは[Rust Advent Calendar 2019 - Qiita](https://qiita.com/advent-calendar/2019/rust) 8日目の記事です。
昔書いたRustのコードをもけもけ漁ってたら、しばらく前にRustでCPS変換しようとしてた下書きをたまたま発見しました。

当時はコンパイラの制約で簡単には書けなかったのですが今見ると簡単に書けるようになってたのでRustも進歩してるね、という確認をしたいと思います。

<!--more-->

# CPS変換とは

CPS変換とは、プログラムを継続渡し形式(Continuation-Passing Style)に変換することです。
じゃあCPSとはというと、雑に言えば「ネストした関数の呼び出し禁止」の形式です。
…ちょっと雑すぎますね。
プログラムのコントロールフロー(`if` 式や 関数呼出)を陽に受け取った継続で記述する形式です。
この形式はよくコンパイラの内部表現なんかに使われます。
変数束縛の右側に `if` 式や関数呼び出しがこないという制約がつくので解析がやりやすくなるんですね。

例えば以下のコードをCPS変換してみましょう。

```rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}

fn sub(x: i32, y: i32) -> i32 {
    x - y
}

// ↓ これ
sub(add(1, 2), 5)
```


関数呼出がネストしてますね。これを書き換えたいです。
しかし変数束縛の右側に関数呼び出しを置くのは禁止なのでちょっと困ります。

関数内の末尾で関数を呼出すのは可能なので、「後でやりたい事を関数で渡して関数内で呼んでもらう」という戦略でいきましょう。

そのためには `add` を「最後に呼ぶ関数」を受け取るように変換する必要があります。

```rust
fn add_cps<R>(x: i32, y: i32, c: impl FnOnce(i32) -> R) -> R {
    c(x + y)
}

fn sub(x: i32, y: i32) -> i32 {
    x - y
}


add_cps(1, 2, |ret| sub(ret, 5))
```

`add` だけ変換してると今度は `add` して `sub` したくなったときにまた不都合なので `sub` の方も変換しておきましょう。 `sub` の引数が1つ浮つくので `id` を渡してそのまま結果を返すようにしておきましょう。

```rust
fn add_cps<R>(x: i32, y: i32, c: impl FnOnce(i32) -> R) -> R {
    c(x + y)
}

fn sub_cps<R>(x: i32, y: i32, c: impl FnOnce(i32) -> R) -> R {
    c(x - y)
}


fn id<T>(t: T) -> T {
    t
}

add_cps(1, 2, |ret| sub_cps(ret, 5, id))
```


こういうのがCPS変換です（雑）。
制御フロー構文の多いRustで「RustにおけるCPS変換」をバシっと定義するのは難しいのでふわっとしたまま進んでいきます。

もう少し例を見ましょう `if` 式の場合です。

```rust
fn max_1(x: i32, y: i32) -> i32 {
   let max = if x < y {
        y
    } else {
        x
    };
    add(max, 1)
}
```


変数束縛の右側にif を置くのも禁止なのでこれも変換します。

これも `if` から返ったあとの処理を一旦関数にまとめて、then/else節でそれを呼んであげます。

```rust
fn max_1_cps<R>(x: i32, y: i32, c: impl FnOnce(i32) -> R) -> R {
   let c = |max| {add_cps(max, 1, c) };
   if x < y {
        c(y)
    } else {
        c(x)
    }
}
```

変数束縛の関数「呼び出し」は禁止されてますが、関数（クロージャ）の「定義」は可能なのでこれで問題ありません。

CPS変換は自然に体が求めるものなので、いつでもできるようにしておきたいですね。

# 問題のコード

「関数呼び出しや `if` の後の処理（＝継続）を一旦クロージャにして引数に渡す」という戦略でCPS変換はできるのですが、Rustでは別の問題が起きます。


フィボナッチ数列を求める関数をCPSで書いてみましょう。

```rust
fn add_cps<R>(x: u64, y: u64, c: impl FnOnce(u64) -> R) -> R {
    c(x + y)
}

fn sub_cps<R>(x: u64, y: u64, c: impl FnOnce(u64) -> R) -> R {
    c(x - y)
}

fn fib_cps<R>(n: u64, c: impl FnOnce(u64) -> R) -> R {
    if n < 2 {
        c(1)
    } else {
        // fib(n - 1) + fib(n - 2)の変換
        // -> add(fib(sub(n, 1)), fib(sub(n, 2)))
        // -> let n1 = sub(n, 1);
        //    let f1 = fib(n1);
        //    let n2 = sub(n, 2);
        //    let f2 = fib(n2);
        //    add(f1, f2)
        //  ->
        sub_cps(n, 1, move |n1| {
            fib_cps(n1, move |f1| {
                sub_cps(n, 2, move |n2| fib_cps(n2, move |f2| add_cps(f1, f2, c)))
            })
        })
    }
}
```

これでロジックは正しいんですが、呼び出そうとするとコンパイルエラーです。

```rust
fib_cps(10, id)
```

```text
error: reached the type-length limit while instantiating `sub_cps::<u64, [closure@cps_fib....64 {id::<u64>}]]]]]]]]]]]]]]]]]>`
  --> cps_fib.rs:23:1
   |
23 | / fn sub_cps<R>(x: u64, y: u64, c: impl FnOnce(u64) -> R) -> R {
24 | |     c(x - y)
25 | | }
   | |_^
   |
   = note: consider adding a `#![type_length_limit="1310713"]` attribute to your crate
```

Rustのジェネリクス（`impl FnOnce`）はC++のテンプレートと同じく実装をコピーしていることを思い出して下さい。
さらに、クロージャも1回書く毎に別々の型が生成されることを思い出して下さい。
その上で `fib_cps` の末尾を見てみましょう。`fib_cps` は引数の型に応じて本体をコピーしますが、その中でクロージャを `fib_cps` に渡しているので新たな本体のコピーを作らないといけません。
するとまたクロージャのコピーが増えて…新しい生まれてしまい…と、いくつコピーが必要か計算しようとすると無限に再帰してしまうので型チェックに失敗するのです。

別の解釈をすると、CPS変換はスタックを使うコードをヒープを使うコードに変換します。
Rustはジェネリクスで受け取ったデータ型の大きさを静的に決定しようとします。
ところが `fib` で使うスタックサイズ（呼び出しのネストの深さ）は静的には決まらないので当然 `fib_cps` が受け取る `c` のサイズも決定できずにコンパイルエラーになっています。


これはジェネリクスをやめれば解決するはずです。 `Box<dyn FnOnce>` を使ってみましょう。

```rust
fn add_cps<R>(x: u64, y: u64, c: Box<dyn FnOnce(u64) -> R>) -> R {
    c(x + y)
}

fn sub_cps<R>(x: u64, y: u64, c: Box<dyn FnOnce(u64) -> R>) -> R {
    c(x - y)
}

fn fib_cps<R: 'static>(n: u64, c: Box<dyn FnOnce(u64) -> R>) -> R {
    if n < 2 {
        c(1)
    } else {
        sub_cps(
            n,
            1,
            Box::new(move |n1| {
                fib_cps(
                    n1,
                    Box::new(move |f1| {
                        sub_cps(
                            n,
                            2,
                            Box::new(move |n2| fib_cps(n2, Box::new(move |f2| add_cps(f1, f2, c)))),
                        )
                    }),
                )
            }),
        )
    }
}

```

`Box::new` が増えた分冗長になりましたがまあ、前と変わらないコードです。
`Box::new` の呼び出しがネストしてる点については目を瞑って下さい。

これを実行してみましょう。

```rust
fn main() {
    let ret = fib_cps(10, Box::new(id));
    println!("fib(10) = {}", ret);
}

```

```text
fib(10) = 89
```

無事実行できました。めでたしめでたし。

# Rust 1.35.0

…と、今のRust (1.39)であれば問題ないんですが、つい最近まではこのコードはコンパイルできませんでした。
試しに 1.35.0 より前のコンパイラでこのコードをコンパイルしてみましょう。

```text
$ rustc +1.34.2 cps_fib.rs
error[E0161]: cannot move a value of type (dyn std::ops::FnOnce(u64) -> R + 'static): the size of (dyn std::ops::FnOnce(u64) -> R + 'static) cannot be statically determined
  --> cps_fib.rs:74:5
   |
74 |     c(x + y)
   |     ^

error: aborting due to previous error

For more information about this error, try `rustc --explain E0161`.
```

エラーが出てしまいました。当時のRustでは `Box<dyn FnOnce>` を呼び出せなかったのです。
`FnOnce` は捕捉した環境をby moveで渡す + FnOnceはクロージャなのでサイズが静的に決まらない + Rustはサイズが静的に決まらない値を関数の引数に渡せないというコンボが決まった結果です。
詳しくは以下のブログを読んで下さい。こちらでもfibのCPS変換が出てきていますね。人類の自然な欲求なのでまあ、よくあることでしょう。

[FnBoxについて - 簡潔なQ](http://qnighy.hatenablog.com/entry/2017/05/24/070000)


というのが1.35.0より前の話。
Rust 1.35.0からこの問題が解決されました。1.35.0の[リリースノート](https://github.com/rust-lang/rust/blob/master/RELEASES.md#language-4)を見てみるとこう書かれてます。

> [`FnOnce`, `FnMut`, and the `Fn` traits are now implemented for `Box<FnOnce>`, `Box<FnMut>`, and `Box<Fn>` respectively.](https://github.com/rust-lang/rust/pull/59500/)

こうやってRustでもCPS変換が簡単に書けるようになりました。

# Rust 1.35.0より前

CPS変換は体が自然に求めるものなので 1.35.0より前でも人類はどうにかしてRustでCPS変換をしていました。
折角なのでそのときのテクニックを紹介します。

まず、 `Box` にされたトレイトが呼び出せないならはじめから `Box` を受け取るようなコードを書けばいいだけです。
こういうトレイトを用意しましょう。

```rust
trait FnBox<A> {
    type Out;
    fn call(self: Box<Self>, A) -> Self::Out;
}
```

これはNightlyにある[FnBox](https://doc.rust-lang.org/alloc/boxed/trait.FnBox.html)と同じものですが、4行のためにNightlyを使う意味は薄いのでコピペしてStableコンパイラで使います。

あとはこれを使ってクロージャを作ります。
クロージャはただの `Fn*` トレイトを実装した構造体なのでした。
それくらいならマクロで自動生成できます。

```rust
macro_rules! cls {
    // 捕捉する環境、引数、返り型、本体を受け取る
    ([$($env:ident : $env_ty: ty),*]|$($param:ident : $param_ty: ty),*| -> $out_ty: ty {$body:expr}) => {
        {
            // 捕捉する環境は構造体にエンコード
            struct Cls {
                $($env: $env_ty),*
            }
            // あとはそれにFnトレイトを実装
            impl FnBox<($($param_ty,)*)> for Cls {
                type Out = $out_ty;
                fn call(self: Box<Self>, $($param),* : ($($param_ty,)*)) -> Self::Out {
                    $(let $env = self.$env;)*;
                    $body
                }
            }
            // ここからFnBoxを作る
            let bx:Box<dyn FnBox<($($param_ty,)*), Out = $out_ty>> = Box::new(Cls {
                $($env),*
            });
            bx
        }
    }
}
```


さて、 `fib_cps` を定義しましょう。


```rust
fn add_cps<R>(x: u64, y: u64, c: Box<dyn FnBox<(u64,), Out = R>>) -> R {
    c.call((x + y,))
}

fn sub_cps<R>(x: u64, y: u64, c: Box<dyn FnBox<(u64,), Out = R>>) -> R {
    c.call((x - y,))
}

fn fib_cps<R: 'static>(n: u64, c: Box<dyn FnBox<(u64,), Out = R>>) -> R {
    if n < 2 {
        c.call((1,))
    } else {
        sub_cps(
            n,
            1,
            // ここで関数のジェネリックパラメータを参照してエラー
            cls!([c:Box<dyn FnBox<u64, Out = R>>]|n1: u64| -> R { c.call(n1) }),
        )
    }
}
```


残念ながら、この戦略は失敗します。
`FnBox` を実装するときに `fib` の帰り型の `R` を参照する必要が出てきますが、これは許可されていません。

さて、失敗してしまったので戦略を練り直します。
もっと視座を高くして継続の気持になってみましょう。
第一級市民になって引数を渡り歩く生活。道すがら環境を捕捉して旅をしつつ呼び出されていきます。
旅の終着点は？最後は初期継続を得て値となります。

そう、分かりましたね。つまるところ、「既に受け取ってしまった継続」で使われている型パラメータを参照してしまうのが問題です。
受け取った時点で型が固定されているのに、ジェネリクスなので参照できないというジレンマが生じている訳です。

ということは「継続を後で受け取る」ようにすれば、型パラメータはそのときまで固定されないので問題は解決します。

「継続を後で受け取る」トレイトを定義してあげましょう。

```rust
// 今までの
trait FnBox<A> {
    type Out;
    fn call(self: Box<Self>, arg: A) -> Self::Out;
}

// 継続変換された値を表わすトレイト。 型パラメータは値と継続の返り値の型
trait Cont<A, R> {
    fn cont(self: Box<Self>, c: Box<dyn FnBox<A, Out = R>>) -> R;
}
```

`Cont` はメソッド `cont` で起動できます。
このときに初期継続を渡してあげます。
`Cont` に型パラメータ `R` を保持しているので関数のジェネリクスにある浮いたパラメータを参照しなくてよくなります。

定数のCPS変換は `Cont` 流にやるとこうなります。

```rust
struct Const<C>(C);

impl<C, R> Cont<C, R> for Const<C> {
    fn cont(self: Box<Self>, c: Box<dyn FnBox<C, Out = R>>) -> R {
        c.call(self.0)
    }
}
```


難しいですか？まあ、でも使ってみれば簡単です。

```rust
fn add_cps<R>(x: u64, y: u64) -> Box<dyn Cont<u64, R>> {
    Box::new(Const(x + y))
}

fn sub_cps<R>(x: u64, y: u64) -> Box<dyn Cont<u64, R>> {
    Box::new(Const(x - y))
}

```

さて、 `fib` はネストした継続が出てくるので少しばかり面倒です。

まずは `cls` マクロを改造しておきます。型パラメータを取れるようになりました。

``` rust
macro_rules! cls {
    (<$($ty: ident),*>[$($env:ident : $env_ty: ty),*]|$($param:ident : $param_ty: ty),*| -> $out_ty: ty {$body:expr}) => {
        {
            use std::marker::PhantomData;
            struct Cls<$($ty),*> {
                $($env: $env_ty,)*
                    _p: PhantomData<($($ty),*)>,
            }
            impl<$($ty: 'static),*> FnBox<($($param_ty),*)> for Cls<$($ty),*> {
                type Out = $out_ty;
                fn call(self: Box<Self>, $($param),* : ($($param_ty),*)) -> Self::Out {
                    let s = *self;
                    let Cls {_p, $($env),*} = s;
                    $body
                }
            }
            let bx:Box<dyn FnBox<($($param_ty),*), Out = $out_ty>> = Box::new(Cls {
                $($env,)*
                    _p: PhantomData
            });
            bx
        }
    }
}
```

その上で、便利な道具として `and_then` を定義しておきましょう。

``` rust
fn and_then<A: 'static, B: 'static, R: 'static>(
    c: Box<dyn Cont<A, R>>,
    f: Box<dyn FnBox<A, Out = Box<dyn Cont<B, R>>>>,
) -> Box<dyn Cont<B, R>> {
    Box::new(AndThen {
        c: c,
        f: f,
        _phantom: PhantomData,
    })
}

use std::marker::PhantomData;

struct AndThen<A, B, R> {
    c: Box<dyn Cont<A, R>>,
    f: Box<dyn FnBox<A, Out = Box<dyn Cont<B, R>>>>,
    _phantom: PhantomData<B>,
}

impl<A: 'static, B: 'static, R: 'static> Cont<B, R> for AndThen<A, B, R> {
    fn cont(self: Box<Self>, cont: Box<dyn FnBox<B, Out = R>>) -> R {
        let s = *self;
        let AndThen { c, f, .. } = s;
        c.cont(cls!(<A, B, R>[
            f: Box<dyn FnBox<A, Out = Box<dyn Cont<B, R>>>>,
            cont: Box<dyn FnBox<B, Out = R>>]
                    |arg: A| -> R { (f).call(arg).cont(cont) }))
    }
}

```


見た目はごちゃっとしてますけど「よくある」 `and_then` のコードです。

いよいよラスト、`and_then` と `cls` を駆使して `fib` のCPS変換をします。

``` rust
fn fib_cps<R: 'static>(n: u64) -> Box<dyn Cont<u64, R>> {
    if n < 2 {
        Box::new(Const(1))
    } else {
        and_then(
            and_then(
                sub_cps(n, 1),
                cls!([]<R>|n1: u64| -> Box<dyn Cont<u64, R>> { fib_cps(n1) }),
            ),
            cls!([n: u64]<R>|f1: u64| -> Box<dyn Cont<u64, R>> {
                and_then(
                    and_then(
                        sub_cps(n, 2),
                        cls!([]<R>|n2: u64| -> Box<dyn Cont<u64, R>> { fib_cps(n2) })
                    ),
                    cls!([f1: u64]<R>|f2: u64| -> Box<dyn Cont<u64, R>> {
                        add_cps(f1, f2)
                    }))
            }),
        )
    }
}
```

やった！できた！
あとは適当に初期継続 `Id` を用意してあげれば呼び出せます。

``` rust
fn main() {
    struct Id;
    impl<T> FnBox<T> for Id {
        type Out = T;
        fn call(self: Box<Self>, t: T) -> Self::Out {
            t
        }
    }
    let ret = fib_cps::<u64>(10).cont(Box::new(Id));
    println!("fib(10) = {}", ret);
}
```


Rust 1.34.2でコンパイルしてみましょう。

``` text
$ rustc +1.34.2 cps_fib.rs
$ ./cps_fib
fib(10) = 89
```


できました！！いつの時代でも欲求は満たされることが分かりました。

# まとめ

Rust 1.35.0より前の世界では苦労して得ていたCPS形式のプログラムも1.35.0からは簡単に得られるようになりました。
時代は進むし世の中便利になっていくのです。
