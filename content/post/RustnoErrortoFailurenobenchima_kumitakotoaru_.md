---
categories: ["Rust"]
date: 2019-07-20T18:15:56+09:00
title: "RustのErrorとFailureのベンチマーク見たことある？"
---
κeenです。最近ちょくちょく標準ライブラリの `Error` とfailureライブラリの `Fail` が比べられる（というより `Fail` を推奨する）のが増えてきたようです。
個人的にちょっと気になってる点があって、みんなそれを抜きに議論してるようなので少しお話します。

<!--more-->

タイトルにあるとおり、ベンチマークの話なのでいきなりですがベンチマークを使いましょう。
`Box<dyn Error>` を作るコストと failureライブラリの `Error` を作るコストを比べます。
何故 `Fail` ではなくfailureの `Error` かというと `Fail` は少し込み入ったベストプラクティスがあり、コードが長くなるからです。
ベンチマーク的にはベストプラクティスを使った `Fail` とfailureの `Error` は変わらないと思います。ベストプラクティスについては以下を参照して下さい。

[rust のエラーライブラリは failure を使え！ - Qiita](https://qiita.com/legokichi/items/d76b6aa5dac2ad781bda)

さて、ベンチマークには[bencher](https://docs.rs/bencher/0.1.5/bencher/)を使いましょう。

Cargo.toml
```toml
[package]
# ....



[[bench]]
name = "bench"
harness = false

[dependencies]
failure = "0.1.5"

[dev-dependencies]
bencher = "0.1.5"

```


benches/bench.rs

``` rust
use failure::Error as FailureError;
use std::error::Error as StdError;
use std::fmt;

// エラー型を準備
#[derive(Debug)]
struct MyError;

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "My Error")
    }
}

impl StdError for MyError {}

// ベンチマークコード
use bencher::{benchmark_group, benchmark_main, Bencher};

fn bench_error(b: &mut Bencher) {
    b.iter(|| -> Box<dyn StdError> { Box::new(MyError) })
}

fn bench_fail(b: &mut Bencher) {
    b.iter(|| -> FailureError { MyError.into() })
}

// セットアップ
benchmark_group!(benches, bench_error, bench_fail);
benchmark_main!(benches);

```


シンプルに `Box<dyn Error>` と `Error` を作ります。
bencherは対象の実行速度を見てイテレーション回数を勝手に増やしてくれるので「何回実行したら有意な数値が取れるかな」とか考えなくてもよいのが利点です。

これをベンチマークしてみます。

``` console
$ cargo bench
running 2 tests
test bench_error ... bench:           1 ns/iter (+/- 0)
test bench_fail  ... bench:          18 ns/iter (+/- 1)

test result: ok. 0 passed; 0 failed; 0 ignored; 2 measured
```

`Box<dyn Error>` が1ns（ナノ秒）、failureの `Error` が 18ns。
failureライブラリの方が18倍遅いという結果です。
単位が `ns` なので誤差ではと思うかもしれませんが右の括弧書きで書かれている測定誤差をみてもせいぜい1nsくらいです。

これ、どうしてか分かる方いますか？

ヒントというかほぼ答えですが追加のベンチマークを取ってみましょう。コールスタックの深いところでエラー値を作ってみます。

``` rust
// スタックを深くする関数
fn deep_stack_call<T>(n: u32, f: impl FnOnce() -> T) -> T {
    if n == 0 {
        f()
    } else {
        deep_stack_call(n - 1, f)
    }
}

//コールスタックを100掘ってからエラー値を作る
fn bench_error_stack(b: &mut Bencher) {
    b.iter(|| -> Box<dyn StdError> { deep_stack_call(100, || Box::new(MyError)) })
}

fn bench_fail_stack(b: &mut Bencher) {
    b.iter(|| -> FailureError { deep_stack_call(100, || MyError.into()) })
}

benchmark_group!(deep_stack_bench, bench_error_stack, bench_fail_stack);
benchmark_main!(deep_stack_bench);
```

これを測ります。

``` console
$ cargo bench
running 2 tests
test bench_error_stack ... bench:           1 ns/iter (+/- 0)
test bench_fail_stack  ... bench:         131 ns/iter (+/- 12)

test result: ok. 0 passed; 0 failed; 0 ignored; 2 measured
```

`Box<dyn Error>` が変わらず1ns（ナノ秒）、failureの `Error` が 131ns。failureの `Error` は7倍以上コストが上がってます。

このあたりでもうお分かりでしょうか。プロファイルまで取った訳ではないので多少推測ですが、failureの `Error` はバックトレースを取るのでその分のコストが乗るのです。

実際、`impl<F: Fail> From<F> for failure::Error` の実装を辿っていくとこういうコードにいきつきます(failure 0.1.5)。

``` rust
impl<F: Fail> From<F> for ErrorImpl {
    fn from(failure: F) -> ErrorImpl {
        let inner: Inner<F> = {
            let backtrace = if failure.backtrace().is_none() {
                Backtrace::new()
            } else { Backtrace::none() };
            Inner { failure, backtrace }
        };
        ErrorImpl { inner: Box::new(inner) }
    }
}
```


ここまでが測定結果のお話。ここから個人的な意見です。

Rustのコードはエラー値を作るのが軽いのを前提に作られているものがちょくちょくあります。
例えばバリデーションの結果を `Result` 型で返すだとか。これは例外ではなく値でエラーを表わすことの利点の1つだと思ってます。
そういう所でもfailureを使ってると意図しないパフォーマンスの劣化を招きかねません。
必要なところで使う分には全く問題ないのですが無闇矢鱈に使うのはいただけないなと思っています。

もちろん、コストが掛かるといってもせいぜい100ns程度ですしスタックトレースが取れるメリットに比べたら大した問題ではないでしょう。
臆せず使って下さい。

余談:

* 今回のコードはGitLabに[あります](https://gitlab.com/blackenedgold/error-bench)
* `bench_error_stack` が `bench_error` とパフォーマンス変わらないのは末尾呼び出しの最適化が効いたか、インライン化されたんじゃないかと思いますが定かではないです。
* `bench_fail_stack` と `bench_fail` のパフォーマンスが大きく異なるのはスタックトレースを取得している部分があるのでその手の最適化が効かなかったんじゃないかと思いますが定かではないです。
* 非常に個人的な話ですがバックトレースはそんなに好きじゃないです。どうせ見てもフレームワークのトレースが多かったりクロージャがあると問題の箇所とずれたりしてあんまり役に立たないので。その割にターミナルの画面を吹き飛ばして迷惑ですし。
