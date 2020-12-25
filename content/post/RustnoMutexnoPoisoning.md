---
categories: [Rust, Rust Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-25T23:08:50+09:00
title: "RustのMutexのPoisoning"
---

このエントリは[Rust 3 Advent Calendar 2020](https://qiita.com/advent-calendar/2020/rust3)の16日目の記事です。
前はbantechさんで[actix-webでformいろいろ](https://banatech.net/blog/view/46)、後はkaz399さんで[2020年 Rust で使える Bluetooth Low Energy ライブラリはどうなってるの (Windows10) - Qiita](https://qiita.com/kaz399/items/efee902888869efec8e3)でした。

空いている日を埋める担当のκeenです。
最近RustのMutexのPoisoningについて動向があったので紹介します。

<!--more-->

[`Mutex::lock`](https://doc.rust-lang.org/std/sync/struct.Mutex.html#method.lock) は以下の型をしています。

```rust
pub fn lock(&self) -> LockResult<MutexGuard<'_, T>>
```

ここで `LockResult` はこう定義されています。

```rust
type LockResult<Guard> = Result<Guard, PoisonError<Guard>>;
```

なにやら `PoisonError` というのが返っていますね。
[ドキュメント](https://doc.rust-lang.org/std/sync/struct.PoisonError.html)を読んでみましょう。

ロックというのは他のスレッドに割り込まれたくない一連の操作（[クリティカルセクション](https://ja.wikipedia.org/wiki/クリティカルセクション)）を保護するのに使いますよね。つまり操作の途中の状態を見られたくない訳です。
ではその操作の途中でスレッドがパニックしたらどうなるでしょう。
見られたくない中途半端な状態で処理が止まってしまいます。
そういう場合にロックを開放するのもロックを掴みっぱなしにするのもどうなの、ということで標準ライブラリの `Mutex` は「このロックで保護されてる値は中途半端な状態かもよ」というマークをつけます。それがPoisoningです。

以下のコードを走らせるとPoisoningが確認できます。

```rust
use std::sync::{Arc, Mutex};
use std::thread;

let mutex = Arc::new(Mutex::new(1));

// poison the mutex
let c_mutex = Arc::clone(&mutex);
let _ = thread::spawn(move || {
    let mut data = c_mutex.lock().unwrap();
    *data = 2;
    panic!();
}).join();

match mutex.lock() {
    Ok(_) => unreachable!(),
    Err(p_err) => {
        let data = p_err.get_ref();
        println!("recovered: {}", data);
    }
};
```

ロックを取ったスレッドがパニックして、ロックが汚染された状態になります。
それを検知したメインッスレッドでは `println!("recovered: {}", data)` と、エラーから回復します。

```text
thread '<unnamed>' panicked at 'explicit panic', poison.rs:12:9
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
recovered: 2
```

# Poisoningの問題点

2点問題点が指摘されています。

## ゼロオーバーヘッド原理

Poisoning自体はすごく有用なテクニックです。ですが、それを標準ライブラリでやる必要ある？というツッコミです。
ロックを掴んだままスレッドがパニックしたときに「中途半端な状態」になるケースもあればならないケースもあります。
これはどういうコードを書いたか次第です。

中途半端にならないケースではPoisoningの処理は無駄になる訳です。
C++の標語ですが、「ゼロオーバヘッド原理」というのがあります。
「使ってないもののコストを支払わなくていい」と説明されるように、あらゆる処理は必要な人にしか行なわないようにしようという目標です。
RustもC++と同じくシステムプログラミング言語ですから必要のないものはできればやらない方がいいですよね。
その観点からするとPoisoningは余計な処理になってしまう訳です。

ここで「Rustは安全な言語だから仕方がない」という意見があります。
しかしRustの保証する安全性はプログラムのロジックにバグがないことではなく、メモリ破壊などの未定義動作が起きないということです。
Poisoningで防げるバグはプログラムのロジックのバグなので、テリトリーが違うのではないかと言われています。

## エラーとパニック問題

プログラムのエラーには2種類あります。
1つは予期されたエラー。これはユーザの入力が期待と違うなどの、プログラムを書いている時点で想定されたエラーです。もう1つは予期しないエラー。これは言ってしまえばプログラムのバグです。
Rustでは前者を `Result<T, E>` で、 後者をパニックで表現します。

ここでPoisoningを振り返ってみましょう。
ロックが汚染された状態はスレッドがパニックした場合に起きます。つまりプログラムバグがあった場合ですね。
ですが、 `Mutex::lock` が返すのは `Result` です。想定内のエラーということになっています。
このパニック → `Result` の変換はよくないんじゃないのというツッコミです。


# 今後の話

問題があるとして、そのあとに取れるアクションの可能性は複数あります。
既にあるAPIだから変えない、Poisoningをやめる、Poisonしてたら `Result` を返すのではなくパニックする、ロックとPoisoningをそれぞれ別のAPIで提供するなどなど。

Rustの公式でどうしようというサーベイが走っています。

* [Launching the Lock Poisoning Survey | Rust Blog](https://blog.rust-lang.org/2020/12/11/lock-poisoning-survey.html)
* [Poisoning Survey](https://docs.google.com/forms/d/e/1FAIpQLSehk-GkwoCag_w3YfXDfgeANulR0h5m2d3EzUMQaiY1vRfIEw/viewform)

期限がいつまでなのかわかりませんが、今のところまだアンケートは回答可能なようなので興味を持った方は回答してみて下さい。

本件についてrust-analyzerの中の人のmatklad氏が大変示唆に富むブログを書いていたので併せて紹介しておきます。

[Notes On Lock Poisoning](https://matklad.github.io/2020/12/12/notes-on-lock-poisoning.html)
