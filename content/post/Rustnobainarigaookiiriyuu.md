---
categories: [Rust]
date: 2017-12-18T23:21:32+09:00
title: "Rustのバイナリが大きい理由"
---
κeenです。方々で言われる話ですがRustコンパイラの吐くバイナリはそこそこ大きいです。
この理由を自分で納得してなかったので追います。
<!--more-->

様々な理由からnightlyを使います。

# 初期

```console
$ cat hello.rs
fn main() {
    println!("Hello, World");
}
$ rustc +nightly hello.rs
$ ls -l hello
-rwxr-xr-x 1 kim kim 5049344 12月 18 23:30 allocator
```

5MBくらいあります。等価なCのコード(gccでオプションなし)が8.2Kだったのでかなり大きいですね

# 最適化
`cargo --release`と同じく`-Copt-level=3`を付けましょう。`-Copt-level=s`の方が小さくなりますが普段やらないので。

```
$ rustc +nightly -Copt-level=3  hello.rs
$ $ ls -l hello
-rwxr-xr-x 1 kim kim 5049200 12月 18 23:27 hello

```

ほんの少しだけ小さくなりました。

# デバッグシンボル
`strip`してませんね。デバッグシンボルを落とします。

```
$ rustc +nightly -Copt-level=3  hello.rs
$ strip hello
$ ls -l hello
-rwxr-xr-x 1 kim kim 461768 12月 18 23:35 hello
```

460KBくらいに減りました。
大体ここまでやったのがスタートラインですかね。
こっからもうちょいけずっていきます。

# LTO
コンパイル時だけでなくリンク時にも最適化をします。

```
$ rustc +nightly -Copt-level=3 -Clto  hello.rs
$ strip hello
$ ls -l hello
-rwxr-xr-x 1 kim kim 453544 12月 18 23:37 hello
```

数KB減りました

# アロケータ
Rustはlibcの`malloc`でなく`jemalloc`を使っています。そこが効いてるかもしれません。

```rust
#![feature(alloc_system)]
extern crate alloc_system;

fn main() {
    println!("Hello, World");
}
```


このように書き換えて

```
$ rustc +nightly -Copt-level=3 -Clto  hello.rs
$ strip hello
$ ls -l hello
-rwxr-xr-x 1 kim kim 453544 12月 18 23:38 hello
```

あれ！？減ってない
`strings hello | grep jemalloc` でまだjemallocのシンボルが残ってるようですし何か失敗してるのかもしれません。

原因がわからないので後回し。

# panicをやめる
Rustはpanicしたときのために色々してます。panicしたら即座にabortするようにしてみましょう。

```
$ rustc +nightly -Copt-level=3 -Clto -Cpanic=abort hello.rs
$ strip hello
$ ls -l hello
-rwxr-xr-x 1 kim kim 433064 12月 18 23:41 hello
```

20KBくらい痩せました。

# staticリンク
staticします

```
$ rustc +nightly -Copt-level=3 -Clto -Clink-args=-static -Cpanic=abort hello.rs
$ strip hello
$ ls -l hello
-rwxr-xr-x 1 kim kim 412504 12月 18 23:46 hello
```

もうちょい痩せました。


# `opt-level=s`

ここまできたら最初は使わないと言っていた`opt-level=s`も試してみましょう

```
$ rustc +nightly -Copt-level=s -Clto -Clink-args=-static -Cpanic=abort hello.rs
$ strip hello
$ ls -l hello
-rwxr-xr-x 1 kim kim 408328 12月 18 23:49 hello

```

もう数KBばかり。

# 終わりに
Rustのバイナリサイズが(デバッグシンボルを除いても)大きいのは`jemalloc`のせいと思ってたんですがシステムアロケータを使っても改善しませんでしたね。

因みに並行サポートを意識したRustは標準出力に吐くだけでも標準出力のロックを取って、と複雑なコードになっているので単純に見えるコードでもオーバーヘッドがありますね。

システムアロケータの件は納得いってないので追求したい。

# 参考
[Why is a Rust executable large?](https://lifthrasiir.github.io/rustlog/why-is-a-rust-executable-large.html)

こちらではバイナリサイズが小さい上にjemallocをやめるとバイナリサイズが1/3くらいになってるのでやっぱり自分のやつは何かまちがってそう

# 2017-12-19 追記: システムアロケータの本気
どうやら古いドキュメントを参考にしていたようで、ちゃんと書いたらできました。

```rust
#![feature(alloc_system, allocator_api, global_allocator)]

extern crate alloc_system;


use alloc_system::System;

#[global_allocator]
static A: System = System;


fn main() {

    println!("Hello, World");

}
```

```
$ rustc +nightly -Copt-level=z -Clto -Clink-args=-static -Cpanic=abort hello.rs
$ strip hello
$ ls -l hello
-rwxr-xr-x 1 kim kim 145816 12月 19 10:06 hello
```

143KiB。Cと比べるとまだまだ大きいですが十分小さくなりました。

因みに「よくやる」コンパイルオプションだとこうなります。

```
$ rustc +nightly -Copt-level=3 hello.rs
$ ls -l hello
-rwxr-xr-x 1 kim kim 2587696 12月 19 10:10 hello
$ strip hello
$ ls -l hello
-rwxr-xr-x 1 kim kim 186960 12月 19 10:10 hello
```

元の`strip`前の5MBや`strip`後の460KBに比べて半分(以下)です。
`jemalloc`で最低限のバイナリへのオーバーヘッドの半分を占めていたことになります。

`jemalloc`は`libc`の`malloc`に比べて速かったりRustに都合の良い(消費メモリが少なくて済む)アロケーションをしてくれたりするので使われているのですがバイナリサイズを気にするならシステムアロケータを使うのも手でしょう。

`jemalloc`と`libc`の`malloc`のベンチマークは読者の課題とする。
