---
categories: [Rust, Advent Calendar, Advent Calendar 2021, Rust Advent Calendar]
date: 2021-12-20T02:53:43+09:00
title: "moldを使うとRustのビルドが速くなる"
---

このエントリは[Rust Advent Calendar](https://qiita.com/advent-calendar/2021/rust)の2日目の記事です。

空いてる日を埋める担当のκeenです。2日目が空いてたので遡って記事を投稿します。

最近v1.0.0がリリースされたリンカ、[mold](https://github.com/rui314/mold)を使うとビルドが高速化するよというお話です。

<!--more-->

Rustのビルド、特にインクリメンタルビルドにおいてはRustコンパイラの速さと同じくらいリンカの速度がコンパイル時間に影響します。
この最後のバイナリを作る時間は意外と効いてきます。

具体例として[Actix Web](https://github.com/actix/actix-web)にあるexample、 `basic` をインクリメンタルビルドする例をみてみましょう。

この `basic` はたった42行の小さなアプリケーションです。

```shell
$ wc -l basic.rs
42 basic.rs
```

ただし、依存に Actix Webという巨大なライブラリを使っているので最終的なバイナリには非常に多くのコードが含まれます。フレームワークを使ってアプリケーションを開発してるときに似た状況な訳です。多くの人にとっては実感のある例でしょう。これを `mold` ありなしでビルドしたときのコンパイル時間の差をみてみます。

まずはビルドキャッシュを有効にするために `cargo build` を走らせておきます。

```shell
$ cd actix-web
$ cargo build --example basic
```

そのあとインクリメンタルビルドの速度を `time` コマンドで測ってみます。一度 `examples/basics.rs` に `touch` すれば再度ビルドが走ってくれます。

```shell
$ touch examples/basic.rs && time cargo build --example basic
   Compiling actix-web v4.0.0-beta.15 (/home/shun/Rust/actix-web)
    Finished dev [unoptimized] target(s) in 2.48s
cargo build --example basic  1.93s user 3.88s system 228% cpu 2.545 total
```

すると約2.5秒かかります。何も変更がないはずなのにけっこう長いですね。Actix Webで開発するコードにおいては `cargo run` や `cargo test` する度にこのリードタイムが税金のように乗ってくる訳です。できる限り減らしたいですよね。

次に `mold` を使ってビルドしてみましょう。 `mold` はGitHubから `clone` してきて依存を揃えたら `make && sudo make install` でインストールできます。公式の案内に従って下さい。

`mold` をRustのプロジェクトで使うのは簡単です。 `mold -run cargo ...` のように `cargo` の前に `mold -run` を置くだけで使えます。

```shell
$ mold -run cargo build --example basic
```

これでさきほどの `basic` を再度ビルドしてみましょう。

```shell
$ touch examples/basic.rs && time mold -run cargo build --example basic
   Compiling actix-web v4.0.0-beta.15 (/home/shun/Rust/actix-web)
    Finished dev [unoptimized] target(s) in 0.78s
mold -run cargo build --example basic  0.59s user 1.06s system 195% cpu 0.848 total
```

約0.85秒とかなり高速化しました。2.545 / 0.848 ≒ 3 なのでおおむね3倍高速化しています。
この数値はプロジェクトの大きさやマシンスペックなどに応じて変わるので素直に3倍とは言い切れませんが、 `mold` を使ってインクリメンタルビルドすると少なくない高速化が見込めるのは事実でしょう。

`cargo test` のように小さな修正をしては実行してみるようなユースケースを考えると `mold` によるコンパイルの高速化、手返しの改善は大きな恩恵があります。
導入も手軽にできますしお試しで手元で開発するときのテスト用途などで使ってみてはいかがでしょうか。
