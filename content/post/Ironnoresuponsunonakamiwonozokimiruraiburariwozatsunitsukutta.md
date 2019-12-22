---
categories: [Rust, Advent Calendar, Advent Calendar 2017, Iron, 小ネタ, Web, Rust Advent Calendar]
date: 2017-12-02T17:23:53+09:00
title: "Ironのレスポンスの中身を覗き見るライブラリを雑に作った"
---
κeenです。[Rustその2 Advent Calendar 2017](https://qiita.com/advent-calendar/2017/rust-lang-2)2日目の記事です。
タイトルのままです。
<!--more-->

作ったライブラリは[iron_inspect](https://github.com/KeenS/iron_inspect)で、[crates.io](https://crates.io)に[あります](https://crates.io/crates/iron_inspect)。

モチベーションとしては[`logger`](https://crates.io/crates/logger)による通常のアクセスログの他にInternal Server Errorも別途ログ吐きたいよねというもの。
良さげなライブラリがなかったので雑に作りました。
Ironミドルウェアなので `Chain` に `link_after` してあげればすぐ使えます。

`Result` を取るかその`Ok`のみをとるか`Err`を取るか選べます。

`Resultを取るのは` READMEにある使い方だと

``` rust
let mut chain = Chain::new(handler);
chain.link_after(Inspect::new(|_req, res| match res {
    Ok(r) => println!("ok: {:?}", r),
    Err(e) => println!("err: {:?}", e),
}));
```

な感じですね。

欲しかったのはこんな感じに `Err` だけ受けて雑にログ取る仕組みです。

``` rust
    ch.link_after(Inspect::error(|_, err| {
        error!("internal errror occurred because of {}", err);
    }));
```

以上
