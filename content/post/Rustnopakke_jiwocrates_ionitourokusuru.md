---
categories: [Rust]
date: 2016-01-31T14:43:14+09:00
title: Rustのパッケージをcrates.ioに登録する
---

κeenです。技術の話題を書くのは久し振りですね。今日初めてRustのパッケージのセントラルレポジトリ、[crates.io](crates.io)にパッケージを登録したのでその流れを共有します。

<!--more-->
# 登録したいパッケージを用意する
[file_logge](https://github.com/KeenS/file_logger)を作りました。
しばらく前に(Advent Calendar向けに)それっぽく動くところまで作って放置してたやつですね。これを整形していきます。

# (任意)ドキュメントを生成する
Rustには素晴しき `cargo doc` があります。

```
$ cargo doc --no-deps
```

でドキュメントを生成しましょう。ドキュメントを書いてないなら[こちら](http://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/documentation.html)を見ながら書きましょう（すいません、私の奴は全然書いてないです…）。
`target/doc` 以下に生成されたドキュメントがあるので適当に `gh-pages` ブランチに放り込みましょう。
尚、`--no-deps` オプションがないと依存パッケージのドキュメントも全部生成してしまいます。

求: gh-pages管理のベストプラクティス。

# パッケージ情報を充実させる

普段使っているままの `Cargo.toml` だとcrates.ioで見た時に情報が全然ありません。
`[package]` セクションに色々書きましょう。私の場合は

``` toml
[package]
name = "file_logger"
version = "0.1.0"
authors = ["Sunrin SHIMURA (keen) <3han5chou7@gmail.com>"]
```

から

``` toml
[package]
name = "file_logger"
version = "0.1.0"
authors = ["Sunrin SHIMURA (keen) <3han5chou7@gmail.com>"]
repository = "https://github.com/KeenS/file_logger"
keywords = ["log", "file"]
license-file = "LICENSE"
readme = "README.md"
description = "A simple logger backend that outputs to a file. \nThis is alpha state."
documentation = http://KeenS.github.io/file_logger/file_logger/
```

になりました。
あ、ちゃんとライセンスも明示しましょうね。

どういう情報が書けるのかは[ここ](http://doc.crates.io/manifest.html)に載ってます。


# crates.ioにログインする

[crates.io]()に行き、右上にあるGitHubログインボタンをクリックします。

![github login](/images/crates.io/login.png)

そうするとログイン状態になります

![github logged in](/images/crates.io/loggedin.png)

メニューから

![menu](/images/crates.io/menu.png)

Account Settingsに飛ぶと下記のようにログイン用cargoコマンドが提示されるのでそのままペタっとコマンドラインに貼って終了です。

![account settings](/images/crates.io/account_settings.png)

# パッケージング

```
$ cargo package
```


# crates.ioに登録する


```
$ cargo publish
```


# その他

パッケージに含めるファイルを細かく調整したい、既にpublishしたパッケージをdeprecated的な扱いにしたいなどは[こちら](http://doc.crates.io/crates-io.html)にドキュメントが載っています。

# まとめ
* crates.ioにログインしたよ
* Cargoを使えば簡単にパッケージを登録出来るよ
* Cargoってすごいね
