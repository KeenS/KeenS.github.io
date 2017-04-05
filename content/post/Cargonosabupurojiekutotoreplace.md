---
categories: [Rust, Cargo]
date: 2017-04-05T20:46:26+09:00
title: Cargoのサブプロジェクトとreplace
---

κeenです。最近Cargoのreplace機能を使おうとしてハマったのでメモを残しておきます。

<!--more-->
最近何箇所かで喋ったのでご存知の方もいらっしゃるかと思いますが、Cargoにはワークスペース機能があります。
fat repositoryスタイルに便利な機能で、1つのディレクトリ下に複数のサブプロジェクトを配置しつつ`target/`は共有するのでマイクロプロジェクトのモジュール性とモノリシックプロジェクトのコンパイルの速さを実現することが出来ます。

さて、問題になるのはクレートを公開するときの依存関係の記述です。
サブプロジェクト同士で依存し合っているので`Cargo.toml`には`my-project-lib = {path = "../my-project-lib"}` のような依存の記述がある筈です。
しかしながらローカルのクレートに依存していると`crates.io`に公開は出来ません。
そこで今回はそれをどうにかするお話。

# replace

Cargoには[replace](http://doc.crates.io/manifest.html#the-replace-section)という機能があります。
特定のライブラリの特定のバージョンを、crates.ioにあるものではなくてユーザが指定した場所にあるものを使うように指示します。
[Specifying Dependencies](http://doc.crates.io/specifying-dependencies.html#overriding-dependencies)のドキュメントによると、以下のように使えます。

例えば`uuid`クレートを使っているときにそれが依存している`rand`クレートにバグを見付けて修正し、修正した結果を試したいとします。
その時にreplaceは以下のように使えます。

``` toml
# ワークスペースの親プロジェクト
...

[replace]
"rand:0.3.14" = { path = "./rand" }

```

``` toml
# ワークスペースの子プロジェクト

[package]
name = "my-awesome-crate"
version = "0.2.0"
authors = ["The Rust Project Developers"]

[dependencies]
uuid = "0.2"
```


間接的に依存してる`rand:0.3.14`を、crates.ioにあるものではなく指定したパスにあるものを使うように指示しています。

この仕組みを使って先のローカルパス問題を解決出来ないでしょうか。すなわち、

``` toml
# 子プロジェクト
[dependencies]
my-project-lib = "0.1.0"

```

のように、見た目上はcrates.ioにあるものに依存しておきつつ実際は

``` toml
# 親プロジェクト
[replace]
"my-project-lib:0.1.0" = { path = "./my-project-lib" }
```

のように実際の依存先をローカルのパスに向ければ問題を解決できないでしょうか。

この答えは、半分no、半分yesになります。


`replace`はあくまでcrates.ioにあるものを置き換えるのでまだ公開していないプロジェクトに対しては使えません。
一方、一度crates.ioに公開してしまえば上記のテクニックは可能になります。

なので一番最初にcrates.ioに公開する時の手順はやや煩雑になります。

1. (ローカルサブプロジェクトへの依存の記述は`path = "..."`のまま)
2. どのプロジェクトにも依存しないローカルサブプロジェクトを公開する
3. 既に公開されたローカルサブプロジェクトへの依存は順次`replace`に書き換える
4. ローカルパスへの依存のなくなったローカルプロジェクトから順に公開していく
5. 全てのサブプロジェクトのローカルサブプロジェクトへの依存が`replace`に置き換わったら以後はそのまま使える。

このようなテクニックは例えばdieselなどで[使われています](https://github.com/diesel-rs/diesel/blob/master/Cargo.toml#L21)。
この辺、改善あるといいですね。
