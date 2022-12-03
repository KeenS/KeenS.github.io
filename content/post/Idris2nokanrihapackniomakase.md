---
categories: [Idris, Idris2, Advent Calendar, Idris Advent Carendar, Advent Calendar 2022, Idris Advent Carendar 2022]
date: 2022-12-03T17:50:37+09:00
title: "Idris2の管理はpackにお任せ"
---
このエントリは[Idris/Idris2 Advent Calendar 2022](https://qiita.com/advent-calendar/2022/idris)の1日目の記事として過去に遡って投稿しています。

κeenです。ぼちぼちIdris2を触りはじめたので小出しに色々なことを書いていきます。今回はIdris2のインストーラ兼パッケージマネージャ兼ビルドツールのpackについて

<!--more-->
[pack](https://github.com/stefan-hoeck/idris2-pack)はIdris2のインストーラ兼パッケージマネージャ兼ビルドツールです。これ1つでほとんどのことが済む便利ツールです。名前がなんか汎用的すぎて他と被りそうで心配ですね。

## インストール

ちゃんとした情報は最新のインストールガイドを参照して欲しいのですが、以下のコマンド1発でインストールできます。

```shell
bash -c "$(curl -fsSL https://raw.githubusercontent.com/stefan-hoeck/idris2-pack/main/install.bash)"
```

これだけでpackのインストールとIdris2の処理系のインストールまで済みます。ただし、事前にChezSchemeのインストールが必要です。Ubuntuとかだと `apt install chezscheme` とかで入るのでサクっと入れておきます。

インストールは処理系のビルドも含むのでちょっと時間がかります。コーヒーを用意してのんびり待ちましょう。

## 便利な使い方
### パッケージ管理

手軽にライブラリをインストールできます

```shell
pack install hogehoge
```

バイナリも同様です。

```shell
pack install-app fugafuga
```

### プロジェクト管理

`pack new` で新しくプロジェクトを作れます。

```shell
pack new lib piyo
```

以下のようにデフォルトでビルドできるファイルを置いてくれるので手早くはじめられますね。

```shell-session
$ pack new bin HelloWorld
$ cd HelloWorld
$ cat src/Main.idr
module Main

main : IO ()
main = putStrLn "Hello from Idris2!"
```

因みにtreeは以下のようになっています。

```text
.
├── HelloWorld.ipkg
├── pack.toml
├── src
│   └── Main.idr
└── test
    ├── src
    │   └── Main.idr
    └── test.ipk
```

ipkgを指定してビルドをしてくれます。idris2のコマンド体系が長いので地味に有り難いですね。

```shell
pack build json.ipkg
```

型検査もできます。

```shell
pack typecheck elab-util.ipkg
```

### 実行

普通に実行できます。

```shell
pack run test.ipkg -n 50
```

また、REPLも走らせられます。

```shell
pack repl
```

`-P` でパッケージをリンクしたり `--rlwrap` でreadline wrapperを噛ませたりできるので使い勝手はかなりいいんじゃないでしょうか。

```shell-session
$ pack -P contrib --rlwrap repl
     ____    __     _         ___
    /  _/___/ /____(_)____   |__ \
    / // __  / ___/ / ___/   __/ /     Version 0.6.0-59aadd650
  _/ // /_/ / /  / (__  )   / __/      https://www.idris-lang.org
 /___/\__,_/_/  /_/____/   /____/      Type :? for help

Welcome to Idris 2.  Enjoy yourself!
Main> :module Language.JSON
Imported module Language.JSON
Main> parse "{\"hoge\": true}
```

## パッケージソース

パッケージ管理をしてくれるツールをみるとどこにサーバがあるか気になりますが、packはGitHubを使って手動管理しているようです。emacsのelpaとかと同じ仕組みですね。

[idris2-pack-db](https://github.com/stefan-hoeck/idris2-pack-db)

特にバージョン管理などはせず、指定したブランチの最新版でビルドしています。コミュニティが小さいうちはこれで十分でしょう。

## まとめ

Idris2の開発を便利にしてくれるpackの紹介でした。以後の私のIdris2の記事はpackがあることを前提に書いていこうかなと思ってます。

