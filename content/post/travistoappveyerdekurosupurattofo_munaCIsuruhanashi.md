---
categories: [Rust, Advent Calendar, Advent Calendar 2015, Rust Advent Calendar]
date: 2015-12-20T22:19:21+09:00
title: travisとappveyorでクロスプラットフォームなCIする話
---
このエントリは[Rust Advent Calendar 2015](http://qiita.com/advent-calendar/2015/rust-lang)20日目の記事です。
日付を覚え間違っていて、締切ギリギリに書いてます。

κeenです。ものすごい小ネタですがRustでCIする話でも。
<!--more-->

Rustはクロスプラットフォームな言語なのでLinux, Macそしてあまり聞き慣れませんがWindowsというOSでも動きます。
とは言っても普段の開発環境がLinux、運悪くMacを使わざるを得ない人はMacで開発をしているとそれらでの動作検証は出来るでしょうが他のOSでの動作検証が難しいでしょう。

そこでCIしましょう。Travis CIはLinuxと、あまり使われていませんがMacでのCI環境を、AppveyorでWindowsというOSでのCI環境を作ります。

しかしクロスプラットフォームでしかもテスト方法がCargoで統一されているRustではそこまで難しいこともなく、以下の設定をコピペするだけです。

travis

``` yaml
language: rust
rust:
  - nightly
  - beta
  - stable
os:
  - linux
  - osx
```

appveyor

```yaml
branches:
  except:
    - gh-pages

platform:
  - x64
environment:
  RUST_INSTALL_DIR: C:\Rust
  matrix:
    - RUST_INSTALL_TRIPLE: i686-pc-windows-msvc
      RUST_VERSION: 1.4.0
    - RUST_INSTALL_TRIPLE: i686-pc-windows-msvc
      RUST_VERSION: beta
    - RUST_INSTALL_TRIPLE: i686-pc-windows-msvc
      RUST_VERSION: nightly
    - RUST_INSTALL_TRIPLE: x86_64-pc-windows-msvc
      RUST_VERSION: 1.4.0
    - RUST_INSTALL_TRIPLE: x86_64-pc-windows-msvc
      RUST_VERSION: beta
    - RUST_INSTALL_TRIPLE: x86_64-pc-windows-msvc
      RUST_VERSION: nightly

install:
  - ps: Start-FileDownload "https://static.rust-lang.org/dist/rust-${env:RUST_VERSION}-${env:RUST_INSTALL_TRIPLE}.exe"
  - cmd: rust-%RUST_VERSION%-%RUST_INSTALL_TRIPLE%.exe /VERYSILENT /NORESTART /DIR="%RUST_INSTALL_DIR%"
  - cmd: SET PATH=%PATH%;%RUST_INSTALL_DIR%\bin
  - rustc --version
  - cargo --version

build: false
test_script:
  - cargo build --verbose
  - cargo test --verbose
```


バッジとかは適当に持ってきましょう。

サンプルが欲しければ[Stebalien/tempfile](https://github.com/Stebalien/tempfile)あたりを参考にしましょう。


それではHave Happy Hacκings!


※OS関連の煽りはただのネタです。また「あまり使われていませんがMacでのCI環境を」は「(travisの中では)あまり使われていません(のでもしかしたら使えることを知らない人がいるかもしれません)が」という意味です。
