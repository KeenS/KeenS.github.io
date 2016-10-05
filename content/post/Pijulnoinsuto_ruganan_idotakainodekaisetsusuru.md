---
categories: [Pijul]
date: 2016-09-29T22:15:51+09:00
title: Pijulのインストールが難易度高いので解説する
---

κeenです。ふとTwitterで[Pijul](http://pijul.org)の話題を見掛けたついでにインストールしようと思ったら難易度が高かったので解説します。
Pijulそのものについては[過去のエントリ](/blog/2016/02/14/dvcsnomoderu_aruihapijulnitsuite/)を参照して下さい。

因みにPijulは「ピーフール」みたいに発音するそうです。「フー」は喉の奥から出す破擦音ですね。

<!--more-->
まず、公式ドキュメントには `cargo install pijul` すると書いてますが、これはバージョン0.1、古いやつな上にビルドに失敗します。
ということでソースからビルドするのですがビルドガイドがないので非常に難しいです。コマンドだけ載せてしまうと

```
$ darcs get https://pijul.org
$ cd pijul.org/pijul
$ cp -R ../libpijul src
$ cd src/libpijul/src
$ darcs get https://pijul.org/sanakirja
$ cd ../../../
$ cargo install
```

です。これだと常に最新版をビルドしてしまいますがdarcsでタグを指定する方法が分からなかったのでこれで。
あ、あと`darcs get https://pijul.org/sanakirja`でnested repositoryで怒られますがそれもよく分からなかったのでそのまま。
darcsに馴れてる方は好きにして下さい。

因みにその後のコマンドはちゃんと動くので[公式ガイド](http://pijul.org/documentation/getting-started/)に従えば使えます。
