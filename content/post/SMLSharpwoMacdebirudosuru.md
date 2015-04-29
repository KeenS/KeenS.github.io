---
categories: [SML, SMLSharp]
date: 2015-04-29T16:09:08+09:00
title: SML#をMacでビルドする
---
κeenです。SML#2.0.0を文鎮と化していたMBAにインストールしたのでメモをば。

基本は[よんたさんの記事](http://d.hatena.ne.jp/keita44_f4/20140412/1397279451)をMacに翻訳した感じです。
<!--more-->
モチベーションは、[公式の配布物](http://www.pllab.riec.tohoku.ac.jp/smlsharp/ja/?Download)がMac版だとMacPorts版しかなく、
portsとhomebrewの混在は避けた方が良いと聞いたのでどうにかして自前ビルドしようとしたことです。

# 注意書き
冒頭にも書いてあるようにMBAは普段使ってなくて、このエントリーもMBAじゃないマシンから書いているのでコマンド類はコピペでなく写経してます。
typoがあるかもしれないのでコピペして動かなかったら一応この記事のtypoを疑って下さい。

# GMP32bitの準備

```
$ brew install gmp --32-bit
```

で終わり。バージョン6.0.0aが入りました。
既にインストールされていたら多分64bit版が入っているので一旦 `brew remove gmp` してから再度インストールすると良いです。尚、バイナリ版はないようで、ビルドが始まります。checkに時間が掛かる。

# LLVM34 32bitの準備
骨が折れるところですね。

```
$ wget http://llvm.org/releases/3.4.2/llvm-3.4.2.src.tar.gz
$ gzcat llvm-3.4.2.src.tar.gz | tar xf -
$ cd llvm-3.4.2.src
$ ./configure --build=i686-mac-darwin CC='gcc -m32' CXX='g++ -m32' --prefix=/usr/local/Cellar/llvm34/3.4.2a
$ make -j4
$ make -j4 install
$ cd ../
```

としたら上手くいきました。何故上手くいったんでしょうねー。あとprefixは割と気持悪いのでみなさん適切な場所にインストールしましょうね。

# SML# のビルド
これが一番骨が折れるところですね。

```
$ wget http://www.pllab.riec.tohoku.ac.jp/smlsharp/download/smlsharp-2.0.0.tar.gz
$ gzcat smlsharp-2.0.0.tar.gz | tar xf -
$ cd smlsharp-2.0.0
$ ./configure --with-llvm=/usr/local/Cellar/llvm34/3.4.2a/      \
               LDFLAGS='-L/usr/local/Cellar/gmp/6.0.0a/lib'     \
              CPPFLAGS='-I/usr/local/Cellar/gmp/6.0.0a/include' \
                    CC='gcc -m32'                               \
                   CXX='g++ -m32'
$ make -j4
$ make -j4 install
```

としたら出来ました。

# まとめ
誰かhomebrewにして。
