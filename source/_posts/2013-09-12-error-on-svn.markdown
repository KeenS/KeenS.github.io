---
layout: post
title: "FreeBSD10でちょっとこけた"
date: 2013-09-12 23:21
comments: true
sharing: true
categories: [FreeBSD, FreeBSD10, svn, svnlite, 備忘録, 小ネタ]
---
FreeBSD10をインストールしてたときにちょっとつまったところがあったのでメモ。

<!-- more -->
## (前回までのあらすじ)

FreeBSD10をインストールしたkvmのイメージでFreeBSDのソースをコンパイルできなくなっていたのでκeenはイメージに最新のFreeBSDを上書きインストールすることを決意したのであった

## 今回

[https://pub.allbsd.org/FreeBSD-snapshots/](https://pub.allbsd.org/FreeBSD-snapshots/)から最新版isoをダウンロードして

    $ kvm -cdrom ~/Download/FreeBSD-10.0-HEAD-r255472-JPSNAP-i386-i386-disc1.iso -boot d -m 2048 -smp 3 FreeBSD10.img

でブート&インストール。普通にインストーラに従って問題無し。  
もう一度

    $ kvm -m 2048 -smp 3 FreeBSD10.img

でブート。`/usr/src`が空なのを確認して、最近標準添付になったsvnliteでソースをチェックアウト。

    # cd /usr
    # rmdir src
    # svnlite co https://svn0.us-west.FreeBSD.org/base/head src

で、エラー。  
内容は

> server sent a truncated http response body.

不明。`svn(lite)`のエラーらしいことは分った。`https:`が悪い気がするので`http:`にしようかと思ったけど、ふとダメ元で以前はできなかった`svn:`のスキーマにしてみた。

    # svnlite co svn://svn0.us-west.FreeBSD.org/base/head src

できた。なんだよ。

### 蛇足

このあと

    # svnlite co svn://svn0.us-west.FreeBSD.org/ports/head ports

でportsもチェックアウトしましたよ


