---
type: post
title: "FreeBSDのmanページを手動インストールする"
date: 2014-02-01
comments: true
sharing: true
categories: [Command Line, FreeBSD, 小ネタ]
---
FreeBSD10をインストールするときにdocをインストールしなかったら後で困ったのでメモ。環境はUbuntu13.10上のkvmに入れたFreeBSD10です。

<!--more-->

本来なら`sudo bsdconfig`(9.xまでは`sudo sysinstall`)でメニューを選んでいけばインストールできるのですが、`ping`か何かがおかしくてサーバーの名前解決ができない。`ping FreeBSD.org`や`ping 4.4.4.4`が返ってこない。しかし`curl`などは普通に動きます。

とりあえず

    $ ftp anonymous@ftp.jp.freebsd.org

でログインし、

    ftp> cd pub/FreeBSD/releases/i386/10.0-RELEASE

と進んで

    ftp> get doc.txz

します。んで

    ftp> bye
    $ unxz -cd doc.txz | sudo tar xf - -C /

でインストール完了。

普通に配置するだけで良いんですね。mandbとかあるのかと思ってた。


