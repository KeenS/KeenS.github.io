---
type: post
title: "Tiarraにercで接続したときの/loadの問題"
date: 2013-10-06
comments: true
sharing: true
categories: [Emacs, irc, 備忘録, 小ネタ]
---
[tiarra](http://www.clovery.jp/tiarra/)に [erc](http://www.emacswiki.org/ERC)で接続したときの/loadの問題。

<!--more-->

tiarraは有名なircプロキシ。ercはEmacs上で動くircクライアント。これらを使っているとtiarraが自動でログインしてくれないのでおかしいなと思っていたらtiarraの自動ログインモジュールを有効にしていなかった模様。

書き換えていざ設定を反映しようとすると若干困りました。どこに`/load`と打てばいいんだろ。

最初に誘導されるバッファ(ircバッファ名がプロキシサーバー名になってるやつ)に`/load`と打つと

    ERC> /load
    Incorrect arguments. Usage:
    /LOAD line
    Load the script provided in the LINE.
    If LINE continues beyond the file name, the rest of
    it is put in a (local) variable `erc-script-args',
    which can be used in Emacs Lisp scripts.
    
    
    The optional FORCE argument is ignored here - you can't force loading
    a script after exceeding the flood threshold.

ですって。ああ…。`/load`コマンドがercに食われてる…

流石に解決策あるだろって思って調べたら

    /quote load

だそうです。

    ERC> /quote load
    -192.168.1.4- *** Reloaded configuration file.
    -192.168.1.4- *** Module Channel::Join::Connect will be loaded newly.

はい。ちゃんとloadしてくれました。

以上小ネタでした。


