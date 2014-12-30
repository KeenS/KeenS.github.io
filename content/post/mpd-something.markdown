---
type: post
title: "Mpd 小ネタ"
date: 2014-01-21
comments: true
sharing: true
categories: [Raspberry Pi, mpd, Command Line]
---
mpdのちょっとした便利な使い方。

<!--more-->

`at`がRSPiにインストールされてる前提で、RSPi上で

    $ at 7:00
    at> mpc toggle
    ^D

とすればRSPiを目覚ましや時報として使えます。停止にはsshログインして`mpc toggle`する必要があるのでそこまでする頃には起きてる筈。


