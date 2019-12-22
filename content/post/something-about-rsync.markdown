---
type: post
title: "Rsync小ネタ"
date: 2013-11-30
comments: true
sharing: true
categories: [rsync, CLI, 小ネタ]
---
[mpdを導入したとき](/blog/2013/11/13/what-recent/)にrsyncでパーミッションの問題が発生したと言ってましたが解決策がありました。

<!--more-->

まず基本。

`rsync`でバックアップをとるときは

    rsync -avz dir/ me@remote:backup

です。`dir`のあとの`/`は重要です。`dir`自体か`dir`の中身かを左右します。`zsh`はそこんとこイマイチ理解してないようで悲しいです。因みに`-a`は`-rltogpD`の略です。でも今回みたいにパーミッションを含めたくないときは

    rsync -rltogD -vz dir/ me@remote:backup

と`-p`を抜くかと思いきや、

    rsync -avz --no-p dir/ me@remote:backup

なる記法があるそうです。便利ですね。


