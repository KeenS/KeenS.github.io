---
type: post
title: "MPlayerがうまく動かない"
date: 2013-09-29
comments: true
sharing: true
categories: [MPlayer, MPlayer2, Ruby]
---
MPlayerを使ってBGMを流すコマンドを作ろうとしたときに困ったことがいくつかあったので誰か解決して下さい。

<!--more-->

スクリプトは3年前で更新がとまってる`mplayer-ruby`を使ってRubyでmplayerを叩く方針。

案の定バグに当りました。スペースの入ってるファイルをうまく処理できない。これは内部的にシェルを呼んでるのですが、そのときにシェルエスケープをしてないためです。`require 'shellwords'`しとけば`String#shellescape`メソッドが追加されるので`mplayer-ruby`の何箇所かにある`file`を`file.shellescape`すれば解決しました。

さらにDirty Hack。複数ファイルを`load_file`で渡してると詰まるようなので思いきって`MPlayer::Slave#initialize`を改造して引数に再生ファイルの配列(元々はファイル名の文字列)を渡すようにしました。

これで

<script src="https://gist.github.com/KeenS/6308894.js?file=BGM"></script><noscript><pre><code>#!/usr/bin/env ruby -Ku
# _*_ coding:UTF-8 _*_
require 'mplayer-ruby'
require 'libnotify'

class MPlayer::Slave
  def now_playing
    summary = ""
    summary += get(:meta_title).chomp
    summary += " -- " + get(:meta_artist).chomp
    body = "NowPlaying"
    Libnotify.show(body: body, summary: summary)
  end
end

music = "/your/path/to/music/directory/"
playlist = File.read( music + "BGM.list").split("\n").map{|f| music + f }.shuffle
player = MPlayer::Slave.new playlist
player.loop
Signal.trap(:INT) {player.quit;exit}
Signal.trap(:KILL) {player.quit;exit}
player.now_playing
while line = player.stdout.gets
  player.now_playing if line =~ /playback/
end
</code></pre></noscript>

が動くようになりました。

しかし一つ問題がありまして、再生が1曲目で止まっちゃいます。`loop`を指定してるのにです。mplayerを直接叩いてみるに、mplayerのバグっぽい…複数ファイル指定したときは本来なら全曲1回は再生してくれるはずなのに再生しませんし`-loop 0`オプションガン無視。今回入れたのはUbuntuのapt-getで入るやつで、バージョン(?)は「MPlayer2 UNKNOWN © 2000-2012 MPlayer Team」だそうです。

Ubuntuはパッケージが古いことが往々にしてあるので冷静に自分でビルドしようとしても`fridibi`がないとかで怒られる。

どなたか解決方法御存じないですか???

## 追記

`-loop -1`を渡すとループしてくれました。それドキュメントと逆…


