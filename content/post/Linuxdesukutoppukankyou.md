---
categories: [Linux, 開発環境]
date: 2017-01-10T22:07:31+09:00
title: 頑張らないLinuxデスクトップ環境
---

κeenです。みんなカスタマイズしてLinux使っているようなのであまりカスタマイズしてない私のものも紹介しますね。

因みに私は会社でMac使ってたら使いにくすぎてVMに逃げたくらいのLinuxユーザです。家では勿論Linuxしか使ってないです。
<!--more-->

# ディストリ
Ubuntu。定番。色々言う人がいるけど何も不満はない。

# ウィンドウマネージャ（統合デスクトップ環境）
Unity。デフォルト。

# IM
uim-skk。何故uim-かというと最初にたまたまそれを使ったから。

# ブラウザ
FireFox。確かデフォルト。入れてる拡張はkeysnailのみ。

# Twitterクライアント
公式Web。新機能の追加が一番早くて便利。

# ターミナルエミュレータ
gnome-terminal。デフォルト。速いし簡単に背景半透明に出来るし何も不満はない。tmuxと一緒に使ってる。

# エディタ
Emacs。デフォルトではない。毎日masterをコンパイルして使う。

# キーバインド変更
xmodmap + [xremap](https://github.com/k0kubun/xremap)。これ CF [Linux向けの最強のキーリマッパーを作った - k0kubun's blog](http://k0kubun.hatenablog.com/entry/xkremap)。
CapsLockとctrlのスワップにxmodmapを、Emacs風バインドを作るためにxremapを使ってる。xremapは別になくてもどうにかなる。

# メーラ
Thunderbird。確かデフォルト。でもUbuntuのパッケージは更新が遅いので手でインストールしてる。

# カレンダー
Thunderbird + Lightening拡張。Googlカレンダーと同期出来るし便利。

---

このくらいかな？フォントもデフォルト。あとはSlackに公式クライアントだとか偶に音楽再生にrythmbox（デフォルト）だとか画像表示にeog（デフォルト）だとかPDF表示にevice（デフォルト）だとかファイラにnautilus（デフォルト）スクリーンショットにgnome-screenshot（デフォルト）だとかを使うけどこの辺は当たり前なので書かなくていいかな。

ということでそんなにカスタマイズしなくてもLinuxは便利なのでみなさん使いましょう。
