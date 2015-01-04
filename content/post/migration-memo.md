---
categories: [備忘録]
date: 2015-01-04T05:26:25Z
title: OSお引っ越しの時のデータ移行の自分用注意点
---
自分用。

<!--more-->

# コピー

* `cp -R`じゃだめ。タイムスタンプが変わる。アーカイブ専用に`cp -a`がある。あるいは`rsync`
* 移動するのは
  + 非隠しフォルダ/ファイル
      - 但しDownloads/は除く。あれはプラットフォーム依存なバイナリなんかも含む
      - Dropbox/はどっちでも良い。コピーせずにDropboxの同期に任せた方が確実
      - Desktop/はどうせ空だからコピーしてしまっても害はない
  + .ssh/
  + .emacs.d/
  + .twittering-mode.gpg
  + .gitconfig
  + .thunderbird/
  + .config/
  + 残りの隠しファイルはdot.filesにある
  + もしかしたら.mozilla/もコピーしておくといいかも
* compile/以下は`make clean`しておく。オブジェクトファイルがプラットフォーム依存だからコピーする意味ない
* compile/以下はファイル数が多くて時間かかるので要らないものはこの際削除
* 同じ理由で.cim/, .rvm/, .nvm/, .opam/なんかもコピーしない

# コピーが終わったら

* Emacsをビルドして起動、動作確認
* Thunderbirdをインストール、動作確認
* gitをインストール、githubとの接続確認
* CIMをインストール、`cim install sbcl`の動作確認
* rustをビルドするときは`./configure`から始める

# いつかやる

* tarballにコピーするファイルを詰め込んどく
  + 理想的にはメインのHDDとは別の記憶媒体にtarballを保存
  + 定期実行が面倒になるくらいなら~/.backupにでも。気が向いた時に別の媒体にコピー
* どうもappendオプションを使えばヒストリ付きのアーカイバとして使えるらしい
