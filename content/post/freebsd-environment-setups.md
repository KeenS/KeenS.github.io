---
categories: [FreeBSD]
date: 2015-01-08T14:37:36Z
title: FreeBSDの環境構築
---
[以前の記事](/blog/2014/12/27/freebsd/)の続編。FreeBSDで普通の(Ubuntuで使っていたのと同程度の)環境を整えようとしたときの話。普通ならPC BSDを使えば苦労しないんだろうけど私のラップトップは少し古いのか癖があるのかGPTパーティションに対応してなくてPC BSDのインストーラが使えなかったのでFreeBSDを入れてデスクトップ環境を一から構築した。
<!--more-->
# キーボード
デフォルトでdvorakにしてるのに起動時にはqwertyになってる問題。なんかいつのまにか解決してた。多分だけど /etc/rc.confに`keymap="jp.106"`書いてから直った気がする。LANGの設定かも。

# Firefox
## 漢字が汚ない問題
フォントの問題と思ってたらアンチエイリアスの方だった。KDEの設定で[システム設定]->[アプリケーションの外観]->[フォント]->アンチエイリアスの設定->[スタイルのヒント]をなしにすると漢字にもアンチエイリアスが掛かる。しかしHelveticaが汚ない問題は直らなかった。というか今度はアンチエイリアスが掛かりすぎてぼやけてしまうようになった。[Firefox (日本語) - ArchWiki](https://wiki.archlinux.org/index.php/Firefox_(%E6%97%A5%E6%9C%AC%E8%AA%9E))に解決策があった。ビットマップフォントを拒否すれば良いらしい。

## メニューが日本語にならない問題
クイックロケールチェンジャーみたいなのがいつの間にか入ってたのでそれを使った。それを見るとロケールに日本語が入ってるのにデフォルトで英語を使うようになってたので英語のロケールを削除して日本語だけにしたらメニューも日本語になった。

## Flash
一応[公式ハンドブック](https://www.freebsd.org/doc/ja/books/handbook/desktop-browsers.html)に設定方法が載ってるが、これでは動かなかった。linuxの互換レイヤーがカーネルパニック起こしてた。
`sudo sysctl compat.linux.osrelease=2.6.18`したらwww/linux-c6-flashplugin11のコンパイルと`nspluginwrapper -a -v -i`が通った。勿論後でcompat.linux.osrelease=2.6.18は/etc/sysctl.confに書き足した。あと/ect/rc.confにlinux_enable=YESも書いてある。多分必要。

蛇足だけどlinux-c6がCentOS6互換レイヤ、linux-f10がFedora10互換レイヤらしい。排他的っぽい。

# Dropbox
DropboxのデスクトップアプリはFreeBSDをサポートしないものの、pkgにdropboxのAPIを叩くコマンドラインスクリプトがある。dropbox-api-commandって名前。手動でsyncすることになるけど実際スマホと写真のやりとに使ってる程度なので必要なときに手動でsyncしてる。unisonみたいな使い方。

# Wifi
KDEにデフォルトでセットアップアプリケーションがついてこないっぽいのでwifimgrを入れた。

# SSH Key
ksshaskpassを導入、~/.xprofileに

```sh
export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8
export LANG=ja_JP.UTF-8
export SSH_ASKPASS=/usr/local/bin/ksshaskpass
eval $(ssh-agent) >> /dev/null
```
を追加、~/.kde/Autostart/に
```sh
#!/bin/sh
export SSH_ASKPASS=/usr/local/bin/ksshaskpass
/usr/bin/ssh-add
```
を実行権限付きでssh-addという名前で保存してあとはKDE Walletをポチポチやったらログイン時にマスターパスワードを聞かれて、ログイン後はパスワード不要になった。

また蛇足。Xの起動時に読まれる設定ファイルは.xinitrc、.xprofile、.xsessionなどがある。.xsessionは過去の遺物、.xinitrcは`startx`された後ウィンドウマネージャの起動まで管理するスクリプトで由緒あるが最近は使われないことが多い、.xprofileは設定だけでウィンドウマネージャの起動まではしない設定ファイルで、gdm、kdmなどのウィンドウマネージャから読まれるとのこと。.xinitrcに.xprofileを読みにいく行を書いておけばXに関連する主な設定は.xprofileに書いておけば良い。

# Emacsでximが使えない問題
uim-ximが起動してなかった。

.xprofileに
```sh
export XIM="uim"
export GTK_IM_MODULE=uim
export QT_IM_MODULE=uim
export XMODIFIERS='@im=uim'
```
を書いて、.kde4/Autostart/に
```sh
#!/bin/sh
exec /usr/local/bin/uim-xim
```
を置いておけば使えるようになった。
# 残った問題
* スリープからの復帰時にディスプレイが点かない
* ラップトップ内のwifiが使えない
* 音が鳴らない
* KVM相当のものが使えない

やっぱうちのラップトップ(TOSHIBAのDynabook)特殊なのかなあ。
