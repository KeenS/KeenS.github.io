---
categories: [YubiKey, Linux, PAM]
date: 2021-03-28T07:52:48+09:00
title: "YubiKeyをLinuxの「鍵」にする"
---

κeenです。最近YubiKeyを買ったので色々試しています。今回はそのうちのLinuxログイン回です。

<!--more-->

# YubiKeyについて
[YubiKey](https://securitykey.yubion.com)は米瑞企業のYubico社が販売している認証デバイスです。FIDOやらWebAuthnやらの文脈で耳にした方も多いんじゃないしょうか。YubiKeyは日本ではソフト技研社が販売代理店をしています。

YubiKeyはラインナップがいくつかありますが私が買ったのはYubiKey 5 NFCです。

YubiKeyでできることは色々あります。

* FIDO U2F
* FIDO2 / WebAuthn
* Challenge and Response
* OATH-TOTP / OATH-HOTP
* Yubico OTP
* PIV
* OpenPGP
* 静的パスワード

参考：[Yubikey 5をArchLinuxで使う - Qiita](https://qiita.com/onokatio/items/15108b9c199ccf378f91)

このうち今回はFIDO U2F、Challenge and Responseの機能を使います。

# 今回やること

以下のことをやろうと思ってます。

* YubiKeyでLinuxにログイン
* sudoの認証もYubiKeyで
* YubiKeyを引っこ抜くと画面ロック

ロックとロック解除をYubiKeyで行えるのでまさしく「鍵」っぽいですね。これはお遊びというか、実用性はあと一歩といったところなので「へー、面白い」と思いながら眺めて下さい。


# PAMを使ったYubiKey認証

[Linux Pluggable Authentication Modules (PAM) ](http://www.linux-pam.org/index.html)はその名の通りLinuxのログインやsudoなどの認証を担当するフレームワークです。PAMで認証方法を色々カスタマイズできるので、YubiKeyで認証できるようにしていこうという趣旨です。先立って参考リンクをいくつか挙げておきます。

* [PAM - ArchWiki](https://wiki.archlinux.jp/index.php/PAM)
* [PAM設定ファイルの意味と書き方メモ - Qiita](https://qiita.com/satken2/items/9a0604e1d4a78217897c)
* [Ubuntu Linux Login Guide - U2F – Yubico](https://support.yubico.com/hc/en-us/articles/360016649099-Ubuntu-Linux-Login-Guide-U2F)
* [Ubuntu Linux 20+ Login Guide - Challenge Response – Yubico](https://support.yubico.com/hc/en-us/articles/360018695819-Ubuntu-Linux-20-Login-Guide-Challenge-Response)


## Challenge Response / FIDO U2F

PAMでYubiKey認証ができるようにするには2つの手段があるようです。Challenge ResponseかFIDO U2Fかです。Challenge Responseの方を試してないので推測ですが、Challenge Responseはユーザの操作不要、FIDO U2FはYubiKeyに触れるプロセスが必要っぽいです。

それぞれでインストールするモジュールが異なります。私は今回FIDO U2Fを選択します。Challenge Responseを設定する場合は公式ドキュメントを読んで下さい。FIDO U2Fなら `libpam-u2f` をインストールします。


```shell
$ sudo apt install libpam-u2f
```

あとなんか初期設定を吐く必要があるっぽいです。

``` shell
$ mkdir -p ~/.config/Yubico
$ pamu2fcfg > ~/.config/Yubico/u2f_keys
```



## PAMの設定

以後FIDO U2Fを設定するものとします。

今回私はパスワードなしでYubiKeyを挿してYに触れたらログインできるように設定します。U2Fはその名の通り2要素認証のためのものなので、変則的な使い方であることを了承下さい。

さて、まずは `/etc/pam.d/` に `u2f-sufficient` というファイルを作って以下の内容を書きます。

``` text
auth sufficient pam_u2f.so debug_file=/var/log/pam_u2f.log
```

`sufficient` を設定しているとその認証が成功したらそれだけでログイン成功します。2段階認証にしたければ `required` を設定します。

ログファイルを作っておきます。

``` shell
$ sudo touch /var/log/pam_u2f.log
```

それでは各種認証方法にYubiKeyを追加しましょう。

まずは安全な `sudo` から。ログインの設定を最初にやってしまうと失敗したときにログインできなくなりますからね。 `/etc/pam.d/sudo` を編集して `@include common-auth` の行の上に `@include u2f-sufficient` を追加します。

``` shell
$ sudo vi /etc/pam.d/sudo
$ tail -n 5 /etc/pam.d/sudo
session    required   pam_env.so readenv=1 envfile=/etc/default/locale user_readenv=0
@include u2f-sufficient
@include common-auth
@include common-account
@include common-session-noninteractive
```

今作業したターミナルは閉じないで下さい。大事な `sudo` セッションが残ってるので失敗したときのリカバリに使います。別ターミナルを開いてYubiKeyが挿さった状態で `sudo echo test` とかを打ちます。YubiKeyがピコピコ光って、触ると `sudo` が通って `test` がechoされるのを確認します。さらに別ターミナルを開いて、今度はYubiKeyを抜いて `sudo echo test` と打ち、パスワード入力が促されるのを確認します。

以上2つの確認が通れば `sudo` の設定は大丈夫そうです。ログインの設定に進みましょう。

ログインの設定は使っているディストリビューションやフレーバーによって異りそうですが、私は素のUbuntuを使っているのでGDMのパスワードのところを上書きすればよいようです。 `/etc/pam.d/gdm-password` を開いて `@include common-auth` の行の上に `@include u2f-sufficient` を追加します。

```shell
$ sudo vi /etc/pam.d/gdm-password
$ head -n6 /etc/pam.d/gdm-password
#%PAM-1.0
auth    requisite       pam_nologin.so
auth    required        pam_succeed_if.so user != root quiet_success
@include u2f-sufficient
@include common-auth
auth    optional        pam_gnome_keyring.so
```


一旦ロックしてみてYubiKeyを挿した状態でパスワード入力画面に入ったらYubiKeyがピコピコ光って触るとロック解除できるようになっていればよし。次はYubiKeyを抜いてロックしてみて、パスワードでそのままログインできればよし。

以上2つの確認が通ればPAMの設定は終了です。

# udevを使った自動ロック

udevはLinuxのデバイス管理ツールで、デバイスを抜き差ししたときにフックしてコマンドを実行できます。普通はマウントとかに使っているようですが今回はそれを画面のロックに使います。こちらも先立って参考リンクを挙げておきます。

* [udev - ArchWiki](https://wiki.archlinux.jp/index.php/Udev)
* [Passwordless logins with Yubikey](https://adl1995.github.io/passwordless-logins-with-yubikey.html)
* [Linux: Login/sudo with Yubikey](https://koka831.github.io/2019/08/08/autolock-with-yubikey/#monitoring-u2f-key-by-udev)

作業は2ステップです。

1. YubiKeyのデバイスID（とか）を調べる
2. 調べたデバイスIDにフックしてロックコマンドを走らせる設定をする


まずはデバイスIDを調べます。以下のコマンドでデバイスの動向をモニタします。

``` shell
$ udevadm monitor --environment --udev
```

その間にYubiKeyを抜くと色々情報が出てきます。その中からYubiKeyの情報をさがし、`ID_VENDOR_ID` と `ID_MODEL_ID` の値を調べます。YubiKeyの情報はだいたい一番上にあるようです。

それでは調べたIDにフックして画面のロックが走るようにします。因みに私のデバイスでは `ID_VENDOR_ID` が 1050、 `ID_MODEL_ID` が0407だったので以下ではそれを使います。画面のロックをする方法はいくつかあるようですが、GNOMEのセッション外にあるudevからキックしてもちゃんとロックしてくれるのは `loginctl lock-sessions` でした。それをふまえて `/etc/udev/rules.d/40-u2fkey.rules` に以下の設定を書きます。

``` text
ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0407", RUN+="/usr/bin/loginctl lock-sessions"
```

これを書いたらudevに今書いたルールを反映させます。

``` shell
sudo udevadm control --reload-rules
```

これでYubiKeyを抜いて画面がロックされれば成功です。失敗したらそもそも `/usr/bin/loginctl lock-sessions` でログインできるかやデバイスのIDがあってるかを確認して下さい。ログは適当に `/var/log/syslog` を `grep udev` するとかして取得して下さい。

# 既知の問題

紹介しといてなんですが、この方法は色々と問題が残ってます。

## セキュリティは向上しない

パスワードに加えて認証可能な方法を提供するのでセキュリティ的にはマイナスです。例えばYubiKeyを失くした場合にそのPCへログインされてsudoまで使われる可能性があります。

私は自分しか物理アクセスできないマシンでやってるのでそこまで問題にはならないんですがオフィスやコワーキングスペースなどでやろうと思ってる方は注意して下さい。

## 2段階認証のバックアップ…？

私がやった設定とは違って `u2f` を `required` にして本当に2段階認証にした場合の話です。YubiKeyを失くした場合に認証の代替手段がないので詰みます。やるとしたらシングルユーザモードでログインしてPAMの設定を外すくらいですがちょっと大変そうですね。

## ログインの判定がシビア

YubiKeyログインは、ログイン画面にいった瞬間にYubiKeyが挿さっていないとYubiKey認証は失敗したと見做すらしく、そのままパスワード認証に進んでしまいます。なので先にYubiKeyを挿してログイン画面に進むか、パスワード画面まで進んでしまったらYubiKeyを挿してパスワードを間違えれば認証が先頭から再開されるのでYubiKey認証をするチャンスが訪れます。

## Yubicoソフトウェアとの相性

Yubico AuthenticatorやYubikey Personalization Toolを起動するときに内部的に1回YubiKeyを挿し直しているようで、udevが反応して画面がロックされます。特にYubikey Personalization Toolはロックを解除した瞬間にも挿し直しているようで無限ロックに陥ります。

仕方ないので現状Yubico製ソフトウェアを起動するときは一旦udevの設定をコメントアウトしてルールをリロードしてから起動しています。そこまで頻度は多くないのでこれくらいなら我慢の範疇かなというところです。


# まとめ

YubiKeyの抜き差しで画面のロック/アンロックができる仕組みを構築しました。細かな点で問題は残るものの仕組みとしては面白いんじゃないでしょうか。今回の作業は以下のスクラップの末尾の方にログが残ってるので参考にして下さい。


[yubikeyを買ったので色々セットアップのメモ](https://zenn.dev/blackenedgold/scraps/1c44b845a6aa68)
