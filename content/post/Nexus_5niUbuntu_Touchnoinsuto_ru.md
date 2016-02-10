---
categories: [Ubuntu Touch]
date: 2016-02-10T23:09:09+09:00
title: Nexus 5にUbuntu Touchのインストール
---

κeenです。先日中古のNexus 5を入手したのでUbuntu Touchをインストールしました。その時のメモ。
尚、Multi ORMなどを使ったマルチブートではなく完全にAndroidを殺したシングルブートです。
<!--more-->
色々調べてみましたが[公式ドキュメント](https://developer.ubuntu.com/en/start/ubuntu-for-devices/installing-ubuntu-for-devices/)のままやるのが一番無難なようです。
まあ、スクショがないので他のサイトのスクショは参考にした方がいいかもしれませんが。

途中adbを使ったりする部分も丁寧に解説してるのでAndroid初心者でも問題ないはず。


# ツールの準備
Ubuntu公式で案外色々なツールを用意してくれている模様。

```
sudo add-apt-repository ppa:ubuntu-sdk-team/ppa
sudo apt-get update
```

でppaを登録した後は

Ubuntu Touchインストールに必要なツール

```
sudo apt-get install ubuntu-device-flash
```

USBで繋いだデバイスをデスクトップから操作するのに便利なツール

```
sudo apt-get install phablet-tools
```

をインストールします。（恐らく）このコマンドでadbとfastbootも入ります。

# AndroidのUSBデバッグを有効にする

普段から自機でAndroidアプリを開発してない人はこのステップが必要でしょう。

## 開発者モードの解放

[設定のアプリ] → [一般] → [端末情報] → [ソフトウェア情報]

と進んで[ビルド番号]を7回タップすると開発者モードが解放されます。

私がAndroid Appを作ってた頃(Android 2.3)にはこんな手順なかったんですけどいつのまにやら変わったんですね。

## USBデバッグの有効化
さて、これで[一般]タブに戻ると[開発者向けのオプション]という項目が出てくるのでそこからUSBデバッグを有効にしましょう。

# Nexus 5を繋ぐ
USBケーブルでNexus 5をPCに繋ぎます。

```
adb devices
```

で1つデバイスが表示されていたらちゃんとデバッグモードで繋げています。

# Androidデータのバックアップ
Androidに未練はなくてももしUbuntu Touchのインストールに失敗した時に引き返せるようにバックアップは必要です。

```
adb backup -apk -shared -all
```

のコマンドでカレントディレクトリに`./backup.ab`という名前でデータが吸い出されます。

さらにAndroidの再インストールに必要なデバイスデータも取得します。

```
adb shell grep ro.product.name /system/build.prop > mydevicedata \
&& adb shell grep build.id /system/build.prop >> mydevicedata \
&& adb shell grep ro.product.device /system/build.prop >> mydevicedata
```

中身はこんな感じ。

```
$ cat mydevicedata
ro.product.name=hammerhead
ro.product.device=hammerhead
ro.build.id=MMB29S
```

# ブートローダのアンロック

そろそろ危いところですね。

Androidの再起動 & ブートローダへ突入。倒れたドロイド君が現れます。

```
adb reboot bootloader
```

そしてアンロックします。

```
sudo fastboot oem unlock
```

デバイス側で色々訊かれるので音量ボタンと電源ボタンを駆使して答えていきましょう。

質問が終わってドロイド君の画面に戻ったら

```
fastboot reboot
```

します。これでAndroidは初期化されてるので最初のセットアップを済ませましょう。どうせ直ぐに殺すので最小限で大丈夫です。

# Ubuntu Touchのインストール
さて、ここは公式ドキュメントと異なる部分です。
[公式サポートのチャネル](https://developer.ubuntu.com/en/start/ubuntu-for-devices/image-channels/)ではNexus 5がサポートされていません。
[Ubuntu wiki](https://wiki.ubuntu.com/touch/devices)にいくとコミュニティベースでホストされているイメージがあるのでそれを使います。

```
ubuntu-device-flash --server=https://system-image.ubports.com touch --channel=ubuntu-touch/stable --bootstrap
```

まあまあな量ダウンロードしますが放置しておけばそのままubuntu touchのインストールが完了します。

おめでとうございます。

# 触ってみた所感

上下左右の端からスワイプインするとメニューとか諸々が出てくるのは面白い。特に左のメニューはUbuntu使いなら体に馴染む。
使い勝手は悪くなさそう。Android使いならすぐに馴染める。

アプリはまだまだといった所。ストアのアプリも少なければCanonicalの公式アプリもWeb Appという仕上がり(Twitterがモバイル版Webそのままだった)


<blockquote class="twitter-tweet" data-lang="ja"><p lang="in" dir="ltr">Ubuntu Touch nihongo utenai?</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/696885260655570944">2016, 2月 9</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

一応パッチは出ている。

[Merge into trunk : japanese-keyboard-rebooted : Code : ubuntu-keyboard](https://code.launchpad.net/%7Ecosmos-door/ubuntu-keyboard/japanese-keyboard-rebooted/+merge/268158)
