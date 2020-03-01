---
categories: ["Android", "備忘録"]
date: 2020-03-01T23:15:03+09:00
title: "Essential PhoneにCarbonROMを焼く"
---

κeenです。ふとした興味からEssential PhoneにAndroidのカスタムROMの1つ、CarbonROMを焼いたのでそれをメモします。

<!--more-->

Essential Phoneを買ったあと、1年も経たずにPixel 3aに買い替えてしまっていました。綺麗なまま死蔵するのももったいないのでカスタムROMを焼いて遊んでみます。

# カスタムROMとは？

Androidはオープンソースで開発されています（Android Open Source Project）。なのででGoogleが配ってるROM以外にも有志が配布しているROMがあります。それがカスタムROMです。
有名どころだと[LineageOS](https://lineageos.org/)なんかがあります。

あんまりROMという言葉が馴染まないのですが、Linuxでいうディストリと同じ意味という理解であってますかね？ブートローダまでは含んでないようなので微妙に違う？

ちょっとGoogleに依存しないAndroid、AOSPにできるだけ近いAndroidに興味があったので、Google以外のところが配っているROMを試してみることにしました。

# CarbonROMとは？

[CarbonROM](https://carbonrom.org/)はStock Android（手を加えていない生のAndroid）に近い形で配られているROMです。安定性に重点を置いているようです。

> our vision is to be the best alternative to a stock operating system for your device.

とのことなので今回の私の目的に適っていそうです。

# CarbonROMのインストール

CarbonROMをインストールしていきます。
おおまかには

1. OEMアンロック
2. TWRPのインストール
3. CarbonROMのインストール

の順番でやっていきます。

これは一般的な手順とは少し異なっています。Google PlayなどのGoogleが配っているアプリ（gapps）をインストールしていません。今回の目的はGoogleに依存しないAndroid環境の構築なのでそういうもんです。Google Playを入れずにアプリを使う方法は後述します。

インストールにはパソコン（ターミナル）をほとんど使わないやりかたもあるみたいですが、私はむしろターミナルを使った操作の方がやりやすいのでターミナル操作中心にやっていきます。

途中、スマホにマウスを挿す手段が必須になります。私は手元にUSB Type-A to Type-Cのアダプタがあったのでそれを使って普通のマウスを使いました。

余談ですが今回のインストールにあたってはまず英語記事を漁りました。
どうせ日本語情報はないだろうと踏んだので。
ですが思ったよりめぼしい情報がありませんでした。
最後にダメ元で日本語で情報をさがしてみたら下記の記事にあたりました。

[Essential Phone PH-1をLineageOSにした話 (追記)(再び追記 細かい修正等) - Gadget&amp;LifeStyle  Blog](https://7800titanim.hateblo.jp/entry/2019/12/31/130610)

今回の記事は上記の記事をおおいに参考にして書かれています。感謝します。

その他には以下の記事を読みました。

* [How to Root and Install Official TWRP Recovery on Essential Phone PH-1](https://www.getdroidtips.com/root-twrp-essential-phone-ph-1/)
* [Update CarbonROM on Essential Phone based on Android 8.1 Oreo](https://www.getdroidtips.com/carbonrom-essential-phone-8-1-oreo/)
* [How to Wipe Data using TWRP Recovery](https://www.getdroidtips.com/wipe-data-using-twrp-recovery/)

## 0. 諸々のダウンロード

CarbonROMのダウンロードは[こちら](https://get.carbonrom.org/)から。Essential Phoneのコードネームはmataなので[mata](https://get.carbonrom.org/device-mata.html)の最新版をダウンロードします。サイズは600MBちょい。

[TWRP](https://twrp.me/)は[こちら](https://twrp.me/essential/essentialph1.html)からダウンロードします。TWRPはカスタムリカバリツールだそうです。よく分かってないですが、ファクトリリセットする機能を上手く使って別物のROMを焼くソフトウェアなのかな？

あとはadbとfastbootを手元のUbuntuにインストールします。 `apt install adb fastboot` で入ります。

## 1. OEMアンロック

これは以前[Ubuntu Touchをインストールした](/blog/2016/02/10/nexus_5niubuntu_touchnoinsuto_ru/)ときと共通ですね。

### 1.1. AndroidのUSBデバッグを有効にする
#### 1.1.1. 開発者モードの開放
[設定のアプリ] → [一般] → [端末情報] → [ソフトウェア情報]

と進んで[ビルド番号]を7回タップすると開発者モードが解放されます。

#### 1.1.2. USBデバッグの有効化

さて、これで[一般]タブに戻ると[開発者向けのオプション]という項目が出てくるのでそこからUSBデバッグを有効にしましょう。

### 1.2. OEMロックの解除

ちょっと記憶があいまいなんですが、同じく[開発者向けのオプション]に[OEMロック解除]のメニューがあるらしいので有効にします。ターミナル叩かなくてもアンロックできるんですね。

### 1.3. データのバックアップ

略。以前のUbuntu Touchをインストールしたときの記事を読んで下さい。
私はやってません。

## 2. TWRPのインストール

Essential Phoneをパソコンに繋いで画面のロックを解除しましょう。 `adb devices` でそれっぽい表示が出たらOK。

ブートローダに入ります。

```console
$ adb reboot bootloader
```

再起動してブートローダに入ったらtwrpをインストールします。

```console
$ fastboot flash boot twrp-VERSION-mata.img
```

私の場合はVERSIONは3.2.3-0でした。

終わったら再起動するらしいです（覚えてない）

```console
$ fastboot reboot
```

## 3. CarbonROMのインストール

先程のコマンドで再起動したらtwrpの画面に入ります。
この画面ではなんとタッチパネルも音量ボタンも効きません。マウスを繋いで操作します。
ひとまずロックを解除すればOK。

ロック解除したらデータを消します（データ保護のため、既存のデータを消さないとROMを焼けない仕組みになってるらしい）

```console
$ adb shell twrp wipe data
$ adb shell twrp wipe system
```

パソコンからROMを流し込みます（sideload）

```console
$ adb shell twrp sideload
$ adb sideload CARBON-CR-7.0-OPAL-RELEASE-mata-DATE-TIME.zip
```

DATEとTIMEは自分でダウンロードしたファイル名に合わせて下さい。

あとは再起動するだけです。

```console
$ adb reboot
```



これでCarbonROMが立ち上がってくるはずです。

# インストール後のセットアップ

以下は私がやったセットアップです。各人の好みに合わせて判断して下さい。

## システムの言語を日本語に

記憶にないけど多分やってるはず。

Settingsから [System] → [Languages & Input] → [Languages] → [Add a language]で日本語を選択。

戻った画面でまだ英語のままのはずなので。右上のメニューからremoveを選んで英語を削除。
これで日本語になるはず。

## 高度なリブートメニューをOFFにする

デフォルトでは端末を再起動しようとするとtwrp関連の選択肢が増えています。twrpはもう用済みなのでこの選択肢を出さず、素のAndroidと同じようにします。

設定から[Carbon Fibers] → [システム] → [高度な再起動] をOFFに。

## 電源ボタンで画面の録画をONにする

スクショ以外にも録画もできるみたいです。

設定から [Carbon Fibers] → [システム] → [電源メニュー] → [画面の録画] をONに

## ホームボタンを上にスワイプをONに

いつデフォルトになったか覚えてないですが、今まで使ってたのと同じ動きにします。

設定から [Carbon Fibers] → [ジェスチャー] → [ホームボタンを上にスワイプ] → [ホームボタンを上にスワイプ] をONに

## テーマをブラックに

Carbonって名前なのでカーボンっぽい色にします。因みにデフォルトの壁紙は炭ですね。

設定から [ディスプレイ] → [詳細設定] → [端末のテーマを使用] を[常にON] に。その下にある[ブラックテーマを使用] をONに

ブラックテーマよりダークテーマの方が目にやさしい気がしますがCarbonROMのページだと多分ブラックテーマのスクショが貼られてる気がするのでそれにあわせます。

## F-Droidをインストール

[F-Droid](https://f-droid.org)Adroidアプリの自由ソフトウェアのストアです。
Google Playに比べるとアプリ数は少ないですがGoogleに依存していません。

プリインストールのブラウザから https://f-droid.org にアクセスして[Download F-Droid]を選択します。システムに怒られるはずなのでブラウザからのapkのインストールを許可するようにして、インストールします。

慎重な方はパソコンでダウンロードしてPGPを検査してからadbでインストールするとよいかもしれません。

因みにアプリはFirefoxのフォーク（？）の[Fennec](https://f-droid.org/en/packages/org.mozilla.fennec_fdroid/)や[NextCloud](https://nextcloud.com/)の[公式クライアント](https://f-droid.org/packages/com.nextcloud.client/)、[Simple Mobile Tools](https://www.simplemobiletools.com/)、[OpenStreetMap](https://openstreetmap.org)クライアントの[OsmAnd](https://f-droid.org/en/packages/net.osmand.plus/)などがあります。


F-Droidの他にはGoogle Play Storeの非公式クライアントの[AuroraOSS](https://auroraoss.com/)なんかもありますが、利用規約に違反しているらしいです。
うっかりAuroraOSSからログインしてPlay StoreにアクセスするとBANされる可能性もあるとか。

# 感想とか

OS自体は今までと遜色なく使えて満足しています。
ただまあ、Play Storeがないと不便極まりないですね。Slackとか使いたい。
Play StoreをインストールするかAuroraOSSをどうにかして使えるようにするかしたいですね。
Term of serviceに違反しても偽計業務妨害とかに問われないんなら使ってみようかなって気になるんですがね…。
