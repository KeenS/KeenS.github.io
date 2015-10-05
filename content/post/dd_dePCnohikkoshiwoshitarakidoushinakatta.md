---
categories: [CLI, BIOS, UEFI]
date: 2015-10-05T21:30:06+09:00
title: ddでPCの引っ越しをしたら起動しなかった
---
κeenです。この間長年使ってたDynabookからThink Padに乗り換えました。引っ越しの時に困ったのでメモ。

<!--more-->

元々あるのが

* Dynabook
* 外付けハードディスクカバー

新たに購入したのが

* Think Pad (HDDモデル)
* SSD

SSDだけ先に手元に来たのでまずSSDへ引っ越しを済ませ、Think Padが届いたらHDDとSSDを交換することに。

# DynabookのHDDからSSDへの引っ越し
Ubuntuをインストールしてバックアップから復元しても良かったのですがHDDもSSDも512GBだったので`dd`を使って引っ越ししました。

事前に出来る限りプロセスは殺す。本来は別ディスクから立ち上げたOSでやるのが良いんだろうけど面倒だったので引越し対象のOSから実行しました。。コマンドはすごいシンプル。

```sh
$ dd if=/dev/sda of=/dev/sdb bs=512k
```

外付けハードディスクカバーとDynabookがUSB2.0までにしか対応してないからか20MB/sしか出ず、一晩掛かりました。しかし終わったあとDynabookのHDDとSSDを交換して起動してみると何の問題もなく起動しました。素晴しい。

# SSDをThink Padに移植

一瞬ハードディスクの外し方が分からず苦戦するもネットで調べると分解マニュアルが出てきたので楽に換装。しかしThink Padの電源を入れても起動しなかった。

# BIOSとUEFI
## BIOS
BIOSといえばコンビュータの電源をポチっと入れた瞬間に起動するソフトウェアで、Basic I/O Systemの略かと思っていましたがBasic I/O Systemの実装の1つの名前でもあるらしいです。

BIOSはディスクの先頭にあるMaster Boot Recordを読みにいきます。そこからパーティションテーブルの情報を得てgrubなどを起動します。（Master Boot RecordのことをMBRと略すこともあるしMaster Boot Recordを使うパーティションテーブルのことをMBRと呼ぶこともある…？）。昔、[GPTを使うと起動しなかった経験がある](/blog/2015/01/08/freebsd-environment-setups/)ので私のDynabookのBasic I/O SystemにはBIOSが使われていた模様。

## UEFI
UEFIはBIOSに代わるBasic I/O Systemらしいです。MBRの他、GPTもサポートします。GPTは **MBR を使わず** 、EFI System Partitionなるものを使うらしいです。Think PadのBasic I/O Systemはこれだったようです。

# 原因、対策、今後
ということでUEFIがGPTを期待していたのにこちらはBIOS向けのMBRを用意していたのでダメだった模様。(いわゆる従来)BIOS画面(と呼ばれていたもの)に入って起動タイプをUEFIからLegacyにすることで起動出来ました。

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> 可能ですよ。WindowsでもLinuxでもBSDでも、です。</p>&mdash; orumin (@kotatsu_mi) <a href="https://twitter.com/kotatsu_mi/status/647688708661972992">2015, 9月 26</a></blockquote>

<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> gptgenなどを用いてパーティションをMBRからGPTに変換すること、FAT32のパーティションを作り(先頭パーティションであることが好ましい)パーティションタイプを0xEF00にすること、そしてブートローダーをそのパーティションにインストールする</p>&mdash; orumin (@kotatsu_mi) <a href="https://twitter.com/kotatsu_mi/status/647692419090112512">2015, 9月 26</a></blockquote>
<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> 以上があらましになります。FAT32のこのパーティションはEFI System Partition, ESPと呼ばれ、従来ファイルシステムから不可視な先頭セクタにインストールされていたブートローダーはただの実行ファイルとしてESPに置かれます</p>&mdash; orumin (@kotatsu_mi) <a href="https://twitter.com/kotatsu_mi/status/647692825853755392">2015, 9月 26</a></blockquote>

<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> ブートローダーをESPに入れる手段としては、ArchWikiのUEFIの項をご覧ください。また、この時efibootmgrコマンドを用いてブートローダーのパスをUEFIに記録させておくと、UEFIのブートデバイス選択画面にブートローダーが出現します。</p>&mdash; orumin (@kotatsu_mi) <a href="https://twitter.com/kotatsu_mi/status/647693202737098757">2015, 9月 26</a></blockquote>
<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> GRUBの場合efibootmgrコマンドを使うべき操作は自動でしてくれます。</p>&mdash; orumin (@kotatsu_mi) <a href="https://twitter.com/kotatsu_mi/status/647693310149025792">2015, 9月 26</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>


とのことですがまあ、SSDの先頭のスペース空いてないしちょっとミスったら即死だしどうしましょうね。
一旦SSDの中身を元のHDDに退避してSSDにUEFIで起動するパーティション作ってそのパーティションに元のパーティションをddでコピーとかで出来ますかね…

サルでも分かる操作マニュアルが欲しい。

