---
categories: [Linux, 小ネタ]
date: 2018-04-28T02:34:48+09:00
title: "Ubuntuでバックアップが失敗する話、あるいはディスクサイズとファイルサイズについて"
---
κeenです。寝れないので小ネタを。数年悩んでた問題がようやく解決しましたのでそれについて。
<!--more-->

# 経緯

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">30GBくらいのファイルのバックアップをdejadupを使って500GBのHDDに取ろうと思ったらディスクの空き領域がないって言われるんだけど何が原因なんだろう。因みにホストのSSDも500GBある。Ubuntu 16.04と16.10で起きた。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/805017551776251904?ref_src=twsrc%5Etfw">2016年12月3日</a></blockquote>

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">Ubuntuのバックアップがとれない病に掛かってるんだけどこの度17.04に上げてもダメだった。因みにバックアップデータ数十GBに対してバックアップ先は500GBと十分にある。 <a href="https://t.co/vgSE8uoLtB">pic.twitter.com/vgSE8uoLtB</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/854333150519975940?ref_src=twsrc%5Etfw">2017年4月18日</a></blockquote>

エラーメッセージの情報量が少ないので打てる手が少ないですね…。

このあとバックアップ用のHDDを買い替えて2TBのものを用意しましたがダメでした。
ハードの問題ではないと判明したのでUbuntuのバックアップ(ソフトウェア名はdeja-dup)のバグを疑い、しばらく放置することにしました。
流石に再現条件を割り出せてないので自らバグレポートをすることはせず、熱心な他人に委ねようという判断です。

# 原因究明
最初に問題が発生してから2年くらいになりますがふと思い立って少しずつバックアップを取ったらどうかと試し始めます。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">ubuntuのdejadupがバックアップに失敗する問題、ちょっとずつやれば成功するな。どこのサイズが足りてないんだろう。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/989518503441924102?ref_src=twsrc%5Etfw">2018年4月26日</a></blockquote>

少し進展しましたね。

さらにここでデバッグする気力が湧いたので色々試し始めます。
`top` でCPU Usageをみてるとdeja-dupはただのフロントエンドでバックアップをしているのはduplicityということに気づきます。
そこで `ps aux` でコマンド引数を抜き出して自分の手でduplicityを操作し始めます。

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">とりあえずduplicity叩いたらエラーメッセージは見えた。150Gくらいのバックアップに4TB必要と言っている。ほんまか？</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/989690301420716034?ref_src=twsrc%5Etfw">2018年4月27日</a></blockquote>

150Gというのは `du ~/` で測ったものです。そもそもUbuntuをインストールしているディスク全体でも500GBなので何か設定を間違っていたとしても流石に500GBには収まる筈。バックアップのメタデータでちょっと嵩むとしてもこれはおかしいですね。

さらにこのあと `--dry-run` ではなく実際にコピーを走らせてみると、とあるファイルでずっと止まってることに気づきます。

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">理解した。pijulのdbファイルが中身はほぼ空だけどサイズ上4TBあることになってた。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/989725492755353600?ref_src=twsrc%5Etfw">2018年4月27日</a></blockquote>

ということで犯人がわかりました。余談ですが[pijul](https://pijul.org/)というのは新興バージョン管理システムの1つです。このファイルはpijulがバージョン0.1くらいのときに作ったもののようでした。その時のpijulはメタデータDBにひとまずでかいファイルを作るようになってたんですね。

ということでこのファイルを消して無事バックアップを取れました。

しかし4TBのファイルを作っているのに `du` は 150GBと報告しました。これはどういうことなのでしょうか。これが今日のお話。

# ディスク上のサイズとファイルサイズ

`du` のマニュアルを見ると、 `du` は **ディスク上のサイズ** 、いわば物理サイズを見積もるツールだと言っています。一方 `ls` などでみるのは **ファイルサイズ** 、論理的なサイズです。
両者に違いがあるかというと、[スパースファイル](https://wiki.archlinux.jp/index.php/%E3%82%B9%E3%83%91%E3%83%BC%E3%82%B9%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB)の場合実際のディスクサイズはずっと小さくなります。

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">Linuxだと0ページ最適化してくれるから4TBあってもディスクスペースは食わないんだけどそれをtarにすると4TBのtarが出来上がるんだろうか。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/989727168639848449?ref_src=twsrc%5Etfw">2018年4月27日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

ということで実験しましょう(終わった後で上のArch Wikiに全部書いてることに気づきましたが気にしない)。tarが出てきたのはduplicityがフォーマットとしてtarを用いているからです。

`truncate` コマンドでファイルを無意味に伸ばすことができます(普段はファイルをサイズ0に切り詰めるコマンドですね:) )

```console
$ truncate -s10M file
```

さて、これを `du` と `ls -l` で確認してみましょう。


```console
$ ls -lh file
-rw-r--r-- 1 kim kim 10M  4月 28 02:59 file
$ du -h
4.0K    .
```

見かけ上のサイズと実際のサイズが異なっていますね。これをtarballにしてみましょう。


```console
$ tar cf file.tar file
$ ls -lh
合計 11M
-rw-r--r-- 1 kim kim 10M  4月 28 02:59 file
-rw-r--r-- 1 kim kim 11M  4月 28 03:00 file.tar
$ du -h
11M     .
```

tarballにすると本来のサイズに膨らむことがわかりました。


そしてArch Wikiによると `tar` はオプションでsparse fileを扱えるようなのでそれも実験してみましょう。


```
$ rm file.tar
$ tar Scf file.tar file
$ ls -lh
合計 11M
-rw-r--r-- 1 kim kim 11M  4月 28 03:00 --sparse
-rw-r--r-- 1 kim kim 10M  4月 28 02:59 file
-rw-r--r-- 1 kim kim 10K  4月 28 03:01 file.tar
$ du -h
16K     .
```

本当に小さくなってますね。


# まとめ

* ext4などのファイルシステムはsparse fileを効率的に保存してるよ
* duがデフォルト表示するのはディスク上のサイズであってファイルサイズではないよ
* sparse fileを上手く扱えないとバックアップが元のディスクサイズより巨大になることがあるよ
