---
categories: ["Ubuntu", "zfs"]
date: 2019-10-19T12:17:24+09:00
title: "Ubuntu 19.10でzfs root試してみた"
---

κeenです。Ubuntu 19.10出ましたね。Ubuntu 19.10からZFSをroot fsにしたインストールがサポートされるようになったので試しました。

<!--more-->

一番気になるのはライセンスですよね。
過去に何度もやるって言ってはライセンスで揉めて取り下げてます。
[この記事](https://news.mynavi.jp/article/20190808-874101/)によるとCanonicalは問題ないというスタンスだということです。本当に大丈夫なのかな…。


さて、インストールは特に難しいことはなくて、いつものパーティションどうする？と聞いてくるところでzfsの選択肢が出てきます。それを選択すればよいようです。


<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">やっていきー <a href="https://t.co/91g5cAwGKb">pic.twitter.com/91g5cAwGKb</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1185218284590813185?ref_src=twsrc%5Etfw">October 18, 2019</a></blockquote>

インストール後は、LTSの直前のリリースにしては何も問題がないくらい普通に動いてます。
よく壊れるSlack for Linux (β)も問題なく動いてます。

因みにDropboxは最近btrfsやzfsのサポートを再開したらしいです。
私もちゃんと動きました。Dropboxユーザも安心してzfsを試せますね。

<blockquote class="twitter-tweet" data-conversation="none" data-dnt="true"><p lang="ja" dir="ltr">ところでDropboxがbtrfsとかzfsとかのサポート再開してたの知らなかった<a href="https://t.co/8PV6yA7Bdq">https://t.co/8PV6yA7Bdq</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1185230069242875904?ref_src=twsrc%5Etfw">October 18, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

さて、少しだけの機能をzfsを使ってみましょう。

zfsのプールはこのように切られているようです。

```console
$ zpool list
NAME    SIZE  ALLOC   FREE  CKPOINT  EXPANDSZ   FRAG    CAP  DEDUP    HEALTH  ALTROOT
bpool  1.88G  66.1M  1.81G        -         -      -     3%  1.00x    ONLINE  -
rpool   460G  15.3G   445G        -         -     0%     3%  1.00x    ONLINE  -
$ zfs list bpool
NAME    USED  AVAIL     REFER  MOUNTPOINT
bpool  65.8M  1.69G      176K  /boot
$ zfs list rpool
NAME    USED  AVAIL     REFER  MOUNTPOINT
rpool  15.3G   430G       96K  /
```

ちょっとスナップショットを試してみましょう。

適当なファイルを用意します。

``` console
$ mkdir tmp
$ cd tmp
$ echo hogehoge > text.txt
```

スナップショットを取りましょう。
`rpool/USERDATA/shun_2meu5j` がユーザのホームディレクトリを指しているようです。
そこのスナップショットを `test` という名前で撮ります。

``` console
$ sudo zfs snapshot rpool/USERDATA/shun_2meu5j@test
```

`zfs list` に `-t snapshot` を渡すとスナップショットの一覧が取れます。

``` console
$ zfs list -t snapshot
NAME                              USED  AVAIL     REFER  MOUNTPOINT
rpool/USERDATA/shun_2meu5j@test   332K      -     11.2G  -
```

スナップショットのリネームもできるらしいです。

``` console
$ sudo zfs rename rpool/USERDATA/shun_2meu5j@test backup
$ zfs list -t snapshot
NAME                                USED  AVAIL     REFER  MOUNTPOINT
rpool/USERDATA/shun_2meu5j@backup   648K      -     11.2G  -
```

さて、これでrollbackも試してみましょう。
適当に用意したファイルの内容を上書きます。

``` console
$ echo fugafuga > text.txt
```

そしてロールバック

``` console
$ sudo zfs rollback rpool/USERDATA/shun_2meu5j@backup
```

これで中身を見ると変更が差し戻されているのが分かります。すごい。

``` console
$ cat text.txt
hogehoge
```

因みにrollbackしても該当のスナップショットは消えません。
該当のスナップショット以降に撮ったスナップショットは消えるらしいです。

``` console
$ zfs list -t snapshot
NAME                                USED  AVAIL     REFER  MOUNTPOINT
rpool/USERDATA/shun_2meu5j@backup   468K      -     11.2G  -
```

最後にスナップショットを消しましょう。 `zfs destroy` でできるらしいです。

``` console
$ sudo zfs destroy rpool/USERDATA/shun_2meu5j@backup
$ zfs list -t snapshot
no datasets available
```

今回試したのはノートパソコンだったのでホットプラグとかは試せませんでした。
詳しくはFSとかに詳しそうな人のブログをあたって下さい。


