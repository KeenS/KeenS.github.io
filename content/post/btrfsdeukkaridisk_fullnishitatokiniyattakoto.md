---
categories: [btrfs, Linux]
date: 2022-08-01T01:06:58+09:00
title: "btrfsでうっかりdisk fullにしたときにやったこと"
---
κeenです。先日ついうっかりbtrfsのファイルシステムをdisk fullにしてしまったので、そこからリカバリしたときの記録を残しておきます。

<!--more-->

## 原因

一時ファイルでついうっかり250GBのファイルを作ってしまってディスクが満杯になりました。インターネットを調べるとでてくる知らないうちにメタデータが嵩んで…系じゃなくて普通にファイルが一杯のやつです。

btrfsはCopy on Writeなので単純にファイルを消しただけだとディスクスペースは恢復しなくて、ツリーからデータを削除しないとディスクスペースが空きませんでした。

因みに4ストレージ上でraid5を組んでます。

## 対処

色々がちゃがちゃやったので本当にこれが正解か分からないですが、以下の手順で恢復しました。

1. 巨大なファイルを消す
2. スナップショットを `btrfs subvolume delete` で全部消す
  + 巨大ファイルを含んでないスナップショットも消さないと何故かダメでした
  + 恐らくdefragのときにtreeの作り直しが走ってるから…？
3. スナップショットの変更を `btrfs subvolume sync` でコミットする
4. `btrfs filesystem defrag` で消したファイルを回収する


直感的には2, 3の手順で恢復してほしいんですが `defrag` までしないとだめでした。なんかdefragの副作用を使ってそうなので怪しいですが、ひとまず解決したのでよし。
