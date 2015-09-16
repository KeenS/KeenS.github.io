---
categories: [xhyve, FreeBSD, Mac]
date: 2015-09-16T21:55:25+09:00
title: xhyveでFreeBSD guestを動かす
---

久しぶりですね。κeenです。最近はMacでもまともな仮想環境が整ってきたのでFreeBSDでも動かすかーといって試したやつを共有します。

<!--more-->
大部分は[ここ](http://blog.holidayworking.org/entry/2015/06/27/xhyve_%E3%81%A7_FreeBSD_%E3%82%92%E5%8B%95%E3%81%8B%E3%81%97%E3%81%A6%E3%81%BF%E3%81%9F)を参考にしてますが、ちゃんと動きます。

# xhyveとは
[xhyve – Lightweight Virtualization on OS X Based on bhyve | pagetable.com](http://www.pagetable.com/?p=831)


FreeBSDのBHyVeをMac OS Xに移植したものです。Intel CPUの仮想化フレームワークを使うことで仮想化の実装が（比較的）が楽になるからやろうぜってプロジェクトです。
カーネルレベルでのサポートが必要になりますがyosemiteからHyervisor.frameworkが入っているのでそれが使えます。

因みに開発はそんなに活発ではないです。

# 動かす

## xhyveの準備
FreeBSDのサポートは既にマージされているのでcloneしてきてそのまま使えます。

```sh
git clone git@github.com:mist64/xhyve.git
cd xhyve
make
```

FreeBSDのイメージの入手
イメージは現在10.2が出ているので

```sh
fetch http://ftp.freebsd.org/pub/FreeBSD/releases/VM-IMAGES/10.2-RELEASE/amd64/Latest/FreeBSD-10.2-RELEASE-amd64.raw.xz
unxz FreeBSD-10.2-RELEASE-amd64.raw.xz
```

で入手出来ます。

## 起動スクリプトの準備

参照ブログのままだとハードディスクが読めないのでIMG_HDDにもイメージを指定します。

名前は何でも良いのですが`freebsd.sh`として保存し、`chmod +x freebsd.sh`します。

```sh
#!/bin/sh

USERBOOT="test/userboot.so"
BOOTVOLUME="FreeBSD-10.2-RELEASE-amd64.raw"
KERNELENV=""

MEM="-m 1G"
#SMP="-c 2"
#NET="-s 2:0,virtio-net"
#IMG_CD="-s 3,ahci-cd,/somepath/somefile.iso"
IMG_HDD="-s 4,virtio-blk,$BOOTVOLUME"
PCI_DEV="-s 0:0,hostbridge -s 31,lpc"
LPC_DEV="-l com1,stdio"
#UUID="-U deadbeef-dead-dead-dead-deaddeafbeef"

build/xhyve -A $MEM $SMP $PCI_DEV $LPC_DEV $NET $IMG_CD $IMG_HDD $UUID -f fbsd,$USERBOOT,$BOOTVOLUME,"$KERNELENV"

```

## 起動

```sh
./freebsd.sh
```

で起動します。

# その他

FreeBSDのイメージを一杯作りたい人はダウンロードしてきたやつを`cp`して使うと良いと思います。
