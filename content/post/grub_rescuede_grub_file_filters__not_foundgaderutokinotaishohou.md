---
categories: ["Linux"]
date: 2019-10-21T18:14:12+09:00
title: "grub rescueで'grub_file_filters' not foundが出るときの対処方"
---
κeenです。[昔変な構成でRAID1を組んだ](https://keens.github.io/blog/2018/05/12/ubuntuderootfswobtrfsnoraid1insuto_ru/)のですが、その状態でUbuntuのアップグレードをしたらgrubが壊れたらしくどうにもならなくなったのでそれを復旧したときのメモです。

<!--more-->

grub rescueに落ちたら普段ならカーネルの場所さえ分かれば復帰できるんですが今回はgrub rescueが壊れてるようなのでそれもできません。

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">current status <a href="https://t.co/2yxwSMrdnL">pic.twitter.com/2yxwSMrdnL</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1186194005970706432?ref_src=twsrc%5Etfw">October 21, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 


色々調べた結果、これはgrubを再インストールする他ないようです。
外部から起動して、元のシステム内にchrootで入ってから ` grub-install` を叩きます。

という訳でUbuntuのインストールメディアをもってきて、そちらから起動します。

インストールメディアには「Ubuntuを試す」のオプションがあるのでそこからUbuntuに入りましょう。
そこからGNOME Terminalなどのターミナルを立ち上げます。

ここから先は記憶で書いているので注意です。

# デバイスを見付ける
rootが載っていたデバイスの他に、今挿したインストールメディアや、バックアップ用のディスクもあるのでgrubがrootが載っていたデバイスを探します。

色々方法があるかと思いますが、私は gpartedでパーティションを見て探します。
今回は `/sdb` と `/sdd` がそれだったみたいです。

# mdadm の復元
/boot を格納している筈の `/dev/md0` がありませんでした。なのでmd0を復元させます。これはコマンド1つでできるみたいです。

```console
$ mdadm --assemble --scan
```

これで `/dev/md0` ができました。

# root fsをマウントする

`/` はbtrfsなのでそれをマウントします。
RAIDを構成しているデバイスを全部列挙し、サブボリュームも指定してあげましょう。

```console
$ sudo mount -t btrfs -o device=/dev/sdb5,device=/dev/sdd5,subvol=@ /dev/sdb5 /mnt
```

# /boot をマウントする

普通にマウントすればいいです。

```console
$ sudo mount /dev/md0 /mnt/boot
```

# その他疑似ファイルシステムをマウントする

マウントしましょう。 See also [linux - mount dev, proc, sys in a chroot environment? - Super User](https://superuser.com/questions/165116/mount-dev-proc-sys-in-a-chroot-environment)
```console
$ sudo mount -t proc proc /mnt/proc
$ sudo mount --rbind /sys sys/
$ sudo mount --rbind /dev dev/
```

# chrootする

chrootします。

```
$ chroot /mnt /bin/bash
```

# grubの再インストール

前をブログのようにやればよいです。

```console
$ sudo grub-install /dev/sdb
$ sudo grub-install /dev/sdd
$ sudo update-grub
```

これであとは再起動するだけ。

ファイルシステムのマウントのところは人それぞれだと思うので各自で判断してやって下さい。
