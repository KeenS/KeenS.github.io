---
categories: [Linux, Ubuntu, btrfs, RAID1]
date: 2018-05-12T20:44:22+09:00
title: "UbuntuでrootfsをbtrfsのRAID1インストール"
---
κeenです。Ubuntu 18.04 LTSリリースに合わせてマシンを新調し、ついでにrootfsをRAID1にしたのでそのメモを。
因みに後述するようにブートに問題を抱えているので一般におすすめできるかは怪しいです。

<!--more-->

# 動機と構成
動機は単純で、普段あまり使わない技術を試してみたかったから。本当はRAID5をやりたかったのですがSSD2つしか用意しなかったのでRAID1で。

UbuntuのRAID1化にはmdadmというRAID化モジュールとファイルシステムによるRAID化があるようです。
面白いことをしたかったのでmdadmではなくファイルシステムによるものを選択。それなりに多機能でかつ興味のあったbtrfsを選択しました。


結果の構成は

* ブートローダ -- grub2を各ディスクに手作業でインストール
* / -- btrfsによるソフトウェアRAID1
* /boot -- ext2 + mdadmによるソフトウェアRAID1

というようにしました。/bootはブートローダからも読めるように保守的に単純なファイルシステムにしましたが/bootまでbtrfsにする例もあるようです。


# Ubuntuのインストール
公式のインストーラを使ってインストールしていきます。ここでは直接RAIDに関わることをしないので既存のシステムがある場合はここを飛ばしても構わないと思います。
インストール時点でRAID1化してしまって手間を省きたかったのですが叶わず。

言語は日本語を選択してキーボードもそのように。インストールオプションは失敗したらやり直すので最小インストール+アップデートなしが良いでしょう


ディスク構成はカスタムにし、インストールディスクを

* ディスク1
  * 512MB, ext2 /boot
  * 残り全部, btrfs /
* ディスク2
  * 512MB, ext2
  * 残り全部, btrfs

のようにします。要はRAID array全てで同じパーティション構成にします。

残りはインストーラの導きに従ってポチポチしていきます。

インストールが終わったら再起動しましょう。

# /bootのRAID1化
/と/bootは別々に作業した方がいいでしょう。起動しなかったときに問題の切り分けが難しいので

作業は概ね[このエントリ](https://centossrv.com/centos5-raid-2.shtml)の通りに進めると上手く行きます。

ただし/bootしか作業しないのとfdisk相当の処理をインストーラでやってしまったでかなり簡略化されます。

まずはUbuntuがinstallされて**いない**方のデバイスを準備します。

``` console
$ sudo apt install mdadm
# RAID1 の準備。なんかメタデータでwarning出るけど強行突破しても問題なさそう
$ sudo mdadm --create /dev/md0 --auto=yes --level=raid1 --raid-devices=2 missing /dev/sdb1
# 設定ファイルの生成
$ mdadm --detail --scan | sudo tee -a /etc/mdadm.conf
# ファイルシステムの作成
$ sudo mkfs.ext2 /dev/md0
# /bootをコピーするため作成したファイルシステムをmount
$ sudo mount /dev/md0 /mnt
# /bootをコピー
$ sudo rsync -avz -e ssh --delete /boot /mnt
# 新しいファイルシステムにinitramfsをインストールするため/bootを/dev/md0に入れ替え
$ sudo umount /mnt
$ sudo umount /boot
$ sudo mount /dev/md0 /boot
# ここでfstabを編集して /boot の起動デバイスを/dev/md0にする
$ vi /etc/fstab
# mdadmを使った構成をブートシーケンスに反映させる
$ sudo update-initramfs -u
```

因みに`update-initramfs`は失敗すると起動しなくなるコマンドなので盲目的に叩くのではなくて何をするコマンドなのかは調べてから使いましょう。
余談ですが私はこのタイミングで`/etc/fstab`のswapfsの行を削除して/swapのファイルを消しました。


そして再起動します。

## 再起動後
ここでは既にディスク2の/bootから起動しているはずです。ディスク1の/bootパーティションをこれのRAID1 Arrayに加えます。

``` console
$ sudo mdadm /dev/md0 --add /dev/sda1
```

最後にgrubをインストールします。

``` console
$ sudo install-grub2 /dev/ディスク1
$ sudo install-grub2 /dev/ディスク2
$ sudo update-grub
```

これで再起動して正常に起動したら/bootはmdadm化できました。

# / のRAID化
簡単ですが鬼門です。本来なら[ここ](https://btrfs.wiki.kernel.org/index.php/Using_Btrfs_with_Multiple_Devices)の通りに進めれば上手く行くはずです。

今は / はディスク1から読まれていて、ディスク2が空いてるはずです。これにディスク2を加えます。

btrfsの機能を使うのですぐに終わります。


``` console
$ sudo btrfs device add -f /dev/ディスク2 /mnt
$ sudo btrfs balance start -dconvert=raid1 -mconvert=raid1 /
```

これだけ。

…のはずですがinitramfsにバグがあるらしく、これだと起動時にディスク2が見つからずにエラーが出ます。
色々回避策を試行錯誤したのですが決定的なものは見つかりませんでした。一応観測した状況を書くと、

* カーネルのコマンドラインオプションに `rootfstype=btrfs rootflags=device=ディスク1,device=ディスク2` と書いても起動失敗
* もちろん同様にfstabをいじっても不能
* initramfsに落ちた後に **ディスク2を** マウントしようとするとマウントできる
  + ディスク1だとエラー
* initramfsに落ちた後に`btrfs device scan` をしてからディスク1をマウントすると通る
  + しかしinitramsが叩くスクリプトを編集してマウント前に`btrfs device scan`を発行するようにしてもエラー
* grubのメニューからコマンドラインオプションをいじって`root=ディスク2`とすると起動する
  + ディスク1だとエラー

ということで今安定して起動できる方法は「毎度grubのメニューにいって `root=ディスク2` を指定」になっています。
もうちょっとマシな方法があるやろという気もしますが私では見つけられませんでした。

上手くいったのかいってないのかわかりませんがこれで生活してます。どうにかブートシーケンスを改善できたらまた記事を書くことにします。

# トラブルシューティングの助け
## grub2のメニューに遷移
デフォルトだと暗黙にメニューを飛ばしてしまいます。
Shiftを押しながら起動時するとgrubのメニューにいくようです

## initramfsで落ちたときにbusyboxのコンソールにいく
Ubuntuの紫の画面の後に黒い画面にいったときの対処方法です。

Ctrl+Alt+F6,Ctrl+Alt+F1とタイプするとコンソールにいくようです。なぜこのシーケンスなのかはよくわかってないので遷移しないときはF7とか色々試してからCtrl+Alt+F1をタイプするとコンソールにいけるようです。
因みにUbuntuはデフォルトでCtrl+Alt+Fxでコンソール間を遷移できます。

## initramfsからrootfsにいく

適切なシーケンスを踏んでない(systemdとかを起動してない)ので修正作業用の一時処理です。

Ubuntuのインストーラでbtrfsでフォーマットすると裏で勝手に/用の`@`と/home用の`@home`の2つのsubvolumeを作るようです。

``` console
(initramfs) mount -o subvol=@ /dev/ディスク2 /root
(initramfs) mount -o subvol=@home /dev/ディスク2 /root/home
(initramfs) mount -o move /sys /root/sys
(initramfs) mount -o move /proc /root/proc
(initramfs) mount -o move /dev /root/dev
(initramfs) chroot root
bash # mdadm --assemble --scan
bash # mount /dev/md0 /boot
```


これでfstabやgrubやinitramfsを修正できます。

# 参考

* [非RAIDシステムのRAID化](https://centossrv.com/centos5-raid-2.shtml)
* [Using Btrfs with Multiple Devices](https://btrfs.wiki.kernel.org/index.php/Using_Btrfs_with_Multiple_Devices)
* [Btrfs/System Root Guide](https://wiki.gentoo.org/wiki/Btrfs/System_Root_Guide)
* [Ubuntu Weekly Recipe 第384回　Initramfsのしくみ](http://gihyo.jp/admin/serial/01/ubuntu-recipe/0384)
