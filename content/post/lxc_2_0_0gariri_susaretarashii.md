---
categories: [Linux, Ubuntu, 仮想化, コンテナ, lxd, lxc]
date: 2016-04-14T22:51:20+09:00
title: LXDがリリースされたらしい
---

κeenです。頭痛い。LXDがリリースされたようなので1つ記事をば。
LXDはLinuxで動く軽量仮想化コンテナ、LXCのラッパでREST APIとCLIが提供されています。
<!--more-->

LXC、LXD共にUbuntuの開発元、Canonicalが開発しています。
LXCについては初期のDockerがバックエンドに使っていたので知名度もそこそこあるかと思いますが、
そのラッパであるLXDはこの度ようやく正式リリースされました。

既にDockerがあるのになぜわざわざ新たにLXDを使うんだって気もしますが、対象とするレイヤーが違います。
Dockerはアプリケーションコンテナ、つまりアプリケーションを動かすためのプロセスより強い分離環境を提供するために使われますが
LXDはシステムコンテナ、つまり仮想マシンより軽い仮想化環境を提供します。
なのでファイルシステムはCopy on Writeしませんし一度終了したコンテナは破棄されず、再起動出来ます。
詳しくはここら辺に書いてあります。

[LXD 2.0: Blog post series [0/12] | Stéphane Graber's website](https://www.stgraber.org/2016/03/11/lxd-2-0-blog-post-series-012/)


1つDockerとLXDの違いの分かりやすい例を出すと、それぞれのOpenStackでの立ち位置を見ると面白そうです。
Dockerは[OpenStack Magnum](https://wiki.openstack.org/wiki/Magnum)でOpenStack上で扱えますが、OpenStackのコンピュートノード **上** で動作します。
一方LXDは[nova-compute-lxd](https://insights.ubuntu.com/2015/05/06/introduction-to-nova-compute-lxd/)でOpenStack上で扱えますが、 OpenStackのコンピュートノード **として** 動作します。
なんだかんだ重いVMがコンテナのお陰で軽くなる訳です。そして勿論のこと、Docker in LXDなんかも出来るのでちゃんと共存出来ます。


今のところUbuntu 14.04あるいは未来の16.04でリリースされているようです。Ubuntu 15.10をお使いの方は

```
add-apt-repository ppa:ubuntu-lxc/lxd-stable
apt-get update
apt-get dist-upgrade
apt-get install lxd
```

でインストール出来るようです([マニュアル](https://linuxcontainers.org/ja/lxd/getting-started-cli/)より)。
LXDはLXCのラッパでありながらもliblxcにしか依存しないのでlxc自体はインストールする必要はありません。

さてこれでLXDがインストールされた訳ですが、LXDはコンテナなどのリソースを管理するデーモンとそのCLIから成ります。
CLIのコマンド名はなんと`lxc`です。LXCのコマンド群が`lxc-*`なので非常に勘違いしやすいですね。

詳しいコマンド群は[マニュアル](https://linuxcontainers.org/ja/lxd/getting-started-cli/)を読めばいいのですがそれだけだとお粗末なので多少紹介します。

インストール直後は新規に作ったグループを今のログインセッションに反映させる必要があるので

```
newgrp lxd
```

とする必要があります。その次は初期化が必要になります。`lxd init`です。いくつか質問されますがデフォルトの回答を選んでいけば問題なさそうです。

```
$ sudo lxd init
Name of the storage backend to use (dir or zfs): zfs
error: The requested backend 'zfs' isn't available on your system (missing tools).
Name of the storage backend to use (dir or zfs): dir
Would you like LXD to be available over the network (yes/no)? yes
Address to bind LXD to (not including port): 0.0.0.0
Port to bind LXD to (8443 recommended): 
Invalid input, try again.

Port to bind LXD to (8443 recommended): 8443
Trust password for new clients: 
Again: 
Do you want to configure the LXD bridge (yes/no)? yes
Warning: Stopping lxd.service, but it can still be activated by:
  lxd.socket
LXD has been successfully configured.

```

Ubuntuでzfsが使えるのは16.04からなので15.10では使えませんでした。また、ブリッジのコンフィギュアにはウィザードが出てきます。

これで初期化が終わり、コンテナを立ち上げる準備が出来ました。

コンテナはイメージを元に立ち上げるのですが、今回はイメージはネット上のものから立ち上げます。
インストール時点でいくつかサーバが登録されているので名前で指定してあげるだけでよいようです。
他にイメージを使う方法はリモートのLXDをサーバとして使う、手動でファイルからインポートするなどがあるようです。

ということでコンテナの立ち上げはこうです。折角なので未来のUbuntu 16.04を使ってみましょう。

```
sudo lxc launch ubuntu:16.04 future-ubuntu
```

このコマンドでビルトインの"Ubuntu"サーバにある16.04バージョンのイメージをfuture-ubuntuという名前のコンテナで立ち上げます。

これでコンテナが立ち上がりました。確認してみましょう。

```
sudo lxc list
+---------------+---------+--------------------------------+----------------------------------------------+------------+-----------+
|     NAME      |  STATE  |              IPV4              |                     IPV6                     |    TYPE    | SNAPSHOTS |
+---------------+---------+--------------------------------+----------------------------------------------+------------+-----------+
| future-ubuntu | RUNNING | 10.197.202.251 (eth0)          | fdc:d0ef:6a90:89f5:216:3eff:fe42:fb45 (eth0) | PERSISTENT | 0         |
|               |         | 10.0.3.1 (lxcbr0)              |                                              |            |           |
+---------------+---------+--------------------------------+----------------------------------------------+------------+-----------+
```

ついでにローカルにあるイメージも確認しましょう。インポートまで終わっているようです。

```
sudo lxc image list
+-------+--------------+--------+-------------------------------------------+--------+----------+------------------------------+
| ALIAS | FINGERPRINT  | PUBLIC |                DESCRIPTION                |  ARCH  |   SIZE   |         UPLOAD DATE          |
+-------+--------------+--------+-------------------------------------------+--------+----------+------------------------------+
|       | 6cb0ba80a5fe | no     | ubuntu 16.04 LTS amd64 (beta2) (20160322) | x86_64 | 140.43MB | Apr 14, 2016 at 3:55pm (UTC) |
+-------+--------------+--------+-------------------------------------------+--------+----------+------------------------------+
```

立ち上げたコンテナにログインしてみましょう。これはDockerをお使いの方は馴染があるんじゃないでしょうか。

```
sudo lxc exec future-ubuntu -- /bin/bash
root@future-ubuntu:~# lsb_release -a
No LSB modules are available.
Distributor ID:	Ubuntu
Description:	Ubuntu Xenial Xerus (development branch)
Release:	16.04
Codename:	xenial
root@future-ubuntu:~# exit
```

ちゃんと16.04になっているようです。

コンテナの停止は

```
sudo lxc stop future-ubuntu
```

です。ちゃんと止まったか確認してみましょう。

```
sudo lxc list
+---------------+---------+------+------+------------+-----------+
|     NAME      |  STATE  | IPV4 | IPV6 |    TYPE    | SNAPSHOTS |
+---------------+---------+------+------+------------+-----------+
| future-ubuntu | STOPPED |      |      | PERSISTENT | 0         |
+---------------+---------+------+------+------------+-----------+
```

ちゃんとSTATEがSTOPPEDになってますね

もう一度起動してみます

```
sudo lxc start future-ubuntu
sudo lxc list
+---------------+---------+--------------------------------+----------------------------------------------+------------+-----------+
|     NAME      |  STATE  |              IPV4              |                     IPV6                     |    TYPE    | SNAPSHOTS |
+---------------+---------+--------------------------------+----------------------------------------------+------------+-----------+
| future-ubuntu | RUNNING | 10.197.202.251 (eth0)          | fdc:d0ef:6a90:89f5:216:3eff:fe42:fb45 (eth0) | PERSISTENT | 0         |
|               |         | 10.0.3.1 (lxcbr0)              |                                              |            |           |
+---------------+---------+--------------------------------+----------------------------------------------+------------+-----------+
```

次はコンテナを削除します

```
sudo lxc stop future-ubuntu
sudo lxc delete future-ubuntu
sudo lxc list
+------+-------+------+------+------+-----------+
| NAME | STATE | IPV4 | IPV6 | TYPE | SNAPSHOTS |
+------+-------+------+------+------+-----------+
sudo lxc image list
+-------+--------------+--------+-------------------------------------------+--------+----------+------------------------------+
| ALIAS | FINGERPRINT  | PUBLIC |                DESCRIPTION                |  ARCH  |   SIZE   |         UPLOAD DATE          |
+-------+--------------+--------+-------------------------------------------+--------+----------+------------------------------+
|       | 6cb0ba80a5fe | no     | ubuntu 16.04 LTS amd64 (beta2) (20160322) | x86_64 | 140.43MB | Apr 14, 2016 at 3:55pm (UTC) |
+-------+--------------+--------+-------------------------------------------+--------+----------+------------------------------+
```

コンテナが消えただけでイメージは残るんですね。

さて、いかがでしょうか。軽量なVMと考えれば非常に手軽で中々面白いんじゃないでしょうか。
因みにコンテナとホスト間でのファイルのやりとりも簡単(`lxc file pull/push`)なようなのでコンテナ内でコンパイラを動かしてホストにもってくるとかをすれば開発環境の隔離環境としても使えそうです。

時間があればもう少し詳細に検証してみます。SNAPSHOTとかも試してみたかった。それでは今日はこの辺で。
