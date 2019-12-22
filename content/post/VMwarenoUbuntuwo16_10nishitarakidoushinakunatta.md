---
categories: [Linux, Ubuntu, VMware]
date: 2016-10-31T22:06:17+09:00
title: VMwareのUbuntuを16.10にしたら起動しなくなった
---

κeenです。普段はネイティブのUbuntuを使ってますが会社では使えないのでVMwareにUbuntuを入れて使っています。
今日そのUbuntuを16.04から16.10にアップグレードしたら起動しなくなったので対策メモ。

<!--more-->
現象は、Ubuntuを起動しようとするとゲストOSによってCPUがオフにされた旨の通知が来てフリーズします。
このリリースノート（[VMware Fusion 8.5.1 Release Notes](http://pubs.vmware.com/Release_Notes/en/fusion/8/fusion-851-release-notes.html)）を見れば分かりますが、Linux Kernel 4.7以上を使うと発生するバグのようです。

> Known Issues
> 
>     Virtual machines running Linux kernel version 4.7 or above may crash or hang on hosts with Intel CPUs
> 
>     Virtual Machines running Linux kernel version 4.7 or above may crash during installation or during power-on with a crash in the intel_powerclamp driver. This issue has been reported with Ubuntu 16.10, but it is a generic Linux kernel issue.
> 
>     Workaround: VMware is working with the OS vendors to have them fix their respective kernels. Until a fix is available, you can blacklist the intel_powerclamp driver so that the buggy code doesn't get loaded.
> 
>     To blacklist the driver:
> 
>     Add the kernel command-line option modprobe.blacklist=intel_powerclamp to the guest OS's default grub configuration or add it directly at the grub menu during boot.
> 


どうやらIntel CPUのホストだと特定のカーネルモジュールが誤動作してCPUをoffってしまうっぽい？OSベンダと協力して直していくと言っているのでVMwareのアップデートでは直らなそう。OSのアップデートはそもそも起動しないので出来ない、というかなりキツい状況です。
幸いにもワークアラウンドがあるのでそれを使って起動、修正します。

# Ubuntuの起動
誤動作するカーネルモジュールをロードしないようにしたら起動するっぽいです。

今OSがハングしている状態ならとりあえず殺して、 `ESC` キー(多分。もしかしたら `Shift` かも)を押しながらUbuntuを起動します。そうするとgrub menuの画面に遷移する筈です。

grub menuで（恐らく1番上にある）使用中のカーネルに選択肢を合わせた状態で`e`を押すと起動シーケンスをいじれるようになります。

Emacsっぽく操作出来る画面が出てくるので

```
	linux	/vmlinuz-4.4.0-45-generic root=/dev/mapper/ubuntu--vg-root ro  quiet splash $vt_handoff
```

のようにLinuxカーネルを起動してるっぽい行に


```
	linux	/vmlinuz-4.4.0-45-generic root=/dev/mapper/ubuntu--vg-root ro  quiet splash $vt_handoff modprobe.blacklist=intel_powerclamp
```

と `modprobe.blacklist=intel_powerclamp` を追加してあげれば起動します。

# 起動オプションの変更

とりあえず今のでワンショットで起動出来るようになったので起動オプションの設定を変更して次回以降も起動出来るようにしておきます。

`/etc/default/grub` をいじれば良いようです。

```
GRUB_CMDLINE_LINUX=""
```


の行をこれまた

```
GRUB_CMDLINE_LINUX="modprobe.blacklist=intel_powerclamp"
```

と `modprobe.blacklist=intel_powerclamp` を追加するように書き換えてあげて、

```
$ sudo update-grub2
```

でgrubに変更を反映してあげればOKです。

# その他
Ubuntu 16.10を1日使ってみましたが特に不自由なく使えています。
