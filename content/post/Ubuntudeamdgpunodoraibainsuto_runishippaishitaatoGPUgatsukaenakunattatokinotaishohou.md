---
categories: [Linux, Ubuntu, amdgpu, 備忘録]
date: 2022-01-01T22:18:41+09:00
title: "Ubuntuでamdgpuのドライバインストールに失敗したあとGPUが使えなくなったときの対処法"
---

κeenです。このタイトルで正しく説明できてるか分かりませんが、ちょいちょい遭遇するのでメモ。

<!--more-->

手っ取り早く説明すると、dkmsのビルドに失敗するとamdgpuモジュールがblacklist行きになるので、blacklistから削除してあげると解決します。

```shell
sudo rm /etc/modprobe.d/blacklist-amdgpu.conf
```

このあと再起動するか、amdgpuのモジュールをロードしてあげます。

```shell
modprobe amdgpu
```

因みにamdgpuのカーネルモジュールがロードできているかどうかは `lsmod` で確認できます。以下のように `amdgpu` をはじめとしていくつかのカーネルモジュールがロードされていればOK。

```shell
$ lsmod | grep amdgpu
amdgpu               6389760  54
iommu_v2               24576  1 amdgpu
gpu_sched              36864  1 amdgpu
drm_ttm_helper         16384  1 amdgpu
ttm                    69632  2 amdgpu,drm_ttm_helper
drm_kms_helper        262144  1 amdgpu
drm                   561152  25 gpu_sched,drm_kms_helper,amdgpu,drm_ttm_helper,ttm
i2c_algo_bit           16384  2 igb,amdgpu
```


また、amdgpuのロードに失敗していると現象として `/dev` に以下のファイルがみつからなくなります。

```shell
$ ls -l /dev/kfd
crw-rw---- 1 root render 506, 0 12月 22 23:32 /dev/kfd
$ ls -l /dev/dri
合計 0
drwxr-xr-x  2 root root         80 12月 22 23:32 by-path
crw-rw----+ 1 root video  226,   0  1月  1 16:13 card0
crw-rw----+ 1 root render 226, 128 12月 22 23:32 renderD128
```

`/dev/dma_heap` もだったかな？

たまにひっかかっては思い出せなくなるのでメモでした。
