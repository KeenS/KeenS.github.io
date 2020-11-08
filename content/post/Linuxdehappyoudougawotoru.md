---
categories: [Linux, 動画, SimpleScreenRecorder, Shotcut]
date: 2020-11-08T09:21:53+09:00
title: "Linuxで発表動画を撮る"
---

κeenです。
最近テックイベントに参加するにあたって発表動画を提出する機会があったので使用したツールを紹介します。

<!--more-->

私が使っているのはUbuntu 20.10ですが大抵のLinuxなら同様のことができると思います。
また、今回は右下にワイプで本人が喋ってる様子を出したりはしてません。
人間が口をパクパクさせてるのを見てもしょうがないので。
どうしても人間が口をパクパクさせてる姿が必要な方はスマホで撮って合成するなりして頑張って下さい。

# TL;DL

* [SimpleScreenRecorder](https://www.maartenbaert.be/simplescreenrecorder/)使って録画
* (機材の問題で)音声にトラブルがあったのでスマホで録音して [Shotcut](https://www.shotcut.org) で音声を合成
* ↑普通に[FFmpeg](https://ffmpeg.org)でもよかったわ

# 録画

スクリーンショットならGNOME Screenshotがあるのですが、録画ってやったことないなと思ったので探してみたらSimpleScreenRecorderがひっかかりました。

以下のコマンドでインストールできます。

```text
# apt install simplescreenrecorder
```

SSRで特に難しいところなく録画できるのですが、私のやった操作を共有します。
私のディスプレイは3840x2160あってそのままだと他人に共有したときに文字が潰れがちなので1920x1080の枠で録画します。
ちょうどスピーカーノートを開いて喋りたかったのでVideo inputをRecord a fixed rectangleにして、そこにスライドをピタリと合わせて撮りました。

![SimpleScreenRecorderの画面](/images/Linuxdehappyoudougawotoru/simplescreenrecorder.png)

私の場合はブラウザベースのスライドなのでサイズ調整が楽ですが、一般的なスライドツールだと全画面にしないといけないので画面の解像度を落として録画することになるかもしれません。

機材の問題で私の場合は音声をオフにしてますが、マイク入力もちゃんと録音できるので機材が揃ってればそのまま収録が完了します。

この画面あと出力の場所やフォーマットを選ぶ画面、録画画面と続きます。
プレビューとかも見れるので安心して録画できますね。

最終的には特に圧縮とかされてないmp4ができあがります。

# 録音

上手くいけばSSRで全て済むんですが、私の場合はマザボの不具合なのかデバイスドライバが合ってないのか、音声が途切れ途切れだったりノイズが入ったりしてまともに聞き取れるクオリティではありませんでした。
ちょっとPCだけではどうしようもなさそうだったので音声だけスマホで録音して、あとで合成することにしました。

スマホで適当なアプリ使って録音します。
PCの録画ボタンとスマホの録音ボタンを同時に押す運用で。

# 合成

録画と録音が終わったら動画編集ソフトを使って合成します。
Linuxで使えるツールはいくつかあるようなのですが、Shotcutを選びました。

以下のコマンドでインストールできます

``` text
# apt install shotcut
```

使用中の画面がこちら。

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">current status <a href="https://t.co/fSjEgZVXcH">pic.twitter.com/fSjEgZVXcH</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1322493933348413442?ref_src=twsrc%5Etfw">October 31, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

画面下にあるタイムラインにトラックを追加することで色々なメディアファイルを合成できます。

ファイルを開く→タイムラインにトラックを追加→「＋」ボタンで開いたファイルをトラックに追加でメディアを追加できます。今回はシンプルに動画と音声の合成なので映像トラック（画面中V1）と音声トラック（画面中V2）の2つがあります。

動画編集ソフトなのでもちろん切り抜いたりなんたりできますが、ただの発表動画なら難しいことは必要ないでしょう。

最後に「書き出し」で動画を作って完成です。
ちょっと分かりづらいんですが、動画の書き出しはジョブとしてバックグラウンドで動いてるっぽくてパッと見ボタンを押しても何も動いてないように感じるので注意です。

## FFmpeg

私の場合は動画編集ソフトの練習もしたかったのでShotcutを使いましたが、単純に音声と動画を合成したいだけならFFmpegで十分でしたね。

以下のコマンドでインストールできます。

``` text
# apt install ffmpeg
```


使い方も簡単で、録画と録音をインプットファイルに指定するだけです。

``` text
$ ffmpeg -i video.mp4 -i audio.m4a  output.mp4
ffmpeg version 4.3.1-4ubuntu1 Copyright (c) 2000-2020 the FFmpeg developers
  built with gcc 10 (Ubuntu 10.2.0-9ubuntu2)
  configuration: --prefix=/usr --extra-version=4ubuntu1 --toolchain=hardened --libdir=/usr/lib/x86_64-linux-gnu --incdir=/usr/include/x86_64-linux-gnu --arch=amd64 --enable-gpl --disable-stripping --enable-avresample --disable-filter=resample --enable-gnutls --enable-ladspa --enable-libaom --enable-libass --enable-libbluray --enable-libbs2b --enable-libcaca --enable-libcdio --enable-libcodec2 --enable-libdav1d --enable-libflite --enable-libfontconfig --enable-libfreetype --enable-libfribidi --enable-libgme --enable-libgsm --enable-libjack --enable-libmp3lame --enable-libmysofa --enable-libopenjpeg --enable-libopenmpt --enable-libopus --enable-libpulse --enable-librabbitmq --enable-librsvg --enable-librubberband --enable-libshine --enable-libsnappy --enable-libsoxr --enable-libspeex --enable-libsrt --enable-libssh --enable-libtheora --enable-libtwolame --enable-libvidstab --enable-libvorbis --enable-libvpx --enable-libwavpack --enable-libwebp --enable-libx265 --enable-libxml2 --enable-libxvid --enable-libzmq --enable-libzvbi --enable-lv2 --enable-omx --enable-openal --enable-opencl --enable-opengl --enable-sdl2 --enable-pocketsphinx --enable-libmfx --enable-libdc1394 --enable-libdrm --enable-libiec61883 --enable-nvenc --enable-chromaprint --enable-frei0r --enable-libx264 --enable-shared
  libavutil      56. 51.100 / 56. 51.100
  libavcodec     58. 91.100 / 58. 91.100
  libavformat    58. 45.100 / 58. 45.100
  libavdevice    58. 10.100 / 58. 10.100
  libavfilter     7. 85.100 /  7. 85.100
  libavresample   4.  0.  0 /  4.  0.  0
  libswscale      5.  7.100 /  5.  7.100
  libswresample   3.  7.100 /  3.  7.100
  libpostproc    55.  7.100 / 55.  7.100
Input #0, mov,mp4,m4a,3gp,3g2,mj2, from 'video.mp4':
  Metadata:
    major_brand     : isom
    minor_version   : 512
    compatible_brands: isomiso2avc1mp41
    encoder         : Lavf58.45.100
  Duration: 00:20:48.73, start: 0.000000, bitrate: 234 kb/s
    Stream #0:0(und): Video: h264 (High) (avc1 / 0x31637661), yuv420p(tv, bt709), 1920x1080 [SAR 1:1 DAR 16:9], 228 kb/s, 59.99 fps, 60 tbr, 15360 tbn, 120 tbc (default)
    Metadata:
      handler_name    : VideoHandler
Input #1, mov,mp4,m4a,3gp,3g2,mj2, from 'audio.m4a':
  Metadata:
    major_brand     : mp42
    minor_version   : 0
    compatible_brands: isommp42
    creation_time   : 2020-10-31T13:54:23.000000Z
    com.android.version: 11
  Duration: 00:20:48.65, start: 0.000000, bitrate: 97 kb/s
    Stream #1:0(eng): Audio: aac (LC) (mp4a / 0x6134706D), 44100 Hz, mono, fltp, 96 kb/s (default)
    Metadata:
      creation_time   : 2020-10-31T13:54:23.000000Z
      handler_name    : SoundHandle
Stream mapping:
  Stream #0:0 -> #0:0 (h264 (native) -> h264 (libx264))
  Stream #1:0 -> #0:1 (aac (native) -> aac (native))
Press [q] to stop, [?] for help
[libx264 @ 0x563cd2ea2c00] using SAR=1/1
[libx264 @ 0x563cd2ea2c00] using cpu capabilities: MMX2 SSE2Fast SSSE3 SSE4.2 AVX FMA3 BMI2 AVX2
[libx264 @ 0x563cd2ea2c00] profile High, level 4.2, 4:2:0, 8-bit
[libx264 @ 0x563cd2ea2c00] 264 - core 160 r3011 cde9a93 - H.264/MPEG-4 AVC codec - Copyleft 2003-2020 - http://www.videolan.org/x264.html - options: cabac=1 ref=3 deblock=1:0:0 analyse=0x3:0x113 me=hex subme=7 psy=1 psy_rd=1.00:0.00 mixed_ref=1 me_range=16 chroma_me=1 trellis=1 8x8dct=1 cqm=0 deadzone=21,11 fast_pskip=1 chroma_qp_offset=-2 threads=34 lookahead_threads=5 sliced_threads=0 nr=0 decimate=1 interlaced=0 bluray_compat=0 constrained_intra=0 bframes=3 b_pyramid=2 b_adapt=1 b_bias=0 direct=1 weightb=1 open_gop=0 weightp=2 keyint=250 keyint_min=25 scenecut=40 intra_refresh=0 rc_lookahead=40 rc=crf mbtree=1 crf=23.0 qcomp=0.60 qpmin=0 qpmax=69 qpstep=4 ip_ratio=1.40 aq=1:1.00
Output #0, mp4, to 'output.mp4':
  Metadata:
    major_brand     : isom
    minor_version   : 512
    compatible_brands: isomiso2avc1mp41
    encoder         : Lavf58.45.100
    Stream #0:0(und): Video: h264 (libx264) (avc1 / 0x31637661), yuv420p(progressive), 1920x1080 [SAR 1:1 DAR 16:9], q=-1--1, 60 fps, 15360 tbn, 60 tbc (default)
    Metadata:
      handler_name    : VideoHandler
      encoder         : Lavc58.91.100 libx264
    Side data:
      cpb: bitrate max/min/avg: 0/0/0 buffer size: 0 vbv_delay: N/A
    Stream #0:1(eng): Audio: aac (LC) (mp4a / 0x6134706D), 44100 Hz, mono, fltp, 69 kb/s (default)
    Metadata:
      creation_time   : 2020-10-31T13:54:23.000000Z
      handler_name    : SoundHandle
      encoder         : Lavc58.91.100 aac
frame=74925 fps=578 q=-1.0 Lsize=   46121kB time=00:20:48.70 bitrate= 302.6kbits/s dup=10 drop=0 speed=9.63x
video:33530kB audio:10587kB subtitle:0kB other streams:0kB global headers:0kB muxing overhead: 4.540816%
[libx264 @ 0x563cd2ea2c00] frame I:321   Avg QP:17.74  size: 52621
[libx264 @ 0x563cd2ea2c00] frame P:19077 Avg QP:15.53  size:   530
[libx264 @ 0x563cd2ea2c00] frame B:55527 Avg QP:16.40  size:   132
[libx264 @ 0x563cd2ea2c00] consecutive B-frames:  0.9%  0.8%  0.5% 97.9%
[libx264 @ 0x563cd2ea2c00] mb I  I16..4: 33.6% 51.8% 14.6%
[libx264 @ 0x563cd2ea2c00] mb P  I16..4:  0.1%  0.1%  0.1%  P16..4:  0.2%  0.0%  0.0%  0.0%  0.0%    skip:99.6%
[libx264 @ 0x563cd2ea2c00] mb B  I16..4:  0.0%  0.0%  0.0%  B16..8:  0.5%  0.0%  0.0%  direct: 0.0%  skip:99.4%  L0:63.4% L1:36.5% BI: 0.1%
[libx264 @ 0x563cd2ea2c00] 8x8 transform intra:48.8% inter:23.1%
[libx264 @ 0x563cd2ea2c00] coded y,uvDC,uvAC intra: 12.1% 5.6% 5.2% inter: 0.0% 0.0% 0.0%
[libx264 @ 0x563cd2ea2c00] i16 v,h,dc,p: 71% 24%  4%  0%
[libx264 @ 0x563cd2ea2c00] i8 v,h,dc,ddl,ddr,vr,hd,vl,hu: 51% 15% 33%  0%  0%  0%  0%  0%  0%
[libx264 @ 0x563cd2ea2c00] i4 v,h,dc,ddl,ddr,vr,hd,vl,hu: 44% 22% 11%  3%  3%  4%  4%  4%  4%
[libx264 @ 0x563cd2ea2c00] i8c dc,h,v,p: 95%  2%  2%  0%
[libx264 @ 0x563cd2ea2c00] Weighted P-Frames: Y:0.0% UV:0.0%
[libx264 @ 0x563cd2ea2c00] ref P L0: 75.4%  4.6% 13.9%  6.1%
[libx264 @ 0x563cd2ea2c00] ref B L0: 44.3% 55.2%  0.6%
[libx264 @ 0x563cd2ea2c00] ref B L1: 99.0%  1.0%
[libx264 @ 0x563cd2ea2c00] kb/s:219.96
[aac @ 0x563cd2ea1980] Qavg: 121.461
```

# おまけ： 動画の再生

何故かUbuntuには動画の再生ソフトがインストールされていません。

ですが、普通にブラウザでファイルを開いたら再生できます。

``` text
$ firefox output.mp4
```


あるいは有名なVLCでもいいですね。
以下のコマンドでインストールできます。

``` text
# apt install vlc
```

こっちだとnautilusでダブルクリックで開けます。

もちろん、コマンドラインからでも開けます。

``` text
$ vlc output.mp4
```

動画のファイル名がオーバーレイして表示されるのでちょっとビビります。
