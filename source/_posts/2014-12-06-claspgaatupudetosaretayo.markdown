---
layout: post
title: "claspがアップデートされたよ"
date: 2014-12-06 23:29:46 +0900
comments: true
categories: [Lisp, Common Lisp, Advent Calendar]
---
κeenです。アドベントカレンダーめっちゃ書いてますね。
<!-- more -->
# Clasp!
[clasp](https://github.com/drmeister/clasp)のmasterに色々マージされました。リリースノートには、

>Clasp version 0.11
>* Added ASDF support.
>This is still alpha. Compile the ASDF module using (core:compile-asdf).
>After that you can load the module using (load "sys:kernel;asdf;build;asdf.bundle").
>It takes between 15-30 seconds to load (this is why I'm integrating Cleavir).
>* Added the :CLASP *feature* and removed the :ECL *feature*.
>Clasp will continue to mimic the underlying ECL functionality so that
>Common Lisp code that supports ECL can be made to support Clasp by converting
>#+ecl to #+(or ecl clasp) and #-ecl to #-(or ecl clasp)
>* Added code to generate object files directly from Clasp.
>The LLVM bitcode compiler "llc" no longer needs to be in the PATH
>for Clasp to generate object files from Common Lisp source.
>The "ld" linker does need to be accessible.

とあります。ASDFが使える！リリースノートには書いてませんがslimeサポートもmasterにコミットされてます。あとコミット読んだら最適化もされてるような…。

[以前](http://keens.github.io/slide/clasp.html)二十数秒掛かっていた`(fib 29)`ですが、今回はなんと

```
$ /usr/local/clasp/bin/clasp_boehm_o
Starting Clasp 0.11 ... loading image... it takes a few seconds
Top level.
> (load "fib.lisp")
1346269
real time : 34.294 secs
run time  : 38.844 secs
T
> (compile-file "fib.lisp")

#P"/home/kim/Lisp/fib.bc"
NIL
NIL
> (load "fib.bc")
1346269
real time : 21.355 secs
run time  : 25.785 secs
T
```

うわぁ。遅くなってる。

因みに.bcファイルはLLVMの中間ファイルなのでClaspとは独立に`opt -f -O3 fib.bc > fib.opt.bc`で最適化出来ます。それをやると

```
$ opt -f -O3 fib.bc > fib.opt.bc
$ /usr/local/clasp/bin/clasp_boehm_o
Starting Clasp 0.11 ... loading image... it takes a few seconds
Top level.
> (load "fib.opt.bc")
1346269
real time : 34.981 secs
run time  : 37.986 secs
T
```
あれ？遅くなった。

mpsはまだコンパイル中だから待ってね

# ASDFを使ってみる

```
$ /usr/local/clasp/bin/clasp_boehm_o
Starting Clasp 0.11 ... loading image... it takes a few seconds
Top level.
> (time (core:compile-asdf))
zsh: segmentation fault (core dumped)  /usr/local/clasp/bin/clasp_boehm_o
```

はい。解散。因みに50分くらいは動いてた。
