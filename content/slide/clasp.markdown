---
type: slide
title: "claspを少し触ってみた"
date: 2014-10-28
aliases:
    - /slide/clasp.html
categories: [Lisp, Common Lisp, clasp, Lisp Meet Up]
description: "Lisp Meet Up #21 の発表資料です。<br>
最近リリースされたCommon Lispの処理系Claspについて。<br>
<br>
<br>
"
---
<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">

# claspを少し触ってみた
----------------------
Lisp Meet Up presented by Shibuya.lisp #21  
2014-10-29  
κeen(@blackenedgold)

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:relative;right 0;" -->

 + κeen
 + 東大数学科の4年生
 + ソーシャルアカウントは上のアイコン達から。
 + Lisp, Ruby, OCaml, Shell Scriptあたりを書きます


# clasp
-------

* Github: [drmeister/clasp](https://github.com/drmeister/clasp)
* 2014-09-18に0.1リリース(現在0.11プレビューが出ている)
* 今は0.11プレビューが出ている
* ANSI Common Lisp準拠を目指す(現在80 ~90%)
* ECLからのフォーク
* LLVMベースのJIT([MCJIT](http://llvm.org/docs/MCJITDesignAndImplementation.html))搭載
* C++とLispで書かれている
* C++との連携を意識

Note:
C++との連携の部分を強調。今回は0.11プレビューでの話


# メモリ管理
------------------

* Memory Pool Systemと Boehm GCから選べる
* ビルドするときにどっちかを選ぶ


# メモリ管理
------------------
## [Boehm GC](http://www.hboehm.info/gc/)

* Mark &amp; Sweepのライブラリ
* 枯れた有名なライブラリ
* Lisp有名どころではGaucheが使う
* RedHat系だとこれを使ってないとrpmリジェクトされやすいらしい(?)

Note:
他にはMozilla, W3M, GNU GCJ, GNU Obj-Cなどなど

# メモリ管理
------------------
## [Memory Pool System](http://www.ravenbrook.com/project/mps/)

* 複数のGCアルゴリズムを組み合わせて使えるらしい
* since 1994
* あまり分からないです&gt;&lt;


# 依存ライブラリ
----------------------

* LLVM **3.6**
* LLVM/clang **3.5** compiler
* Boost build v2
* boost libraries ver 1.55
* Boehm 7.2
* gmp-6.0.0
* expat-2.0.1
* zlib-1.2.8
* readline-6.2


<span style="font-size:600%">無理</span>


<!-- .slide: class="center" -->

# externals-clasp
--------------------------

* Github: [drmeister/externals-clasp](https://github.com/drmeister/externals-clasp)
* 依存ライブラリを揃えるのが難しいとの声から作られた
* 依存ライブラリを全てビルド<!-- .element: class="fragment grow" -->


<span style="font-size:300%">＼LLVMをビルド／</span>


<!-- .slide: class="center" -->

<span style="font-size:300%">＼Boostをビルド／</span>

<!-- .slide: class="center" -->

<span style="font-size:200%">ビルド時間はお察しです</span>

<!-- .slide: class="center" -->

# 本体のビルド
------------------
Twitter実況をどうぞ↓↓

# 本体のビルド
------------------
<blockquote class="twitter-tweet" lang="ja"><p>clangがメモリ6GB以上食い続けてて怖い。何やってんの。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/526417151486205952">2014, 10月 26</a></blockquote>

# 本体のビルド
------------------
<blockquote class="twitter-tweet" lang="ja"><p>clangの起動時間1時間超えてますよ…</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/526418636735401984">2014, 10月 26</a></blockquote>

# 本体のビルド
------------------
<blockquote class="twitter-tweet" lang="ja"><p>HDDプチプチ言ってるし今にも壊れそう</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/526420001587068929">2014, 10月 26</a></blockquote>

# 本体のビルド
------------------
<blockquote class="twitter-tweet" lang="ja"><p><a href="https://twitter.com/nobkz">@nobkz</a> claspのコンパイル中です。&#10;clangなのにIOネックという謎の状態です。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/526420473232367616">2014, 10月 26</a></blockquote>

# 本体のビルド
------------------
<blockquote class="twitter-tweet" lang="ja"><p>clangの消費メモリ7GB超えた</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/526423982992420864">2014, 10月 26</a></blockquote>

<span style="font-size:300%">ビルドは自己責任で</span>

<!-- .slide: class="center" -->

# 使ってみる
------------
## 起動オプション
```
/usr/local/clasp/bin/clasp_mps_o --help
clasp options
-I/--ignore-image    - Don't load the boot image/start with init.lsp
-i/--image file      - Use the file as the boot image
-N/--non-interactive - Suppress all repls
-v/--version         - Print version
-s/--verbose         - Print more info while booting
-f/--feature feature - Add the feature to *features*
-e/--eval {form}     - Evaluate a form
-l/--load {file}     - LOAD the file
-r/--norc            - Don't load the ~/.clasprc file
-n/--noinit          - Don't load the init.lsp (very minimal environment)
-s/--seed #          - Seed the random number generator
-- {ARGS}*           - Trailing are added to core:*command-line-arguments*
```

# 使ってみる
------------
## ASDF

* <!-- .element: class="fragment" data-fragment-index="1" -->動かない
  + <!-- .element: class="fragment" data-fragment-index="2" -->ASDFの処理系依存の部分(`#+`)の問題
  + <!-- .element: class="fragment" data-fragment-index="2" -->ECLと見做されるけどECLのコードは動かない

# 使ってみる
------------
## cl-ppcre

* [手動ロードスクリプト](https://gist.github.com/KeenS/4e25cb6e424ebe4e7a4a)
* <!-- .element: class="fragment" data-fragment-index="1" -->動かない
  + <!-- .element: class="fragment" data-fragment-index="2" -->`schar`がない
  + <!-- .element: class="fragment" data-fragment-index="2" -->ANSI Common Lispにあるのに…

# 使ってみる
------------
## C++との連携

* <!-- .element: class="fragment" data-fragment-index="1" -->ドキュメントがない
* <!-- .element: class="fragment" data-fragment-index="2" -->サンプルコードもない
* <!-- .element: class="fragment" data-fragment-index="3" -->遂行不能

# 使ってみる
------------
## LLVMの呼び出し
* [clasp/src/llvmo/demo.lisp](https://github.com/drmeister/clasp/blob/master/src/llvmo/demo.lsp)
* <!-- .element: class="fragment" data-fragment-index="1" -->動かない

# 使ってみる
------------
<style type="text/css">
.graph{
  background:#aaa;
  border-radius:5px;
  white-space: nowrap;
  text-align: left;
}
td {
  white-space: nowrap;
}
</style>

処理系              |  `(time (fib 29))`
--------------------|-----------------------------------------------------------------
clasp-0.1(boehm)    | <div class="graph fragment" style="width:calc(264.3px * 3);" data-fragment-index="1">26.43s</div>
clasp-0.1(mps)      | <div class="graph fragment" style="width:calc(172.8px * 3);" data-fragment-index="1">17.28s</div>
clasp-0.11(boehm)   | <div class="graph fragment" style="width:calc(213.2px * 3);" data-fragment-index="2">21.32s</div>
clasp-0.11(mps)     | <div class="graph fragment" style="width:calc(187.9px * 3);" data-fragment-index="2">18.79s</div>
ECL-13.5.1          | <div class="graph" style="width:calc( 16.0px * 3);">1.603s</div>
ECL-13.5.1(compile) | <div class="graph" style="width:calc(  1.9px * 3);">0.192s</div>
ABCL-1.3.1          | <div class="graph" style="width:calc( 32.9px * 3);">3.292s</div>
ABCL-1.3.1(compile) | <div class="graph" style="width:calc(  2.4px * 3);">0.241s</div>


# 使ってみる
------------

処理系              |  `(time (fib 29))`
--------------------|-----------------------------------------------------------------
CLISP-2.49          | <div class="graph" style="width:calc( 38.4px * 3);">3.847s</div>
CLISP-2.49(compile) | <div class="graph" style="width:calc(  7.1px * 3);">0.7146s</div>
ccl-1.10            | <div class="graph" style="width:calc(  0.4px * 3);">0.04033s</div>
sbcl-1.2.5          | <div class="graph" style="width:calc(  0.6px * 3);">0.06469s</div>
alisp-9.0           | <div class="graph" style="width:calc(  230px * 3);">23.09s</div>
alisp-9.0(compile)  | <div class="graph" style="width:calc(  0.6px * 3);">0.06194s</div>

<span style="font-size:200%">LLVM/JITなのになぜ遅い？</span>

<!-- .slide: class="center" -->

# LLVM/JITで遅い？
-------------------------

* LLVMって速いんじゃ？
* JIT搭載した○○が速いって聞いたよ
  + JVM
  + lua-jit
  + Rubinius
  + pypy


# 言葉の罠: LLVM
--------------------

* 遅いコードはどんなに頑張っても遅い
* ECLのバックエンドにClang(=LLVM)を使ってもSBCLに勝てないのと同じ


# 言葉の罠: JIT搭載
--------------------

* JIT(実行時コンパイル)の意味は広い
* 実行時にネイティブコードを吐けばJITと言える
* claspは実行直前にコンパイルするだけ
* 多くの速いJIT処理系はTracing JITを使う
  + またの名を適応的コンパイル
  + 実行時の情報に基いて実行中に最適化する
  + `(declare ...)` を自動生成してる的な


# Tracing JITへの道
--------------------

* LLVMのJITは実行直前にコンパイルするだけ
  + <!-- .element: class="fragment" data-fragment-index="1" -->Tracing JITのバックエンドには使える
* 実行時最適化には最適化用のコードが必要
  + <!-- .element: class="fragment" data-fragment-index="2" -->Common Lispでは`(declare ...)`用のコードを使い回せる
* <!-- .element: class="fragment" data-fragment-index="3" -->案外近い


# 結論
------

* <!-- .element: class="fragment" data-fragment-index="1" -->遅い
* <!-- .element: class="fragment" data-fragment-index="2" -->時期尚早
* <!-- .element: class="fragment" data-fragment-index="3" -->今後機能や速度改善があれば使えるようになるかも


<span style="font-size:600%">以上</span>  
何か質問あればどうぞ

<!-- .slide: class="center" -->
</script>
</section>
