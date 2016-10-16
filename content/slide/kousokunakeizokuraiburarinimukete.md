---
categories: [継続, 限定継続, Lisp, Common Lisp]
date: 2016-05-08T21:26:32+09:00
description: "継続勉強会向け
http://connpass.com/event/28150/"
title: 高速な継続ライブラリに向けて
---

<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# 高速な継続ライブラリに向けて
----------------------
[継続勉強会](http://connpass.com/event/28150/) 2016-05-22
<!-- .slide: class="center" -->

===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバーエージェントのエンジニア
 + Lisp, ML, Rust, Shell Scriptあたりを書きます
===
# 継続欲しい
-----------

* 色々な場面で便利
* Schemeで使い回してるのうらやましい
* Common Lispでも使いたい
* 現実には限定継続が欲しい
  +  Common Lispには大域脱出はある

===
# 限定継続を使う例
-----------------
## 非同期プログラミング

* コールバック形式だと厳しい
* 限定継続を使うと綺麗に書き直せる

===
# 限定継続を使う例
-----------------
## ゲームのコルーチン

* 複数のオブジェクトを制御するのにコルーチンが欲しい
* cf [コルーチンをCommon Lispで簡単に定義 - さくらんぼのlambda日記](http://lambdasakura.hatenablog.com/entry/20111026/1319598590)

===
# 限定継続を使う例
-----------------
## do記法

* モナドのdo記法は限定継続を使って実装出来る
* [Operational monad in scheme](http://www.slideshare.net/yuichinishiwaki/operational-monad-in-scheme)


===
# Common Lispでの限定継続の実現
---------------

1. 仕様に入れてもらう
2. 処理系に手を入れる
3. ユーザレベルで(限定)継続ライブラリを作る
   + 柔軟なCommon Lispでは可能

===
# CPS変換
---------

* (限定)継続の実現方法の1つ
  + スタックを切り取る方式とかもある
* 機械的にも出来る
* グローバルな変換なのとプリミティブな式しか書けないでコンパイラ内部でやることが多い
  + 継続関係なしに中間形式として採用されることが多い
* 関数定義/呼び出し以外にも諸々の構文とかに対しても定義が必要

===
# CPS変換
----------

Q. Common Lispだといくつの構文に対して定義が必要?

1. 1つ
2. 26つ
3. 42つ
4. 無数


===
# CPS変換
----------

A. 26つ (スペシャルフォーム25+funcall)

===
# Common Lispのプリミティブ
--------------------------

* スペシャルフォームと呼ばれる
* 仕様で25個定められている
* [CLHS: Section 3.1.2.1.2.1](http://www.lispworks.com/documentation/HyperSpec/Body/03_ababa.htm)
* この中に関数定義だとか例外だとかは入っていない
  + マクロで定義されている

===
# マクロ
--------

* 構文木 to 構文木(S式to S式)変換器( = 普通のLispの関数)
* 新しい構文を作れる
* CPS変換は?????

===
# `macroexpand`
-------------

* [CLHS: Function MACROEXPAND, MACROEXPAND-1](http://clhs.lisp.se/Body/f_mexp_.htm)
* マクロを手動展開する関数
* 雑にいうと普段pre-orderなマクロ展開をin-orderやpost-orderにする時に使う
* 本来はあまり使いたくない
  + 処理系の展開器に任せた方が間違いが少ない
* これでマクロを排したプリミティブのCommon Lispの構文木にアクセス出来る

===
# cl-cont
---------

* 上記のことを全てやったライブラリ
* デファクトというか唯一のライブラリ
* [Common Lispで限定継続と遊ぶ | κeenのHappy Hacκing Blog](http://keens.github.io/slide/Common_Lispdegenteikeizokutoasobu_/)

===
<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">「shift/resetがわからない時にあげる声」</p>&mdash; かず(原材料に小麦粉を含む) (@kazzna) <a href="https://twitter.com/kazzna/status/674026894602309632">2015年12月8日</a></blockquote>

<!-- .slide: class="center" -->

===
# cl-contの使用例

``` common-lisp
(with-call/cc
  (+ 1 (call/cc
        (lambda (k)
          (funcall k 2)))))
```

===
# cl-contの使用例

``` common-lisp
(FUNCALL
 (LAMBDA (&OPTIONAL #:G542 &REST #:G543)
   (DECLARE (IGNORABLE #:G542))
   (DECLARE (IGNORE #:G543))
   (FUNCALL
    (LAMBDA (&OPTIONAL #:G544 &REST #:G545)
      (DECLARE (IGNORABLE #:G544))
      (DECLARE (IGNORE #:G545))
      (FUNCALL (LAMBDA (K) (FUNCALL K 1))
               (LAMBDA (&OPTIONAL #:G546 &REST #:G547)
                 (DECLARE (IGNORABLE #:G546))
                 (DECLARE (IGNORE #:G547))
                 (FUNCALL (CL-CONT::FDESIGNATOR-TO-FUNCTION/CC #:G542) #'VALUES
                          #:G544 #:G546))))
    1))
 #'+)
```


===
# cl-contへの不満
-----------------

* 遅い
* lambda多い。
  + 多分コンパイラと相性が悪い
* lambda禁止おじさんと分かりあえる

===
# cl-fast-cont


<!-- .slide: class="center" -->
===
# cl-fast-cont
--------------

* [KeenS/cl-fast-cont: faster partial contiuation library of common lisp](https://github.com/KeenS/cl-fast-cont)
* とりあえずレポジトリ作っただけ
* 完成させたい…

===
# アプローチ1
<!-- .slide: class="center" -->

===
# SSA使う
---------

* CPSと等価
* だけどSSAだったらlambda出てこない
* Common Lispならgotoあるしいけるんじゃね？

===
```common-lisp
(let (x y z)
 (tagbody
    (setq x 1)
  :call/cc
    (setq y 1)
    (setq z (+ x y))))
```


===
# 問題
-------

* ネイティブスタックとは別に自分でスタック作らないといけない
  + 例外とかでスタック巻き戻されるとつらい
* gotoのタグをtagbodyの外に持ち出せない(=継続を外に持ち出せない)
* 変数を準備するのが面倒orパフォーマンスに影響しそう
* そもそもtagbodyそこまで柔軟じゃなかった
* 関数が消し飛ぶ

===
# アプローチ2
<!-- .slide: class="center" -->

===
# SSA+CPS
---------

* SSAとCPSを組み合わせる
* 基本はSSA
* スタックを使う/継続が必要な所でだけCPS
===
# 問題
-------

* SSAの部分意味なくね？
* そもそも継続を取り出すのが目的なので関係ない所で変換しても意味がない

===
# アプローチ3
<!-- .slide: class="center" -->
===
# Selective CPS
----------------

* 継続が必要な部分でのみ変換
* 2 pass transformation
* [A Selective CPS Transformation](http://www.sciencedirect.com/science/article/pii/S1571066104809691)
===

```common-lisp
(with-call/cc
  (let ((x 3) y)
    (setq y (* x x))
    (+ 1 (call/cc
          (lambda (k)
            (funcall k y))))))

```

===

```common-lisp
(with-call/cc
  (let ((x 3) y)
    (setq y (* x x))
    (+ 1 @(call/cc
          (lambda (k)
            (funcall k y))))))

```

===

```common-lisp
(with-call/cc
  (let ((x 3) y)
    (setq y (* x x))
    @(+ 1 @(call/cc
          (lambda (k)
            (funcall k y))))))

```

===

```common-lisp
(with-call/cc
  @(let ((x 3) y)
    (setq y (* x x))
    @(+ 1 @(call/cc
          (lambda (k)
            (funcall k y))))))

```

===
# そもそもCommon Lispのつらい話
-------------------------------
* セマンティクスが動的
  + catch, block, tagbody
  + special variable
  + 変換は静的なのでどう頑張っても追い付かない
* multiple valueが面倒


===
# ダイナミック!!
----------------

``` common-lisp
(block name
 (let ((f
        (lambda (x) (return-from name x))))
   (with-call/cc
       (funcall
        f
        (call/cc
         (lambda (k)
           (funcall k 2)))))))
```

===
# スペシャル変数
---------------

* Common Lispにはレキシカルスコープとダイナミックスコープ両方ある
* CPS変換すると継続の全てがスコープ下に入る
  + 関数の呼び出し関係が木だったのが線型になる
  + ダイナミックスコープだと困る
===
![CPS前のAST](/images/cps/pre-cps.png)


===
![CPS後のAST](/images/cps/post-cps.png)


===
```
(defvar *x* 1)
(with-call/cc
  (progn
    (let ((*x* 2))
      (call/cc ..)
      (format t "~a~%" *x*)) ; *x* = 2
    (format t "~a~%" *x*))) ; *x* = 1

```

===

```
(defvar *x* 1)
(with-call/cc
  (progn
    (let ((*x* 2))
      (...
       (lambda (ignore)
         ((lambda (ignore)
            (format t "~a~%" *x*)) ; *x* = 2!!
          (format t "~a~%" *x*))))))) ; *x* = 1
```

===
# 多値
------

* Common Lispの多値はGoと違って無視出来る
* 変換が空気読む必要がある
* 下手するとプログラムを壊す
  1. 本当は多値を返してるのに変換で無視された
  2. 意図的に無視してるのに変換で加えられた

===
# 関数定義と引数の数
-----------------

* **Selective** CPS
* 関数をCPS変換するときとしない時がある
* 呼び出す時にどっちか分かんなくね？
  1. 統一的に変換してしまう
  2. Selectiveに変換して関数にメタデータつける

-> まだ決めきれてない

===
# パフォーマンス
----------------


<!-- .slide: class="center" -->

===
# フィボナッチ数列
-----------------
* とりえあずのフィボナッチ数列で計測
  + Full CPS変換に割と不利
  + 何も考えずにライブラリを使うとこうなるよって例
* Selective CPSは何もしない=普通の定義と同じ


===
```common-lisp
(defun fib (n)
  (if (<= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))
```

``` common-lisp
(defun/cc fib-cont (n)
  (if (<= n 1)
      1
      (+ (fib-cont (- n 1)) (fib-cont (- n 2)))))
```

===
# Selective


```
Evaluation took:
  1.517 seconds of real time
  1.516000 seconds of total run time (1.516000 user, 0.000000 system)
  99.93% CPU
  4,388,993,782 processor cycles
  0 bytes consed
```

===
# Full

```
Evaluation took:
  18.347 seconds of real time
  18.576000 seconds of total run time (18.248000 user, 0.328000 system)
  [ Run times consist of 1.396 seconds GC time, and 17.180 seconds non-GC time. ]
  101.25% CPU
  53,149,416,888 processor cycles
  22,922,853,904 bytes consed
```
===
# コルーチン
------------

* そこまでFull CPSに不利じゃない
* 割と実用しそうな例
* Selective CPSは少しだけラムダが少ない

===
# Selective

``` common-lisp
(let (c)
  (setf c ((lambda ()
             (write-line "in thread A 1")
             (lambda ()
              (write-line "in thread A 2")
              (lambda ()
                (write-line "in thread A 3"))))))
  (write-line "in main thread 1")
  (setq c (funcall c))
  (write-line "in main thread 2")
  (setq c (funcall c))
  (write-line "in main thread 3"))
```


===
# full

```common-lisp
(let (c)
  (setf c (with-call/cc
            (write-line "in thread A 1")
            (let/cc k k)
            (write-line "in thread A 2")
            (let/cc k k)
            (write-line "in thread A 3")))
  (write-line "in main thread 1")
  (setq c (funcall c))
  (write-line "in main thread 2")
  (setq c (funcall c))
  (write-line "in main thread 3"))
```
===
# Selective

```
Evaluation took:
  0.905 seconds of real time
  0.904000 seconds of total run time (0.904000 user, 0.000000 system)
  99.89% CPU
  2,617,396,241 processor cycles
  0 bytes consed

```

===
# full

```
Evaluation took:
  1.272 seconds of real time
  1.272000 seconds of total run time (1.272000 user, 0.000000 system)
  100.00% CPU
  3,681,362,466 processor cycles
  0 bytes consed
```

===
# まとめ
--------

* 限定継続便利だよ
* マクロを使えばCPS変換で限定継続実装出来るよ
* でもパフォーマンや言語の問題もあるよ
* Selective CPSを使えばパフォーマンスの問題解決出来るよ

</script>
</section>
