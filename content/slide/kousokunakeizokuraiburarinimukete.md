---
categories: [継続, 限定継続, Lisp, Common Lisp]
date: 2016-05-08T21:26:32+09:00
description: "継続勉強会向け"
draft: true
title: 高速な継続ライブラリに向けて
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# 高速な継続ライブラリに向けて
----------------------

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバーエージェントのエンジニア
 + Lisp, ML, Rust, Shell Scriptあたりを書きます

# 継続欲しい
-----------

* 色々な場面で便利
* Schemeで使い回してるのうらやましい
* Common Lispでも使いたい
* 現実には限定継続が欲しい
  +  Common Lispには大域脱出はある


# 限定継続を使う例
-----------------
## 非同期プログラミング

* コールバック形式だと厳しい
* 限定継続を使うと綺麗に書き直せる


# 限定継続を使う例
-----------------
## ゲームのコルーチン

* 複数のオブジェクトを制御するのにコルーチンが欲しい
* cf [コルーチンをCommon Lispで簡単に定義 - さくらんぼのlambda日記](http://lambdasakura.hatenablog.com/entry/20111026/1319598590)


# 限定継続を使う例
-----------------
## do記法

* モナドのdo記法は限定継続を使って実装出来る
* [Operational monad in scheme](http://www.slideshare.net/yuichinishiwaki/operational-monad-in-scheme)



# Common Lispでの限定継続の実現
---------------

1. 仕様に入れてもらう
2. 処理系に手を入れる
3. ユーザレベルで(限定)継続ライブラリを作る
   + 柔軟なCommon Lispでは可能


# CPS変換
---------

* (限定)継続の実現方法の1つ
  + スタックを切り取る方式とかもある
* 機械的にも出来る
* グローバルな変換なのとプリミティブな式しか書けないでコンパイラ内部でやることが多い
  + 継続関係なしに中間形式として採用されることが多い
* 関数定義/呼び出し以外にも諸々の構文とかに対しても定義が必要


# CPS変換
----------

Q. Common Lispだといくつの構文に対して定義が必要?

1. 1つ
2. 26つ
3. 42つ
4. 無数



# CPS変換
----------

A. 26つ (スペシャルフォーム25+funcall)


# Common Lispのプリミティブ
--------------------------

* スペシャルフォームと呼ばれる
* 仕様で25個定められている
* [CLHS: Section 3.1.2.1.2.1](http://www.lispworks.com/documentation/HyperSpec/Body/03_ababa.htm)
* この中に関数定義だとか例外だとかは入っていない
  + マクロで定義されている


# マクロ
--------

* 構文木 to 構文木(S式to S式)変換器( = 普通のLispの関数)
* 新しい構文を作れる
* CPS変換は?????


# `macroexpand`
-------------

*[CLHS: Function MACROEXPAND, MACROEXPAND-1](http://clhs.lisp.se/Body/f_mexp_.htm)
* マクロを手動展開する関数
* 雑にいうと普段pre-orderなマクロ展開をin-orderやpost-orderにする時に使う
* 本来はあまり使いたくない
  + 処理系の展開器に任せた方が間違いが少ない
* これでマクロを排したプリミティブのCommon Lispの構文木にアクセス出来る


# cl-cont
---------

* 上記のことを全てやったライブラリ
* デファクトというか唯一のライブラリ
* [Common Lispで限定継続と遊ぶ | κeenのHappy Hacκing Blog](http://keens.github.io/slide/Common_Lispdegenteikeizokutoasobu_/)


<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">「shift/resetがわからない時にあげる声」</p>&mdash; かず(原材料に小麦粉を含む) (@kazzna) <a href="https://twitter.com/kazzna/status/674026894602309632">2015年12月8日</a></blockquote>

<!-- .slide: class="center" -->


# cl-contの使用例

``` common-lisp
(with-call/cc
  (+ 1 (call/cc
        (lambda (k)
          (funcall k 2)))))

```


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



# cl-contへの不満
-----------------

* 遅い
* lambda多い。
  + 多分コンパイラと相性が悪い
* lambda禁止おじさんと分かりあえる


# cl-fast-cont


<!-- .slide: class="center" -->

# cl-fast-cont
--------------

* [KeenS/cl-fast-cont: faster partial contiuation library of common lisp](https://github.com/KeenS/cl-fast-cont)
* とりあえずレポジトリ作っただけ
* 完成させたい…


# アプローチ1
<!-- .slide: class="center" -->


# SSA使う
---------

* CPSと等価
* だけどSSAだったらlambda出てこない
* Common Lispならgotoあるしいけるんじゃね？


```common-lisp
(let (x y z)
 (tagbody
    (setq x 1)
  :call/cc
    (setq y 1)
    (setq z (+ x y))))
```



# 問題
-------

* ネイティブスタックとは別に自分でスタック作らないといけない
  + 例外とかでスタック巻き戻されるとつらい
* gotoのタグをtagbodyの外に持ち出せない(=継続を外に持ち出せない)
* 変数を準備するのが面倒orパフォーマンスに影響しそう
* そもそもtagbodyそこまで柔軟じゃなかった


# アプローチ2
<!-- .slide: class="center" -->


# SSA+CPS
SSAコンパイラとCPSコンパイラ
SSAからの継続じゃだめなの？
 →gotoを使って自分でコールスタック組み立てる必要があった
Selective CPS
2pass transformation
※詳しい図解
load-time-value, catch, block, tagbody, special variableつらい
変換は会議室でおきてるんじゃない、現場で起きてるんだ
関数定義と引数の話 -> まだ決めきれてない
計測


</script>
</section>
