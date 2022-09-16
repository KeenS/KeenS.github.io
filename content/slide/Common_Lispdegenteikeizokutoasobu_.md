---
categories: [Lisp, Common Lisp, Lisp Meet Up, 限定継続, CPS]
date: 2015-04-26T21:09:07+09:00
description: "Lisp Meet Up #27 でのLT用スライド。<br>
Common Lispで限定継続をサポートするライブラリ、cl-contの紹介と
実装の話。
"
title: Common Lispで限定継続と遊ぶ
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Common Lispで限定継続と遊ぶ
----------------------
[Lisp Meet Up #27](https://atnd.org/events/64988)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 渋谷のエンジニア
 + Lisp, ML, Shell Scriptあたりを書きます

===
# Agenda
--------

0. cl-contの紹介
1. 限定継続の話
2. 限定継続の使い方の話
3. 限定継続の実装の話

===
# CL-CONTの紹介

<!-- .slide: class="center" -->
===
# CL-CONT
---------

* Common Lispの限定継続ライブラリ
* 結構古くからあるっぽい
* 割と色々なライブラリが使ってる
===
# CL-CONT
---------

![cl-cont dependers](/images/cl-cont-dependers.png)
===
# API
-----

* 継続を区切るマクロ
  + `with-call/cc`
  + `defun/cc`
  + `lambda/cc`
* 継続を取得するマクロ
  + `call/cc`
  + `let/cc`

※後で説明するので意味が分からなくても問題ないです。

===
# 限定継続の話

<!-- .slide: class="center" -->
===
# 継続とは
---------

* "その後"の計算を表わす概念。
* 値が決定した後トップレベルに戻るまでの計算。
* Schemeが一級市民として扱えることで有名
* 値として取り出した時は0-1引数関数として振る舞う
===
# 継続とは
----------
`foo`の継続は、`foo`を虫食いにしたものと思えば良い。

```lisp
(mapc #'writ-line
      (loop :for x :in list
         :collect (if (evenp x)
                      (foo x))))
```
===
# 継続とは
----------
`foo`の継続は、`foo`を虫食いにしたものと思えば良い。

```lisp
(mapc #'writ-line
      (loop :for x :in list
         :collect (if (evenp x)
                      □)))
```
===
# 継続とは
----------
あとはそれを関数にするだけ

```lisp
(lambda (k)
  (mapc #'writ-line
        (loop :for x :in list
           :collect (if (evenp x)
                        k))))
```

===
# 限定継続とは
---------

* "その後"の計算を表わす概念。
* 値が決定した後 *指定した位置* に戻るまでの計算。
* 継続がトップレベルまで戻るのに対して限定継続途中で止まる。それだけ。
* 部分継続などの言い方もある
  + 英語もpart contとdelimited contで分かれる

===
# 限定継続とは
-------------
先の例を`loop`までの限定継続にすると

```lisp
(mapc #'writ-line
      (loop :for x :in list
         :collect (if (evenp x)
                      (foo x))))
```

===
# 限定継続とは
-------------
先の例を`loop`までの限定継続にすると

```lisp
(loop :for x :in list
           :collect (if (evenp x)
                        □))
```

===
# 限定継続とは
-------------

```lisp
(lambda (k)
  (loop :for x :in list
     :collect (if (evenp x)
                  k)))
```
===
# 限定継続の挙動
---------------
普通のやつ

```lisp
(with-call/cc
  (+ 1 (call/cc
        (lambda (k)
          (funcall k 2)))))
```

これは

```lisp
(+ 1 2)
```

と等価
===
# 限定継続の挙動
---------------
今度は継続を呼ばないでみる

```lisp
(with-call/cc
  (+ 1 (call/cc (lambda (k) 2))))
```

これは

```lisp
2
```

と等価
===
# 限定継続の挙動
---------------
もうちょっと呼ばない例  
(`(let/cc k ...)` = `(call/cc (lambda (k) ...))`)

```lisp
(with-call/cc
  (write-line "hello")
  (let/cc k 1)
  (write-line "world"))
```

は

```lisp
(progn
  (write-line "hello")
  1)
```

と等価

===
# 限定継続の使い方の話

<!-- .slide: class="center" -->
===
# 限定継続の使い方の話
---------------------

* グリーンスレッド
* コールバックを綺麗に書き換える
* 非決定性計算
* etc...

===
## グリーンスレッド

<!-- .slide: class="center" -->

===
## グリーンスレッド
-----------------

* またの名をコルーチン
* またの名を強調スレッド


===
## グリーンスレッド
-----------------
```lisp
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
## グリーンスレッド
-----------------
実行結果

```
in thread A 1
in main thread 1
in thread A 2
in main thread 2
in thread A 2
in main thread 3

```

===
## コールバックの書き換え

<!-- .slide: class="center" -->
===
## コールバックの書き換え
----------------------
本当はこう書きたい

```lisp
(with-event-loop
    (format t "Hello, ~a!~%" (async-read stream)))
```
===
## コールバックの書き換え
----------------------
しかしライブラリがコールバック関数を要求してくる

```lisp
(with-event-loop
    (async-read stream (lambda (line)
                         (format t "Hello, ~a!~%" line))))
```
===
## コールバックの書き換え
-----------------------
コールバック = 限定継続（後述）なのでこうしてやれば良い。

```lisp
(with-event-loop
    (with-call/cc
      (format t "Hello, ~a!~%"
              (call/cc (lambda (k))
                       (async-read stream k)))))
```
===
## 非決定性計算

<!-- .slide: class="center" -->
===
## 非決定性計算
-------------
### ベースアイディア

* 継続を関数として取り出した後同じ処理を何回も実行出来るんじゃね？

===
## 非決定性計算
-------------
### ベースアイディア
複数回呼び出してみる

```lisp
(let (c)
  (setf c (with-call/cc
            (+ 1 (* 2 (- 3 (let/cc k k))))))
  (list (funcall c 1)
        (funcall c 2)
        (funcall c 3)))
```

===
## 非決定性計算
-------------
### ベースアイディア
普通の1引数関数として使ってみる

```lisp
(let (c)
  (setf c (with-call/cc
            (+ 1 (* 2 (- 3 (let/cc k k))))))
  (mapcar c (list 1 2 3)))
```
===
## 非決定性計算
-------------
### ベースアイディア
マクロでラップしてみる

```lisp
(defmacro for (&body expr)
  `(with-call/cc ,@expr))
(defmacro in (m)
  `(let/cc k (apply #'append (mapcar k ,m))))
(defun yield (x) (list x))
(defun unit () nil)
```

===
## 非決定性計算
-------------
使ってみる

```lisp
(for
  (let ((x (in '(1 2 3)))
        (y (in '(a b c))))
    (yield (cons x y))))
```

```
((1 . A) (1 . B) (1 . C) (2 . A) (2 . B) (2 . C) (3 . A) (3 . B) (3 . C))
```

===
# 限定継続の実装の話

<!-- .slide: class="center" -->
===
# 限定継続の実装の話
-------------------

* 継続は0~1引数関数として取り出せるのであった。
  + 実は機械的に取り出せる
* 継続は全ての式に暗黙に存在するのであった
  + 全ての式を継続を明示的に使うようにも出来る

===
# CPS変換

<!-- .slide: class="center" -->
===
# CPS変換
---------

* 全ての関数の引数を1つ増やして、そこで継続を受け取る
* 値を返す時は暗黙のreturnを使うのではなく明示的に継続を呼ぶ
* 関数を呼ぶ時は必ず継続を渡す。呼び出し元には返ってこない（自然と末尾再帰になる）

===
# CPS変換
---------
例えば

```lisp
(with-call/cc
  (foo (call/cc
        (lambda (k)
          (funcall k 2)))))
```

は、取り出された継続が

```lisp
(lambda (k) (foo k))
```

。

===
# CPS変換
---------
よって

```lisp
((lambda (k) (foo k)) 2)
```

と変換される

===
# CPS変換
---------
再帰関数だと少し面倒

```lisp
(defun fact (n)
  (if (<= n)
      1
      (* n (fact (- n 1))))))

```

が

```lisp
(defun fact (n c)
  (if (<= n)
      (c 1)
      (fact (- n 1) (lambda (v) (* n v)))))
```

となる

===
# CPS変換
---------

* CPS変換を行なうことでいつでも継続を値として使える

===
# 限定継続の実装の話
-------------------

* CPS変換を裏で行なっている
* 自動で出来るのでマクロで変換をしている
  + 組み込み関数は変更出来ないので特別扱い
  + スペシャルフォームも気をつける必要がある
* `lambda`が乱立するのでパフォーマンスは酷い。

===
# まとめ
-------

* cl-contというライブラリがあって、限定継続が扱える
* 限定継続は処理を中断したり再開したり繰り返したりに使える
* 裏ではえげつないことをやっている
* パフォーマンスが必要なところでは使っちゃダメ

===
# 参考
------
* [picrin/partcont.scm at master · picrin-scheme/picrin](https://github.com/picrin-scheme/picrin)
* [cl-cont | Quickdocs](http://quickdocs.org/cl-cont/)
* [コルーチンをCommon Lispで簡単に定義 - さくらんぼのlambda日記](http://lambdasakura.hatenablog.com/entry/20111026/1319598590)
* [継続渡しスタイル - Wikipedia](http://ja.wikipedia.org/wiki/%E7%B6%99%E7%B6%9A%E6%B8%A1%E3%81%97%E3%82%B9%E3%82%BF%E3%82%A4%E3%83%AB)
* [M.Hiroi's Home Page / お気楽 Scheme プログラミング入門](http://www.geocities.jp/m_hiroi/func/abcscm20.html)
</textarea>
