---
categories: [Lisp, Scheme, Lisp Meet Up]
date: 2016-11-28T15:41:36+09:00
description: null
title: SchemeでClassとProtocol
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# ClassとProtocol
----------------------
[Lisp Meet Up presented by Shibuya.lisp #46](https://lisp.connpass.com/event/45517/) Scheme回
<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * サイバーエージェントのエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# Class
-------

* ここでは単に値の集合を表わす
* `new` のように新しいインスタンスを作る機能はつけない
* ほとんど型と同じ機能
* 唯一の機能 `instance?` を持つ

===

``` scheme
(instance? 3 <number>)     ; => #t
(instance? "foo" <number>) ; => #f
```

===

# Classの実装
-------------

* 事実上述語があればいい
* [詳細](https://github.com/picrin-scheme/picrin/blob/master/contrib/50.class/piclib/picrin/class.scm)

``` scheme
(define-class <any> (lambda (x) #t))
(define-class <list> list?)
(define-class <procedure> procedure?)
(define-class <number> number?)
```

===

# Classの用途
-------------

* 述語だけで意味ある？
  + プリミティブにのみあった述語をユーザが拡張出来る
* その上に何かを構築出来る
* protocolとか

===

# Protocol
-----------

* Clojureのprotocol
* 多重ディスパッチの機構
* ディスパッチするのにClassを使う

===

```scheme
(define-protocol (PLUS x y)
  (plus x y))

(define-instance (PLUS <number> <number>)
  (lambda (x y) (+ x y)))


(define-instance (PLUS <number> <string>)
  (lambda (x y) (string-append (number->string x) y)))

(define-instance (PLUS <string> <string>)
  (lambda (x y) (string-append x y)))

```

===

``` scheme
(display (plus 1 2)) (newline)
; -> 3
(display (plus 1 "foo")) (newline)
; -> 1foo
(display (plus "bar" "foo")) (newline)
; -> barfoo
(display (plus "bar" 1)) (newline)
; -> "error: method not found"
```

===
# ユーザ定義型とProtocol
------------------------

``` scheme
(define-record-type complex
  (make-complex real img)
  complex?
  (real complex-real)
  (img  complex-img))

(define (complex-+ c1 c2)
  (make-complex (+ (complex-real c1) (complex-real c2))
                (+ (complex-img  c1) (complex-img  c2))))

(define (complex->string c)
  (string-append
   (number->string (complex-real c)) "+"
   (number->string (complex-img  c)) "i"))
```

===

```scheme
(define-class <complex> complex?)

(define-instance (PLUS <complex> <complex>)
  complex-+)


(display (complex->string
          (plus (make-complex 1 2)
                (make-complex 2 3)))) (newline)
; -> 3+5i
```

===

# Protocolの実装
----------------

* メソッドの登録とか述語でディスパッチとか割と面倒
* [詳細](https://github.com/picrin-scheme/picrin/blob/master/contrib/80.protocol/piclib/picrin/protocol.scm)

===
# まとめ
---------

* 「追加可能な型」は便利だよ
* 型相当のものがあればダイナミックディスパッチ出来るよ
* picrinに実装されてるよ `(picrin class)` `(picrin protocol)`

</textarea>
