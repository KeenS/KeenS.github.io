---
categories: [Clojure, FRESH勉強会]
date: 2015-06-22T22:53:39+09:00
draft: true
description: "Clojureおじさん"
title: Clojureおじさん
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# Clojureおじさん
----------------------
サイバーエージント新卒  
第5回 Fresh勉強会
<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 渋谷のエンジニア
 + Lisp, ML, Shell Scriptあたりを書きます

# Clojure 
---------

* 2007年から
* JVMで動くLisp系言語
* 函数型
* 動的型付き
* 値は基本immutable
* 遅延シーケンス
* STMがあり、並列処理に強い


# Hello World
-------------

```clojure
(println "Hello, World")
```


# JVM
------
## Javaのコードをシームレスに呼べる

* `obj.method()` ではなく `(.method obj)`
* `Class.staticField` ではなく `Class/staticField`
* `obj.method1().method2()` ではなく `(.. obj method1 method2)`


# JVM
-----

```clojure
(.println System/out "Hello JVM")
```

```clojure
(Math/random)
```


# Immutable
-----------

* 状態を持たないのでコードの見通しが良い
  + 並列で考える時には重要
  + 複雑さだけでなく不整合などバグの原因になりやすい
* 変更可能な値もある
  + それらはトランザクション内でのみ変更可能
  + 不整合が起きない


# 函数型
--------

```clojure
(filter odd? (map #(+ 1 %) '(1 2 3)))
```


# 遅延シーケンス
---------------

```clojure
(def natural_number (iterate inc 1))
(take 5 natural_number) ;=> (1 2 3 4 5)
```


# リスト内包表記
----------------

```clojure
(for [x (range 5)] (* x x))
;=> (1 4 9 16 25)
```


#並列処理に強い
---------------

* 簡単にはJavaのスレッドを呼べばいい
  + Clojureの関数は全てCallableでRunnablea

```clojure
(.start (Thread. (fn [] (Thread/sleep 1000) (println 'foo))))
```


#並列処理に強い
---------------
## core.async

* goroutineとgochannelが使える


## core.async
-------------

```clojure
(go)
```
Goroutine/gochannel
</script>
</section>
