---
layout: slide
title: "Semantic S式"
date: 2014-11-27 15:30
format: markdown
categories: [lisp, lisp meet up, デザインスタイル]
description: "Lisp Meet Up #22でLT間に合えばなと思っている内容。<br>
コーディングスタイルというかデザインスタイル的なもの。<br>
"
---
# Semantic S式
---
2014-11-27  
κeen(@blackenedgold)


# About Me
----------
![κeenのアイコン](/images/icon.png)

 + κeen
 + 東大数学科の4年生
 + ソーシャルアカウントは上のアイコン達から。
 + Lisp, Ruby, OCaml, Shell Scriptあたりを書きます

# Semantic Web とは

# [Semantic Web](http://ja.wikipedia.org/wiki/%E3%82%BB%E3%83%9E%E3%83%B3%E3%83%86%E3%82%A3%E3%83%83%E3%82%AF%E3%83%BB%E3%82%A6%E3%82%A7%E3%83%96)
----------------

* W3C のティム・バーナーズ＝リーが提唱
* 文章の見た目を記述するんじゃなくて構造を記述しよう
* そうすると機械がクローリングするの楽になるよね


# Semantic S式とは

# Semantic S式
--------------

* Shibuya.lispのκeenが提唱 (昨日思い付いた)
* S式の見た目じゃなくて構造で括弧をつけよう
* そうするとコーディングが楽になるよね


# 良いところ
------------

1. 意味が分かり易い
2. パースし易い
3. 編集が楽になる
4. 壊れにくい


# 1. 意味が分かり易い

# 1. 意味が分かり易い
--------------------
## 例1

* Clojure
  ```clojure
  (let [a 1
        b 2]
    ...)
  ```

* Common Lisp
  ```lisp
  (let ((a 1)
        (b 2))
    ....)
  ```
どっちが分かり易い？

# 1. 意味が分かり易い
--------------------
## 例1
（日本語にしてみる（イメージ））

* Clojure  
  aを1bを2とする
* Common Lisp
  <table border="1"><tr><td>`a`</td><td>1</td></tr><tr><td>`b`</td><td>2</td></tr></table>
  とする


# 1. 意味が分かり易い
--------------------
## 例2
何をしてる？

```lisp
(destructuring-bind (a b)
    (some-function)
  ...)
```

# 1. 意味が分かり易い
--------------------
## 例2
何をしてる？

```lisp
(destructuring-bind ((a b)
    (some-function))
  ...)
```

# 1. 意味が分かり易い
--------------------
## 例3
これの返り値は？
```lisp
(getf '(:hoge :foo :huga :baz :piyo :pon :chun) :pon)
```

# 1. 意味が分かり易い
--------------------
## 例3
これの返り値は？
```lisp
(assoc :pon '((:hoge :foo) (:huga :baz) (:piyo :pon) (:chun)))
```

# 2. パースし易い

# 2. パースし易い
----------------
## 例

* Clojure
  ```clojure
  (let [a 1
        b 2]
    ...)
  ```

* Common Lisp
  ```lisp
  (let ((a 1)
        (b 2))
    ....)
  ```
どっちが実装し易い？

# 2. パースし易い
----------------

* Common Lispの方は意味で分割してある
* 括弧は無くてもパースは出来る
* 機械にパースし易い≒人間にパースし易い


# 3. 編集が楽になる

# 3. 編集が楽になる
------------------
## 前提
* エディタに文/式/トークン(Lispの場合は全てS式)単位の編集機能がある
   + S式単位でカーソル移動
   + S式単位の削除/カット
   + S式単位のスワップ
   + etc....

# 3. 編集が楽になる
------------------
## 例（偶によくある）
この`c`の束縛を上のletに持っていきたいときどうする？

```lisp
(let ((a 1)
      (b 2))
  ....
  (let ((c 3)
        (d 4))
    ...))
```

# 3. 編集が楽になる
------------------
## 例（偶によくある）

* 意味で括弧をつけていれば1カット移動1ペースト
* Clojure方式だと2カット移動1ペースト
* `c`だけじゃなくて`d`も、と考えると…

# 4. 壊れにくい

# 4. 壊れにくい
---------------
## 例
ageの計算が壊れているとする

```lisp
(:name "κeen"
 :age  (year-of-time-interval (date- (today) (date 1992 5 17)))
 :place "Shibuya")
```


# 4. 壊れにくい
---------------
## 例
コメントアウトしてみる（壊れた）

```lisp
(:name "κeen"
 :age  ;(year-of-time-interval (date- (today) (date 1992 5 17)))
 :place "Shibuya")
```

# 4. 壊れにくい
---------------
## 例
ageの計算が壊れているとする

```lisp
((:name "κeen")
 (:age  (year-of-time-interval (date- (today) (date 1992 5 17))))
 (:place "Shibuya"))
```

# 4. 壊れにくい
---------------
## 例
(エディタサポートあり)

コメントアウトしてみる（壊れない）

```lisp
((:name "κeen")
 (:age  ;(year-of-time-interval (date- (today) (date 1992 5 17)))
 )
 (:place "Shibuya"))
```

# ここまでのまとめ
-----------------
* 構造が文脈に依存していると
  + 人間に分りづらい
  + プログラムに分りづらい
  + エディタに分りづらい
  + 壊れやすい

# Q. When Semantic?
------------------------------
## A. 言語を設計するとき <!-- .element: class="fragment" data-fragment-index="1" -->

* つまりマクロを書くとき <!-- .element: class="fragment" data-fragment-index="2" -->

# Q. When Semantic?
------------------------------
## A. 言語を設計するとき

* マクロを書くときそれなりのDSLを設計する
* プログラム上は必須ではない括弧もあった方が良いときもある


# まとめ
--------

* 括弧が少ない方が書き易いとは限らない
  + 「書く」だけなら速くてもwrite onlyになってしまうかも<!-- .element: class="fragment" data-fragment-index="1" -->
* プログラムの意味を考えながら括弧つけよう
  + Lisperはどうせ括弧は見えない<!-- .element: class="fragment" data-fragment-index="2" -->

<span style="font-size:600%">以上</span>  
何か質問あればどうぞ
