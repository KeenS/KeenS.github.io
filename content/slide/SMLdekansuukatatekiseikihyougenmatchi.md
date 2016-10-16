---
categories: [SML, ML, 正規表現]
date: 2016-03-29T01:44:39+09:00
description: null
title: SMLで函数型的正規表現マッチ
---

<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# SMLで函数型的正規表現マッチ
----------------------

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
# 元ネタ
--------
[関数型的正規表現マッチ | Preferred Research](https://research.preferred.jp/2010/11/regexp-play/)

<!-- .slide: class="center" -->
===
# 正規表現
----------

必要最小限の要素は5つだけ

1. 空文字
2. アルファベット1つ
3. 正規表現のOR結合
4. 正規表現のAND結合
5. 正規表現の繰り返し

===
# SMLで表してみる
-----------------

``` sml
datatype  reg
  = Empty
  | Sym of t
  | Or of reg * reg
  | And of reg * reg
  | Rep of reg
```
===
# 本当に大丈夫？

<!-- .slide: class="center" -->

===
# `/(a|b)*c/`
-----------

``` sml
And(Rep(Or(Sym "a", Sym "b")), Sym "c")
```

===
# `/https?:\/\/[a-z]*/`
-----------

``` sml
val a_z = Or(Sym"a", Or(Sym "b", Or(Sym "d", ...)))
And(Sym "http", And(Or(Sym "s", Empty), And(Sym "://", Rep a_z)))
```

===
# 実装してみる

<!-- .slide: class="center" -->

===
# `Empty`, `Sym`, `Or`
----------------
trivial

``` sml
fun match Empty u = isEmpty u
  | match (Sym a) u = a = u
  | match (Or(p, q)) u = match p u orelse match q u
```

===
# `And`
--------

`And(p, q)` に入力が`u`の時`p`がどこまでマッチするか分からないので

* `u` から一部取ってきて`p`にマッチするか確認
* 残りの文字列が`q`にマッチするか確認

===
# `And`
--------
`match (And (Sym "a", Sym "b")) "abd"` の時

* (Sym "a"と"")、(Sym "b"と"abd")
* (Sym "a"と"a")、(Sym "b"と"bd")
* (Sym "a"と"ab")、(Sym "b"と"d")
* (Sym "a"と"abd")、(Sym "b"と"")


===
# `And`
--------

``` sml
  | match (And(p, q)) u = 
    withSprits u (fn (u1, u2) => 
      match p u1 andalso match q u2)

```

`withSprits u f` は`u`を2分割するパターン全通りに対して`f`を呼び、最初に`true`になったものを返す。

===
# `Rep`
-------

`Rep(r)` に入力が`u`の時、`r`がどこまでマッチするかも`Rep`が何回繰り返すかも分からないので

* `u` を任意の個数に分割し
* その全てが`r`にマッチするか確認

===
===
# `Rep`
--------
`match (Rep (Sym "a")) "abd"` の時

* (Sym "a"と"abd")
* (Sym "a"と"a")、(Sym "a"と"bd")
* (Sym "a"と"ab")、(Sym "a"と"d")
* (Sym "a"と"a")、(Sym "a"と"b")、(Sym "a"と"d")



===
# `Rep`
--------

``` sml
  | match (Rep(r)) u =
    withParts u (fn input =>
      List.all (match r) input)
```

`withParts u f` は`u`を分割するパターン全通りに対して`f`を呼び、最初に`true`になったものを返す。


===
# チェック
----------

```
# match (And(Rep(Or(Sym "a", Sym "b")), Sym "c")) "ababbca";
val it = false : bool
# match (And(Rep(Or(Sym "a", Sym "b")), Sym "c")) "ababbc";
val it = true : bool
# match (And(Rep(Or(Sym "a", Sym "b")), Sym "c")) "c";
val it = true : bool
```

===
# まとめ
--------

* 適当に実装したら正規表現も簡単に実装出来るよ
* SMLで正規表現実装したよ

===
# 参考
------

* [KeenS/regexp](https://github.com/KeenS/regexp)

</script>
</section>
