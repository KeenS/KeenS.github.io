---
categories: [Scala, Isabelle, ScalaMeetUp, 社内]
date: 2016-10-05T00:10:25+09:00
description: "
社内のScala Meet Upでの発表用。テストがテーマ。
"
title: 正しいScalaのコードが欲しい
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# 正しいScalaのコードが欲しい
----------------------

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 基盤開発グループ
 + Lisp, ML, Rust, Shell Scriptあたりを書きます
   + Scalaはあんまり
===
# Scalaを始めたばかりの頃の話

<!-- .slide: class="center" -->
===


``` scala
def revappend[A](
    xs: List[A],
    ys: List[A]): List[A] = match xs {
  case Nil => Nil
  case x::xs_ => revappend(xs_, x::ys)
}
```

===

``` scala
def length(xs: List[_]): Int = xs match {
  case Nil => Nil
  case x::xs_ => 1 + length(xs_)
}
```

===

``` scala
def append[A](
    xs: List[A],
    ys: List[A]): List[A] = xs match {
  case Nil => Nil
  case x::xs_ => x :: append(xs_, ys)
}

append(List(1, 2, 3), List(4, 5, 6))
// => List(1, 2, 3)

```

===
# Scalaを始めたばかりの頃の話
-----------------------------

* 正しくないコードばかり書いてしまう
  + 他の言語の構文と混ぜてしまう
  + 型エラー
  + 実装ミス
* 正しいScalaのコードが欲しい

===
# 正しさって？
-------------

* 構文が正しいコード?
* コンパイルが通るコード?
* バグのないコード？
  + テストが通るコード?

===
# 正しさって？
-----------

* 数学だったら？
* 数学なら証明されれば正しい
* プログラムの証明????

===
# Isabelle
----------

* 汎用証明支援系
* [Isabelle](https://isabelle.in.tum.de/)
* 結構昔からある
* 関数型言語 + 高階論理

===
# コード
--------

``` isabelle
datatype 'a list = Nil                 ("[]")
  | Cons 'a "'a list"    (infixr "#" 65)

primrec app :: "'a list => 'a list => 'a list" (infixr "@" 65)
  where
  "[] @ ys      = ys" |
  "(x # xs) @ ys = x # (xs @ ys)"
```

===

# 証明
------

``` isabelle
lemma app_Nil2 [simp]: "xs @ [] = xs"
  apply(induct_tac xs)
  apply auto
  done

lemma app_assoc [simp]: "(xs @ ys) @ zs = xs @ (ys @ zs)"
  apply (induct_tac xs)
  apply auto
  done
```

===
# Extract
---------

```isabelle
export_code append
   in Scala
   module_name "Example"
   file "Example.scala"
```

===
# Scalaのコード
---------------

証明されたScalaのコードが手に入る

``` scala
object Example {

abstract sealed class list[A]
final case class Nila[A]() extends list[A]
final case class Cons[A](a: A, b: list[A]) extends list[A]

def append[A](x0: list[A], ys: list[A]): list[A] = (x0, ys) match {
  case (Nila(), ys) => ys
  case (Cons(x, xs), ys) => Cons[A](x, append[A](xs, ys))
}

} /* object Example */

```

===
# 余談
-------

他の言語にもextract出来る

```isabelle
export_code append
   in SML
   module_name "Example"
   file "Example.sml"
```


===
# まとめ
---------

* プログラムは証明出来るよ
* Isabelleで証明が出来るよ
* Isabelleで証明したら正しいScalaのコードが手に入るよ

</textarea>
