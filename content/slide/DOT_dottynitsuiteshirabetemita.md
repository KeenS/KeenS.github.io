---
categories: [Scala, 言語処理系, 社内, Scala Meet Up]
date: 2016-05-24T23:40:36+09:00
description: "Scala Meet Upでの発表用"
title: DOT/dottyについて調べてみた
---

<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">

# DOT/dottyについて調べてみた
----------------------
サイバーエージェント Scala Meet Up  
2016-05-27

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
 + Scala初心者
   + Scala歴1年未満&gt;&lt;

===
# Scalaコンパイラ
----------------

* 型推論（特にimplicit）が遅い
* コンパイルフェーズが多い
  + 中間オブジェクトが多くて遅くなる
* 多くのクラスファイルを生成する
  + コンパイラのくせにディスクIOが多い
* 気を抜いてると `Any` に推論される  
  e.g.) `if(x) 1 else "a"`
* そもそものScalaの設計に起因する点が多い
  + 抜本的変更が必要

===
# Scala基礎
-----------

* Scalaが成立するのに最低限必要な機能って？
  + e.g.) `case class` はなくても `class` だけでどうにかなる
* 「最低限の機能」を減らせばコンパイラがシンプルになる
  + 他の機能はただのシンタックスシュガーになる
  + コンパイラのバグを減らせる
  + 機能追加時の矛盾確認が楽
* コア言語

===
# Featherlight Scala
---------------------

* Scalaのシンタックスシュガーを減らした感じ
* 小さな言語
* 元のScalaはこれをベースに議論されてきた

===
# 形式言語理論
-------------

* 最低限必要な機能を形式的にモデル化
  + 全ての動作は予め決められた「規則」に基く
  + 実際に書いて動かすものではない
* 形式的にすることで「証明」とかが出来る
  + 「型検査に通れば実行時に型エラーが起きない」
  + 「型検査が必ず終了する」
* 逆に、「このモデルだとこの機能は実現出来ない」とかも分かる

===
# Dependent Object Type
------

* Scalaのコアをモデル化したもの（要出典）
* 形式言語
* ジェネリクスもなければクラスも継承もない、パッケージもない。
* 小さい言語ながら表現力豊か
* 全ての値はオブジェクト。
  + オブジェクトのフィールドとメソッドと型メンバーだけ
  + 型はそのまま
* Path Dependent Type
* サブタイピング


===
# About DOT
---------

* 型付の健全性が証明された
* System F<:より強力
  * System F<: -> System D<: -> DOT
* existential typeを自然に表現出来る
* 交差型と合併型
  + `A extends B` -> `A & B`
  + 合併型は主に型推論の結果に出てくる

===
```scala
package scala.collection.immutable trait List[+A] {
  def isEmpty: Boolean; def head: A; def tail: List[A]
}
object List {
  def nil: List[Nothing] = new List[Nothing] {
    def isEmpty = true; def head = head; def tail = tail /* infinite loops */
  }
  def cons[A](hd: A, tl: List[A]) = new List[A] {
    def isEmpty = false; def head = hd; def tail = tl
  }
}
```

===
```scala
object scala_collection_immutable { sci =>
  trait List { self =>
    type A
    def isEmpty: Boolean
    def head: self.A def
      tail: List{type A <: self.A}
  }
  def nil: sci.List{type A = Nothing} = new List{ self =>
    type A = Nothing
    def isEmpty = true
    def head: A = self.head
    def tail: List{type A = Nothing} = self.tail
  }
  def cons(x: {type A})(hd: x.A)(tl: sci.List{type A <: x.A})
      : sci.List{type A <: x.A} = new List{ self =>
    type A = x.A
    def isEmpty = false
    def head = hd
    def tail = tl
  }
}
```

===
# dotty
--------

* DOTに影響を受けたコンパイラ
* いくつかの新しい機能
  + DOTの交差型、合併型も
  + Nullable = `T | Null`
  + `if (x) 1 else "a"` は `Int | String` にアノテーション可能
* `forSome` が消えた
  + DOTのお陰

===
# dotty
--------

* Java8のラムダを使う
  + 生成するclassファイルの減少
* Implicitの探索アルゴリズムを改善
  + 反変implicitについても改善
* 型推論のアルゴリズムを改善
  + DOTのお陰
  + 特にサブタイピングが交差/合併型で楽に
* コンパイルパスを融合して高速化
  + 中間木がなくなってGCにやさしい
* 他にも一杯改善が

===

```scala
object DaysOfTheWeek{
  object Mon
  object Tue
  object Wed
  object Thu
  object Fri
  object Sat
  object Sun

  type Weekend = Sat.type | Sun.type
  type Workweek = Mon.type | Tue.type | Wed.type | Thu.type | Fri.type
  type All = Weekend | Workweek
}
```

===
# TASTY/Linker
--------------

* classファイルを作るとScala固有の情報が落ちる
* プログラム全体を見渡すと不要なコードとかもコンパイルしないといけない
* かといって毎回プログラム全部をコンパイルし直すのは遅い
* → TASTY。型推論後のASTをシリアライズする
  + Scalaは型推論が遅いのでそこをスキップ出来るだけでそこそこ速くなる
* classファイルを跨げるようになったのでユーザが最適化とかも書ける
* Scala/Scala.js/Scala Native共通プラットフォーム化への布石？
* どうやらclassファイルにバイトコードとTASTYを埋め込む??

===
# まとめ
--------

* Scalaの基礎にDOTがあるよ
* DOTを参考にdottyが作られたよ
* dottyで色々改善されるよ
* ついでにTASTY/Linkerについて話したよ

===
# 参考1
------

* [The Essence of Dependent Object Types](https://infoscience.epfl.ch/record/215280/files/paper_1.pdf)
* [From F to DOT: Type Soundness Proofs with Definitional Interpreters](http://arxiv.org/pdf/1510.05216v2.pdf)
* [Dependent Object Types](http://www.cs.uwm.edu/~boyland/fool2012/papers/fool2012_submission_3.pdf)
* [Why is the Scala compiler so slow? - Quora](https://www.quora.com/Why-is-the-Scala-compiler-so-slow)
* [performance - Java compile speed vs Scala compile speed - Stack Overflow](http://stackoverflow.com/questions/3490383/java-compile-speed-vs-scala-compile-speed/3612212#3612212)

===
# 参考2
-------

* [GHC doesn't do subtyping. I suspect that is the main reason why Scala is slow - ... | Hacker News](https://news.ycombinator.com/item?id=5008761)
* [A Core Calculus for Scala Type Checking](http://lampwww.epfl.ch/~odersky/papers/mfcs06.pdf)
* [namin/dot: formalization of the Dependent Object Types (DOT) calculus](https://github.com/namin/dot)
* [The DOT Calculus](http://lampwww.epfl.ch/%7Eamin/dot/current_rules.pdf)
* [Dotty: exploring the future of scala](https://d-d.me/talks/scalaworld2015/)
* [Dotty and types: the story so far](http://guillaume.martres.me/talks/typelevel-summit-oslo/)

</script>
</section>
