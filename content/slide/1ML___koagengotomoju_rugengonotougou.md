---
categories: [ML, 型]
date: 2017-09-24T23:23:57+09:00
description: "<a src='https://connpass.com/event/63454/'>型システム論文読書会 Vol.1</a>での発表用.
<a src='https://people.mpi-sws.org/~rossberg/1ml/1ml.pdf>1ML – Core and Modules United (F-ing First-Class Modules)</a>についてざくっと解説"
title: "1ML - コア言語とモジュール言語の統合"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# 1ML - コア言語とモジュール言語の統合
----------------------
[型システム論文読書会 Vol.1 - connpass](https://connpass.com/event/63454/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます
 * 型システム初心者です
 * 論文初心者です
===

# MLのモジュールについておさらい
<!-- .slide: class="center" -->

===

# モジュール、シグネチャ
----

* ストラクチャは型、関数、値をまとめたもの
  + なんかレコードっぽい
* シグネチャはその型
* モジュールはシグネチャを充足すればよい（余計なフィールドがあってもよい）
  + もちろん余計なフィールドは隠蔽される
===
``` sml
signature S = sig
    type t
    val f: t -> t
    val zero: t
end

structure M: S = struct
    type t = int
    fun f x = x
    val zero = 0
    fun g x = x
end
```
===
# ファンクタ
-----------

* ストラクチャを引数にとってストラクチャを返す
  + なんか関数っぽい
* 受け取ったモジュールに依存して型が変わったりする

===

``` sml
functor F(X: sig type 'a t end):
        sig
            type 'a t
            val id: 'a t -> 'a t
        end
= struct
    open X
    fun id x = x
end
```


===
# `include`
-----------

* なんかシグネチャをincludeできちゃう
* ストラクトもopenできちゃう

===

``` sml
signature T = sig
    include S
    val g: t -> t
end

structure N : T
= struct
    open M
    fun g x = x
end
```

===

# オペーク
----------

* シグネチャの型をトランスパレントにするかオペークにするか選べる
* `M: S` vs `M :> S`
* オペークなストラクチャの型は存在型になる
* `:>` をsealing operatorというらしい
* transparent(透明)とopaque(不透明)があるのでtranslucent(半透明)と呼ぶひともいるとかいないとか

===

``` sml
structure M: S = struct ... end
val _ = M.f 1 (* Ok *)
```

``` sml
structure M:> S = struct ... end
val _ = M.f 1 (* Error *)
```

===
# `sharing`
---------

* 2つの型が同じである制約を書ける
  + refinement??
* オペークな型に使うと便利
* 型が別のストラクチャのフィールドに依存するようになる

===

``` sml
signature X = sig
    structure M: sig type t end
    type t
    sharing type t = M.t
end
```

===

# Generative vs Applicative
--------------------------

* 同じストラクチャを同じファンクタを通したものは同じもの？違うもの？
* Generative → 違うもの
* Applicative → 同じもの
* SMLはGenerative
* OCamlはApplicative（らしい）

===

``` sml
(* 再掲 *)
signature S = sig
    type t
    val f: t -> t
    val zero: t
end

functor Id(X: S):> S
= struct
    open X
end

structure M1 = Id(M)
structure M2 = Id(M)

val _ = M1.f M2.zero (* Error *)
```

===

# [1ML – Core and Modules United (F-ing First-Class Modules)](https://people.mpi-sws.org/~rossberg/1ml/1ml.pdf)
<!-- .slide: class="center" -->

===

# 落合先生のフォーマット
---------

* どんなもの？
  + → MLのコア言語とモジュール言語を1つの言語に統一
* 先行研究と比べてどこがすごい？
  + → 第一級モジュールでありながら型が決定可能かつ型推論もある
* 技術や手法のキモはどこ？
  + → System Fωに変換した、型にsmallとlargeの区別を入れた
* どうやって有効だと検証した？
  + 証明、実装
* 議論はある？
  + 完全には型推論されないなど
* 次に読むべき論文は？
  + [F-ing Modules](https://people.mpi-sws.org/~dreyer/courses/modules/f-ing.pdf)
  + [1ML with Special Effects | SpringerLink](https://link.springer.com/chapter/10.1007/978-3-319-30936-1_18) WadlerFest 2016. Extends 1ML with effect polymorphism and generativity polymorphism.
===

# 1MLの論文
* 論文 A. Rosburg. [1ML – core and modules united (F-ing first-class modules)](http://dl.acm.org/citation.cfm?id=2784738). In ICFP, 2015.
 + https://people.mpi-sws.org/~rossberg/1ml/
* 拡張（次に読む）[1ML with Special Effects | SpringerLink](https://link.springer.com/chapter/10.1007/978-3-319-30936-1_18) WadlerFest 2016. Extends 1ML with effect polymorphism and generativity polymorphism.
* ベースになるやつ[F-ing Modules](https://people.mpi-sws.org/~dreyer/courses/modules/f-ing.pdf)
  + https://people.mpi-sws.org/~rossberg/f-ing/

===
# Abstract
-----------

* MLには2つの言語がある。コア言語とモジュール言語
  + コア: 型、式
  + モジュール: シグナチャ、ストラクチャ、ファンクタ
* コア言語の上の層にモジュール言語がある感じ
* それぞれを分離せずに扱えたら便利
  ```
  module Table = if size > threshold
                 then HashMap
                 else TreeMap
  ```
===
# Abstract (Cont.)
-----------

* 1MLでそれを実現した
  + 関数とファンクタと型コンストラクタが同じ表現
  + レコードとタプルとstructも同じ表現
  + てかほとんどをモジュールにエンコードする
* System Fωにエンコードできる程度の表現能力
  + 依存型までは使わない
  + 元々あったF-ing modulesの拡張
* ある意味ではSystem Fωのシンタックスシュガーと捉えることもできる
* ある程度の型推論もある

===

# 1. Intro - 既存研究からの流れ
<!-- .slide: class="center" -->

===

# packaged modules
------------------

* ocamlで採用されてるやつ
* first class moduleではない
  + module <-> 値は手でやる
  ```
  module Table = (val (if size > threshold
                       then (module HashMap : MAP)
                       else (module TreeMap : MAP)) : MAP)
  ```
* type sharingも弱い
  + `f : (module S with type t = ’a) → (module S with type t = ’a) → ’a`
  + 本来はこう書きたい`f : (X : S) → (S with type t = X.t) → X.t`
  + タイプコンストラクタに至ってはそもそも表現できない
* → もうちょっとリッチなのが欲しい

===

# first class modules
---------------------

* もしコアとモジュールの区別を完全になくしたら？
* なんかオペーク型にファンクタがきたりエグそう
* 実際[型が決定不能](https://dl.acm.org/citation.cfm?id=176927)
  + ファンクタと反変関数とサブタイプとopaqueのせい
  + opaqueを任意の型でサブタイプできるとマズいらしい
* 実用上も諸々問題ある
  + サブタイピングが入るとユニフィケーションが…とか
  + 型が束にならないので扱いづらいとか
* → リッチすぎると崩壊する


===

# first class modules
---------------------

```
type T = {type A; f : A → ()}
type U = {type A; f : (T where type A = A) → ()}
type V = T where type A = U
g (X : V) = X : U   (* V ≤ U ? *)

```

===
# F-ing modules
----------------
* これ自体はfirst class modulesではない
  + 論文内のapplicationでpackaged first class moduleに言及はしている
* moduleをF(ω)にエンコードすることに成功
  + 元々モジュールは[依存型ベースの議論がされていた](https://people.mpi-sws.org/~dreyer/courses/modules/macqueen86.pdf)
  + ファンクタがApplicativeだとωが必要らしい
* →F(ω)にエンコードできるのならある程度first-classに扱えるのでは？


===

# 1ML
-----
* moduleをfirst classに扱いつつFωにエンコードすることに成功
  + 但し一部にsmall typeしか使えない制約がある
* 既存のモジュールは不整合を防ぐための「構文的」制約が強すぎる→「意味論的」制約に緩和
* コア言語の方はSystem F
* [small typeとlarge type](https://dl.acm.org/citation.cfm?id=169696)に区別(カインドとは言わないんだね)
* small typeの推論はほぼできる（レコード幅についてのみ注釈必要）
  + 実用上まあ、しゃあないよね
  + SMLもレコード注釈必要な場面あるしね
* large typeは注釈必要
  + モジュールも元々そうだしね

===

## 1MLのContribution
--------------------
* First-Class Moduleをもちつつ決定可能な型システムの構築
* それのSystem Fωへのエンコード
* Damas/Milner-styleの型推論
* これらを使ったML方言の設計

===

# 2. 1ML with Explicit Types
<!-- .slide: class="center" -->

===

# 1ML with Explicit Types
-------------------------

* (4ページ目のFigure 1参照)
* bool型の他はレコード、関数、`type`型、transparent型、sharing制約など
* 関数はpureとimpureに分かれる
* `let`がレコードなどほとんどの構文は糖衣になってる
  + `(fun (n : int) ⇒ n + n) 3`
  + → `let f = fun (n : int) ⇒ n + n; x = 3 in f x`
  + → `{f = fun (n : int) ⇒ n + n; x = 3; body = f x} .body`

===

# どんなコードが書けるか
---------------------

* Functional Core
* Reified Types
* Translucency
* Functor
* Applicative vs Generative
* Higher Order Polymorphism
* Computed Modules
* Recursion
* Impredicativity Reloaded

===
## Reified Types
----------------

* 多相型やタイプコンストラクタなど
* 匿名モジュールの省略記法とも捉えられる

```
id = fun (a : type) ⇒ fun (x : a) ⇒ x
```
```
pair = fun (a : type) ⇒ fun (b : type) ⇒ type {fst : a; snd : b}
second = fun (a : type) ⇒ fun (b : type) ⇒ fun (p : pair a b) ⇒ p.snd
```

===

## Translucency

* opaque: 型が`type`で詳細が分からない
* transparent: 型の詳細が`(= type ...)`で書いてある
  + `(= E)` はシングルトン型。
  + `(= E)` は `E` のサブタイプだって。

```
size : type
pair : (a : type) ⇒ (b : type) ⇒ type
```

```
size : (= type int)
pair : (a : type) ⇒ (b : type) ⇒ (= type {fst : a; snd : b})
```


===

## Functor
----------

* だいたいMLっぽいシンタックスで書ける
* `empty a : map a;`が`empty : (a : type) ⇒ map a`などの構文糖がある
* なんか引数の型に依存してるっぽいけど後で消える
* sealing operatorがある
* type refinement syntaxがある

===


```
type EQ = {
  type t;
  eq : t → t → bool
};

type MAP = {
  type key;
  type map a;
  empty a : map a;
  add a : key → a → map a → map a;
  lookup a : key → map a → opt a
};

```

===

```
Map (Key : EQ) :> MAP where (type .key = Key.t) = {
  type key = Key.t;
  type map a = key → opt a;
  empty a = fun (k : key) ⇒ none a;
  lookup a (k : key) (m : map a) = m k;
  add a (k : key) (v : a) (m : map a) =
    fun (x : key) ⇒ if Key.eq x k then some a v else m x : opt a
}

```


===

## Applicative vs Generative
---------------------------

* 基本的にはSMLスタイルのGenerative Functor
  + 全てApplicativeだと面倒毎が起こるらしい
  + First-Class moduleとコンフリクトするとかなんとか
* 型コンストラクタも内部ではFunctor
  + `pair a b`とかもFunctor
  + 型コンストラクタはApplicativeであってほしい
* → 関数にPureとImpureの区別を付ける
  * pure(`=>`) はapplicative
  * impure(`->`) はgenerative
* ひとまずコア言語の方は全て`impure`で議論

===
## Higher Order Polymorphism
----------------------------

* コアにSystem F採用したからやりたい放題

```
f (id : (a : type) ⇒ a → a) = {x = id int 5; y = id bool true}
```

```
type SHAPE = {type t; area : t → float; v : t}
volume (height : int) (x : SHAPE) = height * x.area (x.v)
```

===
## Higher Order Polymorphism
----------------------------

```
type COLL c = {
  type key;
  type val;
  empty : c;
  add : c → key → val → c;
  lookup : c → key → opt val;
  keys : c → list key
};
entries c (C : COLL c) (xs : c) : list (C.key × C.val) = ...
```


===


```
type MONAD (m : type ⇒ type) = {
  return a : a → m a;
  bind a b : m a → (a → m b) → m b
};
```

===

## Computed Modules
--------------------


```
Table = if size > threshold then HashMap else TreeMap : MAP
```

===
## Predicativity
----------------

* `type`型はsmall type(型シグネチャに`type`型を持たない型、つまり単相型)にのみマッチできる
  + large typeは例えばこんなの`type T = (a : type) ⇒ {};`
* この制約によって型が決定可能になってる
* そもそもMLにはこんな制約は入ってるし既存のMLより表現力が劣ることはない
* 因みにTransparent Typeならこの制約はない

===

# 3. Type System and Elaboration
<!-- .slide: class="center" -->

===

# 明示的型付き1ML
------------------

* (6ページのFigure 2, 3を参考に)
* Fωに変換(elaborate)される
  + まずはSyntactic Type -> Semantic Type
  + 次にSemantic Type Directedにelaborate
* Fωは型の進行と保存が成り立つよ
* この辺はF-ingを読みながらやった方がいいかも

===

## Semantic Types
-----------------

* (7ページの左上の図を参考に)
* 型は `Ξ = ∃ α. Σ` に変換されるよ
  + ∃を外に出すことで依存型を避けてる
* 基本的にレコードのフィールドに出てくる`type`を∃にして外に出す戦略
* ただし関数の引数の位置では∀になる
  + 返り値でもこのパラメータを参照することで依存型を避ける
* pure(applicative)とimpure(generative)で∃の位置が変わる
  + なんか[スコーレム標準形](https://ja.wikipedia.org/wiki/%E3%82%B9%E3%82%B3%E3%83%BC%E3%83%AC%E3%83%A0%E6%A8%99%E6%BA%96%E5%BD%A2)にしたりして頑張るらしい
* transparent type `[= α]`もある

===

# Elaboration
-------------

* (8ページのFigure 4を参考に)
* 波矢印でelaboration規則
* グレーの規則は値レベル
* 細かい話はF-ingに書いてある
* サブタイピングは変換関数にelaborateされる

===

# Metatheory
-------------

* Sound
* System FωへのelaborationもSound
* elaborationはdecidable

===

# 4. Full 1ML
<!-- .slide: class="center" -->

===



## Full 1ML
-----------
* > A language without type inference is not worth naming ML.
* (10ページのFigure 5参考に)
* _ で推論
  + small typeしか推論できない
  + `type` が入っている型はダメ
* なんかimplicit functionが導入された
*  → 'a の引数を無言で受け付けるための関数
* implicit functionはpureな関数にのみ導入される
* →value restrictionより緩い制限
* 少しMLっぽくなった

===

```
type MAP = {
  type key;
  type map a;
  empty ’a : map a;
 lookup ’a : key → map a → opt a;
 add ’a : key → a → map a → map a
};
Map (Key : EQ) :> MAP where (type .key = Key.t) = {type key = Key.t;
 type map a = key → opt a;
 empty = fun x ⇒ none;
 lookup x m = m x;
 add x y m = fun z ⇒ if Key.eq z x then some y else m z
}
```

===

# 5. Type Inference
------------------

* (12ページのFigure 6を参考に)
* なんかつらそう
* サブタイピングあるとつらそう
  + → small typeに限るとほとんどtype equivalenceになるよ
  + 例外はrecord width
* 型推論はincomplete
  + record width
    - 前述
    - `r.id`って書いたときに`r`の型(`id`以外のフィールド)を決定できない
  + type scoping
    - よく分かんないけど推論のフェーズの問題で一般的な型にならないらしい？
  + purity annotations
    - effect subtypingのせい
* Sound
* terminates

===

# 6. Related Works
----------------
* Packaged Modules
  + 案外一杯あるっぽい
  + OCaml, Moscow ML, Alice ML
* First-Class Modules
  + レッドオーシャンっぽい
  + 歴史が書いてあるので読むと面白い
* Applicative Functors
  + Moscow MLにGenerativeとApplicative両方入ってるらしい
  + full applicative functorsはfirst class modulesとコンフリクトする
* 型推論
  + モジュールの型推論システムとかあるけど1MLはmonomorphic typeしか推論しないよ

===

# 7. Future Work
-------------

* 実装 (Toyはあるよ)
* Applicative Functors
* implicit (type class)
  + `type` しかないところに色々制約書けるようにしたらできそう
* 型推論（今のところ単純だよね）
* row polymorphismとかeffect polymorphismとか
* Recursive Module(既にSystem Fに落とす先行研究がある)
* 依存型

</textarea>
