---
categories: [Idris, Idris2, 型]
date: 2021-09-26T16:53:22+09:00
description: 関数型言語Idrisの次世代版として開発されているIdris 2の大きな特徴の1つとして型システムに数量的型理論（Quantitative Type Theory）を導入していることが挙げられます。このシステムではIdris 1にあった証明環境での利用可能な値の問題や、本来ならば実行時に不要な型を持ってしまっていた問題を解決します。一方で数量的型を便利に使うための仕組みであった多重度の部分型付けにより非健全性が入るなどの問題もありました。本発表ではIdris 2の数量的型のそういった特徴を紹介します。
title: "Idris 2の数量的型が解決した問題、導入した問題"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# Idris2の数量的型が解決した問題導入した問題
----------------------
[第一回関数型プログラミング（仮）の会 - connpass](https://opt.connpass.com/event/222709/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa2_vest.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

* κeen
* [@blackenedgold](https://twitter.com/blackenedgold)
* GitHub: [KeenS](https://github.com/KeenS)
* GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
* [Idein Inc.](https://idein.jp/)のエンジニア
* Lisp, ML, Rust, Shell Scriptあたりを書きます

===
Agenda
-------

* IdrisとIdris 2の紹介
* Idrisでの問題意識
* Idris 2での解決
* Idris 2の問題

===
# Idrisとは？
-------------

* *型駆動開発* のために作られた言語
* [最新版は1.3.3](https://github.com/idris-lang/Idris-dev)
* だいたい「依存型のあるHaskell」
* 読んで→[プログラング言語Idrisに入門させたい](https://zenn.dev/blackenedgold/books/introduction-to-idris)

``` haskell
main : IO ()
main = putStrLn "Hello World"
```

===
# Idris2とは？
-------------

* Idrisを改善した言語
  + 以後便宜的に現IdrisをIdris 1と呼ぶ
  + Idris 1とはおおむね互換
* [最新版は0.5.1](https://github.com/idris-lang/Idris2)
* Idris 1との違いの1つはコア言語が数量的型システムベース
* その他色々速いなど

===
# Idrisで依存型入門
-------------------

* 感覚的には「型の位置に値が置ける」システム
  + 計算もできる

``` haskell
import Data.Vect

v : Vect 3 Integer
v = [1, 2, 3]

-- 数値の計算
append: Vect n a -> Vect m a -> Vect (n+m) a
-- ...
```

===
# Idrisでの依存型
-------------------

* Idrisでは依存型に書いた値を取り出せる

``` haskell
import Data.Vect

len: Vect n a -> Nat
len {n} _ = n
```

===
# Idrisでの依存型
-------------------

* 内部的には型を引数で受け取ってる

``` haskell
import Data.Vect

len: {n:Nat} -> {a:Type} -> Vect n a -> Nat
len {n} _ = n
```

===
# Idrisで型駆動開発
------------------

* Idrisでは型を書いたらそれをガイドに実装できる
* →デモ

``` haskell
append: Vect n a -> Vect m a -> Vect (n+m) a
append xs ys = ?appendV_rhs

```

===
# Idrisで型駆動開発
------------------

```text
- + Main.appendV_rhs [P]
 `--                 a : Type
                     m : Nat
                    ys : Vect m a
                     n : Nat
                    xs : Vect n a
     --------------------------------------
      Main.appendV_rhs : Vect (plus n m) a
```

===
# Idris 1の型の問題点
---------------------

* 依存型の引数
* 型駆動開発の変数

===
# 依存型の引数
-------------

* 型にその型を値として使っているかの情報がない
  + 下の例でnは使っているがaは使っていないなどが分からない
* コンパイル時に型消去していいか分からない
  + → Idris 1はあんまり速くなかった

``` haskell
import Data.Vect

len: {n:Nat} -> {a:Type} -> Vect n a -> Nat
len {n} _ = n
```

===
# 型駆動開発の変数
------------------

* 型と値が両方環境に入っていて何が使えるのか分かりづらい

```text
- + Main.appendV_rhs [P]
 `--                 a : Type
                     m : Nat
                    ys : Vect m a
                     n : Nat
                    xs : Vect n a
     --------------------------------------
      Main.appendV_rhs : Vect (plus n m) a
```



===
# Idris 2の数量的型
------------------

* その型の値を使える回数を表現できる
  + 0, 1, n（∞）がある
  + 1は実験的
  + デフォルトはn
* 数値は半環になってればよい

===
# 例: id
---------

* デフォルトはnなので従来のプログラムと同じ

``` haskell
id : (x : a) -> a
id x = x
```

===
# 例: id1
---------

* xを1回しか使わないことを表明できる

``` haskell
id1 : (1 x : a) -> a
id1 x = x
```


===
# 例: double
-------------

* 1回以上使うとエラー

``` haskell
double : (1 x : a) -> (a, a)
double x = (x, x)
```

===
# 例: double
-------------

* 1回以上使うとエラー

``` text
1/1: Building playground (playground.idr)
Error: While processing right hand side of double. Trying to
use linear name x in non-linear context.

playground:10:16--10:17
 06 | id1 : (1 x : a) -> a
 07 | id1 x = x
 08 |
 09 | double : (1 x : a) -> (a, a)
 10 | double x = (x, x)
```

===
# 例: id0
---------

* 0は使えないのでこの関数は実装できない

``` haskell
id0 : (0 x : a) -> a
id0 x = x
```

===

# 例: id0
---------

* 0は使えないのでこの関数は実装できない

``` text
1/1: Building playground (playground.idr)
Error: While processing right hand side of id0. x is not accessible in this context.

playground:7:9--7:10
 3 | main : IO ()
 4 | main = putStrLn "Hello"
 5 |
 6 | id0 : (0 x : a) -> a
 7 | id0 x = x
             ^
```

===
# 例: ignore1
--------------

* 1を一度も使わないとエラー

``` haskell

```

===
# 例: ignore1
--------------

* 1を一度も使わないとエラー

``` text
Error: While processing right hand side of ignore1. There are 0 uses of linear name (implicit) _.

playground:7:13--7:15
 3 | main : IO ()
 4 | main = putStrLn "Hello"
 5 |
 6 | ignore1 : (1 x : a) -> ()
 7 | ignore1 _ = ()
                 ^^

Suggestion: linearly bounded variables must be used exactly once.
Error(s) building file playground.idr
```

===
# 数量的型による解決
## 依存型の型消去

* 使う型引数だけnにすればよい
  + 型引数は省略するとデフォルト0

``` haskell
len: {n:Nat} -> Vect n a -> Nat
len {n} _ = n
```

===
# 数量的型による解決
## 型駆動開発の変数問題

* 型に0がつくことで使える値と使えない値が分かりやくなる

``` text
 0 m : Nat
 0 a : Type
 0 n : Nat
   ys : Vect m a
   xs : Vect n a
------------------------------
hole : Vect (plus n m) a
```

===
# Idris 2の型の問題点
---------------------------

* 多重度が違うと違う型になるので扱いづらい
  + `id1` は `map` には渡せない
* v0.2までは多重度のサブタイピングをしていた
* → サブタイピングにより[健全性が壊れていた](https://github.com/idris-lang/Idris2/issues/73)
* 今はサブタイピングを止めた
  + 将来的には多重度の多相などが入るかも

===
# まとめ
---------

* 数量的型で依存型の実行効率が良くなった
* 数量的型で型駆動開発が便利になった
* 関数の世界が分かれるので扱いづらい面も
  + 将来改善されるかもね

</script>
</section>
