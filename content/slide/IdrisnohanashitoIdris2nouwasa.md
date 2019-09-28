---
categories: ["Idris", "依存型"]
date: 2019-09-21T22:11:57+09:00
description: "Proof Summit 2019での発表用。Idrisと依存型と証明"
title: "Idrisの話とIdris2のウワサ"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# Idrisの話とIdris2のウワサ <!-- .element: style="font-size: 60pt" -->
----------------------
[Proof Summit 2019](https://proof-summit.connpass.com/event/141191/)
<!-- .slide: class="center" -->

===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# Idris
-------

* Edwin Brady 2013 〜
* 証明支援系ではなくてプログラミング言語
* 依存型のあるHaskell
* Eager Evaluation
* コンパイルするとバイナリが出る
  + C経由
  + JSバックエンドなども

===

# Hello, Idris
--------------

```idris
main : IO ()
main = putStrLn "Hello"
```

---

``` console
$ idris -o hello hello.idr
$ ./hello
"Hello"
```

===
# 依存型
---------------

* 値を型に書ける

```idris
data Vect :
  (len : Nat)
  -> (elem : Type)
  -> Type
where
  Nil  : Vect Z elem
  (::) : (x : elem)
         -> (xs : Vect len elem)
         -> Vect (S len) elem
```

---

```idris
v : Vect 3 Int
v = [1, 2, 3]
```


===
# 依存型
---------------

* 型で計算がでる

```console
append: Vect n a
        -> Vect m a
        -> Vect (n+m) a
append [] y = y
append (x :: xs) y = x :: append xs y
```

===
# 依存型
---------------

* 値として型を書ける

```idris
the : (a : Type) -> a -> a
```

---

``` idris
λΠ> 1
1 : Integer
```

---

``` idris
λΠ> the Double 1
1.0 : Double1
```

===
# 型と型の型
------------

* `(a : Type)` って何
* `a` : 名前
  + Idrisでは型シグネチャにも名前が書ける
* `Type` : `a` の型
* `Type` 型の値
  + `Int`
  + `Double`
  + `Vect 3 Int`
  + …

===
# 型と型の型
------------

* `Type` の型は？
  + → `Type 1`
* `Type 0` (=`Type`), `Type 1` , `Type 2` … と続く
  + Idrisの文法上は `Type n` とは書けない
* (Demo1.idr)

===
# 証明の話
----------

* もちろんカリー・ハワード対応で証明が書ける
  + プログラミング言語と論理学に対応関係がある
  + 型 ⇔ 命題
  + プログラム ⇔ 証明
  + …
* Idrisには依存型がある→述語論理の証明が書ける
* 多少証明専用の機能もある

===
# Modus Ponens
--------------

* $A \to (A \to B) \to B$
* 大文字は定数扱いなのでカインド宣言 `{A, B: Type}` を挟んでおく

```idris
total
modusPonens : {A, B: Type}
              -> A
              -> (A -> B)
              -> B
modusPonens a ab = ab a
```

===
# `partial` と `total`
----------------------

* 一般にプログラムが停止するかは判定できない
* Idrisは型にプログラムを書ける
* → コンパイル終わるの？
* → 証明として見たときに循環論法になったりしないの？
    ``` idris
    specialTheorem : {A, B: Type} -> A -> B
    specialTheorem x = specialTheorem x
    ```

===
# `partial` と `total`
----------------------

* 関数に `partial` や `total` の修飾子を付けられる
  + デフォルトで `partial`
* `total` を付けるとIdrisが停止すると確認できるものしかコンパイルが通らない


===
# `partial` と `total`
----------------------

``` idris
total
specialTheorem : {A, B: Type} -> A -> B
specialTheorem x = specialTheorem x
```

---

```console
   |
21 | specialTheorem x = specialTheorem x
   | ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Main.specialTheorem is possibly not total due to recursive path Main.specialTheorem --> Main.specialTheorem
```

===
# `partial` と `total`
----------------------

* 再帰でもこっちは通る
  + 構造的に最初の引数が小さくなってるから

``` idris
total
append: Vect n a
        -> Vect m a
        -> Vect (n+m) a
append [] y = y
append (x :: xs) y = x :: append xs y
```

===
# 型環境とHole
--------------

* プログラムの一部を未完成のまま(=Hole)コンパイルできる
* `?ident` でHoleを作れる
    ```idr
    map : List a -> (a -> b) -> List b
    map xs f = ?hole
    ```
* IdrisがHoleの型を教えてくれる

===
# 型環境とHole
--------------

```idr
map : List a -> (a -> b) -> List b
map xs f = ?hole
```

---

```console
              b : Type
              a : Type
             xs : List a
              f : a -> b
     --------------------
           hole : List b
```

===
# Type Driven Development
-------------------

* Holeといくつかのコマンドを使うと型からプログラムを生成できる
* プログラム⇔証明でいうと証明支援に対応
* 「型定義から本体の雛形を作る」
* 「引数でパターンマッチする」
* 「Holeをいい感じに埋める」

===
# Type Driven Development
-------------------

```idris
append : Vect n a
         -> Vect m a
         -> Vect (n + m) a
```

===
# Type Driven Development
-------------------

型定義から本体の雛形を作る

```idris
append : Vect n a
         -> Vect m a
         -> Vect (n + m) a
append xs ys = ?append_rhs
```

===
# Type Driven Development
-------------------

引数 `xy` でパターンマッチする

```idris
append : Vect n a
         -> Vect m a
         -> Vect (n + m) a
append [] ys = ?append_rhs_1
append (x :: xs) ys = ?append_rhs_2
```

===
# Type Driven Development
-------------------
Hole `append_rhs_1` をいい感じに埋める

```idris
append : Vect n a
         -> Vect m a
         -> Vect (n + m) a
append [] ys = ys
append (x :: xs) ys = ?append_rhs_2
```

===
# Type Driven Development
-------------------

Hole `append_rhs_2` をいい感じに埋める

```idris
append : Vect n a
         -> Vect m a
         -> Vect (n + m) a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys
```


===
# Dependent Pair
----------------

* 述語論理だから $\forall$ と $\exists$ が書きたいよね
* $\forall x \in T, P(x)$ ⇔ `(x: T) -> P x`
  + こっちは組み込みの機能
* $\exists x \in T, P(x)$ ⇔ `x: T ** P x`
  + こっちはユーザランドで定義
    ```idris
    data DPair : (a : Type) -> (P : a -> Type) -> Type where
      MkDPair : .{P : a -> Type} -> (x : a) -> (pf : P x) -> DPair a P
    ```

===
# Dependent Pair
----------------

* ${}^\exists n \in \mathbf{N} \to$ `Vect n Int`

```idris
someVect: (n: Nat ** Vect n Int)
someVect = (_ ** [1, 2, 3])
```

===
# Dependent Pair
----------------

* 実用的(?)な例
* `filter` したあとの長さは分からないのでDPairを使う

```idris
filter: (a -> Bool)
        -> Vect n a
        -> (p ** Vect p a)
filter p [] = (\_ ** [])
filter p (x :: xs) with (p x, filter p xs)
   filter p (x :: xs) | (True, (_ ** xs')) = (_ ** x :: xs')
   filter p (x :: xs) | (False, (_ ** xs')) = (_ ** xs')
```


===
# Reification(?)
-------------

* 型情報を実行時に取り出せる
  + やばいよね

```idris
length : Vect n a -> Nat
length {n=n} _ = n
```

===
# 証明っぽい証明
---------------

* 簡単な命題の書き方は分かった
* もう少し証明らしい証明を書いてみる
  + 証明特有の書き方みたいなのがある

===
# 1 + 1 = 2
------------
命題

```idris
total
onePlusOneEqualsTwo : 1 + 1 = 2
```

===
# 1 + 1 = 2
------------

* コマンドだけで証明できる
* `Refl` で `a = a` の証明

```
total
onePlusOneEqualsTwo : 1 + 1 = 2
onePlusOneEqualsTwo = Refl
```

===
# 自然数
--------

* 自然数もデータ型で定義
  + 1進数(`S` の数 = 数値)
* 最適化で多倍長整数になるらしい

``` idris
data Nat =
  ||| Zero
  Z |
  ||| Successor
  S Nat

```

---

```idris
three : 3
three = S (S (S Z))
```

===
# 足し算
--------

``` idris
total plus : (n, m : Nat) -> Nat
plus Z right        = right
plus (S left) right = S (plus left right)
```


===
# n + m = m + n
---------------

* ちょっと長い
  + いくつかの関数に分ける
* いくつかの機能を使う
  + 依存型のパターンマッチができる
  + `rewrite <式> in <式>` で結果の型をrewriteできるよ
  + `%default total` で全部の関数を `total` に宣言できる

===
# n + m = m + n
---------------
## `0 + m = m + 0`

```idris
%default total

plus_commutes_Z : Z + m = m + Z
plus_commutes_Z {m=Z} = Refl
plus_commutes_Z {m=(S k)} =
    rewrite plus_commutes_Z {m=k} in
    Refl
```

===
# n + m = m + n
---------------
## `n = n + 0`

```idris
plus_reduces_Z: {n: Nat} -> n = n + Z
plus_reduces_Z {n=Z} = Refl
plus_reduces_Z {n=(S k)} =
    rewrite plus_reduces_Z {n=k} in
    Refl
```


===
# n + m = m + n
---------------
## `S (m + k) = m + (S k)`

```idris
plus_commutes_S : S (plus m k) = plus m (S k)
plus_commutes_S {k=k} {m=Z} =
    rewrite plus_reduces_Z {n=k} in
    Refl
plus_commutes_S {k=k} {m=(S j)} =
    rewrite plus_commutes_S {k=k} {m=j} in
    Refl
```


===
# n + m = m + n
---------------

```idris
plus_commutes: {n, m: Nat} -> n + m = m + n
plus_commutes {n = Z} = plus_commutes_Z
plus_commutes {n = (S k)} {m=m} =
    rewrite plus_commutes {n=k} {m=m} in
    plus_commutes_S
```


===
# タクティックの話
------------------

* いちおう、ある
* 昔： Tacticというのがあった
  + 今はdeprecated
* 今： Elaboration Reflectionがある
  + メタプログラミングっぽさ
* どっちも正直つらい
  + ドキュメントほとんどない
  + エラーが分からない

===
# Elaboration
--------------

```idris
modusPonens' : p -> (p -> q) -> q
modusPonens' = %runElab (do
  intro `{{Hp}}
  intro `{{Hpq}}
  apply (Var `{{Hpq}}) [False]
  solve
  hypothesis
)
```

===
# Idris2のウワサ
------------

* Idris実装
* バックエンドはChez Scheme
  + Cバックエンドより速いらしい
* Quantattive Type Theory
  + Linear Typeの拡張っぽい
  + 値が何回使えるかが型に付く
* 正直まだ書けない
  + IDEプロトコルが未完成

===
# 他の定理証明支援系との比較
---------------------

* Agda vs Idris: 違いが分からん
  + meta varやimplicit argumentsの扱いが違うらしい
* Coq vs Idris: 言語が3つに分かれてない
* Lean vs Idris: 違いが分からん
* Haskell vs Idris: Idrisはトップレベルの型をユーザに書かせるのであんまり複雑にならないらしい
* **Idrisはプログラミング言語**

===
# まとめ
--------

* 依存型の使えるプログラミング言語Idrisがあるよ
* 命題と型、証明とプログラムは対応するよ
* 依存型のある言語だと述語論理が証明できるよ

</script>
</section>
