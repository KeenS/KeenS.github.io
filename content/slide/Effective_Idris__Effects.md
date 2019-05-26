---
categories: [Idris, "Algebraic Effect"]
date: 2019-05-25T15:27:24+09:00
description: "effect system勉強会での発表用。EffectのHandlerを書く話"
title: "Effective Idris: Effects"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# Effective Idris: Effects
----------------------
[effect system勉強会](https://connpass.com/event/124786/)
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
# 今日の話
--------------

* Effectのハンドラを自分で書く

===

# Effectsって？
--------------

* Idrisの標準添付ライブラリ
* Algebraic Effectの実装ライブラリ
* IOとかRandomとかStateとか色々あるよ

===
# 使ってみよう
-------------
Step 1

.ipkgに `effects` を足す

```
package hoge

version = "0.1.0"

-- ..

-- これ
pkgs = effects
```

===
# 使ってみよう
-------------
Step 2

Effectsをインポート

```idris
module Main

import Effects
import Effect.StdIO

```

===
# 使ってみよう
-------------
Step 3

Effectを使って好きなプログラムを書こう

```idris
hello : Eff () [STDIO]
hello = do
   putStr "what's your name: "
   name <- getStr
   putStr "what title do you prefer: "
   title <- getStr
   putStrLn $ "Hello " ++ name ++ title
   pure ()
```

===
# 使ってみよう
-------------
Step 3

`STDIO` Effectを走らせると `IO` モナドになるよ

```idris
main : IO ()
main = run hello
```

---

``` idris
$ ./main
what's your name: keen
what title do you prefer: -san
Hello keen-san
```

===
# Effect便利
------------

* 複数のEffectを1つの `do` 記法で書けるよ

``` idris
game : Eff () [STDIO, RND]
game = do
  -- set the random seed
  -- RND
  srand 111
  -- generate an random number
  n <- rndInt 0 100
  -- enter the main loop
  -- IO
  mainLoop n
```

===
# Effectの中身
-------------

* Effect = 返り値 \* リソース \* リソースの更新
* EFFECT = パラメータ \* Effect

```idris
Effect : Type
Effect = (x : Type) -> Type -> (x -> Type) -> Type

data EFFECT : Type where
     MkEff : Type -> Effect -> EFFECT

```


===
# Effectたち
-----------
State


```idris
data State : Effect where
  Get :      sig State a  a
  Put : b -> sig State () a b

STATE : Type -> EFFECT
STATE t = MkEff t State

get : Eff x [STATE x]
get = call $ Get

put : x -> Eff () [STATE x]
put val = call $ Put val
```

===
# Effectたち
-----------
StdIO

``` idris
data StdIO : Effect where
     PutStr : String -> sig StdIO ()
     GetStr : sig StdIO String
     PutCh : Char -> sig StdIO ()
     GetCh : sig StdIO Char

STDIO : EFFECT
STDIO = MkEff () StdIO

-- putStr, putStrLnなどなど
```

===
# Effect文脈
-------------

* `hello` についてた `Eff () [STDIO]` の話
* 複数の `Effect` をまとめた「どんな種類のEffectが起きるか」の文脈。
* `Eff` は元を辿ると長い `EffM`
* `EffM` = モナドっぽいの \* 返り値 \* Effect文脈 \* 文脈更新関数

``` idris
data EffM : (m : Type -> Type) -> (x : Type)
            -> (es : List EFFECT)
            -> (ce : x -> List EFFECT) -> Type where
  -- ...
```

===
# Effect文脈
-------------

* Effectの更新をしないならもう少し簡単にできる
* さらにモナドっぽいのは一般化して使うので省略してよさそう

``` idris
Eff : (x : Type) -> (es : List EFFECT) -> Type
Eff x es = {m : Type -> Type} -> EffM m x es (\v => es)

EffT : (m : Type -> Type) -> (x : Type) -> (es : List EFFECT) -> Type
EffT m x es = EffM m x es (\v => es)
```

===
# Effect文脈
-------------

* 重要なのは `EffM` はモナドトランスフォーマじゃ **ない** 点
  + Effectの更新が入る
  + Extensible Effectとはそこが違う？
* Idrisの `do` 記法は `(>>=)` の糖衣構文なのでモナドじゃなくても使える

``` idris
(>>=)   : EffM m a xs xs' ->
          ((val : a) -> EffM m b (xs' val) xs'') -> EffM m b xs xs''
(>>=) = EBind
```

===
# `run` について
----------------

* モナドっぽいものはモナドじゃない
  + `pure` さえあればいい
* `m` を `Identity` にとればただの型にもなる
 + `IO` とかのため一般化
* あと本来はEffectの初期値も必要

```idris
run : Applicative m =>
      (prog : EffM m a xs xs') -> {default MkDefaultEnv env : Env m xs} ->
      m a
runWith : (a -> m a) -> Env m xs -> EffM m a xs xs' -> m a
runPure : (prog : EffM Basics.id a xs xs') ->
          {default MkDefaultEnv env : Env Basics.id xs} -> a

```

===
# 小まとめ
----------

* Effect単体は `Effect`
* それらをまとめた文脈が `Eff a [EFFECT]`
* `Eff` を `run` するとモナドっぽいものが出てくる

===
# ハンドラの話
-------------

* 所望の `Effect` に `Handler` を実装すると `run` できるようになる
* そのときに実装する型で `run` のあとに取り出せる型が変わる

``` idris
interface Handler (e : Effect) (m : Type -> Type) where
  handle : (r : res) -> (eff : e t res resk) ->
           (k : ((x : t) -> resk x -> m a)) -> m a
```


===
# ハンドラの例
-------------

* この例だと `StdIO` を `run` すると `IO` モナドが出てくることが分かる


``` idris
implementation Handler StdIO IO where
    handle () (PutStr s) k = do putStr s; k () ()
    handle () GetStr     k = do x <- getLine; k x ()
    handle () (PutCh c)  k = do putChar c; k () ()
    handle () GetCh      k = do x <- getChar; k x ()
```


===
# ハンドラを書いてみよう
-------------

* `IO` だと扱いづらいよね
* 入力を文字列リストで与えて出力を文字列リストで取り出せるpureなハンドラを書いてみよう
* 以下の型の関数を書くのが目的

``` idris
runToStr : List String -> Eff ret [STDIO] -> (ret, List String, List String)
runToStr input eff = ?unimplemented
```

===
# 型定義
-------

* 入力と出力でそれぞれ `State` を使うよ
  + 外側が出力、内側が入力
  + タプルにして1つのステートにした方がよかったかも？
* `run` するとこの型が出てくるよ

``` idris
StringIO : Type -> Type
StringIO a = StateT (List String) (State (List String)) a
```

===
# 操作関数
-----------

* 入力は色々面倒くさい

``` idris
read : StringIO String
read = do
    ss <- lift get
    let hd = fromMaybe "" $ head' ss
    let tl = fromMaybe [] $ tail' ss
    lift $ put tl
    pure hd

write : String -> StringIO ()
write s = modify (\ss => the (List String) (s::ss))
```

===
# ハンドラ
----------

* インターフェースの実装には型エイリアスは書けないみたい？
* `StdIO` を `run` すると `StateT (List String) (StateT (List String) Identity)` が出てくるよ！

``` idris
implementation Handler StdIO (StateT (List String) (StateT (List String) Identity)) where
    handle () (PutStr s) k = do write s; k () ()
    handle () GetStr     k = do s <- read; k s ()
    handle () (PutCh c)  k = do write $ singleton c; k () ()
    handle () GetCh      k = (k ' ' ())
```

===
# `runToStr`
------------

* ハンドルしよう
* `run` が勝手に `StringIO` を推論してることに注目

``` idris
runToStr : List String -> Eff ret [STDIO] -> (ret, List String, List String)
runToStr input eff = let
   writer = run eff
   reader = runStateT writer (the (List String) [])
   ((ret, output), input) = runState reader input
   in (ret,  output, input)
```

===
# 実行
------

* テストするときとかは `print` せずに結果を比較しようね

``` idris
main : IO ()
main = printLn $ runToStr ["keen", "-san"] hello
```

---

``` console
$ ./main
((), (["Hello keen-san\n", "what title do you prefer: ", "what's your name: "], []))
```

===
# まとめ
--------

* Effectsは `Eff` 、 `Effect` 、 `run` をおさえよう
* `Effect` の `Handler` は上書きできるよ
* `run` した結果は `Handler` が実装されてる型だよ

===

# 参考文献
----------

* [The Effects Tutorial](http://docs.idris-lang.org/en/latest/effects/introduction.html)
* Edwin Brady. 2013. Programming and reasoning with algebraic effects and dependent types. SIGPLAN Not. 48, 9 (September 2013)


</script>
</section>
