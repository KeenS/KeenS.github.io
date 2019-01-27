---
categories: ["Idris", "Idris入門"]
date: 2019-01-25T00:00:30+09:00
title: "Idris入門: リファレンス"
---

κeenです。[Idris入門: 数当てゲーム | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2019/01/07/idrisnyuumon__kazuatege_mu/)、[Idris入門: 二分木 | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2019/01/17/idrisnyuumon__nibungi/)で紹介したものをまとめつつ、適当に飛ばしていた部分を補います。
<!--more-->

ぶっちゃけ公式ドキュメント見れば良さそうなんですがself-containedでない記事も嫌なので2つのチュートリアルで出てきたものを拾います。
ここではチュートリアルに出てきた構文しか拾わないのでその他の構文は各自書きながら覚えて下さい。

念を押しますが私はIdrisにはそんなに詳しくないです。勢いで書いてるだけです。一次情報は[公式ドキュメント](http://docs.idris-lang.org/en/latest/)です。

# 構文
## 変数と型の字句

ドキュメントには書かれてないのですが、コンパイラのコードを見る限り識別子に使えるのは `[a-zA-Z][a-zA-Z'_.]*` のようです。
例えば `hog'.'e12_` は適格なIdrisの変数定義です。

変数に小文字始まり、型やバリアントには大文字始まりの識別子をつけるみたいです。

``` idris
data Hoge = Fuga

piyo : Hoge -> ()
piyo hoge = ()
```

一応型やバリアントに小文字を使ったり変数に大文字を使ったりもできますが色々不都合が生じます。


## `data`

代数的データ型と呼ばれるものです。色々な使いみちがあります。
例えば、列挙型のように可能な値を列挙することができます。

```idris
data Direction = Up | Down | Right | Left
```

ここでの `Up` 、 `Down` 、 `Right` 、 `Left` のように列挙されたものをバリアント(列挙子)と呼びます。
バリアントそれぞれがコンストラクタになっています。どのバリアントを書いても `Direciton` 型の値が作れます。

``` idris
λΠ> Up
Up : Direction
λΠ> Down
Down : Direction
```

列挙子はそれぞれ値を持てます。

``` idris
data Few = None
         | One Int
         | Two Int Int
         | Three Int Int Int
```

値を持つ列挙子は単体では関数のように振る舞います。

``` idris
λΠ> One
One : Int -> Few
```

具体的な値を渡してあげると `Few` 型の値を作れます。

``` idris
λΠ> One 3
One 3 : Few
```

代数的データ型のバリアントを1つにすれば構造体のような使い方もできます。


```idris
data Person = MkPerson Int String
```

`data` からデータを取り出すにはパターンマッチを使います。

``` idris
getName : Person -> String
getName (MkPerson age name) = name
```

コンストラクタと同じ形でエクストラクタを書けるので視覚的に分かりやすいですね。


### ジェネリクス

`data` 文はジェネリクスにできます。つまり、型引数を取れます。
名前に続けて型引数を導入し、定義部分で使います。小文字で書くと型変数になるようです。

``` idris
data Tree a = Leaf | Node (Tree a) a (Tree a)
```

何も書いてませんでしたが型は自身を参照することもできます。
括弧は結合の優先順位を決めるのに使います。

複数の引数を受け取るときは空白で並べて書きます。

``` idris
data Either a b = Left a | Right b
```

### タプル

タプルは組です。いくらでも異なる型のデータをまとめられます。

``` idris
λΠ>  (1, 2.0, "3")
(1, 2.0, "3") : (Integer, Double, String)
```

その実はデータ型 `Pair` です。上記の定義は以下と同じです。

``` idris
λΠ>  MkPair 1 (MkPair 2.0 "3")
(1, 2.0, "3") : (Integer, Double, String)
```

ところで、二分木のところで

> 今更ですが、ちょくちょく出てきていた () は空のタプルのことでした。

と書きましたがこれは誤りでした。 `()` は明らかにペアじゃないですね。`()` はそれで1つのデータ型、 `Unit` 型です。

``` idris
λΠ> MkUnit
() : ()
```


### GADT

`data` は別の記法もあります。
正式な呼び方がわからないのですが、[ドキュメント](http://docs.idris-lang.org/en/latest/reference/syntax-guide.html#data-types)ではGADTスタイル構文と書かれています。以下のような構文です。

``` idris
data 名前 : カインド where
  バリアント1: 型
  バリアント2: 型
  ...
```

バリアントは関数としても振る舞うと説明しましたが、ここで書くバリアントの型は関数の型そのものなので分かりやすいですね。
因みにカインドとは型の型です。今回型を定義していて、その型を書くので型の型が必要になります。

先程の `Few` はGADTスタイル構文でこのようにも書けます。

``` idris
data Few : Type where
  None : Few
  One : Int -> Few
  Two : Int -> Int -> Few
  Three : Int -> Int -> Int -> Few
```

カインドは今回は `Type` ですし、ジェネリクスのように型パラメータを取る型なら `Type -> Type` にもなるでしょう。

Idrisは型の記述が柔軟なので明示的に型を書くこのスタイルだと都合が良いことが多いのです。

また、GADTだと各引数に名前を付けられるので構造体風定義もわかりやすくなります。

``` idris
data Person : Type where
  MkPerson:  (age : Int) -> (name : String) -> Person
```

これに構文糖衣をかぶせると [レコード](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#records) 構文になります。


## 値/関数定義

最初のチュートリアルで紹介したように以下の構文で値を定義します。

``` idris
名前 : 型
名前 = 式
```


関数の場合は引数を並べて書きます。

``` idris
名前 : 型
名前 引数1 引数2 .. 引数n = 式
```

Idrisは式指向ですので`return`文のようなものはありません。関数の本体は計算したい式だけ書けば十分です。

引数の位置ではパターンマッチができます。その際は節を並べます。

``` idris
showDirection : Direction -> String
showDirection Up =  "Up"
showDirection Down = "Down"
showDirection Right = "Right"
showDirection Left = "Left"
```

小文字で始まる識別子だと変数になります。


``` idris
moveHorizontal : Direction  -> Integer
moveHorizontal Up = 1
moveHorizontal Down = -1
moveHorizontal other = 0
```


マッチした変数を使わないなら特別なパターン `_` で無視することもできます。

``` idris
moveHorizontal : Direction  -> Integer
moveHorizontal Up = 1
moveHorizontal Down = -1
moveHorizontal _ = 0
```

つまり、関数定義はもう少し複雑で、こういう構文です。

``` idris
名前 : 型
名前 バターン1_1 バターン1_2 .. バターン1_n = 値
名前 バターン2_1 バターン2_2 .. バターン2_n = 値
...
名前 バターンm_1 バターンm_2 .. バターンm_n = 値
```


## 中置演算子

Idrisではユーザが任意の中置演算子を定義できます。 `:+-*\/=.?|&><!@$%^~#` の組み合わせを中置演算子に使えます。

中置演算子を宣言するには `infixl 優先度 記号` または `infixr 優先度 記号` を使います。それぞれ左結合、右結合の演算子を定義します。
例えば `+?` を左結合の優先度4の演算子として定義するには以下のように書きます。

``` idris
infixl 4 +?
```

中置演算子を定義する時は括弧で包んで関数と同じように定義します。

``` idris
(+?): Maybe Integer -> Integer -> Maybe Integer
(+?) (Just x) y = Just (x + y)
(+?)  _       _ = Nothing
```

こう定義してしまえば自由に使えます。

``` idris
λΠ>  (Just 1) +? 2 +? 3
Just 6 : Maybe Integer
λΠ>  (Nothing) +? 2 +? 3
Nothing : Maybe Integer
```

また、括弧で包めば関数としても使えます。

``` idris
λΠ>  (+?)
(+?) : Maybe Integer -> Integer -> Maybe Integer
```

データ型のバリアントを中置演算子で定義することもできます。
これは標準ライブラリで定義されている例です。

``` idris
infixr 10 ::
data List a = Nil | (::) a (List a)
```

### 関数を中置で

任意の2引数関数を `` ` `` で囲むと中置で書けます。バックティック記法と呼ばれるようです。
バリアントも関数でしたからこのようにも書けます。

``` idris
1 `Two` 2
```

また、パターンマッチでも同様に中置記法で書けます。

``` idris
sum : Few -> Int
sum None = 0
sum (One x) = x
-- 中置記法でパターンマッチ
sum (x `Two` y) = x + y
sum (Three x y z) = x + y + z

```

## `case` とパターン

関数の引数でパターンマッチができますが、`case` を使えばそれ以外の場所でもパターンマッチができます。

パターンはだいたいコンストラクタと1対1対応します。パターンはネスト可能です。
例えばFizz Buzz判定するパターンマッチはこう書けます。

``` idris
fizzBuzz : Integer -> String
fizzBuzz n = case (n `mod` 3, n `mod` 5) of
             (0, 0) => "FizzBuzz"
             (0, _) => "Fizz"
             (_, 0) => "Buzz"
             _      => show n

```

`case` 式があれば不要に思えますが `if` 式もあります。

## `let`

計算の途中式に名前をつける構文です。

``` idris
let パターン = 式 in 式
```

のように使います。

``` idris
quadruple : Integer -> Integer
quadruple x = let double = x + x in double + double
```


## インターフェース

インターフェースはデータ型間で共通の操作を定義する機能です。 インターフェースは以下の構文で定義でます。

``` idris
-- わかりやすさのため微妙に嘘をついてます
interface インターフェース名 対象の型 where
    関数1 : 型
    関数2 : 型
    ...
    デフォルト実装(あれば)
    ...

```

`where` 以降はオフサイドルールですね。インターフェースの関数はメソッドと呼びます。

例えば同値比較のインターフェースはこう定義されています。

``` idris
interface Eq a where
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool

    x /= y = not (x == y)
    x == y = not (x /= y)
```

ジェネリクス型にインターフェース制約をつけるにはこう書きます。

``` idris
インターフェース名 対象の型 => 型
```

複数のインターフェース制約はこうです。

``` idris
インターフェース名 対象の型 => インターフェース名 対象の型 => .. => 型
```

例えば `Either` 型の値を文字列にする関数はこう書けます。

``` idris
showEither : Show a => Show b => Either a b -> String
showEither (Left l) = "Left " ++ show l
showEither (Right r) = "Right " ++ show r
```

## `do` 記法
`do` 記法は[`Monad`](http://docs.idris-lang.org/en/latest/tutorial/interfaces.html#monads-and-do-notation)インターフェースのための構文糖衣です。`IO` だけでなく `Monad` を実装している型ならなんでも `do` 記法が使えます。
インターフェースの構文糖衣なので `do` そのものには意味がないのですが、キモチとしては「中身を取り出す」ような働きをします。
例えば `Maybe` は `Monad` を実装しているのでこう使えます。

``` idris
addMaybe : Maybe Integer -> Maybe Integer -> Maybe Integer
addMaybe x y = do
  x' <- x
  y' <- y
  Just $ x' + y'
```

このコードのキモチは `Maybe` に包まれた `x` と `y` から`Integer` 型の値 `x'`, `y'` を取り出し、足し算しています。もちろん取り出せるのは `Just` のときのみで、 `Nothing` のときはすぐさま `Nothing` が返ります。

### パターン

`<-` で取り出した値に更にパターンマッチできます。
`パターン <- 式 | パターン => 式 | パターン => 式 ...`の形です。

荒唐無稽な例ですが受け取った値が1と2だった場合だけ足し算するにはこう書きます。

``` idris
addOneAndTwo : Maybe Integer -> Maybe Integer -> Maybe Integer
addOneAndTwo x y = do
  1 <- x
    | _ => Nothing
  2 <- y
    | _ => Nothing
  Just $ 1 + 2

```


## コメントとドキュメントコメント

`--` ではじまるのがコメント、 `|||` ではじまるのがドキュメントコメント。

[ドキュメントコメント](http://docs.idris-lang.org/en/latest/reference/documenting.html)は色々書式があるみたいです。

## 名前空間

Idrisのコードは以下のような形になっています。

``` text
<モジュールヘッダ>?

<インポート文>

<コード>

```


### モジュールヘッダ

`module モジュール名` です。あってもなくてもいいです。1ファイル1モジュールです。
ドットで区切った階層構造です。一応ファイル名とは関連がないことになってますが、`Foo/Bar.idr` には `module Foo.Bar` モジュールを定義するのが通例です。たまに `Foo.Bar` と書いてモジュールではなくファイル `Foo/Bar.idr` のことを指す記法もあるので注意が必要です。

### インポート文

`import モジュール名` です。人によっては混乱すると思いますが、 `import Data.Vect` で `Vect` モジュール内の全てのアイテムが今の名前空間にインポートされます。
単純にモジュール名を略記したいなら `import Data.Vect as V` などで別名をつけることになります。

# 値と型
## 関数

関数も値です。気軽に変数を束縛できます。

``` idris
add : Integer -> Integer -> Integer
add = (+)
```

Idrisの関数は1引数をとり、1返り値を返します。複数の引数を受け取りたい時は関数をネストすると複数の引数を受け取れるように見えます。

例えば以下の2つの定義は同等です。

``` idris
add: Integer -> Integer -> Integer
add = \x => \y => x + y

add : Integer -> Integer -> Integer
add x y =  x + y

```

同様に、関数適用も1引数の適用の連続です。
以下の2つは同等の表現です。

``` idris
add 1 2
(add 1) 2
```

型も、 `Integer -> Integer -> Integer` は `Integer -> (Integer -> Integer)` の意味です。

複数引数関数にみえるのは実は関数の集合体なので中途半端に使うこと(部分適用)もできます。

``` idris
inc : Integer -> Integer
inc = add 1
```

## 型の扱い

Idrisでは型も第一級です。型の型は `Type` です。

``` idris
λΠ> Int
Int : Type
```

ジェネリクスになっている型は引数を受け取ると型になる、要は型を受け取って型を返す関数なので`Type -> Type` 型になります。

``` idris
λΠ> Maybe Int
Maybe Int : Type
λΠ> Maybe
Maybe : Type -> Type
```

ジェネリクスと関数適用が同じ構文な理由が理解できたかと思います。
型エイリアスも値と同じように定義できます。

``` idris
Option : Type -> Type
Option = Maybe
```

余談ですが `Type` の型は `Type 1` です。

``` idris
λΠ> :t Type
Type : Type 1
```

あとは想像出来ますね。 `Type 1 : Type 2`, `Type 2 : Type 3`, .. とずっと続いていきます。

## プリミティブ型

Idrisのプリミティブ型は思ったより少ないです。


* `Int` : 固定長整数
* `Integer` : 多倍長整数
* `Double` : 倍精度浮動小数点数
* `Char` : 文字
* `String` : 文字列
* `Ptr` : FFI用

あとは裏では8bitから64bitまでの整数もあるようですがドキュメントには載ってないです。

真偽値などはライブラリで定義されています。

``` idris
data Bool = False | True
```

# ライブラリ
ドキュメントに記述が見つけられなかったのですが、Idrisはデフォルトでいくつかのライブラリをリンクしています。

* プリミティブ : 処理系組み込みの機能
* ビルトイン : Idrisで書かれているが処理系が特別扱いする
* [プレリュード](https://www.idris-lang.org/docs/current/prelude_doc/) : デフォルトでインポートされているライブラリ群
* [ベース](https://www.idris-lang.org/docs/current/base_doc/) : インポートはされていないがいつでもインポートできるライブラリ群

プリミティブは型のところで紹介したプリミティブ型の他に雑多な関数などがあるようです。ビルトインはタプルやユニット、 `ifThenElse` などです。

プレリュードはトップレベル関数相当で、例えば何気なく使っていた `putStrLn` は[`Prelude.Interactive`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Interactive.html)で定義されています。

ベースはデフォルトでリンクするライブラリで、標準ライブラリ相当です。[`Data.Complex`](https://www.idris-lang.org/docs/current/base_doc/docs/Data.Complex.html)や[`System`](https://www.idris-lang.org/docs/current/base_doc/docs/System.html)のようなものがあります。

その他にもIdrisと一緒に配布されている [contrib](https://www.idris-lang.org/docs/current/contrib_doc/)や[effects](https://www.idris-lang.org/docs/current/effects_doc/)なんかもあります。[`Data.SortedMap`](https://www.idris-lang.org/docs/current/contrib_doc/docs/Data.SortedMap.html)なんかもcontribにあるのでcontribを使う機会は多いでしょう。

以下に、出てきた関数やデータ型を紹介します。

## プリミティブ

`-` は負号ではなく、符号反転の前置演算子のようです。どのユーザ定義演算子より優先順位は高いです。

`$` はプリミティブの中置演算子のようです。結合は右結合で、どのユーザ定義演算子より弱い優先順位を持ちます。
下記2式は等価です。

``` idris
f (g (h x))
f $ g $ h x
```


## プレリュード

### [`Prelude`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.html)
`Prelude` にはシンプルな関数が定義されています。

チュートリアルで使ったのは`shiftL` ですね

``` idris
shiftL: Int -> Int -> Int
```




### [`Prelude.Basics`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Basics.html)
`Prelude.Basics` には基本的な操作が定義されています。

#### `the`

``` idris
the : (a : Type) -> a -> a
```

型アノテーションを書く関数です。他言語なら型アノテーションは専用の構文になってますが、Idrisだと型も第一級なのでこういう関数が書けます。

``` idris
λΠ> the Int 1
1 : Int
λΠ> the Integer 1
1 : Integer
```

指定した型になっているのが分かると思います。

#### `(.)`

``` idris
(.) : (b -> c) -> (a -> b) -> a -> c
```

一瞬混乱しますが、  `(.)` は関数を合成する関数(中置演算子)です。数学で $f(g(x))$ を $f \circ g (x)$ と書くのに似せた記法ですね。
実際にはこう定義されています。

``` idris
infixr 9 .

||| Function composition
(.) : (b -> c) -> (a -> b) -> a -> c
(.) f g = \x => f (g x)
```

### [`Prelude.Cast`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Cast.html)

`Prelude.Cast` には型変換を管理するインターフェース、 `Cast` が定義されています。

``` idris
interface Cast from to where
    cast : (orig : from) -> to
```

いくつかの変換が用意されています。

``` idris
λΠ>  the String (cast 1)
"1" : String
λΠ>  the Integer (cast (the Int 1))
1 : Integer
λΠ> the  Double (cast 1)
1.0 : Double
```

びっくりなことに、文字列→数値の変換もあります。しかし無効な文字列を与えると0が返ってきてしまうようです。

``` idris
λΠ> the Integer (cast "123abc")
0 : Integer
```


### [`Prelude.Interfaces`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Interfaces.html)

`Prelude.Interfaces` には基本的な二項演算などのインターフェースが定義されています。

``` idris
data Ordering = LT | EQ | GT

infix 6 <, <=, >, >=
interface Eq ty => Ord ty where
    compare : ty -> ty -> Ordering

    (<) : ty -> ty -> Bool
    (<) x y with (compare x y)
        (<) x y | LT = True
        (<) x y | _  = False

    (>) : ty -> ty -> Bool
    (>) x y with (compare x y)
        (>) x y | GT = True
        (>) x y | _  = False

    (<=) : ty -> ty -> Bool
    (<=) x y = x < y || x == y

    (>=) : ty -> ty -> Bool
    (>=) x y = x > y || x == y

    max : ty -> ty -> ty
    max x y = if x > y then x else y

    min : ty -> ty -> ty
    min x y = if (x < y) then x else y
```

チュートリアルでは `Ordering` と `compare` を使いましたがその他の演算子も用意されています。

同様に数値演算も定義されています。

``` idris
infixl 8 +
infixl 9 *
interface Num ty where
    (+) : ty -> ty -> ty
    (*) : ty -> ty -> ty
    fromInteger : Integer -> ty

interface Num ty => Neg ty where
    negate : ty -> ty
    (-) : ty -> ty -> ty

infixl 9 `div`, `mod`
interface Num ty => Integral ty where
   div : ty -> ty -> ty
   mod : ty -> ty -> ty

```

`mod` や `div` は ``n `mod` m`` の形で使われることを想定して演算子の結合性や優先順位が指定されています。

### [`Prelude.Strings`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Strings.html)

`Prelude.String` には文字列操作に関する関数が定義されています。
意外にもチュートリアルで使ったのは2つだけでした。

``` idris
infixl 7 ++
unpack : String -> List Char
(++) : String -> String -> String
```

Idrisのようにデータ型が便利な言語だと文字列は人間とのやりとりくらいでしか使わないので思ったほど出番は多くないです。

### [`Prelude.Chars`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Chars.html)

`Prelude.Chars` には文字に関する関数が定義されています。文字を扱うことはあまりないと思いますが、Idrisでも操作はそんなに多くないです。

今回使ったのは文字→数値変換の `ord` ですね。

``` idris
ord : Char -> Int
```

Idrisはバックエンドが複数あるのでどう変換されるかはバックエンド依存としています。
が、REPLではユニコードのスカラ値が返っているようです。

``` idris
λΠ> ord 'κ'
954 : Int
```

### [`Prelude.List`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.List.html)

`Prelude.List` にはリストに対する操作が定義されています。
実はリストに固有の操作はチュートリアルでは使っていません。
`map` や `foldl` などはリストに限らない汎用の関数として定義されているのです。

リストはこのように定義されています。

``` idris
infixr 7 ::
data List : (elem : Type) -> Type where
  Nil : List elem
  (::) : (x : elem) -> (xs : List elem) -> List elem
```

関数型言語のユーザには馴染みの在る定義ですね。以下のようにいくつかの `(::)` と末尾の `Nil` で構成されます。

``` idris
λΠ> 1 :: 2 :: 3 :: Nil
[1, 2, 3] : List Integer
```

再帰関数を書くときも `(::)` と `Nil` で分岐します。分かりやすいですね。

``` idris
sum : List Integer -> Integer
sum Nil = 0
sum (x::xs) = x + (sum xs)
```

### [`Prelude.Maybe`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Maybe.html)

`Prelude.Maybe` には `Maybe` 型とそれに関連する関数が定義されています。

`Maybe` は以下のように定義されています。

``` idris
data Maybe : (a : Type) -> Type where
    Nothing : Maybe a
    Just : (x : a) -> Maybe a
```

`Maybe` はあるかないか分からない値を表すのに使います。
チュートリアルでは `Maybe` 本体しか触らず、他の関数は使いませんでした。
`do` 記法のところで紹介した通り、 `Maybe` にも `do` は使えますしその他 `map` なんかも使えます。

### [`Prelude.Either`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Either.html)

`Prelude.Either` には `Either` 型とそれに関連する関数が定義されています。

`Either` は以下のように定義されています。

``` idris
data Either : (a, b : Type) -> Type where
  Left : (l : a) -> Either a b
  Right : (r : b) -> Either a b
```

`Either` もデータ型本体しか使わず、周辺の関数は使いませんでした。

`Either` は2つの型をとるジェネリクスですが、関数と同じく部分適用できるので左の型だけ決めた `Either` なんかも作れます。

``` idris
λΠ> Either Int
Either Int : Type -> Type
```

逆に右の型だけ決めた(左を自由にした) `Either` は作れません。
こういう事情があるので右の型のほうが少し扱いやすくなっています。

### [`Prelude.Functor`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Functor.html)

`Prelude.Functor` には `Functor` インターフェースが定義されています。

``` idris
interface Functor (f : Type -> Type) where
    map : (func : a -> b) -> f a -> f b
```

`Functor` は 「`map` 可能な型」のイメージです。 `List` にある `map` を一般化して色々な型に使えるようにしたものです。

`f` はカインド `Type -> Type` を持ちます。`f` に `List` を当てはめると
`map : (func : a -> b) -> List a -> List b` ですし、 `Maybe` をあてはめると `map : (func : a -> b) -> Maybe a -> Maybe b` です。
左の型だけ決めた`Either Int` も `Type -> Type` でしたので `Functor` の実装がされています。

因みに、 `map` を中置演算子にした `<$>` というのもあります。

``` idris
λΠ>  negate <$> (Just 1)
Just -1 : Maybe Integer
```

### [`Prelude.Applicative`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Applicative.html)

`Prelude.Applicative` には `Applicative` インターフェース と `Alternative` インターフェースが定義されています。

``` idris
infixl 3 <*>
interface Functor f => Applicative (f : Type -> Type) where
    pure  : a -> f a
    (<*>) : f (a -> b) -> f a -> f b


infixr 2 <|>
interface Applicative f => Alternative (f : Type -> Type) where
    empty : f a
    (<|>) : f a -> f a -> f a
```


チュートリアルで使ったのは`Applicative` の `pure` です。

すこし分かりづらいですが、 `Applicative` は1つの捉え方として `Functor` を多引数関数に拡張したものとみれます。
例えば`Maybe Integer` 同士を足し算するとしましょう。
`map` (`<$>`) を多引数関数に適用すると `Maybe (Integer -> Integer)` と `Maybe` に包まれた関数がでてきてしまいます。

``` idris
λΠ>  add <$> (Just 1)
Just Integer : Maybe (Integer -> Integer)
```

そこで `Applicative` の `<*>` を使うと `Maybe` に包まれた関数と `Maybe` に包まれた値を計算できます。

``` idris
λΠ> add <$> (Just 1) <*> (Just 2)
Just 3 : Maybe Integer
```

`pure` というのは何もしないコンストラクタです。`List` なら `pure x = [x]` ですし `Maybe` なら `pure x = Just x` です。
これがあると`Maybe` に包まれた型と包まれてない型を混ぜて計算できます。

``` idris
λΠ> add <$> (Just 1) <*> (pure 2)
Just 3 : Maybe Integer
```

`Alternative` にいついては説明を省きます。以下の例でなんとなく分かるかと思います。

``` idris
λΠ>  Nothing <|> (Just 1)
Just 1 : Maybe Integer
λΠ>  (Just 1) <|> Nothing
Just 1 : Maybe Integer
λΠ> the (Maybe Integer) empty
Nothing : Maybe Integer
```


### [`Prelude.Monad`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Monad.html)

`Prelude.Monad` には `Monad` インターフェースが定義されています。`do` 記法の中身ですね。
`join` または `>>=` で実装が与えられます。相互に変換できるので本質的には両者には代わりはありません。

``` idris
infixl 1 >>=

interface Applicative m => Monad (m : Type -> Type) where
    (>>=)  : m a -> ((result : a) -> m b) -> m b
    join : m (m a) -> m a

    -- default implementations
    (>>=) x f = join (f <$> x)
    join x = x >>= id

```

`join` は `flatten` というと分かりやすいでしょうか。`Functor` では包んでる型を変えず、`Applicative` では包んでる型を増やし (`pure`) 、 `Monad` になって型を減らす(`join`)操作がでてきました。

`Monad` (`>>=`)のキモチは、「前のアクション(`m a`)から結果(`a`)を受け取って次のアクション(`m b`)を作る」操作です。
`getLine >>= putStrLn` とかですね。これを `do` 記法で書くとこうなります。

``` idris
do
  input <- getLine
  putStrLn input
```


### [`Prelude.Foldable`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Foldable.html)

チュートリアル中でも紹介したとおり、 `Foldable` は関数型的内部イテレータです。

``` idris
interface Foldable (t : Type -> Type) where
  foldr : (func : elem -> acc -> acc) -> (init : acc) -> (input : t elem) -> acc
  foldl : (func : acc -> elem -> acc) -> (init : acc) -> (input : t elem) -> acc

```

コレクションならだいたい実装できそうですよね。

`Functor` , `Applicative` , `Monad`, `Foldable` あたりでコレクションというかジェネリックなデータ型の操作は一通りできるんじゃないでしょうか。あとは[`Prelude.Traversable`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Traversable.html)にも多少操作があります。

例えば `for_` 。

``` idris
main : IO ()
main = for_ [1, 2, 3] $ \x =>
  printLn x
```

### [`Prelude.Interactive`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Interactive.html)

`Prelude.Interactive` には標準出入力に関わる関数が定義されています。

``` idris
putStr : String -> IO ()
putStrLn : String -> IO ()
```

見た目の通り文字列を表示します。`Ln` が付くと改行も表示します。

``` idris
getLine : IO String
```

標準入力から文字列を取得する IO アクションです。


### [`Prelude.File`](https://www.idris-lang.org/docs/current/base_doc/docs/Prelude.File.html)

`Prelud.File` にはァイル操作関連の関数が定義されています。

``` idris
openFile : String -> Mode -> IO (Either FileError File)
fGetChars : File -> Int -> IO (Either FileError String)
closeFile : File -> IO ()
data Mode = Read | WriteTruncate | Append | ReadWrite | ReadWriteTruncate | ReadAppend
```

特筆することはないですね。

## Base
Idrisはプレリュードがリッチすぎるのでチュートリアルの範囲ではBaseはほとんど使いませんでしたね。

### [`Data.String`](https://www.idris-lang.org/docs/current/base_doc/docs/Data.String.html)

ほんの少しだけ文字列操作関数が定義されています。
チュートリアルではこの関数を使いました。

``` idris
parsePositive : String -> Maybe Integer
```

なんか[バグってるくさい](https://github.com/idris-lang/Idris-dev/issues/4637) のですが直される気配なし

# その他
ひとまずこれでチュートリアルシリーズは締めようと思います。
気が向いたらEffective Idrisとかそういうのを書くかもしれません。
