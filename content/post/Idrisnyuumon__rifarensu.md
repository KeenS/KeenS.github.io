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

一応、変数に小文字始まり、型やバリアントには大文字始まりの識別子をつけるみたいです。

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

## `where`

TODO: 書く


## 中置演算子

Idrisではユーザが任意の中置演算子を定義できます。 `:+-*\/=.?|&><!@$%^~#` の組み合わせを中置演算子に使えます。

中置演算子を宣言するには `infixl 優先度 記号` または `infixr 優先度 記号` を使います。それぞれ左結合、右結合の演算子を定義します。
例えば `+?` を左結合の優先度4の演算子として定義するには以下のように書きます。

``` idris
infixl 4 +?
```

中置演算子を定義する時は括弧で包んで関数と同じように定義します。

``` idris
(+?): Maybe Integer -> Maybe Integer -> Maybe Integer
(+?) (Just x) (Just y) = Just (x + y)
(+?)  _        _       = Nothing
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

任意の2引数関数を `` ` `` で囲むと中置で書けます。バリアントも関数でしたからこのようにも書けます。

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
例えば `Maybe` は `Monad` を実装しているのでこう使えます。

``` idris
addMaybe : Maybe Integer -> Maybe Integer -> Maybe Integer
addMaybe x y = do
  x' <- x
  y' <- y
  Just $ x' + y'
```



## コメントとドキュメントコメント

`--` ではじまるのがコメント、 `|||` ではじまるのがドキュメントコメント。

[ドキュメントコメント](http://docs.idris-lang.org/en/latest/reference/documenting.html)は色々書式があるみたいです。

## 名前空間

Idrisのコードは以下のような形になっています。

``` idris
<モジュールヘッダ>?

<インポート文>

<コード>

```


### モジュールヘッダ

`module モジュール名` です。あってもなくてもいいです。1ファイル1モジュールです。
ドットで区切った階層構造です。一応ファイル名とは関連がないことになってますが、`Foo/Bar.idr` には `module Foo.Bar` モジュールを定義するのが通例です。たまに `Foo.Bar` と書いて `Foo/Bar.idr` のことを指す記法もあるので注意が必要です。

### インポート文

`import モジュール名` です。人によっては混乱すると思いますが、 `import Data.Vect` で `Vect` モジュール内の全てのアイテムが今の名前空間にインポートされます。
単純にモジュール名を略記したいなら `import Data.Vect as V` などで別名をつけることになります。

# 値
## 関数

Idrisの関数

# 型

関数の引数の数と型
タプルとUnit
Maybe
Either
IntとInteger
  lift
Ordering
String
Char
  ord
List
 unpack
 foldl
 map
# ライブラリ
プレリュード
the
$ .
putStr
putStrLn
getLine
parsePositive

# モジュール
# その他
