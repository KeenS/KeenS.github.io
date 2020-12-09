---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-09T03:34:57+09:00
title: "Idrisのインタフェースとモナドなどなど"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の9日目の記事です。

κeenです。Idrisのインタフェースやモナドについて紹介します。

<!--more-->

# インタフェース

色々な言語にあるやつとだいたい一緒です。
厳密にいうとアドホックポリモーフィズムのための機構なのでHaskellの型クラスやRustのトレイトに例えた方がいいのですが、細かい話は置いておきましょう。

`interface インタフェース名 型変数 where 本体` の構文で書きます。
本体の部分には値の型や実装などを書きます。値とは関数も含みます。
例えば任意の型の値を文字列にするインタフェース `Show` の定義は以下のように書けます。

例：インタフェース `Show` の定義

```idris
interface Show a where
    show : a -> String
```

`a` が `Show` を実装する型を表します。
そして `show` には実装がありません。型だけ示しているのでインタフェースっぽいですね。
因みに、 `Show` はプレリュードに定義されているので自分で書かなくても使えます。

これを実装するには `インタフェース名 型名 where 本体` の構文を使います。

例： `Name` 型にインタフェース `Show` を実装するコード

```idris
record Name where
  constructor MkName
  firstName, lastName: String

Show Name where
  show (MkName firstName lastName) = firstName ++ " " ++ lastName
```

実装の方には逆に型の宣言がありません。

実装した `Show` は普通の関数のように呼び出すだけで使えます。

```text
Idris> show (MkName "Edwin" "Brady")
"Edwin Brady" : String
```

## デフォルト実装

先ほどインタフェースの本体には関数の型や *実装* を書くと説明しました。
インタフェースに実装を持つこともできるんですね。

例えば等価比較を行なうためのインタフェース `Eq` はデフォルト実装を持ちます。

例：インタフェース `Eq` の定義

```idris
interface Eq a where
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool

    x /= y = not (x == y)
    x == y = not (x /= y)
```

等しい（ `==` ）は等しくない（ `/=` ）の逆だしその逆もまた然りという定義ですね。
どちらか一方だけ実装すればもう一方は自動でついてくる仕組みです。


## 関連型

Idrisでは特に特別なものではないんですが、HaskellやRustで関連型（associated type）と呼ばれているものも書けます。

例えばとある別の型からその型の値を取得できる `Extract` というインタフェースを考えてみましょう。それは以下のように定義できます。

例：インタフェース `Extract` の定義

``` idris
interface Extract a where
  From: Type
  extract: From -> a
```

この `From` が関連型です。
Idrisでは型も値なので関数や値のように普通にメンバーに書けばそれで済みます。

上記の型を `Name` に実装してみましょう。
まず準備として `Name` を保持する型 `Person` を定義しておきます。

例：`Person` 型の定義

``` idris
record Person where
  constructor MkPerson
  age: Int
  name: Name
```

すると `Person` から `Name` を `extract` できるので、以下のように `Extract` を `name` に実装できます。

例：`Extract` を `Name` に実装するコード

``` idris
Extract Name where
  From = Person
  extract = name
```

## 多パラメータのインタフェース

インタフェースの型パラメータは複数書くことができます。
例えばある型から別の型に変換するインタフェース `Cast` は以下のように定義されています。

例：プレリュードでのインタフェース `Cast` の定義

``` idris
interface Cast from to where
    cast : (orig : from) -> to
```

`from` と `to` の2つのパラメータがありますね。
実装するときも2つのパラメータを指定します。

例： `Cast` を `Double` と `Int` に定義するときの書き出し

``` idris
Cast Double Int where
  -- ...
```

`Cast` のパラメータに `Double` と `Int` を指定しています。
`cast` の実装はプリミティブの呼び出しになるので省略しました。

# インタフェースを実装できる条件

インタフェースの実装は1つの型につき1つしか持てません [^1]。また関数はインタフェースを実装できません。

[^1]: 名前付き実装という機能を使えばその限りではないのですが、話がややこしくなるので一旦置いておきます。



例えば `Name` に対してもう1つの `Show` のインタンスを追加しようとするとコンパイルエラーになります。

例：2つ目の `Show` インタフェースを `Name` に実装しようとした際に出るエラー

``` text
- + Errors (1)
 `-- (no file) line 0 col -1:
     interface.idr:21:1-9:Main.Name implementation of Prelude.Show.Show already defined
```


逆に、意外と実装できるケースにプリミティブを含む既存の型にインタフェースを実装できるというものが挙げられます。

以下にその例を示します。

例：インタフェース `Zero` を定義し、それをプリミティブ `Int` に実装するコード

``` idris
interface Zero a where
  zero : a

Zero Int where
  zero = 0
```

プリミティブである `Int` に対してインタフェースを実装できました。

# ジェネリクスとインタフェース

ジェネリクスで扱う型に特定のインタフェースを実装していることを要求したい場合があります。

## ジェネリクス関数へのインタフェース制約

例えば引数を2つ受け取って、その小さい方、大きい方の順で並べて返す関数を定義したいとします。そのときに `<` で比較する必要がありますよね。
今までの知識で関数を定義すると `<` が実装されていないのでコンパイルエラーになります

例： `a` 同士を比較できないためエラーになるコード例

``` idris
ordered: a -> a -> (a, a)
ordered a b =
  if a < b
  then (a, b)
  else (b, a)

```

``` text
- + Errors (1)
 `-- interface.idr line 35 col 2:
     When checking right hand side of ordered with expected type
             (a, a)
     
     When checking argument b to function Prelude.Bool.ifThenElse:
             Can't find implementation for Ord a
```


そういうときは特定のインタフェースを実装している型のみ受け付ける制約を書きます。`インタフェース名 変数名 => 型` の構文です。`<` 演算子は `Ord` インタフェースで定義されているため、上記の `ordered` を修正すると以下のようになります。

``` idris
ordered: Ord a => a -> a -> (a, a)
ordered a b =
  if a < b
  then (a, b)
  else (b, a)
```

また、複数の制約を書きたい場合は `(インタフェース名 変数名, インタフェース名 変数名, ....) => 型` と丸括弧で括ってカンマで区切って書きます。

例：ジェネリクスの型変数に複数のインタフェース制約を書いたコード

``` idris
orderedMsg: (Ord a, Show a) => a -> a -> String
orderedMsg a b =
  let (a, b) = ordered a b in
  (show a) ++ " < " ++ (show b)

```

## インタフェース実装へのインタフェース制約

インタフェース自身にも関数を含みますからインタフェースの実装にインタフェース制約を加えたいというのも自然な要求です。実際、そういう機能があります。
`インタフェース制約 => インタフェース名 型名 where 本体` の構文です。
以下に例を示します。

例：プレリュードのタプルへの `Eq` の実装例

``` idris
(Eq a, Eq b) => Eq (a, b) where
  (==) (a, c) (b, d) = (a == b) && (c == d)
```

余談ですが、Idrisの3つ組以上のタプルは2つ組の組み合わせの糖衣構文となっています。例えば `(A, B, C)` は `(A, (B, C))` です。
なので上記の2つ組のタプルの実装で全てのタプルの実装をカバーできるのです。

## インタフェースの拡張

インタフェース制約をインタフェースの宣言に書くこともできます。
これは事実上既存のインタフェースを拡張した新しいインタフェースを定義していると捉えることもできますね。
`interface インタフェース制約 => インタフェース名 型変数 where 本体` の構文になります。

例えばプレリュードの `Neg` は `Num` を拡張したインタフェースです。

例：プレリュードの `Num` と `Neg` のコード


``` idris
||| The Num interface defines basic numerical arithmetic.
interface Num ty where
    (+) : ty -> ty -> ty
    (*) : ty -> ty -> ty
    ||| Conversion from Integer.
    fromInteger : Integer -> ty

||| The `Neg` interface defines operations on numbers which can be negative.
interface Num ty => Neg ty where
    ||| The underlying of unary minus. `-5` desugars to `negate (fromInteger 5)`.
    negate : ty -> ty
    (-) : ty -> ty -> ty

```


# 高カインド多相とファンクタ

少し難しめの機能に入ります。
特に複雑という訳ではないんですが、抽象度が高いので慣れてないと理解に時間のかかる機能です。

`List a` や `Maybe a` のようにジェネリクスなデータ型がありますね？
これに対してインタフェースを定義したいとします。
例えば `map` なんかは分かりやすいでしょう。

例： `List a` と `Maybe a` に対する素朴な `map` の実装


``` idris
-- List
map : (a -> b) -> List a -> List b
map f []      = []
map f (x::xs) = f x :: map f xs

-- Maybe
map : (a -> b) -> Maybe a -> Maybe b
map f (Just x) = Just (f x)
map f Nothing  = Nothing
```

これを抽象化するインタフェースを定義します。
するとパラメータになるのは `List` や `Maybe` の部分です。
これらは型コンストラクタ、Idris的にいうと `Type -> Type` の値です。

``` idris
Idris> :t List
List : Type -> Type
Idris> :t Maybe
Maybe : Type -> Type
```

従来の型変数（Idris的にいうと `Type` の値）とは異なるのでインタフェースの定義に少し手を加えます。具体的には型変数が `Type` ではなく `Type -> Type` であることを表わすために型注釈を加えます。構文は `interface インタフェース名 (変数名: Type -> Type) where 本体` です。

例： プレリュードのインタフェース `Functor` の定義


``` idris
interface Functor (f : Type -> Type) where
    map : (func : a -> b) -> f a -> f b

```

`Functor` （関手）は `List` や `Maybe` などのように「`map` できる型」を抽象化する型です。`f` の部分に `List` や `Maybe` などが当て嵌ります。

`Functor` インタフェースのおかげでこのように `List` や `Maybe` の値に対して1を足す関数を適用できます。

``` text
Idris> map (1+) (Just 1)
Just 2 : Maybe Integer
Idris> map (1+) [1, 2, 3]
[2, 3, 4] : List Integer
```

このように `Type` ではなく `Type -> Type` などの複雑な「型の型」を持つものに対するジェネリクスを高カインド多相と呼びます（「型の型」はカインド（kind）と呼ばれています）。

因みに `map` の代わりに `<$>` という演算子も使えます。

``` idris
infixr 4 <$>
(<$>) : Functor f => (func : a -> b) -> f a -> f b
func <$> x = map func x
```


``` text
Idris> (1+) <$> (Just 1)
Just 2 : Maybe Integer
Idris> (1+) <$> [1, 2, 3]
[2, 3, 4] : List Integer
```

後述するApplicativeと組み合わせるときに便利です。

余談ですがHaskellだとリスト専用の `map` と `Functor` で定義される `fmap` で分かれています。恐らくですが先にリストの `map` を作ったあとに `Functor` という抽象化に気付いたので要らぬ複雑性が入ってるんじゃないかと思います。その点IdrisはHaskellの後発なのもあってシンプルですね。

# 多引数関数とApplicative

`map` は便利ですが、痒いところに手が届かないことがあります。
多引数関数には使いづらいのです。

例えば2引数関数 `(+)` を `Just 1` と `Just 2` に適用したいとしましょう。
そこで素朴に `map` で適用しようとするとエラーになります。

例： `map` を使って素朴に `(+)` を `Just 1` と `Just 2` に適用した式

``` text
Idris> map (+) (Just 1) (Just 2)
(input):1:1-25:When checking an application of function Prelude.Functor.map:
        Type mismatch between
                Maybe a1 (Type of Just x)
        and
                (\uv => _ -> uv) a (Expected type)

        Specifically:
                Type mismatch between
                        Maybe
                and
                        \uv => _ -> uv
```

これは落ち着いて型を考えるとエラーになる理由が分かります。
`map` の型は `(a -> b) -> Maybe a -> Maybe b` です。 `(+)` の型は `Integer -> Integer -> Integer` で、これは `Integer -> (Integer -> Integer)` です。
これらを組み合わせると、 `map (+)` は `Maybe Integer -> Maybe (Integer -> Integer)` になります。これを `Just 1` に適用すると `Maybe (Integer -> Integer)` になります。
ここで関数ではなくて `Maybe` 型の値が出てきてしまいました。これでは `Just 2` に適用できません。

しかしながらみなさんは無理矢理適用させる実装を思い付くんじゃないでしょうか。
以下のようにパターンマッチで取り出してしまえばいいのです。

例： `Maybe` に包まれた関数を無理矢理適用してしまうコード

``` idris
ap: Maybe (a -> b) -> Maybe a -> Maybe b
ap (Just f) (Just x) = Just (f x)
ap _        _        = Nothing
```

実現できそうなのでインタフェースで抽象化しましょう。
`Functor` を拡張したインタフェースにするのが具合がよさそうです。
これはプレリュードで `Applicative` と呼ばれています。

例：プレリュードの `Applicative` の定義

``` idris
infixl 3 <*>
interface Functor f => Applicative (f : Type -> Type) where
    pure  : a -> f a
    (<*>) : f (a -> b) -> f a -> f b
```

`ap` ではなく `<*>` という演算子になっていますが、やってることは先程定義した `ap` と同じものです。
これに対する `Maybe` の実装は以下のようになっています。

例：プレリュードの `Applicative` の `Maybe` への実装

``` idris
Applicative Maybe where
    pure = Just

    (Just f) <*> (Just a) = Just (f a)
    _        <*> _        = Nothing
```

ちゃんと `<*>` の実装が `ap` と同じものになっていますね。

`map` を `<$>` と書けることと組み合わせて、以下のように使えます。

例： `Functor` と `Applicative` の利用

``` idris
Idris> (+) <$> (Just 1) <*> (Just 2)
Just 3 : Maybe Integer
```

因みにインタフェースであるからには複数の型（型コンストラクタ）に実装されている訳です。例えば `List` での実装がどうなっているかというと、全ての要素に対して繰り返すようになっています。

例： `List` での `Functor` と `Applicative` の利用

``` idris
Idris> (+) <$> [1, 2, 3] <*> [10, 11, 12]
[11, 12, 13, 12, 13, 14, 13, 14, 15] : List Integer
```

ところで `Applicative` に `pure` というのがいますね。
これの役割に触れておきましょう。
`func` を `x` に適用するとします。
`func` と `x` の型がそれぞれ `Maybe` （一般化して `f`）に包まれている/いないで4つの組み合わせがありますね？それぞれどう適用するか見てみましょう。

| 関数         |  引数 |     適用     |
|--------------|-------|--------------|
| `a -> b`     | `a`   | `func x`     |
| `a -> b`     | `f a` | `map func x` |
| `f (a -> b)` | `a`   | ????         |
| `f (a -> b)` | `f a` | `func <*> x` |

`f (a -> b)` を `a` に適用する場合だけまだ出てきてませんね。
この隙間を `pure` が埋めてくれます。
`func <*> (pure x)` と書けばいいのです。

余談ですが`Functor` を使った `map func x` も `(pure func) <*> x` と `Applicative` の機能だけで書くことができますね。そういった意味で `Applicative` は `Functor` の拡張になっています。

# プログラムとモナド

`Applicative` で `Maybe` などのジェネリクス型に包まれているデータや関数に対して操作できるようになりました。では新しく包む操作についてはどうでしょう。

例えば割る数が0以外では割った商を、0では `Nothing` を返す `safeDiv` を考えます。

例：`safeDiv` 関数

``` idris
safeDiv : Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv d m = d `div` m
```

これを `<$>` と `<*>` で組み合わせることもできますが、結果はあまり嬉しくありません。

例：`safeDiv` を `Functor` と `Applicative` と一緒に使った例

```text
Idris> safeDiv <$> (Just 1) <*> (Just 0)
Just Nothing : Maybe (Maybe Integer)
```

返り型が `Maybe (Maybe Integer)` と `Maybe` が二重に出てきてしまいました。そして返り値も `Just Nothing` になってしまっています。これは数値が返ってきていないという意味では `Nothing` と変わりません。`Maybe Integer` に「潰せ」たらうれしいですよね。

そういう操作は一般に、flatten、あるいはjoinと呼ばれますね。
さらにflattenの派生型であると嬉しいのがflatMapです。Idrisの型で書くと `Maybe a -> (a -> Maybe b) -> Maybe b` です。flattenとflatMapは片方があればもう片方を定義できるので双子のような存在です。

そんなjoinとflatMapをインタフェースにしたのがモナドです。

例：プレリュードでの `Monad` の定義

```Idris
infixl 1 >>=

interface Applicative m => Monad (m : Type -> Type) where
    ||| Also called `bind`.
    (>>=)  : m a -> ((result : a) -> m b) -> m b

    ||| Also called `flatten` or mu
    join : m (m a) -> m a

    -- default implementations
    (>>=) x f = join (f <$> x)
    join x = x >>= id
```

この `Monad` を使えば先程の `safeDiv` は `Maybe Integer` を返すように使えるようになります。

```text
Idris> join (safeDiv <$> (Just 1) <*> (Just 0))
Nothing : Maybe Integer
Idris> Just 1 >>= \d => (Just 0 >>= \m => safeDiv d m)
Nothing : Maybe Integer
```

## `do` 記法

先程の例、 `Just 1 >>= \d => (Just 0 >>= \m => safeDiv d m)` は見づらいですよね。演算子や無名関数が乱舞してどこに何が書いてあるのか分かりません。そこでこれを書きやすくする `do` 記法というのがあります。

先程のコードを `do` 記法で書き直すと以下のようになります。

例

``` idris
do
  d <- Just 1
  m <- Just 0
  safeDiv d m
```

これだとぐっと見やすくなりますね。

ところで基本文法のところで触れ忘れたんですが、オフサイドルールには別の記法もあります。`{記述1; 記述2; ...}` と `{}` で包んでそれぞれの記述を `;` で分けます。こうすることで1行でも書けるようになります。
REPLなどでは1行で書きたいケースもあると思うのでお試し下さい。

例： `do` 記法をREPLで使うコード

``` text
Idris> do {d <- Just 1; m <- Just 0; safeDiv d m }
Nothing : Maybe Integer
```

モナドを使うときは大抵 `do` 記法を使うことになるでしょう。

`Monad` もまたインタフェースなので `Maybe` 以外の型も実装を持ちます。例えば `List` はその要素について繰り返します。
リスト内包表記のようなことを `do` 記法でもできるのです。

例：九九の左斜め下半分を `do` 記法で計算するコード

``` idris
do
  x <- [0..9]
  y <- [0..x]
  pure (x * y)
```

# モナドはDSL?

`Functor` 、 `Applicative` 、 `Monad` で何かに包まれた値を計算できるようになりました。では、包まれた値から取り出すにはどうしたらいいでしょう。
残念ながらいい方法はありません。
`Nothing` なんかは値がないから `Nothing` な訳で、そこから値を取り出せません。

逆に言うとモナドにすることで操作を「閉じ込めて」しまうことができます。
使える操作は `Functor` と `Applicative` で「持ち上げた」操作と、 `Monad` で「結合」できる `a -> m b` の型の関数のみです。

そういった意味でモナドはDSLと捉えることができます。
ライブラリなんかでもモナドを提供し、主な操作は `do` 記法でやるものが多くあります。

# IOモナド

いままで、まともにHollo Worldを解説してませんでしたね。
それはIO操作もモナドで書かれているからです。

ということでモナドを知った今、改めてHello Worldをしてみましょう。
`putStrLn` は以下のような型をしています。

``` text
Idris> :t putStrLn
putStrLn : String -> IO ()
```

そしてIdrisは `main : IO ()` な値からプログラムの実行を始めます。
なのでHello Worldは以下のように書きます。

例：IdrisでのHello World

``` idris
main : IO ()
main = putStrLn "Hello, World"
```

これを `Hello.idr` として保存し、以下のように実行します。

例：Hello Worldをコンパイル・実行するコマンド

``` text
$ idris -o Hello Hello.idr
$ ./Hello
Hello, World
```

`-o` オプションをつけて `idris` コマンドを起動するとREPLではなくコンパイラが起動し、 `-o` で指定したファイルへとコンパイル結果を出力します。

もうちょっと複雑なことをしましょう。
`getLine: IO String` で標準入力から1行取得できます。
これと `putStrLn` で入力をエコーバックするプログラムはこう書けます。

例： `getLine` と `putStrLn` を使ってユーザの入力を表示するプログラム

``` idris
main : IO ()
main = getLine >>= \s => putStrLn ("Your input is " ++ s)
```

あるいは、 `do` 記法で書くこともできます。

例： `getLine` と `putStrLn` を使ってユーザの入力を表示するプログラムを `do` 記法で書いたもの

``` idris
main : IO ()
main = do
  s <- getLine
  putStrLn ("Your input is " ++ s)
```

これを `Echo.idr` に保存し、コンパイル、実行すると以下のように動作します。

``` text
$ idris -o Echo Echo.idr
$ ./Echo
echooooo
Your input is echooooo
```

## ところでIOって何？

`IO` の型にちょっと違和感を覚えた方もいるんじゃないかと思います。
`main` の型は `IO ()` という値です。関数じゃありません。
同じく `getLine` も `IO String` という値です。
これだと書いたそばから実行されてしまわないでしょうか。
まあ、動いてるからにはそうならないんのは分かるんですが、どういう仕組みなんでしょう。

Idrisのプログラムからは `IO` の値を実行することができません。
`getLine` と書いたからといって即座に標準入力から文字列を取り出したりしないのです。
唯一 `main` に書いた `IO` の値のみが処理系側で実行されます。
処理系側で実行されてはじめて標準入力から文字列を取り出すというアクションが行なわれます。
`IO` は実行される前のプログラムのようなものなのです。

`IO` を実行できるのは `main`の1箇所のみとなると、複数のIO処理をしたいときは `IO` の値を合成する必要があります。
その仕組みに選ばれたのがモナドという訳です。
`>>=` は別名 bind （結合）ですが、先程の `getLine` と `putStrLn` のように複数のIO処理を結合するのに使われているのです。

## 純粋関数型言語とIO

さて、 `main` でしか `IO` を実行できないとなると他の関数内でIO処理をしたい場合はどうすればいいのでしょう。

1つの答えは「そういう関数は設計が悪いから書くな」です。
純粋関数型言語であるIdrisの基本方針として、IOや変数への破壊的代入などの計算以外の処理はよくないものとされています。
関数を呼んだときに何が起こるか分からなくなるからです。
なので関数内でIO処理を書きたくなったときはまずは「計算部分とIO部分に分離できないか」と考えてみましょう。

もう1つの答えは 「全て `IO` モナドの中で書く」です。
`IO` モナドをリレーのように `main` まで伝えればIOを実行できます。
なので関数の中でIOをしたければ `IO` モナドの中で書くことにすれば実現できます。
とはいえやっぱりIOの中でプログラムを書くのは面倒なので基本的には純粋な計算部分とIO部分に分けて、
IO部分でだけ `IO` モナドを使うようになります。

じゃあデバッグプリントを関数の中に仕込みたかったらどうなるの、という疑問はあるかもしれません。
まあ、普通に `IO` を使ってそれを呼ぶ関数を全て `IO` モナドの中で書くように変更します。
ちょっと面倒ですよね。一応そういった用途のためのバックドアの機構はあるのでそのうち紹介します。

# まとめ

Idrisの重要な機能インタフェースと、重要なインタフェース `Monad` 、重要なモナド `IO` を立て続けに紹介しました。
`IO` まで辿りついたことでIdrisのプログラムを書けるようになりました。

未紹介のIdrisの機能もあるのでAdvent Calendarの残りでは手を動かしつつ他の機能も紹介していけたらなと思います。
