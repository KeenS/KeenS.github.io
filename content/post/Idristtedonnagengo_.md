---
categories: [Idris, Idris Advent Calendar, Advent Calendar, Advent Calendar 2020]
date: 2020-11-30T23:46:01+09:00
title: "依存型のあるHaskellことIdrisってどんな言語？"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の1日目の記事です。

κeenです。
Idrisのことを知らない人も多いと思うのでIdrisの紹介からはじめていきたいと思います。

<!--more-->
# コード例

ひとまずコードを見てみましょう。


## Hello World

```idris
main : IO ()
main = putStrLn "Hello World"
```

``` text
$ idris hello_world.idr -o hello_world
$ ./hello_world
Hello World
```


## FizzBuzz

``` idris
import Data.String

data FizzBuzz = F | B | FB | I Integer

Show FizzBuzz where
  show F     = "fizz"
  show B     = "buzz"
  show FB = "fizzbuzz"
  show (I n)  = show n

fizzBuzz : Integer -> FizzBuzz
fizzBuzz n = case (n `mod` 3, n `mod` 5) of
               (0, 0) => FB
               (_, 5) => B
               (0, _) => F
               _      => I n
fizzBuzzSeq : Integer -> List FizzBuzz
fizzBuzzSeq n = map fizzBuzz [1..n]

main : IO ()
main = do
  [_, arg] <- getArgs
    | _ => putStrLn "prease specify N"
  let Just n = parseInteger arg
    | Nothing => putStrLn "arg must be an integer"
  for_(fizzBuzzSeq n) (putStrLn . show)
```


``` text
$ idris fizz_buzz.idr -o fizz_buzz
$ ./fizz_buzz 15
1
2
fizz
4
5
fizz
7
8
fizz
10
11
fizz
13
14
fizzbuzz
```

# Idrisとは
[Idris](https://www.idris-lang.org/index.html)とは *型駆動開発* のために設計されたプログラミング言語です。
静的型付きの関数型言語で、コンパイル方式の処理系を持ちます。

大きな特徴としては型駆動開発のために強力な型、特にプログラミング言語としては珍しい依存型を持つこと、文法がHaskellに似ていることが挙げられます。

色々キーワードが出てきましたがAdvent Calendarの続きで紹介するとして、ここでは依存型とは何かを紹介します。

# 依存型とは

依存型とは項でインデックス付けされた型です（TaPLより）。

ざっくりと言うと型を書く場所に値を書けます。

例えばこういう関数の実装の型を考えてみましょう。

``` idris
foo True  = "True"
foo False = 0
```

引数が `True` のときに `String` 型の値を返して、 `False` のときに `Integer` 型の値を返しています。
これは大抵の言語では型づけできません[^ts]。

[^ts]: TypeScriptのように型付けできる変態もいますが…

しかしIdrisなら簡単に型付けできます。まさしく「引数が `True` のときに `String` 型、 `False` のときに `Integer` 型」と記述するだけです。

``` idris
foo: (b: Bool) -> if b then String else Integer
foo True  = "True"
foo False = 0
```

どうですか？面白くないですか？

もうちょっとユースケースが分かりやすい例に[ベクタ型](https://www.idris-lang.org/docs/current/base_doc/docs/Data.Vect.html)などもあります。
リスト型のようですが、 `Vect n a` と型引数にその長さ `n` を保持できます。

例えばそのベクタを結合する関数 `++` は以下のような型をしています。

``` idris
(++) : (xs : Vect m elem) -> (ys : Vect n elem) -> Vect (m + n) elem
```

長さ `m` のベクタと長さ `n` のベクタを結合すると長さ `m + n` のベクタになるというシンプルですが強力な表明が書けます。

あるいは要素 `n` 個を取り出す関数 `take` は以下のような型をしています。

``` idris
take : (n : Nat) -> Vect (n + m) elem -> Vect n elem
```

`n` 個取り出すからには `n` 個以上のベクタを渡さないといけないんですね。

このように詳細な制約を書けるのが特徴です。
もう一歩進めるとこの制約を使って数学的な証明を書いたりもできます。
いつかそういった面も紹介していきたいですね。

# まとめ

Idrisについてかるーく紹介しました。続く記事でもうちょっと色々紹介できたらなと思います。
