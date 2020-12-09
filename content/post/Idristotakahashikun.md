---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-10T02:18:48+09:00
title: "Idrisと高橋君"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の10日目の記事です。

κeenです。
今回はIdrisで簡単な競技プログラミングの問題を解いてみたいと思います。

<!--more-->


参考にしたのは以下の記事です。

[AtCoder に登録したら次にやること ～ これだけ解けば十分闘える！過去問精選 10 問 ～ - Qiita](https://qiita.com/drken/items/fd4e5e3630d0f5859067)

競技プログラミングというのは与えられた問題を解くプログラムをできるだけ速く書くコンテストです。
入力と出力にあまり凝ったものがなく、プログラミングの技法よりもどちらかというと解く方針を考える方に重点が置かれるのが特徴です。
であれば慣れてないプログラミング言語の練習の題材に丁度いいのでここで採り上げることとします。

AtCoderというのは国内最大手の競技プログラミングのサイトです。

今回は練習問題として採り上げるだけで特に競技には参加しません（そもそもAtCoderはIdrisでの提出を受け付けていないようです）。
なので競技プログラミング風の速く解く書き方ではなくできる限りIdris風になるような書き方をします。

# 第1問
## 問題

[A - Product](https://atcoder.jp/contests/abc086/tasks/abc086_a)


> $a$ 、 $b$ の2つの入力が与えられるのでその積が偶数か奇数かを判定して下さい
> ## 制約
> * $1 ≤ a,b ≤ 10000$
> * $a,b$ は整数
>
> ## 入力
>
> 入力は以下の形式で標準入力から与えられる。
>
>  a b
>
> ## 出力
>
> 積が奇数なら `Odd` と、 偶数なら `Even` と出力する


例えば入力が以下の場合

```
3 4
```

$3 \times 4 = 12$ は偶数なので出力は以下になります。

```
Even
```

## コード

まず、問題を解く `solve` という関数を定義しましょう。
そして出入力を担当するコードを書きます。

結果は `Even` または `Odd` なのでそのデータ型を定義しましょう。

```idris
data Result = Even | Odd
```

これを表示するときは `Even` または `Odd` とするのでした。
`Show` を実装しましょう。

```idris
Show Result where
  show Even = "Even"
  show Odd  = "Odd"
```

さて、 `solve` です。 `a` 、 `b` を受け取って `Even` または `Odd` を返します。

こう書けるでしょう。

``` idris
solve : Integer -> Integer -> Result
solve a b =
  if a * b `mod` 2 == 0
  then Even
  else Odd
```

これを一旦REPLにロードして動作を確認してみましょう。

``` text
λΠ> solve 3 4
Even : Result
λΠ> solve 1 21
Odd : Result
λΠ> solve 1000 101
Even : Result
```


よさそうです。それでは出入力を担当するコードを書いてみます。
いくつか新要素が出てきます。

``` idris
main : IO ()
main = do
  line <- getLine
  let [a, b] = words line
  printLn (solve (cast a) (cast b))
```

まず `main : IO ()` から `line <- getLine` まではいいでしょう。今まで出てきました。

`let [a, b] = words line` の行は新要素が3つあります。
1つずつやっていきましょう。

### `words` 関数
`words` は文字列を空白で分解する関数です。

``` text
Idris> :doc words
Prelude.Strings.words : String -> List String
    Splits a string into a list of whitespace separated strings.
    
        > words " A B C  D E   "
        ["A", "B", "C", "D", "E"]
    
    The function is: Total & public export
Idris> words "1 2 3"
["1", "2", "3"] : List String
```

この関数は `String -> List String` で `:search` すると2番目に出てきます。


``` text
Idris> :search String -> List String
= Prelude.Strings.lines : String -> List String
Splits a string into a list of newline separated strings.

= Prelude.Strings.words : String -> List String
Splits a string into a list of whitespace separated strings.
...
```


### `do` 記法内の `let` 行

普通の `let` と同様に変数を束縛します。ただし末尾の `in` が不要です。

``` idris
do
  l1 <- getLine
  l2 <- getLine
  let twoLines = l1 ++ l2
  ...
```

`do` 記法内ではオフサイドルールが適用されているのでそれに合わせた構文になっているんですね。

### `let` でのパターンマッチ

`let [a, b] = ...` の部分です。
パターンマッチできる構文として既に `case` と関数定義の引数を紹介しました。
それらに `let` が加わります。

しかしこの `let` 、パターンが網羅的ではありません。
`words line` の結果が `[]` や `["1", "2", "3"]` の場合にマッチしません。
マッチしない入力がきた場合はどうなるのでしょうか？答えはIdrisのプログラムが終了します。
ちょっと激しいですが、競技プログラミングでは変な入力がこない想定でプログラムを組めるのでまあ、許容範囲でしょう。
とはいえ安全にプログラムを組めるならそれに越したことはないので次の問題でもう少し安全にプログラムを組める方法も紹介します。

ということで `let [a, b] = words line` の行で入力の行を空白で分割し、それが2要素であった場合にのみ取り出していることになります。

最終行は以下のようになっています。

``` idris
 printLn (solve (cast a) (cast b))
```

文字列から数値に変換するには `Cast` インタフェースの `cast` 関数が使えます。

``` text
Idris> the Integer (cast "1")
1 : Integer
Idris> the Integer (cast "-1")
-1 : Integer
Idris> the Integer (cast "hoge")
0 : Integer
```

これも `:search String -> Integer` すると出てきます。

`printLn: Show ty => ty -> IO ()` は `Show` を実装した値を出力できます。


全体の解説が終わったのでプログラムをコンパイル・実行してみましょう。

``` text
$ idris -o Product Product.idr
$ ./Product
1 2
Even
$ ./Product
3 4
Even
$ ./Product
1 21
Odd
```

正しく動作しているようです。

正しくない入力を与えたらどうなるか見てみましょう。

``` text
$ ./Product
1
*** Product.idr:17:16-25:unmatched case in Main.case block in main at Product.idr:17:16-25 ***
$ ./Product
1 2 3
*** Product.idr:17:16-25:unmatched case in Main.case block in main at Product.idr:17:16-25 ***
```

見事にクラッシュしていますね。
次の問題ではもう少しまともにします。

# 第2問
## 問題

[A - Placing Marbles](https://atcoder.jp/contests/abc081/tasks/abc081_a)

> 1、2、3の番号のついたマス目があります。各マスには `0` か `1` が書かれています。
> マス $i$ には $s_i$ が書かれています。
> `1` が書かれたマスにビー玉を起きます。ビー玉が置かれるマスがいくつあるか求めて下さい。
>
> ## 制約
> $s1,s2,s3$ は `1` あるいは `0`
>
> ## 入力
> 入力は以下の形式で標準入力から与えられる。
>
> s1s2s3
>
> ## 出力
> 答えを出力する

例えば入力が以下の場合

``` text
101
```

1が2つあるので出力は2です。

``` text
2
```

## コード

まずは `solve` を書きましょう。
$s_1$ 、 $s_2$ 、 $s_3$ と3つの数値を引数で取ってもいいんですが、せっかく $s_i$ と一般化してくれているのでリストで引数をとることにしましょう。
また、各マス目に書かれているものが何なのか難しいですがここでは文字 `Char` ということにしておきます。

すると `solve` は文字のリストを受け取って、 `1` が含まれている数をカウントする関数ということになります。

``` idris
solve: List Char -> Integer
solve ss = cast (length (filter (== '1') ss))
```

`filter : (a -> Bool) -> List a -> List a` は引数の関数の条件にマッチする要素だけ集めてくる関数です。

`(== '1')` 、あるいは度々出てきている `(演算子 引数)` や `(引数 演算子)` はセクションという機能です。
演算子に引数を部分適用します。`(+1)` は何度か出てきましたね。

``` text
Idris> :t (+ 1)
\ARG => ARG + 1 : Integer -> Integer
Idris> :t (== '1')
\ARG => ARG == '1' : Char -> Bool
```

ということで `(== '1')` は 「引数が `'1'` なら `True` を返しそれ以外なら `False` を返す関数」ということになります。
紹介してませんでしたがIdrisの文字リテラルは `'文字'` です。

`length` のあとに `cast` がついているのは `length` の返り値が `Nat` だからです。

``` text
Idris> :t length
Prelude.List.length : List a -> Nat
```

ところで、今回の `solve` は括弧が多いですね。
括弧を減らせる便利演算子 `$` を紹介しておきます。
`func $ arg` は `func arg` と同じ意味なのですが、 `$` の優先順位が低いので事実上 `$` から式の末尾までの括弧のように振舞います。
例えば先程の `solve` は以下のように書き換えられます。

``` idris
solve: List Char -> Integer
solve ss = cast $ length $ filter (== '1') ss
```


出入力の部分は以下です。

``` idris
main : IO ()
main = do
  line <- getLine
  let ss = unpack line
  printLn $ solve ss
```

今回はパターンマッチは出てきませんでしたね。

`unpack: String -> List Char` は文字列を1文字ずつに分解する関数です。

``` text
Idris> unpack "101"
['1', '0', '1'] : List Char
```

それではこのプログラムをコンパイル・実行してみましょう。

``` text
$ idris -o PlacingMarbles PlacingMarbles.idr
$ ./PlacingMarbles
101
2
$ ./PlacingMarbles
000
0
```

動いているようです。

# 第3問
## 問題

[B - Shift only](https://atcoder.jp/contests/abc081/tasks/abc081_b)

> $A_1$ から $A_N$ までの $N$ 個の整数があります。
> これらの整数が全て偶数であるとき、次の操作を行うことができます
>
> * 全ての整数を2で割ったものに置き換える
>
> この操作は最大で何回行うことができるか求めて下さい。
>
> ## 制約
> * $ 1 \le N   \le 200$
> * $ 1 \le A_i \le 10^9$
>
> ## 入力
> 入力は以下の形式で標準入力から与えられる
>
> $N$  
> $A_1$ $A_2$ ... $A_n$
>
> ## 出力
> 最大で何回操作を行うことができるかを出力せよ

例えば以下の入力が与えられたとき

``` text
3
8 12 40
```

結果は以下です。

``` text
2
```

## コード

各整数の素因数に2が何個含まれるか数えて、その最小値を答えればよいですね。

素因数に2が何個含まれるかはビット操作に強い言語ならCTZ（count trailing zeros）で一発で出せたりするんですが、Idrisにはないのでまずは素因数に2が何個含まれるか数える関数を作るところからはじめましょう。

``` idris
isEven: Integer -> Bool
isEven n = n `mod` 2 == 0

countTwos : Integer -> Nat
countTwos n =
  if isEven n
  then 1 + (countTwos (n `div` 2))
  else 0
```

`n` が偶数であれば `n / 2` の素因数に含まれる2の数を数えて、それに1を足してあげればよいです。

これを使って `solve` は以下のように書けます。

``` idris
solve: List Integer -> Integer
solve l = cast $ foldl min 100 $ map countTwos l
```

最初の方針通り `map countTwos l` で各数値の素因数の中にある2の数を数えます。
そこから最小値を取り出すのに `foldl min 100` を使っています。ちょっとこれは解説をしましょう。

`foldl` は「畳み込み」をする演算子です。
二項演算子と演算の最初の値、そしてリストを引数にとり、畳み込んだ結果を返します。
例えば `[1, 2, 3]` があったときに `foldl (+) 0 [1, 2, 3]` は以下の計算をします。

``` idris
(((0 + 1) + 2) + 3)
```

その結果6を返します。

``` text
Idris> foldl (+) 0 [1, 2, 3]
6 : Integer
```

ここで初期値に `0` を使っているのは `0` が `+` の単位元、すなわち $n + 0 = 0 + n = n$ を満たす値だからです。

同様のことを `min` で行ったのが `solve` です。
`foldl min 100 [1, 2, 3]` は ``(((100 `min` 1) `min` 2) `min` 3)`` を計算するので結果1が返ります。

``` text
Idris> foldl min 100 [1, 2, 3]
1 : Integer
```

ここででてきた100というのが妥協の産物です。
本来なら `min` の単位元を使いたいところですが、今回の対象である `Nat` には `min` の単位元が存在しません。 `min` の単位元はその型の値の最大値ですが、 `Nat` には最大値がないからです。
なので単位元は使えません。
そこで妥協案として「今回のどの入力よりも大きい値」を据えることにします。
今回の入力の $A_i$ の最大値は $10^9$ なので素因数に含まれる2の個数はせいぜい30個程度です。ということでどんなに大きめにみても100は越えないので100を据えました。

因みにですが `foldl` は `Foldable` インタフェースに定義されていて、 `List` での `foldl` の定義は以下です。

``` idris
-- foldl : ( acc -> elem -> acc) -> acc -> List elem -> acc
foldl f q [] = q
foldl f q (x::xs) = foldl f (f q x) xs
```

さてさて、 `solve` が定義できたので一旦REPLで試してみましょう。

``` idris
λΠ> solve [8, 12, 40]
2 : Integer
λΠ> solve [5, 6, 8, 10]
0 : Integer
λΠ> solve [382253568, 723152896, 37802240, 379425024, 404894720, 471526144]
8 : Integer
```

動いていますね。

それでは入力を受け取る部分を書きます。


```idris
main : IO ()
main = do
  _ <- getLine
  line <- getLine
  let input = map cast $ words line
  printLn $ solve input
```

最初に入力の個数を教えてくれていますが、 個数に関係なく `words` でバラしてしまうので無視してしまいます。

それではこれをコンパイル・実行してみましょう。

``` idris
$ ./ShiftOnly
3
8 12 40
2
$ ./ShiftOnly
4
5 6 8 10
0
$ ./ShiftOnly
6
382253568 723152896 37802240 379425024 404894720 471526144
8
```

動いていますね。

# ここまで解いたら

最初は10問全部やろうとしたのですが、疲れたのでこれでラストにします。
ラストは高橋君の出てくる問題です。

## 問題
[B - Trained?](https://atcoder.jp/contests/abc065/tasks/abc065_b)

> 筋力をつけたい高橋君は、AtCoder 社のトレーニング設備で、トレーニングをすることにしました。

> AtCoder 社のトレーニング設備には $N$ 個のボタンがついており、ちょうど $1$ 個のボタンが光っています。
> ボタンには、$1$ から $N$ までの番号がついています。
> ボタン $i$ が光っているときにそのボタンを押すと、ボタン $i$ の明かりが消え、その後ボタン $ai$ が光ります。
> $i=ai$ であることもあります。
> 光っていないボタンを押しても、何も起こりません。
>
> 最初、ボタン $1$が光っています。高橋君は、ボタン $2$ が光っている状態で、トレーニングをやめたいと思っています。
>
> そのようなことは可能かどうか判定し、もし可能なら最低で何回ボタンを押す必要があるかを求めてください。
>
> ## 制約
> * $2 \le N \le 10^5$
> * $1 \le a_i \le N$
>
> ## 入力
> 入力は以下の形式で標準入力から与えられる。
>
> $N$  
> $a_1$  
> $a_2$  
> $:$  
> $a_N$
>
> ## 出力
> ボタン2を光らせることが不可能な場合は -1を出力せよ。
> そうでない場合はボタン2を光らせるために必要なボタンを置く回数の最小値を出力せよ。

## コード

困りましたね…。
高橋君のいる問題を選んだらまだ説明してない機能が必要になりました。
本来は集合とキーバリューマップが必要になる問題なのですが、それを使うにはモジュールや標準ライブラリ、パッケージ、コンパイラオプションなどの説明が必要になります。
ここでは非効率を承知の上で集合とキーバリューマップをリストで代用します。

さて、問題ですが、解けますよね？
$1$ を押したら $a_i$ が点灯し、 $a_i$ を押したら $a_j$ が点灯し…を繰り返していくとどこかで今まで押した番号に合流し、操作がループします。
今回の問題はループに突入する前に $2$ を引いたらそれまでの操作回数を、ループに突入したら $-1$ を返せばよさそうです。

まず集合とキーバリューマップの代用品を作ります。

集合は `empty` と `insert` と `contains` があれば十分です。

``` idris
-- Set

Set : Type -> Type
Set = List

empty : Set a
empty = []

insert : Eq a => a -> Set a -> Set a
insert a s = a :: s

contains : Eq a => a -> Set a -> Bool
contains a s = elem a s
```

Idrisは純粋関数型言語なので値が変更されることはありません。
なので `empty` は関数ではなく値です。
同じく `insert` は値を追加した新しい集合を返します。
使っている関数については、見てとれるかと思いますが、 `elm` は `a` が `s` に含まれるか検査する関数です。

キーバリューマップは `fromList` と `lookup` があれば十分です。

``` idris
-- Map
Map : Type -> Type -> Type
Map k v = List (k, v)

fromList : List (k, v) -> Map k v
fromList l = l

lookup : Eq k => k -> Map k v -> Maybe v
-- ...
```

`List (k, v)` に対する `lookup` はプレリュードに既に定義されているので特に定義しなくてもよいです（というか定義すると多重定義で怒られます）。

では `solve` を定義していきましょう。
まずは `List a` から `List (Integer, a)` を作る `indexed` 関数です。

``` idris
indexed : List a -> List (Integer, a)
indexed l = loop l 1
where
  loop : List a -> Integer -> List (Integer, a)
  loop []      _ = []
  loop (x::xs) i = (i, x) :: loop xs (i + 1)
```

Idrisではループは再帰関数で表わすのでした。
ループで持ち回る値（ここではインデックス）は引数で渡します。
`indexed` に無駄な引数を増やす訳にはいかないのでローカル関数を定義して、そこでループを回します。


`solve` も `indexed` と同じようにループをするローカル関数を定義して、そちらで実行します。

``` idris
solve : List Integer -> Integer
solve l =
  let map = fromList $ indexed l in
  let set = empty in
  case loop map set 1 0 of
    Just i  => i
    Nothing => -1
where
  loop : Map Integer Integer -> Set Integer -> Integer -> Integer -> Maybe Integer
  -- ...
```

ループで持ち回る値は以下です。

* `map`   : $i$ 番目のボタンを押したら $a_i$ 番目のボタンが光るという対応関係
* `set`   : 今までに押したことのあるボタン
* `cur`   : 現在光っているボタン
* `count` : ボタンを押した回数


`map` は入力から最初に作ってしまって以後特に増減しません。

``` idris
  let map = fromList $ indexed l in
```

`set` は初期値は空集合です。

``` idris
  let set = empty in
```

`cur` は問題文にあるとおり1が、ボタンを押した回数は最初は0です。

``` idris
loop map set 1 0
```


それでは `loop` の実装を見ていきましょう

``` idris
where
  loop : Map Integer Integer -> Set Integer -> Integer -> Integer -> Maybe Integer
  loop _    _  2   count = Just count
  loop map set cur count =
    let set = insert cur set in
    let Just next = lookup cur map | _ => Nothing in
    if contains next set
    then Nothing
    else loop map set next (count + 1)
```

まず、現在光っているボタンが2なら今までにボタンを押した回数を返して終了です。

``` idris
  loop _    _  2   count = Just count
```

それ以外の場合はボタンを押します。

``` idris
  loop map set cur count =
  -- ...
```

押したボタンを記録しましょう。

``` idris
    let set = insert cur set in
```

すると次のボタンが光るはずです。それを `next` とします。

``` idris
    let Just next = lookup cur map | _ => Nothing in
```

ここで、ようやく `let` とパターンマッチが出てきました。`let パターン = 式 | 他の場合 => 式 in` の構文です。
パターンマッチで `lookup` の結果が `Just` の場合のみ変数を束縛しています。
では `Nothing` の場合はどうなるかというと、 `| _ => Nothing` の方にいきます。
`Nothing` は `_` パターンにマッチし、腕の `Nothing` が返ります。

全体として、上記の式は以下のように読み替えられます。

``` idris
case lookup cur map of
  Just next => ....
  _         => Nothing
```

パターンにマッチしなかったら強制終了するよりずっと安全に想定していないケースを扱うことができました。

さて、プログラムの続きをみていきましょう。
もし次に光ったランプが今までに光ったことのあるランプだったらループに突入するので `Nothing` です。それ以外の場合は次のボタンを調べるため、ループを回します。

``` idris
    if contains next set
    then Nothing
    else loop map set next (count + 1)
```

ボタンを押したら同じボタンが光ることもあると書いてあったので、その辺の処理にも気を配りましょう。今回は先に `insert cur set` しているので大丈夫です。

最終的に、2に到達すればそのときの操作数を、到達できなければ-1を返します。

``` idris
  case loop map set 1 0 of
    Just i  => i
    Nothing => -1
```

REPLで動作を確認してみましょう。

``` idris
λΠ> solve [3, 1, 2]
2 : Integer
λΠ> solve [3, 4, 1, 2]
-1 : Integer
λΠ> solve [3, 3, 4, 2, 4]
3 : Integer
```

正しく動いていそうです。

それでは入力を受け取る部分を書きます。

``` idris
main : IO ()
main = do
  n <- getLine
  l <- getNIntegers (cast n)
  printLn $ solve l
where
  getNIntegers : Nat -> IO (List Integer)
  getNIntegers Z     = pure []
  getNIntegers (S n) = do
    i  <- getLine
    is <- getNIntegers n
    pure $ (cast i) :: is
```

今回は `N` を使います。
`N` で受け取った数の分だけ `getLine` してその数値をリストに入れます。
そのためにローカル関数 `getNIntegers` を定義しています。

`getNIntegers` は自然数を受け取って `IO (List Integer)` を返します。

``` idris
  getNIntegers : Nat -> IO (List Integer)
```

`Nat` は `Z | S Nat` で定義されているのでした。パターンマッチもそれを使います。

`Z` の場合は 0 要素のリストを返します。 `IO` モナドに入れるために `pure` を使っています。


``` idris
  getNIntegers Z     = pure []
```

`Z` でない場合は `S n` にマッチします。

``` idris
  getNIntegers (S n) = do
```

`S n` でマッチしているので `getNIntegers 10` と呼ばれたら `n` には `9` が入っていることに注意して下さい。

まずは1行読み込みます。

``` idris
    i  <- getLine
```

残り `n` 個の自然数を読めばいいので再帰呼び出しします。

``` idris
    is <- getNIntegers n
```

あとは結合して返すだけです。

``` idris
    pure $ (cast i) :: is
```

こちらも `IO` モナドに入れるために `pure` を使っています。


それではプログラムが完成したのでコンパイル・実行してみましょう。

``` text
$ idris -o Trained Trained.idr
$ ./Trained
3
3
1
2
2
$ ./Trained
4
3
4
1
2
-1
$ ./Trained
5
3
3
4
2
4
3
```

正しく動いているようです。

# まとめ

リストや文字列、IOの操作の練習に競技プログラミングの問題を解いていみました。
ある程度は練習になったものの、ライブラリがないと足りない機能があることも分かりました。

次は標準ライブラリやモジュールを紹介できたらなと思います。
