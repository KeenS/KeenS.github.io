---
categories: [依存型, Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-22T04:53:18+09:00
title: "Idris面白機能：with構文と依存型、View"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の18日目の記事です。

κeenです。Idrisで個人的に面白いなーと思ってる機能、 `with` 構文とそれを利用したViewについて紹介します。

<!--more-->

# パターンマッチを補助する `with` 構文

関数を書くときに条件分岐したくなることがありますよね。
データ型の構造に沿う条件であれば引数でのパターンマッチで済むんですが、もう少し複雑な条件だと `if` や `case` を使わざるを得なくなります。
例えば「リストに値がなければ追加する」関数 `addToList` はこう書けるでしょう。

```idris
addToList : Eq a => a -> List a -> List a
addToList x xs =
  if elem x xs
  then xs
  else x::xs
```

こういうのも引数のパターンマッチで書けると綺麗ですよね。
これは `with` 構文で解決できます。
`関数名 引数… with (補助式) 節…` の構文です。
節は `関数名 引数… | 補助パターン = 本体` と書きます。

例えば先程の `addToList` だと以下の構文になります。


```idris
addToList : Eq a => a -> List a -> List a
addToList x xs with (elem x xs)
  addToList x xs | True  = xs
  addToList x xs | False = x :: xs
```

関数の本体が `= xs` と `= x :: xs` でサッパリしたので気持良いですね。

ところで、上記の記述 `addToList x xs` が続いてちょっとくどいですよね。これは省略できます。
以下のようにも書けるのです。

```idris
addToList : Eq a => a -> List a -> List a
addToList x xs with (elem x xs)
  | True  = xs
  | False = x :: xs
```

もっとサッパリしましたね。

# `with` の発展的利用例

`with` 構文の面白いところは、計算した結果をパターンマッチにかけられるところです。

例えばUNIXタイムスタンプが定義してあるとします。

```idris
data Timestamp = MkTimestamp Int
```

これを受け取って時刻の文字列を返す関数を定義してみましょう。
タイムスタンプから時刻を抜き出す関数と時刻から文字列を作る関数に分けると具合がよさそうです。

ということでまずはタイムスタンプから時刻を抜き出しましょう。JSTにあわせて+9時間しています。

```idris
toTime : Timestamp -> (Int, Int, Int)
toTime (MkTimestamp t) =
  let time = t `mod` (60 * 60 * 24) in
  let h =  time `div` (60 * 60) in
  let m = (time `div` 60) `mod` 60 in
  let s = time `mod` 60 in
  ((h + 9) `mod` 24, m, s)
```

これと `with` 構文を使えば時刻を表示する関数 `showTime` は至極完結に書けます。

```idris
showTime : Timestamp -> String
showTime t with (toTime t)
  | (h, m, s) = (show h) ++ ":" ++ (show m) ++ ":" ++(show s)
```

`toTime` により `Timestamp` の別表現が与えられたような形になっていますね。

# 依存パターンマッチ

Viewの話をする前に依存パターンマッチを紹介しましょう。
Idrisでは型パラメータなども `{}` で取り出せることは説明しましたね？
例えば `Vect n a` の `n` は以下のように取り出せます。

``` idris
length : Vect n a -> Nat
length {n = n} _ = n
```

この `n` の部分でパターンマッチができるのですが、ちょっと面白い振る舞いをします。
以下の関数を見てみて下さい。

```idris
append : Vect n a -> Vect m a -> Vect (n + m) a
append {n=Z}   []        ys = ys
append {n=S k} (x :: xs) ys = append (x :: xs) ys
```

`n` と `Vect` で2つのパターンマッチが走っていますね。
`n` には `Z` と `Suc n` 、 `Vect` には `[]` と `x :: xs` のパターンがあるので普通なら 2 x 2 = 4 種類のパターンがあるはずです。ところが、ここでは2通りのパターンマッチしかしていません。
`n` は `Vect` の長さに連動しているので `n = Z` のときには `Vect` は `[]` ですし、 `n = S k` のときは `Vect` は `x :: xs` です。それ以外の可能性はありません。
なので上記2通りで全ての場合を尽しているのです。

証拠に例えば `n = S k` かつ `Vect` が `[]` のパターンを追加するとエラーになります。

``` idris
append : Vect n a -> Vect m a -> Vect (n + m) a
append {n=Z}   []        ys = ys
append {n=S k} []        ys = append (x :: xs) ys
append {n=S k} (x :: xs) ys = append (x :: xs) ys
```

これをコンパイルすると以下のエラーが出ます。

``` text
- + Errors (1)
 `-- withAndView.idr line 48 col 0:
     When checking left hand side of append:
     When checking an application of Main.append:
             Type mismatch between
                     Vect 0 elem (Type of [])
             and
                     Vect (S k) a (Expected type)
             
             Specifically:
                     Type mismatch between
                             0
                     and
                             S k
```

このように依存型を使うと型と値が軛で繋がれたように連動するのです。
この仕組みを利用したのがViewです。

# View

依存型を使うとパターンマッチのときに型と値が連動すると紹介しました。
では、とある値を受け取ったときにそれに依存する型を作れば値のパターンマッチを自由に制御できるのではないかという発想に至ります。実際可能で、それを利用するプログラミングパターンがViewです。
具体例をいくつか紹介しましょう。

`Data.List.Views` には `List` 型に対するViewがいくつかあります。
その中でも `Split` Viewを使ってみましょう。

``` text
Data type Data.List.Views.Split : List a -> Type
    View for splitting a list in half, non-recursively
    
    The function is: public export
Constructors:
    SplitNil : Split []
        
        
        The function is: public export
    SplitOne : Split [x]
        
        
        The function is: public export
    SplitPair : Split (x :: xs ++ y :: ys)
        
        
        The function is: public export

```


コンストラクタが3つあって、それぞれの型が `Split []` 、 `Split [x]` 、 `Split (x :: xs ++ y :: ys)` ですね。`Split` の値は `split` 関数で作れます。使ってみましょう。


``` idris
splitView : List a -> List a
splitView l with (split l)
  splitView []                   | SplitNil  = []
  splitView [x]                  | SplitOne  = []
  splitView (x :: xs ++ y :: ys) | SplitPair = []
```

`splitView` の引数のパターンマッチ (`|` の左)が `Split` の値に応じて変化しているのが分かりますか？
そして特に目につくのが3節目の `(x :: xs ++ y :: ys)` です。
`++` は関数なので普通はパターンマッチでは使えません。ところがリストに依存している `Split` の方がそういう型をしているのでこういうパターンも書けてしまうのです。

例えばこれを使ってマージソートが書けるでしょう。

``` idris
mergeSort : Ord a => List a -> List a
mergeSort l with (split l)
  mergeSort []               | SplitNil = []
  mergeSort [x]              | SplitOne = [x]
  mergeSort (x::xs ++ y::ys) | SplitPair = merge (mergeSort (x::xs)) (mergeSort (y::ys))
  -- mergeはpreludeで定義されている
```

この機能は他の言語では中々見掛けないんじゃないでしょうか。

# 再帰View

Viewの中には再帰的に定義されるものがあります。例えば `SnocList` などです。
`snoc` というのは `cons` をさかさまにした文字列で、 `cons` がリストの先頭に要素を加えるのに対して `snoc` はリストの末尾に要素を加えます。なので `SnocList` はパターンマッチでリストの末尾からデータを取り出すときに使います。

今までの知識で書くとこうなりますよね？

``` idris
reverseList : List a -> List a
reverseList l with (snocList l)
  reverseList []          | Empty    = []
  reverseList (xs ++ [x]) | Snoc _ = x :: (reverseList xs)
```

これでも動くんですが、効率が悪いです。 `snocList` はリストの末尾まで辿ります。
それを `reverseList` の呼び出しごとに呼んでいるので無駄です。

`SnocList` の `Snoc` には実は `xs` の分の `SnocList` の値も保持されています。
`_` で無視している部分ですね。これを使うと効率的に再帰ができそうです。
ですが、これをどうやって使いましょう？

1つの方法は補助関数を作ることです。

``` idris
reverseList : List a -> List a
reverseList l = helper l (snocList l)
where
  helper : (l : List a) -> SnocList l -> List a
  helper []          Empty      = []
  helper (xs ++ [x]) (Snoc rec) = x :: (helper xs rec)
```

これでもいいんですが、ちょっと野暮ったいですよね？
Idrisには `with` で使うViewを外から差し込む構文があります。
`関数 引数 | view` という構文です。

``` idris
reverseList : List a -> List a
reverseList l with (snocList l)
  reverseList []          | Empty    = []
  reverseList (xs ++ [x]) | Snoc rec = x :: (reverseList xs | rec)
```

`reverseList xs | rec` の部分がそれです。ちょっと奇妙ですが便利ですね。

因みに内部的には `with` と `|` を使った関数は `helper` のような定義に展開されているらしいです。


# 多重 `with`

`with` 使ってる途中で追加の `with` をはじめることもできます。


``` idris
isSuffix : Eq a => List a -> List a -> Bool
isSuffix l1 l2 with (snocList l1)
  isSuffix []          _  | Empty = True
  isSuffix (xs ++ [x]) l2 | Snoc rec with (snocList l2)
    isSuffix _           []          |  _      | Empty   = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | Snoc r1 | Snoc r2 =
      if x == y
      then isSuffix xs ys | r1 | r2
      else False
```


使いすぎると訳がわからなくなりそうですが、覚えておいて損はないでしょう。

# まとめ

`with` 構文と依存型によるパターンマッチの面白い挙動、それを使ったViewについて紹介しました。
`with` 構文のもう1つの使い方として証明があるのですが、それはまた後日ということで。
