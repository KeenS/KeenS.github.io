---
categories: [SML, ML Advent Calendar, Advent Calendar 2020, Advent Calendar, 小ネタ]
date: 2020-12-03T00:47:45+09:00
title: "SMLでバッククォート記法を実現する"
---
このエントリは[ML Advent Calendar 2020](https://qiita.com/advent-calendar/2020/ml)の3日目の記事です。
前はelpinalさんで[StandardMLのwithtypeの挙動](https://elpinal.gitlab.io/writings/withtype/)でした。

κeenです。Haskelなどにある `` ` `` ~ `` ` `` の記法（のようなもの）をSMLで実現する話です。

<!--more-->

MLやHaskellには中置演算子を作る `infix` 系の構文があります。
そしてHaskellにはそれとは別に `` ` `` ~ `` ` `` で普通の関数を中置演算子として使える機能があります。

``` haskell
add :: Integer -> Integer -> Integer
add x y = x + y

1 `add` 2
```


さらに演算子を部分適用するセクションという機能もあります。

``` haskell
map (2 *) list
map (- 1) list
```


SMLでもこういう機能ほしいよね、という題材です。

最初に答えを書いてしまうとこういう演算子を定義します。

``` sml
infix  3 <\     fun x <\ f = fn y => f (x, y)     (* Left section      *)
infix  3 \>     fun f \> y = f y                  (* Left application  *)
infixr 3 />     fun f /> y = fn x => f (x, y)     (* Right section     *)
infixr 3 </     fun x </ f = f x                  (* Right application *)
```

そうすれば以下のように使えます。

``` sml
fun add(x, y) = x + y
1 <\add\> 2 <\add\> 3
```

これは以下と同じ意味になります。

``` sml
add(add(1, 2), 3)
```

`<\` と `\>` の変わりに `</` と `/>` を使うと右結合になります。

``` sml
1 </add/> 2 </add/> 3
(* = add(1, add(2, 3)) *)
```

どうしてこうなるのか詳しく見ていきましょう。

まずは `1 <\add\> 2` からはじめます。これは `<\` と `\>` が左結合の `infix 3` で定義されているので以下のように脱糖されます。

``` sml
op\>(op<\(1, add), 2)
```

それではこれを内側から評価していきましょう。 `<\` は以下のように定義されているのでした。

``` sml
fun x <\ f = fn y => f (x, y)
```

すると上の式はこうなります。

``` sml
op\>(fn y => add(1, y), 2)
```

次に `\>` です。これは以下のように定義されているのでした。

``` sml
fun f \> y = f y
```

すると上の式はこうなります。

``` sml
(fn y => add(1, y)) 2
```

これを簡約するとこうなる訳です。

``` sml
add(1, 2)
```

ちゃんと、関数適用に簡約されましたね。
`</` と `/>` も開と閉の役割が入れ替わるだけなので気になる人は各自で追ってみて下さい。


さて、これを使うとセクションも実現できます。

``` sml
List.map (2<\op*) list
List.map (op- /> 1) list
```

すごい！

因みに余った `\>` と `</` も役割があります。
`\>` がHaskellでいう `$` 相当で、 `</` がF# でいう `|>` 相当の演算子です。
ただし優先順位がこちらは3なのに対して `$` と `|>` は1ですが。

ちなみにこの記事にま元ネタがあって、MLtonの中の人が書いた記事です：  
[InfixingOperators](http://www.mlton.org/InfixingOperators)  
こちらではもう少し色々紹介されています。

ML Advent Calendarの中で他の面白い記事も紹介していけたらなと思います。
