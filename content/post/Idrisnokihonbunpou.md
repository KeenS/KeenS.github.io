---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-02T22:27:01+09:00
title: "Idrisの基本文法"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の4日目のエントリです。前はhelloyukiさんで「IdrisにHello World on VSCode & Mac」でした。

κeenです。今回は基本文法を解説します。

<!--more-->


# 関数と変数

ドキュメントには書かれてないのですが、コンパイラのコードを見る限り識別子に使えるのは `[a-zA-Z][a-zA-Z'_.]*` のようです。
例えば `hog'.'e12_` は適格なIdrisの変数です。

## グローバル変数

変数は以下の構文で定義します。

``` idris
名前 : 型
名前 = 式
```

例：

```idris
version : Integer
version = 100
```

## 関数

関数は以下の構文で定義します。

``` idris
名前 : 型
名前 引数1 引数2 .. 引数n = 式
```

関数の型は `引数1の型 -> 引数2の型 -> 引数2の型 -> 返り値の型` です。

例：

```idris
add : Integer -> Integer -> Integer
add x y = x + y
```


変数や関数はcamelCaseの命名が慣例です。
コンパイラ側でも先頭に小文字がきたら変数と思って処理している箇所があるようです。

## ローカル変数

`let 変数 = 式 in 続く式` の構文で定義します。

例：

``` idris
add3 : Integer -> Integer -> Integer -> Integer
add3 x y z = let tmp = x + y in
             tmp + z
```

他にも式の後ろに `where` を続けて書く記法もあります。

例：


``` idris
add3 : Integer -> Integer -> Integer -> Integer
add3 x y z = tmp + z
where
  tmp : Integer
  tmp = x + y
```

Idrisは [オフサイドルール](https://ja.wikipedia.org/wiki/オフサイドルール)を採用しているのでインデントが同じなら同じブロックとみなしてくれます。
`where` のあとに続く定義はインデントを揃えれば複数書けます。


## ローカル関数

`where` の記法で定義できます。

例：

``` idris
isLeapYear : Integer -> Bool
isLeapYear y = isM4 y && not (isM100 y) && isM400 y
where
  isM4 : Integer -> Bool
  isM4 y = y `mod` 4 == 0

  isM100 : Integer -> Bool
  isM100 y = y `mod` 100 == 0

  isM400 : Integer -> Bool
  isM400 y = y `mod` 400 == 0
```

## 無名関数

`\引数 => 式` の構文で作れます。

``` idris
quatro: Integer -> Integer
quatro n = let double = \i => i * 2 in
           double (double n)
```

# 制御構造
## `if`

`if` 式は `if 条件 then then節 else else節` の構文をしています。

例：

``` idris
if n == 0
then "Zero"
else "Not zero"
```

Idrisは式指向言語なので `if` も値を返します（なので `if` 「式」と呼ばれます）。
他の言語でいういわゆる三項演算子のようなものは必要ありませんん。

``` idris
main : IO ()
main =
  putStrLn (if 1 == 0 then "Zero" else "Not zero")
```

## パターンマッチ

パターンマッチは `case 条件 of パターン => 式 ...` の構文をしています。
`パターン => 式` の部分にはオフサイドルールが適用されます。


例：

``` idris
case n of
  1 => "one"
  2 => "two"
  3 => "three"
  _ => "many"
```

最後の `_` は特殊なパターンで、どんな値にもマッチしてその値を無視します。

パターンマッチはもうちょっと複雑なこともできます。
値の構造がパターンに合致すればマッチ成功となります。
さらに、その値を変数に束縛できます。

例：

``` idris
case list of
  [] => 0
  [x, y] => x + y
  [x, y, z] => x + y + z
  _ => -1
```

これの `list` が `[1, 2, 3]` ならば `[x, y, z]` の節にマッチして `x + y + z` 、つまり `1 + 2 + 3` が計算されて6が返ります。

### 関数の引数でのパターンマッチ

関数の引数でもパターンマッチが可能です。
関数の名前を連ねる形になります。

例：

``` idris
fib: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```


## ループ、 `return` 、 `break`
ないよ。

Idrisは関数型言語なのでループは関数を使います。
自身を呼び出すことでループを作れるのです（再帰関数）。
`break` や `return` は必要ありません。自身を呼び出すことをやめれば自然とループが止まりますし、その場で値が返ります。

例：`1` から `n` までの和を求める関数

``` idris
sumFromOne: Integer -> Integer
sumFromOne n = loop 1 n 0
where
  loop: Integer -> Integer -> Integer -> Integer
  loop i end sum =
    let sum = sum + i in
    if i == end
    then sum
    else loop (i + 1) end sum
```

`loop i end sum = ...` ではじまって `loop (i + 1) end sum` を呼んでいるので `i` を1つづつ増やしていっているのが読み取れるでしょうか。

# プリミティブ

思ったより少ないです。

| 名前      | 説明               |
|-----------|--------------------|
| `Int`     | 固定長整数         |
| `Integer` | 多倍長整数         |
| `Double`  | 倍精度浮動小数点数 |
| `Char`    | 文字               |
| `String`  | 文字列             |
| `Ptr`     | FFI用              |

ブール値なんかもありません。
`Bool` はユーザ定義型として定義されています。

# データ型

`data 名前 = 定義` の構文で定義します。
定義のところには `ヴァリアント | ヴァリアント…` と書きます。
ヴァリアントには `コンストラクタ 引数の型 …` と書きます。
ヴァリアントは少なくとも1つ、コンストラクタの引数は0以上を書きます。

例：引数のないコンストラクタのヴァリアントを2つ持つデータ型

``` idris
data Bool = True | False
```



例：引数の2つあるコンストラクタのヴァリアントを1つ持つデータ型

``` idris
data Person = MkPerson Int String
```

引数ありのコンストラクタのヴァリアントを1つ持つデータ型は頻出パターンで、構造体のように使えます。
そのときのコンストラクタが構造体のコンストラクタのようになります。こういうときは `MkHoge` と `Mk` （makeの略）を前置するのが慣例です。


例： 引数のあるコンストラクタや引数のないコンストラクタのヴァリアントのあるデータ型

``` idris
data FizzBuzz = F | B | FB | I Integer
```


データ型は語ることが多いので回を新ためて説明しようと思います。

# まとめ

Idrisの基本文法を説明しました。
