---
categories: [Idris, Idris Advent Calendar, Advent Calendar 202, Advent Calendar]
date: 2020-12-20T22:50:48+09:00
title: "Idris面白機能：シンタックスシュガーとオーバーロード、あとユーザ定義構文"
---

このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の17日目の記事です。

κeenです。今回は小ネタとしてシンタックスシュガー（糖衣構文）やオーバーロードについて紹介します。
あとIdrisにはユーザが構文を拡張できる機能もあるのでそれも紹介します。

<!--more-->

# `if` 式

Idrisでは `if expr1 then expr2 else expr3` が糖衣構文として定義されています。
では展開すると何になるかというと、 `ifThenElse` 関数です。
「えっ？無理じゃない？」と思った方、正しいです。普通の言語では `if` は関数では書けません。
しかしIdrisでは関数で書けるのです。

昔紹介したようにIdrisでは[Lazyという機能](https://keens.github.io/blog/2019/02/14/effective_idris__lazy/)を使うと遅延評価ができます
（IdrisはHaskellと違ってデフォルトではEager Evaluationをします）。
この `Lazy` を使って `ifThenElse` 関数はこう定義されています。

```idris
ifThenElse : Bool -> Lazy a -> Lazy a -> a
ifThenElse True  t e = t
ifThenElse False t e = e
```

ところで、Idrisでは関数をオーバーロードできるのでした。
`ifThenElse` をオーバーロードするとどうなると思いますか？
そう、 `if` 式をオーバーロードできるのです。
少し試してみましょう。

以下の関数を定義します。


``` idris
ifThenElse: Maybe a -> (a -> b) -> Lazy b -> b
ifThenElse (Just a) f _ = f a
ifThenElse Nothing _  b = b
```

これを読み込んだREPLでは `Maybe` 型の式に対して `if` が使えます。

``` idris
Idris> if Just 1 then (+1) else -1
2 : Integer
Idris> if Nothing then (+1) else -1
-1 : Integer
```

面白いですね。

因みにLazyの記事で紹介しましたがショートサーキットする論理積の `&&` も `Lazy` を使ったただの関数です。
Idrisでは演算子もユーザが定義できるのでこういった部分もユーザランドで定義できてしまうんですね。

# `do` 記法

`do` 記法もシンタックシュガーです。
以下の `do` 式は

``` idris
do
x <- hoge
y <- fuga x
z <- piyo y
pure $ chun z
```

以下のように展開されます。

``` idris
hoge   >>= (\x =>
fuga x >>= (\y =>
piyo y >>= (\z =>
pure $ chun z
)))
```

ここで `(>>=)` もオーバーロード可能なので変テコな定義も可能なはずなんですが、パースエラーになってしまいました。

``` idris
(>>=) : String -> a -> String
(>>=) s _ = s


hoge : String
hoge = do
  "This is string"
  "Others will be ignored"
  ["Nothing Happen"]
```


``` text
Type checking ./doNotation.idr
doNotation.idr:6:8-9:20:
  |
6 | hoge = do
  |        ~~ ...
Incomplete term do "This is string"
                   "Others will be ignored"
                   ["Nothing Happen"]

Holes: Main.hoge
```

謎です。


# その他モナド関連記法

Idrisには何故か `do` 記法と役割の被る構文がいっぱいあります。

以下の関数を書き換えながら紹介します。

``` idris
addDo : Maybe Int -> Maybe Int -> Maybe Int
addDo xs ys = do
  x <- xs
  y <- ys
  pure $ x + y
```

## 内包表記

以前リスト内包表記と紹介しましたが、より一般にはモナド内包表記です。
例えば `Maybe` に対しても使えます。

``` idris
addComplehensions : Maybe Int -> Maybe Int -> Maybe Int
addComplehensions xs ys = [x + y | x <- xs, y <- ys]
```

ガード式も書けるんですが、Alternativeに触れないといけなくなるのでここでは流します。

## `!` 記法

もうちょっと簡潔に書く方法として `!` 記法もあります。

``` idris
addBang : Maybe Int -> Maybe Int -> Maybe Int
addBang x y = pure $  !x + !y
```

## 熟語括弧（idiom brackets）

これはMonadではなくApplicativeの記法です。

``` idris
addBracket : Maybe Int -> Maybe Int -> Maybe Int
addBracket xs ys = [| xs + ys |]
```

これは以下に同じです。


``` idris
addBracket : Maybe Int -> Maybe Int -> Maybe Int
addBracket xs ys = (+) <$> xs <*> ys
```

これはまあ、分かる。


# ユーザ定義構文

Idrisには中置演算子をユーザが定義できる機能があるというのはAdvent Calendarで紹介した通りですが、もうちょっと一般にn項演算子を定義する機能があります。`syntax 構文.... = 内容` の構文です。構文のところにはダブルクォートで囲ったキーワード （`"キーワード"`）か、式を表わす `[仮引数]` 、 変数を表わす `{仮変数}` が書けます [^1]。

[^1]: ドキュメントには言語のキーワードならダブルクォートなしで書けるとあるのですが、実際に試したらエラーになりました。

内容のところにはIdrisの式を書きます。

`if then else` は既にあるので `unless then else` を定義してみましょう。

``` idris
syntax "unless" [test] "then" [t] "else" [e] = ifThenElse test e t
```

``` idris
Idris> unless True then 1 else 2
2 : Integer
Idris> unless False then 1 else 2
1 : Integer
```

次に変数を使うユーザ定義構文も定義してみましょう。 `for` 式です。

``` idris
syntax for {x} "in" [xs] ":" [body] = forLoop xs (\x => body)

forLoop : List a -> (a -> b) -> List b
forLoop l f = map f l
```

`{x}` と仮変数を使っていますね。

REPLで試すと動いているのが分かります。

```text
Idris> for x in [1, 2, 3]: x + 1
[2, 3, 4] : List Integer
```


因みに `syntax` で定義した構文はパターンとしても使えます。
例えば以下のように定義すると、 `Some` を `Just` の代わりに使えます。

``` idris
syntax "Some" [x] = Just x
syntax "None" = Nothing
```

パターンとして使うときはこうですね。

``` idris
hoge : Maybe Int -> Int
hoge x =
  case x of
    Some x => x
    None => -1
```

値として使うときはこうです。

``` idris
Idris> hoge (Some 1)
1 : Int
Idris> hoge None
-1 : Int
```

パターンでだけ、値でだけ定義したいときは `pattern syntax` と `term syntax` という構文もあります。

詳しくは [`syntax` rulesのドキュメント](http://docs.idris-lang.org/en/latest/tutorial/syntax.html#syntax-rules)を参照して下さい。文法にあいまい性が生じそうなケースはエラーになるようです。


# リテラルのオーバーロード

`\x => body` と書いたらユーザ定義データの `Lambda x body` になったり `f x` と書いたら `App f x` になったりのヤバそうな機能もあるようです。詳細は論文読んでねスタイルなのでここではスルーします。

興味のある方は [`dsl` notationのドキュメント](http://docs.idris-lang.org/en/latest/tutorial/syntax.html#dsl-notation) を読んでみて下さい。


# まとめ

Idrisのシンタックスシュガーとそれをオーバーロードする方法を紹介しました。
また、 `syntax` rulesというn項演算子を作れる機能も紹介しました。
