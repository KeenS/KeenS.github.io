---
categories: ["Idris"]
date: 2019-01-07T03:44:57+09:00
title: "Idris入門: 数当てゲーム"
---
κeenです。
Idrisの入門記事ってあまりないなと思ったので少し書いてみます。
私は別にIdrisに詳しいわけではないので間違っているかもしれないことに注意してください。

<!--more-->

[Idris](https://www.idris-lang.org/) は依存型を持つ汎用プログラミング言語で、Haskellっぽい文法や機能を持ちます。
依存型を持っていてかつ汎用のプログラミング言語というのがめずらしいのでその手の文脈ではだいたい名前が上がります。
Idrisの型システムはものすごく雑にいうと型がプログラム可能です。型を書く位置に関数を書いたり逆に型を値としてプログラムに渡したり出来ます。
これらの機能を使って型を詳細に書けるので正しい入力しか受け付けないぜ！みたいなことが書けます。

ところでIdrisに入門しようとすると難解な記事が多いのが現状です。
Idrisの関連記事はどうしても依存型に目が行ってしまうので普通のプログラミング言語としての紹介が中々見当たらないのです。
そこで今回は依存型の部分を完全に無視してプログラムを書いてみます。

対象読者はプログラミング言語を最低1つは理解している人に設定しますが、ぶっちゃけHaskell読める人(書けなくてもいい)じゃないときついよなーと思いつつ書いてます。`IO` とか `do` とか。

因みに一次情報として[公式チュートリアル](http://docs.idris-lang.org/en/latest/tutorial/index.html)もあります。
HaskellやOCamlに馴染んでいる人向けに高速で言語機能を1巡りしています。

この記事ではIdris 1.3.1を使ってコードを書いていきます。

# はじめに

このチュートリアルは正直なところ題材があまりチュートリアル向きでないです。
もちろん「数当てゲーム」自体は簡単なプログラムですし出来上がるアプリケーションも小さなものです。
しかし純粋関数型言語であるIdrisで、ユーザとの連携や乱数取得という副作用の多いプログラムを書こうとすると色々な機能を使わざるを得ません。
そのため、1つのコードを説明するのに出てくる概念が多くなります。Haskellの入門書でも "Hello World" は後の方に出てくることが多いようです。
それを承知の上で、やっぱり動くアプリケーションを作らないと気がすまない人向けに「数当てゲーム」を実装するチュートリアルがこれです。

数当てゲームを簡単に説明すると、ゲームが立ち上がると裏で1から100までの数値(答え)を1つ決めます。
そしてユーザがなにか数値を入力します。
ユーザの入力が答えと一致したらその場でゲーム終了、違えば入力が答えより大きいか小さいかを教えてくれ、また入力を促します。

こういうゲームを作りながらIdrisを学んでいきます。

# 環境構築
[別記事](https://keens.github.io/blog/2019/01/06/idriskankyoukouchiku/) 参照

# Hello World

まずは動くものを作りましょう。
`hello.idr` に以下を書きます。

``` idris
main: IO ()
main = putStrLn "Hello world"
```

コンパイル、実行してみましょう。

``` console
$ idris hello.idr -o hello
$ ./hello
Hello world
```

ひとまず動きました。

## Hello World解剖

Idrisの値/関数定義は以下のような形をしています。
型と値で2行に分けて書きます。

``` idris
名前 : 型
名前 [引数 ...] = 値
```

先のプログラムの例でいくと `main` が名前、 `IO ()` が型、 `putStrLn "Hello world"` が値です。
`putStrLn "Hello world"` は `putStrLn` 関数に `"Hello world"` という値を渡しています。関数呼び出しに括弧は不要なので注意してください。

Idrisのプログラムは `main` から実行されます。

Cの `main` の返り値は `int` ですしJavaの `main` の返り値は `void` ですね。
Idrisの `main` はおおむね `()` 型を返していますが、`IO`というのがついています。
`IO ()` という書き方はJavaでいう `IO<()>` のように型パラメータを渡す記法です。
`IO` というのは出入力をするプログラムであることを表しています。
今回は出力をしていますね。
これについて踏み込むと[モで始まる名状し難いなにか](https://ja.wikipedia.org/wiki/%E3%83%A2%E3%83%8A%E3%83%89_(%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0))が出てくるのですがその前に次のプログラムに進みましょう。

# 入力を得る

次は世界ではなくユーザに挨拶してみましょう。先程のプログラムを以下のように書換えます。
インデントは大事ですので変えないで下さいね。

``` idris
main: IO ()
main = do
  putStr "What's your name? "
  name <- getLine
  putStrLn ("Hello " ++ name)
```

このコードも先程と同様にコンパイル、実行できますが、今度はインタラクティブシェル(REPL)を使ってみます。
`idris` コマンドでREPLを起動し、 `:load hello.idr` でロード、 `:exec main` で `main` を実行します。

``` console
$ idris
     ____    __     _
    /  _/___/ /____(_)____
    / // __  / ___/ / ___/     Version 1.3.1-git:1510fce92
  _/ // /_/ / /  / (__  )      http://www.idris-lang.org/
 /___/\__,_/_/  /_/____/       Type :? for help

Idris is free software with ABSOLUTELY NO WARRANTY.
For details type :warranty.
Idris> :load hello.idr
*hello> :exec main
What's your name? keen
Hello keen
```

`:` ではじまるのはREPLのコマンドです。 `:help` で一覧が見れます。 REPLを終了する `:quit` 、 ロードしたファイルの変更を監視してリロードする `:watch` などがあります。
今後は実行方法は指定しないので好きな方法で実行して下さい。

さて、先程のプログラムを解説しましょう。

``` idris
main: IO ()
main = do
  putStr "What's your name? "
  name <- getLine
  putStrLn ("Hello " ++ name)
```

新しく登場した `putStr` 、 `getLine` の関数については説明不要ですかね。
`do` と `<-` について解説します。

`do` を正確に説明するにはそこそこの前提知識が必要なので雰囲気で説明します。
`do` はプログラムを連接するときに使う記法です。
`do` は[オフサイドルール](https://ja.wikipedia.org/wiki/%E3%82%AA%E3%83%95%E3%82%B5%E3%82%A4%E3%83%89%E3%83%AB%E3%83%BC%E3%83%AB)でブロックが書けるのでここでは続く3行を連接しています。
以下のように波括弧で括ったりさらにセミコロンをつけても同じことです。

``` idris
main: IO ()
main = do {
  putStr "What's your name? ";
  name <- getLine;
  putStrLn ("Hello " ++ name);
}

```

複数行のプログラムを書くだけでもったいぶってるようですが、Idrisは純粋関数型言語なのでIOのように純粋でない操作は特別な対処をしてあげないといけないのです。
Idrisは純粋なのでIOや値の変更などができません。
じゃあさっき書いたプログラムはどうなってるんだと思いますよね。
`main` に書いたのは「IOを行うプログラム」という **値** です。よく考えてみると `main` には引数がありませんでしたよね。 `main` は関数ではなく値です。
値を書いただけなので副作用はありません。
Idrisのランタイムが `main` に書いてある値を読み取って勝手にIOを実行しただけです。

屁理屈じみてますが、これで純粋性との折り合いが付いているようです。実際、`main` 以外ではIOを行えないので純粋性は保たれますしね。
しかし `main` に値として渡さないと実行されないので複数回IOをするときに困ります。値は1つだけなので複数のIOをするプログラムを1まとめにしてmainに渡さないといけません。
そこで `do` は複数の(ある条件を満たした)値を1つに連接する働きをします。

繰り返しますが、雰囲気で説明しています。いつか正確な説明を受けて下さい。

ここでの `do` が何を連接しているか確認するためにREPLで今回使った3つの関数の型を見てみましょう。
`:t <expr>` あるいは `:type <expr>` で式の型が見れます。

``` text
*hello> :t putStr
putStr : String -> IO ()
*hello> :t getLine
getLine : IO String
*hello> :t putStrLn
putStrLn : String -> IO ()
```

`->` は関数の型で、 `String -> IO ()` は「`String` を受け取って `IO ()` を返す関数」という意味の型です。
引数を埋めたあとだと今回の `do` は `IO ()` 、 `IO String` 、 `IO ()` を繋げています。
`IO ()` は「IOをして `()` を返すプログラム」、 `IO String` は 「IOをして `String` を返すプログラム」です。

ここでようやく `<-` の説明ができます。
プログラムを合成するときに前のプログラムが返した値を取り出したいですよね。
それをするのが `<-` です。

ここまできたら先程のプログラムは理解できるかと思います。 因みに `++` はここでは文字列の連結です。

``` idris
main: IO ()
main = do
  putStr "What's your name? "
  name <- getLine
  putStrLn ("Hello " ++ name)
```

# ビルドシステム
Idrisには一応[パッケージシステム](http://docs.idris-lang.org/en/latest/reference/packages.html)が付いています。
パッケージシステムとはいってもインターネットから依存をダウンロードしたりはできません。しかしビルドだけならこれで十分です。

数当てゲームのパッケージを作りましょう。
プロジェクトテンプレートを作る機能はないので自分で作ります。
以下のようなディレクトリ構成でディレクトリを作りましょう。

```
├── idris-guessing-game.ipkg
└── src
    └── Main.idr
```

idris-guessing-game.ipkgには以下を書きます。内容はコメントから推察して下さい。

```text
package idris-guessing-game
-- バッケージ名

-- ソースコードの場所
sourcedir = src
-- ビルドするモジュール(ファイル名)
modules = Main
-- 生成する実行可能ファイルの名前
executable = "guessing_game"
-- `Main.main` を定義するモジュール(ファイル名)
main = Main
```

Main.idrには以下を書きます。


``` idris
module Main

main : IO ()
main = putStrLn "Hello ipkg"
```

今まで出てこなかった `module` というのがでてきましたね。名前空間です。
なくても動きますし、ファイル名と関連づく必要もないです。しかしソースディレクトリの `Foo/Bar.idr` ファイル内には `module Foo.Bar` を書くことが多いようです。
モジュール名は自由ですが、バイナリを作る際は `Main` モジュールに `main` を書く必要があります。

さて、これをビルドしましょう。 `idris --build idris-guessing-game.ipkg ` でできます。

``` console
$ idris --build idris-guessing-game.ipkg
Entering directory `./src'
Type checking ./Main.idr
Leaving directory `./src'
```

成功すれば `guessing_game` という名前で実行可能ファイルができます。

``` console
$ ./guessing_game
Hello ipkg
```

ビルドできました。

因みに ipkg ファイルを書いておくとだいたいのエディタプラグインはそれを読み込んでくれるようです。
また、REPLを起動するのも `idris --repl hoge.ipkg` のようにipkgファイルを指定してプロジェクトを読み込めます。

さて、プロジェクトができたのでこのままゲームを開発していきます。

# 数字を得る

最初の例で文字列を取得できたので今度は文字列を数値にしてみます。
[base](https://www.idris-lang.org/docs/current/base_doc/)ライブラリの[`Data.String`](https://www.idris-lang.org/docs/current/base_doc/docs/Data.String.html)にある[`parsePositive`](https://www.idris-lang.org/docs/current/base_doc/docs/Data.String.html)が目的のもののようです。
これは一度REPLで試した方が良いでしょう。
一旦REPLに `Data.String` をロードします。

``` text
Idris> :module Data.String
```

`parsePositive` は `String` を受け取って `Maybe Integer` を返す関数です。

``` text
*Data/String> parsePositive
parsePositive : String -> Maybe Integer
```

`Maybe` は他言語で言うところのNullableとかOptionalとかに相当します。
値がある `Just` か、値がない `Nothing` のどちらかです。
REPLで入力を与えてみましょう。

``` idris
*Data/String> parsePositive  "10"
Just 10 : Maybe Integer
*Data/String> parsePositive  "-1"
Nothing : Maybe Integer
*Data/String> parsePositive  "0"
Just 0 : Maybe Integer
*Data/String> parsePositive  "100"
Just 100 : Maybe Integer
*Data/String> parsePositive  "hoge"
Nothing : Maybe Integer
```

これを使ってユーザから数値を得るプログラムを書きます。
ところで、 `parsePositive` の返り値が `Just` か `Nothing` かで場合分けが必要ですよね。
`case` 式をみてみましょう。

## `case` による条件分岐
Idrisには `if` 式もありますが `case` をみてみます。
`case` は以下のような構文で使います

``` idris
case 条件 of
  パターン1 => 式1
  パターン2 => 式2
  ...
```

ここでもオフサイドルールが適用されます。
これを使ってひとまず「ユーザから得た入力が正の数値ならそれを、そうでなければ`-1`を返す」アクションを作ってみましょう。
これは `IO` のアクションで、返り値が整数なので型は `IO Integer` になります。
素朴に書くとこうなりそうですかね。

``` idris
import Data.String

-- これはコンパイルエラー
getInteger : IO Integer
getInteger = do
  putStr "guess a number: "
  n <- getLine
  case parsePositive n of
    Just n => n
    Nothing => -1

```

しかしこれはエラーです。

``` text
Entering directory `./src'
Type checking ./Main.idr
Main.idr:10:15:
   |
10 |     Just n => n
   |               ^
When checking right hand side of Main.case block in getInteger at Main.idr:9:9-22 with expected type
        IO Integer

Type mismatch between
        Integer (Type of n)
and
        IO Integer (Expected type)
```

`do` 記法で `IO` アクションを合成しようとしているのに `case` から整数値を返しているのでエラーになっているのです。
これは整数値の方を `IO` に合わせてあげると解決します。純粋な値から `IO` に合わせるには `pure` 関数を使います。

``` idris
getInteger : IO Integer
getInteger = do
  putStr "guess a number: "
  n <- getLine
  case parsePositive n of
    Just n => pure n
    Nothing => pure (-1)
```

`-` が減算演算子と解釈されないように括弧が必要ですが素直にこれで動きます。

さて、取得はできたのですが数値以外が入力されたら `-1` が返ってしまいます。
数値以外が入力されたらもう一度入力を促すようにしましょう。これにはループが必要です。

## ループ

Idrisは関数型言語なので制御構造のループはないです。関数をもう一度呼ぶとループになります。
`case` の `Nothing` の部分で `getInteger` を呼びましょう。

``` idris
getInteger : IO Integer
getInteger = do
  putStr "guess a number: "
  n <- getLine
  case parsePositive n of
    Just n => pure n
    Nothing => do
      putStrLn (n ++ " is not a number.")
      getInteger
```

厳密にいうと `getInteger` は引数を取らないので関数ではなく値なのですが、まあ細かいことはいいですよね。

# ゲーム

ここまできたら一気にゲームを書けそうです。
因みにユーザの入力値と秘密の値の比較結果には「小さい」、「合った」、「大きい」の3種類がありますね。そういう用途には `compare` 関数が便利です。2値を比較して `LT` 、 `EQ` 、 `GT` のいずれかを返してくれます

``` text
λΠ> compare 1 1
EQ : Ordering
λΠ> compare 1 2
LT : Ordering
λΠ> compare  2 1
GT : Ordering
```

これを使ってゲームはこう書けます。ただし秘密の数字は適当に渡ってくるものとします。

``` idris
game : Integer -> IO ()
game secret = do
  n <- getInteger
  case compare n secret of
    LT => do
      putStrLn "Too small"
      game secret
    EQ => putStrLn "You got it"
    GT => do
      putStrLn "Too big"
      game secret



-- 秘密の数字を適当に渡す
main : IO ()
main = game 42

```

これで秘密の数値が固定な以外はゲームが動くようになりました。
最後に秘密の数値を乱数にしましょう。これには一仕事必要です。

# ファイルIO

UNIX系システムでは `/dev/random` ファイルを読むと乱数が得られます。Windowsは私は詳しくないので適当に調べてください。ここから4バイトほど読み出して乱数を取得しましょう。
ファイルIOは `Prelude.File` に多少の操作が定義されています。

今回使うのは `openFile` 、 `fGetChars` 、 `closeFile` です。REPLでドキュメントをみてみましょう。
ドキュメントを読んでびっくりしたのですがバイト(列)を読み出すAPIがないみたいなので `fGetChars` で文字として読み出してから数値に変換します。

``` text
λΠ> :doc openFile
Prelude.File.openFile : (f : String) -> (m : Mode) -> IO (Either FileError File)
    Open a file
    Arguments:
        f : String  -- the filename
        
        m : Mode  -- the mode; either Read, WriteTruncate, Append, ReadWrite,
        ReadWriteTruncate, or ReadAppend
        
    The function is Total
λΠ> :doc Mode
Data type Prelude.File.Mode : Type
    Modes for opening files
    
Constructors:
    Read : Mode
        
        
    WriteTruncate : Mode
        
        
    Append : Mode
        
        
    ReadWrite : Mode
        
        
    ReadWriteTruncate : Mode
        
        
    ReadAppend : Mode
        
        
λΠ> :doc fGetChars
Prelude.File.fGetChars : (h : File) ->
    (len : Int) -> IO (Either FileError String)
    Read up to a number of characters from a file
    Arguments:
        h : File  -- a file handle which must be open for reading
        
    The function is Total
λΠ> :doc closeFile
Prelude.File.closeFile : File -> IO ()
    
    
    The function is Total
```


`openFile` の返り値に出てくる `File` というのがファイルハンドルを表すデータ型です。 `FileError` は名前の通りエラーですね。
`Mode` はドキュメントの通りファイルを開く時のモードです。今回は `Read` が必要なもののようです。

ところで地味にここで初めて2引数関数がでてきましたね。
`T -> U -> S` で概ね「`T` 型の値と `U` 型の値を受け取って `S` 型の返り値を返す関数」の意味です。本当は違います。気になる人は「カリー化」でググってみて下さい。役に立たない情報がいくらでもでてきます。
2引数の関数を呼び出すときは `openFile "/dev/random" Read` のようにスペースを空けて引数を並べます。これも本当は違います。同様にググってみて下さい。

上記以外で初めて出てきたのは `Either` でしようか。それについて軽く紹介します。

## `Either`

`Either` はパラメータを2つ取るデータ型です。 `Either E T` は Java風に書けばだいたい `Either<E, T>` になります。
この型は名前通りに「`E` 型の値、もしくは `T` 型の値」を表すデータ型です。
例えば `Integer` と `String` を混ぜて扱うことができるのです。以下の例を見てみましょう。

``` idris
tryParse : String -> Either String Integer
tryParse input =
  case parsePositive input of
    -- パースに失敗したら `String` 型の値。`Left` で `Either` にする
    Maybe => Left input
    -- パースに成功したら `Integer` 型の値。 `Right` で `Either` にする
    Just n => Right n
```

さて、この `Either` の使い道ですが、エラーの扱いでよくでてきます。
多くの関数は「失敗してエラー値を返す、または成功して正常値を返す」という挙動をするので `Either` がもってこいなのです。

元の `openFile` を見てみると `Either FileError File` とエラー値、または正常値を返していますね。例外ではないのでエラーも値として扱えます。
慣習的に左 (`Left`) がエラー値、 右 (`Right`) が正常値を表します。
一部のAPIも右だけ優遇されていたりします。

## 読み出す

`Either` について理解したのでひとまずファイルを開くところまでは一気に書けそうですね。
`openFile` の返り値が `Either` なので `case` で分岐してあげます。

``` idris
getRandom : IO (Either FileError Int)
getRandom = do
  efile <- openFile "/dev/random" Read
  case efile of
    Left e => pure (Left e)
    Right file => ?Unimplemented
```

因みに `Int` は固定精度整数、 `Integer` は任意精度整数です。

``` text
λΠ> :doc Int
Primitive type Int : Type
    Fixed-precision integers of undefined size
λΠ> :doc Integer
Primitive type Integer : Type
    Arbitrary-precision integers
```

流石に任意長の乱数を取得してると終わらないので固定長にします。あとで `Integer` に変換して使います。
さて、ここから処理が続きます。 `fGetChars` で読み出します。返り値の`Either`から同じく分岐で文字列を取り出します。
上の `?Unimplemented` の部分をこう書換えます。

``` idris
do
   echars <- fGetChars file 4
   case echars of
     Left e => pure (Left e)
     Right chars => ?Unimplemented
```

`Char` と `Int` のサイズはわからないのですが、 4文字くらい読んだらそれなりのサイズの乱数になるだろうという判断で4文字読み取ります。
文字列を取り出した後は普通の処理です。次に進みましょう。

## 文字列処理

文字列から数値に変換しましょう。
`unpack`, `ord` などで文字列から数値の列に変換します。
まずはREPLで挙動を試してみましょう。

``` text
λΠ> unpack "1234"
['1', '2', '3', '4'] : List Char
λΠ> map ord (unpack "1234")
[49, 50, 51, 52] : List Int
```

`map` が出てきましたがこれは多くの言語にも似たような関数があるので分かるかと思います。

次は数値の列をバイト列として結合して1つの数値にします。
Javaならだいたいこんな感じの処理になりますかね。

``` java
int acc = 0;
for (int i : intList) {
    acc = (acc << 8) + i;
}
return acc;
```

関数型言語ではループは関数で書くのでした。
ループを関数で書くとこういう感じになりますかね。


``` idris
-- `loop 0 intList` と呼ばれることを想定
loop : Int -> List Int -> Int
loop acc intList = case intList of
  [] => acc
  i :: intList => loop ((shiftL acc 8) + i) intList
```

しかしこういう処理はあまりに定形すぎます。そこで `foldl` という専用の関数が用意されています。
`loop 0 intList` 相当の処理はこう書けます。

``` idris
foldl (\acc, i => (shiftL acc 8) + i) 0 intList
```

`\引数1, .. ,引数n => 本体` はクロージャの構文です。

これをREPLで試してみましょう

``` text
λΠ> foldl (\acc, i => (shiftL acc 8) + i) 0 (map ord (unpack "1234"))
825373492 : Int
```

結果、`getRandom` はこうなります。

``` idris
getRandom : IO (Either FileError Int)
getRandom = do
  efile <- openFile "/dev/random" Read
  case efile of
    Left e => pure (Left e)
    Right file => do
      echars <- fGetChars file 4
      case echars of
        Left e => pure (Left e)
        Right chars => pure (Right (foldl (\acc, i => (shiftL acc 8) + i) 0 (map ord (unpack chars))))
```

## 結合

`main` で `getRandom` を使いましょう。
`getRandom` はエラーを返す可能性がありますが、その場合は無言で終了しましょう。

``` text
main : IO ()
main = do
  eint <- getRandom
  case eint of
    -- エラーが起きたらそのまま終了する
    Left _ => pure ()
    Right int => game (cast (1 + (mod int 100)))
```

1から100までの数値が欲しいので `(1 + (mod int 100))` と計算しています。
`cast` は型変換をする関数です。ここでは `Int` から `Integer` に変換します。
これで動かしてみましょう。

1回目

``` text
guess a number: 50
Too big
guess a number: 25
Too big
guess a number: 12
Too small
guess a number: 16
Too big
guess a number: 14
Too small
guess a number: 15
You got it
```

2回目

``` text
guess a number: 50
Too small
guess a number: 75
Too big
guess a number: 63
Too big
guess a number: 56
Too small
guess a number: 60
Too big
guess a number: 58
Too small
guess a number: 59
You got it
```

うまく動いているようです。
ゲームは完成しました。
しかし少しコードが不格好ですね。それに開いた `/dev/random` を閉じ忘れてます。
もう少しIdrisの機能に踏み込んでリファクタリングしましょう。

# リファクタリング
いくつかのIdrisの構文や機能を使いながらリファクタリングしていきます。

## `where`

関数の中で使う関数を定義できます。

``` idris
関数定義
where
  サブ関数定義
```

の構文です。
`getRandom` の中で文字列から数値に変換する部分を関数として抜き出しましょう。

``` idris
--  ...
        Right chars => pure (Right (stringToInt chars))
where
  stringToInt : String -> Int
  stringToInt chars = foldl (\acc, int => (shiftL acc 8) + int) 0 (map ord (unpack chars))

```

さらにファイルから数値を読み出す関数も用意しましょう。

``` idris
  fGetInt : File -> IO (Either FileError Int)
  fGetInt file = do
    echars <- fGetChars file 4
    case echars of
      Left e => pure (Left e)
      Right chars => pure (Right (stringToInt chars))

```

全体として `getRandom` はこうなります。

``` idris
getRandom : IO (Either FileError Int)
getRandom = do
  efile <- openFile "/dev/random" Read
  case efile of
    Left e => pure (Left e)
    Right file => fGetInt file
where
  stringToInt : String -> Int
  stringToInt chars = foldl (\acc, int => (shiftL acc 8) + int) 0 (map ord (unpack chars))

  fGetInt : File -> IO (Either FileError Int)
  fGetInt file = do
    echars <- fGetChars file 4
    case echars of
      Left e => pure (Left e)
      Right chars => pure (Right (stringToInt chars))

```

これは `where` の紹介のために使ったので別にサブ関数にせずに独立した関数でもよかったかもしれません。

## ガード

何箇所かで `case .. of Left e => pure (Left e)` と書かれていますね。これはもう少し楽に書けます。
`do` の中で `<-` を取り出すときに同時にパターンマッチできるのです。そしてマッチに失敗したら即座に返ります。返るときの値も自分で指定できます。
`fGetInt` をこう書き直しましょう。

``` idris
  fGetInt : File -> IO (Either FileError Int)
  fGetInt file = do
    -- Rightにマッチしたら値を取り出す。失敗したら(= Leftだったら) 別の値で即座に返る
    Right chars <- fGetChars file 4
      | Left e => pure (Left e)
    pure (Right (stringToInt chars))

```

ネストが減って楽になりました。

## `$`
`stringToInt` 、括弧が多くてカッコ悪いですよね。

``` idris
stringToInt chars = foldl (\acc, int => (shiftL acc 8) + int) 0 (map ord (unpack chars))
```

idrisには `$` という演算子があります。これは末尾まで続く括弧を省略できる演算子です。
以下の2つの式は等価です。

``` idris
map ord (unpack chars)
map ord $ unpack chars
```

括弧と違って「どこまで続くんだ」と考えながら読まなくていいので読みやすくなります。
ということで `stringToInt` はこう書けます。

``` idris
stringToInt chars = foldl (\acc, int => (shiftL acc 8) + int) 0 $ map ord $ unpack chars
```

同様に書換えられる箇所はいくつもあるでしょう。

## ファイルを閉じる

閉じ忘れていたファイルを閉じましょう。
ファイル操作は例外を投げなかったのでシンプルに「開く→処理する→閉じる」でよさそうです。
これは一般的な操作なので関数として定義してしまいましょう。
「処理する」の部分だけ変えられるように関数で受け取ることにして、 `withOpenFile` 関数をこう定義しましょう。

``` idris
withOpenFile : String -> Mode -> (File -> IO (Either FileError a)) -> IO (Either FileError a)
withOpenFile filename mode f = do
  Right file <- openFile filename mode
    | Left e => pure (Left e)
  ret <- f file
  closeFile file
  pure ret
```

説明がなくてもだいたい読めるようになったと思います。 `a` は型変数です。要するにジェネリクスですね。

さて、 `withOpenFile` を使って `getRandom` を書き直しましょう。


``` idris
getRandom : IO (Either FileError Int)
getRandom = withOpenFile "/dev/random" Read fGetInt
where
  stringToInt : String -> Int
  stringToInt chars = foldl (\acc, int => (shiftL acc 8) + int) 0 $ map ord $ unpack chars

  fGetInt : File -> IO (Either FileError Int)
  fGetInt file = do
    Right chars <- fGetChars file 4
      | Left e => pure (Left e)
    pure $ Right $ stringToInt chars

```

大分すっきりしましたね。

## 中置記法

ある程度好みの問題になりますが、関数を中置で書けます。
`main` に `mod int 100` という部分があったかと思います。
関数を `` ` `` で囲むとこれを中置で書けます。
以下は同等の表現です。

``` idris
mod int 100
int `mod` 100
```

同様に `stringToInt` 関数の `shiftL` も中置で書き直せます。

# 結びに

大体こんなところでしょうか。あんまり関数型言語っぽい部分に触れなかったので物足りない人もいるかもしれません。
今回のコードは[こちら](https://github.com/KeenS/idris-guessing-game)に置いておくので参考にして下さい。

繰り返しますが、私も全然詳しくないので間違ったことを書いてるかもしれません。なんか深夜テンションで書き始めてしまったので最後まで書ききっただけです。
詳しい方、誤りなどあれば教えて下さい。

# その他の記事

Idrisの特徴である依存型のチュートリアルは「こわくないIdris」シリーズなどがあります。
Idris 0.9.9時点での情報なので少し古いかもしれませんが。

* [こわくない Idris (1)](http://mandel59.hateblo.jp/entry/2013/09/02/184831)
* [こわくない Idris (2)](http://mandel59.hateblo.jp/entry/2013/09/03/162145)
* [こわくない Idris (3)](http://mandel59.hateblo.jp/entry/2013/09/06/104042)
* [こわくない Idris (4)](http://mandel59.hateblo.jp/entry/2013/09/14/082342)
* [こわくない Idris (5)](http://mandel59.hateblo.jp/entry/2013/09/14/115924)
* [こわくない Idris (6)](http://mandel59.hateblo.jp/entry/2013/09/14/153041)
* [こわくない Idris (6) 補足](http://mandel59.hateblo.jp/entry/2014/10/05/233126)
* [こわくない Idris (7)](http://mandel59.hateblo.jp/entry/2013/09/17/142709)

他は証明の紹介だとこういうのがあります。

[プログラミング言語 idris - wkwkesのやつ](http://wkwkes.hatenablog.com/entry/2016/12/17/000000)
