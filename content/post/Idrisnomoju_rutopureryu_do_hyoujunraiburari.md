---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-15T06:03:03+09:00
title: "Idrisのモジュールとプレリュード、標準ライブラリ"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の11日目の記事です。

κeenです。今回はIdrisの複数のファイルだとか標準ライブラリだとかを扱おうと思います。

<!--more-->

# モジュール

ファイルの先頭で `module モジュール名` と宣言するとモジュールが宣言されます。

例：モジュール `Hoge` を宣言するコード

```idris
module Hoge

-- ...
```

モジュールからアテムをエクスポートするには可視性の修飾子をつけます。
可視性の修飾子（export modifiers）は以下の3つがあります。

* `private`: エクスポートしない
* `export`: 型をエクスポートする
* `public export`: 定義の詳細をエクスポートする

関数なんかは `export` 、 インタフェースやデータ型なんかは `public export` が基本の選択肢になるでしょう。

他のファイルをインポートするには `import ファイル名` を使います。

例：他のファイルをインポートするコード

```idris
import Hoge
```


すると `Hoge.idr` でエクスポートされたアイテムが全て見えるようになります。

少し試してみましょう。

`Hoge.idr` に以下の内容を書きます。

例： `Hoge.idr` に書く内容

```idris
module Hoge

private
privateHoge : String
privateHoge = "private"


export
exportHoge : String
exportHoge = "export"


export
data exportData = MkExportData String


public export
data publicData = MkPublicData String
```

モジュール名とファイル名は独立ですが、一致させることが推奨されています。確かIdris2からは一致が強制だったはず。

次に `HogeMain.idr` という名前のファイルに以下の内容を書きます。

例： `HogeMain.idr` に書く内容

```idris
module Main

import Hoge

main : IO ()
main = do
  putStrLn privateHoge
  putStrLn exportHoge


ex: ExportData
ex = MkExportData "export"

pub: PublicData
pub = MkPublicData "public"

```

これをコンパイルしてみましょう。

例： `HogeMain.idr` をコンパイルするコマンド

```text
$ idris -o HogeMain HogeMain.idr
HogeMain.idr:6:8-8:21:
  |
6 | main = do
  |        ~~ ...
When checking right hand side of main with expected type
        IO ()

When checking an application of function Prelude.Interactive.putStrLn:
        No such variable privateHoge

HogeMain.idr:12:6-26:
   |
12 | ex = MkExportData "export"
   |      ~~~~~~~~~~~~~~~~~~~~~
When checking right hand side of ex with expected type
        ExportData

No such variable MkExportData
```

2箇所エラーが出ました。1つ目はここ。

例：エラーになるコード（1箇所目）

``` idris
  putStrLn privateHoge
```

`Hoge` では `private` で宣言したのでインポートできないんですね。

2つ目はここ。

例：エラーになるコード（2箇所目）

``` idris
ex: ExportData
ex = MkExportData "export"
```

この `MkExportData` の方です。
`ExportData` は `export` 宣言されているので型はエクスポートされていますが、 `public export` でないので定義、すなわちコンストラクタがエクスポートされていません。
こういう細かい制御ができるんですね。

# モジュールとパス

さっきのコンパイルコマンド、気付いたことありませんか？再掲すると以下です。

例：HogeMain.idrをコンパイルするコマンド（再掲）

``` text
$ idris -o HogeMain HogeMain.idr
```

`Hoge.idr` が登場してませんね。
`HogeMain.idr` をコンパイルするときにどこから `Hoge.idr` を探す情報を得ているかというと、 `import Hoge` です。

びっくりなことに、 `import` 文はインポートする **ファイル名** を指定する構文なのです。
`import ディレクトリ名.ディレクトリ名.ファイル名` とパスを指定する構文になっています。
ファイル内に書かれている `module` 文は一切関係ありません。

実験してみましょう。 `Hoge/Fuga/Piyo.idr` というファイルに `Foo` というモジュールを作ってみます。

例： `Hoge/Fuga/Piyo.idr` を作成するコマンド

``` text
$ mkdir -p Hoge/Fuga
$ cat > Hoge/Fuga/Piyo.idr <<EOF
module Foo
export
foo : String
foo = "foo"
EOF
```

これを利用する `Bar.idr` というファイルを作ってみて、 `Foo` ではなく `Hoge.Fuga.Piyo` をインポートします。

例： `Bar.idr` を作成するコマンド

``` text
$ cat > Bar.idr <<EOF
module Main
import Hoge.Fuga.Piyo

main : IO ()
main = putStrLn foo
```

コンパイル/実行してみましょう。

例： `Bar.idr` をコンパイルするコマンド

``` text
$ idris -o Bar Bar.idr
$ ./Bar
foo
```

動いてますね。

では `module` は何に使うかというと、名前空間


# モジュールと名前空間

インポートしたアイテムを `foo` のようにそのまま使っていましたが、名前空間を修飾した形でも使えます。 `名前空間.アイテム名` の構文です。

先程の例だと `Hoge.Fuga.Piyo` のモジュール名は `Foo` なので `foo` の代わりに `Foo.foo` と書くこともできます。

例： `foo` の修飾名を使うコード

``` idris
main : IO ()
main = putStrLn Foo.foo
```


モジュール名は `.` で区切ることができるんですが、これで階層構造を作っているようです。
例えば `module Hoge.Fuga.Piyo` に `foo` が定義されているとします。
この場合 `foo` 、 `Piyo.foo` 、 `Fuga.Piyo.foo` 、 `Hoge.Fuga.Piyo.foo` が有効な修飾名です。

これも試してみましょう。
先程の `Hoge/Fuga/Piyo.idr` を以下のように変更します。

例： `Hoge/Fuga/Piyo.idr` に書くコード

``` idris
module Hoge.Fuga.Piyo
export
foo : String
foo = "foo"
```

そして `Bar.idr` には以下のコードを書きます。

例： `Bar.idr` に書くコード

``` idris
module Main
import Hoge.Fuga.Piyo

main : IO ()
main = do
  putStrLn foo
  putStrLn Piyo.foo
  putStrLn Fuga.Piyo.foo
  putStrLn Hoge.Fuga.Piyo.foo
```

これをコンパイルしてみると、正常に動くことが分かります。

``` text
$ idris -o Bar Bar.idr
```

## 名前空間とmain

ところで、さっきから `main : IO ()` を書くモジュールに全て `Main` とつけていたのに気付きましたか？Idrisは `Main.main` を実行しようとするのです。なので `main` を置くモジュールは `Main` と名付ける必要があります。
今までモジュール宣言がなくても動いたのは、宣言がなかったら自動で `Main` として扱ってくれるからです。

## 再エクスポート

インポートしたアイテムを再度エクスポートしたい場合は `import pubil パス` の構文を使います。

## デフォルト可視性

可視性修飾子をつけなければデフォルトでは `private` です。ですが、ファイル内でこのデフォルトを変更できます。

Idrisにはディレクティブというのがあるんですが、ひとまずその事実を受け入れて下さい。
そして可視性を変更するには `access` ディレクティブを使って `%access 可視性` と書きます。
すると以降のアイテムにはデフォルトでその可視性が適用されます。
例えば以下のように書きます。

例： `access` ディレクティブを使った可視性の制御

``` idris
privateItem : String
privateItem = "private"

%access export

exportItem1 : String
exportItem1 = "export"

exportItem2 : String
exportItem2 = "export"
```

# namespaceとオーバーロード

Idrisの名前空間の特徴の1つとして名前空間さえ異なれば同じ名前のアイテムを定義できるという点にあります。すなわちオーバーロードできるんですね。

これをもう一歩すすめると、オーバーロードのために名前空間を作りたくなります。
そのための機能が `namespace` です。
`namespace 名前 本体` の構文で使います。

例えば以下のように書きます。

例： `namespace` 文を使ったオーバーロード

``` idris
module Namespace


namespace Int
  add : Int -> Int -> Int
  add x y = x + y

namespace Double
  add : Double -> Double -> Double
  add x y = x + y
```


REPLにロードして少し遊んでみましょう。

``` text
Idris> add 1 2
Can't disambiguate name: Namespace.Double.add, Namespace.Int.add
Idris> add 1.0 2.0
3.0 : Double
Idris> Int.add  1 2
3 : Int
Idris> Double.add  1 2
3.0 : Double
Idris> :t add
Namespace.Double.add : Double -> Double -> Double
Namespace.Int.add : Int -> Int -> Int
Idris> add
Can't disambiguate name: Namespace.Double.add, Namespace.Int.add
```

オーバーロードは型で判別できれば勝手にコンパイラが解決してくれるのですが、 `add 1 2` のように `Int` にも `Double` にもとれるものだとエラーになります。

## Duplicate Record Fields

名前空間のおかげで複数のレコードでラベルが被っても問題なく使えます。

例えばラベルがモロ被りなレコード `User` と `Group` を定義したとしましょう。

例

``` idris
record User where
  constructor MkUser
  id : Integer
  name: String

record Group where
  constructor MkGroup
  id : Integer
  name: String
```


これらのアクセサ関数はそれぞれ `namespace User` と `namespace Group` に定義されるのでした

``` text
Idris> :browse User
Namespaces:

Names:
  id : User -> Integer
  name : User -> String
  set_id : Integer -> User -> User
  set_name : String -> User -> User
Idris> :browse Group
Namespaces:

Names:
  id : Group -> Integer
  name : Group -> String
  set_id : Integer -> Group -> Group
  set_name : String -> Group -> Group
```

このおかげで `id` や `name` はオーバーロードされて、両方が同時に使えます。

``` text
Idris> name (MkUser 1 "user")
"user" : String
Idris> name (MkGroup 1 "group")
"group" : String
```

# preludeとbase

ようやくこのときがやってきました。プレリュードと標準ライブラリについて説明します。

preludeとはコンパイラに付属してついてくるライブラリのうち、インポート状態のものです。
`import` 文を書かなくても使えます。

baseはコンパイラに付属してついてくるライブラリのうち、インポートしないと使えないものです。

普通の言語ではプレリュード相当のものは小さいか、クラスなどに小分けにされているんですがIdrisでは結構色々なものがプレリュードに入っています。
baseはデータ型などが入っています。

preludeとbaseに何が定義されているかは以下から見れます。

* [prelude](https://www.idris-lang.org/docs/current/prelude_doc/)
* [base](https://www.idris-lang.org/docs/current/base_doc/)


これら2つにはお世話になることでしょう。ブックマークしておいて下さい。

## baseを使ってみる

折角なのでbaseを使ってみましょう。

[`Data.Comple`](https://www.idris-lang.org/docs/current/base_doc/docs/Data.Complex.html) あたりが手頃ですかね。
`BaseExample.idr` に以下のコードを書きます。

例：複素数ライブラリを使ったコード

``` idris
module Main

import Data.Complex

main : IO ()
main = do
  -- Complexのコンストラクタ :+
  let cpx = 3 :+ 2
  printLn cpx
  -- 共役
  let cnj = conjugate cpx
  -- 共役な複素数の積
  printLn $ cpx * cnj
```

これをコンパイル/実行するとこうなります。

例：複素数ライブラリを使ったコードをコンパイル/実行するコマンド

``` text
$ idris -o BaseExample BaseExample.idr
$ ./BaseExample
3 :+ 2
13 :+ 0
```

使えているようですね。

preludeには今までさんざん使ってきた [`putStrLn`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Interactive.html)や [`+`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Interfaces.html)、baseには初日で紹介した [`Vect`](https://www.idris-lang.org/docs/current/base_doc/docs/Data.Vect.html)などが入っています。

# まとめ

Idrisのモジュールとインポート、エクスポート、名前空間、オーバーロード、ライブラリなどを紹介しました。
