---
categories: [依存型, Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-16T02:40:32+09:00
title: "Idris手習い: ビッットマップ画像の書き出し"
---

このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris) の12日目の記事です。

κeenです。今回は手を動かすパートとしてビットマップ画像の書き出しをIdrisでやってみます。
依存型やIdrisの標準ライブラリ、ファイルの扱いなどの練習になればなと思ってます。

<!--more-->

# ビットマップ画像とは

ビットマップ画像（正確にはBMP、Windows bitmap）とはほぼ色のデータをそのまま持っていることで有名なシンプルな画像フォーマットですね。[Wikipedia](https://ja.wikipedia.org/wiki/Windows_bitmap)なんかに画像フォーマットが載っています。

フォーマットにはいくつか変種があるのですが、その中で一番簡単なフォーマットを書き出してみようと思います。
具体的には画像フォーマットにはいくつかパラメータがあるのですが、以下の設定に固定して書き出します。

* 1ピクセルあたりのビット数：24
  + こうすることでカラーパレットが不要になる
* 圧縮形式：0
  + 無圧縮

一番簡単なデータの場合はBMPは以下のような構造をしています。

```text
+-------------------+
| ファイルヘッダ     | (BITMAPFILEHEADER)
+-------------------+
| 情報ヘッダ        | (BITMAPINFOHEADER)
+-------------------+
| ビットマップデータ |
+-------------------+
```

特筆すべきはカラーパレットが不要になる点です。

それぞれ見ていきましょう。

## BITMAPFILEHEADER

| オフセット | サイズ  | 格納する情報   | 値・備考                                                                                           |
|------------|---------|----------------|----------------------------------------------------------------------------------------------------|
| 0x0000     | 2バイト | ファイルタイプ | 常にBM (0x42, 0x4d)（マジックナンバー）                                                            |
| 0x0002     | 4バイト | ファイルサイズ | ビットマップファイルのサイズを格納する（単位はバイト）。                                           |
| 0x0006     | 2バイト | 予約領域1      | 常に0                                                                                              |
| 0x0008     | 2バイト | 予約領域2      | 常に0                                                                                              |
| 0x000a     | 4バイト | オフセット     | ファイルヘッダの先頭アドレスからビットマップデータの先頭アドレスまでのオフセット（単位はバイト）。 |

引用元: [Windows bitmap - Wikipedia](https://ja.wikipedia.org/wiki/Windows_bitmap)


今回の形式だとカラーパレットがないのでオフセットが固定値になります。

## BITMAPINFOHEADER

| オフセット | サイズ  | 格納する情報              | 値・備考                                                               |
|-----------|---------|---------------------------|------------------------------------------------------------------------|
| 0x000e     | 4バイト | ヘッダサイズ              | 40                                                                     |
| 0x0012     | 4バイト | ビットマップの横幅        | 単位はピクセル                                                         |
| 0x0016     | 4バイト | ビットマップの縦幅        | 単位はピクセル。値が負の場合はトップダウン画像となる                   |
| 0x001a     | 2バイト | プレーン数                | 常に1                                                                  |
| 0x001c     | 2バイト | 1ピクセルあたりのビット数 | 0,1,4,8,16,24,32                                                       |
| 0x001e     | 4バイト | 圧縮形式                  | 0,1,2,3,4,5 ※1                                                        |
| 0x0022     | 4バイト | 画像データサイズ          | 単位はバイト                                                           |
| 0x0026     | 4バイト | 水平方向の解像度          | 単位はピクセル/m                                                       |
| 0x002a     | 4バイト | 垂直方向の解像度          | 単位はピクセル/m                                                       |
| 0x002e     | 4バイト | 使用する色数              | ビットマップで実際に使用するカラーパレット内のカラーインデックスの数。 |
| 0x0032     | 4バイト | 重要な色数                | ビットマップを表示するために必要なカラーインデックスの数。             |

引用元: [Windows bitmap - Wikipedia](https://ja.wikipedia.org/wiki/Windows_bitmap)

1ピクセルあたりのビット数は24bit、圧縮形式は0にするとしました。
水平方向の解像度と垂直方向の解像度はよくわからない（設定しても表示に変わりがない）ので0にします。
今回カラーパレットがないので使用する色数と重要な色数は0にできます。

すると残るパラメータはビットマップの横幅、ビットマップの縦幅、画像データサイズです。

## ビットマップデータ

1ピクセルあたり24bitならばBGR形式でデータを置いていきます。
ただし1行のデータが4の倍数でない場合は4の倍数になるように末尾に `0x00` を詰めてあげます。

例えば3x3の画像だとこんな感じですかね。

```text
 0123456789ab
+------------+
|BGRBGRBGR000|
+------------+
|BGRBGRBGR000|
+------------+
|BGRBGRBGR000|
+------------+
```

因みにBMPは座標系が数学と一緒で0行目が一番下、最終行が一番上です。
他のよくある画像フォーマットと天地が逆になってるので注意して下さい。


イメージ掴めましたか？

# やってみよう

ビットマップデータを受け取ってそれをBMP形式として書き出すプログラムを書いてみます。
ほとんどのパラメータは固定なので、ビットマップデータから以下の4つのパラメータさえ計算できればあとは簡単に書き出せます。

* ファイルサイズ
* ビットマップの横幅
* ビットマップの縦幅
* 画像データサイズ

## `Vect`

今回は依存型に触れてみるというテーマもあるので `Vect` 型を使います。
アドベントカレンダー初日でも紹介しましたが、`Vect` は長さ情報を型に持つリストです。
baseの [`Data.Vect`](https://www.idris-lang.org/docs/current/base_doc/docs/Data.Vect.html) に定義されています。

少しだけREPLで `Vect` を使ってみましょう。

```text
Idris> :module Data.Vect
*Data/Vect> the (Vect _ _) [1, 2, 3]
[1, 2, 3] : Vect 3 Integer
*Data/Vect> the (Vect _ _) [1, 2, 3, 4]
[1, 2, 3, 4] : Vect 4 Integer
*Data/Vect> the (Vect _ _) ([1, 2, 3, 4] ++ [1, 2, 3])
[1, 2, 3, 4, 1, 2, 3] : Vect 7 Integer
```

このようにちゃんと要素数と型の `Vect n Integer` の `n` の部分が対応しています。
しかも `(++)` で結合しても正しく要素数が保持されています。

これを使えば入力画像のデータを表現できそうです。


## 画像のデータ型

画像のデータ型 `BitMap` を定義しましょう。

…とその前に色を表わす型、 `Color` を定義しておきます。

``` idris
public export
Color : Type
Color = (Bits8, Bits8, Bits8)
```

復習するとIdrisでは型も値として扱えるので、グローバル変数の定義でもって型エイリアスが作れるのでした。
ここで登場する `Bits8` は（あんまりドキュメントに載っていない）Idrisのプリミティブで、8bitの数を表わします。符号があるかないかは分かりません。

可視性に `public export` がついています。
外部とのやりとりにも `Color` を使いたいので `export` が、中身が `(Bits8, Bits8, Bits8)` であることも公開したいので `public` がついています。

さて、色も定義し終わって `BitMap` レコードを定義します。

``` idris
import Data.Vect

export
record BitMap (x : Nat) (y : Nat)
where
  constructor MkBitMap
  imgData: Vect y (Vect x Color)

```

画像データは `Vect y (Vect x Color)` と表現しています。シンプルで分かりやすいですね。

レコードの定義部分が `record BitMap (x : Nat) (y : Nat)` と、パラメータ `x` 、 `y` が `(x : Nat)` の形になっています。レコードのパラメータは無言でパラメータを書くと型として扱われるのですが、今回は `Vect` に渡す数値をパラメータとしたいのでその宣言です。

画像データの定義に `Vect` を使っているので入力は正確に横幅 `x` 高さ `y` であることが保証できます。
リストのリストや可変長ベクトルの可変長ベクトルのようなデータ型だとサイズを正確に表現することができません。こういったところで正確になれるのは依存型の利点かなと思います。

`BitMap` の定義から直ちに `fromData` のような関数は書けますね。

``` idris
export
fromData : Vect y (Vect x Color) -> BitMap x y
fromData img = MkBitMap img
```

`MkBitMap` そのまま公開するよりもあとで他の操作を加える余地が残ります。
実際、BitMapの複雑なフォーマットに対応しようと思ったらカラーパレットの計算などの処理が挟まります。

因みに、Idrisは関数型言語なので関数は一級市民です。
上記の関数は仮引数を取らずに以下のようにも書けます。

``` idris
export
fromData : Vect y (Vect x Color) -> BitMap x y
fromData = MkBitMap
```

好きな方で定義してみて下さい。上級者ほど仮引数を取らない書き方を好むようです。

## パラメータの計算

復習すると、必要なパラメータは以下の4つでした。

* ファイルサイズ
* ビットマップの横幅
* ビットマップの縦幅
* 画像データサイズ


まずは簡単なビットマップの縦横幅を計算しましょう。

### Type reification

正確な用語は分からないのですが、Idrisでは型を実行時に取り出すことができます。これをひとまずtype reificationと呼ぶことにします。
どういうことかというと、普通の言語で上記 `BitMap x y` から `y` を取り出そうとすると以下のようなコードを書くことになるかと思います。

``` idris
getY : BitMap x y -> Int
getY b = cast $ length $ imgData b
```

`Vect` のデータの長さを計算していますね。これが普通です。

ですが、Idrisは `Vect y _` のようにデータの長さを型に持っています。
これを取り出せないでしょうか。結論を言うと取り出せます。

以下のようなコードで取り出せます。

``` idris
getY : BitMap x y -> Int
getY {y} _ = cast y
```

`getY` の仮引数の前に `{y}` を置くことで型にある `y` を取り出せるのです。
すごいですね。

この記法はいくつかの機能が混ざってるのでちょっとほぐしましょう。

まず、 `{y}` と書いてあるのは `{y = y}` の省略形です。 `{パラメータ名 = 変数名}` の構文です。
型にある変数 `y` を値にある変数 `length` に束縛するには以下のように書きます。

``` idris
getY : BitMap x y -> Int
getY {y = length} _ = cast length
```

さらに、 `{y = ...}` で取り出せるのは暗黙のパラメータ（implicit parameter）という機能のおかげです。
`BitMap x y` と無言で書くと `x` 、 `y` が自然数の型パラメータであることが宣言されるのが不思議に思った方はいませんか？これはコンパイラが裏でパラメータを追加しているからです。

暗黙のパラメータを省略せずに書くとこうなります。

``` idris
getY : {x: Nat} -> {y: Nat} -> BitMap x y -> Int
getY {y = length} _ = cast length
```

暗黙のパラメータは `{変数名: 型}` の構文で定義します。複数ある場合は上記のように1つづつ書くか、
型が同じなら `{変数名1, 変数名2...: 型}` とカンマで区切って書いてもよいです。

``` idris
getY : {x, y: Nat} -> BitMap x y -> Int
getY {y = length} _ = cast length
```

暗黙のパラメータは普段はコンパイラが勝手に埋めてくれるので気にしなくていいんですが、手で与えることもできます。

``` text
Idris> :t getY
getY : BitMap x y -> Int
Idris> :t getY {x = 1} {y = 1}
getY : BitMap 1 1 -> Int
```


さて、ちょっと色々情報を詰め込みすぎましたかね。
さしあたっては `getX` と `getY` は以下のように定義できるとだけ了解しておいて下さい。

``` idris
getX : {x: Nat} -> BitMap x y -> Int
getX {x} _ = cast x

getY : {y: Nat} -> BitMap x y -> Int
getY {y} _ = cast y
```

計算せずにサイズがとれる！ついでにいうと `getX` 行のサイズを計算するために1行目の要素を取り出すけど値がなかったら場合は…とかを考えなくてすみます。

Idris2からは暗黙のパラメータは `{...} ->` で明示的に書かないと取り出せないようになってるらしいので今のうちにその書き方をすることにします。


### ファイルサイズとデータサイズ

続けてファイルサイズとデータサイズを計算しましょう。こっちは地道に計算するだけです。

データサイズを計算しましょう。1ピクセル3バイトなので $x \times y \times 3$ かと思いきや、1行を4バイトの倍数に切り上げるという仕様なのでちょっとだけ計算が必要です。

まず、切り上げるときのパディングサイズの計算はこうできますね。

``` idris
padSize: Nat -> Int
padSize x = (4 - ((cast $ x * 3) `mod` 4)) `mod` 4
```

`` `mod` 4`` が2回挟まってますが、2回目の `` `mod` 4`` は4を0にするための計算です。

これを使って `BitMap x y` からデータサイズを計算する関数は以下のように書けます。

``` idris
dataSize : {x, y: Nat} -> BitMap x y-> Int
dataSize {x} {y} _ =
  let rowSize = (cast $ x * 3) + (padSize x) in
  rowSize * (cast y)
```

ここでもtype reification（？）を使ってます。

ヘッダサイズが54でそれ以外にはビットマップデータしかないのでファイルサイズは簡単に計算できます。

``` idris
offset : Int
offset = 14 + 40

fileSize : BitMap x y -> Int
fileSize bitmap = offset + (dataSize bitmap)
```

これで必要なパラメータが揃ったのでデータを書き出していきます。


## ファイルとバッファ

これからBMPフォーマットのファイルを作ります。
最終的にはファイルに書き出すんですが、プログラム内では一旦バッファに書き出して、それをあとでまとめてファイルに書き出しましょう。
その方がファイル関連のエラーを1まとめにできますからね。（あとIdrisのファイルAPIに欠陥があってバイトを書き出す手段がないというのもあります。）

ファイルに書き出せるバッファは [`Data.Buffer`](https://www.idris-lang.org/docs/current/base_doc/docs/Data.Buffer.html) に定義されているのでインポートします。


``` idris
import Data.Buffer
```

`import` 文はファイルの先頭の方で書かないといけないので注意して下さいね。

さて、 `Buffer` のAPIなんですが、主に使うのは `setByte : Buffer -> (loc : Int) -> (val : Bits8) -> IO ()` と `setInt : Buffer -> (loc : Int) -> (val : Int) -> IO ()` です。
これらは書き込む場所を引数にとりますね。
我々の用途では1バイトずつずらしながら書き込んでいくので `loc` を持って回らないといけません。
それはちょっと面倒なので `Buffer` と `loc` をまとめたデータ型を定義しておきましょう。

``` idris
data Output = MkOutput Buffer Int
```

もうちょっとユーティリティ関数を定義します。

まずは `Output` に `Int` を書き込んでくれる関数 `writeInt` です。

```idris
||| Write int in little endian
writeInt : Output -> Int -> IO Output
writeInt (MkOutput buffer loc) int = do
  setInt buffer loc int
  pure $ MkOutput buffer (loc + 4)
```


返り値に `loc` が更新された `Output` を返します。
`|||` ではじまる行はドキュメントコメントです。いつか解説します。
ドキュメントでは明示されてませんが実装を読む限り `setInt` はリトルエンディアンで4バイト書くAPIのようです。
今回の目的に適ってるのでそれを使います。


複数のバイトを受け取って書き出す `writeBytes` も定義しましょう。

``` idris
writeBytes : Output -> List Bits8 -> IO Output
writeBytes o []            = pure o
writeBytes (MkOutput buffer loc) (b::bs) = do
  setByte buffer loc b
  writeBytes (MkOutput buffer (loc + 1)) bs
```

これで準備OKです。

## イメージフォーマットの書き出し

復習するとBMPフォーマットは以下の形をしているのでした。

```text
+-------------------+
| ファイルヘッダ     | (BITMAPFILEHEADER)
+-------------------+
| 情報ヘッダ        | (BITMAPINFOHEADER)
+-------------------+
| ビットマップデータ |
+-------------------+
```

これに対応して、画像フォーマットを書き出すAPIは以下のような見た目になります。


``` idris
export
writeBitMap : File -> BitMap x y -> IO ()
writeBitMap f bitmap = do
  -- ...
  let o = MkOutput buf 0
  o <- writeHeader o bitmap
  o <- writeInfoHeader o bitmap
  o <- writeImgData o bitmap
  -- ...
```

前後にバッファを用意する作業やファイルに書き出す作業が挟まりますがイメージは掴めるでしょう。
こうやって使うことを想定して、 `writeHeader` 、 `writeInfoHeader` 、 `writeImgData` を定義します。

### writeHeader

`writeHeader` はパラメータはファイルサイズしかなかったので直線的なコードになります。


``` idris
writeHeader : Output -> BitMap x y -> IO Output
writeHeader o bitmap = do
  -- file type
  o <- writeBytes o [0x42, 0x4d]
  -- file size
  o <- writeInt o (fileSize bitmap)
  -- reserved 1
  o <- writeBytes o [0x00, 0x00]
  -- reserved 2
  o <- writeBytes o [0x00, 0x00]
  -- offset
  o <- writeInt o offset
  pure o
```

`offset` は「ファイルサイズとデータサイズ」のところで定義した変数です。

### writeInfoHeader

`writeInfoHeader` も `writeHeader` に続いて直線的なコードです。
パラメータが横幅、高さ、データサイズだけです。

``` idris
writeInfoHeader : Output -> BitMap x y -> IO Output
writeInfoHeader o bitmap = do
  -- header size
  o <- writeInt o 40 -- windows format
  -- width
  o <- writeInt o (getX bitmap)
  --  height
  o <- writeInt o (getY bitmap)
  --  # of plane
  o <- writeBytes o [0x01, 0x00]
  --  # of bits of the colors
  o <- writeBytes o [24, 0]
  -- compression format
  o <- writeBytes o [0x00, 0x00, 0x00, 0x00] -- no compression
  -- image data size
  o <- writeInt o (dataSize bitmap)
  -- TODO: horizontal resolution (px/m)
  o <- writeInt o 0
  -- TODO: vertical resolution (px/m)
  o <- writeInt o 0
  -- # of colors in the pallet
  o <- writeInt o 0
  -- # of important colors
  o <- writeInt o 0
  pure o
```

### writeImgData

`writeImgData` だけはちょびっとだけ複雑です。
画像を書き出す処理は2重ループになるからです。
とはいえ、ループを分けて書けばそんなに難しくありません。

1行を（パディングをせずに）書き出す処理はこう書けます。


``` idris
writeRow : Output -> Vect m Color -> IO Output
writeRow o []      = pure o
writeRow o ((r, g, b)::ds) = do
  o <- writeBytes o [b, g, r]
  writeRow o ds
```

BMPが画像をRGBではなくBGRで期待することを思い出して下さい。

`writeRow` を使って全ての画像データを書き出す処理はこう書けます。


``` idris
writeImgData : Output -> BitMap x y -> IO Output
writeImgData o bitmap = loop o $ imgData bitmap
where
  loop : {m: Nat} -> Output -> Vect n (Vect m Color) -> IO Output
  loop {m} o []      = pure o
  loop {m} o (r::rs) = do
    o <- writeRow o r
    o <- writeBytes o (replicate (cast (padSize m)) 0x00)
    loop o rs
```

普通にループを回しているだけですね。
1行書き出したあとでパディングをする処理が入っている程度です。

ここで使った `replicate: Nat -> a -> List a` は [`Prelude.List`](https://www.idris-lang.org/docs/current/base_doc/docs/Prelude.List.html)で定義されている関数で、`x` が `n` 個あるリストを作ります。

``` text
Idris> :t replicate
replicate : Nat -> a -> List a
Idris> :doc replicate
Prelude.List.replicate : (n : Nat) -> (x : a) -> List a
    Construct a list with n copies of x
    Arguments:
        n : Nat  -- how many copies

        x : a  -- the element to replicate

    The function is: Total & public export
Idris> replicate 3 "hoge"
["hoge", "hoge", "hoge"] : List String
```



### writeBitMap

必要な役者が揃ったので `writeBitMap` を作ってみましょう。

一気に全体を載せてしまうとこうなります。

``` idris
export
writeBitMap : File -> BitMap x y -> IO ()
writeBitMap f bitmap = do
  Just buf <- newBuffer (fileSize bitmap)
    | Nothing => putStrLn "memory allocation failed"
  let o = MkOutput buf 0
  o <- writeHeader o bitmap
  o <- writeInfoHeader o bitmap
  o <- writeImgData o bitmap
  let MkOutput buffer _ = o
  buffer <- writeBufferToFile f buffer (fileSize bitmap)
  pure ()
```


`Buffer` は `newBuffer` で作成します。そしてファイルに書き出すのは `writeBufferToFile` を使います。
ここまで読んできた方には解説は不要でしょう。

# 使ってみよう

先程のコードを `EasyBitMap.idr` に保存していたとします。
`EasyBitMapMain.idr` を作成して呼び出してみましょう。

``` idris
module Main

import Data.Vect
import EasyBitMap
```

ひとまず必要なものをインポートします。

画像データは手書きします。

``` idris
bitMapData : Vect 16 (Vect 16 Color)
bitMapData = [
    [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w],
    [w, b, b, b, b, b, b, b, b, b, b, b, b, b, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, w, w, w, w, w, w, w, w, w, w, w, w, b, w],
    [w, b, b, b, b, b, b, b, b, b, b, b, b, b, b, w],
    [w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w]
  ]
where
  w : Color
  w = (255, 255, 255)
  b : Color
  b = (0, 0, 0)
```

今回のデータは上下対称ですが、BMPフォーマットでは天地逆さになるので他の画像を試す方は注意して下さい。

さて、データはこれを使うとして、 `main` はこう書きます。

``` idris
main : IO ()
main = do
  [_, file] <- getArgs
    | _ => putStrLn "Usage: FILENAME"
  Right file <- openFile file WriteTruncate
    | Left => putStrLn ("failed to open file: " ++ file)
  writeBitMap file (fromData bitMapData)
  fflush file
  closeFile file
```

`openFile` 、 `WriteTruncate` 、 `fflush` 、 `closeFile` などは [`Prelude.File`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.File.html)で定義されたAPIです。
`getArgs` は [`Prelude.Interactive`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Interactive.html)で定義されています。

それではこれをコンパイル・実行してみましょう。

``` text
$ idris -o EasyBitMapMain EasyBitMapMain.idr
$ ./EasyBitMapMain test.bmp
```

私は `test.bmp` にBMP形式のデータが書き出されました。
適当に開いてみて下さい。Linuxなら `eog test.bmp` とかで開けます。macOSだと `open test.bmp` かな？あるいはファイラーでディレクトリを開いてダブルクリックしてみて下さい。
こんな感じの画像が見れるはずです。

<img src="data:image/bmp;base64,Qk02AwAAAAAAADYAAAAoAAAAEAAAABAAAAABABgAAAAAAAADAAAAAAAAAAAAAAAAAAAAAAAA////////////////////////////////////////////////////////////////////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAA////////////////////////////////////////////////AAAA////////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA////////////////////////////////////////////////////////////////////">

サイズが小さいので豆粒みたいですが、白地に黒い正方形が描かれています。これが表示されたら成功です。

# まとめ

IdrisでBMP画像を書き出してみました。
その過程で依存型やType Reification、BufferやFileなどの操作を学びました。

実はColor Bitsを24に限定すると実装を簡略化できるというのに気付かず、一度任意のColor Bitsに対応したプログラムも作っています。そちらは今回のものよりも踏み込んだ依存型の使い方をしているのでいつか紹介できたらなと思ってます。

# 参考文献

今回の記事は一度以下の記事を参考にBMPフォーマットを書き出すプログラムを書いてみたのがベースになっています。

[【バイナリファイル入門】Bitmapファイルを手書きで作って遊んでみる - Qiita](https://qiita.com/chooyan_eng/items/151e67684e5ef8d1a695)

詳しいフォーマットは記事中でも触れましたがWikipediaを参考にしました。

[Windows bitmap - Wikipedia](https://ja.wikipedia.org/wiki/Windows_bitmap)

# 付録A: 今回のコード

<script src="https://gitlab.com/-/snippets/2050741.js"></script>


# 付録B: 生成されるBMP画像のバイナリデータ

デバッグ用途にどうぞ

``` text
00000000: 424d 3603 0000 0000 0000 3600 0000 2800  BM6.......6...(.
00000010: 0000 1000 0000 1000 0000 0100 1800 0000  ................
00000020: 0000 0003 0000 0000 0000 0000 0000 0000  ................
00000030: 0000 0000 0000 ffff ffff ffff ffff ffff  ................
00000040: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000050: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000060: ffff ffff ffff ffff ff00 0000 0000 0000  ................
00000070: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000080: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000090: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
000000a0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000000b0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000000c0: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
000000d0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000000e0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000000f0: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
00000100: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000110: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000120: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
00000130: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000140: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000150: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
00000160: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000170: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000180: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
00000190: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000001a0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000001b0: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
000001c0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000001d0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000001e0: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
000001f0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000200: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000210: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
00000220: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000230: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000240: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
00000250: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000260: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000270: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
00000280: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000290: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000002a0: 0000 00ff ffff ffff ff00 0000 ffff ffff  ................
000002b0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000002c0: ffff ffff ffff ffff ffff ffff ffff ffff  ................
000002d0: 0000 00ff ffff ffff ff00 0000 0000 0000  ................
000002e0: 0000 0000 0000 0000 0000 0000 0000 0000  ................
000002f0: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000300: 0000 00ff ffff ffff ffff ffff ffff ffff  ................
00000310: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000320: ffff ffff ffff ffff ffff ffff ffff ffff  ................
00000330: ffff ffff ffff                           ......
```

