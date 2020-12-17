---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-17T06:45:33+09:00
title: "Idrisのパッケージとipkg"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の14日目の記事です。

κeenです。今回はIdrisのパッケージ機能とipkgについて説明します。

<!--more-->

`idris` コマンドは単体でかなり高機能で、パッケージシステムも内包します。

標準ライブラリにpreludeとbaseがあるのは既に説明したとおりですが、他にもcontrib、effects、pruvilojというパッケージも添付されています。これらはパッケージシステムを使ってリンクしないと使えないようになっています。

これらの中でも[contrib](https://www.idris-lang.org/docs/current/contrib_doc/)は重要です。
Idrisは標準ライブラリへの貢献に対してかなり保守的な態度を取っており、大抵のライブラリ強化の提案はcontribへとマージされることになっています。結果としてcontribがないとライブラリが貧弱な言語になってしまいます。

# contribを使う：アナグラム


まずはパッケージを使ってみましょう。[アナグラム](https://ja.wikipedia.org/wiki/アナグラム)を検知するプログラムです。例えば"eat"と"ate"は文字を並べ替えると互いに変換できるのでアナグラムの関係にあります。

実装方針としてはアナグラムの正規形、文字をアルファベット順に整列したものをキーとしてアナグラムの集合をバリューにもつマップを作ればよさそうです。上の例だと `"aet" -> ["eat", "ate"]` の関係を保持します。

キーバリューペアはcontribの [`Data.SortedMap`](https://www.idris-lang.org/docs/current/contrib_doc/docs/Data.SortedMap.html) にあり、集合は [`Data.SortedSet`](https://www.idris-lang.org/docs/current/contrib_doc/docs/Data.SortedSet.html)にあります。早速使っていきましょう。

```idris
module Anagram

import Data.SortedMap
import Data.SortedSet
```

まずは `SortedMap` をインポートしました。

## 最初の値

空のDBも定義しておきましょう。

```idris
export
AnagramDB : Type
AnagramDB = SortedMap String (SortedSet String)

export
emptyDB : AnagramDB
emptyDB = empty
```

## 単語の登録

新しい値を登録する処理 `register` は、まずは登録する単語の正規形を計算して、それをキーにDBにエントがあればリストにデータを加えなければ新たにデータを登録すればよさそうです。

まずは正規形を計算する関数を。

```idris
normalize : String -> String
normalize = pack . sort . unpack

```

`normalize` は少し説明が必要でしょうか。ここで使っている `.` は関数の合成です。
そして `unpack: String -> List Char` と `pack: List Char -> String` はそれぞれ `String` <-> `List Char` の変換を担当します。

合成の様子を順番に見ると、以下のような動きをします。

```text
Idris> unpack "eat"
['e', 'a', 't'] : List Char
Idris> (sort . unpack) "eat"
['a', 'e', 't'] : List Char
Idris> (pack . sort . unpack) "eat"
"aet" : String
```

あとの `register` は簡単に書けます。

```idris
export
register: AnagramDB -> String -> AnagramDB
register db word =
  let key = normalize word in
  case lookup key db of
    Just set => insert key (insert word set) db
    Nothing   => insert key (insert word empty) db
```

`insert word set` と `insert word empty` をみてどうにか頑張れば1つにまとめられるのではと気付いた方、勘がいいです。 `SortedMap` と `union` 演算は（自由）半群なので多少楽に書けるAPIもありますが、今回はパッケージ以外はあんまり変なことをしない方針なので一旦スルーします。興味があればドキュメントを読んで別の実装を与えてみて下さい。

## 単語のクエリ

ある単語のアナグラムを検索する関数 `query` も作っておきましょう。
1つ注意しないといけないのが、その単語は自身のアナグラムなので必ずアナグラムは1つ以上あるということです。
さらに、その単語自体は登録されてなくてもアナグラムが登録されていれば返す値に検索ワードもれないといけない点です。
それに注意して実装すると以下のようになります。


```idris
export
query : AnagramDB -> String -> SortedSet String
query db word =
  let key = normalize word in
  case lookup key db of
    Just set => insert word set
    Nothing   => insert word empty
```


## パッケージのリンク

一旦REPLで様子を見たいんですが、REPLでcontribを使えるようにしないといけませんね。
`-p パッケージ名` でパッケージをロードした状態でREPLをはじめられます。

```text
$ idris -p contrib Anagram.idr
...
Anagram*> 
```

型チェックまで通ったら試してみましょう。

```text
*Anagram> :let db = register (register emptyDB "eat") "ate"
*Anagram> query db "tea"
SetWrapper (M 1 (Branch3 (Leaf "ate" ()) "ate" (Leaf "eat" ()) "eat" (Leaf "tea" ()))) : SortedSet String
```

ちょっとみづらいですが  "ate" 、 "eat" 、 "tea" が含まれているのでよさそうです。

## ファイルからの読み込み

さっきまでの内容はライブラリとして、 `Main` を作っていきましょう

```idris
module Main

import Anagram
```


さて、単語が沢山書かれているファイルから読み込んで、DBに登録してみましょう。
UNIX系OSを使っているなら `/usr/share/dict/words` というファイルがあるはずです。ここに10万くらいの単語が入っています。


```text
$ wc -l /usr/share/dict/words
102774 /usr/share/dict/words
```

1/5000くらいランダムサンプリングしてみましょう。

```text
$ cat  /usr/share/dict/words | awk 'int(rand()*5000) == 1 { print $0 }'
Chengdu's
Edinburgh
Poitier
aforementioned
ambulatory
busbies
confetti
courier's
cowslip
decorous
demurred
deviants
dotcom's
fabricates
freebasing
glorification
libellers
male
neocolonialism
plasticity's
reduces
sough
temptation's
wrapping's
```

このように本当に単語が改行で並べられているだけのファイルです。
ここからアナグラムDBを作りましょう。
`/usr/share/dict/words` がなかった方は適当にファイルを用意して下さい。


もうそろそろ解説がなくても読めるようになった頃ですかね。


```idris
importFromFile : (filename: String) -> IO (Either FileError AnagramDB)
importFromFile filename = do
  Right file <- openFile filename Read
    | Left e => pure (Left e)
  loop file emptyDB
where
  loop : File -> AnagramDB -> IO (Either FileError AnagramDB)
  loop file db = do
    isEOF <- fEOF file
    if isEOF
    then pure $ Right db
    else do
      Right word <- fGetLine file
        | Left e => pure (Left e)
      let db = register db (trim word)
      loop file db
```

これも一旦REPLにロードして実行してみましょう。
`-p contrib` を忘れずに。

ここで `/usr/share/dict/words` をロードしたいところですが問題があります。
今まで説明してませんでしたがREPLだとコンパイラを通さずインタプリタで実行するので遅いです。
そのREPLで10万語を読み込むのには不安があります。

ということで一旦100語くらいの辞書を作りましょう。

```sh
$ cat  /usr/share/dict/words | awk 'int(rand()*1000) == 1 { print $0 }' > smalldict.txt
```

これくらいだったらブログにも載るので付録に置いておきます。

では、これを実行してみましょう。

```text
*AnagramMain> :x (importFromFile "smalldict.txt")
Can't convert handles back to TT after execution.
```

成功したか分かりづらいですがエラーは出てないので大丈夫でしょう。

## 表示、実行

そろそろパッケージから離れてきたのでサクっといきましょう。

`SortedSet` はそのままだと表示できないので加工する必要があります。 `Data.SortedSet` をいインポートしておきましょう。

```idris
import Data.SortedSet
```

表示、 `main` は以下です。

```idris
showResult: AnagramDB -> String -> IO ()
showResult db word =
  let anagrams = Anagram.query db word in
  printLn $ SortedSet.toList anagrams

main : IO ()
main = do
  [_, key] <- getArgs
  Right db <- importFromFile "/usr/share/dict/words"
  showResult db key
```

今回はコンパイルして実行するので `/usr/share/dict/words` を読み込んでます。

以下のコマンドでコンパイル/実行します。


```idris
$ idris -o AnagramMain -p contrib AnagramMain.idr
$ ./AnagramMain tea
["ate", "eat", "eta", "tea"]
```

多少時間がかかりますが、正しく動いています。

# パッケージを作る

さきほどのアナグラムをパッケージにしてみましょう。
パッケージにすることでビルドが楽になります。


新しくディレクトリを作り、さらにその下に `src/` を作ります。
そして先程の `Anagram.idr` と `AnagramMain.idr` を `src` に入れます。

```text
$ mkdir -p anagram/src
$ mv Anagram.idr anagram/src
$ mv AnagramMain.idr anagram/src
$ cd anagram
```

## ipkg

Idrisにはパッケージ機構があるのは説明した通りです。そのためのファイルを書きましょう。
`anagram.ipkg` に以下の内容を書きます。

```text
package anagram

version = "0.1.0"
author = your name

sourcedir = src
modules = Anagram
        , AnagramMain
main = AnagramMain
executable = anagram
pkgs = contrib
```

少し解説しましょう。
ipkgはIdrisの独自フォーマットで、パッケージのメタデータやビルド情報などを記述します。

中身は `package` から始まります。

```text
package anagram
```

そして `version` 、 `author` などのメタ情報を書きます。

```text
version = "0.1.0"
author = your name
```

ソースコード情報として `sourcedir` や `modules` も書きます。

```text
sourcedir = src
modules = Anagram
        , AnagramMain
```

`main` のあるファイルや作成する実行可能ファイルも書きます。

```text
main = AnagramMain
executable = anagram
```

ビルドするときにリンクするパッケージを書きます。

```text
pkgs = contrib
```

詳細は[ドキュメント](http://docs.idris-lang.org/en/latest/reference/packages.html#ref-sect-packages)を読んで下さい。


これをビルドしてみましょう。

```text
$ idris --build anagram.ipkg
$ ls
anagram  anagram.ipkg  src
```

カレントディレクトリに `anagram` ができました。実行してみましょう。

```text
$ ./anagram ocean
["canoe", "ocean"]
```

正しく動いていますね。

このようにパッケージを作るとコマンド一発でビルドできるようになります。


# パッケージをインストールする

`idris --install IPKG` でライブラリをインストールすることもできます。
IdrisのWikiにはサードパーティのライブラリが載っていて、それをダウンロードしてインストールできる仕組みになっています。

[Libraries · idris-lang/Idris-dev Wiki](https://github.com/idris-lang/Idris-dev/wiki/Libraries)

ライブラリを使わないまでも、インストールだけでも試してみましょう。[lightyear](https://github.com/ziman/lightyear)をインストールしてみます。

```text
$ git clone https://github.com/ziman/lightyear.git
$ cd lightyear
$ idris --install lightyear.ipkg
Type checking ./Lightyear/Position.idr
Type checking ./Lightyear/Core.idr
Type checking ./Lightyear/Combinators.idr
Type checking ./Lightyear.idr
Type checking ./Lightyear/Char.idr
Type checking ./Lightyear/Strings.idr
Type checking ./Lightyear/StringFile.idr
Type checking ./Lightyear/Testing.idr
Installing Lightyear.ibc to /home/shun/.cabal/store/ghc-8.8.3/idris-1.3.3-0851a67ad4b1dcdc142d91174b2e9b6104a6df9b6243943de3f9ab0e56756b9a/share/libs/lightyear
Installing Lightyear/Position.ibc to /home/shun/.cabal/store/ghc-8.8.3/idris-1.3.3-0851a67ad4b1dcdc142d91174b2e9b6104a6df9b6243943de3f9ab0e56756b9a/share/libs/lightyear/Lightyear
Installing Lightyear/Core.ibc to /home/shun/.cabal/store/ghc-8.8.3/idris-1.3.3-0851a67ad4b1dcdc142d91174b2e9b6104a6df9b6243943de3f9ab0e56756b9a/share/libs/lightyear/Lightyear
Installing Lightyear/Combinators.ibc to /home/shun/.cabal/store/ghc-8.8.3/idris-1.3.3-0851a67ad4b1dcdc142d91174b2e9b6104a6df9b6243943de3f9ab0e56756b9a/share/libs/lightyear/Lightyear
Installing Lightyear/StringFile.ibc to /home/shun/.cabal/store/ghc-8.8.3/idris-1.3.3-0851a67ad4b1dcdc142d91174b2e9b6104a6df9b6243943de3f9ab0e56756b9a/share/libs/lightyear/Lightyear
Installing Lightyear/Strings.ibc to /home/shun/.cabal/store/ghc-8.8.3/idris-1.3.3-0851a67ad4b1dcdc142d91174b2e9b6104a6df9b6243943de3f9ab0e56756b9a/share/libs/lightyear/Lightyear
Installing Lightyear/Char.ibc to /home/shun/.cabal/store/ghc-8.8.3/idris-1.3.3-0851a67ad4b1dcdc142d91174b2e9b6104a6df9b6243943de3f9ab0e56756b9a/share/libs/lightyear/Lightyear
Installing Lightyear/Testing.ibc to /home/shun/.cabal/store/ghc-8.8.3/idris-1.3.3-0851a67ad4b1dcdc142d91174b2e9b6104a6df9b6243943de3f9ab0e56756b9a/share/libs/lightyear/Lightyear
Installing 00lightyear-idx.ibc to /home/shun/.cabal/store/ghc-8.8.3/idris-1.3.3-0851a67ad4b1dcdc142d91174b2e9b6104a6df9b6243943de3f9ab0e56756b9a/share/libs/lightyear
```

このようにインストールできます。インストールが済んでしまえば `idris -p lightyear` やipkgの `pkgs` に書くと使えるようになります。
シンプルながらも簡単に使えていいですね。

# ipkgの他の機能

`--build` や `--install` を紹介しましたが、REPLに読み込む `--repl` やドキュメントの作成などがあります。
`idris --help` などで確認してみて下さい。

# まとめ

Idrisのパッケージやipkgの紹介をしました。

ipkgは便利ですが、コマンドが長いのといつも `.ipkg` の書き方を忘れるので私は便利コマンドを作っていたりします。

[blackenedgold/ipkg](https://gitlab.com/blackenedgold/ipkg)

もし興味があれば使ってみて下さい。

# 付録

100語の辞書


```text
Alba's
Ceylon
Goddard's
Guggenheim
Ines
Kubrick's
Muslims
Paderewski's
Phillipa's
Reinhardt
Rumsfeld's
Salome's
Saudi's
Schlitz
Titanic
Waldheim's
Xi'an's
abide
aborted
acceded
adaptability
affirm
amiability
asphalting
belligerency
blasphemed
boars
bowl
caldrons
carefullest
cesarian
commissioner
conciliators
conclusion's
constituents
cowardly
crush's
defender
dewy
downers
drabs
drouthes
finality's
gaskets
giggle
glamour's
greasy
grouting
headgear
hied
hookworm's
hurry
knot
lab
lavishest
louvered
mallet
markdown
minuscule
misdeals
misquote's
mitosis
mountaineers
objectionably
overawing
ovule
pertinacious
phototypesetting
phrased
polliwog
promiscuity
quadruplet
replete
repugnance's
safekeeping
salt's
spanks
spun
steamship's
supermen
suspenseful
suspicion's
taxidermists
teetotaller
testosterone's
tints
totalitarians
tramming
typography
wafers
wear
weasel
whined
workingman
```
