---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-07T22:24:20+09:00
title: "REPLでIdris小旅行"
---

このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の8日目の記事です。

κeenです。昨日の予告通り今回は手を動かすパートとしてIdrisのREPLを紹介することにします。

<!--more-->

`idris` を引数なしで起動するとインタラクティブシェルに入ります。

```text
$ idris
     ____    __     _
    /  _/___/ /____(_)
    / // __  / ___/ / ___/     Version 1.3.3
  _/ // /_/ / /  / (__  )      https://www.idris-lang.org/
 /___/\__,_/_/  /_/____/       Type :? for help

Idris is free software with ABSOLUTELY NO WARRANTY.
For details type :warranty.
Idris> 
```

ここに式を入力するとその評価結果を表示してくれます。

```text
Idris> 1 + 1
2 : Integer
Idris> 
```

これは入力の読み取り（Read）、評価（Eval）、表示（Print）を繰り返して（Loop）くれるのでREPLと呼ばれます。

言語の初学者にはいちいちファイルを書いてコンパイルして実行しなくても挙動を確認できるので便利ですね。
さらにIdrisはかなりREPLを作り込んでるので玄人にも重要な機能です。

それではこれを使っていきましょう。


# 式の評価

上にも書いたとおり、式を入力するとそれを計算して表示してくれます。

``` text
Idris> [1..10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10] : List Integer
Idris> [ i | i <- [1..10], i `mod ` 2 == 0]
[2, 4, 6, 8, 10] : List Integer
```

Idrisでは型も値として扱えることを思い出すと、型名を入力するとそれも評価して表示してくれることが分かります。

``` text
Idris> List
List : Type -> Type
Idris> Integer
Integer : Type
Idris> List
List : Type -> Type
idris> List Integer
List Integer : Type
```

とはいえ型は実体がないので特に計算とかは走りません。

一応計算を含む型であればそれを計算した結果が表示されます。


``` text
Idris> if True then Integer else String
Integer : Type
```

ところで「型の型」である `Type` の型、 「型の型の型」が気になりませんか？
これを打ち込むとこう表示されます。

``` text
Idris> Type
Type : Type
```

`Type` 自身が `Type` のインスタンスになってますね。
これではかの有名な[ラッセルのパラドックス](https://ja.wikipedia.org/wiki/ラッセルのパラドックス)になってしまいます。
しかし（私も自信はないですが）恐らくこれは表示を省略しているだけでそういう矛盾はIdrisには含まれていません。

# REPLコマンド

さて、ここからが本番です。
IdrisのREPLには豊富なコマンドがあります。
その全貌はREPLに `:?` または `:help` と打つと表示されます。

``` text
Idris> :?

Idris version 1.3.3
-------------------

   Command         Arguments   Purpose

   <expr>                      Evaluate an expression
   :t :type        <expr>      Check the type of an expression
   :core           <expr>      View the core language representation of a term
   :miss :missing  <name>      Show missing clauses
   :doc            <name>      Show internal documentation
   :mkdoc          <namespace> Generate IdrisDoc for namespace(s) and dependencies
   :apropos        [<package list>] <name> Search names, types, and documentation
   :s :search      [<package list>] <expr> Search for values by type
   :wc :whocalls   <name>      List the callers of some name
   :cw :callswho   <name>      List the callees of some name
   :browse         <namespace> List the contents of some namespace
   :total          <name>      Check the totality of a name
   :r :reload                  Reload current file
   :w :watch                   Watch the current file for changes
   :l :load        <filename>  Load a new file
   :!              <command>   Run a shell command
   :cd             <filename>  Change working directory
   :module         <module>    Import an extra module
   :e :edit                    Edit current file using $EDITOR or $VISUAL
   :m :metavars                Show remaining proof obligations (metavariables or holes)
   :p :prove       <hole>      Prove a metavariable
   :elab           <hole>      Build a metavariable using the elaboration shell
   :a :addproof    <name>      Add proof to source file
   :rmproof        <name>      Remove proof from proof stack
   :showproof      <name>      Show proof
   :proofs                     Show available proofs
   :x              <expr>      Execute IO actions resulting from an expression using the interpreter
   :c :compile     <filename>  Compile to an executable [codegen] <filename>
   :exec :execute  [<expr>]    Compile to an executable and run
   :dynamic        <filename>  Dynamically load a C library (similar to %dynamic)
   :dynamic                    List dynamically loaded C libraries
   :? :h :help                 Display this help text
   :set            <option>    Set an option (errorcontext, showimplicits, originalerrors, autosolve, nobanner, warnreach, evaltypes, desugarnats)
   :unset          <option>    Unset an option
   :color :colour  <option>    Turn REPL colours on or off; set a specific colour
   :consolewidth   auto|infinite|<number>Set the width of the console
   :printerdepth   [<number>]  Set the maximum pretty-printer depth (no arg for infinite)
   :q :quit                    Exit the Idris system
   :version                    Display the Idris version
   :warranty                   Displays warranty information
   :let            (<top-level declaration>)...Evaluate a declaration, such as a function definition, instance implementation, or fixity declaration
   :unlet :undefine(<name>)... Remove the listed repl definitions, or all repl definitions if no names given
   :printdef       <name>      Show the definition of a function
   :pp :pprint     <option> <number> <name>Pretty prints an Idris function in either LaTeX or HTML and for a specified width.
   :verbosity      <number>    Set verbosity level
```

まだ紹介してない機能向けのコマンドなどもあるのでこれを見ただけだと混乱するかもしれません。
ゆっくり1つずつ試していきましょう。

# 関数を定義する

REPLに打ち込んで評価できるのは式までで、 `foo = 1` のような変数定義は扱えません。
その代わり `:let` コマンドで定義できます。
試してみましょう。

``` text
Idris> :let foo : Integer
Idris> foo
foo : Integer
Idris> :let foo = 1
Idris> foo
1 : Integer
```

定義できていますね。

関数も定義できますが、少し癖があるようです。

``` text
Idris> :let add : Integer -> Integer -> Integer
Idris> :let add x y = x + y
When checking an application of function Prelude.Interfaces.+:
        No such variable x
Idris> :let add = \x, y => x + y
Idris> :t add
add : Integer -> Integer -> Integer
```

関数は後述するファイルを読み込む方法を使った方がよさそうです。

定義した関数は `:unlet` （`:undefine`）で未定義に戻すことができます。

``` text
Idris> :undefine
Undefined foo,foo,add,add.
```

# 型の表示

`:t` あるいは `:type` コマンドで式の型を表示できます。

``` text
Idris> :t 1
1 : Integer
Idris> :t Integer
Integer : Type
Idris> :t putStrLn
putStrLn : String -> IO ()
```

式の評価とあまり変わらなそうな気がしますが、評価に時間のかかる式で型情報だけほしい場合に便利です。

ここで例の `Type` の型を見てみましょう。

``` text
Idris> :t Type
Type : Type 1
```

`Type 1` と表示されました。
じゃあその型は…となると察しのとおり `Type 2` になるはずですが、今のところユーザが手で書く手段がないので実験的に確かめることはできません。

``` text
Idris> :t (Type 1)
builtin:Type mismatch between
        Type (Type of Type)
and
        _ -> _ (Is Type applied to too many arguments?)
```

詳しくは省きますがIdrisの型システムはラッセルのパラドックスが起きなように設計されていて、型の型、型の型の型、型の型の型の型…はそれぞれ異なるようになっています。その階層を表わすのが `Type 1` についている1という数字です。

# ファイルを扱う

REPLにファイルを読み込んでみましょう。

`recodr.idr` というファイルに以下の内容を書いて保存します。


``` idris
record Person where
  constructor MkPerson
  age: Int
  name: String
```

## ロード
`:cd` コマンドで `record.idr` を書いたディレクトリまで移動します。

``` text
Idris> :cd ../Idris
```

そして `:l` （ `:load` ）コマンドでファイルを読み込みましょう。
ファイル名はTABによる補完が効きます。


``` text
Idris> :l record.idr
Type checking ./record.idr
*record>
```

プロンプトの表示が変わりましたね。`record.idr` を読み込むとファイルの内容を型チェックし、REPLにロードしてくれます。

例えば `Person` 型のコンストラクタ `MkPerson` が読み込まれてるのが確認できます。

``` text
*record> :t MkPerson
MkPerson : Int -> String -> Person
```

## リロード

次に `record.idr` を編集して `incAge` を追加してみましょう

``` idris
-- ...
incAge: Person -> Person
incAge = record { age $= (1+) }
```


これにはやり方が2種類あります。
1つは普通にエディタで編集して、 IdrisのREPLで `:r` （ `:reload` ）するものです。

``` idris
*record> :r
```

## 編集

もう1つはターミナルにひきこもってる人向けにIdrisのREPLから `:e` （ `:edit` ） でファイルを編集するものです。
`EDITOR` または `VISUAL` 環境変数に設定されているエディタを起動し、ファイルを編集するよう促します。
編集し終わるとIdrisのREPLに戻ってきて、以下のように続きます。


``` text
*record> :e
Type checking ./record.idr

Loading ./record.ibc failed: Module needs reloading:
        SRC : "./record.idr"
        Modified at: 2020-12-07 14:25:15.707323014 UTC
        IBC : "./record.ibc"
        Modified at: 2020-12-07 14:18:51.578095669 UTC

*record> :r
Type checking ./record.idr
```

`:e` のあとに型チェックしてくれるんですが、何故かエラーになってしまいました。コンパイル結果の中間ファイル（`*.ibc`） が悪さをしたのかな？しかし慌てずに `:r` を打てば問題ありません。

では今読み込んだ `incAge` 関数を試してみましょう。

``` text
*record> incAge (MkPerson 28 "Tom")
MkPerson 29 "Tom" : Person
```

こうやってファイルを編集しては試しての繰り返しの作業フローができました。

## 継続的リロード

因みに型チェックしたいだけなら `:w` （ `:watch` ）でファイル変更を監視して継続的にリロードすることもできます。

``` text
*record> :w
record.idr
Watching for .idr changes in ["record.idr"], press enter to cancel.
Type checking ./record.idr
```

# 探索する

IdrisのREPLはIdrisが知っていることを教えてくれます。
例えば `:browse` は名前空間に定義されているシンボル一覧を教えてくれます。
名前空間はまだ出てきてない機能ですが、雰囲気で察して下さい。

今回の `record.idr` には名前空間を指定してなかったので `Main` という名前が割り当てられています。

## 一覧する

`:browse` コマンドで名前空間に定義されているアイテムを一覧することができます。`Main` で定義されているアイテムを表示してみましょう。

``` text
*record> :browse Main
Namespaces:
  Main.Person
Names:
  MkPerson : Int -> String -> Person
  Person : Type
  incAge : Person -> Person
```

表示されましたね。因みに `Main.Person` という名前空間もあるようです。これも表示してみましょう。

``` text
*record> :browse Main.Person
Namespaces:

Names:
  age : Person -> Int
  name : Person -> String
  set_age : Int -> Person -> Person
  set_name : String -> Person -> Person
```

これは `record` 構文で生成された関数が格納されています。

## ドキュメントを表示する

Idrisには言語組み込みでドキュメントの機能があります（基本文法のところでドキュメントコメント構文を紹介しましたね）。それをREPLから表示できます。

例えば標準ライブラリの `List` のドキュメントを表示してみましょう。

``` text
*record> :doc List
Data type Prelude.List.List : (elem : Type) -> Type
    Generic lists

    The function is: public export
Constructors:
    Nil : List elem
        Empty list

        The function is: public export
    (::) : (x : elem) -> (xs : List elem) -> List elem
        A non-empty list, consisting of a head element and the rest of the list.
        infixr 7

        The function is: public export
```

しっかりと表示できてますね。
見なれない記述があってもひとまず無視して自然言語だけでも理解するようにして下さい。
まだ説明してない機能もたくさんあるので分からないことがあっても当然です。

## 探す

ほしい機能がないか探すこともできます。

例えば `List` を結合する関数を探しているとしましょう。
その関数は `List a -> List a -> List a` という型をしているはずです。これを検索してみましょう。
`:s` （ `:search` ） でその型を使って検索できます。

``` text
*record> :s List a -> List a -> List a
= Prelude.List.(++) : List a -> List a -> List a
Append two lists

= Prelude.List.reverseOnto : List a -> List a -> List a
Reverse a list onto an existing tail.

> Prelude.List.(\\) : Eq a => List a -> List a -> List a
The \\ function is list difference (non-associative). In the result of xs \\ ys, the first occurrence of each element of ys in turn
(if any) has been removed from xs, e.g.,

> Prelude.List.union : Eq a => List a -> List a -> List a
Compute the union of two lists according to their Eq implementation.

> Prelude.List.merge : Ord a => List a -> List a -> List a
Merge two sorted lists using the default ordering for the type of their elements.

....

```

いくつか候補がで出てきました。
このうち、 先頭に `=` がついているものは完全一致、 `>` がついているものはより狭い型、 `<` がついているものはより広い型の関数のようです。

ひとまず最初の候補である `Prelude.List.(++)` に 「Append two lists」 と書かれているのでこれが求める演算子のようです。
使ってみましょう。

``` text
*record> [1, 2, 3] ++ [4, 5, 6]
[1, 2, 3, 4, 5, 6] : List Intege
```

想定どおりですね。

もう1つ探し方があります。自然言語で全文検索する方法です。こちらは `:apropos` を使います。
同じく `List` を結合する関数を「append」 のキーワードで検索してみましょう。

``` text
*record> :apropos append

Prelude.List.(++) : List a -> List a -> List a
Append two lists

Prelude.Strings.(++) : String -> String -> String
Appends two strings together.

Prelude.File.Append : Mode


Prelude.File.ReadAppend : Mode


Prelude.Strings.addToStringBuffer : StringBuffer -> String -> IO ()
Append a string to the end of a string buffer

Prelude.List.appendAssociative : (l : List a) -> (c : List a) -> (r : List a) -> l ++ c ++ r = (l ++ c) ++ r
Appending lists is associative.

Prelude.List.appendCong2 : (x1 = y1) -> (x2 = y2) -> x1 ++ x2 = y1 ++ y2
Appending pairwise equal lists gives equal lists

...
```

こちらの場合も `Prelude.List.(++)` が表示されていますね。

適宜2つを使い分けながらほしい関数を探してみて下さい。

## 定義を表示する

`:printdef` で関数の定義を表示することもできます。

``` text
*record> :printdef Prelude.List.(++)
(++) : List a -> List a -> List a
[] ++ right = right
(x :: xs) ++ right = x :: xs ++ right
```

恐らくソースコードに書いてあるものそのままではなくIdrisの内部表現をユーザが読める形に再出力してるんですかね？

# 実行する

一旦今開いてるREPLを閉じて、別のセッションを開くことにします。
REPLを閉じるには `:q` （ `:quit` ） を打ち込むかEOFを送り込む（Ctrl+D）かをします。

そして以下の内容の `hello_world.idr` を用意しておきます。

``` idris
main : IO ()
main = putStrLn "Hello, World"
```

そして `idris` の引数に `hello_world.idr` を与えながら起動します。

``` text
$ idris hello_world.idr
     ____    __     _
    /  _/___/ /____(_)____
    / // __  / ___/ / ___/     Version 1.3.3
  _/ // /_/ / /  / (__  )      https://www.idris-lang.org/
 /___/\__,_/_/  /_/____/       Type :? for help

Idris is free software with ABSOLUTELY NO WARRANTY.
For details type :warranty.
Type checking ./hello_world.idr
*hello_world>
```

これでファイルの中身を読み込みながらREPLが起動してくれます。
一旦REPLを起動してから `:l` で読み込んでも同じですが、複数の方法を覚えておいて損はないでしょう。

さて、このファイルには `main` が定義されています。
つまり実行可能です。 `:exec` でファイルをコンパイ/実行できます。


``` text
*hello_world> :exec
Hello, World
```

`:c` （ `:compile` ） でコンパイルするこもできます。

``` text
*hello_world> :c hello_world
```

また、読み込んだ `main` の名前を指定して実行することもできます。
`:x` です。

``` text
*hello_world> :x main
Hello, World
MkIO (\w => prim_io_pure ()) : IO' (MkFFI C_Types String String) ()
```

ちょっと見えちゃいけないものが見えてるような気がしますが、大丈夫です。

これだけ揃えば「あれIdrisでどう書くんだっけ」というのはなんとかなるんじゃないでしょうか。

# Idris小旅行

結果は貼りませんが、以下のようなコマンドでIdrisの基本の手札を確認できるんじゃないでしょうか。

``` text
Idris> :browse Builtins
idris> :browse Prelude
Idris> :browse Prelude.List
Idris> :doc Prelude.List.head'
Idris> :printdef the
Idris> :printdef id

-- などなど
```

# まとめ

IdrisでのREPLの使い方、そしてそれを通じたIdrisの探索方法を紹介しました。
IdrisのREPLは非常に高機能で、それだけでプログラミングできてしまいます。
ぜひREPLを使いこなしましょう。
