---
categories: [ML, SML]
date: 2015-04-12T19:49:41+09:00
title: SMLのファンクタに少し踏み込んだ
---
κeenです。[SmlSharpContrib](https://github.com/bleis-tift/SmlSharpContrib)にコントリビュートしてます。そこでファンクタを使う用事があったのですが少し踏み込んだ使い方をしようとしたらハマったのでメモ。
<!--more-->
# ファンクタおさらい
SMLの`functor`は`structure`に引数がついたもので、モジュールを引数にとり、モジュールを返します。

```sml
functor List (Args : sig type elem end) =
struct
  type elem = Args.elem
  datatype list = Nil | Cons of elem * list
  fun length Nil = 0
    | length (Cons (x, xs)) = 1 + length xs
end

structure IntList = List(struct type elem = int end)
```

# 複雑なファンクタ
[以前mlyaccを使った時](blog/2015/01/31/mlyaccwotsukattemitehamattatokoro/)に`Join`なる3つのモジュールを引数にとるファンクタが登場したのでした。

```sml
structure PrologParser =
Join(structure LrParser = LrParser
     structure ParserData = PrologParserLrVals.ParserData
     structure Lex = PrologLex)
```

これの定義を覗いてみます。

```sml
functor Join(structure Lex : LEXER
             structure ParserData: PARSER_DATA
             structure LrParser : LR_PARSER
             sharing ParserData.LrTable = LrParser.LrTable
             sharing ParserData.Token = LrParser.Token
             sharing type Lex.UserDeclarations.svalue = ParserData.svalue
             sharing type Lex.UserDeclarations.pos = ParserData.pos
             sharing type Lex.UserDeclarations.token = ParserData.Token.token)
                 : PARSER =
...
```

複数のモジュールの他に`sharing`なるキーワードも出てきています。それに`structure`キーワードもプリフィクスされています。

先程の例とは大分離れてますね。何があったのでしょう。`structure`を付けとけば複数書ける…？

# 省略記法
実はファンクタの引数の中では省略記法が使えます。引数のモジュール名と`sig ... end`が省略可能なのです。さらに適用の時も`struct ... end`も省略可能なのです。

つまり、最初の例はこうも書けるのです。

```sml
functor List (type elem) =
struct
  type elem = Args.elem
  datatype list = Nil | Cons of elem * list
  fun length Nil = 0
    | length (Cons (x, xs)) = 1 + length xs
end

structure IntList = List(type elem = int)
```

# モジュール内モジュールと省略記法
そうです。複雑怪奇な`Join`ファンクタは省略記法で書かれていたのでした。省略せずに書くと

```sml
structure PrologParser =
Join(struct
     structure LrParser = LrParser
     structure ParserData = PrologParserLrVals.ParserData
     structure Lex = PrologLex
     end)
```

```sml
functor Join(X: sig
             structure Lex : LEXER
             structure ParserData: PARSER_DATA
             structure LrParser : LR_PARSER
             sharing ParserData.LrTable = LrParser.LrTable
             sharing ParserData.Token = LrParser.Token
             sharing type Lex.UserDeclarations.svalue = ParserData.svalue
             sharing type Lex.UserDeclarations.pos = ParserData.pos
             sharing type Lex.UserDeclarations.token = ParserData.Token.token
             end)
                 : PARSER =
...
```

となります。形式的には引数のモジュールは1つでありながら事実上複数のモジュールを渡していたのです。`structure`が付いていたのはモジュール内モジュールだったから、`sharing`はモジュール内モジュールに対する制約宣言です。

なぜこれでハマったかというとSML#のインターフェースファイルでは省略記法が使えなかったからです。地雷の数だけ強くなれるよ♪

# 参考

* [モジュール - ウォークスルー Standard ML](http://walk.wgag.net/sml/module.html)
* [ファンクタのサポート in Ch.11. SML#分割コンパイルシステム in プログラミング言語SML#解説](http://www.pllab.riec.tohoku.ac.jp/smlsharp/docs/2.0/ja/Ch11.S6.xhtml)
