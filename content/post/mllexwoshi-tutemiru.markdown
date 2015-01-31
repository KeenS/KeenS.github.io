---
type: post
title: "mllexを使ってみる。あるいはlexユーザーに対するmllexの解説"
date: 2014-12-10
comments: true
categories: [SML, Prolog, mllex, Advent Calendar]
---
このエントリーは[ML Advent Calendar 10日目](http://qiita.com/advent-calendar/2014/ml)の記事です。  
前 [OCamlで作ったgoma言語 - h_sakurai's diary](http://h-sakurai.hatenablog.com/entry/2014/12/09/144655)

ふと思い立ってPrologのコンパイラかインタプリタかを作ってみようとして、その第一段でlexerに着手しました。
<!--more-->
ちょ、SML分かる人はmllexくらい常識的に知ってるなんて言わないで下さい。私は初めて触りました。

ちょっと変則的なんですが、smlsharpの配布物にあるドキュメントを読みながらmlton付属のmllexを使い、sml/nj上で動作を確認しました。SMLの処理系管理ガバガバですね。

まあ、初めて使ってみたとは言ってもほぼC版のlexのCを書く部分をそのままSMLで書けば良いだけなので簡単ですね。

因みにPrologの文法は以下のようなもの。

```prolog
%reverse a list into X
reverse([], []).

reverse([Hd|Tl], X):-
    reverse(Tl, Y),
    append(Y, [Hd], X).

reverse(List, X):-
    reverse(List, []).

```

だいたいの部分は見れば分かるかと思うのですが、小文字で始まるのがリテラル（文字列？識別子？）で大文字から始まるのが変数です。

で、これをイメージしながらlexファイルを書いていきます。

# Lexファイル
全体はこのようになっています。lexと同じですね。

```
SMLの補助コード部分

%%

補助ルールの記述

%%

解析ルールの記述

```

SMLの補助コードの部分は典型的には`datatype`を書きます。あとはどうも`error : string -> unit`と `eof : unit -> 'a`が必要みたいですね（マニュアルちゃんと読んでない）。`'a`というのは他の解析ルールと同じ型です。


補助ルールはlexとだいたい同じです。生成した関数やらデータ型を収めるstructureの指定とかもします。

解析ルールもだいたいlexと同じです。

    pattern => (SMLのコード);

のような形をしてます。

# 実際のコード

結構適当です。あ、カットのこと忘れてた。ほら、適当だった。


このコードをprolog.lexとして保存します。

```sml
datatype lexresult =
         Comment of string
       | LeftParen
       | RightParen
       | LeftBracket
       | RightBracket
       | Comma
       | Dot
       | Bar
       | Medaka
       | UnderScore
       | String of string
       | Number of int
       | Variable of string
       | EOF

val linenum = ref 1
val error = fn x => print (x ^ "\n")
val eof = fn () => EOF
%%
%structure PrologLex                      

alphanum = [A-Za-z0-9];
alpha    = [A-Za-z];
digit    = [0-9];
ws       = [\ \t\n];

%%

{ws}+            => (lex());
%(.*)\n          => (Comment yytext);
"("              => (LeftParen);
")"              => (RightParen);
"["              => (LeftBracket);
"]"              => (RightBracket);
","              => (Comma);
"."              => (Dot);
"|"              => (Bar);
":-"             => (Medaka);
"_"              => (UnderScore);
[a-z]{alphanum}* => (String yytext);
{digit}+         => (Number (foldl (fn(a, r)=> (ord(a)-ord(#"0")) + 10*r) 0 (explode yytext)));
[A-Z]{alphanum}* => (Variable yytext);
    
```

# 実際に使ってみる。

	$ mllex prolog.lex
	
	Number of states = 19
	Number of distinct rows = 8
	Approx. memory size of trans. table = 1032 bytes
	$ rlwrap sml
	Standard ML of New Jersey v110.77 [built: Thu Sep  4 12:32:33 2014]
	- use "prolog.lex.sml";
	[opening prolog.lex.sml]
	[autoloading]
	[library $SMLNJ-BASIS/basis.cm is stable]
	[autoloading done]
	structure PrologLex :
	  sig
	    structure UserDeclarations : <sig>
	    exception LexError
	    structure Internal : <sig>
	    structure YYPosInt : <sig>
	    val makeLexer : (int -> string) -> unit -> Internal.result
	  end
	val it = () : unit
	- val lexer = PrologLex.makeLexer (fn i => TextIO.inputN(TextIO.openIn "reverse.pl", i));
	val lexer = fn : unit -> PrologLex.Internal.result
	- lexer();
	val it = Comment "%reverse a list into X\n" : PrologLex.Internal.result
	- lexer();
	val it = String "reverse" : PrologLex.Internal.result
	- lexer();
	val it = LeftParen : PrologLex.Internal.result
	- lexer();
	val it = LeftBracket : PrologLex.Internal.result
	- lexer();
	val it = RightBracket : PrologLex.Internal.result
	- 


コメントの扱い失敗してますね。この辺はどうしようもないのかなあ

# 困ったところ

ドキュメントが古いSML/NJのものらしく、SMLのコードをそのまま書いても動かなかった。`revfold`とか`inputc`とか。

次回はmlyaccでも触ってみますか。
