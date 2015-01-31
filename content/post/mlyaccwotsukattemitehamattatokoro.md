---
categories: [SML, mlyacc, Prolog]
date: 2015-01-31T11:55:51Z
title: mlyaccを使ってみてハマったところ
---
κeenです。前回の[mllexの記事](http://keens.github.io/blog/2014/12/10/mllexwoshi-tutemiru/)の続きです。今回はmlyaccを使ってみました。
<!--more-->
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

前回はprologをパースするためにこのようなmllexのコードを書いたのでした。

```
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

これでトークナイズしたトークンをパースすることを考えます。結果はAST作るのが面倒なので文字列にします。

とりあえずコードを晒してから解説します。prologの仕様を読まずに書いたので用語や文法は誤りを含みます。今度直すので今回はこれで勘弁して下さい。

```

%%
%name PrologParser
%pos int

%term  Comment of string | LeftParen | RightParen | LeftBracket | RightBracket | Comma | Dot | Bar | Medaka | UnderScore | Number of int | String of string | Variable of string | EOF

%nonterm EXP of string | LIST of string | LISTEXPS of string | TOPDEFINITION of string | SUBDEFINITIONS of string | DEFINITION of string | ARG of string | NAME of string | STMT of string | ATOM of string | START of string

%left Comma
%right Bar
%eop EOF
%noshift EOF
%nodefault

%%

START : STMT (STMT)

STMT           : Comment STMT       (Comment ^ "\n" ^ STMT)
               | TOPDEFINITION STMT (TOPDEFINITION ^ "\n" ^ STMT)
               | ("")

TOPDEFINITION  : DEFINITION Dot                       (DEFINITION ^ ".")
               | DEFINITION Medaka SUBDEFINITIONS Dot (DEFINITION ^ ":-\n" ^ SUBDEFINITIONS ^ ".")

SUBDEFINITIONS : DEFINITION Comma SUBDEFINITIONS      (DEFINITION ^ ",\n" ^SUBDEFINITIONS)
               | DEFINITION                           ("  " ^ DEFINITION)

DEFINITION     : NAME ARG (NAME ^ ARG)

NAME           : String     (String)

               
ARG            : LeftParen EXP RightParen ("(" ^ EXP ^ ")")
               
               
EXP            : EXP Comma EXP  (EXP1 ^ ", " ^ EXP2)
               | LIST           (LIST)
               | ATOM           (ATOM)

LIST           : LeftBracket LISTEXPS RightBracket ( "[" ^ LISTEXPS ^ "]")

LISTEXPS       : ATOM Bar LISTEXPS (ATOM ^ " | " ^ LISTEXPS)
               | ATOM (ATOM)

ATOM           : UnderScore               ("_")
               | Number                   (Int.toString Number)
               | String                   (String)
               | Variable                 (Variable)
               | LeftBracket RightBracket ("[]")

```

全体はやはり

```
補助コード
%%
補助ルール
%%
パースルール
```

となります。

トークンに切り出す部分がないので補助コードはあまり書く必要はないようです。ASTを組み立てる時に必要なことがあるのかな？

補助ルールは、主なものは終端記号(term)と非終端記号(nonterm)の記述です。パーサに馴染のない人には聞き慣れない言葉かもしれませんが、ASTの葉が終端記号で、節が非終端記号です。あとはパースの終わりの記号にeop、パースの区切にnonshiftを指定しましょう。パースの区切はEOFの他にREPLならNewlineなども候補かもしれません。posの型も必須です。leftやrightは中置演算子があればそれの結合の左右を指定します。複数書くと後の方が優先順位が高くなるようです。

```
%left Add Sub
%left Mul Div
```

のように。


パースルールは見て察して下さい。

これをprolog.yaccなどと名前をつけて(.grmがよく使われるそうですが。)、

    $ mlyacc prolog.yacc

とするとprolog.yacc.smlとprolog.yacc.sigが出来ます。

追記:
<blockquote class="twitter-tweet" lang="ja"><p><a href="https://twitter.com/blackenedgold">@blackenedgold</a> あと拡張子ですが、compilation managerは.grmをmlyaccのファイルと認識してくれるはずなので手間が少なそうです。</p>&mdash; ELD-R-ESH-2 (@eldesh) <a href="https://twitter.com/eldesh/status/561515838461583361">2015, 1月 31</a></blockquote>
大人しく.grmを使いましょう。


シグネチャは.sigが

```sml
signature PrologParser_TOKENS =
  sig
    type ('a,'b) token
    type svalue
    val EOF : 'a * 'a -> (svalue,'a) token
    val Variable : string * 'a * 'a -> (svalue,'a) token
    val String : string * 'a * 'a -> (svalue,'a) token
    val Number : int * 'a * 'a -> (svalue,'a) token
    val UnderScore : 'a * 'a -> (svalue,'a) token
    val Medaka : 'a * 'a -> (svalue,'a) token
    val Bar : 'a * 'a -> (svalue,'a) token
    val Dot : 'a * 'a -> (svalue,'a) token
    val Comma : 'a * 'a -> (svalue,'a) token
    val RightBracket : 'a * 'a -> (svalue,'a) token
    val LeftBracket : 'a * 'a -> (svalue,'a) token
    val RightParen : 'a * 'a -> (svalue,'a) token
    val LeftParen : 'a * 'a -> (svalue,'a) token
    val Comment : string * 'a * 'a -> (svalue,'a) token
  end
signature PrologParser_LRVALS =
  sig
    structure Tokens :
      sig
        type ('a,'b) token
        type svalue
        val EOF : 'a * 'a -> (svalue,'a) token
        val Variable : string * 'a * 'a -> (svalue,'a) token
        val String : string * 'a * 'a -> (svalue,'a) token
        val Number : int * 'a * 'a -> (svalue,'a) token
        val UnderScore : 'a * 'a -> (svalue,'a) token
        val Medaka : 'a * 'a -> (svalue,'a) token
        val Bar : 'a * 'a -> (svalue,'a) token
        val Dot : 'a * 'a -> (svalue,'a) token
        val Comma : 'a * 'a -> (svalue,'a) token
        val RightBracket : 'a * 'a -> (svalue,'a) token
        val LeftBracket : 'a * 'a -> (svalue,'a) token
        val RightParen : 'a * 'a -> (svalue,'a) token
        val LeftParen : 'a * 'a -> (svalue,'a) token
        val Comment : string * 'a * 'a -> (svalue,'a) token
      end
    structure ParserData :
      sig
        type pos
        type svalue
        type arg
        type result
        structure LrTable : <sig>
        structure Token : <sig>
        structure Actions : <sig>
        structure EC : <sig>
        val table : LrTable.table
        sharing LrTable = Token.LrTable
      end
    sharing type Tokens.svalue = ParserData.svalue
    sharing type Tokens.token = ParserData.Token.token
  end
```

で、.smlが

```sml
functor PrologParserLrValsFun(<param>: sig
                                         structure Token : <sig>
                                       end) :
                             sig
                               structure ParserData : <sig>
                               structure Tokens : <sig>
                             end
```

な感じです。
# 問題点
これ、以前のlexのコードと組み合わせても動きません。パーサはレキサが吐いたトークン列をパースするのでレキサとパーサで同じdatatypeを共有している必要があります。パーサはtermの指定からTokensを自動で生成してしまうのでレキサの方を修正する必要があります。

こうなります。

```
structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue,pos) token
(* datatype lexresult = *)
(*          Comment of string *)
(*        | LeftParen *)
(*        | RightParen *)
(*        | LeftBracket *)
(*        | RightBracket *)
(*        | Comma *)
(*        | Dot *)
(*        | Bar *)
(*        | Medaka *)
(*        | UnderScore *)
(*        | Number of int *)
(*        | String of string *)
(*        | Variable of string *)
(*        | EOF *)
val pos = ref 0
val linenum = ref 1
val error = fn x => print (x ^ "\n")
val eof = fn () => Tokens.EOF(!pos, !pos)

%%
%header (functor PrologLexFun(structure Tokens: PrologParser_TOKENS));

alphanum = [A-Za-z0-9];
alpha    = [A-Za-z];
digit    = [0-9];
ws       = [\ \t\n];

%%

{ws}+            => (lex());
%(.*)\n          => (Tokens.Comment(yytext, !pos, !pos));
"("              => (Tokens.LeftParen(!pos, !pos));
")"              => (Tokens.RightParen(!pos, !pos));
"["              => (Tokens.LeftBracket(!pos, !pos));
"]"              => (Tokens.RightBracket(!pos, !pos));
","              => (Tokens.Comma(!pos, !pos));
"."              => (Tokens.Dot(!pos, !pos));
"|"              => (Tokens.Bar(!pos, !pos));
":-"             => (Tokens.Medaka(!pos, !pos));
"_"              => (Tokens.UnderScore(!pos, !pos));
[a-z]{alphanum}* => (Tokens.String(yytext, !pos, !pos));
{digit}+         => (Tokens.Number ((foldl (fn(a, r)=> (ord(a)-ord(#"0")) + 10*r) 0 (explode yytext)), !pos, !pos));
[A-Z]{alphanum}* => (Tokens.Variable(yytext, !pos, !pos));

```

モジュールではなくてファンクタを生成するようになります。シグネチャはこうです。

```sml
functor PrologLexFun(<param>: sig
                                structure Tokens : <sig>
                              end) :
                    sig
                      structure UserDeclarations : <sig>
                      exception LexError
                      structure Internal : <sig>
                      structure YYPosInt : <sig>
                      val makeLexer : (int -> string)
                                      -> unit -> Internal.result
                    end
```

# 組み合わせる

まず、必要なモジュールやシグネチャをmlyaccから読み込みます。前回同様mltonのmlyaccを使って、SML/NJで動作確認を行ないました。

私は分かってないのですが、useってもしかしてSML/NJの固有の機能なんですかね。

追記:
<blockquote class="twitter-tweet" lang="ja"><p>useはThe Standard ML Basis Libraryに記載があるのでSML/NJ固有の機能ではないですが、implementation dependentと書いてあるのでまぁそういう事なんでしょう</p>&mdash; ろんだ (@fetburner) <a href="https://twitter.com/fetburner/status/561514796734877698">2015, 1月 31</a></blockquote>

<blockquote class="twitter-tweet" lang="ja"><p><a href="https://twitter.com/blackenedgold">@blackenedgold</a> useは標準ですが意味は実装依存ですね&gt; <a href="http://t.co/fPC38xtD1X">http://t.co/fPC38xtD1X</a> (例外の直後辺り)</p>&mdash; ELD-R-ESH-2 (@eldesh) <a href="https://twitter.com/eldesh/status/561515332469137408">2015, 1月 31</a></blockquote>
とのことなので処理系依存の機能ではなかった模様。

```sml
use "/home/kim/compile/mlton/lib/mlyacc-lib/base.sig";
use "/home/kim/compile/mlton/lib/mlyacc-lib/join.sml";
use "/home/kim/compile/mlton/lib/mlyacc-lib/lrtable.sml";
use "/home/kim/compile/mlton/lib/mlyacc-lib/stream.sml";
use "/home/kim/compile/mlton/lib/mlyacc-lib/parser2.sml";
use "prolog.yacc.sig";
use "prolog.yacc.sml";
use "prolog.lex.sml";
```

mlyacc-libの場所は各自異なるので適宜書き換えて下さい。SML/NJはインストール場所を表わすシンボルを持っているようですが、私はmltonのものを参照しているのでいずれにせよ関係ないですね。


さて、この.yacc.smlに入ってるファンクタです。

```sml
functor PrologParserLrValsFun(<param>: sig
                                         structure Token : <sig>
                                       end) :
                             sig
                               structure ParserData : <sig>
                               structure Tokens : <sig>
                             end
```

何故こうなってるのかは分からないのですがTokenを欲しがってますね。あげましょう。

```sml
structure PrologParserLrVals =
PrologParserLrValsFun(structure Token = LrParser.Token)
```

LrValsって言ってるのでLRパーサーで使うデータ型が用意出来たのかな？

これでレキサで使うTokensとパーサで使うParserDataを持つモジュールが出来たのでレキサのファンクタの餌が用意出来ました。食べさせてあげましょう。

```sml
structure PrologLex =
PrologLexFun(structure Tokens = PrologParserLrVals.Tokens)
```

これは普通にレキサを生成します。

んで最後にLRパーサとLRパーサで使うデータ型(?)とLRパーサに渡すトークンを生成するレキサを組み合わせます。なんかsharingなる機能を使って組み合わせるらしいのでそれ専用のファンクタが用意されています。sharingなんぞ。いつか[The Definition of Standard ML Revised](https://github.com/SMLFamily/The-Definition-of-Standard-ML-Revised)読まねば。

```sml
structure PrologParser =
Join(structure LrParser = LrParser
     structure ParserData = PrologParserLrVals.ParserData
     structure Lex = PrologLex)
```

やっぱり`LrParser`を使ってるのが腑に落ちませんね。どうして生成した時点で含んでないのかな。複数のパーサを作ったときに無駄を無くすため？自前のパーサが使えるようにするため？`LrParser.Token`はまだパーサとsharingしてる必要がありそうなので納得出来ますが。

# 使ってみる
`makeLexer`と`parse`を使うようです。`makeLexer`は呼ぶ度にトークンを返すものではなく、トークンのストリームを返すものになってるらしいです。

prologファイル名を受け取ってその中身をパースして、中身を整形した文字列にするコードです。

```sml
fun invoke lexstream =
    let
        fun print_error (s, _, _) =
            TextIO.output(TextIO.stdOut,
                          "Error: " ^ s ^ "\n")
    in
        PrologParser.parse(0, lexstream, print_error, ())
    end

fun parse filename =
    let
        val f = TextIO.openIn filename
        val lexer = PrologParser.makeLexer
                        (fn i => TextIO.inputN(f, i))
        fun run lexer =
            let
                val (result,lexer) = invoke lexer
            in
                TextIO.output(TextIO.stdOut,
                              "result = " ^ result ^ "\n")
            end
    in
        run lexer
    end
```

今回はposのアップデートをサボったので`print_error`内では無視してますが本来は`print_error`は

```sml
print_error (s, start:int, end:int)
```

として受けるべきです。この時startとendはトークンの開始位置と終了位置です。

他にも説明が足りてない部分がありますが、マニュアルや[東北大のPDF](http://www.pllab.riec.tohoku.ac.jp/education/lectures/compiler/code/mlyaccKaisetsu.pdf)を参照して下さい。

# まとめ
* mlyaccの使い方を書いた
* mllexとmlyaccの組み合わせ方を書いた
* 疑問が残った

次回はASTでも作ってみますが、smlsharpにSMLFormatなるdatatypeの定義とそのプリティプリンタの定義を同時に出来るツールがあるようなのでそれを使ってみます。いくつも中間表現を作ろうと思ったら途中経過も欲しいのでプリティプリンタ重要ですよね。
