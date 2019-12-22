---
categories: ["ML", "SML", "継続"]
date: 2019-12-21T01:38:06+09:00
title: "SMLで継続を使ってprintf/scanf"
---

κeenです。Advent Calendarが賑う時期ですが特に関係なくSMLの話します。今年MLアドベントカレンダーやってないんですね。O.Danvyのprintfとその逆のscanfについて。

<!--more-->

# Printf
printfはO.DanvyのFunctional unparsing('98)で紹介されたテクニックです。
C言語の `printf` はフォーマットに依存して引数が変わるので一見すると依存型が必要そうに見えます。
しかし多少のテクニックを用いてSML内のDSLで実現してしまったというのがO.Danvyのprintfです（今回紹介するのは `sprintf` ですが瑣末な違いですね）。
以下のように使えます。


```text

(* 文字列は `lit "str"` 、改行は `eol` 、 指定子の結合は `++` 。 *)
- sprintf (lit "Hello world!" ++ eol)
val it = "Hello world!\n" : string

(* `%` に続いてフォーマッタ （`str`） を指定すると引数が1つ増える *)
sprintf (lit "Hello " ++ % str ++ lit "!" ++ eol)
val it = fn : string -> string


(* 引数を埋めるとフォーマットされる *)
- sprintf (lit "Hello " ++ % str ++ lit "!" ++ eol) "world"
val it = "Hello world!\n" : string

(* `int` など他のフォーマッタも使える。順に引数が増えていく。 *)
- sprintf (lit "The value of " ++ % str ++ lit " is " ++ % int ++ eol)
val it = fn : string -> int -> string


(* 適用する *)
- sprintf (lit "The value of " ++ % str ++ lit " is " ++ % int ++ eol) "x" 3
val it = "The value of x is 3\n" : string
```

ちょっと不思議な動きですよね。
フォーマッタに何を書くかによって型が変わり、引数が増えたり減ったりしています。
この動きが限定継続を使って実現されています。
実装は以下。意外とシンプルですね。


```sml
structure Printf = struct

    infix ++

    fun lit x s k = k (s ^ x)
    fun eol s k = k (s ^ "\n")
    fun f1 ++ f2 = (fn s => (fn k => (f1 s (fn s => f2 s k))))
    fun int x = Int.toString x
    fun str x = x
    fun % to_str s k = (fn x => k (s ^ to_str x))
    fun sprintf p = p "" (fn x => x)
end
```

結合している中間の状態 (`s: string`) と限定継続 (`k: string -> 'a`) を引き回していますが、 `lit` から `++` までは比較的スムーズに読めるんじゃないでしょうか。`string -> (string -> 'a) -> 'a` がフォーマッタの型ですね。

ここでテクニックが入っているのは `%` です。つぶさに見てみましょう。

```sml
fun % to_str s k = (fn x => k (s ^ to_str x))
```

関数から返るところに継続の起動 (`k` の適用) ではなくただのクロージャ(`fn x => ...`)を置いています。
継続を一旦止めてフォーマッタの外に出て、引数(`x`)を受け取ってから改めて継続を起動しています。
これでフォーマッタの外に並んだ値を1つ取り出せます。そして受け取った値は文字列にしたいので `to_str` で文字列化しています。

`sprintf` については仕組みが分かれば簡単ですね。
成果物の文字列の初期状態 (`""`) と限定継続の初期継続(`(fn x => x)`) を渡してフォーマッタを起動しているだけです。

# Scanf

で、その双対のscanfを考えてみましょう。
printfが20年前のものですし多分既知の結果があるかと思いますが、こういうのは答えを見ずに考えるのが楽しいですよね。

## 値のリーダー

さて、 `scanf` を実装するにあたってSMLで整数やブール値をパースする技法を紹介しておきます。
簡単な処理なら `fromString` などもあるのですが今回のように複数のscanを連接したい場合は [`StringCvt`](http://sml-family.org/Basis/string-cvt.html) モジュール（に関連した機能）を使うことになります。

[`StringCvt.scanString`](http://sml-family.org/Basis/string-cvt.html#SIG:STRING_CVT.scanString:VAL) はリーダーと文字列を取ってリーダーの指定した値を読み取ります。リーダーはそれぞれの型に `scan` などの名前で定義されています。[`Bool`](http://sml-family.org/Basis/bool.html#BOOL:SIG:SPEC) の [`scan`](http://sml-family.org/Basis/bool.html#SIG:BOOL.scan:VAL) を見てみましょう。

```text
- Bool.scan;
val it = fn : (char,'a) StringCvt.reader -> (bool,'a) StringCvt.reader
```

これを `StringCvt.scanString` で使って整数値を読み取ってみましょう。

まず、 `StringCvt.scanString` に `Bool.scan` を渡すと以下のように `string` を受け取って `bool option` を返す型ができます。

```text
- StringCvt.scanString Bool.scan;
val it = fn : string -> bool option
```

これを使っていくつかの文字列を読み取ってみましょう。

```text
- StringCvt.scanString Bool.scan "true";
val it = SOME true : bool option

- StringCvt.scanString Bool.scan "false";
val it = SOME false : bool option

- StringCvt.scanString Bool.scan "hoge";
val it = NONE : bool option
```

このように読み取れました。

ところで、この関数は末尾に余計な文字列があってもそのままパースできます。


```text
- StringCvt.scanString Bool.scan "truetrue";
val it = SOME true : bool option
```

というか、パースしたときにどこまで読んだかが分かりませんね。
この動きは便利な場面もあるかと思いますが、今回のように複数の値を読み出したいときには不便です。
2つ目の値をどこから読み始めたらいいか分かりません。
`scanString` を使うのではなく別の方法を考えましょう。

実は、既にパースした結果と残りの文字列を得る方法はあります。最初に `Bool.scan` を紹介したときに出てきた型、 [`StringCvt.reader`](http://sml-family.org/Basis/string-cvt.html#SIG:STRING_CVT.reader:TY) の中身を見せてませんでしたね。これは以下のように定義されています。


```sml
type ('a,'b) reader = 'b -> ('a * 'b) option
```

`'a` が読み出したい型、 `'b` がストリームの型です。
ここでは `'a` が `bool` 、 `'b` が文字列だとすると文字列から `bool` 値を読み取ってその結果と残りの値を返す関数になります。
これをそのまま文字列に適用してあげればよさそうですね。ここで改めて `Bool.scan` の型を見てみましょう。

``` sml
val scan: (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
```

`(bool, 'a) StringCvt.reader` を得るにはストリーム `char` を読み出すリーダーがまず必要になっています。
それ相当の関数やデータ型は [`String`](http://sml-family.org/Basis/string.html) や [`Char`](http://sml-family.org/Basis/char.html) には定義されていません。
しかし[`Substring`](http://sml-family.org/Basis/substring.html)にはあります。 [`getc`](http://sml-family.org/Basis/substring.html#SIG:SUBSTRING.getc:VAL) です。

``` sml
val getc : substring -> (char * substring) option
```

`Substring` は `string` の上に構築されたビューで、典型的な実装は `string` とその範囲を表わす整数2つで表現されます。`Substring` を作るには `string` から [`Substring.full`](http://sml-family.org/Basis/substring.html#SIG:SUBSTRING.full:VAL) で作れます。ちょっと動きを見てみましょう。

``` text
(* `Substring.full` で `substring` を作る *)
- val s = Substring.full "truetrue";
val s = - : substring

(* ↑の通り `substring` は綺麗に表示されないので自分で表示関数を作る（↓） *)
- val printSubstring  = print o Substring.string;
val printSubstring = fn : substring -> unit

(* `Substring.getc` を使ってみる *)
- case Substring.getc s of SOME((char, s)) => printSubstring s | NONE => ();
ruetrue
(* ↑ 先頭の `#"t"` が消費されて残りの `"ruetrue"` が返ってきてることが分かる。 *)
```


これで望んだ動きをしているようです。
なので `Substring.getc` を `Bool.scan` などの `scan` 関数に渡せばパースする関数を得られます。
少し大袈裟ですがこれをモジュールにしておきましょう。

```sml
structure SubstringCvt = struct
    fun scanWith scan = scan Substring.getc
end
```

ちょっと使ってみます。

``` text
(* `Bool.scan` と組み合わせて `bool` 値ろリーダーを作っておく *)
- val boolReader = SubstringCvt.scanWith Bool.scan;
val boolReader = fn : (bool,substring) StringCvt.reader

(* 前回同様に `substring` の文字列を用意しておく  *)
- val s = Substring.full "truetrue";
val s = - : substring

(* 読み出す。相変わらず `substring` は表示されてないが `"true"` が残っている *)
- boolReader s;
val it = SOME (true,-) : (bool * substring) option
```

因みに整数値のリーダーはパースするときの基数を指定するので引数が1つ多くになってます。

``` text
(* 10進数の整数値リーダー *)
- val intReader = SubstringCvt.scanWith (Int.scan StringCvt.DEC);
val intReader = fn : (int,substring) StringCvt.reader
```


## 固定文字列のリーダー

順番逆だろって気がしなくもないですが、値のリーダーが作れたので固定文字列のリーダーを作りましょう。
これは用意された仕組みがないので自分で作ります。
そのためのパーツを紹介します。
まずは固定文字列と前方一致しているか確認する [`Substring.isPrefix`](http://sml-family.org/Basis/substring.html#SIG:SUBSTRING.isPrefix:VAL) 。

``` sml
Substring.isPrefix: string -> substring -> bool
```


読み込んだ部分を切り取るのは [`triml`](http://sml-family.org/Basis/substring.html#SIG:SUBSTRING.triml:VAL) でできそうです。切り取る長さを指定して左から切り取ります。

```sml
val triml: int -> substring -> substring
```


これを組み合わせて以下です。

``` sml
fun readFixed s input =
    if Substring.isPrefix s input
    then SOME(s, (Substring.triml (String.size s) input))
    else NONE
```

## Scanf その1

リーダーが揃ったところでscanfを作っていきましょう。
いきなり実装を載せると以下です。

``` sml
structure Scanf = struct

    infix ++

    fun lit x s v = case readFixed x s of
                        SOME(_, rest) => SOME(v, rest)
                      | NONE => NONE
    fun eol s v = lit "\n" s v
    fun f1 ++ f2 = (fn s => (fn v => Option.mapPartial (fn (v, s) => f2 s v) (f1 s v)))
    fun int rdr = Int.scan StringCvt.DEC rdr
    fun bool rdr = Bool.scan rdr
    fun % scan s v1 = Option.map (fn (v2, s) => ((v1, v2), s)) (SubstringCvt.scanWith scan s)
    fun sscanf p s = p (Substring.full s) ()
    (*
end
```

基本は読み出すsubstring (`s`) と読み出した値 (`v`) を持ち回ってパースしています。パース結果が `option` に包まれているので `Option.map: ('a -> 'b) -> 'a option -> 'b option` や `Option.mapPartial: ('a -> 'b option) -> 'a option -> 'b option` が登場してますがあんまり気にしなくていいです。

そして今回も `%` の実装がキモになります。見ていきましょう。

``` sml
fun % scan s v1 = Option.map (fn (v2, s) => ((v1, v2), s)) (SubstringCvt.scanWith scan s)
```

引数に渡ってるのはリーダーの `scan` と読み出すストリーム `s` 、引き回している値 `v1` です。
関数本体ではまず `(SubstringCvt.scanWith scan s)` で値をパースして取り出しています。
そのあと `Option.map` で結果が成功だった関数を適用します。リーダは読み出した値 `v2` と、読み出した残り `s` を返すのでそれを返して終わりです。
返すときに `(v1, v2)` のように今までに読んだ値と今読んだ値1つのタプルにまとめています。

あとは `sscanf` ですが `p` にストリームの初期値 (`(Substring.full s)`) と値の初期値 (`()`) を与えているだけです。

これを使ってみましょう。


``` sml
- sscanf (lit "input: " ++ %int ++ lit ", " ++ %int) "input: 5, 17"
val it = SOME ((((),5),17),-) : (((unit * int) * int) * substring) option
```

一応読み出せています。一応と言ったのは返り値を見ると分かります。 `((((),5),17),-)` とちょっと渋いことになってますね。
SMLにはヘテロなリストはありませんしタプルの要素を増やすこともできないので仕方のない話ではあります。

しかし `printf` に渡す値はそうなってませんでしたよね。
カリー化された関数として複数の値を受け取っていました。
じゃあscanfも同様にして複数の値を返してみましょう。
つまり、カリー化された継続を受け取って複数の値を返すのです。

## Scanfその2

継続をユーザから受け取ることにした実装が以下です。

``` sml
structure Scanf2 = struct

    infix ++

    fun lit x s v = case readFixed x s of
                        SOME(_, rest) => SOME(v, rest)
                      | NONE => NONE
    fun eol s v = lit "\n" s v
    fun f1 ++ f2 = (fn s => (fn v => Option.mapPartial (fn (v, s) => f2 s v) (f1 s v)))
    fun int rdr = Int.scan StringCvt.DEC rdr
    fun bool rdr = Bool.scan rdr
    fun % scan s k = Option.map (fn (v, s) => (k v, s)) (SubstringCvt.scanWith scan s)
    fun sscanf p s = p (Substring.full s)
end
```

`lit` などはほぼそのまま受け流しているだけなので変わっていません。
変わったのは `%` の値のタプルを作っていた箇所だけです。
`(v1, v2)` となっていた部分が `k v` と継続に値を渡すように変化しています。多値の継続に1つづず値を埋めていくイメージです。
あとは地味に `sscanf` も値の初期値を取らなくなってますがまあそこはいいですかね。

これを使ってみましょう。

``` text
(* `sscanf` を呼ぶと継続を受け取るように型が変わっている*)
- sscanf (lit "input: " ++ %int ++ lit ", " ++ %int) "input: 5, 17"
val it = fn : (int -> int -> ?.X1) -> (?.X1 * substring) option

(* 継続として2つの値を受け取ってタプルにして返す関数を書くとちゃんと読み取れてる *)
- sscanf (lit "input: " ++ %int ++ lit ", " ++ %int) "input: 5, 17" (fn x => fn y => ((x, y)))
val it = SOME ((5,17),-) : ((int * int) * substring) option
```


ということでscanfも実装できました。めでたしめでたし。

# メタ

SMLのコードがハイライトされずに読みづらいのは今しばしお待ち下さい。そのうち解消されます。

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">hugoでSMLのコードがハイライトされないのにキレてhugo内部で使ってるハイライトエンジンにSMLサポート追加してもらった<a href="https://t.co/65SWaguBwp">https://t.co/65SWaguBwp</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1201413317413109761?ref_src=twsrc%5Etfw">December 2, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 
