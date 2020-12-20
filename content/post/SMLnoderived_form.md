---
categories: [SML, ML Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-20T22:04:47+09:00
title: "SMLのDerived Forms"
---
このエントリは[ML Advent Calendar 2020](https://qiita.com/advent-calendar/2020/ml)の21日目の記事です。
前はelpinalさんで「長さn以上のリストをファンクタで」、後はnymphuimさんで「OCaml の小ネタを書く」です。

κeenです。小ネタなんですが、SMLのDerived Formsについて書きます。

<!--more-->
# Derived Formsとは

The Definition of Standard ML (revised)では基本文法の他にAppendix AでDerived Forms（派生形式）というのが定義されています。平たくいうと糖衣構文ですね。

普段SMLのコードを書く上ではあまり意識する必要はないのですが、SMLのコンパイラを作るときにはよく確認することになります。そしてDerived Formsを見ると「へー、XXって実はYYの構文糖衣だったんだ」と気付くことがあります。この記事ではそういう「へー」を紹介しようかなと思います。

# 意外なDerived Forms
## タプル

これは有名ですかね。タプルはラベルが数値なレコードです。`()` は `{}` です。


```sml
- () = {};;
val it = true : bool
- {1 = "one", 2 = "two"};;
val it = ("one","two") : string * string
```

ただしラベルは1はじまりの2以上なので `{1 = "one"}` に対応するタプルはありません。

``` sml
- {1 = "one"};
val it = {1="one"} : {1:string}
```


余談ですが SML# で `() = {}` を試したらエラーになりました。

``` sml
SML# 3.6.0 (2020-05-29 09:58:49 JST) for x86_64-pc-linux-gnu with LLVM 10.0.1
# () = {};;
(interactive):1.0-1.6 Error:
  (type inference 026) operator and operand don't agree
  operator domain: unit * unit
          operand: unit * {}
# {};;
val it = {} : {}
# ();;
val it = () : unit
```


仕様違反なのでバグですかね。

## ラベル
タプルの要素は `#n` で、レコードの要素は `#label` で取り出せますね。これもDerived Formです。

`fn {label = tmp, ...} => tmp` へと展開されます。

# `andalso` と `orelse` を最後まで展開すると…

`exp1 andalso exp2` はDerived Formです（`orelse` も同じ、以後同様）。
Derived Formsを展開した結果さらにDerived Formsが現われたらまた展開することになっています。最後にはコア言語に到達して展開が停止します。では `exp1 andalso exp2` をコア言語まで展開したらどんな式になるか分かりますか？

* `exp1 andalso exp2` は `if exp1 then exp2 else false` へと展開されます。
* `if exp1 then exp2 else exp3` は `case exp1 of true => exp2 | false => exp3` へと展開されます。
* `case exp of match` は `(fn match)(exp)` へと展開されます。

これらを総合すると

1. `exp1 andalso exp2`
2. → `if exp1 then exp2 else false`
3. → `case exp1 of true => exp2 | false => false`
4. → `(fn true => exp2 | false => false)(exp1)`

と展開され、最後には `fn` とその適用になります。
そもそも `fn` は `fn pattern1 => expr1 | pattern2 => expr2 ...` という構文なんですがご存知でしたか？

## セミコロン

`let ... in expr end` の `expr` の部分ではセミコロンで区切って複数の式が書けます。

``` sml
- let in print "hello"; print ", world\n" end;;
hello, world
val it = () : unit
```

このセミコロンもDerived Formです。
`let ... in expr1; expr2; ...; exprn; exp end` は `let ... in (expr1; expr2; ...; exprn; exp) end` へと展開されます。
そして `(expr1; expr2; ...; exprn; exp)` もDerived Formです。これはちょっとびっくりですが、`case` 式の連鎖へと展開されます。

``` sml
case expr1 of (_) =>
case expr2 of (_) =>
...
case exprn of (_) => exp
```

この後 `case` が `fn` へと展開されていきます。展開結果を見るのがちょっと怖いDerived Formですね。

## ファンクタ

逆に「あ、そんな構文あったんだ」となるのがファンクタの引数。
ファンクタを適用するときにこんなコード書きますよね？

``` sml
structure CharHashTable = HashTable(struct
  val eq = Char.eq
  val hash = Char.hash
end
)
(* 適当コード、多分コンパイルは通らない *)
```

実は `struct` と `end` はなくてもOKです

``` sml
structure CharHashTable = HashTable(
  val eq = Char.eq
  val hash = Char.hash
)
(* 適当コード、多分コンパイルは通らない *)
```

structureの構文を書いたら `struct` と `end` が補われ、 `Struct` とstructureの名前を書いたらそのまま使われる仕組みになってます。パーサが大変そうですね。

## `it`

SMLでREPLを使うとき、直前の式の評価結果に `it` を束縛してくれるので次の入力で直前の結果を `it` として参照できますよね？

``` sml
- 1 + 2;;
val it = 3 : int
- it * 3;;
val it = 9 : int
```

実はこれも仕様で定められたDerived Formです。

書き換え規則は `exp;<program>` → `val it = exp;<program>` です。
REPLで式を直接入力できる理由が分かりましたね。

仕様にあるのでREPLだけでなくファイルでも使えます。
流石に `it` を使う人はいないかと思いますが、トップレベルに式を直接書いてそのまま実行されるのを期待する人は多いんじゃないでしょうか。

以上、SMLのDerived Formsの紹介でした。
