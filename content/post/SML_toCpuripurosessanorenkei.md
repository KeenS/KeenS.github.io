---
categories: [ML, SML, SMLSharp, Advent Calendar Memorial 2015, Advent Calendar Memorial]
date: 2015-12-26T20:09:24+09:00
title: SML#とCプリプロセッサの連携
---
κeenです。Advent Calendarのためにネタやアイディアを用意したものの時間/場所的都合でAdvent Calendarとして出せなかったボツネタでも供養しようかと。
Advent Calendarが終わってしまったので投げやり気味ですね。
第3段はSML#とCプリプロセッサで連携する話。

SML#のC FFIを使ってるとマクロで定義された値などを触りたくなるのですが、触れないのでどうにかしようかと。
<!--more-->
発想自体は単純で、SMLファイルの中でCのヘッダをインクルードしてCプリプロセッサを呼んであげればどうにかなります。


# 簡単な例
[JITを作る話](//KeenS.github.io/blog/2015/12/12/sml_dejitwotsukurukaruihanashi/)の時のように、定数に触りたいとします。
その時、以下のように.smlファイルを記述して、

``` sml
(* 
#include<sys/mman.h>
*)

(* ====END_OF_HEADER==== *)
datatype dummy = $ of int
              
val $PROT_READ  = $ PROT_READ
val $PROT_WRITE = $ PROT_WRITE
val $PROT_EXEC  = $ PROT_EXEC
val $PROT_NONE  = $ PROT_NONE

```

以下のコマンドを実行します。

``` sh
cpp -pipe -E cpp_pre.sml | sed '1,/====END_OF_HEADER====/d;/^#/d' > cpp.sml 
```

ポイントを挙げると、

1. smlファイルに`#include`を書く。lintでエラーが出ないように`#include`はコメントで囲む
2. `include`をコメントで囲ったところで安全でない( インクルードしたファイルに'`*)`'、例えば `int f(char *);`とかが入ってるとコメントが終端される)ので、`include`の部分は後で削除する。
   そのための識別子として、`====END_OF_HEADER====`を置いておく(これも完全には安全ではないが比較的マシ)。
3. Cのマクロと同じ識別子を使おうとすると識別子も置換されてしまうので左辺にダミーで`$`をつけておく。スペースは空けない。
   逆に右辺は置換されてほしいので`$`の後にスペースを空ける。
4. 最後にCプリプロセッサを呼び、sedでヘッダ部分や無駄に追加されたCのコメントを除く。

ちょっと`$`の部分がこの方式だとintにしか使えないのでイケてませんがないよりマシでしょう。因みに処理結果はこうなります。


``` sml
datatype dummy = $ of int

val $PROT_READ = $ 
                   0x1

val $PROT_WRITE = $ 
                   0x2

val $PROT_EXEC = $ 
                   0x4

val $PROT_NONE = $ 
                   0x0
```

# `ifdef`

一旦発想さえ得てしまえば話は簡単です。次は`ifdef`を使う例です。

```sml
(* ====END_OF_HEADER==== *)


(* 
#ifdef Debug
*)
val debug = true
(* 
#else
*)
val debug = false

(* 
#endif
*)

```

これは

``` sh
cpp -pipe -E -DDebug ifdef_pre.sml | sed '1,/====END_OF_HEADER====/d;/^#/d' > ifdef.sml
```

と`Debug`を定義して処理すればifdef.smlの中身は


``` sml


(*

*)
val debug = true
(*






*)

```

となりますし、定義せずに

``` sh

``` sh
cpp -pipe -E ifdef_pre.sml | sed '1,/====END_OF_HEADER====/d;/^#/d' > ifdef.sml
```

と処理すれば


``` sml


(*





*)
val debug = false

(*

*)

```

となってどちらでもSMLとして有効な文法になります。`ifndef`だったり、`elif`があったとしても有効な文法のままです。

# まとめ

* CプリプロセッサでSMLファイルを処理する方法を提示したよ
* `ifdef`も使えるよ
* 関数マクロは使えないよ
