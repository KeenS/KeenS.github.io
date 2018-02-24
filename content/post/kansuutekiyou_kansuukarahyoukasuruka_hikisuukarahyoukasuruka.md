---
categories: [ML, SML, OCaml, 言語実装]
date: 2018-02-24T01:26:59+09:00
title: "関数適用、関数から評価するか？引数から評価するか？"
---

κeenです。簡単にいうと「OCamlが関数の引数を右から評価する件について」。

これは他人から聞いた話なのですがよく目にするので文章として纏めておきます。

<!--more-->

OCamlは仕様では評価順序は未定義とし(らしい)つつも現在の実装(4.06.0)は引数を右から評価します。

追記

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">丁度読んでいたので参考情報です。OCaml の関数適用の評価順序が not specified と名言しているのは、 <a href="https://t.co/0Zd6Cl7Qgy">https://t.co/0Zd6Cl7Qgy</a> の Function application の項だと思います。（識者の方によると仕様はないそうですが…）</p>&mdash; takl (@takl) <a href="https://twitter.com/takl/status/967230921874194432?ref_src=twsrc%5Etfw">2018年2月24日</a></blockquote>

/追記

```
(* OCaml *)
# let f x y = ();;
# f (print_endline "1st") (print_endline "2nd");;
2nd
1st
- : unit = ()
```

ただ、「右から評価」はやや不正確です。関数はカリー化されているので引数は各々1つです。
関数適用の`f x y`は優先順位を明確にすると`(f x) y`になるので正確には「関数適用は引数から評価」ということになります。
このことは以下のようにして確かめられます。

```
(* OCaml *)
# (print_endline "f"; f) (print_endline "1st") (print_endline "2nd");;
2nd
1st
f
- : unit = ()
```

因みにSMLだと仕様で関数から評価することになっています。

```
(* SML *)
# fun print_endline x = print (x ^ "\n");
val print_endline = fn : string -> unit
# fun f x y = ();;
val f = fn : ['a. 'a -> ['b. 'b -> unit]]
# (print_endline "f"; f) (print_endline "1st") (print_endline "2nd");
f
1st
2nd
val it = () : unit
```

詳しくは[仕様](https://github.com/SMLFamily/The-Definition-of-Standard-ML-Revised)のDynamic Semantics for the Coreから探して下さい。

一見、SMLの方が直観に沿った挙動ですが実装側からするとOCamlの挙動の方が嬉しいケースがあります。

特にOCamlやSMLの実装コードを読んだわけではないですが「概念上こういうケースがありうる」という例をいくつか紹介します。

# Uncurry Optimization
一般論としてcurry化された関数は複数回関数適用をするので遅いです。
そこでuncurry化する、つまり引数をまとめてタプルで渡すようにする最適化があります。

先程の関数をuncurry化してみます。すると印字される結果はどうなるでしょう。


```
(* OCaml *)
# let f(x, y) = ();;
# (print_endline "f"; f) ((print_endline "1st"), (print_endline "2nd"));;
2nd
1st
f
- : unit = ()

```

OCamlは変わらず


```
(* SML *)
# fun f(x, y) = ();;
val f = fn : ['a, 'b. 'a * 'b -> unit]
# (print_endline "f"; f) ((print_endline "1st"), (print_endline "2nd"));
f
1st
2nd
val it = () : unit
```

SMLも変わりません。

OCamlは少し意外ですがタプルも右から評価するので結果が変わらないのです。
恐らく関数適用を引数から評価することに合わせたのでしょう。

さて、ではこのuncurry化、任意の関数で行えるでしょうか。
答えはSMLは出来ませんがOCamlではできます。

SMLの方は1つ引数を受け取った後に副作用を起こしてからもう1つの引数を受け取る関数を書くと壊れてしまいます。

```
(* SML *)
(* uncurry化前 *)
# (fn x => (print_endline "mid"; fn y => ())) (print_endline "1st") (print_endline "2nd");
1st
mid
2nd
val it = () : unit
(* uncurry化後 *)
# (fn (x, y) => (print_endline "mid")) ((print_endline "1st"), (print_endline "2nd"));
1st
2nd
mid (* ←壊れた *)
val it = () : unit
```

一方OCamlでは

```
(* OCaml *)
(* uncurry化前 *)
# (fun x -> (print_endline "mid"; fun y -> ())) (print_endline "1st") (print_endline "2nd");;
2nd
1st
mid
- : unit = ()
(* uncurry化後 *)
# (fun (x, y) -> (print_endline "mid";  ())) ((print_endline "1st"), (print_endline "2nd"));;
2nd
1st
mid
- : unit = ()
```

問題は起こりません。

SMLは `f x y` は「`f`, `x`, `x` への適用, `y`, `y` への適用」 と引数の評価の間に関数適用の評価が入っているので問題になりますがOCamlでは「`y`, `x`, `f`, `x` への適用, `y` への適用」とフェーズが分かれているので問題が起きないのです。

# スタック渡しとレジスタ渡し
雑に説明します。

コンパイル言語でもスタックベースVMを使うことがあったりABIによってはスタックベースだったりしますね。
そういうときに可変長引数を扱うためだとかで引数の後ろをスタックに積む規約があります。
そういう規約の元では逆から評価した方が具合が良いでしょう。

逆にレジスタ渡しだと引数は左から順に並べた方が自然でしょう。

# レジスタ最小化
要は中間変数を出来る限り減らしたいという話です。

これはMLに限らない話ですが、マシンのレジスタは有限なのでコンパイラは頑張ってレジスタを減らそうとします。
コンパイル結果のマシン語の使用するレジスタは評価順序で変わります。
なので評価順序を好きにいじれると上手く減らせるのです。


`(a * b) - (c + d) / (e + f)`

のような式、何も考えず左から評価してみましよう。疑似マシン語です。

```
r1 <- load a
r2 <- load b
r1 <- r1 + r2 // a * b
r3 <- load c
r4 <- load d
r3 <- c + d  // c + d
r4 <- load e
r5 <- load f
r4 <- r4 + r5  // e + f
r3 <- r3 / r4  // (c + d) / (e + f)
r1 <- r1 - r3  // (a * b) - (c + d) / (e + f)
```

頑張って不要になったレジスタを再利用しましたがr5まで出てきたのでレジスタを5つ使ったことになります。

追記

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">r1 &lt;- load a<br>r2 &lt;- load b<br>r1 &lt;- r1 + r2 // a * b<br>r2 &lt;- load c<br>r3 &lt;- load d<br>r2 &lt;- c + d  // c + d<br>r3 &lt;- load e<br>r4 &lt;- load f<br>r3 &lt;- r3 + r4  // e + f<br>r2 &lt;- r2 / r3  // (c + d) / (e + f)<br>r1 &lt;- r1 - r2  // (a * b) - (c + d) / (e + f)<br>でr4までになりませんか?</p>&mdash; エヌユル (@ncaq) <a href="https://twitter.com/ncaq/status/967287000100306944?ref_src=twsrc%5Etfw">2018年2月24日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

/追記


今度は`-`の右辺から計算してみます。

```
r1 <- load c
r2 <- load d
r1 <- r1 + r2  // c + d
r2 <- load e
r3 <- load f
r2 <- r2 + r3  // e + f
r1 <- r1 / r2  // (c + d) / (e + f)
r2 <- load a
r3 <- load b
r2 <- r2 * r3  // a * b
r1 <- r2 - r1  // (a * b) - (c + d) / (e + f)
```

r3までしか使っていないのでレジスタは3つしか使用していません。


これは`-`の右辺の方が使用する引数が多いのでこうなったまでで、一般に右から評価した方がいいというわけではありません。
仕様上は評価順序は「式に依って異なる」、つまり未定義が最適でしょう。

# まとめ

いくつか「なぜ引数の評価が右からなのか」「なぜ仕様では未定義だと嬉しいのか」の理由になるような例を示しました。
あくまで考えられる一例で、必ずしもOCamlがこの理由で現状になっているのを保証するわけではありません。

OCamlはVMコンパイラとネイティブコンパイラを持ち、さらにネイティブコンパイラは複数アーキテクチャをサポートするのでそれぞれで最適な評価順序が異なります。
恐らくですがその兼ね合いもあるのでしょう。
評価順序が未定義でも全然驚きはありませんし、むしろ少なくとも手元のx86_64ではVMとネイティブ両方で同じ評価順序なのはよく出来ていると思います。

因みにSMLでcurry化した関数を何度も関数適用しているかというとそうでもなく、いくつかの最適化でちゃんと関数呼び出しは消えるようです。

まとまりに欠きますが仕様上は評価順序が未定義でも理由があるんだよという話でした。
