---
categories: [ML, ML Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-28T12:09:20+09:00
title: "SMLの処理系組み込み例外ってなーんだ"
---

このエントリは[ML Advent Calendar 2020](https://qiita.com/advent-calendar/2020/ml)の17日目の記事です。空いてるのも寂しいので埋めます。

<!--more-->


タイトルの通りなんですが、もうちょっと説明しますね。
まず「処理系組み込み」は実装によるので正しくないです。
厳密にいうと仕様に載っている例外ですね。
SML '97のAppendix C, DにInitial Basisが載っていて、そこで定義されている例外はなんでしょうというクイズです。

これは知識でも解けますが、落ち着いてよく考えると導き出せます。
例えば `Subscript` は配列の境界外アクセスのときに出る例外ですがそれって仕様で必要なんだっけ…とか。

答えは

<details>

<code>Bind</code> と <code>Match</code> です。それそれ <code>val</code> と <code>case</code> (<code>fn</code>) でパターンマッチに失敗したときにでる例外です。

<pre class="chroma"><code class="language-text" data-lang="text">- val NONE = SOME 1;;

uncaught exception Bind [nonexhaustive binding failure]
  raised at: stdIn:1.6-1.19
</code></pre>



<pre class="chroma"><code class="language-text" data-lang="text">- case NONE of SOME x => x;;
stdIn:1.2-1.26 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
stdIn:1.2-1.26 Warning: match nonexhaustive
          SOME x => ...


uncaught exception Match [nonexhaustive match failure]
  raised at: stdIn:1.26
</code></pre>

これらはSMLの文法に組込まれているのでどうしようもないですね。

</details>
