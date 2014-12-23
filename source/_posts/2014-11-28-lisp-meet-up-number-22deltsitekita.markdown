---
layout: post
title: "lisp meet up #22でLTしてきた"
date: 2014-11-28 23:17:12 +0900
comments: true
categories: [Lisp, Common Lisp, Lisp Meet Up]
---
κeenです。先日 Lisp Meet Up #22 でLTしてきたので報告です。
<!-- more -->

LTは事前に登録されたのが2件、私が当日17時くらいに登録したの1件で、参加は20人募集の内20人応募、20人参加と最近中々の人気を見せてます。

# Common Lispで高速なHTTPパーサーを書く(仮)
LT1件目は深町さんより、「Common Lispで高速なHTTPパーサーを書く(仮)」。fast-httpを作ったときの話。

<iframe src="//www.slideshare.net/slideshow/embed_code/42153462" width="425" height="355" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="//www.slideshare.net/fukamachi/writing-a-fast-http-parser" title="Writing a fast HTTP parser" target="_blank">Writing a fast HTTP parser</a> </strong> from <strong><a href="//www.slideshare.net/fukamachi" target="_blank">fukamachi</a></strong> </div>

[動画](https://docs.google.com/file/d/0B_H0_8eqWuVARnhncWZpRFdUOWM/preview)

[深町さんのブログ](http://blog.8arrow.org/entries/2014/10/23)に書かれていることの他、fast-httpが高速になるまでの経緯や実装方針などの話もありました。

最初ベースにしたnode.jsのHTTPパーサ、http-parseは状態を持っていて、1文字読む毎に状態を保存していた。次にベースにしたpicohttpparseは状態を持たず、HTTPリクエストが全部届く前にパースを始めてしまった場合はあきらめて最初からパースするようにしていた。fast-httpは1行パースする毎に状態を保存するようにした。など。

もう一つ、http-parseは`while`ループの中に巨大な`case`文があって、現在の状態で`case`でディスパッチし、その節の中で読んだ文字に依って現在の状態を変え、またループで先頭に戻って状態に依ってディスパッチするという手法だったそうです。Common Lispの場合は`case`が全て`(cond ((eql ..) ....) ...)`に展開されて遅い[^1]ので`while`と`case`じゃなくて`tagbody`と`goto`で実装したそうです。

[^1]: `cond`は先頭から順番に比較する仕様です。`case`は`cond`に展開されることが仕様で定められていた筈。

この手法は一般にDirect(Navive) ThreadingだとかThreaded Codeだとか呼ばれています。主にバイトコードインタプリタを実装する時にバイトコードに依るディスパッチの部分で使われるようです。Rubyの解説が丁寧だったようなので参考資料として置いておきます。

[Rubyist Magazine - YARV Maniacs 【第 3 回】 命令ディスパッチの高速化](http://magazine.rubyist.net/?0008-YarvManiacs)


会場からはアーキテクチャ依存の最適化はしないのかとの質問がありました。SBCLにはインラインアセンブラであるVOPなるものが存在するのでSIMD命令使えば、とかいう怖い話ですね。

# symbol tree diff
LT2件目はchikuさんより「symbol tree diff」。chikuさんが以前から取り組んでいるプログラムのdiffを構文レベルでとる話の進捗。

<iframe src="//www.slideshare.net/slideshow/embed_code/42160384" width="425" height="355" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="//www.slideshare.net/samugari/symbol-treediff" title="Symbol tree-diff" target="_blank">Symbol tree-diff</a> </strong> from <strong><a href="//www.slideshare.net/samugari" target="_blank">samugari</a></strong> </div>

[動画](https://docs.google.com/file/d/0B_H0_8eqWuVARUoxVGtiMlFrRVU/preview)

diff自体はとれるようになったようですが見せ方に問題があり、見易い形式を模索中のようです。会場に見易い形式は何か投げ掛けましたが良い案を見付けるのは難しいようです。

# Semantic S式
私から「Semantic S式」。括弧が多い方が嬉しいこともあるよねーというゆるい話。
[Semantic S式 | κeenのHappy Hacκing Blog](http://keens.github.io/slide/semantic-sshi.html)

[動画](https://docs.google.com/file/d/0B_H0_8eqWuVAQms2QkZDcnZfVlU/preview)

会場からは半分同意、半分ツッコみたいとの反応が。plistをリテラルから`getf`するやついないだろ、とか。

# 懇親会
私の発表で21時あたりだったのでそのまま解散して懇親会。隣にLand of Lispを読んでLispに興味持ってLispの授業をとってる方がいたのでどの本が入門に良いかなど。

VOPの話の続きもしました。どうしても資料がないのがネックだよねー、と。私の知っているのは

[How to Define New Intrinsics in SBCL - Paul Khuong mostly on Lisp](http://www.pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/)

や

[Hacking SSE Intrinsics in SBCL (part 1) - Paul Khuong mostly on Lisp](http://pvk.ca/Blog/Lisp/hacking_SSE_intrinsics-part_1.html)

かな。日本語のやってみた系だとg1さんの

[#:g1: SBCLでVOPを使ってみよう](http://g000001.cddddr.org/2011-12-08)

あたり。

# その他
テンプレートエンジンを作ってるって以前深町さんに話してたら「まだ出来ないんですか？」とさんざん煽られました。はい。頑張ります。
