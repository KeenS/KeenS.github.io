---
categories: ["GC", "メモリ管理"]
date: 2018-09-09T09:34:53+09:00
title: "Thoughts on GCs"
---
数日Twitterで散発的に呟いていたことをまとめる。
<!--more-->

きっかけはkazuhoさんのツイート。
<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">細かいこと言うと、メモリを逐次解放するつもりがないなら、malloc呼ぶのは誤り。大きなブロックを確保して、先頭から順次使っていくのが正しい戦略です。処理が単純化し使用メモリ量が減り局所性が向上する結果、速度が向上する。世代GCやサーバ等多くのプログラムで用いられる一般的な手法です</p>&mdash; Kazuho Oku (@kazuho) <a href="https://twitter.com/kazuho/status/1037662186544328704?ref_src=twsrc%5Etfw">2018年9月6日</a></blockquote>

世代別GCの件は誤りで、コンパクションをするGCのことかな？

この「大きなブロックを確保して、先頭から順次使っていく」、まったくもって正しいんだけど話を単純化しすぎてて、実用上はいくつか問題が発生する。いつブロックを確保していつ解放するのだとか。
「サーバ等」ではリクエストを受け付けてからメモリを確保してレスポンスを返したら解放すればだいたい上手くいく。しかしそれでもリクエストローカルではないデータもあるしこういうカスタムなメモリアロケーションはCじゃないとできない。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">こういうリージョンベースアロケーティング、いろんな言語でやりたいんだけどそういうAPIがないのとたまに含まれる長寿オブジェクトをちゃんと扱えないといけないから難しい。たしかGHCには入ってたはず。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1037669950024871937?ref_src=twsrc%5Etfw">2018年9月6日</a></blockquote>

GHCにリージョンとかの名前で入ってた気がしたけど探しても見つからず。GHCのメモリにはみんな困ってるらしく、検索ノイズが大きかった。

下記の世代別GCはこれで合ってる。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">この状況を近似したのが世代別GCではあるんだけどいかんせんグローバルに走るのでリクエスト単位でみると全然近似できてない</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1037671958001766400?ref_src=twsrc%5Etfw">2018年9月6日</a></blockquote>


因みにH2Oはアロケーションを使い分けてるらしい。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">リクエストで使う文字列→リクエスト毎のプールで確保し、完了時にまとめて解放<br>同一H2接続のリクエスト間で共有するHPACK文字列→参照カウンタで確保しプールに登録。全てのリクエスト完了時に解放<br>POSTデータ→tmpfsにフォールバックするバッファに確保<br>H2接続を表現するオブジェクト→malloc/free</p>&mdash; Kazuho Oku (@kazuho) <a href="https://twitter.com/kazuho/status/1037665720761376769?ref_src=twsrc%5Etfw">2018年9月6日</a></blockquote>

CならH2Oのように適材適所のアロケーション戦略がとれるがやっぱり手動メモリ管理を放棄したGC付き言語だと厳しい。

また、今回の自分の話はサーバとかを念頭に置いていて、常にこれが最適とは限らない。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">アプリケーション毎に適したGC戦略があるんだけど残念ながら先に言語とGCがあってあとでアプリケーションが出来上がるので先出しジャンケンになってしまう。処理系作者にできるのはErlangみたいに用途を限定するかJVMみたいに色んなGCを書くくらいしかない。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1037673610331095040?ref_src=twsrc%5Etfw">2018年9月6日</a></blockquote>

ところでlldはメモリを開放しないというのをruiさんが度々アピールしているけどそれはGCのある言語でもできる。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">アプリケーション毎の適したGC戦略ってのはGCしないというのも含まれてて、バッチ処理で完走するまでにメモリが枯渇しないならGCしない方が速い。これはlldとかでも取られてる戦略。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1037673903894614019?ref_src=twsrc%5Etfw">2018年9月6日</a></blockquote>

本当にGCはない方がいい。

で、やっぱりGCのある言語でもH2Oのようにリクエスト毎にリージョンを作って破棄したい。色々考えるに、このリージョンは丁度fiberとかと寿命が一致するのではと思いつく。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">昨日のGC戦略の続き。最近Project Loomの件とかでfiberが気になってるんだけどそれに対応して階層的リージョンとか出来ないかな。リージョンは自身か祖先のリージョンのメモリしか触れない。そうするとfiber毎にリージョン作ってfiberと一緒に破棄が簡単に出来るようになる。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1037965731592196096?ref_src=twsrc%5Etfw">2018年9月7日</a></blockquote>

階層化したのはfiberに親子関係があるからと、長寿オブジェクトも親のリージョンに置いとけば寿命長く出来るだろみたいなノリ。もちろんfiberのリージョンの上にはスレッドのリージョンがあるしグローバルなリージョンもある。

しかしやはりAPIの問題がある。ところでここまででてきたリージョンは「一時的に用意されたメモリのカタマリ」くらいのゆるふわな用語だけど下記のregionはちゃんとした方のリージョン。(CF [リージョンについて](http://keens.github.io/blog/2015/12/09/ri_jonnitsuite/))

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">しかしこれはかなりメモリを明示的に扱えるような言語仕様じゃないとだめで、言語設計から考えないといけない。研究レベルだとλ_{Region}の変種になると思うけど実用言語だと存在しないかなぁ。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1037966194169372674?ref_src=twsrc%5Etfw">2018年9月7日</a></blockquote>

これは似たようなものがあったはず。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">ICFP 2016でstack based gcの話があった気がするけど全然覚えてないや</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1037966563280711680?ref_src=twsrc%5Etfw">2018年9月7日</a></blockquote>

今調べたらこれだった。
[Hierarchical memory management for parallel programs](https://dl.acm.org/citation.cfm?id=2951935)
まだ読んでないので自分の考えと近いかは分からない。


で、自分の考えが合理的か考えてみた。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">GCの話続き。fiberみたいな実行単位とメモリを結びつけるのはある程度合理的。メモリをアロケートするのも使うのもコードだから。それをもう少し細かくしたのが関数単位にしたスタック変数になる。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1038311836809015297?ref_src=twsrc%5Etfw">2018年9月8日</a></blockquote>

これは少し間違ってて、自分は親のメモリにもアクセスしようとしてるからスタック変数よりは柔軟になる。しかしやっぱりそれをやろうとするとリージョンになる。
つまりリージョンを前提にした言語設計になる。

結局、既存の言語に簡単には入らなそう。世知辛い。

余談

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">fiber毎とかもうちょっと言うとthread毎とかにメモリプールを持つと副産物としてfiber_local, thread_localが必要なくなる。thread_localの細かい仕様は覚えてないから完全互換ではないと思うけどやりたいことは大抵実現できるはず。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1038351975849459712?ref_src=twsrc%5Etfw">2018年9月8日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
