---
categories: [構文解析, 言語実装]
date: 2015-08-08T04:16:50+09:00
description: "PEGと構文解析に関するアレコレの勉強会 Vol.1 あるいは構文解析手法勉強会
での発表資料。PEG以外の構文解析にまつわる話。
"
title: 構文解析にまつわる小話たち
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">

# 構文解析にまつわる小話たち
----------------------
[#peg_study](https://twitter.com/search?q=%23peg_study&src=typd&vertical=default&f=tweets)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバエージェントの新卒エンジニア
 + Lisp, ML, Shell Scriptあたりを書きます

===
# ウォームアップ

<!-- .slide: class="center" -->
===
# 構文解析はバッドノウハウ
-------------------------

* プログラム言語を使っているなら既にパーサはある
  + 文法も定義されてる
* 目の前のパーサを使え
  + パーサAPIがある言語もある(Lispとか)
* そうでなくても内部DSLを考えろ
  + 内部DSLで解決出来ないときだけ構文解析
===
# 本質はAST
-----------

* 結局はASTになればどんな文法でも同じ
* 文法はただの外皮、欲しいのはAST
* シンタックスシュガーは飾り
  + DRY出来るなら別
* S式を使え
  + ASTをそのまま書き下せる

===
# AST First
-----------

* 最初にASTを考える。そして文法を考える
* 何が欲しいのかイメージし易くなる
* 構文解析はAST生成の自動化。普段してないことを自動化するのは愚か。
* 早めに間違いに気付ける
  + `+`は二項演算子。じゃあ `&&` は？ `=` は？

Note: Lispだと+は関数、andはマクロ、setqはスペシャルフォーム

===
# 構文解析の流れ

<!-- .slide: class="center" -->
===
# 構文解析の流れ
---------------

1. Lexer generaterからlexerを作る
  + Lexとか
2. Parser generaterからparserを作る
  + yaccとか
  + BNFという記法
3. ソースファイルをlexerで処理してトークン化する
4. トークン列をparserで処理してASTを作る

===

```
         [Lexer]        [parser]
[Source]------->[Tokens]------>[AST]
```

<!-- .slide: class="center" -->
===
# LexerとParserを分ける意味
--------------------------

* (上向き構文解析だと分けないとつらい)
* 役割の分担
  + 困難は分割せよ
* 文字列をシンボル化して比較が高速に
* 思考のフレームワークとして

===

<blockquote class="twitter-tweet" align="center" lang="ja"><p lang="ja" dir="ltr">字句解析器手づくりの簡単さに対して構文解析器手づくりはわりと人を殺しにかかる</p>&mdash; gfn (@bd_gfngfn) <a href="https://twitter.com/bd_gfngfn/status/578908166785671168">2015, 3月 20</a></blockquote>

<!-- .slide: class="center" -->
===

# 正規表現の使いどころ
---------------------

* 高速な実装がある
* 部品化しにくい
* 括弧の対応とか入れ子構造(`if .. then .. else`とか)は扱えない
  + perlの正規表現は厳密には正規表現ではない
* 構文解析には向かない
  + 「マッチ」は出来ても「抜き出し」は面倒
* Lexerには向いてる
  + トークン自体末端の部品なので部品化する必要がない
===
<blockquote class="twitter-tweet" align="center" lang="ja"><p lang="ja" dir="ltr">依存型のある言語でlexとか作ったら出てくるトークンの型変数に正規表現出てくるのかな</p>&mdash; eld-r-esh-2 (@eldesh) <a href="https://twitter.com/eldesh/status/597772476244885505">2015, 5月 11</a></blockquote>

<!-- .slide: class="center" -->
===
<blockquote class="twitter-tweet" align="center" lang="ja"><p lang="ja" dir="ltr">「bnf = (大雑把に言って)正規表現+括弧の対応」というのはchomsky–schützenbergerの定理という結構マニアックな定理(ドラゴンブックには載ってないと思う)をさらに僕なりに超訳したものなのであまり知られてないと思います．</p>&mdash; ryoma sin&#39;ya (@sinya8282) <a href="https://twitter.com/sinya8282/status/597465565654024192">2015, 5月 10</a></blockquote>

<!-- .slide: class="center" -->

===
# 構文クラス

<!-- .slide: class="center" -->
===
# 構文クラス
------------

* 学術的だが知っておくと幸せになれる
* 文脈自由文法を解析するためのものを話す
  + 多くのプログラム言語は文脈自由文法
  + 正規言語 ⊂ 文脈自由文法
* 大きく分けると上向き構文解析と下向き構文解析
* 詳しくは[ドラゴンブック](http://www.amazon.co.jp/%e3%82%b3%e3%83%b3%e3%83%91%e3%82%a4%e3%83%a9%e2%80%95%e5%8e%9f%e7%90%86%e3%83%bb%e6%8a%80%e6%b3%95%e3%83%bb%e3%83%84%e3%83%bc%e3%83%ab-information-computing-a-v-%e3%82%a8%e3%82%a4%e3%83%9b/dp/478191229x)参照

===
# LL(1)
----

* 下向き
  + 再帰降下パーサ
* 定義した言語しか厳密に受け取らない
* 線形線形時間でパース可能
* 手書きに向く
* パーサーコンビネータとかも

===
# LR(1)
----

* 上向き
  + トークンをくっつけて構文要素に、構文要素をくっつけてさらに上の構文要素に…
* LL(n)⊂LR(n)
* LRそのものの解析は難しくて、いくつかサブクラスがある
  + 単純LR (SLR):  貧弱
  + 先読みLR (LALR): パーサジェネレータでよく使われる
  + 正準LR: 計算コストが高い。メモリ喰う
* 事前計算のコスト（面倒くささ）が高い
  + パーサジェネレータ

===
# LRパーサジェネレータ
-----

* 基本はBNF(Backus-Naur-Form)
* いくつか方式がある
  + 演算子順位解析も合わせる
    - EmacsのSMIEとか
  + BNFだけでやる
    - 別の言語も受理する可能性がある ドラゴンブック 上 p. 247
    - 普通は問題にならない

===
# LRの限界とか
-------------

* `-`の単項演算子と二項演算子の衝突
  1. SMLみたいに諦める(単項の`-`を`~`で表す)
  2. Lexerで区別する
* 左再帰で無限ループ
  + 手動でどうにか出来る
  + 自動でどうにかしてくれるジェネレータもある
* BNFの書き方によっては文法があいまいになる
  + `if .. then .. if .. then .. else ..` とか
  + 自動ではどうにもできないので気をつけるしかない

===
# BNFとパーサージェネレータの良さ
------------

* BNFは言語を定義する。
  + 言語仕様にも使われる
* 要は「仕様からプログラムを生成する」
* 宣言的

===
# 複数文法のサポートとグローバル変数の衝突
---------------------------------------

* 複数の文法をサポートしたい時がある
  + 独自記法と互換記法とか
* 雑なパーサジェネレータ/コンビネータを使っているとグローバル変数が衝突する
  + パーサライブラリの作者は配慮して下さい。

===
# 言語仕様の配慮
---------------

* LispはLL
* Java 1.0はLALR
  + 今はLALRではないらしい
* PrologはLALRだった気がする

===
# 非文脈自由文法
----------------

* 文脈を持つ（雑）
* ひねりなくパーサージェネレータ使っただけじゃ解析出来ない言語のこと。

===
## sedのs///
-----------

* 実は `s|||`のように区切文字は何でもいい
  + パスネームの置換に便利
* 対応関係が文脈で変わるので非文脈自由

===
## Markdown
-----------

* 元々のperlの実装は正規表現
  + 再帰を使っているので正規言語ではなく文脈自由文法
* GFMなどのTable記法はカラム数という文脈があるので非文脈自由
* Table記法をサポートする時は覚悟を持って。

===
# その他
--------

* 関数の仮引数の数と実引数の数の一致
* 変数の使用の前に変数宣言
* 要は構文解析で出来ることには限界がある。

===
# パーサの実際

<!-- .slide: class="center" -->
===
# 速度と手軽さ
--------------

* 外部DSLやコンフィグファイルだとパーサの速度は必要ない
  + メインループで使われないので起動時間にしか影響しない。
  + むしろ手軽に作れた方がいい
* HTTPだとかメインループで使うものはどうやってでも速くしたい
  + 労力は惜しまない
===
# 手書きとジェネレータとコンビネータ
----------------------------------

* 速度が必要ならジェネレータ
* LLでないならジェネレータ
* 手軽さが欲しいならコンビネータ
* 色々手を加えたいなら手書き…かも

===
<blockquote class="twitter-tweet" align="center" lang="ja"><p lang="ja" dir="ltr">パーサ手書きするのダサイよなぁ。クラスが分からなくなる。</p>&mdash; ELD-R-ESH-2 (@eldesh) <a href="https://twitter.com/eldesh/status/597751470834855938">2015, 5月 11</a></blockquote>

<!-- .slide: class="center" -->
===
# ジェネレータの扱いにくさ
--------------------------

* あんまり人気ない気がする
* 2回も前処理必要なのダサいよね
* 新たな文法覚える必要がある
* そもそも作るのにもコストが高い
  + 言語の文法に合わせたプリンタ
  + 拡張性持たせると厄介
* かといって正規表現はやめましょうね
===
# DSLパーサジェネレータ
----------------------

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">PEGが正規表現と違って辛いところは言語組み込みじゃないのとワンライナーに向かないことなんだよな… <a href="https://twitter.com/hashtag/peg_study?src=hash">#peg_study</a></p>&mdash; わかめ@TypeScriptカッコガチ (@vvakame) <a href="https://twitter.com/vvakame/status/629881217320550401">2015, 8月 8</a></blockquote> 

<!-- .slide: class="center" -->
===
# DSLパーサジェネレータ
----------------------

* あったらそれなりに人気出そう
* メタプログラミングが出来る必要がある
* Common Lisp
  + 作者自身作ってる途中でジェネレータとコンビネータを勘違いする
  + ドキュメントでジェネレータといってるのに実際はコンビネータだったりする
* D
  + なんか作ってる人いるらしい

===
# 複雑性と分かりやすさ
---------------------

<blockquote class="twitter-tweet" align="center" lang="ja"><p lang="ja" dir="ltr">オーバーエンジニアリングを「あいつは力に溺れた」と言い変えていくといいと思う</p>&mdash; イカid:mizchi0x (@mizchi) <a href="https://twitter.com/mizchi/status/565662999063838720">2015, 2月 12</a></blockquote>

<!-- .slide: class="center" -->

===
# 複雑性と分かりやすさ
---------------------
* パーサが複雑な文法に対応出来ても人間が追い付かない
* 周辺のサポートも必要になるのでやっぱりシンプルな方が良い。
  + LRよりLL
  + S式とかシンプルの極み
  + [Clojureシンタックスハイライター開発から考えるこれからのlispに必要なもの](http://www.slideshare.net/sohta/clojurelisp?ref=http://athos.hatenablog.com/entry/2015/07/29/222535)
* 「出来る」と「した方がいい」は別の話

===
<blockquote class="twitter-tweet" align="center" lang="ja"><p lang="ja" dir="ltr">S式はどう考えても読み易い……</p>&mdash; Ocamlアイドル (@no_maddo) <a href="https://twitter.com/no_maddo/status/590528791677546496">2015, 4月 21</a></blockquote>

<!-- .slide: class="center" -->
===

<blockquote class="twitter-tweet" align="center" lang="ja"><p lang="ja" dir="ltr">}]))みたいなのを書いてると、括弧が一種類の言語、いいなぁ、と思ったりします。</p>&mdash; mzp (@mzp) <a href="https://twitter.com/mzp/status/587941717451481088">2015, 4月 14</a></blockquote>

<!-- .slide: class="center" -->
===
# ソースロケーション保持法

<!-- .slide: class="center" -->

===
# ソースロケーション保持法
-------------------------

* エラーメッセージを出すためにはソースロケーションを保持する必要がある
* flymakeの情報: ファイル名、開始行/列 終了行/列 エラーメッセージ
  + 最悪これがあればどうにかなる
  + 「分かりやすい」メッセージはツールに任せる
* 字句解析だけでなく意味解析、さらにはつまるところコンパイルが終わるまで保存する必要がある
  + トークンやASTにメンバが増える
  + オブジェクト指向のカプセル化って素晴らしい

===
# 1. インクルード
----------------------

* トークンのデータに入れてしまうパターン
* `datatype token = Plus of int * int | Symbol of int * int * string` ...
* OOPなら自然だが函数型だとパターンマッチがつらくなる

===
# 2. ラップ
--------

* ロケーションのレコードでトークンをラップする
* `{start:int, end: int, token: token}`
* パターンマッチは少し楽になる
  + 多相レコードがないとそもそもレコードつらいけどな！！
* MLtonがやってるらしい

===
# 3. テーブル
-------------

* ロケーションテーブルを持って、トークンにはテーブルへのキーだけ持たせる
* トークンが軽くなるので速そう
* でも面倒そう

===
# エラー処理
<!-- .slide: class="center" -->
===
# エラーメッセージ
------------------------

* 一応ロケーションがとれればエラー箇所は出せる。
* メッセージの親切さはツールとヒューリスティックと根気
* clangとか頑張ってる

```
ERROR: expected tEnd before '<EOF>'
each do {}
          ^
```

===
# エラー回復
-------------

* シンタックスハイライターは壊れた文法も解析しないといけない
* 1回のコンパイルでできるだけ多くのエラーメッセージを出したい
* シンタックスエラーから回復したい

===
# Cの易しさ
-----------

* エラーがあってもセミコロンまで読み飛ばせば回復出来る
  + CやJavaは結構コンパイラが教えてくれる
* そういう言語設計も大事

===
# 拡張方法

<!-- .slide: class="center" -->
===
# リードマクロ
--------------

* トークンレベルの拡張
* 特定の「文字」がきた時にユーザ定義関数を使ってパースする
* リテラルをユーザが定義することが出来る
  + 正規表現リテラルとか
  + [Common Lispの正規表現](http://weitz.de/cl-interpol/)

===
# マクロ
--------

* ASTレベルの拡張
* LispとかScalaとかRustとか
  + Lispは自由度が高い
  + 関数マクロはないよりマシ程度
* ~衛生性~
* [マクロについて整理してみる | κeenのHappy Hacκing Blog](//KeenS.github.io/blog/2015/07/04/makuronitsuiteseirishitemiru/)

===
## Cのマクロ
------------

* プリプロセッサなのでコンパイラの拡張ではない
* プリプロセッサ自体レキサを持つ
  + パーサとレキサを分ける意味
* ASTに関知しないのでやりたい放題

===
# 中置演算子
-----------

* 新しい中置演算子と優先順位を定義できる言語は多い
  + Haskell, SML, Prolog…
* パーサをその場で書き換えるのは難しいので後で処理する
  + [\[コンパイラ\]\[Haskell\]\[OCaml\] Haskellのinfixの仕組み - mad日記](http://d.hatena.ne.jp/MaD/touch/20090108)
* シンタックスのプラグインをセマンティクスに入れてるのでちょっと無茶

===
# 中置演算子
-----------

* 人間が同時に覚えられるのは3つまで
  + 優先順位がいくつもあっても覚えられない
  + 優先度40とか900とか無理。
* 本質はAST
  + 文法に問題を抱えるくらいならS式を使え

===
# Coq
-----

Coqは謎のテクノロジーにより `Notation`を使えば新しい文法を定義出来る

```coq
Notation "'SKIP'" :=
  CSkip.
Notation "X '::=' a" :=
  (CAss X a) (at level 60).
Notation "c1 ; c2" :=
  (CSeq c1 c2) (at level 80, right associativity).
Notation "'WHILE' b 'DO' c 'END'" :=
  (CWhile b c) (at level 80, right associativity).
Notation "'IFB' e1 'THEN' e2 'ELSE' e3 'FI'" :=
  (CIf e1 e2 e3) (at level 80, right associativity).

Definition fact_in_coq : com :=
  Z ::= AId X;
  Y ::= ANum 1;
  WHILE BNot (BEq (AId Z) (ANum 0)) DO
    Y ::= AMult (AId Y) (AId Z);
    Z ::= AMinus (AId Z) (ANum 1)
  END.
```
  
===
# 雑な話題

<!-- .slide: class="center" -->
===
# テスト
-------

* ASTのテストは案外難しい
  + 微妙な仕様変更で結果が変わる
  + でも言語の動作には関係なかったり

1. あきらめる
2. 不屈の精神でテストを直し続ける
3. 木に対するクエリ言語を使う

===
# 先読みと副作用
--------------

* 先読みしてバックトラックすると副作用があった時に困る
  1. 副作用を許さない
  2. 副作用は自己責任
  3. 一旦先読みで正しいパスを記憶してから正しいパスでもう一度パース
</textarea>
