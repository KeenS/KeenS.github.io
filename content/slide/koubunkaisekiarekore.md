---
categories: [構文解析]
date: 2015-07-01T04:16:50+09:00
description: "PEGと構文解析に関するアレコレの勉強会 Vol.1 あるいは構文解析手法勉強会
での発表資料。PEG以外の構文解析にまつわる話。
"
draft: true
title: 構文解析アレコレ
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# 構文解析アレコレ
----------------------
[#peg_study](https://twitter.com/search?q=%23peg_study&src=typd&vertical=default&f=tweets)

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + AMoAd Inc. (サイバエージェント)
 + Lisp, ML, Shell Scriptあたりを書きます


# ウォームアップ

<!-- .slide: class="center" -->

# 構文解析はバッドノウハウ
-------------------------

* 目の前のパーサを使え
* まず内部DSLを考えろ
  + 内部DSLで解決出来ないときだけ構文解析

# 本質はAST
-----------

* 結局はASTになればどんな文法でも同じ
* シンタックスシュガーは飾り
  + DRY出来るなら別
* S式を使え
  + ASTをそのまま書き下せる


# AST First
-----------

* 最初にASTを考える。そして文法を考える
* 何が欲しいのかイメージし易くなる
* 構文解析はAST生成の自動化。普段してないことを自動化するのは愚か。
* 早めに間違いに気付ける
  + `+`は二項演算子。じゃあ `&&` は？ `=` は？

# 構文解析の流れ

<!-- .slide: class="center" -->

# 構文解析の流れ
---------------

1. Lexer generaterからlexerを作る
  + Lexとか
2. Parser generaterからparserを作る
  + yaccとか
  + BNFという記法
3. ソースファイルをlexerで処理してトークン化する
4. トークン列をparserで処理してASTを作る



```
         [Lexer]        [parser]
[Source]------->[Tokens]------>[AST]
```


# LexerとParserを分ける意味
--------------------------

* 役割の分担
  + 困難は分割せよ
* 文字列をシンボル化して比較が高速に
* 思考のフレームワークとして



<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">字句解析器手づくりの簡単さに対して構文解析器手づくりはわりと人を殺しにかかる</p>&mdash; gfn (@bd_gfngfn) <a href="https://twitter.com/bd_gfngfn/status/578908166785671168">2015, 3月 20</a></blockquote>

<!-- .slide: class="center" -->


<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">構文解析難し過ぎて酒飲んでる <a href="http://t.co/obf4utBcih">pic.twitter.com/obf4utBcih</a></p>&mdash; ろんだ (@fetburner) <a href="https://twitter.com/fetburner/status/606820143868411906">2015, 6月 5</a></blockquote>

<!-- .slide: class="center" -->


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

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">依存型のある言語でlexとか作ったら出てくるトークンの型変数に正規表現出てくるのかな</p>&mdash; eld-r-esh-2 (@eldesh) <a href="https://twitter.com/eldesh/status/597772476244885505">2015, 5月 11</a></blockquote>

<!-- .slide: class="center" -->

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">「bnf = (大雑把に言って)正規表現+括弧の対応」というのはchomsky–schützenbergerの定理という結構マニアックな定理(ドラゴンブックには載ってないと思う)をさらに僕なりに超訳したものなのであまり知られてないと思います．</p>&mdash; ryoma sin&#39;ya (@sinya8282) <a href="https://twitter.com/sinya8282/status/597465565654024192">2015, 5月 10</a></blockquote>

<!-- .slide: class="center" -->


# 構文クラス

<!-- .slide: class="center" -->

# 構文クラス
------------

* 学術的だが知っておくと幸せになれる
* 文脈自由文法を解析するためのもの
  + 多くのプログラム言語は文脈自由文法
  + 正規言語 ⊂ 文脈自由文法
* 大きく分けると上向き構文解析と下向き構文解析
* 詳しくは[ドラゴンブック](http://www.amazon.co.jp/%e3%82%b3%e3%83%b3%e3%83%91%e3%82%a4%e3%83%a9%e2%80%95%e5%8e%9f%e7%90%86%e3%83%bb%e6%8a%80%e6%b3%95%e3%83%bb%e3%83%84%e3%83%bc%e3%83%ab-information-computing-a-v-%e3%82%a8%e3%82%a4%e3%83%9b/dp/478191229x)参照


# LL
----

* 下向き
* 再帰降下パーサ
* 定義した言語しか厳密に受け取らない
* 線形線形時間でパース可能
* 手書きに向く
* パーサージェネレータとかも


# LR
----

* 上向き
  + トークンをくっつけて構文要素に、構文要素をくっつけてさらに上の構文要素に…
* LL⊂LR
* LRそのものの解析は難しくて、いくつかサブクラスがある
  + 単純LR (SLR):  貧弱
  + 先読みLR (LALR): パーサジェネレータでよく使われる
  + 正準LR: 計算コストが高い。メモリ喰う
* 事前計算のコスト（面倒くささ）が高い
  + パーサジェネレータ


# LRパーサジェネレータ
-----

* 基本はBNF(Backus-Naur-Form)
* いくつか方式がある
  + 演算子順位解析も合わせる
    - EmacsのSMIEとか
  + BNFだけでやる
    - 別の言語も受理する可能性がある ドラゴンブック p. 247
    - 普通は問題にならない


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

# 言語仕様の配慮
---------------

* LispはLL
* Java 1.0はLALR
  + 今はLRではないらしい
* PrologはLALRだった気がする


# 非文脈自由文法
----------------

* 文脈を持つ（雑）
* ひねりなくパーサージェネレータ使っただけじゃ解析出来ない言語のこと。


## sedのs///
-----------

* 実は `s|||`のように区切文字は何でもいい
  + パスネームの置換に便利
* 対応関係が文脈で変わるので非文脈自由


## Markdown
-----------

* 元々のperlの実装は正規表現
  + 再帰を使っているので正規言語ではなく文脈自由文法
* GFMなどのTable記法はカラム数という文脈があるので非文脈自由
* Table記法をサポートする時は覚悟を持って。


# その他
* 関数の仮引数の数と実引数の数の一致
* 変数の使用の前に変数宣言
* 要は構文解析で出来ることには限界がある。

# パーサの実際

<!-- .slide: class="center" -->

# 速度と手軽さ
--------------

* 外部DSLやコンフィグファイルだとパーサの速度は必要ない
  + メインループで使われないので起動時間にしか影響しない。
  + むしろ手軽に作れた方がいい
* HTTPだとかメインループで使うものはどうやってでも速くしたい
  + 労力は惜しまない

# 手書きとジェネレータとコンビネータ
----------------------------------

* 速度が必要ならジェネレータ
* LLでないならジェネレータ
* 手軽さが欲しいならコンビネータ
* 色々手を加えたいなら手書き…かも


<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">パーサ手書きするのダサイよなぁ。クラスが分からなくなる。</p>&mdash; ELD-R-ESH-2 (@eldesh) <a href="https://twitter.com/eldesh/status/597751470834855938">2015, 5月 11</a></blockquote>

<!-- .slide: class="center" -->

# ジェネレータの扱いにくさ
--------------------------

* あんまり人気ない気がする
* 2回も前処理必要なのダサいよね
* 新たな文法覚える必要がある
* そもそも作るのにもコストが高い
  + 言語の文法に合わせたプリンタ
  + 拡張性持たせると厄介


# DSLパーサジェネレータ
----------------------

* あったらそれなりに人気出そう
* メタプログラミングが出来る必要がある
* Common Lisp
  + 作者自身作ってる途中でジェネレータとコンビネータを勘違いする
  + ドキュメントでジェネレータといってるのに実際はコンビネータだったりする
* D
  + なんか作ってる人いるらしい


# 複雑性と分かりやすさ
---------------------

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">オーバーエンジニアリングを「あいつは力に溺れた」と言い変えていくといいと思う</p>&mdash; イカid:mizchi0x (@mizchi) <a href="https://twitter.com/mizchi/status/565662999063838720">2015, 2月 12</a></blockquote>

<!-- .slide: class="center" -->
# 複雑性と分かりやすさ
---------------------

* パーサが複雑な文法に対応出来ても人間が追い付かない
* エディタのサポートも必要になるのでやっぱりシンプルな方が良い。
  + LRよりLL
  + S式とかシンプルの極み


<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">S式はどう考えても読み易い……</p>&mdash; Ocamlアイドル (@no_maddo) <a href="https://twitter.com/no_maddo/status/590528791677546496">2015, 4月 21</a></blockquote>


<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">}]))みたいなのを書いてると、括弧が一種類の言語、いいなぁ、と思ったりします。</p>&mdash; mzp (@mzp) <a href="https://twitter.com/mzp/status/587941717451481088">2015, 4月 14</a></blockquote>

# ソースロケーション保持法

# ソースロケーション保持法
-------------------------

flymakeの情報
字句解析だけでなく意味解析、さらにはつまるところコンパイルが終わるまで保存する必要がある
## ラップ
## インクルード(OOP)
## テーブル
# エラー処理
## エラーメッセージ
### Mirah
## エラー回復
## 解析の続行と複数のエラーメッセージ
### Cの易しさ
## Clang
# 拡張方法
## リードマクロ
## マクロ
### 衛生性
## コンパイラマクロ
## Cのマクロ
## 中置演算子
## Coq
# 複数文法のサポートとグローバル変数の衝突

</script>
</section>
