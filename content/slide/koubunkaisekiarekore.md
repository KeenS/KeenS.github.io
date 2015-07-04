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
 + 渋谷のエンジニア
 + Lisp, ML, Shell Scriptあたりを書きます

# 構文解析はバッドノウハウ
## 本質はAST
### シンタックスシュガーは飾り
### DRY出来るなら別
## AST First
## S式を使え
## 目の前のパーサを使え
## まずDSLを考えろ
# LexerとParserを分ける意味
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">字句解析器手づくりの簡単さに対して構文解析器手づくりはわりと人を殺しにかかる</p>&mdash; gfn (@bd_gfngfn) <a href="https://twitter.com/bd_gfngfn/status/578908166785671168">2015, 3月 20</a></blockquote>

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">構文解析難し過ぎて酒飲んでる <a href="http://t.co/obf4utBcih">pic.twitter.com/obf4utBcih</a></p>&mdash; ろんだ (@fetburner) <a href="https://twitter.com/fetburner/status/606820143868411906">2015, 6月 5</a></blockquote>

# 正規表現の使いどころ
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">依存型のある言語でlexとか作ったら出てくるトークンの型変数に正規表現出てくるのかな</p>&mdash; ELD-R-ESH-2 (@eldesh) <a href="https://twitter.com/eldesh/status/597772476244885505">2015, 5月 11</a></blockquote>

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">「BNF = (大雑把に言って)正規表現+括弧の対応」というのはChomsky–Schützenbergerの定理という結構マニアックな定理(ドラゴンブックには載ってないと思う)をさらに僕なりに超訳したものなのであまり知られてないと思います．</p>&mdash; Ryoma Sin&#39;ya (@sinya8282) <a href="https://twitter.com/sinya8282/status/597465565654024192">2015, 5月 10</a></blockquote>

## Markdownの怪仕様
# 構文クラス
## LL
## LR
## 言語仕様の配慮
# 手書きとジェネレータとコンビネータ
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">パーサ手書きするのダサイよなぁ。クラスが分からなくなる。</p>&mdash; ELD-R-ESH-2 (@eldesh) <a href="https://twitter.com/eldesh/status/597751470834855938">2015, 5月 11</a></blockquote>
## ジェネレータの扱いにくさ
## DSLパーサジェネレータ
### Common Lisp
### D
# 複雑性と分かりやすさ
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">オーバーエンジニアリングを「あいつは力に溺れた」と言い変えていくといいと思う</p>&mdash; イカid:mizchi0x (@mizchi) <a href="https://twitter.com/mizchi/status/565662999063838720">2015, 2月 12</a></blockquote>

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">S式はどう考えても読み易い……</p>&mdash; Ocamlアイドル (@no_maddo) <a href="https://twitter.com/no_maddo/status/590528791677546496">2015, 4月 21</a></blockquote>


<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">}]))みたいなのを書いてると、括弧が一種類の言語、いいなぁ、と思ったりします。</p>&mdash; mzp (@mzp) <a href="https://twitter.com/mzp/status/587941717451481088">2015, 4月 14</a></blockquote>
## 機械、人間、エディタ
## S式を使え
# ソースロケーション保持法
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


</script>
</section>
