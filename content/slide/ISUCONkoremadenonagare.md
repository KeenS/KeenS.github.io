---
categories: [isucon]
date: 2015-09-04T08:47:05+09:00
description: "予備に作ったスライド"
title: ISUCONこれまでの流れ
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# ISUCONこれまでの流れ
----------------------
雑に書き殴った
<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + ISUCON3、 4の予選、本戦に出場（学生枠）

# ISUCON1 ブログ
---------------

* node perl ruby
* kazeburoの罠
* ブログのサイドバーが重いやつ
* 割と親切なルール


# ISUCON2 チケット予約サイト
---------------------------

* perl ruby node php java python
* 席ランダムにしてなくてもベンチマーカー通っちゃう
* 変更が1秒以内に反映されていること → 0.9秒毎に裏でバッチを回す
* JOINが3連になってる所があってみんなそこに引っ掛かったがボトルネックはそこじゃない
* ちょっとルールが雑になった


# ISUCON3予選 スニペット投稿サイト
--------------------------------
* go node perl python ruby php
* AWS1台
* Markdown変換がperl製の重いやつ
* AWSだとプロセスのフォークも重い
* titleの抜き出しも重い
* チェッカーが甘くてVarnishを導入しただけで点数爆上がり
* SQLの典型問題とかも
* workloadに気付かなかった人多数
* 学生枠が出来た
* ベンチマーカが通れば何でもアリな風潮


# ISUCON3本戦 画像SNS
--------------------
* go node perl python ruby
* オンプレ5台
* 画像の変換が重い
* 実は帯域もつらい
* 画像のチェッカがあってチェッカが通れば画質を落としても良かった
* 組長の罠
* ベンチマーカーに通れば何でもアリ
* 複数サーバーでのファイルの共有にWebDAVが盲点
* 点数計算の罠に嵌まる人多数
  + 基本点より追加点に目が眩む人がそれなりに
* 推測するな計測せよ
* nodeで初期パスワードが間違っているトラブル


# ISUCON4予選 銀行
--------------------------------------

* go node perl php python ruby
* AWS1台
* ワークロードを桁外れに指定するとベンチマーカーが走り続けるバグ
* ベンチマーカーにアクセスして行動パターンを解析した人が出る（その情報は使ってない。スポーツマンシップ。）
* テンプレートエンジンをやめて静的ファイル
* Go勢のon memory戦略が跋扈
* 「メモリに載せれば勝てるぜ」風潮
* 静的ファイルを返さない反則ギリギリのチューニング
* ベンチマーカーのチューニング
* Varnish潰し
* アプリをC++で書き換えて予選突破したチームも出る


# ISUCON4本戦 動画広告
----------------

* go perl php python ruby
* オンプレ3台
* 最初からRedisに動画が載ってる
* それ外したらスコア頭打ち(帯域がつまる)
* ベンチマーク同士が干渉しあう
* 実はCache-Controlをしたらスコアが爆上がり(上位2チームだけ気付いた)
* 3位のチームは外向きと内向きの2枚のNICを使って団子状のスコアから頭1つだけ出た


#ISUCON5予選 ???
----------------
* perl ruby node php python go java (scala)
* GCP 
* 今までの予選だとベンチマーカが競技者の手元に渡る問題があった。
* かといって外部からのベンチマークは安定しない。
* GCPならGoogleの謎の技術によって安定するらしい。
* @tagomoris 「もはやISUCONは予選で落ちる人の方が多くなった。その人達にとっては予選こそがISUCONなのだ」
  
</script>
</section>
