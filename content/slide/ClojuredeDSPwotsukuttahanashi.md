---
categories: [Clojure, AdTech]
date: 2015-10-27T21:04:20+09:00
description: "Lisp Meet Upでの発表用。AdTech Studioでの研修でDSPをClojureで作った話。"
title: ClojureでDSPを作った話
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# ClojureでDSPを作った話
----------------------
Lisp Meet Up #3
<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバーエージェントのエンジニア
 + Lisp, ML, Shell Scriptあたりを書きます


# DSPとは
--------

* Demand Side Platform
* 広告主から広告を集めて広告の表示権を競り落し、適切な広告を出す
  0. 広告主を集めて
  1. 競りの通知を受け取って
  2. 一番コスパが良さそうな広告を選んで
  3. 入札して
  4. 落札通知を受け取る
  5. 落札したら広告を出す


 
# ひとりDSP
----------

* AdTech Studioの新卒研修
* 2ヶ月間、業務時間の20%を使ってDSPを作る
  + 実質8営業日
* 業務ロジックを理解することが目的
* 最後に新卒の作ったDSP同士で競争、利益が得点になる
* 他には最終発表での得点も
* **研修なので好きにやっていい**
  + Clojureで書くことに
* サーバーの他は広告主データと学習用データが与えられる



<iframe src="//www.slideshare.net/slideshow/embed_code/key/92I5tQt6q6IjII" width="425" height="355" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="//www.slideshare.net/prir/ss-35918532" title="日本におけるアドテク市場とサイバーエージェントのアドテク事業について" target="_blank">日本におけるアドテク市場とサイバーエージェントのアドテク事業について</a> </strong> from <strong><a href="//www.slideshare.net/prir" target="_blank">CyberAgent, Inc.</a></strong> </div>


# 作るもの
---------

* 競りの通知を受け取って入札するJSON REST API
* 落札の通知を受け取るJSON REST API
  + 割とシンプル
* 「一番コスパが良さそうな広告を選ぶ」部分は色々と工夫する
* 今回は広告を出すところまではやらない
* クリック情報は落札通知に入ってる


# b11dについて
-------------

* 今回作ったDSP
* [KeenS/b11d](https://github.com/KeenS/b11d)
* Clojure製
* 5日くらいで作った
* あまりゴテゴテしない方針
  + 今回パフォーマンスは無視していい
  + Clojureに慣れてないので学習コストも抑えたい
* NginXとAppとMySQL構成+α
* DBは綺麗に設計したい
* 1リクエスト毎にDBを引きにいく素敵仕様


# WAP
-----

* Compojure
* Sinatra likeなやつ
* ringの上に乗っかてる
* かなりシンプルな部類だがJSON REST API程度ならこれで十分
* ringミドルウェアのでJSON部分も抽象化
* 学習コストが低いので気軽に始められる


# JSONライブラリ
--------------

* ring.middleware.json/wrap-json-{body, response}
  + JSON->マップとマップ->JSON
  + bodyの方はキーがStringになるのが微妙
    - セキュリティ的に仕方ない
  + Content-Typeを指定しないと動かない罠
* cheshire
  + 事前データをインポートするのに使った
  + ringのJSONミドルウェアの依存なのでそのまま使った
  + 自然で使い易いAPI


# データベース接続
----------------

* java.jdbc
* JDBCのClojure向けラッパ?
* DataSourceを自分で渡すのでコネクションプールも簡単
* SQL手書きしたら良い感じにマップを返してくれる
* Storeはカラムと値の対応を手書き
* 便利マクロもいくつか


# データベース接続
----------------

* eager loading面倒問題
  + 入れ子オブジェクトを保存する方も面倒だった
  + ORMの便利さを実感。
* bulk insert難しい問題
  + 実行時可変長引数難しい
* ORMはXXXを使うかJavaのやつをそのまま使う?
* JavaはXMLさえ我慢すれば割と良い奴揃ってる
* しかしレスポンスはマップで欲しいかも


# 運用
-----

* warに固めてTomcat vs スタンドアロンなfat jar
* 今回はfat jarを選択
* スタンドアロンだとsupervisordとかでの管理が楽
* Tomcatの運用経験がない
* Jetty中々優秀らしい
* jstatを使ってMackerelでメトリクス取った


# 非同期化
---------

* core.async/goで手軽に非同期
* DBへの書き込みを非同期にした
* DB書き込みでエラーが出ても500にならない恐怖
* 最初の方で支払いを記録出来てなくて予算管理が死亡
  + 予算管理失敗ペナルティで、大分点数引かれたらしい
* goroutineでの例外処理どうやるんだろう。


# 開発環境
---------

* nREPL + Emacs + CIDER
* 補完の設定上手くいかなかった
* 諸々地味に不便
* 起動遅い
* Emacs側からnREPL立ち上げたい
* サーバーのライブリロード欲しい
* 正解が欲しい



# その他Clojureについて
---------------------

* コンスのCarを取るのに`hd`か`head`か`car`か分かんなかった
  + 結局どれでもなくて`first`だった
* 引数のパターンマッチが便利
  + JSON REST APIだとリクエストに何を期待してるのか分かりづらい
  + パターンマッチがドキュメントになる
* やっぱJavaのライブラリ使えるの便利


# 落穂拾い
---------

* デプロイにはシェルスクリプト
  + お家芸
* 「一番コスパが良さそうな広告を選んで入札」で統計か機械学習が必要
  + 1回表示した時の収益の期待値を計算する
* 今回は事前データの統計を使った
  + 理想的には機械学習。時間変化についていける
* In Appでオンライン学習出来るライブラリが思ったよりない
  + 分散処理向けの大袈裟なのが多い
  + Weka? liblinear-java?
* 後で考えたらIncanter使えばよかった


# まとめ
-------

* 給料貰いながらLisp書いた
* ClojureでDSP作った
* Clojureで機械学習したかったけど間に合わなかった


# 参考
------

* [Internal of b11d | κeenのHappy Hacκing Blog](//KeenS.github.io/slide/Internal_of_b11d/)

</script>
</section>
