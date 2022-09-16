---
categories: [Clojure, Lisp, transducer]
date: 2015-06-03T22:29:50+09:00
description: "朝の3分スピーチ用"
title: Transducerについて
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Transducerについて
----------------------
サイバーエージェント  
朝の3分スピーチ
<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 渋谷のエンジニア
 + Lisp, ML, Shell Scriptあたりを書きます
===
# Transducerって？
------------------

* Clojure 1.7で入るフレームワーク
* 関数のパイプライン化を簡単にする
* 入力、出力には依存しない

===
# Reduce関数について
-------------------

* `reduce: ('a -> 'b -> 'a) 'a -> 'b list`
* `('a -> 'b -> 'a)` でシーケンスを集約する
* `(reduce + 0 '(1 2 3)) ;=> 6`
* 要はreduceは関数を使って集約する。
===
# Transducerについて
-------------------

* `transduce:(('a -> 'b -> 'a) -> ('a -> 'b -> 'a)) ('a -> 'b -> 'a) 'a -> 'b list`
* 集約関数を変換する関数もとる
* `(transduce xf + 0 '(1 2 3))`
* transducerで集約する前に処理を挟める
===
# すごいところ
-------------------------
## コンポーサビリティ

* `(('a -> 'b -> 'a) -> ('a -> 'b -> 'a))`
* 変換関数は入力と出力が同じ。
* つまり変換関数を合成出来る
* ex) `(comp (filter odd?) (map inc))`

===
# すごいところ2
--------------------------
## 汎用性

* 各関数は入力や出力について知る必要はない
  + シーケンスではなく要素に対して定義される
* シーケンスだけでなく遅延シーケンス、ストリーム、チャネルなどなどにも使える
* 中間シーケンスを作らない
  + 遅延シーケンスやストリームには大きなメリット
* 集約先をシーケンスなどにすることも出来る
  + 関数の汎用性が上がる
===
# すごいところ3
--------------
## 並列/非同期

* core.asyncもtransducerをサポート
* 各変換関数を並列/非同期に実行が可能
* さらに各関数に割り当てるプロセッサの数も細かく制御出来る

===
<span style="font-size: 500%;">変換して</span>

<!-- .slide: class="center" -->
===
<span style="font-size: 500%;">集約する</span>

<!-- .slide: class="center" -->
===
<span style="font-size: 300%;">何かに似てない？</span>

<!-- .slide: class="center" -->
===
<span style="font-size: 600%;">MapReduce</span>

<!-- .slide: class="center" -->
===
<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/oza_x86">@oza_x86</a> ozaさんの近しい環境というとHadoop...多段Mapreduceをtransdecerみたいにしようってはなしとかですか？</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/605368356393218048">2015, 6月 1</a></blockquote>

<!-- .slide: class="center" -->
===
<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> まさにその通りです．MapReduce には Combiner という中間集約をする API があるのですが，それをもっと汎用的にしようという話です． <a href="http://t.co/injEiPjePG">http://t.co/injEiPjePG</a></p>&mdash; oza (@oza_x86) <a href="https://twitter.com/oza_x86/status/605369771505221633">2015, 6月 1</a></blockquote>

<!-- .slide: class="center" -->
===
<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/oza_x86">@oza_x86</a> すごい楽しそうですね。Clojure向けのAPIがtransducerのバッグエンドになって普段書いてるコードがそのままHadoopで動いたりしたら、とか夢が無限に広がります</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/605370461476646912">2015, 6月 1</a></blockquote>

<!-- .slide: class="center" -->
===
<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> おお，その案は良いですね！！</p>&mdash; oza (@oza_x86) <a href="https://twitter.com/oza_x86/status/605370619509637121">2015, 6月 1</a></blockquote>

<!-- .slide: class="center" -->
===
<span style="font-size: 200%;">Clojure 1.7 + Transducer</span>

<!-- .slide: class="center" -->
===
<span style="font-size: 400%;">Comming Soon</span>

<!-- .slide: class="center" -->
===
# 参考
------
* [Clojure 1.7のtransducersを動かしてみよう - Qiita](http://qiita.com/tokomakoma123/items/1ca3fb0dddc5b901b032)
* [core.async+transducers Shibuya.lisp #21](http://www.slideshare.net/ktsujister/coreasynctransducers-shibuyalisp-21)
* [Hadoop Combinerメモ(Hishidama's Hadoop Combiner Memo)](http://www.ne.jp/asahi/hishidama/home/tech/apache/hadoop/Combiner.html)
</textarea>
