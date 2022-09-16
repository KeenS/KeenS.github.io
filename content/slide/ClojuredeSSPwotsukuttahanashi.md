---
categories: [Clojure, Lisp, AdTech]
date: 2016-02-22T20:19:11+09:00
description: "Lisp Meet Upでの発表用。アドテクコンペのためにSSPをClojureで作った話。"
title: ClojureでSSPを作った話
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# ClojureでSSPを作った話
----------------------

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバーエージェントのエンジニア
 + Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# SSPとは
-------
※今回作ったものの話なので実際は少し違う

* Supplier Side Platform
* メディアからの広告リクエストを受けて表示権をオークションに掛け、落札された広告を表示する
  0. 広告リクエストを受け取って
  1. 各DSPに競りの通知を投げて
  2. 入札を受け取って
  3. 落札者と落札価格を決めて
  4. 落札通知を出す

===
# アドテクコンペ
---------------

* [ビジネスモデルもエンジニアリングも学べるアドテクスタジオの育成プログラムとは | 株式会社サイバーエージェント](https://www.cyberagent.co.jp/techinfo_detail/id=11380)
* サイバーエージェントの学生向けインターン
* 3日間、3、4人のチームで **DSP** を作る
* 学生は7チーム、1チームあたり2000q/sを捌くことになってる
* 学生のDSPを繋ぐための **SSP** が必要になったので作ることに。

===
<iframe src="//www.slideshare.net/slideshow/embed_code/key/92I5tQt6q6IjII" width="425" height="355" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="//www.slideshare.net/prir/ss-35918532" title="日本におけるアドテク市場とサイバーエージェントのアドテク事業について" target="_blank">日本におけるアドテク市場とサイバーエージェントのアドテク事業について</a> </strong> from <strong><a href="//www.slideshare.net/prir" target="_blank">CyberAgent, Inc.</a></strong> </div>

===
# 作るもの
----------

* 各DSPに競りの通知(HTTPリクエスト)を投げる大規模HTTPクライアント
  + 各DSPが2000q/s x 7チーム + 落札通知 = 16000q/s
  + 丁度2000q/sになるような制御も必要
* 入札を受けてのオークション、結果のロギングなど
* 管理画面

===
# s7pについて
-------------

* 今回作ったSSP
* [KeenS/s7p](https://github.com/KeenS/s7p)
* Clojure製
* 3日くらいで作った
* やや粗い部分も

===
# 今日話すこと
--------------

* なぜClojureか
* 16000q/s出すための工夫
* 16000q/sに抑えるための制御
* 運用して困った話とか

===

# なぜClojureか

<!-- .slide: class="center" -->

===
# なぜClojureか
---------------

* リクエストの数が多いので非同期IOは必須
* 100msでタイムアウトする仕様なのでタイムアウト処理も
* もともとはScala(akka)で作ったs6pがあった
* Actorの設計が良くなかったので遅かった
* あと非同期HTTPクライアントにタイムアウトがなかった
* プロダクションコードを流用したのでインターンが終わった後公開出来なかった
* 別言語で書き直すにあたってGo, Clojure, Common Lisp, Rust, Erlangが検討された
===
# なぜClojureか
---------------

* Common Lisp: 一番慣れてるが、非同期HTTPクライアント(とfutureライブラリ)が使いづらそうなのでやめた
* Rust: とりあえずパフォーマンスは出そうだし使いたかったが非同期HTTPクライアントが見付からなかった
* Clojure: そこそこ慣れてるし速そうな非同期HTTPクライアントがあった。core.asyncで非同期プログラミングもし易い
* Erlang: 恐らく一番向いてるが、ほとんど経験がない
* Go: 結構向いてそうだがあまり経験がないのでClojureを選んだ

===
# 16kq/s出すための工夫
<!-- .slide: class="center" -->
===
# 16kq/s出すための工夫
------------------------
非同期IO（一部同期しちゃったけど）

``` clojure
(defn work [test req result]
  (->> @dsps
       (sequence (comp
       ;; 全DSPにPOSTしてから(timeout 100ms)
                  (map (fn [dsp] {:dsp dsp :response (http/post (:url dsp) (json-request-option req))}))
       ;; 結果を待ち合わせる
                  (map destruct)
                  ...))
       ...))
    ....
```

===
# 16kq/s出すための工夫2
------------------------
一部同期したのでとにかくスレッド。スレッドへのディスパッチはチャネルで一発解決。

``` clojure
(defn worker [c]
  (thread
   ...))
```

```clojure
(defn make-workers [ch n]
  (doall (map (fn [_] (core/worker ch)) (range n))))
```

``` clojure
 workers (manage/make-workers ch 1024)
```

===
# 16kq/s出すための工夫3
------------------------
ZeroMQを使ったmaster-slave構成
(最終的にSlave18台)

![master-slave構成図](/images/s7p/s7p.svg)


===
# <span style="font-size: 90%">16kq/sに抑えるための制御</span>
<!-- .slide: class="center" -->
===
# <span style="font-size: 90%">16kq/sに抑えるための制御</span>
---------------------------

* DSPの買い付け能力というビジネス的な問題ではなくサーバの負荷という技術的な問題による制御
* 1秒で160kクエリ投げて9秒休むとかは出来ない。もう少し細かく制御する必要がある。
* しかし制御をあまり細かくしすぎると今度はそこで遅くなりそう。
* リクエストを実際に投げるSlaveは分散してるけど統一した制御が必要

===
# <span style="font-size: 90%">16kq/sに抑えるための制御</span>
---------------------------
Master側で100ms毎にに200個だけZeroMQのキューに積む

![QPS制御の図](/images/s7p/qps_control.svg)

===
# <span style="font-size: 90%">16kq/sに抑えるための制御</span>
---------------------------
実装にはcore.asyncの `chan` と `timeout` を使用。(非同期プログラミング便利!)

```Clojure
(defn start-query [sender reqs]
  (let [timer (timer 100)]
    (go-loop []
      (let [t (<! timer)
            took (take @qp100ms @reqs)]
        (doall
         (doseq [req took]
           (zmq/send-str sender (json/generate-string req))))
        (swap! reqs #(drop @qp100ms %))
        (if (and t (not (empty? took)))
          (recur)
          (println "request done"))))
    timer))
```

===
# 運用して困った話とか
<!-- .slide: class="center" -->

===
# 運用して困った話とか
---------------------
## <span style="font-size: 50%">レスポンスが遅いDSPにSSPのパフォーマンスが引き摺られる</span>
* DSPが速ければ余裕を見ても10台あれば十分だった
* DSPのパフォーマンスに仮定をおけないので上限ギリギリの18台
* 同期: 「DSP作ってる時はちょっとくらい待ってくれよ、って思ってたけどSSP運用したら遅いDSPガンガン切りたくなる気持分かった」
===
# 運用して困った話とか
---------------------
## パフォーマンス検証が大変
* リクエストを捌くサーバのパフォーマンスをどうするか悩む
* サーバをチューニングすればSSPも速くなるが、検証にならない
* かといって投げたクエリを捌けないサーバに対して検証する訳にもいかない
* 結局安全側に倒したパフォーマンス見積りに。

===
# 運用して困った話とか
---------------------
## <span style="font-size: 80%">Masterがログ吐きすぎてDisk Full</span>
* 最初の予定ではMasterはディスクをそんなに使わなかったのでディスクの小さいインスタンスだった
* ディスクスペース空けてMaster再起動で復旧。焦った。
===
# 運用して困った話とか
---------------------
## <span style="font-size: 60%">timeoutし続けるDSPがいてSlave完全沈黙</span>
* コネクションのクローズ待ちで固まってた
* 該当DSPを切った上でSlaveの再起動で復旧
  * カーネルのチューニングかtimeout頻度の検知が必要そう
* timeoutを考慮に入れた構成にしてた筈なので想定外だった
===
# まとめ
---------

* ClojureでSSP作ったよ
* Clojure使えば非同期プログラミングが簡単に出来るよ
* 運用って大変だよ

===
# 参考
-------

* [KeenS/s7p: sexp version of s6p; a toy SSP.](https://github.com/KeenS/s7p)
* [ClojureでDSPを作った話 | κeenのHappy Hacκing Blog](http://keens.github.io/slide/ClojuredeDSPwotsukuttahanashi/)
* [KeenS/b11d: A toy DSP](https://github.com/KeenS/b11d)
* [アドテクコンペ | 株式会社サイバーエージェント](https://www.cyberagent.co.jp/recruit/fresh/program_detail/id=11303&season=2016)
* [ビジネスモデルもエンジニアリングも学べるアドテクスタジオの育成プログラムとは | 株式会社サイバーエージェント](https://www.cyberagent.co.jp/techinfo_detail/id=11380)

</textarea>
