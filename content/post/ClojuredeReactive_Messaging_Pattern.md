---
categories: [Lisp, Clojure, Advent Calendar, Advent Calendar 2015, Lisp Advent Calendar]
date: 2015-12-19T22:30:19+09:00
title: ClojureでReactive Messaging Pattern
---

このエントリは[Clojure Advent Calendar 2015](http://qiita.com/advent-calendar/2015/clojure)の16日目の記事です（大遅刻）

κeenです。ここのところ体調優れず、また、特に面白いネタもなくClojure Advent Calendarに遅刻したことをお詫び申し上げます。

さて、今日はReactive Messaging PatternをClojureのcore.asyncでやってみたいと思います。Reactive Messaging Pattersは、Java/Scala向けActorシステムの[Akka](http://akka.io/)の作者が著した本、[Amazon.co.jp: Reactive Messaging Patterns with the Actor Model: Applications and Integration in Scala and Akka](http://www.amazon.co.jp/Reactive-Messaging-Patterns-Actor-Model-ebook/dp/B011S8YC5G)で書かれているデザインパターンです。

非常に興味深い本なのですがScala及びAkka初心者の私には中々辛いので一旦Clojureで試してみようかと。
<!--more-->
ところで、AkkaとClojureのcore.asynkには若干のモデルの違いがあります。詳しくは[ここ](http://blog.paralleluniverse.co/2015/05/21/quasar-vs-akka/)とかにあるのですが、一番大きくは非同期/同期モデルの違いがあります。
Akkaの場合はActorにメッセージを送っても届いたという保障はなく(「その瞬間に」届いてないだけでなく、届く前にActorが再起動したらメッセージがロストしてしまう!)、また、メッセージの受信も`receive`というコールバック用のメソッドを用意してそこでハンドルすることになります。

そういう違いがある中でどれだけClojureに翻訳出来るか多少不安ですがチャレンジしてみます。尚、原書には様々なパターンがあるのですが流石に本一冊分全部は紹介し切れないので面白そうでかつClojureでも役立ちそうなものだけ紹介することにします。このコードは原書のものをClojureに翻訳したものではなく、私が勝手に書いたものです。コードを参考にするのは雰囲気程度に留めて、実際を知りたかったら原書を読みましょう。


今回、

```clojure
(defproject reactive-message-patterns "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.2.374"]])
```

というプロジェクト設定で試しました。また、ソースコードは[こちら](https://github.com/KeenS/reactive-message-patterns)で公開しています。

# Messaging With Actors
## Pipes And Filters
メッセージが来る際に、そのまま受け取るのではなく一旦Filter用のActorで変換やvalidationをかけて本処理に入ります。


~~Clojureなら`filter<`/`filter>`/ transducerで瞬殺~~ Filter処理をActorに分離することで並列性やモジュール性が上がります。原書の例ではまずDecrypt Filterを噛ませてAuthenticate Filterで認証出来たら De-dup Filterで重複を取り除いてメッセージをクリーンにしてから処理を始める、とういうように非常に実用的な例が出ています。

また、(transformでなく、篩い分けの)Filterを分離することでFilterした後の処理、例えばinvalidなメッセージが来たらログに出すなどの柔軟性を得ることが出来ます。

これを実装してみましょう。すごい適当ですがメッセージが来たらそれを2倍するfilterを作ってみます。


``` clojure
(use 'clojure.core.async)

(defn filter-actor
  ([f]
   (let [in (chan)
         out (chan)]
     (do
       (filter-actor f in out)
       [in out])))
  ([f in out]
   (go-loop []
     (if-let [v (<! in)]
       (if-let [ret (f v)]
         (if (>! out ret)
           (recur)))
       (close! out)))))

(let [[in out] (filter-actor (fn [x] (* x 2)))]
  (go
    ;; producer
    (doseq [v (range 1 10)]
      (>! in v))
    (close! in))
  (go-loop []
    ;; consumer
    (if-let [v (<! out)]
      (do
        (println v)
        (recur))
      (close! out))))

```


簡単ですね。チャネルが終わったかどうか調べるのに一々`if-let`を使うのが面倒ですがリストに対する再帰関数もそんな感じなのでまあ、こういうもんなんでしょう。

## その他

他にもMessaging With ActorsにはMessage Router, Message Translator, Message Endpointsなどのパターンが載っているのですが割愛します。特にMessage Translatorは外部システム(HTTPリクエスト、MQ, SQLなど)とのやりとりの話なので簡単に試すには少し重すぎるようです。

# Messaging Channels
この章にはActor同士のメッセージのやりとりの方法が色々書かれています。
Point-to-Point Channel, Publish-Subscribe Channel, Datatype Channel, Invalid Message Channel, Dead Letter Channel, Guaranteed Delivery, Channel Adapter, Message Bridge, Message Bus。
中々多くのパターンが載っているのですが一部はAkka固有であったりClojureには必要なかったりしますので、私が興味を持ったものを紹介します。


## Publish-Subscribe
定番ですね。概念自体の説明は不要と思います。core.asyncだとどうなるのかを見てみます。`pub`と`sub`を使います。


``` clojure
(let [publisher (chan)
      publication (pub publisher :topic)
      subscriber1 (chan)
      subscriber2 (chan)]
  (sub publication :delete subscriber1)
  (sub publication :create subscriber1)

  (sub publication :update subscriber2)
  (sub publication :read subscriber2)

  ;; start subscribers before publish start
  (go-loop [] (when-let [v (<! subscriber1)] (printf "I'm One, got %s\n" (:type v)) (recur)))
  (go-loop [] (when-let [v (<! subscriber2)] (printf "I'm Two, got %s\n" (:type v)) (recur)))

  (go (onto-chan publisher [{:topic :update, :type "dog"}
                            {:topic :create, :type "cat"}
                            {:topic :read  , :type "fox"}]))
  )
```

おもちゃのような例ですがこれで動くようです。因みに原書ではここに結構なページ数が割かれていて、分散システムを念頭に、異なるシステムにメッセージを送る話なども扱っています。core.asyncだとそこまでは出来そうにないですね。

## Invalid Message Channel
とあるActorに予期しないメッセージが届いたらどうしましょうか。そのまま捨てますか？もしかしたらバグを潰せる好機かもしれないのにそのまま捨てるには勿体なすぎます。
かといって全てのActorにデバッグ用のコードを入れていては埒が明かないどころかコードが複雑化してしまいます。そこでInvalid Massage Channelを使いましょう。

あるActorに予期しないメッセージが届いた時に、全てを放り投げるチャネルを用意しておいて、その先のActorでログを出すなりするのです。

今回は`:price`タグのついていないメッセージが届いた時にinvalid actorに放り投げ、ログを出すものを作りました。

``` clojure
(defn invalid [data]
  {:data data
   :from (Thread/currentThread)})

(let [invalid-chan (chan)
      in (chan)
      tax 0.08]
  ;; invalid message processor
  (go-loop []
    (when-let [{data :data from :from} (<! invalid-chan)]
      (printf "invalid data %s from %s from\n" data from)
      (recur)))

  ;; main processing actor
  (go-loop []
    (when-let [v (<! in)]
      (if-let [price (:price v)]
        (printf "price: %f\n" (* price (+ 1 tax)))
        (>! invalid-chan (invalid v)))
      (recur)))

  (go (onto-chan in [{:goods "はじめてのClojure"
                      :price 1900}
                     {:goods "プログラミングClojure"
                      :price 3400}
                     {:goods "へび"
                      :description "にょろにょろ"}])))

```

`from`についてはActorならアクターオブジェクトに対してリフレクションかけて何かしらの情報をとれるのですが`go`に対するリフレクションが分からなかったので代わりにスレッド情報を使いました。
私はこの章を読んだ時にいたく完動しました。ログの扱いを集約出来るし地味にIOして非同期モデルのアクターの邪魔になるログをそれぞれのアクターに持たなくて済む。
しかしこれのActorが落ちたら、とかこのActor自身にinvalidなメッセージが来たら、とか色々考えたくなります。詳しくは原書を読みましょう。

## その他

この章には他にも外部システムと連携するMessage Adapter、外部のメッセージングシステムと連携するMessage Bridge、複数のシステム間でメッセージをやりとりするMessage Busなどが載っていますが割愛します。

# Message Construction
この章にはCommand Message, Document Message, Event Message, Request-Reply, Return Address, Correlation Identifire, Message Sequence, Message Expiration, Format Indicatorなどのメッセージ自体にまつわる話題が載っています。

~~疲れてきたため~~ AkkaやScala特有の話題が多いので1つだけに絞ります。。まあ、だいたいErlangとかやってたら自然と覚えるやつらです。

## Document Message
Command MessageやEvent Messageは良く見ますが、Document Messageとはどのようなものでしょうか。原書にはこう書いてあります

> Use a Document Message to convey information to a receiver, but without indicating how the data should be used.

「受取側に情報を伝えはするが、その情報がどのように扱われるべきかを指示しない時にDocument Messageを使います」Command Messageに付随する情報でもなくEvent Messageに付随する情報でもなくただ単なる情報です。

これは例えばデータベースアダプタのような所で使うことになります。

``` clojure
(let [db [{:id 1 :name "Aho"} {:id 2 :name "Ulman"} {:id 3 :name "Sethi"}]
      in (chan)]
  (go-loop []
    (when-let [[id return] (<! in)]
      (>! return (some #(when (= id (:id %) ) %) db))
      (recur)))

  (let [chan-cache (chan)]
    (go-loop [id 1]
      (>! in [id chan-cache])
      (when-let [res (<! chan-cache)]
        (println res)
        (recur (+ id 1))))))
```

複数のアクセスがあることを考えると双方向チャネルがあるからといってそれ1つを使うのは良くないでしょう。混線してしまいます。今回は返信用のチャネル自身も一緒に送ってしまってそこにDocumentを貰うことにしました。
ActorならActor ID(Ref)を送ることになるでしょう。


# Message Routing
この章は主にAkkaのクラスタ、分散実行に関る内容です。それ自体非常に興味深いのですがClojureだと試せないので飛ばします。
まあ、実際にはクラスタを組まなくてもいくつかのactorを使ってactor群同士でやりとりをすればいいのですが準備が中々面倒ですね。

一応扱われているトピックを拾うとContent Based Router, Message Filter, Dynamic Router, Recipient List, Splitter,
Aggregator, Resequencer, Composed Message Processor, Scatter-Gather, Routing-Slip, Process Manager, Message Brokerです。
見ての通り非常に数が多く、ページ数もかなり割かれています。そして、よく使いそうなパターンが多く載っています。Scatter-Gatherとか重い処理する時に使いそうなんですけどね。

# などなど
ここまでで、4~7章を紹介しました。原書は10章まで続きます(1-3章は何故Actorが必要なのかなどを延々語ってます)が私はこの辺で筆を置くことにします。続きが気になる方は原書をお読み下さい。

Advent Calendar遅刻して大変申し分けありませんでした。
