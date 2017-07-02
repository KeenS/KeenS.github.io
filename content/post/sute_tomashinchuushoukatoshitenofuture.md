---
categories: [Rust, Future, 非同期]
date: 2017-07-02T12:31:07+09:00
title: ステートマシン抽象化としてのFuture
---

κeenです。最近Futureと一口にいってもいくつか種類があるなと気付いたのでRustの[`futures-rs`](https://github.com/alexcrichton/futures-rs)に実装されているFutureの挙動を紐解こうと思います。

<!--more-->

# イベント駆動IO
TCPサーバをノンブロッキングに書こうとすると、思いの他大変です。
ブロックせずにIOできるタイミングまで処理を溜めておいて、できるときに読み書きします。ブロックしないタイミングをみつけるのも一苦労なので大抵ライブラリに頼ってライブラリからイベントを通知してもらいます。Rustには[mio](https://github.com/carllerche/mio)というライブラリがあります。

さて、これイベントの発見は書かなくてよくなりましたが、まだつらいです。リクエストがないのにレスポンスを返せる訳もないので処理とイベントに依存関係があって、「read readyイベントでデータを読み取って、足りれば処理開始、足りなければまだ読み取る。処理が終わればwrite readyイベントを待って書き込み。その後は次のリクエストに備えてread readyイベント待ち」などの複雑な条件分岐と処理をしなければなりません。しかもクライアントは1つじゃないのでそれらを複数管理しないといけません。やりきれませんね。私が昔書いた[Websocketのechoサーバ](https://github.com/KeenS/chat/blob/master/src/handler.rs#L32)を少し見ると大変そうなことが伝わると思います。（余談ですが、つらそうなコードを書いたのは抽象化ライブラリに頼らないコードを実感するためでした）


# ステートマシン
先程の複雑な条件分岐の例は、「ステート」があることに気付きます。「リクエストがまだ来てない」「リクエストの一部が届いたけど全部でない」「リクエストの処理中」「レスポンスの送信待ち」など。これを明確に「ステート」としてプログラムを書いてあげると綺麗に書くことができます。

```
[リクエストがまだ来てない]<----------------+
  | read ready                            |
  +------------------------------------+  |
  |                                    |  |
[リクエストの一部がきたが全部でない]<-+  |  |
  | read ready                      |  |  |
  +---------------------------------+  |  |
  |                                    |  |
[リクエストの処理中]<-------------------+  |
  |                                       |
[レスポンスの書き出し待ち]                 |
  | write ready                           |
[レスポンスの書き出し完了]-----------------+

```

さて、このステートマシンを図に書く範囲では明瞭なのですが、実際のプログラムに落とすとやや見通しが悪いです。

ナイーブな実装を疑似コードで書くとこうなるでしょうか。

```
enum State = リクエストがまだ来てない
           | リクエストの一部がきたが全部でない
           | リクエストの処理中
           | レスポンスの書き出し待ち
           | レスポンスの書き出し完了

event arrived
  case (state, event)
    when (リクエストがまだ来てない, read ready)
      ...
      if request is complete
        state <- リクエストの処理中
        wait for write ready
      else
        state <- リクエストの一部がきたが全部でない
        wait for read ready
    when リクエストの一部がきたが全部でない
      ...
      if request is complete
        state <- リクエストの処理中
        wait for write ready
      else
        state <- リクエストの一部がきたが全部でない
        wait for read ready
    ...
```

モデリングは上手くいっているものの実際のコードに落とすとみづらいですね。
ナイーブな実装方法の他に、デザインパターンのステートパターンを使う手もあります。まあ、そのまんまですね。
疑似コードにするとこうなるでしょうか。

```
abstract class State
  method do returns (State, Event)

class リクエストがまだ来てないState
  method do returns (State, Event)
      ...
      if request is complete
        return (new リクエストの処理中State, write ready)
      else
        return (new リクエストの一部がきたが全部でないState, read ready)

class リクエストの一部がきたが全部でない
  method do returns (State, Event)
      ...
      if request is complete
        return (new リクエストの処理中State, write ready)
      else
        return (new リクエストの一部がきたが全部でないState, read ready)

event arrived
  if event equals waiting_event
    (next_state, waiting_event) <- state.do()
```


ステートと処理のかたまりで分離することができたのでコードの見通しもよくなりました。

しかし、Rust的にはまだ問題があります。Stateのサブクラスでモデリングしてますが、それだとメソッドのディスパッチが動的ディスパッチになってしまって遅いです。Zero-cost abstractionできてません。

もう1つ問題があって、これでもまだ直感的なコードとはかけ離れてます。本来ならこういうコードを書きたい筈です。

```
request <- empty
until request is complete
  request << input
response <- // do something with request
output << response
```

これをどうにかできないでしょうか。

# Future
そこでfutures-rsです。ゼロコストでステートマシンを抽象化してくれます。[Zero-cost futures in Rust · Aaron Turon](https://aturon.github.io/blog/2016/08/11/futures/)を読んだことのある人も多いでしょう。

基本的なアイディアは、1つのステートとその時の処理を表わす`Future`の他にステート同士を繋げるコンビネータを用意することで柔軟にプログラムを書けるようにするということです。そしてステートの処理関数が静的ディスパッチされるようにコンビネータを工夫する（基本的には型パラメータに情報を残して静的ディスパッチできるようにする）ことでゼロコスト抽象化を実現しています。

先程のコードをfuturesを使って書くとこのような雰囲気になるでしょうか。

``` rust
let f = loop_fn(Vec::new(), |mut buff| {
    input.read()
      .and_then(|data| {
        buff.append(data);
        match Request::parse(buff) {
          Ok(request) => Ok(Loop::Break(request))
          Err(_) => Ok(Loop::Continue((buff)))
        }
      })
  }).and_then(|request| {
    // do something with request
  }).and_then(|response| {
    output.write(response)
  });

run(f)
```


ダイレクトなコードに比べるとまだノイズが多いですが、ステートマシンを陽に作るコードよりは直感的になったと思います。

# 他のFutureとの違い
futuers-rsのFutureは1イベント起きる度に([`poll`](https://docs.rs/futures/0.1.14/futures/future/trait.Future.html#tymethod.poll)が呼ばれてReadyになる度に)次のステートの処理をし、その次のステートに移るという流れが基本です。
次のステートに移っても、即座にそののステートの処理が走る訳ではありません。次に`poll`が呼ばれるまで処理をしません。つまりマルチスレッドの文脈で語ると、イベントが発火したスレッド *ではなく* `poll`を呼んだスレッド、おおむね **Futureを所有しているスレッドで処理が走ります** 。Futureの処理がスレッドを飛び越えたりはしません。ある1つのFutureを別スレッドで計算したければ[CPU Pool](https://github.com/alexcrichton/futures-rs/tree/master/futures-cpupool)などが使えますが、その後（[spawn](https://docs.rs/futures-cpupool/0.1.5/futures_cpupool/struct.CpuPool.html#method.spawn)の返り値の新たなFuture）に続けた処理は今のスレッドで実行されます。

一方他のFutureはほぼマルチスレッドと密結合したプロミスのようになっていて、

1 Future = 別スレッドで実行される計算への先物。
コンビネータ(callback) = 別スレッドで実行される計算が終わったあとに同じスレッドで実行されるべき計算

のようなものが多い印象です。要は1つ処理が終わるとそのまま次の処理、その次の処理と自動で発火していく。もちろん、ライブラリの作りによってスレッドとの関係性などにバリエーションはあるでしょうが、Rustのように[`poll`](https://docs.rs/futures/0.1.14/futures/future/trait.Future.html#tymethod.poll)を基本とした設計はあまりみません。


このようにFutureにも色々あるうち、Rustでメジャーに使われているfutures-rsの`Future`はステートマシンの抽象化になっているよ、というお話でした。
