---
categories: [非同期, Go]
date: 2017-05-19T20:51:46+09:00
title: 非同期とノンブロッキングとあと何か
---
κeenです。最近同期/非同期、ブロッキング/ノンブロッキング、直接形式/継続渡し形式あたりが混乱してきたので個人的に整理します。
あくまで私個人の理解を纏めただけなので誤謬などに注意して下さい。

追記: [@tanaka_akr](https://twitter.com/tanaka_akr)さんから[指摘](https://twitter.com/tanaka_akr/status/865722507281580032)されたのですが、用語の説明が間違っていそうだったので書き直しました。

<!--more-->

非同期とノンブロッキングはよく混同されます。また、非同期処理の記述形式として直接形式や継続渡し形式などがあります。
私自身違う言葉だなとは思いつつも混同したり違いを忘れたりしています。
非同期もノンブロッキングもナイーブなIOに比べると速い方式だな程度の理解でいてそんなに困らないと思ってますし混同や誤用に目くじらを立てるつもりもありません。
しかしながら3者を区別しないと意味を成さない文脈で3者を混同している技術を何度か見掛けたので（自分の中で）整理しようと思ったのがこの記事を書こうと思ったきっかけです。

# 用語


色々と調べましたが、[The Open Group Base Specifications Issue 6 IEEE Std 1003.1, 2004 Edition](http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html)の定義を使います。

* [ブロッキング](http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_77):
  IO操作で、操作する前に待ちが発生するという性質
* [ノンブロッキング](http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_240):
  IO操作で、操作する前に待ちが発生しそうなら即座に関数から返るという性質
* [同期](http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_378):
  IO操作で、操作が終わるまでブロックするという性質
* [非同期](http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_29):
  IO操作で、IO操作を発効したスレッド自体がブロックせずに別のCPU処理を行えるという性質
* IOの多重化: 複数のfdに対して監視を行ない、ブロックせずに操作可能なfdを通知する仕組み。
   調べると「複数のfdに対してブロックできる」という記述があるのですがノンブロッキングなオプションもありますよね

これらはどのようにIOを行うかの問題で、 **実行時** に関係する話題です。

次に、直接形式と継続渡し形式について。非同期やノンブロッキングなIOをしようと思うと後続の処理、すなわち（限定）継続が必要になる。この（限定）継続をプログラマがどのように渡すかの問題。

[直接形式](https://en.wikipedia.org/wiki/Direct_style)は（限定）継続がプログラムには陽には現れず、コンパイラが頑張るかコルーチンなどの言語機能、`async`/`await`、`do`、`for`などの構文を用いて実現されます。
[継続渡し形式](https://en.wikipedia.org/wiki/Continuation-passing_style)(CPS)はプログラマが陽に（限定）継続を渡すスタイルで、コールバック形式などとも呼ばれます。

これらは主に言語の問題で、 **コンパイル時** に関係する話題です。
よく直接形式と同期処理が混同されて「同期プログラミングをすることで非同期プログラミングができます」などの矛盾した一文が書かれがちですが区別しましょう。

## 誰が何を
IO処理の話題はスレッドとカーネルとのやりとりに関することが多い気がしますが、Goのようにユーザランドで実行単位を持つと「ブロックする」といったときにスレッドがブロックするのかgoroutineがブロックするのか分かりづらいですね。
なのでこれ以後は誰が何をブロックするのかを出来る限り明示しながら書きたいと思います。例えば「カーネルがgoroutine Aを実行しているスレッドをブロックする」など。

# Scala
前職でScalaを書いていました。
Scalaの`Future`そのものは`flatMap`が関数をとるので継続渡し形式ですが、そのシンタックスシュガーの`for`式は直接形式でプログラミングができます。
IOの実体についてはFutureで抽象化されているのでexecutorによります。

# Go
前職の新卒研修で5日ほどGoを書いたことがあります。
多くの言語は比較的シンプルなIOモデルを持つので用語を意識しなくても問題ありませんが、Goは実行時に色々工夫しているので用語をちゃんと区別しないと齟齬が出るようです。
なのでGoを例に理解を深めようと思います。

まず、Goは実行時に動作するコンポーネントが多いので整理します。
[ここの質問](https://www.quora.com/How-does-the-Go-runtime-work)への回答によるとgoroutine実行用のスレッド、スケジューラ、GCに大別してよさそうです。
このうち、IOに関連するのはgoroutine実行用のスレッドとスケジューラなのでGCについては触れません。
あまりよく分かってないのですがスケジューラはgoroutine実行用のスレッド上で呼ばれるための関数（群）なんですかね。

普段は1スレッドが複数のgoroutineの実行を受け持ち、goroutineが何かしらの理由でブロックしそうなタイミングで別のgoroutineにコンテキストスイッチするようです。
ブロックしそうなタイミングというのはやや古いですが[ここ](http://stackoverflow.com/questions/17953269/go-routine-blocking-the-others-one)に書かれてます。
直接形式で書きつつコンテキストスイッチをするということはどうにかして(限定)継続を取り出す必要がありますが、それこそgoroutineですね。goroutine自身でスタックを持つようですし。
C関数を呼ぶときなどはコンパイラが裏で呼び出す前と後に分割しているようです。

さて、IOについてはファイルIOとネットワークIOで操作が異なるようです。

ファイルIOでは普通に同期/ブロッキングなシステムコールを発効し、OSがスレッドをブロックする時間が長いようなら別スレッドで元スレッドの持っていたgoroutineを実行するようです。
信頼できる情報が出てこなかったのですが自身で長くなりそうと思っているシステムコールの場合はgoroutineの実行権限（プロセス）を手放してあとは他のプロセスがstealするに任せてる？
ファイルIOは、スレッドがシステムコールでブロックし同期的で、goroutineもシステムコールでブロックし同期的です。ただスケジューラのお陰で他のgoroutineに迷惑がかからないだけです。

ネットワークIOはポーリングを行なうようです。
スケジューラがfinbrunnableする中でブロックしない方のpoll(Linuxだとepollにノンブロッキングオプションつけたやつかな？)を呼んで、操作可能なfdがあればそれを待っているgoroutineを起こしている模様。
であれば上の分類に従うとネットワークIOはスレッドはブロックせず同期的でIOを多重化し、goroutineはスケジューラからブロックされ同期的で（特別に関数を呼ばなければ）IOの多重化もしません。ただ、操作可能なfdに操作しているのでgoroutineはカーネルからはブロックされません。

この他にもgoroutine同士のコミュニケーションにチャネルがあります。そこまで重要じゃないかなと思って詳しく調べてないので間違いがあるかもしれないです。
チャネルへの操作は同期的ですね。チャネルへのread/writeはバッファがempty/fullだったらブロックするようなのでgoroutineはチャネルへの操作でブロックします。調べてないですが普通スレッドはブロックしないでしょう。

チャネルへのselectはあまり情報が出てこなかったのですが恐らくスケジューラがIO可能なチャネルを調べて1つでもあればそれからランダムに選ぶ、1つもなくdefaultもなければスケジューラがgoroutineをブロックするんだと思います。
同期的でIOの多重化をしていますね。

Goroutineやスレッドレベルでは同期的ですが、Goのプロセス全体でみるとGOMAXPROC分のスレッドは常にCPUを使っているので非同期なんですかね。

# まとめ
Goの件について表にするとこうですかね。非同期処理をやろうと思ったら大抵処理単位を分ける必要がありますが、Goの最小処理単位がgoroutineなのでよく考えたらgoroutineが全て同期するのはあたりまえですね。

|               | スレッド                | goroutine            | Goプロセス |
|---------------|-------------------------|---------------------|-----------|
| ファイル       | ブロック/同期/          | ブロック/同期/       | 非同期     |
| ネットワーク   | ノンブロック/同期/多重化 | ノンブロック/同期/   | 非同期     |
| チャネルRW     | -                      | ブロック/同期/       | -          |
| チャネルselect | -                      | ブロック/同期/多重化  | -          |

ということでGoのIOについては「goroutineを直接形式で同期的な書き方をしても裏でスケジューラが動いているからCPUを効率的に使えてGoプロセス全体でみれば非同期」になるんですかね。

Goに詳しい人、コメントや誤り訂正お願いします。[GitHubのイシュー](https://github.com/KeenS/KeenS.github.io/issues)とか使えば良い気がします。


# 参考

* [multithreading - Golang blocking and non blocking - Stack Overflow](http://stackoverflow.com/questions/36112445/golang-blocking-and-non-blocking)
* [src/os/file_unix.go - The Go Programming Language](https://golang.org/src/os/file_unix.go?s=#L224)
* [src/runtime/proc.go - The Go Programming Language](https://golang.org/src/runtime/proc.go?s=#L1795)
* [Frequently Asked Questions (FAQ) - The Go Programming Language](https://golang.org/doc/faq#goroutines)
* [channel - go routine blocking the others one - Stack Overflow](http://stackoverflow.com/questions/17953269/go-routine-blocking-the-others-one)
* [src/runtime/cgocall.go - The Go Programming Language](https://golang.org/src/runtime/cgocall.go)
* [select - How does channel blocking work in Go? - Stack Overflow](http://stackoverflow.com/questions/32538438/how-does-channel-blocking-work-in-go)
* [The Go Programming Language Specification - The Go Programming Language](https://golang.org/ref/spec#Select_statements)
* [How does the Go runtime work? - Quora](https://www.quora.com/How-does-the-Go-runtime-work)
