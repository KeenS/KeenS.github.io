---
categories: [非同期, Go]
date: 2017-05-19T20:51:46+09:00
title: 非同期とノンブロッキングとあと何か
---
κeenです。最近同期/非同期、ブロッキング/ノンブロッキング、直接形式/継続渡し形式あたりが混乱してきたので個人的に整理します。
あくまで私個人の理解を纏めただけなので誤謬などに注意して下さい。

<!--more-->

非同期とノンブロッキングはよく混同されます。また、非同期処理の記述形式として直接形式や継続渡し形式などがあります。
私自身違う言葉だなとは思いつつも混同したり違いを忘れたりしています。
非同期もノンブロッキングもナイーブなIOに比べると速い方式だな程度の理解でいてそんなに困らないと思ってますし混同や誤用に目くじらを立てるつもりもありません。
しかしながら3者を区別しないと意味を成さない文脈で3者を混同している技術を何度か見掛けたので（自分の中で）整理しようと思ったのがこの記事を書こうと思ったきっかけです。

# 用語
色々と調べましたが、以下のブログ記事が一番信頼できそうなのでこの記事の用語に従います。ここでは同期/非同期とブロッキング/ノンブロッキングとIOの多重化を区別しています。

[人間とウェブの未来 - 非同期I/OやノンブロッキングI/O及びI/Oの多重化について](http://blog.matsumoto-r.jp/?p=2030)

一部抜粋します


> * ブロッキングI/O:
>  データ処理が完了するまで待つ
> * ノンブロッキングI/O:
>  データ処理の完了を待たずに他の処理を行う
> * synchronous I/O Operation:
>  データ処理の入出力が可能になった時点で通知
>  その後に同期的にデータの転送が必要。その処理をブロックあるいはノンブロック（別の処理をしつつ定期的に転送完了をチェックする）に行うかは自由であり、データの整合性は含まない
> * Asynchronous I/O Operation:
>  データ処理の入出力が完了した時点で通知
>  非同期なので読込の場合は通知のあった段階で既にデータの転送は完了（I/O Completion）しバッファ内にデータがある。シグナルやコールバックによる通知があるまではユーザスペースでその他の処理が可能であるため、基本的にはノンブロック。別にやりたければ通知があるまでブロックしても良い、その辺りは自由
> * I/Oの多重化:
>   I/O可能になったfdを通知（I/OはブロッキングI/O）

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

ネットワークIOはpollingを行なうようです。
スケジューラがfinbrunnableする中でブロックしない方のpoll(Linuxだとepollにノンブロッキングオプションつけたやつかな？)を呼んで、操作可能なfdがあればそれを待っているgoroutineを起こしている模様。
であれば上の分類に従うとネットワークIOはスレッドはブロックせず同期的でIOを多重化し、goroutineはスケジューラからブロックされ同期的で（特別に関数を呼ばなければ）IOの多重化もしません。

この他にもgoroutine同士のコミュニケーションにチャネルがあります。そこまで重要じゃないかなと思って詳しく調べてないので間違いがあるかもしれないです。
チャネルへの操作は同期的ですね。チャネルへのread/writeはバッファがempty/fullだったらブロックするようなのでgoroutineはチャネルへの操作でブロックします。調べてないですが普通スレッドはブロックしないでしょう。

チャネルへのselectはあまり情報が出てこなかったのですが恐らくスケジューラがIO可能なチャネルを調べて1つでもあればそれからランダムに選ぶ、1つもなくdefaultもなければスケジューラがgoroutineをブロックするんだと思います。
goのselectはチャネルからデータが取り出された状態でくるので上の定義に従えば非同期…？そしてIOの多重化ですね。

# まとめ
Goの件について図にするとこうですかね。

|               | スレッド                | goroutine              |
|---------------|-------------------------|------------------------|
| ファイル       | ブロック/同期/          | ブロック/同期/         |
| ネットワーク   | ノンブロック/同期/多重化 | ブロック/同期/         |
| チャネルRW     | -                       | ブロック/同期/         |
| チャネルselect | -                       | ブロック/非同期/多重化 |

Goで非同期プログラミングが楽になると言っているのをよくみかけますが、今回の私の理解では非同期操作はほとんどないようです。
Goはコンパイラが頑張ってgoroutineをコンテキストスイッチ可能にしているのと、スケジューラが頑張ってプロセス全体がブロックしないようにしているもののgoroutine自体はほとんどの操作でブロック/同期ですし、下のスレッドも同期かつネットワーク操作以外ではブロックするようです。
なので言うとしたら「Goはコンパイラとスケジューラのお陰で見掛け上ブロックしても全体への影響が小さい」ですかね。

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
