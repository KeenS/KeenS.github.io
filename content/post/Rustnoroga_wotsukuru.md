---
categories: [Rust, Advent Calendar Memorial 2015, Advent Calendar Memorial]
date: 2015-12-26T19:18:31+09:00
title: Rustのロガーを作る
---
κeenです。Advent Calendarのためにネタやアイディアを用意したものの時間/場所的都合でAdvent Calendarとして出せなかったボツネタでも供養しようかと。
Advent Calendarが終わってしまったので投げやり気味ですね。
第1段はRustのロガーを作る話。

[Rust Advent Calendar](http://qiita.com/advent-calendar/2015/rust-lang)の[初日](//KeenS.github.io/blog/2015/11/14/rustdechiisanatsu_ruwotsukuttemiru/)でロガーはfacadと実装に分かれてると書きましたが、実装を作る話ですね。
<!--more-->
[ドキュメント](https://doc.rust-lang.org/log/log/index.html)を見てもらえば分かる通り、logは

``` rust
pub trait Log: Sync + Send {
    fn enabled(&self, metadata: &LogMetadata) -> bool;
    fn log(&self, record: &LogRecord);
}
```

とだけインターフェースが定まっているのでそこを実装してあげれば終わりです。
なぜまたloggerを作ろうとしたのかというと、Rust公式の[env_logger](https://crates.io/crates/env_logger/)は標準出力にしか出せず、crates.ioを捜しても（作ろうと思った時点ではパっと見）ファイルに書き出せるものがなさそうだったからです。

私が作ったのは[file_logger](https://github.com/KeenS/file_logger)といいます。まだcrates.ioには出してません。TOMLでコンフィグを書けるようになったら出そうかと思うのですがもう既にファイルに書きだせるものが他にありますかね。

今回作るにあたって目指したのは1つのことにだけ集中する他と組み合わせられるライブラリです。
logはロガーをグローバル変数に代入して使い回してるので同時に1つしかロガーを登録出来ません。そこでロガーアグリケーターのようなものを作ってそれに複数のロガーを登録することが考えられます。
調べきれてませんが恐らく既にいくつか存在するでしょう。アグリゲータの存在を仮定してしまえばアグリゲータに任せれる部分は任せてしまうのが良いでしょう。

ということで次のことを最初に決めました

* ログフォーマットは柔軟性を持たせる
* ログメッセージの内容に応じて出力を制御出来るフィルターを入れる。(アグリゲータが無差別にログを流し込んで来た時のため)
* 複数ファイルへのログはサポートしない。アグリゲータに任せる
* プラグインはサポートしない。アグリゲータに任せる
* ログローテーションはしない。`Write`の実装に任せる。

一応file loggerですがWriteをすべからく受け取れるので実際はストリームにも使えたりします。今書き出してみたらフォーマッタとかフィルタとかもアグリゲータに任せた方がいいのかなって気がしてきた。
しかし単体での利便性を考えると仕方ない。


実装は大したことをやっていなくて、精々ログフォーマットを文字列で書けるように（TOMLで設定を書きたいため）フォーマットのパーサを書いた程度でしょうか。


もう少し考えてからリリースします。あとFile Lotating Writerも一緒に出さなきゃ片手落ちですかね…
