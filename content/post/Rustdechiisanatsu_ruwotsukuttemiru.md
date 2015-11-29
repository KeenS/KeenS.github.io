---
categories: [Rust, Advent Calendar, Advent Calendar 2015]
date: 2015-11-14T23:57:43+09:00
title: Rustで小さなツールを作ってみる(前編)
---

この記事は[Rust Advent Calendar 2015](http://qiita.com/advent-calendar/2015/rust-lang) 1日目の記事です。  
次 [Rustで小さなツールを作ってみる(後編)](/blog/2015/11/29/rustdechiisanatsu_ruwotsukuttemiru_kouhen_/)

κeenです。Rust Advent Calendar初日ということで軽い話から。
小さなツールって何だよって感じですが手元にIRCの生ログが大量にあるのでそれをDBにインポートしてみましょう。
あまりRustに向いた仕事じゃなさそうですが手始めとして。

前編でログのパースを、後編でDBへのインポートをやります。
<!--more-->

IRCの生ログは[tiarra](http://www.clovery.jp/tiarra/)が吐いたものです。

`#emacs@freenode`のように`チャネル名@サーバ名`のディレクトリに入っていて、ファイル名は`YYYY-mm-dd.txt`で、

```
13:43:24 <#emacs@freenode:codingquark> Such key generation, much wow.
13:43:59 ! Vejeta` (Ping timeout: 264 seconds)
13:44:30 + jcazevedo (jcazevedo!~jcazevedo@a94-132-141-99.cpe.netcabo.pt) to #emacs@freenode
13:45:30 + quazimodo (quazimodo!~quazimodo@155.143.14.28) to #emacs@freenode
13:45:32 ! Hugehead_ (Read error: Connection reset by peer)
13:48:48 + favadi (favadi!~textual@113.190.4.106) to #emacs@freenode
13:51:06 wizzo -> bitchimightbe
13:52:23 bitchimightbe -> wizzo
13:53:05 - frost3772 from #emacs@freenode ("ERC (IRC client for Emacs 24.5.1)")
13:56:37 ! chandan (Quit: WeeChat 1.2)
13:56:38 ! plunderous (Read error: Connection res
```

のような形式で入ってます。先頭に`<`が付いてるのが発言、`! `がついてるのがなんかメッセージ(多分quitとかkillとか)、`+ `がjoin、`- `がpart、無しがnickとかnoticeですかね。他にもログ取ってるircコマンドはあるのですが`!`に内包されてそうですね(tiarraのマニュアルに載ってそうですが見るの面倒)


因みにログ全体は`~/log`に置いてあります。

一応treeするとこんな感じ。

```
.
├── #emacs@freenode
│   ├── 2014-04-11.txt
│   ├── 2014-04-12.txt
│   ├── 2014-04-13.txt
..  ..
```

2014-04-11から2015-11-01までの間ロギングしてます。


# ディレクトリリスティングまで

まずは

```
$ cargo new irc_log --bin
$ cd irc_log
```

そして`src/main.rs`を開いて

```rust
use std::fs;

fn main() {
    let paths = fs::read_dir("/home/kim/log").unwrap();
    for path in paths {
        let path = path.unwrap().path();
        println!("{}", path.to_string_lossy().to_string());
    }
}
```

と書きましょう。path path煩いですね。OSの返す文字列(Cの文字列)とRustの文字列の違いだとか一旦`Path`で抽象化してるだとかで文字列を抜き出すのは一苦労です。

さて、

```
$ cargo run
```

でディレクトリ名が出てくればOK。

# ファイル名リスティング
mainを


```rust
use std::fs;

fn main() {
    let paths = fs::read_dir("/home/kim/log").unwrap();
    for path in paths {
        let path = path.unwrap().path();
        on_channel_dir(&path);
    }
}
```

と置き換えてチャネルのディレクトリでの作業は`on_channel_dir`でやりましょう。

## チャネル名とサーバ名の抜き出し

ディレクトリ名が`チャネル名@サーバ名` の 形をしているので @の前後を抜き出せば良さそうです。

src/main.rsの先頭に

``` rust
use std::path::Path;
use std::str::from_utf8;
use std::os::unix::prelude::OsStrExt;

```

を追記して、

``` rust
fn on_channel_dir(path: &Path) {
    let dirname = from_utf8(path.file_name().unwrap().as_bytes()).unwrap().to_string();
}
```

でとりあえず`/`の付かないディレクトリ名を取れそうです。難しい。やはりOSの返す文字列の扱いは面倒ですね。
しかし一旦Rustの文字列になってしまえばこちらのもの。

``` rust
    let at = match dirname.find('@') {
        Some(i) => i,
        None => return
    };
```

で`@`の位置を取得出来ます。`@`がなかったらreturnして無視しておきましょう（実際そういう名前のディレクトリがあるのですが今回の集計の対象外なので）。

あとは部分文字列を取るだけ。

``` rust
    let channel = &dirname[..at];
    let server = &dirname[at+1..];
    println!("{} at {}", channel, server);
```

## ファイルリスティング

これは先程と同じですね。

``` rust
    let logs = fs::read_dir(path).unwrap();
    for log in logs {
        let log = log.unwrap().path();
        println!("{}", log.to_string_lossy().to_string());
    };
```

で、`on_channel_dir`全体はこうです。

``` rust
fn on_channel_dir(path: &Path) {
    let dirname = from_utf8(path.file_name().unwrap().as_bytes()).unwrap().to_string();
    let at = match dirname.find('@') {
        Some(i) => i,
        None => return
    };
    let channel = &dirname[..at];
    let server = &dirname[at+1..];
    println!("{} at {}", channel, server);
    let logs = fs::read_dir(path).unwrap();
    for log in logs {
        let log = log.unwrap().path();
        println!("{}", log.to_string_lossy().to_string());
    };
    
}
```

# ファイルのパース
次にまた`println!`の部分を`on_log`としてファイルをパースしていきましょう。

## 日付の抜き出し
先程と似てますので飛ばし気味にいきます。

簡単にやっても良いのですがお作法として日付ライブラリを使ってパースしましょう。
`Cargo.toml`に以下を追記。

```toml
[dependencies]
time = "*"
log = "0.3.3"
env_logger = "0.3.2"
```

本当はバージョンも指定した方が良いのですが書き捨てのコードなのでまあ、良いでしょう。因みにtimeライブラリ自体扱いづらいとか色々問題があるのですがそれは後のAdvent Calendarに譲りましょう。しれっとloggerも導入します。printfデバッグにlogger便利!

```
cargo update
```

してdependenciesをインストールします。因みに`log`がlogger facadで`env_logger`がloggerの実装です。使い方は後程。

またsrc/main.rsの先頭に

``` rust
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate time;
```

を追記。



そして`on_log`です。

``` rust
fn on_log(log: &Path) {
    let datestr = from_utf8(log.file_stem().unwrap().as_bytes()).unwrap().to_string();
    let date = time::strptime(&datestr, "%Y-%m-%d").urwrap();
}
```

今回は失敗しないことが分かってるので全部`unwrap`してます。本当は`try!`を使って上流に委ねた方がいいのですが今回は面倒なのでこうしちゃってます。正しいエラーハンドリングはtrplの[Error Handling](https://doc.rust-lang.org/book/error-handling.html)を読みましょう。


## ファイルの読み出し
### 開く
コードをいきなり載せると

``` rust
    let file = match fs::File::open(log) {
        Ok(f) => f,
        Err(e) => {
            error!("could not open {}; skipping.", log.display());
            return;
        }
    };
```

です。これはエラーハンドリングします。1つのファイルに失敗しても次があるので。ここで使ってる`error!`がloggerのマクロです。

## 行毎の読み出し
行毎の読み出しはバッファーリーダーが必要です。

```rust
use std::io::BufReader;
use std::io::BufRead;
```

をsrc/main.rsの先頭部分に追記しましょう。因みに`BufRead`がトレイトで、`BufReader`が実装です。実装が必要なのは良いとして、トレイトまで必要なのはトレイトで定義された関数を呼び出すのにトレイトのインポートが必要だからですね。

さてさて、行の読み出しを進めます。先程オープンしたファイルを`BufReader`で包んでやればOKです。


``` rust
    let br = BufReader::new(&file);
    for line in br.lines() {
        let line = match line {
            Ok(l) => l,
            Err(e) => {
                warn!("ignoring error {}", e);
                continue;
            }
        };
        ....
    }
```

行の読み出し結果もResultで返ってくるんですね。これも失敗したら読み飛ばせばいいので`urwrap`せずに扱います。

## 行のパース
### ガード
先頭部分の時間をパースする時に部分文字列を抜き出しますが、変な行だと10文字もなかったりするので一旦ガードを付けておきましょう。

``` rust
        if line.len() < 10 {
            warn!("ignoring line {}", line);
            continue;
        }

```

### 時間のパース
これはファイル名の時とほぼ同じ。

``` rust
        let time = match time::strptime(&line[0..8], "%H:%M:%S") {
            Ok(t) => t,
            Err(e) => {
                warn!("Parse error {}; ignoring", e);
                continue;

            }
        };
```

今度は変な行が紛れてそうな（実際、ログファイルにはよくある）のでエラーは無視します。

### メッセージのパース
0-8文字目に時間があって、1スペース挟むので9文字目からがメッセージですね。そして先頭の文字で判断出来るのでした。


``` rust
        let msg = &line[9..];
        match &msg[0..1] {
            "!" => println!("sysmsg {}", &msg[1..]),
            "+" => println!("join {}", &msg[1..]),
            "-" => println!("part {}", &msg[1..]),
            "<" => match msg.find('>').map(|e| (&msg[1..e], &msg[e+1..])) {
                Some((user, body)) => println!("user: {} body: {}", user, body),
                None => ()
            },
            _ => println!("info {}", &msg[1..]),
        }

```

で判断出来そうです。パースした後の話は後回し。

# ソース全体
ソースの断片が続いたので一旦ここまでの全体像を出します。

``` rust
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate time;
use std::str::from_utf8;
use std::io::BufReader;
use std::io::BufRead;
use std::fs;
use std::path::Path;
use std::os::unix::prelude::OsStrExt;


fn on_log(log: &Path) {
    let datestr = from_utf8(log.file_stem().unwrap().as_bytes()).unwrap().to_string();
    let date = time::strptime(&datestr, "%Y-%m-%d");
    let file = match fs::File::open(log) {
        Ok(f) => f,
        Err(e) => {
            error!("could not open {}; skipping.", log.display());
            return;
        }
    };
    let br = BufReader::new(&file);
    for line in br.lines() {
        let line = match line {
            Ok(l) => l,
            Err(e) => {
                warn!("ignoring error {}", e);
                continue;
            }
        };
        if line.len() < 10 {
            warn!("ignoring line {}", line);
            continue;
        }
        let time = match time::strptime(&line[0..8], "%H:%M:%S") {
            Ok(t) => t,
            Err(e) => {
                warn!("Parse error {}; ignoring", e);
                continue;

            }
        };
        let msg = &line[9..];
        match &msg[0..1] {
            "!" => println!("sysmsg {}", &msg[1..]),
            "+" => println!("join {}", &msg[1..]),
            "-" => println!("part {}", &msg[1..]),
            "<" => match msg.find('>').map(|e| (&msg[1..e], &msg[e+1..])) {
                Some((user, body)) => println!("user: {} body: {}", user, body),
                None => ()
            },
            _ => println!("info {}", &msg[1..]),
        }
    }
}

fn on_channel_dir(path: &Path) {
    let dirname = from_utf8(path.file_name().unwrap().as_bytes()).unwrap().to_string();
    let at = match dirname.find('@') {
        Some(i) => i,
        None => return
    };
    let channel = &dirname[..at];
    let server = &dirname[at+1..];
    println!("{} at {}", channel, server);
    let logs = fs::read_dir(path).unwrap();
    for log in logs {
        on_log(&log.unwrap().path());
    };
}


fn main(){
    env_logger::init().unwrap();
    let paths = fs::read_dir("/home/kim/log").unwrap();
    for path in paths {
        let path = path.unwrap().path();
        on_channel_dir(&path);
    }
}

```

実はmainの先頭で`env_logger::init().unwrap();`でロガーの初期化してました。忘れてましたね。ごめんなさい。

さて、これで一旦走らせてみましょう。動く筈です。

```
$ cargo run
```



動いてるようですが、遅い。出力が多くてターミナル側がボトルネックのようです。

# パニックハンドルとチューニング

一旦メッセージを出力をやめて様子見してみましょう。

``` rust
            "!" => (),
            "+" => (),
            "-" => (),
            "<" => (),
            _ => (),

```

それでもまだ遅い上に謎のパニックが出てます。

```
thread '/home/kim/log/#lisp@freenode' panicked at 'index 0 and/or 8 in ` ;ZZZz;…,` do not lie on character boundary', ../src/libcore/str/mod.rs:1311
```

ログを見てみましょう。env_loggerは環境変数からログレベルを渡してあげるとログを出します。

```
$ RUST_LOG=Trace cargo run
...
WARN:irc_log: ignoring error stream did not contain valid UTF-8
WARN:irc_log: ignoring error stream did not contain valid UTF-8
WARN:irc_log: ignoring error stream did not contain valid UTF-8
WARN:irc_log: ignoring error stream did not contain valid UTF-8
WARN:irc_log: ignoring error stream did not contain valid UTF-8
WARN:irc_log: ignoring error stream did not contain valid UTF-8
WARN:irc_log: ignoring error stream did not contain valid UTF-8
...
```

意外と変な行が一杯あるのですね。しかしこれらはちゃんとエラーハンドリングしてるので問題ありません。

各所で`unwrap`してるせいな気もしますが、`urwrap`のせいならそういうメッセージが出るので違います。

実はこれ、非UTF-8のバイト列でrustの標準ライブラリが内部でパニックを出してます。
これの扱いも後のAdvent Calendarに譲るとして、ワークアラウンドを捜しましょう。rust1.4時点ではパニックをハンドルする手段はありません。

パニックメッセージの先頭に`thread`とついてるのでthreadで包めば良さそうです。

## Thread
まずはthreadをインポートします。

``` rust
use std::thread::Builder;
```

そしてon_channel_logを少し弄りましょう。無効なバイト列を含むファイルをざっくり切る感じで。

``` rust
    for log in logs {
        on_log(&log.unwrap().path());
    };

```

としていた部分を

```rust
    let threads = logs.map(|log| {
        let log = log.unwrap().path();
        let pathname = log.to_string_lossy().to_string();
        Builder::new().name(pathname).spawn(move|| on_log(&log))
    }).collect::<Vec<_>>();
    for thread in threads {
        let _ = thread.unwrap().join();
    }
```

とします。一見複雑ですが、ファイル毎にスレッドを作って全部をvectorにして集めてそれぞれjoinを待ってるだけです。
そしてスレッド名を処理するファイル名にしたいのでスレッドビルダを使っています。

さて、これで走らせてみましょう。

```
$ time cargo run
...
#lisp at freenode
thread '/home/kim/log/#lisp@freenode/2014-08-18.txt' panicked at 'index 0 and/or 8 in ` ;ZZZz;…,` do not lie on character boundary', ../src/libcore/str/mod.rs:1311
#emacs at freenode
...
cargo run  327.21s user 8.71s system 630% cpu 53.289 total
```

lispチャネルの1ファイルだけに変なバイト列があることが分かりました。

そしてパフォーマンスですが、とりあえず`top`で見る限りCPUは最大770%使ってくれてます（8コアマシン）。ファイル毎にスレッド作ってるので若干リソースの食い合いが発生してますがこれだけ並列度があればスレッドプールを使うまでもないでしょう。

## さらなるチューニング…？

しかしそれでもまだ遅いですね。ちょこっと書いては走らせるのサイクルを回すには耐えられません。

そういえばログ全体のファイルサイズを確認してなかった。

```
$ du -h
...
774M   .
```

意外と小さい。まだチューニングの余地はありそうです。メモリアロケーションを減らしましょうか、それとも時間のパースを手書きでやりましょうか。

いいえ。もっと簡単に速くする方法があります。`--release`をつけて実行すればOKです。


```
$ cargo run --release
...
...
cargo run --release  3.63s user 5.44s system 203% cpu 4.453 total
```

`(/ 53.289 4.453);=> 11.966988547046935` 。12倍速くなりました。1ディレクトリ700以上のファイルがあってそれぞれにスレッドを作っているのでそろそろそのオーバーヘッドが効いてきてますね。

スレッドプールを試してみたい気もしますがひとまずこれで置いておきましょう。

次は後編、DBへのインポートです。

蛇足ですが行のパースのところは正規表現でも出来ましたね。そっちの方が好きな方は試してみて下さい。
