---
categories: [Rust, Linux]
date: 2021-02-01T00:09:28+09:00
title: "epollで作るチャットサーバ"
---

κeenです。
普段お世話になってるけど使ったことのないAPIを叩いてみよう、ということで `epoll(7)` を使ってみます。

<!--more-->

# Epollとは

[`epoll(7)`](https://linuxjm.osdn.jp/html/LDP_man-pages/man7/epoll.7.html) はLinux固有のAPIで、パイプやソケットなど出入力に待ちが発生する対象を複数同時に待つ、いわゆるIOの多重化の機能を提供します。
同等のことをするAPIはUNIX全般で使える `select(2)` や `poll(2)` などもありますが、使い勝手やパフォーマンスの面で `epoll` が勝るようです。

一方で `epoll(7)` はLinux固有のAPIなのでmacOSやFreeBSDでは使えません。それらのOSでは別のAPI（`kqueue(2)`）を叩くことになります。
Rustなら[mio](https://github.com/tokio-rs/mio)が互換レイヤとして存在し、LinuxでもBSDでもmacOSでも使えるライブラリになっています。
mioはTokioなんかで使われていますね。ですが今回はその内側を覗いてみようという趣旨なので直接 `epoll(7)` を叩きます。。


# Epoll API

`epoll(7)` と呼んでますが、実際のAPIは複数の関数からなります。


* `epoll_create` , `epoll_create1`: 初期化
* `epoll_ctl`: 待つ対象の登録、変更などの操作
* `epoll_wait`, `epoll_pwait`: 実際に待つ

ざっくりと、Rust風の疑似コードで以下のようなコードを書くことになります。

```rust
// 初期化
let epoll_fd = epoll_create();

// 監視対象の追加
let some_fd = /* 監視対象 */;
let some_event = /* 監視するイベント + データ */;
let epoll_ctl(epoll_fd, EPOLL_CTL_ADD, some_fd, some_event);

let events = /* バッファの確保 */
let timeout = -1; // -1 = タイムアウトしない

loop {
  // 実際に待つ
  let n = epoll_wait(epoll_fd, events, timeout);

  for event in events[0..n] {
    // 何かする

    // 次に必要なイベントを登録する
    epoll_ctl(epoll_fd, EPOLL_CTL_ADD, some_fd, some_event);
  }
}
```

`epoll_create()` してループの中で `epoll_wait` します。
そして適宜 `epoll_ctl` でpoll対象を操作します。

`fd` はFile Descriptorのことです。fdについて知らない方は適当にググって下さい。

ここで、 `epoll_wait` の結果得られるのは `event` であることに注目です。
`event` の中身はCで以下のように定義されています。

``` c
typedef union epoll_data {
    void        *ptr;
    int          fd;
    uint32_t     u32;
    uint64_t     u64;
} epoll_data_t;

struct epoll_event {
    uint32_t     events;      /* Epoll events */
    epoll_data_t data;        /* User data variable */
};
```

`events` に何が起きたか（read/writeが可能になった、クライアントが切断した、など）、`data` に `epoll_ctl` のときに任意に持たせたデータを保持しています。
簡単には `fd` を持たせればそのまま読み書きができます。
しかし複雑なアプリケーションになるともうちょっと色々データを持たせたくなります。
そこで `data` にIDを登録しておいて、アプリケーション側で `HashMap` なんかで実際のデータを管理することになるでしょう。



# Epollで作るチャットサーバ in Rust

IOの多重化を必要とするアプリケーションとしてチャットサーバをRustで作ってみようと思います。コードはGitHubに上げておくので、この記事内では要点を絞って解説していきます。

* [epoll-chat](https://github.com/KeenS/epoll-chat)

作るチャットサーバは簡素なものです。telnetで接続すると名前を聞かれるので答えます。
するとログインに相当し、以後入力したものがそのまま自分の発言として他のユーザに表示されます。
入力は1行（telnetなので改行は `\r\n`）が1単位です。
ログアウトするにはコネクションを切断します。

このチャットサーバを作っていきましょう。

## Nix

Rustからepollを叩く手段はいくつかありますが、ここでは [`nix`](https://crates.io/crates/nix)によるラッパを使います。[`nix::sys::epoll` のドキュメント](https://docs.rs/nix/0.19.1/nix/sys/epoll/index.html)を見て分かるとおり割と素直にRustでラップできているので使い勝手がよいです。

例えば `epoll_create` は以下のように使えます。

``` rust
use nix::sys::epoll::*;
let epoll_fd = epoll_create().expect("can create epoll queue")
```

`unsafe` がなく、しかも `epoll_create` はRustの `Result` として返っているのでRustにとって自然に使えるようになっています。

## コネクションの受け付け

`TcpListener` でアドレスにバインドしておきます。そして `epoll` で使うために `set_nonblocking` しておきます。

``` rust
let listener = TcpListener::bind(addr)?;
listener.set_nonblocking(true)?;
```

これも新しいコネクションがきたことを `epoll` で観察できるので `epoll_ctl` で監視対象に追加します。

``` rust
const NEW_CONNECTION: u64 = 100;
let event = EpollEvent::new(EpollFlags::EPOLLRDHUP | EpollFlags::EPOLLIN, NEW_CONNECTION);
let fd = listener.as_raw_fd()
epoll_ctl(epoll_fd, EpollOp::EpollCtlAdd, fd, event)
    .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
```

ポイントは以下の2点です。

* `listener` のfdを `EPOLLRDHUP` （切断検知）と `EPOLLIN` （read ready待ち）で監視対象にいれる
* `listener` に固有のID 、 `NEW_CONNECTION` を与える

上の `epoll(7)` の紹介のところでも触れたのですが、 `epoll_wait` で読み取りできるようになった `fd` を管理するのにIDを使います。
ここでは `listener` の管理をするIDに `NEW_CONNECTION` を使っています。

### メインループ

受け付け準備ができたらメインループを書きましょう。
新しいコネクションがきたら `handle_new_connection` を呼ぶことにして、以下のようなコードを書きます。


``` rust
let mut events = vec![EpollEvent::empty(); 1024];
loop {
    println!("clients connected: {}", self.users.len());

    let res = match epoll_wait(epoll_fd, &mut events, -1) {
        Ok(v) => v,
        Err(e) => panic!("error during epoll wait: {}", e),
    };

    for ev in &events[0..res] {
        match ev.data() {
            NEW_CONNECTION => handle_new_connection(&listener)?,
            _ => todo!(),
        }
    }
}
```


ポイントは以下の3点です。

* `epoll_wait` の第2引数に事前に確保した配列を渡す
* `epoll_wait` の第3引数にタイムアウト値を渡す（-1はタイムアウトしない）
* `epoll_wait` の返り値で読み書き可能になった対象の数を知る


## ユーザを作る

`handle_new_connection` では `accept` でコネクションを取り出せます。

``` rust
fn handle_new_connection(listener: &TcpListener) -> io::Result<()> {
    match listener.accept() {
        Ok((stream, addr)) => {
            stream.set_nonblocking(true)?;
            // do something
        }
        Err(e) => eprintln!("couldn't accept: {}", e),
    };
    Ok(())
}
```

この `stream` が1クライアントに相当します。これを管理する構造体 `User` を定義しましょう。

``` rust
#[derive(Debug)]
struct User {
    stream: TcpStream,
    queue: VecDeque<Arc<String>>,
    // and other fields
}

impl User {
    fn new(stream: TcpStream) -> Self {
        Self {
            stream,
            queue: VecDeque::new(),
            // and other fields
        }
    }

    fn push_message(&mut self, message: Arc<String>) {
        self.queue.push_back(message)
    }

    fn pop_message(&mut self) -> Option<Arc<String>> {
        self.queue.pop_front()
    }

    fn raw_fd(&self) -> i32 {
        self.stream.as_raw_fd()
    }
}
```


`User` はコネクションとそのユーザに送られるメッセージキューを保持します。
メッセージを実際に送る部分は後で実装するとして、ひとまず初回接続時のメッセージを出すのはこう書けます。

``` rust
        Ok((stream, addr)) => {
            stream.set_nonblocking(true)?;
            let mut user = User::new(stream);
            user.push_message(Arc::new("Enter your name: ".into()));
            // and others
        }
```

さて、ここで作った `User` を `epoll` に登録したりIDで管理したりしたいのですが、このままだとやりづらいですね。`Server` 構造体を作りましょう。

## サーバを作る

ユーザなどを管理するためのサーバを作りましょう。


### ID生成器

…とその前に `User` のIDを管理するためのID生成器を作ります。
`User` のIDはメインループで使った `NEW_CONNECTION` のものと被ってはいけないので `NEW_CONNECTION` の次の番号から返すようにしておきます。

``` rust
#[derive(Debug)]
struct IdGenerator(u64);

impl IdGenerator {
    fn new() -> Self {
        Self(NEW_CONNECTION)
    }

    fn next(&mut self) -> u64 {
        self.0 += 1;
        self.0
    }
}

```

### `Server`

`Server` を以下のように定義します。

``` rust
#[derive(Debug)]
struct Server {
    epoll_fd: RawFd,
    id_gen: IdGenerator,
    users: HashMap<u64, User>,
}
```

今まで出てきたコードは以下のように整理しておきましょう。

``` rust
impl User {
    fn new() -> Self {
        Self {
            epoll_fd: epoll_create().expect("can create epoll queue"),
            id_gen: IdGenerator::new(),
            users: HashMap::new(),
        }
    }

    fn add_interest(&self, fd: RawFd, event: &mut EpollEvent) -> io::Result<()> {
        epoll_ctl(self.epoll_fd, EpollOp::EpollCtlAdd, fd, event)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
    }


    fn handle_new_connection(&mut self, listener: &TcpListener) -> io::Result<()> {
        // ...
    }

    fn run<A: ToSocketAddrs>(&mut self, addr: A) -> io::Result<()> {
        let listener = TcpListener::bind(addr)?;
        listener.set_nonblocking(true)?;
        self.add_interest(
            listener.as_raw_fd(),
            &mut listener_read_event(NEW_CONNECTION),
        )?;

        let mut events = vec![EpollEvent::empty(); 1024];

        loop {
            println!("clients connected: {}", self.users.len());

            let res = match epoll_wait(self.epoll_fd, &mut events, -1) {
                Ok(v) => v,
                Err(e) => panic!("error during epoll wait: {}", e),
            };

            for ev in &events[0..res] {
                match ev.data() {
                    NEW_CONNECTION => self.handle_new_connection(&listener)?,
                    _ => todo!(),
                }
            }
        }
    }
}
```


監視するイベントについてもここでまとめてしまっています。
イベントについては色々ありえるのですが、今回のチャットサーバにおいては2種類しか必要ありません。すなわちコネクションの切断+read readyか、それに加えてwrite readyまで見るかです。それを以下のように定義してしまいます。

``` rust
fn listener_read_event(key: u64) -> EpollEvent {
    EpollEvent::new(EpollFlags::EPOLLRDHUP | EpollFlags::EPOLLIN, key)
}

fn listener_read_write_event(key: u64) -> EpollEvent {
    EpollEvent::new(
        EpollFlags::EPOLLRDHUP | EpollFlags::EPOLLIN | EpollFlags::EPOLLOUT,
        key,
    )
}
```


### ユーザの管理

これからユーザを管理するので、そのためのメソッドを生やしておきます。

``` rust
impl Server {
    fn add_user(&mut self, key: u64, user: User) {
        self.users.insert(key, user);
    }

    fn watch(&self, user: &User, event: &mut EpollEvent) -> io::Result<()> {
        epoll_ctl(self.epoll_fd, EpollOp::EpollCtlAdd, user.raw_fd(), event)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
    }

    fn change_event(&self, user: &User, event: &mut EpollEvent) -> io::Result<()> {
        epoll_ctl(self.epoll_fd, EpollOp::EpollCtlMod, user.raw_fd(), event)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
    }

    fn unwatch(&self, user: &User) -> io::Result<()> {
        epoll_ctl(self.epoll_fd, EpollOp::EpollCtlDel, user.raw_fd(), None)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
    }
}
```

`add_user` はいいとして、 `watch` / `change_event` / `unwatch` について触れます。これらは全て `epoll_ctl` のラッパです。
`watch` は `epoll` への登録、 `unwatch` は登録解除、 `change_event` は監視イベントの変更を担当し、それぞれ `EpollCtlAdd` 、 `EpollCtlDel` 、 `EpollCtlMod` を呼んでいます。


これらのAPIを使って `handle_new_connection` を完成させられます。


``` rust
    fn handle_new_connection(&mut self, listener: &TcpListener) -> io::Result<()> {
        match listener.accept() {
            Ok((stream, addr)) => {
                stream.set_nonblocking(true)?;
                println!("new client: {}", addr);
                let key = self.id_gen.next();
                let mut user = User::new(stream);
                user.push_message(Arc::new("Enter your name: ".into()));
                self.watch(&user, &mut listener_read_write_event(key))?;
                self.add_user(key, user);
            }
            Err(e) => eprintln!("couldn't accept: {}", e),
        };
        Ok(())
    }
```

`user` を `watch` していますね。

## ユーザイベントを処理する

ここまでで `User` を作ってメッセージをキュー入れ、 `epoll` に登録するところまできました。
`epoll_wait` で `User` も監視できるようになっているので、 `User` で何か動向があったときの処理も書きましょう。
処理本体は `handle_user_event` というメソッドに書くとして、メインループでは以下のように `NEW_CONNECTION` か否かで処理を振り分けることになるでしょう。

``` rust
loop {
    // ...

    for ev in &events[0..res] {
        match ev.data() {
            NEW_CONNECTION => self.handle_new_connection(&listener)?,
            // ここ↓↓
            _ => self.handle_user_event(*ev)?,
        }
    }
}
```


そして肝心の `handle_user_event` についてです。
このメソッドユーザに動向があったときに毎回呼ばれるので内部でどのイベントで呼ばれたのかで処理を振り分けることにします。
中々本丸に到達しませんね。

``` rust
    fn handle_user_event(&mut self, ev: EpollEvent) -> io::Result<()> {
        let key = ev.data();
        let events = ev.events();

        if let Some(mut user) = self.users.remove(&key) {
            if events.contains(EpollFlags::EPOLLIN) {
                self.handle_user_readable(&mut user)?;
            }

            if events.contains(EpollFlags::EPOLLOUT) {
                self.handle_user_writable(&mut user, key)?;
            }

            if events.contains(EpollFlags::EPOLLRDHUP) {
                self.handle_user_closed(&mut user)?;
            } else {
                self.add_user(key, user)
            }
        }
        Ok(())
    }
```

ところでこのコード、 `if { ... } else if { ... }` ではなく `if { ... }` の連続である点に注意して下さい。
`User` が同時にreadableかつwritableになることもあるので複数のイベントを同時に処理します。

それではwritableの場合の処理に進みましょう。 `User` がwritableになったらwritableのコールバックを呼びます。そしてキューが空になっていたらwritableの購読をやめます。

``` rust
    fn handle_user_writable(&mut self, user: &mut User, key: u64) -> io::Result<()> {
        user.write_cb()?;
        if user.queue.is_empty() {
            self.change_event(&user, &mut listener_read_event(key))?;
        }
        Ok(())
    }
```


`User::write_cb` はキューにあるメッセージを書けるだけ書くだけです。


``` rust
    fn write_cb(&mut self) -> io::Result<()> {
        while let Some(msg) = self.pop_message() {
            match self.stream.write(msg.as_bytes()) {
                Ok(_) => (),
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => break,
                Err(e) => eprintln!("could not write message to {:?}, {}", self.name, e),
            };
        }

        Ok(())
    }
```

`set_nonblocking` しているのでブロックしそうになったら（＝OS内部のバッファが一杯になったら） `WouldBlock` が返ります。そうしたら書き出しを止めて次にバッファが空になるのを `epoll_wait` で待つようにします。

ざっとこういう流れで `epoll` を使っていきます。

`handle_user_readable` については状態を持っていてややこしいのでこの記事では触れないことにします。`handle_user_closed` は自明なので気にしなくていいですよね。

書き終わったら実行してみましょう。

## 動作例

サーバとクライアント2つがいるのでターミナル（A、B、C）を2つ開きます。

まずはターミナルCでサーバを立てます。

``` text
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `target/debug/epoll-chat`
clients connected: 0
```


ターミナルAでAliceをログインさせましょう。

``` text
$ telnet localhost 8000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Enter your name: alice
```


ターミナルBでBobをログインさせます。

``` text
$ telnet localhost 8000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Enter your name: bob
```

あとはなんか適当に会話するとチャットっぽくなります。
tmuxで左右分割をしたターミナルでの動作例を貼っておきます。

``` text
$ telnet localhost 8000         │$ telnet localhost 8000
Trying 127.0.0.1...             │Trying 127.0.0.1...
Connected to localhost.         │Connected to localhost.
Escape character is '^]'.       │Escape character is '^]'.
Enter your name: alice          │Enter your name: bob
bob: hello                      │hello
hello, I'm Alice                │alice: hello, I'm Alice
bob: I'm Bob                    │I'm Bob
bob: bye                        │bye
^]                              │^]
telnet> Connection closed.      │telnet> Connection closed.
```

こんな具合にチャットができます。


# まとめ

IOの多重化をしてくれるEpoll APIを使ってチャットサーバを作りました。ざっくりと `epoll_create` で用意したfdに監視したいfdを `epoll_ctl` で登録し、 `epoll_wait` で待つループを書けば作れました。
関連するURLを貼っておきます。

* [epoll-chat](https://github.com/KeenS/epoll-chat)：今回のコード
* [Basic non-blocking IO using epoll in Rust - zupzup](https://zupzup.org/epoll-with-rust/)：今回参考にした記事

また、Epoll APIに関連していくつか面白fdを作れるAPIが存在するようです。

* [timerfd](http://linuxjm.osdn.jp/html/LDP_man-pages/man2/timerfd_create.2.html)
* [signalfd](http://linuxjm.osdn.jp/html/LDP_man-pages/man2/signalfd.2.html)
* [eventfd](http://linuxjm.osdn.jp/html/LDP_man-pages/man2/eventfd.2.html)

興味のある方は触ってみて下さい。
