---
categories: [Linux, Rust, 非同期]
date: 2021-02-08T20:33:15+09:00
title: "io_uringで高速IO処理（？）"
---

κeenです。普段お世話になってるけど使ったことのないAPIを叩いてみよう、ということで `io_uring` を使ってみます。

<!--more-->

io_uringが何なのかは以下の記事が詳しかったです。

* [Linuxにおける非同期IOの実装について - Qiita](https://qiita.com/tmsn/items/0b9e5f84f9fbc56c1c82)

ざっくり、io_uringはLinuxで非同期IOをするためのAPIです。
ユーザランドとカーネルランドで2つのキューを共有し、そこを通じて会話をします。
1つのキューはユーザランドからカーネルへのリクエストの提出用、もう1つのキューはカーネルからユーザランドへの完了の通知用です。
また、epollを制御するためにfdもあります。こんなイメージですかね。

```text
         user

        |   ^   ^
submit |||  || ||| completion
-------|||--fd-|||----------
       |||  || |||
        v    v  |

        kernel
```


それぞれのキューはリングバッファで実装されています。

io_uringではIO処理の依頼とその完了待ちが分離されているので処理を発行しておいて自分のタイミングで結果を受け取ることができます。例えば3つのIO処理をするときにio_uringなしに普通の処理をやろうとすると3回分IOの待ちが発生します。

```text
|    |
+--->|
|    |
|<---+
+--->|
|    |
|<---+
+--->|
|    |
|<---+
```

しかしio_uringを使えばIOリクエストを矢継ぎ早に送れるので待ち時間を短縮できます。

```text
|    |
+--->|
+--->|
+--->|
|    |
|<---+
|<---+
|<---+
```

特にSSDだとコマンドキューにどれだけ命令を詰め込めるかが勝負らしい（？）のでio_uringだと有利なのかもしれません。

ということでio_uringを使ってIO処理を高速化してみましょう。

# liburing

io_uringはLinuxのAPIですが、そのまま使うにはいささか手間が大きいです。
今までカーネルで全てやってきたことをユーザでコントロールできるようにするということはユーザでやることが増えるということでもあります。そこでio_uringを簡単に使えるライブラリとしてliburingがあります。

* [axboe/liburing](https://github.com/axboe/liburing)

Ubuntuならaptで入ります。

```shell
$ apt get install liburing-dev
```

liburingの有無で使い勝手がどう違うかは以下のリポジトリが参考になりました。

* [shuveb/io_uring-by-example: A companion repository for the io_uring by Example article series](https://github.com/shuveb/io_uring-by-example)

まあ、今回私はRustのラッパを使うのでliburingは関係ないんですけどね。

# Rustからio_uringを使ってみる

ということでio_uringを使ってみましょう。Rustからだといくつかラッパが存在するようですがTokioのio_uringラッパを使ってみましょう。

* [tokio-rs/io-uring: The `io_uring` library for Rust](https://github.com/tokio-rs/io-uring)

これはliburingを使わずにlibcのio_uringを直接ラップしたライブラリです。


やることはシンプルに 'aaaaaaa...' と5GiBの 'a' が書かれたファイルを作ってみます。
今回のコード全体は以下に置いておきます。

* [KeenS/io-uring-write-file](https://github.com/KeenS/io-uring-write-file)

以後は重要なところだけを拾って解説していきます。

## 標準ライブラリでの実装

io_uringを使ずに標準ライブラリだけで書いたシンプルなコードであれば以下のように書けるでしょう。

```rust
use std::fs::File;
use std::io;
use std::io::prelude::*;

pub const TOTAL: usize = 5 * 1024 * 1024 * 1024;
pub static DATA: [u8; 4096] = [0x61; 4096];

let mut file = File::create("std.text")?;
let npages = TOTAL / DATA.len();
for _ in 0..npages {
    file.write_all(&DATA)?;
}
```

これと同等のことをio_uringで実装します。

## io-uringを使った実装

io_uringで書き直してみます。
まずはファイルを開いておきましょう。

```rust
use std::fs::File;

let file = File::create("iouring.text")?;
```

そしてio_uringを初期化します。

```rust
use io_uring::opcode::types::Fd;
use io_uring::opcode::Write;
use io_uring::IoUring;

let mut uring = IoUring::new(2048)?;
```

引数はリングバッファのサイズです。
2048という数値はまあ、なんか雰囲気で決めました。
Linuxのio_uringではsubmitのキュー（sq）とcompletionのキュー（cq）それぞれ長さを指定できるはずですがTokioのバインディングでは両方とも同じ長さを指定しているようです。

上の方でio_uringにはfd、sq、cqがあるといいましたが、Tokioのバインディングも3つのコンポーネントが取り出せます。

```rust
let (submitter, sq, cq) = uring.split();
```

submitterはfd経由でsubmissionを管理するからのネーミングなんでしょう。

一旦データサイズとかを用意して


```rust
pub const TOTAL: usize = 5 * 1024 * 1024 * 1024;
pub static DATA: [u8; 4096] = [0x61; 4096];

let npages = TOTAL / DATA.len();
```

タスクを発行したときに完了まで待たないので完了したタスクを管理する変数を別途用意します。

```rust
let mut completed = 0;
```

さて、ここからIOリクエストを提出する段に入ります。
IOリクエストを `entry` として、以下の3行でIOリクエストを提出できます。

```rust
unsafe {
    let entry = /* ... */
    sq.available().push(entry).map_err(...)?
    submitter.submit()?;
}
```


`sq.available()` というのがTokioのバインディングのユニークな点で、その時点で空いているキューのビューを提供します。
一瞬「それスレッドセーフじゃなくね？」って思いますがRustなので大丈夫です。所有権システムが守ってくれます。

この `submit` ですが一気に複数のIOリクエストを溜めて一気に提出できます。
`submit` の回数は少ない方がカーネルとのやり取りが減って速そうなので1024回分のリクエストを溜めて一気に送ってみます。

```rust
for j in 0..1024 {
    unsafe {
        let entry = /* ... */;
        sq.available().push(entry)?;
    }
}
submitter.submit()?;
```

つまり、全体はこういうイメージです。

```rust
let outer = npages / 1024;
for i in 0..outer {
    for j in 0..1024 {
        unsafe {
            let entry = /* ... */;
            sq.available().push(entry).map_err(...)?;
        }
    }
    submitter.submit()?;
}

```


ここで先送りにしていた `entry` を作りましょう。
Cだと `IORING_OP_XXX` で操作を指定してデータを詰めるんですがTokioのバインディングはOPごとに構造体が用意されていて、それをビルダとして `Entry` を作る設計のようです。

単刀直入にentryを作るコードを書くとこうです。

```rust
// libc crateをdependencyに追加しておく
use libc::off_t;
use io_uring::opcode::types::Fd;
use io_uring::opcode::Write;

let n = i * 1024 + j;
let entry = Write::new(Fd(file.as_raw_fd()), &DATA as *const u8, DATA.len() as u32)
    .offset((n * DATA.len()) as off_t)
    .build();
```

要点を解説すると以下です。

* `Write` で書き込みのIOリクエスト
* `Write` にはファイルを表わすfd、バッファと長さを指定
  + 今回は固定の内容を書き込むのでバッファを使い回してる
* そのままだとファイルの先頭に書くだけなので `offset` でファイルのどこに書くかを指定

総合して、以下のコードになります。

```rust
let outer = npages / 1024;
for i in 0..outer {
    for j in 0..1024 {
        let n = i * 1024 + j;
        unsafe {
            let entry = Write::new(Fd(file.as_raw_fd()), &DATA as *const u8, DATA.len() as u32)
                .offset((n * DATA.len()) as off_t)
                .build();
            sq.available().push(entry).map_err(...)?;
        }
    }
    submitter.submit()?;
}
```

なんですが、これだと `cq` が詰まってしまうらしくエラーになります。device busyのエラーだったんですがなんでなんでしょうね。
なので適度に `cq` も消費します。
今回は返り値には興味がないので返り値が成功かどうかはみずにそのまま消費します。
`cq.available()` はイテレータになっているので `count` を呼んであげると `completed` も計算できて丁度よさそうです。

```rust
for i in 0..outer {
    for j in 0..1024 { /* ... */ }

    submitter.submit()?;
    // cqを消費する
    if i % 4 == 0 {
        completed += cq.available().count();
    }
}
```


これでIOリクエストを提出できました。

次はIOリクエストの完了待ちです。それには `submitter` の `submit_and_wait` を使うとよさそうです。
残ってる分のリクエストを順次消費していきます。

```rust
while completed != npages {
    let rest = npages - completed;
    submitter.submit_and_wait(rest)?;
    let count = cq.available().count();
    completed += count
}
```

`rest` を計算していますがそもそもキューの長さが2048なので最大で2048までしか返ってきませんがまあ、気にしなくてよいでしょう。どのみち `while` で完了するまで待ちます。


## ベンチマーク

高速なIO処理を目指して書いたのでベンチマークを取ってみましょう。
雑に処理の開始と終了の時間の差をとります。
標準ライブラリを使った実装をstd、io_uringを使った実装をuringとします。

| name  | time (ms) |
|-------|----------:|
| std   |      2031 |
| uring |      4486 |


…………。stdの方が倍以上速いという結果になりました。
因みに `time` コマンドによるとuringの方が使っているCPU時間は短いです。

```text
$ /usr/bin/time ./target/release/std
2031 ms
0.04user 1.98system 0:02.03elapsed 99%CPU (0avgtext+0avgdata 1888maxresident)k
0inputs+10485760outputs (0major+90minor)pagefaults 0swaps
$ /usr/bin/time ./target/release/iouring
4486 ms
0.02user 0.19system 0:04.48elapsed 4%CPU (0avgtext+0avgdata 1948maxresident)k
0inputs+0outputs (0major+91minor)pagefaults 0swaps
```

キューの待ちでも発生したんですかねぇ？

## io_uringの高速化

ちょっとこのままだと収まりが悪いのでio_uringを高速化してみます。
ポイントは2つあって、ファイルfdの事前登録とバッファの事前登録です。

カーネルのことはよく分からないんですがfdをカーネルに渡してそれをカーネル側であれこれしてもらうのは時間がかかるらしいです。
そこでカーネルに事前に使う予定のfdを事前に渡しておけばその時間を短縮できるらしいです。


もう1つがバッファの事前登録です。これは分かりやすいですね。
`submit` の度にRustのメモリ領域からカーネル内にデータをコピーして使っています。
これは無駄なので事前にデータ提出に使うバッファをユーザランドとカーネルランドで共有しておけばコピーの手間が省けます。
ただしこれにはカーネルと共有できるメモリを確保したり共有メモリ専用のIOリクエストを使ったりの作業が必要になります。

これら2つの操作はlibc的には `io_uring_register` で、Tokioのio-uring的にはsubmitterに生えてる `register_xxx` 系の関数でやります。

### ファイルの事前登録

これは単純ですね。関数を呼ぶだけです。


```rust
submitter.register_files(&[file.as_raw_fd()])?;
```

### バッファの事前登録

これはちょっと厄介です。まず、 `mmap` でANONYMOUSなメモリを確保します。

```rust
use libc::{mmap, MAP_ANONYMOUS, MAP_PRIVATE, PROT_READ, PROT_WRITE};
use std::ffi::c_void;
use std::ptr::null_mut;

let buf;
unsafe {
    buf = mmap(
        null_mut::<c_void>(),
        DATA.len(),
        PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS,
        -1,
        0,
    );
    if (buf as isize) == -1 {
        return Err(io::Error::last_os_error());
    }

    for i in 0..DATA.len() {
        *((buf as *mut u8).offset(i as isize)) = DATA[i];
    }
}
```

それを `register_buffers` します。

```rust
use libc::iovec;

submitter.register_buffers(&[iovec {
    iov_base: buf,
    iov_len: DATA.len(),
}])?;
```

そして `entry` を作るところで `Write` の代わりに `WriteFixed` を使います。

```rust
let entry =
    WriteFixed::new(Fd(file.as_raw_fd()), buf as *const u8, DATA.len() as u32, 0)
        .offset((n * DATA.len()) as off_t)
        .build();
```

`WriteFixed` は `Write` と比べて1つ引数が増えており、最後の引数で `register` したどのバッファを使うのかを指定します。
全体を知りたい方はGitHubのコードを読んで下さい。

### ベンチマーク

これを走らせてみます。uring'と呼びましょうか。

| name   | time (ms) |
|--------|----------:|
| uring' |      1917 |


わずかにstdよりも速く見えますが、測定でそこそこバラつきがあったので多分有意な差はないです。
少なくとも工夫のないio_uringよりは速いことだけは分かります。

因みにですが `time` コマンドによると相変わらずCPU使用率が低いようです。

```text
$ /usr/bin/time ./target/release/iouring_tuned
1917 ms
0.02user 0.29system 0:01.97elapsed 16%CPU (0avgtext+0avgdata 1956maxresident)k
0inputs+64outputs (0major+91minor)pagefaults 0swaps
```


stdと実時間では変わらないけどsystemのCPU使用時間と全体のCPU使用率が低いのは気になりますね。

私にはこれ以上深く追求するための知識が足りないのでここまでとします。
perfの結果なんかも微妙に違ってたんですが解釈できないデータを貼っても意味がないので気になった方は手元で走らせてみて下さい。

そもそも、 `dd` で同様の処理をしてもRustのコードより遅いのでそういうもんなのかもしれません。

```text
$ /bin/time dd if=/dev/zero of=dd.bin bs=4K count=1310720
1310720+0 レコード入力
1310720+0 レコード出力
5368709120 bytes (5.4 GB, 5.0 GiB) copied, 2.88155 s, 1.9 GB/s
0.09user 2.89system 0:02.99elapsed 99%CPU (0avgtext+0avgdata 2352maxresident)k
0inputs+10486112outputs (0major+91minor)pagefaults 0swaps
```

`dd` がそこまでパフォーマンスを気にして書かれてるか分かりませんが。


# まとめ

io_uring APIの雑な紹介とそれをRustから叩くコード3種を紹介しました。
io_uringで処理を高速化する目論みでしたが失敗しました。
よく考えたらどのみちOSのバッファに載って処理されるのでio_uringあんまり関係なかったのではという説もあります。
やるんなら高速化ではなくマルチタスクの方がよかったかもしれませんね。

今出すかよって感じですが実行環境はM.2のNVMe SSD x 4の上にbtrfsでRAID 5を組んだファイルシステムで、ちょっとパフォーマンス特性に癖があったのかもしれません。
調べたらbtrfsはジャーナリングする関係でシーケンシャルアクセスに弱いとかSSDはブロック単位で操作するからどうこうとか色々出てくるのですが、こういうのはfolkloreとかも多くてどこまで信じていいのか分からないので追求しないことにしました。
@naota さんとかが解説してくれないかな…。因みにカーネルのバージョンは5.8.0-41です。

再掲になりますが今回のコードはこちらです。

* [KeenS/io-uring-write-file](https://github.com/KeenS/io-uring-write-file)

何回か走らせて平均と分散とんないと何も議論できなくない？とかファイルキャッシュどうなってるの？とか色々気になるかと思います。そういう方は手元で走らせて実験してみて下さい。

最後に、参考にした資料を貼っておきます。

* [Linuxにおける非同期IOの実装について - Qiita](https://qiita.com/tmsn/items/0b9e5f84f9fbc56c1c82)
* [axboe/liburing](https://github.com/axboe/liburing)
  + 特にexamplesとREADMEに貼ってあるPDF
* [shuveb/io_uring-by-example: A companion repository for the io_uring by Example article series](https://github.com/shuveb/io_uring-by-example)
* `man io_uring_enter` 、 `man io_uring_register`
* [io_uring - Rust](https://docs.rs/io-uring/0.4.0/io_uring/)

締まりのない記事になりましたが参考になる方がいれば幸いです。
