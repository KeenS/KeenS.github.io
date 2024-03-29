---
categories: [Rust]
date: 2020-11-15T15:54:34+09:00
description:
title: "Rust with Fearless Concurrency"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Rust with Fearless Concurrency
----------------------

<!-- .slide: class="center" -->

===
# About Me
---------
![κeenのアイコン](/images/kappa2_vest.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

* κeen
* [@blackenedgold](https://twitter.com/blackenedgold)
* GitHub: [KeenS](https://github.com/KeenS)
* GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
* [Idein Inc.](https://idein.jp/)のエンジニア
* Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# Rust with Fearless Concurrency
---------------------------------

* 前半Rustの紹介
* 後半RustのFearless Concurrencyについて

Note:

普段のRustのイントロだと言語機能の紹介だけで50分まるまる使う。
今回はConcurrencyに言及してほしいという依頼だったので言語機能の紹介が少なめ

===
# Rustとは
------------

* 比較的最近できた **システムプログラミング言語**
  + 2015年に1.0リリース
  + C/C++並に[速い](https://benchmarksgame-team.pages.debian.net/benchmarksgame/which-programs-are-fastest.html)
* システムソフトウェアを作るのに便利
  + [OS](https://redox-os.org), [ブラウザ](https://servo.org), [DB](https://www.influxdata.com/blog/announcing-influxdb-iox/)など
* Rustは「安全」
  + 未定義動作がない

===

# Rustは安全
------------

* ざっくり言うとC/C++でいう未定義動作がない
* メモリ安全
  + Null PointerやDangling Pointerなど発生しない機構
  + FYI: [「Chrome」の深刻なセキュリティ脆弱性、70％はメモリー安全性の問題](https://japan.zdnet.com/article/35154338/)
* データ競合がない
  + 今日の後半の話
* などなど

===
# 安全である価値
----------------

* 重要なパーツからRustに置き換えられていく流れがきている
* マイクロソフトがWindowsのコンポーネントの一部にRustを使うことを[検討](https://msrc-blog.microsoft.com/2019/11/07/using-rust-in-windows/)
* Amazonのセキュリティコンテナの実装に[使われてる](https://github.com/firecracker-microvm/firecracker/)
* Rust製TLSライブラリの監査の結果[高評価](https://github.com/ctz/rustls/blob/master/audit/TLS-01-report.pdf)
* cURLにRustバックエンド追加の[計画](https://www.abetterinternet.org/post/memory-safe-curl/)
  + ついでに↑のRust製TLSライブラリも

===
# Hello, Rust
--------------


[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=811e91c9fd9230f2c2d1db1182abe1ac)

``` rust
fn main() {
    println!("Hello, Rust");
}
```

===
# FizzBuzz
----------

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=7e5653ef7b1b7fb2cb292ed22f448d56)

``` rust
fn fizz_buzz(n: u64) {
    for i in 0..n {
        match (i % 3, i % 5) {
            (0, 0) => println!("FizzBuzz"),
            (0, _) => println!("Fizz"),
            (_, 0) => println!("Buzz"),
            _      => println!("{}", i),
        }
    }
}
```

===
# Sum?
------

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=3eda2f981b8a001d791983e912f468cb)

``` rust
fn sum(vec: Vec<i64>) -> i64 {
    let result = 0;
    for e in vec {
        result += e;
    }
    return result;
}
```

===
# Sum
------

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=0f6bfff337d14d071e309052ee1f01f9)

``` rust
fn sum(vec: Vec<i64>) -> i64 {
    let mut result = 0;
    for e in vec {
        result += e;
    }
    return result;
}
```

===
# Sum!
------

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=8c040eb82c4c117d103f211bf109c17e)

``` rust
fn sum(vec: Vec<i64>) -> i64 {
    vec
      .into_iter()
      .fold(0, |e, acc| e + acc)
}
```

Note:
returnがない

===
# Rustの特徴
------------

* MLやHaskellなどの関数型言語から影響を受けた
  + 変数が不変だったり式指向だったり
  + 代数的データ型とパターンマッチなど
* `obj.method()` などの構文はあるが、クラスがない
* メモリを自動で管理しつつもGCなどのランタイムがない

===

# 構造体
---------
* クラスと継承はない
  + 継承は最近はアンチパターンとされつつある（要出典）

``` rust
struct Point {
    x: f32,
    y: f32,
}

struct Rectangle {
    top_left: Point,
    bottom_right: Point,
}

```

===
# 構造体のメモリレイアウト
--------------------------

ポインタを挟まず `f32` が4つ並ぶ

``` text
Rectangle
+-----+-----+-----+-----+
| f32 | f32 | f32 | f32 |
+-----+-----+-----+-----+
```

===

# 構造体とメソッドの例
---------------------

``` rust
impl Rectangle {
    fn space(&self) -> f32 {
        let width = self.bottom_right.x - self.top_left.x;
        let height = self.top_left.y - self.bottom_right.y;
        width * height
    }
}

let rect = Rectangle {
    top_left: Point { x: 0.0, y: 0.0 },
    bottom_right: Point { x: 200.0, y: 100.0 },
};
rect.space();
```

===
# 列挙型
--------

* 可能な値を列挙できる

from [std::net::Shutdown](https://doc.rust-lang.org/std/net/enum.Shutdown.html)

``` rust
pub enum Shutdown {
    Read,
    Write,
    Both,
}
```

===
# 複雑な列挙型
--------

* 列挙子は値を持てる
  + Javaでいう抽象クラス+継承相当？

from [std::net::IpAddr](https://doc.rust-lang.org/std/net/enum.IpAddr.html)

``` rust
pub enum IpAddr {
    V4(Ipv4Addr),
    V6(Ipv6Addr),
}
```

===
# Option
---------

* Rustにnullはない
  + `Option<T>` を使う

``` text
// <T> はジェネリクス
enum Option<T> {
    Some(T),
    None
}
```

===
# Optionの利用例
----------------

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=bf778a36c19e40b4227c66ae2c8a607a)

``` rust
impl UserDb {
    fn find(&self, id: &u64) -> Option<&User> {
        self.db.get(&id)
    }
}

match db.find(&1) {
    Some(user) => println!("User found"),
    None => println!("User not found"),
}
```


===
# Result
--------

* Rustに例外はない
  + `Result<T, E>` を使う

``` rust
enum Result<T, E> {
    Ok(T),
    Err(E)
}
```

===
# Resultの利用例
----------------

* 「例外が投げられる」という了解的ものだったのが第一級市民となることで扱いやすくなった

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=4e483d5a76a6ba0bf840200921cc07bc)

``` rust
let arg = env::args().nth(1);
let env = env::var("PARAM");
match (arg, env) {
    (Some(s), _) => println!("Param is given via arg: {}", s),
    (_, Ok(s)) => println!("Param is given via env: {}", s),
    _ => println!("No param given "),
}
```

===
# 所有権
--------

* 値には唯一の所有者がいる
* 変数に代入すると所有権が移る
  + 関数呼び出しでも

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=83f71af33ce956c6fb222432288eba29)

``` rust
let x = vec![1, 2, 3];
let y = x;  //          <-- 所有者が移った
// println!("{:?}", x); <-- エラー
```

===
# ライフタイム
-------------

* スコープの末尾で値は開放される
  + デストラクタも呼ばれる

``` rust
let x = vec![1, 2, 3];
{
  let y = x;
  println!("{:?}", y);
  // ここでyが開放される
}
// ここではVecは開放済み
```


===
# 借用
------

* ポインタがある
  + 所有権は移動しない

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=da39ae24c347798067851a6276153f1c)

``` rust
let x = vec![1, 2, 3];
{
  let y = &x; // <- 借用
  println!("{:?}", y);
  // ここでyが開放される
  // つまりxに返却される
}

// 返してもらったあとはまた使える
println!("{:?}", x);
```


===
# 借用とライフタイム
-------------------

* 借用は元の値のライフタイムを越えられない

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=bf4a82438c38677914f4a6fc47909e23)

``` rust
fn return_ref() -> &i32 {
  let x = 1;
  &x
  // <- xはここで終了
}
```

===
# staticライフタイム
--------------------

* 他の値に縛られないライフタイムもある
  + 借用じゃない値とか
  + 文字列リテラルとか
* そういうライフタイルに `'static` という名前がつく

``` rust
let s: &'static str = "literal value";
```

===
# 参照の可変と不変
------------------

* 参照には可変と不変がある
* 可変な参照は1つしか存在しない
* 不変な参照は複数存在できる
  + つまり値を共有できる
* 可変な参照と不変な参照は同時に存在できない
* 上記制約を **コンパイル時に** 解決する

===
# 可変性エラー
-------------

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=e92ea6a7c5d9dcfbb046338c3de5668e)

``` rust
fn main() {
    // ベクトルを用意する
    let mut vec = vec![1, 2, 3];
    // ベクトルの要素への参照を取り出す。
    // ベクトルをイミュータブルに参照する
    for i in &vec {
        // 既にベクトルはイミュータブルに参照されているので
        // ここでベクトルを変更しようとするとエラー
        vec.push(i * 2);
    }
}
```

===
# 所有権とRAII
----------------

* 所有権のおかげで自然にRAIIできる
  + Resource Acquisition Is Initialization
* リソースの開放はデストラクタに任せる
  + ライフタイムのおかげで勝手にデストラクタが走る
* `File` に `close` がない
  + 代わりにデストラクタを使う

===
# Fileとデストラクタ
--------------------

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=9785b579d020aca62392ac92b0da5e4c)

``` rust
let mut file = File::create("test.txt")?;
file.write_all(b"Hello")?;
// ライフタイムが終わると自動で閉じられる
```

===
# ランタイム
------------

* Rustにはランタイム（GC）がない
  + 所有権のおかげ
  + システムプログラミング言語として重要
* パフォーマンス以外にも移植性が良
  + 組み込み
  + Webブラウザ(WebAssembly)

===
# 所有権の共有
--------------

* 所有権を共有したいケースもある
  + ロックとか
* Rustはいくつか所有権を共有する手段を提供している
  + 参照カウントとか

===
# `Rc`
-----

* 参照カウント（Reference Count）
* 所有権を共有する代表的手段
* `clone` でカウントを1増やしてデストラクタで1減らす
* カウントが0になったらメモリ開放
* マルチスレッドでは **使えない**
  + カウントを増減させる動作がスレッドセーフでない

===
# `Rc` の例
-----

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d4127622f721f614f70f66fd9fbe6b7c)

``` rust
use std::rc::Rc;
let foo = Rc::new(vec![1.0, 2.0, 3.0]);
// The two syntaxes below are equivalent.
let a = foo.clone();
let b = Rc::clone(&foo);
// a and b both point to the same memory location as foo.
```


===

# その他Rustの機能など
----------------------

* トレイト
  + インタフェースのようなもの
* クロージャ
* unsafe
  + FFI

===
# クロージャ
------------

* 匿名関数を作れる
* 環境にある変数を保存できる

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d10ea3de7c89b00ed1addbdbe989e921)

``` rust
let mut x = 0;
|| {
  x +=1;
  x
}
```

===
# クロージャとライフタイム
------------

* 変数は借用するのでライフタイム制約を受ける
* moveを使うと変数の所有権を奪える

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=dba3806b12adeaa1a231c0c33f7ab888)

``` rust
fn counter() -> impl FnMut() -> u64 {
    let mut x = 0;
    || {
      x +=1;
      x
    }
}
```

``` rust
fn inc() -> impl FnMut() -> u64 {
    let mut x = 0;
    move || {
      x +=1;
      x
    }
}
```

===
# unsafe
--------

* Rustの制約を一時的に外すことができる
  + `unsafe {}` で囲む
* `unsafe` ブロックの中ではC並に安全でないことができる
  + NULL pointerとか色々

===
# unsafeなコード
---------------

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2015&gist=6a04ca87dc616a2dfece0aae00e9e981)


```rust
use std::ffi::c_void;
use std::ptr::null_mut;

extern "C" {
  // FFIの関数のプロトタイプ宣言
  // 参照とは別のマジのポインタ型
  fn free(p: *mut c_void);
}

fn main() {
  // unsafeで囲むとやりたい放題
  unsafe {
    // ヌルポが作れる！！
    let p: *mut i32 = null_mut::<i32>();
    // ヌルポに書き込める！！
    *p = 1;
    // freeできる！！
    free(p.cast());
    // use after freeできる！！
    println!("{}", *p);
  }
}
```


===
# FFI
-----

* Cの関数を呼び出せる
* CにAPIを提供できる

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=96f62fd8635f09e4ba1b9eee071c148d)

``` rust
use libc::{suseconds_t, time_t};
use std::mem;
use std::os::raw::c_int;
use std::ptr;

// #[repr(C)]をつけることでCと相互運用できる型になる。
// メモリ上の表現がC互換になるというだけで、それ以外は普通のRustの構造体として扱える。
// struct timeval {
//     time_t      tv_sec;     /* seconds */
//     suseconds_t tv_usec;    /* microseconds */
// };
#[repr(C)]
#[derive(Debug)]
struct Timeval {
    tv_sec: time_t,
    tv_usec: suseconds_t,
}

// struct timezone {
//     int tz_minuteswest;     /* minutes west of Greenwich */
//     int tz_dsttime;         /* type of DST correction */
// };
#[repr(C)]
#[derive(Debug)]
struct Timezone {
    tz_minuteswest: c_int,
    tz_dsttime: c_int,
}

extern "C" {
    // 上記で定義した型をFFIの型に使える。
    // int gettimeofday(struct timeval *tv, struct timezone *tz);
    fn gettimeofday(tv: *mut Timeval, tz: *mut Timezone) -> c_int;
}

fn main() {
    unsafe {
        // Cによって初期化するメモリは`mem::zeroed`で確保できる。
        // もちろん、Rustの構造体の初期化構文も使える。
        let mut tv: Timeval = mem::zeroed();
        // あるいはNULLを渡したい場合は`ptr::null_mut`も使える。
        let tz: *mut Timezone = ptr::null_mut();
        let ret = gettimeofday(&mut tv as *mut _, tz);
        if ret == -1 {
            println!("failure");
            return;
        }
        println!("{:?}", tv);
    }
}
```

===

# Fearless Concurrency

<!-- .slide: class="center" -->

===
# Fearless Concurrency
------------------

* Rustではバグの心配なく並行処理が書ける
* データ競合が起きない
  + 借用のおかげ
* データの管理者がわかりやすい
  + 所有権のおかげ
* ミスを防ぐAPI設計
* 便利ライブラリなど

===
# Goとの違い
------------

* Goの「便利なパーツが揃ってる」とは違う
* Rustにgoroutineはない
  + async/awaitみたいに近いものはある
* Goにバグを防ぐ仕組みはない
  + 便利な道具で起きにくくはある
* 善し悪しというより設計の違い
  + システムプログラミング言語vsアプリケーション言語

===
# Rustで並行処理
-----------------

* `spawn` 関数でスレッドを作れる
  + 中身はOSスレッド

``` rust
use std::thread;

let handler = thread::spawn(|| {
    // thread code
});

handler.join().unwrap();
```

===
# 並行処理とライフタイム
----------------------

* スレッドに渡すデータには `'static` が要求される

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=ab08caa85dc05937a53e862308f11a33)

``` rust
use std::thread;

let mut x = 0;
let handler = thread::spawn(|| {
    x += 1;
});

println!("{}", x);
handler.join().unwrap();
println!("{}", x);
```


===
# データ競合を防ぐ仕組み
----------------------------

* スレッド間で移動していいデータにはマークがついている
  + トレイトをマーカとして使う
* マーカがついていないデータを送ろうとするとコンパイルエラー
* `Rc` などのスレッドセーフでないデータ型にはマーカがついていない

===
# スレッドとRc
--------------

* `Rc` を別スレッドに渡そうとするとコンパイルエラー

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=99b14133ec329cac0511f3d8c6b71918)


``` rust
use std::thread;
use std::rc::Rc;

let x = Rc::new("data".to_string());
let handler = thread::spawn(move || {
    let x = x.clone();
    println!("{}", x);
});

println!("{}", x);
handler.join().unwrap();
println!("{}", x);
```


===
# スレッドとArc
--------------

* `Arc` （Atomic reference count）なら別スレッドに渡せる

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=5a09fcd00028b2c26b596371e65d26de)

``` rust
use std::thread;
use std::sync::Arc;

let x = Arc::new("data".to_string());
let y = x.clone();
let handler = thread::spawn(move || {
    println!("{}", y);
});

println!("{}", x);
handler.join().unwrap();
println!("{}", x);
```


===
# ロック
--------

* `Arc` だと値を変更できない
* 値を変更するにはアトミック型やロックが必要
* ロック: [`Mutex<T>`](https://doc.rust-lang.org/std/sync/struct.Mutex.html)
  + Mutexは守る値を明示するスタイル
* アトミック型: [`atmoic`](https://doc.rust-lang.org/std/sync/atomic/index.html)

===
# `Mutex`
---------

* `lock` したあとに値を変更できる
* `File` と同じく `unlock` はない
  + ライフタイムが終わったら自動でunlockされる

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=6fc46b52ba35c70eda4c1e791f6fc458)

``` rust
use std::thread;
use std::sync::{Mutex, Arc};

let x = Arc::new(Mutex::new("data".to_string()));
let y = x.clone();
let handler = thread::spawn(move || {
    let mut lock = y.lock().unwrap();
    *lock = "modified".to_string();
    println!("{}", lock);
});

handler.join().unwrap();
let lock = x.lock().unwrap();
println!("{}", x);
```

===
# デッドロック
--------------

* Rustでもデッドロックは防げない
* Rustが防げるのは同じメモリを読み書きして起きるエラー
  + = データ競合

===
# チャネル
----------

* スレッド間でデータを送る仕組み
  + メモリを共有するよりバグりづらいとされる
* Rustでは所有権ごと別のスレッドに送る

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=24c6f7aff9d4241be74fabe5c568b7d1)

``` rust
use std::thread;
use std::sync::mpsc::channel;

// Create a simple streaming channel
let (tx, rx) = channel();
thread::spawn(move|| {
    tx.send(10).unwrap();
});
assert_eq!(rx.recv().unwrap(), 10);
```

===
# チャネルとSend
----------------

* `spawn` と同じく `Send` でないデータは送れない

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=66d0da55e9ebbe77ab66c5f4570e28f4)

``` rust
use std::sync::mpsc::channel;
use std::thread;
use std::rc::Rc;

let (tx, rx) = channel();
thread::spawn(move || {
    let rc = Rc::new(1);
    tx.send(rc.clone()).unwrap();
});
assert_eq!(rx.recv().unwrap(), Rc::new(1));
```

===

# チャネル小話
--------------

* Goのチャネルとは違う
  + スケジューラが絡まなに
* Rustのチャネルはいくつか種類がある
  + bounded/unbounded
  + ブロックする/しない



===

# spawnの中身
---------------

* `spawn` は言語組込み **ではない**
    + C FFIで呼んでる
    ``` rust
    // UNIX系OSの場合
    pub struct Thread {
        id: libc::pthread_t,
    }
    ```
    + [spawnの中身](https://doc.rust-lang.org/src/std/sys/unix/thread.rs.html#43-91)
* 言語組み込みで安全なのではなく、安全に作る仕組みで安全に作ってる
  + → 他のFFIでもRustを通して使うと安全にすることができる

===
# コード例
----------

* IOスレッドを立ててチャネルで書き込むデータを送る

[Run](https://play.rust-lang.org/?version=stable&mode=release&edition=2018&gist=b7818e1251990858fe11eaccf2a7351c)

``` rust
use std::fmt::Display;
use std::io::{prelude::*, stdout};
use std::sync::mpsc::{channel, Receiver};
use std::thread::{self, spawn, JoinHandle};

fn start_writer<T: Display + Send + 'static>(rx: Receiver<T>) -> JoinHandle<()> {
    spawn(move || {
        let stdout = stdout();
        let mut out = stdout.lock();
        for data in rx {
            match writeln!(out, "{}", data) {
                Ok(()) => (),
                Err(e) => {
                    eprintln!("Error: {}", e)
                }
            }
        }
    })
}

fn main() -> thread::Result<()> {
    let (tx, rx) = channel();
    let handle = start_writer(rx);
    if let Err(e) = tx.send("hello") {
        eprintln!("Error while sending: {}", e)
    }
    {
        let tx = tx;
    }
    handle.join()?;
    Ok(())
}
```

===
# 並行性とライブラリ
--------------------

* 実際のところ自分でスレッドを作ることはそんなにない
  + もちろん、場面による
* 大抵はライブラリを使う
  + [rayon](https://crates.io/crates/rayon) など
* 生のスレッドよりも非同期プログラミングが主流
  + `async` / `await`

===
# Rayon
-------

* Rustの並行プログラミングライブラリ
* データ並列を気軽に書ける

[Run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=3e4e6f7ddb084666c55ef0e397f81b2f)

``` rust
use rayon::prelude::*;
fn sum_of_squares(input: &[i32]) -> i32 {
    input.par_iter() // <-- just change that!
         .map(|&i| i * i)
         .sum()
}
```

===
# コード例
----------

* CSVを読み込んで検索
* Rayonで並列化
  + （実際は検索処理が軽すぎて並列化しない方が速い）

``` rust
use rayon::prelude::*;
use regex::Regex;
use std::io::{prelude::*, stdout, BufWriter};
use std::sync::mpsc::{channel, Receiver};
use std::thread::{spawn, JoinHandle};
use std::{env, fmt::Display};

fn start_writer<T: Display + Send + 'static>(rx: Receiver<T>) -> JoinHandle<()> {
    spawn(move || {
        let stdout = stdout();
        let mut out = BufWriter::new(stdout.lock());
        for data in rx {
            match writeln!(out, "{}", data) {
                Ok(()) => (),
                Err(e) => {
                    eprintln!("Error: {}", e)
                }
            }
        }
    })
}

fn main() {
    let file = env::args().nth(1).expect("arg: FILE");

    let re = Regex::new("[rR]ust").expect("Regex syntax error");
    let (tx, rx) = channel();
    let handle = start_writer(rx);

    let rdr = csv::Reader::from_path(file).expect("failed to open input file");

    rdr.into_records()
        .par_bridge()
        .for_each_with(tx, move |tx, rcd| {
            let rcd = rcd.expect("failed to parse line");
            let text = &rcd[5];
            if re.is_match(text) {
                tx.send(text.to_string()).expect("failed to send data");
            }
        });

    handle.join().expect("thread join error");
}

```

===
# 非同期プログラミング
----------------------

* Rustには `async` / `await` 構文がある
  + 協調的マルチタスク
  + スレッドより細かい単位でタスクを管理できる
* 言語側は構文だけ提供して、ランタイムはライブラリが担当
  + 言語本体はランタイムレスのまま
  + 組み込みとかでも `async` / `await` が使える

===
# 同期の問題点
---------------------------

* 他にやりたいことがあってもスレッドが止まる

``` rust
fn sync_do() -> Result<(), io::Error> {
    write(b"Hello, ")?; // <- ここでスレッドが止まる
    write(b"World")?;   // <- ここでスレッドが止まる
    Ok(())
}
```

===
# `async` / `await`
--------------------

* いくつかの変更で同期処理を非同期処理に書き換えられる
* 裏ではものすごい変換が行なわれている

[Run](https://play.rust-lang.org/?version=stable&mode=release&edition=2018&gist=5c63fd079d135c42090ef60b7e856529)

``` rust
// async fn
async fn async_do() -> Result<(), io::Error> {
    // async専用関数
    async_write(b"Hello, ").await?; // <- await構文
    async_write(b"World").await?;
    Ok(())
}
```


===
# 非同期についてもっと
--------------------------

* 正直、巨大すぎるトピックで全部は説明できない
  + そもそも非同期プログラミングが難しい
  + Rust特有のライフタイムとかランタイムとかも絡む
* [Async Book](https://rust-lang.github.io/async-book/01_getting_started/01_chapter.html)などを参考に


===
# まとめ
--------

* Rustはあたらしいシステムプログラミング言語
* システムプログラミング言語ながら高級な機能もある
* Rustを使うとある種の並行性由来のバグをコンパイルエラーにできる
* Rustには並行性を上手く扱ってくれるライブラリが揃ってる

</textarea>

