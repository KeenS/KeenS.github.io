---
categories: ["Rust", "Future"]
date: 2019-07-07T12:26:32+09:00
title: "RustのFutureとそのRunnerを作ってみた"
---
κeenです。Rust 1.36.0がリリースされましたね。
ここで標準ライブラリにFutureが入ったので試しに実装してみます。

<!--more-->
Rust 1.36.0ではFutureとそれに関連したいくつかのアイテムが安定化されました。
これは今までcrates.ioにあったfuturesのAPIとは異なるもので、 1.38.0 で安定化される予定の `async` / `await` 導入の布石になるものです。futuresと標準ライブラリのFutureの関係や `async` / `await` についてはここでは詳しく触れないので別の記事を参照して下さい。

参考:

* [Rustの未来いわゆるFuture - OPTiM TECH BLOG](https://tech-blog.optim.co.jp/entry/2019/07/05/173000)
* [Rustのasync/awaitの特徴4つ - Qiita](https://qiita.com/qnighy/items/05c38f73ef4b9e487ced)

# 何もしないFutureの実装
ひとまず `Future` のAPIを確認するために何もしない、ただ値を返すだけのFutureを作ってみましょう。
`Future` と関連する型は以下のように定義されています。

``` rust
// std::future

pub trait Future {
    type Output;
    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output>;
}

pub enum Poll<T> {
    Ready(T),
    Pending,
}
```

`poll` でFutureが準備できているか確認し、できていれば `Ready` を、 できていなければ `Pending` を返す仕組みです。
準備ができていなかった際は `Context` を使ってごにょごにょするのですが、今回は待たせないので一旦無視しましょう。

ところで `self` が `Pin<&mut Self>` になっていますね。
`Pin` はデータを移動できないことを示すデータ型です。 `async` / `await` を導入した際に起こる面倒事を回避するためにこうなっています。また、 `Pin` で包まれていてもデータを移動できることを表わす `Unpin` と呼ばれる自動トレイトもあります。

`Pin` のメソッドはいくつかあるのですが、さしあたって必要になるAPIを紹介しておきます。

``` rust
// std::pin
pub struct Pin<P> { /* fields omitted */ }

impl<T> From<Box<T>> for Pin<Box<T>>
where
    T: ?Sized;

impl<P> Pin<P>
where
    P: Deref,
    <P as Deref>::Target: Unpin,
{
    pub fn new(pointer: P) -> Pin<P>
}

impl<P> Pin<P> where
where
    P: DerefMut,
{
    pub fn get_mut(self) -> &'a mut T
    where
        T: Unpin;
}

```

作るときは `new` 関連関数もあるのですが `Box` から `into` でも作れます。違いは `T` に `Unpin` を要求するかどうかです。


さて、必要なAPIを確認したのでただ値を返すだけのFutureを実装してみましょう。


``` rust
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

pub struct ReturnFuture<T>(Option<T>);

impl<T> ReturnFuture<T> {
    pub fn new(t: T) -> Self {
        Self(Some(t))
    }
}

impl<T> Future for ReturnFuture<T>
where
    T: Unpin,
{
    type Output = T;
    fn poll(self: Pin<&mut Self>, _: &mut Context) -> Poll<Self::Output> {
        Poll::Ready(
            self.get_mut()
                .0
                .take()
                .expect("A future should never be polled after it returns Ready"),
        )
    }
}

```

ほぼ値を保持して返すだけですが、 `Option` を使っています。これは `&mut Self` になっているので所有権をムーブできないからです。こういうときは `Option<T>` と `Option::take` でどうにかするイディオムが知られているのでそれを使います。

ここで、 `Pin<&mut Self>` から `&mut Self` を作るために `Pin::get_mut` を使っていて、さらにそのために `T` に `Unpin` を要求しています。分析してみると、 `take` で保持した値をムーブして返しているのでこれは必要な要求です。

ところで `Option::expect` を呼んでいるのが気になりますね。
しかしメッセージにも書いているように、 `Future` は `Poll::Ready` を返したらそれ以上は `poll` が呼ばれない規約になっています ([`poll`のドキュメント](https://doc.rust-lang.org/std/future/trait.Future.html#tymethod.poll)に書いてあります)。なのでここでは二度目が呼ばれたらパニックすることにしておきます。

試しにこのFutureを使ってみましょう。

``` rust
fn main() {
    let mut future = ReturnFuture::new(42);
    // futureを実行する
}
```

使おうと思いましたが、実行する手立てがありませんね。 `poll` を呼ぶにしても `Context` がありません。
仕方ないのでFutureのRunnerも作りましょう。

# 工夫のないRunner

ということでFutureを実行するために `Context` が必要なので `Context` のAPIを確認します。

``` rust
// std::task
pub struct Context<'a> { /* fields omitted */ }

impl<'a> Context<'a>
{
    pub fn from_waker(waker: &'a Waker) -> Context<'a>;
    pub fn waker(&self) -> &'a Waker;
}

```

なんと `Waker` を準備しないと `Context` が作れません。では `Waker` を。

``` rust
// std::task
pub struct Waker { /* fields omitted */ }
impl Waker
{
    pub fn wake(self);
    pub fn wake_by_ref(&self);
    pub fn will_wake(&self, other: &Waker) -> bool;
    pub unsafe fn from_raw(waker: RawWaker) -> Waker;
}
```

`Waker` はさらに `RawWaker` から作られます。では `RawWaker` を。

``` rust
// std::task
pub struct RawWaker { /* fields omitted */ }
impl RawWaker
{
    pub const fn new(data: *const (), vtable: &'static RawWakerVTable) -> RawWaker;
}
```


`RawWaker` さらに `RawWakerVTable` から作られます。では `RawWakerVTable` を。

``` rust
// std::task
pub struct RawWakerVTable { /* fields omitted */ }
impl RawWakerVTable
{
    pub const fn new(
        clone: unsafe fn(*const ()) -> RawWaker,
        wake: unsafe fn(*const ()),
        wake_by_ref: unsafe fn(*const ()),
        drop: unsafe fn(*const ())
    ) -> RawWakerVTable;
}
```

ここまできてようやく作れそうです。ですが `unsafe` や生ポインタが出てきて不穏ですね。
一旦ここまで出たデータ型を見返してみると、主な部分は `RawWaker` に渡す `*const ()` と `RawWakerVTable` の組が担います。`Waker` はそのラッパで `Context` は将来 `Waker` 以外の機能も提供するために一枚噛ませてるんですかね？

`*const ()` と `RawWakerVTable` ですが、vtableという名前を知っている人には何なのか一目で分かるでしょう。

vtableを知らない人に軽く説明します。Rustのメソッド呼び出しは何も特別なことをしない関数呼び出しです。
以下の3つの呼び出しは同じ挙動をします。

``` rust
// 1
data.clone()
// 2
Clone::clone(&data)
// 3
<Data as Clone>::clone(&data)
```

`<Data as Clone>::clone` はただの関数ですので、ざっくりいうと `data` へのポインタと `clone` 関数があれば `clone` メソッドを呼び出せるのです。普段はこの関数をコンパイラが適当なものを探索して呼出してくれているのです。
この 「`data` へのポインタ」 部分が `*const ()` で、 「`clone` 関数」が `RawWakerVTable` に渡している `clone` です。
データ部分は普通なら `&Data` のように参照を使いますが、 `Waker` はジェネリクスになっていないので型を消すためにポインタにして `()` にキャストしているのでしょう。
具体的な型を消しつついくつかのメソッドを提供するのはやっていることは `Box<dyn Trait>` とあまり変わらないのですが（というか `dyn Trait` も上記と同じような仕組みで動いている）、 `Box` 部分を抽象化するためなのか `Sized` 制約の扱いの問題からなのか、それを分解したものすごく低レベルなAPIになっていますね。

VTableに入っているメソドは、  `clone` と `drop` は `Waker` の `Clone` と `Drop` で使われ、`wake` と `wake_by_ref` はそのまま`Waker` の `wake` と `wake_by_ref` で使われるようです。

これで `Waker` の作り方が分かったので `Context` も作れて、 Runner も作れます。
`Waker` (`Context`) は実際は何もしないので先に Runner の方を作ります。
あまり褒められた実装ではないですが、`Ready` が返ってくるまでビジーループで `poll` し続けるRunner、 `SpinRunner` を実装します。


``` rust
pub struct SpinRunner;

use std::ops::{Deref, DerefMut};

impl SpinRunner {
    pub fn new() -> Self {
        Self
    }

    pub fn run<F>(&mut self, future: F) -> F::Output
    where
        F: Future,
    {
        let mut future = Pin::from(Box::new(future));
        self.run_pin(future.as_mut())
    }

    pub fn run_pin<F>(&mut self, mut future: Pin<F>) -> <<F as Deref>::Target as Future>::Output
    where
        F: DerefMut,
        <F as Deref>::Target: Future,
    {
        let waker = SpinWaker::waker();
        let mut cx = Context::from_waker(&waker);
        // loopでpollするだけ
        loop {
            match future.as_mut().poll(&mut cx) {
                Poll::Ready(ret) => {
                    return ret;
                }
                Poll::Pending => continue,
            }
        }
    }
}

```

ここでは`waker` から `cx` を作ってはいますがそのあと `Future` に渡すだけで何もしていませんね。
ということで何もしない `Waker` を作りましょう。

``` rust
use std::task::{RawWaker, RawWakerVTable, Waker};

#[derive(Debug, Clone)]
struct SpinWaker;

static SPIN_WAKER_VTABLE: RawWakerVTable = RawWakerVTable::new(
    SpinWaker::unsafe_clone,
    SpinWaker::unsafe_wake,
    SpinWaker::unsafe_wake_by_ref,
    SpinWaker::unsafe_drop,
);

impl SpinWaker {
    fn waker() -> Waker {
        // this is safe because waker's data and vtable is consistent
        unsafe { Waker::from_raw(Self::new().into_raw_waker()) }
    }

    fn new() -> Self {
        Self
    }

    unsafe fn into_raw_waker(self) -> RawWaker {
        let ptr = Box::into_raw(Box::new(self)) as *const ();
        RawWaker::new(ptr, &SPIN_WAKER_VTABLE)
    }

    unsafe fn unsafe_clone(this: *const ()) -> RawWaker {
        let ptr = this as *const Self;
        Box::new(ptr.as_ref().unwrap().clone()).into_raw_waker()
    }
    fn wake(self: Self) {}
    unsafe fn unsafe_wake(this: *const ()) {
        let ptr = this as *mut Self;
        Box::from_raw(ptr).wake()
    }

    fn wake_by_ref(&self) {
        Box::new(self.clone()).wake()
    }

    unsafe fn unsafe_wake_by_ref(this: *const ()) {
        let ptr = this as *const Self;
        ptr.as_ref().unwrap().wake_by_ref()
    }

    unsafe fn unsafe_drop(this: *const ()) {
        let ptr = this as *mut Self;
        Box::from_raw(ptr);
    }
}
```

`*const ()` の部分は `Box::into_raw` で作っています。
これは `unsafe_drop` の `Box::from_raw` で復元し、Rustに開放させています。
実は今回の `SpinWaker` はデータを持たないので `Box` を使わなくてもNULLポインタでも差し支えないのですが例示として `Box` のポインタを使っています。

`wake_by_ref` は `clone` を使う実装にしておきます。
`wake` は何もしないメソッドとして定義します。
`unsafe_*` 関数は主には `*const ()` から `Self` を復元する役目を負います。
全て同じ `*const ()` ですが、実際は呼出した側からすると `&self` のつもりだったり `Box<Self>` のつもりだったりするので適切に状況判断をして `Self` を復元しましょう。

これで役者が揃ったので `Future` を実行してみましょう。

``` rust
fn main() {
    let mut runner = SpinRunner::new();
    let future = ReturnFuture::new(42);
    let ret = runner.run(future);
    println!("answer is {}", ret);
}
```

``` console
answer is 42
```

結果を取り出せました。
しかしビジーループで実装されています。
今扱っているFutureはすぐに返りますが全てがそうではありません。
これでは結果が出てくるまでCPUを使いっきりですね。
それに、 `poll` を何度も呼ぶと結果の確認にコストのかかるFutureだったらFutureの計算自体も遅くなりかねません。

結果が出てくるまでは休むようにできないでしょうか。
これは `Waker` をちゃんと実装してあげるとできるようになります。
そのために、もう少しまともな Runnerを実装しましょう。

# 工夫のあるRunner
条件を満たすまで待つには、channelを使う方法もありますがここでは[条件変数](https://ja.wikipedia.org/wiki/%E3%83%A2%E3%83%8B%E3%82%BF_(%E5%90%8C%E6%9C%9F)#%E6%9D%A1%E4%BB%B6%E5%A4%89%E6%95%B0)を使ってみましょう。

条件変数（[`Condvar`](https://doc.rust-lang.org/std/sync/struct.Condvar.html)） はロックと組で使います。

待つ側は以下のように、ロックに対してそれが通知されるのを `Condvar::wait` で待ちます。

``` rust
let lock = mutex.lock().unwrap();
condvar.wait(lock).unwrap();
```

通知する側は適当に `Condvar::notify_all` などを呼びます。

``` rust
condvar.notify_all();
```

これを使って `Future` を `poll` して `Poll::Pending` なら `wait` する Runnerを書いてみましょう。

``` rust
use std::sync::{Arc, Mutex, Condvar};

struct CondRunner(Arc<(Mutex<()>, Condvar)>);
impl CondRunner {
    // 略

    pub fn run_pin<F>(&mut self, mut future: Pin<F>) -> <<F as Deref>::Target as Future>::Output
    where
        F: DerefMut,
        <F as Deref>::Target: Future,
    {
        let waker = CondWaker::waker(self.0.clone());
        let mut cx = Context::from_waker(&waker);
        loop {
            match future.as_mut().poll(&mut cx) {
                Poll::Ready(ret) => {
                    return ret;
                }
                Poll::Pending => {
                    // 一回pollしてまだだったら条件変数の通知を待つ
                    let lock = (self.0).0.lock().unwrap();
                    *(self.0).1.wait(lock).unwrap();
                    // 条件が通知されたら再度pollする
                }
            };
        }
    }
}

```


ビジーループに `Condvar::wait` が足されました。
`Mutex` は実際にはデータを使ってないので `()` を持たせています。
`wait` したあとに `poll` したら結果が返ってくることが保証されてもよさそうな気がしますが、Futureの計算が多段階に分かれる場合などは `wake` のあとにさらに計算があることもあるので結果が返ってくるとは限りません。

これに対応する `Waker` を作ります。
今度は `Runner` と `Waker` でデータを共有するので `Box` ではなく `Arc` を使います。

``` rust
#[derive(Debug)]
struct CondWaker(Arc<(Mutex<()>, Condvar)>);
```

先程は `Box<SpinWaker>` と外側でポインタを扱ってましたが今度は `CondWaker` の中にポインタ型があるので混乱しないで下さいね。

それでは `CondWaker` を実装していきます。

``` rust
static COND_WAKER_VTABLE: RawWakerVTable = RawWakerVTable::new(
    CondWaker::unsafe_clone,
    CondWaker::unsafe_wake,
    CondWaker::unsafe_wake_by_ref,
    CondWaker::unsafe_drop,
);

impl CondWaker {
    fn waker(inner: Arc<(Mutex<()>, Condvar)>) -> Waker {
        // this is safe because waker's data and vtable is consistent
        unsafe { Waker::from_raw(Box::new(Self::new(inner)).into_raw_waker()) }
    }

    fn new(inner: Arc<(Mutex<()>, Condvar)>) -> Self {
        Self(inner)
    }

    unsafe fn into_raw_waker(self) -> RawWaker {
        let ptr = Arc::into_raw(self.0) as *const ();
        RawWaker::new(ptr, &COND_WAKER_VTABLE)
    }

    unsafe fn unsafe_clone(this: *const ()) -> RawWaker {
        let ptr = this as *const (Mutex<()>, Condvar);
        let arc = Arc::from_raw(ptr);
        let ret = Self::new(arc.clone()).into_raw_waker();
        std::mem::forget(arc);
        ret
    }
    fn wake(self) {
        (self.0).1.notify_all()
    }
    unsafe fn unsafe_wake(this: *const ()) {
        let ptr = this as *const (Mutex<()>, Condvar);
        Self::new(Arc::from_raw(ptr)).wake()
    }

    unsafe fn unsafe_wake_by_ref(this: *const ()) {
        let ptr = this as *const (Mutex<()>, Condvar);
        let arc = Arc::from_raw(ptr);
        let ret = Self::new(arc.clone());
        std::mem::forget(arc);
        ret.wake()
    }
    unsafe fn unsafe_drop(this: *const ()) {
        let ptr = this as *const (Mutex<()>, Condvar);
        Self::new(Arc::from_raw(ptr));
    }
}
```

いくつか目立ったところを解説します。
まず、 `wake` で `notify_all` を呼んでいますね。
ポインタの操作は `Arc` にも `from_raw` と `into_raw` があるので `SpinRunner` のときとさほど変わりません。
唯一違うのは `unsafe_clone` です。 `SpinRunner` と違って `Mutex` などは `clone` できないので `Arc` の `clone` を呼びます。
そのために一旦ポインタから `Arc` を復元するのですがそのままにすると `drop` が走ってデクリメント、開放されてしまうので `forget` で `drop` が走らないようにしています。

さて、このRunnerを使ってみましょう。

``` rust
fn main() {
    let mut runner = CondRunner::new();
    let future = ReturnFuture::new(42);
    let ret = runner.run(future);
    println!("answer is {}", ret);
}
```


``` console
answer is 42
```

計算できました。
しかし思い出すと `ReturnFuture` は `Context` を呼び出さなかったので条件変数は使われていませんね。
これでは正しく実装できているか分かりません。
ちゃんと別スレッドで計算して `Context` も使うFutureを作りましょう。

# スレッドを使うFuture

`Waker` を使う例を見るためにスレッドを使うFutureを作ります。今回は結果のやりとりにchannelを使いましょう。
Wakerは計算スレッドと呼び出し側で共有するので `Arc<Mutex<_>>` を使います。
さらに `poll` されるまでは `Waker` は存在しないので `Option<_>` に包まれます。

``` rust
use std::sync::mpsc::{channel, Receiver};
use std::thread;

pub struct ThreadFuture<T> {
    rx: Receiver<T>,
    waker: Arc<Mutex<Option<Waker>>>,
}
```

このFutureの開始でスレッドをスタートします。

``` rust
impl<T> ThreadFuture<T>
where
    T: Send + 'static,
{
    pub fn start<F>(f: F) -> Self
    where
        F: FnOnce() -> T + Send + 'static,
    {
        let (tx, rx) = channel();
        let waker = Arc::new(Mutex::new(None::<Waker>));
        let w = waker.clone();
        thread::spawn(move || {
            tx.send(f()).unwrap();
            if let Some(waker) = &*w.lock().unwrap() {
                waker.wake_by_ref()
            }
        });

        Self { rx, waker }
    }
}

```

スレッドの中身は `f` の計算結果をチャネルに突っ込んだら `waker` を使って計算が終わったことを通知します。
`waker` がなかったときか気になるかもしれませんが、そのときはまだ `poll` が呼ばれていないので特に `wake` を呼ぶ必要はありません。

これのFuture側の実装はこうです。

``` rust
impl<T> Future for ThreadFuture<T>
where
    T: Send + Sync + 'static,
{
    type Output = T;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        *self.waker.lock().unwrap() = Some(cx.waker().clone());
        match self.rx.try_recv() {
            Ok(t) => Poll::Ready(t),
            Err(_) => Poll::Pending,
        }
    }
}
```

受け取った `Waker` をスレッドに共有してから `try_recv` を呼びます。
`Waker` は何度も `poll` が呼ばれた場合は最新の `Waker` を使って `wake` する規約なので呼ばれる度に上書きます。
`try_recv` はブロックせず、 `Ok` が返れば値が準備できていて、 `Err` なら値は（まだ）ありません。

因みにスレッドのチャネルに `send` して `waker` の確認、Futureの `waker` をセットしてから `try_recv` の順番は重要です。これが逆だと二度と `poll` されない場合が出てきます。具体的には以下のようなトレースです。

futureがpollしてからwakerをsetする場合:

1. [Future]: pollされたので `try_recv` する。まだ値がない
2. [スレッド]: 計算が終わったので値を `send` する
3. [スレッド]: `waker` を取り出そうとするが、まだない。これでスレッドは終了する
4. [Future]: `waker` をセットし、 `Pending` を返す。`wake` が呼ばれるまで `poll` は呼ばれない

スレッドがwakeしてから計算をsendする場合:

1. [スレッド]: 計算が終わったので `waker` を取り出そうとするがまだない。スルーする
2. [Future]: pollされたので `waker` をセットする
3. [Future]: `try_recv` する。まだ値はないので `Pending` を返す。`wake` が呼ばれるまで `poll` は呼ばれない
4. [スレッド]: チャネルに値を `send` する。これでスレッドは終了する

どちらの場合もタイミングによってはFutureが `Pending` を返したあとに `wake` が呼ばれないままスレッドが終了しており、このまま `poll` が呼ばれないようなトレースが存在します。注意しましょう。

さて、これを使ってみましょう。適当なワークロードとして `fib` 関数を置いておきます。

``` rust
fn fib(n: u64) -> u64 {
    if n < 2 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

```

それでは走らせてみます。

``` rust
fn main() {
    let mut runner = CondRunner::new();
    let future = ThreadFuture::start(|| fib(42));
    let ret = runner.run(future);
    println!("answer is {}", ret);
}
```

``` console
answer is 433494437
```

ひとまず走ることは確認しました。でも、ちゃんとwaitできてるか不安ですよね。
Runnerをすこし弄って`poll` した回数を測ってみましょう。
2つのRunnerのループの部分でカウンタを設け、 `poll` で `Poll::Ready` が返ったときにカウンタを表示してみましょう。

``` rust
fn run_pin(...)
   // ...
   let mut i = 0;
   loop {
       i += 1;
       match future.as_mut().poll(&mut cx) {
           Poll::Ready(ret) => {
               println!("{}", i);
               return ret;
           },
           // ...
       }
       // ....
    }
}
```

2つとも書き換えたら走らせてみます。

``` rust
fn main() {
    let mut runner = CondRunner::new();
    let future = ThreadFuture::start(|| fib(42));
    println!("cond runner");
    let ret = runner.run(future);
    println!("answer is {}", ret);

    let mut runner = SpinRunner::new();
    let future = ThreadFuture::start(|| fib(42));
    println!("spin runner");
    let ret = runner.run(future);
    println!("answer is {}", ret);
}
```

``` console
cond runner
2
answer is 433494437
spin runner
49128182
answer is 433494437
```

はい、 `CondRunner` は2回しか `poll` してませんが `SpinRunner` は 49,128,182 回 `poll` したようです。ちゃんと `wait` できてますね。めでたしめでたし。


# まとめ

単純なFutureとスレッドを使うFuture、単純なRunnerと同期プリミティブを使ったRunnerを実装してみました。
その過程で、関連するデータ型の使い方も示しました。

今回作ったFutureとRunnerはどちらも単純なものでしたので、実際はもう少し複雑なものになるでしょう。
Futureはスレッドを使うにしてもスレッドプールを用意した方が効率的でしょうし、IO待ちの場合は `select` や `poll` 、 `epoll` 、 `kqueue` などのシステムコールを使うケースが多いでしょう。
Runnerも今回は1度に1 Futureだけ扱いましたが複数のFutureをまとめて処理できた方がよさそうです。
そうするとFutureのpollのスケジューラを実装することになるでしょう。

今回は複数のFutureを組み合わせるFutureは作りませんでした。
2つともの結果を待ってそのタプルを返す `join(f1, f2)` やどちらか早い方の結果を返す`choice(f1, f2)` 、 1つの結果を受けてさらにFutureの計算をする `and_then(f1,|ret| f2(ret))` などを実装してみると理解が深まるかもしれません。

# 付録
ソースコード全文

<script src="https://gitlab.com/snippets/1872760.js"></script>

