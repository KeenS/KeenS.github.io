---
categories: [Rust, 非同期]
date: 2022-11-27T18:53:38+09:00
description: TechFeed Experts Night#9 〜 Rust/WebAssemblyの「いま」を探るでの発表用。tokioから出ている非同期プログラムのデバッグツール(ライブラリ)群の紹介
title: "Rustの非同期デバッグツールを使いこなせ!"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Rustの非同期デバッグツールを使いこなせ!
[TechFeed Experts Night #9](https://techfeed.io/events/techfeed-experts-night-9)

<!-- .slide: class="center" -->

===
# About Me

![κeenのアイコン](/images/kappa2_vest.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

* κeen
* [@blackenedgold](https://twitter.com/blackenedgold)
* GitHub: [KeenS](https://github.com/KeenS)
* GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
* [Idein Inc.](https://idein.jp/)のエンジニア
* Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# Futureとタスク

* Futureはasync/awaitで繋げて1つの実行系列になる
  + (多くの場合)最終的には `main` まで到達
  + `main` から辿れる `async fn` の並びが1つの実行系列

```rust
async fn foo() {}
async fn bar() {
  foo().await
}

#[tokio::main]
async fn main() -> Result<()> {
    bar().await;
    bar().await;

    Ok(())
}
```

===
# Futureとタスク

* spawnとか使うと新しい実行系列を作れる
  + Webサーバを書くときはFW側がやってることが多い
* `join` とかでも擬似的に実行系列が増える
* この実行系列を **タスク** と呼ぶことにする

```rust
async fn foo() {}
async fn bar() {
  foo().await
}
fn main() -> Result<()> {
    // spawnを呼ぶ
    tokio::spawn(bar());
    tokio::spawn(bar());
    Ok(())
}
```

===
# 非同期のデバッグ

* 基本、つらい
  + どのタスクがどこまで進んだのか分からない
  + 同時に走ってるタスクが把握できない
  + 休止するのが普通なのでデッドロックしてるのか単に開始してないのか判断が難しい
  + スタックトレースが壊滅する

===
# スタックトレース

* 非同期では役に立たない
* ユーザの書いた関数がトレースに現われない
  + 関数から抜けてFutureを返してる
* Futureの実行時エラー=非同期ランタイムの動作時のスタック

===

```
  sentry_backtrace    0x55da458e076b sentry_backtrace::current_stacktrace (lib.rs:26)
  sentry_backtrace    0x55da458df623 sentry_backtrace::integration::current_thread (integration.rs:105)
  sentry_backtrace    0x55da458df40e sentry_backtrace::integration::AttachStacktraceIntegration::process_event (integration.rs:77)
  sentry_core         0x55da458eb321 sentry_core::client::Client::prepare_event (client.rs:189)
  sentry_core         0x55da458eb321 sentry_core::client::Client::capture_event (client.rs:263)
  sentry_core         0x55da45904425 sentry_core::hub::Hub::capture_event::{{closure}} (hub.rs:284)
  sentry_core         0x55da45904425 sentry_core::hub::HubImpl::with (hub.rs:45)
  sentry_core         0x55da45904425 sentry_core::hub::Hub::capture_event (hub.rs:281)
  actix_reporter      0x55da45866f42 actix_reporter::post_sentry (lib.rs:299)
  actix_reporter      0x55da457ec2f6 actix_reporter::ErrorLoggerMiddleware<T>::call::{{closure}}::{{closure}} (lib.rs:114)
  core                0x55da457ec2f6 core::future::from_generator::GenFuture<T>::poll (mod.rs:91)
  futures_util        0x55da45827eaa futures_util::future::future::map::Map<T>::poll (map.rs:55)
  futures_util        0x55da458104bb futures_util::future::future::Map<T>::poll (lib.rs:91)
  tokio               0x55da458104bb tokio::runtime::task::core::CoreStage<T>::poll::{{closure}} (core.rs:184)
  tokio               0x55da458104bb tokio::loom::std::unsafe_cell::UnsafeCell<T>::with_mut (unsafe_cell.rs:14)
  tokio               0x55da458104bb tokio::runtime::task::core::CoreStage<T>::poll (core.rs:174)
  tokio               0x55da458104bb tokio::runtime::task::harness::poll_future::{{closure}} (harness.rs:480)
  core                0x55da458104bb core::panic::unwind_safe::AssertUnwindSafe<T>::call_once (unwind_safe.rs:271)
  std                 0x55da458104bb std::panicking::try::do_call (panicking.rs:492)
  std                 0x55da458104bb std::panicking::try (panicking.rs:456)
  std                 0x55da458104bb std::panic::catch_unwind (panic.rs:137)
  tokio               0x55da458104bb tokio::runtime::task::harness::poll_future (harness.rs:468)
  tokio               0x55da458104bb tokio::runtime::task::harness::Harness<T>::poll_inner (harness.rs:104)
  tokio               0x55da458104bb tokio::runtime::task::harness::Harness<T>::poll (harness.rs:57)
  tokio               0x55da46e4903a tokio::runtime::task::raw::RawTask::poll (raw.rs:134)
  tokio               0x55da46e4903a tokio::runtime::task::LocalNotified<T>::run (mod.rs:385)
  tokio               0x55da46e4903a tokio::task::local::LocalSet::tick::{{closure}} (local.rs:578)
  tokio               0x55da46e4903a tokio::coop::with_budget::{{closure}} (coop.rs:102)
  std                 0x55da46e4903a std::thread::local::LocalKey<T>::try_with (local.rs:445)
  std                 0x55da46e4903a std::thread::local::LocalKey<T>::with (local.rs:421)
  tokio               0x55da46e4903a tokio::coop::with_budget (coop.rs:95)
  tokio               0x55da46e4903a tokio::coop::budget (coop.rs:72)
  tokio               0x55da46e4903a tokio::task::local::LocalSet::tick (local.rs:578)
  tokio               0x55da466f758f tokio::task::local::RunUntil<T>::poll::{{closure}} (local.rs:814)
  tokio               0x55da466f758f tokio::task::local::LocalSet::with::{{closure}} (local.rs:633)
  std                 0x55da466f758f std::thread::local::LocalKey<T>::try_with (local.rs:445)
  std                 0x55da466f758f std::thread::local::LocalKey<T>::with (local.rs:421)
  tokio               0x55da466ee0f8 tokio::task::local::LocalSet::with (local.rs:616)
  tokio               0x55da466ee0f8 tokio::task::local::RunUntil<T>::poll (local.rs:800)
  tokio               0x55da466ee0f8 tokio::task::local::LocalSet::run_until::{{closure}} (local.rs:536)
  core                0x55da466ee0f8 core::future::from_generator::GenFuture<T>::poll (mod.rs:91)
  core                0x55da466f7cd0 core::pin::Pin<T>::poll (future.rs:124)
  tokio               0x55da466f7cd0 tokio::runtime::scheduler::current_thread::CoreGuard::block_on::{{closure}}::{{closure}}::{{closure}} (current_thread.rs:525)
  tokio               0x55da466f7cd0 tokio::coop::with_budget::{{closure}} (coop.rs:102)
  std                 0x55da466f7cd0 std::thread::local::LocalKey<T>::try_with (local.rs:445)
  std                 0x55da466f7cd0 std::thread::local::LocalKey<T>::with (local.rs:421)
  tokio               0x55da466ea0d1 tokio::coop::with_budget (coop.rs:95)
  tokio               0x55da466ea0d1 tokio::coop::budget (coop.rs:72)
  tokio               0x55da466ea0d1 tokio::runtime::scheduler::current_thread::CoreGuard::block_on::{{closure}}::{{closure}} (current_thread.rs:525)
  tokio               0x55da466ea0d1 tokio::runtime::scheduler::current_thread::Context::enter (current_thread.rs:349)
  tokio               0x55da46708d54 tokio::runtime::scheduler::current_thread::CoreGuard::block_on::{{closure}} (current_thread.rs:524)
  tokio               0x55da46708d54 tokio::runtime::scheduler::current_thread::CoreGuard::enter::{{closure}} (current_thread.rs:595)
  tokio               0x55da46708d54 tokio::macros::scoped_tls::ScopedKey<T>::set (scoped_tls.rs:61)
  tokio               0x55da466e9e04 tokio::runtime::scheduler::current_thread::CoreGuard::enter (current_thread.rs:595)
  tokio               0x55da466e9e04 tokio::runtime::scheduler::current_thread::CoreGuard::block_on (current_thread.rs:515)
  tokio               0x55da466e9e04 tokio::runtime::scheduler::current_thread::CurrentThread::block_on (current_thread.rs:161)
  tokio               0x55da466fdb9c tokio::runtime::Runtime::block_on (mod.rs:490)
  tokio               0x55da466e46e4 tokio::task::local::LocalSet::block_on (local.rs:497)
  actix_rt            0x55da466e46e4 actix_rt::runtime::Runtime::block_on (runtime.rs:80)
  actix_rt            0x55da466e46e4 actix_rt::arbiter::Arbiter::with_tokio_rt::{{closure}} (arbiter.rs:144)
  std                 0x55da466e46e4 std::sys_common::backtrace::__rust_begin_short_backtrace (backtrace.rs:122)
  std                 0x55da46716d43 std::thread::Builder::spawn_unchecked_::{{closure}}::{{closure}} (mod.rs:505)
  core                0x55da46716d43 core::panic::unwind_safe::AssertUnwindSafe<T>::call_once (unwind_safe.rs:271)
  std                 0x55da46716d43 std::panicking::try::do_call (panicking.rs:492)
  std                 0x55da46716d43 std::panicking::try (panicking.rs:456)
  std                 0x55da46716d43 std::panic::catch_unwind (panic.rs:137)
  std                 0x55da46716d43 std::thread::Builder::spawn_unchecked_::{{closure}} (mod.rs:504)
  core                0x55da46716d43 core::ops::function::FnOnce::call_once{{vtable.shim}} (function.rs:248)
  alloc               0x55da46ebb8d3 alloc::boxed::Box<T>::call_once (boxed.rs:1951)
  alloc               0x55da46ebb8d3 alloc::boxed::Box<T>::call_once (boxed.rs:1951)
  std                 0x55da46ebb8d3 std::sys::unix::thread::Thread::new::thread_start (thread.rs:108)
  0x7fafa44bd609 start_thread
  0x7fafa428f103 clone
  0x000000000000 <unknown>
```

===
# 今日の話

ツールやライブラリを使って  
非同期コードのデバッグをしよう

<!-- .slide: class="center" -->

===
# ログ
## 問題意識

* ログを見るのに前後関係って大事だよね
* でも非同期タスクだと実行が入り乱れるから前後関係が破壊される
* →タスク単位で見れると嬉しいね

===

```
DEBUG server: accepted connection from 106.42.126.8:56975
DEBUG server: closing connection
DEBUG server::http: received request
DEBUG server: accepted connection from 11.103.8.9:49123
DEBUG server::http: received request
DEBUG server: accepted connection from 102.12.37.105:51342
 WARN server::http: invalid request headers
TRACE server: closing connection
```

===
# ログ

* [tracing](https://crates.io/crates/tracing)
* 非同期関係なく使える
* ログにどの文脈で実行されてるかのメタデータを加える
  + 関数がどのタスク内で実行されるか分からないからログを出す箇所単体だと情報が足りない
  + → 属性マクロで情報付加

===
# tracing

ログ出す側

```rust
use tracing::{info, instrument};

#[instrument]
async fn subtask(number: usize) -> usize {
    info!("polling subtask...");
    number
}

```

===
# tracing

初期化([`tracing_subscriber`](https://crates.io/crates/tracing-subscriber)も使う)

```rust
fn main() {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
        .try_init()?;
    // ...
}
```

===
# tracing

[デモ](https://github.com/tokio-rs/tracing/blob/master/examples/examples/spawny-thing.rs)

```
Nov 28 22:26:03.578  INFO parent_task{subtasks=10}: spawny_thing: spawning subtasks...
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=1
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=2
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=3
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=4
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=5
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=6
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=7
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=8
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=9
Nov 28 22:26:03.579 DEBUG parent_task{subtasks=10}: spawny_thing: creating subtask; number=10
Nov 28 22:26:03.579  INFO parent_task{subtasks=10}:subtask{number=1}: spawny_thing: polling subtask...
Nov 28 22:26:03.579  INFO parent_task{subtasks=10}:subtask{number=2}: spawny_thing: polling subtask...
Nov 28 22:26:03.579  INFO parent_task{subtasks=10}:subtask{number=3}: spawny_thing: polling subtask...
Nov 28 22:26:03.579  INFO parent_task{subtasks=10}:subtask{number=4}: spawny_thing: polling subtask...
Nov 28 22:26:03.579  INFO parent_task{subtasks=10}:subtask{number=5}: spawny_thing: polling subtask...
Nov 28 22:26:03.579  INFO parent_task{subtasks=10}:subtask{number=6}: spawny_thing: polling subtask...
Nov 28 22:26:03.579  INFO parent_task{subtasks=10}:subtask{number=7}: spawny_thing: polling subtask...
Nov 28 22:26:03.579  INFO parent_task{subtasks=10}:subtask{number=8}: spawny_thing: polling subtask...
Nov 28 22:26:03.579  INFO parent_task{subtasks=10}:subtask{number=9}: spawny_thing: polling subtask...
Nov 28 22:26:03.580  INFO parent_task{subtasks=10}:subtask{number=10}: spawny_thing: polling subtask...
Nov 28 22:26:03.580 DEBUG parent_task{subtasks=10}: spawny_thing: all subtasks completed
Nov 28 22:26:03.580  INFO parent_task{subtasks=10}: spawny_thing: sum=55
```

===
# スタックトレース
## 問題意識

* 非同期だとスタックトレースが壊滅する
* 止まってる(スケジューリングされてない)タスクの情報がない
  + 止まってるのはバグの可能性がある
  + 止まってるのでログとかも出ない

===
# async-backtrace

* [async-backtrace](https://crates.io/crates/async-backtrace)
* プログラム内からスタックトレース(？)のようなものを出せる
  + 全ての実行中のタスクとその親子関係

===
# async-backtrace
コード側
`#[async_backtrace::framed]` をつけるだけ

```rust
#[tokio::main]
async fn main() {
    joining().await;
}

#[async_backtrace::framed]
async fn joining() {
    let (_, _) = tokio::join!(yielding(), ready());
}
```

===
# async-backtrace
ダンプ
`async_backtrace::taskdump_tree` 呼ぶだけ

```rust
#[async_backtrace::framed]
async fn ready() {
    println!("{}", async_backtrace::taskdump_tree(true));
}
```

===
# async-backtrace

[デモ](https://github.com/tokio-rs/async-backtrace/blob/main/backtrace/examples/join.rs)

```
╼ join::joining::{{closure}} at backtrace/examples/join.rs:8:1
  ├╼ join::ready::{{closure}} at backtrace/examples/join.rs:18:1
  └╼ join::yielding::{{closure}} at backtrace/examples/join.rs:13:
```

===
# top
## 問題意識

* どのタスクが時間を食ってるのか分からない
  + スレッドだったら `top` とかで観察できる
* タスクがどの状態なのか活きた情報が得られない

===
# tokio-console

* [tokio-console](https://crates.io/crates/tokio-console)
* 非同期タスク向けの `top` のようなもの
* tokioプロジェクトでのみ使える
  + tokioの中でもやや実験的
* 2つのものを指す
 + アプリケーションから情報を出すプロトコル
 + アプリケーションの情報を拾って表示するツール

===
# tokio-console

1. コード内で初期化する

    ```rust
    console_subscriber::init();
    ```
2. `tokio_unstable` フラグをつけてビルドする

    ```
    RUSTFLAGS="--cfg tokio_unstable" cargo build
    ```

[デモ](https://github.com/tokio-rs/console/blob/main/console-subscriber/examples/app.rs)

===
# まとめ

* 非同期のデバッグは難しいよ
* 色々なツールを使うと様々な角度から情報が得られるよ
  + tracing: 文脈付与したログ
  + async-backtrace: タスクの親子関係
  + tokio-console: 生きたタスク情報


</textarea>
