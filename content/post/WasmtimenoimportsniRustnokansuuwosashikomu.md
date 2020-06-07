---
categories: [Rust, WebAssembly]
date: 2020-06-07T03:08:39+09:00
title: "WasmtimeのimportsにRustの関数を差し込む"
---
κeenです。WebAssembly/WASIのランタイム[Wasmtime](https://wasmtime.dev/)をちょっと触ってみたのでそのときのメモを残します。

<!--more-->

私の手元にブラウザ上で動作するWebAssemblyがあるのですが、その動作のためにJavaScriptの関数をimportsで渡していました。 `console.log` とか。
このwasmを毎回ブラウザで動作確認してると手間なので、ローカルで実行することを試みます。そこで `console.log` 相当の関数をローカルでも用意する必要がでてきました、というお話。

WebAssemblyのランタイムはいくつかあるのですが、今回はRustのライブラリとして使える [`wasmtime`](https://github.com/bytecodealliance/wasmtime)を使って、カスタマイズした処理系を用意します。

プロジェクトを用意しましょう。

```console
$ cargo new wasmtime_imports
$ cd wasmtime_imports
$ cargo add wasmtime@0.17.0
```

まずはシンプルな、Rustの関数を渡さないモジュールを作ってみましょう。

``` rust
use wasmtime::{Instance, Module, Store};

fn main() {
    let store = Store::default();
    let module = Module::new(
        store.engine(),
        r#"
(module
  (func $add (param i32 i32) (result i32)
    (return (i32.add (get_local 0) (get_local 1))))
  (func $main
    (drop (call $add (i32.const 1) (i32.const 2))))
  (start $main)
)"#,
    )
    .expect("failed to create module");
    let _ = Instance::new(&store, &module, &[]).expect("failed to instantiate");
}
```

これを実行してみると、何も起きずに終了します。

``` console
$ cargo run
```

これだと何が起きてるか分かりませんね。結果をコンソールに表示しましょう。
とはいってもWebAssemblyにはコンソールを扱う機能がありません。
コンソールに表示する関数をRustから渡してみましょう。

モジュールに `imports` を渡す機能を担うのが [`Linker`](https://docs.rs/wasmtime/0.17.0/wasmtime/struct.Linker.html)です。
そして `Linker::func` でRustの関数をリンカに登録できます。やってみましょう。

``` rust
use wasmtime::{Linker, Module, Store};

fn main() {
    let store = Store::default();
    // linkerを準備し、
    let mut linker = Linker::new(&store);
    // 関数を登録する
    linker
        .func("ffi", "print", |x: i32| println!("{}", x))
        .expect("function registration failed");
    let module = Module::new(
        store.engine(),
        // "ffi"モジュールの"print"をimportして使う
        r#"
(module
  (func $print (import "ffi" "print") (param i32))
  (func $add (param i32 i32) (result i32)
    (return (i32.add (get_local 0) (get_local 1))))
  (func $main
    (call $print (call $add (i32.const 1) (i32.const 2))))
  (start $main)
)"#,
    )
    .expect("failed to create module");
    // linkerを使ってインスタンス化する
    let _ = linker
        .instantiate(&module)
        .expect("failed to instantiate module");
}
```

Linkerを使ってモジュールにRustの関数を渡し、モジュール内でimport funcとして使っています。
これを実行してみましょう。

```console
$ cargo run
3
```

ちゃんと3が印字されました。

ということでWasmtimeのランタイムにRustの関数を渡す方法を紹介しました。
今回のコードは[こちら](https://gitlab.com/blackenedgold/wasmtime_imports)に置いておきます。

