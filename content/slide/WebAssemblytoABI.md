---
categories: ["WebAssembly", "Compiler"]
date: 2018-11-10T00:49:26+09:00
description:
title: "WebAssemblyとABI"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# WebAssemblyとABI
----------------------
[コンパイラ勉強会 - connpass](https://connpass.com/event/103976/)


<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# WebAssmblyとは
----------------

* Webブラウザで動く<!-- .element: class="fragment" data-fragment-index="1" -->
* アセンブリ<!-- .element: class="fragment" data-fragment-index="2" -->
* ではない<!-- .element: class="fragment" data-fragment-index="3" -->

===
# WASMの動作環境
------------------------

* ブラウザ
* NodeJS
* [CommonWA](https://github.com/CommonWA/cwa-spec)
* [losfair/IceCore](https://github.com/losfair/IceCore) : Build efficient and reliable backend applications in WebAssembly.
* [Introducing Wasmjit: A kernel mode WebAssembly runtime for Linux | Packt Hub](https://hub.packtpub.com/introducing-wasmjit-a-kernel-mode-webassembly-runtime-for-linux/)
* [piranna/wasmachine: Put WebAssembly in your washing machine](https://github.com/piranna/wasmachine)

===

# 言語としてのWASM
-----------------

* アセンブラっぽい雰囲気
* スタックマシン
* 無限のレジスタ(変数)とスタックがある
* コードの単位は関数
* 上位にモジュールがある
* モジュール毎に1仮想マシン(インスタンス)

===

# コード例1
-----------

```webassembly
(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    get_local $lhs
    get_local $rhs
    i32.add)
  (export "add" (func $add)))
```

===

# コード例2
-----------

``` webassembly
(func $sum1 (param $n i32) (result i32) (local $sum i32) (local $i i32)
  block $break
    loop $loop
      get_local $i
      get_local $n
      i32.ge_s
      br_if $break
      get_local $sum
      get_local $i
      i32.add
      set_local $sum
      get_local $i
      i32.const 1
      i32.add
      set_local $i
      br $loop
    end
  end
  get_local $sum
  return)
```

===
# コード例3
------------

``` webassembly
(func $sum2 (param $n i32) (result i32) (local $sum i32) (local $i i32)
  (block $break
    (loop $loop
      (br_if $break (i32.ge_s (get_local $i) (get_local $n)))
      (set_local $sum (i32.add (get_local $sum) (get_local $i)))
      (set_local $i (i32.add (get_local $i) (i32.const 1)))
      (br $loop)))
  (return (get_local $sum)))
```

===

# WASM詳細
-----------

* 型は4つ (`i32`, `i64`, `f32`, `f64`)
* モジュールに色々ついてくる
  * メモリ
  * 関数import/export
  * グローバル変数
* インスタンスの情報はJS側から色々アクセスできる

===
# import/export
----------------

``` webassembly
(module
  (type $alert_type (func (param i32)))
  (import "module" "alert" (func $alert (type $alert_type)))
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    get_local $lhs
    get_local $rhs
    i32.add)
  (func $run
    (call $alert (call $add (i32.const 1) (i32.const 2))))
  (export "run" (func $run)))
```

===
# import/export
----------------

``` webassembly
fetch('../out/main.wasm').then(response =>
  response.arrayBuffer()
).then(bytes =>
   WebAssembly.instantiate(bytes, {"module": {"alert": alert}})
).then(results => {
  instance = results.instance;
  instance.exports.run();
}).catch(console.error);
```

===
# メモリのexport
----------------

``` webassembly
(module
  (memory $mem 10)
  (export "memory" (memory $mem)))
```

``` javascript
// メモリを直接いじれる
instance.exports.memory.buffer

```

===
# 本題
------

* この仕様でJS-WASM間のABIどうしたらいい？
  + 型は4種類の即値のみ
  + メモリは触れる
===

# 答え
------

* Rustが既に実装している
* [Rust and WebAssembly](https://github.com/rustwasm)
* 以後追っていく

===
# JS -> Rust
-------------

* Rustでメモリを確保してJSが渡す

===
# JS -> Rust
-------------


``` webassembly
#[wasm_bindgen]
fn greet(name: &str) {
  alert(&format!("hello, {}!", name))
}
```

===
# JS -> Rust
-------------


``` webassembly
function passStringToWasm(arg) {
    const buf = cachedTextEncoder.encode(arg);
    const ptr = wasm.__wbindgen_malloc(buf.length);
    getUint8Memory().set(buf, ptr);
    return [ptr, buf.length];
}
```

===
# JS -> Rust
-------------

``` webassembly
export function greet(arg0) {
    const [ptr0, len0] = passStringToWasm(arg0);
    try {
        return wasm.greet(ptr0, len0);

    } finally {
        wasm.__wbindgen_free(ptr0, len0 * 1);

    }

}
```

===
# Rust -> JS
------------

* 可能ならポインタのまま渡す
* 不可能なら指定されたメモリに書き込む

===

# Rust -> JS
------------
ポインタ

``` rust
#[wasm_bindgen]
impl Universe {
    pub fn new() -> Universe {
        // ...
    }
}
```

===
# Rust -> JS
------------
ポインタ

``` javascript
export class Universe {
    static __wrap(ptr) {
        const obj = Object.create(Universe.prototype);
        obj.ptr = ptr;

        return obj;
    }

    free() {
        const ptr = this.ptr;
        this.ptr = 0;
        freeUniverse(ptr);
    }

    /**
    * @returns {Universe}
    */
    static new() {
        return Universe.__wrap(wasm.universe_new());
    }
}
```

===
# Rust ->  JS
--------
メモリ

``` rust
#[wasm_bindgen]
impl Universe {
    pub fn render(&self) -> String {
        self.to_string()
    }
}

```

===
# Rust -> JS
--------
メモリ

``` javascript

export class Universe {
    render() {
        const retptr = globalArgumentPtr();
        wasm.universe_render(retptr, this.ptr);
        const mem = getUint32Memory();
        const rustptr = mem[retptr / 4];
        const rustlen = mem[retptr / 4 + 1];

        const realRet = getStringFromWasm(rustptr, rustlen).slice();
        wasm.__wbindgen_free(rustptr, rustlen * 1);
        return realRet;

    }
}
```
===
# メモリ
------------
メモリ

``` javascript
let cachedGlobalArgumentPtr = null;
function globalArgumentPtr() {
    if (cachedGlobalArgumentPtr === null) {
        cachedGlobalArgumentPtr = wasm.__wbindgen_global_argument_ptr();
    }
    return cachedGlobalArgumentPtr;
}
```

===
# Rust -> JS
--------
メモリ


``` text
JS     retprt
            |      rustptr
----        |      |            データ
            v      v            v
WASM  [...|   |...|ptr|size|...|   |...]
           |       ^ |          ^
           +-------+ +----------+
```

===
# まとめ
---------

* WebAssemblyというWebとは限らないアセンブリでないものがあるよ
* シンプルな仕様だよ
* 任意のコンパイラ - JS連携できるよ
* RustからJSオブジェクトを扱う話はしてないよ

</script>
</section>
