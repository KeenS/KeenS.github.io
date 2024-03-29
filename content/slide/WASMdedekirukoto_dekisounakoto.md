---
categories: [WebAssembly]
date: 2022-11-02T21:31:40+09:00
description: みんなのPython勉強会での発表用。比較的低レベルなWasmの入門
title: "Wasmでできること、できそうなこと"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Wasmでできること、できそうなこと
----------------------

[みんなのPython勉強会#87 - connpass](https://startpython.connpass.com/event/263565/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa2_vest.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

* κeen
* [@blackenedgold](https://twitter.com/blackenedgold)
* [Idein Inc.](https://idein.jp/)のエンジニア
* Lisp, ML, Rust, Idrisあたりを書きます
* Wasm関連プロジェクト: [WebML](https://github.com/KeenS/webml)、[WebAssembler-rs](https://github.com/KeenS/WebAssembler-rs)

===
# 今日の話
----------

* × ツールの使い方
* × Wasmでのアプリケーションの作り方
* ○ WebAssembly(Wasm)がどんな技術なのか
* ○ Wasmをどこで使うといいのか

===
# Wasmって何？
------------------------------

* ブラウザで(も)動くアセンブリっぽいもの
* バイナリ
* 機械語ではなく、バイナリなだけ(≠アセンブリ)
  + 特定のマシンに依存しない
  + 低レベル
  + JSより幾分か高速(JSも結構速いよ)

===
# 論よりコード
-------------

```
0000000 060400 066563 000001 000000 003001 060001 077401 077401
0000020 001003 000001 003407 001401 072563 000155 005000 000447
0000040 000445 077402 040002 040003 000040 000440 006514 020001
0000060 020002 065001 001041 000440 000501 020552 006001 005400
0000100 005400 001040 005417
0000106
```

===
# 論よりコード
-------------

* 流石にバイナリだとコミュニケーションとかで困る
* → テキストフォーマットもある
  + WebAssembly Text Format (WAT)
  + ブラウザではなく周辺ツール向け
* ※基本は手書きするもんじゃない


===
# 論よりコード
------

```lisp
;; hello.wat
(module
  (func $sum (param $n i32) (result i32)
    (local $i i32) (local $sum i32)
    (block $exit
      (loop $loop
        (br_if $exit (i32.le_s (local.get $n) (local.get $i)))
        (local.set $sum (i32.add (local.get $sum) (local.get $i)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)))
    (return (local.get $sum)))
  (export "sum" (func $sum)))
```

===
# WATのコンパイル
------------------


```shell
$ wasm-as -o out.wasm hello.wat
```

この `out.wasm` をブラウザで `fetch` したりしてロードする

===
# Wasmの実行
------------

バイナリをロードしてコンパイルすると関数が参照できるのでJSから呼ぶ

```js
// ロード
const response = await fetch('out.wasm');
const bytes = await response.arrayBuffer();
// コンパイル
const results = await WebAssembly.instantiate(bytes, {});
// 実行
console.log(results.instance.exports.sum(10));
```

→ [デモ](https://github.com/KeenS/wasm-hello-demo)

===
# Wasmの目的
------------

* Cから変換できる、ロードが高速、実行が高速の3拍子
  + C→JSの変換は以前からemscriptenというのがあった
  + 高速な実行は以前からasm.jsというのがあった
  + これらはロードの遅さが課題だった
* Wasmの当初の目標は **ロード** が高速
* WasmはJSを置き換えるもの **ではない**
  + Cのライブラリをブラウザで動かすなどの用途
  + 速度面でJSを補完する働き
* 基本はJSよりできることが少ない
  + (最近は色々機能が増えてきている)

===

# 言語としてのWasm
-----------------

* 整数や浮動小数点数など基本的な型のみ
  + オブジェクトや文字列などはない
* スタック型の実行モデル
* ローカル変数、ループや関数呼出がある
* 単体ではブラウザに作用できない
  + `console.log` とかも呼べない
* 人間が手で書くには厳しい
  + コンパイラなどで生成する前提

===
# Wasmとブラウザ
-------------------------

* JS APIでWasmをコンパイルしたり実行したりできる
  + コンパイルはバイナリ→機械語
* JSとWasm間で関数の相互呼出ができる
  + `console.log` とかをWasmにインポートもできる
* JSからWasmのメモリを操作できる
  + 文字列などのやりとりはこれを使ってやる
  + オブジェクトもJSONシリアライズしてやりとり
  + 参考: [WebAssembly と JavaScript との間で自在にデータをやりとりする](https://zenn.dev/a24k/articles/20221107-wasmple-passing-buffer)

===

# JSをWasmから呼ぶ
---------------------------
→[デモ](https://github.com/KeenS/wasm-hello-demo)

Wasm
```lisp
(module
  ; "console" の "log" をインポートする
  (import "console" "log" (func $log (param i32)))
  (func $print-log (param $n i32)
    ; logを呼び出す
    (call $log (i32.const 10)))
  (export "printLog" (func $print-log)))
```
JS
```js
const response = await fetch('log.wasm');
const bytes = await response.arrayBuffer();
// instanciateするときに"console"の"log"を渡す
const results = await WebAssembly.instantiate(bytes, {
    "console": {
        "log": (i) => console.log(i)
    }
});
results.instance.exports.printLog();
```

===
# Wasmでできないこと
-------------------

* Wasmの外部への作用が基本できない
  + DOM操作
  + HTTP操作
  + etc...
* ブラウザっぽいことやるにはJSとの連携が必須
  + 逆に、連携してしまえばなんでもできる
  + 例えばReactみたいなフレームワークの[yew](https://yew.rs)もある
  + JSとの役割分けは使ってみて判断

===
# Wasmの利用例
--------------

* (教育用に)PostgreSQLをブラウザで[動かす](https://supabase.com/blog/postgres-wasm)
  + Cで書いたプロダクトを動かせる
* Google meet
  + 使ってるらしい(多分映像処理とかその辺)
  + ブラウザで重い処理が動く
* Unityなど
  + Unityから出力したゲームみたいにロードが重いゲームが軽くなる

===
# (オフトピ)Web外Wasm
-----------

* Wasmは単体ではブラウザと密結合してないのでブラウザ外でも使える
* プラグインとかそういうった仕組みに向いてる
  + 素のままでは外界に作用できないので安全性が高い
  + 外部から関数を与えられるので簡単にカスタマイズできる
* 参考: https://logmi.jp/tech/articles/324956

===
# PythonがWebで動く？
--------------------

* Q: Python処理系ってブラウザで動かせる？
* A: 動かせるけどあんまり便利じゃないよ
  + 実際、[Pyodide](https://pyodide.org/en/stable/)というプロジェクトはある
  + Webページにしては起動が遅い
    - ランタイムとか一式をロードしないといけない
  + → 学習用に動かすとかの限定的な用途

===
# PythonがWebで動く？
--------------------

* Rubyだけどゲームを作ってる例はある
  + [Posting Puzzle](https://mame.github.io/posting-puzzle/)
  + 用途次第では使えるかも

<blockquote class="twitter-tweet" data-conversation="none" data-dnt="true"><p lang="ja" dir="ltr">スクリプト部分はすべてWasm版rubyで書きました。直接書いたJavaScriptは一切なし。<br>ruby.wasm、ときどきデバッグがしんどいけど、JavaScript/TypeScriptを書かなくていいのは素直に最高体験だった（個人差はあります）</p>&mdash; Yusuke Endoh (@mametter) <a href="https://twitter.com/mametter/status/1589783794852458496?ref_src=twsrc%5Etfw">November 8, 2022</a></blockquote>


===
# Wasmで動かせる言語
-------------------

* Q: 逆にブラウザで動かすのに向いた言語は？
* A: いくつか満たしてほしい条件がある
  + 事前コンパイル
  + ランタイムが軽い
    - GCはない方がいい
    - 標準ライブラリも省きたい
  + クロスコンパイルが簡単

===
# Wasmで動かせる言語
-------------------

* 今のところRustが有力候補
  + ✅事前コンパイル
  + ✅GCはない
  + ✅標準ライブラリを省ける
  + ✅Wasm向けのクロスコンパイルサポートが充実
* C/C++と違ってメモリ安全なのでアプリケーションを書くのに向いてる
  + とはいえC/C++は既存のライブラリをブラウザで動かす用途で重要

===
# Rust on Web
-------------

[ドキュメント](https://rustwasm.github.io/wasm-bindgen/examples/fetch.html)

```rust
let mut opts = RequestInit::new();
opts.method("GET");
opts.mode(RequestMode::Cors);

let url = format!("https://api.github.com/repos/{}/branches/master", repo);

let request = Request::new_with_str_and_init(&url, &opts)?;

request
    .headers()
    .set("Accept", "application/vnd.github.v3+json")?;
```


===
# Wasm = Rust?
------------

* Q: Wasm使ってブラウザでコードを動かすのにRustは必須？
* A: 難しいところ
  + 今のところお勧めできる選択肢がRustくらいしかない
    - Unityとかツールから出力するならまた話は別
  + ツールの充実やWasmの進化で状況変わるかも

===
# Wasmの発展
------------

* ちょくちょく「今のところは」とか出てきたやつ。
* Wasmは最低限の機能(MVP)から[proposals](https://github.com/WebAssembly/proposals)という形でインクリメンタルに拡張していっている
  + → Proposalsを集めて[Wasm 2.0](https://webassembly.github.io/spec/core/intro/introduction.html)が策定中
* パフォーマンスのためのものやWAMSでできることを増やすものなど様々

===
# 注目のProposals
-----------------

* [SIMD](https://github.com/webassembly/simd)(標準化済み)
  + CPUの並列演算命令を使って高速化できる
* [ガベージコレクション](https://github.com/WebAssembly/gc)
  + ブラウザ側でGCの仕組みを提供することでGC有りの言語も使いやすくなる
* [例外ハンドリング](https://github.com/WebAssembly/exception-handling)
  + 特殊な操作である例外の送出とハンドルをサポート
* → 将来もっと色々な言語を動かせるようになるかも？

===
# まとめ
---------

* Wasmはブラウザで動く第二の言語だよ
  + 手で書くものではなく他言語からコンパイルされるよ
  + JSを置き換えるのではなく相補的なものだよ
* JSより速く動くけどユーザとの作用はJSに依存するよ
* 今のところRust経由で使うのが有力な手段だよ
* 今後の発展にも期待

</textarea>

