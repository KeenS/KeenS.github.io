---
categories: [WebAssembly, compiler]
date: 2017-01-22T13:35:46+09:00
description: "コンパイラ勉強会での発表用"
title: コンパイラの人からみたWebAssembly
---

<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">

# コンパイラの人からみたWebAssembly
----------------------
[コンパイラ勉強会 - connpass](https://connpass.com/event/46850/)

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
# アジェンダ
------------

* WebAssemblyの概要
* 具体的な話
* WebAssembly吐こうとした話

===

# WebAssemblyの概要

<!-- .slide: class="center" -->
===

# JavaScript
-------------

* ブラウザ上で動くスクリプト言語
* 動的型付
* 高級
* GCとかある
* 今までブラウザで動く唯一の言語だった
* 遅い
  + 各ブラウザJITなどで補強

===

# [asm.js](http://asmjs.org/)
------------

* プラウザ上で動く言語
* 低級
* GCなし
* JSのサブセット
  + asm.jsをサポートしないプラウザでも動かせる
* コンパイラから生成されることを想定
  + emscriptenなど

===

``` javascript
function geometricMean(start, end) {
  start = start|0; // start has type int
  end = end|0;     // end has type int
  return +exp(+logSum(start, end) / +((end - start)|0));
}
...
```

===
# asm.jsの問題点
---------------

* JS互換文法なため嵩張る
* パースも遅い
* そもそもコンパイラが吐くならバイナリでも良いのでは？

===

# WebAssembly
--------------

* ブラウザ上で動く言語
  + 仮想機械命令？
* 低級
* バイナリフォーマット
  + ロード、パース時間が短かい
  + パース20倍くらい速いらしい
* wasm32とwasm64がある
* セマンティクスは（今のところ）ams.jsをほぼ踏襲
  + 実行エンジンは既存のものを使える
* 将来SIMD、スレッドなどの拡張が入る

===

# WASMのゴール
--------------------

* ポータブルでコンパクトで速い
* 仕様策定と実装をインクリメンタルにやっていく
  + 今はとりあえずC/C++をターゲットに
* 既存のJS環境と協調する
  + JSとの相互呼び出しとか
* ブラウザ以外への組込みもサポート
  + Node.jsとか
* プラットフォームになる
  + ツール類のサポートとか

===

# なぜWebAssembly?
------------------

* 実行までのレイテンシが短かい
* asm.jsより速度を出しやすい設計
* クライアントヘビーにしやすい？
* **JSを補完する存在**
* non-determinismが少ない
* LLVM IRと比べてデコードが速くてコンパクト
* (**code generator IR** vs optimization IR)
* 余計なことをしない
  + fast mathとかはない
  + 既に最適化されたコードが吐かれる前提

===
# WebAssemblyの現状
-------------------

* Minimum Viable Product(MVP)
* 機能を削ってとりあえず動くものを作ってる
* 今はC/C++からLLVMを通して吐けるのが目標
* 今後SIMDとかスレッドとかDOM APIとか増えていく
* ChromeとFirefoxでオプトインで使える
  + そろそろFirefoxで普通に動く

===

# 具体的な話
<!-- .slide: class="center" -->

===

# セマンティクス
---------------

* https://github.com/WebAssembly/spec
* 形式的定義されている
* 実行以外にも静的バリデーションもある
* 1ファイル1モジュール
  + JSのモジュールと同じ概念

===
# 実行モデル
--------------

* i32,i64,f32,f64のみ
  + bitエンコーディングは指定
* スタックマシン
  + 命令のオペランドや関数の引数はスタック経由で渡す
  + バイナリがコンパクト+雑にコンパイルしても速い
* 無限のローカル変数が使える
  + 型がある
* 関数の引数はローカル変数経由で渡される
* コントロールフローはgotoじゃなくてstructured
  + 静的検証がしやすい

===

# wasm、wast
------------

* バイナリフォーマットだけでは人間が読めない
* テキストフォーマットも欲しい
* バイナリ: wasm
  + コンパクト
  + intとかもパッキングする
* テキスト: wast
  + 人間可読+機械可読=S式
  + 低級にもちょっと高級にも書ける

===

``` javascript
function geometricMean(start, end) {
  start = start|0; // start has type int
  end = end|0;     // end has type int
  return +exp(+logSum(start, end) / +((end - start)|0));
}
...
```

===

```wasm
(module
  (type (;0;) (func (param i64 i64) (result f64)))
  (type (;1;) (func (param i64 i64) (result f64)))
  (type (;2;) (func (param f64) (result f64)))
  (func (;0;) (type 0) (param i64 i64) (result f64)
    (get_local 0)
    (get_local 1)
    (call 1)
    (get_local 1)
    (get_local 0)
    (i64.sub)
    (f64.convert_s/i64)
    (f64.div)
    (call 2))
  ...)
```

===

```wasm
(module
  (type (;0;) (func (param i64 i64) (result f64)))
  (type (;1;) (func (param i64 i64) (result f64)))
  (type (;2;) (func (param f64) (result f64)))
  (func (;0;) (type 0) (param i64 i64) (result f64)
    (call 2
      (f64.div
       (call 1 (get_local 0) (get_local 1))
       (f64.convert_s/i64
        (i64.sub
         (get_local 1)
         (get_local 0))))))
  ...)
```

===

```
0000000: 0061 736d                                 ; WASM_BINARY_MAGIC
0000004: 0d00 0000                                 ; WASM_BINARY_VERSION
; section "TYPE" (1)
0000008: 01                                        ; section code
0000009: 00                                        ; section size (guess)
000000a: 03                                        ; num types
; type 0
000000b: 60                                        ; func
000000c: 02                                        ; num params
000000d: 7e                                        ; i64
000000e: 7e                                        ; i64
000000f: 01                                        ; num results
0000010: 7c                                        ; f64
; type 1
0000011: 60                                        ; func
0000012: 02                                        ; num params
0000013: 7e                                        ; i64
0000014: 7e                                        ; i64
0000015: 01                                        ; num results
0000016: 7c                                        ; f64
; type 2
0000017: 60                                        ; func
0000018: 01                                        ; num params
0000019: 7c                                        ; f64
000001a: 01                                        ; num results
000001b: 7c                                        ; f64
0000009: 12                                        ; FIXUP section size
; section "FUNCTION" (3)
000001c: 03                                        ; section code
000001d: 00                                        ; section size (guess)
000001e: 03                                        ; num functions
000001f: 00                                        ; function 0 signature index
0000020: 01                                        ; function 1 signature index
0000021: 02                                        ; function 2 signature index
000001d: 04                                        ; FIXUP section size
; section "CODE" (10)
000002e: 0a                                        ; section code
000002f: 00                                        ; section size (guess)
0000030: 03                                        ; num functions
; function body 0
0000031: 00                                        ; func body size (guess)
0000032: 00                                        ; local decl count
0000033: 20                                        ; get_local
0000034: 00                                        ; local index
0000035: 20                                        ; get_local
0000036: 01                                        ; local index
0000037: 10                                        ; call
0000038: 01                                        ; func index
0000039: 20                                        ; get_local
000003a: 01                                        ; local index
000003b: 20                                        ; get_local
000003c: 00                                        ; local index
000003d: 7d                                        ; i64.sub
000003e: b9                                        ; f64.convert_s/i64
000003f: a3                                        ; f64.div
0000040: 10                                        ; call
0000041: 02                                        ; func index
0000042: 0b                                        ; end
0000031: 11                                        ; FIXUP func body size
...
```

===

# JS API
---------

``` javascript
var importObj = {js: {
    import1: () => console.log("hello,"),
    import2: () => console.log("world!")
}};
fetch('demo.wasm').then(response =>
    response.arrayBuffer()
).then(buffer =>
    WebAssembly.instantiate(buffer, importObj)
).then(({module, instance}) =>
    instance.exports.f()
```

===

# メモリ
------------

* メモリアドレスが0から始まって飛びのない **リニアメモリ**
  + 命令で伸び縮み出来る
  + 将来複数のリニアメモリとか出てくるかも
* メモリサイズは32bit(wasm32)か64bit(wasm64)が選べる
  + 現状はwasm32のみ
  + 1つのモジュールでメモリ4GiBバイトも使わないから普通は32bitで十分
* アドレッシングは `アドレス+オフセット`
* アラインメントは必須ではない（した方が速い）
* コード列のメモリは見えない
* スタックスキャンも出来ない

===

# 例外とか
----------

* Trap -- WebAssemblyのインスタンスが異常終了する
  + 例えばメモリの範囲外アクセスとか
* スタックオーバーフロー
  + オーバーフローするとインスタンスが異常終了する
  + 処理系/環境毎にスタック長は違う
    - non-determinism

===

# テーブル
----------

* 要素の配列的なもの
  + 整数インデックスでアクセス出来る
* 今のところ関数を入れてindrect callするため
* 将来的にはOSのハンドラとかGCの参照とか

===

# ローカル変数
--------------

* 無限にある型付きストレージ
* 0 初期化
* 関数の引数もローカル変数に入る

===

# グローバル変数
---------------

* 型付きストレージ
* 可変/不変がある
* 不変Globにsetするとvalidationエラー
* リニアメモリとは違うメモリ領域

===
# import / export
-----------------

* 他のモジュールから色々インポート出来る
* 関数
* テーブル
* グローバル変数
* リニアメモリ
* 勿論exportも

===
# 一旦まとめ
------------

* WASMは1ファイル1モジュール
* WASMには以下がある
  + 関数
    - 関数内ローカル変数
  + リニアメモリ
  + グローバル変数
  + テーブル
  + importテーブル
  + exportテーブル

===

# 命令の話
----------

* コントロールフロー
* Call
* パラメトリック
* 変数アクセス
* メモリ関連
* 定数
* 比較
* 数値
* 変換
* 再解釈

===

# コントロールフロー
-------------------

* `loop` + `br` (名前付き)
* `block` + `br` (名前付き)
* `br_if` or `br_table`
* `if` + `else` + `end`
* `return`
* 等

===

# コントロールフロー
-------------------

* gotoがない
  + `br` はブランチじゃなくてブレイク
* gotoからstructuredに[変換出来る](https://github.com/kripken/emscripten/blob/master/docs/paper.pdf)
    + loop, block, br, br_ifを使う
* 高級言語から変換するなら `if` を使う
  + ifが2系統あることになる

===

# Call
------

* `call`
* `call_indirect`
  + 関数テーブルを使った呼び出し
  + ダイナミックな関数ディスパッチに

===

# パラメトリック
------

* `drop`
* `select`
  + 三項演算子相当


===

# 変数アクセス
--------------

* `get/set_local`
* `get/set_global`
* `tee_local`
  + スタックに値を残しつつset

===

# メモリ関連
------------

* `{i,f}{32,64}.load{,8,16,32,64}{,_s,_u}`
* `{i,f}{32,64}.store{,8,16,32}`
* `current_memory`
* `grow_memory`
  + メモリを増やす命令もある

===
# 定数
------

* `{i,f}{32,64}.const`

===
# 比較
-------

* 各種 `eq`, `eqz`, `ne`,`lt`, `le`, `gt`, `ge`

===

# 数値
-------

* 四則(`i32.add`とか)
* 論理(`i64.popcnt`とか)
* 丸め,最{大,小}(`f32.ceil`とか)
* ルート(`f64.sqrt`とか)

===

# 変換
------

* `f32.convert_s/i32`とか
* `i32.wrap/i64`とか

===

# 再解釈
--------

* `i32.reinterpret/f32`とか
* ビットキャスト
* ビットエンコーディングが定まってるのでwell-defined

===
# WASM吐こうとした話

<!-- .slide: class="center" -->
===
# 作ったやつ
-------------

* なんかコンパイラ作ろうと思い立った
* [KeenS/webml: An ML like toy language compiler](https://github.com/KeenS/webml)
* とりあえずSMLのサブセット
* Rust製
* 未完成
  + 正月気抜いてたら進捗ダメでした

===
# やりたかったこと
------------------

* ブラウザで動くコンパイラ作ってみたい
  + Rustはemscripten通せる
* WebAssembly面白そう
* SML処理系作りたかった
* 最適化書く練習

===
# 中身
------------

* パーサ、AST, HIR, MIR, LIR
* LIRがレジスタマシンなのでそれをWASMに変換したい
* オンメモリで生成するためにアセンブラ自作
  + [KeenS/WebAssembler-rs: An in memory wasm assembler for Rust](https://github.com/KeenS/WebAssembler-rs)
  + ブラウザで動かすのに必要
* 最適化はまだ

===
# コード生成
-----------

* (ほぼ)SSAの1変数 = 1ローカル変数
  + どうせエンジン側でレジスタ割り当てするでしょ
* スタックはほぼ使わない
* gotoを構造化制御フローにする
  +  一応出来る
  + [Reloop](https://github.com/kripken/emscripten/blob/master/docs/paper.pdf)
  + 何言ってるのかよく分からない
  + ステートマシンは勿論可能
* →自分で考えた
* なんかつらいので詳解します
* みんな基本ブロックとCFGは分かるかな？

===
# blockと前方ジャンプ
------------

* `block` + `break` で前方ジャンプ
* 閉じ括弧の位置にジャンプ
* `block` の位置は自由

===

<pre>
<code>
(<span class='hljs-keyword'>block</span>
  ...
  (<span class='hljs-keyword'>br</span> 0)--+
  ...     |
  )<------+
</code>
</pre>

===

<pre>
<code>
(<span class='hljs-keyword'>block</span>
  ...
  ...
  ...
  (<span class='hljs-keyword'>br</span> 0)--+
  ...     |
  )<------+
</code>
</pre>

===

# loopと前方ジャンプ
------------


* `loop` + `break`で後方ジャンプ
  + `loop` からの `break` はいわゆる `continue`
* 開き括弧の位置にジャンプ
* 閉じ括弧の位置は自由

===

<pre>
<code>
(<span class='hljs-keyword'>loop</span><----+
  ...     |
  (<span class='hljs-keyword'>br</span> 0)--+
  ...
  )
</code>
</pre>


===

<pre>
<code>
(<span class='hljs-keyword'>loop</span><----+
  ...     |
  (<span class='hljs-keyword'>br</span> 0)--+
  ...
  ...
  ...
  )
</code>
</pre>


===
# ジャンプのクロス
------------------

* 単一gotoは割り当て出来る
* 複数のgotoが入り組んだら？

===
# 前前
------

```
   [ ]--+
    |   |
+--[ ]  |
|   |   |
|  [ ]<-+
|   |
+->[ ]
```

===
# 前前
------

```
(block
  (block
    ...
    (br 0)-+
    ...    |
    (br 1)-+-+
  )<-------+ |
)<-----------+
```

===
# 後後
------


```
   [ ]<-+
    |   |
+->[ ]  |
|   |   |
|  [ ]--+
|   |
+--[ ]
```

===
# 後後
------

```
(loop<-----+
  (loop<---+-+
    ...    | |
    (br 1)-+ |
    ...      |
    (br 0)---+
  )
)
```

===
# 後前
------


```
   [ ]<-+
    |   |
+--[ ]  |
|   |   |
|  [ ]--+
|   |
+->[ ]
```

===
# 後前
------

```
(block
  (loop<---+
    ...    |
    (br 1)-+-+
    ...    | |
    (br 0)-+ |
  )          |
)<-----------+
```

===
# 前後
------

```
   [ ]--+
    |   |
+->[ ]  |
|   |   |
|  [ ]<-+
|   |
+--[ ]
```

===
# 前後
------

* 出来ない…？
* 部分的にステートマーシン作る？
* ブロック組み換えたら出来る…？
* emscriptenはステートマシンっぽい？
* どうすればいいか不明
* もはやCPS変換して全部Callにする？
  + Compiling With Continuations!!!

===
# ランタイム
------------

* スタック走査出来ない
* GC書けないのでは？？？
* 今のところターゲットはC/C++なので問題ない
* 将来はGC Integration入るかも

===
# 現状の解
----------

1. メモリの自動管理を諦める
2. リージョン推論や線形型で静的管理する
3. スタックを使わないコードにする
  + Compiling With Continuations!!!

===
# まとめ
--------

* ブラウザでアセンブリっぽいコードが動くよ
* バイナリはコンパクトだよ
* コントロールフロー難しいよ
* ランタイム難しいよ
* Compiling With Continuations

</script>
</section>
