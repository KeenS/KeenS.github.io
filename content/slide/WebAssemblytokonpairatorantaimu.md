---
categories: [WebAssembly, Compiler, Runtime]
date: 2017-02-16T15:46:21+09:00
description: null
title: WebAssemblyとコンパイラとランタイム
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# WebAssemblyとコンパイラとランタイム
----------------------
[emscripten night !! #3 - connpass](https://emsn.connpass.com/event/48100/)

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
# 何の話
---------

* WebAssemblyをバックエンドに使う言語を作りたい
* [KeenS/webml: An ML like toy language compiler](https://github.com/KeenS/webml)
* 既に発表したものの補足記事的な
  + [コンパイラの人からみたWebAssembly | κeenのHappy Hacκing Blog](https://keens.github.io/slide/konpairanoninkaramitaWebAssembly/)
  + ある程度再度説明します

===
# アジェンダ
------------

* なぜWebAssembly
* コンパイラ
* コード生成
* ランタイム

===

# なぜWebAssembly

<!-- .slide: class="center" -->
===
# なぜWebAssembly
----------------

* なんかバイナリでテンション上がる
* ポータブルでコンパクトで速い
* 機能的にJSを越えるかもしれない…？
* コンパイラがブラウザで動くと言語導入のハードル下がる
  + 気軽に試せる
  + コンパイラだけでなく吐いたコードもブラウザで動かしたい

===
# WASM over LLVM
----------------

* 既存のJS環境と協調する
  + JSとの相互呼び出しとか
* **non-determinismが少ない**
* 細かいアラインメント考える必要がない
* **API安定しそう**

===

# WASM over asm.js
--------------------

* ブラウザ以外への組込みもサポート
  + Node.jsとか
  + **JITエンジンに使えそう**
* プラットフォーム
* 実行までのレイテンシが短かい
* asm.jsより速度を出しやすい設計
* テキストフォーマットで生成コードの文法エラーが怖い
* 将来機能が増えるかも

===
# WebAssemblyの現状
-------------------

* Minimum Viable Product(MVP)
* 機能を削ってとりあえず動くものを作ってる
* 今はC/C++からLLVMを通して吐けるのが目標
  + C/C++には不要でも自分の言語に欲しい機能は……
* 今後SIMDとかスレッドとかDOM APIとか増えていく

===

# コンパイラ

<!-- .slide: class="center" -->

===
# WASM概要
----------

* WASMには以下がある
  + 関数
    - 関数内ローカル変数
  + リニアメモリ
  + グローバル変数
  + テーブル
  + importテーブル
  + exportテーブル

===
# WASM実行モデル
--------------

* i32,i64,f32,f64のみ
* **スタックマシン**
  + 命令のオペランドや関数の引数はスタック経由で渡す
  + 1 passのコード生成が楽そう
* 無限のローカル変数が使える
  + 型がある
* 関数の引数はローカル変数経由で渡される
* コントロールフローはgotoじゃなくてstructured
  + 静的検証がしやすいらしい
  + 安全だけどコンパイラ的には…

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
# サンプル
---------

``` javascript
function geometricMean(start, end) {
  start = start|0; // start has type int
  end = end|0;     // end has type int
  return +exp(+logSum(start, end) / +((end - start)|0));
}
...
```

===
# サンプル
---------

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
# サンプル
---------

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

# WebMLコンパイラ
------------

* パーサ、AST、HIR、MIR、LIR
* LIRがRTLなのでそれをWASMに変換したい
* gotoからstructured control flow…

===

```
[コード]
   | パーサ
 [AST] 型推論とか
   | AST2HIR
 [HIR] 早期最適化、K正規化、A正規化など
   | HIR2MIR
 [MIR] 諸々の最適化
   | MIL2LIR
 [LIR] シリアライズ、レジスタ割り当てなど
   | コード生成
 [WASM]
```

===

# 変数
------

* LIRはレジスタで計算する
* LIRはCFG由来の大量の変数を使う
  + レジスタ割り当ては一旦置いとく
* WASMはスタックで計算する
* どうコード生成すると最適か？

===

```
fun main: () -> i64 = {
    entry@0:
        r0: i64 <- 1
        r1: i32 <- 0
        r2: i64 <- 1
        r3: i64 <- 2
        r4: i64 <- 3
        r5: i64 <- r3 * r4
        r6: i64 <- 4
        r7: i64 <- r5 + r6
        r8: i64 <- r2 + r7
        r9: i64 <- 1
        r10: i64 <- 2
        r11: i64 <- r9 + r10
        r12: i64 <- 3
        r13: i64 <- r11 * r12
        r14: i64 <- 4
        r15: i64 <- r13 + r14
        r16: i32 <- 1
        jump_if_zero r16 then@1
        jump else@2
    then@1:
        r17: i64 <- r8
        jump join@3
    else@2:
        r17: i64 <- r15
        jump join@3
    join@3:
        r18: i64 <- 1
        r19: i64 <- r18 + r15
        r20: i64 <- 1
        r21: i64 <- call d@9(r20, )
        r22: i64 <- 2
        r23: i64 <- call #g37(r22, )
        r24: i64 <- 1
        r25: i64 <- heapalloc(16)
        [r25+0] <- <anonfun>@11
        [r25+8] <- r24
        r26: i64 <- 0
        ret r26
}
```

===

# 変数割り当て
--------------

* SSAの1変数 = WASMの1ローカル変数
* スタックの効率利用を完全に無視
* 計算はLV→スタック→LVに書き戻し
* どうせスタックもLVもレジスタ扱いにしてレジスタ割り当てされるでしょ
* （測ってないけど）多分速度は変わらない

===

# CFG
-----

* コンパイラが一旦gotoを使うコントロールフローグラフを作る
* WASMにはgotoがない
* どうやったら生成出来るか？
* そもそも生成出来るの？

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

# 絶対出来る
------------

* チューリング完全なら必ず書ける
  + whileとswitchでステートマシン作ればいい
* 効率的とは限らない
* 効率的なコードを吐きたい

===
# ステートマシンはつらい
-----------------------

* これをナイーブに変換すると…

```c
#include <stdio.h>
int main() {
  int sum = 0;
  for (int i = 1; i <= 100; i++)
    sum += i;
  printf("1+...+100=%d\n", sum);
  return 0;
}
```

===

```javascript
function _main() {
    var __stackBase__ = STACKTOP;
    STACKTOP += 12;
    var __label__ = -1;
    while(1)
        switch(__label__) {
        case -1:
            ...
            __label__ = 0;
            break;
        case 0:
            ...
            if ($4) {
              __label__ = 1;
              break;
            } else {
              __label__ = 2;
              break;
            }
        case 1:
            ...
            __label__ = 3;
            break;
        case 3:
            ...
            __label__ = 0;
            break;
        case 2:
            ...
            return 0;
        }
}
```

===
# ステートマシンはつらい
----------------------

* どうにかする必要がある
  + emscriptenは[ReLoop](https://github.com/kripken/emscripten/blob/master/docs/paper.pdf)で最適化
* なんか気に食わなかった
* そもそもステートマシンを使わずに生成したい
* 複数のgotoが入り組んだパターンを自分で考えたの紹介します。
* CFGと基本ブロックは知ってるかな？

===
# 前前
------

```
[ ]--+
 |   |
[ ]--+-+
 |   | |
[ ]<-+ |
 |     |
[ ]<---+
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
[ ]<-+-+
 |   | |
[ ]--+ |
 |     |
[ ]----+
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
[ ]--+-+
 |   | |
[ ]--+ |
 |     |
[ ]<---+
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
[ ]<-+-+
 |   | |
[ ]<-+ |
 |     |
[ ]----+
```

===
# 前後
------

* 素直には出来ない…？
* 部分的にステートマーシン作る？

===
# 部分的ステートマーシン
-------------------------

```
[1]--+
 |   |
[2]<-+-+
 |   | |
[3]<-+ |
 |     |
[4]----+
```

===
# 部分的ステートマーシン
-------------------------

```
   [1] label = 0
    |
+->[br]-+ if label == 0
|   |   | then goto 3
|   |   | else goto 2
|   |   |
|  [2]  |
|   |   |
|  [3]<-+ label = 1
|   |
+--[4]
```

===
# コード生成

<!-- .slide: class="center" -->
===

# フォーマット
------------

* バイナリフォーマットとテキストフォーマットがある
* バイナリ: wasm
  + コンパクト
  + intとかもパッキングする
* テキスト: wast
  + 人間可読+機械可読=S式
  + 低級にもちょっと高級にも書ける
  + 一旦アセンブラ噛まさないと動かない

===

# アセンブラ
------------

* オンメモリで生成するためにアセンブラ自作
  + [KeenS/WebAssembler-rs: An in memory wasm assembler for Rust](https://github.com/KeenS/WebAssembler-rs)
  + ブラウザで動かすのに必要
  + 既存のツールは使えない
* アセンブラ自体ブラウザで動かすのでRust製
* まだ動かしてない

===

```rust
let mut mb = ModuleBuilder::new();
let f = mb.new_function(FunctionBuilder::new(funtype!((i32, i32) -> i32))
    .code(|cb, args| {
        cb.constant(2)
            .get_local(args[0])
            .i32_add()
    })
    .build());
mb.export("addTwo", f);
let module = mb.build();
```

===
# スタック領域
-------------

* Cでいう`struct foo x;`みたいにエフェメラルな多ワード領域が欲しい
* WebAssemblyのローカルストレージはLVだけ
  + 最大1ワードしか保存出来ない
* 可能性
  1. 諦めてメモリに確保（場合によっては最適化で消えるかも）
  2. ワード毎に分割してLVに保存（大変だけど速そう）

===
# ランタイム

<!-- .slide: class="center" -->
===
# 言語のランタイム
-----------------

* 主にはGC
* その他データのメモリ表現
  + アラインメントも
* スタック領域
* FFI
* コンパイラなのでシンボルテーブルはなし

===
# メモリ
--------

* mallocとかはない
* ページ単位のアロケーションだけ
* 自前でGCを実装することになる

===
# GC
-----

* コールスタックを遡れない
  + コールスタックをGCルートに出来ない
* メモリonlyな走査なら可能
  + ポインタを都度メモリ領域(arena)にコピーすれば良い。
  + arenaもルートになる
  + コールスタックと連動するのでスタックで管理出来る。
  + CF [Matzにっき(2013-07-31)](http://www.rubyist.net/~matz/20130731.html)
  + ただしコンパイラなので関数全部をsave/restoreで囲んだりはしない。

===
# アロケータ
------------

* どうにか書いてブラウザでリンクする必要がある
  + ブラウザにもランタイムライブラリの時代…
  + WASMはライブラリ間でメモリ共有出来る
* とりあえずRustで書く方針
  + WASMのページアロケートとかどうすればいいんだろう
  + まだ色々未定

===

# メモリ表現
------------

* 出来れば楽して64bit統一したかった
  + i32やf32を抹殺したかった
* WASM32しかないのでポインタが32bit…
* 仕方ないので64bitアラインメントでパディングする
* 空いた32bitの使い道は未定
  + 静的型付だし型タグが要らない
  + 代数的データ型のタグ？

===

# 高階関数
----------

* WASMに関数ポインタがない
* テーブルに関数を登録してインデックス参照
  + C++のvtableのための機能
  + 型も動的チェック
* ちょっと遅そう
* 気合でインライン化を頑張ろう

===

# FFI
------

* JSの関数を呼びたい
* 細かいところどうなってるんだろう
  + 例外は？JSオブジェクトは？GCは？
* 型付…
* ノープラン

===
# 雑にまとめ
-------------

* WASMはちょっと高級なので最適化コンパイラは困るよ
  + 雑なコンパイラにはむしろ嬉しい
* コード生成は努力で解決
* GCは割とつらいよ
  + 将来楽になるかも
* JS連携や将来のスレッドとかはみんなで考えよう

</textarea>
