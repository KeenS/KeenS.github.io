---
categories: ["Rust"]
date: 2018-08-25T16:09:33+09:00
description: "LLイベントでの発表用"
title: "Rustのこれまでとこれから"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">

# Rustこれまでとこれから<!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)"-->
----------------------
[Learn Languages 2018 in ODC (LL2018) ](https://llevent.connpass.com/event/95443/)

<!-- .slide: class="center" -->

===

# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [ちゅーんさんだよー](https://shindanmaker.com/789932)
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
  + Rustで開発してるよ
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# 今日話すこと
-------------

* X Rustの勉強法
* X Rust言語の紹介
* O Rustの歩み
* O Rustの開発体制

===
# Rustとは
----------

* システムプログラミング言語
* 安定性、信頼性が売り
* 2015年に[1.0リリース](https://blog.rust-lang.org/2015/05/15/Rust-1.0.html)
  + ~~いつまで20年ものの言語使ってるの~~
* 大体の便利な言語機能を取り入れてる
* 所有権システムにより **GCがない**
  * コンパイラが `free` を自動挿入

===

```rust
// ジェネリクス+トレイト境界
// move セマンティクス
fn dup<T: Clone>(t: T) -> (T, T) {
    // タプルあるよ
    (t.clone(), t)
}

fn main() {
    // マクロ
    let vec = vec![1, 2, 3];
    // パターンマッチで分配束縛
    let (v1, v2) = dup(vec);
    // 雑にデータ型を印字できる
    println!("v1: {:?}, v2: {:?}", v1, v2);
    // `v1`, `v2`はコンパイラが勝手にfreeしてくれる
    // (`vec`はmoveしてるのでそもそも関係ない)
}

```

===

# 特徴
------

* エルゴノミクスに拘った設計
  + explicitな言語
* 後方互換を大事に
  + SemVerベースの管理
  + バージョンアップでコードが壊れることは殆ど無い
* 学習難易度は高め

===
# 学習難易度
----------------

* 難しい(直球)
* 悪い難しさではない
  + プログラミング本来の難しさ
* その代わり学習補助が手厚い
 + エラーメッセージが親切
 + 公式ドキュメントがよく出来てる

===

![Rustの学習曲線の図](/images/ll_rust/rust_learning_curve.png) <!-- .element: style="width:100%;height:100%;" -->

===
# ドキュメント文化
-----------------

* [The Rust Programming Language](https://doc.rust-lang.org/book/)がよく出来てる
* 他にも[Rust by Example](https://doc.rust-lang.org/rust-by-example/index.html)、[Rustnomicon](https://doc.rust-lang.org/nomicon/index.html)、[The Unstable Book](https://doc.rust-lang.org/unstable-book/index.html)などなど
* 全クレート(ライブラリ)のドキュメントも[ある](https://docs.rs/)
* [This Week in Rust](https://this-week-in-rust.org/)などのニュースレター

===

# 日本
------

* ドキュメントの和訳とか頑張ってる
  + [Rustの日本語ドキュメント/Japanese Docs for Rust](https://doc.rust-jp.rs/)
* [Slackコミュニティ](http://rust-jp.herokuapp.com/)
* [勉強会](https://rust.connpass.com/)
  + [初心者向けハンズオン](https://rust.connpass.com/event/56275/)なども
* などなど

===
# Rustの開発体制
---------------

* コミュニティベースの開発
  + [担当チーム](https://www.rust-lang.org/ja-JP/team.html)などあり組織的
* Mozillaが開発を支援
  + 「Mozillaが作った言語」ではない
* [RFC](https://github.com/rust-lang/rfcs)で言語機能を決める
  + 意思決定がオープン
* 6週間ごとにリリース
  + stable, beta, nightlyの3チャネルのサイクル
  + 新機能はfeature gateで一旦試してからbeta, stableに降ってくる

===

![Rustの開発サイクル](/images/ll_rust/rust_dev_cicle.png) <!-- .element: style="width:100%;height:100%;" -->

===
# 意思決定とか
-------------

* サーベイしてデータドリブンで注力領域を決めてる
  + [Survey 2017](https://blog.rust-lang.org/2017/09/05/Rust-2017-Survey-Results.html)
  + [2018](https://blog.rust-lang.org/2018/08/08/survey.html)もやってる([和訳](https://docs.google.com/forms/d/e/1FAIpQLSeueHtp6L0hPGy6h9tUxgNEnUv1xBxBqBdJlHsKCCx37yGMug/viewform)もあるよ)
* サーベイの結果Domain Working Groupが[発足](https://internals.rust-lang.org/t/announcing-the-2018-domain-working-groups/6737)
* WGのミーティングなども公開されてる
  + [GitHub](https://github.com/rust-lang-nursery/wg-net)
  + 徹底的にオープンだよね。

===

## 4つのWorking Groupとその目標

<style>
#grid-container .box26 {
    position: relative;
    margin: 0.5em 1em;
    padding: 0.5em 1em;
    border: solid 3px #000;
    border-radius: 8px;
}
#grid-container .box26 .box-title {
    position: absolute;
    display: inline-block;
    top: -13px;
    left: 10px;
    padding: 0 9px;
    line-height: 1;
    font-size: 24px;
    background: #FFF;
    font-weight: bold;
}
#grid-container .box26 p {
    margin: 0;
    padding: 0;
}
</style>

<div style="display:grid;grid-template-rows: 100% 1fr;grid-template-columns: 50% 1fr;" id="grid-container">
   <div style="grid-row:1/2;grid-column:1/2" class="box26">
       <span class="box-title"><a href="https://github.com/rust-lang-nursery/wg-net">Network</a></span>
       <p> 非同期、IoT、ウェブの3つに注力</p>
   </div>
   <div style="grid-row:1/2;grid-column:2/3" class="box26">
       <span class="box-title"><a href="https://github.com/rust-lang-nursery/cli-wg">CLI</a></span>
       <p>クロスプラットフォームでよくテストされてモダンなCLIツールを作れるように</p>
   </div>
   <div style="grid-row:2/3;grid-column:1/2" class="box26">
       <span class="box-title"><a href="https://github.com/rustwasm/team">WASM</a></span>
       <p>Rustを最高のWASM開発言語にする</p>
   </div>
   <div style="grid-row:2/3;grid-column:2/3" class="box26">
       <span class="box-title"><a href="https://github.com/rust-embedded/wg">Embed</a></span>
       <p>組み込みプログラミングをもっと簡単に</p>
   </div>
</div>



===

# 最近の話題
--------------

* SIMD Intrinsic
* メモリアロケータが選択可能に
* 非同期
  + futureやasync/awaitなど
* Generic Associated Type
  + [Chalkプロジェクト](https://github.com/rust-lang-nursery/chalk)
  + 雑にいうとProlog処理系をコンパイル時に動かす
* Compile Time Function Execution
  + [Miriプロジェクト](https://github.com/solson/miri)
  + 雑に言うとRustインタプリタをコンパイル時に動かす

===

# SIMD

``` rust
#[cfg(all(any(target_arch = "x86", target_arch = "x86_64"),
      target_feature = "avx2"))]
fn foo() {
    #[cfg(target_arch = "x86")]
    use std::arch::x86::_mm256_add_epi64;
    #[cfg(target_arch = "x86_64")]
    use std::arch::x86_64::_mm256_add_epi64;

    unsafe {
        _mm256_add_epi64(...);
    }
}
```
===

# メモリアロケータ

``` rust
use std::alloc::System;

#[global_allocator]
static GLOBAL: System = System;

fn main() {
    let mut v = Vec::new();
    // This will allocate memory using the system allocator.
    v.push(1);
}
```

===

# Rustのこれから
<!-- .slide: class="center" -->

===

# Rust 2018 Edition

<!-- .slide: class="center" -->

===

# Rust 2018
-----------

* 言語に非互換アップデート入るよ
  + キーワードの追加とか
* 今までのを2015、新しいのを2018と呼ぶ
 + Rust 2015: stability
 + Rust 2018: productivity
* 3年に1度更新される
* 互換性はないけど相互運用性はある
  + コンパイラは2015と2018同時サポート
  + 2015と2018は混ぜて使える
* さらに移行ツールも提供されてる

===
# 変わるところ
-------------

* 新規構文
  + `async` / `await`
  + `try`
* ライフタイムの省略や自動推論などの強化
* 2015も継続して改善される
  + 急に大きく変わる訳ではない
  + 2018の機能が既に入っているものも
* いままでの変更のまとめ的な意味もある
  + [edition guide](https://rust-lang-nursery.github.io/edition-guide/introduction.html)で2015(1.0.0)との差分が見れる

===
# まとめ
--------

* Rustは新しい言語だよ
* Rustはオープンな開発体制をしてるよ
* アウトリーチの努力がすごいよ
* 今も継続的に改善されてるよ

</script>
</section>
