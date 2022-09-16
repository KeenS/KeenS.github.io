---
categories: ["Rust"]
date: 2018-10-07T12:38:57+09:00
description:
title: "Rust around you"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Rust around you
----------------------
[第51回 情報科学若手の会](https://wakate.org/2018/07/28/51th-general/)

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
# Rust
------

* 安全なC++
* ML/Haskellみたいな機能
  + タプル
  + 代数的データ型
  + トレイト
  + 式指向

===

![](/images/rust_kappa.jpg) <!-- .element: height="640px" width="640px" -->

<span style="font-size: 50px">みなさんの脳にRustを叩き込みます</span>

===
# Rustは色々な領域で使われている
-------------------------------

* OS
* ネットワーク
* ブラウザ
* コンパイラ
* WebAssembly
* マイコン
* VCS

===
# Rustは色々な領域で使われている
-------------------------------

* CLI
* ウェブアプリケーション
* DB
* ゲーム
* 暗号通貨
* GPU

===
# OS
-----

* [redos](https://gitlab.redox-os.org/redox-os/redox): フルセットのOS
  + [tfs](https://gitlab.redox-os.org/redox-os/tfs): zfs的なの。並行で動く。
  + [ion](https://gitlab.redox-os.org/redox-os/ion): シェル
  + [orbital](https://gitlab.redox-os.org/redox-os/orbital): Windowシステム
* [intermezzOS](https://intermezzos.github.io/): 小さいOS
* [Writin an OS in Rust](https://os.phil-opp.com/)
  + RustでOSを書くチュートリアル
  + 30日OS本みたいなの

===
# ネットワーク
-------------

* [libpnet](https://github.com/libpnet/libpnet): バケットからネットワークを扱えるライブラリ
* [trust-dns](https://github.com/bluejekyll/trust-dns): DNSクライアント、サーバ、リゾルバ

===

# CLI
-----
* [uutils](https://github.com/uutils/coreutils): catとかlsとかのrust実装
* [ripgrep](https://github.com/BurntSushi/ripgrep): プロジェクト単位で `grep`
* [exa ](https://github.com/ogham/exa): 高級 `ls`
* [fd](https://github.com/sharkdp/fd): 高級 `find`
* [xsv](https://github.com/BurntSushi/xsv): CSVツール
* [tokei](https://github.com/Aaronepower/tokei): clocを取る

===
# WebAssembly
-------------

* 現状の最有力候補
* [wasm-bindgen](https://github.com/rustwasm/wasm-bindgen): Rust-JSブリッジ
* [stdweb](https://github.com/koute/stdweb): ブラウザサイドライブラリ
* [js-sys](https://github.com/rustwasm/wasm-bindgen/tree/master/crates/js-sys): JS標準ライブラリのバインディング
* [web-sys](https://github.com/rustwasm/wasm-bindgen/tree/master/crates/web-sys): WebGLなどのバインディング

===
# その他
--------

* [tock](https://github.com/tock/tock): Cortex-Mで動く組み込みOS
* [pijul](https://pijul.org): 分散VCS
* [REmacs] https://github.com/Wilfred/remacs
* [Rust](https://github.com/rust-lang/rust): rustコンパイラ
* [parity](https://github.com/paritytech/parity-ethereum): ethereum実装

===

# その他
--------

* [TiKV](https://github.com/tikv/tikv): KVS
* [Piston](http://www.piston.rs/): ゲームエンジン
* [amethyst](https://www.amethyst.rs/): ゲームエンジン
* [accel](https://github.com/rust-accel/accel): GPGPUライブラリ
* [ndarray](https://github.com/bluss/ndarray): 多次元配列ライブラリ


</textarea>
