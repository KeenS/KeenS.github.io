---
categories: [λ計算, リージョン推論, Rust]
date: 2015-04-21T00:25:56+09:00
description: 会社の新卒エンジニア勉強会での発表用
title: 静的なメモリ管理の話。リージョン推論とλ計算からRustまで
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# 静的なメモリ管理の話。リージョン推論とλ計算からRustまで
----------------------
サイバーエージェント新卒エンジニア勉強会

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * 大学では非情報系学科。趣味のプログラマ。
   + 非ガチ勢なので優しくして下さい><
 * Lisp, Ruby, OCaml, Shell Scriptあたりを書きます

===
# メモリ管理の話
---------------
(一般的ではない用語)

* 弱い静的メモリ管理
* 強い動的メモリ管理
* (弱い動的メモリ管理)
* 強い静的メモリ管理

===
# メモリ管理の話
---------------
## 弱い静的メモリ管理
### 特徴

* 弱い = ユーザがミスるとメモリ周りのエラーやリークが実行時に起きる
* 静的 = コンパイル時にメモリ管理が決定する

### 例

* C言語

===
# メモリ管理の話
---------------
## 弱い静的メモリ管理
### 利点

* 実行時は安定している

### 欠点
* バグる
* 面倒
* 危険

===
# メモリ管理の話
---------------
## 強い動的メモリ管理
### 特徴

* 強い = 基本的にメモリでエラーやリークが起きない
* 動的 = 実行時になるまでメモリの使われ方が分からない

### 例

* GCのある言語全般
===
# メモリ管理の話
---------------
## 強い動的メモリ管理
### 利点

* メモリ管理からの開放

### 欠点

* 動作が不安定 (cf [これがCassandra](http://www.slideshare.net/TakehiroTorigaki/cassandra-21191674))
* パフォーマンスの問題
* リアルタイム性の問題

===
# メモリ管理の話
---------------
## (弱い動的メモリ管理)
### 特徴

* 弱い = ユーザがミスるとメモリ周りのエラーやリークが実行時に起きる
* 動的 = 実行時になるまでメモリの使われ方が分からない

### 例

* バグったGC
  + 普通はない
  + (参照カウント？)

===
# メモリ管理の話
---------------
## 強い静的メモリ管理
### 特徴

* 強い = 基本的にメモリでエラーやリークが起きない
* 静的 = コンパイル時にメモリ管理が決定する

===
# メモリ管理の話
---------------
## 強い静的メモリ管理
### 利点

* メモリ管理からの開放
* 実行時は安定している
* リアルタイム
* パフォーマンスが出る

### 欠点

* 実現可能性は？

===
# 強い静的メモリ管理の話をしよう


<!-- .slide: class="center" -->
===
# 強い静的メモリ管理の話をしよう
------------------------------

* まずは可能性の議論が必要
* 実現可能性
* 実用性

===
# 可能性の議論
-------------

* プログラミング言語の理論 ≒ λ計算
* そもそもλ計算はメモリのことを考慮してない
  + GCがあるかのように記述される

===
# 可能性の議論
-------------

* λ計算にメモリ管理まで含めて理論を立てたものは存在する（静的なメモリ管理）
  + リージョンというものをベースにしている [参考](http://www.elsman.com/mlkit/pdf/popl94.pdf)
* さらにリージョンを自動で推論する理論もある（強いメモリ管理）
  + リージョン推論という

===
# 軽くリージョンの話
-------------------

* 型と同じようにプログラム全体を伝わるメタデータ。
* データが保存される場所を指す。リージョンはいくつもある。
* リージョン推論でデータがどのリージョンに入るかが分かる
* さらにリージョンのサイズもある程度予想がつくので静的に管理出来る
* 関数などは引数のリージョンに対して多相になる「リージョン多相」などもある
* この辺は[Martin Elsmanの論文たち](http://www.elsman.com/mlkit/papers.html)を参考にして下さい
  + [A Brief Introduction to Regions](http://www.elsman.com/mlkit/pdf/ismm98.pdf)とか。


===
# 強い静的メモリ管理の話をしよう
------------------------------

* ✓ まずは可能性の議論が必要
* 実現可能性
* 実用性
===
# 実現可能性

<!-- .slide: class="center" -->
===
# 実現可能性
-----------

* <!--.element: class="fragment" data-fragment-index="1" -->Martin ElsmanによるSML処理系、[ML Kitに一部導入された](http://www.elsman.com/mlkit/pdf/pldi2002.pdf)（多分世界初）
  + 但し完全ではなく、GCと組み合わせてある
  + 動的型付き言語に無理矢理静的型を付けても完全には上手くいかないようなもの？
  + 多分リージョン推論を前提とした言語を設計する必要がある
* <!--.element: class="fragment" data-fragment-index="2" -->Cyclone というC likeな文法の言語が完全に[リージョン推論のみでメモリ管理を実現した](http://www.cs.umd.edu/projects/cyclone/papers/cyclone-regions.pdf)

===
# 強い静的メモリ管理の話をしよう
------------------------------

* ✓ まずは可能性の議論が必要
* ✓ 実現可能性
* 実用性

===
# 実用性


<!-- .slide: class="center" -->
===
# Rust言語
----------

* Mozillaが開発した言語
* 2008~
* Cycloneを参考にしたらしい。
* 活発に開発される
* 大きなプロジェクトに現行のレンダリングエンジン、Geckoを置き換えるべく開発された[Servo](https://github.com/servo/servo)がある
  + 既にC++製のGockoの3倍速い
  + 並列レンダリングすればさらに速い。

===
# Rust言語
----------

* リージョン推論(ライフタイム)でメモリを管理する
  - かなり賢くて、ヒープにアロケートする必要なけばスタックを使う。
* mallocとfreeは全てコンパイル時に自動で挿入される
* (多分)リージョン推論のみでメモリ管理するために所有権という概念がある。
  + 所有権自体は並列性の導入などにも有用だと思われる。
    -  競合状態の回避とか
* その他
  + 代数的データ型とパターンマッチ
  + トレイトベース(non-nominal)のジェネリクス

詳細は[公式ページ](http://www.rust-lang.org/)から

===
# 強い静的メモリ管理の話をしよう
------------------------------

* ✓ まずは可能性の議論が必要
* ✓ 実現可能性
* ✓ 実用性

===
# Rustのライフタイムと所有権
-------------------------


<!-- .slide: class="center" -->
===

# Cの例
-------

```c
{
    int *x = malloc(sizeof(int));

    // we can now do stuff with our handle x
    *x = 5;

    free(x);
}
```

===
# Rustに翻訳
------------

```rust
{
    let x = Box::new(5);
}
```

===
# 少しいじってみる
---------------

trivialに見える

```rust
fn main() {
    let x = Box::new(5);

    add_one(x);
}

fn add_one(mut num: Box<i32>) {
    *num += 1;
}
```

===
# 少しいじってみる
---------------

printlnを追加してみる

```rust
fn main() {
    let x = Box::new(5);

    add_one(x);

    println!("{}", x);
}

fn add_one(mut num: Box<i32>) {
    *num += 1;
}
```

===
# 少しいじってみる
----------------

エラーになる。

```
error: use of moved value: `x`
   println!("{}", x);
                  ^
```

===
# 所有権
-------

`add_one` を呼んだ時点で所有権が `add_one` に移るので `println!` では使えない。

```rust
fn main() {
  let x = Box::new(5);

  add_one(x);

  println!("{}", x);
}

fn add_one(mut num: Box<i32>) {
    *num += 1;
}
```

===
# 所有権
-------
新たに値を返してもらえば使える。

```rust
fn main() {
  let x = Box::new(5);

  let y = add_one(x);

  println!("{}", y);
}

fn add_one(mut num: Box<i32>) -> Box<i32> {
  *num += 1;

  num
}
```

===
# 所有権の貸し借り
---------------

* さっきの例は面倒。
* `add_one` が `x` を奪ったのが問題。
* `x` を「借り」ることが出来る。

===
# 所有権の貸し借り
----------------

```rust
fn main() {
  let mut x = 5;

  add_one(&mut x);

  println!("{}", x);
}

fn add_one(num: &mut i32) {
  *num += 1;
}
```

===
# ライフタイム
------------

先の `add_one` はライフタイム(リージョン)アノテーションを省略していた。  
省略せずに書くとこうなる。(リージョン多相)

```rust
fn add_one<'a>(num: &'a mut i32) {
  *num += 1;
}
```

===
# ライフタイム
------------

スコープの終わりでライフタイムが終わる。

```rust
fn main() {
  let y = &5;     // -+ y goes into scope
                  //  |
  // stuff        //  |
                  //  |
}                 // -+ y goes out of scope
```

===
# LTの明示的宣言
--------------
こんな構造体を宣言したとする。 `x` はコンストラクタに渡された値のライフタイムを引き継ぐ。

```rust
struct Foo<'a> {
    x: &'a i32,
}
```

===
# LTの明示的宣言
--------------
`f.x`のライフタイムが `y` のライフタイムに制限されるので
`y` より広いスコープにある `x` には代入出来ない。

```rust
fn main() {
  let x;                  // -+ x goes into scope
                          //  |
  {                       //  |
    let y = &5;           // ---+ y goes into scope
    let f = Foo { x: y }; // ---+ f goes into scope
    x = &f.x;             //  | | error here
  }                       // ---+ f and y go out of scope
                          //  |
  println!("{}", x);      //  |
}                         // -+ x goes out of scope
```

===
# まとめ
-------

* メモリ管理の性質についてまとめた
* 静的メモリ管理が出来れば
  + メモリ管理からの開放
  + 実行時は安定している
  + リアルタイム
  + パフォーマンスが出る
* 強い静的メモリ管理の手法としてリージョン推論がある
* Rust言語がリージョン推論を利用している。
* Rustをみんな使おう!

===
# 参考
* [ML Kit](http://www.elsman.com/mlkit/)
* [Rust](http://www.rust-lang.org/)
* [Allocators in Rust - Baby Steps](http://smallcultfollowing.com/babysteps/blog/2014/11/14/allocators-in-rust/)

<!-- .slide: class="center" -->
</textarea>
