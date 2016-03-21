---
categories: [Rust]
date: 2016-03-06T05:27:39+09:00
description: "異種プログラミング言語格闘勉強会での発表用。
主に初心者/使ったことのない人にRustを紹介する内容
"
title: Rustとは何か。どんな言語か。
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# Rustとは何か。どんな言語か。
----------------------
[異種プログラミング言語格闘勉強会](http://kbkz.connpass.com/event/26677/)  
2016-03-20


<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバーエージェントのエンジニア
 + Lisp, ML, Rust, Shell Scriptあたりを書きます


# Rustって?
----------

* システムプログラミング言語
* だけど函数型言語から影響を受けた
* 安全かつ高速
* ゼロコスト抽象化


## システムプログラミング言語
---------------------------

* スレッドはネイティブ、Cとの相互呼び出し、小さなバイナリサイズ
* ゲームエンジンとか作れる([piston](https://github.com/PistonDevelopers/piston))
* レンダリングエンジンとか作れる([Servo](https://github.com/servo/servo))
* Lチカとか出来る
* OSとか作れる([Redox](https://github.com/redox-os/redox))


## 函数型言語的からの影響
-----------------------

最近ではめずらしくなくなった


* デフォルトイミュータブル
* 代数的データ型
* コンビネータ
* トレイト (型クラス)


## 安全かつ高速
--------------

* データ競合が起きない
  + 所有権システム
  + だいたいコンパイル時Read Writeロック
  + Read only参照複数 or Write可能参照1つ
* LLVMベースの強力な最適化
* スレッド周辺のAPIが揃ってる


## ゼロコスト抽象化
------------------
省略

* [Rustのゼロコスト抽象化 | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2016/03/01/rustnozerokosutochuushouka/)


# Rustの誤解
-----------

* 安定してないって聞いたよ
* 函数型言語ってホント?
* GCがなくて大丈夫?
* 継承がなくて大丈夫?


## 安定してないって聞いたよ

* 1.0以前の話
* 1.0(2015-05以降)はAPIの変更がルール化された
* 新規APIのstablizeはあれどdeprecateはまれ


## 函数型言語ってホント?

* 函数型言語の機能をつまみぐいしてるだけ
* 標準ライブラリとかはバリバリ手続的
* Lispよりも函数型言語っぽくない。
* とはいえ抽象力は高いのでそこまで煩雑にはならない


## GCがなくて大丈夫?

* GCはなくてもメモリ管理は自動でする
* コンパイル時にメモリ管理
  + コンパイラが必要なところにmalloc/freeを挟むイメージ
* むしろメモリ以外(fd、ロックなど)も自動管理
  + GCがある言語よりリソース管理の自動化が徹底してる
* [リージョンについて | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2015/12/09/ri_jonnitsuite/)


## 継承がなくて大丈夫?

* むしろ継承必要？インターフェースだけでよくない？
* Rustはデータ型とメソッドを分離するので継承のメリットそんなにない
  + トレイトが強力ともいう
* 逆にサブタイプ関係による複雑さの上昇のデメリットが多い
  + 3回以上継承してるの全部把握出来るの？
* ジェネリクスとかはあるよ


# Rustをとりまく環境
-------------------

* racer/rustfmt
* Cargo
* crates.io
* [Rust Playground](https://play.rust-lang.org/)
* The Rust Programming Language



# racer/rustfmt
---------------

* racerがコーディング支援ツール
  + 補完
  + 定義元ジャンプ
  + 型情報
  + 各種エディタプラグインあり
* rustfmtがコードフォーマッタ
  + エディタプラグインあり
  + ビルドツール連携あり


# Cargo
-------

* Rustのビルドツール兼パッケージマネージャ
* これだけ覚えとけば困らない


# crates.io
-----------

* Rustパッケージのセントラルレポジトリ
* 必ずビルドが通るような工夫
  + cargo yank
  + 同じライブラリの違うバージョンがいても大丈夫
* Cargoとの連携
* Cargoからpublishも可能


# [Rust Playground](https://play.rust-lang.org/)
---------------------------------------

* webからRustを試せる
* とりあえず試すと色々分かる
  + 所有権難しいとか


# [The Rust Programming Language](https://doc.rust-lang.org/book/)
------------------------------

* Rustの入門用ドキュメント
* とりあえずこれ読めば始められる
* [和訳版](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/)もある
  + [GitHub](https://github.com/rust-lang-ja/the-rust-programming-language-ja)で作業中
  + これの宣伝しに来ただけ
* 1.6ほぼ終わり、1.7はまだ


# ライブラリ紹介
----------------
## MIO

* [mio](https://github.com/carllerche/mio)
* 低レベルな非同期IOライブラリ
  + libuvやlibev2相当
  + イベントスレッドではなくイベントループ
* これを純Rustで実装
* つまり(比較的)高水準なAPIで使える
* 色んなライブラリの下地になっている


# ライブラリ紹介
----------------
## diesel

* [diesel](https://github.com/sgrif/diesel)
* ORM & クエリビルダ
  + mioとはうってかわって高水準なライブラリ
* `infer_schema!(dotenv!("DATABASE_URL"));` でコンパイル時に
  + DBにアクセスしてスキーマ情報抜いて
  + コード生成
* モデルもCRUDでちゃんと使い分けている


# 所有権難しい
--------------
下記のコードは動かない。

``` rust
fn main(){
    let hello = "Hello, ".to_owned();
    let world = "World!";
    let hello_world =  hello + world;
    println!("{}", hello);
    println!("{}", world);
    println!("{}", hello_world);
}

```



# まとめ
--------

* Rustはシステムプログラミング言語だよ
* 面白い機能いっぱいあるよ
* もう「安定待ち」じゃないよ
* Try it now!



</script>
</section>
