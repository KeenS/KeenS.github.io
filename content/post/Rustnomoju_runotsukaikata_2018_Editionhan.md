---
categories: ["Rust", "Advent Calendar", "Advent Calendar 2018", "Rust Advent Calendar"]
date: 2018-12-08T01:54:39+09:00
title: "Rustのモジュールの使い方 2018 Edition版"
---
このエントリーは[Rust Advent Calendar 2018](https://qiita.com/advent-calendar/2018/rust)7日目の記事です。

κeenです。Rust 2018が来ましたね。最近本業も副業(?)も大詰めで中々時間がとれずAdvent Calendarに遅刻してしまいました。
それはさておき、私は[Rustのモジュールの使い方](https://keens.github.io/blog/2017/01/15/rustnomoju_runokirikata/)を過去に書きました。
この記事は中々好評だったようですが、状況が変わりまして2018 Editionでモジュールが大幅に変更され、分かりやすくなりました。
巷に2018での変更点は多く喧伝されていますが2015との差分ばかりで、今からモジュールシステムを学ぶ方はまず複雑な2015を理解してから差分を読んで、簡単な2018を理解しないといけなくなっています。
そこで2018からはじめて学習する方向けにモジュールシステムの解説をしようと思います。
<!--more-->

とりあえずサンプルプロジェクトを作ります。

```
$ cargo new module2018
```

# エライのは `lib.rs` と `main.rs`
モジュールの話をする前にクレートの話をしないといけません。
クレートはRustのプログラムの一番大きな単位です。
大きく分けてライブラリを作るlibクレートと実行可能ファイルを作るbinクレートがあります。

libクレートならば`lib.rs`が、binクレートならば`main.rs`がルートにあたるファイルです。ここから辿れるモジュールが全てです。プロジェクトにファイルが存在していてもこれらから辿れなかったらコンパイルされません。

# モジュールの作り方は2種類

1. ファイルの中にインラインで作る
2. 1ファイルで作る


のやり方があります。あと別解もあります。 1.はそんなに使われなくて、説明のために1ファイルにまとめたい時や、可視性でアレコレする時のテクニックなんかに使われます。

2.のファイルで作る時はまず以下のようにファイルを作ります。

``` console
$ tree src
src
├── lib.rs
└── new_module.rs
```

そして `lib.rs` の中で以下のように`mod`で参照してあげれば使えます。

``` rust
mod new_module;
```

## サブモジュールをディレクトリで作る

モジュールは入れ子にできます。 `new_module.rs` のファイル内に書く方法もありますがここではディレクトリで作る方を紹介します。
以下のように`new_module.rs`の他に `new_module/new_submodule.rs` を用意します。

``` console
$ tree src
src
├── lib.rs
├── new_module
│   └── new_sub_module.rs
└── new_module.rs
```

そして `new_module.rs` に以下を書いてあげます。

``` rust
mod new_sub_module;
```

すると `src/new_module/new_sub_module.rs`  の中身が `new_module` のサブモジュールになります。

## 別解: 2015の遺産

`new_module.rs` の代わりに `new_module/mod.rs` も使えます。多分後方互換のために残してるんですかね。
先の例でいくと以下のような形です。

最初のモジュールを作る例は以下のようにも書けます。

``` console
$ tree src
src
├── lib.rs
└── new_module
    └── mod.rs
```

サブモジュールを作る例は以下のようにも書けます。 `mod.rs` に `mod new_sub_module;` を書きます。

``` console
$ tree src
src
├── lib.rs
└── new_module
    ├── mod.rs
    └── new_sub_module.rs
```

昔はディレクトリを切ったら `mod.rs` が `lib.rs` の代わりの役割をしていたんですね。

# 兄弟モジュール同士の可視性

## モジュールの身分
さて、次のように2つのモジュールを作ってみましょう。lib.rsには`module_a`のみ`mod`宣言しておきます。

```console
$ tree src
src
├── lib.rs
├── module_a.rs
└── module_b.rs
```

lib.rs 
``` rust
mod module_a;
```

この状態で`module_a.rs`に`module_b`をサブモジュールとして宣言してみましょう。

``` rust
mod module_b;
```

これをコンパイルすると

``` console
$ cargo build
   Compiling module2018 v0.1.0 (/home/shun/Rust/module2018)
error[E0583]: file not found for module `module_b`
 --> src/module_a.rs:1:5
  |
1 | mod module_b;
  |     ^^^^^^^^
  |
  = help: name the file either module_a/module_b.rs or module_a/module_b/mod.rs inside the directory "src"

error: aborting due to previous error

For more information about this error, try `rustc --explain E0583`.
error: Could not compile `module2018`.

To learn more, run the command again with --verbose.
```

と怒られます。一番エラいのは`lib.rs`ですから、身分の低い `module_a`では`mod`宣言出来ません。

## モジュールのパスネーム

同じディレクトリ構成で、今度は`lib.rs`に`module_a`と`module_b`両方を`mod`宣言しておきます。

``` console
$ tree src
src
├── lib.rs
├── module_a.rs
└── module_b.rs

```
lib.rs
``` rust
mod module_a;
mod module_b;
```

ついでに中身を追加しましょう。`module_a.rs`に以下を書きます。

``` rust
pub fn name() {
    println!("module_aだよ");
}

```

この時、`module_b`が`lib.rs`から見た時と`module_a.rs`から見た時で名前が変わります。

lib.rsから見た時は、以下の2つが使えます。

```
use crate::module_a::name;  // 絶対パス
use self::module_a::name;   // 相対パス
```

キーワード `crate` と `self` がでてきました。
`crate`というのは冒頭で説明したとおりRustのプロジェクトの単位です。ここでは「このプロジェクトの `module_a` に所属する `name` 関数」と指定しているわけです。
`self`というのはディレクトリでいうところの`.`に相当します。「いまいるモジュールのサブモジュールの `module_a` に所属する `name` 関数」と指定しています。

module_b.rsからは同じく2つが使えますが、相対パスが変わります。

```
use crate::module_a::name;  // 絶対パス
use super::module_a::a;     // 相対パス
```

`super`も予約語で、`..`に相当します。
ファイルが同じディレクトリにいるので分かりづらいですが、libが1つ上の階層で、その下にmodule_aとmodule_bがぶら下がってる感じですね。

2020-06-07追記: 上記の絶対パスが `crate::module_a::module_a::name` になっていたのを `crate::module_a::name` に修正しました。 /追記
# おじさんと隠し子
次のように、サブモジュールを作ってみます。

```
$ tree src
src
├── lib.rs
├── module_a
│   └── submodule.rs
├── module_a.rs
└── module_b.rs
```

lib.rsに以下を書きます。

``` rust
mod module_a;
mod module_b;
```

module_a.rsに以下を書きます。

``` rust
mod submodule;
```

さて、この時`sumbmodule`から見た`module_b`の名前はどうなるでしょうか。もうお分かりかと思いますが、以下の2つです。

``` rust
use crate::module_a;     // 絶対パス
use super::super::module_a; // 相対パス
```

じゃ、逆に`module_b`から見た`submodule`はどうなるでしょうか。実は、`module_b`から`submodule`は見えません（lib.rsからも見えません）。`module_a`1つで所帯を持っているので、子である`submodule`を外に出すかは`module_a`の一存で決まります。今回は隠し子にしている訳ですね。いくら兄弟モジュールとはいえ家庭にまでは入り込めないのです。

子供をちゃんと公開するには

`module_a.rs` の

```rust
mod submodule;
```

となっていたモジュール宣言に`pub` を付けてあげて

```rust
pub mod submodule;
```

としてあげればOKです。そうすると`module_b`からも見えて、以下の2種類で参照出来ます。

```rust
use crate::module_a::submodule;
use super::module_a::submodule;
```

# 家庭裁判

モジュールは名前を変えられます。

``` rust
mod module_a;
use self::module_a as a;
```

これで以後、 `a` というモジュールがあるかのように扱えます。
改名したものを公開したければ

``` rust
mod module_a;
pub use self::module_a as a;
```

のように `use` の方に `pub` を付けてあげれば目的を達成できます。

# 木箱の外から
さて、今までは1クレート内での話でした。クレートの外から見るとどうなるでしょうか。

新たに`main.rs`を追加しましょう。先程 `lib.rs`と`main.rs`がエライと話しましたが、どちらも別々のクレートを作るので`main.rs`は完全にクレートの外です。

こんな感じですね。

```
$ tree src
src
├── lib.rs
├── main.rs
├── module_a
│   └── submodule.rs
├── module_a.rs
└── module_b.rs
```

`lib.rs` は以下です。

``` rust
mod module_a;
mod module_b;
```

`module_a.rs` は以下です。

``` rust
pub mod submodule;
```

まず、binクレートなので`main`関数も必要ですね。

`main.rs`

```rust
fn main() {}
```

では、`module_b`はどういう名前で見えるでしょうか。見えません。一番エライ`lib.rs`が`module_b`を`pub`にしていないのでクレートの外からは見えなくなっています。
見たければ `lib.rs` を以下のようにします。

``` rust
mod module_a;
pub mod module_b;
```

`mod module_b` の前に `pub` が付きました。

さて、これでmain.rsからどのように見えるかというと、以下のようになります。

``` rust
use module2018::module_b;
```

`crate::` なしでアクセスしてます。 `crate::` がなかったら外様という扱いなんですね。

## 外部クレートのリネーム

`Cargo.toml` でできます。今回は1クレートで説明している関係で詳しく説明できません。
[ドキュメント](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#renaming-dependencies-in-cargotoml)を参照してください。

# 福は内

`module2018` を内部のモジュールのように扱う方法もあります。 2015まではこの方法しかありませんでした。
`extern crate module2018;` と、 `extern crate` 宣言してあげます。
するとあとは内部モジュールと同じようにアクセスできます。

``` rust
extern crate module2018;

use crate::module2018::module_b; // crate::.. と内部モジュールのように扱える
```

これは `extern crate` したモジュールのサブモジュール扱いになっています。UNIXファイルシステムに例えるとマウントしてるようなイメージですね

因みに`extern crate` を使ったリネームもできます。

``` rust
extern crate module2018 as m18;

use crate::m18::module_b;
```

# 家出
さて、同じように`lib.rs`で`module_a`を`pub mod`すれば`module::module_a::submodule`も参照出来るようになります。ところで、例えば`module_a`は外に出さずに`submodule`だけを公開したい時にはどうしたらいいでしょうか。そんなケースあるのかと疑問に思うかもしれませんが、あります。同じクレートであっても`pub`にしないとサブモジュールにアクセス出来ないことを思い出して下さい。そうすると

``` rust
pub mod submodule;
pub mod internal_submodule_a;
pub mod internal_submodule_b;
```

みたいなケースが発生するのは想像出来ると思います。そういう時は、`lib.rs`側でどうにかいじれます。

``` rust
mod module_a;
pub mod module_b;
```

となっていると思いますが、

```rust
mod module_a;
pub mod module_b;

pub use self::module_a::submodule;
```

と、`pub use` を加えてあげることで`module_a`の下から出すことが出来ます。

これを`main.rs`から使う時は

```rust
use module2018::submodule;
```

のように名字の`module_a`が取れます。

さらに踏み込めば、`use`は`as`を使ってリネーム出来るので`lib.rs`から以下のようにリネーム`use`してあげれば

```rust
mod module_a;
pub mod module_b;

pub use self::module_a::submodule as module_c;
```

`main.rs` から以下のように参照することも出来ます。

```rust
use module::module_c;
```

完全に籍を外れて名前の上では別人ですね。

実装する時の都合とAPIとして公開する時の都合が違うので公開用にいじれる作りになってるんですね。

# クレート内限定公開

クレート内からは自由に参照したいけど外部には公開したくないこともあると思います。そんなときは`pub(範囲)` が使えます。
一番よく使うのは`pub(crate)` でしょう。クレート内全体からはアクセスできるけど外部クレートからはアクセスできなくなります。
このように使います。


``` rust
pub(crate) fn name() {
    println!("module_aだよ");
}
```

もうちょっと色々書けるのですがこのくらいにしておきましょう。
# lib.rsとmain.rs
上では`lib.rs`を使って説明しましたが、`main.rs`でも同じことが出来ます。

さて、ここからはスタイルの話ですが、私がRustを書く時は`main.rs`の中に`mod`を書くことはないです。必ず`lib.rs`を作って、そこでライブラリとしてまとめてから`main.rs`で使います。「アプリケーションはアプリケーションを記述するための巨大なDSLとそれを使った小さな実装からなる」という思想ですね。明示的に境界を作ることで自然とAPIを設計出来るのでコードが整理しやすくなります。

# まとめ

* `lib.rs`と`main.rs`が一番エラい。
* `mod` で「モジュールがある」宣言
* `pub mod` で加えて上位のモジュールにも公開
* `crate` と `self`と`super`の予約語
* `pub use` で改名


[公式ドキュメント](https://doc.rust-lang.org/book/ch07-02-modules-and-use-to-control-scope-and-privacy.html)も参考にして下さい
