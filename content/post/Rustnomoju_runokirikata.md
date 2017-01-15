---
categories: [Rust]
date: 2017-01-15T20:52:14+09:00
title: Rustのモジュールの使い方
---

κeenです。たまにRustのモジュールが分かりづらいとの声を聞くので解説しますね。

<!--more-->
とりあえずサンプルプロジェクトを作ります。

```
$ cargo new module
```

# エラいのは `lib.rs` と `main.rs`
モジュールの話をする前にクレートの話をしないといけません。
クレートはRustのプログラムの一番大きな単位です。
大きく分けてライブラリを作るlibクレートと実行可能ファイルを作るbinクレートがあります。

libクレートならば`lib.rs`が、binクレートならば`main.rs`がルートにあたるファイルです。ここから辿れるモジュールが全てです。プロジェクトにファイルが存在していてもこれらから辿れなかったらコンパイルされません。

# モジュールの作り方は3種類

1. ファイルの中にインラインで作る
2. 1ファイルで作る
3. 1ディレクトリで作る

のやり方があります。 1.はそんなに使われなくて、説明のために1ファイルにまとめたい時や、可視性でアレコレする時のテクニックなんかに使われます。

2.と3.の境界はサブモジュールを持つかどうかです。

2.のファイルで作る時は

```
$ find src
src
src/lib.rs
src/new_module.rs
```

のように作っておいて、lib.rsの中を

``` rust
mod new_module;
```

のように`mod`で参照してあげれば使えます。

3.のディレクトリの時もそんなに変わらなくて、`lib.rs`は変わらず、ファイルを

```
$ find src
src
src/lib.rs
src/new_module
src/new_module/mod.rs
```

のようにしてあげます。`mod.rs`の名前はこれでないといけません。丁度サブモジュールの`new_module`にとっての`lib.rs`のような存在が`mod.rs`な訳です。

# 兄弟モジュール同士の可視性

## モジュールの身分
さて、次のように2つのモジュールを作ってみましょう。lib.rsには`module_a`のみ`mod`宣言しておきます。

```
$ find src
src
src/lib.rs
src/module_a.rs
src/module_b.rs
$ cat src/lib.rs
mod module_a;
```

これで`module_a.rs`に

``` rust
mod module_b;
```

と書くと

```
$ cargo build
   Compiling module v0.1.0 (file:///home/kim/Rust/module)
error[E0432]: unresolved import `module_a`
 --> src/lib.rs:1:5
  |
1 | use module_a;
  |     ^^^^^^^^ no `module_a` in the root

error: aborting due to previous error

error: Could not compile `module`.

To learn more, run the command again with --verbose.
```

と怒られます。一番エラいのは`lib.rs`ですから、身分の低い `module_a`では`mod`宣言出来ません。

## 君の名は

同じ設定で、`lib.rs`に`module_a`と`module_b`両方を`mod`宣言しておきます。

```
$ find src
src
src/lib.rs
src/module_a.rs
src/module_b.rs
$ cat src/lib.rs
mod module_a;
mod module_b;
```

この時、`module_b`が`lib.rs`から見た時と`module_a.rs`から見た時で名前が変わります。

lib.rsから見た時は、以下の3つが使えます。

```
use ::module_a;     // 絶対パス
use module_a;       // 省略絶対パス
use self::module_a; // 相対パス
```

`self`というのは予約語で、ディレクトリでいうところの`.`に相当します。

module_a.rsからは同じく3つが使えますが、相対パスが変わります。

```
use ::module_a;     // 絶対パス
use module_a;       // 省略絶対パス
use super::module_a; // 相対パス
```

`super`も予約語で、`..`に相当します。
ファイルが同じディレクトリにいるので分かりづらいですが、libが1つ上の階層で、その下にmodule_aとmodule_bがぶら下がってる感じですね。

# おじさんと隠し子
次のように、サブモジュールを作ってみます。

```
$src
src/lib.rs
src/module_a
src/module_a/mod.rs
src/module_a/submodule.rs
src/module_b.rs
$ cat src/lib.rs
mod module_a;
mod module_b;
$ cat src/module_a/mod.rs
mod submodule;
```

さて、この時`sumbmodule`から見た`module_b`の名前はどうなるでしょうか。もうお分かりかと思いますが、以下の3つです。

``` rust
use ::module_a;     // 絶対パス
use module_a;       // 省略絶対パス
use super::super::module_a; // 相対パス
```

じゃ、逆に`module_b`から見た`submodule`はどうなるでしょうか。実は、`module_b`から`submodule`は見えません（lib.rsからも見えません）。`module_a`1つで所帯を持っているので子である`submodule`を外に出すかは`module_a`の一存で決まります。今回は隠し子にしている訳ですね。いくら兄弟モジュールとはいえ家庭にまでは入り込めないのです。

子供をちゃんと公開するには

```
$ cat src/module_a/mod.rs
mod submodule;
```

となっていたモジュール宣言を

```
$ cat src/module_a/mod.rs
pub mod submodule;
```

としてあげればOKです。そうすると`module_b`からも見えて、以下の3種類で参照出来ます。

```rust
use ::module_a::submodule;
use module_a::submodule;
use super::module_a::submodule;
```

# 木箱の外から
さて、今までは1クレート内での話でした。クレートの外から見るとどうなるでしょうか。

新たに`main.rs`を追加しましょう。先程 `lib.rs`と`main.rs`がエラいと話しましたが、どちらも別々のクレートを作るので`main.rs`は完全にクレートの外です。

こんな感じですね。

```
$ find src
src
src/lib.rs
src/main.rs
src/module_a
src/module_a/mod.rs
src/module_a/submodule.rs
src/module_b.rs
$ cat src/lib.rs
mod module_a;
mod module_b;
$ cat src/module_a/mod.rs
pub mod submodule;
```

まず、外部のクレートを参照するには`extern crate`を書く必要があります。あとまあ、binクレートなので`main`関数も必要ですね。

```
$ cat src/main.rs
extern carte module;
fn main() {}
```

では、`module_b`はどういう名前で見えるでしょうか。見えません。一番エラい`lib.rs`が`module_b`を`pub`にしていないのでクレートの外からは見えなくなっています。見たければ、

```
$ cat src/lib.rs
mod module_a;
pub mod module_b;
```

のようにします。

さて、これでmain.rsからどのように見えるかというと。

```
use ::module::module_b;
use module::module_b;
use self::module::module_b;
```

のように気持的にルート直下にクレートがモジュールとして配置されたように見えます。

# 家出
さて、同じように`lib.rs`で`module_a`を`pub mod`すれば`module::module_a::submodule`も参照出来るようになります。ところで、例えば`module_a`は外に出さずに`submodule`だけを公開したい時にはどうしたらいいでしょうか。そんなケースあるのかと疑問に思うかもしれませんが、あります。同じクレートであっても`pub`にしないとサブモジュールにアクセス出来ないことを思い出して下さい。そうすると

``` rust
pub mod submodule;
pub mod internal_submodule_a;
pub mod internal_submodule_b;
```

みたいなケースが発生するのは想像出来ると思います。そういう時は、`lib.rs`側でどうにかいじれます。

```
$ cat src/lib.rs
mod module_a;
pub mod module_b;
```

となっていると思いますが、

```rust
$ cat src/lib.rs
mod module_a;
pub mod module_b;

pub use module_a::submodule;
```

と、`pub use` を加えてあげることで`module_a`の下から出すことが出来ます。

これを`main.rs`から使う時は

```rust
use module::submodule;
```

のように名字の`module_a`が取れます。

さらに踏み込めば、`use`は`as`を使ってリネーム出来るので

```rust
$ cat src/lib.rs
mod module_a;
pub mod module_b;

pub use module_a::submodule as module_c;
```

のようにリネーム`use`してあげれば

```rust
use module::module_c;
```

のように参照することも出来ます。完全に籍を外れて名前の上では別人ですね。

実装する時の都合とAPIとして公開する時の都合が違うので公開用にいじれる作りになってるんですね。

# lib.rsとmain.rs
上では`lib.rs`を使って説明しましたが、`main.rs`でも同じことが出来ます。

さて、ここからはスタイルの話ですが、私がRustを書く時は`main.rs`の中に`mod`を書くことはないです。必ず`lib.rs`を作って、そこでライブラリとしてまとめてから`main.rs`で使います。「アプリケーションはアプリケーションを記述するための巨大なDSLとそれを使った小さな実装からなる」という思想ですね。明示的に境界を作ることで自然とAPIを設計出来るのでコードが整理しやすくなります。

# まとめ

* `lib.rs`と`main.rs`が一番エラい。
* `mod` で「モジュールがある」宣言
* `pub mod` で加えて上位のモジュールにも公開
* `self`と`super`の予約語
* `pub use` で改名

力尽きてテストのためのモジュールの話が出来なかったので[ドキュメント](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/testing.html#tests-%E3%83%A2%E3%82%B8%E3%83%A5%E3%83%BC%E3%83%AB)を読んでみて下さい。

因みに[これ](https://github.com/BurntSushi/fst/blob/master/src/lib.rs)のようにテクニックが詰まった`lib.rs`ファイルなんかも存在するので参考にどうぞ。
