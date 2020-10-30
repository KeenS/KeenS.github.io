---
categories: [Rust, RustFest]
date: 2020-10-26T21:24:22+09:00
description: "Rustに入門してはじめてまとまった量のコードを書いたのが自作のLispインタプリタでした。当時の自分にはインタプリタは複雑すぎる題材でしたが、それゆえに多くのことを学びました。その後もシェルやMLコンパイラなどいくつかの言語を作っていきながらRustの機能を学んできました。本講演では言語自作を通して学んできたことをベースに、Rustに入門したての段階でのありがちなトラブルや意外と知られていない機能、Rustコンパイラの進化などを紹介します。Rustの入門体験記は色々ありますが、1つのテーマに沿って何度もプログラムを書いたことで徐々に問題へのアプローチが上手くなっていく点や、言語実装者が新たな言語を学んでいくという側面にも触れられたらなと思います。"
title: "言語自作を通して学んだRust"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# 言語自作を通して学んだRust<!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)" -->
----------------------
[RustFest Global 2020](https://rustfest.global/)

<!-- .slide: class="center" -->


Note:

Good morning, everyone. I'm κeen.
First of all, I'd like to say thank you to RustFest's organizers.
This is my first time to have a talk at an international conference.
Because I'm not good at English, attending international conference is not an easy task.
I guess this is true for most of non native English speakers.
However, in this time, as we have interpreters there was a chance for me. Thank you.
Then, the rest of my talk is in Japanese.

それでははじめていきます。「言語自作を通して学んだRust」というタイトルで発表していきます。

===
# About Me
---------
![κeenのアイコン](/images/kappa2_vest.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * GitHub: [KeenS](https://github.com/KeenS)
 * Engineer at [Idein Inc.](https://idein.jp/)
   + working on Rust job!
 * An author of 実践Rust入門
 * A translator (to Japanese) of the Book 1st edition and (old) Rust's official website

Note:

私はκeenといいます。
TwitterやGitHubのアカウントを持っていて、Ideinという会社で仕事でRustを使っています。
右にある画像がTwitterやGitHubで使っているアイコンです。
日本で出版された実践Rust入門という本の著者の一人です。
またプログラミング言語Rustの初版やRustの公式ウェブサイトの旧版の翻訳に協力したりもしました。

===

# Background
------------

* Have created many (toy) languages
  + [a Scheme implementation](https://github.com/picrin-scheme/picrin) (one of commiter)
  + [Lisp in Scala Types](https://github.com/KeenS/scala-lisp)
  + [Whitespace compiler in Whitespace](https://github.com/KeenS/whitelie)
  + ..and many others
* Love functional languages like ML and Lisp
  + ADTs and pattern matches is suitable for interpreters <!-- .element: style="font-size: 80%" -->
* Like C
  + fast and low level

Note:

さて、Rustに入門する前に私がどういうことをしてきたかというと、おもちゃ言語を色々作ってました。
一例を挙げるとScheme処理系のコミッタをやってたり、Scalaの型でLispを作ったり、WhitespaceでWhitespaceのコンパイラを書いていたりしました。
MLやLispといった関数型プログラミング言語が好きです。
Algebraic data typesやパターンマッチといった機能がインタプリタを書くのに適していたからです。
Cもほどほどに好きです。
速くてローレベルなことができるからです。

===

# Between C and FPL
------------

* FP languages are good for writing interpreters
  + 😊 ADT and pettern matches
  + 😊 readable and safe
  + 😖 have runtimes
* C is low level
  + 😊 fast
  + 😊 rutime-less
  + 😖 unsafe (no more SEGV!)
* I was seeking for a safe, fast, and runtime-free language with ADT and pattern matches

Note:

ただし関数型プログラミング言語もCも一長一短あります。
関数型言語にはADTやパターンマッチがあり、可読性が高く安全である一方ランタイムがあるので速いインタプリタを作るのには向いてません。
逆にCは速くてランタイムを持ちませんが、安全ではありません。
なので安全で速くてランタイムがなくてADTとパターンマッチのある言語を探していました。

===

# Met Rust
----------

* One day I met Rust
* When rust was 0.12.0 (just before 1.0-alpha)
* Seemed an ultimate language to write a language
  + 😊 ADT and pettern matches
  + 😊 Readable and Safe
  + 😊 Fast
  + 😊 Rutime-less
* → Started using Rust for writing languages

Note:

そうしてある日Rustに出会いました。
出会ったのはRust 0.12.0の頃で、1.0-alphaが出る直前でした。
Rustには先程あげた機能が全部揃っていたので言語を作るときはRustを使うようになりました。

===

# Languages Wrote in Rust <!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)" -->
----------------------

* 2015-09: [κLisp](https://github.com/KeenS/kappaLisp) (subset of Emacs Lisp)
* 2016-09: [igaguri](https://github.com/KeenS/igaguri) (Shell)
* 2016-11: [rustlisp](https://github.com/KeenS/rustlisp) (small lisp in Rust Type)
* 2016-12: [WebML](https://github.com/KeenS/webml) (Standard ML compiler)
* 2016-12: [WebAssembler-rs](https://github.com/KeenS/WebAssembler-rs) (in-memory WebAssembly assembler)
* 2017-11: [chema](https://github.com/KeenS/chema) (alt JSON Schema)
* ..and some misc languages

Note:

そうしてRustで作った言語をいくつか挙げるとこんな感じです。
まずはLisp方言。
それから1年後くらいにシェルだとか、また型レベルLispだとか、SMLコンパイラだとかインメモリのWebAssemblyアセンブラだとかを作りました
そしてまた1年後くらいにalt JSON Schemaだとかを作りました。
細かいものを含めたら他にもあるんですがひとまずこんなところです。

===

# In this talk
---------------

* κLisp: Learned basics
* WebML: Maintaining
* chema: Making usable product

Note:

本当はこれらの言語を全部話したかったんですが時間が足りないので3つ選んで、それぞれの言語を作って何を学んだかを紹介します。
選んだ3つがこれで、まずκLispからは基礎を学びました。
次はWebMLで、メンテナンスするということを学びました。
最後がchemaで、使えるものを作るということを学びました。
それではこの三本立てで話していきたいと思います。

===

# κLisp

<!-- .slide: class="center" -->

Note:

まずはκLispです。

===
# What is κLisp
---------------

* Designed to interpret SKK Lisp
  + SKK is a input method for Japanese
  + It's not important in this talk :)
* (Almost) my first Rust project
* I tried to write in 4 days
  + I was confident because I was experienced to write Lisp interpreters

Note:

κLispはSKK Lispを解釈するために作られました。
SKKというのは日本語入力メソッドの名前なんですが、この話にはあまり関係ないので気にしなくてよいです。
ほぼ私の最初のRustのプロジェクトでした。
当初はこれを4日で作ろうと意気込んでました。
4日というのは日本では秋頃に4連休があるのでその間に作りたかった訳です。
Lisp処理系は何度か作ったことがあるので4日あれば作れるだろうと踏んでました。

===
# Basic Items used
------------------

* Writing a language requires basic knowledge of the host language
* Many of basic items are used
* Language featuers:
  + structs, methods
  + enums, pattern matches
  + macros
  + ...
* Standard Libraries
  + iterators
  + HashMap
  + ...

Note:

言語処理系を作るのにはホスト言語の色々な基本機能が必要になります。
使った機能の一例を挙げると、
言語側だと構造体、メソッド、列挙型、パターンマッチ、マクロなどなど。
標準ライブラリだとイテレターやハッシュマップなどです。
こういうった基本機能を駆使しつつインタプリタを書いていきました。


===
# How I failed
--------------

* Failed to complete it in 4 days
* Faced many difficulties
  + Garbage collection (GC)
  + Lifetime / ownership
  + Error handling
* Was taught Rust was a difficult language

Note:

そして、失敗しました。4日では終わりませんでした。
失敗した理由としてはいくつか難しいところがあったからです。
具体的にはガベージコレクション、ライフタイムと所有権、エラーハンドリングあたりです。
ここでRustは難しい言語というのを思い知ることになります。

===
# GC and Box
-------------

* My first attempt of Lisp expression used `Box<T>` s as pointer:

```rust
pub enum Expr {
    Int(isize),
    Cons(Box<Expr>, Box<Expr>),
    // ...
}
```

Note:

GCで躓いたのはこういう部分です。
Lispの式（expression）を `Expr` というenumで表現するのですが、そこに出てくるポインタに `Box` 型を使っていました。
下にコードスニペットがあって `enum Expr` を定義していますね。
そこの2つ目のヴァリアントが `Cons` で、ポインタを2つ保持します。
そのポインタに `Box` を使っていた訳です。


===
# GC and RC
-----------

* Rust doesn't have GC!
  + Unlike FP languages
* Values must be shared!
  + Lisp allows share values
* Correct approach is use `Rc` instead of `Box`:

```rust
pub enum Expr {
    Int(isize),
    Cons(Rc<Expr>, Rc<Expr>),
    // ...
}
```


Note:

何がよくなかったかというとRustにはGCが無い点です。
関数型言語だとGCはあるのでそういうことをすっかり忘れてました。
Rustの `Box` 型だとその値を一人しか使えませんが、今回実装しているLispは値を共有できるので、ダメでした。
正しくは `Box` の代わりに `Rc` を使わないといけません。
下のコードスニペットでは先程の `Expr` とほぼ同じですが、 `Cons` の保持しているポインタが `Rc` になったものになっています。
これが正しい実装です

===
# Lifetime / Ownership
-----------------------

* It was new notion to me (and I guess to most of you)
* Taking ownership at argument makes writing funcions easy, but calling them very hard
* Eventually I noticed overall design is important
  + Like "This data is registered to the runtime and should live to the end of execution, thus you cannot take its ownership"
* (The borrow checker was lexical)
  + `get` then `insert` against `HashMap` was illegal at that time

Note:

次に困ったのがライフタイムと所有権です。
恐らくほとんどの人もそうだと思いますがこれは初めて体験した概念です。
関数を定義するときに引数で所有権を取ってしまうと書くのは楽になりますが、呼ぶのが大変になります。
最終的に全体の設計が大事ということに気付きました。
全体の設計というのは「このデータはランタイムに登録されるから実行が終わるまで生きないといけない、だから所有権を奪ってはいけない」などです。
あと当時はボローチェッカがレキシカルだったというのもあります。
有名な `HashMap` に対して `get` して `insert` しようとするとエラーというのも実際に体験していました。

===
# Error Handling
-----------------

* I had no idea of how to handle errors
* The first code used `panic`s everywhere

```rust
fn k_add_aux(x: &Expr, y: &Expr) -> Expr {
    match (x, y) {
        (&Expr::Int(x), &Expr::Int(y)) => Expr::Int(x + y),
        _ => panic!("non int args {:?} and {:?} are given to +", x, y)
    }
}
```

Note:

困った3つ目がエラーハンドリングです。
正直、どうすればいいか分かりませんでした。
最初のコードは各所で `panic` を使っていました。
下に貼ってあるコードスニペットでは `k_add_aux` という関数を定義していて、関数の返り型は素の `Expr` になっています。
関数本体の方では引数に対してパターンマッチするときに想定していないデータだった場合にパニックしています。

===
# Option
---------

* I started use `Option<T>` when failed to prepare correct values

```rust
fn read_aux(
    mut input: &mut Peekable<Chars>,
    first: char
 ) -> Option<Expr>
{ ... }
```

Note:

次に正しい値を用意できなかったら `Option` を使いはじめました。
下に貼ってあるコードスニペットでは `read_aux` という関数を定義していて、関数の返り型が `Option<Expr>` になっています。

===
# String Errors
---------------

* Then I learned `Result<T, E>` is recommended
* However, all the error was `String`

``` rust
fn k_funcall(
    mut env: &mut Env,
    args: Expr
) -> Result<Expr, String> {
   match args {
     Expr::Cons(f, args) => {
       funcall(env, f.deref(), args.deref().clone())
     },
     args => {
       Err(format!("illeagal form of funcall {:?}", args))
     }
   }
}
```

Note:

次にエラーには `Result` 型を使うと学んだので使いはじめました。
しかしエラーは全て `String` でした。
下に貼ってあるコードスニペットでは `k_funcall` という関数を定義していて、 返り型が `Result<Expr, String>` になっています。
関数本体の方では引数に対してパターンマッチするときに想定していないデータだった場合に `Err(format!())` を返しています。


===
# Custom Errors
----------------

* Finally, I learned the correct way
  + [Error Handling in Rust - Andrew Gallant's Blog](https://blog.burntsushi.net/rust-error-handling/)
* Defined custom erros:

``` rust
pub enum Error {
    InvalidArgument,
    Type,
    ArityShort,
    ArityExceed,
    Form,
    NotFunction,
    Unbound,
    User(String)
}
```

Note:
最後に正しい方法を知りました。
「Error Handling in Rust」というAndrew Gallantさんのブログです。
このブログを読んで独自のエラー型を定義することを覚えました。
下に貼ってあるコードスニペットでは `Error` という名前のenumを定義していてκLispで起きるエラーをヴァリアントで表現しています。

===
# Lessons learned from κLisp <!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)" -->
-----------------

* Many basics of Rust
* Variants of pointers
  + `&`, `Box` and `Rc`
* Ownership / lifetime and design of data lifetimes
* Error handling

Note:

κLispで学んだこととしては、まずはRustの基礎です。
そしてポインタがいくつかあることも学びました。κLispで使ったのは参照、 `Box` 、 `Rc` ですね。
所有権、ライフタイム、そしてデータのライフタイムの設計についても学びました。
最後にエラーハンドリングについても学びました。

===

# WebML

<!-- .slide: class="center" -->

Note:

次はWebMLです。


===


# What is WebML
----------------

* A Standard ML to WebAssembly compiler
  + [WebAssembler-rs](https://github.com/KeenS/WebAssembler-rs) is a side project of this
* The biggest among my hobby projcets
* My "bonsai" project
  + You take care of it constantly when you have times
  + A long term project

<figure style="position:absolute;right:0;z-index:-1">
<img alt="bonsai" src="/images/gengojisakuwotoushitemanandaRust/Japanese_Black_Pine,_1936-2007.jpg"  width="30%"/>
<figcaption style="font-size:10%"><a href="https://commons.wikimedia.org/wiki/File:Japanese_Black_Pine,_1936-2007.jpg">A Japanese Black Pine (Pinus thunbergii) bonsai on display at the National Bonsai & Penjing Museum at the United States National Arboretum. According to the tree's display placard, it has been in training since 1936. It was donated by Yee-sun Wu.</a> 2007 <a href="https://creativecommons.org/licenses/by-sa/4.0/">CC BY-SA 4.0</a></figcaption>
</figure>

Note:

WebMLはStandard MLからWebAssemblyへのコンパイラです。
WebAssembler-rsこれのサイドプロジェクトです。
私の趣味プロジェクトの中で一番規模が大きいです。
そして私の盆栽プロジェクトでもあります。
盆栽プロジェクトというのは、下にある画像が盆栽ですが、時間のあるときにちょくちょく手入れして、ずっと続けていくようなプロジェクトのことです。


===

# Difficulties
--------------

* It's big
  + Better code organization is required
* SML has complex syntax compared to Lisp
  + It's too complicated to write a parser by hand
* Many similar types
  + You'll write many similar functions to treat them

Note:

WebMLを作るにあたって何が難しいかというと、まずは大きいということです。
コードベースが大きいのでコードの管理についても工夫が必要です。
次にSMLの構文がLispと比べて複雑ということです。
Lispと違って手でパーサを書く訳にはいきません。
そしてコンパイラ特有の似たような型が沢山でてくる問題もあります。
他にもそもそもコンパイラは難しいとかあるのですが、それは置いておいてこの3つに焦点を当てて喋っていきます。

===

# Code organization
--------------------

* It's time to use submodules
  + i.e. `directory/mod.rs`
* But it was complex to me...
  + "Why I cannot declare `mod a;`  in `b.rs`?"
  + "Where should I put `mod.rs`?"
* I try-and-errored and finally understood
  + [Rustのモジュールの使い方 | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2017/01/15/rustnomoju_runokirikata/)


Note:
コードの管理ですが、サブモジュールを使うタイミングですね。
サブモジュールというのはつまり `directory/mod.rs` とかです。
ですが、これが複雑でした。
「`b.rs` の中で `mod a;` を宣言したいけどできないんだけど？」
「`mod.rs` ってどこに置けばいいの？」
という具合でした。
色々試行錯誤して最終的には理解できて、ブログにまとめたりもしました。

===

# Parser
--------

* Hand written parsers aren't suitable for SML
* I employed a [parser combinator](https://en.wikipedia.org/wiki/Parser_combinator) library
  + Namely [nom](https://crates.io/crates/nom) (2.0)
* It was macro-full
  + Because `impl Trait` hadn't been arrived yet at that time



``` rust
named!(top < Vec<AST> >, do_parse!(
    opt!(multispace) >>
        tops: separated_list!(multispace, map!(bind, AST::Top)) >>
        opt!(multispace) >>
        (tops)
));
```

Note:

次にパーサについてです。
SMLパーサは手書きするものではないです。
ということでパーサコンビネータライブラリを使いました。nomっていうやつです。当時バージョン2.0でした。
これがマクロまみれでした。というのも当時は `impl Trait` がまだなかったので、パフォーマンスの都合でこうなっていたようです。

下に貼ってあるのがnomを使ったパーサのコードです。 `named!` というマクロの引数の中に `do_parse!` だとか `separated_list!` だとか `opt!` だとかマクロがたくさん書かれてます。
APIを見るとマクロがすごいことになっています。（nom 2.0のAPIドキュメント）

===

# Similar Types
----------------

* Many similar types with slightly different definitions appear
  + `UntypedExpr`
  + `UntypedCoreExpr`
  + `TypedCoreExpr`
* They are relatively large
  + 10+ variants
* I wrote all of them at first

Note:

続いて型が多い問題です。
コンパイラの中には似てるけど微妙に違う型が沢山でてきます。
`UntypedExpr`、`UntypedCoreExpr`、`TypedCoreExpr`などです。
それぞれ比較的大きくて、10個以上のヴァリアントがあります。
最初はこれを全部書いていました。

===
# Type Aliases
---------------

* Employed type aliases

``` rust
enum ExprKind<Ty, DE = DerivedExprKind<Ty>, DS = DerivedDeclaration<Ty>> {/* ... */}
struct Annot<Ty, Inner> {/* ... */}
type Expr<Ty, DE = DerivedExprKind<Ty>, DS = DerivedDeclaration<Ty>> = Annot<Ty, ExprKind<Ty, DE, DS>>;

type UntypedExpr = Expr<Empty>;
type CoreExpr<Ty> = Expr<Ty, Nothing, Nothing>;
type CoreExprKind<Ty> = ExprKind<Ty, Nothing, Nothing>;
type UntypedCoreExpr = CoreExpr<Empty>;
type UntypedCoreExprKind = CoreExprKind<Empty>;
type TypedCoreExpr = CoreExpr<Type>;
type TypedCoreExprKind = CoreExprKind<Type>;
```

Note:

色々試行錯誤するうちに型エイリアスでどうにかできることに気付きました。
下に掲載したコードはちょっとごちゃっとしてますが、型エイリアスを使った解決です。
型パラメータで振る舞いを変える `Expr` という型を用意しておいて、望ましい振る舞いをする型パラメータを与えたものをそれぞれ `UntypedExpr` とか `TypedCoreExpr` とかのエイリアスを定義しています。

===
# Alias and Methods
-------------------

* Type aliases are flexible compared to other languages


``` rust
impl<Ty> CoreExpr<Ty> { /* ... */ }
impl<Ty, DE, DS> fmt::Display for Expr<Ty, DE, DS>
where
    Ty: fmt::Display,
    DE: fmt::Display,
    DS: fmt::Display,
{ /* ... */ }

```

Note:

Rustの型エイリアスはかなり柔軟なのでエイリアスにしてしまえます。
例えば `CoreExpr` は `Expr` のエイリアスであり、かつ `UntypedCoreExpr` と `TypedCoreExpr` へとエイリアスされます。
`UntypedCoreExpr` と `TypedCoreExpr` 両方に同じメソッドを定義したかったら `CoreExpr<Ty>` に `impl` してあげればい訳です。
また、 `Display` のように `Expr` 属全てに実装したいトレイトは型パラメータを全てジェネリクスにすればよいのです。

===
# Lessons learned from WebML <!-- .element: style="font-size: calc(var(--title-font-size) * 0.7)" -->
-------------------

* The experience of writing interpreters helps writing compilers
  + Defining data types desiging lifetimes were straightforward
* Rust's module system is complex
* Debugging macro-full code is a tough task
* We need `impl Trait`
* Rust's type system is awesome

Note:

WebMLから学んだこととしては、まずはインタプリタを作った経験がコンパイラを作るときにも生きたという点です。
内部で使うデータ型とかライフタイムの設計とかで躓くことはありませんでした。
そして、紹介したとおりモジュールシステムが複雑だったり、マクロを使って書かれたコードのデバッグがつらいということだったり `impl Trait` が早く欲しいだとかを学びました。
他にもRustの型システムがよくできているというのも学びました。

===

# By the way
------------

* I changed my job at that time
* I started writing Rust in daily work
* My Rust skill advanced blazingly

Note:

ちょっと話が脇道に逸れるんですが、ここで今の職場に転職しました。
そうして普段の仕事でRustを使うようになって、Rust力がメキメキ上がっていきました。
そういう状態で次のchemaに移ります。


===

# chema

<!-- .slide: class="center" -->

Note:

ということでchemaです。発音はSchemaのSを取ったものです。


===
# What is chema
--------

* A orginal notation to [JSON Schema](https://json-schema.org) compiler
  + i.e. an alt JSON Schema
* A tool used in company's project
  + → should be usable one

```text
/** @title User */
type user = struct {
    /** unique id of the user */
    id: id,
    name: string?,
    type: enum {"admin", "writer", "reader"},
    SNSs: [string],
};
```

Note:
chemaが何かというと独自記法からJSON Schemaへのコンパイラです。
要するにalt JSON Schemaとでも呼ぶべきものです。
これは会社のプロジェクトで使うために作ったので「使える」ものじゃないといけません。
下に貼ってあるのがchemaの記述例です。
`/** ~ */` でコメントをはじめたり、 `type name =` で型を定義したりします。


===
# What is needed
-----------------

* The compiler
  + It's no problem to me at that time :)
  + → It helped tackling other difficulties
* Command line interface
* Binary releases

Note:

chemaに求められるものなんですが、まずはコンパイラ部分です。
この時点ではもう問題なく作れるようになってました。
なので他の問題に集中することができました。
WebMLで困っている部分をここで試してWebMLに持ち帰ったりもしました。
そして他にはコマンドラインインタフェースとバイナリのリリースが必要でした

===

# CLI
-----

* A Parser of command line arguments is needed
* → Employed [structopt](https://crates.io/crates/structopt)

``` rust
#[derive(StructOpt)]
pub struct Config {
    #[structopt(long = "no-swagger", help = "don't use swagger spesific notation")]
    pub no_swagger: bool,
    #[structopt(help = "input file")]
    pub input: String,
}
```

Note:

同僚に使ってもらわないといけないので、コマンドラインインタフェースをリッチにしないといけません。
という訳でコマンドライン引数のパーサが必要でした。
そこで `structopt` を採用しました。
`structopt` の利用例を下に貼りました。
構造体定義に `#[derive(StructOpt)]` をつけて、それぞれのフィールドの定義に `#[structopt()]` のアトリビュートをつけます。アトリビュートの中には `long` や `help` などが書けます。
`long` は長いオプション、ここでは `--no-swagger` って書くオプションを指定しています。
`help` は `--help` で見れるヘルプメッセージですね。

===
# CLI examples
--------------

``` text
$ chema --help
chema 0.0.8
Sunrin SHIMURA (keen) <3han5chou7@gmail.com>
An external DSL for JSON Schema

USAGE:
    chema [FLAGS] [OPTIONS] <input>

FLAGS:
    -h, --help          Prints help information
        --no-swagger    don't use swagger spesific notation
        --pack          if pack the output
    -V, --version       Prints version information

OPTIONS:
        --format <format>              output format (json|yaml) [default: json]
        --path-prefix <path_prefix>    path prefix of paths [default: /definitions]

ARGS:
    <input>    input file
```

Note:

これがstructoptで生成したヘルプメッセージです。
非常によくできてますね。


===
# Binary releases
-----------------

* Build distributions on local machine and uploading them is a hard task
  + And it have problems arount cross-compiling
* Releasing from CI is desired
* → Introduced [trust](https://github.com/japaric/trust)
  + A template configuration of Travis CI
  + It is able to create binary release for many architectures

Note:

次がバイナリリリースです。
手元のマシンで配布物を作ってGitHubにアップロードするのは手間ですし、クロスコンパイルの問題もあります。
CIでリリースできるならそれに越したことはありません。
そういう訳でtrustを導入しました。
trustはTravis CIの設定の雛形で、これを使うと複数ターゲットのバイナリを簡単に作れます。
chemaのリリースをちょっと見てみましょう（chemaのリリースページ）
こういう風に色んなプラットフォーム向けのリリースを簡単に作れる訳です。

===
# Lessons learned from chema <!-- .element: style="font-size: calc(var(--title-font-size) * 0.7)" -->
-----------------

* Creating a "usable" product requires additional labor
* There are many resources available that helps such labor

Note:

chemaから学んだこととしては「使える」ものを作るには一手間必要である点、そしてその一手間を助けてくれるものが既に揃っている点です。

===
# Looking back
---------------

* I growed through writing languages
  + The first attempt was all about the language
  + Then overall code
  + Then while project
* Some of difficulties have resolved by updates of Rust
  + `impl Trait` (1.26.0)
  + module system (2018 edition)
  + NLL (2018 edition)

Note:
まとめに入る前に少し振り返ってみましょう。
私は言語を色々書きながら成長していきました。
最初はRustでどうやって書くかでいっぱいっぱいでしたが、コード全体をどうすればいいか、そしてプロジェクト全体としてどうリリースするかなどまで気が回るようになりました。
もう1つ気付いた方もいるかと思いますが、私が遭遇した壁のいくつかは既にRustのアップデートで解決されています。
`impl Trait` は今でも覚えてますが1.26.0で入りました。
複雑なモジュールシステムや融通の効かないライフタイムは2018 editionで解決しました。

===
# Conclusion
-------------

* Writing a small Lisp interpreter tells you many thing about Rust
* You can gradually know details by tackling one theme repeatedly
  + Because you already have know the overview, you can concentrate other details
* Rust is an evolving language

Note:
ということでまとめに入ると一度Lispのインタプリタを書いてみるとRustのかなりの部分を理解できるようになります。
そして同じテーマの問題、私の場合は言語ですが、に繰り返し取り組むただんだんと細部まで理解できるようになります。
というのも既に概観は理解できているので詳細にまで踏み込めるからです。
最後にRustは進化しつづけている言語ということでした。
ありがとうございました。

</script>
</section>
