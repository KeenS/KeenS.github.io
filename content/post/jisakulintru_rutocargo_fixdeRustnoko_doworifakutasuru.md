---
categories: [Rust, Advent Calendar, Rust Advent Calendar, Advent Calendar 2019]
date: 2019-12-23T19:56:13+09:00
title: "自作lintルールとcargo fixでRustのコードをリファクタする"
---
このエントリは[Rustその2 Advent Calendar 2019 - Qiita](https://qiita.com/advent-calendar/2019/rust2) 25日目の記事です。

κeenです。私は既に冬休みに突入してます。Happy Holiday！
「Rustで簡単なコードの書き換えを自動化したいよなー」と思っていたら `cargo fix` でできるよという啓示を貰ったのでやってみました。

<!--more-->

↓ 啓示。リプライ元は私じゃないですけどね :)

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">write a lint plugin that uses the diagnostics API, then just use cargo fix</p>&mdash; Manish (@ManishEarth) <a href="https://twitter.com/ManishEarth/status/1205262664131178497?ref_src=twsrc%5Etfw">December 12, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

なるほど。Lintプラグインとcargo fixを使えばいいようです。

# コンパイラプラグイン
unstable featureですがRustにはコンパイラプラグインの仕組みがあります。

[plugin - The Rust Unstable Book](https://doc.rust-lang.org/unstable-book/language-features/plugin.html)

Rustのコンパイル中に動くコードを自分で作れるんですね。
proc_macroで代替される予定っぽいのでdeprecated扱いですが一応現時点ではまだ動きます。

## Lintプラグイン

このプラグインの一種にLintプラグインがあります。
よくRustコンパイラやClippyが「この書き方よくないよ」とか「これこう書けるよ」とか出してくれるやつをユーザが書ける訳です。
上の啓示で触れられているLintプラグインはこれのことです。

# cargo fix

恐らく多くの方がご存知かと思いますが、コンパイラ（や実験的にClippy）が出すLintを自動で適用してくれるツールとしてcargo fixがあります。

[rust-lang/rustfix: Automatically apply the suggestions made by rustc](https://github.com/rust-lang/rustfix)

それ以外にもEditionの移行にも使われているので2018 Editionに移行するときにお世話になった方も多いと思います。

これらを合わせるとコンパイラのLintプラグインを書けばcargo fixで自動リファクタリングができるようになる訳です。
ちょっと面白そうなのでやってみましょう。


# Lint自作入門

先程のコンパイラプラグインの所のドキュメントに大体書いてあります。
Lintがどういう仕組みで動いているかはこちらのブログが参考になりました。

[ Rust 公式 linter の clippy に新しいルールを実装した - はやくプログラムになりたい](https://rhysd.hatenablog.com/entry/2019/02/04/205712)

Clippyの話ですがClippy自身もLintプラグインの仕組みを使っているのでだいたい共通します。

軽くまとめると

* コンパイラの内部APIにアクセスしてLintを出す
  + そのためコンパイラはnightly必須
  + ついでにrustc-devコンポーネントも必要
* Rustc内部で使われているLintの仕組みがあるのでそのまま乗っかる
  + `EarlyLintPass` または `LateLintPass` を実装してregisterしてあげるだけ

となっております。
`EarlyLintPass` はコンパイラ内部でASTと呼ばれるフェーズでのLinterで、ほぼソースコードの見た目を構造体にしたものにアクセスできます。 `LateLintPass` はその次のHIRと呼ばれるフェーズでのLinterで、マクロ展開してたり型情報にアクセスできたりしてます。

さて、今回の目的はコードの自動修正です。
Lintにはいくつか種類があるようなので自動修正できるLintを出しましょう。

細々した詳細は省いて元のpluginのドキュメントで以下のように書かれたLintルールがあります。

```rust
declare_lint!(TEST_LINT, Warn, "Warn about items named 'lintme'");

declare_lint_pass!(Pass => [TEST_LINT]);

impl EarlyLintPass for Pass {
    // アイテムに対するLinterを作る
    fn check_item(&mut self, cx: &EarlyContext, it: &ast::Item) {
        // もしアイテムの名前が "lintme"だったら
        if it.ident.name.as_str() == "lintme" {
            //lintを出す
            cx.span_lint(TEST_LINT, it.span, "item is named 'lintme'");
        }
    }
}
```

これは `lintme` と名付けられたアイテム -- 関数定義やモジュール定義 -- を検出するものです。
これを 「`lintme` と名付けられたアイテムがあったら `lintyou` と改名するように提案する」というルールに変えてみましょう。関数定義部分だけ変更していてそれを呼び出す部分は変更しないのでそのままやるとコードが壊れますが、実験的コードなのでまあいいでしょう。

先に書き換えたコードを見せるとこうなっています。


```rust
impl EarlyLintPass for Pass {
    fn check_item(&mut self, cx: &EarlyContext, it: &ast::Item) {
        if it.ident.name.as_str() == "lintme" {
            // "lintme" を見付けるまでは同じ

            // lintメッセージも同じだが、 `struct_span_lint` を使っている。さらに情報を足せる
            let mut diag = cx.struct_span_lint(TEST_LINT, it.span, "item is named 'lintme'");
            let span = it.ident.span;
            // メッセージに提案を含める
            diag.span_suggestion(
                // 変更したい部分
                span,
                // 提案のメッセージ
                "you can rename it",
                // 提案内容。これがそのまま適用される
                "lintyou".into(),
                // この提案はツールでどう扱うか。MachineApplicableは一番強くて、cargo fixで自動適用される
                Applicability::MachineApplicable,
            )
            .emit()
        }
    }
}
```


メッセージに追加情報で提案を載せて、機械で扱えるようにします。
変更するときのAPIは[ここ](https://doc.rust-lang.org/nightly/nightly-rustc/rustc/lint/struct.LateContext.html)とかから辿っていきましょう。APIが多いのである程度エスパー能力が求められます。


さて、これをコパイラプラグインとしてビルドし、既存のソースコードに変更を適用してみましょう。

コンパイラプラグインとしてビルドするには以下のようにCargo.tomlの `lib` セクションに `plugin = true` をつけるだけです。


```toml
[package]
name = "minimal-lint"
# ...

[lib]
plugin = true

```

ビルドするときはnightlyコンパイラが必要ですし、rustc-devコンポーネントも必要なので  `rustup component add rustc-dev` を忘れないようにしましょう。

そしてこのルールを適用したいソースコード側でプラグインとしてロードします。
これは普通の依存クレートとして書いてあげます。

```toml
[dependencies]
minimal-lint = { path = "/path/to/minimal-lint" }
```

そしてソースコードの `lib.rs` なり `main.rs` なりでプラグインを有効にします。


```rust
#![feature(plugin)]
#![plugin(minimal_lint)]
```

試しに`lintme` という名前の関数を定義してみましょう。

```rust
fn lintme() {}
```

これをビルドすると警告とともに提案が出ます。

```text
warning: item is named 'lintme'
 --> tests/lintme.rs:6:1
  |
6 | fn lintme() {}
  | ^^^------^^^^^
  |    |
  |    help: you can rename it: `lintyou`
  |
  = note: `#[warn(test_lint)]` on by default

    Finished test [unoptimized + debuginfo] target(s) in 0.72s
     Running target/debug/deps/minimal_lint-6460443341f748b2
```


`cargo fix` で修正してみましょう。

```text
$ cargo fix
$ cat source.rs
#![feature(plugin)]
#![plugin(minimal_lint)]

#[allow(dead_code)]
fn lintyou() {}
// ^^^^^^^
// 変更されている
```

修正されていますね。

今回試したコードはこちらに置いています。良かったら参考にして下さい。

[KeenS/minimal-lint](https://github.com/KeenS/minimal-lint)


# 実践Lint & Fix
「自作Lintルールを書いてcargo fixでリファクタする」だと長いので「Lint & Fix」と呼んでいきましょう。
もう少し大きなプロジェクトでLint & Fixしてみます。

超個人的な話なのですが、そもそもこれをやろうとしたモチベーションとなるルールがあります。
趣味で[コンパイラ](https://github.com/KeenS/webml)を書いているのですが、そこによく `Box::new(expr)` というコードが出てきます。 `expr` というのはコンパイラのコードに出てくる `Expr` という名前のデータ型の式です。
こういうコードを書く度に、式を書いたあとに先頭に戻って `Box::new()` で包むのが面倒と感じていました。
そこで `Expr::boxed()` というメソッドを生やして `expr.boxed()` と書けるようにしました。
書けるようにしたはいいのですが今度は既存のコードを修正して回るのが面倒です。
なので自動でやりたいなというのが今回のモチベーションです。

要約すると、以下の書き換えをします。

```text
Box::new(expr: Expr) => expr.boxed()
```

ルールとしてはシンプルですね。これを実装していきましょう。

## `LateLintPass`

`Expr` _型_ の式を `Box::new` で包んでいる部分をみつけたい訳です。
ここに型情報が登場したので `EarlyLintPass` ではダメで、 `LateLintPass` が必要になります。

つまり書き出しはこうなります。


```rust
impl<'a, 'tcx> LateLintPass<'a, 'tcx> for Pass {
    // ...
}
```

ライフタイムパラメータが増えた以外は変わらないですね

## `check_expr`

最初の例ではアイテムのLintだったので `check_item` を使いましたが式をLintしたいので式のList関数をオーバーライドします。こんな感じ

```rust
impl<'a, 'tcx> LateLintPass<'a, 'tcx> for Pass {
    fn check_expr(&mut self, cx: &LateContext<'a, 'tcx>, expr: &'tcx hir::Expr) {
        // ...
    }
}
```

ここまでは順調ですね。

## `Box::new` にマッチする
これは中々大変です。
複雑に入り組んだデータ型から目的に合うものを捜します。
[ExprKind](https://doc.rust-lang.org/nightly/nightly-rustc/rustc/hir/enum.ExprKind.html)を眺めてどういう表現になっているかエスパーしましょう。

いくつか絞り込み条件を並べてみます。

* 関数呼び出しである ( `Box::new()` )
* 引数は1つである
* 関数はパス形式 ( `Box::new` ) である
* パス名は `"std::boxed::Box::<T>::new"` である
* 唯一の引数の型のパスは `"ast::Expr"` である

これを丁寧にやっていきます。

```rust
// find Box::new(expr: hir::Expr)
let box_span = expr.span;
if let hir::ExprKind::Call(fun, args) = &expr.kind {
    if args.len() < 1 {
        return;
    }
    let fun_hir_id = fun.hir_id;
    if let hir::ExprKind::Path(fun) = &fun.kind {
        let res = cx.tables.qpath_res(fun, fun_hir_id);
        if let Some(defid) = res.opt_def_id() {
            let funpath = cx.tcx.def_path_str(defid);
            if funpath.as_str() == "std::boxed::Box::<T>::new" {
                let expr = &args[0];
                if let TyKind::Adt(def, _) = cx.tables.expr_ty(expr).kind {
                    let name = cx.tcx.def_path_str(def.did);
                    if name.as_str() == "ast::Expr" {
                        // ...
                    }
                }
            }
        }
    }
}
```

ごつい見た目ですね。しかしよく見るとちゃんと絞り込み条件通りにやってるのが見てとれるかと思います。
[if_chain](https://crates.io/crates/if_chain)を使って大変さを軽減するのが常套手段っぽいですが、まだ不慣れでif_chain由来のエラーなのかコード由来のエラーなのか調べるのがきつそうだったので素のまま書きました。
今回で慣れたので次からは `if_chain` 使っていきます。

因みにここでパス名を文字列化して比較しているのが正解かは知りません。
どうせ動けばいいだけのコードなので適当に書いてます。
行儀の良い書き方（同等のASTを組み立てて構造体同士で比較など）があれば教えて下さい。

## 変更を提案する

さて、suggestionを作っていきます。

パッと思いつくのは `Box::new(expr)` のうち、 「`Box::new(`」 を削って 「`)`」 を `.boxed()` に変更する提案をすればよさそうです。
真ん中のexpr部分は箇所によってさまざまですが `Box` の部分は固定文字列なので扱いが簡単そうですからね。
これは変更箇所が複数になるので [`multipart_suggestion`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_errors/diagnostic_builder/struct.DiagnosticBuilder.html#method.multipart_suggestion-1)を使っていたのですが、cargo fixしたときに適用されない問題がありました。
調べるとcargo fixでmultipart suggestionが[未実装](https://github.com/rust-lang/rustfix/issues/141)でした。残念…。


ということで頑張って 「`Box::new(expr)` を `expr.boxed()` に変更する」 ルールを書いていきます。

まずは `expr` からそこに該当する文字列を取り出します。
ソースの文字列は `Session` に入っているようだったのでそこから該当する範囲のスニペッットを取り出します。

```rust
let expr_str = cx.tcx.sess.source_map().span_to_snippet(expr.span).unwrap();
```

これさえあればあとは `format!("{}.boxed()", expr_str)` で済みますね。
残りの部分はこう書けます。


```rust
let mut diag = cx.struct_span_lint(
    EXPR_LINT,
    box_span,
    "using Box::new() is not recommended",
);
diag.span_suggestion(
    box_span,
    "you can use .boxed()",
    format!("{}.boxed()", expr_str),
    Applicability::MachineApplicable,
)
.emit()
```

## 適用する

あとは先程と同じくプラグインとしてロードしてcargo fixしてあげるだけです。
出来上がった差分は[こちら](https://github.com/KeenS/webml/commit/ddbf0949d12715481d6a986d8320477019492c96)にあります。また、Lint & Fixに使ったコードは[こちら](https://github.com/KeenS/webml-lint-refactor) です。

# まとめ

コンパイラプラグインとcargo fixを使うことでLint & Fixができます。
最初に触れた通りコンパイラプラグインの機能はdeprecatedなので今書いたコードが将来動かなくなる可能性はありますが、こういうワンショットのためのツールは文字通り書き捨てなので今の一瞬だけ動けばいいですよね。

もし手元に単純だけど面倒な修正がある方がいたら試してみて下さい。
