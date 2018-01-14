---
categories: [Rust]
date: 2018-01-05T22:20:30+09:00
title: "Rustに何が欲しいだろう"
---
κeenです。[New Year's Rust: A Call for Community Blogposts](https://blog.rust-lang.org/2018/01/03/new-years-rust-a-call-for-community-blogposts.html)の一環のつもりです。
恐らく英語の記事が望まれてるんだろうなと思いつつも試しに日本から声を上げてみます。
<!--more-->
私はRustは1.0前後の頃から触っていて、ドキュメントの翻訳をしたりここ1年くらいは[Idein Inc.](https://idein.jp/)での業務でも使っています。

1年ほど使ってみて何が足りないか考えてみます。

# 言語
## Associated HKT
本当は一般のHKTがあると便利ですがひとまずはAssociatedなものが欲しいです。RFCでいうとこれです [Generic associated types (associated type constructors) ](https://github.com/rust-lang/rfcs/pull/1598)。
欲しい状況は1つにはRFCと似たようなものなので割愛します。

GeneralなHigher Kinded Typesが欲しいのは[transaction-rs](https://crates.io/crates/transaction)の[解説](https://keens.github.io/blog/2017/06/06/rustnotoranzakushonchuushoukaraiburaritsukutta/)でも説明したように、返り値を`Result<T, E>`と`BoxFut<T, E>`どちらでもいいように抽象化したいケースなどです。

## &[u8]と[u8;32]の相互運用
32要素を手で詰め替えるのでつらいです。例えば[このコード]((https://github.com/diesel-rs/diesel/blob/master/diesel/src/pg/types/network_address.rs#L84-L87))とか。

これは[Add TryFrom and TryInto traits](https://github.com/rust-lang/rfcs/pull/1542)が入れば解決します。

上記で「できない」はなくなりますが、例えば`[u8;32]`->`[u8;16]`のように切り詰めたいケースで少し不便です。一旦`&[u8]`を経由しないといけないからです。
しかしこれを直接変換できるようにするとそれはそれで困難が付いてきます。`[u8; n] -> [u8; m] where m <= n`の制約が必要になって[Const Generics](https://github.com/rust-lang/rfcs/blob/master/text/2000-const-generics.md)でもまだ機能が足りないですね。難しい。

## ライフタイムとTCOの保証
簡潔にいうと、一部でいいのでTail Call(Recursion) Optimizationを保証して欲しいです。今は最適化を有効にするとrustcは事実上TCOをしますが言語的に保証はしていなかったかと思います。
ループで書けばいいじゃんという気もしますが [末尾再帰をループにできないRustプログラムの例](http://qnighy.hatenablog.com/entry/2017/05/06/070000) にあるようにライフタイムの関係でループだとダメなケースもあるわけです。
`#[inline]`アトリビュートのように`#[tco]`アトリビュートがあると良いなーという気持ちです。


## Zero Cost New Type
New Type Pattern, つまり何かの型`T`を`struct S(T);`のようにunary structで包むパターンがあります。
これをあらゆるケースでゼロコストにしたいです。
例えば以下のようなケースでコピーコストが発生しますが、本当はcoerceで済むはずです。

``` rust
#![feature(test)]
extern crate test;
use test::Bencher;

#[derive(Clone)]
struct Wrap(i32);

#[bench]
fn no_op(b: &mut Bencher) {
    b.iter(|| vec![1; 1024 * 1024].into_iter().collect::<Vec<i32>>());
}

#[bench]
fn rewrap(b: &mut Bencher) {
    b.iter(|| {
        vec![1; 1024 * 1024]
            .into_iter()
            .map(Wrap)
            .collect::<Vec<_>>()
    });
}
```

``` console
$ rustc --version
rustc 1.23.0 (766bd11c8 2018-01-01)
$ rustc +nightly -Copt-level=3 --test nop_optimize.rs
$ ./nop_optimize --bench

running 2 tests
test no_op  ... bench:     794,861 ns/iter (+/- 36,187)
test rewrap ... bench:   1,800,202 ns/iter (+/- 136,487)

test result: ok. 0 passed; 0 failed; 0 ignored; 2 measured; 0 filtered out
```

# ツール

書いてて思ったのですがやっぱりマクロ周りのサポートが欲しいですね。


## インタラクティブ環境
一つには[rusti](https://github.com/murarth/rusti)のようなREPLが欲しいです。
私はLisperなのでどうしてもREPLで動作確認してからコードを書きたくなります。

もう一つにはGHCiのように「この値の型」「この型が実装しているトレイト一覧」のようなコンパイラが知っている情報を引き出したいです。
Rustはマクロがあるので知らないコードが生えてきがちです。コードだけでは全貌がわからないのでマクロまで解析したコンパイラから情報が引き出せると便利です。

## rustfmt, clippy, rlsなどのstable化
みなさんがどう使っているのか知りたいというのも含めて、開発ツールがnightlyでしか動かないのがちょっと困ります。
業務でRustを書いているのでstableで動くツールのみ使っています。
rustfmtの旧版を使っていますし、人が増えたときのことも考えてclippyも使いたいのですがそれにはnightlyを要求されます。
コンパイラ内部のASTを利用しているのでstable化しづらいのは分かりますがどうにか頑張ってほしいところ。

## rustfmtのマクロサポート
stable版のrustfmtを使っているのでもしかしたら最新のrustfmtではもう対応されているのかもしれませんが、マクロ内のRustコードが整形されません。
マクロ引数内ではシンタックス情報が取れないのは分かりますが`expr`だけでも整形してくれると嬉しいなと。

## rustcに速くなって欲しい
[diesel](https://crates.io/crates/diesel)を使っているとDBスキーマから大量のコードが生成されます。
そして型安全にするためにトレイトを駆使した型付けがされてます。
これが結構コンパイルに時間が掛かります。
`cargo check`で90%の問題は解決してるのですがCIでのビルドなどのようにどうにもならないケースがあるので可能なら速くしたいです。

# コミュニティ
## 公式Webなどが翻訳の差分が追いづらい
最近何もしてなくてあまり言えた口じゃないんですが、[rust-www](https://github.com/rust-lang/rust-www)の翻訳の話です。無造作に更新されるので翻訳者はどこまで翻訳してどこを更新したかが非常に追いづらいです。
[git localize](https://gitlocalize.com/)などのツールを導入してほしいなと思います。

あと地味に[Rust is Universal](https://www.rust-lang.org/en-US/contribute-translations.html)が永遠に執筆されないのは構造的問題なのではと思っています。
英語でドキュメントを書けてかつ他言語への翻訳もやっている人は少ないと思うので。

# 終わりに
「今年の抱負」のような大きな目標ではなく細々した内容が多かったですね。
概ねRust Teamに信頼を置いているというか大きな方針には不満がないのでこういう小手先の話が多くなりました。
