---
categories: [Rust, Advent Calendar, Advent Calendar 2017]
date: 2017-12-03T23:14:35+09:00
title: "クレートを公開する前に埋めておくべきメタデータ"
---
κeenです。[Rustその2 Advent Calendar 2017](https://qiita.com/advent-calendar/2017/rust-lang-2)3日目の記事です。

クレートを`cargo publish`する前に何すればいいんだっけと毎回なるので備忘録
publishするまでの手順自体は[過去記事](https://keens.github.io/blog/2016/01/31/rustnopakke_jiwocrates_ionitourokusuru/)を参考にして下さい
<!--more-->

# version

上げ忘れないようにしましょう。[`cargo-bump`](https://crates.io/crates/cargo-bump)が便利かもしれません。

# license
独自記法のライセンス表記をします。例えばMITとApache-2.0のデュアルライセンスなら`"MIT OR Apache-2.0"`など。略称は[ここ](https://spdx.org/licenses/)で調べられます。


1つ気をつけないといけないのが依存ライブラリのライセンスに違反しないように気をつけないといけないということです。Rustは本体がそうなのでMIT OR Apache-2.0が多いようですがGPLのものもあるかもしれません。一々調べるのは難しいので[`cargo-license`](https://crates.io/crates/cargo-license)を使うと便利です。

# description

好きに書きましょう

# documentation

最近はdocs.rsがあるので`https://docs.rs/crate-name`が多いでしょうか。
細かくバージョンを指定して`https://docs.rs/crate-name/version/crate-name`としてもいいですが、私はよく上げ忘れるので上のものを使っています。

# readme
概ね `readme = "README.md"` でしょう。最近はcrates.ioでREADMEを展開するようになったので重要です。

README.mdの内容ですが、

* `[![crate-name at crates.io](https://img.shields.io/crates/v/crate-name.svg)](https://crates.io/crates/crate-name)`
* `[![crate-name at docs.rs](https://docs.rs/crate-name/badge.svg)](https://docs.rs/crate-name)`

などのバッジを付けると親切でしょう。

# repository
大抵githubのレポジトリです。

# keywords
好きにつけられます。と言われても困るのですが、依存ライブラリのキーワードなどを見て決めましょう。
[昨日紹介した](https://keens.github.io/blog/2017/12/02/ironnoresuponsunonakamiwonozokimiruraiburariwozatsunitsukutta/) [`iron_inspect`](https://crates.io/crates/iron_inspect) は `["iron", "log", "error", "debug"]`というキーワードを付けています。

# categories
個人的に難敵です。[ここ](https://crates.io/categories)にあるものから選ぶのですが、表示名と実際に書く文字列が異なります。"Web programming"なら`["web-programming"]`です。小文字にしてスペースをハイフンにした感じですね。
変なものを書くと`cargo package`の時点で怒られます。


---

書ける情報はもうちょいあるのですが(badgeとか)ひとまずこれを埋めとけば大丈夫そうです。


ということで小ネタでした
