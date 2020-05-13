---
categories: [Rust]
date: 2020-04-27T23:38:53+09:00
description: "下町.rsでの発表用。rust-analyzerについて。"
title: "rust-analyzerの紹介"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# rust-analyzerの紹介
----------------------
[下町.rs #1 (オンライン） - connpass](https://shitamachi.connpass.com/event/173972/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# rust-analyzer
---------------

* 最近開発が盛んなRustのIDEバックエンド
  + この発表より先に紹介記事でてきちゃった… https://rust-analyzer.github.io/blog/2020/04/20/first-release.html
* 最近alphaがリリースされたよ


<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">最近成熟してきているrust-analyzer、勧告が提案されてる<br><br>Transition to rust-analyzer as our official LSP (Language Server Protocol) implementation by nikomatsakis · Pull Request #2912 · rust-lang/rfcs<a href="https://t.co/O0Wvi4nGAL">https://t.co/O0Wvi4nGAL</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1253168378333675520?ref_src=twsrc%5Etfw">April 23, 2020</a></blockquote>

===
# RustのIDEバックエンド
-----------------------

* 昔: racer
* 今: rls
* 未来: rust-analyzer


===
# rlsとLSP
-----------

* rlsのバックグラウンドについておさらい
* LSP: IDEバックエンドのプロトコル
* rls: RustのLSPサーバ
  + rust language server
  + 現在公式の推奨ツール
  + みんな使ってるよね？

===
# LSP
-----

* [これ](https://microsoft.github.io/language-server-protocol/)
* Microsoft主導で策定したIDEバックエンドのプロトコル
  + JSON RPCベース
* コーディング支援をIDE（フロントエンド）と解析ツール（バックエンド）に分離する
  * 型とかドキュメントとか定義ジャンプとか
  * 言語解析が必要なので言語ごとに必要
* 一回バックエンドツールを作れば使い回せる

===
# LSP

![lspとエディタ、バックエンドの関係](/images/lsp.png) <!-- .element: style="width="100%" -->

===
# 余談1: rust survey
-------------------

* [Rust Survey 2019 Results | Rust Blog](https://blog.rust-lang.org/2020/04/17/Rust-survey-2019.html)
* Rustの使用歴とか開発環境とかのアンケート
* エディタ（IDE）はVSCode, Vim, IntelliJが3大巨頭でEmacs, Sublimeが次ぐくらい
* IDEバックエンドは43%がrls, 21.7%がIntelliJ, 15.2%がrust-analyzer

===
# 余談2: IntelliJ Rust
-----------------------

* IntelliJ Rustは自前でRustを解析している
* rlsもrust-analyzerも使ってない
* あとフォーマッタも独自っぽい
  + それはちょっと…

===
# IDEバックエンド比較
---------------------

<table>
<tr><th>ツール</th><th>開発</th><th>適用範囲</th><th>特徴</th></tr>
<tr><td>rls</td><td>公式</td><td>LSP</td><td>コンパイルして中間生成物としてIDE向けの情報を出す（遅い）</td></tr>
<tr><td>IntelliJ Rust</td><td>JetBrain</td><td>IntelliJでのみ使える</td><td>編集にあわせてオンデマンドで情報を出す（速い）</td></tr>
<tr><td>rust-analyzer</td><td>公式</td><td>LSP</td><td>編集にあわせてオンデマンドで情報を出す（速い）。開発途上</td></tr>
</table>

===
# library-ification
----------------

* rls → rust-analyzerは単にフルスクラッチしただけではない
* Rustコンパイラを周辺ツールフレンドリに再構成する流れもきてる
  + [library-ification](https://smallcultfollowing.com/babysteps/blog/2020/04/09/libraryification/)
  + [demand-driven compilation](https://rustc-dev-guide.rust-lang.org/query.html)
* 例えば[Chalk](https://blog.rust-lang.org/inside-rust/2020/03/28/traits-sprint-1.html)とか

===
# 使ってみた
------

* 少しだけrust-analyzer使ってみた
* 感覚的に、慣れもあってrlsの方が使いやすい気がする
  + 補完とか賢い気がする…？
* rust-analyzerはコマンドがいっぱいあって便利
  + 今後の発展に期待

===
# デモ
-------

===
# まとめ
--------

* rust-analyzerはRustの新しいlspサーバだよ
* 荒削りだけど現時点でもrlsより優れているところもあるよ
* 見えづらいけどコンパイラ言語以外の面でも進化してるよ
  + 言語は総合力！
* 多分そのうち公式になるから覚えておいてね


</script>
</section>
