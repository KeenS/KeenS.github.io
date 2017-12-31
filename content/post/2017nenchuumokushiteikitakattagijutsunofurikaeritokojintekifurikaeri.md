---
categories: [番外編]
date: 2017-12-31T17:27:20+09:00
title: "2017年注目していきたかった技術の振り返りと個人的振り返り"
---
κeenです。年始に[2017年注目していきたい技術 | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2017/01/01/2017nenchuumokushiteikitaigijutsu/)ってのを書いたので雑に振り返ります。あと個人的な一年の総括を。

※個人メモなので雑多な記述が多いです。
<!--more-->

# 2017年注目していきたかった技術

今年一年でどうなったかを見ていく。

## [WebAssembly](http://webassembly.org/)

各ブラウザでも使えるようになったし盛り上がってる。
[WebAssembly Advent Calendar](https://qiita.com/advent-calendar/2017/webassembly)も盛況だったし個人的にも[GCを書いた](https://keens.github.io/blog/2017/12/07/webassemblynogc/)。

RustやClangがemscriptenを通さず吐けるようになってるしツーリング周りでも改善された。

今の所ゲーム系の人が注目してるみたいだけどもっと裾野が広がったらいいな。

## [QUIC](https://www.chromium.org/quic)

確実に進んでるはずなんだけど今年あんまり話題にならなかった。
個人的には古いやつだけど[仕様を読んだ](http://keens.github.io/blog/2017/01/02/quicnonakamigawakaranaikarashiyouyondemita/)。
推奨になるのいつだろう。

## [Idris](http://www.idris-lang.org/)

1.1.1も出てる。[Idris Tutorial](http://docs.idris-lang.org/en/latest/tutorial/)をこの年末でやった。
totalityを要求する部分と要求しない部分を分けたり、型から得られる情報が多い分インタラクティブ開発機能に力が入ってたり中々面白い。
来年も時間があればもう少しやっていきたい。

## [Lean](http://leanprover.github.io/)
ちらほら使ってる人がいるけど個人的には何もしなかった。

## [Coq](https://coq.inria.fr/)
かなりスピードが落ちたけどまだ[ソフトウェアの基礎](http://proofcafe.org/sf/)をやってる。
この先Appelの[Verified Functional Algorithms](https://softwarefoundations.cis.upenn.edu/vfa-current/index.html)やAdam先生の[Certified Programming with Dependent Types](http://adam.chlipala.net/cpdt/)もあるしまだまだ継続。CPDTは和訳もでるらしいし期待。

## [Finagle](https://twitter.github.io/finagle/)
そもそも今年はScalaを触ってない

## [Tokio](https://github.com/tokio-rs/tokio)
tokioの0.1は出たしhyperも0.11からTokioを使うようになったし確実に普及してる。よかったよかった。

## [TiDB](https://github.com/pingcap/tidb)
[1.0](https://pingcap.com/blog/2017-10-16-ga/)もリリースされたし順調。
今までは技術的に面白いから注目してたけどそろそろ実用的かの目線で見るべきなんだろうか。

## [tantivy](https://github.com/tantivy-search/tantivy)
注目してなかったけど活発に開発されてるみたい。Apache Luceneのdrop-in-replacementになっていくのかな。

# 個人的振り返り
## メトリクス

ブログ記事32本、スライド9綴、mendeleyに突っ込んだ論文112綴、amazonで買った技術書10冊、GitHubのcontribution 1773回作ったレポジトリ10個。

論文と本は全て読んだわけじゃない(むしろ本は読んだ方が少ない)けどだいたいこんな感じ。

## 総括
一番大きな出来事は年始の[転職](http://keens.github.io/blog/2017/01/06/idein_incninyuushashimashita/)とそれに付随する引っ越し。
Scala(Javaも少し)を使っていた職場からよくわからないところに入ってRustを書くようになった。なのでRustのコードを書く時間は圧倒的に多くなったし仕事から派生していくつかライブラリも作った[1](https://crates.io/crates/www-authenticate) [2](https://crates.io/crates/transaction-diesel) [3](https://crates.io/crates/moneyforward-invoice-api) [4](https://crates.io/crates/iron_inspect) [5](https://crates.io/crates/chema) [6](https://crates.io/crates/cargo-pack-docker)。

もう一つ大きなのはクローズドなプロジェクトが1つ走っていて、プライベートがそっちに割かれた。なので例年に比べてアウトプットもインプットも少ない。できれば年内にカタを付けたかったが間に合わそうなのでもうちょっと。
あと年明けからなんか始まる感じなのでクローズドなのが2つになる。

これらの影響で致命的にまずいのがRustのドキュメントの翻訳がほぼ止まってしまった件。先述のとおり来年もまだ忙しそうなので後任を探したい。

あと地味に危機感を持ってるのがやっぱプライベートが削られてオープンなコードを書く習慣が 減ってきた件。本も読めてない。エンジニアとして死にそう。

2017年は何も出来なかったので2018年はもうちょっと進捗を出したい。小説なんかも含めて積読が21あるのでそれの消化とか。岳飛伝の刊行に消化が追いついてない。
