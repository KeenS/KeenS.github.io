---
categories: [番外編]
date: 2017-01-01T22:12:01+09:00
title: 2017年注目していきたい技術
---

κeenです。毎年これやっていく。個人的に注目していきたい技術と飛び込んでみたい技術書いく。

あくまで個人的な内容。

<!--more-->

# [WebAssembly](http://webassembly.org/)
ブラウザ上で動く仮想アセンブラ。ブラウザ上でのJSの高速実行はブラウザでの至上命題である。
JIT技術を各ブラウザベンダが切磋琢磨していたがそれでも限界があるので[asm.js](http://asmjs.org/)なんかが産まれた。
これは例えば

``` javascript
function geometricMean(start, end) {
  start = start|0; // start has type int
  end = end|0;     // end has type int
  return +exp(+logSum(start, end) / +((end - start)|0));
}
```

のように`x|0`と書くと`x`が整数であることを表わす、などとしてJSのサブセットで型情報も付与出来るようにしたものだ。冗長なので人の手で書くことは意図していない。
これはある程度上手くいって、[emscripten](https://github.com/kripken/emscripten)のようにLLVMからjsへのコンパイラでも使われている。

それでもまだ問題がある。1つに、JSよりも冗長な記法を使っているためファイルが嵩張る点。ロード時間やパース時間が長くなる。そもそも人の手で掛かないならバイナリフォーマットでもいい筈だ。
そしてもう1つに低レベルな処理、例えばSIMDなんかは扱えない点。

WebAssemblyはこれらを解決する。仮想的な機械語でバイナリフォーマットがあるので低レベルなことが（将来）出来てコンパクトになっている。
計算モデルはスタックベースのマシンになっている。メモリや関数テーブルなどもある。

```
(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    get_local 0
    get_local 1
    i32.add)
  (export "addTwo" (func 0)))

```

多くの人にとってWebAssemblyは意識して関るものではなく、emscriptenの吐くコードが効率的になる、程度のものだろう。
私はコンパイラを作る人なので意識する必要がある。
WebAssemblyにはundefined behaviourがないだとかThread API、 SIMD APIなどが入る予定だとかがあるのでLLVM経由で吐くよりも直接吐いた方が面白い。

少し追っていきたい。

# [QUIC](https://www.chromium.org/quic)
HTTP/2のための代替TCP。HTTP/2はもうリリースされてるので次はQUICを。
HTTP/2は1コネクションを複数のstreamに分割するが、stream内での到達順序保障は必要なもののstream同士ではそれが不要なのでTCPの到達順序保障が過剰要求になってしまう。
それを緩めるためにUDPベースでプロトコルを作ったのがQUIC。まあ、他にも色々改善点はあるが。

これも基本的にはあまり追う必要はないが、自分の使いたい言語で実装がなかったら自分で実装することになる。
まだその辺の環境が整っていないので今後どうなるか注視する必要がある。

# [Idris](http://www.idris-lang.org/)
依存型のある言語。今年もRustに忙しい可能性があるが、ちょっとIdrisに興味が湧いた。
常々「多相があって正格評価で高階関数を簡単に扱えてサブタイピングのない、C FFIやThreadを扱える言語」を捜していて、それがATS2だったりSML#だったりRustだったりした。
最近はRustに落ち着いたが、今度は低レベルな部分、「C FFIやThreadを扱える」がなくてもいいから依存型が入ってる言語が欲しくなった。ATS2は置いといてIdrisかなーと。

生の依存型だとつらいかな、と思っていたらtacticもあるようだったので機会があればやってみたい。

CF [プログラミング言語 idris - wkwkesのやつ](http://wkwkes.hatenablog.com/entry/2016/12/17/000000)

# [Lean](http://leanprover.github.io/)
MSRで開発されている定理証明支援系。よく「Coqでいいじゃん」「Agdaは？」と訊かれるが、[オンラインチュートリアル](https://leanprover.github.io/theorem_proving_in_lean/index.html)が良さげだったのと、Emacsから使えるのと、
[RustからLeanへのトランスレートをやっている](https://kha.github.io/2016/07/22/formally-verifying-rusts-binary-search.html)人がいたので興味湧いた。
そもそもCoqをある程度やってからにしろとは自分でも思う。

# [Coq](https://coq.inria.fr/)
定理証明支援系。去年も上がっていたが、今年は酉年なので。「Agdaは？」。知らん。

# [Finagle](https://twitter.github.io/finagle/)
RPCのクライアント/サーバフレームワーク。RPCをやる時にいくつか問題が出る。
1つはペイロードがRPC毎に違うのでフレームワークが定まりづらい点。
もう1つはロードバランシングがしづらい点。
ロードバランシングの方に言及しておくと、RPCをやる時は大抵コネクションを張りっぱなしなのでTCPロードバランサが使えない。
例えば順番にサーバを起動していくと最初に上がったサーバにコネクションが集中して以後バランスされない。

これを解決するのがFinagleで、クライアントが全てのサーバにコネクションを貼って、クライアントサイドでロードバランシングをする。
さらにクライアントが複数のサーバを知っているのでサーバがエラーを返したら別のサーバにリクエストを投げることも出来る。
ペイロードの話は多相型で解決する。パーサとかその辺も含めたフレームワークになっている。

# [Tokio](https://github.com/tokio-rs/tokio)
FinagleのRust版。Rustは非同期IOに強いと思っているのでTokioがリリースされたらそこら辺のHTTPフレームワークも非同期化するのではと思っている。

# [TiDB](https://github.com/pingcap/tidb)
分散スケール可能なSQL DB。Rust製。[Google F1](https://research.google.com/pubs/pub41344.html)を参考に作られているらしい。
ストレージエンジン自体は[RocksDB](http://rocksdb.org/)を使っていて、その上に分散合意、MVCC、トランザクションを載せてさらにそれにSQLレイヤー、MySQLプロトコルレイヤーを載せている。

![TiDBのアーキテクチャ画像](https://pingcap.github.io/blog/assets/img/how-we-build-tidb-2.png)

アーキテクチャについては上の画像を引用した[この記事](https://pingcap.github.io/blog/2016/10/17/how-we-build-tidb/)が詳しい。

# [tantivy](https://github.com/tantivy-search/tantivy)
全文検索エンジンライブラリ。Rust製。アーキテクチャやアルゴリズムは[Apache Lucene](http://lucene.apache.org/core/)を参考に作られているらしいのでだいたいそのレイヤーのライブラリと思ってもらえれば。

Rust製なのでインデックスの構築が速いのが一つの特徴。今後、自前でElastic SearchやApache Solrのようなレイヤーを作るのかLuceneの置き換えを狙ってJava APIを提供するのかは不明。

注目したい理由はベースで使っている[fstライブラリ](https://github.com/BurntSushi/fst)の[紹介記事](http://blog.burntsushi.net/transducers/)が気に入ったから。
