---
categories: [番外編]
date: 2018-01-04T23:36:22+09:00
title: "2018年注目していきたい技術"
---

κeenです。毎年恒例です。個人的に注目していきたい技術と飛び込んでみたい技術を書いていきます。

あくまで個人的な内容なので悪しからず。

<!--more-->
# [Swagger](https://swagger.io/) (OpenAPI Specifitation)
YAMLで書いたREST APIの仕様からドキュメント、クライアントコード、サーバコードのテンプレートを生成するツール。

ネットワーク経由でなんらかのプロトコルを喋るとき、仕様とクライアントとサーバで独立してしまうとそれぞれで齟齬が出る。
プロトコルの詳細以外コードはどれも似たようなものになる訳だし自動生成できると望ましい。
仕様からクライアントとサーバのテンプレートが出てくる感じ。
一応「コードが仕様だ」みたいなものはいっぱいあるしサーバコードから生成も考えられる。
けどサーバが出来上がるまでクライアントが生成できないのはいただけないし何より言語に依存してしまう。
言語非依存に書けるのはやっぱり仕様。

そのようなツールスタックを持ったものとして[Thrift](https://thrift.apache.org/)がある。
個人的にThriftのツーリングは好きなんだけどThriftの(シリアライズ)プロトコルが独自でThriftにロックインされてしまうので公開APIとかには使いづらい。

一方Swaggerはツーリング自体はThriftと似ているが対象ドメインがREST APIなので公開APIにも使える。
さらにドキュメントも書けるしJSONやXML以外にも画像ファイルなんかも扱える、認証もある、と大体機能が揃ってる。

個人的にはADTをもうちょっとスマートにエンコード出来たら嬉しいなという気持ち。

Swaggerの対抗馬として[GraphQL](http://graphql.org/)もある。
ほぼデータベースのラッパのようなREST APIにとってはこちらの方がサーバの実装もクライアントの使い勝手も良さそう。
あとはツーリングの問題だけどそこがコミュニティベースなので言語によって温度感マチマチになるだろうなあという感想。

# [diesel](http://diesel.rs/)

Rustのクエリビルダー/ORM。

それだけだと「ああ、そう」という気分だが、型安全性を追求した点が面白い。
型安全性を追求した結果いくつか興味深い性質が付いてきている。
1つには大堀先生の目指す[「式は型が正しい限り自由に組み合わせることができる」](http://www.pllab.riec.tohoku.ac.jp/smlsharp/docs/3.4.0/ja/Ch11.S2.xhtml)を実現出来ている件。
サブクエリに名前を付けたりして可読性の高いコードが書ける。
もう1つにはその逆、正しく型付けされたクエリDSLはほとんどSQLエラーを起こさない件。
RustとSQLの多言語開発がある程度Rustのみの開発に落とせる。
最後に、型付が細かくできているので型からSQLがほぼ従い、[プリペアドステートメントを最大限活用出来る](https://medium.com/@sgrif/announcing-diesel-1-0-a-safe-extensible-query-builder-and-orm-15e6bd8a9ed0)件。
文字列クエリよりクエリビルダーを使ったほうが速いのは驚き。

もう一つ興味深い点はクエリビルダーファーストで開発されている点。
SQLをRustの上に再現し、その結果から可能な範囲でデータ型にマップする。データ型が対応するのはテーブルでなくてクエリ結果。
オブジェクト指向に寄せるために宣言的なSQLの詳細を隠して手続き的にする方向でなくてSQLを出来る限りそのまま使わせる。
それにSELECT, INSERT, UPDATEでデータ型を分けるのもよく分かってる感じがする。

そして一番の驚きはこれの作者がRails/ActiveRecordの開発もしている点。かなり設計思想が違う気がするけどActiveRecordで悟りを開いた結果なんだろうか。

# [Gotham](https://gotham.rs/)
RustのWebアプリケーションフレームワーク。

非同期かつマルチスレッドで、Stable主義なのが良い。
非同期かつマルチスレッドを目指すとRustでは1つ問題が生じる。
サーバのデータをどう持つか。Rustはデータ競合を許さないので共有状態はMutexで包む必要がある。
多くのRustのWAFはそうしている。しかし個人的にはそこに疑問がある。
どうせ1マシンで完結しないのだからマシン内で(スレッド間)共有状態を持つ必要はないのではないか。
スレッドごとにデータをコビーして共有状態はRedisやRDBに置いておくのが自然な設計なのではないか。

実はhyperの非同期版(つまり0.11)はそういう設計になっている。
非同期WAF側が余計なことをしなければ自然と実現できるはずだ。

Gothamはそれを実現できている。逆に、Gotham以外にそれを実現できているものが見当たらなかった。
もう一つのGothamのセールスポイントはhyperのデータを生で扱える点。
とりあえずhyperで使えるHTTP系の拡張（例えば[multipart](https://github.com/abonander/multipart)など）を上流のコード変更なしに使える。

# Coq
酉年も終わったけどまだ引き続き。SF終わったあとにCPDTとVFAやるまで継続。

# Idris
去年に引き続き。思ったより依存型プログラミングが楽しかった。
あと型情報が強いし基本的にtotalだったりと静的な情報が多くてかなりアグレッシブな最適化ができるはずなのでそこも期待したい。最適化の論文とかないかな。

# 線形論理

Girardの[元論文](https://www.sciencedirect.com/science/article/pii/0304397587900454)や[まとめたやつ（？）](http://www.cs.brandeis.edu/~cs112/2006-cs112/docs/girard-intro.pdf)を読むぞというだけ。
線形論理そのものは[竹内外史の本](https://www.amazon.co.jp/dp/4535782148/)を読んだのだが照井先生の[線形論理の誕生](http://www.kurims.kyoto-u.ac.jp/~terui/birth.pdf)を読むにその基礎づけやproof netが面白そうなのでそこに踏み込んでみたい。

# ベクトル最適化

これも[High Performance Compilers for Parallel Computing](https://dl.acm.org/citation.cfm?id=572937)を読むというだけ。社内で読書会やるとかやらないとかいいつつ結局何も動いてない。


# 2018-01-05追記 量子コンピュータ
[量子コンピュータ Advent Calendar 2017](https://qiita.com/advent-calendar/2017/quantum)に触発されて。
量子コンピュータのためのプログラミング言語がいくつか出てるけど自分でも設計してみたくなった。
[ここ](http://lyncs.hateblo.jp/entry/2017/12/16/000103)によるとCNOT取れるQUBitにも制限があるみたいだしある程度コンパイラが頑張るのかなあとか思ってる。

量子プログラミング言語がどうなるのかまだイメージがつかめてない。
チューリングマシンを模倣出来るらしいし普通のプログラミング言語にbit演算的なノリでアダマール演算とかが入るんだろうか。それとも実質アクセラレータとして使われるだけだから半分アセンブラみたいな言語になるんだろうか。
少なくともそれくらいは判断付くように勉強したい。
