---
categories: [Rust, Thrift, 非同期]
date: 2016-11-26T13:07:29+09:00
description: "<a src='https://kbkz.connpass.com/event/40629/'>歌舞伎座.tech#12「メッセージフォーマット/RPC勉強会」 - connpass</a>での発表用
"
title: Rustで非同期Thriftしたい
---

<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">

# Rustで非同期Thrift
----------------------
[歌舞伎座.tech#12「メッセージフォーマット/RPC勉強会」 - connpass](https://kbkz.connpass.com/event/40629/)

<!-- .slide: class="center" -->

===

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * サイバーエージェントのエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

* <span style="font-size:150%">X</span> RPCライブラリを使う話
* <span style="font-size:150%">O</span> RPCライブラリを作る話

<!-- .slide: class="center" -->

===

# RPC
-----

* Remote Procedure Call
* リモートで呼べる
* 言語跨げる
* シリアライズフォーマットが決まれば大体出来る
* Thrift, protobuf/gRPC, avro...
* 大抵バイナリ
  + JSONに比べて2倍くらい効率がいい

===

# Thrift
--------

* Facebook発(現apache)RPCフレームワーク
* IDLから複数の言語向けのコードを吐ける
* 対応言語多い
* 新しい言語はthriftレポジトリフォークして追加

===

# Thrift vs gRPC
----------------

\                       | Thrift | gRPC
------------------------|:------:|:----:
ベース                  | 自前   | HTTP/2
コード生成              | o      | o
プロトコルのアップデート | o      | o
通信の多重化            | トランスポート次第  | o (HTTP/2)
認証                    | x  | o
例外                    | ユーザ定義可能 | 事前定義のみ?
対応言語                | 多い   | ほどほど
その他                  | union  | timestamp, anyなど
<!-- .element: style=" margin-right:auto; margin-left:auto"-->

===

# 何故Thrift?
-------------

* 仕事で使ってたから
* 仕事は「gRPCかthrift、まあthriftでいっしょ」で決まった

===

# RustとThrift
--------------
いくつか実装がある

===

## [sgnr/rust-thrift](https://github.com/sgnr/rust-thrift)

<!-- .slide: class="center" -->

===

![sgnr/rust-thrift is deprecated](/images/thrift/sgnr-rust-thrift.png)

===

## [maximg/thrift](https://github.com/maximg/thrift)

<!-- .slide: class="center" -->

===

![maximg/thrift is deprecated](/images/thrift/maximg-thrift.png)

===

## [terminalcloud/thrift](https://github.com/terminalcloud/thrift)

<!-- .slide: class="center" -->

===

![terminalcloud/thrift is deprecated](/images/thrift/terminalcloud.png)

===

# [thehydroimpulse/thrust](https://github.com/thehydroimpulse/thrust)
---------------------------

* Thrift RPC in Rust (Async I/O)
* 非同期!!
* apache/thriftベースじゃない（フルスクラッチ）
* コンパイラプラグインサポートとかも
* 作りかけ
  + `Latest commit 0a37b77 on 11 Apr`

→ フォークすることに

===

# [Finagle](https://github.com/twitter/finagle)
---------

* **Scalaの** RPCフレームワーク
* [You Server as a Function](https://monkey.org/~marius/funsrv.pdf)
* Nettyベース
* サーバとクライアント両方サポート
* Twitter製
* 良くも悪くもTwitterべったり

===

# Finagle Client
--------

* クライアントが賢い
* RPCではTCPベースのLBが使えない
  + クライアントはコネクション張りっぱなし
* クライアントサイドロードバランシングが必要
* Finagleはそこまでカバー
* 他にもエラーが起きたサーバ外したり色々

===

# Tokio
--------

* **Rustの** RPCフレームワーク
* mioベース
* Finagleを参考に開発
  + **開発中**
* tokio-core
  + コア部分
* tokio-proto
  + プロトコル実装のサポート
  + 絶賛API変更中
* tokio-service
  + サービス実装のサポート


===

# TokioとFinagle
-----------------

  \           | Tokio       | Finagle
--------------|:-----------:|:-------:
コア          | [tokio-core](https://github.com/tokio-rs/tokio-core) | [finagle-core](https://github.com/twitter/finagle/tree/develop/finagle-core)
Future        | [future](https://github.com/alexcrichton/futures-rs) | [twitter util](https://github.com/twitter/util)
非同期エンジン | [mio](https://github.com/carllerche/mio) | [netty](http://netty.io/)
<!-- .element: style=" margin-right:auto; margin-left:auto"-->


===

# [mio](https://github.com/carllerche/mio)
------

* 非同期イベントループライブラリ
* イベントドリブン
* ループ内ゼロアロケーション
* `epoll` などの薄いラッパ
* libuv, libevent, libev2などのRust版
* ソケットだけでなく *ファイルIO* 、インメモリチャネルとかも監視出来る

===

# [future](https://github.com/alexcrichton/futures-rs)
--------------

* [Zero-cost futures in Rust · Aaron Turon](https://aturon.github.io/blog/2016/08/11/futures/)
* ゼロコスト
* 普通にFutureを使うとランタイムにステートマシンになる
* 「将来実行される」という「状態」を第一級の値として表現する大事な手段
* お型付けは大変
    + `map` したら `Map<A, fn (A)->B>` の型が返ったり
* ストリーミング指向のAPIもある

===

# [tokio-core](https://github.com/tokio-rs/tokio-core)
--------------

* 非同期IOのフレームワーク
* Futureベース
* フレームドIOとストリーミングIO
* フレームワークというよりユーティリティ？
  + mioとfutureを合わせて使う

===

# [tokio-proto](https://github.com/tokio-rs/tokio-proto)
-------------

* 主にバイナリ列<->構造体の部分の面倒を見てくれる
* API変更中…
* バッファリングしてパースしやすくしてくれる
* プロトコルの多重化やってくれる(後述)

===

# [tokio-service](https://github.com/tokio-rs/tokio-service)
-----------------

* タスクサーバの面倒見てくれる
* 主にロードバランシング


===

# [Service](https://tokio-rs.github.io/tokio-service/tokio_service/trait.Service.html)
-------------

``` rust
pub trait Service {
    type Request;
    type Response;
    type Error;
    type Future: Future<Item=Self::Response, Error=Self::Error>;
    fn call(&self, req: Self::Request) -> Self::Future;
}
```

===

# Service
----------

* いわゆるハンドラ
* インターフェースさえ満たせばいい
  + ミドルウェアもハンドラも同じ扱い

===

# [NewService](https://tokio-rs.github.io/tokio-service/tokio_service/trait.NewService.html)
--------------

```rust
pub trait NewService {
    type Request;
    type Response;
    type Error;
    type Instance: Service<Request=Self::Request, Response=Self::Response, Error=Self::Error>;
    fn new_service(&self) -> Result<Self::Instance>;
}

impl<F, R> NewService for F where F: Fn() -> Result<R>, R: Service
```

===

# NewService
------------

* いわゆるAbstractServiceFactory的な存在
* コア数に応じてサービスを作ってくれる
* ただし`Service`を返す普通の関数も`NewService`になる
* [example](https://github.com/tokio-rs/tokio-line/blob/master/examples/echo_client_server.rs#L21)


===

# tokio-thrift
<!-- .slide: class="center" -->

===

# [tokio-thrift](https://github.com/KeenS/tokio-thrift)
--------------

* [KeenS/tokio-thrift](https://github.com/KeenS/tokio-thrift)
* この発表のために作った（作ってる）
* tokioベースのrustのthriftサポート
* thrustのフォーク
* 基本ツール全て
  + コードジェネレータ
  + コンパイラプラグイン
  + ランタイムライブラリ

===

# プロトコルスタック
-------------------

ユーザは実装に集中出来る仕組み

```
+------------------------------------------------+
| service   | tokio-service + 生成コード + ユーザ |
|------------------------------------------------|
| protocol  | tokio-proto   + tokio-thrift       |
|------------------------------------------------|
| transport | tokio-core                         |
+------------------------------------------------+
```

===

# サンプルコード
----------------

* [これ](https://github.com/KeenS/tokio-thrift/blob/master/example/simple_server_client/src/main.rs)

===

# ユーザビリティ
---------------

* サーバ: ユーザはインターフェースに沿ったコードを書くだけ
* クライアント: インターフェース通りに呼べる
* 返り値はFuture
* サーバ: 非同期実装と相性がいい
* クライアント: 非同期通信をそのままエンコード

===

# 実装の話

<!-- .slide: class="center" -->

===

# Thriftのプロトコル
-------------------

* 現状binary protocolのみ
  + fork元が実装してあった
* compactとか需要ある？
* 因みにJSONはやめとけ

===

# 所有権とゼロコピー
-------------------

* プロトコルのパースの部分の話
* Rustっぽい部分
* バッファの所有権をパース結果に渡すことでデータのコピーを避ける

===

# 所有権とゼロコピー
-------------------
バッファをresultにパースして

```
buffer
  |
+-+---------------+
|                 |
-------------------
| | | | | ... | | | <- バイト列
-------------------
```

===

# 所有権とゼロコピー
-------------------
resultにデータの所有権を渡してしまう

```
 result   buffer
   |        |
+--+----++--+-----+
|       ||        |
-------------------
| | | | | ... | | | <- バイト列
-------------------
```

===

# 複数メソッド
-------------

* 複数メソッドだと複数の引数/返値の型を扱う
* コネクションのハンドラは1つ
* 複数型をどうやって扱う？
  + [enumを定義](https://github.com/KeenS/tokio-thrift/blob/master/example/simple_server_client/src/thrift.rs#L67)
  + ゼロオーバーヘッド…

===

# 複数メソッド
-------------

```
 (methodA) (methodB)
     |         |
     +----+----+
          | <----- enum化
      [client]
          |
      [server]
          | <----- enumでディスパッチ
     +----+-----+
     |          |
(handlerA) (handlerB)
```

===

# つらい話
-----------------

* Thrift IDLのセマンティクスが書いてない
  + throws節のフィールドは直積？直和？
  + Javaで生成してみて推測するしかない
* フォークライブラリの実装が不完全
  + パーサはフルスクラッチで書き直した
  + [パーサコンビネータ便利](https://github.com/KeenS/tokio-thrift/blob/master/tokio-thrift-codegen/src/parser.rs#L517)
  + フォークした意味…
* Rustにエンコード出来ないthriftの機能もある
  + mapの定数は普通には無理
  + エラーどうしよう(`Error`トレイトの実装とEnum)
* 所有権の問題

===

# 言語との統合
-------------

* Rustには公式のシリアライズフレームワークがある
* [rustc-serialize](https://github.com/rust-lang-nursery/rustc-serialize)
* あるいはユーザ側のも
* [serde](https://github.com/serde-rs/serde)
* のっかりたいけどThriftに必要なデータがない
  + フィールドのシーケンス番号とか
* 今のところ自動生成に頼る
  + フレームワークにフィードバックした方がいい？

===

# コード生成の話
---------------

* [コンパイラプラグイン](https://github.com/KeenS/tokio-thrift/blob/master/tokio-thrift-macros/tests/service.rs#L10)微妙
  + 定義元ジャンプ、フィールド名補完などに難
* CLI生成は悪手
  + IDL変更する度CLI実行？
  + 生成コードはコミット？
    - CLIのバージョンがあるので基本はコミット。でも…
* ビルドツール統合が最良
  + FinagleにはScoogeがある
  + tokio-thriftでもやりたい
    - build.rsのサポート

===

# 多重化の話
------------

* 生のthrift on TCPだと通信を多重化出来ない
* tokio-proto側でpipelineやmultiplexなどをサポート
* pipeline: レスポンスを待たずに次のリクエスト
* multiplex: リクエストとレスポンスが入り乱れてもよい。プロトコルオーバーヘッドがある
* tokio-thriftは未対応

===

# Tokioの利点
-------------

* service: サーバとクライアント両方面倒見てくれる
  + サーバもCPUの数だけハンドラをクローンしてくれたりする
* protocol: 面倒毎が少ない(バッファ使ってくれる)
* Futureベースの非同期
* pipeline, multiplexによる多重化

===

# Tokioの欠点
-------------

* APIが不安定
  + tokio-thriftは今動かないorz
* thriftはストリーミング指向なのにバッファ使う
* Rustの痒い所に当たる
  + 返り値多相がない(`Future`を直接返せない)
  + アロケーションが必要になる
  + ダイナミックディスパッチが必要になる
  + 他言語と同水準のコスト

===

# まとめ
--------

* Thriftは広さで勝負
* コードジェネレータはビルドツールへの統合を
* IDLは既存のコードに乗っかった方が楽（上と競合）
* フレームワークを使うとみんな幸せ
* RPCはインターフェースベースであるべき
* 多重化はRPCより下のレイヤーで出来る

</script>
</section>
