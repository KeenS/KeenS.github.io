---
categories: ["型", "smlsharp"]
date: 2019-12-21T22:31:35+09:00
title: "リソースを管理する型"
---

このエントリは[型 Advent Calendar 2019 - Qiita](https://qiita.com/advent-calendar/2019/type) 22日目の記事です。

κeenです。Session Typeの話をするつもりでしたが気が変わったのでリソースを管理する型の話をします。
リソースといっても所有権の話ではないです。
DBにWriteできるだとかS3からGetできるだとかです。リソースというよりはアクセス権限の方が正確かもしれません。

<!--more-->

他のエントリが学術的なものばかりなのに対してこれは実装テクニック的な記事です。
型理論のアドベントカレンダーですが、まあいいや。

# モチベーション

Webアプリケーションを作っていると、アプリケーションが触るリソースを静的に知りたいことがあります。
例えば、以下のような需要があります。

* データベースが壊れてReadしかできなくなった時にそれでも動く機能はどれか知りたい
* この機能遅そうだけどキャッシュしたりしてる？
* このアプリケーションに渡すAWSの権限はどれが適切か
  + 例えば「このリソース使ってなさそうだから権限外したいんだけど大丈夫？」とか

そのくらい作った人が知っとけよと思うかもしれませんが、複雑になったアプリケーションで正確に把握するのは簡単ではありません。
そういうときに型で静的に「この関数を呼ぶにはこのリソースへのこの権限によるアクセスが必要になる」というのが分かると嬉しいです。

もうちょっと言うと、それが自動的に求まると嬉しいです。
以下の抽象的な例を見ましょう。

```text
fetchData:
    let result = getFromCache
    if result is null:
      let data = getFromDB
      writeToCache(data)
      result <- data
    enfif
    return result
```

キャッシュにRead/Write、DBにReadしています。
このコードから `fetchData` がアクセスするリソースが「キャッシュにread/write、dbにread」であることが自動で求まって欲しいです。


この例だとリソースは2種類、権限もreadとwriteの2種類ですが、現実ではもっと沢山のリソースと権限が必要になります。
例えばawsのリソースはサービス、さらにその中での特定のパスのようにいくらでも細かく分けられますし、そのリソースの各操作単位で権限を管理できるので多様なリソースと権限が出てきます。
以下はawsのドキュメントの[ポリシーとアクセス許可](https://docs.aws.amazon.com/ja_jp/iam/latest/userguide/access_policies.html)から引用した設定json。

```json
{
  "version": "2012-10-17",
  "statement": [
    {
      "sid": "firststatement",
      "effect": "allow",
      "action": ["iam:changepassword"],
      "resource": "*"
    },
    {
      "sid": "secondstatement",
      "effect": "allow",
      "action": "s3:listallmybuckets",
      "resource": "*"
    },
    {
      "sid": "thirdstatement",
      "effect": "allow",
      "action": [
        "s3:list*",
        "s3:get*"
      ],
      "resource": [
        "arn:aws:s3:::confidential-data",
        "arn:aws:s3:::confidential-data/*"
      ],
      "condition": {"bool": {"aws:multifactorauthpresent": "true"}}
    }
  ]
}
```

リソースやアクション（権限）が色々あるのが分かるかと思います。

こういうのを（ある程度）表現できる型があると実行時に権限が足りなくて落ちるなんてことがないので嬉しいよね、というのがモチベーションです。

# 要件を詰める

何が欲しいのかもうちょっとはっきりさせていきましょう。

前提として、コードはDBやキャッシュなど外部にアクセスする部分とロジックを組み立てる部分に分かれてますしロジックは具体的なDBに依存せずに書かれているとします。

つまり先程の例をもう少し詳細に書くと以下ではなくて

```text
fetchData():
    let cache = connect to redis
    let result = cache.query("get data")
    if result is null:
      let db = connect to postgres
      let data = db.query("SELECT * from DATA")
      cache.setData("set ?", data)
      result <- data
    enfif
    return result

fetchData()
```

以下のように書かれています。

```text
getFromCache(conn):
    return conn.query("get data")

writeToCache(conn, data):
    return conn.setData("set ?", data)

getFromDB(conn):
    return db.query("SELECT * from DATA")

fetchData(ctx):
    let result = getFromCache(ctx.cache)
    if result is null:
      let data = getFromDB(ctx.db)
      writeToCache(data)
      result <- data
    enfif
    return result

let cache = connect to redis
let db = connect to postgres
let ctx = {cache, db}
fetchData(ctx)
```

こういう状況から

* `fetchData` がアクセスするリソースはDBとキャッシュである
  + つまり `ctx` はDBとキャシュへのアクセスを提供するコンテキストでないといけない
* `fetchData` がアクセスするDBにはRead権限が必要である
  + つまり `ctx` が提供するDBへのアクセスにはRead権限が含まれる
* `fetchData` がアクセスするキャッシュにはReadとWriteの権限が必要である
  + つまり `ctx` が提供するキャッシュへのアクセスにはReadとWriteの権限が含まれる

というのを抜き出して欲しいです。
もちろん、 `fetchData` はDBへのReadしか要求していませんが ReadとWrite両方のアクセス権限があるコンテキストを渡されてもコンパイルが通ってほしいです。

これらの要件をそれっぽい言い方をすると

* アクセスするリソースのintersectionを自動でとってほしい
* リソースの中でもさらにその権限のintersectionを自動でとってほしい
* リソースの一覧や権限の一覧はサブタイピングなどの構造的多相性が欲しい

ということになります。

`fetchData` の話をしましたが流石に `getFromCache` などは外部ライブラリになるので手でアノテーションをつけます。


こういったシステムに見覚えないですか？

…そう、レコード多相です。

# SML#による解法

上記の例をSML#で実装してみます。

その前にSML#のレコード多相の記法についておさらいしておきましょう。

## SMLの多相パラメータ

(SML#とは限らない一般の)SMLの関数定義では `fun` に続いて多相パラメータを導入できます。

```sml
(* 型アノテーションなしの `id` 関数 *)
fun id x =x

(* 型アノテーションありの `id` 関数。 `fun` に続いて型パラメータ 'a を導入している。 *)
fun 'a id (x: 'a): 'a = x
```

## SML# のレコード多相
SML# はこの `'a` に続いて `'a#{..}` と書いて型にアノテーションを書けます。
例えば 「`name` フィールドを持つレコード」 を表わす型パラメータ以下のように書けます。

``` sml
'a#{name: 'b}
```

## レコード多相によるリソースの管理

それではSML#でリソースを管理してみましょう。

まずはデータベースへのコネクションは幽霊型を付けておきます。
この幽霊型で権限を表わします。

``` sml
datatype 'a db_conn = DbConn
datatype 'a cache_conn = CacheConn
```

権限はReadならば `{read : unit}` 、 Writeならば `{write: unit}` とつけることにします。

そしてそれぞれデータアクセス関数には最小限の権限のアノテーションをつけておきます。

``` sml
(* それぞれの実装は空 *)
fun 'a#{  read: unit }     readDb (DbConn:    'a db_conn)    = ()
fun 'a#{ write: unit }    writeDb (DbConn:    'a db_conn)    = ()
fun 'a#{  read: unit }  readCache (CacheConn: 'a cache_conn) = ()
fun 'a#{ write: unit } writeCache (CacheConn: 'a cache_conn) = ()
```

これを使って `fetchData` を定義します。
この関数には型アノテーションが一切出てこないことに注目して下さい。


``` sml
fun fetchData ctx = let
    val () = readCache (#cache_conn ctx)
    val () = readDb (#db_conn ctx)
    val () = writeCache (#cache_conn ctx)
in () end
```

この関数をREPLにロードすると望み通りDBにRead、キャッシュにWriteの型がついていることが分かります。

``` text
val fetchData = fn
  : ['a#{cache_conn: 'b cache_conn, db_conn: 'c db_conn},
     'b#{read: unit, write: unit},
     'c#{read: unit}.
       'a -> unit]
```

一応ちゃんと型検査で弾けるか試してみましょう。

各のリソースにread only、 read writeな権限を持つコンテキストを用意します。

``` sml
type read_context = {
    db_conn: {read: unit} db_conn,
    cache_conn: {read: unit} cache_conn
}

type read_write_context = {
    db_conn: {read: unit, write: unit} db_conn,
    cache_conn: {read: unit, write: unit} cache_conn
}

val readContext:            read_context = { db_conn = DbConn, cache_conn = CacheConn }
val readWriteContext: read_write_context = { db_conn = DbConn, cache_conn = CacheConn }
```

今回の `fetchData` はキャッシュに書き込んでいてWriteを要求するので `readContext` ではだめで、 `readWriteContext` が必要になります。
`fetchData` に `readContext` と `readWriteContext` をそれぞれ与えて実行できるか試してみましょう。

```text
(* `readContext` を与えると型エラー *)
# fetchData readContext;
(interactive):14.0-14.20 Error:
  (type inference 016) operator and operand don't agree
  operator domain: 'BTNJ#{cache_conn:
                            'BTNK#{read: unit, write: unit} (lambdaDepth: 2147483647 )
                              cache_conn,
                          db_conn:
                            'BTNL#{read: unit} (lambdaDepth: 2147483647 )
                              db_conn}
          operand: {cache_conn: {read: unit} cache_conn,
                    db_conn: {read: unit} db_conn}
```

``` text
(* `readWriteContext` を与えると実行される *)
# fetchData readWriteContext;
val it = () : unit
```

目論見通り正しいコンテキストでのみ実行されました。
fetchという名前からReadアクセスしかしないだろと思って痛い目に遭わずに済みますね。

# まとめ

SML#を使えば安全なWebアプリケーションが作れることが分かりました。

# 余談

アクセスするリソースを型で管理するというアイディアは[ドワンゴのFujitask](https://qiita.com/pab_tech/items/86e4c31d052c678f6fa6)からきています。
これを真似て作った[Rustのトランザクション抽象化ライブラリ](https://keens.github.io/blog/2017/06/06/rustnotoranzakushonchuushoukaraiburaritsukutta/)ではリソースの管理まではしていませんでした。
そのときは特にやるモチベーションがなかったのと、サブタイピングを使ったアクセス権限管理がRustとは相性が悪そうだったのでためらったという経緯があります。
また、AWSのようにアクセス権限管理が必ずしも包含関係にないケースもあるのでそもそもサブタイピングを使うことに疑問がありました（というより個人的にはアクセス権限は直交しててほしいし、Write権限にRead権限を含意しないでほしいと思っています）。
時が流れて権限エラーでアプリケーションが起動に失敗する経験があったり、「データベースのmasterが壊れてreplicaしか読めなくなったらどうなるんだっけ？」などと考えていたりすると権限管理が欲しくなりました。
最初はRustでガッと書こうとしたのですがその前に落ち着いて何があれば表現できるかを考えた結果、レコード多相に行き着きました。
多分同様の表現をRustでもトレイトと関連型で表現できる気がするので試してみようかと思います。
