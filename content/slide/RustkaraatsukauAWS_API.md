---
categories: ["Rust"]
date: 2019-03-12T00:23:17+09:00
description: "Shinjuku.rs #3 での発表用。rusotoについて"
title: "Rustから扱うAWS API"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# Rustから扱うAWS API
----------------------
[Shinjuku.rs #3 @FORCIA ](https://forcia.connpass.com/event/117142/)

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
# AWSとは
---------

* https://aws.amazon.com/jp/
* > アマゾン ウェブ サービス（AWS）は、信頼性と拡張性に優れたクラウドコンピューティングサービスを低料金で提供して(中略)います。
* クラウドサービス = オンデマンドの利用 + **API** (私見)
* AWSは豊富なサービスとそれを叩けるAPIが用意されている

===

![AWSのサービス一覧](/images/rusoto/AWSのサービス一覧.png)


===
# AWS SDK
---------

* AWSが公式にAPIバインディングを用意してくれている
* 残念ながらRustは入ってない

![AWS SDKの一覧](/images/rusoto/AWS_SDKの一覧.png)


===
# Rusoto
--------

* [ドキュメント](https://rusoto.org/)
* RustのAWS APIバインディング
* 公式ではない
* Pythonの[SDK](https://github.com/boto/botocore)から自動生成
* rusotoのユーザが増えるとAWSから公式サポートくる[かも](https://users.rust-lang.org/t/getting-rust-official-support-on-aws/20079/4)
  + → みんなRustからAWS API叩こう
* 初見だと圧倒される
  + →
===

![Rusotoのサービス一覧](/images/rusoto/Rusotoのサービス一覧.png) <!-- .element:  height="480px" -->

===

ひとまずS3だけみてみる


===

![Rusoto S3の定義一覧](/images/rusoto/Rusoto_S3の定義一覧.png) <!-- .element:  height="480px" -->

===
# Rusotoの使い方
-----------------

* 一見すると手の付け所が分からない
* 基本を押さえれば簡単
  1. rusoto_core は必ず使う
  2. 使いたいサービスに合わせて他のクレートを選ぶ
  3. クレートのトレイトとクライアントを使う
* やってみよう

===
# RusotoでS3
-------------

* お題: S3のバケットを作ってオブジェクトを置いてそれを取得してみる
* 使うのは[rusoto_core](https://rusoto.github.io/rusoto/rusoto_core/)と[rusoto_s3](https://rusoto.github.io/rusoto/rusoto_s3/)
* AWSのクライアントの設定は各自で


===
# RusotoでS3
-------------
## ステップ1

* プロジェクトを作る

``` console
$ cargo new rusoto-example-s3
     Created binary (application) `rusoto-example-s3` package
$ cd rusoto-example-s3
$ cargo add rusoto_core rusoto_s3
      Adding rusoto_core v0.36.0 to dependencies
      Adding rusoto_s3 v0.36.0 to dependencies
```

===
# RusotoでS3
-------------
## ステップ2

* クライアントを作る

``` rust
use rusoto_s3::{S3Client, S3};
fn main() {
    let client = S3Client::new("ap-northeast-1".parse().unwrap());
}
```

===
# RusotoでS3
-------------
## ステップ2
ポイント
* サービス毎にクライアントとトレイトが定義されてる
  + `S3Client` - 実体。ほぼ `S3` を実装するためのもの
  + `S3` - S3への操作の全て
* テストしやすい構造


===
# RusotoでS3
-------------
## ステップ3

* クライアントを使う

``` rust
use rusoto_s3::{CreateBucketConfiguration, CreateBucketRequest};
client
    .create_bucket(CreateBucketRequest {
        bucket: bucket.clone(),
        create_bucket_configuration: Some(CreateBucketConfiguration {
            location_constraint: Some("ap-northeast-1".into()),
        }),
        ..Default::default()
    })
    .sync()
    .expect("create bucket failed");
```


===
# RusotoでS3
-------------
## ステップ3
ポイント
* API単位で型が定義されている(自動生成)
  + O 型から使い方がわかる
  + X 型定義多すぎ
* 必須パラメータ以外は `Option` だから デフォルト値で埋める
  + [構造体更新構文](https://doc.rust-lang.org/book/ch05-01-defining-structs.html#creating-instances-from-other-instances-with-struct-update-syntax)は知ってるかな？
  + `..Default::default()`
* 返り値は `Future` だから必要に応じて `sync` しよう


===
# RusotoでS3
-------------
## ステップ4
* 完成

``` rust
use rusoto_s3::{
    CreateBucketConfiguration, CreateBucketRequest, GetObjectRequest, PutObjectRequest,
};
use rusoto_s3::{S3Client, S3};
use std::io;

fn main() {
    let client = S3Client::new("ap-northeast-1".parse().unwrap());

    let bucket = "test.blacekenedgold".to_string();
    client
        .create_bucket(CreateBucketRequest {
            bucket: bucket.clone(),
            create_bucket_configuration: Some(CreateBucketConfiguration {
                location_constraint: Some("ap-northeast-1".into()),
            }),
            ..Default::default()
        })
        .sync()
        .expect("create bucket failed");
    client
        .put_object(PutObjectRequest {
            bucket: bucket.clone(),
            key: "test".into(),
            body: Some("κeenさんだよー ⸜( ¯⌓¯ )⸝ ".to_string().into_bytes().into()),
            ..Default::default()
        })
        .sync()
        .expect("put object failed");

    let out = client
        .get_object(GetObjectRequest {
            bucket: bucket.clone(),
            key: "test".into(),
            ..Default::default()
        })
        .sync()
        .expect("get object failed");
    if let Some(body) = out.body {
        let mut body = body.into_blocking_read();
        let mut out = io::stdout();
        io::copy(&mut body, &mut out).expect("output failed");
    }
}

```


===
# RusotoでS3
-------------
## ステップ4

``` console
$ cargo run
   Compiling rusoto-example-s3 v0.1.0 (/home/shun/Rust/rusoto-example-s3)
    Finished dev [unoptimized + debuginfo] target(s) in 2.61s
     Running `target/debug/rusoto-example-s3`
κeenさんだよー ⸜( ¯⌓¯ )⸝
```


===
# まとめ
--------

* RustからAWSのAPIが叩けるRusotoがあるよ
* Rusotoは一見複雑だけど基本を押さえたら難しくないよ
* Rusotoをみんな使うと公式サポートくるかもね

===
# 次回予告
----------

* AWS LambdaをRustで書く完全ガイド
* @[Rust LT #3 ](https://rust.connpass.com/event/122377/) (3/25)



</script>
</section>
