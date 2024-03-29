---
categories: [Rust]
date: 2017-03-22T19:57:28+09:00
description: "Rustプログラマミートアップでの発表用"
title: Rust in Production
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Rust in Production
----------------------
[Rust プログラマーミートアップ / Rust programmers' meetup - connpass](https://rust.connpass.com/event/49304/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * 若者
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# Rust採用までの流れ
--------------------

Ω＜「κeen君継続的ベンチマークサーバ作って」<!-- .element: style="color:#a00;text-align:left;" -->  
Ω＜「GitHubにpushする度ベンチマーク走る感じで」<!-- .element: style="color":#a00;text-align:left;" -->  
Ω＜「GPUとRaspberry Piでベンチマークするから」<!-- .element: style="color:#a00;text-align:left;" -->  
Ω＜「全部任せるね」<!-- .element: style="color:#a00;text-align:left;" -->  

⸜( ¯⌓¯ )⸝「Rustで書くか」<!-- .element: style="color:#00a;text-align:left;" -->  


やったこととか知見とかトピック毎にかいつまんで話します

<!-- .slide: class="left" -->

===
# Rustのメリット
---------------

* κeenが馴れてる
* 社員の半分くらい書ける
* Linux, Mac, Raspberry Piで動く
* テスト書かなくてもそんなにバグらなそう
* 将来のために小さいところでノウハウ溜めていきたい
* シングルバイナリで楽に動きそう
* **単純に使いたかった**
* ※今回は速度や安全性は必要ない

===
# コンポーネント
---------------

* CLIクライアント
  + 各自の手元やCIサーバから起動
* アグリゲータ
  + ワーカに仕事を投げ、結果を受け取る
  + ジョブ管理
* ワーカ
  + GPUマシンやRSPiなどでベンチマーク
  + 結果をアグリゲータに返す
* ブラウザ
  + ベンチマーク結果の閲覧

===
# プロジェクト構成
-------------------

* [ワークスペース](http://doc.crates.io/manifest.html#the-workspace--field-optional)を使ってサブプロジェクトに分割
  + モノリシックだとビルドが重かった
  + targetは全てのプロジェクトで共有する
* CLI, aggregator, worker, browserの他にmodel, api
* modelはaggregatorとbrowserで共有
* apiはaggergatorとcliとworkerで共有
* コード

===

# プロジェクト構成
-------------------


```
Cargo.lock
Cargo.toml
README.md
bench_aggregator/
bench_api/
bench_browser/
bench_cli/
bench_model/
bench_worker/
build.sh
circle.yml
docker-compose-deploy.yml
docker-compose.yml
migrations
rustfmt.toml
src/
static/
target/
```

===
# プロジェクト構成
-------------------

![構成のイメージ](/images/rust-in-production/composition.png)<!--  width="100%" -->


===
# パーサ
--------

* workerで使う
* timeやgprofの結果を数値で欲しい
* →パースするか…
  + フォーマット
* timeはbash固定
  + シェルコマンドなのでシェル毎にフォーマットが違う
* gprofは`-b`オプションでの結果をパース
  + gmon.outではない

===
# パーサ
--------

* パーサコンビネータ [nom](https://github.com/Geal/nom)
* サクっと作れた
* デバッグつらい
  + gprofが思ったより変なフォーマットしてた
  + nomがバイト指向なのと位置を保存しない
  + 自力でどうにかする？
  + [combine](https://github.com/Marwes/combine)使うか手書きにするかで書き直したい
* コード

===

# CLIオプション
---------------

* workerとcliで必要
* 多機能な[clap](https://github.com/kbknapp/clap-rs)を採用
* 特に困らない
* 採用プロジェクトも多くてliving exampleに事欠かない
* CLIツールに思考停止で採用していいと思う
* コード

===
# HTTPクライアント
------------------

* CLIとworkerで使う
* [hyper](https://github.com/hyperium/hyper) vs [reqwest](https://github.com/seanmonstar/reqwest)
* 非同期な必要はないのでreqwest使いたかった
* reqwestにmultipart対応がない→生hyper+[multipart](https://github.com/abonander/multipart)
* コネクションの管理とかjsonの扱いとかそこそこ面倒だった…
* reqwestにmultipart対応入ったら乗り換えるかも

===
# HTTPサーバ
------------

* aggregatorとbrowserで使う
* 簡単なREST API
* 生hyper vs [nickel](https://github.com/nickel-org/nickel.rs) vs [iron](https://github.com/iron/iron)
* 生hyperはすぐつらくなる→やめた
* **nickelとironはそんなに変わらなかった**
* nickel(agregator)+iron(browser)
  + aggregatorはなんとなくでnickel選んじゃった
  + browserは結局[rustc-perf](https://github.com/rust-lang-nursery/rustc-perf)からフォークしたためironだった

===

# HTTPサーバ(Nickel)
------------

* サーバデータの扱いがちょっと特殊？
  ```rust
    let data = req.server_data();
  ```
* 400 BadRequestと500 InternalServerErrorのマクロ作った
  + `validate!(res, serde_json::from_str(field));`
  + `server_try_with!(res, serde_json::to_string(&ret));`
  + 「エラー出たら処理終了」が他の言語と比べて書きづらい？
* 非同期IOしない…
  + ポーリングでコネクション張りっぱなしなのでクライアント分スレッドが必要（ハマった）
* マルチスレッド→DBコネクションにロックが必要
* コード

===
# APIモデル
-----------

* apiクレートを分けた
  + バイナリ毎に別クレートに分けるために必要だった
* [serde](https://github.com/serde-rs/serde)と[serde_json](https://github.com/serde-rs/json)でjson化
* proc-macroがstableになって格段に使いやすくなった
* モジュール分けて`Request`と`Response`を作ってAPIが分かりやすく
* コード

===
# DB
-----

* aggergatorとbrowserで必要
* ほぼ[diesel](https://github.com/diesel-rs/diesel)一択
  + コネクション、マイグレーション、ORMなど
  + proc-macroで大分楽に
* [仕事でdiesel使ってみた | κeenのHappy Hacκing Blog](https://keens.github.io/slide/shigotodedieseltsukattemita/)
* DBはPostgreSQL
  + 半分dieselのため、もう半分機能のため
* 開発時はdocker-composeで立てる
  + CF [docker-composeでmysql & postgreSQL をサクッと起動 - Qiita](http://qiita.com/astrsk_hori/items/1e683a7a2f2b7189cb6e)

===
# diesel
-----

* Pros
  + SQLでスキーマ書ける
  + マイグレーションの面倒みてくれる
  + proc-macroのおかげで楽
  + クエリDSL案外普通に書けた
  + 最悪SQL生牡蠣出来る
  + CLI便利

===
# diesel
--------

* Cons
  + deriveいっぱいあって難しい
    - has_manyとbelongs_toあたりをどう書くか
  + enum扱えない
  + DSLで2重join書けない
  + 偶に機能足りない
    - ネットワークアドレス型サポートとか
  + SQL生牡蠣の型付けつらい
  + has_manyの扱いに難あり
    - eager loadingしない設計判断
    - `(User, Vec<Tweets>)` みたいなのを持ち回す羽目に
    - モデル設計にも影響

===
# マイグレーション
-----------------

* 普通のマイグレーション運用かな
* 最初のデプロイまでは`diesel database reset`で何度も初期化した
* 最初デプロイ後は手元で`diesel migration run`と`diesel migration revert`を何度か
* デプロイ時に雑に手元から本番サーバに `diesel migration run`
  + 本当はデプロイプロセスに組込みたい
  + diesel CLIの入ったdockerコンテナ…
* まだほとんど使われてないので雑運用


===
# マイグレーション(コード)
-----------------

* スキーマ変更したらコードの変更も必要
  + そもそも必要だからスキーマ変更するので割と自然に出来る
* `infer_schema` があるので先に開発環境のマイグレーションをしないといけない
* カラムの順番とフィールドの順番が一致する必要
  + 地味に面倒
  + 今後改善予定 CF [#747](https://github.com/diesel-rs/diesel/pull/747)
* SQL生牡蠣の対応がつらい
  + コンパイル時にカラム不足が分からない

===
# エラー管理
------------

* 全ての場所で必要
* [error_chain](https://github.com/brson/error-chain)を採用
* 公式ドキュメントの[エラーハンドリング](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/error-handling.html)のボイラプレートをマクロで生成
* `?`でのエラー処理が楽になる→ `unwrap`が消えた
* 全てのプロジェクトに思考停止で採用していいんじゃないかな

===

# ログ
------

* 全ての場所で必要
* nurseryの[log](https://github.com/rust-lang-nursery/log)と[env_logger](https://github.com/rust-lang-nursery/log)を採用
* `println!`より高機能な（on/offが出来る）ので`println!`の代わりに`debug!`を入れる
* あとたまにエラーハンドリングで`error!`使ったり
* `main`で一行`env_logger::init().expect("failed to init env logger");`を忘れずに
* 運用以前に開発に便利なので問答無用で入れていいと思う

===
# コンフィグ
------------

* [serde_yaml](https://github.com/dtolnay/serde-yaml) + [dotenv](https://github.com/slapresta/rust-dotenv)
* コンフィグファイルといえばyamlだよね
  + ちょこっといじって再起動がやりやすい
* dotenvはdockerと相性がいい気がする
* どっちがいいか判断出来なかったので両方実装
* プロダクションで動いてるのはdotenv（というか普通の環境変数）の方

===

# テスト
--------

* 全ての場所で必要
* 組込みの`#[test]`を使用
* 正直あまり出来てない
* 型強いし多少はなくてもバグらない…？
* 一応レイヤ(DB/ビジネスロジック/インタフェース)を跨ぐときにはtraitで抽象化
  + 出来てないところもいっぱい
* 今後の課題とさせて頂きます

===
# デバッグ
----------

* 基本はlogの`debug!`でトレース
* ちゃんと型つけときゃバグらない
* GDB使う…？
* デモ

===

# ビルド
--------

* cargoでサブプロジェクト全てをビルドするコマンドがなかった
  + 最近 `cargo build --all` が入った
* Raspberri Pi向けにクロスコンパイルが必要
* シェルスクリプトでビルドスクリプト
* クロスコンパイルはdockerを使う CF [dlecan/rust-crosscompiler-arm: Docker images for Rust dedicated to cross compilation for ARM v6 and more](https://github.com/dlecan/rust-crosscompiler-arm)
* 正解なのか分からない

===
# CI
----

* 全てのクレートで必要
* Circle CIを使ってる
* 普通にrustupでRustのインストール
  + travisならRustサポートあるけど…
* 普通に `cargo test --all`

===

# デプロイ
----------

* シングルバイナリだけど…
* シュっとdocker-swarmとかでデプロイしたい
* dockerイメージ作ってdocker-composeで管理
  + 開発も便利に
* raspberry piはバイナリだけ配る
* browserは`static/`もイメージに同梱

===
# Docker
--------

* ベースイメージはUbuntu
  + libcの扱いが面倒そうなのでalpineはやめといた
  + イメージサイズに困ってからどうにか考える
* swarmなのでdocker-composeの設定そのままで動かせる
* ログの管理もswarm任せ
  + env_loggerが標準出力に吐くのもそんなに困ってない

===
# Docker
--------

* rustのビルドからのdockerのビルドが面倒
* Dockerfileの管理も面倒
* 便利なの作った
* [KeenS/cargo-pack-docker: a cargo subcommand to package your application into a docker image](https://github.com/KeenS/cargo-pack-docker)
* DockerfileなしでRustのビルドからイメージ作成までやってくれる
* 今のところ使えてる
  + 作りが甘いことは認識してる…

===
# まとめ
---------

* Rustを仕事で使ったときの知見を話した
* 微妙に痒いところに手が届かないけど概ね揃ってる
* 開発が速いので問題は放置しとくと勝手に直るかも
* テストとかデプロイとかまだ知見が足りない


</textarea>
