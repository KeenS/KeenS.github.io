---
categories: ["Rust", "ISUCON"]
date: 2018-09-02T05:53:50+09:00
title: "isucon7予選のアプリをRustに移植したから解説するね"
---
κeenです。こういう流れがあったので移植しました。
<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">keenさんが「2028年、Rust でイケてる web application framework はこれだ！」みたいな記事を書くとと &quot;余力&quot; が生まれるかもしれません…！！</p>&mdash; FUJI Goro (@__gfx__) <a href="https://twitter.com/__gfx__/status/1034362116633812992?ref_src=twsrc%5Etfw">2018年8月28日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

<!--more-->
まず[ISUCON](http://isucon.net/)を知らない方に雑に説明しておくと、意図的に遅く作られたWebアプリケーションが与えられるので7時間くらいでどれくらい高速化できるかを競うコンテストです。
このお題のWebアプリケーションが参加者や流行りに合わせて複数言語で提供されるのですが、今年はRustが来そうだということで参加者の肩慣らしのために過去問を移植しましたというお話。

ひとまずソースコードは [こちら](https://github.com/KeenS/isucon7-qualify)

手元でベンチマークをしてみた限り、Pythonの2倍くらいは速いもののGoには劣るようでした。これの考察については後で書きますが、1つ注意してほしいのは実際の予選では1コアマシンが複数与えられたそうですが手元では16コア/32スレッドマシンでベンチマークを取っているので実戦でのスコアを反映するものではありません。

使い方はREADMEを見てもらうとして、以下は主にライブラリやコード例などを解説します。

# ライブラリ
## RustのWebアプリケーションフレームワーク
出来る限り非同期フレームワークを使いたいですが、今Rustの非同期は丁度バタバタしている領域なので決定版といえるものがありません。

今回検討したというか実際に途中まで書いてみたのは以下の3つ

* [tower-web](https://github.com/carllerche/tower-web) - mioやtokioなどRustの非同期ライブラリの大本をやっているcarllerche氏によるフレームワークです。使い勝手もよく期待が持てそうでしたがまだ若く、必要なライブラリが足りなかった(具体的にはセッションサポートがなかった)のでやめました。
* [Gotham](https://github.com/gotham-rs/gotham) - 設計上パフォーマンスが出そうと踏んでいたのですがDBコネクションの持たせ方が分からなかったので諦めました。調べ方が悪かったのかもしれません。
* [actix-web](https://github.com/actix/actix-web) - actorフレームワークの上に乗っかったHTTPフレームワークですがactorを無視して使うこともできます。普段の仕事でも使っていますし無難にこれを使いました。


他にもあるかと思いますが検討できていないです。他社でもactix-webの採用事例を聞くので多分外してないと思います。

## その他のライブラリ
### sha-1
今SHA-1を扱いたいなら[RustCryptoプロジェクト](https://github.com/RustCrypto)の[`sha-1`](https://github.com/RustCrypto/hashes/tree/master/sha1)になると思います。気をつけてほしいのはcrates.ioには`rust-crypto`クレートも `sha1` クレートもありますがどちらも別物です、 `sha-1` を使いましょう。私は両方とも踏みました。

### テンプレートエンジン
あまりRustでテンプレートエンジンを扱う話を聞かないので決定版がわかりません。
一応[`handlebars`](https://github.com/sunng87/handlebars-rust)が人気のようですし、私もよく使っているのでそれを採用しました。

速度や他のエンジンと比べたときの使いやすさはわかりません。みんなが使っている安心感があります。

### JSON

[`serde_json`](https://github.com/serde-rs/json)一択です。

### MySQL
これは迷いました。普段なら[`disel`](http://diesel.rs/)を使いますがISUCONではそこまで大げさなものは必要にならないので適当に見つけた[`mysql`](https://crates.io/crates/mysql)を使いました。
兄弟ライブラリに[`mysql_async`](https://crates.io/crates/mysql_async)もあるようですがこちらはまだ試せてないです。

余談ですがこのライブラリ、MySQLのプロトコルを自前で実装しているので`libmysqlclient`に依存しません。

### エラー
actix-webを使うと自動的に[`Failure`](https://github.com/rust-lang-nursery/failure)を使うことになります。
移植元のコードも例外は全然気にせず書いてますしこちらもブラックホールのようにエラーを全部`Failure`に投げ込むことにしました。

# コード

Rubyのコードを見ながら移植しました。

コード全体を `Result` を使って書くか `Future` を使って書くか迷いましたが `Result` にしました。

[`mdo!`](https://github.com/TeXitoi/rust-mdo) などの代用品はあるものの `?`記法が使えないのは結構辛かったです。

## actix-webの基本的な使い方

まず、DBコネクションなどを保持するデータ型を定義します。

```rust
#[derive(Clone)]
struct Isu {
    pool: my::Pool,
    templates: Arc<Handlebars>,
}
```

Rubyでいう `App` クラスに近い役割を果たします。`App`という名前はフレームワーク側で既に使われているので `Isu` にしました。

そしてハンドラはこう書きます。Sinatraに雰囲気を併せるためにクロージャでハンドラを書きます。

```rust
fn app(isu: Isu) -> App<Isu> {
    let mut app: App<Isu> = App::with_state(isu);

    // ...

    app = app.route(
        "/initialize",
        Method::GET,
        |state: State<Isu>| -> Result<HttpResponse, Error> {
            state.exec_sql("DELETE FROM user WHERE id > 1000", ())?;
            state.exec_sql("DELETE FROM user WHERE id > 1000", ())?;
            state.exec_sql("DELETE FROM image WHERE id > 1001", ())?;
            state.exec_sql("DELETE FROM channel WHERE id > 10", ())?;
            state.exec_sql("DELETE FROM message WHERE id > 10000", ())?;
            state.exec_sql("DELETE FROM haveread", ())?;
            Ok(http_status(204))
        },
    );
   //...
}
```

actix-webはハンドラの引数にほしいものを書いたら自動で渡してくれるタイプのフレームワークです。
`State<Isu>` がRubyの `App` 内での `self` に近い存在です。 `Deref<Target = Isu>` を実装しているので `Isu` のメソッドがそのまま使えます。


パスパラメータ、クエリパラメータなどの取り出しは一旦型を定義してあげて

```rust
#[derive(Deserialize)]
struct ParamChannelId {
    channel_id: u64,
}

```

以下のようにハンドラの引数に `Path<ParamChannelId` と書いておくと `channel_id` という名前のプレースホルダから値を取得してくれます。


```rust
    app = app.route(
        "/channel/{channel_id}",
        Method::GET,
        |state: State<Isu>,
         session: Session,
         path: Path<ParamChannelId>|
         -> Result<HttpResponse, Error> {
           // ...
         }
     );
```

型を定義する手間はありますが `#[derive(Deserialize)]` のようにメタプログラミングで色々やってくれるメリットもあるので一長一短です。

これでフレームワークは大体使えると思うのであとは書いていくだけです。



## JSON

マクロがあるのでjsonをそのまま書けます。

``` rust
json!({
    "user": user,
    "channels": channels
})
```

## エラー

あらゆるエラーを一旦Failureのエラーに潰してからactix-webのエラーに変換する関数です。
どんなエラーが来ても`.map_err(err)`で処理できるようになります。


``` rust
fn err(e: impl ::failure::Fail) -> Error {
    let e: FailureError = e.into();
    e.into()
}
```

エラーハンドリングをまともにしないISUCON用のものなのであまり真似しないで下さい。


## マルチパート

アイコンの扱いのところでマルチパートが出てきます。
`actix-web` は一応使えないことはないくらいのサポート具合でしたのでかなりつらい対応になりました。
[`MultipartItem`](https://actix.rs/api/actix-web/stable/actix_web/multipart/enum.MultipartItem.html)などのほぼプロトコルそのままマッピングしたデータ型を扱います。
`POST /profile` のハンドラだけ異様な形をしていますが半分がマルチパートサポートの貧弱さのせい、もう半分が非同期プログラミングのせいです。

## DB

まずテープルに対応するデータ型を定義して

``` rust
#[derive(Debug, Clone, Serialize, Deserialize)]
struct User {
    id: u64,
    name: String,
    salt: String,
    password: String,
    display_name: String,
    avatar_icon: String,
    created_at: NaiveDateTime,
}

```

DBから取得したデータとのマッピングを書いて

``` rust
impl FromRow for User {
    fn from_row(row: my::Row) -> Self {
        Self::from_row_opt(row).expect("failed to deserialize data")
    }

    fn from_row_opt(row: my::Row) -> Result<Self, my::FromRowError> {
        FromRow::from_row_opt(row).map(
            |(id, name, salt, password, display_name, avatar_icon, created_at)| Self {
                id,
                name,
                salt,
                password,
                display_name,
                avatar_icon,
                created_at,
            },
        )
    }
}

```

使うのは1行です

``` rust
fn db_get_user(&self, user_id: u64) -> Result<Option<User>, Error> {
    self.first_sql("SELECT * FROM user WHERE id = ?", (user_id,))
}
```

DBとデータとのマッピングはフルスタックのORMならメタプログラミングで自動生成してくれるのですがこれは軽量ライブラリなので手書きのようです。

また、データ型の定義が面倒ならタプルで取り出す方法もあります。


``` rust
let (name, display_name, avatar_icon): (String, String, String)
  = state
    .first_sql("SELECT name, display_name, avatar_icon FROM user WHERE id = ?", (message.user_id,))
    .map(|opt| opt.expect("application reached inconsistent state"))?;
```


## Rubyとの比較とか

Rubyから移植したのでRubyっぽいコードになってます。
Rustのお手本的コードは無駄がなく速いコードになるんですがそもそも遅いアプリケーションがお題なのでどこまで効率的に書くか悩ましかったです。

Rustはコストやリスクが目に見える言語です。

たとえばRubyの

```ruby
pass_digest = Digest::SHA1.hexdigest(salt + password)
```

というコードはRustではほぼ直訳して


``` rust
let pass_digest = format!("{:x}", Sha1::digest_str(&(salt.clone() + password)));
```

としています。しかし `salt.clone()` のようにデータをコピーしていたりそもそも結合する必要のない文字列を結合していたりしてあまりよろしくないです。


あるいはRubyの

``` ruby
statement = db.prepare('SELECT name, display_name, avatar_icon FROM user WHERE id = ?')
statement.execute(row['user_id']).first
```

というコードはRustでは

``` rust
state.first_sql(
        "SELECT name, display_name, avatar_icon FROM user WHERE id = ?",
        (message.user_id,),
    )
      .map(|opt| opt.expect("application reached inconsistent state"))?;
```

と翻訳しています。 `opt.expect("application reached inconsistent state")` とリスクが目に見える形になっています。


上記のように基本的にRustで書くとRubyより冗長になるのですが案外Rustの方が短いケースもあります。


rubyのこのコードは


``` ruby
description = ''
channels.each do |channel|
  if channel['id'] == focus_channel_id
    description = channel['description']
    break
  end
end
```

このように翻訳されます。

``` rust
let description = channels
    .iter()
    .find(|ch| Some(ch.id) == focus_channel_id)
    .and_then(|ch| ch.description.clone())
    .unwrap_or_else(|| "".into());
```

# パフォーマンスとか

先述のとおり初期状態でPythonより速くてGoより遅かったです。
N+1クエリが仕込まれてるので最初はアプリケーションの速さはあまり問題にはならなくて、ほぼDBの速度で決まります。
そんな中Goだとgoroutineでブロッキングする部分を上手く分離できるので効率が良かったんじゃないかなと推測します。
Rustも`mysql_async`を使ったら速くなるかもしれません。

しかしそんなことより普通にN+1クエリを解消してインデックスを張ってDBを速くするのであまり初期スコアには意味がないと思います。
DBアクセスを非同期にするのはまずは筋が悪い部分を消してからでしょう。


# Rustで出るチームへのアドバイス
私は参加登録してないので好き放題言えます。

言語の基本性能ではRustはGoよりは速いはずなのである程度アプリケーションにボトルネックが移ったらRustの方が有利になる可能性があります。
競技中にそこまでボトルネックが移らない可能性も十分にあります。

`cargo build` に `--release` を付け忘れないようにしましょう。

[`cargo watch`](https://github.com/passcod/cargo-watch)でソースが更新されたらビルドされるようにしておくと古いバイナリを見ることがないかも?

非同期コードを書く時はnightlyを使って`async`/`await`で挑む手があるかもしれません。少なくとも生のFutureだとかなりつらいので何かしらを手を用意した方がよさそうです。

`cargo build` に `--release` を付け忘れないようにしましょう。

普段使わないところはライブラリ選びからになるので一通り肩慣らししておくといいと思います。

クロスコンパイルはどうせハマるので大人しくサーバでコンパイルした方がいいと思います。

`cargo build` に `--release` を付け忘れないようにしましょう。

# 移植してみた感想

思ったより大変でした。マルチパートのユーティリティを除いても900行オーバーのアプリケーションになりました。Goが750行くらいなので20%くらい長いですね。

実装もそうですがライブラリの選定で苦労しました。
一回実装して中途半端に使いづらくて別ライブラリで実装し直したりしてました。普段扱わないことやると大変ですね。

因みにRustで書きあがった後ベンチマーカが完走するまでに出たバグは5,6個(種類)でした。
ほとんどがhandlebarsの記法由来で、Rust側ではほとんどバグはなかったです。
こういう点は静的型付き言語の良いところでしょうか。


# 最後に
とりあえずで実装したので荒いコードですが皆様是非練習にお使い下さい。
