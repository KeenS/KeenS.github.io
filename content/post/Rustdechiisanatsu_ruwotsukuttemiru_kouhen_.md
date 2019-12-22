---
categories: [Rust, Advent Calendar, Advent Calendar 2015, Rust Advent Calendar]
date: 2015-11-29T13:54:39+09:00
title: Rustで小さなツールを作ってみる(後編)
---

この記事は[Rust Advent Calendar 2015](http://qiita.com/advent-calendar/2015/rust-lang) 2日目の記事です。  
前 [Rustで小さなツールを作ってみる(前編)](/blog/2015/11/14/rustdechiisanatsu_ruwotsukuttemiru/)  
次 [Cargoの使い方](/blog/2015/11/29/cargonotsukaikata/)

κeenです。昨日の記事で作りかけだったIRCの生ログのDBへのインポートの続きです。今日はDBへのインポートをやります。

<!--more-->

とはいってもRustでDBを扱った経験がないので探り探りです。

# 要件
今回は簡単なのでORMは要らない気がしますが、サンプルということでORMも使いましょう。さらに、サンプルということでクエリビルダも使いましょう。

…と思ったのですが中々良いライブラリがありませんでした。ドキュメントもろくにないまま放置されてる[deuterium](https://github.com/deuterium-orm/deuterium)と開発途中でまだpostgresしかサポートされていない[rustorm](https://github.com/ivanceras/rustorm)/[codegenta](https://github.com/ivanceras/codegenta)くらいしかないようです。うぅ。
最近話題になった[diesel](https://github.com/sgrif/diesel)もまだ若すぎるようです。

ということでシンプルなDBIを使います。そろそろ牡蠣の美味しい季節ですしSQL生書きしましょう。当たると怖いですが。で、テスト用にsqlite3を、（今回は使いませんが）本番用（？）にMySQLを使える奴を捜しましょう。

# ライブラリ捜し
まずは[crates.io](https://crates.io)を捜します。"sqlite"で捜せばいいでしょうか。

MySQLとSQLite3をサポートしているライブラリ…[r2d2](https://github.com/sfackler/r2d2)がそれのようです。
コネクションプールのライブラリなので何か違う気がしますが、mysqlもsqlite3も扱えて、結果コネクションを抽象化してくれるので大体大丈夫です。

蛇足ですが、名前はJavaにc3p0というコネクションプールがあるのでそこから来ているのでしょう。

# スキーマ定義
SQLiteを使い慣れないのですが大したことやってないので大丈夫でしょう。`DATETIME` 型がないのが苦しいですね。

自然キーが文字列や複合キーなのでサロゲートキーを使って自然キーにUNIQUE制約をかけてますがちょっと気持悪いですね。

``` sql
-- _*_ mode: sql; sql-product: 'sqlite _*_

CREATE TABLE servers(
        id   INTEGER PRIMARY KEY,
        name  TEXT NOT NULL UNIQUE,
        address TEXT
        );
m
CREATE TABLE channels(
        id INTEGER PRIMARY KEY,
        server_id INTEGER REFERENCES servers(id) ON UPDATE CASCADE,
        name TEXT NOT NULL,
        UNIQUE(server_id, name)
        );

CREATE TABLE entries(
        id INTEGER PRIMARY KEY,
        channel_id INTEGER REFERENCES channels(id) ON UPDATE CASCADE,
        user_id INTEGER REFERENCES users(i) ON UPDATE CASCADE,
        type TEXT NOT NULL,
        body TEXT NOT NULL,
        created_at TEXT NOT NULL
        );

CREATE TABLE users(
        id INTEGER PRIMARY KEY,
        server_id INTEGER REFERENCES servers(id) ON UPDATE CASCADE,
        name TEXT NOT NULL,
        UNIQUE(server_id, name)
        );
```

これをetc/schema.sqlに保存します。

# モデル定義
折角シンプルなツールを作っているのでモデルは使わない。

# コネクションまで
あらかじめ

```
$ sqlite test.db < etc/schema.sql
```

としてDBを作っておきましょう。

Cargo.tomlのdependenciesには

``` toml
r2d2_sqlite = "0.0.3"
r2d2 = "0.6.1"
rusqlite = "0.4.0"
```

を追記。

src/main.rsの先頭部分に

``` rust
extern crate r2d2;
extern crate r2d2_sqlite;
extern crate rusqlite;
use std::sync::Arc;
use r2d2_sqlite::SqliteConnectionManager;
use rusqlite::SqliteError;
```

を追記しましょう。

そしてmainの中に次を追加。


``` rust
    let manager = SqliteConnectionManager::new("test.db").unwrap();
    let config = r2d2::Config::builder().pool_size(16).build();
    let pool = Arc::new(r2d2::Pool::new(config, manager).unwrap());
```

これで一旦コンパイル。

```
$ cargo build
```

これが通れば良いです。


# はじめての挿入

INSERTを発行してみましょう。`on_channel_dir`の引数を変える必要があります。

``` rust
fn on_channel_dir(path: &Path, pool: Arc<r2d2::Pool<SqliteConnectionManager>>) 
```

併せて、呼び出しも

``` rust
        on_channel_dir(&path, pool.clone());
```

となります。

そうしたら`on_channel_dir`の中で

``` rust
    let channel = &dirname[..at];
    let server = &dirname[at+1..];
    println!("{} at {}", channel, server);
```

としていた辺でまずはserversへのインサートを発行しましょう。

``` rust
    let conn = pool.get().unwrap();
    let server_id = match conn.execute("INSERT INTO servers (name) VALUES ($1)", &[&server]) {
        // unique constraint failed
        Err(SqliteError{code: 19, message:_}) => conn.query_row("SELECT id FROM servers WHERE name = $1", &[&server], |r| r.get(0)).unwrap(),
        Ok(_) => conn.last_insert_rowid(),
        e => {e.unwrap(); return}
    };
```

プールからコネクションを持ってきて`INSERT`を発行します。成功するか既にUNIQUEな名前が存在していて失敗したらサーバのidを持ってきます。それ以外の失敗だったらそのままパニックしていいでしょう。

しかしラッパがちょっと雑すぎやしませんかね。エラーステータスくらいenumでラップして欲しかった。

同じくチャネルもインサートしましょう。


``` rust
    let channel_id = match conn.execute("INSERT INTO channels (name, server_id) VALUES ($1, $2)", &[&channel, &server_id]) {
        // unique constraint failed
        Err(SqliteError{code: 19, message:_}) => conn.query_row("SELECT id FROM channels WHERE name = $1", &[&channel], |r| r.get(0)).unwrap(),
        Ok(_) => conn.last_insert_rowid(),
        e => {e.unwrap(); return}

    };
```

こういうupsertというかinsert or get_idってどうやるのが正解なんでしょう。DB力が低い…

# エントリのインサート
同じく`on_log`の引数を増やします。


``` rust
fn on_log(log: &Path, channel_id: i64,  pool: Arc<r2d2::Pool<SqliteConnectionManager>>) {
```

呼び出し側も。

``` rust
        let pool_ = pool.clone();
        Builder::new().name(pathname).spawn(move|| on_log(&log, channel_id, pool_))
```

`move`する前に`clone`しないとダメですね。所有権難しい。

さて、今度は色々することがあります。
まず、dateとtimeをくっつけてdatetimeにしましょう。timeがイミュータブルだったので

``` rust
        let mut time = ...
```

とし、その下で

``` rust
        time.tm_mday = date.tm_mday;
        time.tm_mon = date.tm_mon;
        time.tm_year = date.tm_year;
        let created_at = time.to_timespec();
```

とします。わざわざ`to_timespec`で変換してるのは`Timespec`だとDBライブラリが良い感じに変換してくれるからですね。


次にメッセージの抜き出し部分も値を返すように変更。

``` rust
        let (user, type_, body) = match &msg[0..1] {
            "!" => ("server", "sysmsg", &msg[1..]),
            "+" => ("server", "join", &msg[1..]),
            "-" => ("server", "part", &msg[1..]),
            "<" => match msg.find('>').map(|e| (&msg[1..e], &msg[e+1..])) {
                Some((user, body)) => (user, "msg", body),
                None => {
                    warn!("cannot parse the entry; skipping");
                    continue;
                }
            },
            _ => ("server", "notice", &msg[1..]),
        };
```

ここまで来たらあとはユーザとエントリをインサートするだけですね。

``` rust
        let conn = pool.get().unwrap();
        let user_id = match conn.execute("INSERT INTO users (name) VALUES ($1)", &[&user]) {
            // unique constraint failed
            Err(SqliteError{code: 19, message:_}) => conn.query_row("SELECT id FROM users WHERE name = $1", &[&user], |r| r.get(0)).unwrap(),
            Ok(_) => conn.last_insert_rowid(),
            e => {e.unwrap(); return}
        };
        conn.execute("INSERT INTO entries (channel_id, user_id, type, body, created_at) VALUES ($1, $2, $3, $4, $5)", &[&channel_id, &user_id, &type_, &body, &created_at]);
```

一旦これで走らせてみましょう。


```
$ cargo run --release
...
thread '/home/kim/log/#hongo@utmc/2015-02-15.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-08-18.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-12-13.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-10-13.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-02-26.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-05-23.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-04-30.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2013-10-18.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-10-14.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-01-19.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-08-12.txt' panicked at 'called `Result::unwrap()` on an `Err` value: GetTimeout(())', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-06-03.txt' panicked at 'called `Result::unwrap()` on an `Err` value: GetTimeout(())', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-02-05.txt' panicked at 'called `Result::unwrap()` on an `Err` value: GetTimeout(())', ../src/libcore/result.rs:736
...
```

あちゃー。マルチスレッドでINSERTしてるのでロックが掛かってますね。SQLiteはファイルロックしか持たないので並列インサートはつらそう。

# トランザクション
はい、そういう時のトランザクションですよ。


最後のユーザやエントリーをインサートしてる部分で使ってみます。

``` rust
        let conn = pool.get().unwrap();
        let trx = conn.transaction().unwrap();
        let user_id = match conn.execute("INSERT INTO users (name) VALUES ($1)", &[&user]) {
            // unique constraint failed
            Err(SqliteError{code: 19, message:_}) => conn.query_row("SELECT id FROM users WHERE name = $1", &[&user], |r| r.get(0)).unwrap(),
            Ok(_) => conn.last_insert_rowid(),
            e => {e.unwrap(); return}
        };
        trx.commit();
        let trx = conn.transaction().unwrap();
        conn.execute("INSERT INTO entries (channel_id, user_id, type, body, created_at) VALUES ($1, $2, $3, $4, $5)", &[&channel_id, &user_id, &type_, &body, &created_at]);
        trx.commit();
```

排他制御が目的なのでトランザクションを2回取ってます。

これだとどうなるかというと


``` rust
$ cargo run --release
...
thread '/home/kim/log/#hongo@utmc/2015-02-15.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-08-18.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-12-13.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-10-13.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-02-26.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-05-23.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-04-30.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2013-10-18.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-10-14.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-01-19.txt' panicked at 'called `Result::unwrap()` on an `Err` value: SqliteError { code: 5, message: "database is locked" }', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-08-12.txt' panicked at 'called `Result::unwrap()` on an `Err` value: GetTimeout(())', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2014-06-03.txt' panicked at 'called `Result::unwrap()` on an `Err` value: GetTimeout(())', ../src/libcore/result.rs:736
thread '/home/kim/log/#hongo@utmc/2015-02-05.txt' panicked at 'called `Result::unwrap()` on an `Err` value: GetTimeout(())', ../src/libcore/result.rs:736
...
```


エラーは減ったもののまだ出ます。

# その他

他にスレッドプールを使って並列度を下げる、ワーカースレッドを使ってSQLiteへの書き込みを1スレッドにするなども試したのですが上手く行きませんでした。

スレッドプールについては、今回エラーハンドリングのためにスレッドを立てている訳ですが、パニックを出してスレッドが死ぬとあまり思わしくない挙動をするのでダメでした。

ワーカースレッドは書き込んだ後にまたchannel_idなどを受け取らないといけないのでやめました。promiseなどを使えば上手くいきそうな気もしますが色々面倒そうなので止めておきます。

# コネクション
並列度を下げる方法はもう1つあります。

mainの中で

``` rust
    let config = r2d2::Config::builder().pool_size(16).build();
```

としていたのを覚えていますでしょうか。これはコネクションプールに16個のコネクションを持っています。この数を落とせばなんとかならないでしょうか。

結論からいうとダメでした。今度は複数スレッドがコネクションを取り合ってタイムアウトが発生してしまいます。


# 並列度を下げる
最後の最後、本当に並列度を下げる方法があります。スレッドを立てていた部分で


``` rust
    let threads = logs.map(|log| {
        let log = log.unwrap().path();
        let pathname = log.to_string_lossy().to_string();
        Builder::new().name(pathname).spawn(move|| on_log(&log))
    }).collect::<Vec<_>>();
    for thread in threads {
        let _ = thread.unwrap().join();
    }
```

としていました。これはファイル数分並列に実行してしまいます。ここを抑えましょう。


``` rust
    for log in logs {
        let log = log.unwrap().path();
        let pathname = log.to_string_lossy().to_string();
        let pool_ = pool.clone();
        let _ = Builder::new().name(pathname).spawn(move|| on_log(&log, channel_id, pool_)).unwrap().join();
    }
```

`spawn`してそのまま`join`します。こうすれば並列度を抑えつつパニックをスレッドに分離出来ます。

これで実行するとどうなるかというと


```
$ cargo run --release
...
thread '/home/kim/log/#lisp-ja@freenode/2015-04-08.txt' panicked at 'called `Result::unwrap()` on an `Err` value: GetTimeout(())', ../src/libcore/result.rs:736
...
```

いくつかタイムアウトを出します。えー。並列度1でこれならもうどうしようもないでしょう。

# まとめ

さて、小さなツールを作るというタイトルにしては後半(執筆が)重い内容になってしまいました。というか、sqlite3の問題もしますが。

このブログの内容としては

* rustでディレクトリを扱った
* ファイルを扱った
* 文字列を扱った
* 日付を扱った
* スレッドを扱った
* ロガーライブラリを扱った
* エラーハンドリングをした
* パニックのハンドリングをした
* DBを扱った
* sqlite3に並列書き込みはつらい

ボツ案も含めれば私個人はORMやスレッドプール、チャネルも扱いましたが関係ないですね。

こういう簡単なツールでも思ったより色々な機能を触れるのでみなさん試してみてはいかがでしょうか。

ソースコード全体は[ここ](https://github.com/KeenS/irc_log)においておきます。

因みに本筋とは外れますが、今回のSQLiteの問題を扱うには一旦CSVファイルに書き出してからCSVインポートでバルクインサートが考えられます。
CSVに書き出すのはちまちまやってもいいし今回のデータ量ならオンメモリ構築して一気に書き出しも出来るでしょう。あるいはSQLiteをやめてMySQLを使う。
