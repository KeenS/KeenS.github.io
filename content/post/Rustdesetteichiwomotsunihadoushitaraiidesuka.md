---
categories: [Rust]
date: 2022-04-24T15:54:37+09:00
title: "Rustで設定値を持つにはどうしたらいいですか"
---
κeenです。Rustではじめてアプリケーションを書くときに困りがちなことの1つにグローバルな値を持つにはどうしたらいいか分からないというのがあるようです。
その書き方を何パターンか紹介しできたらなと。

一応この記事には元ネタというかインスパイア元があり、以下のリポジトリも見ながら書かれています。

https://github.com/paulkernfeld/global-data-in-rust

<!--more-->


## `let` にする

一番よくあるケースだとCLIや起動時に読み込んだ設定ファイルの値をどうするかでしょう。データの性格として起動時に一度だけ生成してあとは(ほとんど)いじらないようなものです。そういうものは `main` 関数内で `let` して、あとは借用で持ち回ればよいです。

起動時にファイルかろ設定値を読み込んで動くアプリケーションを雰囲気で書くとこんな感じです。

```rust
use std::io;

struct Config {
    limit: u32,
}

fn run(config: &Config) -> io::Result<()> {
    // do something
    Ok(())
}


fn read_config_from_file() -> io::Result<Config> { /* */ }

fn main() -> io::Result<()> {
    // 読み込んだ値を `main` 内の `let` で保持する
    let config = read_config_from_file()?;
    // 借用して関数の引数に渡す
    run(&config)
}
```

アプリケーションが巨大になるとやや引数で持ち回るのが面倒ですが、一番素直な選択肢です。

### メソッドにする

`let` の亜種も紹介しておきます。データの持ち方としては `let` ですが、それを関数の引数で持ち回るのではなく、メソッドを生やしてメソッドでやりとりするものです。データの性格として、起動時に一度だけ生成するのは `let` と共通ですがただの値ではなくメソッドなどが生えた外部ライブラリのデータ型などが多いでしょう。

起動時にコネクションプールを作って動くアプリケーションを雰囲気で書くとこんな感じです

```rust
use some_crate::ConnectionPool;
use std::io;

struct App {
    pool: ConnectionPool,
}

impl App {
    fn run(&mut self) -> io::Result<()> {
        let cn = self.pool.get_conn()?;
        // do something
        Ok(())
    }
}

fn main() -> io::Result<()> {
    let pool = ConnectionPool::new()?;
    // 生成した値を `main` 内の `let` で保持する
    let mut app = App { pool };
    // メソッドを呼び出す
    app.run()
}
```

コンフィグでもこれをやってもいいのですが、分けた方が失敗が少ないかなと思っています。典型的な `App` が `&mut self` を要求するのに対して大抵の場合 `Config` は可変性が不要で、本来なら問題ないはずのコードも `&mut self` の借用チェックに引掛ってコンパイルが通らないケースがたまに発生するからです。そういうケースが発生しないのであれば `App` のフィールドに `config: Config` を生やすとよいです。

## `const` にする

環境変数から値を受け取るときの環境変数名など、アプリケーション側で決める値を保持するときは `const` が候補にあがります。
`const` にはヒープを扱う式が書けず、値もコピーされるのでデータの性格としてリテラルで書ける程度の軽いものなどです。

アプリケーションが読み込む環境変数名を `const` に持つコードを雰囲気で書くとこんな感じです。

``` rust
use std::io;
use std::env;

const APP_HOME_ENV: &str = "APP_HOME";

fn main() -> io::Result<()> {
    let home = env::var(APP_HOME_ENV).unwrap_or_else(||"/var/app".into());
    // do something
    Ok(())
}
```

## `lazy_static` / `once_cell` にする

アプリケーション側で決める値を初期化したいがヒープを使うので `const` や `static` で使えないような場合です。
このような場合は外部クレートの [`lazy_static`](https://crates.io/crates/lazy_static) や [`once_cell`](https://crates.io/crates/once_cell) を使うことになるでしょう。
データの性格としてコンストラクタを介さないといけないようなものが多いです。

[regex](https://docs.rs/regex/latest/regex/) のパターンを初期化するコードはこう書けます。

``` rust
// regexのドキュメントより

use lazy_static::lazy_static;
use regex::Regex;

fn some_helper_function(text: &str) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new("...").unwrap();
    }
    RE.is_match(text)
}

fn main() {}
```

おおむねシングルトンパターンのようなものです。

---

大抵のケースではこの記事に書いた方法のどれかで事足りるでしょう。
冒頭に貼った元ネタのリポジトリではもう少しバリエーションが紹介されているのでやりたいことが解決できなかったらそちらも覗いてみて下さい。
