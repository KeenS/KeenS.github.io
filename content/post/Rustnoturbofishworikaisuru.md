---
categories: ["Rust", "Advent Calendar", "Advent Calendar 2019"]
date: 2019-12-03T00:12:31+09:00
title: "Rustのturbofishを理解する"
---
このエントリは[Rustその3 Advent Calendar 2019](https://qiita.com/advent-calendar/2019/rust3)3日目に飛び入り参加しているエントリです。
Rustの型パラメータ指定の構文、通称ターボフィッシュ(turbofish)について。
<!--more-->

Rustでジェネリクス関数は以下のように関数名に続いて `<パラメータ名>` で指定しますよね？

``` rust
fn generics<T>(t: T) {
    // ...
}
```

これを呼び出すときはどうやって指定しましょう？直感的にはこうですよね？

``` rust
generics<u8>(0)
```

しかしこれは構文エラーです。

``` text
error: chained comparison operators require parentheses
 --> turbofish.rs:6:13
  |
6 |     generics<u8>(0);
  |             ^^^^^
  |
  = help: use `::<...>` instead of `<...>` if you meant to specify type arguments
  = help: or use `(...)` if you meant to specify fn arguments

error: aborting due to previous error
```

エラーメッセージにも書いてある通り、型を指定するときは名前と型パラメータの間に `::` を置く必要があります。

```rust
generics::<u8>(0)
```



この `::<>` という構文は魚がロケットエンジンで右に進んでいるように見えるからか、[turbofish](https://turbo.fish/)と呼ばれています。

``` text
::<> ::<> ::<> ::<> ::<>
  ::<> ::<> ::<> ::<> ::<>
::<> ::<> ::<> ::<> ::<>
```

よくターボフィッシュのお世話になる関数の1つは `str::parse` でしょうか。以下のように使います。

``` rust
use std::net::Ipv4Addr;
// 型推論が効かない環境でもターボフィシュを使えば `Ipv4Addr` を指定できる
let addr = "127.0.0.1".parse::<Ipv4Addr>();
```


ところでなんで `::` が必要なの？というと、どうも `::` がないと他の構文と区別がつかないケースがあるからです。


``` rust
fn generics2<T1, T2>(_: (T1, T2)) {
  // ...
}

let tuple = ('a', 0);
// 関数呼び出し + 型パラメータの指定のつもりのコード
(generics2<char, i32>(tuple))
```

``` rust
let generics2 = 'b';
let char = 'a';
let i32 = 0;
let tuple = 1;
// 2つの比較演算のタプルのつもりのコード
(generics2<char, i32>(tuple))
```

現行のRustでは後者の方が既に合法なコードとして存在するので簡単には入れられません。

さて、この文法さえ覚えれば基本はオッケーなんですが、どこに書くのか意外と迷いやすいです。

例えば以下のように `str` の値を `String` に変換したいとします。

``` rust
let s: String = "str".into();
```

今回のケースは単純なので変数の方に型アノテーションを書くこともできますが、メソッドチェーンの途中だと型アノテーションを書ける場所がなく、ターボフィッシュを使いたいこともあるでしょう。そういうケースでのお話しです。

`parse` の類推から、 `into` にターボフィッシュをあてがえばコンパイルできそうに思えます。

``` rust
let s = "str".into::<String>();
```

しかし残念ながら、このように `unexpected type argument` が出てしまいます。

``` text
error[E0107]: wrong number of type arguments: expected 0, found 1
 --> turbofish.rs:6:26
  |
6 |     let s = "str".into::<String>();
  |                          ^^^^^^ unexpected type argument

error: aborting due to previous error

For more information about this error, try `rustc --explain E0107`.
```

これは `Into` の定義を見ると納得がいきます。


``` rust
pub trait Into<T> {
    fn into(self) -> T;
}
```

よくみると `into` メソッドではなくて `Into` のトレイトの方に型パラメータがついていますね。
なので `Into` の方にターボフィッシュが必要です。

``` rust
let s = Into::<String>::into("str");
```


まあ、このケースでは `From` があるので `From` を使った方が手っ取り早いでしょう。

``` rust
let s = String::from("str");
```

今回のは `From` があるという細かい話はありますが、基本は型パラメータのあるところにターボフィッシュが必要と覚えて下さい。

例えば `Vec` の `new` はこう。

``` rust
let v = Vec::<u32>::new();
```

これでターボフィッシュをマスターできましたか？

実はまだよく分からないケースがあります。 `enum` の型パラメータはどうやらバリアントにも付けられるようです。

`Option` は以下のように定義されています。

``` rust
pub enum Option<T> {
    None,
    Some(T),
}
```

順当にいけば `None` の型パラメータは以下のように指定するのが正しいですね。

``` rust
let o = Option::<String>::None;
```

実際、これは正しくコンパイできます。

しかし、以下のような書き方も許容されています。

``` rust
let o = None::<String>;
```


どうも、昔のRustは `Option::None` という書き方ができず、ターボフィッシュを置く場所がなかったのでバリアントに型パラメータが書けるようになったようです。
複雑ですね。

ということで飛び入りの小ネタでした。
