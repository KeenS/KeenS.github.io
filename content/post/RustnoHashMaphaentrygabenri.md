---
categories: ["Rust"]
date: 2020-05-23T23:47:12+09:00
title: "RustのHashMapはentryが便利"
---

κeenです。個人的によく設計したなと思っているRustのEntry APIについて紹介します。
標準ライブラリのHashMapの操作を無駄なく書きたい人におすすめ。

<!--more-->

# Entry APIとは

`HashMap` のEntry APIというのは私が勝手に呼んでる名前ですが、 `HashMap::entry` を起点にして使えるメソッド群のことを指しています。
これは「`HashMap` に に対応する値があればそれにXXし、なければYYする」をするときに使います。
例えば `HashMap<String, Vec<String>>` にキーに対応する値（`Vec<String>`）があれば新たな文字列をpushし、なければ空 `Vec` を作って新たに文字列をpushするという操作をしてみましょう。

素直に `HashMap::get_mut` とパターンマッチを使って書くとこうなりますね。

``` rust
// 準備
let mut map = HashMap::<String, Vec<String>>::new();
let key = "Hoge".to_string();
let value = "Huga".to_string();

// パターンマッチを使ったコード
match map.get_mut(&key) {
    Some(v) => v.push(value),
    None => {
        map.insert(key, vec![value]);
    }
};
```

これをEntry APIを使えば以下のように1式で書けます。

``` rust
// 準備
let mut map = HashMap::<String, Vec<String>>::new();
let key = "Hoge".to_string();
let value = "Huga".to_string();

// Entry APIを使ったコード
map.entry(key).or_insert_with(|| vec![]).push(value);
```

綺麗ですね。

上記のコードをよく見ると1つ気付くことがあるかと思います。 `map` に対して `key` で検索している回数が2回から1回に減っています。こういった点も含めてEntry APIを深堀りしてみましょう。


# Entry API

先程みたとおり、 [`HashMap::entry`](https://doc.rust-lang.org/std/collections/hash_map/struct.HashMap.html#method.entry) から始まるAPIです。`map.entry(key)` のように、探したいキーを渡します。探した結果、なければそのままデータとして挿入するのに使うので、 `&key` ではなく `key` になっていることに注意してください。
`entry` の返り値は [`Entry`](https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html) になっています。。 この定義を見てみましょう。

``` rust
pub enum Entry<'a, K: 'a, V: 'a> {
/// A view into a single entry in a map, which may either be vacant or occupied.
///
/// This `enum` is constructed from the [`entry`] method on [`HashMap`].
///
/// [`HashMap`]: struct.HashMap.html
/// [`entry`]: struct.HashMap.html#method.entry
#[stable(feature = "rust1", since = "1.0.0")]
pub enum Entry<'a, K: 'a, V: 'a> {
    /// An occupied entry.
    #[stable(feature = "rust1", since = "1.0.0")]
    Occupied(#[stable(feature = "rust1", since = "1.0.0")] OccupiedEntry<'a, K, V>),

    /// A vacant entry.
    #[stable(feature = "rust1", since = "1.0.0")]
    Vacant(#[stable(feature = "rust1", since = "1.0.0")] VacantEntry<'a, K, V>),
}
```

`Entry` は列挙型で定義されていて、キーに対応する値を探した結果キーと値の組があればそれ（`Occupied`）、なければないことを表わす値（`Vacant`）をとります。一見 `Option` でよさそうに見えますが、もうちょっと複雑なことをやっています。

さて、列挙型のの定義を紹介しましたが、実際にこの列挙子を直接使うことは稀でしょう。`Entry` に生えているメソッドを叩くことが多いです。
例えば冒頭に出てきた [`Entry::or_insert_with`](https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html#method.or_insert_with) は「キーに対応する値があればそれを使い、なければ引数に渡されたクロージャを評価した値をハッシュマップに挿入した上で使う」という動作をします。これの兄弟メソッドに [`Entry::or_insert`](https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html#method.or_insert) や [`Entry::or_default`](https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html#method.or_default) があります。
用途によって使い分けましょう。

# Entry APIの真価

説明が長くなるのでまずは使い方だけ説明しました。
しかしハッシュマップの動作原理を理解しているとEntry APIのありがたみというか、何をしているAPIなのかが分かります。

## ハッシュマップの復習
ハッシュマップの動作原理完全理解とはいきませんが、知らない人のために少しだけ説明します。
以下の事実を知ってる人は飛ばして構いません。

* ハッシュマップはキーに対応づけられた場所にキーと値のペアを格納する
* ハッシュマップでキーに対応づけられた場所を探すのは多少の計算を要する

また、もっと詳しく知りたい方は[Open Data Structuresの和訳](https://sites.google.com/view/open-data-structures-ja)とかをあたって下さい。
因みにハッシュテーブルというかハッシュマップというか迷いましたが、ハッシュテーブルをマップとして使っているのでハッシュマップでいきます。

まずはハッシュ関数をおさえましょう。
ハッシュ関数は与えられた値から、非負整数を算出する関数です。整数になるとコンピュータで扱いやすくなります。
このとき、「できるだけ値がバラける」ようにハッシュ関数を工夫します。「できるだけ」なので、値が被ることもあります。ハッシュ関数を `h` としたとき、例えば `h("Hoge") = 2` 、 `h("fuga") = 110` 、 `h("Fuga") = 2` のような結果であることもあります。ハッシュ関数で計算した値をハッシュ値と呼びます。

ハッシュ関数が分かったところでハッシュマップに移りましょう。
ハッシュマップのデータの実体はただの配列です。配列の要素はキーと値のペアです。

``` text
ハッシュマップ
+------+----
| K, V | ...
+------+----
```

このハッシュマップにデータを挿入する処理をみてみましょう。
ハッシュマップにキー `"Hoge"` でデータを挿入しようとすると、まず `"Hoge"` のハッシュ値を計算します。
仮に `2` と出たとしましょう。すると配列の2番目をみにいきます。
これは空いているので無事データを書き込めて、ハッシュマップに `"Hoge", "fuga"` というデータを保管できました。


``` text
h("Hoge") = 2, 2番目のセルにデータを書く


ハッシュマップ
+------+------+----------------+-----
| //// | //// | "Hoge", "hoge" | ...
+------+------+----------------+-----
                        ↑ put data
```

値を取り出すときも同様です。
`"Hoge"` に対応する値が欲しいとします。
`"Hoge"` に対応するハッシュ値を計算して2を得ます。
そして配列の2番目のセルを見るとキーと値が格納されています。
配列内のキー `"Hoge"` と検索に使ったキー `"Hoge"` が一致しているのでこのセルで正解です。
という訳で対応する値 `"hoge"` を得ます。

``` text
h("Hoge") = 2, 2番目のセルを見にいく


ハッシュマップ
+------+------+----------------+-----
| //// | //// | "Hoge", "hoge" | ...
+------+------+----------------+-----
                        ↑ get data
```


これで `h("Hoge")` は操作できました。
ハッシュ値が `"Hoge"` と同じ `"Fuga"` はどうでしょう。
今までの方法だとハッシュ値に2が出て、そこに書こうとすると既に `"Hoge"` のデータがあります。

``` text
h("Fuga") = 2, 2番目のセルにデータを書きたい


ハッシュマップ
+------+------+----------------+-----
| //// | //// | "Hoge", "hoge" | ...
+------+------+----------------+-----
                ↑ 別のデータが既にある
```

この場合の解決方法は色々ありますが、ここでは「空いてる場所に置く」という方法を紹介しておきます。

``` text
h("Fuga") = 2, 2番目のセルにデータを書きたい


ハッシュマップ
+------+------+----------------+----------------+----
| //// | //// | "Hoge", "hoge" | "Fuga", "fuga" | ...
+------+------+----------------+----------------+----
                                 ↑ 2番目ではなく、近くの空いている場所に書く

```

「その場所は他のキーのハッシュ値とぶつかるのでは？」「他にもハッシュ値が被ったキーがあった場合どうするの？」など色々疑問が湧くかと思いますが、一旦忘れて下さい。
ハッシュマップは色々な変種があるのでそれぞれで対応方法が違います。
今回の目的はEntry APIの解説なので微には入らず次に進みます。

## `Entry` が表すもの

ここまできたら `Entry` が表すものが分かりますね。「`K, V` があるはずの場所」です。
`Entry` が `Occupied` なら「ここにあります」、 `Vacant` なら「そこになければないですね」を表しています。

``` text
h("Hoge") = 2, 2番目のセル

ハッシュマップ
                `Occupied`
                    ↓
+------+------+----------------+-----
| //// | //// | "Hoge", "hoge" | ...
+------+------+----------------+-----
                  ↑データがある

                `Vacant`
                    ↓
+------+------+----------------+-----
| //// | //// | ////////////// | ...
+------+------+----------------+-----
                  ↑データがない


```


`Entry` に対して `or_insert` を呼ぶときは `Occupied` ならそこにある値を返せばいいですし、なければ `or_insert` の引数をその場に値を置いた上で返せばいいです。既に場所が確定したあとなので、値を置くのは一瞬です。
`map.get_mut(&key)` からの `map.insert(key, value)` と比べると場所を探す回数が2回から1回に減っています。

Entry APIは、ハッシュマップの「場所を検索して値を返す」や「場所を検索して値を挿入する」の「場所を検索する」の部分を抜き出したAPIだと言えます。
因みにこのAPIを安全に提供できるのはRustの借用/所有権システムのおかげです。他言語でこれをやると、 `Entry` を保持している間にその「場所」に別の値を上書かれてしまった場合に破滅します。
Rustの場合は所有権のおかげで `Entry` がある間にハッシュマップを更新できなくなるので破滅を回避できます。
これを思いついた人すごいなーって思います。

## Entry APIの歴史的価値

余談になりますが、Entry APIにはもう1つの側面がありました。
古いRustではパターンマッチを用いた方のコードはコンパイルが通らなかったのです。

Rust 1.35.0で冒頭のコードをコンパイルしてみましょう。

``` console
$ rustc +1.35.0 hashmap_entry.rs
error[E0499]: cannot borrow `map` as mutable more than once at a time
  --> hashmap_entry.rs:11:13
   |
8  |     match map.get_mut(&key) {
   |           --- first mutable borrow occurs here
...
11 |             map.insert(key, vec![value]);
   |             ^^^ second mutable borrow occurs here
12 |         }
13 |     };
   |     - first borrow ends here

error: aborting due to previous error

For more information about this error, try `rustc --explain E0499`.
```

これは昔のRustの借用検査が粗く、 `match` 式全体で `map` を借用していると判断していたためです。
「ハッシュマップの値を検索して、なければ値を挿入する」というありがちな操作でさえコンパイルエラーになるので初心者殺しのエラーでした。昔に比べるとRustも進化していますね。

# まとめ

Rustの標準ライブラリのEntry APIについて解説しました。
Entry APIはハッシュマップの特性をRustの特徴を上手く使って設計されたAPIで興味をそそるものがあります。
`HashMap` を使うときは使ってみて下さい。
