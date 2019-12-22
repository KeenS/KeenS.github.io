---
categories: ["Rust", "Advent Calendar", "Advent Calendar 2019", "Rust Advent Calendar"]
date: 2019-12-06T02:36:37+09:00
title: "itertoolsの紹介"
---

κeenです。このエントリは[Rust Advent Calendar 2019 - Qiita](https://qiita.com/advent-calendar/2019/rust) 6日目のエントリです。
空いてたので飛び入りました。
飛び入りなので軽い小ネタ。便利クレート itertoolsの紹介です。

<!--more-->

[itertools](https://crates.io/crates/itertools) は標準ライブラリの `Iterator` を拡張し、今まで痒いところに手が届かなかった部分をサクっと埋めてくれるライブラリです。

# 使い方
`cargo add itertools` などしてあとは `use itertools::Itertools;` するだけです。これだけでイテレータが便利になります。

# 何ができるの
## `Itertools`

「そう、それ！」と言いたくなるようなメソッドが生えてきます。
例えばイテレータの要素をまとめて処理する `chunks` と、セパレータで文字列を結合する `join` を組み合わせるとこういうコードが書けます。

```rust
for chunk in &(0..100).chunks(20) {
    println!("{}", chunk.map(|i| format!("{:2}", i)).join(", "))
}
```

この実行結果はこうです。

```text
 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19
20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59
60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79
80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99
```

`chunks` は実用では大きいデータを一括で処理すると負荷が高いから100個づつくらいやりたい、なんかのケースで有用そうですね。

因みに上記の例はもうちょっと便利メソッドを使って書き直せます。
`for` 式相当処理をする `for_each` メソッド（これは標準ライブラリの `Iterator` にある）と、気持ち悪いですが「フォーマット文に渡されると各要素にそのフォーマット指定子を適用し、さらに引数の文字列で結合する」処理をする `format` メソッドです。

```rust
(0..100)
    .chunks(20)
    .into_iter()
    .for_each(|chunk| println!("{:2}", chunk.format(", ")))
```

例えば `Vec<u8>` を表示するのにも便利なんじゃないでしょうか。

```rust
let bytes = "あいうえお".as_bytes();
println!("{:2x}", bytes.iter().format(", "))
```


```text
e3, 81, 82, e3, 81, 84, e3, 81, 86, e3, 81, 88, e3, 81, 8a
```

あとは `sorted` はイテレータをソートしてくれます。ソートしようとして「えっ、一旦 `Vec` にしないとダメなの」と思ったことのある方も多いんじゃないでしょうか。itertoolsならイテレータのままソートできます。

```rust
println!("{:?}", "bdacfe".chars().sorted().collect_vec())
```

因みにここで使っている `collect_vec()` は `collect::<Vec<_>()` 相当です。

結果はこう。

```text
['a', 'b', 'c', 'd', 'e', 'f']
```

この他にも `cartesian_product` だとか `tuple_windows` だとか `merge` だとか `group_by` だとか便利そうなメソッドが一杯生えてます。

## マクロ
2つ生えてます。 `izip` と `iproduct` 。 `zip` と `cartesian_product` の可変長引数版ですね。

## 関数

いくつか関数も生えてます。だいたいはイテレータのメソッドを関数にして `map` なんかに渡しやすくしたものですが、中には面白い関数もあります。

例えば `repeat_n` 。同じデータを繰り返してくれます。

```rust
fn mask(s: String) -> String {
    format!("{}{}", itertools::repeat_n('x', 32).format(""), &s[32..])
}
```


`cons_tuples` なんかも面白いですね。 `zip` の繰り返しで積ったタプルをフラットにしてくれます。


```rust
itertools::cons_tuples((0..10i32).zip(10..20i32).zip(20..30i32))
    .for_each(|t| println!("{:?}", t))
```

本来なら `zip` を2回やってるので要素は `((i32, i32), i32)` ですがこれを `(i32, i32, i32)` にしてくれます。

```text
(0, 10, 20)
(1, 11, 21)
(2, 12, 22)
(3, 13, 23)
(4, 14, 24)
(5, 15, 25)
(6, 16, 26)
(7, 17, 27)
(8, 18, 28)
(9, 19, 29)
```


# 練習問題

[言語処理100本ノック](http://www.cl.ecei.tohoku.ac.jp/nlp100/)をitertoolsを使って問いてみましょう。
面白い問題をいくつか選んでやっていきます。

## 02. 「パトカー」＋「タクシー」＝「パタトクカシーー」

> 「パトカー」＋「タクシー」の文字を先頭から交互に連結して文字列「パタトクカシーー」を得よ．

まさしくな `interleave` というメソッドがあります。

```rust
fn p02() -> String {
    let s1 = "パトカー";
    let s2 = "タクシー";
    s1.chars().interleave(s2.chars()).collect()
}
```


## 05. n-gram

> 与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．

bi-gramなら `tuple_windows` で簡単に作れます。

``` rust
fn bi_gram<I>(input: I) -> impl Iterator<Item = [I::Item; 2]>
where
    I: Iterator,
    I::Item: Clone,
{
    input.tuple_windows::<(_, _)>().map(|(t1, t2)| [t1, t2])
}

fn p05() -> (Vec<[char; 2]>, Vec<[&'static str; 2]>) {

    let s = "I am an NLPer";
    let char_bigram = bi_gram(s.chars()).collect_vec();
    let word_bigram = bi_gram(s.split_whitespace()).collect_vec();

    (char_bigram, word_bigram)
}
```

`tuple_windows` は4つ組までにしか対応してないので 5-gram以上はできません。 `tuple_windows` のタプルじゃない版欲しい…。


という訳でいくつか練習問題を解いてみました。

# まとめ

itertoolsという便利なライブラリを紹介しました。 `log` とかと同じで使うかは分からなくてもプロジェクトを作ったら取り敢えず入れておくくらいの勢いで使っていけばいいと思います。


