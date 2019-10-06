---
categories: ["Rust", "小ネタ"]
date: 2019-10-06T19:00:53+09:00
title: "Rustで「文字が特定の文字集合に含まれるか」を判定するのはどれが速いか"
---

κeenです。ブログタイトルが長い。
普段、どれがいいとか特に気にせず書いている「文字が特定の文字集合に含まれるか」の判定をするのはどの方法が速いかが気になったのでベンチマークを取ってみました。

想定しているのは何かのパーサを書いていて、例えば識別子に使える文字が `[a-zA-Z0-9_!?=<>]` だったときにそれを判定する方法です。

<!--more-->

今回試すのは3種類。

1. 文字集合を `HashSet` に入れて、 `contains` で判定する
2. 文字集合を文字列(文字ベクトル)に入れて、 `contains` で判定する
  1. `contains` の代わりに先に文字列をソートしておいて、 `binary_search` で探索する
3. `char::is_alphanumeric` などのメソッドを組み合わせる
4. 正規表現を使う

教科書的な知識だと、 1. は $O(1)$ で文字集合の濃度に依らず高速に動作するはずです。
一方 2. は $O(n)$ なので文字集合の濃度に比例してしまい、分は悪そうです。
しかし実際には 2. が使われることが多いような気がします。
1. は多少複雑な計算が必要なので少し遅く、 2. は連続した要素の探索なので現代のCPUは得意な操作で有利なのかもしれません。 2.1. は教科書的には 2. より速そうですが、やはり 2. は連続した要素の探索なのでそちらが速い可能性もあります。
3. はどう考えても速いやつですが、 `alphanumeric` 以外の文字を `c = '-' && ...` と繋ぐと分岐が多くなるのでもしかしたら不利かもしれません。
4. は一見遅そうですが、Rustの正規表現はかなりよく出来てるので実は速いかもしれません。

それではベンチマークを取ってみましょう。
線形探索が一番不利になるように、また、 `alphanumeric` にも引っ掛からないように、 `<` を対象に探索してみます。

コードはこちら。
ベンチマークにはbencherを使っています。


```rust
#[macro_use]
extern crate bencher;

use bencher::Bencher;

const CHAR: char = '>';
const CHARSET: &str = "abcdefghijklmnopqrstuvwxyz1234567890_!?=<>";

fn hashset(bench: &mut Bencher) {
    use std::collections::HashSet;
    let hash = CHARSET.chars().collect::<HashSet<_>>();
    bench.iter(|| hash.contains(&CHAR))
}

fn contains(bench: &mut Bencher) {
    // contains は str には使えないので Vec<char> を作る
    let vec = CHARSET.chars().collect::<Vec<_>>();
    bench.iter(|| vec.contains(&CHAR));
}

fn contains_binary_search(bench: &mut Bencher) {
    let mut vec = CHARSET.chars().collect::<Vec<_>>();
    vec.sort();
    bench.iter(|| vec.binary_search(&CHAR).is_ok());
}

fn is_alphanumeric(bench: &mut Bencher) {
    // 今回は小文字しか扱わないので `is_ascii_alphanumeric` は直接には使わない
    bench.iter(|| {
        (CHAR.is_ascii_alphabetic() && CHAR.is_ascii_lowercase())
            || CHAR.is_digit(10)
            || CHAR == '_'
            || CHAR == '!'
            || CHAR == '?'
            || CHAR == '='
            || CHAR == '<'
            || CHAR == '>'
    });
}

fn regex(bench: &mut Bencher) {
    use regex::Regex;
    let regex = Regex::new("[a-zA-Z0-9_!?=<>]").unwrap();
    let c = CHAR.to_string();

    bench.iter(|| regex.is_match(&c))
}

benchmark_group!(
    benches,
    hashset,
    contains,
    contains_binary_search,
    is_alphanumeric,
    regex
);
benchmark_main!(benches);

```

追記:
コードを修正しました。
<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">is_alphanumeric() よりも is_ascii_alphanumeric() などの方がこの場合適切なのでは (is_lowercase() と is_ascii_lowercase() も同様)</p>&mdash; らりお・ザ・何らかの🈗然㊌㋞㋰㋷㋓ (Mastodon に引っ越しました) (@lo48576) <a href="https://twitter.com/lo48576/status/1180805193887354881?ref_src=twsrc%5Etfw">October 6, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

/追記

HashSetや正規表現などは事前に準備してからベンチマークにかけています。

実行結果は

``` console
test contains               ... bench:          10 ns/iter (+/- 2)
test contains_binary_search ... bench:           3 ns/iter (+/- 1)
test hashset                ... bench:          13 ns/iter (+/- 1)
test is_alphanumeric        ... bench:           0 ns/iter (+/- 0)
test regex                  ... bench:          18 ns/iter (+/- 3)
```

と、 `is_alphanumeric` > binary search >  `contains` > `HashSet` > 正規表現 の結果になりました。
まあ、全部ns単位なのであんまり意味ないんですけどね。

ということでシンプルに `is_alphanumeric` を使いましょう。

# 参考

* [`is_alphabetic` の実装](https://doc.rust-lang.org/src/core/char/methods.rs.html#542-548)
* [今回のコード](https://gitlab.com/blackenedgold/bench_alphabetic)
