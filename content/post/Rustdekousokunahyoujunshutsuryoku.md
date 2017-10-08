---
categories: [Rust]
date: 2017-10-05T20:36:33+09:00
title: "Rustで高速な標準出力"
---
κeenです。Rustで何も考えずに標準出力に吐いてると遅いよねーって話です。
<!--more-->

今回、標準出力に「yes」と1000万回出力するアプリケーションを書いてみたいと思います。

# `println!`

まあ、最初に思いつくのはこれでしょうか。

``` rust
fn main() {
    for _ in 0..10_000_000 {
        println!("yes");
    }
}
```

``` console
$ rustc -O yes.rs
$ time ./yes > /dev/null
./yes > /dev/null  1.19s user 0.49s system 99% cpu 1.681 total
```


はい、1.681秒。結構時間掛かります。

# ロックレス

上記の`println!`が遅いのは毎度ロックを取ってるからなので、直接stdoutを取得して一度だけロックを取るようにすると速度は改善します。


``` rust
use std::io::{stdout, Write};

fn main() {
    let out = stdout();
    let mut out = out.lock();
    for _ in 0..10_000_000 {
        writeln!(out, "yes").unwrap();
    }
}
```


``` console
$ rustc -O yes.rs
$ time ./yes > /dev/null
./yes > /dev/null  0.62s user 0.49s system 99% cpu 1.111 total
```

1.111秒。多少は速くなりましたがあんまり変わんないですね。

# 改行？

ところで上記のプログラム、`writeln!`の代わりに`write!`を使うと（改行を挟まないと）急激に速くなります。

``` rust
use std::io::{stdout, Write};

fn main() {
    let out = stdout();
    let mut out = out.lock();
    for _ in 0..10_000_000 {
        write!(out, "yes").unwrap();
    }
}
```

``` console
$ rustc -O yes.rs
$ time ./yes > /dev/null
./yes > /dev/null  0.25s user 0.00s system 98% cpu 0.255 total
```

0.255秒、4.4倍くらいになりました。 何故だか分かりますか？

`writeln!`の方は改行でフラッシュされるからです。`write!`だと（改行がないと）されない。あれ？フラッシュ？

# バッファリング

はい、ということで忘れがちですが標準出力もデフォルトではバッファリングされないのでバッファリングしてあげましょう。バッファリングしなくてIOが遅い、Rustあるあるですね。

``` rust
use std::io::{stdout, Write, BufWriter};

fn main() {
    let out = stdout();
    let mut out = BufWriter::new(out.lock());
    for _ in 0..10_000_000 {
        writeln!(out, "yes").unwrap();
    }
}
```

``` console
$ rustc -O yes.rs
$ time ./yes > /dev/null
./yes > /dev/null  0.15s user 0.00s system 97% cpu 0.152 total
```

はい、最初から比べると10倍以上速くなりました。めでたしめでたし。

# さらなる高み

一般的にはここまでで十分ですが、興味としてさらに高速化してみましょう。

1行yesを書く度に毎度`write!`を呼んでいては遅いです。ある程度まとめて`write!`を呼びましょう。

今回2048 yes毎に`write_all`を呼ぶようにしてみます。


``` rust
use std::io::{stdout, Write, BufWriter};

fn main() {
    let out = stdout();
    let mut out = BufWriter::new(out.lock());
    let yes = {
        let mut s = String::with_capacity(4096);
        for _ in 0..2048 {
            s += "yes\n";
        }
        s
    };
    let rest = {
        let mut s = String::with_capacity(4096);
        for _ in 0..(10_000_000 % 2048) {
            s += "yes\n";
        }
        s
    };

    for _ in 0..(10_000_000 / 2048) {
        out.write_all(yes.as_bytes()).unwrap();
    }

    out.write_all(rest.as_bytes()).unwrap();
}
```

``` console
$ rustc -O yes.rs
$ time ./yes > /dev/null
./yes > /dev/null  0.00s user 0.00s system 0% cpu 0.003 total
```

比べようもないくらい速くなりました。


# 参考

* [Drop the default buffer size to 8K by sfackler · Pull Request #32695 · rust-lang/rust](https://github.com/rust-lang/rust/pull/32695)
* [How is GNU `yes` so fast? : unix](https://www.reddit.com/r/unix/comments/6gxduc/how_is_gnu_yes_so_fast/)



因みにですが手元の環境だとGNU yesより速いです。※Rust版の方は出力数を10億回に増やしたものを使用

``` console
$ ./yes | pv > /dev/null
3.73GiB 0:00:00 [7.73GiB/s]
$ yes | pv > /dev/null
^C.6GiB 0:00:06 [7.29GiB/s]
```

``` console
$ yes --version
yes (GNU coreutils) 8.26
Copyright (C) 2016 Free Software Foundation, Inc.
ライセンス GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

作者 David MacKenzie。
```

# 2017-10-08 追記

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">バッファリングするやつの<br>writeln!(out, &quot;yes&quot;).unwrap();<br>を<br>out.write(b&quot;yes\n&quot;).unwrap();<br>にするだけで最期の 2048 ごとのに匹敵するほどはやくなりました。<br>文字列からバイトへの変換にコピーが発生していそう。</p>&mdash; いじゅういん (@kei10in) <a href="https://twitter.com/kei10in/status/916038667600142336?ref_src=twsrc%5Etfw">2017年10月5日</a></blockquote>
<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">ありがとうございます。言われてみれば確かwritelnは一旦改行文字を結合してから（新たにアロケートしてから）出力していた気がします。後程追記しますね。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/916088821741101056?ref_src=twsrc%5Etfw">2017年10月5日</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

ということで実験してみましょう。

まずは指摘されたコード。

``` rust
use std::io::{stdout, Write, BufWriter};

fn main() {
    let out = stdout();
    let mut out = BufWriter::new(out.lock());
    for _ in 0..10_000_000 {
        out.write(b"yes\n").unwrap();
    }
}
```


``` console
$ rustc -O yes.rs
$ time ./yes > /dev/null
./yes > /dev/null  0.02s user 0.00s system 89% cpu 0.027 total
```

かなり速くなってます。ふむむむ。

もう1つ、私が言及した`writeln!`よりも`write!`の方が速いというやつ。


``` rust
use std::io::{stdout, Write, BufWriter};

fn main() {
    let out = stdout();
    let mut out = BufWriter::new(out.lock());
    for _ in 0..10_000_000 {
        out.write(b"yes\n").unwrap();
    }
}
```

``` console
$ rustc -O yes.rs
$ time ./yes > /dev/null
./yes > /dev/null  0.15s user 0.00s system 98% cpu 0.155 total
```


あれ！？`write!`を使うと遅い…。`write!`は単に[`write_fmt`に置き換えられる](https://github.com/rust-lang/rust/blob/master/src/libcore/macros.rs#L403)だけなので大したコストじゃないと思ってたんですが`write_fmt`って以外とコストかかるんですね。

ところで`writeln!`は[マクロの`concat!`を呼んでいる](https://github.com/rust-lang/rust/blob/master/src/libcore/macros.rs#L448)ので実行時にはアロケーションコストが掛からなそうです（汗。調べもせずに適当なことを言うのはやめましょう（戒め）

/追記
