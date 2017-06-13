---
categories: [Rust, 小ネタ]
date: 2017-06-13T22:53:46+09:00
title: Rustでエラーが出てないのにファイルに書き出せないときは
---

κeenです。随分前から書こうと思いつつ先送りになっていた小ネタです。
<!--more-->

例えばカレントディレクトリにある`some_file.txt`に適当なデータを書き込もうとして、以下のようなコードを書いたとします。

``` rust
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;

fn main() {
    let file = File::open("some_file.txt").unwrap();
    let mut w = BufWriter::new(file);
    // unwrapを呼んで書き込みエラーを検知
    write!(w, "hello").unwrap();
}
```

これを実行してみましょう。

``` rust
$ rustc write_file.rs
$ ./write_file
```

特段エラーは出ません。しかしながら`some_file.txt`の中身は特に書き変わっていません。

``` rust
$ cat some_file.txt
$
```

これ、パッと原因分かりますか？

直接の原因は[`File::open`](https://doc.rust-lang.org/std/fs/struct.File.html#method.open)です。`File::open`はリードオンリーでファイルを開くので`File::open`で開いたファイルに書き込もうとしても書き込めません（書き込みたいなら[`File::create`](https://doc.rust-lang.org/std/fs/struct.File.html#method.create)を使います）。
じゃあなぜエラーが出ないかというと`BufWriter`のせいです。
書き込んだ文字列`"hello"`は短いので`write!`を発効した時点ではまだデータはバッファに書き込まれるだけです。
このときにはまだエラーは出ません。
そして`main`の末尾で`w`のライフタイムが終わるときに`BufWrite`の[`drop`](https://doc.rust-lang.org/src/std/io/buffered.rs.html#511-518)が呼ばれますが、ここではエラーが無視されるのでユーザにはエラーが起きてないように見える訳です。

このような事故を防ぐために以下のように`flush`を呼びましょう。

``` rust
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;

fn main() {
    let file = File::open("some_file.txt").unwrap();
    let mut w = BufWriter::new(file);
    // unwrapを呼んで書き込みエラーを検知
    write!(w, "hello").unwrap();
    // flushを呼ぶことで書き込みエラーを全て拾える
    w.flush().unwrap();
}
```

``` rust
$ rustc write_file.rs
$ ./write_file
thread 'main' panicked at 'called `Result::unwrap()` on an `Err` value: Error { repr: Os { code: 9, message: "Bad file descriptor" } }', /checkout/src/libcore/result.rs:859
note: Run with `RUST_BACKTRACE=1` for a backtrace.
```

参考: [Rustといえどリソースの解放は注意 | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2016/01/08/rusttoiedoriso_sunokaihouhachuui/)
