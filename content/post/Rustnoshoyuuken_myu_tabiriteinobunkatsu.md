---
categories: [Rust]
date: 2016-06-19T20:42:14+09:00
title: Rustの所有権/ミュータビリティの分割
---

κeenです。昔所有権/ミュータビリティを分割したくなったけどぱっと方法が思いつかなくて困ったことがあるので今更ながら備忘録として残しておきます。

<!--more-->

# ミュータビリティ

例えば、HTTPパーサを作ろうとしてるとしましょう。
受け取ったバイト列のバッファをパースしてヘッダやらボディやらのサブスライスを返します。この時、返り値のサブスライスはユーザへの自由を与えるために `mut` にしたいですよね。


そう思ってこういうコードを書くと

``` rust
use std::io;

fn parse(buf: &mut [u8]) -> io::Result<(&mut [u8], &mut [u8])> {
    ...
    Ok((&mut buf[hstart..hend], &mut buf[bstart..bend]))
}
```

エラーになります。


```rust
mutability_split.rs:4:31: 4:34 error: cannot borrow `*buf` as mutable more than once at a time [E0499]
mutability_split.rs:4     Ok((&mut buf[hstart..hend], &mut buf[bstart..bend]))
...
```

普通にやろうとすると
1つ1つミュータブルなサブスライスを作ることになり、2つ目以降でborrow checkに引っ掛かってしまいます。2つのバッファがオーバーラップしてなくても。難儀ですね。コピーするくらいしかないのでしょうか。いいえ。rustはちゃんと助けになるAPIを用意してくれています。

## [`split_at_mut`](https://doc.rust-lang.org/std/primitive.slice.html#method.split_at_mut)

``` rust
fn split_at_mut(&mut self, mid: usize) -> (&mut [T], &mut [T])
```

スライスを2つに分割してくれます。重要なのが、ミュータビリティも分割してくれるところです。返り値の2つ共に `mut` がついています。ということで、次のように実装出来ます。（実行してないので結果が正しいかは確認してませんが）

``` rust
fn parse(buf: &mut [u8]) -> io::Result<(&mut [u8], &mut [u8])> {
    ...

    let (h, b) = buf.split_at_mut(bstart);
    let h = {
        let (_, h) = h.split_at_mut(hstart);
        let (_, h) = h.split_at_mut(hend - hstart);
        h
    };

    let (_, b) = b.split_at_mut(bend - bstart);

    Ok((h, b))
}

```

# 所有権

似たような話で、構造体の所有権というか、貸与権(?)を分割したい時もあります。

``` rust
struct PersonDB {
    name: String,
    age: usize,
    address: String,
}


fn use_immutable(s: &str) {
    // empty
}

fn use_mutable(s: &mut str) {
    // empty
}



fn from_db(person: PersonDB) {
    let name = &person.name;
    let mut address = &mut person.address;
    use_immutable(name);
    use_mutable(address);
}

```

イミュータブルとミュータブル両方の借用があるのでエラーになってしまいます。実際は別のフィールドなので問題は起きないのですが、コンパイラに怒られます。理不尽ですね。



```
mutability_split.rs:36:28: 36:42 error: cannot borrow immutable field `person.address` as mutable
mutability_split.rs:36     let mut address = &mut person.address;
                                                  ^~~~~~~~~~~~~~

```

そういう時は、パターンマッチによる分配束縛でクリア出来ます。

``` rust
fn from_db(person: PersonDB) {
    let PersonDB {name, mut address, ..} = person;
    use_immutable(&name);
    use_mutable(&mut address);
}

```

# まとめ

Rustの所有権やミュータブルな参照など、1つしか存在出来ないとされるものが分割出来ることを示しました。
地味に悩む所なので誰かの助けになれば幸いです。

蛇足を足すと、本当は所有権の分割についても書く予定でしたがエラーになる筈のコードがあっさりコンパイルを通ってしまったのでナシになりました。コンパイラも進化してますね。
