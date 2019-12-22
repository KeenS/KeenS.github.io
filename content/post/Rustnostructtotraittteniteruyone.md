---
categories: [Rust, 関連型, 小ネタ, 型]
date: 2016-12-03T22:06:18+09:00
title: Rustのstructとtraitって似てるよね
---

κeenです。関連型について考えてたらtraitがstructに見えてきたので一筆。小ネタです。


<!--more-->

structは普通の構造体とtupl structと言われる形式二種類あります。

``` rust
struct Color {
  r: u8,
  g: u8,
  b: u8,
}
```

``` rust
struct Color(u8, u8, u8);
```

traitはというと関連型と型パラメータ二種類あります。


``` rust
trait Into {
  type Item;
}
```


``` rust
trait Into<T>{}
```

インスタンス化も似てます。

``` rust
// struct
struct Color {
  r: 255,
  g: 0,
  b: 0,
}

struct Color(255, 0, 0)
```

``` rust
// trait
struct Dummy;

impl Into for Dummy {
  type Item = i8
}

impl Into<i8> for Dummy {}
```

また、両者の使い分けも名前で参照したいかor手軽に使いたいかで使い分けます。

以上小ネタでした。
