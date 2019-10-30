---
categories: ["Rust"]
date: 2019-10-31T02:52:22+09:00
title: "Rustで構造体に配列のようなインデックスアクセス"
---

κeenです。ネタ記事です。ふと、構造体に配列のようなアクセスができる方法を思い付いたので共有します。

<!--more-->

ざっくり言うとこういうことが書けるようになります。

```rust
fn main() {
    let mut user = User::new(1, "user".into());

    // 名前でアクセス
    assert_eq!(user[name], "user");

    // アップデートもできる
    user[id] += 1;

    assert_eq!(user[id], 2);
}
```


もちろん、構造体はごく自然にこういう風に定義されています。

```rust
pub struct User {
    id: u64,
    name: String,
}

impl User {
    pub fn new(id_var: u64, name_var: String) -> Self {
        User {
            id: id_var,
            name: name_var,
        }
    }
}
```


Rustでは `[]` はオーバーロード可能です。インデックスに使う値もオーバーロード毎に変えることができます。
なので以下のようにそれぞれのフィールド専用の構造体を定義してあげて、それをインデックスに使うことで上記のようなコードを実現できます。

``` rust
use std::ops::{Index, IndexMut};

// Userへのインデクシング専用の型を定義する
#[allow(non_camel_case_types)]
pub struct id;
#[allow(non_camel_case_types)]
pub struct name;

// それぞれのインデックスとフィールドを対応づけてIndexとIndexMutを実装する
impl Index<id> for User {
    type Output = u64;
    fn index(&self, _: id) -> &Self::Output {
        &self.id
    }
}

impl IndexMut<id> for User {
    fn index_mut(&mut self, _: id) -> &mut Self::Output {
        &mut self.id
    }
}

impl Index<name> for User {
    type Output = String;
    fn index(&self, _: name) -> &Self::Output {
        &self.name
    }
}

impl IndexMut<name> for User {
    fn index_mut(&mut self, _: name) -> &mut Self::Output {
        &mut self.name
    }
}

```



因みにこのコードは `id` や `name` が構造体のコンストラクタでもあるので、パターンとしても書けてしまいます。
なので以下のコードは `id` がパーターンと認識されて型エラーになります。

``` rust
let id = user[id];
```

コンパイルするとこう。

```console
57 |     let id = user[id];
   |         ^^ expected u64, found struct `id`
   |
   = note: expected type `u64`
              found type `id`
```

気をつけましょう。

コード

<script src="https://gist.github.com/KeenS/3def338ad482182bc5030b82216aa687.js"></script>

[playground](https://play.rust-lang.org/?gist=3def338ad482182bc5030b82216aa687)
