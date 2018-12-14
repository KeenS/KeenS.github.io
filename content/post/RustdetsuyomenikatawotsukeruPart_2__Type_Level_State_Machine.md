---
categories: ["Rust", "AdventCalendar", "Advent Calendar 2018", "Rustで強めに型をつける"]
date: 2018-12-15T05:10:47+09:00
title: "Rustで強めに型をつけるPart 2: Type Level State Machine"
---

このエントリは[Rustその2 Advent Calendar 2018](https://qiita.com/advent-calendar/2018/rust2) 7日目の記事を時空を遡って書いています。

κeenです。寝れないので空いてる日の分を埋めに行きます。次はType Level State Machine。あるいはやりすぎてない方のBuilderバターン。
[過去記事](https://keens.github.io/blog/2017/02/09/rustnochottoyarisuginabuilderpata_n/)で当たり前のように書いたコードをもう少し丁寧に説明します。

<!--more-->

以下のようなデータ型のビルダーを作りたいとします。



``` rust
#[derive(Debug)]
struct Person {
    id: u32,
    name: String,
    age: u32,
    address: Option<String>,
    zipcode: Option<String>,
}
```

ビルダは以下のように使いたいとします。

```rust
let person = PersonBuilder::new()
       .id(1)
       .name("κeen".to_string())
       .age(26)
       .address("Tokyo".to_string())
       .build();
```

このとき、 `Person` には `id`、`name`、`age`のフィールドは自明なデフォルト値を持たないので必須にしたいです。
つまり少なくとも `.id()`、`.name()`、`.age()` の3つのメソッドを呼ばないと `.build()` を呼べないようにしたいです。さて、どうしましょう。

3つのメソッドを呼ばないと`.build()`を実装した型がでてこないようにすればいいのです。
型の話は置いておいて、ひとまず以下のようなステートマシンをイメージしましょう。

```text
                                   .zipcode()
                                   .address()
                                  +----------+
                                  |          |
    .id()     .name()     .age()  v          | .build()
(S)-------[1]---------[2]--------[3]---------+----------(E)
```

メソッドを呼ぶ度そのラベルのついた状態に遷移すると思って下さい。
`id`、`name`、`age` を呼ばないと `build` が呼べないですね。`zipcode` や `address` が複数回呼べてしまいますがまあ、それは目を瞑りましょう。
これを型にエンコードします。遷移はメソッド呼び出しで表現します。


```rust
// Sに対応
struct PersonBuilderId;
impl PersonBuilderId {
    pub fn new() -> Self {
        PersonBuilderId
    }
    // idの次はname
    pub fn id(self, id: u32) -> PersonBuilderName {
        PersonBuilderName { id: id }
    }
}
// 1に対応
struct PersonBuilderName {
    id: u32,
}
impl PersonBuilderName {
    // nameの次はage
    pub fn name(self, name: String) -> PersonBuilderAge {
        PersonBuilderAge {
            id: self.id,
            name: name,
        }
    }
}
// 2に対応
struct PersonBuilderAge {
    id: u32,
    name: String,
}
impl PersonBuilderAge {
    // ageは最後
    pub fn name(self, age: u32) -> PersonBuilder {
        PersonBuilder {
            id: self.id,
            name: self.name,
            age: age,
            address: None,
            zipcode: None,
        }
    }
}
// 3に対応
struct PersonBuilder {
    id: u32,
    name: String,
    age: u32,
    address: Option<String>,
    zipcode: Option<String>,
}

impl PersonBuilder {
    pub fn new() -> PersonBuilderId { ... }
    pub fn address(self, address: String) -> Self { ... }
    pub fn zipcode(self, zipcode: String) -> Self { ... }
    pub fn build(self) -> Person { ... }
}
```

これで目的の「`id`、`name`、`age`が揃うまで`build`」が呼べないが達成されました。


因みにこのコードはRustの所有権を上手く使っています。各メソッドで `self` の所有権を取るので古い状態が消えるのが都合がいいんですね。
こういう状態遷移はビルダーに限らず色々あると思います。例えばヘッダを書いてからボディを書きたいとか特定のメソッドを呼ぶとアクティブになって特定のメソッドを呼ぶとパッシブになるとか。
そういうのの状態管理に使えると便利です。

なんかあんまり丁寧な解説にならなかった
