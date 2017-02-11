---
categories: [Rust, デザインパターン]
date: 2017-02-09T23:03:45+09:00
title: RustのちょっとやりすぎなBuilderパターン
---

κeenです。Rustでちょっとやりすぎだけど使う側の自由度が高くて安全なBuilderパターンを思い付いたので紹介しますね。

※2017-02-11T13:18:58Z+09:00 最下部に追記しました
<!--more-->

# 目的コード
以下のような構造体のビルダーを作りたいとします。

```struct
#[derive(Debug)]
struct Person {
    id: u32,
    name: String,
    age: u32,
    address: Option<String>,
    zipcode: Option<String>,
}

```

雑にやると

``` rust
struct PersonBuilder {
    id: Option<u32>,
    name: Option<String>,
    age: Option<u32>,
    address: Option<String>,
    zipcode: Option<String>,
}

impl PersonBuilder {
  pub fn new() -> Self { ... }
  pub fn id(self, id: u32) -> Self { ... }
  pub fn name(self, name: String) -> Self { ... }
  pub fn age(self, id: u32) -> Self { ... }
  pub fn address(self, address: String) -> Self { ... }
  pub fn zipcode(self, zipcode: String) -> Self { ... }
  // 返り値がOption型になってしまう
  pub fn build(self) -> Option<Person> {
    // try_opt!は標準ライブラリには存在しませんが各所で使われてるので動きは分かると思います
    Person {
       id: try_opt!(self.id),
       name: try_opt!(self.name),
       age: try_opt!(self.age),
       address: self.address,
       zipcode: self.zipcode,
    }
  }
}
```

とbuildの返り値が`Option`型になってしまい、あまりうれしくありません。
さらには例えば`id`を2回呼んだらどうなるんだとかツッコミどころが数多くあります。

ということで色々な回避策を先人達は考えてきました。

# 不自由なコード

普通にやるなら

``` rust
struct PersonBuilder {
    id: u32,
    name: String,
    age: u32,
    address: Option<String>,
    zipcode: Option<String>,
}

impl PersonBuilder {
  fn new(id: u32, name: String, age: String) -> Self { ... }
  fn address(self, address: String) -> Self { ... }
  fn zipcode(self, zipcode: String) -> Self { ... }
  // 返り値がOptionじゃない
  fn build(self) -> Person { ... }
}
```

のように`Option`でないフィールドをコンストラクタで受け取れば安全に作れますが、コンストラクタを呼ぶ時点でいくつかフィールドが揃っている必要がある、そもそも元の構造体と何が違うんだ、など色々問題があります。

# 少しマシなコード
オートマトンのようにビルダーが状態遷移するコードを使うと、1つ1つの引数をビルドしつつ安全なコードが出来ます。

```rust
struct PersonBuilderId;
struct PersonBuilderName{id: u32}
struct PersonBuilderAge{id: u32, name: String}
struct PersonBuilder {
    id: u32,
    name: String,
    age: u32,
    address: Option<String>,
    zipcode: Option<String>,
}

impl PersonBuilderId {
  pub fn new() -> Self { ... }
  // idの次はname
  pub fn id(self, id: u32) -> PersonBuilderName {
    PersonBuilderName { id: id }
  }
}

impl PersonBuilderName {
  // nameの次はage
  pub name(self, name: String) -> PersonBuilderAge {
    PersonBuilderAge {
      id: self.id,
      name: name,
    }
  }
}
impl PersonBuilderAge {
  // ageは最後
  pub name(self, age: u32) -> PersonBuilder {
    PersonBuilder {
      id: self.id,
      name: self.name,
      age: age,
      address: None,
      zipcode: None,
    }
  }
}

impl PersonBuilder {
  // 気持ち悪いけど PersonBuilderIdを返す
  pub fn new -> PersonBuilderId { ... }
  pub fn address(self, address: String) -> Self { ... }
  pub fn zipcode(self, zipcode: String) -> Self { ... }
  // これまたOptionじゃなくなってる
  pub fn build(self) -> Person { ... }
}

```

これは以下のように

```
fn main() {
  let person = PersonBuilder::new()
         .id(1)
         .name("κeen".to_string())
         .age(24)
         .address("Tokyo".to_string())
         .build();
  println!("{:?}", person)
}
```

と使えます(即興で書いたので試してないけど)。

とりあえず使えそうな気がしますが、型レベルで状態遷移をハードコードしているのでメソッドを呼び出す順番が固定されてしまいます。
なので以下はエラーです。

```
fn main() {
  let person = PersonBuilder::new()
         // nameとidを逆順に呼び出してみる
         .name("κeen".to_string())
         .id(1)
         .age(24)
         .address("Tokyo".to_string())
         .build();
  println!("{:?}", person)
}
```

あとは単純に`struct PersonBuilderId`、`struct PersonBuilderName{id: u32}`、`struct PersonBuilderAge{id: u32, name: String}`と作っていくのが面倒という話もあります。

今回はそれを改善したいという話

# 幽霊型を使ったフィールド状態

今回提案するビルダー型はこれです。提案するといってもどうせ既に誰かが考えてるでしょうが。


``` rust
use std::marker::PhantomData;
struct Empty;
struct Filled;

struct PersonBuilder<Id, Name, Age> {
    id: Option<u32>,
    id_state: PhantomData<Id>,
    name: Option<String>,
    name_state: PhantomData<Name>,
    age: Option<u32>,
    age_state: PhantomData<Age>,
    address: Option<String>,
    zipcode: Option<String>,
}
```

[PhantomData](https://doc.rust-lang.org/std/marker/struct.PhantomData.html)という特殊な型を使ってビルダーに余計な型を付けています。

この型、最初は`Empty`から出発します。

``` rust
impl PersonBuilder<Empty, Empty, Empty> {
    pub fn new() -> Self {
        PersonBuilder {
            id: None,
            id_state: PhantomData,
            name: None,
            name_state: PhantomData,
            age: None,
            age_state: PhantomData,
            address: None,
            zipcode: None,
        }

    }
}
```

そしてフィールドが埋められると対応する型がEmptyからFilledになります。

フィールドに`Some`が埋められるのと同時に対応する`_state`フィールドを`Empty`から`Filled`にすると整合性が崩れません。

``` rust
// Id以外、つまりNameとAgeはいじらないのでジェネリクスに
impl<Name, Age> PersonBuilder<Empty, Name, Age> {
    pub fn id(self, id: u32) -> PersonBuilder<Filled, Name, Age> {
        PersonBuilder {
            id: Some(id),
            id_state: PhantomData,
            name: self.name,
            name_state: self.name_state,
            age: self.age,
            age_state: self.age_state,
            address: self.address,
            zipcode: self.zipcode,
        }
    }
}
impl<Id, Age> PersonBuilder<Id, Empty, Age> {
    // 地味なテクニックとして、SringではなくてInto<String>で受けることでリテラルのStringとかも受け取れる。
    pub fn name<S: Into<String>>(self, name: S) -> PersonBuilder<Id, Filled, Age> {
        PersonBuilder {
            id: self.id,
            id_state: self.id_state,
            name: Some(name.into()),
            name_state: PhantomData,
            age: self.age,
            age_state: self.age_state,
            address: self.address,
            zipcode: self.zipcode,
        }
    }
}

impl<Id, Name> PersonBuilder<Id, Name, Empty> {
    pub fn age(self, age: u32) -> PersonBuilder<Id, Name, Filled> {
        PersonBuilder {
            id: self.id,
            id_state: self.id_state,
            name: self.name,
            name_state: self.name_state,
            age: Some(age),
            age_state: PhantomData,
            address: self.address,
            zipcode: self.zipcode,
        }
    }
}

```

`Option`のままで構わないフィールドについては雑で構いません。

``` rust
impl<Id, Name, Age> PersonBuilder<Id, Name, Age> {
    pub fn address<S: Into<String>>(mut self, address: S) -> Self {
        self.address = Some(address.into());
        self
    }

    pub fn zipcode<S: Into<String>>(mut self, zipcode: S) -> Self {
        self.zipcode = Some(zipcode.into());
        self
    }
}
```


そして必須フィールド全てが満たされた時にだけ`build`が呼べます。`unwrap`を呼んでますが安全であることに注意して下さい。

```rust
impl PersonBuilder<Filled, Filled, Filled> {
    pub fn build(self) -> Person {
        Person {
            id: self.id.unwrap(),
            name: self.name.unwrap(),
            age: self.age.unwrap(),
            address: self.address,
            zipcode: self.zipcode,
        }
    }
}
```

こうすることで、呼び出しが非常に自由になります。

``` rust
fn main() {
    let person = PersonBuilder::new()
        .age(24)
        .id(1)
        .address("Tokyo")
        .name("κeen")
        .build();
    println!("{:?}", person);
}
```

メソッドは自由に組み換えられますが、必須フィールドがなかったら`build`が呼べません（コンパイルエラー）になります。

これは応用も出来て、それぞれの状態が独立しているので例えば「2つのVector合計3つ以上要素が入っていたら」、とか「R、G、BあるいはH、S、Vの3つが揃っていたら」とかオートマトンではエンコードしづらいものでも比較的楽に書けます。


ということで便利なbuilderが出来ました。めでたしめでたし。といいたいところですが、ちょっとコードが冗長ですよね。マクロで生成なんかも出来るのですがあまりやりたくありません。ということで「ちょっとやりすぎな」Builderパターンでした。

# Some like it unsafe

上のコード、実はもうちょっと短く書けます。ただし`unsafe`な機能を使いますが。

こう書いていたものが

```
impl<Name, Age> PersonBuilder<Empty, Name, Age> {
    pub fn id(self, id: u32) -> PersonBuilder<Filled, Name, Age> {
        PersonBuilder {
            id: Some(id),
            id_state: PhantomData,
            name: self.name,
            name_state: self.name_state,
            age: self.age,
            age_state: self.age_state,
            address: self.address,
            zipcode: self.zipcode,
        }
    }
}
```

[`std::mem::transmute`](https://doc.rust-lang.org/std/mem/fn.transmute.html)を使うことで2行になります。

``` rust
use std::mem::transmute;

impl<Name, Age> PersonBuilder<Empty, Name, Age> {
    pub fn id(mut self, id: u32) -> PersonBuilder<Filled, Name, Age> {
        self.id = Some(id);
        unsafe { transmute(self) }
    }
}

```

すごいですね。
しかし、案外しょっちゅうtransmuteはミスなどで間違った変換をしてしまう（上記で言えばidフィールドを更新したのにname_stateの型を変更してしまう）上に型検査で弾けないので個人的にはあまりおすすめ出来ません。
これも「ちょっとやりすぎ」なTipsでした。

# 2017-02-11T13:18:58Z+09:00 追記
qnighyさんからもっと便利な方法の提案がありました。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">こう <a href="https://t.co/undNgEM2IQ">https://t.co/undNgEM2IQ</a> やったらunwrapしなくてすみそう？ / “RustのちょっとやりすぎなBuilderパターン | κeenのHappy Hacκing Blog” <a href="https://t.co/ogDOpYifjh">https://t.co/ogDOpYifjh</a></p>&mdash; Masaki⊣Hara (@qnighy) <a href="https://twitter.com/qnighy/status/830053476096843776">2017年2月10日</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

実際のコードはこちら。

<script src="https://gist.github.com/qnighy/e7a833eebd57ef778eaff3a8ab3649d7.js"></script>

transmuteを使ったサボった実装は出来なくなるけどそもそも余計なフィールドを使う必要がなくなるので手軽ですね。

qnighyさんありがとうございました。
