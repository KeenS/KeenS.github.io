---
categories: [型クラス]
date: 2016-07-16T02:53:49+09:00
description: "
型クラス勉強会での発表用。Rustのトレイトについて。
"
title: 型クラスを越えて
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# 型クラスを越えて
----------------------
[歌舞伎座.tech#10「型クラス勉強会」](http://kbkz.connpass.com/event/32420/)

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバーエージェントのエンジニア
 + Lisp, ML, Rust, Shell Scriptあたりを書きます


# Rustとは
----------

* システムプログラミング言語
* GCなし！でもメモリ管理は自動
* **Zero-Cost Abstraction**
* **Trait-Based Generics**
* パターンマッチ、代数的データ型などなど
* [プログラミング言語Rust](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/README.html)


# Rustのトレイト的なの
----------------

* 型に固有のメソッド
  + オブジェクト指向的な
* トレイト
  + 型クラス


# 型クラスじゃない方
-------------------

``` rust
struct Person {
  name: String,
}
impl Person {
  // Static constructor
  fn new(name: String) -> Self {
    Person{name: name}
  }

  // method
  fn hello(&self) {
    println!("Hello, {}", self.name);
  }
}
```


``` rust
let person = Person::new("κeen");
person.hello();
```


# 多相型と制約
--------------

``` rust
struct Temp<T>(T);

impl <T: Celsius> Temp<T> {
  fn fromCelsius(t: isize) -> Self {
    Temp(Celsius(t))
  }
}

impl <T: Fahrenheit> Temp<T> {
  fn fromFahrenheit(t: isize) -> Self {
    Temp(Fahrenheit(t))
  }
}

```


# 型クラス
---------

* 便利
* 他の言語にも取り入れてほしい
* 型クラスを入れることで言語設計がどうなるか
* ユーザランドより言語機能的な部分フォーカス


# Rustの型クラスの実装
---------------------

* 動的ディスパッチと静的ディスパッチ両方がある
* 動的ディスパッチ
  + implicit parameterを渡すやつ
* 静的ディスパッチ
  + コンパイル時に解決してしまうやつ
* デフォフォルト静的
  + 動的を選ぶことも出来る


# 静的ディスパッチの意味
-----------------------

* Zero-Cost Abstraction
* ユーザはパフォーマンスのために設計を曲げる必要がなくなる
* インライン化などの最適化も出来る
* 逆の見方をすればZero-Cost Abstraction出来るからシステムプログラミング言語に高級な機能を入れれた


# 型クラス+α
-----------
それぞれ面白い特徴が。

* `FromStr`
* `Write`
* `Add`
* `Default`, `Zero`
* `Iterator`


# 型クラス+関数
--------------

* 関連関数
* `FromStr`
* static関数的なものになる

```rust
pub trait FromStr {
    type Err;
    fn from_str(s: &str) -> Result<Self, Self::Err>;
}
```


``` rust
use std::str::FromStr;

let s = "5";
let x = i32::from_str(s).unwrap();

assert_eq!(5, x);
```


# 型クラス+構文
--------------

* = メソッド
* 第一引数が `self` な関数はメソッド構文で呼び出せる
* クラスがなくても継承がなくてもオブジェクト指向
* `Write`

``` rust
trait Write {
  fn write(&mut self, buf: &[u8]) -> Result<usize>;
}
```


``` rust
impl Write for Foo {
  fn write(&mut self, buf: &[u8]) -> Result<usize> {
    ...
  }
}

let foo = Foo::new();
foo.write(aa);
```


# 型クラス+UFCS
---------------

* = 実質オーバーロード
* 中身の違うメソッドを複数定義出来る
* どのメソッドを呼ぶかを決定する構文がある
  + = Universal Function Call Syntax


```rust
trait Foo {
    fn foo() -> i32;
}

struct Bar;

impl Bar {
    fn foo() -> i32 {
        20
    }
}

impl Foo for Bar {
    fn foo() -> i32 {
        10
    }
}

```


```rust
<Bar as Foo>::foo();
Bar::foo();
```



# 型クラス+演算子
-----------------

* = 演算子オーバーロード
* `Add`

``` rust
pub trait Add<RHS = Self> {
    type Output;
    fn add(self, rhs: RHS) -> Self::Output;
}
```



``` rust
use std::ops::Add;

struct Foo;

impl Add for Foo {
    type Output = Foo;

    fn add(self, _rhs: Foo) -> Foo {
        println!("Adding!");
        self
    }
}

fn main() {
    Foo + Foo;
}
```

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">for (my_int i = 0; i &lt; 10; i++) { … }<br><br>Cならどういうアセンブリに落ちるかすぐわかるけどC++ならmy_intはクラスかもしれず=0はコンストラクタを起動し&lt;はメソッド呼び出しになり++はejectを発行して光学ディスクトレイが開き相手は死ぬ</p>&mdash; わさびず <a href="https://twitter.com/___yuni/status/681891856335032320">2015年12月29日</a></blockquote>

<!-- .slide: class="center" -->


# 型クラス+値
-------------

* = 関連定数
* unstable...
* `Zero`, `Default`

``` rust
// current
pub trait Zero {
    fn zero() -> Self;
}
```

``` rust
// ideal
#![feature(associated_consts)]
pub trait Zero {
    const ZERO: Self;
}
```


```rust
trait Monoid: Add<Self> + Zero
  where Self::Output : Add<Self> + Zero {
}

```


# 型クラス+型
-------------

* 関連型
* 型族…？
  + あまり違いを分かっていない
  + 関連型を持った型をまとめたのが型族？
* `Iterator`

```rust
pub trait Iterator {
  type Item;
  ....
}
```


``` rust
trait Iterator {
  ...
  fn next(&mut self) -> Option<Self::Item>;
}
```


# 型クラス+暗黙のルール
---------------------

* オーバーライド
* `Drop`
  + 暗黙に呼ばれるデストラクタをオーバーライド出来る

```rust
pub trait Drop {
    fn drop(&mut self);
}
```


``` rust
impl Drop for Lock {
    fn drop(&mut self) {
        self.free();
    }
}
```


# 型クラス+アノテーション
------------------------

* 単純に便利
* `derive`(`Debug` , `Eq`)

``` rust
#[derive(Debug, Eq)]
struct Foo(usize);
```



``` rust
let foo1 = Foo(1);
let foo2 = Foo(2);
println!("{:?} == {:?} ?: {:?}",
         foo1,
         foo2,
         foo1 == foo2);
```


# まとめ
--------

* 型クラスは便利だよ
* 型クラスの実装は効率的に出来るよ
* 型クラスを使うと言語設計も変わるよ

</script>
</section>
