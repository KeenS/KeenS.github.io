---
categories: ["Rust", "Advent Calendar", "Advent Calendar 2018", "Rust Advent Calendar"]
date: 2018-12-25T16:20:46+09:00
title: "Rustのマクロでネストした呼び出しを実現するために"
---

このエントリは [Rustその2 Advent Calendar 2018](https://qiita.com/advent-calendar/2018/rust2)の19日目のの記事です。時空を遡って書いています。

κeenです。

マクロのためのユーティリティマクロを書いていると呼び出しをネストしたくなりますが、簡単には実現できないのでそれについて。
あるいは[過去記事](https://keens.github.io/blog/2018/02/17/makurokurabu_rustshibu/)の前半と後半のギャップについて。
<!--more-->

TL;DR: CPSする

例えば構造体定義をラップしてそれをJSON風に出力するメソッドを生やすマクロを考えてみましょう。
こんなのです。


```rust
macro_rules! json {
    (struct $name: ident {
        $($field: ident : $ty: ty,)*
    }) => {
        struct $name {
            $($field: $ty),+
        }

        impl $name {
            fn print_json(&self)  {
                println!("{{");
                $(
                    println!("  {}: {},", stringify!($field), self.$field);
                )*
                println!("}}");
            }
        }
    };
}
```

こういう風に使います。

``` rust
json! {
    struct Person {
        name: String,
        age: u32,
    }
}

fn main() {
    let p = Person {
        name: "κeen".into(),
        age: 26,
    };
    p.print_json()
    // {
    //   name: κeen,
    //   age: 26,
    // }
}

```

クォートを忘れてますがご愛嬌。これをもう少し拡張して、フィールドに `#[json]` とつけたもののみ出力したいとしましょう。
以下のような動きです。

``` rust
// nameにのみ #[json] を付ける
json! {
    struct Person {
        #[json]
        name: String,
        age: u32,
    }
}

fn main() {
    let p = Person {
        name: "κeen".into(),
        age: 26,
    };
    p.print_json()
    // nameのみ出力される
    // {
    //   name: κeen,
    // }
}

```


それがかなり面倒だよ、というお話。
まず、今のRust(1.31)ではあってもなくてもいいというパターンは書けません(つい最近nightlyに[入った](https://github.com/rust-lang/rust/issues/48075))。
ではどうするかというとひとまず「なんでもあり」のパターンで受け取って、あとで自分でパースします。
何でもありのパターンは `tt` の繰り返しで書けます。こんな感じですね。

``` rust
macro_rules! json {
    (struct $name: ident {
        $($tt: tt)*
    }) => {
        // ...
    };
}
```

ここから構造体のフィールド定義パターンを抜き出すユーティリティマクロを定義しましょう。
こんな感じですかね。この時点で結構Hackyなんですがコメントを読めば大体分かるかと思います。
因みにマクロの「ループ = 再帰」、「マクロの分岐 = パターンマッチ」です。

```rust
// 全体的に、 `collect_fields!(中間結果 | 入力)` という形になっている
// 入力を少しずつ切り出して中間結果に加えていく。
macro_rules! collect_fields {
    // `#[json]` が付かないフィールドをそのまま抜き出す
    ($($fields:ident : $tys:ty,)* | $field: ident : $ty: ty, $($tt:tt)*) => {
        collect_fields!($($fields: $tys,)* $field: $ty, | $($tt)*);
    };
    // `#[json]` が付くフィールドは `#[json]` 以外を抜き出す
    ($($fields:ident : $tys:ty,)* | #[json] $field: ident : $ty: ty, $($tt:tt)*) => {
        collect_fields!($($fields: $tys,)* $field: $ty, | $($tt)*);
    };

    // 入力が終わったら集めた値を返す
    ($($fields:ident : $tys:ty,)* | ) => {
        $($fields : $tys,)*
    };

}
```

このマクロを用いて以下のように構造体を定義してみましょう。

``` rust
macro_rules! json {
    (struct $name: ident {
        $($tt: tt)*
    }) => {

        struct $name {
            collect_fields!(|$($tt)*)
        }

        // ...
    };
}
```

しかしコンパイルするとエラーになります。

``` console
error: expected `:`, found `!`
  --> macro_cps.rs:50:27
   |
50 |               collect_fields!(|$($tt)*)
   |                             ^ expected `:`
...
67 | / json! {
68 | |     struct Person {
69 | |         name: String,
70 | |         age: u32,
71 | |     }
72 | | }
   | |_- in this macro invocation

error: aborting due to previous error
```

構造体定義にはマクロを書けないんですね。まあ、言われてみれば当たり前です。
ならば、と構造体を定義するマクロを定義してみましょう。`collect_fields` の返り値を受け取って構造体定義に展開します。

``` rust
macro_rules! def_struct {
    ($name: ident, $($field: ident : $ty: ty,)*
    ) => {
        struct $name {
            $($field: $ty),+
        }
    }
}
```


これを使ってみましょう。


``` rust
macro_rules! json {
    (struct $name: ident {
        $($tt: tt)*
    }) => {

        def_struct!($name, collect_fields!(|$($tt)*));
    };
}
```

しかしやはりこれもエラーになります。

``` console
error: no rules expected the token `!`
  --> macro_cps.rs:48:42
   |
48 |           def_struct!($name, collect_fields!(|$($tt)*));
   |                                            ^
...
63 | / json! {
64 | |     struct Person {
65 | |         name: String,
66 | |         age: u32,
67 | |     }
68 | | }
   | |_- in this macro invocation

error: aborting due to previous error
```

少し意外かもしれまれん。普段書いているプログラムはネストした式の内側から計算するので何も考えずに引数に書くと上手く計算してくれます。
しかしマクロは外側から計算するのでこれはネストした式のように見えてそうではありません。
実際には `def_struct` に4つの引数 `$name`, `$collect_fields`, `!`, `(|$($tt)*)` を渡しているだけなのです(括弧のある式の数え方は場合によります)。

これを上手くやるのが今回の主題です。
外側から計算されるなら先に計算したい `collect_fields` を外側に持ってくればいいのです。
その代わり、結果を使いたい `def_struct` マクロをコールバックとして渡してあげます。
`collect_fields` を改造して引数の先頭にコールバックを受け取るようにしてみましょう。
コールバックは中途半端に引数が与えられた状態で、 `collect_fields` の結果を引数の最後に加えると正しくなるとしましょう。


``` rust
// 全体的に、 `collect_fields!(コールバック!(部分引数)|中間結果 | 入力)` という形になっている
// 入力を少しずつ切り出して中間結果に加えていく。
macro_rules! collect_fields {
    // `#[json]` が付かないフィールドをそのまま抜き出す
    ($callback:ident !($($args:tt)*) | $($fields:ident : $tys:ty,)* | $field: ident : $ty: ty, $($tt:tt)*) => {
        collect_fields!($callback!($($args)*) | $($fields: $tys,)* $field: $ty, | $($tt)*);
    };
    // `#[json]` が付くフィールドは `#[json]` 以外を抜き出す
    ($callback: ident !($($args:tt)*) | $($fields:ident : $tys:ty,)* | #[json] $field: ident : $ty: ty, $($tt:tt)*) => {
        collect_fields!($callback!($($args)*) | $($fields: $tys,)* $field: $ty, | $($tt)*);
    };

    // 入力が終わったら集めた値を返す
    ($callback: ident !($($args:tt)*) | $($fields:ident : $tys:ty,)* | ) => {
        $callback!($($args)* $($fields : $tys,)*);
    };

}

```

呼び出しはこう書きます。


``` rust
macro_rules! json {
    (struct $name: ident {
        $($tt: tt)*
    }) => {
        collect_fields!(def_struct!($name, ) | |$($tt)*);

        // ...
   };
}
```


これは意図通りコンパイルが通ります。めでたしめでたし。

あとは同様に `collect_json_fields` と `print_json` を書いて下さい。

最終的なコードは[ここ](https://gitlab.com/snippets/1792722)に置いておきます。
