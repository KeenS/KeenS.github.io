---
categories: [Rust, 構文解析]
date: 2018-06-26T16:02:24+09:00
description: "Rust LTでの発表用"
title: "nom勘所"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# nom勘所
----------------------
[Rust LT](https://rust.connpass.com/event/88656/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# nom
-----

* [GitHub](https://github.com/Geal/nom)
* マクロベースのパーサコンビネータ
* ゼロコピー、速い、バイト指向

===
# nomの使い方
------------

* [ドキュメント](https://docs.rs/nom/4.0.0/nom/)に大量にパーツがあるので分かりづらい
* 基本的なパーツは以下
  + `named!` - パーサを定義
  + `do_parse!` - 連接
  + `alt(_complete)!` - 選択
  + `tag!` - トークン
  + `map!` - 型変換
* 1データ型1パーサくらいの感覚
* あとは必要に応じて覚える
  + 似たような実装を探して真似る

==
# 例: 四則演算
-------------
まずは列挙型を定義

```rust
#[derive(Debug, Clone, PartialEq)]
enum Expr {
    BinOp(BinOp),
    Number(Number),
}
```

===
# 例: 四則演算
-------------

対応するパーサを`alt!`で定義


``` rust
named!(
    expr<&str, Expr>,
    alt_complete!(
        map!(binop, Expr::BinOp) |
        map!(number, Expr::Number)
    )
);
```

===
# 二項演算
------

まずは構造体を定義

``` rust
#[derive(Debug, Clone, PartialEq)]
struct BinOp {
    l: Box<Expr>,
    op: Op,
    r: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}
```

===
# 二項演算
------
対応するパーサを定義(ただしバグあり)

``` rust
named!(binop<&str, BinOp>, do_parse! (
    l: map!(expr, Box::new) >> multispace0 >>
        op: op >> multispace0 >>
        r: map!(expr, Box::new) >>
        (BinOp{l, op, r})
));

named!(op<&str, Op>, alt!(
    map!(tag!("+"), |_|Op::Add) |
    map!(tag!("-"), |_|Op::Sub) |
    map!(tag!("*"), |_|Op::Mul) |
    map!(tag!("/"), |_|Op::Div)
));

```

===
# 数値
------

型そのものは簡単

``` rust
#[derive(Debug, Clone, PartialEq)]
struct Number(i64);
named!(number<&str, Number>, map!(atom_number, Number));
```

数値のフォーマットのパースは？

===
# 数値
------

* `{符号}?{数値}` のフォーマット
* rustの`parse!`が使いたい
  - `do_parse!` だと符号と数値が別になる
  - 別々にパースしてconcatも出来るけどゼロコピーしたい
* `recognize!` があるよ

===
# 数値
------

``` rust
named!(
    atom_number<&str, i64>,
    map!(
        // recognizeで文字列全体を受け取る
        recognize!(
            // do_parse!で文字列のみ認識。変換はしない
            do_parse!(
                opt!(tuple!(tag_s!("-"), multispace0)) >>
                    digit >> ()

            )),
            |n: &str| n.parse().unwrap()
    )
);
```

===
# 優先順位
----------

* 四則演算に優先順位をつけたい
* 左結合

|         | 優先度 |
|---------|--------|
| 数値    | 3      |
| `*` `/` | 2      |
| `+` `-` | 1      |


===
# 左結合
----------

``` rust
1 + 2 + 3
```
↓
``` rust
   +
  / \
 +  3
/ \
1 2
```

===
#  優先順位
-----------

* 優先度に合わせてパーサも3つに分ける
* `binop3`, `binop2`, `binop1`

===
#  優先順位
-----------

まだバグってる


``` rust
named!(expr1 <&str, Expr>, alt_complete!(
    map!(binop1, Expr::BinOp) |
    expr2
));

named!(binop1<&str, BinOp>, do_parse! (
    l: map!(expr1, Box::new) >> multispace0 >>
        op: alt!(map!(tag!("+"), |_|Op::Add) | map!(tag!("-"), |_|Op::Sub)) >> multispace0 >>
        r: map!(expr2, Box::new) >>
        (BinOp {l, op, r})
));
```

===
# 左再帰
--------

* `binop1` -> `expr1` -> `binop1` で無限ループ
* 手でインライン化すると解決
  + 頑張って`loop`を書く

===

# 左再帰の除去
--------

``` rust
fn binop1(input: &str) -> IResult<&str, BinOp> {
    named!(parse_op <&str, Op>,
           alt!(map!(tag!("+"), |_|Op::Add) | map!(tag!("-"), |_|Op::Sub)));

    let (input, l) = expr2(input)?;
    let (input, _) = multispace0(input)?;
    let (input, op) = parse_op(input)?;
    let (input, _) = multispace0(input)?;
    let (input, r) = expr2(input)?;
    let mut ret = BinOp {
        l: Box::new(l),
        op: op,
        r: Box::new(r),
    };
    let mut input_mut = input;
    loop {
        let (input, op) = match parse_op(input_mut) {
            Ok(ok) => ok,
            Err(_) => return Ok((input_mut, ret)),
        };
        let (input, _) = multispace0(input)?;
        let (input, r) = expr2(input)?;
        ret = BinOp {
            l: Box::new(Expr::BinOp(ret)),
            op: op,
            r: Box::new(r),
        };
        input_mut = input;
    }
}

```

===
# マクロエラー
-------------

このエラーの原因分かる人？

``` rust
named!(expr3 <&str, Expr>, map!(number Expr::Number));
```

↓

<code><pre>
error: no rules expected the token `i`
   --> src/main.rs:117:1
    |
117 | named!(expr3 <&str, Expr>, map!(number Expr::Number));
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |
    = note: this error originates in a macro outside of the current crate (in Nightly builds, run with -Z external-macro-backtrace for more info)
</pre></code>

===
# 今日話さなかったこと
--------------------

* 手軽なエラーの出し方
* 空白の扱い
  + 多分一番難易度高い

===
# まとめ
--------
* nomはマクロベースのパーサコンビネータだよ
* `named`, `do_parse`, `alt`, `tag`, `map` を覚えよう
* だいたい型とパーサが対応するよ
* 演算子の扱いは難易度高いよ
* コード: [KeenS/calc](https://github.com/KeenS/calc)


</script>
</section>
