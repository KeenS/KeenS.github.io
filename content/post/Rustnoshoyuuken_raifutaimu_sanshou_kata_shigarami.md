---
categories: [Rust, 型]
date: 2016-12-14T11:19:12+09:00
title: Rustの所有権、ライフタイム、参照、型、しがらみ
---

κeenです。Rustには所有権やらライフタイムやら参照やらがあって型システムが面倒ですよね。線形（アフィン）と言われてるのに「あれ？」と思う部分もあるので詳しく探っていこうと思います。

尚、この記事は型システムに興味のある人向けです。単にRustを書きたい方に有用な情報があるかは分かりません。

2016-212-14T15:28Z09:00 加筆訂正しました。diffは[こちら](https://github.com/KeenS/KeenS.github.io/commit/f86c03a3793fde1787a9c0eb47f8efda305c2be4)

<!--more-->

#  線形型？アフィン型？
はじめにこの辺をハッキりさせておきたいです。
アフィン型は線形型に弱化規則を許す型です。
プログラミング的に言えば線形型は `free` しないと型エラーになるけどアフィン型はそうでない。
Rustはご存知の通り `free` 相当のものを手で書くことはないのでアフィンな気がしますし、昔のドキュメントにもそう書いてました。

しかし実際に実行されるプログラムではコンパイラが差し込んだ `free` が実行されるので `free` を省略出来るというシンタックスシュガーの類な気もします。

ただまあ、 `free` を差し込む位置を特定するのに弱化規則を使った場所で推論している筈なのでアフィンを使ってる筈ですよね。

ってことで今回の記事はアフィン型で統一したいと思います。

# 参照も値
なんとなく、構文が用意されているので参照自体特別な存在な気がしますが、そんなことはありません。

``` rust
fn take_any<T>(t: T) -> T {
    t
}
```

このように任意の値を取れる関数を用意してあげます。
すると、以下のように所有、参照、可変参照全て渡せます。

``` rust
let s = "foo".to_string();
let mut s = take_any(s);
take_any(&s);
take_any(&mut s);
```

# `&` は `Copy` 、 `&mut` は違う

先と同じように `Copy` を実装した型を受け取る関数を書いてみましょう。

``` rust
fn take_copy<T: Copy>(t: T) -> T {
    t
}
```

これに参照を渡してあげると、 `&mut` がエラーになります。

``` rust
let mut s = "foo".to_string();
take_copy(&s);
take_copy(&mut s); // error[E0277]: the trait bound `&mut std::string::String: std::marker::Copy` is not satisfied
```

もちろん、 `Copy` なので普通に何度も使えます。線形型でいう `!` に近いですね。違いますけど。

``` rust
let s = "foo".to_string();
let ref_s = &s;
let x: &String = ref_s;
let y: &String = ref_s;
// Copy(が要求するClone)に実装されているメソッドを陽に呼ぶ
let z: &String = (&ref_s).clone();
```

また、 `Copy` への参照は参照外しが可能です。

``` rust
let x: isize = 1;
let ref_x = &x;
let x: isize = *x;
```

なので、 `&&` や `&mut&` は `&` への参照（`Copy`への参照）なので `&` へ参照外しが可能ですが、 `&&mut` は`&mut`への参照（`Copy`ではない値への参照）なので参照外し出来ません。`&mut&` や `&&mut` とややこしいですね。

``` rust
let mut s = "foo".to_string();
// &&
{
    let ref_ref_s = &&s;
    let ref_s: &String = *ref_refs;
}
// &mut &
{
    let refmut_ref_s = &mut & s;
    let reft_s: &String = *s;
}

// &&mut
{
    let ref_refmut_s = &&mut s;
    let refmut_s: &mut String = *s; // error[E0389]: cannot borrow data mutably in a `&` reference
}
```

ということで `&&&&&&&&&&&&&&&&&&&T` が `&T` に参照外し出来る理由も分かったかと思います。

# `&mut` は `Deref`

さて、 `&mut` がたまに `&` のように振る舞うことがありますが、これは `Deref` のせいです。

``` rust
use std::ops::Deref;

let mut s = "foo".to_string();
let refmut_s = &mut s;
{
    // derefを陽に呼ぶ
    let ref_s: &string = refmut_s.deref();
}

{
    // 暗黙のDerefによる型強制
    let ref_s: &string = refmut_s;
}


```

rustは[`deref` による型強制](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/deref-coercions.html)を行うため、無言で `&mut` を `&` に変換することが出来ます。


----------

さて、ここまでで `&` や `&mut` もただの値であること、アフィン型に反しないことを調べました。
次はライフタイムについて探ります。

# ライフタイムとサブタイピング

実は、Rustにはサブタイピングがあります。ライフタイムです。ライフタイムにはサブタイプ関係があるのです。

このように、2つの引数が厳密に同じライフタイムであることを要求する関数を用意します。

``` rust
fn take_two_ref<'a>(_: &'a String, _: &'a String) {
}
```

これに、違うライフタイムを持つ型を放り込んでみます。

``` rust
{
    let a = "aaa".to_string();
    let ref_a = &a;
    {
        let b = "bbb".to_string();
        let ref_b = &b;
        take_two_ref(ref_a, ref_b);
    }
}
```

これはコンパイルが通ります。明示的にライフタイムを書くとこうです。これは有効な構文ではないです（余談ですがラベル付きbreak文のための構文と衝突します）。


``` rust
'a: {
    let a: 'a String = "aaa".to_string();
    let ref_a = &'a a;
    'b: {
        let b: 'b String = "bbb".to_string();
        let ref_b = &'b b;
        take_two_ref::<'b>(ref_a, ref_b);
    }
}
```

大は小を兼ねるので `'a` は `'b` のサブタイプになっています。
ちょっと言葉がややこしいのですが、スコープが狭いライフタイムの方が大きい型です。狭い方が大きい。この言い回しに馴れてない人に説明すると、上の例でいえば `'b` を要求する部分に渡せるのは `'a` と `'b` 2つありますが、 `'a` を要求する部分に渡せるのは `'a` だけです。 2つと1つだったら2つの方が大きいですよね。

さて、今回の例ではちゃんとスコープの入れ子とサブタイプ関係が一致していました。

``` rust
'a: {----------
              |
              |
    'b: {--   |
          |   |
          |   |
          |   |
    }------   |
}--------------

```

参照を取るときは参照のライフタイムが元のライフタイムのサブタイプになっている必要があります。

この関係に反するとエラーになります。

``` rust
let ref_s: &String;
{
    let s = "foo".to_string();
    ref_s = &s; // error: `s` does not live long enough
}
```

図示するとこうでしょうか。

``` rust
let ref_s: &String;
{
    let s = "foo".to_string();-----
    ref_s = &s; ----------------  |
                               |  |
}------------------------------+---
                               |
--------------------------------
```

ライフタイムといえどある種、型なんですね。余談ですが確かリージョンの論文には「リージョンはプログラム全体を静的に伝播する型のようなもの」と表現していた気がします。既存の型と直行するので型とは別とも言えますし、タプルのように両方並べたものを型と言うことも出来ますね。

# 所有型とライフタイムと `'static`
適切な語彙がなかったので勝手に所有型と名付けましたが、参照でない型のことを指しています。（先の例で見せた通り、参照型にも所有権はあるので適切ではないですがひとまず我慢して下さい。）

さて、なんとなくライフタイムと参照型は結び付いている気がしますが、参照じゃない型にもライフタイムはあります。

``` rust
fn take_two_value<'a, T: 'a, S: 'a>(_: T, _: S){}
```

このように、任意の2つの型を取るけども両方ともにライフタイムを要求する関数を書いてみます。

これに、所有型を渡すことが出来ます。

``` rust
let t = "t".to_string();
{
    let s = "s".to_string();
    let s_ref = &s;
    take_two_value(t, s_ref);
}
```

ということで、所有型にもライフタイムはあります（多分）。

ところで、ライフタイムのボトム型相当のものに `'static` というものがあります。
グローバル変数やリテラルなどに割り当てられるライフタイムで、プログラムが死ぬまで生き続けます。


``` rust
static foo: &'static isize = &3;
static bar: isize = 3;

fn main() {
    let x: &'static str = "x";
    let ref_bar: &'static isize = &bar
}
```

`'static` のみを受け取る関数を書いてみましょう。

``` rust
fn take_static<T: 'static>(_: T){}
```

もちろん、先の例に出たstaticな値達を渡せます。

``` rust
static foo: &'static isize = &3;
static bar: isize = 3;

fn main() {
    let x: &'static str = "x";
    let ref_bar: &'static isize = &bar;
    take_static(foo);
    take_static(x);
    take_static(ref_bar);
}
```

実は、これに所有型を渡せます。

``` rust
let s = "foo".to_string();
take_static(s);
```

そして、任意の参照型は渡せません。

``` rust
let s = "foo".to_string();
take_static(&s); // error: `s` does not live long enough
```

ということで所有型には `'static` というライフタイムが付いているようです（？）。
`'static` イメージとしては「自分がその値を保持している限り無効になることはない型」ですかね。

これに関連するTipsとして関数や構造体のジェネリクスで所有型しか受け取らないようにするには `<T: 'static>` が使えます。


-----------------

さてさて、これまた参照型は特別扱いされることなく他のRustの値と同じようにライフタイムで管理されていることを調べました。

最後に今回到達出来なかった謎、「参照を取る」について考えたいと思います。

# `&mut` の規則

みなさん御存じの通り、`&mut` を取ると値への他の操作が許されなくなります。

``` rust
let mut s = "foo".to_string();
let refmut_s = &mut s;
let t = s;  // error[E0505]: cannot move out of `s` because it is borrowed
let ref_s = &s; // error[E0502]: cannot borrow `s` as immutable because it is also borrowed as mutable
```

`&mut T` と `T` が相互排他的なのでこれは線形論理のシーケント計算っぽく書くと以下のような規則を考えれば良さそうです。

```
Γ, T:'a |- Σ
-------------- &mut-intro
Γ, (&'a mut T): 'b |- Σ
where 'a <: 'b

Γ, (&'a mut T):'b |- (&'a mut T): 'b, Σ
----------------------- &mut-elim
Γ, T: 'a |- Σ
```

型付け規則の書き方になれてないのでシーケント計算で書きましたがサブタイプの記述で困りましたね。

# `&` の謎

さて、問題は `&` です。 `&` は `Copy` なのでそこら中に生えてきます。
なので `&-elim` のような規則では対応出来ません。

ただ、ライフタイムによる制約があるので「参照のライフタイムが死ぬときに元の値が復活する」ような規則を考えたくなります。

しかし、それでもだめです。複数回 `&` を取れてしまうので、以下のように複数のライフタイムを持つ場合で破綻します。`& T` と `T` の間でなにかしらのしがらみがある筈です。


``` rust
let s = "foo".to_string();
{
    let ref_s1 = &s;
    {
        let ref_s2 = &s;
    }
}
```

それに、 `&mut` と `&` が相互排他であることも説明出来る規則でないといけません。謎が多い。

---------------------------

# まとめ
さて、Rustの型システムのリソース管理回りを探ってみました。とりあえず分かったことを纏めると

* 値には全てアフィン型が付く
  + 参照型も例外ではない
* 値には全てライフタイムが付く
  + 所有型も例外ではないっぽい
  + もしかしたら所有型はライフタイムを無視している？
* 「参照を取る」のセマンティクスが謎
  + 勿論、直感的には分かる
  + どういう規則なんだろう
  + アフィン型とライフタイム両方が絡むはず。

特に論文も読んでないのでちょっと遠回りだったかもしれません。
もしかしたら論文漁ったら一発で解決するのかも。
詳しい方、 `&` の謎を教えて下さい。
