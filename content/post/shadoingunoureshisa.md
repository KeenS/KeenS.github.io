---
categories: ["言語設計", "Rust"]
date: 2020-06-20T00:28:15+09:00
title: "シャドイングの嬉しさ"
---
κeenです。
Rustに限った話ではないのですが、よくRustを触ってみた人がシャドイングが気持ち悪いという反応をしているのを見かけるのでシャドイングがどういう機能かを解説します。

<!--more-->

# Rustのシャドイング

まずは「気持ち悪い」と言われることもあるRustのシャドイングについて。
Rustでは以下のコードが合法です。

```rust
let input = "42";
let input = input.parse::<i32>().unwrap();
// 以後、i32 型のinputが見える
let result = input * 2;
```

`input` という名前の変数を2回導入しています。
最初の `input` は `&str` 型で、2つ目の `input` は `i32` 型です。
このコード以降では後に定義した `i32` 型の方の `input` が有効になっています。
後から導入した変数が先にあった変数を覆い隠す（shadow）ので、シャドイングと呼ばれます。

これを見てどう感じたでしょうか。
色々感想はあるかと思いますが、他の言語の例も見ながらこれが何をしているのかを調べたいと思います。

# 変数は箱ではない

最初に確認しておきたいのは、上記のRustのコードは1つの変数を `&str` 型に使ったり `i32` 型に使ったりしている訳ではないという点です。
1つ目の `input` と2つ目の `input` は別物です。
強調するなら以下のように `input_1` 、 `input_2` と別の変数を導入しているのと考えることもできます。

```rust
let input_1 = "42";
let input_2 = input_1.parse::<i32>().unwrap();
```

このような変数のリネーミングをコンパイラが自動でやっているわけです。

ここで少し束縛（binding）と代入（assignment）の違いに触れておきましょう。
どちらも変数と値の関係を作る操作ですが、それぞれ使い分けられています。
言語によって言葉の使い方がそれぞれ微妙に違うので厳密にどうこう言えるものではないですが、おおまかな傾向として違いをみてみましょう。

## 束縛

束縛は変数を値に紐付ける操作です。
「変数は値に一時的につけた名前」という気持ちです。
変数があるなしに関わらず値は存在し、それに人間に分かりやすいように名札をつけたのが変数です。
値に紐付けられた変数は他の値につけかえられないのが一般的です。
特別なことがなければ、一度値に名前をつけたら値と名前の関係は変わりませんし値が変更されることもありません。
シャドイングも変数をつけかえるのではなく、新しい変数で覆い隠すと説明しましたね。

Rustの `let` や、ほとんどの関数型言語での変数束縛がこの挙動です。

## 代入

代入は変数が用意した領域に値を置く操作です。
まず領域を指し示す変数というものがあり、そこに値を書き込みます。
このとき、変数の型と値の型が一致してないといけません。
大抵の手続型言語ではこのような気持で変数を使っているかと思います。
このようなモデルだと変数と領域が結び付いているので、気軽に変数宣言を覆い隠したり名前は一緒なのに別の型で変数を宣言したりするともとの領域が気になって仕方がないですよね。

Rustでは `let mut` で導入した変数には代入が可能です。
代入と束縛の違いを強調したいときは代入の方を再代入や破壊的代入と呼んだりします。

## 束縛と代入の違い

Rustには束縛と（再）代入両方があります。
少し挙動の違いを見てみましょう。

まずは再代入を使ったコードです。
`let mut` で変数を宣言すると変数に再代入ができるようになります。

``` rust
let mut n = 0;
for i in 0..10 {
    // 再代入
    n = n + i;
}
println!("n = {}", n);
```

このコードはループ内で再代入しているので外側で定義された変数 `n` を更新しています。
結果、 `n = 45` という出力が得られます。


次に束縛を使ったコードを見てみましょう。

``` rust
let n = 0;
for i in 0..10 {
    // 束縛
    let n = n + i;
    // n のスコープはここで終わる
}
println!("n = {}", n);
```

ループの中で束縛をしています。
ですが、`n` のスコープは `for` 内に閉じているので外側には影響ありません。
結果、 `n = 0` という出力が得られます。

結果に釈然としない方は先程のように変数名を変えると分かりやすいですかね。

``` rust
let n_0 = 0;
for i in 0..10 {
    // 束縛
    let n_1 = n_0 + i;
    // n_1 のスコープはここで終わる
}
println!("n = {}", n_0);
```

`n_0` は特に更新されていないのが見てとれると思います。

## 束縛と代入の使い分け

束縛と代入は言語の設計の違いを反映しています。
関数型言語では値を変更するとバグに繋るから破壊的変更は許さないといい、手続型言語では値を書き換えながら処理を進めるのが自然だといいます。

因みにコンパイラの内部では束縛に近いコード、値の書き換えを許さない[SSA](https://ja.wikipedia.org/wiki/%E9%9D%99%E7%9A%84%E5%8D%98%E4%B8%80%E4%BB%A3%E5%85%A5)という形式に変換してからマシン語へとコンパイルします。
なのでどちらが効率的とかはないです。

Rustはデフォルトでは再代入できないようになっていますね。
関数型言語の破壊的変更があるとバグに繋がるという言を採用したようです。
`let mut` があるので必要なところでは破壊的変更はできますが、可能な限り減らすことでバグの入る余地を減らそうという設計です。

ここまでで以下のことが分かりました。

* 冒頭のコードは変数を使い回しているのではなく、同じ名前の変数を2回導入している
* 変数束縛と変数への代入は別物

次はシャドイングについてみていきましょう。

# シャドイングは色々な言語にある

「シャドイングが気持ち悪い」と言われがちですが、実はシャドイングそのものは大抵の言語にあります。

例えばCでもシャドイングがあります。

```C
// char *の変数を宣言
char *i = "i";
{
  // intで宣言しなおし
  int i = 42;
  // 以後 `int` 型の `i` が見える
}
// `int` の `i` のスコープが終わったので
// hogeここからは `char*` 型の `i` が見える
```

内側のスコープで外側のスコープの変数をシャドイングするのは一般的といっていいでしょう。
シャドイングそのものは大抵の言語にある機能なのです。

Rustで見慣れないのはシャドイングではなく、スコープの作り方です。
Rustでは `let` 文1つにつき1つスコープが作られているのです。
先のRustのコードは大袈裟な書き方をすれば以下のようになっています。

```rust
let input = "42";
{
    let input = input.parse::<i32>().unwrap();
    // 以後、i32 型のinputが見える
}
```

なので正確に言えば「Rustのシャドイングが気持ち悪い」ではなく、「Rustのシャドイングを推進するようなスコープが気持ち悪い」ですね。
このような設計はMLなどの関数型言語ではよくあります。MLはRustが設計時に参考にした言語の1種類ですね。
一旦MLに寄り道して変数をどう扱っているか様子を眺めてみましょう。

# MLの `let`

基本的に破壊的変更を許さない関数型言語では変数が再利用できるのは重要です。
手続型言語とは違って、操作を加えるときに値を変更するのではなく新しい値を返すのでその都度変数が必要になるからです。

リストに要素を追加する例を手続的な言語と関数型言語でみてみましょう。

Javaだとこうなります。

```java
List<Integer> list = new ArrayList<>();
list.add(1);

// 以後 `1` を格納したリストが使える
```

`list` という変数を変更して1を追加しています。

同じようなコードをOCamlでも書いてみましょう。
OCamlのローカルの変数束縛は `let 変数 = 値 in 変数を使う式` という構文です。
ほぼRustの `let 変数 = 値; 後続の式` と同じような使い方をします。
この `let` 式を使ったコードがこちら。

```ocaml
let list = [] in
let list = 1 :: list in
(* 以後 `1` を格納したリストが使える *)
```

`list` という変数を元に作った新たな値に `list` という名前をつけています。
この場合、古い方の `list` にはもう用事がないのでシャドイングで見えなくしてしまうのは理に適っています。

手続型のコードで破壊的変更していた部分で、関数型のコードではシャドイングが使われています。
値の更新はよくある操作ですから頻繁にシャドイングしたくなりますね。
シャドイングが簡単にできるような設計は合理性があるのです。

蛇足ですが値の破壊的変更とシャドイングはシャドイングの方がコード上追いかけやすいです。
値の変更はプログラムのフロー全てを（場合によっては呼び出した関数の先まで）追わないと分かりませんが、シャドイングの関係はプログラムの字面から簡単に分析できます。

## Rustとの関連

ここでRustに戻ってきます。
OCamlの `let` を参考にしましたといえばそれまでですが、一応このような設計がRustでも合理的か考えてみます。
Rustは所有権の関係上、値を新しく生成して返すというAPIが多くあります。
それを考えると破壊的変更を許す言語であってもこのようにど新しい値をどんどの新しい変数に束縛していけるような設計は便利そうです。
さらに、Rustにはムーブがあるので「名前空間には存在しているがもう使えない変数」というのも発生します。
その面でも名前を再利用できるシャドイングは相性がいいんじゃないかと思います。


ここまでで以下のことを確認しました。

* Rustで見慣れないのはシャドイングではなくスコープ
* こういうスコープの作り方には一定の合理性がある

最後に、可読性の話題をとりあげます。

# シャドイングと可読性

シャドイング（というか変数宣言のスコープ）の便利さを理解してもらったところで、次に言われるのが可読性の話です。
「それ、可読性下がらないの？」「意図せずシャドイングしてバグを生みそう」などの声が聞こえてきます。
実際に使ってる人は「そんなことはない」ときっぱり否定するので試したことのない人が言っているようですが、その言を紹介しておきましょう。
ついでにシャドイングがないことによる可読性の低下やバグも紹介しておきます。

## シャドイングをすることによるバグ

まだ使っている変数を意図せずシャドイングしてしまい、あまつさえそれに気付かずに使ってしまうというものです。

``` rust
let data = get_data();

// ...

let data = new_data();

// ...

data.do_something();
// ↑ ここで最初の方の `data` を使っているつもりだが、
// 実際に使われるのは2つ目の方
```


経験上、こういうバグを出してしまうことはほぼないです。
スコープにある変数が管理できなくなるほど大きな関数でも書かない限りまずやらないです。

## シャドイングをしないことによるバグ

一方でシャドイングがあれば防げたような類のバグもあります。
既に使わない、あるいは使ってはいけないような値にアクセスしてしまうものです。

``` rust
let data = get_data();

// ...

let data2 = new_data();

// ...

data.do_something();
// ↑ ここで2つ目の方の `data2` を使っているつもりだが、
// 実際に使われるのは最初の方
```

シャドイングしないと変数のスコープが不要に長くなってしまうので起きる問題です。
これも、変数が管理できなくなるほど大きなメソッドを書かない限りまずやらないでしょう。

## シャドイングをすることによる可読性の低下

変数が頻繁に上書かれるので最終的にどの値が有効なのかよく分からないというものです。
コードだとまさしく最初に上げたようなものがあてはまるでしょうか。

```rust
let input = "42";
let input = input.parse::<i32>().unwrap();
// 以後、i32 型のinputが見える
```

私はむしろ変数が散らばるよりも使わなくなった変数を隠してくれた方が読みやすいと感じるのですが、こういう意見もあるようです。

## シャドイングをしないことによる可読性の低下

一方でシャドイングせずに不適切に変数を使い回すのも考えものです。
違うものなのにたまたま同じスコープにあって、たまたま型が同じ値に1つの変数を使い回す例があります。
典型的なのはCの返り値などでしょうか。

``` c
int ret;

ret = func1();
if (ret) {
    return ret;
}

ret = func2();
if (ret) {
    return ret;
}
```

これを自然と捉える人もいるかもしれませんが、私は変数の役割が途中で変化してて分かりづらいなぁと思います。


# まとめ

Rustのシャドイングが気持ち悪いと言われる件について、背景を説明しました。
ごちゃごちゃ言わずに使ってみればいいと思います。
