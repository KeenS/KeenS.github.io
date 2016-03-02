---
categories: [Rust]
date: 2016-03-01T23:32:12+09:00
title: Rustのゼロコスト抽象化
---

κeenです。今日Twitter上でのやりとりから少し面白いことが分かったのでそれについて。

<!--more-->

最近1.0が出たKotlinについて、水島さんがツイートしてました。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">nullableに対してはmapとかの高階関数を一切使えないのが痛い。 ?. でカバーできるケースは一部だけだ。zero-overhead null-safetyと唄っとるが、代わりにnullチェックお化けになるわけで、どこがzero-overheadだ <a href="https://twitter.com/hashtag/kotlin_dis?src=hash">#kotlin_dis</a></p>&mdash; 水島 宏太(Klassic作成中) (@kmizu) <a href="https://twitter.com/kmizu/status/704453235277324288">2016年2月29日</a></blockquote>

それについて私が無関係なツイートを。


<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">全く無関係だけどRustはOptionみたいな0-1の型をnull or valueに最適化するそうな。これこそがゼロコスト抽象かな <a href="https://t.co/5Y7cBEyrMe">https://t.co/5Y7cBEyrMe</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/704453898996547584">2016年2月29日</a></blockquote>

これはRustのnomiconに書かれています。


[repr(Rust)](https://doc.rust-lang.org/nomicon/repr-rust.html)


そうすると水島さんからお返事が。


<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> Rust詳しくないですけど、Optionにmapとかした場合インライン展開されるんですかね？だとしたらとても理想的。</p>&mdash; 水島 宏太(Klassic作成中) (@kmizu) <a href="https://twitter.com/kmizu/status/704455185125408768">2016年2月29日</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>


確かにそうなると面白そう。ということで少し調べてみました。


まず、上記の話をまとめると、Rustの`Option`に対する`map`

``` rust
pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Option<U> {
    match self {
        Some(x) => Some(f(x)),
        None => None,
    }
}
```

は`x`がポインタ型だった時に以下と同値です。

``` rust
pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Option<U> {
    if (self != nullPointer ) {
        f(x)
    }
}
```


さらに、[`map`はインライン宣言されている](https://github.com/rust-lang/rust/blob/master/src/libcore/option.rs#L386)ので以下のコード

``` rust
let opt = Some(&v);
opt.map(|x| x + 1);
```


は以下と同値です。

``` rust
let opt = &v;
if (opt != nullPointer) {
  (|x| x + 1)(opt)
};

```

さて、ここで無名関数がどうコンパイルされるかという問題が出てきますが、[クロージャのドキュメント](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/closures.html)によるとこういう雰囲気のコードになるらしいです。


``` rust
let opt = &v;
struct AnonymousType;

impl FnOnce<(&i32)> for AnonymousType {
    type Output = i32;
    fn call_once(self, args: (&i32)) -> Self::Output {
        args + 1
    }
}

if (opt != nullPointer) {
    let fn_once: FnOnce = AnonymousType;
    fn_once.call_once(opt)
};

```

思ったよりも複雑…。さて、問題は`let fn_once: FnOnce = AnonymousType;`としているので一旦元の無名関数の情報が抜けてしまいそうな気がします。
となるとコンパイル時に具体的なメソッドを決定出来ないので`fn_once.call_once(opt);`は以下のような雰囲気のコードになってしまいます。

``` rust
let call_once_fn = fn_once.get_call_once_fn();
call_once_fn(opt);
```

毎回呼び出すべき関数の取得が入るのは面倒ですね。


しかしなががらクロージャのドキュメントをよく読むと無名関数は静的ディスパッチされると書いてあります。つまり、

``` rust
let call_once_fn = fn_once.get_call_once_fn();
call_once_fn(opt);
```

と2段ではなく

``` rust
the_call_once_fn_of_AnonymousType(opt);
```

とコンパイルされ、

よって

``` rust
let opt = Some(&v);
opt.map(|x| x + 1);
```

は

``` rust
fn the_call_once_fn_of_AnonymousType(x: &i32) -> i32 {
    x + 1
}


let opt = &v;
if (opt != nullPointer) {
  the_call_once_fn_of_AnonymousType(opt)
};

```
と同値ということです。

ここからは私の推測ですが、`the_call_once_fn_of_AnonymousType`は本体が小さい上に1回しか呼ばれないのでインライン化されるのではないかと思います。
よってこの推測が正しければ

``` rust
let opt = Some(&v);
opt.map(|x| x + 1);
```

は


``` rust
let opt = &v;
if (opt != nullPointer) {
  opt + 1
};

```

となる筈です。

ゼロコスト抽象化すごい!
