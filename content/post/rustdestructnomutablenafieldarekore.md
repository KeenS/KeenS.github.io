---
categories: [Rust, Rust1.0]
date: 2015-06-14T17:53:11+09:00
title: Rustでstructのmutableなfieldあれこれ
---
κeenです。人々にRustを薦めておきながら本人は昨日ようやく入門しました。その時に困ったことをメモ。タイトルがルー語になってますが気にしない。

因みにこれはRust 1.0の情報です。
<!--more-->

# 導入
Rustを知らない人のために説明すると、Rustの値はデフォルトでイミュータブルです。デフォルトで、というのはもちろんミュータブルにすることも出来ます。
標準ライブラリにも値がイミュータブルであることを要求するものもあります。
そしてミュータビリティは`mut`として型にも現れます。厳密に同じかは知りませんが`const`の逆、と思えばいいでしょう。

```rust
struct Point {
    x: isize,
    y: isize
}

fn double(p: &mut Point) {
    p.x = p.x * 2;
    p.y = p.y * 2;
}

fn main(){
    let mut p1 = Point{x: 1, y: 2};
    let p2 = Point{x: 1, y: 2};
    double(&mut p1);
    double(&mut p2); // error! p2 is immutable
}
```

イミュータビリティは継承します。親のstructがイミュータブルなら子もイミュータルになります。因みにフィールドに`mut`を指定することは出来ないようです。

```rust
use std::collections::HashMap;

struct IntHashMap {
    hash: HashMap<isize, isize>
}
fn main(){
    let h = IntHashMap{hash: HashMap::new()};
    h.hash.insert(1, 2);        // error! h.hash is immutable
}
```

最後に、少し本筋とずれますがtraitについて。他の言語でいうインターフェースのようなものです。今回これで困ったので。

例えばHTTPライブラリの[hyper](http://hyper.rs/hyper/hyper/server/trait.Handler.html)では次のようなトレイトを実装しているstructをrequest handlerとして登録できます。

```rust
pub trait Handler: Sync + Send {
    fn handle<'a, 'k>(&'a self, Request<'a, 'k>, Response<'a, Fresh>);

    fn check_continue(&self, _: (&Method, &RequestUri, &Headers)) -> StatusCode { ... }
}
```

これを見て下さい。

```rust
fn handle<'a, 'k>(&'a self, Request<'a, 'k>, Response<'a, Fresh>);
```

`self`に`mut`がついてませんね。つまりhandlerはイミュータブルな値として渡されます。例えば先の例のようにフィールドにハッシュマップを持っていても更新出来ません。ちょっと困りますね。

# Cell/RefCell
ということでフィールドにミュータビリティを入れるのが `std::cell::{Cell, RefCell}` です。この辺のブログを参考に。

[#rustlang における構造体のmutabilityと`Cell/RefCell` - snyk_s log](http://saneyukis.hatenablog.com/entry/2014/05/30/230351)

で、喜び勇んで使ったのですが次なるエラーが。

```rust
struct MyHandler {
    cache: RefCell<HashMap<String, Vec<u8>>>
}

impl Handler for MyHandler {
    fn handle<'a, 'k>(&'a self, Request<'a, 'k>, Response<'a, Fresh>){
        ....
    }
}
```

```
the trait `core::marker::Sync` is not implemented for the type `core::cell::UnsafeCell<...
```

どうも、hyperは複数スレッドでも動かせるのでハンドラにスレッドセーフであることが要求されるようです。そしてRustコンパイラはRefCellがスレッドセーフでない事を知っているのでコンパイルを弾きます。怖いですね。

# Mutex
無理っぽいので最早別の手段を捜し始めます。

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">キャッシュ用のアクター走らせるのが良い気がしてきた</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/609471732970209280">2015, 6月 12</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

これはちょっと無理がありそうですね。

しかし、別の方法があるようでした。[std::sync::Mutex](https://doc.rust-lang.org/std/sync/struct.Mutex.html)です。

```rust
struct MyHandler {
    cache: Mutex<HashMap<String, Vec<u8>>>
}
```

こんな感じで`lock().unwrap()`するだけで使えます。

```rust
let mut cache = self.cache.lock().unwrap();
```

因みにロックの解除は不要です。Rustコンパイラは値の生存期間を知っているので値がこれ以上使われなくなった箇所にコンパイラがunlockを挟みます。(正確に言うとdrop(デストラクタ)が挿入され、dropがリソースの開放を行なう)

# まとめ

* rustのイミュータビリティは継承する
* structのfieldに直接`mut`は指定出来ない
* シングルスレッドでミュータブルなフィールドが欲しいなら`Cell`/`RefCell`
* マルチスレッドなら`Mutex`
