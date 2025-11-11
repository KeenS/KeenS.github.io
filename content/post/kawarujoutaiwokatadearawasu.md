---
categories: [Rust]
date: 2025-11-11T23:53:00+09:00
title: "変わる状態を型で表わす"
---
κeenです。
プログラムで扱うものに状態、操作によって変わるものってありますよね。それを扱うのは普通なら難儀するのですが、Rustだと上手く扱う方法があるよって話です。

<!--more-->

本質的には同じ話を [Rustで強めに型をつけるPart 2: Type Level State Machine | κeenのHappy Hacκing Blog](/blog/2018/12/15/rustdetsuyomenikatawotsukerupart_2__type_level_state_machine/)でもしてるんですが、具体例で趣味に走ってしまったのでもうちょっと分かりやすい例を挙げます。

例に[Vecのsort](https://doc.rust-lang.org/std/vec/struct.Vec.html#method.sort)をとってみましょう。

```rust
// vはsortされていない
let mut v = [4, -5, 1, -3, 2];

v.sort();
// vはsortされている
assert_eq!(v, [-5, -3, 1, 2, 4]);
```

`sort` はメモリ効率の観点からin-placeなメソッド、つまり `self` を受け取って `()` を返す関数です。つまり `sort` を呼んだ前後で `v` がソートされているかいないかが変わります。

これだと一見してどのタイミングで `sort` されているか分かりづらいですよね。この短いコード例だったら見れば分かるんですが、 `v.sort()` の代わりに100行くらいの処理が続いていてその中の関数のどこかで `sort` が呼ばれていたらどうなっているのか分かりません。

そこで、型で今 `v` がどのような状態であるのか表現してみます。

まずは `Vec` をラップする型を用意します。ただし、型引数を1つ増やして細工します。

```rust
struct Array<T, S: sealed::State> {
    _phantom: PhantomData<S>,
    value: Vec<T>,
}
```

`sealed::State` というトレイトを実装した無駄な型引数を受け取っていますね。
[`PhantomData`](https://doc.rust-lang.org/std/marker/struct.PhantomData.html)については知らなければ各々調べて下さい。

`sealed::State` についてはこのような実装が与えられます。細々したテクニックが使われていますが受け入れて下さい。

```rust
mod sealed {
    pub trait State {}
}

enum Unsorted {}
enum Sorted {}

impl sealed::State for Unsorted {}
impl sealed::State for Sorted {}
```

これで、 `Array<T, Sorted>` か `Array<T, Unsorted>` しか受け付けないようになりました。

こうして定義した型には例えば以下のような実装が与えられます。

```rust
impl<T, S: sealed::State> Array<T, S> {
    fn shuffle(self) -> Array<T, Unsorted> {
        unimplemented!()
    }
}

impl<T: Ord> Array<T, Unsorted> {
    fn new() -> Self {
        Self {
            _phantom: Default::default(),
            value: Vec::new(),
        }
    }

    fn add(&mut self, t: T) {
        self.value.push(t);
    }

    fn sort(self) -> Array<T, Sorted> {
        let mut value = self.value;
        value.sort();
        Array {
            _phantom: Default::default(),
            value,
        }
    }
}

impl<T: Ord> Array<T, Sorted> {
    fn add(&mut self, t: T) {
        let i = self.value.iter().rposition(|e| &t < e).unwrap_or(0);
        self.value.insert(i, t);
    }
}
```

ポイントは

* `Array<T, Sorted>` か `Array<T, Unsorted>` かで実装が分かれる
* `sort()` はソートされていない配列にのみ与えられ、呼ぶとソートされている配列になる
* `shuffle()` するとソートされていない配列になる

という実装になっている点です。細かいことを言えば `add` の実装が違いますが、オマケみたいなもんなので気にしなくてよいです。

このように定義した `Array` はソート済みの配列に対して二重に `sort` を呼んでしまうことがありません(呼ぶとエラー)し、ソート済みの配列を期待する関数は `Array<T, Sorted>` を要求すれば必ずソート済みの配列が手に入ります。

```rust
fn main() {
    let mut arr = Array::new();
    arr.add(3);
    arr.add(1);
    // ここでarr.shuffle(); を呼ぶと
    // error[E0599]: no method named `shuffle` found for struct `Array<{integer}, Unsorted>` in the current scope
    //   --> array_sort.rs:65:9
    //    |
    // 13 | struct Array<T, S: sealed::State> {
    //    | --------------------------------- method `shuffle` not found for this struct
    // ...
    // 65 |     arr.shuffle();
    //    |         ^^^^^^^ method not found in `Array<{integer}, Unsorted>`
    //    |
    //    = note: the method was found for
    //            - `Array<T, Sorted>`

    // sorted_arrはArray<i32, Sorted>
    let sorted_arr = arr.sort();
    // unsorted_arrはArray<i32, Unsorted>
    let unsorted_arr = sorted_arr.shuffle();
}
```


よかったですね。

で、これは何故Rustじゃないといけないの？というのが気になった人がいると思います。それは状態遷移が破壊的だからですね。
以下の部分に注目しましょう。

```rust
// sorted_arrはArray<i32, Sorted>
let sorted_arr = arr.sort();
// unsorted_arrはArray<i32, Unsorted>
let unsorted_arr = sorted_arr.shuffle();
```

`shuffle()` を呼んだあと大抵の他言語では破壊的変更された `sorted_arr` がまた使えてしまいます。しかし中身は破壊的変更されているのでソートされていません。これでは困ってしまいますね。Rustなら所有権で `sorted_arr` が無効になるのでそのような問題は起きないのです。

別解として破壊的変更せずに新しい値を作るというのがありますが、それではメモリ効率がよくありません。メモリ効率がよくプログラミングミスも防げるこちらの方がスマートな解放ですよね。


## 実例
このような型レベルでの状態遷移のパターンは実際にRustでよく使われます。今回は `Type<T> → Type<S>` のようにパラメータ化されたものでしたが、より一般化して `T → S`でも同じことですよね？
例えばTLSライブラリの [rustls](https://docs.rs/rustls/latest/rustls/index.html) ではこのようなパターンが使われています。
[`Accepted` の `into_connection`](https://docs.rs/rustls/latest/rustls/server/struct.Accepted.html#method.into_connection)は `Accepted → ServerConnection` の型レベルの状態遷移が行なわれます。

プログラミングで操作するコンピュータが本質的に状態を遷移させながら動作していくものなので上手く状態と付き合っていきましょう。
