---
categories: [Rust, 小ネタ]
date: 2022-02-21T02:17:08+09:00
title: "Rustでポインタから`Box<[T]>`を作る"
---

κeenです。表題のとおりのことをやりたかったのですが、ピンポイントのAPIが見当らなかったのでやり方を書き留めます。

2022-02-22: 末尾に追記しました

<!--more-->

やりたいこととしてはCとのFFIをやっていて要素列へのポインタと要素数があるときに、それをRustの世界に持ち込みたいというもの。

コードはこういう感じですね。

```rust
use std::ffi::c_void;
use std::mem;

extern "C" {
    fn calloc(nmemb: usize, size: usize) -> *mut c_void;
}

fn main() {
    unsafe {
        let nitems = 512;
        let ptr = calloc(nitems, mem::size_of::<i32>());
        // ↑このポインタを `Box<[i32]>` として扱いたい
    }
}
```


一発で変換するメソッドがないので2段に分けてやります。まず[`std::slice::slice_from_raw_parts_mut`](https://doc.rust-lang.org/std/ptr/fn.slice_from_raw_parts_mut.html)で配列のfat pointerを作り、[`std::boxed::Box::from_raw`](https://doc.rust-lang.org/std/boxed/struct.Box.html#method.from_raw)で `Box` に変換します。


```rust
use std::ffi::c_void;
use std::mem;
use std::ptr;

extern "C" {
    fn calloc(nmemb: usize, size: usize) -> *mut c_void;

    fn memset(s: *mut c_void, c: u8, n: usize);
}

fn main() {
    let buffer = unsafe {
        let nitems = 512;
        let ptr = calloc(nitems, mem::size_of::<i32>());
        // ↑このポインタを `Box<[i32]>` として扱いたい

        // ただのサンプルだけど一応メモリは初期化しておく
        memset(ptr, 0, nitems * mem::size_of::<i32>());


        // ポインタと要素数から配列のfatポインタを作る
        let fat_ptr: *mut [i32] = ptr::slice_from_raw_parts_mut(ptr as *mut i32, nitems);
        // fatポインタになればBoxを素直に作れる
        Box::<[i32]>::from_raw(fat_ptr)
    };
    // do something
}
```

小ネタでした。

# 2022-02-22 追記

有識者からそもそも外部のポインタを `Box` にするのは危いのではという指摘を頂きました。

<blockquote class="twitter-tweet" data-conversation="none"><p lang="ja" dir="ltr">* non-null で &quot;fully alligned&quot; であること<br>* global allocator で deallocate できること<br><br>が必要っぽくて、前者についてはまあ i32 の場合は問題ないとして、やっぱり後者がマズそうなんだよな (<a href="https://t.co/vy33f356Mi">https://t.co/vy33f356Mi</a> の最後らへんに書かれているやつ)</p>&mdash; らりお・ザ・.*🈗然㊌㋞㋰㋷㋓ (@lo48576) <a href="https://twitter.com/lo48576/status/1495459648681361408?ref_src=twsrc%5Etfw">February 20, 2022</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

Cで確保したデータはRustの要求する配置になっていない可能性がある点、CでallocしてRustでfreeできるか怪しい点が指摘されています。

**私の書いたサンプルコードと実行環境ではたまたま** これらの問題をクリアしていました。

Linuxは基本メモリ確保は成功してNULLを返すことはありませんし、ポインタも `i32` の要求するアラインメントに適っています（多分glibcだと大丈夫なはず）。

``` rust
assert!(ptr as usize % std::alloc::Layout::new::<i32>().align() == 0);
```

アロケータに関してはLinuxでは現行のRustはlibcのアロケータを使いますし、サンプルコードが全てなので別のところで[カスタムアロケータ](https://doc.rust-lang.org/std/alloc/index.html#the-global_allocator-attribute)に差し替えられたりもしていません。

しかし但し書きが多いことからも分かるように、Windowsだと怪しかったり、古いコンパイラ（std）だとjemalloc使ってたり、ライブラリで書くと別のところでアロケータを差し替えてたりするので未定義動作になりえます。これらはコードの書き方ではなくコンパイルや実行の方法に依存して変わるので気をつけて書いても防げない問題です。

という訳で外部からやってきたポインタを `Box<[T]>` に変換するのはやめておいた方が無難なようです。Cとの連携などで一度 `Box<[T]>` を `*mut T` なんかにしたものを改めて `Box<[T]>` に戻す手段として利用するに留めておいて下さい。
