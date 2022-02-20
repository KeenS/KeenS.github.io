---
categories: [Rust, 小ネタ]
date: 2022-02-21T02:17:08+09:00
title: "Rustでポインタから`Box<[T]>`を作る"
---

κeenです。表題のとおりのことをやりたかったのですが、ピンポイントのAPIが見当らなかったのでやり方を書き留めます。

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
