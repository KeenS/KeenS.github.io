---
categories: [Rust, Cargo]
date: 2018-04-04T06:00:57+09:00
title: "cargo asmでRustのメモリ周り最適化をチェック"
---
κeenです。
Rustはたとえば`Box`を使っていても必ずしもヒープにアロケートされる訳ではないなど、メモリの扱いを多少最適化してるらしいです。
しかし何がどう最適化されるのかは実際にコードを書いてみて実験しないとわからないことが多いので実験してみます。

rust 1.25.0です。
<!--more-->

# cargo asm
実験の前にツールを紹介します。[cargo asm](https://github.com/gnzlbg/cargo-asm)です。
クレートの関数名を指定するとディスアセンブルしてくれます。

たとえば

```rust
pub fn add(x: i32, y: i32) -> i32 {
    x + y
}

```

という関数を`some_crate`に用意すれば以下のようにディスアセンブルできます。

``` console
$ cargo asm some_crate::add
some_crate::add:
 lea     eax, [rdi, +, rsi]
 ret
```

ここでは味気ないですがコンソール上では色がついています。
因みにデフォルトで`release`ビルドのものが使われます。

cargoプロジェクトを作らないといけないのでやや手間ですがgdbやobjdumpよりは格段に使いやすいでしょう。

他にはllvm-irを出力したりJSON形式で出力したりもできるようですがここでは使いません。

# `Box`の実験
## 普通の`Box`
`Box`をアロケートして関数から返して見ましょう。これは最適化の余地が無いので普通にヒープにアロケートすると予想されます。

``` rust
pub fn heap_box() -> Box<i32> {
    Box::new(1)
}
```

これをディスアセンブルすると

``` assembly
 sub     rsp, 56
 lea     rdx, [rsp, +, 8]
 mov     edi, 4
 mov     esi, 4
 ; allocが呼ばれている
 call    __rust_alloc
 ; アロケート失敗したら失敗処理へ
 test    rax, rax
 je      .LBB6_1
 ; 成功したらそのままreturn
 mov     dword, ptr, [rax], 1
 add     rsp, 56
 ret
; 失敗処理。
.LBB6_1:
 movups  xmm0, xmmword, ptr, [rsp, +, 16]
 movaps  xmmword, ptr, [rsp, +, 32], xmm0
 movaps  xmm0, xmmword, ptr, [rsp, +, 32]
 movups  xmmword, ptr, [rsp, +, 16], xmm0
 lea     rdi, [rsp, +, 8]
 call    alloc::heap::exchange_malloc::{{closure}}
 ud2
```

とアロケートしています。まずは当たり前のことが確認できました。

## 別の関数に渡す`Box`
値をエスケープさせる先として別の関数に渡すのも試してみましょう。恐らく仕方なくアロケートするでしょう。

渡す先の関数はこれを用意します。

``` rust
#[inline(never)]
fn take<T>(t: T) {
    let _ = t;
}

```

で、これ。

``` rust
pub fn take_box() {
    let b = Box::new(1);
    take(b)
}
```

ディスアセンブルします。

``` assembly
 sub     rsp, 56
 lea     rdx, [rsp, +, 8]
 mov     edi, 4
 mov     esi, 4
 ; allocして
 call    __rust_alloc
 test    rax, rax
 je      .LBB7_1
 mov     dword, ptr, [rax], 1
 mov     rdi, rax
 ; takeを呼ぶ
 call    memory_check::take
 add     rsp, 56
 ret
; 失敗処理。
.LBB7_1:
 movups  xmm0, xmmword, ptr, [rsp, +, 16]
 movaps  xmmword, ptr, [rsp, +, 32], xmm0
 movaps  xmm0, xmmword, ptr, [rsp, +, 32]
 movups  xmmword, ptr, [rsp, +, 16], xmm0
 lea     rdi, [rsp, +, 8]
 call    alloc::heap::exchange_malloc::{{closure}}
 ud2

```

やはりアロケートしてますね。

## 関数内で閉じた`Box`
先程は関数の外に返していましたが今度は内部で消費してみます。これは最適化できそうです。

``` rust
pub fn stack_box() -> i32 {
    let b = Box::new(1);
    let b = *b + 1;
    b
}
```

これをディスアセンブルすると

``` assembly
 mov     eax, 2
 ret
```

アロケートどころか全部消し飛んでますね。

アグレッシブー。

## 別の関数に渡す`Box` - inline化あり

2つ前のやつ、`take`の`#[inline(never)]`をはずすとどうなるかというと


``` assembly
 ret
```

アグレッシブー。

## 別の関数に`&mut`で渡す`Box`
では、中途半端に`&mut`で渡してみて関数内で消費してみましょう。これは最適化でアロケートが消えてスタック上の値の参照を渡すようになるんですかねー。


``` rust
pub fn mut_stack_box() -> i32 {
    let mut b = Box::new(1);
    take(&mut b);
    *b + 1
}
```

ディスアセンブルすると

``` assembly
 mov     eax, 2
 ret
```

？！？！？！`take` が何もしないことを見抜いている！？

恐らくこれはpurity解析をしていて、`take`が純粋なので`optimize out`してもよいと判断できるのでしょう。

適当に副作用を起こす`take_print`を用意して

``` rust
#[inline(never)]
fn take_print<T>(t: T) {
    let _ = t;
    println!("hello");
}
```

それを使うコードにすると

``` rust
pub fn mut_stack_box_print() -> i32 {
    let mut b = Box::new(1);
    take_print(&mut b);
    *b + 1
}
```

こうなります。

``` assembly
 push    rbp
 push    rbx
 sub     rsp, 56
 lea     rdx, [rsp, +, 8]
 mov     edi, 4
 mov     esi, 4
 ; allocして
 call    __rust_alloc
 mov     rbx, rax
 test    rbx, rbx
 je      .LBB11_4
 mov     dword, ptr, [rbx], 1
 call    memory_check::take_print
 mov     ebp, dword, ptr, [rbx]
 add     ebp, 1
 mov     esi, 4
 mov     edx, 4
 mov     rdi, rbx
 ; dealloc
 call    __rust_dealloc
 mov     eax, ebp
 add     rsp, 56
 pop     rbx
 pop     rbp
 ret
; 失敗処理。
.LBB11_4:
 movups  xmm0, xmmword, ptr, [rsp, +, 16]
 movaps  xmmword, ptr, [rsp, +, 32], xmm0
 movaps  xmm0, xmmword, ptr, [rsp, +, 32]
 movups  xmmword, ptr, [rsp, +, 16], xmm0
 lea     rdi, [rsp, +, 8]
 call    alloc::heap::exchange_malloc::{{closure}}
 ud2
.LBB11_3:
 mov     rbp, rax
 mov     rdi, rbx
 call    core::ptr::drop_in_place
 mov     rdi, rbp
 call    _Unwind_Resume
 ud2
```

ふむふむ。スタックは使わずにヒープにアロケートしてすぐにデアロケートするんですね。

しかしちょっと気になる点が。これ、`&mut Box<i32>`をとってませんかね。

型を明示してみます。

``` rust
pub fn i32_mut_stack_box_print() -> i32 {
    let mut b = Box::new(1);
    take_print::<&mut i32>(&mut b);
    *b + 1
}
```

これでどうですか

``` assembly
memory_check::i32_mut_stack_box_print:
 push    rbp
 push    rbx
 sub     rsp, 56
 lea     rdx, [rsp, +, 8]
 mov     edi, 4
 mov     esi, 4
 ; allocして
 call    __rust_alloc
 mov     rbx, rax
 test    rbx, rbx
 je      .LBB13_4
 mov     dword, ptr, [rbx], 1
 call    memory_check::take_print
 mov     ebp, dword, ptr, [rbx]
 add     ebp, 1
 mov     esi, 4
 mov     edx, 4
 mov     rdi, rbx
 ; dealloc
 call    __rust_dealloc
 mov     eax, ebp
 add     rsp, 56
 pop     rbx
 pop     rbp
 ret
; 失敗処理。
.LBB13_4:
 movups  xmm0, xmmword, ptr, [rsp, +, 16]
 movaps  xmmword, ptr, [rsp, +, 32], xmm0
 movaps  xmm0, xmmword, ptr, [rsp, +, 32]
 movups  xmmword, ptr, [rsp, +, 16], xmm0
 lea     rdi, [rsp, +, 8]
 call    alloc::heap::exchange_malloc::{{closure}}
 ud2
.LBB13_3:
 mov     rbp, rax
 mov     rdi, rbx
 call    core::ptr::drop_in_place
 mov     rdi, rbp
 call    _Unwind_Resume
 ud2
```

だめですか。

# 構造体の実験
今度は`Box`ではなくて構造体で実験します。

用意するのはこれ。40byteの構造体。

``` assembly
#[derive(Default)]
pub struct Struct {
    a: i64,
    b: i64,
    c: i64,
    d: i64,
    e: i64,
}
```

## 構造体の値返し
まずは`Box`と同じくそのまま関数から返してみます。

``` rust
pub fn stack_struct() -> Struct {
    Struct::default()
}
```

これをディスアセンブルするとこうなります。

``` assembly
 ; 128bit(=16byte)レジスタを0初期化
 xorps   xmm0, xmm0
 ; メモリに16byte書き込む。書き込み先は引数で与えられたポインタ
 movups  xmmword, ptr, [rdi, +, 16], xmm0
 ; メモリに16byte書き込む
 movups  xmmword, ptr, [rdi], xmm0
 ; メモリに8byte書き込む。
 ; SIMD命令は16byteアラインされていないといけないので端数は`mov`を使う
 mov     qword, ptr, [rdi, +, 32], 0
 mov     rax, rdi
 ret
```

へー。SIMD使って初期化するんですね。
それはともかく外部からポインタが渡されてますね。

この`stack_struct`を`#[inline(never)]`して別の関数で受け取ってみましょう。

``` rust
pub fn receive_struct()  {
    let _ = stack_struct();
}
```

これをディスアセンブルすると

``` assembly
 ; スタックを40byte広げて
 sub     rsp, 40
 ; その領域へのポインタを`stack_struct`に渡す
 mov     rdi, rsp
 call    memory_check::stack_struct
 add     rsp, 40
 ret

```

となっています。ふむふむ、スタック返しになっているんですね。

## 構造体の値返し大小
40byteではスタック返しでした。では、もっと小さかったり大きかったりするとどうなんでしょう。

8byteの場合: レジスタ返しのようです

``` rust
#[derive(Default)]
pub struct SmallStruct {
    a: i64,
    b: i64,
}
```

``` assembly
 xor     eax, eax
 xor     edx, edx
 ret
```

8192byteの場合: スタック返しのようです。これは`memset`を使うんですね。

``` rust
#[derive(Default)]
pub struct BigStruct([[i64; 32]; 32]);
```

``` assembly
 push    rbx
 mov     rbx, rdi
 xor     esi, esi
 mov     edx, 8192
 call    memset
 mov     rax, rbx
 pop     rbx
 ret
```

因みに8192byteの場合は受取側はスタックが溢れないかチェックするようです。

``` assembly
 mov     eax, 262152
 ; なんか呼ばれてる
 call    __rust_probestack
 sub     rsp, rax
 lea     rdi, [rsp, +, 8]
 call    memory_check::stack_big_struct
 add     rsp, 262152

```

この`__rust_probestack`、[ドキュメント](https://manishearth.github.io/rust-internals-docs/compiler_builtins/probestack/index.html)によると、普段stack overflow検出にはガードページか使われていますがあまりにstackを伸ばす幅が大きいとガードページを飛び越えて伸ばしてしまう可能性があるため手動で検査する必要があるんだそうです。へー。因みに予想どおり確保サイズが4096byte以上になったらprobestackされるようです。

## 構造体を`Box`で受け取る

運が良ければ`Box`で確保した領域に直接書き込めるでしょう。運が悪ければ一旦スタックで受け、そこから `Box` に書き込むでしょう。

``` rust

pub fn recieve_struct_in_box() -> Box<Struct> {
    let b = Box::new(stack_struct());
    b
}
```

因みに`stack_struct`には`#[inline(never)]`がついてます。

ディスアセンブルしてみましょう

``` assembly
 ; スタックを伸ばして
 sub     rsp, 88
 lea     rdi, [rsp, +, 48]
 ; stack_structを呼ぶ
 call    memory_check::stack_struct
 lea     rdx, [rsp, +, 8]
 mov     edi, 40
 mov     esi, 8
 ; メモリを確保して
 call    __rust_alloc
 test    rax, rax
 je      .LBB20_1
 mov     rcx, qword, ptr, [rsp, +, 80]
 mov     qword, ptr, [rax, +, 32], rcx
 movups  xmm0, xmmword, ptr, [rsp, +, 48]
 movups  xmm1, xmmword, ptr, [rsp, +, 64]
 ; 確保した領域に書き込み
 movups  xmmword, ptr, [rax, +, 16], xmm1
 movups  xmmword, ptr, [rax], xmm0
 add     rsp, 88
 ret
; 失敗処理。
.LBB20_1:
 movups  xmm0, xmmword, ptr, [rsp, +, 16]
 movaps  xmmword, ptr, [rsp, +, 32], xmm0
 movaps  xmm0, xmmword, ptr, [rsp, +, 32]
 movups  xmmword, ptr, [rsp, +, 16], xmm0
 lea     rdi, [rsp, +, 8]
 call    alloc::heap::exchange_malloc::{{closure}}
 ud2
```

残念な方でしたね。普通に最適化できないのかメモリ確保の失敗を勘案すると関数呼び出しと順番を入れ替えられないのか気になりますね。

因みにnightlyのrustには[place構文が用意されていて](https://doc.rust-lang.org/std/ops/trait.Place.html)、メモリ確保した領域に直接書き込むことができます。

``` rust
#![feature(box_syntax)]
pub fn receive_struct_in_place_box() -> Box<Struct> {
    let b = box stack_struct();
    b
}

```

ディスアセンブルしてみると、ちゃんと先にメモリを確保しています。

``` assembly
 push    rbx
 sub     rsp, 48
 lea     rdx, [rsp, +, 8]
 mov     edi, 40
 mov     esi, 8
 ; メモリを確保してから
 call    __rust_alloc
 mov     rbx, rax
 test    rbx, rbx
 je      .LBB21_1
 ; そこに書き込ませる
 mov     rdi, rbx
 call    memory_check::stack_struct
 mov     rax, rbx
 add     rsp, 48
 pop     rbx
 ret
; 失敗処理。
.LBB21_1:
 movups  xmm0, xmmword, ptr, [rsp, +, 16]
 movaps  xmmword, ptr, [rsp, +, 32], xmm0
 movaps  xmm0, xmmword, ptr, [rsp, +, 32]
 movups  xmmword, ptr, [rsp, +, 16], xmm0
 lea     rdi, [rsp, +, 8]
 call    alloc::heap::exchange_malloc::{{closure}}
 ud2
```

良いですね。

# まとめ

* `Box`は最適化で消えることはあるけどスタックに変わることはなかったよ
* Rustのスタックアロケートは本当にスタック返しをしてたよ
* `Box::new(Struct)`は一旦スタック経由で書き込んでたよ
* Placeの機能が入ると直接書き込めるようになりそうだね

# こぼれ話
ずっとRustのメモリ周りの最適化が実際どうなっているのか調べたいと思っていました。
しかし毎度ディスアセンブルしてマングリングされた名前をさがすのも手間なのでしばらく放置していました。
ところがcargo-asmが登場したことにより手間が大分削減できるようになったのでこの記事が作成されました。ツールって偉大ですね。
