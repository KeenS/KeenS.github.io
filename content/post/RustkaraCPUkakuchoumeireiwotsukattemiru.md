---
categories: [小ネタ, Rust, CPU, x86]
date: 2021-04-04T17:19:58+09:00
title: "RustからCPU拡張命令を使ってみる"
---

κeenです。ちょっと気紛れでCPUの拡張命令を使ってみようかなと思ったのでRustから叩いてみます。

<!--more-->

Rustから叩くとはいってもあんまり難しいものではなくて、intrinsicsとして実装されているのでそれを呼ぶだけです。今回は[BMI2](https://en.wikipedia.org/wiki/Bit_manipulation_instruction_set)拡張のPDEP/PEXTを呼んでみます。

# PDEP/PEXT

BMI（Bit Manipulation Instruction Set）はその名のとおりビット操作を提供するx86の拡張命令です。IntelのHaswellから導入されてAMDのCPUもサポートしているようです。ABM、 BMI1、 BMI2と関連する拡張が色々あってややこしいので詳しくはWikipediaの記事を参考にして下さい。今回はBMI2を使います。その中のPDEP/PEXTはビットフラグ関連の操作を提供します。PEXTは「ビットマスクでマスクしてシフトでLSBに移動」、PDEPは逆で「マスクの下位の0の分だけMSB側に移動」という操作をします。

CTZ = count trailing zerosとして、以下の等式が成り立つのかな？


```
PEXT(data, mask) = (data & mask) >> CTZ(mask)
PDEP(data, mask) = data << CTZ(mask)
```

追記
Twitterで指摘がきました。

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">RustでCPU拡張命令が使えるという本題には関係ないけど、PDEPとPEXTはWikipedia見た感じだとビットマスクとシフトだけの等式で表せるほど単純な命令ではないように見えた。 <a href="https://t.co/TmxDfhSmyX">https://t.co/TmxDfhSmyX</a></p>&mdash; megumish (@megumish_unsafe) <a href="https://twitter.com/megumish_unsafe/status/1378647824980701185?ref_src=twsrc%5Etfw">April 4, 2021</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

bitmapが連続していない場合はただのmaskしてshiftでは表わせない挙動をするので私の書いた式は誤りでした。詳しくは末尾の追補を参照下さい。

/追記

うん、便利そう。ということでPDEPとPEXTを使ってみます。

# Rustから使うPDEP/PEXT

まあ、普通にintrinsicsとして実装されています。具体的には[`core::arch::x86_64`](https://doc.rust-lang.org/core/arch/x86_64/index.html)に[_pdep_u32](https://doc.rust-lang.org/core/arch/x86_64/fn._pdep_u32.html)と[_pext_u32](https://doc.rust-lang.org/core/arch/x86_64/fn._pext_u32.html)、あとそれぞれの `_u64` 版が実装されています。[`core::arch::x86`](https://doc.rust-lang.org/core/arch/x86/index.html)にも `_u32` はありますが付け加えるまでもないですね。

この2つの関数を使ってみましょう。データのLSBから数えて5bit目から8bit目までを取り出したいとします。

```
0b11110111_00110001
           ^^^^
        この4bitを取り出したい
```

すると以下のようなコードを書くことになります。

```rust
use std::arch::x86_64::_pext_u64;

fn main() {
    let data = 0b11110111_00110001;
    let mask = 0b00000000_11110000;
    unsafe {
        println!("0b{:016b}", _pext_u64(data, mask));
    }
}
```

BMI2をサポートしているCPU上で実行すると結果は期待とおり `0b0000000000000011` になります。

逆に `PDEP` だとデータをマスクにフィットするようにします。

```
0b11110111_00110001
               ^^^^
       この4bitをマスクの位置に移動したい
```

先のコードの `pext` を `pdep` に書き換えてみましょう。

```rust
use std::arch::x86_64::_pdep_u64;

fn main() {
    let data = 0b11110111_00110001;
    let mask = 0b00000000_11110000;
    unsafe {
        println!("0b{:016b}", _pdep_u64(data, mask));
    }
}
```

BMI2をサポートしているCPU上で実行すると結果は期待どおり `0b0000000000010000` になります。

因みにRustのPlaygroundを動かしているCPUはBMI2をサポートしているようなので上記コードはPlayground上で動きます。

# まとめ

RustからCPUの拡張命令を使うにはIntrinsicsを使えばよいです。今回はサボりましたがCPUが特定の拡張命令をサポートしているかを確認するには [cpuid](https://docs.rs/cpuid/0.1.1/cpuid/)クレートで実行時に判断するか `#[cfg(target_feature = "xxx")]` でコンパイル時に判断することになります。PEXT/PDEPはビットフラグやビットマップを操作するときに使えて便利ですね。私の用途だったらビットマップGCを実装するときにビットマップ操作で使うかなーとぼんやり考えています。今回は純粋な命令でしたが[MPK](https://en.wikipedia.org/wiki/Memory_protection#Protection_keys)みたいに面白いことのできる命令もいつか試してみたいですね（まだRustのサポートはない）。


# 追補

PEXT/PDEPの挙動の説明が間違っていたので訂正します。私の説明はmask bitが連続している場合は正しかったのですが、離れていた場合には異なる挙動をします。例えば以下のようにmask bitが離れていた場合、


```
0b11110111_00110001
     ^   ^    ^   ^
        この4bitを取り出したい
```

取り出されるのはこれらを一箇所に集めた `0b00000000_00001111` です。実際、以下のコードを走らせるとそのような結果が得られます

``` rust
use std::arch::x86_64::_pext_u64;

fn main() {
    let data = 0b11110111_00110001;
    let mask = 0b00010001_00010001;
    unsafe {
        println!("0b{:016b}", _pext_u64(data, mask));
    }
}
```


逆に PDEPは `0b00000000_00001111` に対して `0b00010001_00010001` のマスクをあてがうと、 下位4bitをそれぞれマスクのビットが立っている位置までもってきて `0b00010001_00010001` を返します。これも以下のコードを書けば確認できます。

``` rust
use std::arch::x86_64::_pdep_u64;

fn main() {
    let data = 0b00000000_00001111;
    let mask = 0b00010001_00010001;
    unsafe {
        println!("0b{:016b}", _pdep_u64(data, mask));
    }
}
```

色々用途は考えられますが、GCを実装するときにも便利そうですね。例えば8byteアラインメントされた48bitポインタを64bit整数に納めると下位3bitと上位16bitが余っています。そこにメタデータを突っ込みたい場合なんかに使えそうです。
