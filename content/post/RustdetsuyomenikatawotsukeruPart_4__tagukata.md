---
categories: ["Rust", "Rustで強めに型をつける"]
date: 2020-01-11T17:05:51+09:00
title: "Rustで強めに型をつけるPart 4: タグ型"
---

κeenです。久しぶりに「強めに型をつける」シリーズです。
マーカとして使われる型について。呼び方が分からなかったので「タグ型」と呼ぶことにします。

<!--more-->

# アドホックポリモーフィズム

同じ名前の関数で、（静的に分かる）型に応じて振る舞いを変えるポリモーフィズムのことをアドホックポリモーフィズムと言います。代表的なものはオーバーロードでしょう。

Rustでも「オーバーロード」という名前では呼ばれてませんがアドホックポリモーフィズムがあります。
例えば `str::parse` メソッドは渡された型パラメータに応じてパースする処理を切り替えます。

```rust
// 整数値としてパースする
let integer = "127".parse::<i32>().unwrap();

use std::net::IpAddr;

// IPアドレスとしてパースする
let addr = "127.0.0.1".parse::<IpAddr>().unwrap();
```

同じ `str::parse` メソッドですが返り値の型が `i32` であるか `IpAddr` であるかで振る舞いを変えています。

この例は型パラメータと返り値の関係が直接的ですね。型パラメータに `IpAddr` を指定したら `IpAddr` 用のパース処理が走って、 `IpAddr` 型が返っています。
しかし返り値（や引数）の型によっては期待する振る舞いが複数あるなど、単純な対応関係にない場合もあります。

例えばバイト列からu32型を取り出すときに[ビッグエンディアン](https://ja.wikipedia.org/wiki/%E3%82%A8%E3%83%B3%E3%83%87%E3%82%A3%E3%82%A2%E3%83%B3#%E3%83%93%E3%83%83%E3%82%B0%E3%82%A8%E3%83%B3%E3%83%87%E3%82%A3%E3%82%A2%E3%83%B3)か[リトルエンディアン](https://ja.wikipedia.org/wiki/%E3%82%A8%E3%83%B3%E3%83%87%E3%82%A3%E3%82%A2%E3%83%B3#%E3%83%AA%E3%83%88%E3%83%AB%E3%82%A8%E3%83%B3%E3%83%87%E3%82%A3%E3%82%A2%E3%83%B3)かを選びたいことがあるでしょう。

```rust
let mut data: &[u8] = &[1, 0, 0, 0];
// リトルエンディアンで読み出したい
let uint: u32 = data.read_u32(); // <- ???
assert_eq!(uint, 1);

let mut data: &[u8] = &[0, 0, 0, 1];
// ビッグエンディアンで読み出したい
let uint: u32 = data.read_u32(); // <- ???
assert_eq!(uint, 1);
```

このコードのままではリトルエンディアンかビッグエンディアンかが使い分けられてなさそうです。
`read_u32_be` 、 `read_u32_le` などのように名前を使い分ける手もありますが、読み出す型 x エンディアンで増えていくので作る方も使う方も面倒ですね。
そういった時にタグ型を使うと複数の振る舞いを使い分けられます。
具体的には以下のように書けるようになります。


```rust
let mut data: &[u8] = &[1, 0, 0, 0];
// リトルエンディアンで読み出す
let uint: u32 = data.read_u32::<LittleEndian>(); // <- !!!
assert_eq!(uint, 1);

let mut data: &[u8] = &[0, 0, 0, 1];
// ビッグエンディアンで読み出す
let uint: u32 = data.read_u32::<BigEndian>(); // <- !!!
assert_eq!(uint, 1);
```


# タグ型

先述の例を見てもらったら分かるとおり、 `LittleEndian` と `BigEndian` という型を使っていますが、値には現われません。タグとしてしか使っていないので作る必要がないからです。

そういうときに、 `LittleEndian` と `BigEndian` に以下のような定義を与えると便利です。


```rust
enum LittleEndian {}
enum BigEndian {}
```


`struct LittleEndian;` ではなく `enum LittleEndian {}` です。
`enum` の値は列挙子をコンストラクタとして作られますが、コンストラクタが1つもないので値を作ることができません。これで値の世界には住んでなくて、型の世界の住人であることが明示できます。

あとはこれらの `LittleEndian` と `BigEndian` に「所望のエンディアンで読み出す」機能をつけてあげるだけです。これはトレイトで実現できます。

```rust
// エンディアンを表わすトレイト
trait ByteOrder {
    fn read_u32(buf: &[u8]) -> u32;
    // ...
}

// それぞれのエンディアンに実装していく

impl ByteOrder for LittleEndian {
    fn read_u32(buf: &[u8]) -> u32 {
        // ...
    }
    //...
}

impl ByteOrder for BigEndian {
    fn read_u32(buf: &[u8]) -> u32 {
        // ...
    }
    //...
}
```

最後に `data.read_u32::LitleEndian()` と呼ぶために以下のような便利トレイトを定義します。

```rust
use std::io;

trait ReadBytesExt: Read {
    fn read_u32<BE: ByteOrder>(&mut self) -> u32 {
        let mut buf = [0; 4];
        self.read_exact(&mut buf).unwrap();
        Ok(BE::read_u32(&buf))
    }
}

// Tip: `&[u8]` は `Read` を実装している
impl ReadBytesExt for Read {}
```

こうすれば例のように `data.read_u32::<LittleEndian>()` と呼び出すことができます。

```rust
let mut data: &[u8] = &[1, 0, 0, 0];
// リトルエンディアンで読み出す
let uint: u32 = data.read_u32::<LittleEndian>(); // <- !!!
assert_eq!(uint, 1);

let mut data: &[u8] = &[0, 0, 0, 1];
// ビッグエンディアンで読み出す
let uint: u32 = data.read_u32::<BigEndian>(); // <- !!!
assert_eq!(uint, 1);
```

めでたしめでたし。

今回の例は [byteorder](https://docs.rs/byteorder/1.3.2/byteorder/) クレートから採りました。

# 別の解釈

軽めのStrategy Patternとも解釈できます。
FullのStrategy Patternと違って値がなくて状態を持てませんが、変わりにストラテジを作るときにアレコレ考える必要がなくて名前をポンッと置けば機能します。
