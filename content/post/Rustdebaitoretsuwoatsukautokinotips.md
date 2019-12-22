---
categories: [Rust, Advent Calendar, Advent Calendar 2016, Rust Advent Calendar]
date: 2016-12-01T14:29:33+09:00
title: Rustでバイト列を扱う時のtips
---

このエントリは[Rust その2 Advent Calendar 2016 - Qiita](http://qiita.com/advent-calendar/2016/rust-lang-2)2日目の記事です。

Rustはシステムプログラミング言語なのでバイト列をあれこれしたいことがあると思います。その時にイテレータでバイト列を舐める以外にも色々方法があるなと気付いたので。

<!--more-->

# `Read` と `Write`
私には割と衝撃だったのですが`&[u8]`や`&mut [u8]`、`Vec<u8>`は直接`Read`や`Write`のインスタンスになってます。
例えば`Read`ならこういう風に使えます。

``` rust
let mut bytes: &[u8] = &[1, 2, 3, 4, 5, 6];
let mut buf = [0;3];
bytes.read_exact(&mut buf).unwrap();
println!("read: {:?}, rest: {:?}", buf, bytes);
```

```
read: [1, 2, 3], rest: [4, 5, 6]
```

あるいは `.bytes()`でbyteのイテレータを取り出してもいいですし、如何様にも扱えます。

少し注意が必要なのは`Vec`は `Read` を実装してないので一旦スライスに変換してあげる必要がありますが、やり方を工夫する必要があります。

以下は少しびっくりする例。

``` rust
let bytes: Vec<u8> = vec![1, 2, 3, 4, 5, 6];
let mut buf = [0;3];
(&bytes[..]).read_exact(&mut buf).unwrap();
println!("read: {:?}, rest: {:?}", buf, bytes);
```

```
read: [1, 2, 3], rest: [1, 2, 3, 4, 5, 6]
```

一旦スライスを取り出してその場で捨てているので`bytes`は消費されません。

そうやりたいなら

``` rust
let bytes: Vec<u8> = vec![1, 2, 3, 4, 5, 6];
let mut bytes = &bytes[..];
let mut buf = [0;3];
bytes.read_exact(&mut buf).unwrap();
println!("read: {:?}, rest: {:?}", buf, bytes);
```

のように一旦スライスを束縛してから使います。

Writeの方も似ていて、そのまま`write`出来ますし、消費されます。ただ、書き込み領域が空になってもそのままスルーされるので注意です。

```
let mut bytes: &mut [u8] = &mut [0; 6];
let data = &[1, 2, 3];
println!("data: {:?}, buf: {:?}", data, bytes);
bytes.write(data).unwrap();
println!("data: {:?}, buf: {:?}", data, bytes);
bytes.write(data).unwrap();
println!("data: {:?}, buf: {:?}", data, bytes);
bytes.write(data).unwrap();
println!("data: {:?}, buf: {:?}", data, bytes);
```

```
data: [1, 2, 3], buf: [0, 0, 0, 0, 0, 0]
data: [1, 2, 3], buf: [0, 0, 0]
data: [1, 2, 3], buf: []
data: [1, 2, 3], buf: []
```

最後、バッファが空になった状態で書き込んでも無言で書き込みが終了しています。そして書き込んだデータへのアクセスは出来てないですね。

こうすると出来ます。

``` rust
let mut bytes:[u8; 6] =  [0; 6];
let data = &[1, 2, 3];
{
    let mut buf: &mut [u8] = &mut bytes;
    println!("data: {:?}, buf: {:?}", data, buf);
    buf.write(data).unwrap();
    println!("data: {:?}, buf: {:?}", data, buf);
    buf.write(data).unwrap();
    println!("data: {:?}, buf: {:?}", data, buf);
}
println!("bytes: {:?}", bytes);
```

``` rust
data: [1, 2, 3], buf: [0, 0, 0, 0, 0, 0]
data: [1, 2, 3], buf: [0, 0, 0]
data: [1, 2, 3], buf: []
bytes: [1, 2, 3, 1, 2, 3]
```

まあまあ面倒ですね。というかそもそも固定長のバッファに書き込みたいという需要が少ない。

でもこれは`Vec`を使えば解決します。可変長ですし`Write` も実装しているので便利です。

``` rust
let mut bytes: Vec<u8> = Vec::new();
let data = &[1, 2, 3];
println!("data: {:?}, buf: {:?}", data, bytes);
bytes.write(data).unwrap();
println!("data: {:?}, buf: {:?}", data, bytes);
bytes.write(data).unwrap();
println!("data: {:?}, buf: {:?}", data, bytes);
println!("bytes: {:?}", bytes);
```

``` rust
data: [1, 2, 3], buf: []
data: [1, 2, 3], buf: [1, 2, 3]
data: [1, 2, 3], buf: [1, 2, 3, 1, 2, 3]
bytes: [1, 2, 3, 1, 2, 3]
```

# `Cursor`
さて、上で見た通り、なんとなく生のバイト列だと扱いづらそうな場面がありそうですよね。
そこで [`std::io::Cursor`](https://doc.rust-lang.org/stable/std/io/struct.Cursor.html)を使うと便利です。

`Cursor`はコンストラクタで引数の所有権を奪うタイプの、ラッパーオブジェクト的構造体です。

readだとこんな感じです。

``` rust
let bytes: &[u8] = &[1,2,3,4,5,6];
let data: &mut [u8] = &mut [0;3];
let mut cur = Cursor::new(bytes);
println!("data: {:?}, position {}", data, cur.position());
cur.read_exact(data).unwrap();
println!("data: {:?}, position {}", data, cur.position());
cur.read_exact(data).unwrap();
println!("data: {:?}, position {}", data, cur.position());
println!("bytes: {:?}", cur.into_inner());
```

``` rust
data: [0, 0, 0], position 0
data: [1, 2, 3], position 3
data: [4, 5, 6], position 6
bytes: [1, 2, 3, 4, 5, 6]
```

ポジションが取れるのと元のオブジェクトが無事なのが違いますね。

Writeも同様です。

``` rust
let bytes: &mut [u8] = &mut [0;6];
let data: &[u8] = &[1, 2, 3];
let mut cur = Cursor::new(bytes);
println!("bytes: {:?}, position {}", cur.get_ref(), cur.position());
cur.write(data).unwrap();
println!("bytes: {:?}, position {}", cur.get_ref(), cur.position());
cur.write(data).unwrap();
println!("bytes: {:?}, position {}", cur.get_ref(), cur.position());
println!("bytes: {:?}", cur.into_inner());
```

```
data: [0, 0, 0, 0, 0, 0], position 0
data: [1, 2, 3, 0, 0, 0], position 3
data: [1, 2, 3, 1, 2, 3], position 6
bytes: [1, 2, 3, 1, 2, 3]
```

さて、この`Cursor`の面白いのは`&[u8]`でなく`AsRef<[u8]>`で`Read`を実装していますし`std::io::Seek`も実装しているのでこういうことが出来ます。


``` rust
let bytes: Vec<u8> = Vec::new();
let data: &[u8] = &[1, 2, 3];
let buf: &mut [u8] = &mut [0; 3];
let mut cur = Cursor::new(bytes);
println!("bytes: {:?}, position {}", cur.get_ref(), cur.position());
cur.write(data).unwrap();
println!("bytes: {:?}, position {}", cur.get_ref(), cur.position());
cur.write(data).unwrap();
println!("bytes: {:?}, position {}", cur.get_ref(), cur.position());
cur.seek(SeekFrom::Start(0)).unwrap();
println!("bytes: {:?}, position {}", cur.get_ref(), cur.position());
cur.read(buf).unwrap();
println!("bytes: {:?}, position {}, buf: {:?}", cur.get_ref(), cur.position(), buf);
```


```
bytes: [], position 0
bytes: [1, 2, 3], position 3
bytes: [1, 2, 3, 1, 2, 3], position 6
bytes: [1, 2, 3, 1, 2, 3], position 0  // <- 0にシークした
bytes: [1, 2, 3, 1, 2, 3], position 3, buf: [1, 2, 3] // <- 0からリード出来てる
```

生の`Vec`では出来なかったread and writeが実現出来ています。そして好きにカーソルをシーク出来ます。
ここまでくるとほとんどファイルと変わらなく扱えますね。



# おりに
ちょっとしたTipsですが道具箱にこういうのを増やしておくと便利ですよね！！


# 参考

* [std::io::Read - Rust](https://doc.rust-lang.org/stable/std/io/trait.Read.html)
* [std::io::Write - Rust](https://doc.rust-lang.org/stable/std/io/trait.Write.html)
* [std::io::Cursor - Rust](https://doc.rust-lang.org/stable/std/io/struct.Cursor.html)
* [std::io::Seek - Rust](https://doc.rust-lang.org/stable/std/io/trait.Seek.html)
* [std::io::SeekFrom - Rust](https://doc.rust-lang.org/stable/std/io/enum.SeekFrom.html)
