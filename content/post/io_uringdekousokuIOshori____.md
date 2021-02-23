---
categories: [Linux, Rust, 非同期]
date: 2021-02-24T04:11:20+09:00
title: "io_uringで高速IO処理（！）"
---

κeenです。[前回の記事](blog/2021/02/08/io_uringdekousokuioshori___/)にもうちょっと実験を加えたのでその結果を書き留めます。

<!--more-->

# 前回の結果

前回の結果を貼っておきます。

ベンチマーク対象は標準ライブラリの `write` (std)、io_uringの `Write` (uring)、io_uringのfdやバッファの事前登録を使った `WriteFixed` (uring')で、それぞれ4KiBのバッファを使って5GiB分書き込むコードでした。その結果が以下です。


| name   | time(ms) |
|--------|---------:|
| std    |     2031 |
| uring	 |     4486 |
| uring' |     1917 |

M.2のNVMe SSD x 4の上にbtrfsでRAID 5を組んだファイルシステム上で実験しており、Linuxカーネルは5.8.0でした。

# 公平性の調整

ネタバレになるですが、キャッシュの具合やファイルが残ってるかどうかで速度が結構変わるのでベンチマーク後にファイルは削除し、ベンチマークの間に `sync()` を挟むことでキャッシュの影響をなくすようにしました。

# Linuxのアップデート

本件とは別の事情で最新版のLinuxを使う用事が発生したのでLinux 5.11.0を使っています。リリースノートを見るとbtrfsの高速化なども含んでいるので一応ベンチマークを取り直してみます。

| name   | time(ms) |
|--------|---------:|
| std    |     1779 |
| uring	 |     5088 |
| uring' |     2097 |

stdは速くなった一方uringはむしろ遅くなりましたね…。まあ、それぞれ1回しか測ってないのであんまり比べてもしょうがなさそうですが。

# パラメータチューニング

前回の記事を書いた直後くらいに元ScyllaDBの中の人が書いたブログをみつけました。

[Modern storage is plenty fast. It is the APIs that are bad. | by Glauber Costa | ITNEXT](https://itnext.io/modern-storage-is-plenty-fast-it-is-the-apis-that-are-bad-6a68319fbc1a)

バッファサイズなどをいじると速くなるよとのことです。ページサイズである4KiBが最適とばかり思っていたので意外でした。バッファサイズを512KiBにし、io_uringのsubmitするバッチサイズを1024から64に減らしたのが以下のベンチマークです。

| name   | time(ms) |
|--------|---------:|
| std    |     1472 |
| uring	 |     1668 |
| uring' |     1527 |

stdの速度向上が目覚しいですね…。


# Sync

上記のベンチマークはシンプルに `write` してるだけです。OSがファイルへの書き込みをキャッシュするので実はストレージには大した量の書き込みが走っていません。普通はソフトウェア的にはそれでいいんですが、今回の目的の1つにNVMe SSDの特性を知るというのがあるので勿体無いですね。ストレージへの書き込みまで含めて実験してみましょう。

ストレージにまで書き込むには、ざっくりファイルを開くときに `O_DIRECT | O_SYNC` をつける方法と、書き込みが終わったら `fsync` を呼ぶ方法があるようです。io_uringにも `fsync` に対応する命令はあるので両方ともベンチマークできそうです。試してみましょう。

`O_DIRECT | O_SYNC` をつけるのは標準ライブラリを使ったコードでもio_uringを使ったコードでも共通で、以下のようにやります。


```rust
let mut opt = OpenOptions::new();
opt.custom_flags(libc::O_DIRECT | libc::O_SYNC);

// オプションからファイルを作る
opt.write(true).create(true).open(path)
```


本当はファイルを開くのもio_uringでやることもできるんですが、どの途ファイルが開かれたのを待たないとIOを開始できないので普通にブロッキングなシステムコールを使ってます。


`fsync` は標準ライブラリとio_uringで異なります。標準ライブラリなら `sync_all` です。

```rust
file.sync_all()?;
```

io_uringなら `Fsync` オペコードを使います。

```rust
unsafe {
    let entry = Fsync::new(Fd(file.as_raw_fd())).build();
    sq.available()
        .push(entry)
        .map_err(|_| io::Error::new(io::ErrorKind::Other, "failed to push entry to sq"))?;
}
submitter.submit()?;
```

それぞれでベンチマークを走らせてみましょう。


`O_DIRECT | O_SYNC`

| name   | time(ms) |
|--------|---------:|
| std    |    84190 |
| uring	 |     7290 |
| uring' |     3196 |


`fsync`


| name   | time(ms) |
|--------|---------:|
| std    |     3281 |
| uring	 |     3430 |
| uring' |     2781 |


全体的に uring' の優秀さが目立ちますね。そして `O_DIRECT | O_SYNC` は特にstdで圧倒的に遅いです。ある意味で私が見たかったのはこのベンチマークだった気がします。 `fsync` についてはどれも最終的に一気に書き出すコードになっているのでそこまで差はつかないようです。


# Fallocate

先に書いた通り、前のベンチマークで作ったファイルが残ってるかどうかが速度に影響しました。恐らく書き足していくとストレージ上のファイルを大きくする処理が入って遅くなるのでしょう。今回は書く量が5GiBと先に決まっているのであらかじめファイルサイズを適切に変更してから書き出してみましょう。

ファイルサイズをあらかじめ確保するのは `fallocate`、 `posix_fallocate` 、 `truncate` などのシステムコールがあるようです。今回みたいにストレージ上の領域の確保を目的とするなら `fallocate` / `posix_fallocate` の方が向いているらしいですが、まあ `truncate` でも大丈夫でしょう。

標準ライブラリなら `File::set_len` が使えます。

```rust
file.set_len(TOTAL as u64)?;
```

これはソースを読むとLinuxでは `truncate` を呼んでいるようでした。適切とされる `fallocate` ではありませんが細けぇことはいいんだよ！

io_uringなら `Fallocate` オペコードが使えます。

```rust
let entry = Fallocate::new(Fd(file.as_raw_fd()), TOTAL as i64).build();
sq.available()
    .push(entry)
    .map_err(|_| io::Error::new(io::ErrorKind::Other, "failed to push entry to sq"))?;
submitter.submit()?;
```


特に確認はしてないですが名前からして `fallocate` 相当のオペコードだと信じていいんじゃないでしょうか。

ということで工夫なし、`fsync` 、 `O_DIRECT | O_SYNC` のそれぞれについて `fallocate` してから書き込みを開始したもののベンチマークもとってみます。


`fallocate`

| name   | time(ms) |
|--------|---------:|
| std    |     1376 |
| uring	 |     1585 |
| uring' |     1426 |



`fallocate` + `O_DIRECT | O_SYNC`

| name   | time(ms) |
|--------|---------:|
| std    |    80043 |
| uring	 |    12501 |
| uring' |     2520 |


`fallocate` + `fsync`


| name   | time(ms) |
|--------|---------:|
| std    |     3337 |
| uring	 |     3425 |
| uring' |     2835 |

素の `write` と `fsync` ではあまり効果がなさそうですが `O_DIRECT | O_SYNC` と一緒なら `fallocate` しておいた方がかなり速いですね。特に、 uring' を `fallocate` + `O_DIRECT | O_SYNC` で走らせたときがストレージへの書き込みまで含んだ処理の中で最速になります。 uring が何故か遅くなってるのはよく分かりません。

# まとめ

前回雑にベンチマークを取った結果を見直し、OSのキャッシュやストレージ上のファイル領域の確保まで踏み込んだベンチマークをとりました。ただのwriteだと依然シンプルに `write` を走らせる方が速かったものの、ストレージ上の領域への書き込み完了まで含めた処理だとio_uringを工夫して使った方が速いという結果になりました。

uringでfallocateした方が遅くなるなど多少不可解な挙動はありましたがひとまず私は満足しました。今回のコードは前回同様こちらに置いておきます。

[KeenS/io-uring-write-file](https://github.com/KeenS/io-uring-write-file)
