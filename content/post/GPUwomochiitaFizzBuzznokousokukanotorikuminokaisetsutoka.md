---
categories: [FizzBuzz, OpenCL, GPU]
date: 2022-04-01T00:00:01+09:00
title: "GPUを用いたFizzBuzzの高速化の取り組みの解説とか"
---

κeenです。今年のエイプリルフールのジョークの[GPUを用いたFizzBuzzの高速化の取り組み]()について解説します。

<!--more-->

今年のテーマはGPUで、最近GPUプログラミングを勉強してるのでその成果発表会も兼ねています。メインのジョークとしては特に高速化する意味のないプログラムを、特に向いていないGPUで高速化したと主張するものです。サブのジョークとして筋の悪い論文のように論拠に欠けていたり何をしたかったのか分からなかったりする内容を目指しました。みなさんも読んでいても心の中で突っ込んだ箇所は数知れないと思います。これの元ネタとしてはTwitterでみかけた「ラーメンにアイスクリームをのっけたような論文」があったのですが、今探したら元ネタのツイートがみあたりませんでした。

# コードの速度とか

さて、少し真面目にプログラムを解説しましょう。なんとなくFizzBuzzをGPUで実行しても速くならんだろ、というのは理解できると思います。GPUは演算器が多くて並列な計算が高速ですが、メモリをCPUとやりとりするのは遅いので、行列積のようなメモリのコピーよりも演算コストが大きく、かつそのデータ並列性も高いような処理が得意です。FizzBuzzを省みると入力はただの数値だけ、演算コストは大きくなく、分岐があり、データ並列性も工夫しないと生まれず、出力は大量のメモリ上のデータといった具合にGPUで実行するのはおよそ向いていないプログラムです。さらに言えば出力を `/dev/null` にしてごまかしてますが、標準出力やファイルに書き出すならIOの方が重くなるのでGPUでやろうがCPUでやろうが大差ないはずです。

とはいえ記事中では出力は `/dev/null` ですし多少は並列化の恩恵はあり、記事中で紹介したプログラムはナイーブなFizzBuzzよりは3倍くらい速いようです。


```console
$ time ./fizzbuzz 500000000 > /dev/null
./fizzbuzz 500000000 > /dev/null  11.60s user 0.65s system 99% cpu 12.255 total
```

しかし、並列化するのであれば別にCPUでやってもよいのでRustでRayonを使って並列化してあげると普通にGPUのコードより速くなります。


```console
$ time cargo run --release 500000000 > /dev/null
   Compiling rayon-fizzbuzz v0.1.0 (/home/shun/Rust/rayon-fizzbuzz)
    Finished release [optimized] target(s) in 0.48s
     Running `target/release/rayon-fizzbuzz 500000000`
cargo run --release 500000000 > /dev/null  17.90s user 6.44s system 2757% cpu 0.882 total
```

約4.1GiB/s出ています。速いですね。因みにGPUはAMD Radeon RX6800（演算ユニット数60）、CPUはRythen Threadrippir 3990X（64C128T）です。約4.1GiB/sというスコアは `/dev/zero` を `/dev/null` に `dd` したときのものに迫る勢いです。

``` console
$ time dd if=/dev/zero of=/dev/null bs=3907412008 count=1
0+1 レコード入力
0+1 レコード出力
2147479552 bytes (2.1 GB, 2.0 GiB) copied, 0.418874 s, 5.1 GB/s
dd if=/dev/zero of=/dev/null bs=3907412008 count=1  0.00s user 0.47s system 95% cpu 0.489 total
```

こうすると元記事のGPUを使った約845 MiB/sという数値の低さが気になるかと思います。どこで遅くなっているのでしょう。ちゃんと検証していませんが、いくつか心当たりはあります。

1. GPUへのenqueu→メモリのマッピング→読み出しが同期的
2. writevが遅いっぽい…？

多少解説しておきます。憶測なので正しいかは分かりませんが。

## GPUの同期的な実行

元のOpenCLのプログラムを抜粋するとこうなっています。

```c
    ret = clEnqueueNDRangeKernel(queue,
                                 kernel,
                                 1,
                                 &offset,
                                 &worksize,
                                 &local_work_size,
                                 0,
                                 NULL,
                                 NULL);
    // エラー処理省略

    // ここでGPUの実行を待つ
    ret = clFinish(queue);

    // エラー処理省略

    cl_char *ptr;
    ptr = (cl_char *) clEnqueueMapBuffer(
                                         queue,
                                         buffer,
                                         // ここがCL_TRUEだとメモリマッピングを
                                         // 同期的に行う
                                         CL_TRUE,
                                         CL_MAP_READ,
                                         cell_size * offset,
                                         cell_size * worksize,
                                         0,
                                         NULL,
                                         NULL,
                                         &ret);

```

GPUでの実行とメモリのマップ操作が同期的になっています。この部分で遅くなったんじゃないかと思っています。特にメモリのマップ操作はRayonで書いたものには存在しないのでその辺のコストが効いてそうですよね。

これの対策としてループ展開してレイテンシを隠蔽するか、OpenCLにあるイベント機構を使って全部非同期にするかという案はあったのですが実装が面倒だったのでやってません。少なくともイベント使えば `clFinish` は要らないはずなんですけどね。

## writevが遅い…？

記事中で説明していたとおり、値が入っているバッファはやや飛び飛びなのでした。

```text
+---------------+---------------+---------------+---------------+
| fizzbuzz fizzbuzz| .......... | fizzbuzz fizzbuzz| .......... | global
+---------------+---------------+---------------+---------------+
```

これに対して1カタマリごとに `write` を呼んでると遅いので、複数の断片に一気に `write` を発行できる `writev` を使っていました。今回は書き込み対象が `/dev/null` なのでカーネル側での処理はほとんどなく、システムコールのコンテキストスイッチが一番嵩む予定でした。しかし軽く計測してみると `writev` がそこそこ時間を取っていました。謎です。

多少まとめたとはいえバッファが細かすぎたんじゃないかとか、なんかエラーが出るので複数回に分けて `writev` を使ってたりとか色々心当たりはあるのですが、コードを書き換えてしまいましたし、パラメータ調整でまた状況も変わってたりするのであんまり深追いはしていなくて、確かなことは言えません。

最終的には `io_uring` で `writev` を発行するという頑張ったことをしているのですがむしろ頑張ったせいで遅いという可能性もあり、よくわかりません。

まあエイプリルフールのジョークだし、ということで適当に流してます。

# 実装言語の話

GPU使う手段はけっこう色々あるのでどのドライバとどのカーネル言語を使うか迷ったのですが、GPUのイベントを使いそうな気がしたのでOpenCLを使ってみました。結局イベントは使わなかった（使う余力がなかった）んですけどね。OpenCLのRustバインディングがあるのでそれを使ってRustで書く可能性もあったのですが変にはまって間に合わないリスクを考慮して素のOpenCLを使ってます。また、カーネル言語に[rust-gpu](https://github.com/EmbarkStudios/rust-gpu)というRustをカーネルにコンパイルするプロジェクトを使う案も密かにありましたが、対応状況を見るとバリアが未実装のようだったので多分使ってたら爆死してました。

最終的にできたコードを見ればRustのGPUドライバのwgpuでも書けたんじゃないかという気もします。ただ、今のところブロッキングなAPIしかないのでGPU操作を非同期にするというFuture Workには辿りつけなそうですね。

# おわりに

今年のエイプリルフールのプロジェクトはいかがだったでしょうか。年明けあたりからちまちま勉強してたGPU関連の知識をアウトプットできて書いた側としては満足しています。まだ初心者なので書いた内容に誤りがあったりコードに変なところがあったりしないか不安ですが、成長記録として残しておこうかなと思ってます。

それでは新年度頑張っていきましょう。

# 付録A ナイーブなFizzBuzz

``` rust
use std::env;
use std::io::{stdout, Result, Write};
use std::time::Instant;

fn fizzbuzz(n: u64) -> Result<Vec<u8>> {
    let mut o = Vec::with_capacity((n * 8) as usize);
    for i in (0..n).step_by(15) {
        let i = i * 15;
        writeln!(&mut o, "FizzBuzz")?;
        writeln!(&mut o, "{}", i + 1)?;
        writeln!(&mut o, "{}", i + 2)?;
        writeln!(&mut o, "Fizz")?;
        writeln!(&mut o, "{}", i + 4)?;
        writeln!(&mut o, "Buzz")?;
        writeln!(&mut o, "Fizz")?;
        writeln!(&mut o, "{}", i + 7)?;
        writeln!(&mut o, "{}", i + 8)?;
        writeln!(&mut o, "Fizz")?;
        writeln!(&mut o, "Buzz")?;
        writeln!(&mut o, "{}", i + 11)?;
        writeln!(&mut o, "Fizz")?;
        writeln!(&mut o, "{}", i + 13)?;
        writeln!(&mut o, "{}", i + 14)?;
    }
    Ok(o)
}

fn main() -> Result<()> {
    let n = env::args()
        .nth(1)
        .map(|s| s.parse::<u64>().unwrap())
        .unwrap_or(1024 * 1024 * 1024);
    let buf = fizzbuzz(n)?;
    let o = stdout();
    let mut o = o.lock();
    o.write_all(&buf)?;
    Ok(())
}
```

# 付録B Rayonを使ったFizzBuzz

``` rust
use rayon::iter::IntoParallelIterator;
use rayon::prelude::*;
use std::env;
use std::io::{stdout, Result, Write};

fn fizzbuzz(start: u64, end: u64) -> Result<Vec<u8>> {
    let mut o = Vec::with_capacity(((end - start) * 8) as usize);

    for i in (start..end).step_by(15) {
        writeln!(&mut o, "FizzBuzz")?;
        writeln!(&mut o, "{}", i + 1)?;
        writeln!(&mut o, "{}", i + 2)?;
        writeln!(&mut o, "Fizz")?;
        writeln!(&mut o, "{}", i + 4)?;
        writeln!(&mut o, "Buzz")?;
        writeln!(&mut o, "Fizz")?;
        writeln!(&mut o, "{}", i + 7)?;
        writeln!(&mut o, "{}", i + 8)?;
        writeln!(&mut o, "Fizz")?;
        writeln!(&mut o, "Buzz")?;
        writeln!(&mut o, "{}", i + 11)?;
        writeln!(&mut o, "Fizz")?;
        writeln!(&mut o, "{}", i + 13)?;
        writeln!(&mut o, "{}", i + 14)?;
    }
    Ok(o)
}

fn main() -> Result<()> {
    let n = env::args()
        .nth(1)
        .map(|s| s.parse::<u64>().unwrap())
        .unwrap_or(1024 * 1024 * 1024);
    // 面倒くさいのでCPU並列数はきめうち
    let block_size = (n + 127) / 128;
    let bufs = (0..128)
        .into_par_iter()
        .map(|k| fizzbuzz(k * block_size, (k + 1) * block_size - 1))
        .collect::<Vec<_>>();
    let o = stdout();
    let mut o = o.lock();
    for buf in bufs {
        o.write_all(&(buf?))?;
    }
    Ok(())
}

```
