---
categories: [番外編]
date: 2018-02-03T21:57:45+09:00
title: "情報科学若手の会冬の陣2018でライブコーディングしてきた"
---
κeenです。[情報科学若手の会冬の陣2018 #wakate2018w](https://wakate.connpass.com/event/74427/)でライブコーディングしてきたのでその補足です。
Rustは`unwrap`を多用する言語ではありません。
<!--more-->

# コード

お題はカレントディレクトリ以下のファイルのサイズを数える`du`みたいなもの。
書いたコードはこれ。

``` rust
use std::path::Path;
use std::fs::read_dir;
use std::env;

fn count_dir<P: AsRef<Path>>(path: P) -> u64 {
    read_dir(path)
        .unwrap()
        .map(|e| {
            let e = e.unwrap();
            if e.metadata().unwrap().is_file() {
                e.metadata().unwrap().len()
            } else if e.metadata().unwrap().is_dir() {
                count_dir(e.path())

            } else {
                0
            }

        })
        .sum()
}


fn main() {
    let path = env::args().nth(1).unwrap();
    println!("{}", count_dir(path));
}
```


`unwrap`まみれでひどいですね。


これを書き直したらこうなります。

``` rust
use std::path::Path;
use std::fs::read_dir;
use std::env;
use std::io;

fn count_dir<P: AsRef<Path>>(path: P) -> io::Result<u64> {
    read_dir(path)?
        .map(|e| {
            let e = e?;
            let meta = e.metadata()?;
            if meta.is_file() {
                Ok(meta.len())
            } else if meta.is_dir() {
                Ok(count_dir(e.path())? + meta.len())
            } else {
                Ok(0)
            }

        })
        .sum()
}


fn main() {
    let path = env::args().nth(1).unwrap();
    println!("{}", count_dir(path).unwrap());
}
```

ほぼ`count_dir`の返り値を`io::Result<u64>`にして`unwrap`を`?`にしただけですね。`?`便利!。

# `du`と出力が合わない問題

``` console
$ du ./
...
5553252 ./
$ rust du.rs ./
4404925617650
```

ライブコーディングの時はちょっとずれてると思ったんですがよくみたら桁が違いますね。
これはどうやら`du`がディスク上の専有スペースの推定を出すかららしく、ちゃんとオプションをつけたらそれっぽくなりました。

``` rust
# -bでファイルサイズの合計を出す
$ du -b ./
4403619672761   ./
```

だいだい合ってそう。

# 何故かrust版の方がファイルサイズが多い問題

`du`と出力が合わない問題突き詰めるとrust版の方が処理しているファイルが多くなってます。
原因不明。`du`はバイナリやrlibファイルの一部をスルーしているようですが規則性分からず。識者求む。

# 何故か読めないファイルがある問題
ただのsymlinkでした
