---
categories: [Rust, Advent Calendar, Advent Calendar 2016]
date: 2016-12-02T17:52:30+09:00
title: Rustのプロセス
---

このエントリは[Rust その2 Advent Calendar 2016 - Qiita](http://qiita.com/advent-calendar/2016/rust-lang-2)3日目の記事です。

κeenです。Rustの[`std::process`](https://doc.rust-lang.org/std/process/index.html)の扱い方を紹介します。

<!--more-->

# ビルダー
これはビルダーになっていて、以下のように使えます。公式ドキュメントの例です。

``` rust
let output = Command::new("sh")
    .arg("-c")
    .arg("echo hello")
    .output()
    .expect("failed to execute process");

let hello = output.stdout;
println!("{}", std::str::from_utf8(&hello).unwrap());
```

```
hello
```

環境変数も渡せます

``` rust
let output = Command::new("sh")
    .arg("-c")
    // 環境変数を参照するコマンド
    .arg("echo $HELLO")
    // 環境変数を設定する
    .env("HELLO", "hello, world")
    .output()
    .expect("failed to execute process");

let hello = output.stdout;
println!("{}", std::str::from_utf8(&hello).unwrap());
```

``` rust
hello, world
```


あるいは実行するディレクトリも指定出来ます

``` rust
let output = Command::new("ls")
    // プロセスを実行するディレクトリを指定する
    .current_dir("/")
    .output()
    .expect("failed to execute process");

let hello = output.stdout;
println!("{}", std::str::from_utf8(&hello).unwrap());
```

```
bin
boot
cdrom
core
...
```


# 出力

上の例では `output` でstdio,stderr, exitstatus取り出していますが、子プロセスとして実行することも出来ます。

``` rust
let mut child = Command::new("ls")
    .current_dir("/")
    // outputではなくspawnを使う
    .spawn()
    .expect("failed to execute process");
let status = child.wait().unwrap();
println!("{}", status);
```


```
bin  boot  cdrom  core  dev  etc  home  initrd.img  initrd.img.old  lib  lib32  lib64  libx32  lost+found  media  mnt  opt  proc  root  run  sbin  snap  srv  sys  tmp  usr  var  vmlinuz  vmlinuz.old
```

この場合、子プロセスはRustへではなく直接stdoutへ出力するのでSIGPIPEが送られません。

あるいは出力先を手で指定することも出来ます。

``` rust
let child = Command::new("ls")
    .current_dir("/")
    // stdoutをnullにする
    .stdout(Stdio::null())
    .spawn()
    .expect("failed to execute process");
let status = child.wait().unwrap();
println!("{}", status);
```

ここではstdoutをnull ioに指定しています。他には`inherit`で親プロセスのものを引き継ぐか、`piped`で次のプロセスに渡すことも出来ます

``` rust
// 1つめのプロセスを作る
let mut child1 = Command::new("ls")
    .current_dir("/")
    // 出力は親プロセスへパイプする
    .stdout(Stdio::piped())
    .spawn()
    .expect("failed to execute process");

// 2つめのプロセスを作る
let mut child2 = Command::new("grep")
    .arg("bin")
    // 入力は親プロセスへパイプする
    .stdin(Stdio::piped())
    .spawn()
    .expect("failed to execute process");

{
    // Someと分かっているのでunwrapする。
    // この辺はrustの型システテムの限界。
    let out = child1.stdout.as_mut().unwrap();
    let in_ = child2.stdin.as_mut().unwrap();
    // 1つめのプロセスの出力から2つめのプロセスの入力へデータをコピーする
    io::copy(out, in_).unwrap();
}


let status1 = child1.wait().unwrap();
let status2 = child2.wait().unwrap();
println!("{}, {}", status1, status2);
```

```
bin
sbin
exit code: 0, exit code: 0
```

ちょっと繋ぎ込みが面倒ですし`io::copy`を使って手でコピーしてるのが頂けませんね。


# Unix
Unixに依存することを認めてしまえばもうちょっと色々なことが出来ます。

1つには

``` rust
use std::os::unix::process::CommandExt;
```

するといくつか追加のメソドが生えてくる仕組みになってます。

例えば、`exec` が使えます。


``` rust
let error = Command::new("ls")
    .current_dir("/")
    // execを呼ぶ
    .exec();

println!("after exec!!");
```

`exec`した後はRustのプロセスを別のものに置き換えてしまうので`println!("after exec!!");`の行が実行されません。
されるとしたら何らかの理由で`exec`に失敗した場合だけです。なので面白いことに`exec`の返り値はエラーのみです。

あるいは、`unsafe`なコードを認めるなら先のプロセスの繋ぎ込みはもうちょっとスマートに書けます。


``` rust
use std::os::unix::io::{AsRawFd, FromRawFd};
let mut child1 = Command::new("ls")
    .current_dir("/")
    // 標準出力をパイプする
    .stdout(Stdio::piped())
    .spawn()
    .expect("failed to spawn a process");

let mut child2 = Command::new("grep")
    .arg("bin")
    // 標準入力は手で作ったStdioオブジェクトにする。
    // ここでは1つめのプロセスの標準出力を直接繋ぐ。
    .stdin(unsafe{Stdio::from_raw_fd(child1.stdout.as_ref().unwrap().as_raw_fd())})
    .spawn()
    .expect("failed to spawn a process");

// 直接繋いでしまったのでコピーの必要はない

let status1 = child1.wait().unwrap();
let status2 = child2.wait().unwrap();
println!("{}, {}", status1, status2);
```

```
bin
sbin
exit code: 0, exit code: 0
```

`Stdio`がfdと行き来出来るのでそれを経由することでパイプを作れます。

もちろん、ファイルに書き出すことも出来ます。


``` rust
use std::os::unix::io::{AsRawFd, FromRawFd};

let file = File::create("test.txt").unwrap();
// ファイルからFDを経由してStdioを作る
let out = unsafe{Stdio::from_raw_fd(file.as_raw_fd())};
let mut child1 = Command::new("ls")
    .current_dir("/")
    .stdout(out)
    .spawn()
    .expect("failed to spawn a process");

let status1 = child1.wait().unwrap();
println!("{}", status1);
```

```
$ cat test.txt
bin
boot
cdrom
core
dev
...
```

# おわりに
最初は思ったよりCのプロセス回りのAPIと違って戸惑いますが馴れてしまえば使いやすいAPIでしょう。
