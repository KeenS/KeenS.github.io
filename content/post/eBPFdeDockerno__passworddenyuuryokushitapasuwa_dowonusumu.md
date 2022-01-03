---
categories: [Docker, Linux, eBPF, Rust]
date: 2022-01-04T01:47:31+09:00
title: "eBPFでDockerの--passwordで入力したパスワードを盗む"
---

κeenです。[以前の記事](https://keens.github.io/blog/2021/12/20/rustdedockerno__passworddenyuuryokushitapasuwa_dowonusumu/)の続きで、 `/proc` を監視するのではなくeBPFで取得してみます。ついでにRustのコードをeBPFで動かせるライブラリのayaも使ってみます。

<!--more-->

以前の記事では `/proc` 以下の監視をビジーループで回すという力技で新しく作られたプロセスを補足していました。これだとCPU使用率が上がって美しくありませんし、何より原理的には見逃しもありえてしまいます。そこでカーネルの機能を使って全てのプロセスの作成を監視して低CPU使用率かつ捕捉率100%の実装を目指します。

## eBPFとは？
なんかLinuxカーネルでユーザの書いたコード動かせるやつです。私は説明できるほど詳しくないので適当にググって下さい。

1つやっかいな点として、独自のバイナリを動かすのでカーネル内で動かすコードはコンパイラがeBPFに対応したものでないといけません。また、カーネル内で動かすコードをユーザランドから制御するプログラムも同時に書く必要があります。

## Ayaとは？
RustでeBPFのコードが書けるライブラリです。
LLVMがeBPFに対応してるので、頑張ってRustをeBPFにコンパイルする部分、eBPFのコード内で使えるライブラリ、eBPFのコードを走らせるユーザランドプログラムなどの面倒をみてくれます。

[公式チュートリアル](https://aya-rs.github.io/book/intro/index.html)なんかもあります。ドキュメントの更新が追い付いてないようでちょくちょくそのままだと動かない所もありますが、落ち着いて最新のコードを読みにいくなどすれば完走できます。

## Ayaで `docker --password` を抜き出そう

eBPFでどうやるかは前回のやつで調べてたときにみつけました。

* [process - How to track newly created processes in Linux? - Unix & Linux Stack Exchange](https://unix.stackexchange.com/questions/260162/how-to-track-newly-created-processes-in-linux)

ここの回答を参考に、以下の `bpftrace` のプログラムを書けば取り出せることが分かっています。

```text
$ sudo bpftrace -e 'tracepoint:syscalls:sys_enter_exec* /str(*args->argv) == "docker"/{ printf("pid: %d, comm: %s, args: ", pid, comm); join(args->argv); }'
Attaching 2 probes...
pid: 884154, comm: zsh, args: docker login --password hoge
```

これをAyaを使ったRustのコードに移植することを目標にしましょう。

Ayaのチュートリアルにある内容はある程度省略するとして進めましょう。
`tracepoint` のテンプレートで作成し、 `syscalls` の `sys_enter_execve` を目標とするように入力します。
するとユーザランド側のプログラムが以下のように `syscalls` の `sys_enter_execve` にアタッチされるようになります。

```rust
    let program: &mut TracePoint = bpf.program_mut("docker_password_aya").unwrap().try_into()?;
    program.load()?;
    program.attach("syscalls", "sys_enter_execve")?;
```

本当は `execve` の他に `execveat` も必要みたいですが、この際まあいいでしょう。

eBPF側のプログラムを書き進めていきます。まず `-common` に以下を書きます。これに抜き出したパスワードを入れてやりとりします。

```rust
#[derive(Clone, Copy)]
#[repr(C)]
pub struct DockerLog {
    pub count: usize,
    pub data: [u8; 32],
}

#[cfg(feature = "user")]
unsafe impl aya::Pod for DockerLog {}
```

見てのとおりデータが最大32バイトになってます。任意長のデータのやりとりをどうするか分からなかったのでこうしました。ユーザ側で確保したメモリのポインタを送ってeBPF側で書いてあげればいいんですかね？それとも普通に文字列へのポインタ渡せば読み出せる？

`-common` はこれだけで、 `-ebpf` の方でやっていきます。

まず、 `ctx` から `argv` 情報を取り出したいので `ctx` のデータアラインメントを調べます。今回の例だと `/sys/kernel/debug/tracing/events/syscalls/sys_enter_execve/format` を見れば載っています。

```shell
$ sudo cat /sys/kernel/debug/tracing/events/syscalls/sys_enter_execve/format
name: sys_enter_execve
ID: 711
format:
 field:unsigned short common_type;      offset:0;       size:2; signed:0;
 field:unsigned char common_flags;      offset:2;       size:1; signed:0;
 field:unsigned char common_preempt_count;      offset:3;  size:1;    signed:0;
 field:int common_pid;  offset:4;       size:4; signed:1;

 field:int __syscall_nr;        offset:8;       size:4; signed:1;
 field:const char * filename;   offset:16;      size:8; signed:0;
 field:const char *const * argv;        offset:24;      size:8; signed:0;
 field:const char *const * envp;        offset:32;      size:8; signed:0;

print fmt: "filename: 0x%08lx, argv: 0x%08lx, envp: 0x%08lx", ((unsigned long)(REC->filename)), ((unsigned long
)(REC->argv)), ((unsigned long)(REC->envp))
```

`const char *const * argv` のオフセットが24です。この情報を元に作業していきましょう。

まず `argv` を取り出して、 `*argv` が `"docker\0"` と一致しているかを確認します。

```rust
use aya_bpf::helpers::{bpf_probe_read_user, bpf_probe_read_user_str},

unsafe fn try_docker_password_aya(ctx: &TracePointContext) -> Result<u32, i64> {
    let target = b"docker\0";
    let argv = ctx.read_at::<*const *const u8>(24)?;
    if argv.is_null() {
        return Err(0);
    }
    let mut exe = [0u8; 7];
    let exe_ptr = bpf_probe_read_user(argv)?;
    bpf_probe_read_user_str(exe_ptr, &mut exe)?;
    if &exe == target {
        // ...
    }
    Ok(0)
}
```

このときメモリの読み出しに `bpf_probe_read_` 系の関数を使わないといけないのに気付かずにハマりました。あとこのデータってカーネルじゃなくてユーザ側にあるんですね。そこでもハマりました。


`*argv` が `"docker\0"` だったら次に進みます。本当は `/usr/bin/docker` とかもケアした方がいいんでしょうが、細けえことはいいんだよ。

`if` の中身では `argv` を1つ1つ確認していって `--password` が見付かったら次の引数を `DockeLog` に入れて送り出すということをします。イテレータとかが使えずにちょっと煩雑ですがトリッキーなところはありません。

```rust
    if &exe == target {
        #[derive(Eq, PartialEq)]
        enum ReadState {
            NotYet,
            ReadNext,
            Done,
        }
        let target = b"--password\0";
        let mut arg = [0u8; 32];
        let mut read_password = ReadState::NotYet;
        let mut count = 0;
        for i in 0..100 {
            arg = [0u8; 32];
            let arg_ptr = bpf_probe_read_user(argv.offset(i))?;
            if arg_ptr.is_null() {
                return Ok(0);
            }
            count = bpf_probe_read_user_str(arg_ptr, &mut arg)?;
            if read_password == ReadState::ReadNext {
                read_password = ReadState::Done;
                break;
            }
            if &arg[..count] == &target[..] {
                read_password = ReadState::ReadNext;
            }
        }
        if read_password == ReadState::Done {
            let entry = DockerLog { count, data: arg };
            EVENTS.output(ctx, &entry, 0);
        }
    }
```

これでeBPF側は完成です。

最後にユーザランド側で `DockerLog` を受け取って表示する部分です。 `loop { /* ... */ }` の中身だけ書くと以下のようになります。

```rust
let events = buf.read_events(&mut buffers).await.unwrap();
for i in 0..events.read {
    let buf = &mut buffers[i];
    let ptr = buf.as_ptr() as *const DockerLog;
    let data = unsafe { ptr.read_unaligned() };
    let password = &data.data[0..data.count];
    let password = std::ffi::CStr::from_bytes_with_nul(&password);
    println!("LOG: password: {:?}", password);
}
```

NUL終端されてるので `CStr` を使ってる点以外では特段変なところはありません。

実行してみましょう。チュートリアルと違ってeBPFバイナリがプログラムに埋め込まれる設計なので `carg xtask build-ebpf` のあとに `cargo build` をする必要がある点以外は特筆することはありません。


```shell
$ cargo xtask build-ebpf
$ cargo build
$ sudo ./target/debug/docker-password-aya
...
LOG: password: Ok("hoge")


---------別ターミナル-------
$ docker login --password hoge
```

動きましたね。めでたしめでたし。

## まとめ

Ayaを使ってeBPFで新しく作られたプロセスを監視し、`--password` を抜き出すプログラムを書いてみました。まあ、実行に `sudo` が必要な点で前回よりも攻撃性は下がってますが、練習なのでね。文字列のやりとりをちゃんと調べきれてませんが、一旦はRustだけでeBPFを動かすことに成功したのでよしとしましょう。

eBPFに触れて満足しました。ソースコードは以下に置いています。参考にどうぞ。

[KeenS/docker-password-aya](https://github.com/KeenS/docker-password-aya/tree/main)

また、今回のエントリは以下の記事を参考にしました。ありがとうございます。

[RustでeBPFを操れるAyaを触ってみた - Qiita](https://qiita.com/moriai/items/c96636d35ae39809b45e#%E3%82%B9%E3%82%B1%E3%83%AB%E3%83%88%E3%83%B3%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%A0)
