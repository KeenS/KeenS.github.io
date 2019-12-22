---
categories: [RUST, rustup]
date: 2016-06-12T15:23:06+09:00
title: multirustが非推奨になったようなのでrustupに移行する
---

κeenです。どうやらもうそろそろRustのツールチェーン管理はmultirustじゃなくてrustupを使った方が良さそうなので移行しようと思います。
<!--more-->
きっかけはmultirustにそういうコミットがされたから。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">もうmultirustじゃなくてrustup使え、と。<br><br>Update README.md · brson/multirust@84c3459<a href="https://t.co/ySeRB50x5C">https://t.co/ySeRB50x5C</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/741077158651514882">2016年6月10日</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

因みにrustupは[rust-lang/rustup.sh: The rustup.sh script for installing Rust from release channels](https://github.com/rust-lang/rustup.sh)ではなく[rust-lang-nursery/rustup.rs: The Rust toolchain installer](https://github.com/rust-lang-nursery/rustup.rs)の方です。インストールガイドなどはこちらから。[rustup.rs - The Rust toolchain installer](https://www.rustup.rs/)

なんかベータ版のようですが推奨していいんですかね…。

とりあえずmultirustを入れたままコマンドを叩いてみます。

```
$ curl https://sh.rustup.rs -sSf | sh
info: downloading installer
warning: it looks like you have an existing installation of multirust
warning: rustup cannot be installed alongside multirust
warning: run `/usr/local/lib/rustlib/uninstall.sh` as root and delete `/home/kim/.multirust/version` before installing rustup
error: cannot install while multirust is installed
rustup: command failed: /tmp/tmp.9kjQj9WT5O/rustup-init

```

ふーむ。アンインストールしろ、と。一応指示された通りmultirust自体はアンインストールせずにversionだけを削除します。

```
$ sudo /usr/local/lib/rustlib/uninstall.sh
install: uninstalling component 'multirust'

    multirust is uninstalled.
$ rm -rf ~/.multirust/version
```

もう一度。

```
$ curl https://sh.rustup.rs -sSf | sh
info: downloading installer

Welcome to Rust!

This will download and install the official compiler for the Rust programming 
language, and its package manager, Cargo.

It will add the cargo, rustc, rustup and other commands to Cargo's bin 
directory, located at:

  /home/kim/.cargo/bin

This path will then be added to your PATH environment variable by modifying the 
profile file located at:

  /home/kim/.profile

You can uninstall at any time with rustup self uninstall and these changes will 
be reverted.

WARNING: This is beta software.

Current installation options:

     default toolchain: stable
  modify PATH variable: yes

1) Proceed with installation (default)
2) Customize installation
3) Cancel installation

```

選択肢が出ました。とりあえず1を選択します。


```
error: toolchain 'stable' is not installed
info: caused by: not a directory: '/home/kim/.multirust/toolchains/stable-x86_64-unknown-linux-gnu'
rustup: command failed: /tmp/tmp.nugK4VN4p8/rustup-init
```

えー。ちょっと…。仕方ないのでシンボリックリンクを貼って騙してみます。



```
$ ls ~/.multirust/toolchains/
beta  nightly  stable
$ ln -s ~/.multirust/toolchains/{stable,stable-x86_64-unknown-linux-gnu}
```

3度目の正直なるか

```
$ curl https://sh.rustup.rs -sSf | sh
info: downloading installer

Welcome to Rust!

This will download and install the official compiler for the Rust programming 
language, and its package manager, Cargo.

It will add the cargo, rustc, rustup and other commands to Cargo's bin 
directory, located at:

  /home/kim/.cargo/bin

This path will then be added to your PATH environment variable by modifying the 
profile file located at:

  /home/kim/.profile

You can uninstall at any time with rustup self uninstall and these changes will 
be reverted.

WARNING: This is beta software.

Current installation options:

     default toolchain: stable
  modify PATH variable: yes

1) Proceed with installation (default)
2) Customize installation
3) Cancel installation
1

info: updating existing rustup installation


Rust is installed now. Great!

To get started you need Cargo's bin directory in your PATH environment variable.
Next time you log in this will be done automatically.

To configure your current shell run source $HOME/.cargo/env.

```

インストール出来たよう。しかしセットアップは `~/.profile` に書かれたようなので `.zshrc` は自分で更新します。


```diff
diff --git a/zshrc b/zshrc
index 0178873..589c6a5 100644
--- a/zshrc
+++ b/zshrc
@@ -213,7 +213,7 @@ export PATSHOME=~/compile/ATS2-Postiats-$ATS_VERSION/
 export PATH=$PATSHOME/bin:$PATH
 export PATSHOMERELOC=~/compile/ATS2-Postiats-contrib-$ATS_VERSION
 
-export PATH=$PATH:~/.multirust/toolchains/stable/cargo/bin
+export PATH="$HOME/.cargo/bin:$PATH"
 
 # OPAM configuration
 . /home/kim/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

```

使ってみます。


```
rustup 0.1.12 (c6e430a 2016-05-12)
The Rust toolchain installer

USAGE:
    rustup [FLAGS] [SUBCOMMAND]

FLAGS:
    -v, --verbose    Enable verbose output
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    show         Show the active and installed toolchains
    update       Update Rust toolchains
    default      Set the default toolchain
    toolchain    Modify or query the installed toolchains
    target       Modify a toolchain's supported targets
    override     Modify directory toolchain overrides
    run          Run a command with an environment configured for a given toolchain
    which        Display which binary will be run for a given command
    doc          Open the documentation for the current toolchain.
    self         Modify the rustup installation
    telemetry    rustup telemetry commands
    help         Prints this message or the help of the given subcommand(s)

rustup installs The Rust Programming Language from the official
release channels, enabling you to easily switch between stable, beta,
and nightly compilers and keep them updated. It makes cross-compiling
simpler with binary builds of the standard library for common platforms.

If you are new to Rust consider running `rustup doc --book`
to learn Rust.
```

multirustを使っていたら馴染めそうですね。

一応どこにインストールされたのか確認。


```
$ which rustc
/home/kim/.cargo/bin/rustc
```

ふむふむ。ちょいとアップデートしてみます。


```
$ rustup update
info: syncing channel updates for 'stable-x86_64-unknown-linux-gnu'
info: downloading component 'rustc'
 43.2 MiB /  43.2 MiB (100 %)   2.1 MiB/s ETA:   0 s                
info: downloading component 'rust-std'
 55.3 MiB /  55.3 MiB (100 %)   3.0 MiB/s ETA:   0 s                
info: downloading component 'rust-docs'
  6.4 MiB /   6.4 MiB (100 %)   2.9 MiB/s ETA:   0 s                
info: downloading component 'cargo'
  4.1 MiB /   4.1 MiB (100 %)   2.3 MiB/s ETA:   0 s                
info: rolling back changes
error: could not read component file: '/home/kim/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/manifest-'
info: syncing channel updates for 'stable-x86_64-unknown-linux-gnu'
info: downloading component 'rustc'
 43.2 MiB /  43.2 MiB (100 %) 953.6 KiB/s ETA:   0 s                
info: downloading component 'rust-std'
 55.3 MiB /  55.3 MiB (100 %)   1.8 MiB/s ETA:   0 s                
info: downloading component 'rust-docs'
  6.4 MiB /   6.4 MiB (100 %)   2.2 MiB/s ETA:   0 s                
info: downloading component 'cargo'
  4.1 MiB /   4.1 MiB (100 %)   1.5 MiB/s ETA:   0 s                
info: rolling back changes
error: could not read component file: '/home/kim/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/manifest-'
info: syncing channel updates for 'beta-x86_64-unknown-linux-gnu'
info: downloading component 'rustc'
 44.8 MiB /  44.8 MiB (100 %)   1.4 MiB/s ETA:   0 s                
info: downloading component 'rust-std'
 57.9 MiB /  57.9 MiB (100 %)   2.9 MiB/s ETA:   0 s                
info: downloading component 'rust-docs'
  7.0 MiB /   7.0 MiB (100 %)   1.8 MiB/s ETA:   0 s                
info: downloading component 'cargo'
  4.1 MiB /   4.1 MiB (100 %)   1.3 MiB/s ETA:   0 s                
info: installing component 'rustc'
info: installing component 'rust-std'
info: installing component 'rust-docs'
info: installing component 'cargo'
info: syncing channel updates for 'nightly-x86_64-unknown-linux-gnu'
info: downloading component 'rustc'
 46.5 MiB /  46.5 MiB (100 %) 924.8 KiB/s ETA:   0 s                
info: downloading component 'rust-std'
 59.0 MiB /  59.0 MiB (100 %)   2.4 MiB/s ETA:   0 s                
info: downloading component 'rust-docs'
  7.3 MiB /   7.3 MiB (100 %)   1.8 MiB/s ETA:   0 s                
info: downloading component 'cargo'
  4.1 MiB /   4.1 MiB (100 %)   3.2 MiB/s ETA:   0 s                
info: installing component 'rustc'
info: installing component 'rust-std'
info: installing component 'rust-docs'
info: installing component 'cargo'
info: checking for self-updates
info: rustup is up to date

                           stable update failed - rustc 1.9.0 (e4e8b6668 2016-05-18)
  stable-x86_64-unknown-linux-gnu update failed - rustc 1.9.0 (e4e8b6668 2016-05-18)
                                 beta installed - rustc 1.10.0-beta.2 (39f3c16cc 2016-05-26)
                              nightly installed - rustc 1.11.0-nightly (0554abac6 2016-06-10)

```

あれ、なんかコケてる…。シンボリックリンクが悪さしたかな…。しかし試しに手元のプロダクトをコンパイルしてみたら通ったので使えているよう。

さて、クロスコンパイルの方はどうでしょうか。以前Raspberry Pi向けにクロスコンパイルしました [RustでベアメタルRaspberry PiのLチカ | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2016/05/04/rustdebeametaruraspberry_pinolchika/)。これは動くでしょうか。


とりあえずインストール可能なターゲットを確認

```
$ rustup target list
error: toolchain 'stable-x86_64-unknown-linux-gnu' does not support components
```


…………。無理矢理addしてみます。

```
$ rustup target add nightly-arm-unknown-linux-gnueabihf
error: toolchain 'stable-x86_64-unknown-linux-gnu' does not support components
```


…………………………………。


なんか変ですね。

一旦

```
$ rm ~/.multirust/toolchains/stable-x86_64-unknown-linux-gnu
```


してもう一度 `rustup update` します。

```
$ rustup update
info: syncing channel updates for 'stable-x86_64-unknown-linux-gnu'
info: downloading component 'rustc'
 43.2 MiB /  43.2 MiB (100 %)   2.6 MiB/s ETA:   0 s                
info: downloading component 'rust-std'
 55.3 MiB /  55.3 MiB (100 %)   3.7 MiB/s ETA:   0 s                
info: downloading component 'rust-docs'
  6.4 MiB /   6.4 MiB (100 %)   5.0 MiB/s ETA:   0 s                
info: downloading component 'cargo'
info: installing component 'rustc'
info: installing component 'rust-std'
info: installing component 'rust-docs'
info: installing component 'cargo'
info: syncing channel updates for 'beta-x86_64-unknown-linux-gnu'
info: syncing channel updates for 'beta-x86_64-unknown-linux-gnu'
info: syncing channel updates for 'nightly-x86_64-unknown-linux-gnu'
info: syncing channel updates for 'nightly-x86_64-unknown-linux-gnu'
info: checking for self-updates
info: rustup is up to date

                            stable installed - rustc 1.9.0 (e4e8b6668 2016-05-18)
                              beta unchanged - rustc 1.10.0-beta.2 (39f3c16cc 2016-05-26)
     beta-x86_64-unknown-linux-gnu unchanged - rustc 1.10.0-beta.2 (39f3c16cc 2016-05-26)
                           nightly unchanged - rustc 1.11.0-nightly (0554abac6 2016-06-10)
  nightly-x86_64-unknown-linux-gnu unchanged - rustc 1.11.0-nightly (0554abac6 2016-06-10)
```

あれ、今回は上手くいった。やっぱりシンボリックリンクが悪かったよう。リトライ。


```
$ rustup target list
aarch64-apple-ios
aarch64-unknown-linux-gnu
arm-linux-androideabi
arm-unknown-linux-gnueabi
arm-unknown-linux-gnueabihf
armv7-apple-ios
armv7-unknown-linux-gnueabihf
armv7s-apple-ios
i386-apple-ios
i586-pc-windows-msvc
i586-unknown-linux-gnu
i686-apple-darwin
i686-pc-windows-gnu
i686-pc-windows-msvc
i686-unknown-freebsd
i686-unknown-linux-gnu
i686-unknown-linux-musl
mips-unknown-linux-gnu
mips-unknown-linux-musl
mipsel-unknown-linux-gnu
mipsel-unknown-linux-musl
powerpc-unknown-linux-gnu
powerpc64-unknown-linux-gnu
powerpc64le-unknown-linux-gnu
x86_64-apple-darwin
x86_64-apple-ios
x86_64-pc-windows-gnu
x86_64-pc-windows-msvc
x86_64-rumprun-netbsd
x86_64-unknown-freebsd
x86_64-unknown-linux-gnu (default)
x86_64-unknown-linux-musl
x86_64-unknown-netbsd
$ rustup target add --toolchain nightly arm-unknown-linux-gnueabihf
info: downloading component 'rust-std' for 'arm-unknown-linux-gnueabihf'
 62.6 MiB /  62.6 MiB (100 %)   1.4 MiB/s ETA:   0 s                
info: installing component 'rust-std' for 'arm-unknown-linux-gnueabihf'
```

んー、ちゃんとnightlyが入ったのか分からない。

```
$ rustup show
installed toolchains
--------------------

stable
stable-x86_64-unknown-linux-gnu (default)
beta
beta-x86_64-unknown-linux-gnu
nightly
nightly-x86_64-unknown-linux-gnu

active toolchain
----------------

stable-x86_64-unknown-linux-gnu (default)
rustc 1.9.0 (e4e8b6668 2016-05-18)

```

しかし件のARM向けプロジェクトのクロスコンパイルは出来たよう。

もう一度listを見る。

```
$ rustup target list
aarch64-apple-ios
aarch64-linux-android
aarch64-unknown-linux-gnu
arm-linux-androideabi
arm-unknown-linux-gnueabi
arm-unknown-linux-gnueabihf (installed)
armv7-apple-ios
armv7-unknown-linux-gnueabihf
armv7s-apple-ios
i386-apple-ios
i586-pc-windows-msvc
i586-unknown-linux-gnu
i686-apple-darwin
i686-linux-android
i686-pc-windows-gnu
i686-pc-windows-msvc
i686-unknown-freebsd
i686-unknown-linux-gnu
i686-unknown-linux-musl
mips-unknown-linux-gnu
mips-unknown-linux-musl
mipsel-unknown-linux-gnu
mipsel-unknown-linux-musl
powerpc-unknown-linux-gnu
powerpc64-unknown-linux-gnu
powerpc64le-unknown-linux-gnu
x86_64-apple-darwin
x86_64-apple-ios
x86_64-pc-windows-gnu
x86_64-pc-windows-msvc
x86_64-rumprun-netbsd
x86_64-unknown-freebsd
x86_64-unknown-linux-gnu (default)
x86_64-unknown-linux-musl
x86_64-unknown-netbsd
```

`arm-unknown-linux-gnueabihf (installed)` とあるのでインストール出来ているよう。しかし本当にnightlyなのか。


```
rustup target list --toolchain stable
aarch64-apple-ios
aarch64-unknown-linux-gnu
arm-linux-androideabi
arm-unknown-linux-gnueabi
arm-unknown-linux-gnueabihf
armv7-apple-ios
armv7-unknown-linux-gnueabihf
armv7s-apple-ios
i386-apple-ios
i586-pc-windows-msvc
i586-unknown-linux-gnu
i686-apple-darwin
i686-pc-windows-gnu
i686-pc-windows-msvc
i686-unknown-freebsd
i686-unknown-linux-gnu
i686-unknown-linux-musl
mips-unknown-linux-gnu
mips-unknown-linux-musl
mipsel-unknown-linux-gnu
mipsel-unknown-linux-musl
powerpc-unknown-linux-gnu
powerpc64-unknown-linux-gnu
powerpc64le-unknown-linux-gnu
x86_64-apple-darwin
x86_64-apple-ios
x86_64-pc-windows-gnu
x86_64-pc-windows-msvc
x86_64-rumprun-netbsd
x86_64-unknown-freebsd
x86_64-unknown-linux-gnu (default)
x86_64-unknown-linux-musl
x86_64-unknown-netbsd
$ rustup target list --toolchain nightly
aarch64-apple-ios
aarch64-linux-android
aarch64-unknown-linux-gnu
arm-linux-androideabi
arm-unknown-linux-gnueabi
arm-unknown-linux-gnueabihf (installed)
armv7-apple-ios
armv7-unknown-linux-gnueabihf
armv7s-apple-ios
i386-apple-ios
i586-pc-windows-msvc
i586-unknown-linux-gnu
i686-apple-darwin
i686-linux-android
i686-pc-windows-gnu
i686-pc-windows-msvc
i686-unknown-freebsd
i686-unknown-linux-gnu
i686-unknown-linux-musl
mips-unknown-linux-gnu
mips-unknown-linux-musl
mipsel-unknown-linux-gnu
mipsel-unknown-linux-musl
powerpc-unknown-linux-gnu
powerpc64-unknown-linux-gnu
powerpc64le-unknown-linux-gnu
x86_64-apple-darwin
x86_64-apple-ios
x86_64-pc-windows-gnu
x86_64-pc-windows-msvc
x86_64-rumprun-netbsd
x86_64-unknown-freebsd
x86_64-unknown-linux-gnu (default)
x86_64-unknown-linux-musl
x86_64-unknown-netbsd
```

どうやら本当にnightlyのよう。

# まとめ

色々あたふたとしましたがrustupをインストールして使ってみました。multirustを使っているとインストールあたりでやや難はありそうですがそれ以降は普通に使えそうです。

皆様もお試しあれ。
