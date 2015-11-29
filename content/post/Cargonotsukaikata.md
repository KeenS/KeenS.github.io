---
categories: [Rust, Command Line, 小ネタ]
date: 2015-11-29T23:25:54+09:00
draft: true
title: Cargoの使い方
---
この記事は[Rust Advent Calendar 2015](http://qiita.com/advent-calendar/2015/rust-lang) 3日目の記事です。  
前 [Rustで小さなツールを作ってみる(後編)](/blog/2015/11/29/rustdechiisanatsu_ruwotsukuttemiru_kouhen_/)  
次  szkttyさん インデックス構文によるアクセスを実装する

κeenです。Rustを使うにはCargoを使う必要がありますが、`cargo help`を見てもあまり情報が載っていないので少しばかり書きましょう。

<!--more-->

# `new`

```
Create a new cargo package at <path>

Usage:
    cargo new [options] <path>
    cargo new -h | --help

Options:
    -h, --help          Print this message
    --vcs VCS           Initialize a new repository for the given version
                        control system (git or hg) or do not initialize any version
                        control at all (none) overriding a global configuration.
    --bin               Use a binary instead of a library template
    --name NAME         Set the resulting package name
    -v, --verbose       Use verbose output
    -q, --quiet         No output printed to stdout
    --color WHEN        Coloring: auto, always, never
```

これはみなさんよく使うのでほとんど説明不要でしょう。`cargo new`または`cargo new --bin`の形でよく使います。
オプションを見て分かる通り、`cargo new foo-rs --name foo`のようにパスとcrateの名前を変えたりデフォルトのvcsにmercurialを使うことも出来ます。

# `build`

```
Compile a local package and all of its dependencies

Usage:
    cargo build [options]

Options:
    -h, --help               Print this message
    -p SPEC, --package SPEC  Package to build
    -j N, --jobs N           The number of jobs to run in parallel
    --lib                    Build only this package's library
    --bin NAME               Build only the specified binary
    --example NAME           Build only the specified example
    --test NAME              Build only the specified test target
    --bench NAME             Build only the specified benchmark target
    --release                Build artifacts in release mode, with optimizations
    --features FEATURES      Space-separated list of features to also build
    --no-default-features    Do not build the `default` feature
    --target TRIPLE          Build for the target triple
    --manifest-path PATH     Path to the manifest to compile
    -v, --verbose            Use verbose output
    -q, --quiet              No output printed to stdout
    --color WHEN             Coloring: auto, always, never

If the --package argument is given, then SPEC is a package id specification
which indicates which package should be built. If it is not given, then the
current package is built. For more information on SPEC and its format, see the
`cargo help pkgid` command.

Compilation can be configured via the use of profiles which are configured in
the manifest. The default profile for this command is `dev`, but passing
the --release flag will use the `release` profile instead.
```

恐らく一番よく使うタスクですね。ビルド対象を色々指定できるのはいいとして、実は`-j`オプションがあります。並行ビルド出来るやつですね。体感速度は変わりませんが。

# `run`

```
Run the main binary of the local package (src/main.rs)

Usage:
    cargo run [options] [--] [<args>...]

Options:
    -h, --help              Print this message
    --bin NAME              Name of the bin target to run
    --example NAME          Name of the example target to run
    -j N, --jobs N          The number of jobs to run in parallel
    --release               Build artifacts in release mode, with optimizations
    --features FEATURES     Space-separated list of features to also build
    --no-default-features   Do not build the `default` feature
    --target TRIPLE         Build for the target triple
    --manifest-path PATH    Path to the manifest to execute
    -v, --verbose           Use verbose output
    -q, --quiet             No output printed to stdout
    --color WHEN            Coloring: auto, always, never

If neither `--bin` nor `--example` are given, then if the project only has one
bin target it will be run. Otherwise `--bin` specifies the bin target to run,
and `--example` specifies the example target to run. At most one of `--bin` or
`--example` can be provided.

All of the trailing arguments are passed to the binary to run. If you're passing
arguments to both Cargo and the binary, the ones after `--` go to the binary,
the ones before go to Cargo.
```

実行可能ファイルのプロジェクトだった時に成果物を走らせます。あるいはexampleも走らせられます。とはいってもまだビルドしてなかったらビルドもするのでビルド向けのオプションがいっぱいありますね。

実行可能ファイルが複数あるなら`cargo run --bin xxx`で指定して走らせます。

`--release`ビルドした成果物を走らせたかったら`cargo run --release`しないといけません。

`cargo run -- args`で成果物に引数を渡せます。


# `test`

```
Execute all unit and integration tests of a local package

Usage:
    cargo test [options] [--] [<args>...]

Options:
    -h, --help               Print this message
    --lib                    Test only this package's library
    --bin NAME               Test only the specified binary
    --example NAME           Test only the specified example
    --test NAME              Test only the specified integration test target
    --bench NAME             Test only the specified benchmark target
    --no-run                 Compile, but don't run tests
    -p SPEC, --package SPEC  Package to run tests for
    -j N, --jobs N           The number of jobs to run in parallel
    --release                Build artifacts in release mode, with optimizations
    --features FEATURES      Space-separated list of features to also build
    --no-default-features    Do not build the `default` feature
    --target TRIPLE          Build for the target triple
    --manifest-path PATH     Path to the manifest to build tests for
    -v, --verbose            Use verbose output
    -q, --quiet              No output printed to stdout
    --color WHEN             Coloring: auto, always, never
    --no-fail-fast           Run all tests regardless of failure

All of the trailing arguments are passed to the test binaries generated for
filtering tests and generally providing options configuring how they run. For
example, this will run all tests with the name `foo` in their name:

    cargo test foo

If the --package argument is given, then SPEC is a package id specification
which indicates which package should be tested. If it is not given, then the
current package is tested. For more information on SPEC and its format, see the
`cargo help pkgid` command.

The --jobs argument affects the building of the test executable but does
not affect how many jobs are used when running the tests.

Compilation can be configured via the `test` profile in the manifest.
```

テスト走らせるやつです。

`cargo test`とだけすると全てのテストが走ります。地味にrustdocの中に書いたexampleも走ります。

# `search`

```
Search packages in crates.io

Usage:
    cargo search [options] <query>
    cargo search [-h | --help]

Options:
    -h, --help               Print this message
    --host HOST              Host of a registry to search in
    -v, --verbose            Use verbose output
    -q, --quiet              No output printed to stdout
    --color WHEN             Coloring: auto, always, never
```

crates.ioからパッケージを捜してきてくれます。よく使いますね。インデックスのアップデートが地味に重い。


# `fetch`
だんだんニッチなタスクを紹介していきます。

```
Fetch dependencies of a package from the network.

Usage:
    cargo fetch [options]

Options:
    -h, --help               Print this message
    --manifest-path PATH     Path to the manifest to fetch dependencies for
    -v, --verbose            Use verbose output
    -q, --quiet              No output printed to stdout
    --color WHEN             Coloring: auto, always, never

If a lockfile is available, this command will ensure that all of the git
dependencies and/or registries dependencies are downloaded and locally
available. The network is never touched after a `cargo fetch` unless
the lockfile changes.

If the lockfile is not available, then this is the equivalent of
`cargo generate-lockfile`. A lockfile is generated and dependencies are also
all updated.
```

dependenciesを全てローカルに持ってくるタスクです。

# `generate_lockfile`

```
Generate the lockfile for a project

Usage:
    cargo generate-lockfile [options]

Options:
    -h, --help               Print this message
    --manifest-path PATH     Path to the manifest to generate a lockfile for
    -v, --verbose            Use verbose output
    -q, --quiet              No output printed to stdout
    --color WHEN             Coloring: auto, always, never
```

Cargo.lockの生成をします。`cargo update`がロックファイルがないと怒ってくるのでそういう時に使うのでしょう。

# `package`

```
Assemble the local package into a distributable tarball

Usage:
    cargo package [options]

Options:
    -h, --help              Print this message
    -l, --list              Print files included in a package without making one
    --no-verify             Don't verify the contents by building them
    --no-metadata           Ignore warnings about a lack of human-usable metadata
    --manifest-path PATH    Path to the manifest to compile
    -v, --verbose           Use verbose output
    -q, --quiet             No output printed to stdout
    --color WHEN            Coloring: auto, always, never
```

Cargoにはcrates.ioにデプロイする機能もあります。他にも`publish`, `login`, `yank`も見ておきましょう。

# `install`

```
Install a Rust binary
Usage:
    cargo install [options] [<crate>]
    cargo install [options] --list
Specifying what crate to install:
    --vers VERS               Specify a version to install from crates.io
    --git URL                 Git URL to install the specified crate from
    --branch BRANCH           Branch to use when installing from git
    --tag TAG                 Tag to use when installing from git
    --rev SHA                 Specific commit to use when installing from git
    --path PATH               Filesystem path to local crate to install
Build and install options:
    -h, --help                Print this message
    -j N, --jobs N            The number of jobs to run in parallel
    --features FEATURES       Space-separated list of features to activate
    --no-default-features     Do not build the `default` feature
    --debug                   Build in debug mode instead of release mode
    --bin NAME                Only install the binary NAME
    --example EXAMPLE         Install the example EXAMPLE instead of binaries
    --root DIR                Directory to install packages into
    -v, --verbose             Use verbose output
    -q, --quiet               Less output printed to stdout
    --color WHEN              Coloring: auto, always, never
This command manages Cargo's local set of install binary crates. Only packages
which have [[bin]] targets can be installed, and all binaries are installed into
the installation root's `bin` folder. The installation root is determined, in
order of precedence, by `--root`, `$CARGO_INSTALL_ROOT`, the `install.root`
configuration key, and finally the home directory (which is either
`$CARGO_HOME` if set or `$HOME/.cargo` by default).
There are multiple sources from which a crate can be installed. The default
location is crates.io but the `--git` and `--path` flags can change this source.
If the source contains more than one package (such as crates.io or a git
repository with multiple crates) the `<crate>` argument is required to indicate
which crate should be installed.
Crates from crates.io can optionally specify the version they wish to install
via the `--vers` flags, and similarly packages from git repositories can
optionally specify the branch, tag, or revision that should be installed. If a
crate has multiple binaries, the `--bin` argument can selectively install only
one of them, and if you'd rather install examples the `--example` argument can
be used as well.
The `--list` option will list all installed packages (and their versions).
```

まだリリースチャネルには来てませんが、`install`も入る予定です。binプロジェクトをソースを持ってきてそのままビルド、インストールまでするやつです。勿論、ローカルのものもインストール出来ますよ。
[rustfmt](https://github.com/rust-lang-nursery/rustfmt)のようにREADMEのインストール方法に`cargo install`を書いているものもあります。これが使えるようになると配布がぐっと楽になりますね。


# プラグイン
Cargoのサブコマンドを自分で作るのは簡単です。`cargo foo`というタスクを作りたいのなら`cargo-foo`という名前の実行可能ファイルをパスに置いておくだけです。


試してみましょう

```
$ cat <<EOF > ~/bin/cargo-foo
#!/bin/sh
echo args are: "\$@"
echo \\\$0 is: \$0
EOF
$ chmod +x  ~/bin/cargo-foo
$ cargo foo aa bb cc
args are: foo aa bb cc
$0 is: /home/kim/bin/cargo-foo
$ cargo help foo aa bb cc
args are: foo -h
$0 is: /home/kim/bin/cargo-foo
```

ちょっと独特な引数の渡り方をしてますね。しかし予め了解しておけば特に問題はなさそうです。1つサブコマンドを作ってみましょう。

指定した名前のパッケージをCargo.tomlのdependenciesに書き足してくれるサブコマンドです。

``` sh
#!/bin/sh
usage(){
    cat <<EOF
Add the dependency of crate to Cargo.toml

Usage:
    cargo use <crate> [version]
    cargo use -h | --help

Description:
    Add the dependency of crate to Cargo.toml.
    If vension is omitted, adopt the latest version.

EOF
}

version(){
    cargo search "$1" | grep -Eo "^$1 \(.*?\)" | sed "s/^$1 (\(.*\))/\1/"
}

find_cargo(){
    # TODO: look up parent directories
    ls | grep '^Cargo.toml$'
}

ensure_dep_exists(){
    cargo="$1"
    if ! grep -F '[dependencies]' "$cargo" > /dev/null 2>&1; then
        echo '[dependencies]' >> "$cargo"
    fi
}

insert_dep(){
    cargo="$1"
    crate="$2"
    version="$3"
    # FIXME: Because Linux and Mac behaves defferently on null string argument, don't use it and adopt workaround.
    sed -i'' "/\[dependencies\]/{a\
$crate = \"$version\"
}" "$cargo"
}

run(){
    CARGO_TOML="$(find_cargo)"
    if [ $? != 0 ]
    then
        echo "Cargo.toml not found" 1>&2
        exit 1
    fi

    if [ -z "$1" ]; then
        usage
        exit 1
    else
        CRATE="$1"
    fi

    if [ -z "$2" ]; then
        VERSION="$(version $1)"
    else
        VERSION="$2"
    fi

    ensure_dep_exists "$CARGO_TOML"
    insert_dep "$CARGO_TOML" "$CRATE" "$VERSION"    
}

main(){
    # $1 is "use" when called as a cargo plugin
    if [ "$1" != use ]; then
        echo "Use this as a cargo plugin"
        usage
        exit 1
    fi
    shift
    if [ "$1" = -h ] || [ "$1" = --help ]; then
        usage
        exit
    else
        run "$@"
    fi
}


main "$@"

```

まだ洗練されていませんがお気に入りのタスクです。誰かCLIからCargo.tomlをいじれるツール作ってくれないかな。
