---
categories: [Idris, Idris Advent Calendar, Advent Calendar, Advent Calendar 2020]
date: 2020-12-01T21:50:14+09:00
title: "Idrisのインストール"
---

このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の2日目の記事です。前回は[依存型のあるHaskellことIdrisってどんな言語？](https://keens.github.io/blog/2020/11/30/idristtedonnagengo_/)でした。

κeenです。
Idrisの紹介が済んだので処理系のインストール方法を紹介します。

<!--more-->

# 処理系

IdrisはIdris 1とIdris 2があります。

公式で特に言及されてないのですが、1.0を刻んだ1が推奨で、まだ0.2な2が開発版のプレビューと思ってよさそうです。
今回は1のインストール方法を紹介します。

Advent Calendarのどこかで2についても紹介するかもしれません。

# インストール
## Windows/Mac

[バイナリダウンロード](https://www.idris-lang.org/pages/download.html#binary)のところからダウンロードすれば動くんじゃないですかね。
Linuxユーザなので詳しくは分かりません。

## Ubuntu

公式で推奨されてるのがCabalによるインストールです。
[wiki](https://github.com/idris-lang/Idris-dev/wiki/Idris-on-Ubuntu)にUbuntuでのインストールが書かれてますが、コマンドがちょっと古いので別の方法を紹介します。

使ってる環境はUbuntu 20.10です。

まずcabalをインストールするところは変わりません。

```text
$ sudo aptinstall cabal-install make
```

ついでに `libffi` もインストールしておくと便利です。

``` text
$ sudo apt install libffi-dev
```

cabalのバージョンはこうです。

``` text
$ cabal --version
cabal-install version 3.0.0.0
compiled using version 3.0.1.0 of the Cabal library
```

cabalは色々と問題が指摘されてきて、CLIが刷新されています。
Idrisのwikiとは違い、3系ならば `new-` 系コマンドを使うとよさそうです。

``` text
$ cabal new-update
$ cabal new-install idris
```

さきほど `libffi` をインストールした方はFFIを有効にしてビルドしましょう。

``` text
$ cabal new-install -f FFI idris
```

インストールにしばらくかかるので気長に待ちましょう。

完了すれば `~/.cabal/bin/` にidris系コマンドがイントールされます。

``` text
$ ls ~/.cabal/bin
idris  idris-codegen-c  idris-codegen-javascript  idris-codegen-node
```

`~/.caba/bin` にパスを通しておきましょう。

``` text
$ echo 'export PATH=~/.cabal/bin:$PATH' >> ~/.bashrc
$ source ~/.bashrc
```

とかすればよさそうです。

## ソースからビルド

以前ブログに書いたのでそちらを参考にしてみて下さい。

[idris環境構築 | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2019/01/06/idriskankyoukouchiku/)


# 処理系の起動

前回の記事ではコンパイルコマンドを紹介しました。
今一度Hello Worldをコンパイル、実行してみましょう。

``` text
$ cat <<'EOF' > hello.idr
main : IO ()
main = putStrLn "Hello World"
EOF
$ idris hello_world.idr -o hello_world
$ ./hello_world
Hello World
```

Idrisには対話環境もあります。
引数を何も与えずに起動すると対話環境が起動します。

``` text
$ idris
     ____    __     _
    /  _/___/ /____(_)____
    / // __  / ___/ / ___/     Version 1.3.3
  _/ // /_/ / /  / (__  )      https://www.idris-lang.org/
 /___/\__,_/_/  /_/____/       Type :? for help

Idris is free software with ABSOLUTELY NO WARRANTY.
For details type :warranty.
Idris>
```

式を与えたらそのまま評価して返してくれるので簡単に動作を確かめたいときは重宝します。

```text
Idris> 1 + 1
2 : Integer
```

IO系のものは `:exec` で実行できます。

``` text
Idris> :exec putStrLn "Hello REPL"
Hello REPL
```


他にも色々できることはあるのですが、 細かい使い方は別の記事にゆずることにしましょう。

# まとめ

Idrisの処理系のインストール方法を紹介しました。

