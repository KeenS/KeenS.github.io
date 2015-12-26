---
categories: [ML, SML, SMLSharp, Advent Calendar Memorial 2015, Advent Calendar Memorial]
date: 2015-12-26T19:48:30+09:00
title: SML#のオレオレREPLを作る話 初級
---
κeenです。Advent Calendarのためにネタやアイディアを用意したものの時間/場所的都合でAdvent Calendarとして出せなかったボツネタでも供養しようかと。
Advent Calendarが終わってしまったので投げやり気味ですね。
第2段はSML#のオレオレREPLを作る話の初級。

SML#にはFFIがあり、任意のオブジェクトファイル/ライブラリをリンクしてC関数を呼び出すことが出来ますが、REPLで試そうとしてもREPLにリンクされていないので呼び出せません。
そこで呼び出したいオブジェクトファイルをリンクしたREPLを作れば捗るよね、という発想です。
<!--more-->

# 事前に必要なもの

* SML#のビルド時に使ったLLVMのパス
* SML#をビルドした時のオブジェクトファイルの残骸（一部）
* SML#のソーコード（一部）

# コマンド
まず、普通のREPLを作るだけならこれだけです。（コンパイラも一緒にくっついてきますが面倒なのでそのままにしてます。）

```sh
SMLSHARP_SRC=/path/to/smlsharp
LLVM_HOME=/path/to/LLVM
cp ${SMLSHARP_SRC}/src/compiler/smlsharp.sml ./
cat <<'EOF' > smlsharp.smi
_require "prelude.smi"

_require "compiler/main/main/SimpleMain.smi"
EOF
smlsharp -c++ -o oreore_repl "${SMLSHARP_SRC}/src/compiler/smlsharp.smi"  "${SMLSHARP_SRC}"/src/llvm/main/llvm_support.o "${SMLSHARP_SRC}"/src/llvm/main/SMLSharpGC.o $("${LLVM_HOME}/bin/llvm-config" --ldflags --libs)
```

あとは最後のリンクコマンドにフラグを足せばいいでしょう。注意点として、64bit OSを使ってるならちゃんと32bitでビルドしたライブラリをリンクしましょう。例えばlibuvをリンクするならこうです。


``` sh
SMLSHARP_SRC=/path/to/smlsharp
LLVM_HOME=/path/to/LLVM
cp ${SMLSHARP_SRC}/src/compiler/smlsharp.sml ./
cat <<'EOF' > smlsharp.smi
_require "prelude.smi"

_require "compiler/main/main/SimpleMain.smi"
EOF
smlsharp -c++ -o smlsharp_uv "${SMLSHARP_SRC}/src/compiler/smlsharp.smi" -L /opt/libuv32/lib -luv "${SMLSHARP_SRC}"/src/llvm/main/llvm_support.o "${SMLSHARP_SRC}"/src/llvm/main/SMLSharpGC.o $("${LLVM_HOME}/bin/llvm-config" --ldflags --libs)
```

# 終わりに

やり方さえ分かってしまえば簡単ですが見付けるのが難しい小ネタでした。因みに今回初級ですが、REPLにlibeditを統合するなどの上級ネタはいつか気が向いたら書くかもしれません。
