---
categories: [AsciiDoc, CLI, Document]
date: 2015-01-24T10:58:29Z
title: AsciiDocを使ってお手軽manページ生成
---
コマンドラインツールを作ってるみなさん、man書いてますか？コマンドラインツールを使う人は時代の変化に取り残された遺物なのでWebにドキュメント置いても読んでくれませんよ。

いや、私のようにmanだけを頼りにツールを使う人もいるのでちゃんと書いて下さいね。面倒だって?AsciiDocを使えば簡単に書けますよ!
<!--more-->
# Manとは

    man hoge

で見れるやつですね。

## troff
manはtroffというフォーマットで書く必要があります。どんな感じかというと

```troff
.ie n \{\
\h'-04'\(bu\h'+03'\c
.\}
.el \{\
.sp -1
.IP \(bu 2.3
.\}
use the latest sbcl
.RE
.sp
.if n \{\
.RS 4
.\}
```

はい。人間が書くものではないですね。なので別の形式(AsciiDoc)から変換することを考えます。

## セクションナンバー
コマンド（など）はセクションに分かれています。manを書くときに必要になる知識なので覚えておきましょう。コマンドラインから使うものの他、Cの関数やコンフィグファイルについての項目もあります。

|   | Description                        |
|---|:-----------------------------------|
| 1 | 一般コマンド                         |
| 2 | システムコール                        |
| 3 | ライブラリ関数                        |
| 4 | デバイス                             |
| 5 | ファイルフォーマット（コンフィグファイル） |
| 6 | ゲーム                              |
| 7 | その他                              |
| 8 | システムメンテナンス(sudoとか)          |
| 9 | カーネル                             |

一般デベロッパが使うのは1、5、6、8あたりでしょうか。6のゲームには普通のゲームの他、phase of moon、盈虚(えいきょ、月齢のこと)を表示する`pom`コマンドなどがあります。

manを書くときにはmanを必ずどこかのセクションに割り当てます。

manを使う時は`man hoge`だと全てのセクションのhogeのmanを捜しますが`man 1 hoge`だとセクション1のみから捜します。割とセクション1と2で同名のmanがあることがあるので重宝します。また、そのような混同を防ぐためにコマンドやシステムコールの後には括弧書きでセクションナンバーを書くのが慣習です。manならman(1)のように。

因みに`man n intro`とするとセクションナンバーnの説明が見れます。

# AsciiDoc
[AsciiDoc Home Page](http://www.methods.co.nz/asciidoc/)
MarkdownとかrSTとかの類いです。リーダブルなAsciiなDocを書いて色々な形式に変換出来ます。HTMLやDocBookなどの形式に変換出来ます。普通にそのままのテキストでも十分リーダブルだと思います。拡張子は.txtを使います。そのままでも読めるんだぜオーラが出てますね。

有名どころだとgitが使ってるとか。バッククォートをあまり使わないので個人的にはリーダブルさが他より高い気がします。

[ここ](http://powerman.name/doc/asciidoc)にチートシートがあるのでだいたいはそこを見てもらえば良いのですが、例を出すと

```asciidoc
TITLE
=====
keen mail@address
0.0.1, 2015-01-24

Header
------
[source, lisp]
(write-line "Hello, World")
```

な感じです。
タイトル、=で下線のあとにファイルの属性がきます。Author、Mail、Revision、Dateはよく使うのでこのようにリーダブルに書けます。他の属性は

```
:attr: value
```

のように書く必要があります。Authorなどもこのように書いても構いません。

独特なのがパラグラフ毎にマークアップしていくところですね。パラグラフの区切は空行です。上の

```asciidoc
[source, lisp]
(write-line "Hello, World")
```

のようにパラグラフの前にマークアップコマンドを置けます。1パラグラフに収まらないものは例えばソースコードなら

```asciidoc
----
(defun hello (name)
  (format t "Hello, ~a~%" name))

(hello "κeen")
----
```

のように`-`4つで囲む、などの記法もあります。

# manのフォーマット
さて、話戻ってmanのフォーマットです。みなさん見慣れてるかと思いますが、manは最初はNAME、SYNOPSIS、DESCRIPTIONのセクションが並ぶことを要求します。

# AsciiDocからman生成
manが一定のフォーマットを要求するのでAsciiDoc側も一定のフォーマットで書いてやる必要があります。

NAME、SYNOPSIS、DESCRIPTIONは勿論のこと、タイトルが「コマンド名(n)」となっている必要があります。コマンド名にはスペースが入ってはいけません。gitのようにサブコマンドに分かれているものはハイフンで繋ぐようです。nはセクションナンバーですね。

NAMEについて捕捉しておくと、NAMEの書式も決まっていて、

    command-name - one line description

のフォーマットである必要があります。`apropos(1)`で表示するためでしょうね、きっと。

## 例
実際に私が書いたものを載せますね。


```asciidoc
cim-use(1)
==========
keen(@blackenedgold) 3han5chou7@gmail.com
:man manual: CIM Manual

NAME
----
cim-use -  Use specified impl as 'cl' command's backend.

SYNOPSIS
--------
[verse]
cim use <impl[-version]> [--default]

DESCRIPTION
-----------

Use <impl> as background for cl command. It also affects bare lisp command.
If version is not given, use latest version.
If --default is given, use the specified implementation at initial state.

EXAMPLES
--------
* use the latest sbcl
----
$ cim use sbcl
$ sbcl --version
SBCL 1.1.14
----

* use old sbcl
----
$ cim use sbcl-1.1.10
$ sbcl --version
SBCL 1.1.10
----

* use ccl-1.9 and set it default
----
$ cim use ccl-1.9 --default
----

```

:man manual:の属性はヘッダに表示されるものです。

んで変換は

    a2x  --doctype manpage --format manpage filename.txt

。

これ、ファイル名に関らずcim-use.1というファイルを生成します。

んでファイルの内容は

    man ./cim-use.1

で見れます。

```
CIM-USE(1)			  CIM Manual			    CIM-USE(1)



NAME
       cim-use - Use specified impl as 'cl' command's backend.

SYNOPSIS
       cim use <impl[-version]>	[--default]

DESCRIPTION
       Use <impl> as background	for cl command.	It also	affects	bare lisp
       command.	If version is not given, use latest version. If	--default is
       given, use the specified	implementation at initial state.

EXAMPLES
       o   use the latest sbcl

	   $ cim use sbcl
	   $ sbcl --version
	   SBCL	1.1.14

       o   use old sbcl

	   $ cim use sbcl-1.1.10
	   $ sbcl --version
	   SBCL	1.1.10

       o   use ccl-1.9 and set it default

	   $ cim use ccl-1.9 --default

AUTHOR
       keen(@blackenedgold) 3han5chou7@gmail.com
	   Author.



				  01/21/2015			    CIM-USE(1)

```

他の例だと、[gitのドキュメント](https://github.com/git/git/tree/master/Documentation)を見るとよさそうです。あそこはWEB用にもビルドしてるのでAsciiDocのコンフィグ書いてWEBとmanで条件分岐するマクロとかも書いてます。変態ですね。

# ビルド
私は以下のようなスクリプトを書いてビルドしてます。並列ビルド、タイムスタンプセンシティブビルド対応。DOC_ROOTはデファルトでそのスクリプトが置いてあるディレクトリです。環境変数で制御出来ます。make.shって名前で保存したなら

    $ DOC_ROOT=your/root ./make.sh

のように。その他AUTO_POLLなども設定出来ます。必要ならお使い下さい。[CIM](https://github.com/KeenS/CIM)の配布物に含まれるのでBSDライセンスです。

```sh
#!/bin/sh

: ${DOC_ROOT:=$(cd $(dirname $0); pwd)}
: ${MAN_DIR:=${DOC_ROOT}/man/man1}
: ${AUTO_POLL:=false}
: ${POLL_INTERVAL:=5}

txt2man(){
    if [ ! -e "${MAN_DIR}/$(basename $1 .txt).1" ] || [ "$1" -nt "${MAN_DIR}/$(basename $1 .txt).1" ]; then
        echo "Building $1"
        if a2x -v  --doctype manpage --format manpage -D "${MAN_DIR}" "$1" > "log/$(basename $1 .txt).log" 2>&1
        then
            echo "O Built $1"
        else
            echo "X Bulid failed: $1. See log/$(basename $1 .txt).log"
            return 1
        fi
    fi
}

cd "${DOC_ROOT}"
while "${AUTO_POLL}"; do
      for f in *.txt; do
          txt2man "$f" &
      done

      wait
      if  "${AUTO_POLL}"; then
          sleep "${POLL_INTERVAL}"
      fi
done
```

# 運用
`man`はMANPATH環境変数を元にmanを捜します。捜すのはMANPATH直下ではなく、セクションナンバー1なら/man/path/man1/name.1を捜します。それっぽいところに置きましょう。

因みにFreeBSDではちょっと面倒です。詳しくは`manpath(1)`を見て下さい。

# まとめ
* manpageは重要
* だけど手で書くものではない
* AsciiDoc使うと便利!
* 便利なスクリプト用意しといたよ

