---
categories: [量子コンピュータ]
date: 2018-05-01T23:45:26+09:00
title: "量子コンピュータに入門してる"
---
κeenです。最近アウトプットしてないなーと思ってなんか書きます。アウトプットといってもエッセイですが。
ざっくり[量子コンピュータと量子通信](https://www.amazon.co.jp/dp/4274200086)の4章の感想みたいな。
私の[2018年注目していきたい技術](https://keens.github.io/blog/2018/01/04/2018nenchuumokushiteikitaigijutsu/)に入ってるのでやってます。
<!--more-->

# 雑感

量子コンピュータと量子通信はI、II、IIIに分かれてて、4章はII巻の最初の章。なんでいきなりII巻を読んでるかというとどこかでおすすめされてるのを見たからです。
正直Iから読んだ方が良かった気がします。
II巻の全体を追うには[量子コンピュータ Advent Calendar 2017](https://qiita.com/advent-calendar/2017/quantum)のどこかの記事にある導入くらいを把握してれば済むのですが、演習問題などで詰まります。
I巻の結果を前提としていたりI巻の章の内容を受けて書かれていたりするのでI巻を持ってないとダメですね。

ところで、この本は演習問題を解くことを推奨していて、演習が本文の合間にあります。ところがその量が多く、4章だけで51問あります。最初の方は演習問題の合間に本文が書いてあるくらいの密度ですね。
はじめは割と頑張って解いてたのですが先程の前提知識の問題や普通に私の能力の問題で全然解けないこともあり、早々にあきらめてしまいました。たとえば「Hadamardゲートを$R_z$回転、$R_x$回転、およびある$\varphi$に対する$e^{i \varphi}$の積として表わせ(演習4.4)」という問題で1日使ったりと。
しかしまあ読み進めていくと問題密度も減っているようなので気合を入れ直して全部解いた方がいいかもしれませんね。

# 面白いと思ったものとか
と、愚痴めいたことはおいといて興味深い結果を書きます。私は量子コンピュータ言語向けのコンパイラに興味があるので「ユーザが好き勝手書いた高級な表現をどう低級に表現するか」「どうやってゲート数を削減するか」「どのようなプリミティブがあるとユーザは書きやすいか」などが気になります。


## ゲートの結合

シンプルな例ですが

\\\[
XY = iZ; YZ = iX; ZX = iY
\\\]

や

(演習4.13)

\\\[
HXH = Z; HYH = -Y; HZH = X
\\\]



などいくつかの等式が成り立ちます。これらを用いてゲート数の削減が出来るでしょう。

## CNOT基底変換

[この記事](http://lyncs.hateblo.jp/entry/2017/12/16/000103)によると今の量子コンピュータ(IBM qx4)ではCNOTを取れる方向が決まっているようです。q1からq2は取れるけどq2からq1は取れないだとか。
そういうのはコンパイラが頑張って制約問題を解くのかなーと思ってました。しかしどうやらCNOT基底変換を使うとCNOTの方向をソフトウェア的に逆転できるようです。

CNOT基底変換はこのようなものです。

<!--
	qubit	q0
	qubit	q1
    h	q0
	h	q1
	cnot	q0,q1
	h	q0
	h	q1
-->


![CNOT基底変換の量子回路図](/images/qcircs/cnot_conv.png)

これを計算するとCNOTのコントロールとターゲットを反転したものに等しいです(演習 4.20)。

<!--
	qubit	q0
	qubit	q1
	cnot	q1,q0
-->

![反転CNOTの量子回路図](/images/qcircs/cnot_rev.png)

色々工夫があるんですね。

## ユニタリ行列と近似
ハードウェアに実装されるゲートは有限種類ですが量子計算に使うユニタリ行列は連続値を取り無限にあるので両者の間にギャップがあります。
なのでゲートを何回も適用してユニタリ行列を近似する必要があります。もちろん、ハードウェアに実装されるゲートは任意のユニタリ行列を近似できる(普遍性がある)ように設計されます。

これは言語的にはどうなるのかなーと考えてみたところ、古典コンピュータでいうルンゲ=クッタ法で任意の微分可能関数を近似できるというのに似てるんですかね。
なので三角関数とかと同じ扱いで、いくつかの重要なユニタリ行列がライブラリで実装されるのかな？

一歩くらい量子コンピュータ言語に近寄れた。


# 余談
量子回路を載せるために[qasm2png](https://github.com/eschmidgall/qasm2circ/blob/master/original_qasm2circ-v1.4/qasm2png)というのを用いたのですがcshで書かれている上に起動ディレクトリを動かせなくて不便だったのでbshで書き直しました。やっつけで作ったので荒い部分もありますがもとのcshスクリプトよりはマシです。
[qasm2circ](https://github.com/eschmidgall/qasm2circ)をダウンロードし、 `QASM_DIR`をご自身のものに変えてお使い下さい。



``` shell
#!/bin/sh
# templated by http://qiita.com/blackenedgold/items/c9e60e089974392878c8
usage() {
    cat <<HELP
NAME:
   $0 -- convert qasm to png

SYNOPSIS:
  $0 FILE
  $0 [-h|--help]
  $0 [--verbose]

DESCRIPTION:
   convert qasm to png. FILE name must end with .qasm

  -h  --help      Print this help.
      --verbose   Enables verbose mode.

EXAMPLE:
  $ $0 foo.qasm

HELP
}

main() {
    SCRIPT_DIR="$(cd $(dirname "$0"); pwd)"

    while [ $# -gt 0 ]; do
        case "$1" in
            --help) usage; exit 0;;
            --verbose) set -x; shift;;
            --) shift; break;;
            -*)
                OPTIND=1
                while getopts h OPT "$1"; do
                    case "$OPT" in
                        h) usage; exit 0;;
                    esac
                done
                shift
                ;;
            *) break;;
        esac
    done
    set -e

    QASM_DIR=/your/path/to/original_qasm2circ

    source_dir="$(dirname $1)"
    file_base="$(basename "$1" .qasm)"

    work_dir="$(mktemp -d)"
    base="$work_dir/$file_base"
    work_file="${base}.qasm"

    trap "exit 1" HUP INT PIPE QUIT TERM
    trap "rm -rf $work_dir" EXIT

    cp "$1" "$work_dir"

    python "$QASM_DIR/qasm2tex.py" "$work_file" > "${base}.tex"
    (cd "$QASM_DIR"; latex --output-directory="$work_dir" "${base}.tex")
    dvips -D2400 -E -o "${base}.eps" "${base}.dvi"
    gs -sDEVICE=pnmraw -r400 -dNOPAUSE -sOutputFile="${base}.pbm" "${base}.eps" -c quit
    pnmcrop "${base}.pbm" | pnmtopng > "${base}.png"

    cp "${base}.png" "$source_dir"
    rm -rf "$work_dir"
}

main "$@"

```



tex(ubuntuでいうtexliveとtexlive-pictures)依存なのでなんかさくっと作り直したいですねー。
