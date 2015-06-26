---
categories: [Command Line, Shell Script, Lisp, Common Lisp]
date: 2015-06-26T23:24:44+09:00
title: あなたの知らないShebang
---
κeenです。最近は何故かBlack君って呼ばれます。Shebangの書き方にはいくつかあって、それを利用したふと面白い方法を思い付いたので共有を。
<!--more-->

さて、ご存知Shebangといえば

```sh
#!/bin/sh
...
```

のようにファイルの1行目が`#!`から始まっているとシェルがそれ以降の文字列を実行可能ファイルのパス名として捜して実行してくれるものですが、実は`#!`は唯一のフォーマットではありません。

`#!`がデファクトになる前なのかそもそもシェルにコメントがなかった時代のものなのかは知りませんが昔は

```sh
: /bin/sh
...
```

と`:`で始めていたらしいです。因みに`:`は「何もしないコマンド」です。実質的にコメントとして扱ったり副作用のある変数展開だけを行なったりプログラマティックコメントアウトだったりの用途で使われてます。

さて、シェルは全て文字列なのでクォートしてもしなくても構いません。

```sh
":" "/bin/sh"
```

としてもちゃんと動いてくれます。なぜわざわざクォートするかというと[割と処理系ポータブルなCommon Lisp実行可能ファイルを作る | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2014/12/08/ge-tochu-li-xi-potaburunacommon-lispshi-xing-ke-neng-huairuwozuo-ru/)のように別の言語のスクリプトとして実行される時に文字列リテラルになってくれると単純に無視されるので互換性が高まるのです。

ということで完全版ポータブルなCommon Lisp実行可能ファイルはこうなります。


```lisp
":" "/bin/sh"

#|
run_if_exists(){
 command -v $1 > /dev/null 2>&1 && exec "$@"
}
 run_if_exists cl "$0" "$@"
 run_if_exists sbcl --noinform --no-sysinit --no-userinit --script "$0" -- "$@"
 run_if_exists clisp -norc --quiet --silent -on-error exit  "$0" -- "$@"
 run_if_exists ecl -norc -q -shell "$0" -- "$@"
 run_if_exists mkcl -norc -q -shell "$0" -- "$@"
 run_if_exists alisp -qq -#! "$0" -- "$@"
 run_if_exists ccl --no-init --quiet --batch --load "$0"  --eval '(quit)' -- "$@"
 run_if_exists abcl --noinform --noinit --nosystem --batch --load "$0" -- "$@"
 run_if_exists lisp -quiet -noinit -batch -load "$0" -eval '(quit)' -- "$@"
 echo "No lisp implementation found"
 exit 1
|#

(write-line (lisp-implementation-type))
(force-output)

```

abclとcclが利用可能になりました。あとCIMも捜すようにしました。roswellは作者に訊いて下さい。


ちなみに、shebangの解釈はシェルに依存するのですが、B Shell, csh, tcsh, dash, Bash, zshで動作確認しました。古い機能なので新しいシェルほど切り捨てている可能性があったのですがBashやzshが大丈夫だったので良かったです。
一応非推奨な気がしますがこれしか方法がないので仕方ないですね。


ということでみなさんスクリプト書きましょう。
