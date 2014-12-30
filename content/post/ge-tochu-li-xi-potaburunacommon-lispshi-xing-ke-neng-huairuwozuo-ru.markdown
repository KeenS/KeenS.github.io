---
type: post
title: "割と処理系ポータブルなCommon Lisp実行可能ファイルを作る"
date: 2014-12-08
comments: true
categories: [Lisp, Common Lisp, Shell, Command Line]
---
κeenです。Lisp Advent Calendarはもう枠埋まっちゃったので普通にブログで。

コマンドラインから実行可能なLispファイルをそれなりに多くの処理系で動くように作る話。
<!--more-->

この話はCIMの生い立ちとも関連するんだけどシェルからLispを使いたいときは

```
#!/usr/bin/env sbcl --script

...
```

なんて書いてた人も多いんじゃないかと思うんだけどこれは色々問題がある。

1. envは環境に依っては/usr/local/bin/envだったりする
2. envは環境に依っては複数引数を取れない。"sbcl --script"という名前のファイルを捜しにいく
3. sbclでしか動かない

この問題の扱いは一応解決策がある

```
#!/bin/sh

#|
exec sbcl --script "$0" -- "$@"
|#

...
```

3行目の`#|`がシェルのコメントでありLispのブロックコメントであるのがポイント。

1. シェルは`#!/bin/sh`を見てシェルスクリプトとして実行する
2. 1,3行目はコメントなのでシェルは無視
3. 4行目でsbclにそのファイルを引数として与えてexecする。execしたあとはシェルには戻らないのでその後何が書いてあっても構わない
4. sbclを`--script`付きで読んでるので1行目の`#!`で始まる行は無視する
5. 3~5行目はブロックコメントなのでsbclは無視
6. それ以降がLispとして実行される

というカラクリになっている。こういうのをピジン言語っていうんだけ？

まあいいや。

ところでこのブロックコメントの中にはシェルスクリプト書き放題だよね？そのシェルスクリプト内でどのLisp使うか決めたらポータブルになりそうじゃない？

こういうのはどう？

```lisp
#!/bin/sh

#|
run_if_exists(){
 command -v $1 > /dev/null 2>&1 && exec "$@"
}
 run_if_exists sbcl --noinform --no-sysinit --no-userinit --script "$0" -- "$@"
 run_if_exists clisp -norc --quiet --silent -on-error exit  "$0" -- "$@"
 run_if_exists ecl -norc -q -shell "$0" -- "$@"
 run_if_exists mkcl -norc -q -shell "$0" -- "$@"
 run_if_exists alisp -qq -#! "$0" -- "$@"
|#

(write-line (lisp-implementation-type))
(force-output)
```

cclとabclは1行目のシェバンを読み飛ばせなかった&amp;評価結果をエコーバックしない方法が見当らなかったからパス。CMUCLは自分の環境で動かないから検証出来てない。

これでポータブルに実行は可能。コマンドライン引数の扱いとかはライブラリを頼ってくれ。因みにCIMでは

```lisp
  #+allegro  (cdr (system:command-line-arguments))
  #+sbcl (do*  ((var sb-ext:*posix-argv* (cdr list))
                (list var var))
               ((string= (car list) "--") (return (cdr list))))
  #+clisp ext:*args*
  #+ecl (do*  ((var (si:command-args) (cdr list))
               (list var var))
              ((string= (car list) "--") (return (cdr list))))
  #+abcl extensions:*command-line-argument-list*
  #+gcl (do*  ((var si::*command-args* (cdr list))
               (list var var))
              ((string= (car list) "--") (return (cdr list))))
  #+cmu ext:*command-line-words*
  #+ccl ccl:*unprocessed-command-line-arguments*
  #+mkcl (do*  ((var (si:command-args) (cdr list))
               (list var var))
              ((string= (car list) "--") (return (cdr list))))
  #+lispworks system:*line-arguments-list*
```

としている。参考までに。
