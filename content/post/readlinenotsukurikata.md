---
categories: [Linux, Lisp, Common Lisp]
date: 2016-02-14T22:37:35+09:00
title: readlineの作り方
---
κeenです。なんか伏線っぽいもの回収しといた方が良いかなと思ってLinuxっぽい話でも。
<!--more-->

readlineって便利ですよね。
でもCで書かれているから他の言語から使おうと思うと面倒だったり使えなかったりGPLv3の所為で使えなかったりしますよね。
そこである程度POSIXのインターフェースを扱わせてくれる言語でのreadlineの作り方でも。

# カノニカルモードとエコーモード
readlineの仕組み自体は簡単で、全ての文字入力を受け取って、

* readlineの制御キーシーケンスなら指定の制御を行なう
* 普通の文字ならそのまま画面に表示する
* コントロールシーケンスなら `^W` などとエスケープ表示する

これだけです。しかしそう簡単ではありません。
あなたのお気に入りの言語で `readchar` っぽい関数を実行してみると分かるかと思いますが、

1. 一文字入力しただけではプログラムに入力文字が渡されない。エンターキーを押して初めてプログラムの `readchar` 関数が返る。
2. 入力した文字がそのままエコーバックされる。すなわち、 `←` を押しても `^[[D` が入力されてしまい、制御上不都合。

私は昔はこれらの挙動が適切な関数がないからだと思ってました。しかし、そうではありません。
このような挙動をするのはプログラムの責任でもでもシェルの責任でもなくターミナルの責任です。

ターミナルには多彩なモードがあり、それによって挙動が変わるのです。
つまり、1.の挙動をするのはカノニカルモードの挙動、2.の挙動をするのはエコーモードの挙動なのです。

# `tcgetattr` と `tcsetattr`

さて、ターミナルのモードはプログラム側から変更出来ます。それを行なうのが `tcgetattr(3)` と `tcsetattr(3)` です。
ざっくり言うと `tcgetattr` で現在のターミナルのコンフィグレーションを取得して、それを所望のモードに書き換え、 `tcsetattr` を使って反映出来ます。

今回はカノニカルモードとエコーモードをoffにしたいのですごい雑なコードだとこう書けるでしょう。

``` lisp
(require 'sb-posix)
(let* ((stdin 0)
       (termios (sb-posix:tcgetattr stdin))
       (lflag (sb-posix:termios-lflag termios)))
  (setf lflag (logand lflag (lognot sb-posix:icanon)))
  (setf lflag (logand lflag (lognot sb-posix:echo)))
  (setf (sb-posix:termios-lflag termios) lflag)
  (sb-posix:tcsetattr stdin sb-posix:tcsadrain termios))
```


しかし、これは雑すぎます。なぜならreadlineを実行し終わった後にターミナルを元の状態に復元する必要があるからです。
そうしないとreadlineするつもりのない入力関数が予期せぬ挙動をするでしょう。
あるいはLispプロセスを終了した後のターミナルにまで影響が及びます。
なので **必ず** 処理が終わったらターミナルの状態を復元する必要があります。

さて、Common Lispではこの「必ず」は `unwind-protect` を使った `with-` マクロで実現するのが常套手段です。
コードはこのようになるでしょうか。

``` lisp
(defmacro with-readline-mode (&body body)
  (let ((stdin       (gensym "stdin"))
        (old-termios (gensym "old-termios"))
        (new-termios (gensym "new-termios"))
        (lflag       (gensym "lflag")))
    `(let* ((,stdin 0)
            (,old-termios (sb-posix:tcgetattr ,stdin))
            (,new-termios (sb-posix:tcgetattr ,stdin))
            (,lflag (sb-posix:termios-lflag ,new-termios)))
       (unwind-protect
            (progn
              (setf ,lflag (logand ,lflag (lognot sb-posix:icanon)))
              (setf ,lflag (logand ,lflag (lognot sb-posix:echo)))
              (setf (sb-posix:termios-lflag ,new-termios) ,lflag)
              (sb-posix:tcsetattr ,stdin sb-posix:tcsadrain ,new-termios)
              ,@body)
        (sb-posix:tcsetattr ,stdin sb-posix:tcsadrain ,old-termios)))))
```

これで

``` lisp
(with-readline-mode
  (format t "~a~%" (read-char)))
```

などとするとターミナルのモードが変わったことが実感出来るでしょう。

尚、繰り返すとこれはターミナルの設定の話なのでEmacs内から試そうとしても正常に動作しない筈です。

# コントロールシーケンス

さて、Common Lispの標準にない拡張機能が必要なのはターミナルのモード変更だけで、あとは好き勝手出来るのですが私も少し嵌った部分があるのでそこだけ。

readlineに欲しい機能はなんでしょうか。色々あるかと思いますが、まずは `←` 、 `→` でカーソル移動をしたいのではないでしょうか。
実はこれが大落し穴。

`←` キーを押すとターミナルにはなんと3文字入力されます。すなわち、 Esc文字の `^[` 、普通のASCII文字の `[` 、そして大文字の `D` です。

そしてカーソルの移動文字を受けてカーソルを移動するにはターミナルにそのままEsc文字の `^[` 、普通のASCII文字の `[` 、そして大文字の `D` を入力してあげれば出来ます。

なのでこのようなコードになるでしょう。

```lisp
(defconstant left "[d")
(defconstant right "[c")

(defun left ()
  (format t "~a" left)
  (force-output))

(defun right ()
  (format t "~a" right)
  (force-output))


(defun readline ()
  (let ((line '()))
    (flet ((self-insert (char)
             (format t "~c" char)
             (setf line (cons char line))
             (force-output)))
      (with-readline-mode
       (loop
          (let* ((char (read-char))
                 (code (char-code char)))
            (case code
              ; escape
              ((#b11011)
               (let* ((char (read-char))
                      (code (char-code char)))
                 ;; (write-line "called")
                 ;; (format t "~a~%" char)
                 ;; (format t "~a~%" code)
                 (case char
                   ((#\[)
                    (let* ((char (read-char))
                           (code (char-code char)))
                      (case char
                        ((#\c) (right))
                        ((#\d) (left)))))
                   ((t)
                    (self-insert #\escape)
                    (self-insert char)))))
              ((#b1010)
               (return-from readline (coerce (reverse line) 'string)))
              (t (self-insert char)))))))))
```

`case` をネストしていくのはダルいし拡張性がないので現実的にはtrie木を使うことになるかと思いますが単純にはこのようなコードでreadlineを作れます。

Let's Happy Hacκking!

# 付録A: 伏線

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">適当に魚と野菜炒めたらオサレっぽくなった <a href="https://t.co/9IIzhIGjzd">pic.twitter.com/9IIzhIGjzd</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/698821538686984192">2016, 2月 14</a></blockquote>

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> 魚は何？タラ？だったら季節物だし、旬の鱈と野菜のソテー、詳解Linuxカーネルを添えて。と命名しよう</p>&mdash; Shinnosuke Takeda (@cnosuke) <a href="https://twitter.com/cnosuke/status/698824048319410176">2016, 2月 14</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>
# 付録B: Pure Rubyなreadlineのソースコード
努力が必要ということが読み解ければ幸いです。

[rb-readline/rbreadline.rb at master · ConnorAtherton/rb-readline](https://github.com/ConnorAtherton/rb-readline/blob/master/lib/rbreadline.rb)
