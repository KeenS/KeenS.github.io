---
type: post
title: "Common LispでSIGINTを捉える"
date: 2014-08-27
comments: true
categories: [Common Lisp, Lisp]
---
κeenです。もうすぐShibuya.lisp TT #8 ですね。今回はCIMのREPLの改善をしてたらSIGINTを補足しようとして詰まったので共有します。
<!--more-->

結論を書いちゃうと

```lisp
(defvar *interrupt-condition*
  ;; It seems abcl does not raise any conditions
  #+allegro 'excl:interrupt-signal
  #+ccl 'ccl:interrupt-signal-condition
  #+clisp 'system::simple-interrupt-condition
  #+ecl 'ext:interactive-interrupt
  #+sbcl 'sb-sys:interactive-interrupt
  #-(or allegro ccl clisp ecl sbcl) 'no-conditon-known)
  
(defmacro with-handle-interrupt (&body body)
  `(handler-case
       #-ccl(progn
              ,@body)
       #+ccl (let ((ccl:*break-hook* (lambda (condition hook)
                                       (declare (ignore hook))
                                       (error condition))))
               ,@body)
       (#.*interrupt-condition* (c)
         (handle-interrupt c))))
```

こんな感じです。ABCLがコンディションを投げずに即終了してる(?)っぽいのでABCLに対しては無力です。もしかしたらJava側で捉えないといけないのかもしれません。
因みにどうやってそれぞれのコンディション名を調べたかというと、`(handler-case (loop) (condition (c) (print c)))`を各処理系のREPLで実行して`^C`しました。
Allegro CLは`interrupt-signal`がconditionのサブクラスでなかった(`serious-condition`だった)のでちょいと困りましたね。結局マニュアル読みました。

CIMのコードなので上記以外の処理系には対応してないです。

今回詰まったのはCCLですね。`ccl:interrupt-signal-condition`は定義してあるもののユーザーより先に処理系が処理するので無意味でした。なので`ccl:*break-hook*`を使って処理系が捉えるより早くコンディションを発生させました。
