---
type: post
title: "Shellyを使ってCommon LispファイルをExecutableにする"
date: 2013-09-26
comments: true
sharing: true
categories: [Common Lisp, Lisp, Shelly, Command Line]
---
コマンドラインからCommon Lispの関数を実行できる [Shelly](https://github.com/fukamachi/shelly)を使ってCommon Lispファイルを実行形式にしてみました。Shellyは

    $ shly + 1 2
    3

みたいに関数を実行できます。

<!--more-->

モチベーションとしては、 [Shellyの作者深町さん](http://blog.8arrow.org/entry/20120521/1337596483)がTwitterで「Shellyでloadを使うとスクリプトみたいに実行できる」と発言していたのがきっかけで、Shebangを無視できればUNIX系ではExecutableにできるのでは？と思いついてやってみました。

`read`して`eval`して…とか考えてましたが、`load`に`stream`を渡せるようなので案外簡単にできました。

<script src="https://gist.github.com/KeenS/6688683.js?file=script.lisp"></script><noscript><pre><code>(in-package :shelly)
(export (defvar *argv* nil))
(in-package :cl-user)

(defun script (file argv)
  "Execute a file as script ignoring shebang"
  (setf shelly:*argv* argv)
  (let* ((in (open file :if-does-not-exist :error))
     (first-char (read-char in))
     (second-char (read-char in)))
    (cond
      ((and (char= first-char #\#) (char= second-char #\!))
       (read-line in))
      (t (unread-char second-char in)
     (unread-char first-char in)))
    (load in)
    (values)))
</code></pre></noscript>

コマンドライン引数を受け取るために`*argv*`という変数を用意してます。

こんなことを`~/.shelly/Shellyfile`に書いておき、Lispの初期化ファイルに

    #+shelly
    (load "~/.shelly/Shellyfile")

と書いてます。この`~/.shelly/Shellyfile`にはShelly向けの関数を溜め込んでいく予定です。

あとは

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
</pre></td>
<td class="code"><pre><code class="common-lisp"><span class="line"><span class="err">#</span><span class="nv">!shly</span> <span class="nv">script</span>
</span><span class="line">
</span><span class="line"><span class="p">(</span><span class="nb">defun</span> <span class="nv">hello</span> <span class="p">()</span>
</span><span class="line"> <span class="p">(</span><span class="nb">format</span> <span class="no">t</span> <span class="s">"Hello ~a~%"</span> <span class="p">(</span><span class="nb">first</span> <span class="nv">shelly:*argv*</span><span class="p">)))</span>
</span><span class="line"><span class="p">(</span><span class="nv">hello</span><span class="p">)</span>
</span></code></pre></td>
</tr></table></div></figure>

を`foo.lisp`に書き込めば

    $ chmod u+x foo.lisp
    $./foo.lisp κeen
    Hello κeen

と実行できます。勿論、

    $shly script foo.lisp κeen

としても同じです。一部の処理系ではこのようなことが出来ますが、コマンドライン引数の渡し方とかも処理系依存なのでポータブルに書くなら一つ試してみてはいかがでしょうか。


