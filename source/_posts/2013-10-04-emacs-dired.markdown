---
layout: post
title: "Emacsのdired-modeを使い易くする"
date: 2013-10-04 19:51
comments: true
sharing: true
categories: [Emacs, Emacs Lisp, Lisp]
---
Emacsの標準ライブラリ`dired.el`を読んでたら色々発見があったのでメモ兼dired-modeとはなんじゃらほいって方にも一から解説してみます。

<!-- more -->

とは言ってもいきなり解説しなくて申し訳ないのですが、最初に

    (ffap-bindings)

を設定しておきましょう。`C-x C-f`が非常に便利になります。だいたいの方が設定してるかと思いますが、まだという方は`init.el`に書いて保存し、最後の`)`の後にカーソルを持っていって`C-x C-e`しましょう。設定が反映されます。この設定で`C-x C-f`の入力がファイル以外も受け付けてくれますし、カーソルにポイントされてるファイルやURLをデフォルトで開こうとしてくれます。

…よし。じゃあ解説始めますよ。`C-x C-f ~/`してみて下さい。上の設定をしていたらディレクトリでも開ける筈です。するとホームディレクトリが`ls -al`された感じのバッファが出てくると思います。

![私のホームはちょっとマズいのがあったので.emacs.dのスクリーンショット](/images/Screenshot_from_2013-10-04.png "dired-mode-screenshot")

これが`dired-mode`です。これは普通に`n`(`C-n`でも可)や`p`(`C-p`でも可)で移動して、Enterでそのファイルやディレクトリを開けます。これだけでも十分便利なのですが、ここからさらにファイル操作までできます。

適当なファイルにカーソルを合わせて`C`するとコピー先を聞かれるので、入力してあげるとコピーしてくれます。複数ファイルをコピーしたいなら、コピーしたいファイル上で`m`してマークを付けて、`C`すれば一括でコピーできます。

勿論、他にもコマンドはあります。同じような使い方のできる代表的なものを挙げると、

| コマンド | 操作(実行されるLinuxコマンド)
|:--------|:-----------------------------
| `C`     | コピー(`cp`)
| `R`     | リネーム/移動 (`mv`)
| `D`     | 削除(`rm`)
| `M`     | 権限操作(`chmod`)
| `G`     | グループ変更(`chgrp`)
| `O`     | 所有権変更(`chown`)
| `Z`     | 圧縮/展開(`compress/uncompress/gunzip/bunzip/unzip`)
| `B`     | (*.elファイルの)バイトコンパイル
| `!`     | 任意のコマンドの実行。デフォルトで空気読んだコマンドになってる。

他にも無数に操作はありますがファイル操作はこんなものでしょう。あとはバッファを閉じる`q`や親ディレクトリに移動する`^`、バッファの更新を行なう`g`、マークを外す`u`(`U`で全解除)を覚えていれば操作には困らない筈です。

が、頻繁に使っているとより良く使いたくなります。そこで今回見付けた設定を紹介します。

まず

    (add-hook 'dired-load-hook (lambda ()
                      (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))

から。これはdired-modeで`r`するとバッファがエディタブルになり、ファイル名を普通に変更できます。そのまま`C-x C-s`すればリネームの終了です。  
普通にEmacsの編集コマンドが使えるので `M-%`とかで一括拡張子変更とか`C-x r t`でファイル名にプリフィクスつけたり自由自在です。

次に

    (setq dired-listing-switches (purecopy "-Ahl"))

Emacsがdired-modeを表示するとき、実際に裏で `ls -al`を走らせているのですが、そのときのオプションを変更できます。但し内部で`-l`の結果を利用してるのでこれは必須です。しかしそれ以外は自由に設定できます。因みにWindowsでもEmacsがエミュレートしてくれるので設定可能ですが、一部未実装のものもあるそうです。

頻繁に使うなら

    (setq dired-dwim-target t)

もお勧めです。

これを設定しておくと、ウィンドウを分割して左右にdiredバッファを開いているとき、`R`や`C`のデフォルトの宛先がもう片方のディレクトリになります。伝わりますかね？

![分割した両方のWindowでdired-modeを実行した状態。](/images/Screenshot_from_2013-10-05.png "dired-mode in both of split window")

のようにdiredでディレクトリA, Bを開いてるときにディレクトリAでファイルのコピーやら移動やらをするときにデフォルトでBに移動やコピーを行なうようになります。いくつかのファイルをディレクトリ間で移動させるときに非常に便利ですね。

あ、`C-x C-f /ftp:user@ftp-server:/path/to/dir`でFTPサーバーのディレクトリ/ファイルにもアクセスできるのは御存じですよね？これと組合せるとEmacsが割と使い易いFTPクライアントに大変身!

あとは私がどうしても欲しかったtarballですね。

<script src="https://gist.github.com/KeenS/6828197.js?file=dired-tar.el"></script><noscript><pre><code>(defun dired-tar (tarname files &amp;optional arg)
  "A dired-mode extension to archive files marked. With prefix argument, the tarball is gziped."
  (interactive (let ((files (dired-get-marked-files)))

<pre><code> (list (read-string &amp;quot;Tarball name: &amp;quot; (concat (file-relative-name (car files)) &amp;quot;.tar.gz&amp;quot;))
           files &amp;quot;P&amp;quot;)))
</code></pre>

<p> (let ((tar (if arg</p>

<pre><code> (if dired-guess-shell-gnutar
         (concat dired-guess-shell-gnutar &amp;quot; zcf %s %s&amp;quot;)
       &amp;quot;tar cf - %2s | gzip &amp;gt; %1s&amp;quot;)
       &amp;quot;tar cf %s %s&amp;quot;)))
(shell-command (format tar tarname (mapconcat 'file-relative-name files &amp;quot; &amp;quot;)))))
</code></pre>

<p>(add-hook ‘dired-load-hook (lambda () (define-key dired-mode-map "T" ‘dired-tar)))
</p></code></pre></noscript>

これでtarballにまとめたいファイルをマークして`T`すればまとめてくれます。`C-u T`するとgzip圧縮までしてくれます。因みに解凍/展開はファイル上で`!`するとデフォルトコマンドが展開(`tar xzvf`など)になってるので専用コマンドは必要ありません。補足ですが個人用なのでテストとかしてません。一旦御自身で動作テストなどをされてから実行されることをお勧めします。

他にも色々なコマンドがたくさんあったので興味のある方は探求してみて下さい。


