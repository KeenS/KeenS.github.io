---
layout: post
title: "Gitあれこれ"
date: 2013-09-18 15:13
comments: true
sharing: true
categories: [Git, Github, 備忘録]
---
githubで共同作業してたらいろいろ困ったことがあったのでそのときのメモ。はじめてやって困ったことなので経験ある人には常識なのかもしれません。

<!-- more -->
## マスターリポジトリに追従する

マスターをフォークして自分のレポジトリで作業、終ったらマスターにpull-reqという流れで作業してました。するとマスターが変更されたときに自分のリポジトリに反映させる必要があります。そのときは [http://qiita.com/xtetsuji/items/555a1ef19ed21ee42873](http://qiita.com/xtetsuji/items/555a1ef19ed21ee42873)を参考に、

    $ git remote add upstream git@github.com:g000001/google-common-lisp-style-guide-ja.git
    $ git fetch upstream
    $ git merge upstream/master

でできました。

## Pull-Reqを正しく送る

まず、masterでpull-req送るととりこまれるまで自分のマスターへのコミットが反映され続けるんですね。それで一回失敗。

じゃあ、ということでブランチを切ることに。するとまた問題が。まあ、自分がアホっていったらアホなんですけど

    _A_ _B_ _C_
    *_/ ___\A'/___ \B'/___\C'_HEAD

みたいなブランチの作り方してしまいました。`A`,`B`,`C`それぞれでpull-req送ったんですけど当然

    A : A
    B : B-A'
    C : C-B'-A'

みたいなヒストリのまま送られるわけです。`A`,`B`,`C`は同じファイルの違う箇所への変更なのでヒストリを

    _C_
          /_B_
         /_A_ 
    *_/ ______ HEAD

のようにしたいわけです。`magit.el`さんにお助け願おうかと思いましたが、そもそも出来るか分らないのでTwitterで投げてみたところ、@mod\_poppo氏に助けていただきました。`git reset`と`git cherry-pick`でできます。

`A`のブランチは問題ないので`B`でやると、まず私は`magit.el`で必要な`B`のコミットを調べておいて

    $ git reset --hard *

で`*`に`HEAD`を戻して`A`, `B`で加えた変更を全て破棄。

    $ git cherry-pick B-commits

で`B`での変更のみ適用

    $ git push -f origin branch

で自分のリポジトリに反映(=pull-reqにも反映)  
としました。`*`と`B-commits`はそれぞれリビジョンIDを調べて下さいね。

私の説明じゃ分りづらいと思うので [http://d.hatena.ne.jp/murank/20110327/1301224770](http://d.hatena.ne.jp/murank/20110327/1301224770)にあるすばらしい図を参考にして下さい。


