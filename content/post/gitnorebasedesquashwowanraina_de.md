---
categories: ["小ネタ", "git"]
date: 2019-01-24T14:18:40+09:00
title: "gitのrebaseでsquashをワンライナーで"
---
κeenです。コミットを1つに纏めたいときに `git rebase -i` を使って最初のコミットだけ `pick` 、それ以外は `squash` にするのはよくやりますよね。
定形作業をするのが煩わしくなったのでそれをワンライナーにまとめてみました。
<!--more-->

# TL;DR


```console
$ EDITOR='sed -i "2,\$s/^pick/squash/"' git rebase -i <リベース元コミット> && git commit --amend
```

`git rebase` はとても危険なコマンドなので馴れてる人が手間を省くために使って下さい。
何も知らずにこのページにたどり着いた方は悪いことを言わないので `git rebase --interactive` の使い方を調べるところから始めましょう。

# 解説

まず、このようなログがあるとします。

```plaintext
commit bb3db0607fb5730f0726f581485aed54f951dde3 (HEAD -> master)
Author: κeen <3han5chou7@gmail.com>
Date:   Thu Jan 24 14:28:42 2019 +0900

    add d

commit 2db04c2cbb4d8d519da8aeccd3a3b21395a8364e
Author: κeen <3han5chou7@gmail.com>
Date:   Thu Jan 24 14:11:06 2019 +0900

    add c

commit 2ca1e93e628a69dd630b371273f7a7f7d8a9b253
Author: κeen <3han5chou7@gmail.com>
Date:   Thu Jan 24 14:10:49 2019 +0900

    add b

commit 66cd45364b1d1d39330b1c647cf2221afd505e47
Author: κeen <3han5chou7@gmail.com>
Date:   Thu Jan 24 14:10:44 2019 +0900

    add a
```

これを普通に `git rebase -i` したらエディタが立ち上がりこうなりますね。

```plaintext
pick 2ca1e93 add b
pick 2db04c2 add c
pick bb3db06 add d

# Rebase 66cd453..bb3db06 onto 66cd453 (3 commands)
#
# Commands:
# p, pick <commit> = use commit
# r, reword <commit> = use commit, but edit the commit message
# e, edit <commit> = use commit, but stop for amending
# s, squash <commit> = use commit, but meld into previous commit
# f, fixup <commit> = like "squash", but discard this commit's log message
# x, exec <command> = run command (the rest of the line) using shell
# d, drop <commit> = remove commit
# l, label <label> = label current HEAD with a name
# t, reset <label> = reset HEAD to a label
# m, merge [-C <commit> | -c <commit>] <label> [# <oneline>]
# .       create a merge commit using the original merge commit's
# .       message (or the oneline, if no original merge commit was
# .       specified). Use -c <commit> to reword the commit message.
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# If you remove a line here THAT COMMIT WILL BE LOST.
#
#       However, if you remove everything, the rebase will be aborted.
#
#
# Note that empty commits are commented out
```

2つ目以降の `pick` を `squash` にしてこうしたいはずです。

```plaintext
pick 2ca1e93 add b
squash 2db04c2 add c
squash bb3db06 add d
```

Viならコマンド `:2,$/^pick/squash/` でできますがワンライナーに組み込めません。そこでsedを使って編集します。
それが `EDITOR='sed -i "2,\$s/^pick/squash/"' git rebase -i <リベース元コミット>` の部分。sedもviも源流はedなのでコマンドを使い回せて便利ですね。
ただしこれだとgitのプロセス中のエディタ全部がsedに置き換わってしまいます。特にrebase戦略を編集したあとコミットメッセージを編集する部分までエディタを乗っ取ってしまいます。
どうしようか悩んだのですがそこは諦めることにしてコミットしたあとでメッセージを編集することにします。それが `git commit --amend` の部分。

気に入ったらサブコマンドにする選択肢もあるかもしれません FYI: [便利な「git-サブコマンド」を作成する](https://qiita.com/b4b4r07/items/6b76a5f969231e5e9748)

Happy Hacκing~
