---
type: post
title: "Githubの変更を追う"
date: 2013-09-02
comments: true
sharing: true
categories: [Git]
---
こんばんは。κeenです。普段、Github上で最新のソースを追ってるプロジェクトはこまめにgit pullすることで更新を確認してたのですが、別の方法があることに気づきました。

<!--more-->

単純にWatchという便利な機能もあるのですが、RSSでソースを追うこともできるようです。RSSのアイコンが見当らなかったのでないものと思ってましたがちゃんとあるんですね。

RSSのAPIはgithub.com/ORG/PROJECT/commits/BRANCH.atomになります。  
たとえばMirahのmasterブランチの最新のコミットを受け取りたかったら、  
 [https://github.com/mirah/mirah/commits/master.atom](https://github.com/mirah/mirah/commits/master.atom%20%20)になります。試してませんがcommitsのところをissuとかにすればissuを追えるのかもしれません。

watchとの違いは、watchはwikiの変更やissuなど全てを追うところと、通知がメール and/or github上での通知なところですね。

メールじゃなくてgithubの通知でもなくてRSSで受け取りたい方は試してみてはいかがでしょうか。


