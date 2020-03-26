---
categories: ["ブログ"]
date: 2020-03-27T01:35:01+09:00
title: "BlogやるならGitLab Pagesがおすすめ"
---

κeenです。
GitHub Pagesからこんにちは。
私はこのGitHub Pagesのブログの他にGitLab Pagesのブログも持ってるんですが、それが使いやすいので宣伝します。

<!--more-->

GitLab Pagesを使うと

* Markdownでブログがかける
* Markdownのまま `git push` で記事の公開ができる
* コンテンツの管理が(GitHub Pagesより)楽
* サイトでHTMLやJavaScriptが使い放題

になります。
JavaScriptが自由に使えるということは私のように[reveal.js](https://revealjs.com/#/)などのスライドエンジンを使ってMarkdownでスライドを書いて公開することもできます。
ブログに自由度が欲しいという方におすすめです。

# GitHub PagesとGitLab Pages

[GitHub Pages](https://pages.github.com/)と[GitLab Pages](https://docs.gitlab.com/ee/user/project/pages/)はだいたい似たようなことができます。

ユーザのGitHub Pagesは `{username}.github.io` というリポジトリを作って、そこのmasterにpushしたものが `https://{username}.github.io` で公開されます。
プロジェクトのGitHub Pagesだと拾うブランチやディレクトリをいじれたはずですが、ユーザの `{username}.github.io` はmasterにpushしたもの全部公開だけだったと記憶しています。
あと一応、pushしたものは [Jekyll](https://jekyllrb.com/) で処理されます。なので特段セットアップしなくてもJekyllでサイトを作れます。

GitLab Pagesもユーザのページは `{username}.gitlab.io` というリポジトリを作って、そこにpushするのですが、**GitLab CIが走ったあとに** `public` という名前のディレクトリの中身を `https://{username}.gitlab.io` に公開します。
プロジェクトのページはGitHubと同じく `https://{username}.gitlab.io/{project_name}` になります。 See also [Getting started part one · Pages · Project · User · ヘルプ · GitLab](https://gitlab.com/help/user/project/pages/getting_started_part_one.md)

また、どちらもカスタムドメインを使うことができます。

参考までに、私の両者のリポジトリを置いておきます。

* [KeenS.github.io](https://github.com/KeenS/KeenS.github.io)
* [blackenedgold.gitlab.io](https://gitlab.com/blackenedgold/blackenedgold.gitlab.io)

書き忘れてましたがどちらも無料で、特別なセットアップなく、指定のリポジトリを作ってpushするだけで公開されます。

# 静的サイトジェネレータを使うならGitLab Pagesが便利

GitHub Pagesはpushしたらそのまま公開される手軽さ、Jekyllを用意してくれている親切心はあるのですが、それを使わないユーザには結構不便です。
例えばJekyllは仕様で `_` で始まるファイル名のファイルを無視するのですが、そのせいで手元で試すときとpublishしたときで挙動が変わったりします。

翻ってGitLab Pagesはユーザの好きな静的サイトジェネレータを使うのに向いています。
一度GitLab CIを通ったあとに `public` ディレクトリを見にいくのでCIでサイト生成をして `public` に成果物を置けば完成です。

GitLab Pagesのいいところはサイト生成した成果物をGitで管理しなくてよくなる点です。
GitHub Pagesの仕様だとどうしてもソースと成果物で二重管理が必要になりますし、masterの内容が全て公開されてしまうので別途ソースの管理場所を探さないといけなくなります。
See also [Github pagesで意地でもサブディレクトリをルートにする | Matsuura Tomoya|松浦知也](https://matsuuratomoya.com/blog/2016-05-07/githubpage-subdirectory/)。
二重管理になって私がよくやらかすのは、成果物を公開して満足して、ソースをpushし忘れるケースです。
その後に別のPCから内容を変更しようとしてソースコードがなかったりコンフリクトを起こして破滅したりします（git subtreeはヒストリが綺麗じゃないと破滅します）。
そこにきてGitLab Pagesはソースをpushしたらデプロイフローが走る仕組みなのでgitのハックは必要ないですし、二重管理も発生しません。

# 静的サイトジェネレータはHugoがおすすめ

おすすめといっても色々試した訳ではないんですが、私は長らく[Hugo](https://gohugo.io/)を使っています。GitHub Pages （このブログ）とGitLab Pages （[日常ブログ](https://blackenedgold.gitlab.io/)） 両方ともHugoです。
よくメンテナンスされていますし、エコシステムも発達していて、テーマも300種類以上作られているようです。

また、Go製でキビキビ動くのもポイントが高いです。
元々Jekyllの上に構築されたブログエンジンOctopressを使っていたのですがブログが100ページを越えたあたりから遅くて使いものにならなくなったのでHugoに移行したという経緯があります。
今300ページありますが、Hugoで生成するのに1秒未満です。もちろん、Octopressを使っていた当時とはマシンスペックが違うので直接比較はできませんがかなり速度差があるのは事実だと思います。

Hugoの使い方は公式ドキュメントなどを読んでもらうとして、GitLab Pagesのセットアップは[このCI設定](https://gitlab.com/blackenedgold/blackenedgold.gitlab.io/-/blob/master/.gitlab-ci.yml)をコピペするだけでいけるはずです。
というか、GitLabがプロジェクトテンプレートを持っている筈なのでポチポチやるだけでセットアップは終わります。
このように手軽に始められるのもおすすめの理由です。

# GitHub Pages / GitLab Pagesのデメリット

褒めてばかりなのでデメリットも書いておきます。全てはブログをGitで管理することに起因します。

* スマホなどから更新しづらい
* 下書きの扱いが難しい
  + Pagesを公開にしたまま、ソースは非公開にすると解決するかも
* 画像の処理が面倒

私はPCからしか書かないですし、下書きを溜めるタイプでもないので上2つは困ってないのですが最後のはちょっと困ってます。
ブログに画像を入れようとするとプロジェクト下に画像ファイルをコピーしてきて、そのパスをMarkdownから参照する必要があります。
また、画像をgitに突っ込むと操作が重くなります。

このあたりはCLIから気軽に更新できることと表裏一体ですね。

コンテンツマネジメント的な機能が欲しいなら[cloudron](https://cloudron.io/)なんかを使ってWordpressとかのCMSをホスティングするのがいいと思います。

# まとめ

「なんとなく自分の技術ブログを始めようかなー」と思っている方にはGitLab PagesとHugoがおすすめです。
