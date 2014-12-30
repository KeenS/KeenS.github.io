---
type: post
title: "Common Lispの勉強をするには、あるいは情報を得るには"
date: 2014-10-20
comments: true
categories: [Lisp, Common Lisp]
---
[深町さんのブログ](http://blog.8arrow.org/entry/2014/09/05/062028)で日本語でCommon Lispの質問をする方法が載ってましたが質問じゃなくて単に情報を得たい、勉強をしたいという場合が書かれてなかったので勝手に補遺。
<!--more-->

# 処理系選び
[処理系:選び方の目安 - Common LISP users jp](http://cl.cddddr.org/index.cgi?%BD%E8%CD%FD%B7%CF%3A%C1%AA%A4%D3%CA%FD%A4%CE%CC%DC%B0%C2)や
[Common Lisp 処理系 - 紫藤のWiki](https://sites.google.com/site/shidoinfo/Home/programing-lang/%E9%96%A2%E6%95%B0%E5%9E%8B%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E8%A8%80%E8%AA%9E/lisp/common-lisp/common-lisp-%E5%87%A6%E7%90%86%E7%B3%BB)が参考になるかと思います。

# 開発環境構築
[Modern Common Lisp](http://modern-cl.blogspot.jp/)あたりですかね。著者がちょっと古くなってきたので更新するって言ってました。

処理系のインストールなら*nix環境で使える拙作の[CIM](https://github.com/KeenS/CIM)もあります。解説は[こちら](/blog/2014/01/27/cim-explanation/)。
# 入門
## Web
私の一番のおすすめはM.HiroiさんのWebページです。一応Common Lispではなくxyzzyですがここで紹介されている内容であればCommon Lispにも共通します。  
[M.Hiroi's Home Page / xyzzy Lisp Programming](http://www.geocities.jp/m_hiroi/xyzzy_lisp.html)

## 書籍

* [実践Common Lisp](http://www.amazon.co.jp/%E5%AE%9F%E8%B7%B5Common-Lisp-Peter-Seibel/dp/4274067211)は入門から実践までいけて素晴らしいです。内容の密度が濃いです。心無しか手にとってみると少し重いです。
  loopやformatなど複雑な部分はしっかりベージを割いてるので入門が終わってもリファレンスとして役立ちます。私も今でも参照してます。
* [Land of Lisp](http://www.amazon.co.jp/Land-Lisp-M-D-Conrad-Barski/dp/4873115876)は挿絵が多く、少し学んでは実際に使ってみるのルーチンなので勉強のモチベーションを保ちやすいです。
  内容は実践Common Lispに比べると細かな注釈が少ないかな？といったところ。さらっと読めて良いですね。
* [実用Common Lisp](http://www.amazon.co.jp/gp/product/4798118907/ref=pd_lpo_sbs_dp_ss_2?pf_rd_p=466449256&pf_rd_s=lpo-top-stripe&pf_rd_t=201&pf_rd_i=4873115876&pf_rd_m=AN1VRQENFRJN5&pf_rd_r=1YA4XCHT36XJD6ZEF9KC)(通称PAIP本)はぶ厚くて高い本。AI系の内容らしい。私は読んだことが無いのですが、数理システムが入門書として使ってるそうです。
  2014-12-14 追記 [読みました](/blog/2014/12/14/shi-yong-common-lispwodu-nda/)

# 何か作る
## Web

* [Common LispでSTGを作りますが何か](http://www.usamimi.info/~ide/programe/stg_doc/stg-commonlisp.pdf)は有名なPDF。STG(シューティングゲーム)を作る内容。多少古い(SDLが1.2系だとか)ものの、練習には十分です。
* [Common LispでWebAppの開発に必要なN個のこと - 八発白中](http://blog.8arrow.org/entry/2013/09/10/110632)はWeb Appを作るにあたって必要なライブラリを紹介している。紹介しているだけで作り方は書いてないのでどうにか頑張って下さい。

あんまり情報無いですね…Lisperのみなさんもっとアウトプットしましょう。

# リファレンス
## Web

* [Common Lisp Hyper Spec](http://www.lispworks.com/documentation/HyperSpec/Front/)はCommon Lispの機能をサンプルと共に網羅的に、詳細に紹介しています。英語です。
  主に[コンテンツ](http://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm)からドリルダウンで調べていくと良いです。時間があればじっくり眺めても良いかも。
  Common Lispをやる上では必ず必要になるので、英語ですが必ず友達になっておきましょう。
* [逆引きCommon Lisp](http://tips.cddddr.org/common-lisp/)は名前の通りやりたいことからCommon Lispの機能を調べられます。サンプルもあります。残念ながら網羅的ではないのですが凡その需要は満たせると思います。
* [Quickdocs](http://quickdocs.org/)はライブラリのドキュメントサイト(英語)。ライブラリを捜す時にも良いと思います。

## 書籍

* [COMMON LISP 第2版](http://www.amazon.co.jp/COMMON-LISP-%E7%AC%AC2%E7%89%88-Guy-L-Steele/dp/4320025881/ref=cm_lmf_tit_10)は通称cltl2と呼ばれるCommon Lispの原典の和訳です。一応紹介しました。今のCommon LispはANSI Common Lispになってるので少し違います。

# ポータルサイト
* [Common LISP users jp](http://cl.cddddr.org/index.cgi)は日本語で様々な情報が得られます。
* [redditの/r/lisp_ja](http://www.reddit.com/r/lisp_ja/)にはLispに関連するWebページの情報が日々投稿されています。
* [CLiki](http://www.cliki.net/)はCommon Lispの総合Wiki(英語)。非常に有用な情報が溜まってます。
* [Common-Lisp.net](http://common-lisp.net/)はCommon Lispのコミュニティ支援サイト(英語)。多くのプロジェクトをホストしていて、そのWebページなんかもあります。

# ソースコードを読む
κeenの適当チョイスです。他にお勧めがあれば教えて下さい。

* [alexandria](https://github.com/keithj/alexandria)は有名なユーティリティパッケージ。小さな関数やマクロで構成されてるのでイディオム集みたいな感じで読めると思います。
* [log4cl](https://github.com/7max/log4cl)はログライブラリ。ドキュメント(英語)をしっかり書いてるので読み易いです。
* [Hunchentoot](https://github.com/edicl/hunchentoot)はWebサーバ。ドキュメント(英語)を過剰に書いてるので読める筈。

# ステップアップ
## Web

* [On Lisp](http://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/)は主にマクロについて書かれたCommon Lispプログラミングの指南書。書籍もありますが、Web版もあります。

## 書籍

* [Let Over Lambda](http://www.amazon.co.jp/LET-OVER-LAMBDA-Edition-1-0/dp/4434133632)(通称LOL)は過激とか狂気とか宗教とか言われるエッセイ。高速化の話とかクロージャの危ない使い方とかが書かれてます。

# Lisperとコミュニケーションをとる
* [Shibuya.lisp](http://shibuya.lisp-users.org/)は渋谷を中心に半径2万kmを対象としたLispコミュニティ。毎月下旬の平日夜にLisp Meet Upをやってるのでひょこっとやってくると良いと思います。内容はCommon Lisp, Scheme, Clojureで回してるのでCommon Lisp回は3ヶ月に1回ですが毎回Common Lisperは来てるので別のLispの回でも構わず参加すると良いと思います。
  \#lispmeetup の情報は[ここ](https://atnd.org/users/51173)から入手するのが良いかな？あとは[Twitterアカウント](https://twitter.com/shibuya_lisp)もあります。Ustream配信もしてます。
* IRCの#lisp-ja@irc.freenode.orgと#common-lisp-ja@irc.freenode.orgにコミュニティがあります。常に誰かがいる訳ではないのですがちょくちょく見てる人はいるので発言してみると反応があるかもしれません。今もこの投稿に対して反応があってCIMとredditの項目を追加しました。

