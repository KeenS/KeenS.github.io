---
type: post
title: "ISUCON#3予選に参加しました"
date: 2013-10-09
comments: true
sharing: true
categories: [ISUCON]
---
[ISUCON](http://isucon.net/)#3の予選に参加してきた話です。

<!--more-->

ISUCON(Iikanjini SpeedUp CONtest)とは椅子を投げないなどの一部のルールを除き、基本なんでもアリのWebアプリケーションチューニングコンテストです。

なんで私がこんなのに参加するかというと、サークルのOBさんから告知があり、8月に学生向けのISUCON夏期講習会に参加したからです。第３回となる今回は学生枠を作るとのことで事前に学生のレベルを上げておこうという方針だそうです。

その講習会でコテンパンにやられてめげつつも、もう一回チャレンジしようということで夏期講習会で知りあったasamiさんとその友達のyokoteさんでチームを組んで参戦しました。

ちなみにこのチーム、予選3日前にSkype会議で結成したもので、予選当日も各自自宅にいてSkypeで連絡を取ってたので私はyokoteさんとはまだ会ったことありません←

## 準備

事前の話しあいではasamiさんがAWSの準備からgit(bitbucket)、ssh鍵の設定までやってくれることになりました。私も本来ならISUCON#2で練習をしておくべきだったのですが、サークルとかを言い訳に、ISUCONの記事に目を通すだけに終りました。

## 当日

10時に開始しました。10時40分に起きました←

私が起きた頃にはasamiさんが色々設定を済ませていたので問題のアプリケーションを`git clone`して公開鍵をSkypeにペタっと貼って参戦。アプリを読むにMarkdownでメモを投稿できるもの。

フムフムってなってたらsslのバージョン問題でログインできてなかったyokoteさんも参戦し、asamiさんが`my.cnf`をいじってスコアが

900→2500

となりました。スゲー。あ、言語はasamiさんとyokoteさんが書けるperlを選択。私は1ミリも読めませんがサーバーをいじる気だったので気にしませんでした。

で、私がフロントサーバーをapacheからnginxに移行し、staticファイルをnginxから出すようにして、さらにキャッシュするようにし、ログを消したら

2500→2800

となりました。後で考えるとこのログを消したのまずかったですね。asamiさんがなんかpythonのツールを使ってトラフィック監視してたのでいっかなーって思ってたんですが。

その間asamiさんがslow queryのログを吐かせてボトルネックを見付けようとしますが、どれも平均的に重い。ソートに使われてる`created_at`にyokoteさんがインデックスを貼ってくれたりしましたが大した改善はみられず。後にこのインデックスはベンチマークが初期化するときに消されてるのではということが発覚しました。

あとは私がworker\_connectionをいじったりasamiさんがmemcachedを使ってクエリの数を減らしたりyokoteさんがストレージエンジンをarchiveに変えたり色々しましたが努力虚しくスコアは改善しませんでした。

外部で`markdown`コマンドを呼んでるところを(それがperl製のスクリプトだったので)アプリに埋め込んでしまえばちょっとは速くなるかなと思いましたが私はperlをいじれませんし大差ないだろうからいっかと思って放置してたのですが、後で聞くと案外それでスコア伸びたそうです。nginxのログを見てれば気づいたことだったのに…

ということで大したこともできず予選敗退です。因みに1位とは10倍くらいの差がありました。

## 振り返って

後で他の方のブログとかを見るとそんな手があったのかとかそこに罠があったのかとか全然気づかなかったことがたくさんありました。とりあえず勉強しなきゃって思ったのは + redis + プロファイラ + デプロイツールですね。asamiさんがデプロイツールを用意してくれてたのですが私には使いこなせず、折角git管理してたのにその利点を生かせませんでした。(私は手動デプロイしてました)

あとはボトルネックから攻める、redisで書き換えるといったこをとほとんどの上位チームがやってたのでそこは勉強したいです。

来年も開催されるなら是非参加したいですね。

## 追記

まさか…まさかこんなことになるとは…

[学生枠上位3チーム中2チームが動作検証で失格となり](http://isucon.net/archives/32951235.html)、繰り上げで私達のチームakyが予選通過となりました。ビックリすね。終ったあとasamiさんがしっかり動作確認してからAMIを提出してくれたからでしょう。

6割くらい嬉しいのですが、失格となったチーム魔王は夏期講習会で一緒だったメンバーのチームで、内心お祝いしてたのにこんなことになって残念です。スコアも正常に動作していたら16000点だったそうです。

あと1ヶ月、精進して本戦で晒しものにならないよう頑張ります。


