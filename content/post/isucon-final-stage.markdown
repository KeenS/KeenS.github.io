---
type: post
title: "チーム「銀杏絶滅しろ」でisucon学生賞もらってきました"
date: 2013-11-11
comments: true
sharing: true
categories: [ISUCON]
---
しばらく間が空きました。まあ、その間のことはいつかまとめて書きます。今回は [isucon#3](http://isucon.net/)について。

[先日](/blog/2013/10/09/isucon-qualifying-event/)、isucon#3の予選を突破したと書きましたが、日曜にその決勝戦があったので報告です。

<!--more-->
## 準備

とりあえずyokoteさんが予定が入って参加できないので@c0umeさんと二人で毎週会って予選の問題を解き直しつつSQLの勉強とかやってました。MariaDBを試したりもしましたが大して変んないねーとか。最終的に4万点くらいになったのかな？あとisucon#2で複数台構成の練習とか地味にデプロイツールCapistranoのお勉強とかもしてました。非常に便利です。

# 当日

サークルを休む許可も貰えたので何事も心配なく10時にヒカリエに集合。名札とかもらって本格的な感じ。 ![名札とチームの看板(?)](/images/isucon-name-strap.jpg "name-strap")

お題のアプリは画像版ツイッターで、処理的には投稿された画像を生/中/小にサイズ変更して返すのがほとんど。htmlはインデックスだけであとはjson APIですしDBまわりも画像のアクセスレベルとユーザーのフォロー関係程度で、必要最低限といった感じ。単純にファイスサイズで攻めてきましたね。

因みにこれ、ストーリーとしてはエンジニア二人で作って騒いでたら社長が聞き付けて「18時に大規模プロモーションかけるからそれまでに対策しといてね。あ、サーバーは4台増やしといたよ」ってなっててそれを見た瞬間のみんなの反応が

> 社長を殺そう [#isucon](https://twitter.com/search?q=%23isucon&src=hash)
> 
> — tagomoris (@tagomoris) [November 9, 2013](https://twitter.com/tagomoris/statuses/398992801850875904)<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

とかでしたw

そうそう、そんなストーリーなもんでサーバー5台中1台にしかアプリがデプロイされてない状態でスタートです。

## やったこと

@c0umeさんがサーバーセットアップしてる間に私がアプリを読んできになったところをスカイプに張っていきました。

何も考えずにとりあえずnginxを野良ビルドしてgzip配信を有効にしようとするも、なぜかnginx.confが上手く機能せずにハマる。結局conf.d以下を読んでた部分が悪さをしてたと判明。ようやくアプリ側にとりかかる。

とりあえずリクエストの度に画像を変換してるのでそれをどうにかしようということに。POSTされた時点で変換する方針だったのですが、私は変換した画像の「パスネーム」をDBに突っ込もうって思ってたのですが@c0umeさんは画像の「バイナリデータ」を突っ込もうとしてたらしく、意思疎通ができずに若干揉めました。

結局、1台リバースプロキシ、3台app、 1台DBの構成で、appで画像のPOSTを受けたら3サイズにコンバートしてDBに突っ込むことに。ですがとりあえず段階を踏もうということで第一段は画像をblobとしてMySQLにツッコみます。第二段でコンバートして突っ込む、第三段でさらにgzip圧縮して突っ込むことにしました。

私がapp側、@c0umeさんがDB側でやりました。

外部プロセスを呼んでると遅いと聞いたのでRMagickを使って変換するようにして、さらにファイルに書き出さずに`Magick::ImageList#from_blob`と`#to_blob`を使って書き換えました。

今回、RMagickを初めて使ったのですが、ハマることハマること。`Magick::ImageList#crop`非破壊的であったり、関連ブログに書いてあるメソッドがないとか言われたり。最終的にDBが終わった@c0umeさんにも手伝ってもらってバグ取りが完了し、ベンチマークが走るようになった時点でもう終わり近く。大きな変更はやめてキャッシュとかレスポンスだけ返して裏で処理を走らせるとかをやりましたがどれもベンチマークが巧妙に作られてて失敗に終りました。あとUnicornに謎のエラーが出続けてましたが特にベンチに影響はなかったので放置しました

それで初期スコアが1100だったのが5400程度になりました。最後20分くらいは`chkconfig`とかでfailしないよう確認作業をしてタイムアップ。暫定学生1位、全体で7位だったかと思います。

failしないことだけを祈って1時間後の結果発表を待ちます。本計測は3分ですし謎のエラーもありましたし不安要素はいっぱいありました。

## 結果

failしませんでした。よかった。上のチームがfailしたらしく、最終順位は学生1位、全体6位でした。まあ、failしなかったチームは7チームしかいなかったのでなんとも言い難いですが。

![最終スコア。一位のLINE選抜チームは2位に4倍差を付けて圧勝しました。](/images/isucon-scores.jpg "scores-of-each-team")

スコア的には3位と2倍差だったので案外良い線いってたのかもしれません。

それで、タイトルにあるように学生賞もらいました。

![中身は現生5万円。](/images/isucon-prize.jpg "prize")

この5万は2万ずつ二人で分けて、あとの1万でyokoteさんと3人で肉を食べに行きます。

因みに私はこういう風に使いました。

![欲しいものより普段買いそうにないけど気になるものを選びました。](/images/isucon-books.jpg "books")

その後懇親会ではGoとかnodeの人達がライブラリが若くてバグがあって苦しんだりしてた話を聞きました。fujiwara組長のところには行けませんでした。

## 感想

たのしかったです（小並感  
ベンチマークツール自体がラウンドロビンでリクエスト飛ばしてくるのでリバースプロキシ不要でしたね。あとはあせりすぎて手汗でタッチパッドの誤反応が頻発したり挙句なぜかカーネルがCPU100%使い始めて作業できなくなったりして冷や冷やしました。@c0umeさんには多大な迷惑お掛けしました。そのPC寿命じゃね？とか言われたり。

来年は胸を張れるようなスコアを出したいですね。LINE、カヤック、データホテルのみなさんありがとうございました

## その他

κeenは就活生です。RubyかLispでできる仕事探してます。

あとamazonほしいものリストも上げときますね。http://www.amazon.co.jp/registry/wishlist/TA077537OIUP


