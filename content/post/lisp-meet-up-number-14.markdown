---
type: post
title: "Lisp Meet Up #14でLTしてきました"
date: 2014-02-27
comments: true
sharing: true
categories: [Common Lisp, Lisp, Lisp Meet Up, OCaml]
---
どうも、κeenです。Rubyをアップグレードしようとしたら`rvm`が動かなくて、あれ？ってなったら私が`echo -> echo`とかいう謎のsymlinkを作ってたせいでした。このブログはOctopressで作ってるのでその間更新遅れました。

さて、毎月恒例 [Lisp Meet Up Presented by Shibuya.lisp #14](http://atnd.org/events/47730)でLTしてきたので報告です。今回は「Real World OCamlを読んだ感想」というタイトルです。

<!--more-->

まあ、感想とは言っても内容を軽く紹介したあとにLispにインポートしてみたという内容です。

今回はいつも以上にスライドが説明不足なのでどうにか頑張ってUstreamを探して下さい

<iframe src="http://www.slideshare.net/slideshow/embed_code/31658546" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px 1px 0; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

 **[Real World OCamlを読んでLispと協調してみた](https://www.slideshare.net/blackenedgold/real-world-ocamllisp "Real World OCamlを読んでLispと協調してみた")** from **[blackenedgold](http://www.slideshare.net/blackenedgold)** 

会場からはquicksortの例がメモリ使用量的にquicksortとは言えないとか怒られました。あとは`append`が演算子`@`で書けちゃうとコスト高いのにみんな頻繁に使っちゃうんじゃないかとかのツッコミもありました。

Meet Up自体は自己紹介から始まりました初めての参加の方が14人中3人だったかな？もっと増えると嬉しいですね。京都から来た博士過程の方とか「ハッカーと画家」を読んでLispに興味を持った方とかが居ました。

で、私のLTは上の通り。前述の通りUstreamでも流されてます。

次はかにたん(@kazh98)さんから。かにたんここ2ヶ月くらいMeet Upで見掛けないなと思ってたら数学の論文に”Gauche”って文字列を入れる仕事をしてたようです。LTはRacketの新バージョンの機能FFIを使って線形代数ライブラリのバインディングを作る話でした。ジョーク飛しながら軽快に喋ってて楽しかったです。スライドはこちら。

<iframe src="http://www.slideshare.net/slideshow/embed_code/31635279" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px 1px 0; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

 **[すべてをRacketに取り込もう！ ~Racket FFI と Package システムの使い方~](https://www.slideshare.net/kazh98/racket-31635279 "すべてをRacketに取り込もう！ ~Racket FFI と Package システムの使い方~")** from **[Kazuhiro Hishinuma](http://www.slideshare.net/kazh98)** 

そのあとは休憩からのぐだぐだタイムで私はCommon Lisperで [CL21](http://cl21.org/)の`lambda`の略称を考えてたりしました。私は`^`良いと思うんですけど英字キーボードだと`S-6`なので押しにくいそうです。`fn`は`function`の略称っぽくてダメという話。`function`はCommon Lispでは既に別の意味がありますからね。

懇親会では私含めて就活生が二人居るので就活の話になりましたがLisperってまともな就活してないんですね…  
大学中退してバイト先に就職した、友達の始めたベンチャーに参加してから今の職場に転職した、就活せずにふらふらしてたらShibuya.lispでLispの求人があったからそこに行った…。

そういえばまた隣がにとりさんだったので最近何やってますかと訊いたら`Utopian`に向けて着々と準備進めてますとの返事をいただきました。まだ計画あったんですねと返すと

> 昨日「Utopianってまだ計画あったんですね」って言われたけど、3年前からClack、Caveman、ningle、CL-DBI、SxQL、Caveman2、Integralってずっとリリースし続けてるのは全部Utopianの為ですよ
> 
> — 深町英太郎 (fukamachi) (@nitro\_idiot) [2014, 2月 27](https://twitter.com/nitro_idiot/statuses/438909695881797632)<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

とのこと。私もUtopianの下地を作るためにRubyにあってLispにないものを色々考えてたので色々話してたらCL21のコミット権もらっちゃいました。私はライブラリ(DateTimeとかQueueなどのデータ構造とか)を整備したいのですがそれらは0.2で追加される予定だそうなので今はissue潰しですかね。

まあ、そんなところです。そういえばClojureの会社に潜入したといういちせさん(＠ichisemasashi)にどんな感じか訊くの忘れてましたね…まあ、次回もいらっしゃるでしょうから次回訊けば良いですかね。


