---
type: post
title: "Lisp Meet Up #11でLTしてきました"
date: 2013-11-29
comments: true
sharing: true
categories: []
---
「Makefile書けない人がMake 4.0触ってみた」での発表です。

<!--more-->

まあ、これが殆どなんですけどね。

<iframe src="http://www.slideshare.net/slideshow/embed_code/28661586" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC;border-width:1px 1px 0;margin-bottom:5px" allowfullscreen> </iframe>

 **[Lispmeetup11](https://www.slideshare.net/blackenedgold/lispmeetup11 "Lispmeetup11")** from **[blackenedgold](http://www.slideshare.net/blackenedgold)** 

[githubのソース](https://github.com/KeenS/Shibuya.lisp-LispMeetUp-11)も参照下さい。

あとUStreamにも上がってました。

[http://www.ustream.tv/recorded/41155561](http://www.ustream.tv/recorded/41155561)

いや、改めて自分の発表聞いてみると酷いwもうちょっと喋るの上手くなりたいです

会場ではこのあと、「使えるのは良いけど誰が使うんだろーねー」とかの話題になりました。

次の発表はかにたん( [@kazh98](https://twitter.com/kazh98))さんからR<sup>7</sup> RSの新機能についての発表でした。

「すごい！」っていうより「今まで無かったんかい！」って機能が満載でしたね。

その後、chikuさんによるライブコーディングで、お題は`(loop repeat 1000 collect (float (/ (random 1000) 1000)))`で生成された浮動小数点数1000このヒストグラムを作るものです。データの格納をhashでやるかalistかplistかとかだったり最終的にヒストグラムにするときに数字->‘\*’の変換とかで外野から色々飛んで楽しかったです。

自然数nが与えられてn個の’\*‘を返すってどうするんだろ。formatでできるのかな？`(coerce (make-array n :initial-element #\*) 'string)`が個人的には気に入りました。

12/05追記:`(make-string n :initial-elemet #\*)`というもっと直接的な方法がありました。

懇親会は’80~‘90年代のPCの話で盛り上がって私にはちんぷんかんぷんでした。それ以上に小2でファミコンの改造/ハックをやってたってのは凄いですね。

## その他

[cim](https://github.com/KeenS/CIM)の開発ぼちぼち進んでます。`syset`あたりがまだ手をつけてないです。あとインストーラが動いてくれない。コアダンンプ機能必要なのでは？など。`Quicklisp`のロードって案外重いんですね。


