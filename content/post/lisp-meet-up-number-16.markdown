---
type: post
title: "Lisp Meet Up #16 でLTしてきました。"
date: 2014-04-25
comments: true
sharing: true
categories: [Common Lisp, Lisp, Lisp Meet Up]
---
κeenです。年度末/年度始めで色々あり、しばらく更新できてませんでしたが [Lisp Meet Up Presented by Shibuya.lisp #16](http://atnd.org/events/49370)に参加してきたのでレポートです。

<!--more-->

最近はLisp Meet Upも盛況で毎回ほぼ満席ですね。今回も満席でした。というか立ち見出てました。

自己紹介では初参加の方が多くてCLerが多い印象でした。昔研究とかでCL使ってて久し振りに、とか。私個人的にはTwitterでフォローしてる方々とお会い出来て楽しかったです。

LTは私とTOYOZUMIさんが告知してました。しかしTOYOZUMIさんは当日体調が悪くて欠席したので私と飛び入りのchikuさんで計2件のLTがありました。TOYOZUMIさんは何回かに分けて発表するうちの1回だったので次回LTしていただけるかと思います。

私のやつは↓です。時間ギリギリで作ったのであんまりいじめないで下さい。因みにまだソースはgithubに上げてないです（後述）

<iframe src="http://www.slideshare.net/slideshow/embed_code/33794330" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px 1px 0; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

 **[CL21で色々なデータ構造を実装してみようとした](https://www.slideshare.net/blackenedgold/lisp-meetup16 "CL21で色々なデータ構造を実装してみようとした")** from **[blackenedgold](http://www.slideshare.net/blackenedgold)** 

会場からはCL21作者のにとりさんから「abstract-sequenceは最低限実装しなきゃいけないメソッドは少ない」との指摘がありましたがqueueとして機能するために色々頑張ったのです。

abstract-setとかは欲しいですねとの声もありました。issueに上げておきますかな

また、chikuさんからheapの実装について色々と指摘があり、 [Introduction to Algorithms](http://www.amazon.co.jp/%E3%82%A2%E3%83%AB%E3%82%B4%E3%83%AA%E3%82%BA%E3%83%A0%E3%82%A4%E3%83%B3%E3%83%88%E3%83%AD%E3%83%80%E3%82%AF%E3%82%B7%E3%83%A7%E3%83%B3-%E7%AC%AC3%E7%89%88-%E7%B7%8F%E5%90%88%E7%89%88-%E4%B8%96%E7%95%8C%E6%A8%99%E6%BA%96MIT%E6%95%99%E7%A7%91%E6%9B%B8-%E3%82%B3%E3%83%AB%E3%83%A1%E3%83%B3/dp/476490408X)という本を紹介されました。とりあえず大学の図書館から借りてきたのでそれを読んだらcl21-data-collectionsに手を加えてgithubに上げます。

次はchikuさんから [chiku-utils](https://github.com/chiku-samugari/chiku.util)というオレオレライブラリの紹介がありました。スライドは見付け次第貼りますね。結構便利そうな関数やマクロがあったので一度見てみると良いかもしれません。

その後は休憩からのぐだぐだ時間でした。chikuさんの発表の中で出てきた`keymin`という関数について会場から一般的には`argmin`と言うとの指摘があり、その場でかにたんが`argmin`の定義の数式を書いたことから私とかにたんとラジカルこうじさんでマスハラをしあってました。というか主にかにたんがマスハラしてました。Lispの話はあまりしませんでしたね… あとは私がゼミで [パタヘネ](http://www.amazon.co.jp/gp/product/0124077269/ref=oh_details_o00_s00_i00?ie=UTF8&psc=1)を読んでることからパタヘネってC言語の入門書だよねとかの会話とか。StumpWMについてもちょっと話したかな？

そうだ。次回のScheme回は [Sagittarius](https://bitbucket.org/ktakashi/sagittarius-scheme/overview)の作者のKeiさんが次回LTする気満々とのことで話題に上がってました。それに [picrin](https://github.com/wasabiz/picrin)の作者のわざびず君も来るかもとかですしTOYOZUMIさんの連作もありますし次回は長くなるか2回に分けるかになるかもしれません。

その後の懇親会は9人が参加でしたがアルコールを飲む方はあまりいませんでしたね。飲んでたのは3~4人かな?多くの方が普通にお酒に弱いだとかバイクで来てるだとか次の日仕事があるとかですね。もう普通に食事でも良いのでは。私のいたテーブルでの会話は佐野さんがLispの本をめっちゃ持ってるだとかでしたね。実践Common Lispを訳者ということを割り引いても多いくらい持ってるそうです。CLtL2とかもめっちゃ持ってるそうです。謎ですね。

今回のまとめ

- かにたんをホワイトボードに近付けてはいけない

最後になりますがイベント管理や会場手配など運営の方々ありがとう御座いまいした。


