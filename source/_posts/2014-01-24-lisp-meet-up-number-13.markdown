---
layout: post
title: "Lisp Meet Up #13でLTしてきました"
date: 2014-01-24 02:29
comments: true
sharing: true
categories: [CIM, Common Lisp, Lisp, Lisp Meet Up]
---
もう12時回ったので昨日になってしまいましたが [Lisp Meet Up presented by Shibya.lisp #13](http://atnd.org/events/46746#comments)に参加してLTしてきたので報告(?)エントリです。

<!-- more -->

先月東京に戻ってきた深町さんがめちゃくちゃ宣伝してたせいか20人参加希望があり、19人参加しました。

自己紹介ではClojureが多いなーってのとRubyistが混ってたりしたのが印象的でした。Emacs Lispの人も居たり。

LTは一人目がにとり(深町英太郎 (fukamachi))(@nitro\_idiot)さん。今更ながら呼び方に困りましたね。内容はO/Rマッパーの [`integral`](https://github.com/fukamachi/integral)の機能と実装について。  
非Common Lisperが多い中Meta Object Protocolについて色々説明したりと大変そうでした。私はCommon LisperなのでMOPにそんな使い方がるのかとか結構面白かったです。  
<s>スライドはまだ上げてないみたいです。そのうち上がるでしょう。</s>上がりました。

<iframe src="http://www.slideshare.net/slideshow/embed_code/30362150" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px 1px 0; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

 **[Integral – New O/R Mapper for Common Lisp](https://www.slideshare.net/fukamachi/lisp-meet-up-presented-by-shibuyalisp-13 "Integral - New O/R Mapper for Common Lisp")** from **[fukamachi](http://www.slideshare.net/fukamachi)** 

二人目が私。最近作ってる [`CIM`](https://github.com/KeenS/CIM)についてです。スライド見るのが速いですね。

<iframe src="http://www.slideshare.net/slideshow/embed_code/30338391" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px 1px 0; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

 **[CIM – Common Lisp Implementation Managerを作りました](https://www.slideshare.net/blackenedgold/cim-common-lisp-implementation-manager "CIM - Common Lisp Implementation Managerを作りました")** from **[blackenedgold](http://www.slideshare.net/blackenedgold)** 

実際にそれぞれのコマンドを動かしてみた結果、予想してなかったエラーなとがあったものの割と聴衆の反応は良かったです。あとはPure Bourne Shell Scriptで挑んで爆死した話とかしたりですね。「詳解シェルスクリプト」を読んだり`man`ページ読んだりして頑張ったんですよ。

それが終わったら一旦休憩。そしたらわざわざ深町さんが私のところに来て下さって「Shellyより良いプロダクトになると思います。頑張って下さい。」との御言葉を頂きました。非常に嬉しかったです。私が作ったものが認められた。しかも着想の元になったShellyの作者の深町さんから。  
でもその後実際に何人か使ってみてくれたのですが、バグが立て続けに3つくらい発覚して「…これ、テスト必要ですね」と絶句。分かってます。分かってますけど難しいんです泣。  
他はgithubでCIMを探そうとしてる方が居たんですが同じ名前のレポジトリがめちゃくちゃあって検索性悪いなとか。何故か私のハンドルネームの最初の文字がk(ケー)ではなくκ(カッパ)なのを知ってたり。

その後の懇親会では向かいが深町さん、隣が佐野さん(実践Common Lispの訳者)で、Common Lispの今後とか色々な話をしてました。 [ここ](http://redmonk.com/sogrady/2014/01/22/language-rankings-1-14/)のグラフでCommon Lispがありえない位置にいるしどうやったら普及するんだろうねーとか。

運営のchikuさん potixさん、発表を聞いてCIMを試してくれたりGithubにstarくれたりしたみなさんありがとう御座いました。


