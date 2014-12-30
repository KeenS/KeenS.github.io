---
type: post
title: "Lisp Meet Up #19でLTしてきました"
date: 2014-07-30
comments: true
categories: [Lisp, Common Lisp, Lisp Meet Up]
---
κeenです毎月恒例の[Lisp Meet Up presented by Shibuya.lisp](https://atnd.org/events/53246)でLTしてきたのでレポートです。
<!--more-->
今回は募集20に対して参加登録20、参加18、LT4本と集りの良い会でした。

自己紹介ではほぼCLの人ばっかりでした。珍しいところ（？）では数理システムの方が居たり会場を提供して下さってるサイバーエージェントの方が覗きにきてたり。

LT1本目はTOYOZUMIさんから連作の一つのCommon Lisp最適化発展。画像のエッジ検出をするときの関数の最適化の話でした。カーネルを渡すだけで最適なコードを生成するそうです。コーナーケースのif式をループの外に追い出してたりと半分成程、あとはよく分かんない話してるなーって。  
最終的には`eval`を使ってるらしく、本当に速度出るの？って気になりましたが本人もやってみたところ速くなってビックリだったそうです。

スライドはこちら。

<iframe src="//www.slideshare.net/slideshow/embed_code/37456264" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px 1px 0; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="https://www.slideshare.net/toyozumiKouichi/common-lisp-37456264" title="Common lisp最適化発展" target="_blank">Common lisp最適化発展</a> </strong> from <strong><a href="http://www.slideshare.net/toyozumiKouichi" target="_blank">Toyozumi Kouichi</a></strong> </div>

動画は今回から私が担当なので処理が終ったらリンク張ります。

次はTakagiさんよりcl-cudaについて。話聞く前は「どうせC APIをCFFIで叩いただけだろ(ホジ」って思ってましたがさすがにGPGPUはそんなに簡単じゃなかったようです。
cl-cudaはLispのAPIを叩いたらCUDA向けのCコードを吐いてCUDA専用のコンパイラ通してモジュールとして動的に読み込んで、ってやってくれるそうです。すごい。
完成度も、CUDAを研究に使ってたchikuさんによると普通使う分にはほぼ困らなくて、あとは構造体のサポートがあれば良いかも程度だそうです。すごい。

スライドはまだ見付けてないです。

3番目は私から、WebブラウザベースのIDE、[Cloud9](https://c9.io/)の紹介とLispの対応状況について。
スライドはこのブログに載せてますが、こちら

[Cloud 9の紹介 | κeenのHappy Hacκing Blog](/slide/cloud-9.html)

もうちょっと説明が欲しそうな雰囲気だったので別記事立てますね。@potix2さんが社内チャットに流したところなり反応が良かったようなので普通にIDEとして優れてるんじゃないでしょうか。

最後はかにたん(@kazh98)から床下からLispが出てきた話。うん。本当に床下からLispが出てきた話でした。かにたんの研究室すごいですね。みなさんの家の床下にもLispがあるかもしれませんよm9っ。

最後の最後にかにたんの研究室から出てきたLispについてTOYOZUMIさんがモニタをスクリーンに映しつつみんなで色々議論。なんかWEBのマニュアルがポルトガル語で書かれてたのでポルトガル語喋れる人捜したけど居なかったり。次回、床下Lispを動かしてみようってなりました。

そこでもう閉場の時間だったので懇親会へ。11名が参加。

私とかにたんとpotix2さんがschemeの実装の話してたり@mhkojiさんがhunchentootがありえないくらい遅いって話してたり色々でしたね。
CIMの話も出てました。実用上Windowsサポートは外せないってことで@snmstsさんがCでCIMを書き直してるだとか@nitro_idiotさんがshellyでCIMを使うにあたってCIM側でコアのマネジメントして欲しいって言ってただとか。

C CIMは面白そうですね。ビルドしなきゃいけないから手軽さの問題が…とかlibcurlに依存するとLinuxが…とか言ってたら「ディストリのパッケージシステムに乗せてしまえば問題無い」と一刀両断。その発想はなかった。むしろパッケージシステムに入った方が手軽。

今からCloud9の記事とCIMのコアマネジメント頑張りますね。
