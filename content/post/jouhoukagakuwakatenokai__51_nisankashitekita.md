---
categories: ["番外編"]
date: 2018-10-08T22:34:41+09:00
title: "情報科学若手の会 #51 に参加してきた"
---

κeenです。[情報科学若手の会 #51](https://wakate.org/2018/07/28/51th-general/)に参加してきましたのでその報告です。

<!--more-->

CAのときの同期の[くろちゃん](https://twitter.com/kuro_m88)が代表幹事をやってるのでその縁でIdein社が懇親会スポンサーをし、1人分の参加権で私が行ってきました。
あとくろちゃんに個人的に発表もしてと言われたので一般発表もしてきました。

[Futureとその周辺 | Lispエイリアンの狂想曲](https://keens.github.io/slide/futuretosonoshuuhensa_bei/)

最初タイトルにサーベイも付いていたんですがサーベイというにはぞんざいすぎて恥ずかしいので削りました。
私は情報科学は全然分からないですし情報科学をやってる人に現実のプログラミング言語で悩んでる問題を知ってもらって興味を持っていただけたらなと思ってテーマを決めました。
正直想定聴衆をミスったようで、あまり伝わってなかったみたいでした。たとえば冒頭で限定継続やCPSは説明なしに喋ってましたがちゃんと伝わった人が1人もいなかった可能性すらあります。
情報科学を冠してる点、若手の会がプログラミングシンポジウムの下位組織である点からかなりプログラミング言語の概念には詳しい人が多いだろうと踏んでたのですが思った以上に多様性のある参加者でした。

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">ガチにやってる人でも分野違うと全然わからんとかありますし、なにかしらの分野を深く掘り下げている人は多いんだろうとは思いますが、情報科学って広いので、情報科学の中の特定の分野に絞った場合はなんの前提知識も持ってないっていう想定で話した方がいいのかもしれませんね</p>&mdash; さほ (@saho_bofffff) <a href="https://twitter.com/saho_bofffff/status/1048781479793197056?ref_src=twsrc%5Etfw">2018年10月7日</a></blockquote>


反省してます。あと時間足りないだろと思ってマキで喋ったら時間を余してしまった。それにスライドの準備がかなりギリギリになってしまったので全体的に完成度が低いですね。

さて、その他の発表ですがまずは1日目がプログラミング言語系。
プログラミング言語系は私の他は量子計算の話とHalide to Dataflow Architectureの話くらいでした。
Halide to Dataflow Architectureの話は問題意識の導入から取り組みまでの流れがスムーズで分かりやすいなーと関心してました。

次が主にセキュリティやCTFのセッション。特に若手特別講演で黒米さんからマルウェアの難読化と難読化解除の話がありました。

[The Art of De-obfuscation](https://speakerdeck.com/ntddk/the-art-of-de-obfuscation)

難読化と難読化解除では思った以上にプログラミング言語との関わりが深くて興味深かったです。
たとえば難読化のために(インタプリタ的意味の)VMを作ったりDirect Threadingしたりする手法があるそうです。
さらに解析のためにシンボリック実行やSMTソルバを使ったりもしてるようでした。
マルウェア解析ってバイナリとにらめっこみたいなイメージがあったので身近に感じられて新鮮でした。


次はインフラや物理の話に移っていきました。
特に招待講演は宇田さんで、例の[コンテナデータセンター](https://www.syuheiuda.com/?p=3630)の話がありました。
一般には未公開の映像なども交えて話されていました。
コンテナをデータセンターでホスティングする時代はもう古い！これからはデータセンターをコンテナでホスティングする時代！
他には高校生の発表もあり若手ですね。


夜はLTセッションがありました。LTはカジュアルですね。私はRustのエヴァンジェリストじゃないの？と圧を掛けられたので飛び込みLTしてました。

[Rust around you | Lispエイリアンの狂想曲](https://keens.github.io/slide/rust_around_you/)


というか割とRustの人として認識されてたみたいで意外でした。こういうクラスタにも知られてたんだ…。色んな分野の人がいることがわかったので色々な分野で活躍するRustの話をしました。
話をしたというか一方的に情報を叩きつけました。LTですし勢い重視で、自分の中で最速のテンポで進めました。
スライドは12ページ+リンクが大量にありますが、4分で全部終わらせてます。


3日目はAIの話。脳とDeep Learningの関係の話とimosさんのお話。


そして恙無く終わりました。台風が懸念される会でしたが大きな影響なくて大変良かったです。新幹線が少し遅れたくらい。


その他の話題としては軽井沢研修所の裏庭が植物園になってました。キノコが一杯生えてた。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">夜のお散歩。秋の七草の朝貌ときのこ達 <a href="https://t.co/RsjCfr7puY">pic.twitter.com/RsjCfr7puY</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1048495120054513664?ref_src=twsrc%5Etfw">2018年10月6日</a></blockquote>

黒くて見づらいのはヒトヨダケかな？

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">朝の散歩 <a href="https://t.co/SWMXE6UIWz">pic.twitter.com/SWMXE6UIWz</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1048723341148749824?ref_src=twsrc%5Etfw">2018年10月6日</a></blockquote>

あとは雉がいた。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">キジ <a href="https://t.co/gr8iHOnYfe">pic.twitter.com/gr8iHOnYfe</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1048723202866696194?ref_src=twsrc%5Etfw">2018年10月6日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

これ以外にも帰り道に2羽雉を見かけました。


幹事及び参加者のみなさんお疲れ様でした。
