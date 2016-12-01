---
categories: [番外編, Advent Calendar, Advent Calendar 2016]
date: 2016-11-30T15:51:38+09:00
title: サイバーエージェントを退職しました
---

このエントリは[Ex CyberAgent Developers Advent Calendar 2016 - Adventar](http://www.adventar.org/calendars/1638)1日目の記事です。
元サイバーエージェントの人がわいわいやります。

κeenです。二年弱勤めたサイバーエージェントを退職したのでその旨について。

<!--more-->
# サイバーエージェントの思い出
サイバーエージェントに興味をもつきっかけになったのも入社することになったのも学生の頃からずっと参加し続けていた[LispMeetUp](https://lisp.connpass.com)でした。
LispMeetUpでは長らく会場としてCAのセミナールームを使わせてもらっていました。
そこでエンジニアに対して気軽に施設を提供するCAや社員の[@potix2](https://twitter.com/potix2)さんを知って、そのまま@potix2さんの手引でCAに入社する運びとなりました。
「面接でLispは口にしない方が良い。100%落ちる。」なんて言ってた割にはLisp繋りで入社しました。

* [CyberAgentに入社しました | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2015/04/10/cyberagentninyuushashimashita/)

新卒で入社してからは3ヶ月ほど研修でした。
最初の2週間ほどあるビジネス職も含めた全体研修ではなるほど、ここがCAかと思いました。しかしエンジニア研修からは見慣れた光景に戻って静かに研修を受けてました。
エンジニア研修は割と長めにあるので同期のエンジニア同士の顔を覚える余地はありました。勉強会なんかもやりました。

* [静的なメモリ管理の話。リージョン推論とλ計算からRustまで | κeenのHappy Hacκing Blog](https://keens.github.io/slide/seitekinamemorikanrinohanashi/)
* [勉強会駆動勉強で猛勉強 | κeenのHappy Hacκing Blog](https://keens.github.io/slide/benkyoukaikudoubenkyoudetakeshibenkyou/)
* [21世紀のエンジニアのためのHTTP/2入門 | κeenのHappy Hacκing Blog](https://keens.github.io/slide/21seikinoenjinianotamenoHTTP_2nyuumon/)

などなど。Linux, C, Python, Go, Java, Web, iOS, フレームワーク, Android, チーム開発なんかをやりました。Python, Go, iOSあたりは自分じゃやらなかったので面白かったです。

研修中は割と時間があったので他にも色々なことをやってました。

* [tarballベースの軽量バックアップツール'Sheltar'を作った | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2015/05/06/tarballbe_sunokeiryoubakkuapputsu_rutsukutta/)
* [Onigmoを最大49%高速化した話 | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2015/05/26/onigmowosaidai49_kousokukashitahanashi/)
* [第一級ラベルを持たない言語におけるDirect Threaded VMの実装 | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2015/05/29/daiikkyuuraberuwomotanaigengoniokerudirect_threaded_vmnojissou/)

この他にも研修期間中にブログエントリ十数本書いていたようです。下の代からは研修の内容が変わって忙しそうでした。

研修のあとの配属は@potix2さんのいる[アドテクスタジオ](https://adtech.cyberagent.io/)のAMoAdでした。トレーナの清水さんを始めとしてチームの皆さんにお世話になりました。
AMoAdはその昔、外注していたシステムを巻き取ったものなのでやることが一杯あって、JavaやScalaを書いたり自動化をやったりしました。


Scalaは全然いたことがなかったので練習がてら作ったのがこれでした。

* [Scala初心者の新卒が頑張ってLispを作ってみた | Scala Tech Blog](https://adtech.cyberagent.io/scalablog/2015/12/05/scala-lisp/)

これのおかげで型クラスがただのimplpcit parameterに見えるようになったのでそれなりに収穫はあったな、と思ってます。

AMoAdに限らずアドテクスタジオのエンジニアのボリュームゾーンは30代半ばなのでだいたい一回り近く年の離れたエンジニアに暖かく囲まれながら仕事をしていました。
どんな雰囲気なの？とたまに訊かれますがエンジニアが集う部署なのでだいたい工学部を想像して頂けたらと思います。

さて、年が明けて2016年の1月になると、@potix2さんが新しいグループを立ち上げることになって別のグループに移動してしまいました。長らくお世話になりました。
と思いきや自分もそのグループに移動になったので相変わらずお世話になることになりました。基盤開発グループってところでした。仕事と仕事の境目は一瞬仕事がない期間が産まれたりしますが、その隙を使って始めたのがRustのドキュメントの翻訳です。

* [Rustのドキュメントの翻訳プロジェクトを開始します。 - Qiita](http://qiita.com/blackenedgold/items/3b068769736e671805f0)

あたらしいチームでは社内向けマイクロサービスのようなものを作ることになりました。
私ともう一人ベテランのエンジニア([@atty303](https://twitter.com/atty303))でやる筈でしたが私のような雑草と違ってベテランは中々元のチームを抜けられないので1ヶ月ほど私一人でコードを書いてました。
Scalaにも馴れてない新卒一人で。1ヶ月後に[@atty303](https://twitter.com/atty303)さんがジョインすると、コードは書かずにインフラをやるとのことでした。なのでコードを書くのはやっぱり私一人。

私が苦戦しつつコードを書いている間にconsulやnomad、terraform、dockerなんかでインフラが構築されていき、たまに朝来たら自分のコードが消えていたり（「君のコード書き直しといたよ」）して確か5月頃リリースだったような。

因みにdockerを多く使うのにdocker-machineだと不便なのでUbuntuマシンが欲しいって言ったら却下されました。セキュリティ的に無理でした。結局VMでUbuntuを使ってました（使ってます）。

リリースしてからは要求性能も満たしそこまで大きな障害もなく平和に暮らしました。めでたしめでたし。Ubuntuは使えなかったけど。

まあ、あとは新卒研修を担当したり[インターン](https://www.cyberagent.co.jp/recruit/fresh/program_detail/id=11303&season=2016)を担当したり社内ハッカソンをやったりゼミでドローンを飛ばしたり新卒のトレーナーをやったりライブラリの選書をやってコンパイラの本を大量に入荷したりと本業以外も色々やってました。長くなるのでこの辺は省きますね。

# 退職に際して

よく、「合わなかったの？」と言われますがそんなことはないです。（少くとも）アドテクスタジオはエンジニアにとっては非常に働きやすい場所でしたし

* [adtech studio - Photo Tour｜FEATURES](https://www.cyberagent.co.jp/features/detail/id=12837)

CAの一般的イメージはそういう部分だけ切り取ってメディアに出してるんだと思います。
エンジニアから見たら先述の通り工学部のような雰囲気です。Ubuntuは使えないけど。サーバもMacやWindowsで動いてるんですかね。

むしろCAに入って良かったなと思える部分は一杯あって、ベテランのエンジニアに色々教えて貰ったりだとかVMwareのライセンスを買ってくれたりだとか[ICFPに行かせてもらったり](https://adtech.cyberagent.io/techblog?s=icfp)だとか。

同期エンジニアの繋りというのもすごい良くて、得意分野も部署も（今となっては）会社も色々にある60人ほどいるエンジニアとの人脈が無条件に作れるというのは新卒で入らないと出来ないなと思います。
困ったことがあったら同期Slackに投げると数分で解決します。

最後は雑草が枯れるように誰にも気付かれないように消えていくつもりでしたがそうもいかず、大勢の方々に送り出して頂きました。

<img alt="集合写真" src="/images/ca/mall.jpg" style="width: 100%">

私の周りにいるのがお世話になった開発責任者やトレーナー、トレーニーなどですね。

色紙とプレゼントも頂きました。ありがとうございます。

<img alt="プレゼントの写真" src="/images/ca/presents.jpg" style="width: 100%">

退職の旨は業務上必要な方以外には知らせてなかったのでまさか色紙が来るとは思ってませんでした。
多分関係しそうな人にDMを送って集めたんだと思います。ありがとうございます。
プレゼントは麻の模様の風呂敷です。
私はカバン代わりにPCを入れるにも旅行に行くにも風呂敷を使う人です:)

そして女性エンジニアの方([@iyunoriue](https://twitter.com/iyunoriue))からFOUNDRYの詰め合わせも頂きました。

<img alt="FOUNDRYの写真" src="/images/ca/foundry.jpg" style="width: 100%">

スイーツが好きなので非常に嬉しいです。ありがとうございます。抹茶と合いそう。


また、退職に合わせて色々なものも引退することになりました。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">Shibuya.lispの現運営が引退するので <a href="https://twitter.com/hashtag/lispmeetup?src=hash">#lispmeetup</a> がなくなります。引き継いでくれる方を募集しています。残りはClojure回、CL回です。Schemeは今月が最後でした。<a href="https://t.co/o4vv9DRFal">https://t.co/o4vv9DRFal</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/803599706475507716">2016年11月29日</a></blockquote>

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">そろそろ私も野生のLisper引退した方がいい。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/803601106693521408">2016年11月29日</a></blockquote>


お世話になった方々本当にありがとうございました。

# 次の話と退職理由

とりあえず12月中は有給消化で、1月から働きます。給料はまあまあ上がります。どうやらコンパイラ関係の仕事をしそうです。
世の中そんな仕事があるのか自分でも半信半疑ですがあるようです。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">そういえばICFPのときにкeenさんと話して「えっコンパイラが書けるシゴトあると思ってんの!?」と冗談半分で言われたな．</p>&mdash; gfn (@bd_gfngfn) <a href="https://twitter.com/bd_gfngfn/status/780057500485681153">2016年9月25日</a></blockquote>

私のことを知ってる方なら「コンパイラの仕事があるから」で十分退職理由として納得頂けると思います。
技術的にも人生的にもチャレンジングですがまだ20代前半なので後先考えずに行動してもどうにかなるかな、とか思ってます。
地味に社内でRustも使われているようなのでそこも狙っていこうと思います。
あ、こういうのもやってます。興味のある方はお願いします。

* [言語実装 Advent Calendar 2016 - Qiita](http://qiita.com/advent-calendar/2016/lang_dev)
* [Rust その2 Advent Calendar 2016 - Qiita](http://qiita.com/advent-calendar/2016/rust-lang-2)

次の仕事についてはまた入社してから書こうと思いますが、ひとまずの報告として渋谷からは離れます。本郷キャンパスに近いところに引っ越す予定です。付近の人はなんか誘って下さい。

[http://amzn.asia/0JcDzIf](http://amzn.asia/0JcDzIf)

<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

お疲れ様でした
