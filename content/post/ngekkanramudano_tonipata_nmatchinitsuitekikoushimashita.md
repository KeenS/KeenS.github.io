---
categories: ["ML", "SML", "言語実装"]
date: 2019-11-02T16:10:02+09:00
title: "n月刊ラムダノートにパターンマッチについて寄稿しました"
---

κeenです。
[n月刊ラムダノート Vol.1, No.3(2019)](https://www.lambdanote.com/collections/n/products/nmonthly-vol-1-no-3-2019)に代数的データ型とパターンマッチの基礎の章を寄稿しました。
どんな内容か、どうやって執筆までに至ったのかを話そうと思います。
<!--more-->

# 書いたこと

目次:

| 節  | タイトル |
|-----|----------|
| 1.1 | 動作環境 |
| 1.2 | SML#のREPLで速習SML |
| 1.3 | 代数的データ型とパターンマッチ入門 |
| 1.4 | パターンマッチの意味 |
| 1.5 | パターンマッチのC言語へのコンパイル |
| 1.6 | 発展的なパターン |
| 1.7 | 代数的データ型がない言語とパターンマッチ |
| 1.8 | まとめと次回予告 |
| 1.9 | 参考文献 |

代数的データ型とパターンマッチの基礎について書きました。
まずはさしあたって必要になるSMLの文法を紹介したあと、代数的データ型やパターンマッチ、パターンがネストできること、網羅性検査や非冗長性検査などを紹介しました。
次に軽く意味論に触れ、そのままCに変換したコードを見せて「コンパイラは結構複雑なことをしてるんだよ」というのを示しました。
あとは発展編としてORパターンやパターンガード、バリアントのない代数的データ型を眺め、Common LispのoptimaやJavaのProject Amberを紹介しました。

特に意識した訳ではないんですが、パターンマッチだけでなく代数的データ型もセットで使えることの重要性を説いた内容になりました。
代数的データ型とセットで使えることで網羅性検査や非冗長性検査ができるのが大きいからですね。
その結果、タイトルにも代数的データ型が入っています。これはML系言語を使ったことのある読者なら納得するところでしょう。

また、後で経緯を説明しますが元々コンパイラのトピックとしてパターンマッチのコンパイル技法を説明する予定だったことから、どういうコードに落ちるかもちゃんと説明しています。
パターンマッチを見て、どういうアセンブラに落ちるかが分かる人は実はそんなに多くないんじゃないかなと思います。
コンパイルアルゴリズム次第ですが、最終的には決定木に落ちて、ジャンプテーブルになります。
データ構造じゃなくてコードなのでそう呼んでいいのかは分かりませんがtrie木のようなものができあがります。

興味の湧いた方は是非読んでみて下さい。

# 経緯とか

最初にn月刊ラムダノートを知ったのはプログラミング言語系のワークショップ、[PPL](https://jssst-ppl.org/workshop/2019/)でした。

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">これツイートしていいやつですか？ <a href="https://twitter.com/golden_lucky?ref_src=twsrc%5Etfw">@golden_lucky</a> <br>ラムダノートに技術文書を渡して編集しともらって出版までしてもらえるサービス開始するらしいですよ <a href="https://t.co/BOMXkywaZF">pic.twitter.com/BOMXkywaZF</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1103191327888498688?ref_src=twsrc%5Etfw">March 6, 2019</a></blockquote>

ちなみに学会誌とかの感覚だと掲載料は著者が払いますが、n月刊ラムダノートは著者が貰うらしいです。

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">当社がもらう、という発想はなかった！</p>&mdash; keiichiro shikano λ♪ (@golden_lucky) <a href="https://twitter.com/golden_lucky/status/1103193844441526272?ref_src=twsrc%5Etfw">March 6, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 


私が書くとしたらRustか言語処理系かなーと思いつつ当時まだ[実践Rust入門](https://gihyo.jp/book/2019/978-4-297-10559-4)の執筆が終わってなかったので一旦はスルーしておきました。
そして間も無く実践Rust入門が校了し、余裕ができたので寄稿することにしました。

執筆ネタとして思い当たったのがパターンマッチのコンパイル。
パターンマッチのコンパイルアルゴリズムについては以前、[ML Dayで発表した](https://keens.github.io/slide/inside_pattern_machings/)のですが、そのとき調べた限りでは日本語どころか英語でもパターンマッチのコンパイルについて書かれた教科書がありませんでした。
論文ならいくつかあるのですが、そこで参照されている大本の参考文献[^1]が絶版で手に入らないなどの惨憺たる状況で、まとめて日本語で読める資料の必要性を感じていました。

[^1]: SPJ のThe Implementation of Functional Programming Languages。SPJがPDFをMSRで公開してるので読もうと思えば読めます→ [https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf)

私のML Dayの発表資料がそうなっていればよかったのですが、スライドなので資料としては不十分なのと、準備不足で満足な発表ができなかったのでそのままではいささか以上に質が足りません。
それに論文で紹介されているアルゴリズムはデータの持ち方の工夫などが省かれて抽象的に書かれており、実際の動くコードによる解説の必要性も感じていました。

そこで本腰を入れてサーベイしてまとめて、n月刊ラムダノートに寄稿することにしました。
こういうのは中々アウトプットの目標がないと途中でモチベーションが切れたりピントのずれたサーベイをしたりしてしまいがちなので先に目標を据えるのは重要です。

また、ML系言語にあるパターンマッチの知名度が案外低く、網羅性検査や非冗長性検査を伴なわないパターンマッチしか知らない人も多いようです。
逆にML系言語を使う人はあんまりアセンブラまでは興味を払わないようで、上から順に試していくと思ってる人や `if` 文の連鎖と `switch` 文で生成されるコードが違うことを知らない人もいるようです。
そういう人に中身を見せる目的もありました。
特に最近はRubyやJavaにパターンマッチが入ろうとしていて俄に注目を浴びているのでタイミングとしても丁度よさそうです。


そうやってサーベイしはじめたのがゴールデンウィーク。
当初は前半で調べものを終えて後半で執筆し終える予定でしたが、まあ、無理ですよね。
ゴールデンウィーク一杯を使って調べものと実装を終えました。

ゴールデンウィークが終わったらひとまず準備完了ということでラムダノート社に連絡し、執筆をはじめました。このとき提案したタイトルは「コンパイラワントピック： パターンマッチ」。
内容にフォーカスしたタイトルの方がいいと、提案された改題は「パターンマッチはどうコンパイルされるか」でした。

執筆をはじめて、すぐに「あっ、これ思ったより内容多いな」と気付きます。
どう考えてもn月刊1冊に入りきらないので導入と実装以降を分ける提案がされたのが7月の中旬。
導入編を書き終えたのが8月末。
そこからラムダノート側の編集が入ったり、組版の相談をしたりののやりとりをして、最後の変更は10月の29日まで続いていました。
最終的に今回のタイトルになったのは全部の内容を書き上げたあとです。


かくしてみなさんの手元にn月刊ラムダノート Vol.1 No. 3が届けられました。

たまたま辻本さんのRubyのパターンマッチとコロケーションしたのでパターンマッチの回になりました。
しかし経緯を見てもらうと分かる通り、Rubyでパターンマッチの話題が盛り上がってるのを見て書くことにしたので偶然ではなく必然だったのかもしれません。

# n月刊ラムダノートに関するお気持ちとか

気軽に執筆して編集してもらって、出版までしてもらえるサービスが出てくれてすごく助かります。

ここ最近は技術書展の盛り上がりのおかげでエンジニア界隈で執筆ブームが来ているようです。
私も知り合いに誘われて参加を検討したことがありますが、今のところまだ行ったことはないです。

私はOSSが好きなのですが、その理由の1つに「（無料であることも含めて）自由である代わりに自己責任が伴なう」という思想にあります。
これのお陰でアウトプット側のハードルがすごく下がります。
私のブログも同じような気持ちで書いていて、割と適当なことが書かれていることもよくあります。

「無料の仕事に責任は伴わない」という言葉を裏返すと有償のものには責任が伴います。
そういった意味で、一人で書いた技術書を販売するのは少し気が引けます。
技術的内容はエンジニアをやってるならある程度は保証できる[^※]でしょうが、その他の日本語などはそうはいかないでしょう。
編集が入ってない日本語は読みづらいですし、一人でやってたら誤字脱字衍字が多発するのは目に見えてます。
そういう質の悪いと自認する内容でお金を取りたくなくて二の足を踏んでいました。
編集者の人に頼んでやってもらおうかとも考えましたが、どこで頼んだらやってくれるのか、いくらで引き受けてくれるか、そもそもやってくれるのかは知りません。

[^※]: それも本当か怪しいですが…

それに折角人に見せられる文章を書いたなら、イベントに参加してたまたま自分のブースに目が止まった数十人に読んでもらうだけじゃなくてもっと多くの人に読んで欲しいです。

あとこれは私のお金に関する感覚が狂ってるという話なんですが、人からお金を取るのが好きじゃないんですよね。
支払いを立替えたあとに、立替え元に代金を請求するのも気が引けるくらい、好きじゃないんです。
なのでできれば(直接は)お金をとらないで済む方法があると嬉しいです。

そこにくるとn月刊ラムダノートは私にとって最良の選択肢です。
書くのは技術書のイベントなんかで売ってるものと変わらないくらいの分量で、ちゃんと編集を通ります。面倒な組版や印刷、出版は全部やってくれます。
商業出版なので少なくとも数十以上は売れるでしょうし、執筆した原稿は[CC BY-NC-SA 4.0国際](https://creativecommons.org/licenses/by-nc-sa/4.0/)ライセンスの下、著者が頒布する権利も残ります[^2]。
それに私の一番やりたくない直接お金を取る部分は代わってやってくれます。
嘘偽りなく、最初にn月刊ラムダノートの告知を見たときに「あ、2,3万払えばここまでやってくれるんだ」と思いました。
それが払う方じゃなくて貰う方だったとは…。仏のように慈悲深いサービスですね。
ラムダノート社が本当にこれで儲かるのか不安になるくらい良いシステムです。

[^2]: 私の今回の原稿もどこかのタイミングで公開したいと思っています

まだn月刊ラムダノートへの寄稿はパターンマチの実装編も残ってますし、他に書きたいネタもあるので、
ある程度n月刊ラムダノートで経験を積んで、一人でも書籍を書けるようになったら私も技術書のイベントに参加してみたいなと思ってます。


n月刊ラムダノートは[寄稿者を募集している](https://www.lambdanote.com/blogs/news/n-1)そうなので興味のある方は書いてみて下さい。
因みに募集は1万字程度だそうですが、2時間くらいで書いたこのエントリは5000字くらいです。持ちネタのある方なら気軽に書けると思います。
