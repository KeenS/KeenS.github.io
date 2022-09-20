---
categories: [blog, hugo, pdfjs]
date: 2022-09-21T02:08:15+09:00
title: "ブログにPDFスライドを追加した話"
---
κeenです。最近気が向いたときにInkscapeでスライドを作るようにしてるんですが、できあがったPDFの置き場に困ってたのでPDFをインラインで表示できるようにしたお話です。

<!--more-->

発想は私が自力で思い付いたものではなく、画力・博士号・油田氏のブログに導入されていたものです。

<blockquote class="twitter-tweet" data-conversation="none"><p lang="ja" dir="ltr">スライドのPDFをWebページ内で左右に送って読めるようにしたのえらくない？（えらい！）<a href="https://t.co/hNCttRujqg">https://t.co/hNCttRujqg</a></p>&mdash; 画力・博士号・油田 (@bd_gfngfn) <a href="https://twitter.com/bd_gfngfn/status/1524444078116532225?ref_src=twsrc%5Etfw">May 11, 2022</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>


それを真似して私もやってみることにしました。今のところ以下のスライドがPDFで作られています。

[DropとFutureとDropのfuture | κeenのHappy Hacκing Blog](/slide/droptofuturetodropnofuture/)

ボタンとキーボードでページ送りができますし、最悪PDFをダウンロードすることもできるので、読むだけならそこまで困らないでしょう。一応ページ数指定なんかもあります。
発表のときは手元でPDFビュワーを使えばいいので主に聴衆への資料公開のためにPDFへの直リンクを置くだけよりはマシな方法として割り切って実装しました。ブラウザ互換性とかは知りません。

使ったのはMozillaの開発している[PDF.js](https://mozilla.github.io/pdf.js/)です。exampleからコードスニペットをもってきて、`keydown` イベントでページ送りするように実装しました

https://mozilla.github.io/pdf.js/examples/


実装は[このあたり](https://github.com/KeenS/KeenS.github.io/blob/450521b8626b3483304915751126573dc117c5ef/themes/liquorice-k/layouts/slide/single_pdf.html)と[このあたり](https://github.com/KeenS/KeenS.github.io/blob/450521b8626b3483304915751126573dc117c5ef/themes/liquorice-k/layouts/partials/slide/footer_pdf.html)で、ベタッとそのままJSを書いてます。
まあ、他にコードを入れることもないですし破綻はしないでしょう。

スマホ対応は全然やってないのでスマホからだと操作しづらいと思います。
[reveal.jsのコード](https://github.com/hakimel/reveal.js/blob/master/js/controllers/touch.js)を読んで実装イメージは浮かんでるのですが、デバッグが大変そうなので腰が上がらず…。

ということで今後はマークダウンから生成した箇条書きスライド以外も作っていけたらなと思います。
