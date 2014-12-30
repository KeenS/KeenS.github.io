---
categories: [Octopress, Hugo]
date: 2014-12-30T03:26:58Z
title: OctopressからHugoに移行した
---
κeenです。年末ですね。[こちらのブログ](http://deeeet.com/writing/2014/12/25/hugo/)にもあるようにOctopressの生成の遅さに閉口してたのでHugoに移行しました。
出来るだけブログのURLは変えないようにしました。でもどっかでリンク切れちゃってるかも。

<!--more-->

Octopressは遲い以外にもテンプレートのファイルが多い、Rubyのインストールと`bundle install`が必要、柔軟性がない、など不満があったのですがバイナリ配布のHugoは~/binに置くだけで良いですしテーマが気に入らなければユーザーがオーバーライドする仕組みもあるので好きにいじれます。まあ、結局テーマをフォークしちゃったんですけどね。

# 作業
Markdownとメタデータは概ね互換性があるのでcontent/post/に突っ込んで先のブログのワンライナーを回せばとりあえず動きます。URL互換性を保つために

    cd content/post
    for f in *.markdown; do mv ${f} $(basename ${f#*-*-*-} .markdown).md;done

みたいなのを回します。ヒストリ搜すの面倒なので今即興で書きました。

んで記事にカテゴリのタグが欲いのと日付をISOっぽくしたかったのでテンプレートをオーバーライド。あ、テンプレートは[liquorice](https://github.com/eliasson/liquorice/)を使いました。

# シンタックスハイライト
Hugoはデフォルトでは提供しません。サーバーサイドでやろうとするとpygmentsが遲いから。クライアントサイドだと複数あるらしいので[highlight.js](https://highlightjs.org/)にしてみました。Clojure REPLとか絶妙なものがあって良いですね。個人的にはコマンドラインのハイライトが欲かった…

# スライド
Octopressの時はテーマに手を入れるわreveal.jsのテーマは自作するわちょっと表には出せないアドホックでダーティーなプラグインを作るわしてreveal.jsに無理やりブログのテーマを載せてました。

今回はHugoテーマをフォークしてslide用のテンプレート書いて、Octopressの時に使っていたreveal.jsの自作テーマを少しいじるだけで済みました。しかもブロクにreveal.jsを埋め込む形になってます。この際[remark.js](http://remarkjs.com/)にしようかとも思ったのですがまたスライドのテーマ作るの面倒だったのでreveal.jsのままで。remark.jsだったら苦労せずとも導入出来たのかなぁ。

一番の問題はreveal.jsにはmarkdownを食わせるのですが、Hugoがmarkdownをレンダリングしちゃうところですね。Octopressではプラグインで先にスライドだけ処理しちゃってOctopressには触らせないことで実現してました。

Hugoはgoのテンプレートを使っているのですがこれまた癖があって文脈に応じてそのまま出したりレンダリングしたりHTMLエスケープしたりJSエスケープしたりします。試行錯誤する内に *markdown 全体を`<section>`で囲むとレンダリングされない* というバグっぽい挙動を見付けました。というこで汚ないんですがスライドは各記事を手動で

```html
<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">


</script>
</section>

```

で囲むことにしました。

# 雑感
速いし導入楽だしもうOctopressに戻りたいとは思わないです。あとauto refleshが便利すぎる。変更したらビルドしてブラウザのリロードまでやってくれます。デュアルディスプレイだと捗りそうですね。

不満はウィジェットが少ない、コマンド１発でデプロイ出来無い、octomacsが使えない。ソーシャルアイコン気に入ってたのに。

あと折角なのでブログエントリ全体見返して細かな修正行いました。カテゴリの統一とか。


これで今年の大掃除終わり！
