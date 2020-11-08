---
categories: [Inkscape, SVG, website]
date: 2020-11-09T03:10:33+09:00
title: "Inkscapeを使ってSVGからOGPイメージを半自動生成する"
---
κeenです。重い腰を上げてブログ記事のOGPを設定するようにしたので共有します。

<!--more-->

やりたいこととしてはQiitaのOように記事のタイトルを使った画像を生成し、[OGP](https://ogp.me)に設定するというもの。あと[TwitterCard](https://developer.twitter.com/en/docs/twitter-for-websites/cards/overview/abouts-cards)も。
TwitterやSlackなどで共有したときに目立ちます。

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">ウワサのBlawnを触ってみた on <a href="https://twitter.com/Qiita?ref_src=twsrc%5Etfw">@Qiita</a> <a href="https://t.co/yv6DC4Mjal">https://t.co/yv6DC4Mjal</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1187046763909136384?ref_src=twsrc%5Etfw">October 23, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

実はこれには先駆者がいて、[@ladicle](https://twitter.com/Ladicle)さんがやっています。

[HugoでもTwitterCard画像を自動生成したい](https://ladicle.com/post/20200623_164459/)

最初はこれをそのまま使おうとしたのですが、ふとSVGならもっと簡単にできるのではと試してみた次第です。
どういうことかというと

* 画像のテンプレートは決まっていてテキストを差し替えるだけ
* SVGはテキストデータなのでテンプレートエンジンで扱える

という観測からブログのメタデータを元にテンプレートエンジンでSVGを生成し、それをInkscapeでPNGに変換してあげればお手軽に画像を作れるのではという発想です。

# デザイン

TwitterやFacebookでは600x315、あるいは高解像度に対応したいなら1200x630を使うとよいらしいので1200x630のキャンバスを使ってSVGでデザインします。

また、私のブログはHugoを使っているので全ての記事で以下のメタデータが取得できます。

``` yaml
categories: [<tag1>, <tag2>, ...]
date: <datetime>
title: <title>
```

これらを使うことにし、今回のデザインはブログのテーマにあわせてこんな感じにしてみました。
なんかSVGそのまま貼ったら色々とずれてますが御愛嬌。手元では位置とかフォントとかはちゃんとしてます。

<svg xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:cc="http://creativecommons.org/ns#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:svg="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/2000/svg" width="1200" height="630" viewBox="0 0 1200 630" version="1.1" id="svg8">
  <defs id="defs2">
    <rect x="241.72642" y="398.67746" width="114.40255" height="47.501395" id="rect848" />
    <rect x="1.4978614" y="188.50214" width="1197.6794" height="446.49246" id="rect842" />
    <rect x="97.142857" y="1.1980329" width="1104.3592" height="187.3041" id="rect836" />
    <rect x="241.72643" y="398.67746" width="88.996589" height="55.600498" id="rect848-2" />
    <rect x="241.72643" y="398.67746" width="368.69037" height="38.972515" id="rect848-6" />
    <filter style="color-interpolation-filters:sRGB;" id="filter112">
      <feFlood result="result1" flood-color="rgb(240,240,240)" flood-opacity="1" id="feFlood100" />
      <feMerge result="result3" id="feMerge110">
        <feMergeNode in="result1" id="feMergeNode106" />
        <feMergeNode in="SourceGraphic" id="feMergeNode108" />
      </feMerge>
    </filter>
    <filter style="color-interpolation-filters:sRGB;" id="filter126">
      <feFlood result="result1" flood-color="rgb(240,240,240)" flood-opacity="1" id="feFlood114" />
      <feMerge result="result3" id="feMerge124">
        <feMergeNode in="result1" id="feMergeNode120" />
        <feMergeNode in="SourceGraphic" id="feMergeNode122" />
      </feMerge>
    </filter>
    <filter style="color-interpolation-filters:sRGB;" id="filter140">
      <feFlood result="result1" flood-color="rgb(240,240,240)" flood-opacity="1" id="feFlood128" />
      <feMerge result="result3" id="feMerge138">
        <feMergeNode in="result1" id="feMergeNode134" />
        <feMergeNode in="SourceGraphic" id="feMergeNode136" />
      </feMerge>
    </filter>
    <filter style="color-interpolation-filters:sRGB;" id="filter154">
      <feFlood result="result1" flood-color="rgb(240,240,240)" flood-opacity="1" id="feFlood142" />
      <feMerge result="result3" id="feMerge152">
        <feMergeNode
           in="result1"
           id="feMergeNode148" />
        <feMergeNode
           in="SourceGraphic"
           id="feMergeNode150" />
      </feMerge>
    </filter>
  </defs>
  <metadata
     id="metadata5">
    <rdf:RDF>
      <cc:Work
         rdf:about="">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource="http://purl.org/dc/dcmitype/StillImage" />
        <dc:title />
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g id="layer1">
    <text xml:space="preserve" id="text846" style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-size:32px;line-height:134%;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;letter-spacing:0px;word-spacing:0px;white-space:pre;shape-inside:url(#rect848);fill:#aaaabb;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;filter:url(#filter140);" transform="translate(562.06512,-194.72063)"><tspan x="256.66992" y="428.4713"><tspan style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-size:32px;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';fill:#aaaabb;fill-opacity:1">Linux</tspan></tspan></text>
    <rect style="fill:#202020;fill-opacity:1;stroke:#1a1a1a;stroke-width:2.99572;stroke-linecap:round" id="rect7" width="1200.0043" height="187.00427" x="1.4978614" y="1.4978614" ry="0" />
    <text xml:space="preserve" id="text834" style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-size:64px;line-height:134%;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;letter-spacing:0px;word-spacing:0px;white-space:pre;shape-inside:url(#rect836);fill:#ffffff;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;" x="-3.453125" y="0" transform="translate(-49.947268,43.227731)"><tspan x="234.6192" y="68.397267"><tspan style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;text-anchor:middle;fill:#ffffff">κ</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;text-anchor:middle;fill:#ffffff">een</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;text-anchor:middle;fill:#ffffff">の</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;text-anchor:middle;fill:#ffffff">Happy Hac</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;text-anchor:middle;fill:#ffffff">κ</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;text-anchor:middle;fill:#ffffff">ing Blog</tspan></tspan></text>
    <text xml:space="preserve" id="text840" style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:85.3333px;line-height:134%;font-family:'Open Sans';-inkscape-font-specification:'Open Sans, Normal';font-variant-ligatures:normal;font-variant-caps:normal;font-variant-numeric:normal;font-variant-east-asian:normal;text-align:center;letter-spacing:0px;word-spacing:0px;white-space:pre;shape-inside:url(#rect842);fill:#000000;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;" transform="translate(1.6185102,156.2748)"><tspan
         x="151.33873" y="278.10192"><tspan style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:85.3333px;font-family:'Open Sans';-inkscape-font-specification:'Open Sans, Normal';font-variant-ligatures:normal;font-variant-caps:normal;font-variant-numeric:normal;font-variant-east-asian:normal">Linux</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:85.3333px;font-family:'Open Sans';-inkscape-font-specification:'Open Sans, Normal';font-variant-ligatures:normal;font-variant-caps:normal;font-variant-numeric:normal;font-variant-east-asian:normal">で</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:85.3333px;font-family:'Open Sans';-inkscape-font-specification:'Open Sans, Normal';font-variant-ligatures:normal;font-variant-caps:normal;font-variant-numeric:normal;font-variant-east-asian:normal">発表動画</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:85.3333px;font-family:'Open Sans';-inkscape-font-specification:'Open Sans, Normal';font-variant-ligatures:normal;font-variant-caps:normal;font-variant-numeric:normal;font-variant-east-asian:normal">を</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:85.3333px;font-family:'Open Sans';-inkscape-font-specification:'Open Sans, Normal';font-variant-ligatures:normal;font-variant-caps:normal;font-variant-numeric:normal;font-variant-east-asian:normal">撮</tspan><tspan style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:85.3333px;font-family:'Open Sans';-inkscape-font-specification:'Open Sans, Normal';font-variant-ligatures:normal;font-variant-caps:normal;font-variant-numeric:normal;font-variant-east-asian:normal">る</tspan></tspan></text>
    <text xml:space="preserve" style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:24px;line-height:134%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification: '源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#999999;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" x="245.55283" y="229.44456" id="text2451"><tspan id="tspan2449" x="245.55283" y="229.44456">2020-11-08</tspan></text>
    <text xml:space="preserve" id="text846-7" style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-size:32px;line-height:134%;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;letter-spacing:0px;word-spacing:0px;white-space:pre;shape-inside:url(#rect848-2);fill:#aaaabb;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;filter:url(#filter126);" transform="translate(488.92043,-198.90244)"><tspan x="254.22467" y="432.27773"><tspan style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-size:32px;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';fill:#aaaabb;fill-opacity:1">動画</tspan></tspan></text>
    <text xml:space="preserve" id="text846-1" style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-size:32px;line-height:134%;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;letter-spacing:0px;word-spacing:0px;white-space:pre;shape-inside:url(#rect848-6);fill:#aaaabb;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;filter:url(#filter112);" transform="translate(111.62938,-197.51816)"><tspan x="251.53223" y="428.4713"><tspan>SimpleScreenRecorder</tspan></tspan></text>
    <text xml:space="preserve" id="text846-4" style="font-style:normal;font-variant:normal;font-weight:600;font-stretch:normal;font-size:32px;line-height:134%;font-family:'Open Sans';-inkscape-font-specification:'Open Sans Semi-Bold';text-align:center;letter-spacing:0px;word-spacing:0px;white-space:pre;fill:#aaaabb;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;filter:url(#filter154)" x="683.97058" y="-207.09036"><tspan x="945.84753" y="221.38089" id="tspan95"><tspan id="tspan93">Shotcut</tspan></tspan></text>
  </g>
</svg>


# 生成

これであとはテンプレートにあてはめるだけです。テンプレートエンジンはRubyのERBを使うことにします。
メンテナンスしやすいようにスタイルを `<style>` タグに切り出すなどして整理して以下のような形になりました。

``` svg
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg width="1200" height="630" viewBox="0 0 1200 630" version="1.1" id="svg8" >
  <defs id="defs2">
      ....
  </defs>
  <metadata
     id="metadata5">
    <rdf:RDF>
      <cc:Work
         rdf:about="">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource="http://purl.org/dc/dcmitype/StillImage" />
        <dc:title />
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <style id="style11">
      ....
  </style>
  <g inkscape:label="レイヤー 1" inkscape:groupmode="layer" id="layer1">
    <rect id="header" width="1200" height="190" x="0" y="0" ry="0" style="fill:#202020;fill-opacity:1;stroke:#1a1a1a;stroke-width:2.99572;stroke-linecap:round" />
    <text id="site-name" class="site-name" x="186" y="120" xml:space="preserve">κeenのHappy Hacκing Blog</text>
    <text id="title" class="title" x="0" y="0" xml:space="preserve"><%= title %></text>
    <text id="date" class="date" x="246" y="229" xml:space="preserve"><%= date %></text>
    <% if tags %>
    <%   tags.each_with_index do |tag, i| %>
    <text id="tag<%= i %>" class="tag" x="320" y="229" xml:space="preserve"><%= tag %></text>
    <%   end %>
    <% end %>
  </g>
</svg>

```


これであとは記事のメタデータを抜き出してテンプレーティングするRubyスクリプトを書くだけ！
…としたかったのですが、2つ問題がありました。

1. タグ数が可変長
2. タイトルが可変長

これらをどう対処したかを紹介します。

## タグ数が可変長問題

SVGはグラフィックなのでHTMLのように自動で配列してくれたりはしません。全部X座標Y座標を与えてあげる必要があります。
ところでタグ数が可変長な（かつ個々のタグの長さも異なる）ので、2つ目以降のタグの置き場が簡単には分かりません。
InkscapeのUI上だったら等間隔に並べるような処理もありますがこれはInkscapeが画像として表示したときの横幅をメモリ内に持っているからできる芸当です。

ではどうしたかというと、Inkscapeを使いました。
実はInkscapeのCLIにはヘッドレスでInkscapeに問合せたり、UI上で操作できることの一部をできたりします。
残念ながちょうど欲しかった等間隔に並べる処理がCLIからできなかったのですが、個々のタグの横幅を取得することは可能です。
さらに、ノードのIDを指定して座標を動かすこともできるのでこれらを使ってどうにかします。
すなわち

1. Inkscapeに（座標は仮のまま）画像データを読み込ませてサイズなどを計算させる
2. Inkscapeに各タグのノードの横幅を問い合わせる
3. Inkspaceに各タグのX座標を「前のタグのX座標+前のタグの横幅」にするよう指示を出す
4. (その他の処理もしたあと)PNG画像をエクスポートさせる

という処理をします。

1., 2.は以下のコマンドで実現できます。

``` text
inkscape file.svg --actions='select-by-selector:.tag;query-width' 2> /dev/null
```

`--actions=` に続いてInkscapeに処理させるアクションを指定します。
まずは `select-by-selector` でタグを表わすノードを全て選択します。
CSSセレクタが使えるんですね。
今回指定しているのは `.tag` です。複数該当するので複数選択されます。
ちょっとこの辺のCSSの仕様は詳しくないのですが、多分XMLドキュメントに登場した順のはずです。
そして `query-width` で選択したノード全ての横幅をカンマ区切りで出力します。
あとはこれを受け取ってシンプルに `,` で分割して浮動小数点数としてパースすればOKです。


次がそれぞれのタグノードの位置を適切な場所に動かします。
タグはERBでテンプレーティングするときに `tag0` 、 `tag1` 、…とIDを振ってあるのでそれを使います。
動かすアクションは `transform-translate:x,y` です。
これはRuby側で位置を計算して、Inkscapeに指示を出します。

``` ruby
widths = さっき取得したタグの横幅たち
# 最後のタグの横幅は不要
widths.pop
dx = 0
i = 0
actions = ""
for width in widths do
  # タグの間は6px空ける
  dx += width + 6
  i +=1
  actions += "select-by-id:tag#{i};transform-translate:#{dx},0;unselect-by-id:tag#{i};"
end

# このあとこんな感じのことをする
# system ""inkscape file.svg --actions='#{actions}' 2> /dev/null"
```

都度Inkscapeを起動してもいいのですが、少し起動が重いので全てのタグ分のアクションを用意して最後にまとめてInkscapeに指示を出しています。

`select-by-id` でタグをID指定で選択したあと、都度 `unselect-by-id` で選択を外しています。
これはselectしたあともう1度selectすると複数選択扱いになるのでunselectしないと以前操作したタグも動いてしまうからです。

また、 actionで行なった操作は別途ファイルに保存する操作をしない限り元のSVGには反映されず、Inkscapeのプロセスが終了したら揮発してしまいます。
なのでファイルに上書き保存するかPNGのエクスポートまで一気にやってしまうかですが、今回はPNGのエクスポートまで一気にやってしまう方針をとりました。プロセス起動は少ない方がいいですしね。

ひとまず、タグの問題は解決しました。

## タイトルが可変長問題

実はこれはあんまりちゃんと解決できてないです。

タイトルが長すぎて横幅に収まりきらない場合に改行が必要になります。SVG側で自動折り返しとかやってくれたら嬉しかったんですが、残念ながら現行の仕様にはなさそうでした。
頑張って自前で折り返しを実装します。

自前の実装はとてもお粗末なものになりました。
ひとまず改行なしでInkscapeに読み込んで横幅を計算してもらって、はみ出そうだったらタイトルの真ん中あたりに改行文字を入れます。幸いにも改行文字は扱ってくれるようでした。
中にはタイトルが長すぎて3行以上になる記事もあるのでそこは2箇所、3箇所と改行文字を入れています。

この処理は禁則とかなんとかを一切無視しているので生成されるタイトルが非常に残念なことになっています。
また「真ん中あたり」を文字数でカウントしてるので横幅の狭いアルファベットと幅広な日本語文字でバランスが取れてないケースもあります。

さらに改行すると中心位置がずれるので、座標計算も必要になります。これがちょっと面白かったので紹介します。

今回のテキストにはスタイルで `text-align: center` を当てています。するとどうやらX座標はテキストオブジェクトの中心がオブジェクトの座標になるらしく、そのままキャンバスの中心を設定するだけでよかったです。
一方でY座標は入り組んでいました。どうやら1行目のbase lineがY座標になるらしく、「中心からテキストオブジェクトの高さの半分だけ上にいったところからさらに1行の高さ分だけ下」のような計算になりました。

このあたりのつらさは先程貼ったladicleさんの記事でも書かれていますね。
本当は禁則処理をもうちょっと賢くやりたかったのですが、Inkscapeへの問い合わせが重いので精緻な計算はしづらいだろうということで妥協した実装になっています。

# 結果

上記のごちゃごちゃをやってInkscapeにPNGでエクスポートさせるRubyスクリプトを書きました。
結果、以下のような画像ができあがります。

![生成されたOGPイメージ](/images/ogp/post/Linuxdehappyoudougawotoru.png)

あとはこれを記事のファイル名とbasenameが同じになるように画像を生成して、Hugoのテンプレート側で対応するOGP用画像があればOGPのメタタグを生成するというようなことをしています。

これでよいはずなんですが、どうしてかTwitterでだけプレビューがうまくいきません。
TwitterはTwitterCardという独自のフォーマットなのでなんかその辺が原因かなーと思いつつよく分かってません。

記事からPNGを生成するまでは自動化できましたが、記事を書いて上記のツールを叩いて…と生成のトリガは手動になっています。なので「半自動生成」でした

# 今後

まずは記事書いたら自動でOGP用の画像が生成できるようにしたいですね。

次は高速化かな？
今、1画像を生成するのに3〜4回Inkscapeを起動していますがこれを減らしたいです。
多分 `inkscape --shell` と  `file-open` / `file-close` とかを上手く組み合わせれば複数の記事の処理の中で1回だけInkscapeを起動するということも可能かもしれません。

あとは禁則処理ですね。
Inkscapeの扱いが高速化できたら細かな処理も可能になると思うので挑戦したいです。
あるいは `libicu` とかを触ってみるいい機会かもしれません


# おまけ
## SVG画像をプレビューする

意外とSVGって画像ビュワーで表示できないんですよね。
でも大丈夫。ブラウザがあります。

``` text
$ firefox file.svg
```

あとはInkscapeに付属の `inkview` というコマンドでも表示できます。

``` text
$ inkview
```

こっちはInkscapeと同じSVGエンジンを使ってるのでエクスポート画像の仕上がりを事前に確認したいとかのケースでは有用そうです。

## SVGで背景を設定する

タグの部分に背景がついてるのに気付きましたか？
実はこれ、HTMLと違って結構設定するのが難しかったです。
SVGのフィルタのうち、 `feFlood` で設定できます。
今回の設定は以下のようになっています。

``` svg
<filter
   inkscape:label="Fill Background"
   inkscape:menu="Fill and Transparency"
   inkscape:menu-tooltip="Adds a colorizable opaque background"
   style="color-interpolation-filters:sRGB;"
   x="-0.015"
   width="1.03"
   id="filter154">
  <feFlood
     result="result1"
     flood-color="rgb(240,240,240)"
     flood-opacity="1"
     id="feFlood142" />
  <feMerge
     result="result3"
     id="feMerge152">
    <feMergeNode
       in="result1"
       id="feMergeNode148" />
    <feMergeNode
       in="SourceGraphic"
       id="feMergeNode150" />
  </feMerge>
</filter>
```

蓋を開けてみればなるほどといったところなんですが、Inkscapeで設定した背景が不必要に複雑なXMLになっていて、タグの文字を差し替えたときに背景がついてきてくれないなどのトラブルがありました。
また、 `filter` に `x` と `width` を設定していますがこれがないと長いタグ名のときに横に背景が広がってしまうというよく分からない現象に遭遇しました。今でもなんなのかよく分かってないです。

## InkscapeのCLIでできる操作

actionとverbというのがあるようです。
見た感じactionがCLI向けに用意されたコマンドで、verbがInkscapeのGUIでの処理に対応するコマンドっぽいです。
今Wikiを見たらverbは将来actionに置き換えられると書かれてました。

ひとまずactionは以下のものが定義されています。


``` text
$ inkscape --action-list
action-list         :  Print a list of actions and exit.
convert-dpi-method  :  Import DPI convert method.
export-area         :  Export area.
export-area-drawing :  Export drawing area.
export-area-page    :  Export page area.
export-area-snap    :  Export snap area to integer values.
export-background   :  Export background color.
export-background-opacity:  Export background opacity.
export-do           :  Do export.
export-dpi          :  Export DPI.
export-filename     :  Export file name.
export-height       :  Export height.
export-id           :  Export id(s).
export-id-only      :  Export id(s) only.
export-ignore-filters:  Export ignore filters.
export-latex        :  Export LaTeX.
export-margin       :  Export margin.
export-overwrite    :  Export over-write file.
export-pdf-version  :  Export PDF version.
export-plain-svg    :  Export as plain SVG.
export-ps-level     :  Export PostScript level.
export-text-to-path :  Export convert text to paths.
export-type         :  Export file type.
export-use-hints    :  Export using saved hints.
export-width        :  Export width.
file-close          :  Close active document.
file-new            :  Open new document using template.
file-open           :  Open file.
inkscape-version    :  Print Inkscape version and exit.
no-convert-baseline :  Import convert text baselines.
object-set-attribute:  Set or update an attribute on selected objects. Usage: object-set-attribute:attribute name, attribute value;
object-set-property :  Set or update a property on selected objects. Usage: object-set-property:property name, property value;
object-to-path      :  Convert shapes to paths.
object-unlink-clones:  Unlink clones and symbols.
open-page           :  Import page number.
query-all           :  Query 'x', 'y', 'width', and 'height'.
query-height        :  Query 'height' value(s) of object(s).
query-width         :  Query 'width' value(s) of object(s).
query-x             :  Query 'x' value(s) of selected objects.
query-y             :  Query 'y' value(s) of selected objects.
quit-inkscape       :  Immediately quit Inkscape.
select              :  Select by ID (Deprecated)
select-all          :  Select all. Options: 'all' (every object including groups), 'layers', 'no-layers' (top level objects in layers), 'groups' (all groups including layers), 'no-groups' (all objects other than groups and layers, default).
select-by-class     :  Select by class
select-by-element   :  Select by SVG element (e.g. 'rect').
select-by-id        :  Select by ID
select-by-selector  :  Select by CSS selector
select-clear        :  Selection clear
select-invert       :  Invert selection. Options: 'all', 'layers', 'no-layers', 'groups', 'no-groups' (default).
select-list         :  Print a list of objects in current selection.
system-data-directory:  Print system data directory and exit.
transform-remove    :  Remove any transforms from selected objects.
transform-rotate    :  Rotate selected objects by degrees.
transform-scale     :  Scale selected objects by scale factor.
transform-translate :  Translate selected objects (dx,dy).
unselect            :  Unselect by ID (Deprecated)
unselect-by-id      :  Unselect by ID
user-data-directory :  Print user data directory and exit.
vacuum-defs         :  Remove unused definitions (gradients, etc.).
verb                :  Execute verb(s).
verb-list           :  Print a list of verbs and exit.
window-close        :  Close the active window.
window-open         :  Open a window for the active document. GUI only.
```


基本的な処理はできるといったところです。
因みにverbはInkscapeのGUIの個々の操作に対応するので数だけはたくさんあります。

``` text
$ inkscape --verb-list | wc -l
1227
```

とはいえGUIがないと動かないverbも多いのでCLIから使えるのはそのうちの一部になります。
まあ、 GUI起動してしまえばいいって話もありますが。
