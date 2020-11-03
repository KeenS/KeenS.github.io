---
categories: [SVG, Inkscape]
date: 2020-11-03T09:54:05+09:00
title: "Inkscapeで描いたパーツをブラウザ上で合成する"
---

κeenです。最近紙芝居みたいなSVGをブログ（スライド）に載せる需要があったときに調べたのでそのメモ。
Inkscapeで描いたSVGを組み合わせます。Inkscape 1.0.1での情報です。

<!--more-->

紙芝居みたいにちょっとずつ内容が違う画像をたくさん作るのは面倒です。
Inkscapeで描くこと自体はレイヤとかを使えばパーツのon/offを切り替えられるのでいいんですが、それを画像に出力するのが面倒です。
1枚1枚どのレイヤをonにしてどのレイヤをoffにして出力先のファイル名は何々で〜というのを管理しないといけません。ちょっと気になったところがあって微調整するとまた全部出力し直しになります。
そして私のブログの管理上も画像が沢山あると面倒です。
可能ならパーツだけ出力しておいてブラウザ上で合成したいですし、画像ファイルを置くのが面倒なのでテキストでブログのソースに直打ちできると嬉しいです。そういったお話。

ざっくりいうと以下の手順で実現できます

1. 図を描く
  + このとき1パーツに1レイヤを割り当てる
2. SVGをソースにコピペ
  + 不要な要素は消してよい
3. レイヤをシンボルにする
  + このとき分かりやすいidもつける
4. 使う


実例を交じえて解説します。
# 解説
## 図を描く

適当な図をInkscapeで描きます。

![描いた図](/images/Inkscapedeegaitapa_tsuwoburauzauedegouseisuru/drawn.png)

ここで、矩形2つと矢印1つの都合3つをバラバラのパーツにしたいとします。
1パーツあたり1レイヤを割り当てます。

![レイヤの図](/images/Inkscapedeegaitapa_tsuwoburauzauedegouseisuru/layers.png)

## SVGをソースにコピペ

こういうSVGが生成されるはずです。

``` svg
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg
   xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:cc="http://creativecommons.org/ns#"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   width="512"
   height="512"
   viewBox="0 0 512 512"
   version="1.1"
   id="svg8"
   inkscape:version="1.0.1 (3bc2e813f5, 2020-09-07)"
   sodipodi:docname="test.svg">
  <defs
     id="defs2">
    <marker
       style="overflow:visible;"
       id="Arrow1Lend"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Lend"
       inkscape:isstock="true">
      <path
         transform="scale(0.8) rotate(180) translate(12.5,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path856" />
    </marker>
    <marker
       style="overflow:visible"
       id="Arrow1Lstart"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Lstart"
       inkscape:isstock="true">
      <path
         transform="scale(0.8) translate(12.5,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path853" />
    </marker>
  </defs>
  <sodipodi:namedview
     id="base"
     pagecolor="#ffffff"
     bordercolor="#666666"
     borderopacity="1.0"
     inkscape:pageopacity="0.0"
     inkscape:pageshadow="2"
     inkscape:zoom="1.4"
     inkscape:cx="75.429374"
     inkscape:cy="348.53947"
     inkscape:document-units="px"
     inkscape:current-layer="layer3"
     inkscape:document-rotation="0"
     showgrid="false"
     units="px"
     showguides="true"
     inkscape:guide-bbox="true"
     inkscape:window-width="1998"
     inkscape:window-height="1217"
     inkscape:window-x="420"
     inkscape:window-y="595"
     inkscape:window-maximized="0">
    <sodipodi:guide
       position="62.142857,367.71429"
       orientation="1,0"
       id="guide26" />
  </sodipodi:namedview>
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
  <g
     inkscape:label="layer1"
     inkscape:groupmode="layer"
     id="layer1"
     style="display:inline">
    <rect
       style="fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:3;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect72"
       width="174.63353"
       height="136.63985"
       x="62.380371"
       y="279.50797"
       ry="24.02396" />
  </g>
  <g
     inkscape:groupmode="layer"
     id="layer2"
     inkscape:label="layer2">
    <path
       style="display:inline;fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:3;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#Arrow1Lend)"
       d="m 240.5433,359.93792 c 0,0 124.40282,-15.25408 117.51317,-95.20412 C 351.16682,184.78376 236.16935,127.09817 236.16935,127.09817"
       id="path28" />
  </g>
  <g
     inkscape:groupmode="layer"
     id="layer3"
     inkscape:label="layer3">
    <rect
       style="fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:3;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect7-3"
       width="174.63353"
       height="136.63985"
       x="62.142857"
       y="63.106865"
       ry="24.02396" />
  </g>
</svg>

```

このとき冒頭の `<?xml ...?>` は不要です。 `sodipodi:namedview` と `metadata` は消してよいです。
また、 `<svg>` タグのwidth、height、viewBoxも消しとかないと場所をとってしまいます。
さらにそれに伴っていくつか不要なxmlnsが消せます。
XMLの名前空間が分からない人は適当にググって下さい。

結果、以下のようにダイエットできます。

``` svg
<svg
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   version="1.1"
   id="svg8"
   inkscape:version="1.0.1 (3bc2e813f5, 2020-09-07)">
  <defs
     id="defs2">
    <marker
       style="overflow:visible;"
       id="Arrow1Lend"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Lend"
       inkscape:isstock="true">
      <path
         transform="scale(0.8) rotate(180) translate(12.5,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path856" />
    </marker>
    <marker
       style="overflow:visible"
       id="Arrow1Lstart"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Lstart"
       inkscape:isstock="true">
      <path
         transform="scale(0.8) translate(12.5,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path853" />
    </marker>
  </defs>
  <g
     inkscape:label="layer1"
     inkscape:groupmode="layer"
     id="rect1"
     style="display:inline">
    <rect
       style="fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:3;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect72"
       width="174.63353"
       height="136.63985"
       x="62.380371"
       y="279.50797"
       ry="24.02396" />
  </g>
  <g
     inkscape:groupmode="layer"
     id="path"
     inkscape:label="layer2">
    <path
       style="display:inline;fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:3;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#Arrow1Lend)"
       d="m 240.5433,359.93792 c 0,0 124.40282,-15.25408 117.51317,-95.20412 C 351.16682,184.78376 236.16935,127.09817 236.16935,127.09817"
       id="path28" />
  </g>
  <g
     inkscape:groupmode="layer"
     id="rect2"
     inkscape:label="layer3">
    <rect
       style="fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:3;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect7-3"
       width="174.63353"
       height="136.63985"
       x="62.142857"
       y="63.106865"
       ry="24.02396" />
  </g>
</svg>

```

## レイヤをシンボルにする

上記SVG、大枠で見ると以下のような構造になっています。

``` svg
<svg>
  <defs>
    ...
  </defs>
  <g inkscape:groupmode="layer" id="..."> ... </g>
  <g inkscape:groupmode="layer" id="..."> ... </g>
  <g inkscape:groupmode="layer" id="..."> ... </g>
</svg>
```

`inkscape:groupmode="layer"` となっている `<g>` を `<symbol>` にします。
SVGで `<g>` も `<symbol>` もオブジェクトをまとめる働きをしますが、役割が異ります。
`<g>` はその場で表示します。`<symbol>` はその場には表示せずに後でIDで参照する用途で使います。

* [g - SVG: Scalable Vector Graphics | MDN](https://developer.mozilla.org/ja/docs/Web/SVG/Element/g)
* [&lt;symbol&gt; - SVG: Scalable Vector Graphics | MDN](https://developer.mozilla.org/ja/docs/Web/SVG/Element/symbol)

今回はまさに後でIDで参照したいので `<symbol>` に書き換えます。

``` svg
<svg>
  <defs>
    ...
  </defs>
  <symbol inkscape:groupmode="layer" id="..."> ... </symbol>
  <symbol inkscape:groupmode="layer" id="..."> ... </symbol>
  <symbol inkscape:groupmode="layer" id="..."> ... </symbol>
</svg>
```


また、 `<symbol>` は `<defs>` に入れた方が行儀がいいとされているようです。

* [defs - SVG: Scalable Vector Graphics | MDN](https://developer.mozilla.org/ja/docs/Web/SVG/Element/defs)

実は `<defs>` に移動するならシンボルにしなくてもいい説ありますが、まあいいでしょう。
さらに後で参照したいので `id` 属性も分かりやすいようにしておきましょう。
結果、こうなります。

``` svg
<svg>
  <defs>
    ...
    <symbol inkscape:groupmode="layer" id="rect1"> ... </symbol>
    <symbol inkscape:groupmode="layer" id="path">  ... </symbol>
    <symbol inkscape:groupmode="layer" id="rect2"> ... </symbol>
  </defs>
</svg>
```

オブジェクトだったらInkscape内でオブジェクトのプロパティからIDいじれるんですが、レイヤだとできないみたいです。

ここまでくれば準備OK。あとは使います。

## 使う
XMLにはxlinkという仕様があって、それ経由で他の `<svg>` タグのシンボルを参照できるようです。

* [XML, XLink and XPointer](https://www.w3schools.com/xml/xml_xlink.asp)
* [use - SVG: Scalable Vector Graphics | MDN](https://developer.mozilla.org/ja/docs/Web/SVG/Element/use)

例えば先程の全てのオブジェクトを参照するならこう書きます。

``` svg
<svg width="512" height="512" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#rect1" />
    <use xlink:href="#path" />
    <use xlink:href="#rect2" />
</svg>
```


それがこう表示されます。

<svg width="512" height="512" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#rect1" />
    <use xlink:href="#path" />
    <use xlink:href="#rect2" />
</svg>


一部のパーツだけの参照も可能です。

``` svg
<svg width="512" height="512" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#rect2" />
</svg>
```

<svg width="512" height="512" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#rect1" />
</svg>


いくらでも使い回せて楽しいですね。

``` svg
<svg width="512" height="512" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#rect1" />
    <use xlink:href="#path" />
</svg>
```

<svg width="512" height="512" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#rect1" />
    <use xlink:href="#path" />
</svg>

以上使い方でした。

# FAQ

## 非表示にしたオブジェクトが表示されないんだけど？

非表示にしたからですね。SVG的にはCSSで `style="display:none;"` と設定されてるはずなので `inline` にしときましょう。

## レイヤじゃなくてグループでもいいのでは？

Yes。グループだとInkscape内からオブジェクトのプロパティでID設定できるしそっちもあり。
ただし描くときにレイヤの方が扱いやすい（表示/非表示の単位が分かりやすい）し、SVGタグ内でもレイヤがトップレベルにあって探しやすいので私はこうしてます。

## テキストが表示されないんだけど？

どうやらInkscapeがSVGの未来の仕様を使ってるっぽいんですが、現実的には1.1の機能までしかサポートしてないのが原因らしいです。

[javascript - SVG Inkscape generated file does not show flowRoot objects on browser - Stack Overflow](https://stackoverflow.com/questions/19391197/svg-inkscape-generated-file-does-not-show-flowroot-objects-on-browser)

↑の問答にもあるようにInkspace内で該当テキストを選択したあと [テキスト] → [テキストに変換] でテキストオブジェクトになるので表示されます。ただしフォントが変わるのと、微妙に位置がずれます。
もしかしたらエクステンションのグリフとかを使ったらもうちょっと綺麗に解決できるのかもしれませんが、未確認です。

## SVGコピペじゃなくて外部ファイルとして読み込みたいんだけど？

`<object>` タグで読み込めます！

``` html
<object data="sample.svg" type="image/svg+xml"></object>
```

See also:

* [&lt;object&gt; - HTML: HyperText Markup Language | MDN](https://developer.mozilla.org/ja/docs/Web/HTML/Element/object)
* [HTML5でのSVGファイル操作のおさらい - Qiita](https://qiita.com/ka215/items/f9834dca40bb3d7e9c8b)

<svg
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   version="1.1"
   id="svg8"
   inkscape:version="1.0.1 (3bc2e813f5, 2020-09-07)">
  <defs
     id="defs2">
    <marker
       style="overflow:visible;"
       id="Arrow1Lend"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Lend"
       inkscape:isstock="true">
      <path
         transform="scale(0.8) rotate(180) translate(12.5,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path856" />
    </marker>
    <marker
       style="overflow:visible"
       id="Arrow1Lstart"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Lstart"
       inkscape:isstock="true">
      <path
         transform="scale(0.8) translate(12.5,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path853" />
    </marker>
  </defs>
  <symbol
     inkscape:label="layer1"
     inkscape:groupmode="layer"
     id="rect1"
     style="display:inline">
    <rect
       style="fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:3;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect72"
       width="174.63353"
       height="136.63985"
       x="62.380371"
       y="279.50797"
       ry="24.02396" />
  </symbol>
  <symbol
     inkscape:groupmode="layer"
     id="path"
     inkscape:label="layer2">
    <path
       style="display:inline;fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:3;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#Arrow1Lend)"
       d="m 240.5433,359.93792 c 0,0 124.40282,-15.25408 117.51317,-95.20412 C 351.16682,184.78376 236.16935,127.09817 236.16935,127.09817"
       id="path28" />
  </symbol>
  <symbol
     inkscape:groupmode="layer"
     id="rect2"
     inkscape:label="layer3">
    <rect
       style="fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:3;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect7-3"
       width="174.63353"
       height="136.63985"
       x="62.142857"
       y="63.106865"
       ry="24.02396" />
  </symbol>
</svg>
