---
categories: [Hugo, Mermaid.js]
date: 2017-08-27T09:57:09+09:00
title: "Hugoにシーケンス図を埋め込むその2"
---
κeenです。[昨日のエントリ](/blog/2017/08/26/hugonishi_kensuzuwoumekomu/)に[mermaid.js](https://github.com/knsv/mermaid)を試すといいよとのコメントを頂いたので試します。
<!--more-->

昨日のエントリでいう2のやつ、ブラウザ上でレンダリングしてくれます。

導入は簡単で、

headに

``` html
<link rel="stylesheet" href="https://unpkg.com/mermaid@7.0.4/dist/mermaid.forest.min.css">
```
を、

bodyの下の方に

``` html
<script src="https://unpkg.com/mermaid@7.0.4/dist/mermaid.min.js"></script>
<script>mermaid.initialize({startOnLoad:true});</script>
```

を記述するだけです。私のテーマ（liquoriceのフォーク）ではそれぞれ`custom/head.html`、`tail.html`だったんですが恐らくテーマ毎に違うと思うのでよしなにやって下さい。

そして

``` html
<div class="mermaid">
    CHART DEFINITION GOES HERE
</div>
```

と書くとmermaid.jsがこれを拾って処理してくれるようです。

例えばこれが

``` html
<div class="mermaid">
graph LR
    A --- B
    B-->C[fa:fa-ban forbidden]
    B-->D(fa:fa-spinner);
</div>
```

こう

<div class="mermaid">
graph LR
    A --- B
    B-->C[fa:fa-ban forbidden]
    B-->D(fa:fa-spinner);
</div>


あるいはこれが

``` html
<div class="mermaid">
sequenceDiagram
    Alice ->> Bob: Hello Bob, how are you?
    Bob-->>John: How about you John?
    Bob--x Alice: I am good thanks!
    Bob-x John: I am good thanks!
    Note right of John: Bob thinks a long<br/>long time, so long<br/>that the text does<br/>not fit on a row.

    Bob-->Alice: Checking with John...
    Alice->John: Yes... John, how are you?
</div>
```

こう


<div class="mermaid">
sequenceDiagram
    Alice ->> Bob: Hello Bob, how are you?
    Bob-->>John: How about you John?
    Bob--x Alice: I am good thanks!
    Bob-x John: I am good thanks!
    Note right of John: Bob thinks a long<br/>long time, so long<br/>that the text does<br/>not fit on a row.

    Bob-->Alice: Checking with John...
    Alice->John: Yes... John, how are you?
</div>

最後のやつ、左向きの矢印の頭がレンダリングされてない（SVGレベルでarrowheadがない）…
公式のデモページでもそうなのでバグか何かですかね。

ひとまず昨日の方法よりはマシになったのでこれでいこうと思います。
