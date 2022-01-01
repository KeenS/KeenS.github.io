---
categories: [SVG]
date: 2022-01-02T01:18:49+09:00
title: "SVGのアニメーションのお勉強"
---

κeenです。InkscapeとかでSVGをよく扱うので表現の幅を増やすためにSVGのアニメーションを触ってみます。

<!--more-->

SVGを動かすにはCSSで指定するやつとSVGの [`<animate>`](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animate)などのタグを使うやつがありますが、今回は `<animate>` 系の方です。

`<animate>` 系は [`<animateMotion>`](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animateMotion) 、 [`<animateTransform>`](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animateTransform) があります。歴史的には `<animateColor>` もあるようですが `<animate>` に統合されたようです。

主に `<animate>` が属性を変化させるもの、 `<animateMotion>` が位置の移動に特化したもの、 `<animateTransform>` が変換をするやつっぽいですね。

## <animate>
まずはMDNに載ってるやつをば。

```svg
<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <rect width="10" height="10">
    <animate attributeName="rx" values="0;5;0" dur="10s" repeatCount="indefinite" />
  </rect>
</svg>

```

<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <rect width="10" height="10">
    <animate attributeName="rx" values="0;5;0" dur="10s" repeatCount="indefinite" />
  </rect>
</svg>

`rx` を0〜5の間でいったりきたりさせてます。元の `rect` が10なので`rx`が5だと丁度円になるのかな。ループで繋がるように `0;5;0` と0からはじめて0に戻るようになっていて、 `repeatCount="indefinite"` で無限ループさせています。

当然、パラメータをいじればループを速くすることもできます。

```svg
<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <rect width="10" height="10">
    <animate attributeName="rx" values="0;5;0" dur="3s" repeatCount="indefinite" />
  </rect>
</svg>

```

<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <rect width="10" height="10">
    <animate attributeName="rx" values="0;5;0" dur="3s" repeatCount="indefinite" />
  </rect>
</svg>


恐らく数値を取るattributeなら何でもanimateできるっぽくてぼかしエフェクトのぼかし度合いもanimateできます。

```svg
<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <filter id="blur">
    <feGaussianBlur stdDeviation="2">
        <animate attributeName="stdDeviation" from="0" to="5" dur="3s" repeatCount="indefinite" />
    </feGaussianBlur>
  </filter>
  <rect width="10" height="10" filter="url(#blur)">
  </rect>
</svg>

```

<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <filter id="blur">
    <feGaussianBlur stdDeviation="2">
        <animate attributeName="stdDeviation" from="0" to="5" dur="3s" repeatCount="indefinite" />
    </feGaussianBlur>
  </filter>
  <rect width="10" height="10" filter="url(#blur)">
  </rect>
</svg>


値の変化は線型だけでなく、 [`calcMode`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/calcMode) で指定できます。極端には "discrete" を指定するとパッパッと切り替わります。

```svg
<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <filter id="blur2">
    <feGaussianBlur stdDeviation="2">
        <animate attributeName="stdDeviation" from="0" to="5" dur="3s" repeatCount="indefinite" calcMode="discrete" />
    </feGaussianBlur>
  </filter>
  <rect width="10" height="10" filter="url(#blur)">
  </rect>
</svg>

```

<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <filter id="blur2">
    <feGaussianBlur stdDeviation="2">
      <animate attributeName="stdDeviation" from="0" to="5" dur="3s" repeatCount="indefinite" calcMode="discrete"/>
    </feGaussianBlur>
  </filter>
  <rect width="10" height="10" filter="url(#blur2)">
  </rect>
</svg>

これは非数値にも適用できるので丸と四角を交互に表示とかもできます。

```svg
<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <rect width="10" height="10">
    <animate attributeName="display" from="none" to="inline" dur="3s" repeatCount="indefinite" calcMode="discrete"/>
  </rect>
  <circle cx="5" cy="5" r=5>
    <animate attributeName="display" from="inline" to="none" dur="3s" repeatCount="indefinite" calcMode="discrete"/>
  </circle>
</svg>
```

<svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <rect width="10" height="10">
    <animate attributeName="display" from="none" to="inline" dur="3s" repeatCount="indefinite" calcMode="discrete"/>
  </rect>
  <circle cx="5" cy="5" r=5>
    <animate attributeName="display" from="inline" to="none" dur="3s" repeatCount="indefinite" calcMode="discrete"/>
  </circle>
</svg>


`values` で複数値を指定するときにそれぞれの値にどのくらいの時間をかけて変化するかを指定できる [`keyTimes`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keyTimes) もあります。MDNからのコピペ。

```svg
<svg viewBox="0 0 120 120" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <circle cx="60" cy="10" r="10">
    <animate attributeName="cx" dur="4s" repeatCount="indefinite"
        values="60; 110; 60; 10; 60" keyTimes="0; 0.25; 0.5; 0.75; 1"/>
    <animate attributeName="cy" dur="4s" repeatCount="indefinite"
        values="10; 60; 110; 60; 10" keyTimes="0; 0.25; 0.5; 0.75; 1"/>
  </circle>
</svg>
```

<svg viewBox="0 0 120 120" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <circle cx="60" cy="10" r="10">
    <animate attributeName="cx" dur="4s" repeatCount="indefinite"
        values="60; 110; 60; 10; 60" keyTimes="0; 0.25; 0.5; 0.75; 1"/>
    <animate attributeName="cy" dur="4s" repeatCount="indefinite"
        values="10; 60; 110; 60; 10" keyTimes="0; 0.25; 0.5; 0.75; 1"/>
  </circle>
</svg>

これを[`keySplines`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keySplines)にすると緩急がつきます。

```svg
<svg viewBox="0 0 120 120" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <circle cx="60" cy="10" r="10">
    <animate attributeName="cx" dur="4s" calcMode="spline" repeatCount="indefinite"
        values="60; 110; 60; 10; 60" keyTimes="0; 0.25; 0.5; 0.75; 1"
        keySplines="0.5 0 0.5 1; 0.5 0 0.5 1; 0.5 0 0.5 1; 0.5 0 0.5 1"/>
    <animate attributeName="cy" dur="4s" calcMode="spline" repeatCount="indefinite"
        values="10; 60; 110; 60; 10" keyTimes="0; 0.25; 0.5; 0.75; 1"
        keySplines="0.5 0 0.5 1; 0.5 0 0.5 1; 0.5 0 0.5 1; 0.5 0 0.5 1"/>
  </circle>
</svg>
```


<svg viewBox="0 0 120 120" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <circle cx="60" cy="10" r="10">
    <animate attributeName="cx" dur="4s" calcMode="spline" repeatCount="indefinite"
        values="60; 110; 60; 10; 60" keyTimes="0; 0.25; 0.5; 0.75; 1"
        keySplines="0.5 0 0.5 1; 0.5 0 0.5 1; 0.5 0 0.5 1; 0.5 0 0.5 1"/>
    <animate attributeName="cy" dur="4s" calcMode="spline" repeatCount="indefinite"
        values="10; 60; 110; 60; 10" keyTimes="0; 0.25; 0.5; 0.75; 1"
        keySplines="0.5 0 0.5 1; 0.5 0 0.5 1; 0.5 0 0.5 1; 0.5 0 0.5 1"/>
  </circle>
</svg>

他にも色々ありますが、飽きてきたのでこのくらいで。

## `<animateTransform>`
変換するやつですね。まずはMDNに載ってるやつをば。


``` svg
<svg width="120" height="120" viewBox="0 0 120 120"
     xmlns="http://www.w3.org/2000/svg">
    <polygon points="60,30 90,90 30,90">
        <animateTransform attributeName="transform"
                          attributeType="XML"
                          type="rotate"
                          from="0 60 70"
                          to="360 60 70"
                          dur="10s"
                          repeatCount="indefinite"/>
    </polygon>
</svg>
```

<svg width="120" height="120" viewBox="0 0 120 120"
     xmlns="http://www.w3.org/2000/svg">
    <polygon points="60,30 90,90 30,90">
        <animateTransform attributeName="transform"
                          attributeType="XML"
                          type="rotate"
                          from="0 60 70"
                          to="360 60 70"
                          dur="10s"
                          repeatCount="indefinite"/>
    </polygon>
</svg>

ぐるぐる周ります。

これはそれっぽく作れば時計も作れますね。

``` svg
<svg width="120" height="120" viewBox="0 0 120 120"
     xmlns="http://www.w3.org/2000/svg">
    <polygon points="59,60 60,0 61,60 60,80">
        <animateTransform attributeName="transform"
                          attributeType="XML"
                          type="rotate"
                          from="0 60 60"
                          to="360 60 60"
                          dur="60s"
                          repeatCount="indefinite"/>
    </polygon>
    <polygon points="57,60 60,10 63,60 60,70">
        <animateTransform attributeName="transform"
                          attributeType="XML"
                          type="rotate"
                          from="0 60 60"
                          to="360 60 60"
                          dur="3600s"
                          repeatCount="indefinite"/>
    </polygon>
    <polygon points="55,60 60,30 65,60 60,65">
        <animateTransform attributeName="transform"
                          attributeType="XML"
                          type="rotate"
                          from="60 60 60"
                          to="420 60 60"
                          dur="43200s"
                          repeatCount="indefinite"/>
    </polygon>
    <rect id=mark width=3 height=5 x=60 />
    <use href="#mark" transform="rotate(90 60 60)" />
    <use href="#mark" transform="rotate(180 60 60)" />
    <use href="#mark" transform="rotate(270 60 60)" />
</svg>
```

<svg width="120" height="120" viewBox="0 0 120 120"
     xmlns="http://www.w3.org/2000/svg">
    <polygon points="59,60 60,0 61,60 60,80">
        <animateTransform attributeName="transform"
                          attributeType="XML"
                          type="rotate"
                          from="0 60 60"
                          to="360 60 60"
                          dur="60s"
                          repeatCount="indefinite"/>
    </polygon>
    <polygon points="57,60 60,10 63,60 60,70">
        <animateTransform attributeName="transform"
                          attributeType="XML"
                          type="rotate"
                          from="0 60 60"
                          to="360 60 60"
                          dur="3600s"
                          repeatCount="indefinite"/>
    </polygon>
    <polygon points="55,60 60,30 65,60 60,65">
        <animateTransform attributeName="transform"
                          attributeType="XML"
                          type="rotate"
                          from="60 60 60"
                          to="420 60 60"
                          dur="43200s"
                          repeatCount="indefinite"/>
    </polygon>
    <rect id=mark width=3 height=5 x=60 />
    <use href="#mark" transform="rotate(90 60 60)" />
    <use href="#mark" transform="rotate(180 60 60)" />
    <use href="#mark" transform="rotate(270 60 60)" />
</svg>

SVGで時間をとれないので現在時刻はJSとかで取得してDOMを使って設定することになるかと思いますが、駆動部分はSVGだけで書けるのはいいですね。

他にもtransformにはscaleやskewなどいくつかがあります。詳細は [MDNの<animateTransform>のtypeのドキュメント](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/type#for_the_animatetransform_elements)を参照下さい。

## `<animateMotion>`

動かせます。MDNからのコピペ

``` svg
<svg viewBox="0 0 200 100" xmlns="http://www.w3.org/2000/svg" width=200 height=200>
  <path fill="none" stroke="lightgrey"
    d="M20,50 C20,-50 180,150 180,50 C180-50 20,150 20,50 z" />
  <circle r="5" fill="red">
    <animateMotion dur="10s" repeatCount="indefinite"
      path="M20,50 C20,-50 180,150 180,50 C180-50 20,150 20,50 z" />
  </circle>
</svg>
```

<svg viewBox="0 0 200 100" xmlns="http://www.w3.org/2000/svg" width=200 height=200>
  <path fill="none" stroke="lightgrey"
    d="M20,50 C20,-50 180,150 180,50 C180-50 20,150 20,50 z" />
  <circle r="5" fill="red">
    <animateMotion dur="10s" repeatCount="indefinite"
      path="M20,50 C20,-50 180,150 180,50 C180-50 20,150 20,50 z" />
  </circle>
</svg>

これを使って伝説の動く点Pを作れます


``` svg
<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <path fill="none" stroke="lightgrey"
    d="M20,20 L80,20 L80,80 L20,80 L20,20 z" />
  <g>
    <text x=-5 y=-5 >P</text>
    <circle r="5" fill="red" />
    <animateMotion dur="10s" repeatCount="indefinite"
      path="M20,20 L80,20 L80,80 L20,80 L20,20 z" />
  </g>
</svg>
```

<svg viewBox="100 100" xmlns="http://www.w3.org/2000/svg" width=100 height=100>
  <path fill="none" stroke="lightgrey"
    d="M20,20 L80,20 L80,80 L20,80 L20,20 z" />
  <g>
    <text x=-5 y=-5 >P</text>
    <circle r="5" fill="red" />
    <animateMotion dur="10s" repeatCount="indefinite"
      path="M20,20 L80,20 L80,80 L20,80 L20,20 z" />
  </g>
</svg>


他のアニメーション、例えば `<animateTransform>` と組み合わせて手裏剣を飛ばしたりもできます。

``` svg
<svg width="480" height="120" viewBox="0 0 480 120" xmlns="http://www.w3.org/2000/svg">
  <g>
    <polygon points="10,60 45,45 60,10 75,45 110,60 75,75 60,110 45,75" />
    <circle cx="60" cy="60" r=10 fill="white" />
    <animateMotion dur="3s" repeatCount="indefinite"
      path="M 0,0 380,0" />
    <animateTransform attributeName="transform"
                      attributeType="XML"
                      type="rotate"
                      from="0 60 60"
                      to="360 60 60"
                      dur="2s"
                      repeatCount="indefinite"/>
  </g>
</svg>
```

<svg width="480" height="120" viewBox="0 0 480 120" xmlns="http://www.w3.org/2000/svg">
  <g>
    <polygon points="10,60 45,45 60,10 75,45 110,60 75,75 60,110 45,75" />
    <circle cx="60" cy="60" r=10 fill="white" />
    <animateMotion dur="3s" repeatCount="indefinite"
      path="M 0,0 380,0" />
    <animateTransform attributeName="transform"
                      attributeType="XML"
                      type="rotate"
                      from="0 60 60"
                      to="360 60 60"
                      dur="2s"
                      repeatCount="indefinite"/>
  </g>
</svg>



遊んだので満足。
