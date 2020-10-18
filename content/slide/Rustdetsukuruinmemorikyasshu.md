---
categories: []
date: 2020-10-18T18:22:00+09:00
description:
title: "Rustで作るインメモリキャッシュ"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">

# Rustで作るインメモリキャッシュ
----------------------
[RustのLT会 Shinjuku.rs #12 @オンライン - connpass](https://forcia.connpass.com/event/187287/)

<!-- .slide: class="center" -->

===

# About Me
---------
![κeenのアイコン](/images/kappa2_vest.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# モチベーション
--------------

* アプリケーションでキャッシュしたいよね
  + アクセス数の80%はアクセス頻度上位20%のアイテム
* 例えば [crates.io](crates.io)のmost downloaded

===

# ハッシュマップ？
--------------


```rust
struct CachedDao { cache: HashMap<Key, Value>, dao: Dao }
impl CacheedDao {
    fn get(&mut self, key: &Key) -> Result<Option<Value>> {
        match self.cache.get(key) {
            Some(v) => Ok(Some(v)),
            None => {
                let v = dao.get(key)?;
                if let Some(v) = &v {
                    self.cache.insert(k.clone(), v.clone())
                }
                Ok(v)
            }
        }
    }
}
```

===

# 🙅‍
-------

* 使ってないデータを捨てる処理がない
* 最終的に全てのデータをメモリに載せてしまう
* →メモリ使用量制限のあるハッシュマップを作ろう
  + 現実にはアイテム数制限
===

# インメモリキャッシュ
----------------------

* ほとんどハッシュマップと一緒
* 容量が足りなくなったときの挙動が違う

<table style="align:center;border-collapse:separate;border-spacing:8px; display: inline">
  <tr><th style="border-bottom: solid 1px;">マップ</th><th  style="border-bottom: solid 1px;">キャッシュ</th></tr>
  <tr><td>容量を増やす</td><td>アイテムを削除</td></tr>
</table>


===

# キャッシュポリシー
--------------------

* 容量が一杯になったときにどのアイテムを削除するか？
* 色々ポリシーがある
  + ランダム
  + ハッシュが衝突したもの
  + 一番古い要素(FIFO)
  + …
* ポリシーによってパフォーマンス（ヒット率）が変わる

===

# Least Recently Used
---------------------

* 最後に参照したのが最も古いアイテムを削除
* そこそこキャッシュヒット率が良いことで知られる
* →これで実装してみよう

===

# LRU Explained
----------------


<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#new-1" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#new-1" />
    <use xlink:href="#arrow-top" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-1" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-1" />
    <use xlink:href="#new-2" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-1" />
    <use xlink:href="#new-2" />
    <use xlink:href="#arrow-top" />
    <use xlink:href="#arrow-1-2" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-2-1" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-2-1" />
    <use xlink:href="#new-3" />
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-2-1" />
    <use xlink:href="#new-3" />
    <use xlink:href="#arrow-top" />
    <use xlink:href="#arrow-1-2" />
    <use xlink:href="#arrow-2-3" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-3-2-1" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-3-2-1" />
    <use xlink:href="#new-4" />
  </g>
</svg>


===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-3-2-1" />
    <use xlink:href="#new-4" />
    <use xlink:href="#arrow-1-2" />
    <use xlink:href="#arrow-2-3" />
    <use xlink:href="#arrow-3-out" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item--3-2-1out" />
    <use xlink:href="#new-4" />
  </g>
</svg>


===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item--3-2-1out" />
    <use xlink:href="#new-4" />
    <use xlink:href="#arrow-top" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-4-3-2" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-4-3-2" />
    <use xlink:href="#new-2" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-4-3-2" />
    <use xlink:href="#new-2" />
    <use xlink:href="#arrow-1-2" />
    <use xlink:href="#arrow-2-3" />
    <use xlink:href="#arrow-3-1" />
  </g>
</svg>

===

# LRU Explained
----------------

<svg width="640" height="480" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g transform="scale(4)">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-2-4-3" />
  </g>
</svg>


===

# 実装
------

* LRU + ハッシュマップ
* LRUは リスト（あるいはベクタ）で簡単に実装できる
  + しかし効率が悪い
  + $O(n)$ の計算量

===

# 実装
------

<svg
   xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:cc="http://creativecommons.org/ns#"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   width="640"
   height="480"
   viewBox="0 0 169.33333 127"
   version="1.1"
   id="svg136"
   inkscape:version="0.92.5 (2060ec1f9f, 2020-04-08)"
   sodipodi:docname="fully-associative-cache.svg">
  <defs
     id="defs130" />
  <metadata
     id="metadata133">
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
     inkscape:label="レイヤー 1"
     inkscape:groupmode="layer"
     id="layer1"
     transform="translate(0,-169.99998)">
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186"
       width="13.49711"
       height="13.49711"
       x="32.205978"
       y="177.5305" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-2"
       width="13.497111"
       height="13.497111"
       x="32.205978"
       y="191.02762" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-7"
       width="13.497111"
       height="13.497111"
       x="32.205978"
       y="204.52473" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-0"
       width="13.497111"
       height="13.497111"
       x="32.205978"
       y="218.02185" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-9"
       width="13.497111"
       height="13.497111"
       x="32.205978"
       y="231.51897" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-3"
       width="13.497111"
       height="13.497111"
       x="32.205978"
       y="258.51318" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-6"
       width="13.497111"
       height="13.497111"
       x="32.205978"
       y="245.01608" />
    <g
       id="g275">
      <g
         transform="translate(6.2140111,0.73500061)"
         id="g264">
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06"
           width="30.468723"
           height="13.497116"
           x="67.552376"
           y="176.7955" />
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-2"
           width="30.468723"
           height="13.497116"
           x="98.021103"
           y="176.7955" />
      </g>
    </g>
    <g
       transform="translate(0,13.497116)"
       id="g275-1">
      <g
         transform="translate(6.2140111,0.73500061)"
         id="g264-8">
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-7"
           width="30.468723"
           height="13.497116"
           x="67.552376"
           y="176.7955" />
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-2-9"
           width="30.468723"
           height="13.497116"
           x="98.021103"
           y="176.7955" />
      </g>
    </g>
    <g
       transform="translate(0,26.994232)"
       id="g275-2">
      <g
         transform="translate(6.2140111,0.73500061)"
         id="g264-0">
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-23"
           width="30.468723"
           height="13.497116"
           x="67.552376"
           y="176.7955" />
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-2-7"
           width="30.468723"
           height="13.497116"
           x="98.021103"
           y="176.7955" />
      </g>
    </g>
    <g
       transform="translate(0,40.491348)"
       id="g275-5">
      <g
         transform="translate(6.2140111,0.73500061)"
         id="g264-9">
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-22"
           width="30.468723"
           height="13.497116"
           x="67.552376"
           y="176.7955" />
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-2-8"
           width="30.468723"
           height="13.497116"
           x="98.021103"
           y="176.7955" />
      </g>
    </g>
    <g
       transform="translate(0,53.988464)"
       id="g275-9">
      <g
         transform="translate(6.2140111,0.73500061)"
         id="g264-7">
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-3"
           width="30.468723"
           height="13.497116"
           x="67.552376"
           y="176.7955" />
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-2-6"
           width="30.468723"
           height="13.497116"
           x="98.021103"
           y="176.7955" />
      </g>
    </g>
    <g
       transform="translate(0,67.48558)"
       id="g275-12">
      <g
         transform="translate(6.2140111,0.73500061)"
         id="g264-93">
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-1"
           width="30.468723"
           height="13.497116"
           x="67.552376"
           y="176.7955" />
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-2-94"
           width="30.468723"
           height="13.497116"
           x="98.021103"
           y="176.7955" />
      </g>
    </g>
    <g
       transform="translate(0,80.982696)"
       id="g275-7">
      <g
         transform="translate(6.2140111,0.73500061)"
         id="g264-84">
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-5"
           width="30.468723"
           height="13.497116"
           x="67.552376"
           y="176.7955" />
        <rect
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
           id="rect186-06-2-0"
           width="30.468723"
           height="13.497116"
           x="98.021103"
           y="176.7955" />
      </g>
    </g>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="37.782455"
       y="290.31824"
       id="text2526"><tspan
         sodipodi:role="line"
         x="37.782455"
         y="290.31824"
         id="tspan2524"
         style="stroke-width:0.26458332px"><tspan
           x="37.782455"
           y="290.31824"
           id="tspan2522"
           style="stroke-width:0.26458332px">LRU</tspan></tspan></text>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="104.63589"
       y="290.31824"
       id="text2532"><tspan
         sodipodi:role="line"
         x="104.63589"
         y="290.31824"
         id="tspan2530"
         style="stroke-width:0.26458332px"><tspan
           x="104.63589"
           y="290.31824"
           id="tspan2528"
           style="stroke-width:0.26458332px">HashMap</tspan></tspan></text>
  </g>
</svg>



===

# 実装(LRU)
------

* LRUを効率的に実装したい
* 少数（定数）個のバケットを保持するブロックに分ける
  + 今回は16個
  + 少数ならビット演算でLRUを実装できる
* ブロックへの振り分けはハッシュ値を使う
  + 下4bitをブロック内の振り分けに
  + 5bit目以降をブロックの振り分けに
* キャッシュヒット率を捨てて速度をとった

===

# 実装(LRU)
------

<svg
   xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:cc="http://creativecommons.org/ns#"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   width="640"
   height="480"
   viewBox="0 0 169.33333 127"
   version="1.1"
   id="svg136"
   inkscape:version="0.92.5 (2060ec1f9f, 2020-04-08)"
   sodipodi:docname="way-assosiative-cache.svg">
  <defs
     id="defs130">
    <marker
       inkscape:stockid="Arrow1Mend"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="marker1753"
       style="overflow:visible;"
       inkscape:isstock="true">
      <path
         id="path1751"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.4) rotate(180) translate(10,0)" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible;"
       id="marker1711"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Mend"
       inkscape:collect="always">
      <path
         transform="scale(0.4) rotate(180) translate(10,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path1709" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Mend"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Mend"
       style="overflow:visible;"
       inkscape:isstock="true"
       inkscape:collect="always">
      <path
         id="path1390"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.4) rotate(180) translate(10,0)" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Lend"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Lend"
       style="overflow:visible;"
       inkscape:isstock="true">
      <path
         id="path1384"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.8) rotate(180) translate(12.5,0)" />
    </marker>
  </defs>
  <sodipodi:namedview
     id="base"
     pagecolor="#ffffff"
     bordercolor="#666666"
     borderopacity="1.0"
     inkscape:pageopacity="0.0"
     inkscape:pageshadow="2"
     inkscape:zoom="1.979899"
     inkscape:cx="298.24583"
     inkscape:cy="324.53951"
     inkscape:document-units="px"
     inkscape:current-layer="layer1"
     showgrid="false"
     units="px"
     showguides="true"
     inkscape:guide-bbox="true"
     inkscape:snap-bbox="false"
     inkscape:bbox-nodes="true"
     inkscape:snap-text-baseline="true"
     inkscape:snap-nodes="false"
     inkscape:snap-others="true"
     inkscape:window-width="3840"
     inkscape:window-height="2096"
     inkscape:window-x="0"
     inkscape:window-y="27"
     inkscape:window-maximized="1">
    <sodipodi:guide
       position="41.827681,25.247366"
       orientation="0,1"
       id="guide377"
       inkscape:locked="false" />
    <sodipodi:guide
       position="-74.701832,58.932931"
       orientation="0,1"
       id="guide1791"
       inkscape:locked="false" />
  </sodipodi:namedview>
  <metadata
     id="metadata133">
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
     inkscape:label="レイヤー 1"
     inkscape:groupmode="layer"
     id="layer1"
     transform="translate(0,-169.99998)">
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-0"
       width="3.8892272"
       height="3.8892272"
       x="33.23521"
       y="250.15901" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-9"
       width="3.8892272"
       height="3.8892272"
       x="33.23521"
       y="254.04823" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-3"
       width="3.8892272"
       height="3.8892272"
       x="33.23521"
       y="261.82672" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-6"
       width="3.8892272"
       height="3.8892272"
       x="33.23521"
       y="257.93747" />
    <g
       id="g264-9"
       transform="matrix(0.28815256,0,0,0.28815256,25.745557,199.21494)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-22"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-8"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <g
       id="g264-7"
       transform="matrix(0.28815256,0,0,0.28815256,25.745557,203.10417)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-3"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-6"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <g
       id="g264-93"
       transform="matrix(0.28815256,0,0,0.28815256,25.745557,206.9934)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-1"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-94"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <g
       id="g264-84"
       transform="matrix(0.28815256,0,0,0.28815256,25.745557,210.88262)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-5"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-0"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:3.65953732px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.07624036px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="35.195225"
       y="271.70081"
       id="text2441"><tspan
         sodipodi:role="line"
         x="35.195225"
         y="271.70081"
         id="tspan2439"
         style="stroke-width:0.07624036px"><tspan
           x="35.195225"
           y="271.70081"
           id="tspan2437"
           style="stroke-width:0.07624036px">LRU</tspan></tspan></text>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:3.65953732px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.07624036px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="54.187115"
       y="271.75262"
       id="text2447"><tspan
         sodipodi:role="line"
         x="54.187115"
         y="271.75262"
         id="tspan2445"
         style="stroke-width:0.07624036px"><tspan
           x="54.187115"
           y="271.75262"
           id="tspan2443"
           style="stroke-width:0.07624036px">HashMap</tspan></tspan></text>
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-0-3"
       width="3.8892272"
       height="3.8892272"
       x="82.619118"
       y="250.15901" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-9-6"
       width="3.8892272"
       height="3.8892272"
       x="82.619118"
       y="254.04823" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-3-1"
       width="3.8892272"
       height="3.8892272"
       x="82.619118"
       y="261.82672" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-6-0"
       width="3.8892272"
       height="3.8892272"
       x="82.619118"
       y="257.93747" />
    <g
       id="g264-9-6"
       transform="matrix(0.28815257,0,0,0.28815257,75.129461,199.21494)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-22-3"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-8-2"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <g
       id="g264-7-0"
       transform="matrix(0.28815257,0,0,0.28815257,75.129461,203.10417)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-3-6"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-6-1"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <g
       id="g264-93-5"
       transform="matrix(0.28815257,0,0,0.28815257,75.129461,206.99341)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-1-5"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-94-4"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <g
       id="g264-84-7"
       transform="matrix(0.28815257,0,0,0.28815257,75.129461,210.88262)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-5-6"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-0-5"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:3.65953732px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.07624036px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="84.445496"
       y="271.75262"
       id="text2453"><tspan
         sodipodi:role="line"
         x="84.445496"
         y="271.75262"
         id="tspan2451"
         style="stroke-width:0.07624036px"><tspan
           x="84.445496"
           y="271.75262"
           id="tspan2449"
           style="stroke-width:0.07624036px">LRU</tspan></tspan></text>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:3.65953732px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.07624036px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="103.83829"
       y="271.75262"
       id="text2459"><tspan
         sodipodi:role="line"
         x="103.83829"
         y="271.75262"
         id="tspan2457"
         style="stroke-width:0.07624036px"><tspan
           x="103.83829"
           y="271.75262"
           id="tspan2455"
           style="stroke-width:0.07624036px">HashMap</tspan></tspan></text>
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-0-4"
       width="3.8892272"
       height="3.8892272"
       x="134.0685"
       y="250.15903" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-9-7"
       width="3.8892272"
       height="3.8892272"
       x="134.0685"
       y="254.04825" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-3-4"
       width="3.8892272"
       height="3.8892272"
       x="134.0685"
       y="261.82672" />
    <rect
       style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:0.30496144;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1"
       id="rect186-6-4"
       width="3.8892272"
       height="3.8892272"
       x="134.0685"
       y="257.93747" />
    <g
       id="g264-9-3"
       transform="matrix(0.28815257,0,0,0.28815257,126.57885,199.21494)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-22-0"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-8-7"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <g
       id="g264-7-8"
       transform="matrix(0.28815257,0,0,0.28815257,126.57885,203.10417)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-3-68"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-6-8"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <g
       id="g264-93-4"
       transform="matrix(0.28815257,0,0,0.28815257,126.57885,206.99341)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-1-3"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-94-1"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <g
       id="g264-84-4"
       transform="matrix(0.28815257,0,0,0.28815257,126.57885,210.88262)">
      <rect
         y="176.7955"
         x="67.552376"
         height="13.497116"
         width="30.468723"
         id="rect186-06-5-9"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      <rect
         y="176.7955"
         x="98.021103"
         height="13.497116"
         width="30.468723"
         id="rect186-06-2-0-2"
         style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.05833328;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
    </g>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:3.65953732px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.07624036px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="136.56305"
       y="271.75262"
       id="text2465"><tspan
         sodipodi:role="line"
         x="136.56305"
         y="271.75262"
         id="tspan2463"
         style="stroke-width:0.07624036px"><tspan
           x="136.56305"
           y="271.75262"
           id="tspan2461"
           style="stroke-width:0.07624036px">LRU</tspan></tspan></text>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:3.65953732px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.07624036px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="155.0204"
       y="271.75262"
       id="text2471"><tspan
         sodipodi:role="line"
         x="155.0204"
         y="271.75262"
         id="tspan2469"
         style="stroke-width:0.07624036px"><tspan
           x="155.0204"
           y="271.75262"
           id="tspan2467"
           style="stroke-width:0.07624036px">HashMap</tspan></tspan></text>
    <path
       style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:0.79374999;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#Arrow1Mend)"
       d="M 100.49334,187.86098 53.587541,237.30584"
       id="path574"
       inkscape:connector-curvature="0"
       sodipodi:nodetypes="cc" />
    <path
       style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:0.79374999;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker1711)"
       d="m 100.22607,188.12825 0.40091,49.17759"
       id="path1701"
       inkscape:connector-curvature="0"
       sodipodi:nodetypes="cc" />
    <path
       style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:0.79374999;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker1753)"
       d="m 100.35971,188.39552 46.77216,48.91032"
       id="path1743"
       inkscape:connector-curvature="0"
       sodipodi:nodetypes="cc" />
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:19.7555542px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="25.726702"
       y="265.3584"
       id="text121"><tspan
         sodipodi:role="line"
         x="25.726702"
         y="265.3584"
         id="tspan119"
         style="stroke-width:0.26458332px"><tspan
           x="25.726702"
           y="265.3584"
           id="tspan117"
           style="stroke-width:0.26458332px">{</tspan></tspan></text>
    <flowRoot
       xml:space="preserve"
       id="flowRoot2221"
       style="fill:black;stroke:none;stroke-opacity:1;stroke-width:1px;stroke-linejoin:miter;stroke-linecap:butt;fill-opacity:1;font-family:'源ノ角ゴシック JP';font-style:normal;font-weight:normal;font-size:64px;line-height:134.00000334%;letter-spacing:0px;word-spacing:0px;-inkscape-font-specification:'源ノ角ゴシック JP';font-stretch:normal;font-variant:normal;text-anchor:middle;text-align:center"><flowRegion
         id="flowRegion2223"><rect
           id="rect2225"
           width="45.961941"
           height="122.73354"
           x="10.606602"
           y="288.57608" /></flowRegion><flowPara
         id="flowPara2227" /></flowRoot>    <flowRoot
       xml:space="preserve"
       id="flowRoot2237"
       style="fill:black;stroke:none;stroke-opacity:1;stroke-width:1px;stroke-linejoin:miter;stroke-linecap:butt;fill-opacity:1;font-family:'源ノ角ゴシック JP';font-style:normal;font-weight:normal;font-size:29.33333333px;line-height:134.00000334%;letter-spacing:0px;word-spacing:0px;-inkscape-font-specification:'源ノ角ゴシック JP';font-stretch:normal;font-variant:normal;text-anchor:middle;text-align:center;"><flowRegion
         id="flowRegion2239"
         style="font-size:29.33333333px;"><rect
           id="rect2241"
           width="40.406101"
           height="91.455101"
           x="10.606602"
           y="293.12177"
           style="font-size:29.33333333px;" /></flowRegion><flowPara
         id="flowPara2243" /></flowRoot>    <flowRoot
       xml:space="preserve"
       id="flowRoot2245"
       style="fill:black;stroke:none;stroke-opacity:1;stroke-width:1px;stroke-linejoin:miter;stroke-linecap:butt;fill-opacity:1;font-family:'源ノ角ゴシック JP';font-style:normal;font-weight:normal;font-size:29.33333333px;line-height:134.00000334%;letter-spacing:0px;word-spacing:0px;-inkscape-font-specification:'源ノ角ゴシック JP';font-stretch:normal;font-variant:normal;text-anchor:middle;text-align:center;"><flowRegion
         id="flowRegion2247"
         style="font-size:29.33333333px;"><rect
           id="rect2249"
           width="46.467018"
           height="115.66247"
           x="-1.0101526"
           y="296.15222"
           style="font-size:29.33333333px;" /></flowRegion><flowPara
         id="flowPara2251" /></flowRoot>    <flowRoot
       xml:space="preserve"
       id="flowRoot2259"
       style="fill:black;stroke:none;stroke-opacity:1;stroke-width:1px;stroke-linejoin:miter;stroke-linecap:butt;fill-opacity:1;font-family:'源ノ角ゴシック JP';font-style:normal;font-weight:normal;font-size:32px;line-height:134.00000334%;letter-spacing:0px;word-spacing:0px;-inkscape-font-specification:'源ノ角ゴシック JP';font-stretch:normal;font-variant:normal;text-anchor:middle;text-align:center;"><flowRegion
         id="flowRegion2261"
         style="font-size:32px;"><rect
           id="rect2263"
           width="124.24876"
           height="177.78685"
           x="-72.73098"
           y="236.04816"
           style="font-size:32px;" /></flowRegion><flowPara
         id="flowPara2265" /></flowRoot>    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:7.76111126px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="12.310418"
       y="262.11868"
       id="text2435"><tspan
         sodipodi:role="line"
         x="12.310418"
         y="262.11868"
         id="tspan2433"
         style="stroke-width:0.26458332px"><tspan
           x="12.310418"
           y="262.11868"
           id="tspan2431"
           style="stroke-width:0.26458332px">fixed</tspan></tspan></text>
  </g>
</svg>

===

# 実装(ハッシュマップ)
------

* ハッシュマップにはキャッシュに必要のない機能がある
  + 容量が足りなくなったら拡大など
* LRUで管理する関係上ハッシュマップ内部のインデックスを使いたい

===

# hashbrown
------------

* [hashbrown](https://crates.io/crates/hashbrown)
* 標準ライブラリのハッシュマップはcrates.ioに公開されている
* 低レベルAPIの[`RawTable`](https://docs.rs/hashbrown/0.9.1/hashbrown/raw/struct.RawTable.html)が公開されている
* → これを使うと実装できそう

===

# chechire
-----------

* [KeenS/chechire]()
  + 発音はチェシャ猫のcheshireと同じ
* 16-way assosiativeなhashbrownベースのキャッシュ
* ほぼ標準ライブラリの `HashMap` と同じAPI
* 時間の都合で紹介は省略

===
# ベンチマーク
--------------

* 以下の3つが対象
  + chechire
  + LRUを `Vec` で実装した簡易キャッシュ
  + LRUを `VecDeque` で〃
* 指数関数で重みをつけたランダムアクセス100,000回
* キャッシュミスすると5msのスリープ
  + DB叩くとだいたいこのくらい？

===
# 結果
------

<style type="text/css">
.graph{
  background:#aaa;
  border-radius:5px;
  white-space: nowrap;
  text-align: left;
}
td {
  white-space: nowrap;
}
</style>

| subject             | hit rate |  time
|:--------------------|----------|-----------------------------------------------------------------
| chechire            |  99.78%  | <div class="graph" style="width:calc( 142px * 3);">142ms</div>
| easy cache (vec)    |  99.72%  | <div class="graph" style="width:calc( 184px * 3);">184ms</div>
| easy cache (deque)  |  99.71%  | <div class="graph" style="width:calc( 194px * 3);">194ms</div>


===

# まとめ
--------

* 定数個のアイテムを保持できるキャッシュを作ったよ
* アイテム管理にはLRUというポリシーがあるよ
* fully associativeだと遅いから16-wayくらいにしたよ
* Rustの `HashMap` は改造できるよ


</script>
</section>

<svg
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   width="640"
   height="480"
   version="1.1"
   id="lru"
   inkscape:version="0.92.5 (2060ec1f9f, 2020-04-08)">
  <defs id="defs2">
    <marker
       inkscape:stockid="Arrow1Send"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="marker3501"
       style="overflow:visible;"
       inkscape:isstock="true">
      <path
         id="path3499"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.2) rotate(180) translate(6,0)" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible;"
       id="marker3185"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Send"
       inkscape:collect="always">
      <path
         transform="scale(0.2) rotate(180) translate(6,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path3183" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible;"
       id="marker1967"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Send">
      <path
         transform="scale(0.2) rotate(180) translate(6,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path1965" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Send"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Send"
       style="overflow:visible;"
       inkscape:isstock="true"
       inkscape:collect="always">
      <path
         id="path918"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.2) rotate(180) translate(6,0)" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible;"
       id="marker1337"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Mend">
      <path
         transform="scale(0.4) rotate(180) translate(10,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path1335" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible;"
       id="marker1269"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Mend">
      <path
         transform="scale(0.4) rotate(180) translate(10,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path1267" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Mend"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Mend"
       style="overflow:visible;"
       inkscape:isstock="true"
       inkscape:collect="always">
      <path
         id="path912"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.4) rotate(180) translate(10,0)" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Lend"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Lend"
       style="overflow:visible;"
       inkscape:isstock="true">
      <path
         id="path906"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.8) rotate(180) translate(12.5,0)" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Lstart"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Lstart"
       style="overflow:visible"
       inkscape:isstock="true">
      <path
         id="path903"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.8) translate(12.5,0)" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible"
       id="marker1967-2"
       refX="0"
       refY="0"
       orient="auto"
       inkscape:stockid="Arrow1Send">
      <path
         inkscape:connector-curvature="0"
         transform="matrix(-0.2,0,0,-0.2,-1.2,0)"
         style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:1.00000003pt;stroke-opacity:1"
         d="M 0,0 5,-5 -12.5,0 5,5 Z"
         id="path1965-7" />
    </marker>
    <symbol
       inkscape:label="base-box"
       inkscape:groupmode="layer"
       id="base-box"
       transform="translate(0,-169.99998)"
       style="display:inline">
      <g id="g3621">
        <rect
           y="187.70956"
           x="46.238255"
           height="22.753609"
           width="22.753609"
           id="rect10"
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.32291663;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
        <rect
           y="210.46318"
           x="46.238255"
           height="22.753609"
           width="22.753609"
           id="rect10-3"
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.32291663;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
        <rect
           y="233.21678"
           x="46.238255"
           height="22.753609"
           width="22.753609"
           id="rect10-6"
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.32291663;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      </g>
    </symbol>
    <symbol
       inkscape:groupmode="layer"
       id="arrow-top"
       inkscape:label="arrow-top"
       style="display:inline">
      <path
         style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.32291663;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#Arrow1Mend)"
         d="m 128.20608,25.01997 c 0,0 -10.23865,-10.579456 -25.22088,-10.782835 C 88.002972,14.033757 71.916025,26.664809 71.916025,26.664809"
         id="path96"
         inkscape:connector-curvature="0"
         sodipodi:nodetypes="czc" />
    </symbol>
    <symbol
       inkscape:label="new-1"
       id="new-1"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="133.08879"
         y="32.506424"
         id="text149"><tspan
           sodipodi:role="line"
           x="133.08879"
           y="32.506424"
           id="tspan147"
           style="stroke-width:0.26458332px"><tspan
             x="133.08879"
             y="32.506424"
             id="tspan145"
             style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="new-2"
       inkscape:label="new-2">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="133.18018"
         y="32.597801"
         id="text155"><tspan
           sodipodi:role="line"
           x="133.18018"
           y="32.597801"
           id="tspan153"
           style="stroke-width:0.26458332px"><tspan
             x="133.18018"
             y="32.597801"
             id="tspan151"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:label="item-1"
       id="item-1"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.56303"
         y="32.68935"
         id="text161"><tspan
           sodipodi:role="line"
           x="57.56303"
           y="32.68935"
           id="tspan159"
           style="stroke-width:0.26458332px"><tspan
             x="57.56303"
             y="32.68935"
             id="tspan157"
             style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:groupmode="layer"
       id="arrow-1-2"
       inkscape:label="arrow-1-2"
       style="display:inline">
      <path
         style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.32291663;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#Arrow1Send)"
         d="m 46.055496,28.401028 c 0,0 -11.587836,2.282937 -11.605254,11.879395 -0.01742,9.596458 9.869036,10.508692 9.869036,10.508692"
         id="path1327"
         inkscape:connector-curvature="0"
         sodipodi:nodetypes="czc" />
    </symbol>
    <symbol
       inkscape:label="new-3"
       id="new-3"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="133.63707"
         y="32.323662"
         id="text167"><tspan
           sodipodi:role="line"
           x="133.63707"
           y="32.323662"
           id="tspan165"
           style="stroke-width:0.26458332px"><tspan
             x="133.63707"
             y="32.323662"
             id="tspan163"
             style="stroke-width:0.26458332px">3</tspan></tspan></text>
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="item-2-1"
       inkscape:label="item-2-1">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.654408"
         y="32.780727"
         id="text173"><tspan
           sodipodi:role="line"
           x="57.654408"
           y="32.780727"
           id="tspan171"
           style="stroke-width:0.26458332px"><tspan
             x="57.654408"
             y="32.780727"
             id="tspan169"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.554268"
         y="56.455193"
         id="text179"><tspan
           sodipodi:role="line"
           x="57.554268"
           y="56.455193"
           id="tspan177"
           style="stroke-width:0.26458332px"><tspan
             x="57.554268"
             y="56.455193"
             id="tspan175"
             style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       style="display:inline"
       inkscape:label="arrow-2-3"
       id="arrow-2-3"
       inkscape:groupmode="layer">
      <path
         sodipodi:nodetypes="czc"
         inkscape:connector-curvature="0"
         id="path1961"
         d="m 46.055496,28.401028 c 0,0 -11.587836,2.282937 -11.605254,11.879395 -0.01742,9.596458 9.869036,10.508692 9.869036,10.508692"
         style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.32291663;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker1967)" />
      <path
         sodipodi:nodetypes="czc"
         inkscape:connector-curvature="0"
         id="path1961-0"
         d="m 45.964117,54.992593 c 0,0 -11.587837,2.282937 -11.605255,11.879394 -0.01742,9.596458 9.869037,10.508693 9.869037,10.508693"
         style="display:inline;fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.32291663;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker1967-2)" />
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="new-4"
       inkscape:label="new-4">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="132.6319"
         y="32.415043"
         id="text185"><tspan
           sodipodi:role="line"
           x="132.6319"
           y="32.415043"
           id="tspan183"
           style="stroke-width:0.26458332px"><tspan
             x="132.6319"
             y="32.415043"
             id="tspan181"
             style="stroke-width:0.26458332px">4</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:label="item-3-2-1"
       id="item-3-2-1"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.471649"
         y="33.054871"
         id="text191"><tspan
           sodipodi:role="line"
           x="57.471649"
           y="33.054871"
           id="tspan189"
           style="stroke-width:0.26458332px"><tspan
             x="57.471649"
             y="33.054871"
             id="tspan187"
             style="stroke-width:0.26458332px">3</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.37151"
         y="56.729336"
         id="text197"><tspan
           sodipodi:role="line"
           x="57.37151"
           y="56.729336"
           id="tspan195"
           style="stroke-width:0.26458332px"><tspan
             x="57.37151"
             y="56.729336"
             id="tspan193"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.462891"
         y="79.848465"
         id="text203"><tspan
           sodipodi:role="line"
           x="57.462891"
           y="79.848465"
           id="tspan201"
           style="stroke-width:0.26458332px"><tspan
             x="57.462891"
             y="79.848465"
             id="tspan199"
             style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:groupmode="layer"
       id="arrow-3-out"
       inkscape:label="arrow-3-out"
       style="display:inline">
      <path
         style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.05833328;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker3185)"
         d="M 57.112471,85.69626 V 99.129113"
         id="path3175"
         inkscape:connector-curvature="0" />
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="item--3-2-1out"
       inkscape:label="item--3-2-1out">
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="57.554268"
       y="56.089676"
       id="text209"><tspan
         sodipodi:role="line"
         x="57.554268"
         y="56.089676"
         id="tspan207"
         style="stroke-width:0.26458332px"><tspan
           x="57.554268"
           y="56.089676"
           id="tspan205"
           style="stroke-width:0.26458332px">3</tspan></tspan></text>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="57.645649"
       y="79.208801"
       id="text215"><tspan
         sodipodi:role="line"
         x="57.645649"
         y="79.208801"
         id="tspan213"
         style="stroke-width:0.26458332px"><tspan
           x="57.645649"
           y="79.208801"
           id="tspan211"
           style="stroke-width:0.26458332px">2</tspan></tspan></text>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="57.581059"
       y="111.54848"
       id="text221"><tspan
         sodipodi:role="line"
         x="57.581059"
         y="111.54848"
         id="tspan219"
         style="stroke-width:0.26458332px"><tspan
           x="57.581059"
           y="111.54848"
           id="tspan217"
           style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="item-4-3-2"
       inkscape:label="item-4-3-2">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.106129"
         y="32.597969"
         id="text227"><tspan
           sodipodi:role="line"
           x="57.106129"
           y="32.597969"
           id="tspan225"
           style="stroke-width:0.26458332px"><tspan
             x="57.106129"
             y="32.597969"
             id="tspan223"
             style="stroke-width:0.26458332px">4</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.005989"
         y="56.272434"
         id="text233"><tspan
           sodipodi:role="line"
           x="57.005989"
           y="56.272434"
           id="tspan231"
           style="stroke-width:0.26458332px"><tspan
             x="57.005989"
             y="56.272434"
             id="tspan229"
             style="stroke-width:0.26458332px">3</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.09737"
         y="79.391563"
         id="text239"><tspan
           sodipodi:role="line"
           x="57.09737"
           y="79.391563"
           id="tspan237"
           style="stroke-width:0.26458332px"><tspan
             x="57.09737"
             y="79.391563"
             id="tspan235"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:label="item-2-4-3"
       id="item-2-4-3"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.014748"
         y="32.597969"
         id="text245"><tspan
           sodipodi:role="line"
           x="57.014748"
           y="32.597969"
           id="tspan243"
           style="stroke-width:0.26458332px"><tspan
             x="57.014748"
             y="32.597969"
             id="tspan241"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="56.914608"
         y="56.272434"
         id="text251"><tspan
           sodipodi:role="line"
           x="56.914608"
           y="56.272434"
           id="tspan249"
           style="stroke-width:0.26458332px"><tspan
             x="56.914608"
             y="56.272434"
             id="tspan247"
             style="stroke-width:0.26458332px">4</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.005989"
         y="79.391563"
         id="text257"><tspan
           sodipodi:role="line"
           x="57.005989"
           y="79.391563"
           id="tspan255"
           style="stroke-width:0.26458332px"><tspan
             x="57.005989"
             y="79.391563"
             id="tspan253"
             style="stroke-width:0.26458332px">3</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:groupmode="layer"
       id="arrow-3-1"
       inkscape:label="arrow-3-1"
       style="display:inline">
      <path
         style="display:inline;fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.05833328;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker3501)"
         d="m 69.814287,75.735843 c 0,0 16.357008,-8.589715 16.265628,-24.398447 C 85.988536,35.528663 69.631526,28.401028 69.631526,28.401028"
         id="path3475"
         inkscape:connector-curvature="0"
         sodipodi:nodetypes="czc" />
    </symbol>
  </defs>
</svg>
