---
categories: [SML, 月初会]
date: 2015-09-04T06:45:19+09:00
description: "月初会で飛び入りLTのために作った雑なスライド。
"
title: SMLでPNGデコーダを作ろうとして分かったこと
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# SMLでPNGデコーダを作ろうとして分かったこと
------------------------------------------
サイバーエージェント アドテクスタジオ  
エンジニア月初会
<!-- .slide: class="center" -->
===
# About Me
----------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + AMoAdの新卒
 + Lisp, ML, Shell Scriptあたりを書きます

===
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">うーむ。画像フォーマットの扱い一回くらい書いてみないとなーと思ってたけどやっぱり既存なんだよなー。Common LispかSMLあたりならフロンティアになれそう。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/639303085240643584">2015, 9月 3</a></blockquote>

<!-- .slide: class="center" -->
===
<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> 仕様を理解するためにデコーダから実装するのがオススメです</p>&mdash; ELD-R-ESH-2 (@eldesh) <a href="https://twitter.com/eldesh/status/639304790766305281">2015, 9月 3</a></blockquote>

<!-- .slide: class="center" -->
===
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> そんな詳しいわけではないんですが、jpegかpngがおすすめです。webpは動画コーデック由来で難しいと思います。tiffは画像コンテナみたいな立ち位置なので画像フォーマット感がないかなと。</p>&mdash; ELD-R-ESH-2 (@eldesh) <a href="https://twitter.com/eldesh/status/639307400244137984">2015, 9月 3</a></blockquote>

<!-- .slide: class="center" -->
===
明日の朝までに  
<span style='font-size:150%;'>SMLでpngデコーダを実装しよう</span>

<!-- .slide: class="center" -->

===
# Standard ML
-------------

* ML系の函数型言語
* 文法はOCamlよりF#に似てる（というかF#が似せてきた）
* 結構書き易い
* 仕様は SML'90とSML'97がある
* 仕様で言語のformal semanticsが定められてたりする
* 要は研究向き
  + **ライブラリほぼなし**
  + **コミュニティほぼなし**

===
<span style='font-size:250%;'>＞　無理ゲー　＜</span>


<!-- .slide: class="center" -->
===
# 一応フォーマットを調べる
------------------------


```
+-----------------+
|     Chunk       | 画像はChunkの集合。
++---------------++ Chunk自体は簡単なフォーマット
|| length | name ||
|+---------------+|
||    data       ||
||    ...        ||
|+---------------+|
||    CRC        ||
++---------------++
|    Chunk        |
|    ...          |
```

===
<span style='font-size:250%;'>
意外と単純？
</span>


<!-- .slide: class="center" -->
===
# とりあえず書いてみる
---------------------

```sml
structure PNG = struct
    fun readChunk data i = ...
end
```

===
# 案外苦戦
------------
* 型が厳密なので型の行き来が面倒
  + 8bit <-> 32bit
  + 符号付き <-> 符号無し
  + byte <-> char

```sml
val op << = Word.<<
val op >> = Word.>>

fun nameToWord name = CharVector.foldl (fn(c, acc) => <<(acc, 0w8) + (Word.fromInt (ord c))) 0w0 name
```

===
<span style='font-size:250%;'>
3時間後
</span>

<!-- .slide: class="center" -->
===
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">さて、メインのデータ抜き出すところまでは行ったけど次復号だ。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/639501771430211584">2015, 9月 3</a></blockquote>

<!-- .slide: class="center" -->
===
# 今更PNGについて
----------------

* GIFの特許問題を回避するために作られたフォーマット
  + LZ77がマズいらしい
* **可逆圧縮アルゴリズムを使う** <!-- .element: class="fragment" data-fragment-index="1"-->
  + **アルゴリズムは1つとは限らない** <!-- .element: class="fragment" data-fragment-index="2"-->
  + 但し仕様で指定されているのはzlibのみ <!-- .element: class="fragment" data-fragment-index="2"-->
* フィルタを噛ませることでプログレッシブな表示も可能

===
# SMLのZLibライブラリ
--------------------

* ない <!-- .element: class="fragment" data-fragment-index="1"-->

<!-- .slide: class="center" -->
===
<span style='font-size:250%;'>
zlib……実装するか
</span>

<!-- .slide: class="center" -->
===
# ZLib
------

* RFC-1950
* zipやpngで使われるフォーマット
* ZLib自体は圧縮データのコンテナで **圧縮アルゴリズム自体は1つとは限らない**  <!-- .element: class="fragment" data-fragment-index="1"-->
  + 但し仕様で指定されているのはdeflateのみ <!-- .element: class="fragment" data-fragment-index="2"-->

===
# SMLのDeflateライブラリ
--------------------

* ない <!-- .element: class="fragment" data-fragment-index="1"-->

<!-- .slide: class="center" -->
===
<span style='font-size:250%;'>
deflate…実装するか
</span>

<!-- .slide: class="center" -->
===
# Deflate
---------

* RFC-1951
* ハフマン符号の変種の可逆圧縮アルゴリズム
  + **3種類の符号化方式を自由に使ってよい** <!-- .element: class="fragment" data-fragment-index="1"-->

===

<span style='font-size:250%;'>(心)ボキッ</span>

<!-- .slide: class="center" -->

===
# 学んだこと
-----------

* PNGは一晩でデコーダを書ける程柔じゃない
  + 事前調査も大事
* 書き易い言語でもコミュニティが大事
* 1晩でLTの準備はつらい

===
# 付録
------

* [今回のコード](https://github.com/KeenS/sml-png)
* [SMLのパッケージマネージャ](https://github.com/standardml/smackage)
* [PNG](http://www.w3.org/TR/2003/REC-PNG-20031110/)
* [RFC-1950 ZLIB](https://www.ietf.org/rfc/rfc1950.txt)
* [RFC-1951 DEFLATE](https://www.ietf.org/rfc/rfc1950.txt)
</textarea>
