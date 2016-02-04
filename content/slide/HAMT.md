---
categories: [Scala, 社内, Scala Meet Up]
date: 2016-01-31T16:17:35+09:00
description: "社内勉強会「AdTech Scala Meetup」でのLT大会の資料です。"
title: HAMT ~ イミュータブルで高速なハッシュマップ ~
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# <span style="font-size: 90%">Hash-Array Mapped Trie</span>
----------------------
 ~ イミュータブルで高速なハッシュマップ ~  
サイバーエージェント AdTech Scala Meetup LT大会

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 基盤開発グループの新卒
 + Lisp, ML, Rust, Shell Scriptあたりを書きます


# <span style="font-size: 60%">scala.collections.immutable.HashMap</span>
-------------------------------------

* 今日の話題。これの実装をみていく。
* イミュータブル
* キー-バリューペア
* 値を追加する度に新たなハッシュマップを作る <!-- .element: class="fragment grow" data-fragment-index="1" -->


# HashMap
----------------

* `O(1)`のアクセス効率
* 空間効率は悪い
* ハッシュ関数が定義出来れば何でもキーに使える
* 普通はミュータブルに使う
  + 大量のメモリをアロケートするのでコピーはつらい


# メモリ効率悪そう？
-------------------------

```scala
val hash = HashMap.empty + (3 -> 1)
// +---+---+---+---+---+----
// | / | 3 | / | / | / | ...
// +---+---+---+---+---+----
//       |
//      +-+
//      |1|
//      +-+
```


# メモリ効率悪そう？
-------------------------

```scala
val hash2 = hash + (2 -> 2)
// +---+---+---+---+---+----
// | / | 3 | / | / | / | ...
// +---+---+---+---+---+----
//       |
//      +-+
//      |1|
//      +-+
//  > Copy <
// +---+---+---+---+---+----
// | / | 3 | / | 2 | / | ...
// +---+---+---+---+---+----
//       |       |
//      +-+     +-+
//      |1|     |2|
//      +-+     +-+
```


# メモリ効率的データ構造?
----------------------------
## TreeMap

* O(log(n))のアクセス効率
* 空間効率はほどほど
  + イミュータブルに使えばかなり良い
  + 部分構造を再利用出来る
* 全順序関数が定義出来れば何でもキーに出来る
* キーが複数回比較される問題がある


# キー同士の比較
---------------
長いキー同士の比較がO(log(n))回走る可能性がある

```scala
treeMap.get("very long ... key1")

        ....
       /
"very long ... key3" -> "value3"
       \
   "very long ... key2" -> "value2"
         \
      "very long ...key1" -> "value1"
```


# HashMap vs TreeMap
-----------------

<table>
<tr><td></td><th>HashMap</th><th>TreeMap</th></tr>
<tr><th>アクセス効率</th><td class="fragment highlight-red" data-fragment-index="1">`O(1)`</td><td>`O(log(n))`</td></tr>
<tr><th>イミュータブルな時の空間効率</th><td>悪い(毎回コピー)</td><td class="fragment highlight-red"  data-fragment-index="1">良い(部分構造を共有)</td></tr>
<tr><th>キーの比較</th><td class="fragment highlight-red"  data-fragment-index="1">定数回</td><td>`O(log(n))`回</td></tr>
<tr><th>キーの要件</th><td class="fragment highlight-red"  data-fragment-index="1">Hash関数が定義されている</td><td>全順序関数が定義されている</td></tr>
</table>

# <span style="font-size: 90%">Hash-Array Mapped Trie</span>
------------------------

* `O(1)`のアクセス効率
* 部分構造を共有してメモリ効率も良い
* キーの比較は定数回
* Hash関数が定義されていればキーに出来る


# 動作
------

* ざっくり言うと、「Hashして分割してトライ」


# 動作
------
## Hashする

* 40bitくらいの値が生成される

``` scala
hash("key")
// => 0b10101101010101001010110101010100
```


# 動作
------
## 分割する

* 下位から5bit毎に分割する
  + 5bit = 0 ~ 31
  + 32分木になる

```
11111 00010 10110 10101 01001 01011 01010 10100
```


# 動作
------
## トライ

* 32分のトライ木になる
* トライ木の実装は32bitのbitmapを使ったArray Mapped Trieを使う


(図が分かりづらいというか不適切)

```
8     7     6     5     4     3     2     1
11111 00010 10110 10101 01001 01011 01010 10100

1    2    3  4
   ...
  /
10100     ...
  \        /
   \    01011
    \   /  \
     \ /   01001....
     01010
       \
        ...
```


# 特徴
------

* ハッシュ値が固定長なので`O(1)`で動作
* Treeなので部分構造の共有が簡単
* 木を辿る時の比較はhash値（の一部）なので高速
* キーに全順序がなくてもハッシュ関数さえ定義されていれば木を構築出来る


# まとめ
--------

* Scalaのimmutable.HashMapはコピーしても高速だよ
* 裏で動くアルゴリズムを紹介したよ
  + ざっくりなので本物はもう少し工夫してる
  + prefix treeになってる


# 参考
------

* PDF [Ideal Hash Trees](http://lampwww.epfl.ch/papers/idealhashtrees.pdf)
  + HAMTの元論文
* [HAMT(Hash Array Mapped Trie) - sileのブログ](http://sile.hatenablog.jp/entry/20100926/1285467529)
  + HAMTの解説ブログ
* PDF [Optimizing Hash-Array Mapped Tries for Fast and Lean Immutable JVM Collections](http://michael.steindorfer.name/publications/oopsla15.pdf)
  + HAMTを改善したCHAMPというデータ構造の論文。Clojureにこれが入りそう。
* [Optimizing Hash-Array Mapped Tries for Fast and Lean Immutable JVM Collections | the morning paper](http://blog.acolyer.org/2015/11/27/hamt/)
  + CHAMPを解説したブログ

</script>
</section>
