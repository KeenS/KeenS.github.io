---
type: slide
title: "Picrinのチューニングの話、もといGCの話"
date: 2014-09-29
aliases:
    - /slide/picrin-gc.html
categories: [Lisp, picrin, GC, Lisp Meet Up]
description: "<a href='https://atnd.org/events/56406'>Lisp Meet Up presented by Shibuya.lisp #20</a><br>
で話してきた内容です。picrinが遅い話、それにまつわるGCの話などです。

"
---
<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">

# Picrinのチューニングの話、もといGCの話
---
Lisp Meet Up presented by Shibuya.lisp #20  
2014-09-29  
κeen(@blackenedgold)

<!-- .slide: class="center" -->

# 自己紹介
---
 + κeen
 + 東大数学科の4年生
 + ソーシャルアカウントは上のアイコン達から。
 + Lisp, Ruby, OCaml, Shell Scriptあたりを書きます
 + [picrin](https://github.com/picrin-scheme/picrin)のコミッタです

# picrin
---
![picrin](/images/picrin/picrin-logo-fin01-02.png)

# picrin
---
* R7RS compatibility
* bytecode interpreter (based on stack VM)
* internal representation by nan-boxing
* conservative call/cc implementation (users can freely interleave native stack with VM stack)
* exact GC (simple mark and sweep, partially reference count is used as well)
* string representation by rope data structure

# picrin
---
* R7RS compatibility
* <strong>bytecode interpreter (based on stack VM)</strong>
* internal representation by nan-boxing
* conservative call/cc implementation (users can freely interleave native stack with VM stack)
* <strong>exact GC (simple mark and sweep, partially reference count is used as well)</strong>
* <strong>string representation by rope data structure</strong>

# picrin開発者の不満
---
* picrinの起動が遅い<!-- .element: class="fragment" data-fragment-index="1" -->
  + 2秒くらいかかる <!-- .element: class="fragment" data-fragment-index="1" -->
* picrinのテストが遅い<!-- .element: class="fragment" data-fragment-index="2" -->
  + 16秒くらいかかる <!-- .element: class="fragment" data-fragment-index="2" -->

# 少しGCの話
---
GC(ごみ集め)とは

* 使われていないオブジェクトを自動的に解法する
  + メモリ管理に煩わせられないで済む
  + メモリーリークを防ぐ。
* 「使われていないオブジェクト」を判定するのはいくつかアルゴリズムがある


# GCのアルゴリズム
---
## 主なGCのアルゴリズム
* マーク&amp;スイープ
* 参照カウント
* Copying GC

# GCのアルゴリズム
---
## マーク&amp;スイープ
 1. あらかじめ全てのオブジェクトはどこかで一元管理する<!-- .element: class="fragment" data-fragment-index="1" -->
 2. 一元管理元(ヒープ(ページ))でメモリが足りなくなったらGCを開始<!-- .element: class="fragment" data-fragment-index="2" -->
 3. スタックに載っているオブジェクト、変数に格納されているオブジェクトにマークする<!-- .element: class="fragment" data-fragment-index="3" -->
 4. そこから辿れるオブジェクトも再帰的にマークする<!-- .element: class="fragment" data-fragment-index="4" -->
 5. ヒープからマークされていないオブジェクトを開放する<!-- .element: class="fragment" data-fragment-index="5" -->
 6. それでもメモリが足りなければ一ヒープのページを増やす<!-- .element: class="fragment" data-fragment-index="6" -->
 7. 増やせなければメモリを使い果したのでabort<!-- .element: class="fragment" data-fragment-index="7" -->

# GCのアルゴリズム
---
## マーク&amp;スイープの特徴
 * そこそこ速い
 * 正確に集められる
 * GC以外ではメモリ管理を考えなくて良い(≒プラグインは書き易い)
 * 長生きのオブジェクトは何度もマークされる(=>世代別GC)
 * 負荷は1点に集中する
 * メモリの断片化が起きる(=オブジェクトのアロケートが遅くなる)

# GCのアルゴリズム
---
## 参照カウント
 1. オブジェクトに他から参照されている数(参照カウント)を記憶させる<!-- .element: class="fragment" data-fragment-index="1" -->
 2. オブジェクトの参照を変えるときはその都度参照カウントを変える<!-- .element: class="fragment" data-fragment-index="2" -->
 3. 参照カウントが0になったら開放する<!-- .element: class="fragment" data-fragment-index="3" -->

# GCのアルゴリズム
---
## 参照カウントの特徴
 * 負荷は分散する
 * GCの実装は楽
 * 少し遅い(必ずfreeする必要がある)
 * 正確でない(循環参照に弱い)(cf. 大相撲の星の回しあい)
 * プラグインは面倒

# GCのアルゴリズム
---
## Copying GC
 1. オブジェクトプールを2つ用意する(1, 2)とする<!-- .element: class="fragment" data-fragment-index="1" -->
 2. オブジェクトを確保するときは必ず1に確保する<!-- .element: class="fragment" data-fragment-index="2" -->
 3. 1のメモリが足りなくなったらgcを開始<!-- .element: class="fragment" data-fragment-index="3" -->
 4. 1の生きているオブジェクトを再帰的に2に移動する<!-- .element: class="fragment" data-fragment-index="4" -->
 5. 1に残っているものは全てゴミとして良い<!-- .element: class="fragment" data-fragment-index="5" -->
 6. 1、2のラベルを付け換える<!-- .element: class="fragment" data-fragment-index="6" -->

# GCのアルゴリズム
---
## Copying GCの特徴
 * 正確
 * メモリの断片化が起きない(Windowsのデフラグ)(=アロケートが速くなる)
 * 負荷は1点に集中する
 * 遅い(オブジェクトの移動が必要)
 * メモリ食う(オブジェクトプール2つ分)
 * ポインタも変わるため、ハッシュの再計算などが必要

# GCのアルゴリズム
---
## GCの主な属性
* 世代別
  + 新しいオブジェクトだけGCの対象にする<!-- .element: class="fragment" data-fragment-index="1" -->
* 正確
  + ゴミは必ず回収する<!-- .element: class="fragment" data-fragment-index="2" -->
* インクリメンタル
  + 一度にやらずに少しづつGCを進める<!-- .element: class="fragment" data-fragment-index="3" -->

# picrinのGC
---
![picrin](/images/picrin/picrin-logo-fin01-02.png)

# picrinのGC
---
* 基本は普通の正確なマーク&amp;スイープ
  + Copying GCはlightweightでない
* ropeには参照カウント
  + ropeには循環参照が起きない

# picrinのGC
---
 * 基本はマーク&amp;スイープ

> 3. スタックに載っているオブジェクト、変数に格納されているオブジェクトにマークする

 * schemeの変数はそのまま
 * Cの変数は読めないのでアリーナという場所を用意してC内ではそこに一時保管してもらう
   + スコープはFILOなのでスタックを用いればヒープより速くアロケート出来る

# picrinのチューニング
## プロファイルをとってみる
---
```
 % cumulative self self total
time seconds seconds calls s/call s/call name
37.14 0.78 0.78 13136494 0.00 0.00 XROPE_DECREF
23.81 1.28 0.50 1544 0.00 0.00 gc_sweep_phase
15.72 1.61 0.33 59344814 0.00 0.00 gc_mark_object
10.00 1.82 0.21 1544 0.00 0.00 gc_mark_phase
6.67 1.96 0.14 1544 0.00 0.00 gc_mark_trie
2.38 2.01 0.05 6523 0.00 0.00 pic_apply
```

<span style="font-size:200%">ほとんどGCですね。  
本当にありがとう御座いました。</span>

<!-- .slide: class="center" -->

# 解決案
---
* GCのアルゴリズムを変える
* オブジェクトをアロケートしないようにする
* パラメーターをいじる

# 解決案
---
* GCのアルゴリズムを変える
* オブジェクトをアロケートしないようにする
* <strong>パラメーターをいじる</strong>

# GCに関連するパラメータ
---
元々tinyに設計されている
```C
/* アリーナのサイズ */
# define PIC_ARENA_SIZE 1000
/* ヒープページのサイズ */
# define PIC_HEAP_PAGE_SIZE 10000
/* シンボルプール(ハッシュ)のサイズ */
# define PIC_SYM_POOL_SIZE 128
```

# GCに関連するパラメータ
---
チューニング後
```C
/* アリーナのサイズ */
# define PIC_ARENA_SIZE 8 * 1024
/* ヒープページのサイズ */
# define PIC_HEAP_PAGE_SIZE 2 * 1024 * 1024
/* シンボルプール(ハッシュ)のサイズ */
# define PIC_SYM_POOL_SIZE 2048
```

# 結果
---
* 昔
  > テストに16秒

* 変更後
  > time make test-r7rs  
  > make test-r7rs  <strong>1.68s</strong> user 0.23s system 98% cpu 1.931 total
  <!-- .element: class="fragment" data-fragment-index="1" -->


# 今後
---
* GCのアルゴリズムを変える
 + 世代別GCにしたいよねー
 + そもそもマーク&amp;スイープの実装が適当すぎるだとか。協議中。
 + 組み込み目指すならインクリメンタル性必要では？
* オブジェクトをアロケートしないようにする
  + バイトコンパイルするときにもめちゃくちゃアロケートしてる。その辺どうにかする。
  + [Fast Reader](https://github.com/picrin-scheme/picrin/pull/207)


# まとめ
---
* 基本的なGCの解説をした
* picrinのGCの戦略を話した
* GCチューニング大事

<span style="font-size:600%">以上</span>  
何か質問あればどうぞ

<!-- .slide: class="center" -->
</script>
</section>
