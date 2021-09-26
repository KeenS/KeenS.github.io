---
categories: [GC, ランタイム, 言語処理系]
date: 2021-09-17T00:55:32+09:00
description: 社内勉強会のPLDI論文読み会で発表した資料です
title: "PLDI論文読み会: Perceus: Garbage Free Reference Counting with Reuse"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# PLDI論文読み会: Perceus: Garbage Free Reference Counting with Reuse
----------------------
社内勉強会
<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa2_vest.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

* κeen
* [@blackenedgold](https://twitter.com/blackenedgold)
* GitHub: [KeenS](https://github.com/KeenS)
* GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
* [Idein Inc.](https://idein.jp/)のエンジニア
* Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# 概要
（[高速で論文がバリバリ読める落合先生のフォーマットがいい感じだったのでメモ - 書架とラフレンツェ](https://lafrenze.hatenablog.com/entry/2015/08/04/120205)）
<!-- .slide: class="center" -->

===
# 概要
## どんなもの？

参照カウントのdup/dropを挿入するすごいアルゴリズム

===
# 概要
## 先行研究と比べてどこがすごい？

* ゴミが発生しない
* メモリの開放/確保を短絡して再利用ができる
  + 再利用を保証できる
    + Functional but In-Placeという書き方ができるようになる
* 参照カウントを形式化して今回のアルゴリズムの正当性を証明した
* （遅いと言われる参照カウントながら）他のメモリ管理システムと同等の速度が出る

===
# 概要
## 技術や手法のキモはどこ？

* 参照カウントの操作（dup/drop）を明示的に扱うことでメモリ操作の最適化ができるようにした
* 参照カウントのdup/dropをゴミが発生した箇所に正確に挿入する、線型論理に似た形式的な規則（アルゴリズム）を開発した。
* 変数環境に線形環境と借用環境を用意し、それら不変条件を維持しながらdup/dropを挿入するようにしてby constructionで正しく変換した

===
# 概要
## どうやって有効だと検証した？

* 健全性（必要なデータが破棄されないこと）→証明
* 正確性（計算の途中でゴミが発生しないこと）→証明
  + 正当性（最終的にゴミがないこと）よりも強い
* パフォーマンス/メモリ使用量→Kokaという言語に実装していくつかの言語とベンチマークを比較した

===
# 概要
## 議論はある？

* 参照カウントの循環参照問題には対応できていない。
  + ただし入っているのが関数型言語で可変参照を使わない限りcycleは起きないので大きな問題にはなってない
  + 例えばswiftなどでもcycle collectionはないがうまくやっている。


===
# 概要
## 次に読むべき論文は？

* Sebastian Ullrich and Leonardo de Moura. Counting immutable beans – reference counting optimized for purely functional programming. In Proceedings of the 31st symposium on Implementation and Application of Functional Languages (IFL’19), September 2019.
  + 参考にしたLean言語について
* Phillip Wadler. Linear types can change the world! In Programming
Concepts and Methods, 1990.
  + 線形型について

===
# 1. Introduction

* 参照カウントは手軽に実装できてメモリ負荷も低いけど最近は下火
* 静的な保証の強い言語で効率的な参照カウントを目指す
  + Leanに実装された参照カウントのアイディアを一般化してKokaで扱う
  + Lean（定理証明支援系）とは違い、Kokaには例外などがある

===
# やったこと

* ゴミが出ない参照カウントをする
* 正確な参照カウントにより最適化ができることを示す
  + ※論文中では具体的なアルゴリズムは示されていない
* 特定の書き方でin-placeな更新になることを保証する
  + それによりFBIPというパラダイムが可能にる
  + TCOにより関数だけでループが書けるようになるのに似てる
* $\lambda^1$ という計算体系を提案し参照カウントを定式化する
* PerceusというKoka言語に搭載されているアルゴリズムを提案する
* Kokaに実装されたPerceusを他のメモリ管理システムとベンチマーク比較する

===

# 2. Overview

* 参照カウントには3つの問題がある
  * 並行性
  * 正確性
  * 循環参照
* 積極評価で関数型で不変なデータ型でで強い静的型付きでエフェクトシステムのある言語という設定でアプローチする

===
## 2.1 Types and Effects

* Kokaでやるよ
* 例外やasync/awaitみたいに関数を最後まで実行しないやつはコンパイラが変換して消してくれるよ
  + 関数末尾に書いたdropが呼ばれないみたいなのは気にしなくてよくなる
* コンパイルするとC11になってランタイム不要で動く

===
## 2.2 Precise Reference Counting

* よくある手法（Rustなど）だと中間にゴミが出るよ

```
fun foo() {
  val xs = list(1,1000000)
  // ここでxsを開放したい
  val ys = map(xs, inc)
  print(ys)
  drop(xs)
  drop(ys)
}
```

===

## 例: map

```
fun map( xs : list ⟨a⟩, f : a -> e b ) : e list ⟨b⟩ {
  match(xs) {
    Cons(x,xx) -> Cons(f(x), map(xx,f))
    Nil        -> Nil
} }
```

===
## 例: map

dup/dropが多い

```
fun map( xs, f ) {
  match(xs) {
  Cons(x,xx) {
    dup(x); dup(xx); drop(xs)
    Cons(dup(f)(x), map(xx, f))
  }
  Nil { drop(xs); drop(f); Nil }
} }
```

===

## 2.3 Drop Specialization

dropのis-uniqueをまとめたい

```
fun drop( x ) {
  if (is-unique(x)) then drop children of x; free(x)
  else decref(x) }
```

===
## 例: map

```
fun map( xs, f ) {
  match(xs) {
    Cons(x,xx) {
      dup(x); dup(xx)
      if (is-unique(xs))
        then drop(x); drop(xx); free(xs)
        else decref(xs)
      Cons( dup(f)(x), map(xx, f))
    }
    Nil { drop(xs); drop(f); Nil }
} }
```

===
## 例: map

dup→dropをfusionできる

```
fun map( xs, f ) {
  match(xs) {
    Cons(x,xx) {
      if (is-unique(xs))
        then free(xs)
        else dup(x); dup(xx); decref(xs)
      Cons( dup(f)(x), map(xx, f))
    }
    Nil { drop(xs); drop(f); Nil }
} }
```


===

## 2.4 Reuse Analysis

* カウントが1のときにdropして再度コンストラクタで構築するのがもったいない
* →カウントが1ならdropせずにメモリに上書きする

```
fun Cons@ru(x, xx) {
  if (ru!=NULL)
  then { ru->head := x; ru->tail := xx; ru } // in-place
  else Cons(x,xx)                            // malloc’d
}
```

===
## 例: map

```
fun map( xs, f ) {
  match(xs) {
  Cons(x,xx) {
    dup(x); dup(xx);
    val ru = drop-reuse(xs)
    Cons@ru(dup(f)(x), map(xx, f))
  }
  Nil { drop(xs); drop(f); Nil }
} }
```

===
## 例: map

さっきまでの最適化も併用できる

```
fun map( xs, f ) {
  match(xs) {
  Cons(x,xx) {
    val ru = if (is-unique(xs))
             then &xs
             else dup(x); (dup xx);
                  decref(xs); NULL
    Cons@ru(dup(f)(x), map(xx, f))
  }
  Nil { drop(xs); drop(f); Nil }
} }
```

===

## 2.5 Reuse Specialization

* reuseするときにフィールドの一部しか変わらないならそこだけ変更するよ

===

## 2.6 A New Paradigm: Functional but In-Place (FBIP)

* （コード例がでてきて長い）
* パターンマッチして即同じ大きさのデータを作り直す系の操作をデータがユニークならin-placeにできる
  + → それに依拠することで新しい書き方が生まれるのでは

===

`tmap` が末尾再帰でないのでスタックは使う

```
type tree {
  Tip
  Bin(left: tree, value : int, right: tree )
}
fun tmap(t : tree, f : int -> int ) : tree {
  match(t) {
    Bin(l,x,r) -> Bin(tmap(l,f), f(x), tmap(r,f) )
    Tip -> Tip
} }
```

===

Fig. 3
tmapが末尾再帰だしreuseされてるのでin-placeになってる

```
type visitor {
  Done
  BinR(right:tree, value : int, visit : visitor )
  BinL(left:tree, value : int, visit : visitor )
}
type direction { Up; Down }
fun tmap(f : int -> int, t : tree,
  visit : visitor, d : direction ) : tree {
  match(d) {
    Down -> match(t) {       // going down a left spine
      Bin(l,x,r) -> tmap(f,l,BinR(r,x,visit),Down) // A
      Tip -> tmap(f,Tip,visit,Up)                  // B
    }
    Up -> match(visit) { // go up through the visitor
      Done -> t                                    // C
      BinR(r,x,v) -> tmap(f,r,BinL(t,f(x),v),Down) // D
      BinL(l,x,v) -> tmap(f,Bin(l,x,t),v,Up)       // E
} } }
```


===

## 2.7 Static Guarantees and Language Features
今回の前提となる機能について

* 2.7.1 Non-Linear Control Flow
  + 例外やasync/awaitはコンパイル時に潰せる
* 2.7.2 Concurrent Execution
  + 並行な実行はどの値が共有されてるか静的に分かる前提
  + でないと実行の50%くらいがリファレンスカウントの同期を取る操作に使われるという調査もある

===

## 2.7 Static Guarantees and Language Features
今回の前提となる機能について2

* 2.7.3 Mutation
  + 可変な参照はあまり使われない+共有される場合は分かる
  + dropの挿入に注意が必要（double-CASとかが必要になる）
* 2.7.4 Cycles
  + ほとんど発生しない前提
  + 可変参照はヤバいけどあんまり使われないよ

===

# 3. A Linear Resource Calculus

* $\lambda^1$ という計算を提案する。
* 線型論理にちょっと似てる。

```
syntax -> derivation --> standard semantics
     |               \-> heap semantics
      \-> syntax driven derivation
          (= Perceus)
```

===

## 3.1 Syntax

Fig. 4

* 灰背景の式はderivationで作られる
* 変数環境に $\Delta$ と $\Gamma$ があって多重集合
* $\lambda^{ys}\ x. e$ はクロージャ（$ys$ をキャプチャする）

===

## 3.2 The Linear Resource Calculus
Fig. 5

* 環境の値は丁度一度使う。例えば2回使う変数はdupをはさむ。1回も使わない変数はdropする
* $\lambda\ x y. x$ は $\lambda\ x y. \mathbf{drop}\ y; x$
* $\Delta$ は借用環境、 $\Gamma$ は線型環境。

Note:

[VAR] 線型環境は空じゃないといけない
[DUP] $x \in \Delta, \Gamma$ （つまり既にxが使われている）かつ環境にxがある（=xが2回以上使われてる）ならdupする
[DROP] 余計な変数はdropする
[APP] $e1\ e2$ をe1を先に評価するので $Gamma_1$ は $e1$ しか使えず、$\Gamma_2$ はe1で借用できる
[LAM] 全ての線型変数をキャプチャする
[BIND], [BIND], [CON] あんまコメントなし


===

## 例 借用環境が生きるケース
<!-- .slide: class="left" -->

$\lambda\ f g x. (f\ x)\ (g\ x)$を変換することを考える。  
借用がないと$\lambda\ f g x. \mathbf{dup}\ x;(f\ x)\ (g\ x)$になる。  
借用に入れとくと実際に使う（VAR、LAM、MATCH）直前にDUPをはさめる。  
$\lambda\ f g x.(f\ (\mathbf{dup}\ x; x))\ (g\ x)$

===
## 補題
<!-- .slide: class="left" -->

$\lceil e\rceil$でeからdropとdupを除いたものとする。

Lemma 1 (この変換は元の式にdup/dropを挟むだけである)  
$\Delta \| \Gamma \vdash e \rightsquigarrow e'$ ならば $e = \lceil e' \rceil$   
証明: 自明

===

## 3.3 Semantics

* Fig.6 に普通の意味論（普通のとは）
* Fig.7 にヒープの意味論がある
* 定理1 ヒープ意味論は健全
* 定理2 評価の途中にヒープにある値は全てreachableである
  + ただし可変参照を入れると循環になる可能性がある
* 略

Note:

ヒープセマンティクスは
(lam), (con) gensymしてヒープに変数を保存し、その変数を返す
(app), (match) でgensymした変数をdropする
(dup), (drop) 値のリファレンスカウント増減
(dlam), (dcon) 複合型のデストラクタ

===
## 3.3 Semantics

これでとりあえず正しくはなるが、dropが遅いケースもある。

$y \mapsto ^1 () | (\lambda x. x) (\mathbf{drop} y; ())$

など。dropそのものがreachableの定義に入っているので不要になるタイミングより遅くdropしても正しいことになってしまう。

===

## 3.4 Perceus
Fig. 8

* （ふつうの）derivationと似たやつ。ただし4つの不変条件をby constructionで維持する
  1. 借用と線形は排他
  2. $\Gamma$ にはexpressionに必要な変数しかない
  3. expressionに必要な変数は$\Delta$と$\Gamma$でカバーできる
  4. （$\Delta$と$\Gamma$は多重集合だが）多重度を1にする


Note:

[SVAR] 変化なし
[SVAR-DUP] 借用してる変数のみdupするように
[SAPP], [SBIND], [SCON] 不変条件を守るためになんかややこしい式に
[SLAM] 借用してる変数だけをdupするように
[SLAM-DROP], [SBIND-DROP], [SMATCH] 使ってない変数はすぐさまdropするように

===
## 定理（Perceusは健全）
Theorem 3  
$ \Delta \| \Gamma \vdash_s e \rightsquigarrow e'$ ならば $\Delta \| \Gamma \vdash e \rightsquigarrow e'$  
証明: Appendix D.4 ※逆は成り立たない

===
## 逆の反例

例えば $(\mathbf{val}\ y = C_0; (\lambda x. x)\ (\mathbf{drop}\ y; C_1))$ は $\vdash$ なら導出できる

```
                       ----------- [VAR]                --------------- [CON]
                       x |- x to x                      ∅ |- C_1 to C_1
                       ------------------------- [LAM]  ------------------------- [DROP]
                       ∅ |- (λ x. x) to (λ x. x)        y |- C_1 to (drop y; C_1)
-------------- [CON]  ------------------------------------------------------------ [APP]
∅ |- C_0 -> C_0        y |- (λ x. x) C_1 -> (λ x. x) (drop y; C_1)
------------------------------------------------------------------------ [BIND]
∅ |- (val y = C_0; (λ x. x) C_1) -> (val y = C_0; (λ x. x) (drop y; C_1))
```

が、$\vdash_s$はSBIND-DROPにいくので  
$(\mathbf{val}\ y = C_0; \mathbf{drop}\ y; (\lambda x. x)\ C_1)$にしかいけない

===
## 定理（Perceusはgarbage free）

Theorem 4 dup/drop以外の場面ではpreciseかつgarbage free
dup/dropを除去した式でもreachableなのでprecise

やっぱり可変参照があるとor in cycleの言明が入る

===

# 4. Benchmark
対象

* Koka:（perceusを実装している言語、C->ネイティブコードにコンパイルされる）
* Koka no-opt:（Overviewで紹介した最適化をしてないやつ）
  + この論文のベンチマークとしてはこっちの方が適切
* OCaml: Stop the worldする世代別コレクタminorだとcopying GCでmajorだとtracing
  + 一般論としてStWするよりはしない方がいい
  + Kokaのほぼ直訳で実装
* Haskell: 多世代別GC。最適化がすごいことで有名
  + Kokaのほぼ直訳だがstrictnessのアノテーションをつけた

===
# 4. Benchmark
対象

* Swift 参照カウントする処理系代表
  + だいたい直訳だがtail callはループに直した
* Java G1GC（参照カウントと同じく）low latencyを謳う
  + Swiftからの直訳
* C++
  + 手動メモリ管理のベースラインとして。
  + 挙動は微妙に違う。

===
# 4. Benchmark
ベンチマーク

* 中規模で非自明でメモリにストレスをかけるタイプ。
* rbtree: 赤黒木への4200万アイテムの挿入
* rbtree-ck: rbtreeと似ているが、5回に1回の結果を保存
  + subtreeの共有をするのでGCへの影響が大きい
* deriv: 記号微分
* nqueens: n-queens問題のサイズ13の結果をリストに保存（してその長さを返す）。
  + 子問題の結果を多数共有する
* cfold: 簡単な計算式の定数畳み込み
* [コード](https://github.com/koka-lang/koka/tree/master/test/bench)

===
## 結果

* Fig.9に結果（Appendix Bに詳細な結果がある）
* 10回の平均でKokaを1として何倍になるかを実行時間と最大物理メモリ使用量（rss）で計測

===
## 解釈
* KokaはC++のstd::mapに肉薄する速度（Kokaは関数型、C++はin-place）
  + C++は16byteアラインされたメモリが必要だがKokaは8byteアラインで済むから？
* rbtreeではKokaとKoka-no-optで2倍差がついてるから最適化大事

===
## 解釈
* それ以外（メモリ共有のある計算）では最適化があんまり効いてない。
  + そもそもメモリ共有がないケースの最適化だからそれはそう
* garbage freeを標榜してる以上全てのケースでメモリ最適でありたかったけどderivでOCamlに負けてる
  + メモリ以外の最適化（case of case）によりアロケーションが減ってそうだった
  + not (yet) implemented in Kokaなので実装されると改善されるかも


===

# 5. Related Work

* Leanの参照カウントに似てる
  + reuse specializationとLean以外の言語に一般化した点が違う。
  + 参照カウントを形式化して
* メモリの再利用やdup/dropを明示的に使ってメモリ使用を最適化する手法はいくつか先行研究がある
  + 配列の更新の最適化が注目されていた。Kokaにはいるかも。なんかBTreeっぽいやつ作ればいけるっしょ

===

# 5. Related Work

* $\lambda^1$ はだいたい線形論理がベースになっている。ただし線形型は静的にカウントするがこっちは動的にカウントする。
  + 線形型のヒープセマンティクスとして静的にメモリ保証するものや動的にメモリ保証するものなどがある
* Swiftは参照カウントでreuse-analysisをしないが、そもそも可変性を許容するのであんまり関係なさそう
* これは静的に参照カウントの操作を減らすが、逆に動的に参照カウントをどうにかするシステムもある
  + 参照カウントの操作をサボって定期的にスタックを走査してカウントを合わせる

===

# 6. Conclusion

* 正確かつreuseとspecializationをする参照カウントシステムのPerceusを提示した
* $\lambda^1$計算を提案した
* Kokaに実装しており、既存のメモリ管理システムと肩を並べる程度の速度がある
* 将来は循環参照問題や借用などに取り組みたい


</script>
</section>
