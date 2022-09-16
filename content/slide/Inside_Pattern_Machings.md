---
categories: ["ML", "OCaml", "言語実装", "Compiler"]
date: 2018-09-16T01:26:15+09:00
description: "ML Day #2 での発表用。パターンマッチの実装について"
title: "Inside Pattern Matchings"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Inside Pattern Matchings
----------------------
[ML Day #2](https://ml-lang.connpass.com/event/94284/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [ちゅーんさんだよー](https://shindanmaker.com/789932)
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===


<pre>
( ^o^)パターンマッチって便利だなー
( ˘⊖˘) 。o(待てよ？なんでこんなに簡単にコードが書けるんだ)
|コンパイラ|┗(☋｀ )┓三
( ◠‿◠ )☛そこに気づいたか・・・消えてもらう
▂▅▇█▓▒░(’ω’)░▒▓█▇▅▂うわああああああ
</pre>


===
# 発表について
-------------

* 元ネタ [Optimizing Pattern Matching](http://pauillac.inria.fr/~maranget/papers/opat/) (Fabrice Le Fessant, Luc Maranget)
* 自作SMLコンパイラにパターンマッチを入れようとしたら難しかった
  + 「コードを生成」が割と厄介
  + 次の中間言語の設計に影響を与える
    - というか専用の中間言語作るくらいの心意気が必要

===
# 発表について
-------------
* 既存手法を調べたら非自明だった
  + → 既存手法の紹介だけでも価値がありそう
  + → 発表するか
  + (本当は実装までしたかったけど進捗だめです)
* 変数束縛の話はしない
  + ワイルドカードで我慢して

===
# パターンについて
------------------

* パターンは値の集合にマッチする
* ワイルドカードパターンがある
* 直積にマッチするときは _横_ に伸びる
* 直和にマッチするときは _縦_ に伸びる
* パターンをネストするときは _奥_ に伸びる
* パターンが _網羅的_ であるかに関心がある
* 複数マッチする場合は最初のものにマッチする

===

# 例
----

``` standard-ml
case (lx, ly) of
   ([], _) => 1
 | (_, []) => 2
 | (x::xs, y::ys) => 3
```

===
# パターンマッチの実装
---------------------

* いくつか方法がある
  + 素朴なifのチェーン
  + decision tree
  + backtracking automata
* それぞれメリットデメリットがある

===

# ifのチェーン
-------------

``` C
if (lx == [] && true) {
  return 1;
} else if (true && ly == []) {
  return 2;
} else if (lx == (::) &&  ly == (::)) {
  // discriminantとデータは別
  x = lx.0;
  xs = lx.1;
  y = ly.0;
  ys = ly.1;
  return 3;
} else {
  // match fail
}
```

===

# ifのチェーン
-------------

* パターンを1つ1つifで検査する
* 最初に思いつく
* O 素朴
* O 実装が楽
* X 遅い(横、縦、深さ全てに比例)
* X 網羅性検査は別途実装

===
# decision tree
----------------


``` C
swith(lx) {
  case []: return 1;
  case (::): switch (ly) {
    case []: return 2;
    case (::): return 3;
  }
}
```

===
# decision tree
--------------

* パターンから決定木を作る
  * 直和は `switch` に変換する
  * 直積は `switch` のネストに変換する
  * パターンのネストは`switch`のネストに変換する
* ifのチェーンの次に思いつく
* O 実行が(横幅と深さに)線形
* O 網羅性検査がfor freeでついてくる
* X コードが嵩む(パターンがコピーされうる)

===
# decision tree
--------------

* 最初に実装しようとした
* パターンがネストしたケースで実装が難しかった
  + 主にデータの持ち方の問題
  + 1つの節の中にネストさせるパターンを集めるのが大変
* 直積とパターンのネストどっちを先にやるかは調べてない
* ORパターンを入れるとdecision diagramになりそう？
  + CFGが欲しくなる

===
# backtracking automata
------------------------

``` 
catch
  catch
    switch lx with
      case []: 1
      default: exit
  with catch
    switch ly with
      case []: 2
      default: exit
  with catch
    switch lx with
      case (::): switch ly with
                   case (::): 3
                   default: exit
      default: exit
with (failwith "Patrtial match")

```

===
# backtracking automata
------------------------

* 順番に検査しつつ失敗したら巻き戻す
  + パターンの並びのままprefixが共通なら共有する
  + 例外とハンドラを使う
* O コードがコンパクト(パターンはコピーされない)
* X decision treeほど速くない
* O 最適化ができる
  + まずは動くものを作ってあとで高速化

===
# backtracking automata
------------------------

* 今回紹介する内容
* DT vs BA
* 少し正規表現のDFA vs NFAに似てる
  + ただしパターンマッチはコード生成の話
* Backtracking Automataを最適化するとそれなりに速い

===

# イメージ
----------

## if

```
.-.-
.-.-
.-.-
.-.-
```

===

# イメージ
----------

## DT

```
   /
  .
 / \
.
 \./
   \

```

===

# イメージ
----------

## BA

```
.-.-
   \
.-.-
   \
```


===
# 準備
-------
## データ
* データは直和と直積からなるとする
  + intも無限の直和からなるとする
* 値はコンストラクタで作られる
  $v ::= c(v1, .., vn)$
* パターンへの入力はベクトルで与えられる
  $\vec{v} = (v_1 \cdots v_n)$

===
# 準備
-------
## パターン

* パターンは2つ $p ::=$
  + $\mathbf{\\\_}$ (ワイルドカード)
  + $c(p_1, .., p_n)$ (コンストラクタ)
* パターンもベクトルになる
  $\vec{p} = (p_1 \cdots p_n)$

===

# 準備
-------
## 節行列

* パターンは節が複数あるので行列になる

``` standard-ml
case (lx, ly) of
   ([], _) => 1
 | (_, []) => 2
 | (_::_, _::_) => 3
```

を

\\[
\begin{equation\*}
(P \to L) = \begin{pmatrix}
[] & \mathbf{\\\_}  & → & 1 \\\\  
\mathbf{\\\_} & [] & → & 2 \\\\  
(::) & (::) & → & 3
\end{pmatrix}
\end{equation\*}
\\]

に

===

# 準備
-------
## ターゲット言語

* `let`バインディング
* `catch l1 with l2` と `exit`
* `switch` 式
  + `default` 節は省いて良い
  + 定義上は`case` に引っかからず `default` もなければUB
  + 実際はそういうコードを吐かない
* フィールドアクセス `field n x`

===
# コンパイル
------------

* 値 $\vec{x}$ を節行列 $P \to L$ にマッチさせるとする
* 手続き $\mathcal{C}((x), P \to L)$ でコンパイルする
* $\vec{x} = (x_1 \cdots x_n)$
* \\[
\begin{equation\*}
(P \to L) = \begin{pmatrix}
p^1\_1 & \cdots & p^1\_n & → & l^1 \\\\  
&        & \vdots &  &  \\\\  
p^m\_1 & \cdots & p^m\_n & → & l^m
\end{pmatrix}
\end{equation\*}
\\]

===
# コンパイル
------------

* ただしパターンは少なくとも1つはあるとする
  + つまり $m > 0$
* 節なしマッチは許可されない or 特別扱いが多いので一般性を損ねない
* 最初はマッチ失敗のガードから始める

``` standard-ml
catch
  C((x), P → L)
with (failwith "Patrtial match")
```

===
# $n == 0$
-------

* $n$ が0、つまりパターンが残っていなければ最初のパターンがマッチする

\\[
\begin{equation\*}
\mathcal{C}((), \begin{pmatrix}
→     & l^1 \\\\  
\vdots &  \\\\  
→     & l^m
\end{pmatrix}) = l^1
\end{equation\*}
\\]

===
# $n > 0$
-------
## (a)変数則

* 最初の列のパターンが全てワイルドカードならマッチさせない。つまり
  $\mathcal{C}(\vec{x}, P \to L) = \mathcal{C}(\vec{x^{\prime}}, P^{\prime} \to L^{\prime})$
* $\vec{x^{\prime}} = (x_2 \cdots x_n)$
* \\[
\begin{equation\*}
(P \to L) = \begin{pmatrix}
p^1\_2 & \cdots & p^1\_n & → & l^1 \\\\  
&        & \vdots &  &  \\\\  
p^m\_2 & \cdots & p^m\_n & → & l^m
\end{pmatrix}
\end{equation\*}
\\]


===
# $n > 0$
-------
## (b)コンストラクタ則

* 最初の列のパターンが全てコンストラクタならコンストラクタ毎に分解する
* 雑にいうとこうなる

```
switch x1 with
  case c1: C(...)
  case c2: C(...)
     ...
  default: exit
```

===
# $n > 0$
-------
## (b)コンストラクタ則
* コンストラクタ $c$ に特殊化された節行列$\mathcal{S}(c, P \to L)$を次のように定義する
  + $p^i_1 = c(q^i_1, \cdots, q^i_a)$のとき$q^i\_2 \cdots q^i\_a p^i\_2 \cdots p^i\_n \to l^i$
  + $p^i_1$ が $c^{\prime} \not= c$ のときナシ
* 各コンストラクタの腕を以下のようにする

```
(let (y_1 (field 0 x_1)) ... (y_a (field (a-1) x_1))
  C((y_1, ..., y_a, x_2, ..., x_n), S(c, P → L)))
```


===
# $n > 0$
-------
## (c) ORパターン則

* ORパターンは扱わないので飛ばす
* 元論文だとORパターンの最適化も扱っており重要

===
# $n > 0$
-------
## (d) 混合則

* 上記どのルールにも当てはまらない場合は節行列を2分割する
  + 先頭から(a), (b), (c)のどれかが適用できる最大の集合を前半、残りを後半とする
* コードは以下のように生成する

```
catch
  C(\vec{x}, P_1 → L_1)
with C(\vec{x}, P_2 → L_2)
```

===

# 例
----

``` standard-ml
case (lx, ly) of
   ([], _) => 1
 | (_, []) => 2
 | (x::xs, y::ys) => 3
```

===
# 例
-----

* 混合則を2回適用する

``` standard-ml
catch
  catch
    C((lx ly), ([] _ → 1))
  with catch
    C((lx ly), (_ [] → 2))
  with C((lx ly), (x::xs y::ys → 3))
with (failwith "Patrtial match")
```



===
# 例
-----

* 順にコンストラクタ則、変数則、コンストラクタ則を適用する

```
catch
  catch
    switch lx with
      case []: C((ly), (_ → 1))
      default: exit
  with catch
      C((ly), ([] → 2))
  with switch lx with
         case (::): C((ly), (y::ys → 3))
         default: exit
with (failwith "Patrtial match")

```


===
# 例
-----

* 順に変数則、コンストラクタ則、コンストラクタ則を適用する


```
catch
  catch
    switch lx with
      case []: C((), (→ 1))
  with catch
    switch ly with
      case []: C((), (→ 2))
      default: exit
  with switch lx with
         case (::): switch ly with
                      case (::): C((), (→ 3))
                      default: exit
with (failwith "Patrtial match")

```

===
# 例
-----
* n == 0のケースで終了



```
catch
  catch
    switch lx with
      case []: 1
      default: exit
  with catch
    switch ly with
      case []: 2
      default: exit
  with switch lx with
         case (::): switch ly with
                      case (::): 3
                      default: exit
         default: exit
with (failwith "Patrtial match")

```

===

# 最適化(概要)
--------

* 初手3分割だった
* 2節目と3節目が入れ替わってたら2分割で済んでた
* 勝手に入れ替えればいいのでは？
  + ただしマッチの順番を変えてはいけない
* → マッチの順番を変えずに入れ替えられる条件を調べよう
* 他にはバックトラックで遠くに飛ぶためにcatchにラベルを付けたり
* 続きは論文で

===
# まとめ
---------

* パターンマッチには少なくとも3種類の実装があるよ
* そのうちバックトラッキングのアルゴリズムを紹介したよ
  + 変数とORパターンは紹介してないよ
* 最適化もありうるよ
  + 紹介してないよ




</textarea>
