---
categories: [論文メモ]
date: 2021-08-22T15:01:49+09:00
title: "論文メモの続き：The Third Homomorphism Theorem"
---

[前回](https://keens.github.io/blog/2020/12/27/ronbunmemo_the_third_homomorphism_theorem/)の続きで実例を書いていきます。

<!--more-->

$
\def\cat{\mathrm{++}}
\def\hom#1{{#1}\mathrm{-}準同型}
\def\leftwards#1{{#1}\mathrm{-}左方向}
\def\rightwards#1{{#1}\mathrm{-}右方向}
$


## おさらい
今回の導出を追う上で必要になる知識を思い出します。

リストの第三準同型定理はざっくりいうと「ある処理が `foldr` でも `foldl` でも書けるなら分割統治で書き換えられる」という定理なのでした。それを証明する道中でいくつかの性質も確認しました。

定義（リストの準同型）：
二項演算 $\odot$ に対してリスト上の $h$ 関数が以下の条件を満たすとき、 $h$ を $\hom{\odot}$ という

\\\[
h\ (x \cat y) = h\ x \odot h\ y
\\\]

結合的な演算子 $\odot$ とその単位元 $e$ に対して $hom\ (\odot)\ f\ e$ と書いて、 $\hom{\odot}$ な $h \circ [\cdot] = f$ である（一意な）関数 $h$ を表わす。

定義（左方向関数）：
$h: List a \to b$ が二項演算子 $\oplus: a \times b \to b$ に関して以下の条件を満たすとき、 $h$ を $\leftwards{\oplus}$ という。

\\\[
h ([a] \cat y) = a \oplus h\ y
\\\]


$\leftwards{\oplus}$ 関数 $h$ が $h\ [] = e$ を満たすとき、 $foldr\ (\oplus)\ e$ と書く。このとき $h$ は一意である。

ここで $foldr$ の重要な性質を提示します。
$\cat$ の $foldr$ は2つの $foldr$ に分解できるということ。

\\\[
foldr\ (\oplus)\ e\ (x \cat y) = foldr\ (\oplus)\ (foldr\ (\oplus)\ e\ y)\ x
\\\]

左方向関数と同様に、右方向関数も定義できます。

定義（右方向関数）：
$h: List\ a \to b$ が二項演算子 $\otimes: b \times a \to b$ に関して以下の条件を満たすとき、 $h$ を $\rightwards{\otimes}$ という。

\\\[
h\ (x \cat [a]) = h\ x \otimes a
\\\]

左方向関数と同様に $foldl$ も導入しましょう。
$\rightwards{\otimes}$ 関数 $h$ が $h\ [] = e$ を満たすとき、 $foldl\ (\otimes)\ e$ と書く。このとき $h$ は一意である。

左方向関数と同様に、 `foldl` には重要な性質があります。

\\\[
foldl\ (\otimes)\ e\ (x \cat y) = foldl\ (\otimes)\ (foldl\ (\otimes)\ e\ x)\ y
\\\]

です。

補題1：
全ての計算可能で全関数である、列挙可能な定義域をもつ $h$ に対して、
計算可能（だが部分関数である可能性もある）関数 $g$ が存在して $h \circ g \circ h = h$ を満たす。

定理（第三準同型定理）：
リスト上の関数 $h$ が左方向かつ右方向であるとき、 $h$ は準同型である。


それと前回の内容には含まれていませんが $foldr$ と $foldl$ の追加の性質についても確認しておきます。

\\\[
\begin{align}
foldr\ (\oplus)\  x\ (b:y) & = b \oplus\ foldr\ (\oplus)\ x\ y \\\\\
foldl\ (\otimes)\ x\ (b:y) & = foldl\ (\otimes)\ (x \otimes b)\ y
\end{align}
\\\]

これらを元に進めていきます

## 実例：ソート

挿入ソート（$O(n^2)$）をリストの第三準同型定理（など）を用いてマージソート（$O(n\log n)$）へと変換していきます。

### ソートは左方向かつ右方向

ソートは $ins$ を用いて $foldr$ を使って実装できるので左方向です。

\\\[
sort\ = foldr\ ins\ []
\\\]

ここで $ins$ の定義は以下です。

\\\[
\begin{align}
ins\ a\ []    & = [a] \\\\\
ins\ a\ (b:x) & = \begin{cases}
              a : ins\ b\ x   &, \mathrm{if}\ a \le b \\\\\
              b : ins\ a\ x &, \mathrm{otherwise}
                 \end{cases}
\end{align}
\\\]


同時に右方向でもあります。 何故なら `ins` の引数を逆にした `ins'` を用いて `foldl` で書けるからです。

\\\[
\begin{align}
sort\ &= foldl\ ins'\ [] \\\\\
ins'\ x\ a &= ins\ a\ x
\end{align}
\\\]

### リストの第三準同型定理の適用

$sort$ が右方向かつ左方向であることが分かったので、リストの第三準同型定理より $sort$ はリストの準同型です。つまり、ある二項演算 $\odot$ を用いて以下のように書けるということです。

\\\[
sort (x \cat y) = sort\ x \odot sort\ y
\\\]

リストの第三準同型定理を証明する中での補題1の使い方を思い出すと $\odot$ は $sort \circ unsort \circ sort$ を満たす関数 $unsort$ を用いた以下の定義が条件を満たします。

\\\[
u \odot v = sort (unsort\ u \cat unsort\ v)
\\\]

条件さえ満たせば $unsort$ は何でもいいので $id$ を選びます。すると $\odot$ が簡単になります。

\\\[
u \odot v = sort (u \cat v)
\\\]

まとめると、 リストの第三準同型定理を使えば $sort$ は以下の等式を満たすことが分かります。

\\\[
sort (x \cat y) = sort(sort\ x \cat sort\ y)
\\\]

…あれ？あんまり簡単になってる気がしませんね。実際効率は悪いです。そこでここから効率化していきます。
$\odot$ に渡るリストがソート済みであることに着目すれば効率化できそうです。

### $\odot$ の効率化

リスト $u$ がソート済みである場合 $u = sort\ u$ であることに注目しましょう。
その上で $\odot$ への引数 $u$ と $v$ がソート済みであることに着目して式変形していきます。

\\\[
\begin{align}
u \odot v & = sort (u \cat v)                 & （定義より） \\\\\
          & = foldl\ ins'\ []\ (u \cat v)     & （sortは右方向より） \\\\\
          & = foldl\ ins'\ (foldl\ ins'\ u) v & （foldlの性質より） \\\\\
          & = foldl\ ins'\ (sort\ u) v        & （sortは右方向より） \\\\\
          & = foldl\ ins'\ u\ v               & （uはソート済みより） \\\\\
          & = merge\ u\ v                     & （merge = foldl\ ins' と定義）
\end{align}
\\\]

最後に $foldl\ ins'$ のことを $merge$ と呼ぶようにしました。この $merge$ はマージソートで使われるマージと同じ挙動をします。実際、以下の2つ事実から確認できます。

\\\[
\begin{align}
merge\ u\ []    & = foldl\ ins'\ u\ []           \\\\\
                & = []                           \\\\\
merge\ u\ (b:v) & = foldl\ ins'\ u\ (b:v)        \\\\\
                & = foldl\ ins'\ (ins'\ u\ b)\ v \\\\\
                & = merge\ (ins'\ u\ b)
\end{align}
\\\]


これは $u$ がソート済みであるという仮定の下簡易的に使われる $merge$ の実装と同じものです。
ただし簡易的にと書いた通りこれまた効率は悪いので $merge$ も効率化しましょう。

### $merge$ の効率化

$merge$ は $merge\ u\ v$ の形で使われていて、 $u$ がソート済みであれば正しい実装になっていそうなことを確認しました。ここでは $v$ もソート済みであることを用いて効率化します。

値 $a$ とリスト $v$ について $v$ の全ての要素が $a$ 以上であることを $a \le v$ と表わすことにします。

補題3： 値 $a$ 、 リスト $x$ 、 $y$ について $a \le x$ かつ $a \le y$ であるとき以下が成り立つ

\\\[
foldl\ ins'\ (a : x)\ y = a : foldl\ ins'\ x\ y
\\\]

証明：数学的帰納法による。
任意の値 $a$ リスト $x$ について考える。
$y = []$ の場合、両辺ともに $a:x$ になる。
$y'$ の場合に成り立つとして $y = b : y'$ の場合について考える。

\\\[
\begin{align}
foldl\ ins'\ (a:x)\ (b:y')
  & = foldl\ ins'\ (ins'\ (a:x)\ b)\ y' & （foldlの性質より） \\\\\
  & = foldl\ ins'\ (a : ins'\ x\ b)\ y' & （ins'のa \le bより） \\\\\
  & = a : foldl\ ins'\ (ins'\ x\ b)\ y' & （帰納法の仮定より） \\\\\
  & = a : foldl\ ins'\ x\ (b:y')        & （foldlの性質より）
\end{align}
\\\]

□

補題を証明したので $merge$ の性質を確認していきます。リスト$u$ 、 $v$ をソート済み、値 $a$ 、 $b$ を $a \le u$ 、 $b \le v$ を満たすとして $merge\ u\ []$ 、 $merge\ []\ v$ 、 $merge\ (a:u)\ (b:v)$ について考えます。

$merge\ u\ []$

\\\[
\begin{align}
merge\ u\ [] & = foldl\ ins'\ u\ [] & （mergeの定義より） \\\\\
             & = u                  & （foldlの定義より）
\end{align}
\\\]

$merge\ []\ v$

\\\[
\begin{align}
merge\ []\ v & = foldl\ ins'\ []\ v & （mergeの定義より） \\\\\
             & = sort\ v            & （sortは右方向より） \\\\\
             & = v                  & （vはソート済みより）
\end{align}
\\\]


$merge\ (a:u)\ (b:v)$

\\\[
\begin{align}
merge\ (a:u)\ (b:v) & = foldl\ ins'\ (a:u)\ (b:v)        & （mergeの定義より） \\\\\
                    & = foldl\ ins'\ (ins'\ (a:u)\ b)\ v & （foldlの性質より）
\end{align}
\\\]


ここから $a \le b$ と $a \gt b$ で場合分けします。

$a \le b$ の場合。
仮定より $a \le u$。$a \le b$ と仮定より $a \le v$。さらに $a \le ins'\ u\ b$ がいえます。

\\\[
\begin{align}
foldl\ ins'\ (ins'\ (a:u)\ b) v
  & = foldl\ ins'\ (a : ins'\ u\ b)\ v & （ins'のa \le b）\\\\\
  & = a : foldl\ ins'\ (ins'\ u\ b)\ v & （補題3より） \\\\\
  & = a : foldl\ ins'\ u\ (b:v)        & （foldlの性質より） \\\\\
  & = a : merge\ u\ (b:v)              & （mergeの定義より）
\end{align}
\\\]


$a \gt b$の場合。
仮定より $b \le v$。 $b \lt a$ と仮定より $b \lt u$。

\\\[
\begin{align}
foldl\ ins'\ (ins'\ (a:u)\ b) v
  & = foldl\ ins'\ (b:a:u)\ v   & （ins'のa \gt b）\\\\\
  & = b : foldl\ ins'\ (a:u)\ v & （補題3より） \\\\\
  & = b : merge\ (a:u)\ v       & （mergeの定義より）
\end{align}
\\\]

総合すると、 $merge$ は引数が2つともソート済みである場合以下の性質を持つことが分かります。

\\\[
\begin{align}
merge\ []\ v        & = v \\\\\
merge\ u\ []        & = u \\\\\
merge\ (a:u)\ (b:v) & = \begin{cases}
              a : merge\ u\ (b:x) &, \mathrm{if}\ a \le b \\\\\
              b : merge\ (a:u)\ v &, \mathrm{otherwise}
                 \end{cases}
\end{align}
\\\]


これはそのまま関数として定義できて、よくみるマージソートのマージ関数そのものですね。

こうして挿入ソートからマージソートが導出できました。


## まとめ

リストの第三準同型定理やらなんやらを使う実例として挿入ソートをマージソートに運算できることを示しました。
