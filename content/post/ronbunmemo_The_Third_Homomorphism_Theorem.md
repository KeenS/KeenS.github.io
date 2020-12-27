---
categories: [論文メモ]
date: 2020-12-27T09:47:31+09:00
title: "論文メモ：The Third Homomorphism Theorem"
---

κeenです。[Jeremy Gibbsons. 1995. "The Third Homomorphism Theorem"](https://www.researchgate.net/publication/2628619_The_Third_Homomorphism_Theorem) を読んだメモ。
いわゆるリストの第三準同型定理と呼ばれるものについて、原典をあたってみたのでメモを残しておく。

<!--more-->

リストの第三準同型定理はざっくりいうと「ある処理が `foldr` でも `foldl` でも書けるなら分割統治で書き換えられる」という定理です。
運算というプログラムを書き換えて効率的なアルゴリズムを導出する分野でよく使われています。

よく使われる定理なんですが、「どこが準同型なの？」とか「第一と第二どこいった」とか色々気になるので掘り返してみます。
昔、Qiitaによくまとまった記事があったんですが今探したら見当りませんでした。
自分が分かりやすいように色々いじって書くので興味のある人はこのブログで納得せずに原文を読んで下さい。

$
\def\cat{\mathrm{++}}
\def\hom#1{{#1}\mathrm{-}準同型}
\def\leftwards#1{{#1}\mathrm{-}左方向}
\def\rightwards#1{{#1}\mathrm{-}右方向}
$

# リストとは

情報系の論文にありがちですが、どういう対象に対して議論しているのかあまりはっきりせずに書かれています。
雰囲気で察してあげましょう。

「リストは全てが同じ型の要素の有限列である。リストはempty、singleton、2つのリストの結合からなる」と書かれているのでHaskell風に書くと以下のようなデータ構造のようです。

```haskell
data List a = Empy | Singleton a | Concat (List a) (List a)
```

ジェネリクスであるとかはちゃんと書かれてないですが察してあげました。

ここで便利記法を導入します。

```
[]        = Empty
[a]       = Singleton a
a ++ b    = Concat a b
[a, b, c] = [a] ++ [b] ++ [c]
a : x     = [a] ++ x (右結合)
```

「`++` は結合的かつ `[]` はその単位元である」とあるので `Concat a (Concat b c) = Concat (Concat a b) c` とみなす特別な等値関係があるのかもしれません。謎です。
一応この等値関係の元では `++` を適切に定義してあげれば `List a = Nil | Cons a (List a)` と同型になるのかな？ちゃんと確認してないですが。

因みに関数適用は $f$ を $x$ に適用するときは $f\ x$ と書きます。
$\circ$ で関数結合を表わすと書いてあります。どの順序か明記されてないですが、 $f\ \circ\ g = f\ (g\ \mathrm{-} )$ のようです。

# リストの準同型

例えば群の準同型なら $f(x \cdot y) = f(x) \cdot f(y)$ が成り立つこと。
リストの準同型は $\cat$ に対する準同型のよう。

定義（リストの準同型）：
二項演算 $\odot$ に対してリスト上の $h$ 関数が以下の条件を満たすとき、 $h$ を $\hom{\odot}$ という

\\\[
h\ (x \cat y) = h\ x \odot h\ y
\\\]


何も書かれてないですが、 $h: List\ a \to b$ と $\odot: b \times b \to b$ についての記述のようです。

この条件から （$h$ のコドメイン上で） $\odot$ も結合的であり、 $h []$ は（存在すれば） $\odot$ の単位元であることがすぐさま分かります。

例：
リストの要素の総和をとる関数 $sum: List\ Integer \to Integer$ は $\hom{+}$ である。
実際、以下が成り立つ

\\\[
sum\ (x \cat y) = sum\ x + sum\ y
\\\]

例：
リストのを長さを計算する関数 $length: List\ a \to Integer$ は $\hom{+}$ である。
実際、以下が成り立つ。

\\\[
length\ (x \cat y) = length\ x + length\ y
\\\]


論文には $sum$ や $length$ の型は一切書かれてないですが、名前から察しました。

$h\ []$ は存在しないこともあるらしいです。それ準同型って言っていいのかなぁ。
例えば $head$  $a << b = a$ で定義される演算子 $<< : List a \times List a \to List a$ は $\hom{<<}$ ですが $head\ []$ は未定義です。
これは $<<$ に単位元が存在しないことにも対応します。


結合的な演算子 $\odot$ とその単位元 $e$ に対して $hom\ (\odot)\ f\ e$ と書いて、 $\hom{\odot}$ な $h \circ [\cdot] = f$ である（一意な）関数 $h$ を表わす。
何の言及もなく $f$ がでてきましたが、 $f: a \to b$ 、 $h: List\ a \to b$ のようです。
$h$ の一意性について特に言及がないので確認しましょう。empty、singleton、concatenationでそれぞれ場合分けすればよさそうです。

\\\[
\begin{align}
h\ []         & = e & （hは準同型より）            \\\\\
h\ [a]        & = f\ a & （h \circ [\cdot] = fより）  \\\\\
h\ (x \cat y) & = h\ x \odot h\ y & （hは準同型より）
\end{align}
\\\]

$e$ 、 $\odot$ 、 $f$ が固定されているので正しそうです。

原文では特に名前がないのですが、 $h$ のことを $(\odot, e, f)$ から導出される関数とでも呼びましょうか。


例：
$sum$ は $(+, id, 0)$ から導出される関数である。つまり、 $sum = hom\ (+)\ id\ 0$である。そもそも $sum$ の定義が与えられてないので確かめられませんが、そうなんでしょう。あるいはこれを $sum$ の定義とみなしましょう。

例：
$length$ $(+, one, 0)$ から導出される関数である。つまり、 $length = hom\ (+)\ one\ 0$ である。ここで $one: a \to Integer$ は $one\ x = 1$ で定義される関数である。

# 左方向関数、右方向関数

定義（左方向関数）：
$h: List a \to b$ が二項演算子 $\oplus: a \times b \to b$ に関して以下の条件を満たすとき、 $h$ を $\leftwards{\oplus}$ という。

\\\[
h ([a] \cat y) = a \oplus h\ y
\\\]

leftwordsの訳に左方向を充てたら珍妙な用語が誕生してしまいましたがこのままいきましょう。
$\oplus$ は結合的でなくてもよいことに注意して下さい。

$\leftwards{\oplus}$ 関数 $h$ が $h\ [] = e$ を満たすとき、 $foldr\ (\oplus)\ e$ と書く。このとき $h$ は一意である。

また $h$ の一意性を確かめましょう。 $[]$ と $[a]$ の場合は簡単です。

\\\[
\begin{align}
h\ []     & = e & （条件より） \\\\\
h\ [a]    & = a & （h\ [a] = h\ ([a] \cat [])と条件より）
\end{align}
\\\]

$h\ (x \cat y)$ についてですが、恐らく $\cat$ が結合的であることを利用して $h\ ([] \cat z)$ または $h\ ([a] \cat z)$ に書き換えるんだと思います。
そうすれば残りも以下のように確認できます。

\\\[
\begin{align}
h\ ([]  \cat z) & = h\ z           & （\catは[]の単位元より） \\\\\
h\ ([a] \cat z) & = a \oplus h\ y  & （条件より）
\end{align}
\\\]

$h$ が一意であることが確認できました。

例（$lsp$）： リストの最長整列済み接頭辞（longest sorted prefix）を求める関数 $lsp$ は以下の定義をもつ二項演算子 $\oplus$ により $\leftwards{\oplus}$ である。

\\\[
\begin{align}
a \oplus []    &= [a] \\\\\
a \oplus (b:x) &= \begin{cases}
    a : b : x &, \mathrm{if}\ x \le b \\\\\
    [a]       &, \mathrm{otherwise}
  \end{cases}
\end{align}
\\\]

さらに $lsp\ [] = []$ であるので $lsp = foldr\ (\oplus)\ []$ である。

ここで $foldr$ の重要な性質を2つ提示します。
1つは `foldr` の計算が右から左に走るということ。

\\\[
foldr\ (\oplus)\ e\ \[a\_1, a\_2, a\_3\] = a\_1 \oplus (a\_2 \oplus (a\_3 \oplus e))
\\\]

もう1つ、これを一般化して concatenateの $foldr$ は2つの $foldr$ に分解できるということ。

\\\[
foldr\ (\oplus)\ e\ (x \cat y) = foldr\ (\oplus)\ (foldr\ (\oplus)\ e\ y)\ x
\\\]

左方向関数と同様に、右方向関数も定義できます。

定義（右方向関数）：
$h: List\ a \to b$ が二項演算子 $\otimes: b \times a \to b$ に関して以下の条件を満たすとき、 $h$ を $\rightwards{\otimes}$ という。

\\\[
h\ (x \cat [a]) = h\ x \otimes a
\\\]

左方向が $\oplus$ で右方向が $\otimes$ です。
こちらも $\otimes$ は結合的である必要はありません。

左方向関数と同様に $foldl$ も導入しましょう。
$\rightwards{\otimes}$ 関数 $h$ が $h\ [] = e$ を満たすとき、 $foldl\ (\otimes)\ e$ と書く。このとき $h$ は一意である。
一意性の確認は省略します。

左方向関数と同様に、 `foldl` には2つの重要な性質があります。

\\\[
foldl\ (\otimes)\ e\ \[a\_1, a\_2, a\_3\] = ((e \otimes a_1) \otimes a_2) \otimes a_3
\\\]

と、これを一般化したもの

\\\[
foldl\ (\otimes)\ e\ (x \cat y) = foldl\ (\otimes)\ (foldl\ (\otimes)\ e\ x)\ y
\\\]

です。

# 第一、第二準同型

「第一と第二どこいった」と気になる人のために第一準同型定理と第二準同型定理のステートメントが紹介されています。
その前に2つの用語定義しておきます。


定義(畳み込み)：
ある $\odot$ を用いて $hom\ (\odot)\ id\ e$ と書ける関数を畳み込みと呼ぶ。

ここで $id$ は引数をそのまま返す関数、 $e$ は言及がないですが、 $\odot$ の単位元なんでしょう。

定義（マップ）：
関数 $f$ に対して $hom\ (\cat)\ (\[\cdot\] \circ f)\ []$ を $map\ f$ と書き、マップと呼ぶ。

mapを写像と訳すか迷ったんですがマップとしておきます。
これで第一、第二準同型定理を述べられます。

定理（第一準同型定理）：
全てのリストの準同型関数は畳み込みとマップの合成で書ける。
すなわち、準同型関数が $hom\ (\odot)\ f\ e$ と書けるとすると、以下のようにマップと畳み込みの合成の形に分解できる。

\\\[
hom\ (\odot)\ f\ e = hom\ (\odot)\ id\ e \circ map\ f
\\\]

逆に、ある関数がこのような合成で書けるならリストの準同型である。


定理（第二準同型定理、特殊化定理）：
全てのリストの準同型関数は左方向関数であり、かつ右方向関数である。
すなわち $\odot$ が結合的ならば以下の等式が成り立つ

\\\[
\begin{align}
hom\ (\odot)\ f\ e & = foldr\ (\oplus)\ e  & \mathrm{where}\ a \oplus  s = f\ a \odot s \\\\\
                   & = foldl\ (\otimes)\ e & \mathrm{where}\ r \otimes a = a \odot f\ a
\end{align}
\\\]

# 第三準同型定理

第三準同型定理は証明まで載っています。
まずはステートメントです。

定理（第三準同型定理）：
リスト上の関数 $h$ が左方向かつ右方向であるとき、 $h$ は準同型である。

第二準同型定理の逆ですね。

証明にあたって補題を2つ準備します。

補題1：
全ての計算可能で全関数である、列挙可能な定義域をもつ $h$ に対して、
計算可能（だが部分関数である可能性もある）関数 $g$ が存在して $h \circ g \circ h = h$ を満たす。

証明： $g$ を以下の疑似コードで定義する。

```
g t =
  for x in domain of h
    if h x == t
      return x
```


せやな。 $h\ x = t$ を満たす $x$ がないこともあるので $g$ は部分関数になることがあります。

補題2：次の2つは同値である。

1. $h$ がリストの準同型である
2. $h\ v = h\ x$ かつ $h\ w = h\ y$ ならば $h\ (v \cat w) = h\ (x \cat y)$

証明：
$1 \to 2$ は簡単。
$h$ をリストの準同型関数とする。
$h\ v = h\ x$ かつ $h\ w = h\ y$ を仮定すると

\\\[
\begin{align}
  h (v \cat w) & = h\ v \cat h\ w & （hの準同型性より） \\\\\
               & = h\ x \cat h\ y & （仮定より）        \\\\\
               & = h\ (x \cat y)  & （hの準同型性より）
\end{align}
\\\]

$2 \to 1$ を示す。
任意のリスト上の関数 $h$ が $h\ v = h\ x$ かつ $h\ w = h\ y$ ならば $h\ (v \cat w) = h\ (x \cat y)$ であるとする。
$h$ をリスト上の関数とする。 補題1より $h \circ g \circ h = h$ を満たす $g$ が存在するのでそれをとる。
この $g$ を用いて二項演算子 $\odot$ を以下のように定義する

\\\[
t \odot u = h\ (g\ t \cat g\ u)
\\\]


$h$ が $\hom{\odot}$ であることを示す。
$g$ のとり方により $h\ x = h\ (g\ (h\ x))$ 、 $h\ y = h\ (g\ (h\ y))$ である。

\\\[
\begin{align}
h (x \cat y) & = h (g (h x) \cat h (h y))   & （仮定より） \\\\\
             & = h\ x \odot h\ y            & （\odot の定義より）
\end{align}
\\\]


仮定よりのところで悩んでたんですが、 $h\ x = h\ (g\ (h\ x))$ かつ $h\ y = h\ (g\ (h\ y))$ が成り立つので $h\ (x \cat y) = h\ (g\ (h\ x) \cat h\ (h\ y))$ がいえるんですね。
これ書いてて気付きました。スッキリスッキリ。

準備の万端が整ったのでリストの第三準同型定理を証明します。

証明：
リスト上の関数 $h$ を左方向かつ右方向とする。
$h$ は単位元 $e$ （= $h\ []$）、二項演算子 $\oplus$ 、 $\otimes$ を用いて
$h = foldr\ (\oplus)\ e = foldl\ (\otimes)\ e$ と書ける。

補題2を用いて証明する。
適当なリスト $v, w, x, y$ について $h\ v = h\ x$ かつ $h\ w = h\ y$ であるとする。
すると

\\\[
\begin{align}
h\ (v \cat w) & = foldr\ (\oplus)\ e\ (v \cat w)                & （hは左方向より） \\\\\
              & = foldr\ (\oplus)\ (foldr\ (\oplus)\ e\ w)\ v   & （foldrの性質より） \\\\\
              & = foldr\ (\oplus)\ (foldr\ (\oplus)\ e\ y)\ v   & （h\ w = h\ yとfoldr\ (\oplus)\ e = hより） \\\\\
              & = foldr\ (\oplus)\ e\ (v \cat y)                & （foldrの性質より） \\\\\
              & = h\ (v \cat y)                                 & （foldr\ (\oplus)\ e = h より） \\\\\
              & = foldl\ (\otimes)\ e\ (v \cat y)               & （hは左方向より） \\\\\
              & = foldl\ (\otimes)\ (foldl\ (\otimes)\ e\ v)\ y & （foldlの性質より） \\\\\
              & = foldl\ (\otimes)\ (foldl\ (\otimes)\ e\ x)\ y & （h\ v = h\ xとfoldl\ (\otimes)\ e = hより） \\\\\
              & = foldl\ (\otimes)\ e\ (x \cat y)               & （foldlの性質より） \\\\\
              & = h\ (x \cat y)                                 & （foldl\ (\otimes)\ e = h より）
\end{align}
\\\]

補題2より $h$ はリストの準同型。

論文ではこのあと実例が続くんですが（`insert` が右方向かつ左方向であることを使ってインサートソートからマージソートを導出する）、長くなるのでメモは一旦ここまで。
気が向いたら続きを書きます。
