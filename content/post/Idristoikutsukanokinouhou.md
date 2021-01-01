---
categories: [Idris, 証明]
date: 2021-01-01T09:42:17+09:00
title: "Idrisといくつかの帰納法"
---

κeenです。
去年末にhatsugaiさんが面白い問題を出してたのでIdrisで解いてみます。

<!--more-->

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">大晦日クイズ<br><br>P は自然数についての述語 <a href="https://t.co/YfaI2QyNeS">pic.twitter.com/YfaI2QyNeS</a></p>&mdash; hatsugai ∈ PRINCIPIA (@hatsugai) <a href="https://twitter.com/hatsugai/status/1344568880346238976?ref_src=twsrc%5Etfw">December 31, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

Twitterの埋め込み画像を見れない人のためにこちらにも書くとこういう命題を証明せよという問題です。

\\\[
(\forall n. P(n) \implies \exists m. m \lt n \land P(m)) \implies \lnot \exists n. P(n)
\\\]

これは[無限降下法](https://ja.wikipedia.org/wiki/%E7%84%A1%E9%99%90%E9%99%8D%E4%B8%8B%E6%B3%95)とよばれる原理です。
自然言語で書くと、「「任意の $n$ について、 $P(n)$ ならば $m \lt n$ なる $m$ が存在して $P(m)$ となる」ならば $P(n)$ となる $n$ は存在しない」となります。任意の $n$ について、それより小さい $m$ が存在することを保証してしまうといつかは0に辿りついて行きづまってしまうのでそんなことはありえない、すなわち $P(n)$ となる $n$ は存在しないはずですので、感覚的には正しそうです。

数学書なんかではほぼ自明なものとしてそのまま使ってしまうことが多い（主観）のですが、ここではそれを証明しろといっています。
原理を証明というのがちょっと面白いので少し触れますね。

# 数学的帰納法とペアノの公理

中学校で数学的帰納法を習ったかと思います。あれは証明しなくていいのでしょうか。
[ペアノの公理](https://ja.wikipedia.org/wiki/%E3%83%9A%E3%82%A2%E3%83%8E%E3%81%AE%E5%85%AC%E7%90%86)の立場に立つと、数学的帰納法は公理として与えられます。つまり証明なく正しいと認めてよい原理です。

一方他にも帰納法っぽいものはあります。
先の無限降下法の他にも $n = k$ ではなく $n \le k$ で成り立つと仮定する完全帰納法など。
これらは公理でないので証明が必要です。
こういう帰納的な原理の証明はやはり数学的帰納法を使うことになります。

ということで今回は「普段あたり前のように使っている無限降下法の原理をちゃんと数学的帰納法で証明できますか」という問いな訳です。やってみましょう。

# Idrisによる回答

Idrisで解いてみます。

まず証明するときの書き出しはこうでしたね。

```idris
%default total
```

最初は命題をIdrisの型にエンコードします。

ところで、暗黙に `P(n)` なる述語が登場しましたね。Idrisのimplicit parameterで `{P: Nat -> Type}` として受け取ってもいいのですが、Idrisには文脈上定義されている値を扱う構文があるのでそれを使ってみましょう。
`using(変数 : 型, 変数: 型) ...` の構文です。

`P` を暗黙に定義しておきましょう。

```idris
using(P : Nat -> Type)
  ...
  ...
```

`using` ブロック内にある諸々の定義は勝手に `using` の引数にあるパラメータが追加されます。

さて、これを用いて問題は以下のようにIdrisにエンコードできるでしょうか。

```idris
using(P : Nat -> Type)
  infDescent : ((n: Nat) -> P n -> (m : Nat ** (m `LT` n, P m))) -> Not (n: Nat ** P n)
```

これを解いていきましょう。方針としては ``(n: Nat) -> P n -> (m : Nat ** (m `LT` n, P m))`` と `(n: Nat ** P n)` を仮定して矛盾を証明すればいいのは分かりますね。
本体の書き出しはこうなりそうです。

```idris
  infDescent h (n ** pn) = ...
```

ここで、 `h` を即座に `n` と `pn` に適用できてしまうので `with` 構文でひとまとめにやってしまいましょう。こうなります。

```idris
  infDescent h (n ** pn) with (h n pn)
    infDescent h (n ** pn) | (m ** (smaller, pm)) = ...
```

``h: (n: Nat) -> P n -> (m : Nat ** (m `LT` n, P m))`` 、 `n: Nat` 、 `pn: P n` 、 `m: Nat` 、 ``smaller: m `LT` n`` 、 `pm: P m` で証明していきます。

安直にはそのまま再帰して `infDescent h (m ** pm)` とやっていくとよさそうな気がしますが、これではIdrisが許してくれません。
Idrisコンパイラに ``m `LT` n`` という情報が伝わってないので無限ループとみなされてしまいます。
そもそものことを思い出してもらうと、数学的帰納法で証明する必要がありそう、という考察があるのでした。 `n` か `m` 、あるいは両方で帰納法を回せないか考えましょう。

`n` （または `m`） で帰納法を回そうとすると `P n` の存在がやっかいなことに気付きます。 `n = S n'` として取り出すと、 `P n'` が成り立たない（成り立つとは限らない）ので仮定が使えなくなります。

私自身これでかなり悩んだのですが、結論をいってしまうと `P n` を捨てると上手くいきます。すなわち以下のような補題を定義してそれを使えば簡単に証明できるのです。

```idris
    infDescent h (n ** pn) | (m ** (smaller, pm)) =  lemma n m smaller pm
    where
      lemma : (n : Nat) -> (m : Nat) -> m `LT` n -> P m -> Void
      ...
```

この補題を証明していきましょう。
方針としては、仮定の ``(n: Nat) -> P n -> (m : Nat ** (m `LT` n, P m))``を使って `m` をどんどん小さくしていきます。ですがこれだけではIdrisが納得しないのは既に説明した通りです。そこで `n` も1ずつ小さくしていきます。
仮定を適用する度に `l < m < n` な `l` が取得でき、これは `l < n - 1` も成り立つので次のサイクルにいけます。
要は `n` で帰納法を回す訳ですね。

それでは `n` で帰納法を回していきましょう。 `n = Z` の場合、 ``m `LT` n`` なる `m` は存在しないので矛盾します。

```idris
      lemma : (n : Nat) -> (m : Nat) -> m `LT` n -> P m -> Void
      lemma     Z     m  contra            pm = uninhabited contra
      ...
```

`n = k` で成り立つ（`lemma k ...` の呼び出しはできる）と仮定して `n = S k`の場合を証明します。
`S k` と `m <= k` の関係（`smaller`）が取り出せます。

```idris
      lemma  (S k) m (LTESucc smaller) pm =
```

そして仮定を使って ``l < m`` なる `l` で `P l` を満たすものが取得できます。

```idris
        let (l ** (lIsSmallerThanM, pl)) = h m pm in
```

すると `l < m <= k` の関係により `l < k` が証明できます。

```idris
        let lIsSmallerThanK : (l `LT` k) = lteTransitive lIsSmallerThanM smaller in
```

あとは `n = k` で成り立つという仮定を使って証明完了です。

```idris
        lemma k l lIsSmallerThanK pl
```


まとめると証明全体はこうなります。

```idris
%default total

using(P : Nat -> Type)
  infDescent : ((n: Nat) -> P n -> (m : Nat ** (m `LT` n, P m))) -> Not (n: Nat ** P n)
  infDescent h (n ** pn) with (h n pn)
    infDescent h (n ** pn) | (m ** (smaller, pm)) =  lemma n m smaller pm
    where
      lemma : (n : Nat) -> (m : Nat) -> m `LT` n -> P m -> Void
      lemma     Z  m  contra           pm = uninhabited contra
      lemma  (S k) m (LTESucc smaller) pm =
        let (l ** (lIsSmallerThanM, pl)) = h m pm in
        let lIsSmallerThanK : (l `LT` k) = lteTransitive lIsSmallerThanM smaller in
        lemma k l lIsSmallerThanK pl
```

# 余談

無限降下法は数学的帰納法と同値らしいです。

<blockquote class="twitter-tweet" data-conversation="none"><p lang="ja" dir="ltr">数学的帰納法と同値なのでそれほど難しくないけど、定理証明支援系でやると少し面倒だった。もっといい方法があるかもしれない。</p>&mdash; hatsugai ∈ PRINCIPIA (@hatsugai) <a href="https://twitter.com/hatsugai/status/1344687309283680256?ref_src=twsrc%5Etfw">December 31, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

今数学的帰納法を使って無限降下法を証明したので、無限降下法を使って数学的帰納法を証明すれば同値であることの証明になります。
無限降下法は否定の証明なので `Not (P n)` を証明することにして以下を証明すればよいでしょう。

```idris
natInd : Not (P Z) -> ((k: Nat) -> Not (P k) -> Not (P (S k))) -> Not (n: Nat ** P n)
```


数学的帰納法の帰結は `(n: Nat) -> Not (P n)` ですが `Not (n: Nat ** P n)` としても同値なので無限降下法の帰結にあわせて変えました。

これを再帰を使わずに `infDescent` を使って証明しようとしたんですが、難しいのでやめました。
無限降下法の仮定で `(n: Nat)` と `P n` を得ても、数学的帰納法の仮定が `(k: Nat) -> Not (P k) -> Not (P (S k))` なので何もできないんですよね。
もちろん帰納法の仮定を `P Z` に `n` 回適用して `Not P n` を作ることはできるんですがそれって数学的帰納法と同じことをやってるので無意味ですし（それやるなら直接 `Not (n: Nat ** P n)` を作った方が早い）。

うまく証明できる方は教えて下さい。

## 追記

形式証明のプロから排中律が必要そうとの示唆をもらいました。

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">数学的帰納法で無限降下法を証明するのは直観主義論理でできそうに見えるけど，逆はなんか排中律要りそうな気がする（完全に見た目だけで判断しています）</p>&mdash; . (@fetburner) <a href="https://twitter.com/fetburner/status/1344927189909684225?ref_src=twsrc%5Etfw">January 1, 2021</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

排中律を仮定して証明しましょう。


``` idris
lawOfExcludedMiddle : {A: Type} -> Either A (Not A)
lawOfExcludedMiddle = believe_me "axiom"
```

すると以下のように再帰を使わずに証明できます。

``` idris
natInd : (P : Nat -> Type) -> Not (P Z) -> ((k: Nat) -> Not (P k) -> Not (P (S k))) -> Not (n: Nat ** P n)
natInd P npz step =
  infDescent sub
where
  sub :  (n: Nat) -> P n -> (m : Nat ** (m `LT` n, P m))
  sub    Z  pz  = absurd $ npz pz
  sub (S k) psk = case lawOfExcludedMiddle {A = P k} of
    Left pk   => (k ** (lteRefl, pk))
    Right npk => absurd $ (step k npk) psk
```

プロの直感ってすごいですね。

# まとめ

素朴な数学的帰納法を使って発展的な無限降下法を証明しました。
見た目の異なる原理を数学的帰納法に乗せるのは意外と頭を使ったので完全帰納法の証明とかもやってみて下さい。
