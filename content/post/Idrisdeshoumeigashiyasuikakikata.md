---
categories: [Idris, 証明, 小ネタ]
date: 2022-09-17T04:17:30+09:00
title: "Idrisで証明がしやすい書き方"
---
κeenです。なんか証明しづらいなって思ってたところをコード側を書き直したらすんなりいけたのでメモ。

<!--more-->

元のコードは以下。何をするコードかはあまり気にしなくてよい。

```idris
export
dequeue : BoundedQueue n a -> (Maybe a, BoundedQueue n a)
dequeue q@(MkQueue [] [] pad) = (Nothing, q)
dequeue {n=S l + m + pad} (MkQueue {l=S l} {m} (x :: xs) rear pad) =
  rewrite plusSuccRightSucc (l + m) pad in
  (Just x, MkQueue xs rear (S pad))
dequeue {n = S m + pad} (MkQueue {l=Z} {m=S m} [] rear pad) =
  case reverse rear of
    e::front =>
      rewrite plusSuccRightSucc m pad in
      rewrite sym $ plusZeroRightNeutral m in
      (Just e, MkQueue front [] (S pad))
```

コードの形を見ると以下のような外形をしている。

```idris
fun ... .. =
    case ... of
     ... => rewrite .. in
            (Just ..., MkQueue ...)
```

`fun` の本体部分が1つの式になっていて、全体を `case` や `rewrite` が包んでいる。
これだと証明するときに `case` や `rewrite` で止まって簡約が進まないのでお手上げになる。

そこで `let` をうまく使って最後の式を `case` や `rewrite` で包まないようにした。


```idris
export
dequeue : BoundedQueue n a -> (Maybe a, BoundedQueue n a)
dequeue q@(MkQueue [] [] pad) = (Nothing, q)
dequeue {n=S l + m + pad} (MkQueue {l=S l} {m} (x :: xs) rear pad) =
  let q: BoundedQueue (l + m + (S pad)) a = MkQueue xs rear (S pad) in
  let q: BoundedQueue (S (l + m + pad)) a = rewrite plusSuccRightSucc (l + m) pad in q in
  (Just x, q)
dequeue {n = S m + pad} (MkQueue {l=Z} {m=S m} [] rear pad) =
  let rear' = reverse rear in
  let e = head rear' in
  let front = tail rear' in
  let q: BoundedQueue (m + Z + S pad) a = MkQueue front [] (S pad) in
  let q: BoundedQueue (m +     S pad) a = rewrite sym $ plusZeroRightNeutral m in q in
  let q: BoundedQueue (S (m + pad))   a = rewrite plusSuccRightSucc m pad in q in
  (Just e, q)
```

`let` は一旦後回しにできるからか簡約が進むらしく、証明するときに `(Just e, q)` まで進んでいた。これならコンストラクタが露出しているので続く証明がやりやすい。

へー、便利ーと思う反面ちょっとしたコードの書き方で証明できる/できないが変わるのは地獄だよねー。
