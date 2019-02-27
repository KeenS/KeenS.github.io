---
categories: [Idris]
date: 2019-02-28T00:32:46+09:00
title: "Idrisで依存キュー"
---
κeenです。なんか雑にIdrisでキューライブラリを作ったので紹介します。

<!--more-->

ひとまずコードは[これ](https://gitlab.com/blackenedgold/idris-dependent-queue)。


関数型キューというのがあります。関数型プログラミングでよく使うデータ構造にリストがあります。
リストは先頭への操作が速いのでスタックとして使うのに向いています。
一方でキューは後ろに入れて前から取り出します。
後ろから入れるのがリストには向いていないのですが、リストを2つ用意して入れるときは後ろのリスト、取り出すときは前のリストから操作すれば両方速いだろというのが基本的なアイディアです。
前のリストが空になったら後ろのリストをreverseして前のリストにします。1要素につき1回しかreverseが走らないので平均するとenqueue/dequeueが $O(1)$ で実現できます。
だいたいこんな感じで実装されます。


```idris
data Queue : Type -> Type where
  MkQueue: (front : List a) -> (back : List a) -> Queue a

empty : Queue a
empty = MkQueue [] []

enqueue : Queue a -> a -> Queue a
enqueue (MkQueue front back) x = MkQueue front (x :: back)

dequeue : Queue a -> (Maybe a, Queue a)
dequeue (MkQueue (x :: xs) back) = (Just x, MkQueue xs back)
dequeue (MkQueue [] []) = (Nothing, MkQueue [] [])
dequeue (MkQueue [] back) = dequeue (MkQueue (reverse back) [])
```

関数型キューなので変更前のデータも残るのが特徴です。上記のコードは操作前のデータを再利用するとパフォーマンス的によろしくない場合があるのですが詳しくはググって下さい。

で、今回はこれの依存型版、長さ情報も型に持ったものを作ろうというお話です。`List` に対して `Vect` があるように 普通のキューに対して長さ情報を持ったキューを作ります。

データ型の定義はこのようになります。

```idris
import Data.Vect

export
data Queue : Nat -> Type -> Type where
  MkQueue: {l, m: Nat} -> (front : Vect l a) -> (back : Vect m a) -> Queue (l + m) a

```

リストではなく `Vect` を使っていますね。
`Queue : Nat -> Type -> Type` と型コンストラクタの引数が1つ増えて長さ情報を持ちます。
また、 `MkQueue` の方も `front` と `back` の長さ合計が全体の長さなのでそのような定義になっています。

上記定義から即座に以下のような関数は書けますね。

```idris
export
empty : Queue 0 a
empty = MkQueue [] []

export
isEmpty : Queue n a -> Bool
isEmpty (MkQueue [] []) = True
isEmpty _ = False

export
length : Queue n a -> Nat
length (MkQueue front back) = (length front) + (length back)
```


Idrisの関数はオーバーロードできるので `Queue` の `length` の定義中で `Vect` の `length` を呼んでいます。

さて、このまま `enqueue` を実装しようとするとエラーになります。


```idris
export
enqueue : Queue n a -> a -> Queue (1 + n) a
enqueue (MkQueue front back) x = MkQueue front (x :: back)

```

```text
- + Errors (1)
 `-- Queue.idr line 26 col 3:
     When checking right hand side of enqueue with expected type
             Queue (1 + (l + m)) a
     
     Type mismatch between
             Queue (l + S m) a (Type of MkQueue front (x :: back))
     and
             Queue (S (plus l m)) a (Expected type)
     
     Specifically:
             Type mismatch between
                     plus l (S m)
             and
                     S (plus l m)

```

Idrisの自然数は1進数を採用しているので `1+m` は `S m` と表記されていますし、 `1 + (l + m)` は `S (plus l m)` と表記されています。

返り値の型は `1 + n` と書いています。
実際の型は `front` の長さを `l` 、`back` の長さを `m` とすると、返している値の型は `front` は変化しないので長さ `l` のまま、 `back` は1要素追加しているので `1 + m` になっています。
これを併せると `l + (1 + m)` になります。
もちろん、 `l + m = n` が成り立っているので人間が手で計算する限りでは正しい型の値を返しています。しかしIdrisはそこまで計算してくれないのでコンパイルエラーになります。
なのでIdrisコンパイラに「両者の型は同じなんだよ」と教えてあげる必要があります。
今回は `l + m = n` というのが分かっているので `l + (1 + m)` が `1 + (l + m)` と等しいことを納得させてあげるとコンパイルが通ります。

まず、 `MkQueue` の内部で使われている変数 `l` と `m` を捕捉しましょう。くるくる括弧で暗黙のパラメータを捕捉できます。

```idris
export
enqueue : Queue n a -> a -> Queue (1 + n) a
enqueue (MkQueue {l} {m} front back) x = ...

```


次に `l + (1 + m)` と `1 + (l + m)` が等しいと納得させる手段ですが、 [`Prelude.Nat`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Nat.html) に以下のような関数が定義されています。

``` idris
plusSuccRightSucc : (left : Nat) -> (right : Nat) -> S (left + right) = left + S right
```

完全に我々が求めているものですね。これを使ってIdrisを納得させるのは `rewrite` 構文が使えます。
なので `enqueue` は以下のように書けます。

``` idris
export
enqueue : Queue n a -> a -> Queue (1 + n) a
enqueue (MkQueue {l} {m} front back) x =
   rewrite plusSuccRightSucc l m in
   MkQueue front (x :: back)
```


`rewrite` で返り値の型の `Queue (1 + n) a` を `S (left + right) = left + S right` を使って左辺 → 右辺の書換えをしています。

さて、同様に `dequeue` です。
まずはナイーブに実装してみると `enqueue` 同様にエラーが出ます。

``` idris
export
dequeue : Queue (1+n) a -> (a, Queue n a)
dequeue (MkQueue (x :: xs) back) = (x, MkQueue xs back)
dequeue (MkQueue [] back) =
  dequeue (MkQueue (reverse back) [])

```

```text
- + Errors (1)
 `-- Queue.idr line 33 col 3:
     When checking right hand side of dequeue with expected type
             (a, Queue n a)
     
     When checking an application of Queue.dequeue:
             Type mismatch between
                     Queue (S n + 0) a (Type of MkQueue (reverse back) [])
             and
                     Queue (1 + n) a (Expected type)
             
             Specifically:
                     Type mismatch between
                             plus (S n) 0
                     and
                             S n

```

同様に書換えてあげればよさそうですね。今回は `+0` を潰したいので同じく `Prelude.Nat` にある以下の関数を使います。

``` idris
plusZeroRightNeutral : (left : Nat) -> left + fromInteger 0 = left
```

はい、これで１丁あがり。

``` idris
export
dequeue : Queue (1+n) a -> (a, Queue n a)
dequeue (MkQueue (x :: xs) back) = (x, MkQueue xs back)
dequeue {n = n} (MkQueue [] back) =
  rewrite plusZeroRightNeutral n in
  dequeue (MkQueue (reverse back) [])

```

…と思いきや以下のエラーが出ます。

``` text
- + Errors (1)
 `-- Queue.idr line 32 col 2:
     When checking right hand side of dequeue with expected type
             (a, Queue n a)
     
     rewriting plus n 0 to n did not change type (a, Queue n a)

```

エラーを見ると「`rewrite` しようとしたけど書き直せる部分がなかったぜ」と言っています。
よくみると `plus (S n) 0` → `S n` の書換えをしたいのに、 右辺 → 左辺を見ると `left` → `left + 0` なので書き換える方向が逆ですね。
`rewrite` の向きを逆にしたくなります。
Idrisにはそういう機能が用意されていて、[組み込み関数](https://www.idris-lang.org/docs/current/prelude_doc/docs/[builtins].html)に `sym` というのがあります。

``` idris
sym : (left = right) -> right = left
```

`rewrite` そのものを変えるのではなくて書換えルールの型の方を変えます。
この `sym` を用いて `dequeue` は以下のように実装できます。

``` idris
export
dequeue : Queue (1+n) a -> (a, Queue n a)
dequeue (MkQueue (x :: xs) back) = (x, MkQueue xs back)
dequeue {n = n} (MkQueue [] back) =
  rewrite sym (plusZeroRightNeutral n) in
  dequeue (MkQueue (reverse back) [])

```



はい、これで関数型キューができました。繰り返しますが、これは値の再利用を考えたら中途半端なので詳細はBanker's Queueなどでググって下さい。
今回は主に依存型と少しばかりの証明のお話でした。
