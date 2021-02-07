---
categories: [Idris]
date: 2020-12-29T12:44:30+09:00
title: "Idrisモナドアソート"
---

この記事は去年末に書いていた記事です。
執筆中にパソコンがクラッシュして修理に1ヶ月くらいかかったので公開が遅くなりました。
それを踏まえて記事をご覧下さい。

κeenです。 そろそろAdvent Calendarも終盤になってきましたね。
今回は前回の標準ライブラリ紹介であえて外したモナドについて紹介します。

<!--more-->

`Monad`（ や `Applicative` ）には便利な記法が用意されているのである型がモナドを実装しているかどうかは大きな関心ごとになります。
また、「モナド」として提供されている型もあるのでそれらについて紹介していきます。
例によってpreludeとbaseの中から紹介していきます。


## `Prelude.List`

おなじみですね。

以下のコードは九九の計算を全部やります。

```idris
do
  x <- [1..9]
  y <- [1..9]
  pure $ x * y
```


熟語括弧（idiom brackets）を思い出してもらうと、以下のコードでも同等です。

```idris
[| [1..9] * [1..9] |]
```


## `Prelude.Maybe`

`Just` 側の値を取り出します。1つでも `Nothing` があると即座に `Nothing` になります。

```idris
do { x <- Just 1 ; y <- Just 2 ; pure $ x + y }
-- Just 3 : Maybe Integer

do { x <- Nothing ; y <- Just 2 ; pure $ x + y }
-- Nothing : Maybe Integer

do { x <- Just 1 ; y <- Nothing ; pure $ x + y }
-- Nothing : Maybe Integer
```

## `Prelude.Either`

`Either` は本来なら左と右が対等な関係にあるのですが、Haskellからの伝統で左がエラーの値、右が正しい値として使われています。
右側を優先するという規約の下 `Either` はモナドを実装していて、右側の値を取り出せます。

```idris
do
  x <- Right 1
  y <- Right 2
  pure $ x + y
-- Right 3 : Either String Int
```


複数の `Left` があった場合は最初の `Left` のエラーが優先されます。

```idris
do
  x <- Left "First error"
  y <- Left "Second error"
  pure $ x + y
-- Left "First error" : Either String Int
```


## `Prelude.Stream`

`Stream` はちょっとトリッキーです。 `Stream` の前に `List` の動作を確認しておきましょう。

`List` の場合は二重 `for` 文のように最初のリストの要素に対して次のリストの要素のリストを繰り返していきます。

```text
Idris> do { x <- [0..3]; y <- [0..3]; pure (x, y) }
[(0, 0),
 (0, 1),
 (0, 2),
 (0, 3),
 (1, 0),
 (1, 1),
 (1, 2),
 (1, 3),
 (2, 0),
 (2, 1),
 (2, 2),
 (2, 3),
 (3, 0),
 (3, 1),
 (3, 2),
 (3, 3)] : List (Integer, Integer)
```

`Stream` ではこれはできません。
何故なら `Stream` は無限に続くことがあるので2番目の要素の繰り返しが終わらず、最初のリストの要素が1つしか取り出されないからです。

そこで `Stream` は全ての要素を同時に取り出します。
実例をみてみましょう。
`cycle` で `[0, 1, 2, 3, 0, 1, 2, ...]` と無限に続くストリームを2つ作り、
そこから `do` 記法で取り出したものをペアにして最初の10個取り出してみます。

```idris
Idris> take 10 $ do { x <- cycle [0..3]; y <- cycle [0..3]; pure (x, y) }
[(0, 0),
 (1, 1),
 (2, 2),
 (3, 3),
 (0, 0),
 (1, 1),
 (2, 2),
 (3, 3),
 (0, 0),
 (1, 1)] : List (Integer, Integer)
```

すると `List` とは異なりそれぞれの要素が順番に取り出されています。
下の表でいう対角線上の要素が取り出されているのです。


|   | 0      | 1      | 2      | 3      |
|---|--------|--------|--------|--------|
| 0 | (0, 0) | (0, 1) | (0, 2) | (0, 3) |
| 1 | (1, 0) | (1, 1) | (1, 2) | (1, 3) |
| 2 | (2, 0) | (2, 1) | (2, 2) | (2, 3) |
| 3 | (3, 0) | (3, 1) | (3, 2) | (3, 3) |

# `Data.Vect`

`Stream` と同じく対角成分をとります。

``` idris
do
  x <- [1, 2, 3]
  y <- [1, 2, 3]
  pure (x, y)
-- [(1, 1), (2, 2), (3, 3)] : Vect 3 (Integer, Integer)
```

注意しないといけないのは `Vect k` がモナドになっている点です。
ベクタの長さ `k` まで含めているので、モナドとして扱っている途中で長さを変えられません。
つまり全て同じ長さのベクタでないと `do` 文で使えないのです。

# `Data.Morphism`, `Control.Arrow`

略。「Haskell Arrow」とかのワードで調べるとると色々出てきます。
ざっくりいうと関数を組み合わせるときに一時変数を使わずにパイプを繋ぎ合わせるようなやり方で合成するフレームワークです。


# `Control.Monad.Reader`

ようやくモナドらしいモナドがでてきました。
`Reader` は文脈から値が取り出せるモナドです。
以下の3つのAPIがキーになります。

```idris
Reader : Type -> Type -> Type
ask   : Reader r r
asks  : (r -> a) -> Reader r a
local : (r -> r) -> Reader r a -> Reader r a
```


`Reader r a` と書いて 「文脈から `r` の値を取り出せる `a` の計算」です。
普通に計算しつつ、 `r` の値がほしくなったら `v <- ask` と書くと文脈から値を取り出せます。
「文脈」ってなんだよって話ですが、まあ、感覚で理解して下さい。

四の五のいわずに実例を出しましょう。
`Reader` は任意の場所で値を取り出せるということで、Dependency Injectionや設定を渡すのに使われます。
ここでは設定を渡す例を挙げましょう。

まず設定を定義しておきます。

```idris
import Control.Monad.Reader

record Config where
  constructor MkConfig
  width : Nat
```

設定に基いて指定された横幅にそろえる関数 `format: String -> Reader Config String` を定義します。
`Reader Config String` になっているので `String` を返しつつ `Config` を読み出せます。

```idris
format: String -> Reader Config String
format s = do
  config <- ask
  let pad = pack $ replicate (minus (width config) (length s)) ' '
  pure $ pad ++ s
```

`config <- ask` の部分ですね。`Reader` のAPIの `ask` の型が `Reader r r` なので `<-` すると `Reader` のもっている `r` の値が読み出せるのが分かるかと思います。

この関数を使ってみたいのですが、その前に便利関数を定義しておきましょう。
後程説明しますが `Reader` はちょっと入り組んだ構造になってるのでAPIも複雑になってます。
それを緩和するコードです。

```idris
runReader : Reader r t -> r -> t
runReader m r = runIdentity $ runReaderT m r
```

`Reader` モナドを走らせる関数です。モナドと `Reader` で読み出す値を渡して、モナドを走らせます。
ライブラリ側で提供してしかるべき関数なんですがなぜかないので自分で定義します。

それでは `format` を走らせてみましょう。

```text
Idris> runReader (format "Hello") (MkConfig 10)
"     Hello" : String
```

`runReader` での与えた `(MkConfig 10)` を読み出して横幅10にフォーマットしていますね。

`ask` で読み出す値は最初に `runReader` で与えたもので固定されます。
これが文脈といってるやつですね。一時的にこの値を上書きするのが `local` です。

# `Control.Monad.Writer`

`Reader` の逆で文脈に値を書き出せます。

```idris
Writer : Type -> Type -> Type
tell : w -> Writer w ()
listen  : Writer w a -> Writer w (a, w)
listens : (w -> b) -> Writer w a -> Writer w (a, b)
pass   : Writer w (a, w -> w) -> Writer w a
censor : (w -> w) -> Writer w a -> Writer w a
```

ただし `Writer` の `w` には `Monoid` の制約がついています。

```idris
Monoid w => Monad (Writer w)
```

例えば `List String` から `List Integer` へ変換する関数を書くとしましょう。
中には変換に失敗する値もあります。
そういうときに `Writer` を使えば失敗した値を記録しつつ成功したものだけ集めることができます。

```idris
import Control.Monad.Writer
import Data.String

tryConvert : List String -> Writer (List String) (List Integer)
tryConvert []      = pure []
tryConvert (x::xs) =
  case parseInteger x of
    Just i => map (i::) (tryConvert xs)
    Nothing => do
      -- 失敗した値を書き出す
      tell [x]
      tryConvert xs
```

`tell` を使っている部分が `Writer` モナドに値を書き込んでいるところです。

実行してみましょう。 `Writer` には `runWriter` があります。

```text
Idris> runWriter $ tryConvert ["1", "2", "3"]
([1, 2, 3], []) : (List Integer, List String)
Idris> runWriter $ tryConvert ["1", "two", "3"]
([1, 3], ["two"]) : (List Integer, List String)
```

タプルで成功した値と失敗した値が返ってきていますね。

# `Control.Monad.State`

`State` は状態を持ち回るモナドです。 `get` で読み出し、 `put` で書き出しができます。

```idris
State : (stateType : Type) -> (ty : Type) -> Type
get  : State stateType stateType
gets : (stateType -> a) -> State stateType a
put : stateType -> State stateType ()
modify : (stateType -> stateType) -> State stateType ()
```

同じ `State s` を使い回してる限りにおいてグローバル変数のように使えます。

これも実例を出しましょう。
疑似乱数のコードです。
疑似乱数は乱数の状態を更新してまわる必要がありますね？

```idris
import Data.Bits
data Rand = RandomState Bits64

seed : Rand
seed = RandomState 88172645463325252

infixl 7 ^, <<, >>
nextRand: Rand -> (Bits32, Rand)
nextRand (RandomState x) =
  let x = x ^ (x >>  13) in
  let x = x ^ (x << 7) in
  let x = x ^ (x >>  17) in
  let ret = (x << 32) >> 32 in
  (prim__truncB64_B32 ret, RandomState x)
where
  (^): Bits64 -> Bits64 -> Bits64
  (^) = xor' {n = 8}
  (<<): Bits64 -> Bits64 -> Bits64
  (<<) = shiftLeft' {n = 64}
  (>>): Bits64 -> Bits64 -> Bits64
  (>>) = shiftRightLogical' {n = 8}
```


`nextRand: Rand -> (Bits64, Rand)` の型がそれです。
`Rand` を受け取って `Rand` を返していますね。
`Rand` を受け取ったり次の関数に回したりは面倒です。
これは `State Rand` を使うと解決します。

```idris
getRandom: State Rand Bits32
getRandom = do
  rand <- get
  let (i, rand) = nextRand rand
  put rand
  pure i
```

このAPIにしてしまえば乱数の状態を気にすることなく乱数を使う関数が書けます。

```idris
getRandomPair : State Rand (Bits32, Bits32)
getRandomPair = do
  x <- getRandom
  y <- getRandom
  pure (x, y)
```

`State` には実行する関数が3つあります。実行の結果と持ち回った状態をそれぞれ返すか返さないかでの変種です。

```idris
runState  : State stateType a -> stateType -> (a, stateType)
evalState : State stateType a -> stateType -> a
execState : State stateType a -> stateType -> stateType
```

今回は乱数の状態は捨てることにして `evalState` を使ってみましょう。

``` idris
evalState getRandomPair seed
-- (D212EB9E, C7E45E25)
```

となるはずです。

ところがREPLで試すと最後まで計算してくれず、巨大な式が表示されます。
これはIdrisのREPLがコンパイラではなくHaskellのインタプリタで実装されていることによる限界です。
仕方ないのでコンパイラにコンパイルしてもらって実行しましょう。

``` idris
Idris> :exec printLn $ evalState getRandomPair seed
```

これならちゃんと `(D212EB9E, C7E45E25)` と表示されます。


# `Control.Monad.Trans`

複数のモナドを組み合わせたいことがあります。
例えばIOでファイルを処理しつつその結果をWriterに記録するなど。
そういうときに愚直に書くととても大変なことになります。

実際のコードを見てみましょう。
標準入力から1行読んで `Writer` に記録するコードです。

``` idris
readLine : IO (Writer (List String) Integer)
readLine = do
  line <- getLine
  pure $ do tell [line]
            pure 1
```

やりたいことは単純なのに `do` がネストしていてやっかいですね。
両方ともモナドなので1つの `do` で書けると嬉しいですよね。
そこで2つのモナドの合成っぽいことをしたくなります。

モナドの合成のようなことをするのがモナドトランスフォーマ（モナド変換子）です。
あるモナドAを、別のモナドBも混ぜた新しいモナドABに変換します。

先程のコードはモナドトランスフォーマを使うと楽に書けるようになります。

``` idris
readLine' : WriterT (List String) IO Integer
readLine' = do
  line <- lift getLine
  tell [line]
  pure 1

```

* `WriterT` モナドトランスフォーマで `IO` モナドを変換する
  + `WriterT (List String) IO` が新しいモナドになる
* `IO` モナドのアクション `getLine` は `lift` することで `do` 内で使えるようになる

モナドトランスフォーマはTで終わる名前をつけるのが慣習です。
いくつかモナドトランスフォーマに関連する話題を並べておきます。

* モナドトランスフォーマはモナドに適用され、結果モナドになるのでモナドトランスフォーマを重ね掛けすることができる
  + モナドトランスフォーマスタックとも呼ばれる
* 全てのモナドがモナドトランスフォーマとしても実装できる訳ではない
  + できるなら勝手にやってしまえばいいのでトランスフォーマという概念を持ち出すまでもない
* モナドトランスフォーマスタックの底には（もし使うなら） `IO` モナドを置くのがベストプラクティス

モナドトランスフォーマそのものについてはかなり深いトピックなのでここでは紹介するだけに留めて詳しい解説はWeb上にある記事に譲ることにします（私が詳しくないとも言う）。


# `Control.Monad.Identity`

モナドトランスフォーマに関連する重要なモナドが `Control.Monad.Identity` で定義されている `Identity` モナドです。
`Identity a` は何も意味のあることをしません。
`a` を直接使うのとほとんど変わりません。

`Identity` は何もしないのでモナドトランスフォーマと組み合わせて使うとモナドトランスフォーマの方の機能だけを使うことができます。
すなわち `WriterT w Identity a` と `Writer w a` が同じ機能になるのです。

`Identity` があれば任意のモナドトランスフォーマをモナドにできるので、モナドでもモナドトランスフォーマでも実装できるものはモナドトランスフォーマで実装してしまえば、それを `Identity` に適用するとモナドも得られるのです。

実際、 `Reader` や `Writer` はまずモナドトランスフォーマとして定義されていて、それを `Identity` に適用したものになっています。

``` idris
-- Reader
Reader : Type -> Type -> Type
Reader r a = ReaderT r Identity a

-- Writer
Writer : Type -> Type -> Type
Writer w a = WriterT w Identity a

-- State
State : (stateType : Type) -> (ty : Type) -> Type
State = \s, a => StateT s Identity a
```


# まとめ


Idrisの標準ライブラリにあるモナドを紹介しました。
これらは基本的な道具なのでみなさん道具箱の中に入れておいて下さい。
