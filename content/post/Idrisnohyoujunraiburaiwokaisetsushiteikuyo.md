---
categories: [Idris]
date: 2020-12-27T21:15:10+09:00
title: "Idrisの標準ライブラリを解説していくよ"
---

κeenです。俺たちのAdvent Calendarはまだまだ続くぜ！
Idrisの標準ライブラリ、preludeとbaseをサクっと解説していきます。

<!--more-->
# prelude
## `Builtins`

コンパイラから特別扱いされている型などが入っています。

例えば `()` に対応する `Unit` 型や `(A, B)` に対応する `Pair` 型など。

まだ紹介していないものをいくつか紹介しましょう。

`replace : {a:_} -> {x:_} -> {y:_} -> {P : a -> Type} -> x = y -> P x -> P y`
: `x = y`ならば `P x` を `P y` に書き換えられるよね。

`sym : {left:a} -> {right:b} -> left = right -> right = left`
: 等式の左右を入れ替える。 `rewrite sym $ ... in ...` の形で使うことが多い

`Inf : Type -> Type`
: （遅延した）無限の計算を表わす。`Stream` ででてくる。

## `IO`

IOモナド関連の内部実装。直接お世話になることは少ないですかね。

## `Prelude.Algebra`

2つのインタフェースが定義されています。

```idris
interface Semigroup ty where
  (<+>) : ty -> ty -> ty

interface Semigroup ty => Monoid ty where
  neutral : ty
```

それぞれ[半群](https://ja.wikipedia.org/wiki/半群)と[モノイド](https://ja.wikipedia.org/wiki/モノイド)です。

特に `Semigroup` に定義されている二項演算は強力で、リストの結合など、色々な操作を抽象化しています。

```text
Idris> [1, 2, 3] <+> [4, 5, 6]
[1, 2, 3, 4, 5, 6] : List Integer
```

## `Prelude.Functor`

ファンクタが定義されています。

```idris
interface Functor (f : Type -> Type) where
    map : (func : a -> b) -> f a -> f b
```

## `Prelude.Applicative`

既に紹介した `Applicative` が定義されています。

```idris
infixl 3 <*>

interface Functor f => Applicative (f : Type -> Type) where
    pure  : a -> f a
    (<*>) : f (a -> b) -> f a -> f b

```

もう1つ重要なのが、 `Alternative` です。

```idris
infixr 2 <|>
interface Applicative f => Alternative (f : Type -> Type) where
    empty : f a
    (<|>) : f a -> f a -> f a
```

例えば `Maybe` のように失敗するかもしれない計算上で `||` 演算子のような働きをします。

```text
Idris> Just 1 <|> Just 2
Just 1 : Maybe Integer
Idris> Just 1 <|> Nothing
Just 1 : Maybe Integer
Idris> Nothing <|> Just 1
Just 1 : Maybe Integer
Idris> the (Maybe Int) Nothing <|> Nothing
Nothing : Maybe Int
```

他には `guard` や `when` などの処理もあります。

```idris
guard : Alternative f => Bool -> f ()
guard a = if a then pure () else empty

when : Applicative f => Bool -> Lazy (f ()) -> f ()
when True f = Force f
when False f = pure ()
```

これらはモナドと一緒に使うと便利です。

```idris
do
  n <- [1..10]
  guard $ n `mod` 2 == 0
  pure n
-- [2, 4, 6, 8, 10] : List Integer
```

## `Prelude.Monad`

モナドが定義されています。

```idris
infixl 1 >>=

interface Applicative m => Monad (m : Type -> Type) where
    (>>=)  : m a -> ((result : a) -> m b) -> m b

    join : m (m a) -> m a

    -- default implementations
    (>>=) x f = join (f <$> x)
    join x = x >>= id
```

## `Prelude.Foldable`

`foldl` と `foldr` 、他の言語でいうイテラブル相当のインタフェースを提供します。

```idris
interface Foldable (t : Type -> Type) where
  foldr : (func : elem -> acc -> acc) -> (init : acc) -> (input : t elem) -> acc

  foldl : (func : acc -> elem -> acc) -> (init : acc) -> (input : t elem) -> acc
  foldl f z t = foldr (flip (.) . flip f) id t z
```

イテレータにありがちな操作も提供しています。

```idris
concat : (Foldable t, Monoid a) => t a -> a
concat = foldr (<+>) neutral

concatMap : (Foldable t, Monoid m) => (a -> m) -> t a -> m
concatMap f = foldr ((<+>) . f) neutral

and : Foldable t => t (Lazy Bool) -> Bool
and = foldl (&&) True

or : Foldable t => t (Lazy Bool) -> Bool
or = foldl (||) False

any : Foldable t => (a -> Bool) -> t a -> Bool
any p = foldl (\x,y => x || p y) False

all : Foldable t => (a -> Bool) -> t a -> Bool
all p = foldl (\x,y => x && p y)  True
```

`concat` は先程紹介したとおり `List` なんかが `Monoid` を実装しているので以下のように使えます。

```text
Idris> concat [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
[1, 2, 3, 4, 5, 6, 7, 8, 9] : List Intege
```


あとは `Alternative` を使ったAPIもあります。

```idris
choice : (Foldable t, Alternative f) => t (f a) -> f a
choice x = foldr (<|>) empty x

choiceMap : (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
choiceMap f x = foldr (\elt, acc => f elt <|> acc) empty x

```

これは最初の成功した値を取り出します。

```text
Idris> choice [Nothing, Nothing, Just 1, Nothing, Just 2]
Just 1 : Maybe Integer
Idris> the (Maybe Int) $ choice [Nothing, Nothing, Nothing]
Nothing : Maybe Int
```

## `Prelude.Basics`

かなり基本的な操作が入っています。

```idris
Not : Type -> Type
Not a = a -> Void

id : a -> a
id x = x

the : (a : Type) -> (value : a) -> a
the _ = id

const : a -> b -> a
const x = \value => x

fst : (a, b) -> a
fst (x, y) = x

snd : (a, b) -> b
snd (x, y) = y
```

`(.)` は関数合成をします。

``` idris
infixr 9 .

(.) : (b -> c) -> (a -> b) -> a -> c
(.) f g = \x => f (g x)
```

`flip` は2引数関数の引数の順序を入れ替えます。
`(.)` と組み合わせて使うときなんかに便利ですね。

``` idris
flip : (f : a -> b -> c) -> b -> a -> c
flip f x y = f y x
```

ちょっと変わったものとしては証明用途のデータ型があります。

``` idris
||| Decidability. A decidable property either holds or is a contradiction.
data Dec : Type -> Type where
  ||| The case where the property holds
  ||| @ prf the proof
  Yes : (prf : prop) -> Dec prop

  ||| The case where the property holding would be a contradiction
  ||| @ contra a demonstration that prop would be a contradiction
  No  : (contra : prop -> Void) -> Dec prop
```

気持としては `Bool` に近いんですが、「なぜTrueなのか」、「なぜFalse」なのかの証明つきです。

`the (Dec 1 = 1) (Yes Refl)` や `the (Dec (1 = 0)) (No SIsNotZ)` など。

## `Prelude.Bits`

`Bits8` などのビット数指定の型の軽い処理が入っています。


## `Prelude.Cast`

`Cast` インタフェースが定義されています。

``` idris
interface Cast from to where
    cast : (orig : from) -> to
```

以外な型同士にも `Cast` が定義されているのは紹介したとおりです。

``` idris
Cast Double String where
   -- ...
```


## データ型
`Prelude.Bool`, `Prelude.Chars` `Prelude.Either`, `Prelude.Double`, `Prelude.List`, `Prelude.Maybe`, `Prelude.Nat`, `Prelude.Stream`, `Prelude.String`

それぞれのデータ型や関連するコードが定義されています。
このうち `Stream` だけ触れてないので紹介します。

### `Stream`

リストと似ていますが、終わりのないデータ型です。

``` idris
data Stream : Type -> Type where
  (::) : (value : elem) -> Inf (Stream elem) -> Stream elem
```

例えば無限に1が続くデータ型なんかを作れます。

``` idris
Idris> :let ones = the (Stream Int) $ repeat 1
Idris> take 10 ones
[1, 1, 1, 1, 1, 1, 1, 1, 1, 1] : List Int
```

素数一覧みたいに無限に続くデータ型をひとまず作っておいて、必要になったら `take` や `index` などで取り出すのが主な使い方ですかね。

それこそFizzBuzzなんかもそうですね。

## `Prelude.File`

ファイル操作が入っています。

## `Prelude.Interactive`

`putStrLn` などが入っています。

## `Prelude.Interfaces`

`Eq` などの基本的なインタフェースが入っています。

## `Prelude.Pairs`

タプル系の操作かと思いきや依存ペア系の補助データ構造が入っています。

## `Prelude.Providers`

まだ紹介していないIdris面白機能の1つ、Type Providerで使うデータ型が入っています。

## `Prelude.Show`

`Show` インタフェースが定義されています。

``` idris
interface Show ty where
  partial
  show : (x : ty) -> String
  show x = showPrec Open x -- Eta expand to help totality checker

  partial
  showPrec : (d : Prec) -> (x : ty) -> String
  showPrec _ x = show x
```


## `Prelude.Traversable`

`Foldable` と `Applicative` を組み合わせるときに使います。 `Foldable` の要素の1つ1つに操作を加えます。

``` idris
interface (Functor t, Foldable t) => Traversable (t : Type -> Type) where
  traverse : Applicative f => (a -> f b) -> t a -> f (t b)
```


まあ、一番分かりやすいのは `for_` 関数ですかね。

``` idris
for_ : (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ = flip traverse_
```

こう使います。

``` idris
Idris> :exec for_ [1..10] $ \n => printLn n
1
2
3
4
5
6
7
8
9
10
```

## `Prelude.Uninhabited`

証明で使うインタフェースが入っています。

``` idris
interface Uninhabited t where
  total uninhabited : t -> Void
```

## `Prelude.WellFounded`

[整礎帰納法](https://ja.wikipedia.org/wiki/数学的帰納法#整礎帰納法)をします。

## `Decidable.Equality`

等値関係に絞った `Dec` の補助パッケージですかね。

``` idris
interface DecEq t where
  total decEq : (x1 : t) -> (x2 : t) -> Dec (x1 = x2)
```


## `Language`

リフレクション系のAPIが揃っています。

# base

baseの方が複雑なんですが便利なライブラリがあるんですよね。頑張って紹介します。

## `Data.Morphisms`

モーフィズム（射）が定義されています。

``` idris
record Morphism a b where
  constructor Mor
  applyMor : a -> b
infixr 1 ~>
(~>) : Type -> Type -> Type
(~>) = Morphism

record Endomorphism a where
  constructor Endo
  applyEndo : a -> a

record Kleislimorphism (f : Type -> Type) a b where
  constructor Kleisli
  applyKleisli : a -> f b
```

射、自己準同型、クライスリ射です。
まあ、これは射に色々便利なインタフェースを定義するためのデータ型ですね。


## `Control.Category`

圏の定義があります。

``` idris
interface Category (cat : k -> k -> Type) where
  id  : cat a a
  (.) : cat b c -> cat a b -> cat a c
```


射は圏になりますね。

``` idris
implementation Category Morphism where
  id                = Mor id
  (Mor f) . (Mor g) = with Basics (Mor (f . g))
```

これは $\mathbf{Set}$ 圏であってるかな？

射の合成の別記法も用意されています。

``` idris
infixr 1 >>>
(>>>) : Category cat => cat a b -> cat b c -> cat a c
f >>> g = g . f
```

## `Control.Arrow`

arrow系の定義がいっぱいあります。
arrowは矢印のことで、要するに射の合成とかその辺を担当します。

``` idris
infixr 5 <++>
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 \|/

interface Category arr => Arrow (arr : Type -> Type -> Type) where
  arrow  : (a -> b) -> arr a b
  first  : arr a b -> arr (a, c) (b, c)

  second : arr a b -> arr (c, a) (c, b)
  second f = arrow swap >>> first f >>> arrow swap

  (***)  : arr a b -> arr a' b' -> arr (a, a') (b, b')
  f *** g = first f >>> second g

  (&&&)  : arr a b -> arr a b' -> arr a (b, b')
  f &&& g = arrow dup >>> f *** g where
    dup : x -> (x,x)
    dup x = (x,x)

interface Arrow arr => ArrowZero (arr : Type -> Type -> Type) where
  zeroArrow : arr a b

interface ArrowZero arr => ArrowPlus (arr : Type -> Type -> Type) where
  (<++>) : arr a b -> arr a b -> arr a b

interface Arrow arr => ArrowChoice (arr : Type -> Type -> Type) where
  left  : arr a b -> arr (Either a c) (Either b c)

  right : arr a b -> arr (Either c a) (Either c b)
  right f = arrow mirror >>> left f >>> arrow mirror

  (+++) : arr a b -> arr c d -> arr (Either a c) (Either b d)
  f +++ g = left f >>> right g

  (\|/) : arr a b -> arr c b -> arr (Either a c) b
  f \|/ g = f +++ g >>> arrow fromEither

interface Arrow arr => ArrowApply (arr : Type -> Type -> Type) where
  app : arr (arr a b, a) b

interface Arrow arr => ArrowLoop (arr : Type -> Type -> Type) where
  loop : arr (Pair a c) (Pair b c) -> arr a b
```

記号だらけで、個人的にこれを本当に全部覚えてつかいこなしている人がいるのかは疑問に思ってます。

## `Control.Isomorphism`

同型を表わすデータ型です。
ちゃんと証明オブジェクトもついていてえらいですね。

``` idris
record Iso a b where
  constructor MkIso
  to : a -> b
  from : b -> a
  toFrom : (y : b) -> to (from y) = y
  fromTo : (x : a) -> from (to x) = x
```

`Category` などいくつかのインタフェースも実装されています。
基本は証明用かな？

## `Control.Catchable`

失敗するかもしれない計算の失敗を処理する機能を提供します。

``` idris
interface Catchable (m : Type -> Type) t | m where
    throw : t -> m a
    catch : m a -> (t -> m a) -> m a

```

`catch` を 中置記法で使うとそれっぽいですかね。

``` text
*Control/Catchable> Right "Correct" `catch` \_ => Left "Failed"
Right "Correct" : Either String String
*Control/Catchable> the (Either String String) $ Left "Incorrect" `catch` \_ => Left "Failed"
Left "Failed" : Either String String
```

## `Control.IOExcept`

`IO (Either err a)` のパターンが頻出なのでそれをまとめた型です。


``` idris
record IOExcept' (f:FFI) err a where
     constructor IOM
     runIOExcept : IO' f (Either err a)
```

## `Data.IORef`

変更可能な変数のデータ型を定義しています。

``` idris
||| A mutable variable in the IO monad.
export
data IORef a = MkIORef a
```

APIは以下です。

``` idris
newIORef : a -> IO (IORef a)
readIORef : IORef a -> IO a
writeIORef : IORef a -> a -> IO ()
modifyIORef : IORef a -> (a -> a) -> IO ()
```

以下のようにして使います。

``` idris
do
  result <- newIORef 0
  for_ [0..10] $ \n =>
    modifyIORef result (+n)
  readIORef result
-- 55
```

## `Data.Buffer`

バッファが定義されています。
主にファイルとの一括IOで使うのが用途のよう。

IdrisでバイナリIOをしたくなると使うことになると思います。

## `Data.Complex`

複素数を定義しています。

``` idris
infix 6 :+
data Complex a = (:+) a a
```

## `Data.Vect`

長さの決まっているリストを定義しています。

``` idris
data Vect : (len : Nat) -> (elem : Type) -> Type where
  Nil  : Vect Z elem
  (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem
```

## `Data.Fin`

有限の自然数を定義しています。

``` idris
data Fin : (n : Nat) -> Type where
    FZ : Fin (S k)
    FS : Fin k -> Fin (S k)
```

例えば `Fin 3` なら3未満の整数しかとれません。

``` text
Idris> :module Data.Fin
*Data/Fin> the (Fin 3) 0
FZ : Fin 3
*Data/Fin> the (Fin 3) 1
FS FZ : Fin 3
*Data/Fin> the (Fin 3) 2
FS (FS FZ) : Fin 3
*Data/Fin> the (Fin 3) 3
(input):1:13:When checking argument prf to function Data.Fin.fromInteger:
        When using 3 as a literal for a Fin 3
                3 is not strictly less than 3
```

`Vect` に対するインデックスのように `len` 未満の値を指定したいときに便利ですね。

``` idris
index : Fin len -> Vect len elem -> elem
index FZ     (x::xs) = x
index (FS k) (x::xs) = index k xs
```

## `Data.HVect`

ヘテロジーニアスなベクタ型を定義しています。

``` idris
data HVect : Vect k Type -> Type where
  Nil : HVect []
  (::) : t -> HVect ts -> HVect (t::ts)
```

以下のように異なる型の値を格納できます。

``` text
*Data/HVect> the (HVect [Integer, Bool, String]) [1, False, "hetero"]
[1, False, "hetero"] : HVect [Integer, Bool, String]
```

## `Data.Bits`

`Prelude.Bits` に加えてもう少し操作を定義しています。

## `Data.List`

`Preldhude.List` に加えてもう少し操作を定義しています。
`intersect` などもありますが、主に証明用ですね。

## `Data.String`

`Preldhude.String` に加えてもう少し操作を定義しています。

## `Data.List.Quantifiers`
`List` の証明に使えそうな量化子が定義されています。

## `Data.List.Views`

`List` に対するビューが定義されています。

## `Data.Vect.Quantifiers`
`Vect` の証明に使えそうな量化子が定義されています。

## `Data.Vect.Views`

`Vect` に対するビューが定義されています。


## `Data.Nat.Views`

`Nat` に対するビューが定義されています。

## `Data.Primitives.Views`

`Interger` などのプリミティブに対するビューが定義されています。


## `Data.Erased`


実行時に消去された値を表現できるデータ型を定義しています。

``` idris
data Erased : Type -> Type where
    Erase : .(x : a) -> Erased a
```

Idrisでは引数の前に `.` があるとそれは消去されているようです。

## `Data.Mod2`

2^n で割った値を保持する `Mod2 n` が定義されています。

``` idris
public export
data Mod2 : Nat -> Type where
    MkMod2 : {n : Nat} -> Bits n -> Mod2 n
```


## `Data.So`

型レベル `Bool` 相当の機能を提供します。

``` idris
data So : Bool -> Type where
  Oh : So True
```

公式ライブラリだと `usleep` なんかで使われています。

``` idris
usleep : (i : Int) -> { auto prf : So (i >= 0) } -> IO ()
```

`=` なら型にあるんですが、 `>=` はありません。
こういう計算で結果を求めるタイプの証明に便利ですね。

## `Debug.*`
`Debug.Error` と `Debug.Trace`はそれぞれデバッグ用に使います。
`IO` 文脈でなくてもIO処理ができてしまう魔法の関数です。


``` idris
import Debug.Trace

add : Integer -> Integer -> Integer
add x y = trace "debbuging" $ x + y

main : IO ()
main = printLn $ 3 * (add 1 2)
```


``` idris
$ idris -o add add.idr
$ ./add
debbuging
9
```

中身は `unsafePerformIO` というヤバい機能で実装されているのでデバッグ用途以外では使わないようにしましょう。

## `Language.Reflection.Utils`

証明とかに便利そうな関数がちょこっと実装されています。
他はElabに色々なインタフェースを実装する役割。

## `System`

`getEnv` や  `exit` などのシステム関連の機能です。

## `System.Info`

`backend` などの処理系情報です。

## `System.Concurrency.*`

`System.Concurrency.Raw` と `System.Concurrency.Channels` プロセスとチャネルを使ったコミュニケーションを提供します。 `Raw` は低レベルで型安全でないAPIなので `Channels` の方を使いましょう。

`"PING"` を送ったら `"PONG"` を返してくれるプロセスを立ち上げてみます。
`spawn` に `IO ()` の値を渡すとプロセスをスタートしてくれます。その他チャネルのAPIはドキュメントとか見て下さい。


``` idris
import System.Concurrency.Channels

pong : IO ()
pong = do
  Just channel <- listen 10
  req <- unsafeRecv String channel
  if req == Just "PING"
  then ignore $ unsafeSend channel "PONG"
  else pure ()

ping : PID -> IO ()
ping pid = do
  Just channel <- connect pid
  unsafeSend channel "PING"
  Just resp <- unsafeRecv String channel
  putStrLn resp

main : IO ()
main = do
  -- spawnを呼ぶ
  Just pongPid <- spawn pong
  putStrLn "pong spawned"
  ping pongPid
  pure ()
```

`unsafeRecv` の引数に型をとるので1つのチャネルで任意の型の値をやりとりできるのがポイントですね。


プロセス内部はCバックエンドではpthreadが立ち上がっているようです。
ただしプロセスという名前だけあってメモリは共有していないようです。
以下のようにクロージャに持たせた `IORef` を通じてメモリを共有するコードを書いてみました。

``` idris
import System.Concurrency.Channels
import Data.IORef


countDown : Nat -> IORef Int -> IO ()
countDown Z     _ = pure ()
countDown (S n) v = do
  writeIORef v (cast n)
  countDown n v

polling : Nat -> IORef Int -> IO ()
polling Z     _ = pure ()
polling (S n) v = do
  i <- readIORef v
  printLn i
  polling n v


main : IO ()
main = do
  v <- newIORef 100
  spawn $ countDown 100 v
  polling 20 v
```


これは実行時に処理系が落ちました。

``` text
channel_ioref: idris_rts.c:912: doCopyTo: Assertion `0' failed.
zsh: abort (core dumped)  ./channel_ioref
```

同じくクロージャではなくチャネルを介しても同様でした。

``` idris
import System.Concurrency.Channels
import Data.IORef


countDown : Nat -> IO ()
countDown n = do
  Just channel <- listen 100
  Just v <- unsafeRecv (IORef Int) channel
  loop n v
where
  loop : Nat -> IORef Int -> IO ()
  loop Z     _ = pure ()
  loop (S n) v = do
    writeIORef v (cast n)
    loop n v

polling : Nat -> PID -> IO ()
polling n pid = do
  v <- newIORef (the Int 100)
  Just channel <- connect pid
  unsafeSend channel v
  loop n v
where
  loop : Nat -> IORef Int -> IO ()
  loop Z     _ = pure ()
  loop (S n) v = do
    i <- readIORef v
    printLn i
    loop n v


main : IO ()
main = do
  Just pid <- spawn $ countDown 100
  polling 20 pid

```


コンパイル時に防げる仕組みがあったらよかったんですが残念ですね。

# まとめ

Idrisの標準ライブラリをさっくり解説しました。

