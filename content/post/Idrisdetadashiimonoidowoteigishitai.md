---
categories: [Idris]
date: 2020-12-26T11:19:00+09:00
title: "Idrisで正しいモノイドを定義したい"
---

κeenです。俺たちのAdevnt Calendarはまだまだ終わらないぜ！
Idrisの標準ライブラリを含め、世間でモノイド（などの代数構造）とされる型クラスって正しくないよねという話と、正しい定義をする話をします。

<!--more-->

# モノイドの定義

集合 $S$ とその上の二項演算 $\cdot : S \times S \to S$ が以下の2つの条件を満たすときモノイドという

* 結合律： $S$ の任意の元 $a$ 、 $b$ 、 $c$ に対して $(a \cdot b) \cdot c = a \cdot (b \cdot c)$ が成り立つ
* 単位元: $S$ の元 $e$ が存在して、 $S$ の任意の元 $a$ に対して $e \cdot a = a \cdot e = a$ が成り立つ


ポイントはモノイドは集合と演算の組に対して定義されている点と、モノイドであるためにはある程度の性質を満たさないといけない点です。

例えば自然数と `+` の組はモノイドですし、 自然数 と `*` の組もモノイドです。


# よくあるモノイドの定義

Idrisのプレリュードにも定義はありますが、半群を継承していて話がややこしくなるので1まとめにした定義を出します。

```idris
interface Monoid ty where
  neutral : ty
  (<+>) : ty -> ty -> ty
```

これを見て「うん、モノイドだね」と思った方はかなり毒されてます。
モノイドの定義に全然従ってないですよね。

# よくあるモノイドの定義の問題点
## 定義の対象が違う

本来のモノイドは集合と演算の組に対して定義されるのに対して、こちらは型（=集合）に対してのみ定義しています。

これは結構な問題で、例えば自然数には `+` や `*` 、あるいは `max` などの演算で複数のモノイド構造があるのにそれを表現できません（Idrisではインタフェースの実装は原則1型につき1つ）。

標準ライブラリではどうしているかというと、自然数をラップする新しい型を定義して、それぞれに別の演算を定義しています。


```idris
record Multiplicative where
  constructor GetMultiplicative
  _ : Nat

record Additive where
  constructor GetAdditive
  _ : Nat

Monoid Multiplicative where
  (<+>) left right = GetMultiplicative $ left' * right'
    where
      left'  : Nat
      left'  =
       case left of
          GetMultiplicative m => m

      right' : Nat
      right' =
        case right of
          GetMultiplicative m => m
  neutral = GetMultiplicative $ S Z

Monoid Additive where
  left <+> right = GetAdditive $ left' + right'
    where
      left'  : Nat
      left'  =
        case left of
          GetAdditive m => m

      right' : Nat
      right' =
        case right of
          GetAdditive m => m
  neutral = GetAdditive Z
```


めちゃくちゃ使いづらそうですよね。

一応Idrisにはインタフェースの[名前つき実装](http://docs.idris-lang.org/en/latest/tutorial/interfaces.html#named-implementations)というのがあって、別々のインスタンスを作ることもできます。

```idris
[MultNatMonoid] Monoid Nat where
  neutral = S Z
  (<+>) = mult
[AddNatMonoid] Monoid Nat where
  neutral = Z
  (<+>) = plus
```

ですがこれはこれで問題です。
何故なら同じ演算子に対して複数のインスタンスを作れてしまうからです。

```idris
[AddNatMonoid] Monoid Nat where
  neutral = Z
  (<+>) = plus

[AddNatMonoid2] Monoid Nat where
  neutral = Z
  (<+>) = plus
```

さらに、これを使うときに面倒です。
`concat @{AddNatMonoid}` のように名前を指定しないといけなくなります。
できれば型から良い感じに推測してほしいですよね。

## 条件を表現できていない

まあ、これは分かりやすいでしょう。
結合律と単位元の満たすべき条件が表現できていません。

これら2つの問題を解決したモノイドを定義したい、というのが今回の趣旨です。

# パラメータに演算を

真っ先に思い付くのはインタフェースのパラメータで演算まで受け取ってしまうものですよね。

```idris
interface CorrectMonoid s (op : s -> s -> s) where
```


これで定義の対象が違う問題が解決します。

そして続く中身は以下のようになります。

```idris
interface CorrectMonoid s (op : s -> s -> s) where
  unit : s
  unitNeutral : (x : s) -> (op unit x = x,  op x unit = x)
  monoidAssociative : (x, y, z : s) ->  op x (op y z) = op (op x y) z
```

1. （よくある定義と同じように）単位元を定義に持つ
2. （よくある定義と異なり）演算は定義にない
3. 2つの条件を定義に持つ。要は証明オブジェクトを要求する。

これはコンパイルが通ります。

よさそうなので例えば `(Nat, plus)` の組に定義してみましょう。
小文字はじまりのシンボルは勝手に型パラメータ扱いされるので `Prelude.Nat.plus` と完全修飾名を使っています。

```idris
CorrectMonoid Nat Prelude.Nat.plus where
  unit = Z
  unitNeutral n = (plusZeroLeftNeutral n, plusZeroRightNeutral n)
  monoidAssociative = plusAssociative
```

残念ながらこれはコンパイルエラーです。

```text
- + Errors (1)
 `-- correctMonoid.idr line 8 col 0:
     plus  cannot be a parameter of Main.CorrectMonoid
     (Implementation arguments must be type or data constructors)
```

いくらIdrisといっても関数を型パラメータに書くことはできません。
型パラメータに書けるのは型や型コンストラクタ、データ型から作った値などです。
多分関数の同一性の問題とかそのあたりの制約なんでしょう。

という訳で試み1は失敗しました。

# 演算に対応する型

関数を直接置けないなら関数と対応関係にあるダミーの型を作ってそれを置けばいいじゃんという発想に至ります。

```idris
data Add

interface Op s o where
  (<>) : s -> s -> s

Op Nat Add where
  (<>) = plus

interface Op s o => CorrectMonoid s o where
  unit : s
  unitNeutral : (x : s) -> (unit <> x = x, x <> unit = x)
  monoidAssociative : (x, y, z : s) ->  x <> (y <> z) = (x <> y) <> z
```


残念ながらこれもコンパイルが通りません。

```text
- + Errors (1)
 `-- correctMonoid.idr line 22 col 10:
     When checking type of Main.unitNeutral:
     Can't find implementation for Op s o
```

どうやら `interface Op s o` の `o` がどこでも使われていないことで解決に失敗するようです。


インタフェースを使った方法は難しいようですね。別の方法を探しましょう。

# 自作インタフェース

次に思い付くのはインタフェースの仕組みを1から作ればどうにかできるんじゃないかということですよね。

インタフェースは実装レベルではただのレコードです。

```idris
record CorrectMonoid a o where
  constructor MkCorrectMonoid
  _unit : a
  _op : a -> a -> a
  _unitNeutral : (x : a) -> (_op _unit x = x, _op x _unit = x)
  _opAssociative : (x, y, z : a) -> _op x (_op y z) = _op (_op x  y) z
```

ここまではよいでしょう。

では `CorrectMonoid a o => ...` という制約はどう書くんだとなります。
これは事実上自動で渡されるimplicit argumentsですよね。
Idrisには[auto implicit arguments](http://docs.idris-lang.org/en/latest/tutorial/miscellany.html#auto-implicit-arguments)があります。
それを使えば以下のように各関数が作れます。

```idris
namespace CorrectMonoid
  unit : {auto impl : CorrectMonoid a o} -> a
  unit {impl} = _unit impl

  op : {auto impl : CorrectMonoid a o} -> a -> a -> a
  op {impl} = _op impl

  unitNeutral : {auto impl : CorrectMonoid a o} -> (x : a) -> ((_op impl) (_unit impl) x = x, (_op impl) x (_unit impl) = x)
  unitNeutral {impl} = _unitNeutral impl

  opAssociative : {auto impl : CorrectMonoid a o} -> (x, y, z : a) -> (_op impl) x ((_op impl) y z) = (_op impl) ((_op impl) x  y) z
  opAssociative {impl} = _opAssociative impl
```


それではこのインスタンスを作ってみましょう。
`%hint` ディレクティブをつけるとauto implicit argumentsの探索対象に入ります。

```idris
data Add

%hint
natAddCorrectMonoid : CorrectMonoid Nat Add
natAddCorrectMonoid = MkCorrectMonoid unit op unitNeutral opAssociative
where
  unit : Nat
  unit = 0
  op : Nat -> Nat -> Nat
  op = plus
  unitNeutral : (x : Nat) -> (op unit x = x, op x unit = x)
  unitNeutral n = (plusZeroLeftNeutral n, plusZeroRightNeutral n)
  opAssociative : (x, y, z : Nat) -> op x (op y z) = op (op x  y) z
  opAssociative = plusAssociative
```

ここまで問題なく定義できます。
もう1つくらいインスタンスを作ってみます。

```idris
data Max

%hint
natMaxCorrectMonoid : CorrectMonoid Nat Max
natMaxCorrectMonoid = MkCorrectMonoid unit op unitNeutral opAssociative
where
  unit : Nat
  unit = 0
  op : Nat -> Nat -> Nat
  op = maximum
  unitNeutral : (x : Nat) -> (op unit x = x, op x unit = x)
  unitNeutral n = (maximumZeroNRight n, maximumZeroNLeft n)
  opAssociative : (x, y, z : Nat) -> op x (op y z) = op (op x  y) z
  opAssociative = maximumAssociative
```


これを使う関数も定義してみましょう。
ここでもauto implicit argumentsを使います。

```idris
foldAll : {auto impl : CorrectMonoid a o} -> List a -> a
foldAll = foldl op unit
```

もうちょっと踏み込んでみましょう。演算を `Add` に限定した `sum` も定義してみます。

``` idris
sumAll : {auto impl : CorrectMonoid a Add} -> List a -> a
sumAll = foldAll
```

これを使ってみます。

``` idris
Idris> sumAll [Z, 1, 2, 3]
6 : Nat
```

デフォルトでIntegerに推論されるので無理矢理 `Nat` にするために `Z` を突っ込んでますが、おおむね自然に使えてますね。

`foldAll` のように演算が決まってない関数は `o` のimplicit argumentsに使いたい演算（と対応関係にある型）を渡してあげるとそれを使ってくれます。

``` idris
Idris> foldAll {o = Max} [Z, 1, 2, 3]
3 : Nat
```

どうやらauto implicit argumentsとインタフェースでは探索する条件が違うっぽくて、auto implicit argumentsを使えばちゃんとインスタンスをさがせるようです。
多分インタフェースが関数から型を逆算してるのに対してauto implicit argumentsは最初に書いた型から探索してて探しやすいんですかね。

ひとまず正しいモノイドが定義でたようです。

## インタフェースを拡張する

モノイドと定義できて満足したあなたはモノイドを拡張して可換モノイドも定義したくなりましたね。
ちょっとボイラープレートが多いですがそれも可能です。

``` idris
record CorrectCommMonoid a o where
  constructor MkCorrectCommMonoid
  _super : CorrectMonoid a o
  _opCommutative : (x, y : a) -> (_op _super) x y = (_op _super) y x

namespace CorrectCommMonoid
  fromCorrectMonoid : (super : CorrectMonoid a o) -> ((x, y : a) -> (_op super) x y = (_op super) y x) -> CorrectCommMonoid a o
  fromCorrectMonoid super _opCommutative = MkCorrectCommMonoid
    super
    _opCommutative

  unit : {auto impl : CorrectCommMonoid a o} -> a
  unit {impl} = _unit (_super impl)

  op : {auto impl : CorrectCommMonoid a o} -> a -> a -> a
  op {impl} = _op (_super impl)

  unitNeutral : {auto impl : CorrectCommMonoid a o}
                -> (x : a)
                -> ((_op (_super impl)) (_unit (_super impl)) x = x, (_op (_super impl)) x (_unit (_super impl)) = x)
  unitNeutral {impl} = _unitNeutral (_super impl)

  opAssociative : {auto impl : CorrectCommMonoid a o} -> (x, y, z : a) -> (_op (_super impl)) x ((_op (_super impl)) y z) = (_op (_super impl)) ((_op (_super impl)) x  y) z
  opAssociative {impl} = _opAssociative (_super impl)

  opCommutative : {auto impl : CorrectCommMonoid a o} -> (x, y : a) -> (_op (_super impl)) x y = (_op (_super impl)) y x
  opCommutative {impl} = _opCommutative impl

%hint
natAddCorrectCommMonoid : CorrectCommMonoid Nat Add
natAddCorrectCommMonoid = fromCorrectMonoid natAddCorrectMonoid opCommutative
where
  opCommutative : (x, y : Nat) -> plus x y = plus y x
  opCommutative = plusCommutative

%hint
natMaxCorrectCommMonoid : CorrectCommMonoid Nat Max
natMaxCorrectCommMonoid = fromCorrectMonoid natMaxCorrectMonoid opCommutative
where
  opCommutative : (x, y : Nat) -> maximum x y = maximum y x
  opCommutative = maximumCommutative
```

かくして目的が達成できました。

# 余談
## その1

`foldAll` はインタフェースのように `CorrectMonoid a o => ` で書くこともできます。


``` idris
foldAll : CorrectMonoid a o => List a -> a
foldAll = foldl op unit
```

多分「インタフェースが暗黙の引数になる」+「auto implicit parametersはローカル変数からも探索する」の合わせ技だと思います。
これだけならOKです。

ですがそれをやると `sumAll` で死にます。

``` idris
sumAll : CorrectMonoid a Add => List a -> a
sumAll = foldAll
```


``` text
- + Errors (1)
 `-- correctMonoid.idr line 59 col 9:
     When checking right hand side of sumAll with expected type
             List a -> a
     
     Can't find implementation for CorrectMonoid a o
```

暗黙の引数の探索まわりは謎が多いです。

## その2

Idrisのcontribには既存の（正しくない）代数構造を継承して演算に課される条件の証明を持つようにしたインタフェース群が定義されています。

[IdrisDoc: Interfaces.Verified](https://www.idris-lang.org/docs/current/contrib_doc/docs/Interfaces.Verified.html)

実装する対象が違う問題は名前付き実装を使っているようです。

# まとめ

Idrisで正しいモノイドを定義する方法を模索しました。
その過程でインタフェースやauto implicit argumentsなどの詳細に踏み込みました。
