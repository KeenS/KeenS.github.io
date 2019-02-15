---
categories: ["Idris", "Effective Idris"]
date: 2019-02-14T22:14:25+09:00
title: "Effective Idris: Lazy"
---
κeenです。最近はIdrisで簡易ビルドツール作ってます。ある程度出来上がったらお披露目しますね。
さて、今回はIdrisで遅延評価を実現する型、 `Lazy` を紹介します。
<!--more-->

# 遅延評価とは

恐らくみなさんのお使いの言語では関数をネストして呼ぶ時、内側から評価しすよね。

```
// これは f(n) を評価したあと g(..) を評価する
g(f(n))
```

Idrisも同じです。ごく自然ですね。
この評価の順番(評価戦略)は **先行評価** (eager evaluation) と呼ばれます。
わざわざ名前がついているからも分かるとおり、これ以外の評価戦略もありえます。

例えば、引数を評価しきって値になるのを待たずに関数を適用するのも考えられます。
例を出しましょう。今、 `f` と `g` を以下のように定義したとしましょう。

```
f(x) = x + 1
g(x) = x * x
```

このとき先行評価だと `g(f(1))` は

```
g(f(1))
// f(x) の x を 1 で置換
g(1 + 1)
g(2)
// g(x) の x を 2 で置換
2 * 2
4
```

と評価が進みますね。引数が値になってから関数が呼び出されます。

ところが、値になるのを待たずにそのまま置換すると、

```
g(f(1))
// g(x) の x を f(1) で置換
f(1) + f(1)
// f(x) の x を 1 で置換
(1 + 1) + f(1)
2 + f(1)
// f(x) の x を 1 で置換
2 + (1 + 1)
2 + 2
4
```

のようになります。良し悪しはおいておいて、こういう評価戦略も考えられます。少しマクロに似ていますね。別物ですが。
この外側から評価する戦略は *名前呼び* (call by name)と呼ばれます。
ただし賢明な読者ならお気づきのように `f(1)` の評価が2回走ってしまいます。これでは効率が悪いですね。
そこで、「1度計算した値を覚えておく」というのが考えられます。いわゆるメモ化です。


```
g(f(1))
// g(x) の x を f(1) で置換
f(1) + f(1)
// f(x) の x を 1 で置換。
(1 + 1) + f(1)
2 + f(1) // f(1) = 2 を覚えておく
// f(1) = 2 なのでそのまま結果を使う
2 + 2
4
```


こういうメモ化する名前呼び評価は *必要呼び* (call by need) だとか **遅延評価** (lazy evaluation) だとか呼ばれます。

これら3つの評価戦略は副作用がなければ同じ計算結果を返すことが知られています。

さて、副作用がなければと但し書きしたのでもちろん副作用があると挙動に違いがあります。
よく言われるのが先行評価だと「`if` 関数」が書けないのです。以下はLispでよく引き合いに出される例です。

```common-lisp
(defun my-if (cond then else)
  (if cond then else))
```

引数をそのまま `if` に渡しているので `if` と同じ挙動をしそうですが、関数が引数を先に計算してしまうので次のような呼び出しで挙動に違いが出ます。

```common-lisp
;; else節まで評価されてしまう
(my-if t (print t) (print nil))
;; => t
;; => nil

;; else節は評価されない
(if t  (print t) (print nil))
;; => t

```


それ以外にも、先行評価は引数を丁度一回評価するのに対して、遅延評価は引数を高々一回しか評価しないので計算量が減ります。
よく知られた例は[竹内関数](https://ja.wikipedia.org/wiki/%E7%AB%B9%E5%86%85%E9%96%A2%E6%95%B0)を遅延評価で計算すると速い、というのがあります。


# IdrisでのLazyの使い方

普段と少し違う動きをする遅延評価ですが、Idrisでも使えます。使い方は簡単で、型に `Lazy` を適用するだけです。
するとその引数は遅延評価されます。
例えば、Idrisで 「`if` 関数」は以下のように定義できます。 `Lazy` が適用されている2つの `a` 型、仮引数でいうと `t` と `e` が遅延評価されます。

```idris
ifThenElse : Bool -> Lazy a -> Lazy a -> a
ifThenElse True  t e = t
ifThenElse False t e = e
```

実際、`if` 式の内部実装としてこの関数が [`Prelude.Bool`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Bool.html)で定義されています。

`Lazy` は処理系で特別扱いされているので `Lazy a` 型の値から 結果の `a` 型の値を取得する処理を書く必要はありません。

# IdrisでのLazyの利用例

`Lazy` は不要な計算をしたくないだとか、`if` 式を関数で抽象化したいだとかで有用です。
例えば、 `Maybe` 型に値があればそれを返し、なければ計算するような処理は次のように書けますね。

```idris
case maybe of
  Just x => x
  Nothing = calcX 10000
```

これを関数に抽象化するときに `Nothing` と分かるまで計算したくないので `Lazy` が便利です。以下は [`Prelude.Maybe`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Maybe.html)より。

```idris
||| Convert a `Maybe a` value to an `a` value by providing a default `a` value
||| in the case that the `Maybe` value is `Nothing`.
fromMaybe : (Lazy a) -> Maybe a -> a
fromMaybe def Nothing  = def
fromMaybe def (Just j) = j
```

例えば Rustでは同様の関数は即時評価してしまう `unwrap_or(exp)` と少し面倒な代わりに必要でなければ評価しなくて済む `unwrap_or_else(|| exp)` に分かれています。Idrisではこういう使い分けは必要ありません。

同じく `Ordering` を辞書順に合成する関数にも `Lazy` が使われています。以下は [`Prelude.Interfaces`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Interfaces.html)より。

``` idris
||| Compose two comparisons into the lexicographic product
thenCompare : Ordering -> Lazy Ordering -> Ordering
thenCompare LT y = LT
thenCompare EQ y = y
thenCompare GT y = GT
```

あるいは、 `&&` 演算子も `Lazy` を使った関数です。以下は [`Prelude.Bool`](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Bool.html)より。

```idris
||| Boolean AND only evaluates the second argument if the first is `True`.
(&&) : Bool -> Lazy Bool -> Bool
(&&) True x  = x
(&&) False _ = False

```

演算子のオーバーロードを許す大抵の言語ではショートサーキットできないので `&&` はオーバーロードできません。しかし `Lazy` があれば実現できるのです。

# 他言語でのケース
もちろん、遅延評価が使えるのはIdrisだけではありません。
Haskellは基本の評価戦略が遅延評価[^1]なので有名ですし、他にもMirandaなど遅延評価を基本とする言語もあるようです。

[^1]: 厳密には仕様上は「非正格」かな？どうせみんなGHCしか使わないし細かいことはいいんだよ。

あるいはIdrisのように部分的にサポートしている言語だとScalaには `lazy val` というのがあって、変数を遅延評価するよう宣言できます。また、なんと、[pass by name](https://docs.scala-lang.org/tour/by-name-parameters.html)というのもあり、名前渡しができます。

ライブラリレベルだとSchemeではプロミスと呼ばれていたり、SML/NJではサスペンドと呼んでいたりする機能があります。

```ml
signature SUSP = sig
    type 'a susp
    val delay : (unit -> 'a) -> 'a susp
    val force : 'a susp -> 'a
end
```

ライブラリレベルなので他言語への移植は簡単ですが、一々 `delay` と `force` をつける必要があって面倒です。
言語レベルサポートとユーザレベルサポートの違いですね。

# まとめ

遅延評価は先行評価と違って引数を高々一回しか評価しないので計算が高速になるケースがあったり、 「`if` 関数」が作れたりと面白い特徴があります。
Idrisでもこれを簡単に使えて、いくつかの `if` 式の上に構築された処理を関数で抽象化できます。
また、Idrisだけでなくいくつか言語でも同様に遅延評価が使えます。

たまにあると便利な機能、遅延評価を是非使ってみて下さい。
