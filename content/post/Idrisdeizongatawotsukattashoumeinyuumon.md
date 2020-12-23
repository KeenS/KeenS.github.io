---
categories: [依存型, Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-22T19:04:42+09:00
title: "Idrisで依存型を使った定理証明入門"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の19日目の記事です。
次はmock_beginnerさんで[Idrisとはじめる型駆動開発](https://kengo-yamashita.hatenablog.com/entry/2020/12/20/Idrisとはじめる型駆動開発)です。

κeenです。今回はこのAdvent Calendarの山場の1つ、定理証明について解説しようと思います。

<!--more-->

# 依存型で証明ができる原理

[カリー＝ハワード同型対応](https://ja.wikipedia.org/wiki/カリー＝ハワード同型対応)といって、プログラムのと論理学の定理には対応関係があることが知られています。
これはすなわち、我々が普段プログラムを書いているときは同時に論理学の命題を証明していることでもある、ということです。
そんな大それたことしてないよーと思うかもしれませんが、それもそのはず。
普通のプログラムではあまり面白い命題を表現できないので、わざわざ証明というほどでもないからです。

しかしIdrisには依存型があります。
依存型があると型の表現力が上がるので対応する論理学の命題の表現力が上がります。
すると面白いことも書けるのです。

# 三段論法

$P \to Q$ かつ $P$ ならば $Q$ というやつです。有名なのは

1. 全ての人間は死ぬ
2. ソクラテスは人間である
3. ソクラテスは死ぬ

でしょうか。これをIdrisで表現すると以下のようになります。

```idris
modusPonens : ((p -> q), p) -> q
```

これは以下の対応関係による表現です。

* 命題はIdrisの型で表現する
* 命題変数 $P$ 、 $Q$ …はジェネリクス変数 `p` 、 `q` … で表現する
* $\to$ （ならば）は関数型 `->` で表現する
* $\land$ (かつ)はタプルで表現する

これを証明しましょう。命題は型で表現しましたが、証明はプログラムで表現します。
型に合うプログラムを書けばそれで証明したことになります。
これは優しいですかね。
以下のプログラムで型が合います（＝証明できます）。

```idris
modusPonens : ((p -> q), p) -> q
modusPonens (hpq, hp) = hpq hp
```

雰囲気掴めましたか？
論理学のアイテムに対応するIdrisの型を選んで命題を表現し、あとはコンパイルを通せばいい訳です。

# $\lor$（または）

$\lor$（または）に対応するのは `Either` になります。

例えば $A \lor B \to B \lor A$ をIdrisの型で表現するとこうなります。

```idris
orComm : Either a b -> Either b a
```

これは簡単に証明できますね？


```idris
orComm : Either a b -> Either b a
orComm (Left  a) = Right a
orComm (Right b) = Left b
```

ここまでは難しくないんじゃないでしょうか。

# 全関数

プログラムと証明が対応すると書きましたが、どんなプログラムとも対応する訳ではなくて、ある程度条件を満たす必要があります。それはプログラムが全関数（total function）であるということです。

全関数とは（型の合う）入力を与えたら必ず値を返す関数です。
一見当たり前のように見えますが、2つの落とし穴、すなわち関数に値を与えても値が返ってこないケースがあります。

## 場合分け

1つの穴は場合分けの失敗です。
Idrisではパターンマッチで全ての場合を尽していなくてもコンパイルできるのでした。
そして対応していない値がきた場合はプログラムがクラッシュします。

これは証明でいったら場合分けの考慮漏れに相当します。

## 循環論法

もう1つの穴は無限ループです。
無限ループすると値は返ってこないですね。

これは証明でいったら循環論法に相当します。

-----

よくあるプログラミング言語ならこの他にも例外があてはまるのですが、Idrisには例外がないので気にしなくてよいです。

## `total`

さて、これらを避けないと証明として成立しないのですが、Idrisはそのための機能を提供しています。
関数に `total` という修飾子をつけると全関数であるかを確認してくれるのです。

例えば以下のように書きます。

```idris
total
orComm : Either a b -> Either b a
orComm (Left  a) = Right a
orComm (Right b) = Left b
```

少し試してみましょう。上記 `orComm` の1節を削って以下のようにします。

```idris
total
orComm : Either a b -> Either b a
orComm (Left  a) = Right a
```

これは `total` がなければコンパイルが通るコードです。
しかし `total` をつけているとエラーになります。

```text
$ idris --check proving.idr
proving.idr:8:1-26:
  |
8 | orComm (Left  a) = Right a
  | ~~~~~~~~~~~~~~~~~~~~~~~~~~
Main.orComm is not total as there are missing cases
```

`--check` は型検査などの検査のみやってくれるオプションです。
証明をするときは型さえ合えばいいので常にこのオプションを使うことになるでしょう。

同様に無限ループについても検査してくれます。
ただし無限ループについては注意が必要です。
全てのループ（関数の再帰呼出）について無限ループになるかならないかを判定する機能はありません（理論的に不可能）。
代わりに、ループの引数が構造的に確実に減少しているなど「コンパイラが確実に無限ループでないと保証できる」場合にのみ `total` の検査が通ります。
ここだけ必要十分条件ではなく十分条件なんですね。ここは理論的限界なので上手くつきあっていきましょう。


さて、この便利な `total` ですがファイル内の全ての関数をデフォルトで `total` 修飾子が付いているように振る舞わせることができます。`%default total` というディレクティブです。
これを使えば `orComm` の `total` 修飾子は不要になります。

```idris
%default total


orComm : Either a b -> Either b a
orComm (Left  a) = Right a
orComm (Right b) = Left b
```

逆に `%default total` の中で `total` でない関数 （部分関数、partial function）を書くには `partial` 修飾子をつけます。


---

以後この記事内では `%default total` を有効にした上でコンパイラは `idris --check` でコンパイルしているものとして進めます。

# ド・モルガンの法則

命題の否定（$\lnot A$）は `a -> Void` で表します。 `Void` はプレリュードで定義されているデータ型です。
`Not a = a -> Void` なるエイリアスもあります。

```text
Idris> :doc Void
Data type Void : Type
    The empty type, also known as the trivially false proposition.
    
    Use void or absurd to prove anything if you have a variable of type Void in
    scope.
    
    The function is: public export
No constructors.
```

No constructorsとあるように、コンストラクタがありません。つまり、 `Void` の値は作れないのです。
もし `Void` の値を作れたとしたら矛盾ですね。
矛盾からは何でも証明できます（$\bot \to A$）。
この規則はIdrisでは `void` 関数が対応します。

```text
Idris> :doc void
void : Void -> a
    The eliminator for the Void type.
    
    The function is: Total & public export
```

否定を上手く扱えそうですか？
それでは練習問題としてド・モルガンの法則を証明してみましょう。
ド・モルガンの法則はいくつかありますが、まずはこれです。

\\\[
\lnot(A \lor B) \to (\lnot A \land \lnot B)
\\\]

この命題をIdrisに対応させると以下の型になります。

```idris
deMorgan : (Not (Either p q) ) -> (Not p, Not q)
```

これを証明していきましょう。

まずは関数なので引数を取ります。


```idris
deMorgan : (Not (Either p q) ) -> (Not p, Not q)
deMorgan hnpq = …
```

ここで `hnpq` の型は `Not (Either p q)` （= `Either p q -> Void`） です。
ここから `(Not p, Not q)` （= `(p -> Void, q -> Void)`）の値を作ります。
落ち着いて `np : p -> Void` と `nq : q -> Void` の値をそれぞれ作るところからはじめましょう。
そうすれば `(np, nq)` を返すだけになります。

```idris
deMorgan : (Not (Either p q) ) -> (Not p, Not q)
deMorgan hnpq =
  let np = … in
  let nq = … in
  (np, nq)
```

`p -> Void` はよく考えると `Either p q -> Void` を使って簡単に作れますね。
`\hp => hnpq (Left hp)` です [^1]。
同様に `q -> Void` も `\hq => hnpq (Right hq)` から作れます。

総合すると、以下のコードでコンパイルが通ります（=証明できます）。

```idris
deMorgan : (Not (Either p q) ) -> (Not p, Not q)
deMorgan hnpq =
  let np =  hnpq . Left in
  let nq = \hq => hnpq (Right hq) in
  (np, nq)
```

[^1]: あるいは `hnqp . Left` でもいいのですが、ウッカリ `.` を紹介し忘れてるので本文では扱わないことにします。

ここまでは [5日目の記事](https://qiita.com/SekiT/items/855b49ad8b18f332dd5c) でも紹介されています。
この記事では読者への課題として以下の命題を証明する問題が出されています。

\\\[
\lnot a \land \lnot b \to \lnot \(a \lor b)
\\\]

ここで答え合わせといきましょう。

まず、Idrisにエンコードするとこういう型ですね。

```idris
deMorgan' : (Not p, Not q) -> (Not (Either p q) )
```

このプログラムを書いていきます。
まずは関数かつ引数がタプルなのでこういう書き出しです。

```idris
deMorgan' : (Not p, Not q) -> (Not (Either p q) )
deMorgan' (hnp, hnq) = …
```

ここで `Not a` = `a -> Void` であることと `a -> (b -> c)` = `a -> b -> c` であることを思い出しましょう。
すると `Either p q` も関数の引数として受け取れることが分かりますね。しかも `Either`なのでパターンマッチしましょう。こうなります。

```idris
deMorgan' : (Not p, Not q) -> (Not (Either p q) )
deMorgan' (hnp, hnq) (Left  p) = …
deMorgan' (hnp, hnq) (Right q) = …
```

これであとは `Void` を返すだけです。
`Void` は `hnp : p -> Void` と `hnq : q -> Void` から 出てきます。

結果、回答は以下のコードになります。

```idris
deMorgan' : (Not p, Not q) -> (Not (Either p q) )
deMorgan' (hnp, hnq) (Left  p) = hnp p
deMorgan' (hnp, hnq) (Right q) = hnq q
```

# 等価性の証明

Idrisにはほぼ証明専用の型、 `=` があります。
例えば以下のような型が書けるのです。

```idris
oneEqualOne : 1 = 1
```

これの証明はそれ専用の値 `Refl` を使います。

```idris
oneEqualOne : 1 = 1
oneEqualOne = Refl
```

`Refl` は `x = x` の値となります。
ここで `x = x` となる条件ですが、Idrisは「ある程度」計算をしてくれます。
例えば以下のコードはコンパイルが通ります。

```idris
onePlusOneEqualTwo : 1 + 1 = 2
onePlusOneEqualTwo = Refl
```

定数は全部計算してくれるようです。
逆に変数が絡むと途端に諦めるようになります。
例えば以下のコードはコンパイルエラーになります。

```idris
onePlusNEqualNPlusOne : 1 + n = n + 1
onePlusNEqualNPlusOne = Refl
```

``` text
- + Errors (1)
 `-- proving.idr line 31 col 24:
     When checking right hand side of onePlusNEqualNPlusOne with expected type
             1 + n = n + 1
     
     Type mismatch between
             prim__addBigInt n 1 = prim__addBigInt n 1 (Type of Refl)
     and
             prim__addBigInt 1 n = prim__addBigInt n 1 (Expected type)
     
     Specifically:
             Type mismatch between
                     prim__addBigInt n 1
             and
                     prim__addBigInt 1 n
```

ちょうどいいとっかかりなのでこれを証明してみましょう。

## 例題
$1 + n = n + 1$ を証明します。

まず、命題を少しいじります。 `n` をプログラム内で扱いたいので関数の引数で受け取ることにします。

``` idris
onePlusNEqualNPlusOne : (n: Nat) -> 1 + n = n + 1
```

これは命題でいえば $\forall n \in \mathbf{N}. 1 + n = n + 1$ に相当します。

そしてこれを証明していくのですが、数学的帰納法を使います。
すなわち $n = 0$ の場合を証明して、$n = k$ で成り立つならば $n = k + 1$ でも成り立つことを証明します。
Idris的にいうと `n = Z` の場合と `n = S k` の場合で場合分けして、 `n = S k` のときは再帰するということです。
やってみましょう。

### $n = 0$ の場合

まず `n = Z` の場合は `1 + 0 = 0 + 1` で、全て定数なのでIdrisが計算してくれて、 `Refl` で済みます。


``` idris
onePlusNEqualNPlusOne = Refl
```


### $n = k + 1$ の場合

次に $n = k$ で成立する、つまり `onePlusNEqualNPlusOne k` の呼出はできると仮定して `n = S k` の場合を証明します。

``` idris
onePlusNEqualNPlusOne (S k) = …
```

 `n = S k` の場合は `1 + (S k) = (S k) + 1` となります。
ここで、 `n + m` は `plus n m` で定義されていることと、 `plus` の定義から `S (S k) = S (plus k 1)` へと計算されます。Idrisは関数呼出も多少は計算してくれるようです。

``` idris
-- S (S k) = S (plus k 1) を返す
onePlusNEqualNPlusOne (S k) = …
```

ここで `onePlusNEqualNPlusOne k : 1 + k = k + 1` の存在を思い出しましょう。
これも多少Idrisが計算してくれるので `S k = plus k 1` となります。
`S (S k) = S (plus k 1)` の `plus k 1` の部分を `S k = plus k 1` を使って書き換えられたら `S (S k) = S (S k)` になるので `Refl` で証明できますね。

実際、等式を使って型を書き換える構文があります。 `rewrite 等式 in 値` の構文です。
これを使うと `S k` の節はこう書けます。

``` idris
onePlusNEqualNPlusOne (S k) =
  rewrite onePlusNEqualNPlusOne k
  in Refl
```

これで証明完了です。
プログラム全体は以下のようになります。

``` idris
onePlusNEqualNPlusOne : (n: Nat) -> 1 + n = n + 1
onePlusNEqualNPlusOne Z = Refl
onePlusNEqualNPlusOne (S k) =
  rewrite onePlusNEqualNPlusOne k
  in Refl
```

# 何のために証明するの？

上記まででIdrisで証明ができることは分かったかと思います。
では、どうしてIdrisを使って証明するのでしょう。
紙とペンで証明するのと何が違うのでしょう。

1つは機械で検査できる証明として。
上記まででIdrisのコードのコンパイルが通れば証明完了と紹介してきました。
つまりコンパイラが証明が正しいことを証明してくれるのです。
人間誰しも誤りはあるものなので機械がチェックしてくれると嬉しいですよね。

もう1つ、現実的な理由があります。
依存型を使ったコードを書くときに型を合わせるために証明が必要になるケースがあるのです。
例えば以下のコードをみてみましょう。

``` idris
rotateVec : Vect n a -> Vect n a
rotateVec [] = []
rotateVec (x::xs) = xs ++ [x]
```

一見問題ないように見えます。しかしこれをコンパイルするとエラーになります。

``` text
- + Errors (1)
 `-- proving.idr line 40 col 20:
     When checking right hand side of rotateVec with expected type
             Vect (S len) a
     
     Type mismatch between
             Vect (len + 1) a (Type of xs ++ [x])
     and
             Vect (S len) a (Expected type)
     
     Specifically:
             Type mismatch between
                     plus len 1
             and
                     S len
```

`x::xs` のパターンマッチで `n = 1 + k` であることが分かった一方で `xs ++ [x]` は `k + 1` になります。
まさしく先程証明した `1 + n = n + 1` が必要になるのです。実際、 `onePlusNEqualNPlusOne` を使って `rewrite` してあげればコンパイルは通ります。

``` idris
rotateVec : Vect n a -> Vect n a
rotateVec [] = []
rotateVec {n = S k} (x::xs) =
  rewrite onePlusNEqualNPlusOne k
  in xs ++ [x]
```

依存型は強力な保証をしてくれる一方でユーザにも保証の一端を担わせる諸刃の剣です。
Advent Calendar初日で「依存型のあるHaskell」と紹介しつつもあまり依存型について触れてこなかったのはこういう訳があるのです。


さて、最後にもう1つ、夢のある話をしましょう。Idrisで書ける証明で、プログラムの正しさも証明できるのです。
フィボナッチ数列を求める関数を例に採りましょう。

n番目のフィボナッチ数列を求めるナイーブな実装はこうですね？

``` idris
fib1 : Nat -> Nat
fib1 Z         = 1
fib1 (S Z)     = 1
fib1 (S (S k)) = fib1 (S k) + fib1 (k)
```

これは定義に従っていて正しさが目で見て分かりやすい一方でとても効率が悪いです。

そこで先頭から順番に計算していく別の実装を与えたくなります。

``` idris
loop : Nat -> Nat -> Nat -> Nat
loop prev2 prev Z     = prev
loop prev2 prev (S k) = loop prev (prev + prev2) k

fib2 : Nat -> Nat
fib2 n = loop 0 1 n
```

すると今度はこの実装が正しいか（fib1と同じ挙動をするか）パッとは自信がもてません。
ですがIdrisなら「100%正しい」と言える方法がありますよね。
以下の命題を証明すればいいのです。

``` idris
twoFibEq : (n: Nat) -> fib2 n = fib1 n
```

実際、これは以下のようにして証明できます。

``` idris
-- 補題
loopIsLikeFib : (i, j, n: Nat) -> loop (i + j) (i + j + j) n = (loop j (i + j) n) + (loop i j n)
loopIsLikeFib i j Z     = Refl
loopIsLikeFib i j (S k) =
  rewrite plusCommutative i j in
  rewrite plusCommutative (j + i) j in
  rewrite loopIsLikeFib j (j + i) k in
  Refl

-- 証明
twoFibEq : (n: Nat) -> fib2 n = fib1 n
twoFibEq Z         = Refl
twoFibEq (S Z)     = Refl
twoFibEq (S (S k)) =
  rewrite loopIsLikeFib 0 1 k in
  rewrite twoFibEq k in
  rewrite twoFibEq (S k) in
  Refl
```

かくして `fib2` は100% `fib1` と同じ振る舞いをすることが証明できました。

証明さえあれば数個の値で挙動を確かめるいい加減な方法（テストと呼ばれます）よりずっと確実に正しさを保証できるのです。夢があっていいですね。

# まとめ

型とは命題であり、プログラムとは証明であると紹介しました。
実際にいくかの命題を証明したあとプログラミング言語を用いて証明をする意義も紹介しました。
