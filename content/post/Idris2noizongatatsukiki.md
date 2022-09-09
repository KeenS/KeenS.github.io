---
categories: [Idris, Idris2, 依存型]
date: 2022-09-10T02:42:15+09:00
title: "Idris2の依存型つき木"
---
κeenです。[某勉強会](https://opt.connpass.com/event/255865/)の準備で色々Idrisのライブラリを眺めてたら面白いものを見付けたので共有します。
依存型があればどの言語でも定義できるはずですが、標準添付ライブラリとしてIdris2に入ってたのでタイトルにIdris2を入れときました。

<!--more-->

みつけたのはcontribパッケージにある[`Data.SortedMap.Dependent` モジュール](https://www.idris-lang.org/docs/idris2/current/contrib_docs/docs/Data.SortedMap.Dependent.html)です。データ型のカインドは以下のようになっています。

```idris
data SortedDMap : (k : Type) -> (k -> Type) -> Type
```

ちょっと慣れてないと読めないかもしれませんが、キーの型 `k` と、 `k` 型の **値に依存して** 決まるヴァリューの **型** を引数にとります。

概念だけだと多分ピンとこないので実際に値を構築してみましょう。キーに自然数の `Nat` 、 ヴァリューに長さつきリストの `Vect n String` を入れてみます。

```idris
let t = the (SortedDMap Nat (flip Vect String)) empty
let t = insert 0 [] t
let t = insert 1 ["one"] t
let t = insert 1 ["one"] t
let t = insert 2 ["one", "two"] t
```

キーの値に依存してヴァリューの型が変わる、今回の例だと `Vect` の `n` が変わるので長さが変わります。`0` のキーには長さ `0` の `Vect` 、 `2` のキーには長さ `2` の `Vect` のみを挿入できます。

ここでもし `2` のキーに長さ `1` の `Vect` を挿入しようとするとコンパイルエラーになります。

```idris
let t = insert 2 ["one"] t
```


```text
Error: While processing right hand side of main. When unifying:
    1
and:
    2
Mismatch between: 0 and 1.

Main:13:11--13:29
 09 |   let t = insert 0 [] t
 10 |   let t = insert 1 ["one"] t
 11 |   let t = insert 2 ["one", "two"] t
 12 |   -- error
 13 |   let t = insert 2 ["one"] t
                ^^^^^^^^^^^^^^^^^^
```

依存型は長さで制約するなど制限を強める方向の使い方が多いですが、それとペアになるように制約を使いこなすデータ型があるのは面白くていいですね。

コードはGitLabに置いておきます
<script src="https://gitlab.com/-/snippets/2405944.js"></script>
