---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-16T05:27:56+09:00
title: "Idrisの名前つきパラメータとGADT"
---

このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の13日目の記事です。

κeenです。
今回は座学パートとして標準ライブラリでよく使われてるけどまだ触れてない記法に触れていこうと思います。

<!--more-->


# 名前つきパラメータ

Idrisでは関数の型でもパラメータ名を与えられます。
例えば前回出てきた `replicate` の定義なんかがそうですね。

```idris
replicate : (n : Nat) -> (x : a) -> List a
```

`n` と `x` がパラメータ名です。
このケースでは以下のようにパラメータ名がなくても意味は変わりません。

```idris
replicate : Nat -> a -> List a
```

ただのドキュメント的存在として名前がついています。
特に標準ライブラリでは多用されているので覚えておきましょう。
エディタサポートでもこの名前は有用なのですが、それはいつか紹介したいと思います。


さて、この名前つきパラメータが重要な意味をもつこともあります。
`List` ではなく `Vect` の `replicate` を見てみましょう。

```idris
replicate : (len : Nat) -> (x : elem) -> Vect len elem
```

最初にでてきた `len` のパラメータが返り型 `Vect len elem` で使われてますね。
こういう引数に応じて返り型が変わるときにも有用です。

理論的には依存積やΠ型と呼ばれ、論理学では全称量化に対応します。

# GADT

`data name = ...` で定義するデータ型がありますよね？
あれは代数的データ型（Algebraic Data Types、 ADT）と呼ばれるんですが、それを一般化した Generalized ADT、略してGADTというのがあります。普通のADTの定義の別記法のようなものですが、表現力が増してます。
これも標準ライブラリでよく使われています。

`List a` をサクっと定義すると以下のようになりますよね。？

```idris
data List a = Nil | (::) a (List a)
```

この宣言で `List` 、 `Nil` 、 `(::)` の3つのシンボルが定義された訳です。
そしてそれぞれの型は以下のようになります。

```text
Idris> :t List
List : Type -> Type
Idris> :t Nil
Nil : List elem
Idris> :t (::)
(::) : elem -> List elem -> List elem
```

ここまでは大丈夫ですね？

この型を強調する書き方がGADTで、 `data 名前: 型 where コンストラクタ: 型 ...` の書き方をします。
例えばプレリュードでの `List` の定義は以下のようになっています。

```idris
data List : (elem : Type) -> Type where
  Nil : List elem
  (::) : (x : elem) -> (xs : List elem) -> List elem
```

名前つきパラメータと一緒にGADTが使われていますね。
定義としては上の方のシンプルな定義と同等です。
標準ライブラリのドキュメントとかでよく出てくるので覚えておきましょう。

さて、GADTはADTに比べて表現力が増してると書きました。
GADTは `Nil` の `List elem` や `(::)` の `-> List elem` のようにそれぞれのコンストラクタがどの型になるかを指定できます。
型を細かく指定できるIdrisではこれが有用なのです。

ADTで書けなくてGADTで書ける例を見てみましょう。baseの `Vect` の定義です。

```idris
data Vect : (len : Nat) -> (elem : Type) -> Type where
  Nil  : Vect Z elem
  (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem
```

`Nil` は `Vect Z elem` と長さ0であることが指定されています。
`(::)` は `(x : elem) -> (xs : Vect len elem) -> Vect (S len) elem` と長さ `len` の `xs` に `x` を加えて長さ `S len` になることが指定されています。

ようやくIdrisらしさが出てきましたね。

# まとめ

Idrisの名前つきパラメータとGADTを紹介しました。
この2つを押えたらAPIドキュメントは大体読めるはずです。
残るは依存和と証明オブジェクトですがそれはまたの機会に。
