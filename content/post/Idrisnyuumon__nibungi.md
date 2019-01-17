---
categories: ["Idris"]
date: 2019-01-17T23:34:55+09:00
draft: true
title: "Idris入門: 二分木"
---

κeenです。[Idris入門: 数当てゲーム | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2019/01/07/idrisnyuumon__kazuatege_mu/)に続いて入門記事を書いてみます。
前回はバイナリを作ったので今回はライブラリを作ります。題材は(非平衡)二分木。
<!--more-->
非平衡なのは平衡にすると複雑になるのと、平衡にするときは別のネタ(依存型)があるのでそれまでとっておくためです。

対象読者は前回の数当てゲームのチュートリアルを終えた人に設定します。

# プロジェクト作成

以下のような構成でディレクトリを作りましょう。

``` text
├── btree.ipkg
└── src
    └── BTree.idr
```

btree.ipkgには以下を書きます。

```text
package btree

sourcedir = src

modules = BTree
```

そして `BTree.idr` に今から二分木を実装していきます。


# 二分木とは

二分木とはデータ構造です。
値の保持、検索、削除ができるので集合やKey-Valueストアの実装に使われます。
手続き型言語ではそれらの実装にハッシュセットやハッシュマップがよく使われますが、関数型言語では二分木のほうが使われるようです。

二分木はノード(節)とリーフ(葉)からなります。ノードは1つの値と2つの子(部分木)を持ちます。リーフは何もデータを持ちません。
ノードには「ノードの左の子に保持されている全ての値よりもノードの保持している値の方が大きい。ノードの右の子に保持されている全ての値よりも値の方が小さい」という条件が成り立ちます。


図にするとこんな感じでしょうか

![二分木の図]()

``` text
    3
   / \
  1   4
 / \ / \
L   2L  5
   / \ / \
  L   L   L
```

1, 2, 3, 4, 5がノードに保持されている値です。一番上にあるノード(ここでは3を保持しているノード)を根と呼びます。
「ノードの左の子に保持されている全ての値よりもノードの保持している値の方が大きい。ノードの右の子に保持されている全ての値よりも値の方が小さい」という条件もちゃんと成り立っています。

![二分木の図の解説]()



# 二分木への操作

まずは検索と挿入を説明しましょう。

## 検索
検索は二分探索をそのまま行えます。
例えば以下の1, 2, 3, 4, 5を保持している木に2が含まれるか検索してみましょう。


``` text
    3    2
   / \
  1   4
 / \ / \
L   2L  5
   / \ / \
  L   L   L
```

まず3と2を比較します。2は3より小さいのであるとしたら左の子にあるはずです。左の子を見てみましょう。


``` text
    3    2
   / \
  1   4
 / \ / \
L   2L  5
   / \ / \
  L   L   L
```

左の子は1を保持しています。2は1より大きいのであるとしたら右の子にあるはずです。右の子をみてましょう。

``` text
    3    2
   / \
  1   4
 / \ / \
L   2L  5
   / \ / \
  L   L   L
```

右の子は2を保持しています。検索している値が見つかったのでこの木に2が含まれることが分かります。

同様の検索をして葉に行き当たったら検索している値は見つからないことが分かります。

## 挿入

挿入も検索と似たようなことをします。この木に6という値を挿入してみましょう。

``` text
    3    6
   / \
  1   4
 / \ / \
L   2L  5
   / \ / \
  L   L   L
```

6は3より大きいので右の子を見ます。

``` text
    3    6
   / \
  1   4
 / \ / \
L   2L  5
   / \ / \
  L   L   L
```

右の子を見ると6は4より大きいのでさらに右の子を見ます。

``` text
    3    6
   / \
  1   4
 / \ / \
L   2L  5
   / \ / \
  L   L   L
```

6は5より大きいので更に右の子を見ます。


``` text
    3    6
   / \
  1   4
 / \ / \
L   2L  5
   / \ / \
  L   L   L
```


右の子はリーフなのでこの木には6がいないことが分かりました。
そこで6だけを保持する **新しいノードを作ります**。


``` text
    3      6
   / \    / \
  1   4  L   L
 / \ / \
L   2L  5
   / \ / \
  L   L   L
```

5を保持するノードの右の子を今作ったノードにした **新しいノードを作ります**。

``` text
    3
   / \
  1   4
 / \ /
L   2L    5
   / \   / \
  L   L L   6
           / \
          L   L
```

4を保持するノードの右の子を今作ったノードにした **新しいノードを作ります**。

``` text
    3
   / 
  1     4
 / \   / \
L   2  L  5
   / \   / \
  L   L L   6
           / \
          L   L
```

3を保持するノードの右の子を今作ったノードにした **新しいノードを作ります**。

``` text
    3
   / \
  1   4
 / \ / \
L   2L  5
   / \ / \
  L   L   6
         / \
        L   L
```

これで更新が完了しました。

更新というよりは挿入する値も保持した新しい木を作る操作ですね。Idrisは純粋関数型言語なので破壊的変更ができません。なので更新ではなく新しい値を作ることになります。そういったときに部分構造を共有できる二分木は無駄が少なく効率的なデータ構造になるのです。


# 二分木の実装

さて、座学はこのくらいにして実装していきましょう。

## データ型の定義
まずはデータ構造の定義です。二分木とはリーフ、または2つの子と値を持ったノードからなるのでした。
BTree.idrに以下を実装します。

``` idris
module BTree

data BTree a = Leaf
             | Node (BTree a) a (BTree a)
```


これは[データ型](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#data-types)の定義です。`BTree a` というのは `a` 型の値を保持するBTreeという意味です。ジェネリクスになってますね。

定義の中身は `Leaf` または `Node` です。 `Leaf` はデータを持ちません。 `Node` は2つの子(`BTree a`)と 値(`a`)を持ちます。

## 挿入

このデータ型に対して挿入を実装してみましょう。まずは型は木 `BTree a` と値 `a` を受け取って新しい `BTree a` を返すのでこうなりそうです。

``` idris
insert : BTree a -> a -> BTree a
```

実装の方はリーフかノードかで分岐が発生します。
以下のように書けるでしょう。

``` idris
insert tree x = case tree of
                  Leaf => -- ...
                  Node l v r => -- ...
```

しかし引数で分岐するときは別の記法があります。以下のように書けるのです。

``` idris
insert Leaf         x = -- ...
insert (Node l v r) x = -- ...
```

こちらの方がより宣言的で読みやすいスタイルだとされています。


### 余談: エディタサポート
今の実装は実は自動化できます。
ここを書いたところまで巻き戻ってみましょう。

``` idris
insert : BTree a -> a -> BTree a
```

この状態で`insert`にカーソルを合わせてaddclauseと呼ばれるコマンド(Emacsなら`C-c C-s`, Vimなら `\d`)を打つとこうなります。

``` idris
insert:  BTree a ->  a -> BTree a
insert x y = ?insert_rhs
```

変数名が適当ですが定義のモックが自動生成されました。`?` マークで始まるのは穴(Hole)と呼ばれ、「あとで実装する」のマークです。詳しくは[ドキュメント](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#holes)を読んで下さい。

名前はおいておいて、エディタサポートの話を続けます。ここでの `x` で分岐したいのでした。
`x` にカーソルを合わせてcasesplitと呼ばれるコマンド(Emacsなら `C-c C-c`, Vimなら `\c`)を打つとこうなります。

``` idris
insert:  BTree a ->  a -> BTree a
insert Leaf y = ?insert_rhs_1
insert (Node x z w) y = ?insert_rhs_2
```

ここまで自動生成できました。

自動生成される名前を制御するにはこのチュートリアルを大きく超える内容が必要です。
型には `%name` ディレクティブを、ヴァリアントにはGADTを使ってそれぞれこう定義したら目的を達成できます。

``` idris
data BTree: (a : Type) -> Type where
  Leaf : BTree a
  Node : (l: BTree a) -> (v: a) -> (r: BTree a) -> BTree a

%name BTree tree, tree1, tree2
```

これで生成したコードは以下です。

``` idris
insert:  BTree a ->  a -> BTree a
insert Leaf x = ?insert_rhs_1
insert (Node l v r) x = ?insert_rhs_2
```

記法やコンパイラ補助の違いだけで、元の記法もこちらの記法も同じデータ型を定義しているので盲目的に定義を置き換えてしまうのも手です。

/余談

---

さて、これから実装していきましょう。
まずは`Leaf`の方です。`Leaf` なら新しい値を持ったノードで置き換えるのでした。
`Leaf` はこう実装できるでしょう。

``` idris
insert: Ord a => BTree a ->  a -> BTree a
insert Leaf x = Node Leaf x Leaf
insert (Node l v r) x = ...
```


`Node` の方は大きいか小さいかで分岐が発生しますね。
先程は触れませんでしたが既に同じ値がある場合も新しくノードを作って返すことにします。
これは無駄なので後ほど改善します。
比較の3種類の分岐は `compare` が便利でした。早速こう書いてみましょう。

``` idris
insert: Ord a => BTree a ->  a -> BTree a
insert Leaf x = Node Leaf x Leaf
insert (Node l v r) x = case compare x v of
  LT => ?LT
  EQ => ?EQ
  GT => ?GT
```

しかしこれはエラーになります。


```text
- + Errors (1)
 `-- BTree.idr line 18 col 24:
     When checking right hand side of insert with expected type
             BTree a
     
     Can't find implementation for Ord a

```

少し分かりづらいですが、「`a` に `Ord` の実装がない」と言っています。
ジェネリクス `a` はなんでも受け取れます。どんな型でもいいので比較が出来るとは限りません。
比較可能かも分からない `a` を比較しているのでエラーが出ている、というのが直接的原因です。
ではどうやったら修正できるか、はインターフェースの知識が必要なのでインターフェースについて説明します。

### インターフェース
[インターフェース](http://docs.idris-lang.org/en/latest/tutorial/interfaces.html)はデータ型間で共通の操作を定義する機能です。
インターフェースは以下の構文で定義でます。

```idris
interface インターフェース名 対象の型 where
    関数1 : 型
    関数2 : 型
    ...
    デフォルト実装(あれば)
    ...
```

`where` 以降はオフサイドルールですね。インターフェースの関数はメソッドと呼びます。
例えば色々な型を文字列として表示可能にするインターフェース `Show` はこう定義されています。

``` idris
interface Show a where
    show : a -> Strin
```

あるいは同値比較のインターフェース `Eq` はこう定義されています。

``` idris
interface Eq a where
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool

    x /= y = not (x == y)
    x == y = not (x /= y)
```

括弧がついている関数名は中置演算子を関数として扱っているのですが、まあ気にしないで下さい。
これは以下の構文で個々のデータ型に実装できます。

``` idris
インターフェース名 対象の型 where
    関数1 = 実装
    関数2 = 実装
```


適当なデータ型を定義して `Show` と `Eq` を実装してみましょう。

``` idris
data Camellia = Japonica | Sasanqua

Eq Camellia where
  (==) Japonica Japonica = True
  (==) Sasanqua Sasanqua = True
  (==) _        _        = False

Show Camellia where
  show Japonica = "Japonica"
  show Sasanqua = "Sasanqua"
```

そして「`Show` を実装した型を受け取る」はこう書けます。型定義の前に `Show a =>` を置くのです。

``` idris
print : Show a => a -> IO ()
print = putStr $ show
```

この型定義を読み下すなら「`Show` を実装した型 `a` に対して、 `a` を受け取って `IO ()` を返す関数」となるでしょう。

---

ということで先程の答えが出ました。 `Ord` が目的のインターフェースなので `Ord a =>` を追加すればよさそうです。
`insert` 関数の型定義に戻って `Ord a =>` を追加しましょう。


``` idris
insert: Ord a => BTree a ->  a -> BTree a
```

これで比較ができるようになったのであとは実装するだけです。
等しい場合は簡単ですね。そのまま作り直すだけです。

``` idris
insert (Node l v r) x = case compare x v of
  LT => ?LT
  EQ => Node l v r
  GT => ?GT
```

小さい場合は左の子(`l`)を、それに `x` 挿入したもので置き換えるのでした。大きい場合はその対称ですね。
つまり実装はこうなります。

``` idris
insert (Node l v r) x = case compare x v of
  LT => Node (insert l x) v r
  EQ => Node         l    v r
  GT => Node         l    v (insert r x)
```


同様に検索はこう実装できます。


``` idris
member : Ord a => BTree a -> a -> Bool
member Leaf x = False
member (Node l v r) x = case compare x v of
  LT => member l x
  EQ => True
  GT => member r x
```

さて、ひとまずデータを入れて検索するところまでいけました。

地味に、空の木を表す `empty` も作っておきましょう。

``` idris
empty : BTree a
empty = Leaf
```

関数ではなく値です。Idrisは純粋関数型言語なので更新される心配がなく、`empty` を値として定義して使いまわしても何も問題がありません。

# テスト
機能するものができたのでテストしてみましょう。
