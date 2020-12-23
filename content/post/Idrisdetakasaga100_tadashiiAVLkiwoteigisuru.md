---
categories: [依存型, Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-23T20:29:01+09:00
title: "Idrisの依存型で高さが100%正しいAVL木を定義する"
---

このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の22日目の記事です。

κeenです。今回は依存型を使って部分的に正しさを証明したデータ構造を作っていきたいと思います。
慣れてない人には難しめの内容になるかもしれません。

<!--more-->

# 証明オブジェクト


本題に入る前に、依存型の難しさについて触れたいと思います。

自然数から1引く `dec` 関数を定義します。
以下のようなコードになりますよね？

```idris
dec : Nat -> Nat
dec n = n - 1
```

ところがこれ、コンパイルエラーになります。
メッセージは以下です。

```text
- + Errors (1)
 `-- dec.idr line 3 col 10:
     When checking right hand side of dec with expected type
             Nat
     
     When checking argument smaller to function Prelude.Nat.-:
             Can't find a value of type 
                     LTE 1 n
```


何故か "Can't find a value of type" といってますね。これは `Nat.(-)` のドキュメントを見ると分かります。

```text
Prelude.Nat.(-) : (m : Nat) -> (n : Nat) -> {auto smaller : LTE n m} -> Nat
    
    infixl 8
    
    The function is: Total & public export
```

引数に `m` と `n` の他に `{auto smaller : LTE n m}` がありますね。
これは **証明オブジェクト** を要求しているのです。
どういうことか説明しましょう。

まず、今計算している対象は自然数なので負になれません。
そもそも自由に引き算できない対象な訳です。
ですが、引かれる数が引く数以上であれば結果が0以上であることが保証できるので計算できますね。
`Nat.(-)` はそういうケースを想定したAPIです。
この「引かれる数が引く数以上である」を表わす値が証明オブジェクトです。
このことは `LTE` のドキュメントからも確認できます。

```text
Idris> :doc LTE
Data type Prelude.Nat.LTE : (n : Nat) -> (m : Nat) -> Type
    Proofs that n is less than or equal to m
    Arguments:
        n : Nat  -- the smaller number
        
        m : Nat  -- the larger number
        
    The function is: public export
```

`LTE n m` で `n <= m` を表わします。

これを踏まえて `Prelude.Nat.(-)` の型を意訳すると、 「`m` から `n` を引きたければ `n` が `m` 以下である証明をもってこい」となるのです。
言われてみればそうですよね。

ここまでの話が分からなかったら、以下のことだけ覚えておいて下さい

「自然数同士の引き算は面倒くさい」

これを押さえたら本題に入ります。

# AVL木

[AVL木](https://ja.wikipedia.org/wiki/AVL%E6%9C%A8)とは平衡二分探索木の1種です。
二分木なので左右の腕がある訳ですが、その腕同士の高さが、高々1しか違わない木です。
ここではあまり説明しないのでWikipediaの記事を読んで下さい

1つだけ重要なのは[木の回転](https://ja.wikipedia.org/wiki/%E6%9C%A8%E3%81%AE%E5%9B%9E%E8%BB%A2)という操作があることです。
新たに要素を追加した結果、左右の腕の高さが2違うようになることがあります。
そうなった場合に木を組み替えて左右の腕の高さが高々1しか違わないようにする操作が回転です。

この木の高さを正しく維持するのは複雑で、よく間違えます。
なので依存型を使ってコンパイル時に正しいことを保証しよう、というのが今回の趣旨です。

# 依存型を使ったAVL木の定義
## 定義1

高さ `h` のAVL木の左右の腕の高さは `h - 1` または `h - 2` （ただし左右同時に `h - 2` にはならない）です。
正確には木の高さを $h$ 、左の木の高さを $hl$ 、 右の木の高さを $hr$ としたとき、以下の関係式を満たします。

\\\[
 (hl = h - 2 \land hr = h - 1) \lor
 (hl = h - 1 \land hr = h - 1) \lor
 (hl = h - 1 \land hr = h - 2)
\\\]

これを型にエンコードしたAVL木を定義すればよさそうです。
ただし先程紹介したように `-` は扱いが面倒なので移項して足し算にして、以下の定義が考えられます。

``` idris
infixr 1 \/, /\

(\/) : Type -> Type -> Type
(\/) a b = Either a b

(/\) : Type -> Type -> Type
(/\) a b = (a, b)

data Tree : (n: Nat) -> (a : Type) -> Type where
  Leaf: Tree Z a
  Node : {l, m: Nat} ->
   ((2 + l = n) /\ (1 + m = n)) \/
   ((1 + l = n) /\ (1 + m = n)) \/
   ((1 + l = n) /\ (2 + m = n)) ->
   Tree l a -> a -> Tree m a -> Tree n a
```

`Node` 引数のの前半が重いですね。
これは何をしているかというと、証明オブジェクトを受け取っているということになります。

これでも正しくエンコードできているのですが、例の「面倒くさい」証明オブジェクトが登場してしまいました。
証明オブジェクトが出てきても面倒なだけでプログラムは書けるのですが、どうしても冗長になりがちです。
試しにこの定義に対して操作を書いてみたら、後で出てくる6行の関数 `rotateR` がこの定義だと48行になってました。
ちょっとやってられないので別の定義を採ることにします。

## 定義2

先程は条件分岐を1箇所に押し込もうとして冗長になってました。
条件分岐は左が右より1高い、左右が同じ高さ、右が左より1高いの3通りです。
であれば、3通りをバラしてしまえばよさそうです。
するとこういう定義ができあがります。

``` idris
data Tree : Nat -> (a : Type) -> Type where
  Leaf: Tree Z a
  Lefty  : Tree (S n) a -> a -> Tree    n  a -> Tree (S (S n)) a
  Mid    : Tree    n  a -> a -> Tree    n  a -> Tree (S n) a
  Righty : Tree    n  a -> a -> Tree (S n) a -> Tree (S (S n)) a
```

これでもおおむねよいのですが、左右の高さに関係のない関数が冗長になってしまいます。
例えば要素が含まれるかを検査する `member` 関数は3回同じことを書かないといけなくなります。

``` idris
total
member : Ord a => a -> Tree n a -> Bool
member _ Leaf = False
member x (Lefty  l v r) with (compare x v)
  | LT = member x l
  | EQ = True
  | GT = member x r
member x (Mid    l v r) with (compare x v)
  | LT = member x l
  | EQ = True
  | GT = member x r
member x (Righty l v r) with (compare x v)
  | LT = member x l
  | EQ = True
  | GT = member x r
```

このくらいであれば必要経費とすることもできるのですが、もうちょっと上手くやる方法があります。


## 定義3

[View](https://keens.github.io/blog/2020/12/22/idrisomoshirokinou_withkoubuntoview/)のことを思い出してほしいのですが、依存型で値同士の関係をうまいこと制御できるのでした。
この仕組みを使うともう少し整理できます。
左右の木の高さを表現するビューと、実際のデータ構造に分けて管理するのです。

``` idris
data Balance : Nat -> Nat -> Nat -> Type where
  Lefty  : Balance (S n) (S (S n))    n
  Mid    : Balance    n     (S n)     n
  Righty : Balance    n  (S (S n)) (S n)

data Tree : Nat -> (a : Type) -> Type where
  Leaf : Tree Z a
  Node : Balance l n r -> Tree l a -> a -> Tree r a -> Tree n a
```

これなら十分整理されていると言えるでしょう。この定義を使っていきます。

# AVL木の簡単な操作

以下のような簡単な操作ならすぐさま書けるでしょう。

``` idris
total
empty : Tree Z a
empty = Leaf

total
singleton : a -> Tree (S Z) a
singleton x = Node Mid Leaf x Leaf

total
member : Ord a => a -> Tree n a -> Bool
member _ Leaf = False
member x (Node _   l v r) with (compare x v)
  | LT = member x l
  | EQ = True
  | GT = member x r
```

`member` 関数がちゃんと短かくなってるのがポイントですね。

# AVL木への挿入

AVL木は強めに平衡のとれた木なので要素を増減させる操作が重いです。

ちょっとずつ便利関数を積み重ねていきましょう。
まずはノードを作る関数3つ。

``` idris
total
createR : Tree n a -> a -> Tree (S n) a -> Tree (S (S n)) a
createR l x r = Node Righty l x r

total
createM : Tree n a -> a -> Tree n a -> Tree (S n) a
createM l x r = Node Mid l x r

total
createL : Tree (S n) a -> a -> Tree n a -> Tree (S (S n)) a
createL l x r = Node Lefty l x r
```

ちゃんと高さが正しいものしか作れないような型になっています。
一方でそのために `create` 関数が3種類に分かれてしまっています。

次が木の右回転をする `rotateR` 関数です。

``` idris
total
rotateR : Tree (S (S n)) a -> a -> Tree n a -> Either (Tree (S (S n)) a) (Tree (S (S (S n))) a)
rotateR (Node Lefty  ll lv lr)                        v r = Left  $ createM          ll lv          (createM lr  v r)
rotateR (Node Mid    ll lv lr)                        v r = Right $ createR          ll lv          (createL lr  v r)
rotateR (Node Righty ll lv (Node Lefty  lrl lrv lrr)) v r = Left  $ createM (createM ll lv lrl) lrv (createR lrr v r)
rotateR (Node Righty ll lv (Node Mid    lrl lrv lrr)) v r = Left  $ createM (createM ll lv lrl) lrv (createM lrr v r)
rotateR (Node Righty ll lv (Node Righty lrl lrv lrr)) v r = Left  $ createM (createL ll lv lrl) lrv (createM lrr v r)
```

実装は二分木の回転を実装したことのある方なら見慣れてるかと思いますが、型がちょっと独特です。
返り型 `Either (Tree (S (S n)) a) (Tree (S (S (S n))) a)` と `Either` の形になっています。
木の回転をすると高さが1増える場合と増えない場合があるのです。その場合分けのために `Either` を使っています。
`Left` が高さが変わらない場合で、 `Right` が高さが1増える場合ですね。
依存型のついていない普通の関数なら高さが違っても型は同じなので気にしなくていいのですが、今回は必要な分岐です。

これと同様に左回転の `rotateL` も実装できます。

``` idris
total
rotateL : Tree n a -> a -> Tree (S (S n)) a -> Either (Tree (S (S n)) a) (Tree (S (S (S n))) a)
rotateL l v (Node Lefty  (Node Lefty  rll rlv rlr) rv rr) = Left  $ createM (createM l v rll) rlv (createR rlr rv rr)
rotateL l v (Node Lefty  (Node Mid    rll rlv rlr) rv rr) = Left  $ createM (createM l v rll) rlv (createM rlr rv rr)
rotateL l v (Node Lefty  (Node Righty rll rlv rlr) rv rr) = Left  $ createM (createL l v rll) rlv (createM rlr rv rr)
rotateL l v (Node Mid    rl                        rv rr) = Right $ createL (createR l v rl)                   rv rr
rotateL l v (Node Righty rl                        rv rr) = Left  $ createM (createM l v rl)                   rv rr
```


それでは準備が整ったので挿入を定義しましょう。
挿入は回転と同じく木の高さが変わったり変わらなかったりする操作です。
なので型は以下のようになります。

``` idris
total
insert : Ord a => a -> Tree n a -> Either (Tree n a) (Tree (S n) a)
```

続いて簡単な場合から潰していきましょう。
`Leaf` への挿入はシングルトンで一撃です。高さは1増えます。

``` idris
insert x Leaf = Right $ singleton x
```

それ以外の場合はノードの値と挿入しようとしてる値の大小関係を比較して `with` 構文でひとまとめにパターンマッチしましょう。

``` idris
insert x (Node bal l v r) with (compare x v)
```

それぞれ場合分けしていくのですが、 `x` と `v` が等しい場合は挿入せずにそのまま終了します。

``` idris
  insert x (Node bal    l v r) | EQ = Left $ Node bal    l v r
```

`x` が `v` より小さい場合は左の木に `x` を挿入します。

ここで `Balance` の値に応じて依存型で使っている数値が変化することを思い出して下さい。
この挙動を利用するには `Balance` の値に対してパターンマッチしないといけません。
ちょっと面倒ですが `Balance` の値で分岐したあとに `x` を左の木に挿入することになります。。
つまりこのようなコードになります。

``` idris
  insert x (Node Lefty  l v r) | LT = case insert x l of
    ...
  insert x (Node Mid    l v r) | LT = case insert x l of
    ...
  insert x (Node Righty l v r) | LT = case insert x l of
    ...
```

それぞれのケースで、 `Left` の場合、すなわち挿入前と挿入後で木の高さが変わらなかった場合は挿入前と `Balance` は変わりません。

``` idris
  insert x (Node Lefty  l v r) | LT = case insert x l of
    Left  l => Left  $ Node Lefty l v r
    ...
  insert x (Node Mid    l v r) | LT = case insert x l of
    Left  l => Left  $ Node Mid   l v r
    ...
  insert x (Node Righty l v r) | LT = case insert x l of
    Left  l => Left  $ Node Righty l v r
    ...
```

`Right` の場合、すなわち挿入前から高さが1増えた場合は `Righty` → `Mid` 、 `Mid` → `Lefty` へと変化します。では `Lefty` の場合はどうなるかというと、回転が発生します。左に寄りすぎたので右回転ですね。

``` idris
  insert x (Node Lefty  l v r) | LT = case insert x l of
    ...
    Right l => rotateR            l v r
  insert x (Node Mid    l v r) | LT = case insert x l of
    ...
    Right l => Right $ Node Lefty l v r
  insert x (Node Righty l v r) | LT = case insert x l of
    ...
    Right l => Left  $ Node Mid    l v r
```

これと同様に `x` が `v` より大きい場合も書けますね。
総合して `insert` は以下のような見た目になります。

``` idris
total
insert : Ord a => a -> Tree n a -> Either (Tree n a) (Tree (S n) a)
insert x Leaf = Right $ singleton x
insert x (Node bal l v r) with (compare x v)
  insert x (Node bal    l v r) | EQ = Left $ Node bal    l v r
  insert x (Node Lefty  l v r) | LT = case insert x l of
    Left  l => Left  $ Node Lefty l v r
    Right l => rotateR            l v r
  insert x (Node Mid    l v r) | LT = case insert x l of
    Left  l => Left  $ Node Mid   l v r
    Right l => Right $ Node Lefty l v r
  insert x (Node Righty l v r) | LT = case insert x l of
    Left  l => Left  $ Node Righty l v r
    Right l => Left  $ Node Mid    l v r
  insert x (Node Lefty  l v r) | GT = case insert x r of
    Left  r => Left  $ Node Lefty  l v r
    Right r => Left  $ Node Mid    l v r
  insert x (Node Mid    l v r) | GT = case insert x r of
    Left  r => Left  $ Node Mid    l v r
    Right r => Right $ Node Righty l v r
  insert x (Node Righty l v r) | GT = case insert x r of
    Left  r => Left  $ Node Righty l v r
    Right r => rotateL             l v r
```

依存型で高さを保ったAVL木の操作が書けました。
型で高さに矛盾がないことを表現しているので、テストなどを書かなくても高さについてはバグがないことが保証できます。

これで木を作れるようになったので試してみましょう。
`(insert 1 (insert 10 (insert 3 (insert 1 empty))))` で `1` 、 `10` 、 `3` が入った木を作れるはずです。

``` text
Idris> :let tree = (insert 1 (insert 10 (insert 3 (insert 1 empty))))
(input):1:14-61:When checking an application of function Main.insert:
        Type mismatch between
                Tree n1 a2 \/ Tree (S n1) a2 (Type of insert _ _)
        and
                Tree n a (Expected type)
        
        Specifically:
                Type mismatch between
                        Either (Tree n1 a2) (Tree (S n1) a2)
                and
                        Tree n a
```


…あれ？何やらエラーが出ていますね。
思い出すと `insert` の返り型は `Either (Tree n a) (Tree (S n) a)` なので木ではなく `Either` 型の値が返っています。これでは連続して `insert` できませんね。

もう少し言うとこの木を使うユーザに、木の高さで分岐を強要するAPIになっています。
これはいささか以上に不便です。
内部の正しさを保証するのに木の高さを型にエンコードするのはよいのですが、APIでは型から木の高さを消しましょう。

# 依存ペアによる型消去

APIでは型から木の高さを消しましょうといいましたが、型を消去するなんてできるのでしょうか。
結論からいうと、できます。
くどい話は先送りにして、以下のように書けば型から高さの情報を消せます。

``` idris
data Set : Type -> Type where
  MkSet : (n: Nat ** Tree n a) -> Set a
```

ここでポイントになるのが `(n: Nat ** Tree n a)` という型です。
「ある自然数 `n` が存在して、高さ `n` なる木 `Tree n a` 」を表現しています。

この型は依存ペア、依存和、Σ型などと呼ばれます（[公式ドキュメントでは依存ペア](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#dependent-pairs)）。
文法がちょっと変わってるのですが、 `(変数名: 型 ** <変数を使った型>)` の構文です。
さきほど「ある 〜 が存在して」と紹介しましたが、論理学でいう存在量化に相当する機能です。

依存ペアを使うことで依存型を消去できるのです。説明が難しいので細かな解説はあきらめます。

さて、この定義を使ってAPIを定義していきましょう。

手始めに、さっきまでのコードを `Internal` の名前空間に押し込めましょう。

``` idris
namespace Internal
  data Balanse ...

  ...
  total
  inssert : ...
  ...
```

そしてAPIを定義しはじめます。

``` idris
data Set : Type -> Type where
  MkSet : (n: Nat ** Internal.Tree n a) -> Set a
```

なんだかんだ、APIとして提供できるのは `empty` 、 `insert` 、 `member` くらいですね。これらのラッパーを書きましょう。

まずは `empty` 。

``` idris
total
empty : Set a
empty = MkSet (_ ** Internal.empty)
```

依存ペアの値は `(値 ** 値)` の構文で作ります。
ところが左側の値、ここでいうの高さは右側の値から簡単に分かります。
そういう場合は `_` と書くとコンパイラが勝手に埋めてくれます。

`member` 関数も簡単ですね。

``` idris
total
member : Ord a => a -> Set a -> Bool
member x (MkSet (_ ** tree)) = Internal.member x tree
```

`insert` もほぼラップするだけですが、返り値が `Either` なのを思い出して `Left` と `Right` で処理を分けます。

``` idris
total
insert : Ord a => a -> Set a -> Set a
insert x (MkSet (_ ** tree)) =
  case Internal.insert x tree of
    Left t  => MkSet (_ ** t)
    Right t => MkSet (_ ** t)
```

因みに木の高さは値として保存されているので取り出すこともできます。

``` idris
total
height : Set a -> Nat
height (MkSet (height ** _)) = height
```


さてさて、これで役者が揃ったので今度こそ動いているか試せます。

``` idris
Idris> :let tree = (insert 1 (insert 10 (insert 3 (insert 1 empty))))
defined
Idris> member 1 tree
True : Bool
Idris> member 2 tree
False : Bool
Idris> member 3 tree
True : Bool
Idris> member 9 tree
False : Bool
Idris> member 10 tree
True : Bool
```

ちゃんと動いてますね。

# まとめ

依存型によって木の高さが正しいことが保証されたAVL木を作りました。
その過程で証明オブジェクトや依存ペアなどの機能も学びました。

# 付録: 今回のコード

<script src="https://gitlab.com/-/snippets/2053617.js"></script>
