---
categories: ["SML", "データ構造"]
date: 2019-11-02T23:36:25+09:00
title: "代数的データ型とパターンマッチの補遺: 赤黒木"
---

κeenです。
n月刊ラムダノートに書いた「代数的データ型とパターンマッチの基礎」のところで「パターンマッチがあるとこんなに便利になるよ」というのと紹介し忘れていたので勝手に補遺します。
まあ、紙面の都合もあって覚えていたとしても入りませんでしたね。
代数的データ型とパターンマッチの題材としては定番の赤黒木を紹介します。

<!--more-->


赤黒木についてはこちらのページを参考にして下さい。特に、比較になるので掲載されているソースコードを読んで下さい。

* [Red-Black Tree by Java -- これで分かった赤黒木](http://wwwa.pikara.ne.jp/okojisan/rb-tree/index.html)

ソースコードはPython版もあるようです：[Red-Black Tree by Python -- Python による赤黒木](http://wwwa.pikara.ne.jp/okojisan/rb-tree/python.html)。


読みましたか？

それでは赤黒木をStandard MLで書いていきます。
パターンマッチ関係ないところでややこしくならないように保持するデータ型は `int` とします。


色とデータ型は文句なく以下で決まります。

```standard-ml
datatype color = Red | Black
datatype t = Leaf | Node of color * t * int *  t
```

ここからすぐさま簡単な操作は書けます。

``` standard-ml
val empty = Leaf

fun isEmpty Leaf = true
  | isEmpty _    = false

fun isMember Leaf _ = false
  | isMember (Node (_, left, label, right)) x =
    (case Int.compare (label, x) of
         LESS => isMember right x
       | EQUAL => true
       | MORE => isMember left x)
```

さて、それでは `insert` と `balance` を書いていきます。Java（あるいはPython）の実装から分かるとおり、結構泥臭いというか何をやっているのか分かりづらい処理が続きます。これをパターンマッチで書き直したらどうなるか見てみましょう。

まずは便利関数を定義しておきます。

``` standard-ml
fun red   l x r = Node(Red,   l, x, r)
fun black l x r = Node(Black, l, x, r)
```

`balance` は以下で定義できます。

``` standard-ml
fun balance (Node(Black, Node(Red, Node(Red, a, x, b), y, c), z, d)) = red (black a x b) y (black c z d)
  | balance (Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)) = red (black a x b) y (black c z d)
  | balance (Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d))) = red (black a x b) y (black c z d)
  | balance (Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d)))) = red (black a x b) y (black c z d)
  | balance t = t
```


ここから `insert` を定義していきますが、その前に `Int.compare` 関数を紹介します。
`Int.compare` は以下の型を持つ関数です。

``` standard-ml
Int.compare: int * int -> order
```

ここで、 `order` は以下のように定義されています。

``` standard-ml
datatype order = LESS | EQUAL | GREATER
```

Rubyでいう `<=>` メソッドのような存在ですね。

以下のように動作します。

``` standard-ml
# Int.compare (1, 2);
val it = LESS : order
# Int.compare (2, 2);
val it = EQUAL : order
# Int.compare (3, 2);
val it = GREATER : order
```

これを用いて `insert` は以下で定義できます。


``` standard-ml
fun insert tree x = let
    fun ins Leaf x = red Leaf x Leaf
      | ins (t as Node(color, left, label, right)) x =
        (case Int.compare (label, x) of
             LESS => balance (Node(color, left, label, (ins right x)))
           | EQUAL => t
           | MORE => balance (Node(color, (ins left x), label, right))
        )
in
    case ins tree x of
        (* unreachable *)
        Leaf => Leaf
      | Node(_, left, label, right) => black left label right
end
```

`ins` のパターンマッチで使っている `t as Node(...)` という構文は、RubyのAsパターンと同様に `Node(...)` でパターンマッチしつつ全体を `t` に束縛する構文です。

あとは以下のような関数を1つ用意してあげればREPLで遊べます。

``` standard-ml
fun fromList [] = empty
  | fromList (x::xs) = insert (fromList xs) x
```


REPLでの様子：

``` standard-ml
# val tree = RedBlackTree.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
val tree =
  Node
   (
    Black,
    Node
     (
      Black,
      Node
       (
        Red,
        Node (Black,Node (Red,Leaf,1,Leaf),2,Leaf),
        3,
        Node (Black,Leaf,4,Leaf)
       ),
      5,
      Node (Black,Leaf,6,Leaf)
     ),
    7,
    Node (Black,Node (Black,Leaf,8,Leaf),9,Node (Black,Leaf,10,Leaf))
   ) : t
# isEmpty tree;
val it = false : bool
# isMember tree 0;
val it = false : bool
# isMember tree 2;
val it = true : bool
# isMember tree 10;
val it = true : bool
# isMember tree 11;
val it = false : bool
```


ここまで見てきたとおり、パターンマッチを使うことで複雑なデータ構造を簡潔に実装できます。
さらに、コードのほとんどが代数的データ型とパターンマッチで構成されていたことから分かるように、関数型言語ではちょっと便利なツール程度ではなくプログラムを構成する主たるパーツになっています。


今回のコードをちゃんとした書き方に直したものを貼っておきます。
1つ注意しておくと関数型な書き方なのでいわゆる永続データ構造と呼ばれる形になっています。遊んでみる方は留意して下さい。

<script src="https://gitlab.com/snippets/1909738.js"></script>
