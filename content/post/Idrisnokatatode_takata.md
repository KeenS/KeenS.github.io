---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-06T16:13:22+09:00
title: "Idrisの型とデータ型"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の7日目の記事です。
前はrigh1113さんで[自然数の加法の交換法則 by Idris](https://righ1113.hatenablog.com/entry/2020/12/05/074034)でした。

κeenです。今日は型とデータ型を紹介します。

<!--more-->

# プリミティブ

先日紹介したとおり、プリミティブは以下です。

| 名前      | 説明               |
|-----------|--------------------|
| `Int`     | 固定長整数         |
| `Integer` | 多倍長整数         |
| `Double`  | 倍精度浮動小数点数 |
| `Char`    | 文字               |
| `String`  | 文字列             |
| `Ptr`     | FFI用              |

Haskellに慣れている方に注意ですが、 `String` は `List Char` ではありません。

# データ型

先日紹介したとおり、データ型は `data 名前 = 定義` の構文で定義します。
定義のところには `ヴァリアント | ヴァリアント…` と書きます。
ヴァリアントには `コンストラクタ 引数の型 …` と書きます。
ヴァリアントは少なくとも1つ、コンストラクタの引数は0以上を書きます。

例：引数のないコンストラクタのヴァリアントを2つ持つデータ型

``` idris
data Bool = True | False
```



例：引数の2つあるコンストラクタのヴァリアントを1つ持つデータ型

``` idris
data Person = MkPerson Int String
```

引数ありのコンストラクタのヴァリアントを1つ持つデータ型は頻出パターンで、構造体のように使えます。
そのときのコンストラクタが構造体のコンストラクタのようになります。こういうときは `MkHoge` と `Mk` （makeの略）を前置するのが慣例です。


例： 引数のあるコンストラクタや引数のないコンストラクタのヴァリアントのあるデータ型

``` idris
data FizzBuzz = F | B | FB | I Integer
```


データ型は自身を定義に使う再帰的定義もできます。

例：任意個の整数を保持できる型の定義


```idris
data IntList = Cons Int IntList | Nil

Cons 1 (Cons 2 (Cons 3 Nil))
```


## データ型とパターンマッチ

データ型のコンストラクタは値を構築するときだけでなく分解するときにも使います。
パターンマッチでそのままマッチできるのです。

例： `Person` 型から1つ目の引数、2つ目の引数の値を取り出す関数の定義

``` idris
age : Person -> Int
age (MkPerson age _) = age

name : Person -> String
name (MkPerson _ name) = name
```


`|` で複数のバリアントを定義しているデータ型には分岐を使うことになるでしょう。

例： 複数バリアントのあるデータ型へのパターンマッチで分岐するコード


```idris
show : FizzBuzz -> String
show F     = "fizz"
show B     = "buzz"
show FB = "fizzbuzz"
show (I n)  = toString n
-- toStringは実際は存在しないが、説明の簡単さのために使う。
```

再帰的データ型なら呼応するように再帰関数を使うことになります。

例： 再帰関数を用いて `IntList` の長さを求めるコード


```idris
length : IntList -> Int
length Nil         = 0
length (Cons _ tl) = 1 + (length tl)
```

このようにデータ型からプログラムの構造が自然と決まるのでデータ型は便利かつ重要な機能です。


# ジェネリクス

上記の `IntList` は `Int` の値のみ保持できました。
これを任意のデータを保持できるようにするにはジェネリクスを使います。
つまり `IntList` を保持するデータ型について一般化するのです。

一般化したときの型は引数で受け取れるようにし、それを型変数で表します。

ジェネリクスで使う型変数は関数の引数のように `List a` と置きます。
小文字の変数が自動で型変数として扱われます。
そして `List` の定義のところで `Int` だった部分を `a` で置き換えます。

例：任意の型について、その型の値を任意個保持できるデータ型の定義

```idris
data List a = Cons a IntList | Nil

Cons 1 (Cons 2 (Cons 3 Nil))
Cons 'a' (Cons 'b' (Cons 'c' Nil))
```


関数定義も同様にジェネリクスにできます。
関数定義は特に引数などを導入せずにそのまま小文字の変数を使うだけでジェネリクスになります。

例： `List a` の長さを求めるコード

```idris
length : List a -> Int
length Nil         = 0
length (Cons _ tl) = 1 + (length tl)
```



# レコード

`Person` 型のように事実上構造体として扱うデータ型の定義は専用の便利構文が用意されています。

`Person` 型とそのメンバーへのアクセサは以下のように定義されているのでした。

例：データ型を構造体のように使うコード（再掲）

```idris
data Person = MkPerson Int String

age : Person -> Int
age (MkPerson age _) = age

name : Person -> String
name (MkPerson _ name) = name
```


これとほぼ同等のことを `record` 構文でできます。
レコードは `record 名前 [型引数…] where 本体` で定義します。
本体には `constructor コンストラクタ名` か `フィールド, …: 型` を書きます。
例えば以下のように使います。

例： `MkPerson` をレコードで定義するコード

``` idris
record Person where
  constructor MkPerson
  age: Int
  name: String
```


これは裏では以下のようなコードが生成されているようです。

例： `MkPerson` をレコードで定義するコードを展開したイメージ


``` idris
data Person = MkPerson Int String

age : Person -> Int
age (MkPerson age _) = age

name : Person -> String
name (MkPerson _ name) = name

set_age : Int -> Person -> Person
set_age age (MkPerson _ name) = MkPerson age name

set_name : String -> Person -> Person
set_name name (MkPerson age _) = MkPerson age name
```

Idrisは純粋関数型言語なので値を変更することはできません。
代わりにフィールドの値の違う新しい値を返します。

## レコードの関数型更新構文

`set_xxx` を呼び出すだけですが、レコードを更新する（フィールドの値を変更した新しい値を返す）構文が用意されています。
`record { … }` の形をしています。 `…` の中に入る構文はいくつか種類があります。

例： レコードの関数型更新構文のいくつか


``` idris
-- フィールド = 値
record { age = 29 }

-- フィールド $= 更新関数
record { age $= (+ 1) }
-- (+ 1) は1を足す関数

-- 複数のフィールドの更新
record { age $= (+ 1), name = "anonymous" }
```

他にもネストしたフィールドの更新構文なんかもあります。

さて、これらの構文は関数へと展開されます。

例：関数型更新構文が関数として振る舞うことがわかる例

``` idris
incAge: Person -> Person
incAge = record { age $= $(+ 1) }
```

なので実際に使うときは `record { age $= (+ 1) } p` のようにレコード `p` を引数として渡すことになります。


# 関数の型

先日関数の型は `引数1の型 -> 引数2の型 -> … -> 引数nの型 -> 返り値の型` と紹介しましたが、あれは嘘です。
嘘というか事実上は合ってるんですが、厳密には異ります。

Idrisの関数の型は `引数の型 -> 返り値の型` のみです。
じゃあ `Int -> Int -> Int` とかの型は何というと、 `->` が右結合なので `Int -> (Int -> Int)` と解釈される訳です。

同じく、関数の適用も `関数 引数` のみです。 `関数 引数1 引数2` は `(関数 引数) 引数` と解釈されます。

さらに、関数定義の構文 `関数名 引数1 … 引数n = 本体` もこれ以外の定義もありえます。
結果として型の帳尻さえあっていれば引数の個数は柔軟です。
例えば以下の `add` の定義は全て同等です。

例： `add` を関数定義構文やラムダ式やその組み合わせで定義するコード

``` idris
-- 関数定義構文による定義
add: Int -> Int -> Int
add x y = x + y

-- 引数を1つ関数定義構文で受け取り、1つラムダ式で受け取る定義
add: Int -> Int -> Int
add x = \y => x + y

-- 引数を全てラムダ式で受け取る定義
add: Int -> Int -> Int
add = \x => \y => x + y

-- 複数引数をとるラムダ式による定義
add: Int -> Int -> Int
add = \x, y => x + y

```


ところで `Int -> (Int -> Int)` は「 `Int` を与えると `Int -> Int` を返す関数」 です。
試してみましょう。 `add` に引数を1つだけ与えてみます。

``` idris
inc: Int -> Int
inc = add 1
```

つまり `add 1 2` は一度 `add 1` を計算して、その結果の関数を `2` に適用しているということになります。
毎回関数を作っては適用してを繰り返したら遅いじゃないかと思われるかもしれませんが、コンパイラの作者もそれは百も承知です。
ちゃんと最適化でそこら辺のコードは綺麗に消えます。

関数の引数を1つにして、代わりに「関数を返す関数を返す関数を…」とすることで言語のシンプルサを保っています。
また、そのオマケとして `inc` の定義のように引数を部分適用した関数も作りやすくなっています。

# 便利な型

処理系を起動したときに最初から使える型をいくつか紹介したいと思います。

処理系を起動したときに最初から使える型はいくつかの場所で定義されています。

| 名前           | 説明 |
|----------------|------|
| プリミティブ   | コンパイラに組込まれている  |
| ビルトイン     | 定義は組み込みではないが、コンパイラに特別扱いされる |
| プレリュード   | 起動時に読み込まれるファイルで定義されている |

プリミティブとビルトインはあまり区別しないでいいかもしれません。

プレリュードとは何なのかについては日を改めて説明するとしましょう。
ここではプレリュードが何なのかについては触れずにそこで定義されている型を紹介するだけに留めます。

プミティブは冒頭で紹介したのでビルトインとプレリュードで定義された型を紹介します。

## ビルトイン

ビルトインで定義されているデータ型を紹介します。

### ユニット

意味のある値がないことを表わすときに使う型です。
Cなどにある `void` と似ていますが、ユニットは値を持ちます。

値も型も `()` で表わします。

``` idris
unit : ()
unit = ()
```

よく使う値なのでそのうち出てきます。

### タプル

値の組を表わす型です。
異なる型の値の組を保持できます。
リストなどと違って分割したり結合したりするものではないです。

`(値, 値, …, 値)` の構文で記述します。型も `(型, 型, …, 型)` と書きます。

例：タプルの値と型

``` idris
triple : (Int, String, Bool)
triple = (1, "string", True)
```



### その他

込み入った説明が必要になるので詳しくは解説しませんが、いくつか興味深いビルトインの型を紹介します。

* 依存ペア（`a: Type ** P a`）: 依存ペア。存在量化に相当する。
* `Void` ： 値がないことを表わす。矛盾に相当する。
* `Lazy a` ：遅延計算に使う

## プレリュード

プレリュードで定義されている型を紹介します。

### Bool

ブール型です。以下のように定義されています。


``` idris
||| Boolean Data Type
data Bool = False | True
```

Idrisはかなり色々な機能をもつのでブール値をライブラリで定義してしまえます。
例えば `if ~ then ~ else ~` やショートサーキット演算子 `&&` / `||` なんかもライブラリ定義です。
これはアドベントカレンダーのどこかで紹介できたらなと思います。

### Either

「どちらか」 を表わす型です。おおむね以下のように定義されています。

``` idris
||| A sum type
data Either a b =
  ||| One possibility of the sum, conventionally used to represent errors
  Left a |
  ||| The other possibility, conventionally used to represent success
  Right b
```

典型的にはエラーを表わすのに使います。

例： `Either` を使ってエラーを表わすコード

``` idris
divide: Int -> Int -> Either String Int
divide _ 0 = Left "division by zero"
divide m n = Right (m `div` n)
```

慣例的に、 `Left` がエラー、 `Right` が正しい値を表わします。
よく、 「`Right` がright（正しい）と覚えましょう」と言われてます。

### List

任意の型について、その型の値を任意個保持できるコレクションです。

おおむね以下のように定義されています。

``` idris
infixr 7 ::

||| Generic lists
data List elem =
  ||| Empty list
  Nil |
  ||| A non-empty list, consisting of a head element and the rest of
  ||| the list.
  (::) elem (List elem)
```

上の方で定義した `List` とほぼ同じですね。
ただし `Cons` の代わりに中置演算子 `::` が用いられています。

紹介し忘れましたがデータ型のコンストラクタは関数と同等に振る舞うので関数と同様に中置演算子として扱えます。
パターンマッチするときにも中置のままパターンマッチできます。
例をみてみましょう。

例： `List` 型のパターンマッチと構築を行うコード


``` idris
addAll : Int -> List Int -> List Int
addAll _ Nil     = Nil
addAll a (x::xs) = (a + x) :: (addAll a xs)
```

関数の引数で `Nil` と `::` へのパターンマッチを、 関数の本体で `Nil` と `::` の構築を行っています。

#### Listの構文

さて、 `List` にはいくつか特別な構文が用意されています。

`[1, 2, 3]` と書くと `1 :: 2 :: 3 :: Nil` と書いたのと同じことになります（`::` は右結合の演算子なので `1 :: 2 :: 3 :: Nil` は `1 :: (2 :: (3 :: Nil))` と解釈されます）。
これは値としてもパターンとしても使えます。

`[start..end]` と書くとstartからendまで（endを含む）の値のリスト返します。

例：1, 2, 3のリストの構築


``` idris
[1..3]
```

刻み幅を変更することもできます。

例：1から2つ飛ばしで10以下の値、1, 3, 5, 7, 9を含むリストの構築

``` idris
[1, 3..10]
```


また、PythonやHaskellにあるリスト内包表記もあります。
`[式 | 修飾]` の構文です。
修飾に書ける構文は `変数 <- リスト` をカンマ区切りで置けるなどがあります。
例えば九九を生成するリスト内包表記は以下です。

例：1×1〜9×9までの結果を生成するリスト内包表記

``` idris
[ i * j | i <- [1..9], j <- [1..9]]
```

あるいは、修飾の部分に条件式を書くことでその条件に合った値のみを集めることもできます。

例：2, 4, 6, 8, 10のリストを生成するリスト内包表記

``` idris
[n | n <- [1..10], n `mod` 2 == 0]
```

### Maybe

`Maybe` は「値があるか、あるいはない」を表わす型です。おおむね以下のように定義されています。

``` idris
||| An optional value. This can be used to represent the possibility of
||| failure, where a function may return a value, or not.
data Maybe a =
    ||| No value stored
    Nothing |
    ||| A value of type `a` is stored
    Just a
```

`Just x` のとき値があり、 `Nothing` のとき値がないことを表わします。
例えばリストの先頭を取り出す関数 `head'` は以下のように定義されています。

``` idris
head' : (l : List a) -> Maybe a
head' []      = Nothing
head' (x::xs) = Just x
```

`[]` だと先頭の値がないので `Nothing` を返しています。

失敗を表わすという意味では `Either` と似ています。
実際、最初に `Maybe` で書き始めた関数が失敗するケースが増えてきて `Either` に書き直すなどはよくあります。


### Nat

ちょっとここで紹介するか迷いましたが、よく出てくるので先に紹介しておきましょう。
`Nat` は自然数を表わす型です。0からはじまり無限に続きます。
以下のように定義されています。

``` idris
||| Natural numbers: unbounded, unsigned integers which can be pattern
||| matched.
data Nat =
  ||| Zero
  Z |
  ||| Successor
  S Nat
```

これのどこが自然数なんだと思うかもしれません。
これは[ペアノの公理](https://ja.wikipedia.org/wiki/ペアノの公理)に基く定義です。
ペアノの公理とは以下のような記述です。

> 自然数は次の5条件を満たす
> 
> 1. 自然数 0 が存在する。
> 2. 任意の自然数 a にはその後者 (successor)、suc(a) が存在する（suc(a) は a + 1 の "意味"）。
> 3. 0 はいかなる自然数の後者でもない（0 より前の自然数は存在しない）。
> 4. 異なる自然数は異なる後者を持つ：a ≠ b のとき suc(a) ≠ suc(b) となる。
> 5. 0 がある性質を満たし、a がある性質を満たせばその後者 suc(a) もその性質を満たすとき、すべての自然数はその性質を満たす。

このうち1. は `Z` 、 `2` は `S` が担当しています。
Idrisのデータ型では3.、4.は自動で満たされます。5はIdrisの型システムが担当します。

ということで上記 `Nat` は自然数とみなしてよさそうです。
`Nat` を直接使って数字を表わすのは以下のようにします。

例： `S` と `Z` を使って `Nat` の3を構築

``` idris
three : Nat
three = S (S (S Z))
```

`S` が `1 +` 相当なので `S` の数を数えれば何の数値か分かります。
ですがコンパイラの方で数値から `Nat` へ変換してくれるので以下のようにも書けます。

例：数値リテラルを使って`Nat` の3を構築

``` idris
three : Nat
three = 3
```

自然数はインデックスや長さとして使われたりします。
例えばリストの長さを求める関数 `length` は以下のように定義されています。

``` idris
length : List a -> Nat
length []      = Z
length (x::xs) = S (length xs)
```

# 型の型？

さて、以前Idrisは値を型の一部として扱えると書きましたが、型を値の一部としても扱えます。
すると関数や変数の型を書くときに「型の型」も必要になります。
「型の型」は `Type` です。

## 型エイリアス

「型とデータ型」というタイトルでありながら、いままで型エイリアスの紹介がありませんでしたね。
それもそのはず、Idrisには型エイリアスの機能がありません。
いや、あるんですが、「型エイリアス」という機能としては存在しません。

どういうことかというと、型も値として扱えるので、グローバル変数を束縛してしまえばそのまま型エイリアスとして機能するのです。

例：型を変数に束縛して型エイリアスとして使うコード

``` idris
IntList : Type
IntList = List Int
```

## 型アノテーション

いくつかの言語では式中で `1 : Int` のように式がどの型になるかを明示する機能があります。
しかしIdrisにはありません。
型エイリアスと同じく「型アノテーション」という機能としては存在しません。

型アノテーションは関数として書けてしまいます。
プレリュードで `the` という関数として定義されています。

``` idris
||| Manually assign a type to an expression.
||| @ a the type to assign
||| @ value the element to get the type
the : (a : Type) -> (value : a) -> a
the _ = id
```

例えば `the Nat 3` のように使えます。


# まとめ

Idrisのデータ型やレコードの構文を紹介しました。
関数の型についても解説し、1引数1返り値の関数型を組み合わせて複数引数の型のように扱っていることも紹介しました。
また、デフォルトで使えるデータ型をいくつか紹介し、データ型の利用例としました。


まだインターフェースや名前空間系の機能を紹介できてないのですが、次回は一旦手を動かす回にしようかなと思います。
