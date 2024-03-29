---
categories: [オブジェクト指向, 関数型]
date: 2018-03-14T22:22:40+09:00
description: "manabiya.techでの発表資料。オブジェクト指向と関数型についてのスピリチュアルな話"
title: "オブジェクト指向言語と関数型言語"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">

# はじめに
----------

* 椅子のキーストラップは是非お持ち帰り下さい
* 主にプログラミング言語を1つ覚えたくらいの人を対象にしています
* トークで40分全部使い切る予定なので質問はこのあとの職員室でお願いします
* たまに細かい話が出てきますがスルーして下さい
  + 主に重箱の角をつつく人への対策です

===

<h1>
オブジェクト指向言語  
と  
関数型言語
</h1>

----------------

[MANABIYA](https://manabiya.tech/) 2日目5時間目@ギャラリーB  
[#manabiya](https://twitter.com/search?src=typd&q=%23manabiya&lang=ja)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * 言語処理系を作るのが好き
 * 仕事での経験: Java, Scala, Rust
 * 趣味: C, Common Lisp, Standard ML, Rust

===

# 話すこと
--------

* オブジェクト指向/関数型"プログラミング"とはパラダイムのことだよ
* オブジェクト指向/関数型"言語"とはそのパラダイムを支援する言語のことだよ
* 言語とパラダイムの区別を明確に！

===

# 理想のソフトウェア
-------------------

* 変更に強いソフトウェア
* バグの少ないソフトウェア
* 凝集度を高めて結合度を低めたい
  + 似たようなものは同じところに
  + 互いの依存関係を減らす
* 理想のソフトウェアを作るには？

===

# パラダイム
-----------

* [Wikipedia](https://ja.wikipedia.org/wiki/%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E3%83%91%E3%83%A9%E3%83%80%E3%82%A4%E3%83%A0)
* プログラミングにおける思考のフレームワーク
  + 一貫性の取れた設計
  + 組み合わせたときの相性の良さ
  + 一度理解するとその後の学習コストが下がる
* 特定の言語に依存しない概念
* ある程度成功しやすい手法のパターン化

===

# 色々なパラダイム
------------------

* 手続き型 - プログラムとは機械の操作の記述だ
* オブジェクト指向 - プログラムとはオブジェクト間のメッセージのやりとりだ
* 関数型 - プログラムとは計算だ
* 論理型 - ...
* などなど

===

# 複数のパラダイムを知ろう<!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)"-->
----------------------------


[<img src="/images/manabiya/9_langs.png" alt="いま学ぶべき第二のプログラミング言語はコレだ！ 未来のために挑戦したい9つの言語とその理由" width="320px">](https://employment.en-japan.com/engineerhub/entry/2017/05/19/110000)<!-- .element: style="float:left;"  -->

> 「ハンマーしか持っていなかったら、なんでも釘に見える」という戒めがありますが、第二言語を学ぶことは、まさにハンマー以外の道具を持つことだといえます。

===
# オブジェクト指向プログラミング と 関数型プログラミング <!-- .element: style="font-size: calc(var(--title-font-size) * 0.7)"-->
----------

* 何故この２つのパラダイム？
  + → よく使われるパラダイム2つ
* 片方しか経験ない人はもう片方も学んでみよう
  + パラダイムが違うので最初は馴れない
  + コツは過去の成功体験を捨てること
    - パラダイムが違うと作法も違う
* ベタな手続き型プログラミングよりいいコードを書きたい

===
# 参考図書
----------

[<img src="http://image.gihyo.co.jp/assets/images/cover/2016/9784774183619.jpg" alt="オブジェクト指向設計実践ガイド" width="100%">](http://gihyo.jp/book/2016/978-4-7741-8361-9) <!-- .element: style="float:left;width:45%;"-->
[<img src="http://image.gihyo.co.jp/assets/images/cover/2016/9784774183909.jpg" alt="関数プログラミング実践入門" width="100%">](http://gihyo.jp/book/2016/978-4-7741-8390-9)     <!-- .element: style="float:right;width:45%;"-->

===

# OOPって？
----------------------
**オブジェクト** 同士の **メッセージング** によるプログラミング手法

* コード同士の依存関係を上手く管理したい
* 依存関係を上手く扱うことで変更に強いソフトウェアへ
  * [DDD](https://ja.wikipedia.org/wiki/%E3%83%89%E3%83%A1%E3%82%A4%E3%83%B3%E9%A7%86%E5%8B%95%E8%A8%AD%E8%A8%88)などの設計手法
* コードの分割
* コードの再利用

===

# FPって？
--------------------
**副作用** を出来るだけ使わないプログラミング手法[※](https://twitter.com/esumii/status/638591159518887936)

* 副作用 = 計算以外のもの
  + 破壊的変更、出入力など(深入りするとややこしい)
* 状態を排除→文脈に依存しないコードへ
  * 読みやすくなる
  * バグが少なくなる
* コードの分割と合成


===

# 何が違うの？
------------

<table style="width:100%">
<tr style="border-bottom: solid 3px #000"><th style="border-right: solid 3px #000"></th><th>OOP</th><th>FP</th></tr>
<tr><th style="border-right: solid 3px #000">状態</th><td>隠蔽</td><td>排除</td></tr>
<tr><th style="border-right: solid 3px #000">誰が</th><td>オブジェクト</td><td>関数</td></tr>
<tr><th style="border-right: solid 3px #000">対象</th><td>メッセージ</td><td>データ</td></tr>
<tr><th style="border-right: solid 3px #000">抽象</th><td>データ</td><td>処理</td></tr>
</table>


===
# 手続き的(自然言語)
-------------------------

入力: `array` - 配列,  `n` - 配列の長さ  
出力: `array`の要素の合計

1. `sum = 0`, `i=0` とする
2. もし`i` が`n`未満なら4へ飛ぶ
3. 7へ飛ぶ
4. `sum`に`array`の`i`番目を足したものを`sum`に代入
5. `i`をインクリメント
6. 2へ飛ぶ
7. `sum`を返す

===

# 手続き的(C言語)
----------------

``` c
int
procedual_sum(const int array[], size_t n)
{
  int sum = 0;

  for(size_t i = 0; i < n; i++) {
    sum += array[i];
  }

  return sum;
}
```
===

# OOP的発想
----------

* データの中身を列挙するオブジェクト(イテレータ)を用意しよう
  + イテレータはデータにメッセージを送って取得しよう
* イテレータにメッセージを送って要素を取得しよう
* イテレータがあればデータの実装に依存しなくなるな

===
# イメージ

<img src="/images/manabiya/object.png" width="100%" height="100%">


===

# OOPコード例(C言語)
-------------------

``` c
struct iterable {
  struct iter *(*iter)(const struct iterable *);
  void (*fin)(struct iterable *);
};

struct iter {
  int (*next)(struct iter *);
  bool (*has_next)(const struct iter *);
  void (*fin)(struct iter *);
};

int
objective_sum(const struct iterable *data)
{
  int sum = 0;
  struct iter *iter = data->iter(data);
  while (iter->has_next(iter)) {
    sum += iter->next(iter);
  }
  iter->fin(iter);

  return sum;
}
```

===
# 実装イメージ

<img src="/images/manabiya/object_impl.png" width="100%" height="100%">


===
# OOPコード例(C言語) 実装<!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)"-->
-------------------

``` c++
struct array_list {
  struct iterable super;
  int (*get)(const struct array_list *, size_t i);
  size_t (*len)(const struct array_list *);
  int *inner;
  size_t n;
};


struct array_list_iter {
  struct iter super;
  const struct array_list *array;
  size_t i;
};

struct iter *array_list_iter(const struct iterable *);

void array_list_fin(struct iterable *);
int array_list_get(const struct array_list *, size_t);
size_t array_list_len(const struct array_list *);

struct array_list_iter *array_list_iter_new(const struct array_list *);
void array_list_iter_fin(struct iter *);
int array_list_iter_next(struct iter *);
bool array_list_iter_has_next(const struct iter *);


struct array_list *
array_list_new(int *inner, size_t n)
{
  struct array_list *array = (struct array_list *)malloc(sizeof(struct array_list));
  if (! array) {
    return array;
  }

  array->super.iter = array_list_iter;
  array->super.fin = array_list_fin;
  array->get = array_list_get;
  array->len = array_list_len;
  array->inner = inner;
  array->n = n;

  return array;
}

void
array_list_fin(struct iterable *super)
{
  struct array_list *self = (struct array_list *) super;
  free(self);
}

struct iter *
array_list_iter(const struct iterable *super)
{
  struct array_list *self = (struct array_list *) super;

  return (struct iter *)array_list_iter_new(self);
}

int
array_list_get(const struct array_list *self, size_t i)
{
  return self->inner[i];
}

size_t
array_list_len(const struct array_list *self)
{
  return self->n;
}


struct array_list_iter *
array_list_iter_new(const struct array_list *array)
{
  struct array_list_iter *iter = malloc(sizeof(struct array_list_iter));
  if (! iter) {
    return iter;
  }

  iter->array = array;
  iter->i = 0;
  iter->super.fin = array_list_iter_fin;
  iter->super.next = array_list_iter_next;
  iter->super.has_next = array_list_iter_has_next;

  return iter;

}

void
array_list_iter_fin(struct iter *super)
{
  struct array_list_iter *self = (struct array_list_iter *)super;
  free(self);
}

int
array_list_iter_next(struct iter *super)
{
  struct array_list_iter *self = (struct array_list_iter *)super;
  int ret = self->array->get(self->array, self->i);

  self->i++;

  return ret;
}
```

===
# OOPコードの特徴
-----

* オブジェクトにメッセージを送ってループを書いた
  + オブジェクト = `iter`
  + メッセージ = `has_next`、`next`
* インターフェースと実装を分離してコードを書いた
  + インターフェース = `iterable`、`iter`
  + 実装 = `array_list`、`array_list_iter`
* データの中身を知らなくてもコードを書けた
  + 木構造や辞書などにも適用できる
* 具体的実装がなくてもコードを書けた
  + コードの分割ができる

===

# FP的発想
----------

* 配列の中身の合計を求める式を立てよう
* 計算を一般化して汎用性をあげよう
* それをプログラムとして書き下そう

===
# FP的記述
----------

\\\[
\begin{align}
S\_0 &= 0 \\\\
S\_n &= S\_{n-1} + arr[n - 1]
\end{align}
\\\]


===
# FP的記述
----------

\\\[
\begin{align}
S\_0 &= init \\\\
S\_n &= f(S\_{n-1}, arr[n - 1])
\end{align}
\\\]

===
# FPコード例(C言語)
-------------------

``` c++
int
reduce(const int array[], const size_t n, const int init, int(*f)(const int, const int))
{
  if (n == 0) {
    return init;
  } else {
    return f(reduce(array, n - 1, init, f), array[n - 1]);
  }
}


int
add(const int x, const int y)
{
  return x + y;
}

int
functional_sum(const int array[], const size_t n)
{
  return reduce(array, n, 0, add);
}
```

===

# FP的コードの特徴
----------------

* ループと中身に分解してコードを書いた
  + ループ = `for文` → `reduce`
  + 中身 = `sum += array[i]` → `add`
  + 制御構造を関数にできた
* 副作用(変数の更新)を行わずにコードを書いた
* 宣言的になった

===
# OOPコードの問題点
----------------
* メッセージパッシングの書き方が冗長
  ```
  obj->msg(obj)
  ```
* 普通のコードより遅そう
  + 毎回関数ポインタ経由でメッセージ
  + ことある毎にオブジェクトを作る
    - 今回は余計にイテレータオブジェクトを作った
* `int`と`+`はオブジェクトとメッセージになってない
  + 設計の一貫性がとれてない
* メッセージ増やすとデータサイズが増えそう

===
# FPコードの問題点
---------------

* 余計な関数定義が増える
  + 足し算するための`add`関数を定義した
* データに依存したコードになっている
  + 他のデータ型に対して適用できない
* 副作用を使わない
  + 機械の操作とは大分違う
* 一般には毎回データのコピーが発生する
  + 今回の例では運良く`int`しかコピーしなかった

===

# 問題の解決案
--------------
言語による<!-- .element: class="fragment" data-fragment-index="1" -->

* 対象にしているものが広すぎる<!-- .element: class="fragment" data-fragment-index="2" -->
* 具体的な言語抜きに語っても意味がない<!-- .element: class="fragment" data-fragment-index="2" -->

===
そのまえに

# XXX言語とは
------------

[関数型プログラミングの今昔](https://www.slideshare.net/ksknac/120901fp-key)
* オブジェクト指向(プログラミングを支援する)言語
* 関数型(プログラミングを支援する)言語
* マルチパラダイム言語もある
  + 複数のプログラミングパラダイムを支援
  + それらを混ぜて使うことも

===
# OOP言語色々
---------------------
* メソッド呼び出し構文があればOOPを支援(?)
  + `obj->msg(obj)` → `obj.msg()`
* クラスベース
  + Ruby Java C# Python C++ ...
  + 単一継承/多重継承の違いも
* プロトタイプベース
  + Smalltalk JS ...
* その他
  + go rust ...


===

# クラスベースの特徴
---------------------
* メッセージはクラスが知っている
  + メッセージを増やしてもオブジェクトは肥大化しない
* クラス継承によるインターフェースと実装の再利用
  + ある意味では親と子の密結合
* リスコフの置換則
  + 親クラスはいつでもサブクラスに置き換えられるべき
* 差分プログラミングをするとスパゲッティコードになる

> 「オブジェクトの階層構造をコストとして払う代わりに、メッセージの移譲は無料で手に入れられる」

===

<img src="/images/manabiya/class_method.png" width="100%" height="100%">

===
抽象の境界と差分プログラミングとスパゲッティコード

<img src="/images/manabiya/abstract.png" width="100%" height="100%">

===
抽象の境界と差分プログラミングとスパゲッティコード

<img src="/images/manabiya/bad_abstract.png" width="100%" height="100%">


===
# Javaの特徴
------
* クラスベース単一継承
* 抽象クラスやインターフェースによる抽象化
* プリミティブ型はオブジェクトじゃない
* 遅くならない工夫
  + → 実行しながら高速化
  + → メモリ管理の改善
* 割とクラスの機能が強い
  + クラスが名前空間も兼任
  + スタンドアロンな関数が書けない（かった）
  + コールバックには無名クラスとか

===
# Javaのコード例
------

``` java
abstract class Figure {
  void draw() {}
  abstract void move(int dx, int dy);
}

class Triangle extends Figure {
  Point a;
  Point b;
  Point c;

  @Override
  void draw() {
    drawLine(a, b);
    drawLine(b, c);
    drawLine(c, a);
  }

  @Override
  void move(int dx, int dy) {
    a.move(dx, dy);
    b.move(dx, dy);
    c.move(dx, dy);
  }

  void drawLine(Point from, Point to) {}

  class Point {
    int x;
    int y;

    void move(int dx, int dy) {
      x += dx;
      y += dy;
    }
  }
}
```

===
# Javaのコード例について<!-- .element: style="font-size: calc(var(--title-font-size) * 0.9)"-->
------

* 設計は難しい
* `drawLine` はだれが持つべき？
  + `drawLine` ってTriangleだけのものじゃないよね
  + 本来は `new Line().draw()` では？
  + でも毎回オブジェクト作るの？
* `ColoredTriangle` を作ろうとしたらどうする？
  + `drawLine` をオーバーライドする？
  + `new ColoredLine` にする？

===
# Rubyの特徴
-------

* クラスベース単一継承
* 生産性を重視した設計
* 数値や`+`などもオブジェクト/メソッド
* ダックタイピング
  + メッセージに応答すればなんでもいい
* クラスだけでなくモジュールも
  * mix-in
* クラスの権限がそんなに強くない
  + オープンクラス

===

# Rubyらしさ(主観)
-----------
* for文なしでの繰り返し
  + ブロック構文

``` ruby
(1..10).each{|i| puts i}
```

* [ActiveSupport](https://railsguides.jp/active_support_core_extensions.html#time)による数値の拡張など
  + オープンクラス 数値もオブジェクト `+`もメソッド

```ruby
1.week - 2.days
```

===
# Goの特徴
----------

* メソッド呼び出し構文がある
* クラスや継承はない
  + 代わりにインターフェースとインクルードがある

===

# 関数型言語色々
---------------
* ML系
 + SML
 + OCaml
 + F#
* Haskell系
 + Haskell (GHC)
 + Agda
 + Idris
* Erlang
* Lisp系
  + Clojure

<!-- .slide: style="font-size:calc(var(--base-font-size) * 0.9)" -->

===
# ありがちな機能
---------------
* 破壊的変更できないorあまりしない
* 関数の便利な扱い
  + 無名関数
  + 演算子も関数
  + 関数合成
  + 高階関数
  + カリー化(関数を返す関数)
* ※「関数型 = Haskell」はHaskellプログラマの麻疹

===
# 便利な関数の扱い
-----------------
* 高階関数 演算子も関数 関数合成

``` sml
val sum = List.foldl op+ 0;
```

``` standard-ml
val inner_product = List.foldl op+ 0 o List.map op* o ListPair.zip;
inner_product ([1, 2, 3], [1, 2, 3]); (* => 14 *)
```

* カリー化

``` standard-ml
fun findManabiya list = List.find (String.isPrefix "manabiya") list
val findManabiya = List.find (String.isPrefix "manabiya")
```

* 無名関数

``` standard-ml
String.tokens (fn c => c = #" " orelse c = #"\n")
```
===
# データコピーの話
------------------

* リストを2回コピーしてるけど遅くない？

``` standard-ml
fun inner_product l1 = let
  val l2 = ListPair.zip l1
  val l3 = List.map op* l2
in
  List.foldl op+ 0 l3
end
```

* もうちょっと一般に世の中のアルゴリズムを実装すると遅くない？

<!-- .slide: style="font-size:calc(var(--base-font-size) * 0.9)" -->

===

# データコピーの話
------------------

* リストを2回コピーしてるけど遅くない？
  + 言語による
  + [1倍〜100倍遅い](https://gist.github.com/KeenS/35345a4661dc696f467abd2de830568d)
    - 関数型言語に向いたGCアルゴリズムの採用
    - 最適化で消せる
* もうちょっと一般に世の中のアルゴリズムを実装すると遅くない？
  + A1. 遅い部分は諦めて副作用を使う
  + A2. 関数型向きデータ構造/アルゴリズムを使う
     - [純粋関数型データ構造](http://asciidwango.jp/post/160831986220/%E7%B4%94%E7%B2%8B%E9%96%A2%E6%95%B0%E5%9E%8B%E3%83%87%E3%83%BC%E3%82%BF%E6%A7%8B%E9%80%A0)
     - [関数プログラミング 珠玉のアルゴリズムデザイン](http://shop.ohmsha.co.jp/shopdetail/000000004066/)

<!-- .slide: style="font-size:calc(var(--base-font-size) * 0.9)" -->

===

# Clojureの特徴
---------------

* デフォルトイミュータブルなLisp方言
* イミュータブルHashMap/Set
  + イミュータブルだけどデータを全部コピーする訳ではない
  + [HAMT](https://en.wikipedia.org/wiki/Hash_array_mapped_trie)

``` clojure
(assoc {:name "κeen"} :age 25)
  ; ->{:age 25, :name "κeen"}
```

* 並列プログラミングに強い
  + データ競合が起きない

===

# SMLの特徴
------
* 強い静的型付
* 普通に破壊的変更あるよ
* モジュールによるカプセル化
* ファンクタによる依存の注入

===

# SMLのコード例
------
* モジュールによるカプセル化
  + データに対する操作を一箇所に集めるのは変わらない

``` standard-ml
structure MyList: sig
              type t
              val len: t -> int
              val get: t -> int -> int
          end = struct
    type t = int list
    val len = List.length
    fun get (x::xs) 0 = x
      | get (x::xs) n = get xs (n-1)
end

```

===
# SMLのコード例
------
* ファンクタによる依存の注入

``` standard-ml
functor Make(Foldable: sig
                 type 'a t
                 val fold: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
             end) = struct
    val sum = Foldable.fold op+ 0
end
```

===
# Haskell(GHC)の特徴
----------
* 強い静的型付け
* 強力な型システム
* 型クラスによるデータ抽象
* 純粋
  + 破壊的変更とIOを基本許さない
  + 全て式になる
    - 雑にいうとセミコロンなしでプログラミングする
  + 入力からのみ出力が決まる → 型をみたら関数の使い方が大体分かる
* 遅延評価
  + 必要になるまで値を計算しない
    - 評価の順番も変わる
  + 純粋なのでプログラムの結果は変わらない
    - (細かいことを言うと無限ループの挙動が違うけど)

<!-- .slide: style="font-size:calc(var(--base-font-size) * 0.8)" -->

===
# Haskell(GHC)のコード例<!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)"-->
----------

* 型クラスによるデータ抽象

``` haskell
{-# LANGUAGE NamedFieldPuns #-}

class Drawable a where
  draw :: a -> ()

class Movable a where
  move :: a -> (Int, Int) -> a


data Point = Point Int Int
  deriving Show

instance Movable Point where
  move (Point x y) (dx, dy) = Point (x + dx) (y + dy)

data Triangle = Triangle {
  a:: Point,
  b:: Point,
  c:: Point
}
  deriving Show

instance Drawable Triangle where
  draw _ = ()

instance Movable Triangle where
  move Triangle{a, b, c} d = Triangle {
    a = move a d,
    b = move b d,
    c = move c d
    }
```

===

# Haskell(GHC)と逐次処理<!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)"-->
----------------
> 雑にいうとセミコロンなしでプログラミングする

* 逐次処理はどうするの？(e.g. 1行読んでそれを出力)
  1. プログラムを値として扱って合成する
    ```haskell
    Program1 ○ Program2 -> Program2'
    ```
  2. 直前の値も受け取れるようにする
    ```haskell
    Program1 ○ (a -> Program2 ) -> Program2'
    ```
  3. 色々な種類のプログラムに対応可能
    ```haskell
    Program a ○ (a -> Program b) -> Program b
    ```
  4. 具体的には`>>=`という演算子で合成
     ```haskell
     getLine >>= putStrLn
     ```
  5. シンタックスシュガー
    ```haskell
    do
      s <- getLine
      putStrLn s
    ```

<!-- .slide: style="font-size:calc(var(--base-font-size) * 0.8)" -->

===

# Haskell(GHC)の遅延評価<!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)"-->
----------
* 遅延評価
  + 同等のCのコードよりずっと速い
  + 計算量が変わる
  + `tarai(12, 6, 0)`で2,604,860回 vs 110回

``` haskell
tarai:: Int -> Int -> Int -> Int
tarai x y z = if x <= y
              then y
              else tarai (tarai (x-1) y z) (tarai (y-1) z x) (tarai (z-1) x y)
```

``` c
int
tarai(int x, int y, int z)
{
  if (x <= y) {
    return y;
  } else {
    return tarai(
                 tarai(x - 1, y, z),
                 tarai(y - 1, z, x),
                 tarai(z - 1, x, y)
                 );
  }
}
```


<!-- .slide: style="font-size:calc(var(--base-font-size) * 0.7)" -->

===

# 関数型言語のOOP
-----------------
* OCamlのO
* SMLのモジュールは割とOOPに似てる？
* `|>` は割とメソッドチェーンに似てる？

``` elixir
1..999
 |> Enum.filter(&(rem(&1, 3) == 0 || rem(&1, 5) == 0))
 |> Enum.sum
 |> IO.puts
```

===
# オブジェクト指向言語のFP<!-- .element: style="font-size: calc(var(--title-font-size) * 0.8)"-->
-------------------------

* 高階関数
  + Rubyのブロックも
* JavaのStream API
  + [FP in Java](https://www.amazon.co.jp/dp/1937785467)

===
# 結局どういう関係なの？<!-- .element: style="font-size: calc(var(--title-font-size) * 0.9)"-->
----------------------

* 大きな部分では変わらない
  + 関心毎にコードを集めて粗結合な部品を組み立てる
* オブジェクト指向は設計より
* 関数型はコーディングより
* 完全に相反するものでもない
  + マルチパラダイム言語
* 相性の悪い点もある

===
# プログラミング言語のこれから<!-- .element: style="font-size: calc(var(--title-font-size) * 0.75)"-->
--------------------------

* 今回挙げた言語はかなり古い言語
  + Ruby, Java, Haskell, SMLは20年以上前に出来た
* 古い言語は当時技術を元に設計される
  + ハードウェア
  + コンパイル技法
  + ベストプラクティス
* これからは新しい概念、いいとこ取りの言語設計も出てくる？
  + 並列並行サポート
  + 代数的データ型とパターンマッチ、無名関数
  + 所有権
  + などなど

===
# まとめ
--------

* オブジェクト指向/関数型プログラミングとはパラダイムのことだよ
* オブジェクト指向/関数型言語とはそのパラダイムを支援する言語のことだよ
  + 言語とパラダイムの区別を明確に！
* それぞれ目的もアプローチも違うよ
* 両方手札に持って使い分けようね
* これ以外にも新しい言語にも注目


</textarea>


