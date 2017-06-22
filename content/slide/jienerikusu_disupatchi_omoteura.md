---
categories: [型, ジェネリクス, Rust, 言語実装]
date: 2017-06-17T22:25:35+09:00
description: null
title: ジェネリクス ディスパッチ 表裏
---

<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# ジェネリクス ディスパッチ 表裏
----------------------
[ジェネリクス勉強会 - connpass](https://connpass.com/event/56773/?utm_campaign=event_participate_to_owner&utm_source=notifications&utm_medium=email&utm_content=title_link)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# はじめに
-----------

* 例にはJavaとRustを使います
  + それ以外にあまり詳しくない
  + たまにScalaが出てくるかも
* 主に裏側で何が起きてるかに焦点を当てます
* 一般的な手法の比較と言語固有実装の比較がやや混じります
  + 実例重視
* 言語や機能を選ぶときの知識が増えればよし

===
# ジェネリクス
--------------

* 型でパラメータ化された何か
* パラメータ化する方法にいくつか
  + 任意の型に対応する実装にする
    - Java
  + 型毎に実体をつくる
    - 型が引数になるイメージ
    - Rust
* 便宜上前者をポインタ型、後者をテンプレート型と呼ぶ

===
# 関数ジェネリクス
-----------------

* Javaコード


``` java
public class Generics {
    public static void main(String[] args) {
        Generics g = new Generics();
        System.out.println(g.id("hello"));
    }

    <T> T id(T t) {
        return t;
    }
}
```

===

# 関数ジェネリクス
-----------------

* Javaアセンブリ

```
public class Generics {
  // ...
  // Tの中身に言及していない
  <T> T id(T);
    Code:
       0: aload_1
       1: areturn

```

===

# 関数ジェネリクス
-----------------

* Rustコード

``` rust
fn id<T>(t: T) -> T {
    t
}


fn main() {
    println!("{}", id("hello"));
}
```

===

# 関数ジェネリクス
-----------------

* Rustアセンブリ

``` llvm
define internal %str_slice @_ZN3tmp2id17hfe175cfdb5be0f46E(i8* noalias nonnull readonly, i64) unnamed_addr #0 {
start:
  ; str_sliceに特化した関数を生成している
  %2 = insertvalue %str_slice undef, i8* %0, 0
  %3 = insertvalue %str_slice %2, i64 %1, 1
  ret %str_slice %3
}
```

===

# 関数ジェネリクス
-----------------

* ポインタ型は`void *`使ってるイメージ
  +  コンパイルされたコードがコンパクト
  + 必ずポインタ経由する
    + Javaのオブジェクトは参照になってるのであまり問題ない
* テンプレート型は型毎に`id_XXX`関数を定義してるイメージ
  + 構造体の値渡しも可能
  + オブジェクトコードは大きくなる
  + 関数の使用箇所が分からないとコンパイルできない

===

# 返り値ジェネリクス
-------------------

* Rustは返り値のジェネリクスがある
  - コンテキストで返り値が決まる
  ```rust
  // str
  parse<F>(&self) -> Result<F, <F as FromStr>::Err> where
    F: FromStr;
  ```
  ```rust
  let addr: SocketAddr = "127.0.0.1:8080".parse().unwrap();
  ```
* キモい


===

# データ型ジェネリクス
---------------------

* `ArrayList<T>`(Java) vs `Vec<T>`(Rust)
* だいたい関数のときと似たような特徴
* テンプレート型はサイズやアラインメントまで考慮できる
  - `Vec<u8>`(バイト列型)が効率的
* Rustの場合は構造体定義はオブジェクトコードに出ないのでサイズは気にならない

===

# 制約とディスパッチ
-------------------

* パラメータ型に条件をつけたい
  + Javaならインターフェースで `T extends SomeInterface` とか
  + Rustならトレイトで `T: SomeTrait` とか
* さらにパラメータ型の詳細にアクセスしたい
  + `t.someMethod()` とか
* このメソッドってどっからやってくるの？

===

``` java
<W extends Writer> void writeHello(W w) throws IOException {
    // このwriteメソッドはどこから？
    w.write("Hello");
}
```

``` rust
fn write_hello<W: io::Write>(mut w: W) -> io::Result<()> {
    // このwrite_allメソッドはどこから？
    w.write_all(b"Hello")
}
```

===

# 動的ディスパッチ
-------------------
* Javaだとオブジェクトに紐付くメソッドテーブルを *実行時* に引く
  + テーブルもオブジェクトから *実行時* に取得する
   ``` java
   w.vtable[write](w, "Hello");
   ```
* ダイナミックなことができる
  + サブクラスのインスタンスと入り交じっても問題無
    ```java
    Writer w = new MyWriter();
    obj.writeHello(w);
    ```
* vtable引くオーバーヘッドがかかる
[java - Virtual table/dispatch table - Stack Overflow](https://stackoverflow.com/questions/6606481/virtual-table-dispatch-table)
[jvm - Java method table - Stack Overflow](https://stackoverflow.com/questions/10225668/java-method-table)


===

# 静的ディスパッチ
-----------------
* Rustだとメソッドを *コンパイル時* に解決する
  + テーブルを静的に解決するだけじゃなくてテーブルのメソッドまで解決
  ```rust
   SomeWrite::write(w, b"Hello")
  ```
  + 辞書オブジェクトはみんなの心の中にあるんだよ
    - (テーブルだけ静的に解決する方式もある)
* 速い
  + オーバーヘッドがない
  + メソッドのインライン化などの最適化ができる

===
# 動静まとめ
------------

* ポインタ型で動的ディスパッチだとコンパクトだけど遅い
* テンプレート型で静的ディスパッチだと速いけど嵩む
* 特徴は表裏な感じ


===
# Java固有の問題と解決
----------------------

* ジェネリクスとプリミティブ問題
* → ヴァルハラ
* → Scalaのspecialized
* → DottyのLinker

===

# ジェネリクスとプリミティブ問題
--------------------------------

* ジェネリクスは必ずポインタ経由
  + プリミティブはどうするの？
* 一旦オブジェクトに包む(ボクシングする)必要がある
  + 関数もデータ型も同じ問題
  + 関数は暗黙の変換があるので気付きづらい
* 割と深刻なパフォーマンス低下を招くこともある


===

``` java
public class Generics {
    public static void main(String[] args) {
        Generics g = new Generics();
        // プリミティブを渡す
        System.out.println(g.id(0));
    }

    <T> T id(T t) {
        return t;
    }
}

```


===

```
public class Generics {
  public static void main(java.lang.String[]);
   // ..
    Code:
      // ..
      // ここで一旦`Integer`を作る
      13: invokestatic  #5                  // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
      16: invokevirtual #6                  // Method id:(Ljava/lang/Object;)Ljava/lang/Object;
      19: invokevirtual #7                  // Method java/io/PrintStream.println:(Ljava/lang/Object;)V
      22: return

  <T> T id(T);
    Code:
       0: aload_1
       1: areturn

}

```

===

# ヴァルハラ
------------

* [OpenJDK: Valhalla](http://openjdk.java.net/projects/valhalla/)
  + [JEP 169: Value Objects](http://openjdk.java.net/jeps/169)
  + [JEP 218: Generics over Primitive Types](http://openjdk.java.net/jeps/218) ← こっち
* プリミティブタイプもボクシングせずにジェネリクスに使える
* やったね
* いつ入るんだろうね。
* [JDK 9](http://openjdk.java.net/projects/jdk9/) にはまだっぽい？

===

# [scala.specialized](http://www.scala-lang.org/api/2.9.2/scala/specialized.html)
-------------

* ジェネリクスの他にプリミティブ毎に専用のメソッドを生成
* 半分テンプレート型みたい
  ``` scala
  class MyList[@specialized T]  ...
  ```
* 実際には使われない型に対しても生成してしまう
  + → テンプレート型よりも非効率
  + 型パラメータが3つあったら1000メソッドくらい出来てしまう
* > 関数の使用箇所が分からないとコンパイルできない

  + JVMの分割コンパイル下では何が使われるか事前に分からない

===

# DottyのLinker
---------------

* > 関数の使用箇所が分からないとコンパイルできない
* リンクフェーズを用意すれば使用箇所が全て分かる
* [Dotty Linker: Making your Scala applications smaller and faster](https://d-d.me/talks/scaladays2015/#/)
  + スライドを見る限りspecializeしといてDCE?
* 実際には生のバイトコードじゃ情報が足りないのでTASTYも付加
* Dottyはいつ使えるようになるんでしょうね。

===

# Rust固有の問題と解決
---------------------

* トレイト境界とヘテロな型の問題
  * ヘテロなVec
  * 分岐からのreturn
* → トレイトオブジェクト

===
# トレイト境界とヘテロな型の問題
-------------------------------

* ジェネリックデータ型に2つ以上の異なる型を入れられない
* つまり以下のようなコードが書けない
  ``` rust
  trait Processor {}
  let procs: Vec<Processor> = vec![
    TwProc::new(),
    FbProc::new(),
  ];
  ```
* トレイトは実際の型ではないので同じ振舞をしても共通の型として扱えない
* でも一緒に扱いたいケースがあるんだけど？？

===
# トレイト境界とヘテロな型の問題2
-------------------------------

* 分岐して型を出し分けるのも出来ない
  ```rust
  fn getProc() -> Processor {
    if xxx {
      TwProc::new()
    } else {
      FbProc::new()
    }
  }
  ```

===

# トレイトオブジェクト
---------------------

* オプトインで動的ディスパッチする仕組み
* データとトレイトからvtableを作る
* `Box`や`&`などポインタ型を通すと使える

===

``` rust
let procs: Vec<Box<Processor>> = vec![
  Box::new(TwProc::new()),
  Box::new(FbProc::new()),
];

```

``` rust

fn getProc() -> Box<Processor> {
  if xxx {
    Box::new(TwProc::new())
  } else {
    Box::new(FbProc::new())
  }
}
```
===

裏

===

# ジェネリクスの双対
-------------------

* ジェネリクスは$\forall$の量化
  + 関数$T \to S$に対して${}^\forall x(T \to S)$
* $\exists$の量化があってもよくない？
  + 存在型と呼ばれる
  + 因みに${}^\exists x(T \to S)$は虚無

===

# 存在型
---------

* 「`P`を満たす`T`が存在する」ことを表わす型
* `P`とは？
  + 本来は型を引数にとる述語。
  + 実際は型の集合の方が便利
    - CF: $P(x) \equiv x \in \{x| {}^\forall x, P(x) \}$
  + Scala: `forSome`に続く何か
    - よく分からなかった。構造的superset?
  + Rust: トレイト境界
* 実際に使うには`P`を満たす`T`を1つ与える
* 実際の型を変数`T`に匿名化してるとも見れる
  - Rustなら「トレイト`Tr`を実装しているとある型`T`」
===

# 引数の存在型
---------------

* Scalaの`forSome`
  ```scala
  def len(l: List[T] forSome {type T}): Int
  ```
* Rustのarg position `impl Trait`
  ```rust
  fn len(i: impl IntoIterator) -> usize
  ```
* `R`が`x`を含まないなら
  \\\[{}^\forall x(A(x) \to R) \leftrightarrow {}^\exists x A(x) \to R \\\]
  なのでほぼジェネリクス
* 違い
  + 関数が型パラメータを持たなくなる
  + 量化した場所以外（他の引数や返り値）でその型変数を使えない

===
# 返り値の存在型
----------------

* Rustの`impl Trait`
  ```rust
  fn do_later() -> impl Future<Item = (), Err = Error>
  ```
* 実際の型は関数の定義で決まっている
* 実質返り値を匿名化しているだけ
* 必要？

===
# 存在型の利点
--------------

* 何をしたいか伝わりやすい
  ```rust
  // Iterator
  map<B, F>(self, f: F) -> Map<Self, F>;
  ```
  vs
  ```rust
  map<B, F>(self, f: F) -> impl Iterator;
  ```
* サブタイピングがあればアップキャストで終わる
* Rustでも動的ディスパッチを許せばトレイトオブジェクトがある
  + でも絶対動的ディスパッチをしたくない

===
# 存在型の必要性
----------------

* 存在型でないと書けない型が存在する
  + 匿名化した型を含む型
  + クロージャ、お前のことだ
* 以下の型はジェネリクスでは書けない

``` rust
fn counter(x: isize) -> impl FnMut() -> isize;
```

* 動的ディスパッチを許せばトレイトオブジェクトで書ける
  + でも絶対動的ディスパッチをしたくない
  + 極端な話、関数抽象する度にパフォーマンスが落ちる

===

# ユースケース
--------------

* 実際そんなに必要なの？
* 極一部のケースだけじゃないの？
* 分岐したら結局トレイトオブジェクト必要なんじゃなかった？
* 動的ディスパッチで妥協できないの？

===

# `Iterator`
------------

* 標準ライブラリ
* 遅延評価
  * 融合変換するため
* `map`すると元のイテレータとmapする関数の組が返る
  ```rust
  // Iterator
  map<B, F>(self, f: F) -> Map<Self, F>;
  ```
* `impl Trait`で書くと分かりやすい
  ```rust
  map<B, F>(self, f: F) -> impl Iterator;
  ```

===

# [futures-rs](https://github.com/alexcrichton/futures-rs)
-----------------

* 非同期処理を抽象化
* 実行時にはステートマシンになる
  + 動的ディスパッチを挟まない
* `Future`に`map`や`and_then`すると返り値に関数型が出てくる
  + 存在型がないと書けない
  + 引数でクロージャを受け取る訳ではないのでジェネリクスで書けない
* `Future`を使うほぼ全てのコードで存在型が必要になる

``` rust
fn do_later() -> impl Future<Item = (), Err = Error> {
    do_something()
        // ここでクロージャが出てきた
        .and_then(|()| do_another_thing())
        // 本来の`and_then`の返り値は
        // `AndThen<Self, B, F>`だが
        // `F`の型が匿名化されていて書けない
}
```

===
# [transaction-rs](https://github.com/KeenS/transaction-rs)
------------------

* トランザクションを抽象化
* コンセプト的には`futures-rs`に似てる
* 分岐しても`branch` APIでトレイトオブジェクト回避
  + 直和型を信じろ
  ```rust
  match find_user() {
    None => ok(none).branch().first()
    Some(user) =>
      delete_user()
        .map(move|_| user)
        .branch()
        .second()
  }
  ```
* [マイクロベンチマーク](https://github.com/KeenS/transaction-rs/blob/master/transaction-stm/benches/boxed_vs_branch.rs)だとトレイトオブジェクトをなくすと *13%* 高速化

===
# 存在型の深掘
--------------

* ユーザには匿名化された型の実体は分からない
* コンパイラは実際の型で扱う
* 色々エッジケースが出てきそう
  + 同じ関数から返る型は同じ型？
  + 違う関数でも実体が同じなら？
  + 関数がジェネリクスだったら？
    - 返り値もジェネリクスパターン
    - 引数だけジェネリクスパターン
  + トレイトのメソッドだったら？
* 例えば`vec![foo(), bar()]`って書けるの？

===

``` rust
fn foo<T: Trait>(t: T) -> impl Trait {
    t
}

fn bar() -> impl Trait {
    123
}

fn equal_type<T>(a: T, b: T) {}

equal_type(bar(), bar());                      // OK
equal_type(foo::<i32>(0), foo::<i32>(0));      // OK
equal_type(bar(), foo::<i32>(0));              // ERROR, `impl Trait {bar}` is not the same type as `impl Trait {foo<i32>}`
equal_type(foo::<bool>(false), foo::<i32>(0)); // ERROR, `impl Trait {foo<bool>}` is not the same type as `impl Trait {foo<i32>}`
// トレイトのメソッドには`impl Trait`は書けないらしい
```

===
# 話さなかったこと
-----------------

* 存在型のライフタイム
* Rustのfeatureとリリーススケジュール
* 関連型と存在型の関係(なんか関係ありそう)

===

# まとめ
--------

* 総称を表わすジェネリクスというのがあるよ
* ジェネリクスの実装は2種類あるよ
* 存在を表わす存在型というのがあるよ
* 存在型の実装は2種類あるよ
* 2種類の実装は言語機能や型システムに密着してるよ


* 主にRustの話
* 型を匿名化したい `Map`型などの例
* 匿名型が書けない
  + futures
  + transaction
* 「トレイトを実装している何かの型」


関数ジェネリクス(Java)、テンプレート(Rust)
返り血ジェネリクスの話（外部の文脈によって値が決まる）
構造体ジェネリクス(Java)、テンプレート(Rust)
サブクラス/トレイト制約と動的ディスパッチ、静的ディスパッチ
void * とプリミティブ
静的ディスパッチと最適化、インライン、特殊化などなど
ジェネリクスの裏、存在型
  scalaの存在型(`forSome)`はよくわからん。∀ T s.t.っぽい
  → ∀x(P(x) -> A) <-> ∃xP(x) -> A なので実質forall
  こっちはwitness
存在型の動的ディスパッチと静的ディスパッチ
存在型で可能になること（可読性、匿名型（クロージャ）の返り血）
不可能なこと(`trait { fn() -> impl Trait }`、`fn() -> impl Trait {if .. {..} else {..}}`)（後者は`branch`で回避できる）
存在型の問題
  ライフタイムどう表現するの
  `vec![impl Trait, impl Trait]` など
  学習コスト`fn(impl Trait)`

[jvm - Java method table - Stack Overflow](https://stackoverflow.com/questions/10225668/java-method-table)

</script>
</section>
