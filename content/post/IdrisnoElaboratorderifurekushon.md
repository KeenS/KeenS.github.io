---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-25T20:40:02+09:00
title: "IdrisのElaboratorリフレクションでメタプログラミング"
---

このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の25日目の記事です。

今回はIdrisの面白機能の1つElaboratorのリフレクションを利用したメタプログラミング紹介しようと思います。
かなりコアな部分に踏み込むので分からなかったら「へー、そんな機能もあるんだ」と思っておいて下さい。

<!--more-->

# Elaboratorリフレクションとは

Elaboratorリフレクションとは[Elaborator reflection: extending Idris in Idris](https://dl.acm.org/doi/10.1145/2951913.2951932)で提案された機能です。
Elaboratorを何と訳すか難しいのですが、強いて訳すなら推敲器とかになりますかね？

Elaboratorとは多くの（関数型言語の）コンパイラの内部に存在する変換器です。
コンパイラはユーザにはリッチな表現を提供しつつ、内部で扱う言語はかなりシンプルに保っています。

ElaboratorリフレクションではそのElaboratorへのアクセスを提供します。
つまりユーザの書いたコードでコンパイルプロセスに割り込めるのです。

Elaboratorリフレクションの中ではIdrisそのものを対象にプログラムが書けるので、メタプログラミングができます。
すなわちIdrisのプログラムを生成するプログラムが書けるのです。

ここでユーザスクリプトで割り込むのがElaboratorというのが絶妙です。
Elaboratorで推敲した結果、小さなコア言語になります。
そしてそのコア言語で型推論が行われるのです。
つまりハチャメチャなElaboratorスクリプトを書いてもその後の型推論に通らなかったらコンパイルは通りません。
Idrisの担保する安全性を脅かすことなく自由にメタプログラミングができる訳です。

Elaboratorリフレクション難しい機能になるので公式ドキュメントなども参考にしながら読んで下さい。

[Elaborator Reflection Introduction — Idris 1.3.3 documentation](http://docs.idris-lang.org/en/latest/elaboratorReflection/elaborator-reflection.html)

# Elaboratorリフレクションの使い方

4ステップで使えます。

1. Elaboratorリフレクションの機能をonにする
    ```idris
    %language ElabReflection
    ```
2. エスパー能力に目覚める
3. エスパー能力を使ってElaboratorスクリプトを書く
4. `%runElab` でElaboratorスクリプトを走らせる

やってみましょう。

## `id` 関数を生成する

ドキュメントに載っている例です

まずはElaboratorリフレクションをonにします。

``` idris
%language ElabReflection
```

次にエスパー能力ですが、今回はドキュメントに載っているものを使うので使う必要はありません。

Elaboratorスクリプトを書きましょう。
Elaboratorスクリプトは `Elab` モナドで記述します。
こういう書き出しになります。

``` idris
mkId : Elab ()
```

実装する前に使用例を出しておくと、以下のように使います。

``` idris
idNat : Nat -> Nat
idNat = %runElab mkId
```

`\x => x` を生成するイメージですね。

この実装は以下のスクリプトで与えられます。

``` idris
mkId : Elab ()
mkId = do intro `{{x}}
          fill (Var `{{x}})
          solve
```

軽く解説しておきましょう。
`` `{{x}} `` はリフレクションで使う機能で、変数名の内部表現を手軽に書く記法です。

``` text
Idris> `{{x}}
UN "x" : TTName
Idris> `{{Prelude.Basics.id}}
NS (UN "id") ["Basics", "Prelude"] : TTName
```

それぞれのスクリプトは以下のような意味です。

* ``intro `{{x}}`` は `\x => ...` 相当
* ``fill (Var `{{x}})`` は変数 `x` を返り値としていることに相当
* `solve` は （恐らく） 変数 `x` が `\x => ...` から取得できることを確認

それでは先程紹介したとおり使ってみます。

``` idris
idNat : Nat -> Nat
idNat = %runElab mkId
```

今定義した `idNat` を軽く使ってみましょう。

``` text
Idris> idNat 1
1 : Nat
Idris> idNat 0
0 : Nat
Idris> idNat 1000
1000 : Nat
```

想定通り動いていますね。

REPLには `:core` という組み込みコマンドがあります。
任意の式のコア言語での表現を取得できます。
これで生成された `idNat` の中身を見てみましょう。

``` idris
Idris> :core idNat
Main.idNat : (__pi_arg : Prelude.Nat.Nat) → Prelude.Nat.Nat
Main.idNat ↦ λ x . x
```

これも想定通りの中身ですね。

使えるコマンドなどはAPIドキュメントを見ましょう。

* [IdrisDoc: Language.Reflection.Elab](https://www.idris-lang.org/docs/current/prelude_doc/docs/Language.Reflection.Elab.html)
* [IdrisDoc: Language.Reflection](https://www.idris-lang.org/docs/current/prelude_doc/docs/Language.Reflection.html)

# Elaboratorリフレクションの使いどころ

Elaboratorリフレクションにはいくつか使いどころがあります。

1つは証明です。
`intro` とか `fill` とかは証明を意識したネーミングになっています。

もう1つは穴（Hole）を埋めるためです。
Advent Calendar内で触れそびれたのですが、Idrisには部分的に未完成なプログラムをロードする機能があります。
そしてそのHoleの型や、利用できる型を表示できるので、難しい部分をひとまずHoleにしておき、あとで型をみながら埋めることができます。
そのときにElaboratorリフレクションを使ってインタラクティブにHoleの型を変化させながらプログラミングできるのです。
これは証明をするときに使うテクニックですね。

そして今回紹介するのがメタプログラミングです。
手で実装するのが面倒な関数の自動生成に挑戦したいと思います。

余談ですがElaboratorリフレクションのスクリプトはElabと呼んでしまうことが多いようです。
全然関係ないですが[Idris Elba](https://ja.wikipedia.org/wiki/イドリス・エルバ)という俳優がいるらしいです。

# Elabで関数生成

さきほどは関数の本体部分のみを生成しました。
ここでは型宣言や関数名への定義も含めた関数定義全体を生成します。

つまり、以下のコードで

``` idris
%runElab genMyId
```

以下のコード相当のものが定義されるのを目指します。

``` idris
myId : a -> a
myId = \(x: a) => x
```

## ElabかASTか

さきほどはドキュメントに載っているとおり `intro` や `fill` などの命令を使って関数を書きました。
関数を定義するにはもう1つ方法があって、ASTを直接書くこともできます。

ASTを直接書くのはかなり手間が掛かるので正直にいうとゴリラ向けの手法になります。

しかしここではElabを使ったコードは挙動が分かりづらいので、ゴリラになってASTを直接定義する方向で頑張ります。
エスパー能力はもっと難しいことをするために温存しておきましょう。

---

それでは `genMyId` を書きはじめましょう。


``` idris
genMyId : Elab ()
genMyId = do
  -- ...
```

使うAPIは `declareType` と `defineFunction` です。
ドキュメントから関連する型を辿ってみて下さい。

そしてエスパーして使い方を習得します。
するとこう使えるはずです。

```idris
genMyId : Elab ()
genMyId = do
  let name = `{{myId}}
  let x = `{{x}}
  let a = `{{a}}
  -- myId : {a: Type} -> a -> a
  let ty = Declare name [MkFunArg a RType Implicit Erased, MkFunArg x (Var a) Explicit Erased] (Var a)
  -- myId = \{a: Type} => \(x: a) => x
  let clause = MkFunClause (Var name) (RBind a (Lam RType) (RBind x (Lam (Var a)) (Var x)))
  let f = DefineFun name [clause]
  declareType ty
  defineFunction f
```

ちょっと分かりづらい部分だけ解説します。
解説とはいっても私がエスパーした結果なので合ってるか分かりませんが。

## コアでの構文

今回生成するのは `\x => x` ですが、ジェネリクスなので内部ではそのパラメータが増えてます。
`\{a: Type} => (\(x: a) => x)` 相当のものをASTで書いています。


## `MkFunArg`

`MkFunArg a RType Implicit Erased`

と書かれている部分です。 `MkFunArg 名前 型 Plicity Erased?` です。

* 名前：Idrisの型宣言では引数に名前を書くことができますが、ASTでは必須です。
* 型：型の名前を書きます。ただし `Type` 型だけは組込みなので `RType` を書きます。
* Plicity： `{}` な引数は `Implicit` 、 `()` な引数はExplicitです。
* Erased?：基本 `Erased` です。型を値として取り出すときだけ `NotErased` を使います

## `MkFunClause`

変数を導入する系の構文はすべて `RBind` にまとめられているようです。
関数は `RBind 変数名 (Lam 型) 本体` の構文で作ります。


---

これで生成された関数のcore表現を見てみましょう。

``` text
Idris> :core myId
myId : (a : Type) → (x : a) → a
myId ↦ λ a . λ x . x
```

解説通りですね。

# Elabで関数生成その2

先程は以下のような関数を生成しました。

``` idris
myId : a -> a
myId = \(x: a) => x
```


次は少しだけ構文を変えて、以下のように定義します。

``` idris
myId : a -> a
myId x = x
```

これは以下のようなスクリプトで生成できます。

``` idris
genMyId2 : Elab ()
genMyId2 = do
  let name = `{{myId2}}
  let a = `{{a}}
  let x = `{{x}}
  -- myId2 : {a: Type} -> a -> a
  let ty = Declare name [MkFunArg a RType Implicit Erased, MkFunArg x (Var a) Explicit Erased] (Var a)
  -- myId2 x = x
  let clause = MkFunClause (RBind a (PVar RType) (RBind x (PVar (Var a)) (RApp (RApp (Var name) (Var a)) (Var x)))) (RBind a (PVar RType) (RBind x (PVar (Var a)) (Var x)))
  let f = DefineFun name [clause]
  declareType ty
  defineFunction f
```

これもややこしいところだけ解説しておきます。

## 変数の導入

新しい変数を導入するのにも `RBind` を使うようです。
新しい変数というのは `myId x =` の `x` のことです。
`(RBind x (PVar (Var a)) ...)` の構文になってますね。
そしてここで `a` という変数がでてきました。これも引数で受け取るので、こうなります。
`(RBind a (PVar RType) (RBind x (PVar (Var a)) ...))`

さらに加えてややこしいのが、右辺、 `x` の方にも同時に変数の導入が必要です。
なので右辺もこうなってます。 `(RBind a (PVar RType) (RBind x (PVar (Var a)) ...))`


## 関数定義の構文

``` idris
myId a =
```

は

``` idris
(RApp (Var name) (Var x))
```

。 `App` 使うんだって驚きですね。

---

これもコア表現を見てみましょう。

``` text
Idris> :core myId2
myId2 : (a : Type) → (x : a) → a
var a : Type, x : a .
  myId2 a x ↦ x
```


# パターンマッチ

パターンマッチするコードを生成してみましょう。

```idris
myNot : Bool -> Bool
myNot True = False
myNot False = True
```

こうなります。

``` idris
genNot : Elab ()
genNot = do
  let name = `{{myNot}}
  let bool = `{{Prelude.Bool.Bool}}
  let true = `{{Prelude.Bool.True}}
  let false = `{{Prelude.Bool.False}}
  b <- gensym "b"
  -- myNot : Bool -> Bool
  let ty = Declare name [MkFunArg b (Var bool) Explicit NotErased]  (Var bool)
  -- myNot True = False
  let clauseTrue  = MkFunClause (RApp (Var name) (Var true)) (Var false)
  -- myNot False = True
  let clauseFalse = MkFunClause (RApp (Var name) (Var false)) (Var true)
  let f = DefineFun name [clauseTrue, clauseFalse]
  declareType ty
  defineFunction f

%runElab genNot
```

以下のことを押えましょう。

* 既存の名前は完全修飾名で指定する
* `gensym` がある
* パターンマッチは節を複数にすればできる

ジェネリクスがないので簡単ですね。

# `deriving` の実装

IdrisにはHaskellにある `deriving Show, Eq` のような機能がありません。
しかしメタプログラミングを使えばこれも実装できるはずです。やってみましょう。

最終的には以下のコードを書くと

``` idris
data Janken = Gu | Choki | Pa

%runElab deriveShow `{{Main.Janken}}
```

以下が生成されるスクリプトを目指します。

``` idris
Show Janken where
  show Gu    = "Gu"
  show Choki = "Choki"
  show Pa    = "Pa"
```

## `myShow`

まずは実験的にデータ型の型名を受け取ったら「そのデータ型を文字列にする関数 `myShow`」 を定義するスクリプトを書いてみましょう。

イメージとしては以下のコードを書くと

``` idris
data Janken = Gu | Choki | Pa

%runElab (genShow `{{Main.Janken}})
```

以下のコードを生成する感じです。

``` idris
myShow : Janken -> String
myShow Gu    = "Gu"
myShow Choki = "Choki"
myShow Pa    = "Pa"
```

あんまり難しくなっても訳がわからなくなるので、データ型に型引数はなく、コンストラクタも全て引数をとらないものとします。

実装を与えるとこうなります。

``` idris
genShow : TTName -> Elab ()
genShow name = do
  let fname = `{{myShow}}
  dt <- lookupDatatypeExact name
  x <- gensym "x"
  let ty = Declare fname [MkFunArg x (Var name) Explicit NotErased] (RConstant StrType)
  let clauses = map (genClause fname) $ constructors dt
  let f = DefineFun fname clauses
  declareType ty
  defineFunction f
where
    genClause : TTName -> (TTName, List CtorArg, Raw) -> FunClause Raw
    genClause fname (cname, _, _) =
      let NS (UN cnameStr) _ = cname in
      MkFunClause (RApp (Var fname) (Var cname)) (RConstant (Str cnameStr))
```

`genClause` で `myShow Choki = "Choki"` などの節を生成しています。
落ち着いて読めば難しくないですね。

## `Show` インタフェースの実装

今日一番のエスパーの使いどころですよ。

Elabでインタフェース関連の機能はこれだけです。


``` text
Idris> :doc addImplementation
Language.Reflection.Elab.Tactics.addImplementation : (ifaceName : TTName) ->
    (implName : TTName) -> Elab ()
    Register a new implementation for interface resolution.
    Arguments:
        ifaceName : TTName  -- the name of the interface for which an implementation is
        being registered
        
        implName : TTName  -- the name of the definition to use in implementation search
        
    The function is: Total & export
```

インタフェースの実装を追加するのですが、インタフェース名と実装名のみ引数にとります。
実装は…？

ちょっとよく分からないので `show` のコア表現をみてみましょう。

``` idris
λΠ> :core show
Prelude.Show.show : (ty : Type) →
                    (__interface : Prelude.Show.Show ty) → (x : ty) → String
var ty : Type,
    {meth_0} : (x : ty) → String,
    {meth_1} : (d : Prelude.Show.Prec) → (x5 : ty) → String .
  Prelude.Show.show ty
    (constructor of Prelude.Show.Show <<<erased>>> {meth_0} {meth_1})
  ↦
  {meth_0}
```

ふむふむ。ここで温存しておいたエスパー能力をフルに発揮します。
…はい。 `__interface : Prelude.Show.Show ty` や `constructor of Prelude.Show.Show` などからエスパーできましたね。
インタフェースの内部表現を作ればよいのです。

例えば以下のようなインタフェースを定義したとします。

``` idris
interface Name a where
  name: a -> String
```

するとコア的にはだいたい以下のような表現へと変換されます。

``` idris
data Name a = MkName (a -> a)

name : (a : Type) -> Name a -> a -> a
name _ (MkName f) = f
```

次にこれをStringに実装します。

``` idris
Name String where
  name x = x
```

すると以下のようなコア表現へとなります。

``` idris
strName : Name String
strName = MkName name
where
  name : String -> String
  name x = x
```

そして `Name` を使うコードを書きましょう。

``` idris
getName : String -> String
genName = name
```

この関数にはコンパイラが型から `strName` が適切だとみつけ、暗黙に渡すのです。

``` idris
getName : String -> String
genName = name Strig strName
```

ここまでくればもう分かりましたね。
「コンパイラが型から `strName` が適切だとみつけ」るために、コンパイラ内のDBに実装を登録する関数が `addImplementation` な訳です。

これで `Show` の実装方法が分かりました。 `deriveShow` 関数を書いていきましょう。


``` idris
deriveShow : TTName -> Elab ()
deriveShow name = ...
```

ところで、上記の `MkName` に相当する `Show` のコンストラクタ名はなんでしょう。
コア言語では `constructor of Prelude.Show.Show` と表示されているので名前がなさそうですね。
名前がものを在らしめるなんて言葉がありますが、名前のないものを使うことはできません。
どうしたもんでしょう。

でも大丈夫。Elabならデータ型さえ分かればリフレクションでコンストラクタを取り出せます。
そしてデータ型は `Prelude.Show.Show` です。これで完璧！

`Show` の定義には2つのメソッドがあります。

``` idris
interface Show ty where
  show : (x : ty) -> String
  show x = showPrec Open x -- Eta expand to help totality checker

  showPrec : (d : Prec) -> (x : ty) -> String
  showPrec _ x = show x
```

`Show` のコンストラクタを仮に `MkShow` とすると、 `MkShow myShow myShowPrec` という呼び出しになるはずです。
あとは `myShow` と `myShowPrec` を定義するだけですね。
ここでデフォルト実装が与えられているのでそれを使いたいのですが、どこにあるか分からなかったのであきらめました。
`myShow` と `myShowPrec` 両方を定義します。

`myShow` と `myShowPrec` をそれぞれ定義する関数が書けたとして、 `implement Show name` は以下のように書けます。


```idris
deriveShow : TTName -> Elab ()
deriveShow name = do
  dt <- lookupDatatypeExact name

  fshow <- genShow dt
  fshowPrec <- genShowPrec fshow dt

  let ifc = `{{Prelude.Show.Show}}
  inst <- gensym "showInst"
  ifcData <- lookupDatatypeExact ifc
  let [(ctor, _, _)] = constructors ifcData
  let instTy = Declare inst [] (RApp (Var ifc) (Var name))
  let instClause = MkFunClause (Var inst) (RApp (RApp (RApp (Var ctor) (Var name)) (Var fshow)) (Var fshowPrec))
  let instF = DefineFun inst [instClause]
  declareType instTy
  defineFunction instF

  addImplementation ifc inst
where
  genShow : Datatype -> Elab TTName
  ...
  genShowPrec : TTName -> Datatype -> Elab TTName
  ...
```


`genShow` はほぼさっき定義したものとおなじです。

``` idris
  genClause : TTName -> (TTName, List CtorArg, Raw) -> FunClause Raw
  genClause fname (cname, _, _) =
    let NS (UN cnameStr) _ = cname in
    MkFunClause (RApp (Var fname) (Var cname)) (RConstant (Str cnameStr))
  genShow : Datatype -> Elab TTName
  genShow dt = do
    fshow <- gensym "show"
    x <- gensym "x"
    let ty = Declare fshow [MkFunArg x (Var name) Explicit NotErased] (RConstant StrType)
    let clauses = map (genClause fshow) $ constructors dt
    let f = DefineFun fshow clauses
    declareType ty
    defineFunction f
    pure fshow

```

`genShowPrec` は新規コードですが、 `show` を呼び出すだけなのでそこまで難しくないでしょう。

``` idris
  genShowPrec fshow dt = do
    fshowPrec <- gensym "showPrec"
    let prec = `{{Prelude.Show.Prec}}
    a <- gensym "a"
    x <- gensym "x"
    ign <- gensym "ignore"
    -- showPrec : Prec -> name -> String
    let ty = Declare fshowPrec [MkFunArg `{{_er}} (Var prec) Explicit Erased, MkFunArg `{{y}} (Var name) Explicit Erased] (RConstant StrType)
    let clauseArg  = (RApp (RApp (Var fshowPrec) (Var ign)) (Var x))
    let clauseBody = (RBind ign (PVar (Var prec)) (RBind x (PVar (Var name)) (RApp (Var fshow) (Var x))))
    -- showPrec _ x = show x
    let clause = MkFunClause (RBind ign (PVar (Var prec)) (RBind x (PVar (Var name)) clauseArg)) clauseBody
    let f = DefineFun fshowPrec [clause]
    declareType ty
    defineFunction f
    pure fshowPrec
```

これで完成しました。全体を再掲するとこうです。

``` idris
deriveShow : TTName -> Elab ()
deriveShow name = do
  dt <- lookupDatatypeExact name

  fshow <- genShow dt
  fshowPrec <- genShowPrec fshow dt

  let ifc = `{{Prelude.Show.Show}}
  inst <- gensym "showInst"
  ifcData <- lookupDatatypeExact ifc
  let [(ctor, _, _)] = constructors ifcData
  let instTy = Declare inst [] (RApp (Var ifc) (Var name))
  let instClause = MkFunClause (Var inst) (RApp (RApp (RApp (Var ctor) (Var name)) (Var fshow)) (Var fshowPrec))
  let instF = DefineFun inst [instClause]
  declareType instTy
  defineFunction instF

  addImplementation ifc inst
where
  genClause : TTName -> (TTName, List CtorArg, Raw) -> FunClause Raw
  genClause fname (cname, _, _) =
    let NS (UN cnameStr) _ = cname in
    MkFunClause (RApp (Var fname) (Var cname)) (RConstant (Str cnameStr))
  genShow : Datatype -> Elab TTName
  genShow dt = do
    fshow <- gensym "show"
    x <- gensym "x"
    let ty = Declare fshow [MkFunArg x (Var name) Explicit NotErased] (RConstant StrType)
    let clauses = map (genClause fshow) $ constructors dt
    let f = DefineFun fshow clauses
    declareType ty
    defineFunction f
    pure fshow

  genShowPrec : TTName -> Datatype -> Elab TTName
  genShowPrec fshow dt = do
    fshowPrec <- gensym "showPrec"
    let prec = `{{Prelude.Show.Prec}}
    a <- gensym "a"
    x <- gensym "x"
    ign <- gensym "ignore"
    -- showPrec : Prec -> name -> String
    let ty = Declare fshowPrec [MkFunArg `{{_er}} (Var prec) Explicit Erased, MkFunArg `{{y}} (Var name) Explicit Erased] (RConstant StrType)
    let clauseArg  = (RApp (RApp (Var fshowPrec) (Var ign)) (Var x))
    let clauseBody = (RBind ign (PVar (Var prec)) (RBind x (PVar (Var name)) (RApp (Var fshow) (Var x))))
    -- showPrec _ x = show x
    let clause = MkFunClause (RBind ign (PVar (Var prec)) (RBind x (PVar (Var name)) clauseArg)) clauseBody
    let f = DefineFun fshowPrec [clause]
    declareType ty
    defineFunction f
    pure fshowPrec
```

それでは使ってみましょう。

``` idris
data Janken = Gu | Choki | Pa
%runElab deriveShow `{{Main.Janken}}
```

これを動かしてみます。


``` idris
Idris> show Gu
"Gu" : String
Idris> show Choki
"Choki" : String
Idris> show Pa
"Pa" : String
Idris> :t show
show : Show ty => ty -> String
```

ただしく動いていますね。
Idrisのpain pointであったderivingがないという問題を克服できました。


# まとめ

Idrisのかなりユニークな機能であるElaboratorリフレクションを紹介しました。
そしてElabを使ったメタプログラミングのデモとして複雑でないデータ型に対して `Show` を生成できるスクリプトも示しました。
