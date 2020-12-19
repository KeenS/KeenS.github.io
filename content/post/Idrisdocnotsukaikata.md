---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-19T18:21:39+09:00
title: "Idrisdocの使い方"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の15日目の記事です。

κeenです。今回はIdrisのドキュメントコメントとそれを使ったドキュメント生成ツールの使い方を紹介します。

<!--more-->

ベースになるのは [公式リファレンス](http://docs.idris-lang.org/en/latest/reference/documenting.html)です。簡素な説明しかないので少し補いながら説明していきます。

# ドキュメントコメント

トップレベルの宣言の前に `|||` ではじまる行を置くことでドキュメントコメントになります。

例えば以下のように書けます。

```idris
||| モジュールにドキュメントを書ける
module Docs
```

書いたドキュメントはいくつか用途がありますが、例えばREPLから参照することができます。

```text
Idris> :doc Docs
Module Docs:
    モジュールにドキュメントを書ける
```

ドキュメントコメントは連続して書けば複数行でも書けます。
また、Javadocと同様の構文、 `@ パラメータ名 説明` でパラメータに説明を加えることもできます（要[名前つきパラメータ](https://keens.github.io/blog/2020/12/16/idrisnonamaetsukiparame_tatogadt/)）。

```idris
import Data.Vect

||| ベクタを連結する
||| @ a ベクタの内容
||| @ xs 先頭のベクタ（再帰引数）
||| @ ys 次のベクタ（場合分けに使われない）
appendV : (xs : Vect n a) -> (ys : Vect m a) -> Vect (add n m) a
appendV []      ys = ys
appendV (x::xs) ys = x :: appendV xs ys
```

これはREPLだと以下のように表示されます。

```idris
Idris> :doc appendV
Docs.appendV : (xs : Vect n a) -> (ys : Vect m a) -> Vect (add n m) a
    ベクタを連結する
    Arguments:
        (implicit) a : Type  -- ベクタの内容
        
        xs : Vect n a  -- 先頭のベクタ（再帰引数）
        
        ys : Vect m a  -- 次のベクタ（場合分けに使われない）
        
    The function is: Total & public export
```

ドキュメントコメントではマークダウン記法が使えます。

``` idris
||| 数値を足す
|||
||| 足し算はめちゃくちゃすごいよね。この段落はOverviewに含まれない。
||| 後続の行も1つの段落に入る。
|||
||| ドキュメント内に表示されるコードサンプルを書くこともできる
||| ```idris example
||| add 4 5
||| ```
|||
||| リスト記法：
||| * はい
||| * いいえ
||| * コードの `add` や **太字** なども使える
||| @ n は再帰パラメータ
||| @ m は再帰パラメータでない
add : (n, m : Nat) -> Nat
add Z     m = m
add (S n) m = S (add n m)
```

これをREPLで表示すると以下のようになります。

```text
Idris> :doc add
Docs.add : (n : Nat) -> (m : Nat) -> Nat
    数値を足す
    
    足し算はめちゃくちゃすごいよね。この段落はOverviewに含まれない。 後続の行も1つの段落に入る。
    
    ドキュメント内に表示されるコードサンプルを書くこともできる
    
        > add 4 5
        9
    
    リスト記法：
    
    * はい
    * いいえ
    * コードの add や 太字 なども使える
    Arguments:
        n : Nat  -- は再帰パラメータ
        
        m : Nat  -- は再帰パラメータでない
        
    The function is: Total & public export
```

テキストで貼ってしまうと分かりづらいですが、REPLを表示している上ではコードのシンタックスハイライトや等幅、太字なども処理されています。

マークダウンエンジンは[cheapskate](https://hackage.haskell.org/package/cheapskate)が使われているようです。画像へのリンクなどもサポートしていますが、ターミナルでは表示できません。後述のidrisdocによるHTML生成などではできるようです。

Overviewという用語が登場していますが、これは `:apropos` や `search` で検索したときに表示されます。

``` text
Idris> :apropos add
...

Docs.add : Nat -> Nat -> Nat
数値を足す

...
```

ドキュメントの最初の1行だけが表示されていますね。

さて、ドキュメントを書けるのはトップレベルの宣言以外にもデータ型のコンストラクタなどもあります。


``` idris
||| シンプルなデータ型
data Ty =
  ||| Unit型
  UNIT |
  ||| 関数型
  ARR Ty Ty
```

GADTを使うと名前付きパラメータが使えるのでコンストラクタの引数のドキュメントも書けるようになります。

``` idris
||| 型文脈での場所を指す
data Elem : Vect n Ty -> Ty -> Type where
  Here : {ts : Vect n Ty} -> Docs.Elem (t::ts) t
  There : {ts : Vect n Ty} -> Docs.Elem ts t -> Docs.Elem (t'::ts) t

||| もうちょっと面白いデータ型
||| @ n 自由変数の数
||| @ ctxt 自由変数の型文脈
||| @ ty 項の型
data Term : (ctxt : Vect n Ty) -> (ty : Ty) -> Type where

  ||| Unit型のコンストラクタ
  ||| もっとコメント
  ||| @ ctxt 型文脈
  UnitCon : {ctxt : Vect n Ty} -> Term ctxt UNIT

  ||| 関数適用
  ||| @ f 適用する関数
  ||| @ x 引数
  App : {ctxt : Vect n Ty} -> (f : Term ctxt (ARR t1 t2)) -> (x : Term ctxt t1) -> Term ctxt t2

  ||| ラムダ抽象
  ||| @ body 関数本体
  Lam : {ctxt : Vect n Ty} -> (body : Term (t1::ctxt) t2) -> Term ctxt (ARR t1 t2)

  ||| 変数
  ||| @ i de Bruijn インデックス
  Var : {ctxt : Vect n Ty} -> (i : Elem ctxt t) -> Term ctxt t
```

もちろん、レコードにもドキュメントコメントが書けます。

``` idris
||| フィールドやコンストラクタを含めてレコードにもドキュメントが書けるよ
record Yummy where
  ||| Yummyを作る
  constructor MkYummy
  ||| 食べるもの
  food : String

```



それぞれ以下のように表示されます。

``` text
Idris> :doc Term
Data type Docs.Term : (ctxt : Vect n Ty) -> (ty : Ty) -> Type
    もうちょっと面白いデータ型
    Arguments:
        (implicit) n : Nat  -- 自由変数の数
        
        ctxt : Vect n Ty  -- 自由変数の型文脈
        
        ty : Ty  -- 項の型
        
    The function is: public export
Constructors:
    UnitCon : Term ctxt UNIT
        Unit型のコンストラクタ もっとコメント
        Arguments:
            (implicit) ctxt : Vect n Ty  -- 型文脈
            
        The function is: public export
    App : (f : Term ctxt (ARR t1 t2)) -> (x : Term ctxt t1) -> Term ctxt t2
        関数適用
        Arguments:
            f : Term ctxt (ARR t1 t2)  -- 適用する関数
            
            x : Term ctxt t1  -- 引数
            
        The function is: public export
    Lam : (body : Term (t1 :: ctxt) t2) -> Term ctxt (ARR t1 t2)
        ラムダ抽象
        Arguments:
            body : Term (t1 :: ctxt) t2  -- 関数本体
            
        The function is: public export
    Var : (i : Elem ctxt t) -> Term ctxt t
        変数
        Arguments:
            i : Elem ctxt t  -- de Bruijn インデックス
            
        The function is: public export
Idris> :doc Yummy
Record Yummy
    フィールドやコンストラクタを含めてレコードにもドキュメントが書けるよ

Constructor:
    MkYummy : (food : String) -> Yummy
        Yummyを作る
        Arguments:
            food : String  -- 食べるもの
            
        The function is: public export
Projections:
    food : (rec : Yummy) -> String
        食べるもの
        
        The function is: public export
```

もうちょっと凝った文法があるかなと思って実装を読んだんですがマークダウンエンジンに丸投げだったのでマークダウン記法と `@ パラメータ名 説明` の文法以外は特になさそうでした。

# Idrisdoc

次にドキュメントコメントからHTMLを生成する方法を解説します。

## REPLから

`:mkdoc` コマンドでHTMLを生成できます。その際、 `doc/` 以下に生成します。

例えば `Docs.idr` を読み込みつつREPLを開き、 `:mkdoc Docs` でHTMLを生成するとこうなります。

``` text
$ idris Docs.idr
     ____    __     _
    /  _/___/ /____(_)____
    / // __  / ___/ / ___/     Version 1.3.3
  _/ // /_/ / /  / (__  )      https://www.idris-lang.org/
 /___/\__,_/_/  /_/____/       Type :? for help

Idris is free software with ABSOLUTELY NO WARRANTY.
For details type :warranty.
Idris> :mkdoc Docs
IdrisDoc generated
*Docs>
Bye bye
$ ls doc
IdrisDoc  docs  index.html  styles.css
```

[これで生成されたドキュメント](/Idrisdocnotsukaikata/doc/index.html)を見るとpreludeなどに混じって[`Docs` モジュールのドキュメント](/Idrisdocnotsukaikata/doc/docs/Docs.html)があるのが確認できます。


## コマンドラインから

残念ながら単一のファイルのドキュメントを生成するコマンドはなさそうでした
（コンパイラの実装を見ましたが、 `--mkdoc` しかなかったです）。しかしパッケージのドキュメントを生成することはできます。

さっきの `Docs` をパッケージにしましょう。サクッと以下のようなディレクトリを作ります。

``` text
$ tree
.
├── Docs.ipkg
└── src
    └── Docs.idr
```

そして `Docs.ipkg` には以下の記述をします。

``` text
package Docs

version = "0.1.0"
author = your name

sourcedir = src
modules = Docs
```

あとは `idris --mkdoc Docs.ipkg` を打つだけです。

``` text
$ idris --mkdoc Docs.ipkg
Type checking src/Docs.idr
$ ls
Docs.ipkg  docs_doc  src
```

こっちは `小文字のモジュール名_doc` にドキュメントが生成されます。
内容はREPLのものと変わりません。

## インストールとdocdir

ちょっと何に使うのか分かってないのですが、ドキュメントをインストールすることもできます。
`--installdoc IPKG` のコマンドです。

``` text
$ idris --installdoc Docs.ipkg
```

これは `idris --docdir` で表示される場所にドキュメントをインストールします。

``` text
$ ls $(idris --docdir)
base  contrib  docs  effects  prelude  pruviloj
```

`ls` の結果の中に `docs` がいますね。
…ですがこれをどうしたらいいのかよく分かってません。Idris側ではこれらを表示するコマンドやサーバを立てる方法は用意してないようです。

強いていうなら以下のようにワンライナーサーバを立てるくらいでしょうか。

``` text
$ ruby -run  -e httpd $(idris --docdir)
```


# まとめ

Idrisdocの記法と生成ツールの使い方を紹介しました。
Idrisdocにはマークダウンと引数の説明の文法がありました。
生成ツールにはREPLから起動して名前空間を指定する方法とCLIから起動してパッケージのドキュメントを生成する方法がありました。
