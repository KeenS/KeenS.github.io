---
categories: [型, 幽霊型]
date: 2015-05-24T22:38:47+09:00
title: 幽霊型を知った
---

κeenです。かねてより気になっていた幽霊型(Phantom Type)について知ったのでアウトプット。
[このPDF](http://www.cs.rit.edu/~mtf/research/phantom-subtyping/jfp06/jfp06.pdf)がベースになって
ます。

余談ですが英語がファントム・タイプと中々中二な名前なので和訳も幻影型とかそういう方向に走って欲かったな。
<!--more-->

「幽霊型」で調べると真っ先に[このサイト](https://kagamilove0707.github.io/programming/2014/02/20/about-phantom-type/)が出てくるのですが、ミスリーディングと言われていました。
別に間違ったことを書いている訳ではないのですが、幽霊型の応用例なのでこれこそが幽霊型だと思ってしまうと少し視野が狭くなってしまうようです。

# モチベーション

ブーリアンと整数と条件分岐と足し算と比較が出来るミニ言語を考えてみます。自動で型変換は行わない(つまりintとboolの比較などは出来ない)言語とします。こんな感じでしょうか。

```sml
datatype exp
  = Int of int
  | Bool of bool
  | If of exp * exp * exp
  | Plus of exp * exp
  | Equal of exp * exp

fun mkInt x = Int x
fun mkBool x = Bool x
fun mkIf cnd thn els = If(cnd, thn, els)
fun mkPlus x y = Plus(x, y)
fun mkEqual x y = Equal(x, y)

exception Type

fun evalIf cnd thn els =
  case eval cnd of
      Bool x => if x then eval thn else eval els
    | _ => raise Type

fun evalPlus x y =
  case (eval x, eval y) of
      (Int x', Int y') => Int(x' + y')
    | _  => raise Type

fun evalEqual x y =
  case (eval x, eval y) of
      (Int x', Int y') => Bool(x' = y')
    | (Bool x', Bool y') => Bool(x' = y')
    | _  => raise Type

fun eval exp =
  case exp of
      If(cnd, thn, els) => evalIf cnd thn els
    | Plus(x, y) => evalPlus x y
    | Equal(x, y) => evalEqual x y
    | e => e
```

さて、この定義に従うと次のような自明に間違ったプログラムもコンパイルを通ってしまいます[^1]。

[^1]: 因みにこのコードをSML#のREPLで評価すると返ってこなくなります。[イシュー](https://github.com/smlsharp/smlsharp/issues/24)に上がってます。

```sml
mkEqual (mkInt 1) (mkBool false)
```

勿論、評価するとエラーになります。

```sml
eval (mkEqual (mkInt 1) (mkBool false));

uncaught exception Type
```

こういうものを極力コンパイル時に発見出来ないかとうのがモチベーションです。因みにこのモチベーションは変な型変換を行わない言語なら動的型付き言語でも同じで、SBCLで`(+ 1 t)`を評価するとちゃんとコンパイルエラーになります。

# 問題の分析
今回、`Int`だとか`Bool`だとかの型情報をタグ、つまり値として持たせました。しかしコンパイル時に値にはアクセス出来ません。コンパイル時にアクセスできるのは型です。ということでメタデータとして型を付与しましょう。

# 幽霊型
メタデータとして型を付けたのが幽霊型です。名前的に`'a texp`ではなく`'a`の部分が幽霊型なんですかね。

使い方は割と分かりやすくて、`datatype`に余計な型をつけます。この`'a`は使われていませんがメタデータなのでそういうもんです。

```sml
datatype 'a texp = E of exp
```

次に、`mkXxx` にも型を付けます。

まず、`mkInt`なら返り値はIntなので`'a`の部分にその情報を詰め込みます。今回は`int`が宜しいようです。
毎回`E`がつくのが面倒ですが我慢して下さい。

```sml
fun mkInt x: int texp = E (Int x)
```

`mkBool`も同じです。

```sml
fun mkBool x: bool texp = E (Bool x)
```

次は`mkIf`ですが、condにはBool型、thenとelseには同じ型が来て欲しいです。そして返り値というかこのIfを
evalした型はthenやelseと同じ型なのでこのような型になります。Ifに渡す為に一旦`E`を剥がしてます。

```sml
fun mkIf (E cnd: 'b texp) (E thn: 'a texp) (E els: 'a texp): 'a texp = E (If(cnd, thn, els))
```

同じように考えたら`mkPlus`や`mkEqual`も型付け出来ます。

```
fun mkPlus (E x: int texp) (E y: int texp): int texp = E (Plus(x, y))
fun mkEqual (E x: 'a texp) (E y: 'a texp): bool texp = E (Equal(x, y))
```

これ以後は変更ありません。一応修正後のプログラムを載せると、

```sml
datatype exp
  = Int of int
  | Bool of bool
  | If of exp * exp * exp
  | Plus of exp * exp
  | Equal of exp * exp

datatype 'a texp = E of exp

fun mkInt x: int texp = E (Int x)
fun mkBool x: bool texp = E (Bool x)
fun mkIf (E cnd: 'b texp) (E thn: 'a texp) (E els: 'a texp): 'a texp = E (If(cnd, thn, els))
fun mkPlus (E x: int texp) (E y: int texp): int texp = E (Plus(x, y))
fun mkEqual (E x: 'a texp) (E y: 'a texp): bool texp = E (Equal(x, y))

exception Type

fun evalIf cnd thn els =
  case eval' cnd of
      Bool x => if x then eval' thn else eval' els
    | _ => raise Type

and evalPlus x y =
    case (eval' x, eval' y) of
        (Int x', Int y') => Int(x' + y')
      | _  => raise Type

and evalEqual x y =
    case (eval' x, eval' y) of
        (Int x', Int y') => Bool(x' = y')
      | (Bool x', Bool y') => Bool(x' = y')
      | _  => raise Type

and eval' exp =
    case exp of
        If(cnd, thn, els) => evalIf cnd thn els
      | Plus(x, y) => evalPlus x y
      | Equal(x, y) => evalEqual x y
      | e => e

fun eval (E x) = eval' x
```

となります。ユーザインターフェースレベルでの変更はありません。
さて、ということで先の式をもう一度評価してみましょう。

```
mkEqual (mkInt 1) (mkBool false);
stdIn:1.2-1.34 Error: operator and operand don't agree [tycon mismatch]
  operator domain: int texp
  operand:         bool texp
  in expression:
    (mkEqual (mkInt 1)) (mkBool false)
```

ちゃんとエラーが出ました。楽しいですね。

# 発展1 - 任意の型
任意の型を作る手段としてタグ1つ、引数無しのタグ付き共用体を使う方法があります。それを使った技が先のPDFに載ってます。

```sml
datatype tcp = TCP
datatype udp = UDP
datatype 'a safe_socket = W of Word32.word

fun mkTCP ....
fun mkUDP ....
fun sendTCP ...
fun sendUPD ...
fun close ...
```

のように型安全に、そして`close`のように可能なものはジェネリックにプログラムを書くことが出来ます。

# 発展2 - サブタイプ
次のようにヒエラルキーがある場合の話です。面倒なので言語を縮小しますね。

```sml
datatype num
  = Int of int
  | Real of real

datatype exp
  = Num of num
  | Bool of bool
  | Plus of exp * exp
```

この場合、こうなります。

```sml
datatype num
  = Int of int
  | Real of real

datatype exp
  = Num of num
  | Bool of bool
  | Plus of exp * exp

datatype 'a tnum = N
datatype 'a texp = E of exp

exception Type

fun mkInt x: (int tnum texp) = E (Num  (Int x))
fun mkReal x: (real tnum texp) = E (Num (Real x))
fun mkBool x: (bool texp) = E (Bool x)


fun mkPlus (E x: 'a tnum texp) (E y: 'a tnum texp): 'a tnum texp =
  case (x, y) of
      (Num (Int x'), Num (Int y')) => E (Num (Int (x' + y')))
    | (Num (Real x'), Num (Real y')) => E (Num (Real (x' + y')))
    | _ => raise Type  
```

疲れてきたので説明省略しますが型パラメータを入れ子にしたら上手い具合にサブタイプの親子関係を表せるよねって話です。

# まとめ
幽霊型を使うと値レベルの話を一部型レベルに持ち上げれる（ってことで良いのかな？）

# 最後に
どうしても

```sml
datatype 'a texp = E of exp
```

としている所の`E`が気になりますね。型システム上必要っぽい(`type 'a texp = exp`としてもダメだった)ので必要なコストだとは思うのですが
少くとも内部的にはゼロオーバーヘッドであって欲しいです。最適化で消せそうですが実際に消してる処理系はどれほどあるのでしょう。

`type 'a texp = exp`でダメだった(型がミスマッチしてるのにコンパイルが通ってしまった)のも気になります。smlの`type`ってただのエイリアスなんでしたっけ？

若干のモヤモヤは残るものの一応幽霊型について知りました。
