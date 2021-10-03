---
categories: [SML#, SML]
date: 2021-10-03T17:17:23+09:00
title: "SML#のexistential typeで遊ぶ"
---

κeenです。SML# 3.7.0から実験的機能として `_dynamiccase` に存在型のサポートが入ったので試してみます。

<!-- more -->

本記事はSML# 4.0.0 with LLVM 12.0.0で動作確認を行っています。

## Dynamicについて
`_dynamiccase` 以前にDynamicの説明から入りましょう。
DynamicはSML# 3.5.0から入った機能です。
型を消して代わりにデータに埋め込んでおき、使うときにその型を復元して使えるものです。

型を消す（型をデータに埋め込む）のは `Dynamic.dynamic` などのプリミティブを、型を復元するのは `_dynamic` などの制御構造を使います。

``` sml
val x = Dynamic.dynamic {a = 1, b = "c"}
(* val x = _ : Dynamic.void Dynamic.dyn *)

val y = _dynamic x as {a:int, b:string}
(* val y = {a = 1, b = "c"} : {a : int, b : string} *)
```

Dynamicは色々面白い使い方ができます。一旦型情報をデータにしてしまえばリフレクションが可能なのでどんな型にも使えるpretty printも実装可能になります。

``` sml
Dynamic.pp {a = 1, b = "c"}
(* => {a = 1, b = "c"} *)
```

他にも面白い話題はありますが、今回のトピックは `_dynamiccase` なのでそれに触れます。
`_dynamiccase` は `_dynamic` のパターンマッチ版で、マッチした型を復元します。

``` sml
val x = Dynamic.dynamic {a = 1, b = "c"}
_dynamiccase x of
    {a: int, b: int}    => "first" (* 変数aとbが使える *)
  | {a: int, b: string} => "second"
(* "second" *)
```

ここまで、Dynamicと `_dynamiccase` の紹介でした。

## `_dynamiccase` のexistential typeサポート

SML# 3.7.0から入った実験的なexistential typeサポートでは `_dynamiccase` で存在型が使えます。
節の冒頭で `{'a}` の形で型変数を導入し、パターン内でその `'a` が使えます。
パターンの腕では `{'a}` の中身は分からりません。
`'a` 型がついた値はパターンの腕からは脱出できないので関数を値に適用するなどして型を潰す必要があります。

``` sml
fun apply x = _dynamiccase x of {'a} (f:'a -> int, x:'a) => f x;

apply (Dynamic.dynamic (foldl (op +) 0, [1, 1]));
apply (Dynamic.dynamic (trunc, 2.34));
```

上記の例では `'a` にそれぞれ `int` 、 `real` が代入されますが、最終的には `int` を返しています。
ちょっと使い道が分かりづらいかもしれませんが、いくつか面白いことができます。

## ダックタイピング

存在型を使うとダックタイピングができます。
ダックタイピングは「アヒルのように鳴きアヒルのように歩くならそれはアヒルだ」という例えに表されるようにデータの型ではなくメッセージを受け取れるかに着目します。
これからデータの型は異なるが同じ振る舞いをするオブジェクトを作っていきます。

まずは準備からはじめましょう。必須ではないですが、分かりやすいようDynamicをラップした型を定義しておきます。

``` sml
datatype obj = Obj of Dynamic.dynamic
```

`toString` を呼べるオブジェクトを作りたいのでそのオブジェクトを表わす型を作っておきます。データとメソッドのレコードです。

``` sml
type 'self objRec = {data: 'self, toString: 'self -> string}
```

型パラメータに `'self` と名付けているように、`data` がいわゆるselfのつもりです。ユーザがこの型を用意したらオブジェクトが作れます。

雰囲気を出すために `new` 関数も用意しておきましょう。

``` sml
fun 'self#reify new (obj: 'self objRec) = Obj (Dynamic.dynamic obj)
```

`'self` に `#reify` という修飾がついているのはSML# 独自の拡張です。
型をデータとして書き込むために（=Dynamic.dynamicを呼ぶために）型情報を取り出せる必要があるのでついています。ここはあまり深くは踏み込まずに次に進みます。

この `new` で作ったデータ型に対して `toString` を呼べるようにしてみます。
`dynamic` を `_dynamiccase` にかけてデータを復元します。このとき多相になっていた `'self` を存在型で表現してあげるとうまくいきます。

``` sml
fun toString (Obj obj) =
    _dynamiccase obj of
      {'self} {data: 'self, toString: 'self -> string} => toString data
```

selfとselfを第一引数に取るメソッドがあればそれっぽくなる、というのはある程度ダックタイピングに慣れた人なら理解頂けると思います。

これで役者が揃ったのでREPLで使ってみましょう。まずは `new` を使ってオブジェクトをいくつか作ってみます。

``` sml
# val intObj  = new {data = 1,   toString = Int.toString};
val intObj = Obj _ : obj
# val realObj = new {data = 0.1, toString = Real.toString};
val realObj = Obj _ : obj
```

`intObj` と `realObj` はそれぞれ型が異なりますが、どちらも `obj` になっています。これらの `obj` に `toString` を呼んでみましょう。

``` sml
# toString intObj;
val it = "1" : string
# toString realObj;
val it = "0.1" : string
```

元の型は異なるのに1つの関数で双方を扱えました。`intObj` と `realObj` は同じ `obj` 型なのでリストに共存させることも可能です。

``` sml
# map toString [intObj, realObj];
val it = ["1", "0.1"] : string list
```

こうしてSMLでダックタイピングができるようになりました。

### 拡張可能なオブジェクト

上記のオブジェクトは面白いんですが、フィールドが増えたときに `_dynamiccase` に失敗するという問題があります。

例えばメソッド `toInt` を増やした場合にオブジェクトの実体はこうなります。

``` sml
type 'self objRec = {data: 'self, toString: 'self -> string, toInt: 'self -> int}
```

しかし `toString` がそのままだと `toInt` フィールドがないのでマッチせずに呼出に失敗してしまいます。
必要なフィールドは揃っているはずなのにもどかしいですよね。そこで、必要なオブジェクトだけを取り出すようにしてみます。

`_dynamiccase` には余計なフィールドを無視する機能はなさそうでした。例えばレコードのパターンマッチに `...` を使うとICEになります。

``` sml
fun toString (Obj obj) =
    _dynamiccase obj of
      {'self} {data: 'self, toString: 'self -> string, ...} => toString data

(* Bug.Bug: PolyTyElimination: compileTy: BOUNDVARty at src/compiler/compilePhases/polytyelimination/main/PolyTyElimination.sml:17.14(302) *)
```

そこでDynamic側をいじって必要なデータだけを持つDynamicを作ることにします。
このあたりはSML#のリフレクションAPIに踏み込むので深くは触れませんが、以下のようなキャスト関数を定義すると上手くいきそうでした。

``` sml
fun cast dyn labels = let
    val terms = map (fn label => (label, Dynamic.dynamicToTerm (Dynamic.## label dyn))) labels
in
    Dynamic.termToDynamic (Dynamic.Record terms)
end
```

このキャストを使って `obj` と `toString` フィールドだけを抜き出すようにした `toString` を定義してみます。

``` sml
fun toString (Obj obj) =
    _dynamiccase (cast obj ["data", "toString"]) of
      {'self} {data: 'self, toString: 'self -> string} => toString data
```

`_dynamiccase` にかける前に `cast` を呼ぶことで必要なフィールドのみ取り出しています。
同様に `toInt` も定義可能なので定義しておきます。

``` sml
fun toInt (Obj obj) =
    _dynamiccase (cast obj ["data", "toInt"]) of
      {'self} {data: 'self, toInt: 'self -> int} => toInt data
```

これでREPLで試しましょう。 `intObj` と `realObj` を再定義して `toInt` も持つようにします。

``` sml
# type 'self objRec = {data: 'self, toString: 'self -> string, toInt: 'self -> int};
# fun 'self#reify new (obj: 'self objRec) = Obj (Dynamic.dynamic obj);
# val intObj  = new {data = 1,   toString = Int.toString,  toInt = fn x => x};
# val realObj = new {data = 0.1, toString = Real.toString, toInt = Real.round};
```

Dynamicを使って既存のオブジェクトを拡張することもできますが、ちょっとややこしいので素直に再度定義します。

これらの拡張したオブジェクトを `toString` や `toInt` に適用します。

``` sml
# toString intObj;
val it = "1" : string
# toInt realObj;
val it = 0 : int
```

呼べていますね。ということでSML#でダックタイピングでした。

このあと真面目にオブジェクト指向っぽくやるなら作り込み要素はいっぱいありますが、遊ぶだけならこれで十分でしょう。

## 第一級モジュールっぽいやつ

思ったよりダックタイピングで疲れたのでこっちは雑にいきます。

第一級モジュールはモジュールを値として扱えるやつです（雑）。
OCamlとかにはありますが、SMLにはありません。

モジュールを値として扱うときはほとんどレコードのようなものにエンコードできます。
`val` がそのままレコードにエンコードされるのですが、 `type` は存在型へエンコードすることになります。
そこで `_dynamiccase` の存在型を使うとそれっぽくなるんじゃないかという趣旨です。
第一級モジュールの存在型周りはかなりややこしかった気がしますが疲れたのでそれっぽければ良いことにします。

まずはエンコードする前のモジュールのシグネチャと構造を提示します。

``` sml
signature Color = sig
    type t
    val toCode: t -> string
end

structure RGBColor: Color = struct
datatype t = RGB of word8 * word8 * word8
fun toCode (RGB (r, g, b)) =
    let fun to c = if c <= 0wxff
                   then "0" ^ (Word8.toString c)
                   else Word8.toString c
    in "#" ^ (to r) ^ (to g) ^ (to b) end
end
```

はい。これをレコード（など）にエンコードしていきましょう。
初っ端からインチキですが、モジュールのレコードと、`t` の値の両方をデータに持つことにします。

``` sml
type 'a fstColor = {color: 'a, toCode: 'a -> string}
```

例によって雰囲気出すためにpackを関数を作っておきます。

``` sml
fun 'a#reify packColor (m: 'a fstColor) = Dynamic.dynamic m
```

あとはなんか `_dynamiccase` でunpackするだけ！

``` sml
fun toCode d = _dynamiccase d of {'a} {color: 'a, toCode: 'a -> string} => toCode color
```

… `M.t` とかの型を書けないのでダックタイピングと一緒ですね。ダメだったよ…

## まとめ

SML# 3.7.0で入った `_dynamiccase` の存在型のサポートを使ってダックタイピングをやってみました。
試してないですが、reificationで型情報を取り出せるのでクラスやプロトタイプの仕組みを知っている人なら自分でオブジェクト指向っぽい枠組みを作れるんじゃないかと思います。
