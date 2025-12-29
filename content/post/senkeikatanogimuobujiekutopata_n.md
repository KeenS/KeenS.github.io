---
categories: [線形型, Idris2]
date: 2025-12-29T21:11:30+09:00
title: "線形型の義務オブジェクトパターン"
---
κeenです。線形型でできる面白そうなパターンを思い付いたので書いときます。

<!--more-->

まず、線形型とは値をちょうど一度だけ使えるような型です。つまり、二度使ったり一度も使わなかったりするとエラーになります。

線形型があるIdris2で実験してみましょう。Idris2では線形型を使った関数は `->` の代わりに `-@` (通称ロリポップ)で表現します。


まずは線形型な関数内で値を2回使うケース。

``` idris
import Data.Linear.Notation

dup: a -@ (a, a)
dup x = (x, x)
```

このコードをコンパイルするとエラーになります。

```console
$ idris2 -p linear -c dup.idr
1/1: Building dup (dup.idr)
Error: While processing right hand side of dup. Trying to use linear name x in non-linear context.

dup:4:13--4:14
 1 | import Data.Linear.Notation
 2 |
 3 | dup: a -@ (a, a)
 4 | dup x = (x, x)
                 ^
```

ここまではRustの所有権と同じですね。しかしRustとは違って線形型はデータを使わないのも許しません。以下のコードをみてください。

``` idris
import Data.Linear.Notation

ignore: a -@ ()
ignore a = ()
```

引数 `a` を使わずに放置しています。これもコンパイルするとエラーになります。

```console
$ idris2 -p linear -c no_use.idr
1/1: Building no_use (no_use.idr)
Error: While processing right hand side of ignore. There are 0 uses of linear name a.

no_use:4:12--4:14
 1 | import Data.Linear.Notation
 2 |
 3 | ignore: a -@ ()
 4 | ignore a = ()
                ^^

Suggestion: linearly bounded variables must be used exactly once.
```

今度は線形型はちょうど一度使わないとダメだよと指摘してくれてますね。

この線形型の値を「消費」するにはパターンマッチで分解してあげればよいです。例えば以下のようにコンストラクタ `MkData` を持つデータ型 `Data` を定義します。それを受けとる `destruct` 関数は先ほどの `ignore` と同じことをしていますが、コンストラクタでパターンマッチして分解しているのでこのコードはコンパイルが通ります。

``` idris
import Data.Linear.Notation

data Data = MkData

destruct: Data -@ ()
destruct MkData = ()
```

逆にいうとプリミティブにはこれ以外の方法がありません。コンストラクタが非公開なデータ型は生成したら最後、コンパイルを通す手段がなくなります。
もちろん、そんなコードを書くと不便なだけなのでやる人はいません。コンストラクタを非公開にするならその終了処理関数とセットで提供します。まさしく上の例がそうですね。
それっぽく公開範囲などをいじるとこんな感じのコードになるでしょうか。

``` idris
import Data.Linear.Notation

-- export: 名前だけ公開する
-- public export: コンストラクタまで公開する
export
data Data = MkData

-- データの生成
export
newData: () -@ Data
newData () = MkData

-- データの破棄
export
destruct: Data -@ ()
destruct MkData = ()
```

上のコードはユーザには `Data` 、 `newData` 、 `destruct` しかみえていません。`newData` でデータを作り出し、必要がなくなったら `destruct` を呼び出して終了とします。
このようなパターンが活躍するシーンとしては終了処理に気を使うもの、例えばファイルディスクリプタとかそういうものを扱うときとかが挙げられます。Rustだとデータを無視すると暗黙に `drop` が呼ばれますが、そこで出たエラーを処理する方法がないのでエラーは無視されてしまいます。怖いですね。線形型ならユーザが必ずファイルを閉じる関数を呼ぶのでエラーは必ず通知されます。知らぬうちにエラーが出てるのに気づかず処理を継続してしまって思わぬバグになるなんてことを防げます。

さて、この線形型の終了処理を別の視点でみると、ユーザは`data` 型の値を受け取ったらどこかで `destruct` を呼び出さないといけない訳です。つまり、ある種の義務を押し付けることができると思いませんか？これを活用したパターンを義務オブジェクトパターンと呼んでみます。

本質的にはやってることは先の例と同じですが、例えばこんな感じに何か呼び出させたい関数 `callMe` があるとして、その関数を呼び出さないと消滅しないデータ型 `Obligation` を作ってあげると、その関数 `callMe` がどこかで呼ばれることが保証できます。

``` idris
export
data Obligation =  MkObligation

callFunc: () -@ Obligation
callFunc () = MkObligation

callMe: Obligation -@ ()
callMe MkObligation = () -- do something

-- ユーザ側
doSomething: Obligation -@ ()
doSomething obl = callMe obl -- 必ず callMeを呼ばないといけない
```

さらに、Type Stateパターンを組み合わせると複数の関数を呼ばないといけなくすることもできます。
例えばこんなコードになるでしょうか。

``` idris
import Data.Linear.Notation

-- 義務オブジェクトをたくさん用意する
export
data Yak = MkYak
export
data CatchedYak = MkCatchedYak
export
data SleepingYak = MkSleepingYak

export
newYak: () -@ Yak
newYak () = MkYak

-- 状態遷移関数たち
catchYak: Yak -@ CatchedYak
catchYak MkYak = MkCatchedYak

putYakSleep: CatchedYak -@ SleepingYak
putYakSleep MkCatchedYak = MkSleepingYak

shaveYak: SleepingYak -@ ()
shaveYak MkSleepingYak = ()

---
-- Yakを受けとった時点でcatchYak、putYakSleep、shaveYakを呼ばないといけないことが確定する
handleYak: Yak -@ ()
handleYak yak =
  let catched = catchYak yak in
  let sleeping = putYakSleep catched in
  shaveYak sleeping
```

義務オブジェクト `Yak` が `catchYak` を強制し、さらに返り値に別の義務オブジェクト `CatchedYak` を返します。今度は `CatchedYak` が `putYakSleep` を強制し、さらにまた別の義務オブジェト `SleepingYak` を返します。最後に `SleepingYak` が `shaveYak` を呼び出させて、ようやくユーザは一連の処理を終えられる訳です。義務オブジェクトとType Stateで3つの関数の呼び出しを特定の順番で呼び出すことを強制できました。

この義務オブジェクトの実用的なユースケースは例えばスレッド間通信をするときに別スレッドに義務オブジェクトを与えて決められた手順通りに通信することを保証するとかですかね(セッション型の亜種)。他にもハードウェアの操作だとか何らかの要因で決められた手順を踏まないといけないケースでのガードレールになると思います。

まあ、そもそも線形型が使える言語がほとんどないのでこのテクニックを実用する場面はないんですが、逆にこういう線形型でできる面白いことの知識が蓄積されると普及の後押しになるかなと思って書いてみました。
