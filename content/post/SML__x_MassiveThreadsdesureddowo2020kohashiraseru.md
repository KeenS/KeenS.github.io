---
categories: ["SML", "SMLSharp", "MassiveThreads"]
date: 2020-01-04T00:36:19+09:00
title: "SML# x MassiveThreadsでスレッドを2020個走らせる"
---
κeenです。2020年ですね。スレッドを2020個走らせてみましょう。

<!--more-->

# MassiveThreadsとは

[これ](https://github.com/massivethreads/massivethreads) 。東大の[田浦研](https://www.eidos.ic.i.u-tokyo.ac.jp/)が開発している超軽量スレッドライブラリで、通常よりも2桁高速にスレッドを作れるらしいです。

APIはだいたいpthreadに似てます。

# SML#とMassiveThreads

SML# 3.3.0からデフォルトでMassiveThreadsを[使うようになりました](https://www.pllab.riec.tohoku.ac.jp/smlsharp/docs/3.3.0/ja/Ch10.S2.xhtml)。
また、最近リリースされたSML# 3.5.0からpthreadとMassiveThreadsを同時に使うことができるようになりました。
SML#でスレッドをじゃぶじゃぶ扱える訳です。
もうそろそろ64コア128スレッドのThreadripper 3990Xも出るようですしスレッド沢山作れると嬉しいですよね。

ということでナチュラルにSML#からMassiveThreadsを使ってみましょう。

# API

ベラっと張るとこうなっています。

```sml
structure Myth =
struct

  type thread (= *)
  structure Thread =
  struct
    type thread = thread
    val create : (unit -> int) -> thread
    val detach : thread -> unit
    val join : thread -> int
    val exit : int -> unit
    val yield : unit -> unit
    val self : unit -> thread
    val equal : thread * thread -> bool
  end

  type mutex (= array)
  structure Mutex =
  struct
    type mutex = mutex
    val create : unit -> mutex
    val lock : mutex -> unit
    val unlock : mutex -> unit
    val trylock : mutex -> bool
    val destroy : mutex -> unit
  end

  type cond (= array)
  structure Cond =
  struct
    type cond = cond
    val create : unit -> cond
    val signal : cond -> unit
    val broadcast : cond -> unit
    val wait : cond * mutex -> unit
    val destroy : cond -> unit
  end

  type barrier (= array)
  structure Barrier =
  struct
    type barrier = barrier
    val create : int -> barrier
    val wait : barrier -> bool
    val destroy : barrier -> unit
  end

end

```

See on [GitHub](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/Myth.smi)

FFIなのでfinalizerである `join` や `destroy` は自分で呼ぶ規約です。

ところで、これだけだと使いづらいのでいくつかスレッドライブラリの上に構築されたライブラリもあります。[Concurrent](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/Concurrent.smi)や[CML](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/CML.smi)など。
これらも使おうとしたのですが色々あって使ってません。気になる人は使ってみて下さい。

# QuickSort

良い題材が全く思い浮かばなかったのでQuickSortを実装してみます。

まずはインタフェースファイルに以下を書きます。

```sml
_require "basis.smi"
_require "thread.smi"
```

そして並列実行用の関数 `join` を定義しておきましょう。[rayonの `join`](/blog/2018/04/08/rayonnoshinkahabunkatsutouchiniari/)を意識してみました。

```sml
structure Thread = Myth.Thread

fun join (f1, f2) = let
    val chan = ref NONE
    val th = Thread.create (fn () => 0 before chan := SOME(f2 ()))
    val r1 = f1 ()
    val _ = Thread.join th
    val r2 = Option.valOf $ !chan
in (r1, r2) end
```

マルチスレッドで `ref` を使ってるのが気持ち悪いですが一応競合状態にはならないはずなので大丈夫でしょう。

さて、これを使ってQuickSortを書いていきます。
関係ない関数はバッサり省いて以下のように書けますね。

```sml
fun qsortSlice arr =
    if ArraySlice.length(arr) <= 1
    then ()
    else let
        open ArraySlice
        val (l, h) = partition arr
    (*          vvvv  ここで `join` を使った*)
    in ignore $ join((fn () => qsortSlice l), (fn () => qsortSlice h)) end

fun qsort arr = qsortSlice (ArraySlice.full arr)
```

普通のアルゴリズムの1箇所を書き換えるだけで並列化できます。

しかしまあ、これだと遅いので要素数が少なくなったらシングルスレッドにしたり挿入ソートにしたりしましょう。
それが以下のコード。

```sml
val cutOff = 50
val singleThread = 400

fun qsortSlice arr =
    (* 要素数が少なければ挿入ソートに変更 *)
    if ArraySlice.length(arr) <= cutOff
    then insertionSortSlice arr
    else let
        open ArraySlice
        val (l, h) = partition arr
        (* 要素数に応じてスレッドを使うかを変える *)
        val join = if length(arr) <= singleThread then joinSingle else join
    in ignore $ join((fn () => qsortSlice l), (fn () => qsortSlice h)) end
```

ここで、 `insertionSortSlice` は挿入ソートをする関数で、 `joinSingle` は `join` のシングルスレッド版、つまり `fun joinSingle (f1, f2) = (f1(), f2())` です。
`cutOff` や `singleThread` のパラメータは何度か走らせてチューニングしたものです。

それでは、実行してみましょう。
雑な線形合同法で初期化した配列をソートしてみます。

```sml
fun rng seed = let
    open Word32
    val seed = fromInt seed
    val state = ref seed
    fun next () = let val v = (!state * 0w2017 + 0w2020) mod 0w2027
                  in toInt v before state := v end
in next end

val rand = rng 7
val n = case CommandLine.arguments () of
            [] => 2020
          | arg::_  => Option.valOf $ Int.fromString arg
val arr = Array.tabulate(n, fn i => rand () + 1)
val () = qsort arr

```

このコードを `-O3` をつけてコンパイルしてみます。

```console
$ smlsharp -O3 -o qsort qsort.sml
```

デフォルトではMassiveThreadsのワーカが1つしか立ち上がらないのでコア数分立ち上がるようにしながら実行します。
Ubuntu 19.10、16コア32スレッドのマシンで `time` で計測した結果が以下。

```console
$ time MYTH_NUM_WORKERS=0 ./qsort 808080
MYTH_NUM_WORKERS=0 ./qsort 808080  93.84s user 1.36s system 1244% cpu 7.648 total
```

7.6秒くらいで終わりました。
配列がおおむねランダム、400要素ごとに並列化しているので808080要素の配列をソートするとざっくり2020個のスレッドが立ち上がってるはずです。

他のパラメータでもやってみましょう。 `MYTH_NUM_WORKERS` を設定せずに1並列で実行してみます。

```console
$ time ./qsort 808080
./qsort 808080  28.55s user 0.76s system 117% cpu 24.884 total
```

フルの並列の3倍くらいの時間ですかね。16コアあるのに3倍は世知辛い。

ところでスレッドを立てるのにもコストがかかります。スレッドを一切立てない(= `val singleThread = 808080`)で実行してみましょう。

```console
$ time ./qsort 808080
./qsort 808080  8.29s user 0.05s system 100% cpu 8.293 total
```

んー、速い。スレッドを立てるだけでかなりのオーバーヘッドがあるのが分かりますね。

とはいえそもそもスレッドを2000個も立てるの自体普通はやりませんし、まともに動きません。
むしろ、2000個もスレッドを立てた上に何もしないコードより速いと見るべきでしょう。

ということでゆるーくMassiveThreadsを触ってみました。

今回のコードは[こちら](https://gist.github.com/KeenS/a7a353ef240486ac9aad6da029fecd1c)に置いておきます。

# 余談

当初はCMLなどの高レベルなAPIを触ろうとしてたのですがやめて、シンプルに `Myth` を触る方向に舵を切りました。
`select` を使ってスリープソート的なことでもしようかと思ったのですが `CML.sameChannel` や `CML.timeOutEvt` が未実装だったり、 `Concurrent` の `Mvar` などを使うと遅くなったりしてやめました。
Goくらい雑にスレッドとチャネルを使えると嬉しいんですが。あとIOは勝手にスケジューリングしてくれるんですかね？

あと全然関係ないですがGCがMassiveThreadsのスタックをどう見にいってるのか気になってます。
