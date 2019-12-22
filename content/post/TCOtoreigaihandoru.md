---
categories: [言語実装, SML]
date: 2015-05-31T21:02:08+09:00
title: TCOと例外ハンドル
---
κeenです。[先日のエントリー](/blog/2015/05/29/daiikkyuuraberuwomotanaigengoniokerudirect_threaded_vmnojissou)の最後でループ内で例外ハンドルをすると極端に遅くなるということを書きましたが、それについて。
<!--more-->
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">ループ内でExceptionをhandleしてる所為だった。ループの外に出したら超速になってインタプリタの方が20倍遅くなった。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/604235677337714689">2015, 5月 29</a></blockquote>

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> うーん、ちょっと例外のことは詳しくないんですが、予想だと例外ハンドラをループ内にいれると毎回スタックに積むんで外す操作がはさまることになるのでレジスタで完結してるようなループ処理だと露骨に遅くなるかもしれません</p>&mdash; Ocamlアイドル (@no_maddo) <a href="https://twitter.com/no_maddo/status/604537509771501569">2015, 5月 30</a></blockquote>

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> あーありえますね。ジャンプで済むところを戻ってきてスタックに積んだのを除去しないといけませんからね。ちょっと手元にマシンが無いのですがネイティブコードで実験してみますー</p>&mdash; Ocamlアイドル (@no_maddo) <a href="https://twitter.com/no_maddo/status/604540519188815872">2015, 5月 30</a></blockquote>

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> ocamlでも例外ハンドラのはさみ方で末尾再帰になったり、ならなかったりするというのは聞いたことがあるので多分あたりな気がします</p>&mdash; Ocamlアイドル (@no_maddo) <a href="https://twitter.com/no_maddo/status/604545844000325632">2015, 5月 30</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>


ということで実験してみましょう。


次のような何がしたいのか分からないループのベンチマークを取ってみます。


```sml
val () = let
    val arr = Array.array(1000000, 0)
    val i = ref 0
    fun loop () = (
        Array.update(arr, !i, 1);
        i := (!i) + 1;
        loop ()
    )
in
    Benchmark.benchmark "loop" 1 (fn () => 
                                     loop()
                                 )
end
```

結果は無限ループではなく

```
uncaught exception: Subscript
```

と、境界外アクセスの例外が出ます。因みにSubscripは添字って意味だそうです。

これではベンチマークがとれないので、例外が起きたらループを抜けるようにしてみましょう。


```sml
val () = let
    val arr = Array.array(1000000, 0)
    val i = ref 0
    fun loop () = (
        Array.update(arr, !i, 1);
        i := (!i) + 1;
        loop ()
    )
in
    Benchmark.benchmark "loop" 1 (fn () => 
                                     loop()
                                     handle Subscript => ()
                                 )
end
```

```
loop
 Time:
    [Total] 5 ms/1calls
  [Average] 5.0 ms/call
```


一瞬ですね。では、末尾呼び出し位置でhandleしてみます。

```sml
val () = let
    val arr = Array.array(100, 0)
    val i = ref 0
    fun loop () = (
        Array.update(arr, !i, 1);
        i := (!i) + 1;
        loop ()
        handle Subscript => ()
    )
in
    Benchmark.benchmark "loop" 1 (fn () => 
                                     loop()
                                 )
end
```

```
loop
 Time:
    [Total] 1729 ms/1calls
  [Average] 1729.0 ms/call
```

ふーむ。やはり大分遅くなってますね。しかしループ内で例外をハンドルした所為かもしれないのでループ内で非末尾位置で例外をハンドルしてみます。大域脱出に例外使いますがまあ、パフォーマンスに問題ないでしょう。

```sml
exception Exit
val () = let
    val arr = Array.array(1000000, 0)
    val i = ref 0
    fun loop () = (
        Array.update(arr, !i, 1)
        handle Subscript => raise Exit;
        i := (!i) + 1;
        loop ()
    )
in
    Benchmark.benchmark "loop" 1 (fn () => 
                                     loop()
                                     handle Exit => ()
                                 ) 
end
```

```
loop
 Time:
    [Total] 11 ms/1calls
  [Average] 11.0 ms/call
```

やはり多少は遅くなってますが末尾位置の時のように極端には遅くなってないようです。

最後に非末尾再帰ループの速度を測っておきましょう。


```sml
val () = let
    val arr = Array.array(1000000, 0)
    val i = ref 0
    fun loop () = (
        Array.update(arr, !i, 1);
        i := (!i) + 1;
        1 + (loop ())
    )
in
    Benchmark.benchmark "loop" 1 (fn () => 
                                     loop()
                                     handle Subscript => 0
                                 ) 
end
```


```
loop
 Time:
    [Total] 403 ms/1calls
  [Average] 403.0 ms/call
```

思ったより遅いですね。ループか末尾例外ハンドルかというと末尾例外ハンドルに近いスコア。

# 考察
末尾位置で例外をハンドルすると遅くなる原因はTCOが効かないから、で合ってそうです。

しかしそれにしても遅いですね。例外ハンドラをスタックに積むのが1関数呼び出しくらいならせいぜい倍くらいの遅さで済む筈です。
もしかしたらループ展開とかの外の最適化も掛からなくなるのかもしれません。

# 結論
例外をハンドルする時は位置に気をつけましょうね。

# 付録A
ベンチマーカはこんなコードです。

```sml
structure Benchmark =
struct
fun repeat 0 f = ()
  | repeat n f =  (f ();repeat (n - 1) f)
 
fun bench n f = let
    val startTime = Time.now ()
    val _ = repeat n f
    val endTime = Time.now ()
in
    Time.toMilliseconds (Time.-(endTime, startTime))
end
 
fun benchmark name n f = let
    val time = bench n f
in
    print (name ^ "\n");
    print (" Time:\n");
    print ("    [Total] " ^ (LargeInt.toString time) ^ " ms/" ^ (Int.toString n) ^ "calls\n");
    print ("  [Average] " ^ (Real.toString((Real.fromLargeInt time) / (Real.fromInt n))) ^ " ms/call\n")
end

fun nChars n char = CharArray.vector(CharArray.array(n, char))

fun toWidth width str = let
    val len = String.size str
in
    if len < width
    then str ^ (nChars (width - len) #" ")
    else str
end

fun histLine width base value =
  (nChars (Int.fromLarge(width * value div base)) #"*") ^ "\n"

fun benchset name n fs = let
    val res = List.map (fn (label, f) => (label, bench n f)) fs
    val max = List.foldl (fn ((_, time), m) => LargeInt.max(time, m)) 0 res
    val maxLen = List.foldl (fn ((label, _), m) => Int.max(String.size label,  m)) 0 fs
in
    print "name:\n";
    print ((nChars ((String.size " ") + maxLen) #"-") ^ "+" ^ (nChars ((String.size "|") +  50) #"-") ^ "\n");
    app (fn (label, time) => print(" " ^ (toWidth maxLen label) ^ "|" ^(histLine (50:LargeInt.int) max time))) res;
    print ((nChars ((String.size " ") + maxLen) #"-") ^ "+" ^ (nChars ((String.size "|") +  50) #"-") ^ "\n")
end

end
```

# 付録B
元々、なんでこの問題が生じたかというと一々境界チェックして配列にアクセスするより例外出させといた方が速いんじゃね？ってことでそういうコードを書いたからです。
例外が出るってことは内部でも境界チェックしてる筈ですから。

ということでどちらが速いか確認してみましょう。

まず例外ハンドル方式。先程のままだと数ミリ秒で終わってたので配列の大きさを10倍しました。あと。実際に書きそうな書き方に変えました。


```sml
val () = let
    val len = 10000000
    val arr = Array.array(len, 0)
    fun loop i = (
        Array.update(arr, i, 1);
        loop (i + 1)
    )
in
    Benchmark.benchmark "loop" 1 (fn () => 
                                     loop 0
                                     handle Subscript => ()
                                 ) 
end
```

```
loop
 Time:
    [Total] 48 ms/1calls
  [Average] 48.0 ms/call
```

まあ、こんなもんですね。

次にifで分岐するやりかた。

```sml
val () = let
    val len = 10000000
    val arr = Array.array(len, 0)
    fun
    loop i = if i < len
             then (
                 Array.update(arr, i, 1);
                 loop (i + 1))
             else ()
in
    Benchmark.benchmark "loop" 1 (fn () => 
                                     loop 0
                                     handle Subscript => ()
                                 ) 
end
```


```
loop
 Time:
    [Total] 96 ms/1calls
  [Average] 96.0 ms/call
```

倍くらい遅くなってますね。

ということでみだりに境界チェックするより例外を出させといた方が速いようです。
