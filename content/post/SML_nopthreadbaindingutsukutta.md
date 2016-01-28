---
categories: [SML, SMLSharp, ML]
date: 2016-01-11T00:57:20+09:00
title: SML#のpthreadバインディング作った
---

κeenです。またしても深夜になってしまいましたがSML#のpthreadバインディングを作ったのでその報告を。

<!--more-->
SML#はC FFIを簡単に書け、さらにランタイムがnon Moving GCなのでSML#自体はサポートしていないもののランタイムそのものはマルチスレッドに対応しているという大変興味深い特徴を持っています。
処理系側としてもマルチスレッドが欲しいならpthreadのFFIを使って好きにやってくれというスタンスのようです。

ということでさまざまなマルチスレッドアプリケーションの下地になるべくpthreadのバインディングを作りました。とはいってもまだ不完全ですが。

どういうものが用意出来ているかは[こちら](https://github.com/KeenS/smlsharp_pthread/blob/master/src/pthread.smi)が一覧として機能しています。

SML的には改善の余地がある(例えば、排他的なフラグをdatatypeで定義するとか)のですが、あくまで低レベルなバインディングに徹するためにintのままで残してあります。

他にはCのマクロで実現されていたものはインポート出来ないのでどうにかしてあったりどうにかしてなかったりしてます。
例えば`PTHREAD_XXX_INITIALIZER`はマクロで定義された初期値ですが、SMLからは参照出来ないのであきらめました。
代わりに`pthread_xxx_new`関数を用意したので`pthread_xxx_init`と併せて使うことでそれらの値を初期化出来ます。
メモリ確保と初期化同時にやれよとの声が聞こえてきそうですが繰り返すとあくまで低レベルなバインディングなのでそういうのは他に譲ることにします。

`pthread_cleanup_{pop/push}`はバインディングが書けませんでした。
それらはマクロとして実現されていて、しかも`pthread_cleanup_push`で開き括弧を挿入して`pthread_cleanup_pop`で閉じるというエグい実装なので移植は無理でしょう。
POSIX的にもマクロとして実現して良いことになっているので文句は言えません。

# サンプルコード
レポジトリにも入ってますが、スレッドを作る小さな例だとこうなります。

``` sml
open PThread
fun exit reason = (
    print (reason ^ "\n");
    OS.Process.exit OS.Process.failure
)


val fromUnitPtr = SMLSharp_Builtin.Pointer.fromUnitPtr
val toUnitPtr = SMLSharp_Builtin.Pointer.toUnitPtr
(* durty hack *)
val sml_str_new = _import "sml_str_new": (string) -> char ptr

fun threadFunc (arg:unit ptr): unit ptr = let
    val cp: char ptr = fromUnitPtr arg
    val s = Pointer.importString cp
    val () = print s
    (* val ret =ref _NULL *)
    (* val () = ret := (String.size s) *)
in
    (* ret *)
    _NULL
end
                   
val () = let
    val tattr = ref (pthread_attr_new())
    val s = pthread_attr_init(tattr)
    val () = if s <> 0
             then exit "pthread_attr_init"
             else ()
    val thread_ref = ref (pthread_new())
    val arg = sml_str_new "Hello world\n"
    val s = pthread_create(thread_ref, tattr, threadFunc, toUnitPtr arg)
    val t1 = !thread_ref
    val () = if s <> 0
             then exit "thread creation failed"
             else ()
    val s = pthread_attr_destroy(tattr)
    val () = if s <> 0
             then exit "pthread_attr_destroy"
             else ()
    val () = print "Message from main()\n";
    val resRef = ref (Pointer.NULL ())
    val s = pthread_join(t1, resRef)
    val () = if s <> 0
             then exit "thread creation failed"
             else ()
    (* val () = print ("Thread returned" ^ Int.toString  (!resRef) ^ "\n") *)
in
    ()
end
```

言わずとも雰囲気で読み取れそうですがこの例はCのサンプルをそのまま移植しました。Cだと`(void *)`と文字列で相互変換が出来るのですがSML#だと一筋縄では出来ないので内部APIを叩くとかのかなりアレなハックしてます。
[SML#にイシューに上げ](https://github.com/smlsharp/smlsharp/issues/45)ましたが`(void *)`が強敵ですね。


# 可搬性の話
ところで、pthreadはp(POSIX)の名が付いている通り様々なプラットフォームで利用出来、そしてそれぞれのプラットフォーム毎に実装が異なります。
勿論、POSIXで定められているのでAPIレベルでは互換性がある(≒関数のバインディングは問題ない)のですが、ABI、データの表現に互換性がありません(≒データ型のバインディングに問題がある)


実はその辺で苦労がありました。今のところ手元の環境(Linux)でしか移植が済んでません。しかし他のプラットフォームの移植は書ける形になっています。
これは[以前](//KeenS.github.io/blog/2015/12/26/sml_tocpuripurosessanorenkei/)書いたSMLのファイルにCのプリプロセッサを適用するというなんともいえないハックを使うことで実現しています。
他のプラットフォームに移植するには

1. どういうマクロが定義されている時にどのプラットフォームのpthreadを使っているかの対応を調べる
2. そのプラットフォームの`pthread.h`を入手してデータ型を移植する

手順が必要です。私は手元の環境で動かすのが精一杯なのでMacとかで動かしたい方がいればプルリクを頂けると。
前述の通り、関数のバインディングは可搬性があるので必要なのはデータ型と定数の移植です。ある程度はプリプロセッサも使えるのでほぼヘッダファイルを移植する感じですね。

まあ、実をいうと関数の移植も細々したものが面倒なので放置していたりします。ここら辺は作業ゲーなので気が向いたらやります。

あとは若干迷っているのが必ずしも実装されているとは限らないオプショナルな機能の移植ですね。
バリアやリードライトロック、スピンロックがそれにあたります。
可搬性とはいってもSML#自身が動く環境が限られているのでMacでもそれらが使えるなら移植してしまおうかと思っていますがまだ調べきれてません。


# まとめ

* pthreadのバインディング作ったよ
* まだバインドが書かれてない関数もあるよ
* Macの移植パッチ待ってるよ
