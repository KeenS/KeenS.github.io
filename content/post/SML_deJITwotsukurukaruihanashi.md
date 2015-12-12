---
categories: [SML, SMLSharp, Advent Calendar, Advent Calendar 2015, 言語処理系]
date: 2015-12-12T17:26:24+09:00
title: SML#でJITコンパイラを作る軽い話
---

このエントリは[ML Advent Calendar 2015](http://www.adventar.org/calendars/848)及び[言語実装 Advent Calendar 2015](http://qiita.com/advent-calendar/2015/lang_dev)15日目の記事です。  
MLの前:   
言語実装の前:   

κeenです。先日流れてきた[RustでJITコンパイラを作る話](http://www.jonathanturner.org/2015/12/building-a-simple-jit-in-rust.html)が面白かったのでSML#でもやってみます。

<!--more-->
# JITコンパイラってなに？
JITはJust In Timeで、日本語にすると「間に合って」になります。誤読されかねませんが「臨時コンパイラ」と訳すのが適当なのでしょうか。

普通のインタプリタがソースコードを直接評価するのに対してJITコンパイラはソースコードを内部で一旦ネイティブコードにコンパイルして実行します。

よく、JITコンパイラ/JITコンパイルのことをJITと略して呼びます。

# どこで使うの？
主に、インタプリタの高速化の目的で使われるようです。他にはコンパイラをインタプリタ化させる(REPLの実装とか)でも使えそうな気がしますが、詳しくは知りません。

# JITってどうやって作るの？
JIT **でない** コンパイラが吐いたコードは一旦ディスクからメモリに読まれ、メモリ上で実行されます。

JITコンパイラが吐くコードは直接メモリに吐かれ、メモリ上で実行されます。なので細かい条件を無視すれば

1. 吐かれる命令のための(特殊な)メモリ領域を用意する
2. そこに実行したい命令列を書き込む
3. 書き込んだ命令列を実行する

だけです。
3.の「書き込んだ命令を実行する」が若干ふわっとしてますが、多くの場合は命令の書かれたメモリを関数ポインタとして扱って、その関数を呼び出す形で実行されます。
一応直接プログラムカウンタをいじって書き込んだ命令列を実行させることも出来ます。


# 一緒に作ってみよう
思ったより簡単そうですね。作ってみましょう。

## 1. メモリ領域を準備する
先に「特殊な」と付言しましたのでそれについて説明しましょう。プロセス上のメモリは、OSによって保護されています。
特に、そのままではそのメモリ上の命令列を実行出来ません。その保護を外すために`mprotect(2)`システムコールを使います。

``` C
int mprotect(void *addr, size_t len, int prot);
```

SML#を使えば簡単に使えるようになります。

``` sml
val mprotect = _import "mprotect": (unit ptr, word, word) -> int
```

SML#は2.0.0時点では32bitでしか動かないのでsize_tをwordとしてしまって大丈夫です。また、bit演算をやりたいので`prot`は本来はintですがwordにしました。

`mprotect(2)`はPOSIX(SUSv4)的には引数の`addr`がシステムのページの境界上にあることを要求しても良いことになっています。
実際、OS Xが要求するらしいです。また、メモリ保護はページ単位で行われるため、`len`もページサイズの倍数である必要があります。
`prot`は保護フラグですね。

メモリの確保、特にページの境界にアラインメントされたメモリ領域を確保するにはSML#では厳しそうなのでこれもCの関数に頼ります。
Linuxならいくつか方法はありますが、ポータブルには`posix_memalign(3)`を使うのが良いようです。

``` C
int posix_memalign(void **memptr, size_t alignment, size_t size);
```

これもSML#から簡単に使えます。


``` sml
val posix_memalign = _import "posix_memalign": (unit ptr ref, word, word) -> int
```

ついでにメモリの解放も必要ですね。`free(3)`もインポートしましょう。

``` sml
val free = _import "free": unit ptr -> ()
```

さて、メモリを確保するのにいくつかCのマクロで定義された定数を定義しましょう。今のところSML#にCのマクロを使う術はなさそうです。どうにかしてCプリプロセッサを使えれば良いのですが…。

``` sml
val PROT_READ  = 0wx1
val PROT_WRITE = 0wx2
val PROT_EXEC  = 0wx4
val PROT_NONE  = 0wx0

val PAGE_SIZE = 0w4096
```

ページサイズは本当は`sysconf(3)`を使って`sysconf(PAGESIZE)`として取るのが作法ですが面倒なので4KiB決め打ちにしてしまいました。


さて、これらを用いてJIT用のメモリは次のように確保します。`Pointer`モジュールが必要なのでsmiファイルで`require "ffi.smi"`しましょう。


``` sml
val op orb = Word.orb
infix 5 orb
val size = 0wx1
val msize = size * PAGE_SIZE
val pageRef: unit ptr ref = ref (Pointer.NULL ())
val ret = posix_memalign (pageRef, PAGE_SIZE, msize)
val page = if ret = 0
           then !pageRef
           else raise Fail "memory allocation failed"
val PROT_RWEX = PROT_READ orb PROT_WRITE orb PROT_EXEC
val ret = mprotect (page, msize, PROT_RWEX)
val () = if ret = 0
         then ()
         else raise Fail "memory protection error"
```

これで`page`に実行可能フラグが立ったメモリが確保出来ました。sizeは書き込みたい命令列に応じて変えます。


# 2. 命令列を書き込む

さて、ここらへんからアセンブラの世界になってきます。

まず、安全のために件のメモリ領域を`ret`命令で埋めます。
`ret`命令は関数から戻る時に使われる命令で、変なことをしても`ret`で元の場所に戻ればどうにかなる可能性があるのです。

メモリを一気に埋めるには`memset(3)`が便利です。

``` C
void *memset(void *s, int c, size_t n);
```

これをSMLにインポートして使いましょう。`ret`命令に対応するマシン語は`0xc3`です。

``` sml
val memset = _import "memset": (unit ptr, word, word) -> unit ptr
val _ = memset (page, 0wxc3, msize)
```

じゃあ、命令を書き込んでいきましょう。最初はただ1を返すだけの関数に相当する命令です。
64bitマシンを使っていても32bitでコンパイルしたSML#を使っている限りはx86(IA32)のアセンブラを使います。
x86は値を`eax`レジスタに入れることで返り値とします。

``` asm
mov 1 eax
ret
```

このコードを[オンラインアセンブラ](https://defuse.ca/online-x86-assembler.htm#disassembly)でアセンブルしてみると

```
0:  b8 01 00 00 00          mov    eax,0x1
5:  c3                      ret
```

との結果が返ってきました。これを真心込めて1つづつメモリに書き込んでいきます。
SML#からメモリに直接書き込むには内部APIっぽいものを触る必要があります。
タイプ数を減らすためにいくつかエイリアスを定義しておきましょう。


``` sml
val fromUnitPtr = SMLSharp_Builtin.Pointer.fromUnitPtr
val store = Pointer.store
val advance = Pointer.advance
```

メモリにword8の値を書き込むのにキャストが必要になるので`fromUnitPtr`を使います。
さて、書き込んでいきます。

``` sml
val mem = fromUnitPtr page
val () = store page 0wxb8
val mem = advance mem
val () = store page 0wx01
val mem = advance mem
val () = store page 0wx00
val mem = advance mem
val () = store mem 0wx00
val mem = advance mem
val () = store mem 0wx00
val mem = advance mem
val () = store mem 0wxc3
val _ = advance mem
```

これで`page`に1を返すだけの命令列が書き込まれした。

## 3. 書き込んだ命令列を実行する
書き込んだ命令列は関数にキャストして使うことが多いと言いました。今回もそのようにします。

そのために、また内部APIっぽいものを使います。


```sml
val toCodeptr = SMLSharp_Builtin.Pointer.toCodeptr
```

`codeptr`型は関数ポインタを表すもののようです。`DynamicLink`モジュールなどで使われています。

さて、今回の1を返すだけの命令列は`() -> int`型になりますね。
それをインポート、実行しましょう。


``` sml
val freturn1 = toCodePtr page: _import () -> int
val () = print ((Int.toString (freturn1 ())) ^ "\n")
val _ = free page
```

これで見事`1`が印字されたら成功です。

# さらに

先程のコードは繰り返しが多く、整理されていませんでした。もう少し整理しましょう。

``` sml
structure Emit = struct
    val posix_memalign = _import "posix_memalign": (unit ptr ref, word, word) -> int
    val mprotect = _import "mprotect": (unit ptr, word, word) -> int
    val memset = _import "memset": (unit ptr, word, word) -> unit ptr
    val free = _import "free": unit ptr -> ()
    val printf = _import "printf": (string, unit ptr) -> ()

    (* 
#define PROT_READ	0x1		/* Page can be read.  */
#define PROT_WRITE	0x2		/* Page can be written.  */
#define PROT_EXEC	0x4		/* Page can be executed.  */
#define PROT_NONE	0x0		/* Page can not be accessed.  */
    *)

    val PROT_READ  = 0wx1
    val PROT_WRITE = 0wx2
    val PROT_EXEC  = 0wx4
    val PROT_NONE  = 0wx0

    val PAGE_SIZE = 0w4096

    type jitptr = word8 ptr
    val fromUnitPtr = SMLSharp_Builtin.Pointer.fromUnitPtr
    val toUnitPtr = SMLSharp_Builtin.Pointer.toUnitPtr
    val toCodeptr = SMLSharp_Builtin.Pointer.toCodeptr
    val store = Pointer.store
    val advance = Pointer.advance


    fun jitMemory size: jitptr = let
        val op orb = Word.orb
        infix 5 orb
        val msize = size * PAGE_SIZE
        val pageRef: unit ptr ref = ref (Pointer.NULL ())
        val ret = posix_memalign (pageRef, PAGE_SIZE, msize)
        val page = if ret = 0
                   then !pageRef
                   else raise Fail "memory allocation failed"
        val PROT_RWEX = PROT_READ orb PROT_WRITE orb PROT_EXEC
        val ret = mprotect (page, msize, PROT_RWEX)
        val () = if ret = 0
                 then ()
                 else raise Fail "memory protection error"
        (* init with ret for safety *)
        val _ = memset (page, 0wxc3, msize)
    in
        fromUnitPtr page
    end

    fun freeJit (jitMem: jitptr) = free (SMLSharp_Builtin.Pointer.toUnitPtr jitMem)

    fun pushWord page (word: word8) = (store (page, word); advance (page, 1))
    fun pushWords (page: jitptr) l = List.foldl (fn(w,page) => pushWord page w) page l

    val import: jitptr -> codeptr = toCodeptr o toUnitPtr

    fun fromMachineCode l = let
        val len = Word.fromInt(List.length l)
        val size = (len + PAGE_SIZE) div PAGE_SIZE
        val page = jitMemory size
        val _ = pushWords page l
    in
        import page
    end
end
```

次のように使います。一回importした関数はfreeしないことにしましょう。

``` sml
fun println x = print (x ^ "\n")
val return1  =
    (* 0:  b8 01 00 00 00          mov    eax,0x1  *)
    [
      0wxb8, 0wx01, 0wx00, 0wx00, 0wx00
    ] 
val freturn1 = Emit.fromMachineCode return1 :_import () -> int
val () = println (Int.toString (freturn1 ()))
```

## 引数を取る
もうアセンブラの話になります。

x86では引数は右から順にスタックに積まれます。intを1つ取ってそれに1足して返す関数はこうなります。


``` sml
val add1 = 
    (* 0:  8b 44 24 04             mov    eax,DWORD PTR [esp+0x4] *)
    (* 4:  83 c0 01                add    eax,0x1 *)
    [
      0wx8b, 0wx44, 0wx24, 0wx04,
      0wx83, 0wxc0, 0wx01
    ]
val fadd1 = Emit.fromMachineCode add1 :_import (int) -> int
val () = println (Int.toString (fadd1 3))
```

正常に動けば4が印字されます。

# つらい話
## バグ
最初、`Pointer.store`でなく、`SMLSharp_Builtin.Pointer.store`を使っていたら[変なバグ](https://github.com/smlsharp/smlsharp/issues/43)踏みました。バグというか使い方が悪かった。

## デバッグ
デバッガがないので非常につらいです。書き出された命令列を見るのに困りました。gdbで見たかったので、

``` sml
val printf = _import "printf": (string, unit ptr) -> int
val _ = pritnf ("page pointr: %p", page)
```

して`printf`にブレークポイントを張り、そこで止めつつページのアドレスを取得、

```gdb
(gdb) x/20xh 0x81ca000
```

などとして見ていました。

# もっと

JITコンパイラが楽しかったのでアセンブラのDSLを作ってみました。1、2個の命令吐けるだけですがちゃんと動きました。今のところこのように書けます。Intel記法だとしんどそうだったのでAT&amp;T記法っぽく書けるようにしました。


``` sml
val freturn1' = Emit.fromInsts [
        xorl eax eax,
        addl ($1) eax,
        ret
    ]:_import () -> int
val () = println (Int.toString (freturn1' ()))
```

x86は命令フォーマットが1バイトから15バイトまでの可変長で、内部表現をどのようにするか決めるだけでも一苦労でした。x86つらい。

アドレッシングが複雑なのも悩みどころで、複数のアドレッシングを統一的に扱えるようにオーバーロードされた関数を用意したのですがあえなくSML#のバグを踏んで死亡しました。本当はこういう記法が出来る筈だった…

``` sml
addl (%eax) eax
addl eax (%eax)
addl eax (%(eax, ebx))
addl eax (%(eax, ebx, 4))
addl eax (%(4, eax))
```

```
$ /usr/local/bin/smlsharp   -c -o main.o main.sml
uncaught exception: Bug.Bug: InferType: FIXME: user error: invalid instTy at src/compiler/typeinference2/main/InferTypes2.sml:47
Makefile:11: recipe for target 'main.o' failed
make: *** [main.o] Error 1
```

`movl`を実装しようとしたらアドレッシングが動かなかったので萎えてまだ実装してません。

よく考えたら`%`ってミスリーディングだし名前変えよう。


# まとめ

* JITを作るにはメモリ保護をいじれて関数ポインタのインポートが出来ればいいよ
* SML#でもJIT作れるよ
* アセンブラっぽいの作ったよ
