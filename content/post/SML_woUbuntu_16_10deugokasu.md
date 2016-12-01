---
categories: [SML, smlsharp, Advent Calendar, Advent Calendar 2016]
date: 2016-11-30T15:18:58+09:00
title: SML#をUbuntu 16.10で動かす
---

この記事は[ML Advent Calendar 2016 - Qiita](http://qiita.com/advent-calendar/2016/ml)の1日目の記事です。

κeenです。小ネタを。Ubuntu 16.10でSML#を動かそうと思ったら動かなかったので動かします。

<!--more-->

# 現象

リンカがエラーを吐く。バイナリが正しく作れてなさそう


```
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(callback.o): relocation R_X86_64_32 against `.text' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(control.o): relocation R_X86_64_32 against `.text' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(error.o): relocation R_X86_64_32 against `.bss' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(exn.o): relocation R_X86_64_32 against `.rodata.str1.8' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください 。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(finalize.o): relocation R_X86_64_32 against `.text' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(init.o): relocation R_X86_64_32 against `.rodata.str1.1' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(object.o): relocation R_X86_64_32 against `.rodata.str1.1' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(xmalloc.o): relocation R_X86_64_32 against `.rodata.str1.1' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてくだ さい。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(prim.o): relocation R_X86_64_32 against `.rodata' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(top.o): relocation R_X86_64_32 against `.text' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(dtoa.o): relocation R_X86_64_32 against `.rodata.str1.1' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a(heap_concurrent.o): relocation R_X86_64_32S against `.bss' can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
/usr/bin/ld: 最終リンクに失敗しました: 出力に対応するセクションがありません
collect2: error: ld returned 1 exit status
uncaught exception: CoreUtils.Failed: gcc -Wl,-Bsymbolic-functions -Wl,-z,relro test/Main.o /tmp/tmp.IhaEmV/000/tmp_000.a lib/socket.o lib/inet.o lib/net_host_db.o /usr/lib/x86_64-linux-gnu/smlsharp/runtime/main.o /usr/lib/x86_64-linux-gnu/smlsharp/runtime/libsmlsharp.a -lpthread -lyajl -ldl -lgmp -lm  -o testRunner at src/compiler/toolchain/main/CoreUtils.sml:113
Makefile:50: ターゲット 'testRunner' のレシピで失敗しました
```

# 原因
私もよく分かってないのですが、Ubuntu 16.10からGCC 6系になりましたが、そこでデフォルトの挙動が変わったらしいのでその辺らしいです。

# 対策
たいていの言語で `-no-pie` を付けることで解決しています。 Position Independent Executableだっけ？

アドホックにやる方法と恒久的に解決する方法とがあります。

## アドホックな方

簡単で、SML#で **リンクする時** に `-Xlinker -no-pie` を付けてあげます。

## 恒久的な方

SML#を自前でビルドします。そのとき **configure時** に `LDFLAGS=-no-pie` を付けてあげます。

# 余談

`-no-pie` はGCC 6以降で、それ以前は `-nopie` との噂をききましたがGCC 6系しか手元にないので真偽のほどは分かりません。
