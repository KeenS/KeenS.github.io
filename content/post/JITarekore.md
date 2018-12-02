---
categories: ["言語処理系", "言語実装 Adevnt Calendar", "言語実装 Adevnt Calendar 2018", "Adevnt Calendar", "Adevnt Calendar 2018"]
date: 2018-12-01T19:20:40+09:00
title: "JITあれこれ"
---
κeenです。遅刻してしまいましたがこのエントリーは [言語実装 Advent Calendar 2018](https://qiita.com/advent-calendar/2018/lang_dev) 1日目の記事です。
最近私の観測範囲内でJITが流行っているのですが一口にJITと言っても色々あるよなーと思ったので私がJITについて知っていることをグダクダ話します。
<!--more-->

このブログでも何度がJITや周辺技術について取り上げてますが話の流れがスムーズになるので最初から説明していきます。

2018-12-03: 加筆修正しました。差分は[こちら](https://github.com/KeenS/KeenS.github.io/commit/55eea979857879be6f09ef7c52836c29ebedb725#diff-4ca9c4059b3670fab6b5d3a3e1eecc36)

# JITって？
Just in Time(コンパイル)のことで、日本語にすると「間に合ってコンパイル」になりますかね。
インタプリタの高速化テクニックの1つです。
最初はインタプリタのようにコードをコンパイルせずプロセスが起動しますが、メソッドを実行するまでにはメソッドをコンパイルして、ネイティブコードで実行する方式です。

本来ならJITはこのような意味なのですがここではもう少し範囲を広めに取って実行プロセス起動後にコードを生成して実行するものを全部JITと呼ぶことにします。
たとえばCommon Lispの実装の1つ、SBCLは関数を定義するときにネイティブコンパイルするのでJITとして扱うことにします。

余談ですが動的型付言語がネイティブコンパイルできることに驚く方もいますが動的型付言語のインタプリタもCで書かれてコンパイルされているのでコンパイルできない道理はないです。
「動的に型チェックするコード」を生成すればいいだけです。

# コンパイルのタイミング
プロセスが起動してから実行するまでにコンパイルすればいいのでコンパイルするタイミングはいくつかオプションがあります。

1つはプロセスが起動したタイミングで全部コンパイルするもの。
コンパイルして実行とあまり変わらないですね。
雑な実装だとたまに見かけます。

1つはメソッド定義時にコンパイルする方法で、上述のSBCLなどいくつか例があります。

似たようなもので、メソッド定義したらタスクをキューに積んで別スレッドのワーカがコンパイルするモデルもTwitterで見たことありますがどの処理系かは覚えてないです。

一番遅いタイミングだとメソッドが呼ばれる直前にコンパイルするものがあります。
呼ばれないメソッドはコンパイルされないというメリットがあります。

いずれの場合もコンパイル時間が実行時間に含まれますのであまり大掛かりな最適化はできません。

# JITはどうして速いのか
大体の人は「コンパイルするんだからそりゃ速いだろ」くらいの感覚でしょう。しかしそんなに自明な話ではないです。
インタプリタは大抵Cで書かれてコンパイルされています。これは特にコンパイル時間に制約がないので全力で最適化できます。
時間の制約で適当にしか最適化をできないJITと全力で最適化をしたインタプリタ、本当にJITの方が速いと思いますか？

とは言ったもののインタプリタは遅いです。
なぜなら抽象構文木(AST)を辿りながら実行するからです。
実行のコスト+木の巡回のコストが必要になるのでループなどで何度も同じコードを実行すると木の巡回コストが何倍にもなってのしかかってきます。

## VM
シンプルなインタプリタだと遅いので大抵のインタプリタは一旦(インタプリタよりは)低レベルな命令列にコンパイルし、それを実行するという戦略をとります。
VM型というやつですね。

ちょっと実装してみました。
このVMではこういうことをすると関数が定義できます。
`IP_INST_CONST(1)` などと並んでいるのがVMの命令ですね。

```c
ip_proc_ref_t
ip_register_fib(struct ip_vm *vm)
{


  ip_proc_ref_t fib;

  /* registering proc first to recursive call */
  /* creating en empty proc */


  fib = ip_vm_reserve_proc(vm);
  if (fib < 0) {
    return fib;
  }

  /* 0(arg)   - n */
  size_t nargs = 1;
  size_t nlocals = 0;
  #define n 0
  struct ip_inst body[] = {
                           /*  0 */ IP_INST_CONST(1),
                           /*  1 */ IP_INST_GET_LOCAL(n),
                           /*  2 */ IP_INST_SUB(),
                           /*  3 */ IP_INST_JUMP_IF_NEG(5/* else */),
                           /* then */
                           /*  4 */ IP_INST_CONST(1),
                           /*  5 */ IP_INST_RETURN(),
                           /* else */
                           /*  6 */ IP_INST_GET_LOCAL(n),
                           /*  7 */ IP_INST_CONST(1),
                           /*  8 */ IP_INST_SUB(),
                           /*  9 */ IP_INST_CALL(fib),
                           /* 10 */ IP_INST_GET_LOCAL(n),
                           /* 11 */ IP_INST_CONST(2),
                           /* 12 */ IP_INST_SUB(),
                           /* 13 */ IP_INST_CALL(fib),
                           /* 14 */ IP_INST_ADD(),
                           /* 15 */ IP_INST_RETURN(),
  };

  #undef n

  int ret;
  struct ip_proc *proc;

  ret = ip_proc_new(nargs, nlocals, sizeof(body)/sizeof(body[0]), body, &proc);
  if (ret) {
    return -1;
  }

  ip_vm_register_proc_at(vm, proc, fib);

  return fib;

}
```

これを実行するVM部分はこうなります。


``` c
int
ip_vm_exec(struct ip_vm *vm, ip_proc_ref_t procref)
{
  size_t ip = 0;
  size_t fp;
  struct ip_proc  *proc;

#define LOCAL(i)      ip_stack_ref(ip_value_t, &vm->stack, fp - (proc->nargs + proc->nlocals) + i)
#define POP(ref)      do{if (ip_stack_pop(ip_value_t, &vm->stack, ref)) { return 1;}} while(0)
#define PUSH(v)       do{if (ip_stack_push(ip_value_t, &vm->stack, v)) { return 1;}} while(0)
#define POPN(n, ref)  do{size_t i; for (i = 0; i < (n); i++) POP(ref);} while(0)
#define PUSHN(n, v)   do{size_t i; for (i = 0; i < (n); i++) PUSH(v); } while(0)

  proc = vm->procs[procref];

  PUSHN(proc->nlocals, IP_LLINT2VALUE(0));
  fp = ip_stack_size(ip_value_t, &vm->stack);


  while (1) {
    struct ip_inst inst = proc->insts[ip];
    switch(inst.code) {
    case IP_CODE_CONST: {
      ip_stack_push(ip_value_t, &vm->stack, inst.u.v);
      break;
    }
    case IP_CODE_GET_LOCAL: {
      int i;
      ip_value_t v;

      i = inst.u.i;
      v = LOCAL(i);

      PUSH(v);
      break;
    }
    case IP_CODE_SET_LOCAL: {
      int i;
      ip_value_t v;

      i = inst.u.i;
      POP(&v);

      LOCAL(i) = v;

      break;
    }
    case IP_CODE_ADD: {
      ip_value_t v1, v2, ret;
      long long int x, y;

      POP(&v1);
      POP(&v2);
      y = IP_VALUE2LLINT(v1);
      x = IP_VALUE2LLINT(v2);

      ret = IP_INT2VALUE(x + y);

      PUSH(ret);

      break;
    }
    case IP_CODE_SUB: {
      ip_value_t v1, v2, ret;
      long long int x, y;

      POP(&v1);
      POP(&v2);
      y = IP_VALUE2LLINT(v1);
      x = IP_VALUE2LLINT(v2);

      ret = IP_LLINT2VALUE(x - y);

      PUSH(ret);

      break;
    }
    case IP_CODE_JUMP: {
      ip = inst.u.pos;
      break;
    }
    case IP_CODE_JUMP_IF_ZERO: {
      ip_value_t v;

      POP(&v);

      if (!IP_VALUE2LLINT(v)) {
        ip = inst.u.pos;
      }
      break;
    }
    case IP_CODE_JUMP_IF_NEG: {
      ip_value_t v;

      POP(&v);

      if (IP_VALUE2LLINT(v) < 0) {
        ip = inst.u.pos;
      }
      break;
    }
    case IP_CODE_CALL: {
      int ret;
      ip_callinfo_t ci = {.ip = ip, .fp = fp, .proc = proc};

      ret = ip_stack_push(ip_callinfo_t, &vm->callstack, ci);
      if (ret) {
        return 1;
      }

      proc = vm->procs[inst.u.p];

      PUSHN(proc->nlocals, IP_LLINT2VALUE(0));

      ip = -1;
      fp = ip_stack_size(ip_value_t, &vm->stack);


      break;
    }
    case IP_CODE_CALL_INDIRECT: {
      int ret;
      ip_value_t p;
      ip_callinfo_t ci = {.ip = ip, .fp = fp, .proc = proc};

      POP(&p);

      ret = ip_stack_push(ip_callinfo_t, &vm->callstack, ci);
      if (ret) {
        return 1;
      }

      proc = vm->procs[IP_VALUE2PROCREF(p)];

      PUSHN(proc->nlocals, IP_INT2VALUE(0));

      ip = -1;
      fp = ip_stack_size(ip_value_t, &vm->stack);


      break;
    }
    case IP_CODE_RETURN: {
      int ret;
      ip_value_t v;
      ip_value_t ignore;
      ip_callinfo_t ci;

      POP(&v);

      POPN(proc->nlocals + proc->nargs, &ignore);

      PUSH(v);

      ret = ip_stack_pop(ip_callinfo_t, &vm->callstack, &ci);
      if (ret) {
        return 1;
      }

      ip = ci.ip;
      fp = ci.fp;
      proc = ci.proc;

      break;
    }
    case IP_CODE_EXIT: {
      ip_value_t v;
      ip_value_t ignore;
      POP(&v);

      POPN(proc->nlocals + proc->nargs, &ignore);

      PUSH(v);

      return 0;
    }
    default: {
      printf("code: %d, u: %d", inst.code, inst.u.i);
      return 1;
    }
    }
    ip += 1;
  }

#undef POP
#undef PUSH
}
```

長いですが
```c
while(1) {
  struct ip_inst inst = proc->insts[ip];
  switch(inst.code) {
  case XXX: {
    ...;
    break;
  }
  ...
  }
  ip+=1;
}
```
というのが全体の構造です。
配列を舐めているだけなので木を巡回するよりずっと速いです。

ですが、これでもまだ遅いです。必要なコードを実行する以外に`while` 、 `switch` 、 `break` の3回のジャンプが必要になります。
これをどうにか節約できないでしょうか。


## Threaded VM

どうにか節約するのがThreaded VMです。
`break` に到達した時点で次に実行する `inst` は計算できますし `inst` が分かれば `switch` でのジャンプ先も分かります。
それに、 `switch` はただのジャンプテーブルなのでswitchを使わなくてもどうにか自前で実装できます。
なので

1. `switch` の `case` に相当するジャンプテーブルを自分で作る。
2. `break` の代わりにそのジャンプテーブルを作って自力で飛ぶ。

をやればジャンプ回数を節約できそうです。

これも実装してみました。ただし実装にはGCC拡張の[`Labels as Values`](https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html)を使います。

まず、先程の `case` の部分はラベルになります。


``` c
 L_CONST: {
    ...
    }
 L_GET_LOCAL: {
    ...
    }
...
```

そしてこれを用いてジャンプテーブルは以下のように準備できます。ただのラベルの配列ですね。

``` c
  static void *labels[] = {
                         &&L_CONST,
                         &&L_GET_LOCAL,
                         &&L_SET_LOCAL,
                         &&L_ADD,
                         &&L_SUB,
                         &&L_JUMP,
                         &&L_JUMP_IF_ZERO,
                         &&L_JUMP_IF_NEG,
                         &&L_CALL,
                         &&L_CALL_INDIRECT,
                         &&L_RETURN,
                         &&L_EXIT,
  };

```

`&&` がLabels as Valuesの機能です。ここまでくればこれだけのコードで自前ジャンプができます。

``` c
inst = proc->insts[++ip];
goto *labels[inst.code]
```

毎度呼ぶのでマクロでまとめるなどして、結局こんな感じのコードになります。


``` c
#define JUMP()  do{inst = proc->insts[++ip]; goto *labels[inst.code];} while(0);

  inst = proc->insts[ip];
  goto *labels[inst.code];

 L_CONST: {
      ip_stack_push(ip_value_t, &vm->stack, inst.u.v);
      JUMP();
    }
 L_GET_LOCAL: {
      int i;
      ip_value_t v;

      i = inst.u.i;
      v = LOCAL(i);

      PUSH(v);
      JUMP();
    }
...
```


`while` と `break` が消えて `switch` の代わりに各 `case` に `JUMP` が入りました。

これを先程のシンプルなVMと比べてみましょう。ベンチマークに使うのは最初の方に出てきた `fib` のコードです。引数は36を与えてみます。

``` console
$ time ./main_simple
result of fib: 24157817
2.45user 0.00system 0:02.45elapsed 99%CPU (0avgtext+0avgdata 1608maxresident)k
$ time ./main_threaded
result of fib: 24157817
1.61user 0.00system 0:01.61elapsed 99%CPU (0avgtext+0avgdata 1536maxresident)k

```

$ (2.45 - 1.61) / 2.45 = 0.342... $ 34%の高速化です。

## Direct Threaded VM

これでもまだ無駄があります。
先程のコードはジャンプテーブルを使っていました。
しかし次に実行するVM命令は関数を定義した時点で決まっているはずなのでテーブルすら不要です。
要するに `inst.code` というのがテーブルのインデックスになってましたが、インデックスの代わりに直接ジャンプ先のコードを入れてしまえば速いだろということです。

これも実装してみました。関数定義時には一旦 `inst.code` を使いますがすぐにアドレスに書換えてしまいます。それがこのコード。

``` c
for(i = 0; i < ninsts; i++) {
  struct ip_inst_internal inst;
  inst.label = labels[insts[i].code];
  // ...
  result[i] = inst;
}
return 0;
```

あとは`JUMP` を少しだけ書換えたら完了です。

``` c
// `goto *labels[inst.code]` が `goto *inst.label` になっている
#define JUMP()  do{inst = proc->insts[++ip]; goto *inst.label;} while(0);
```

同じくベンチマークを取ってみましょう。


``` console
$ time ./main_direct_threaded
result of fib: 24157817
1.52user 0.00system 0:01.52elapsed 99%CPU (0avgtext+0avgdata 1536maxresident)k
```

元のVMから比べて $ (2.45 - 1.52) / 2.45 = 0.379... $ 38%の高速化です。
因みにThreaded VMから比べると $ (1.61 - 1.52) / 1.61 = 0.055... $ 6%の高速化です。
わずかながら高速化しました。

それでもまだ、ジャンプが1つ残っています。
メソッド定義時点で次に実行するコードは分かっているのでした。
残ったジャンプも取り除けないでしょうか。

## なんちゃってJIT

ジャンプが嫌ならコードをくっつけてしまえばいいのです。
VM命令に対応するネイティブコード片を集めてきて1箇所のメモリに書き込めばジャンプが消えます。

これも実装してみました。

下準備としてVM部分を以下のように書換えます。

``` c
#define NEXT() arg = proc->args[++ip];

 L_CONST: {
      ip_stack_push(ip_value_t, &vm->stack, arg.u.v);
      arg = proc->args[++ip];
      NEXT();
    }
 L_CONST_END:
 L_GET_LOCAL: {
      int i;
      ip_value_t v;

      i = arg.u.i;
      v = LOCAL(i);

      PUSH(v);
      NEXT();
    }
 L_GET_LOCAL_END:
```

ジャンプが消えるので`JUMP` 内にあったgotoは消えますが、 `ip` をインクリメントして仮想命令の引数を取り出すところは残ります。
この`L_CONST` から `L_CONST_END` までが `IP_CODE_CONST` に対応するネイティブコードですね。
事前準備としてこれを1命令毎にバッファに書き込んでいきます。

``` c
case IP_CODE_CONST: {
  code_size = &&L_CONST_END - &&L_CONST
  tmp = realloc(tmp, total_code_size + code_size);
  memcpy(tmp+total_code_size, &&L_CONST, code_size);
  total_code_size += code_size;

}
```

他にも多少変更点はあるのですがだいたいこんなところです。
ただしコードを書き込んだメモリは扱いに注意が必要です。
詳しくは2015年のAdvent Calendarに[書きました](https://keens.github.io/blog/2015/12/12/sml_dejitwotsukurukaruihanashi/)ので気になる方は一読下さい。

準備が済んだコードの実行は `goto` するだけです

``` c
arg = proc->args[ip];
goto *proc->code;
```

さて、これもベンチマークを取ってみましょう

``` console
$ time ./main_simple_jit
Command terminated by signal 11
0.00user 0.00system 0:00.10elapsed 0%CPU (0avgtext+0avgdata 1168maxresident)k
```

はい、あえなくSEGVしてしまいました。
これは `code_size = &&L_CONST_END - &&L_CONST` の部分が嘘だったようです。
コンパイルでラベル位置が意図したところに来ないので`&&L_CONST_END - &&L_CONST`が負になるケースがあるようでした。
これをやりたければアセンブラで頑張るしかないようですね。1日でシンプルなVMからJITまで実装するのは無理だったようです。

動かないのは置いておいて、もし動いたとしたらこれは理想的なコードでしょうか。
これは最適化されたコードをくっつけています。でもやっぱり、くっつけた後に最適化したいですよね？
たとえば以下のような命令列は

``` c
IP_INST_GET_LOCAL(0),
IP_INST_GET_LOCAL(1),
IP_INST_SUB(),
```

コードの並びとしてはこのようになります。

``` c
{
  ip_value_t v;

  v = LOCAL(0);

  PUSH(v);
}
{
  ip_value_t v;

  v = LOCAL(1);

  PUSH(v);
}
{
  ip_value_t v1, v2, ret;
  long long int x, y;

  POP(&v1);
  POP(&v2);
  y = IP_VALUE2LLINT(v1);
  x = IP_VALUE2LLINT(v2);

  ret = IP_LLINT2VALUE(x - y);

  PUSH(ret);

}
```

でも、 `PUSH` して `POP` と無駄を挟んでいるので最適化して

``` c
ip_value_t v1, v2, ret;
long long int x, y;

v1 = LOCAL(0);
v2 = LOCAL(1);

y = IP_VALUE2LLINT(v1);
x = IP_VALUE2LLINT(v2);

ret = IP_LLINT2VALUE(x - y);

PUSH(ret);
```

のように短くできるはずです。
これはコード同士の並びがわからないとできないのでくっつけたあとに最適化したいですよね。

## コンパイラを使ったJIT

コードをくっつけたあとにコンパイルする話です。


コードは概ねこのような見た目なのでした。


``` c
L_CONST: {
    ip_stack_push(ip_value_t, &vm->stack, arg.u.v);
    arg = proc->args[++ip];
    NEXT();
  }

```

これをマクロにしてみます。


``` c
#define OP_CONST(v) {                             \
    ip_stack_push(ip_value_t, &vm->stack, (v));   \
    arg = proc->args[++ip];                       \
    NEXT();                                       \
  }
```

すると、このような命令列から

``` console
/*  0 */ IP_INST_CONST(1),
/*  1 */ IP_INST_GET_LOCAL(n),
/*  2 */ IP_INST_SUB(),
/*  3 */ IP_INST_JUMP_IF_NEG(5/* else */),
/* then */
/*  4 */ IP_INST_CONST(1),
/*  5 */ IP_INST_RETURN(),
/* else */
/*  6 */ IP_INST_GET_LOCAL(n),
/*  7 */ IP_INST_CONST(1),
/*  8 */ IP_INST_SUB(),
/*  9 */ IP_INST_CALL(fib),
/* 10 */ IP_INST_GET_LOCAL(n),
/* 11 */ IP_INST_CONST(2),
/* 12 */ IP_INST_SUB(),
/* 13 */ IP_INST_CALL(fib),
/* 14 */ IP_INST_ADD(),
/* 15 */ IP_INST_RETURN(),
```

以下のようなCのコードを吐くことはたやすいですね。

``` console
int
run(struct ip_vm *vm)
{
  // 諸々の準備
  OP_CONST(1);
  OP_GET_LOCAL(n);
  OP_SUB();
  OP_JUMP_IF_NEG(L_5);
  OP_CONST(1);
  OP_RETURN();
 L_5:
  OP_GET_LOCAL(n);
  OP_CONST(1);
  OP_SUB();
  OP_CALL(fib);
  OP_GET_LOCAL(n);
  OP_CONST(2);
  OP_SUB();
  OP_CALL(fib);
  OP_ADD();
  OP_RETURN()
  // 諸々の後始末
}
```

これをコンパイルしたら望み通りコードをくっつけた後にコンパイルすることができます。目標達成です。

ところ実行中にCコンパイラを呼んで新しい実行可能ファイルを作ったところでどう実行するんだと思うかもしれません。
これはDLLを作ってロードしてあげれば実行できます。
そういう話は話はやはり2015年のAdvent Calendarに[書いた](https://keens.github.io/blog/2015/12/12/sml_nimanabukonpairagengoniokerureplnojissouhouhou/)ので参考にして下さい。

これも実装してみようとしましたが先のJITの例が動かなかったので萎えて書いてないです。
ちゃんと実装したら多分動きます。

## もうちょっと

最終的にCのコードを吐くところまでいきました。実行性能は申し分無いはずです。これより先はあるでしょうか。

やっぱり、実行中にCコンパイラのプロセスを呼ぶのは筋が悪いです。
外部システムに依存しますしプロセスの起動はそこそこ重い処理です。
最適化もCのセマンティクスを通すので本来処理系が知ってるはずの情報が落ちたりもします。
さらにDLLにしてロードというだけで時間的にも空間的にもオーバーヘッドがあります。
そこを気にしだすとインメモリでアセンブルして自前でコード生成になったりします。

ならば、とLLVMを使う例もありますがLLVMはコンパイラ向けの中間言語を使うので軽い気持ちで使うと思ったより面倒なことになるでしょう。
LLVMを使ってJITをしていたRubyのRubiniusやPythonのPystonが奮わないのもそのせいかもしれません。あとLLVMつらいらしい(あんまりLLVM使ったことがないので伝聞)。

あるいはアセンブラを直接書けるとその処理系に特化したコード生成できたりします。
Common LispだとCのコードを吐いてコンパイルする処理系にECLというのがありますが、ほとんどのベンチマークで、自分でアセンブラまで持ってるSBCLより遅いです。
これはCコンパイラが弱い訳ではなくてSBCLがかなり工夫を凝らした実装をしているからです。
たとえば多値と単値を区別するのにキャリーフラグを使っておりレジスタ使用量を抑えたりしています。この話題は最近[話した](https://keens.github.io/slide/common_lispnotachitosonojissoutachi/)ので良かったら見てみて下さい。

歴史あるCommon Lisp処理系のSBCLは様々なプラットホーム向けに頑張ってアセンブラを実装していますが流石にそこまでやれる処理系はそう多くないでしょう。ライブラリの助けを借りることになります。
JITライブラリもいくつかあります。LibJITやGNU Lightning、もう少し軽いのならXbyakなど。あとLuaJITのやつなんだっけ。

ツールは主眼であった問題を解決してくれますが、今度はツールを上手く使いこなすという問題がでます。
たとえば命令の並びが決定すると `PUSH` と `POP` が減らせるという話もメモリが絡む最適化はCコンパイラが苦手とするところなのでやってくれないかもしれません。
結局は作者側での工夫が必要でしょう。

# JITを超えた先へ

JITの常識を超えてみましょう。何があるでしょうか。

たとえば、2度コンパイルする。

たとえば、コンパイルしない。

## 適応的最適化

折角ランタイムにコード生成しているのでもうちょっとダイナミックなことをしてみましょう。

ある関数が何度も呼ばれることが分かった場合、時間を掛けてでも最適化する価値があります。
たとえばHTTPサーバのハンドラは何日間ものあいだ無数に実行されるのでコンパイルに1分掛かろうがペイするでしょう。
何度も呼ばれる関数はもう一度、最適化レベルを上げてコンパイルし直すというのが考えられます。


## 適応的コンパイル

先程までは全てのコードを最適な状態で動かす前提で話していました。
しかしDirect Threaded VMあたりから事前準備が必要になりました。
これは各々の関数定義にオーバーヘッドを加えるのであまり実行させない関数に対して行うとかえって遅くなったりします。
そこでまずはオーバーヘッドのかからない実行方式で実行し、それなりに実行される関数と判明したらもうちょっと速く実行できる方式に切り替えるというのが考えられます。
つまり、必要なければコンパイルしない。

この方法はコンパイルが間に合ってないのでJust in Timeではなないですね。
なので厳密にいうとJITには含まれませんがこういうのも含めてJITと呼ぶことが多いようです。

一見良さそうなアイディアですが今までの方式と比べて大きな技術的トレードオフがあります。
VMとコンパイルされるコード、2種類のインタプリタを実装する必要があるのです。
これは手間なだけではなく両者で挙動が違うと、たとえば256回実行すると結果が変わるバグなどになります。
この大きなトレードオフにも関わらず適応的コンパイルを選択するケースが多いようです(要出典)。みなさん頑張りやさんですね。


## Tracing JIT

今まで、関数(メソッド)単位でコンパイルする話をしてきました。別の方法がないか考えてみましょう。

たとえばこのような命令列を考えます


``` console
/*  0 */ IP_INST_CONST(0),
/*  1 */ IP_INST_SET_LOCAL(i),
/*  2 */ IP_INST_CONST(0),
/*  3 */ IP_INST_SET_LOCAL(sum),
/* loop */
/*  4 */ IP_INST_GET_LOCAL(n),
/*  5 */ IP_INST_GET_LOCAL(i),
/*  6 */ IP_INST_SUB(),
/*  7 */ IP_INST_JUMP_IF_NEG(16 /* exit */),
/*  8 */ IP_INST_GET_LOCAL(sum),
/*  9 */ IP_INST_GET_LOCAL(i),
/* 10 */ IP_INST_ADD(),
/* 11 */ IP_INST_SET_LOCAL(sum),
/* 12 */ IP_INST_GET_LOCAL(i),
/* 13 */ IP_INST_CONST(1),
/* 14 */ IP_INST_ADD(),
/* 15 */ IP_INST_SET_LOCAL(i),
/* 16 */ IP_INST_JUMP(3 /* loop */),
/* exit */
/* 17 */ IP_INST_GET_LOCAL(sum),
/* 18 */ IP_INST_RETURN(),
```


これを実行するともちろんループ内のコードが繰り返し実行されますね。
もし実行されるVM命令列のトレースを取ったらこのような見た目になるでしょう。

``` console
GET_LOCAL(0)
GET_LOCAL(1)
SUB()
JUMP_IF_NEG(16)
GET_LOCAL(2)
GET_LOCAL(1)
ADD()
SET_LOCAL(2)
GET_LOCAL(1)
CONST(1)
ADD()
SET_LOCAL(1)
JUMP(3)
/* 2周目 */
GET_LOCAL(0)
GET_LOCAL(1)
SUB()
JUMP_IF_NEG(16)
GET_LOCAL(2)
GET_LOCAL(1)
ADD()
SET_LOCAL(2)
GET_LOCAL(1)
CONST(1)
ADD()
SET_LOCAL(1)
JUMP(3)
...
```

これは簡単な例でしたがループ内で関数呼び出しがあったり分岐があったりするともう少し状況が複雑になります。
それでも、「よく実行されるトレースパターン」というのが浮かび上がるはずです。
このトレースベースでコンパイルしよう、というのがトレーシングJITです。

私はTracing JITについてPyPyの人のブログを読んで感動した記憶があります。
2012年の記事ですがサンプル実装コードがPrologだったりかなりのニンジャであることが伺える楽しい連載なので是非一読下さい。

[Comparing Partial Evaluation and Tracing, Part 1 ](https://morepypy.blogspot.com/2012/01/comparing-partial-evaluation-and.html)  
素晴らしいことに和訳もあります  
[部分評価とトレーシングの比較 Part 1 ](http://morepypy-ja.blogspot.com/2012/01/part-1.html)  
つながりが見づらいですが左のアーカイブから2012年の1月と2月の記事を開くと続きが見つかります。

さて、Tracing JITですがこれはとてつもなく強力です。
関数に縛られないので数度しか呼ばれないけどものすごく重いループを回す関数もループ内だけ最適化できます。
関数を跨いでのトレースを取れば関数を跨いでの最適化ができます。
ifのthen節がよく実行されるならthen節のみのトレースが出てくるのでifの条件節が常にtrueであることを前提に最適化できます。
たとえば `if x isinstanceof Int` がtrueならば以後は動的型チェックを省いて実行できたりします。

しかしやはりこれにもトレードオフが入ります。これも適応的コンパイルの一種なので適応的コンパイルのデメリットはそのまま残りますが、さらなるデメリットがあります。
メソッドの実行途中でVMから抜けてネイティブコードになったりネイティブコードを実行してたら急にVMに戻ったりするわけです。
そういう橋渡しがかなり難しいと聞きます。
が、私は詳しくないので詳しい方に説明をゆずります。言語実装 Advent Calendarの続きにご期待下さい。

# 最後に

遅刻して大変申し訳ありませんでした。敗因は「VMくらい2時間程度あれば書けるだろ」とタカをくくってたらDirect Threaded VMとJITのデバッグでかなり時間を取られたことです。あと数年ぶりにCを書いた。
因みにVM自体はWebassemblyやHuman Resource Machineを真似して命令セットを書いたら書きやすかったです。

「JITを超えた先へ」のところはぶっちゃけ全然知らないです。PyPyのブログを読むまでは適応的コンパイルのことをTracing JITと思ってました。

# ノート

* 今回書いたコードは[こちら](https://github.com/KeenS/interp)です
* VM, Threaded VM, Direct Threaded VMに関しては毎度ながらこの記事を参考にさせていただきました。
  [YARV Maniacs 【第 3 回】 命令ディスパッチの高速化](https://magazine.rubyist.net/articles/0008/0008-YarvManiacs.html)
* 今回私が書いたVMはメモリの扱いがないので比較的 1命令あたりの計算量が少ないです。実際の処理系だと少し結果が変わるかもしれません
* とはいえ過去に数十%の高速化に成功したこともあるので場合によりけり。 CF [Onigmoを最大49%高速化した話](https://keens.github.io/blog/2015/05/26/onigmowosaidai49_kousokukashitahanashi/)
  + 余談ですが当時Threaded VMとDirect Threaded VMの違いを分かってなかったのでThreaded VMなのにDirect Threaded VMと呼んでしまってます。
* ラベルとラベルの間のコードをコピーするJITの話はOracle Labのどこかのスライドで見た記憶から実装しましたが思い出せませんでした。
* マクロを定義してCコードを生成するのは確かmoclで見た気がします。恐らくKCLの頃からやってるんじゃないでしょうか。
* JITにCコンパイラを使うと無駄が多いという話をしましたがRubyのJITは色々工夫を凝らしているようです。言語実装 Advent Calendarの最終日にその話があるようなので期待しましょう。

# 付録

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">JITあれこれ <a href="https://t.co/pwdpM5jzVA">https://t.co/pwdpM5jzVA</a> 分かりやすくて丁寧なJITの解説。私が試した時は、Threaded VMはそれほど速くならなかった気がするので、使ったコンパイラとOSを書いてくれるとより良いかなと思った</p>&mdash; Miura Hideki (@miura1729) <a href="https://twitter.com/miura1729/status/1069053214190841856?ref_src=twsrc%5Etfw">2018年12月2日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

確かにそうですね。本文中でベンチマークを取った環境は以下です。

* コンパイラ: gcc (Ubuntu 8.2.0-7ubuntu1) 8.2.0
* OS: Ubuntu 18.10
* CPU: AMD Ryzen Threadripper 1950X 16-Core Processor

AMDマシンですね。

ところで私はもう一つマシンを持ってるのでこちらでもベンチマークをしてみます。

* コンパイラ: gcc (Ubuntu 7.3.0-27ubuntu1~18.04) 7.3.0
* OS: Ubuntu 18.04
* CPU: Intel(R) Core(TM) i7-4910MQ CPU @ 2.90GHz

こちらはIntelマシンです。ベンチマークの結果はこうなりました。

``` console
$ time ./main_simple
result of fib: 24157817
1.23user 0.00system 0:01.24elapsed 99%CPU (0avgtext+0avgdata 1556maxresident)k
$ time ./main_threaded
result of fib: 24157817
1.24user 0.00system 0:01.24elapsed 100%CPU (0avgtext+0avgdata 1552maxresident)k
$ time ./main_direct_threaded
result of fib: 24157817
1.19user 0.00system 0:01.19elapsed 100%CPU (0avgtext+0avgdata 1532maxresident)k
```


うおー、Threaded VMで遅くなってる。
コンパイラのバージョンとCPUの両方が変わってるのでわかりませんがIntel CPUの方が分岐予測が賢いかGCCのバージョンが上がって `while (){switch (){}}` のコンパイルが賢くなったか、あるいはそのどちらともでしょう。

ところで、上記までのベンチマークは最適化オプションを `-O3` にしていました。アーキテクチャ間の差異が気になったので `-march=native` を付けて使っているCPU向けに最適化してみます。

AMDマシン

``` console
$ time ./main_simple
result of fib: 24157817
1.66user 0.00system 0:01.66elapsed 99%CPU (0avgtext+0avgdata 1612maxresident)k
0inputs+0outputs (0major+68minor)pagefaults 0swaps
$ time ./main_threaded
result of fib: 24157817
1.60user 0.00system 0:01.61elapsed 99%CPU (0avgtext+0avgdata 1548maxresident)k
0inputs+0outputs (0major+67minor)pagefaults 0swaps
$ time ./main_direct_threaded
result of fib: 24157817
1.50user 0.00system 0:01.50elapsed 99%CPU (0avgtext+0avgdata 1536maxresident)k
0inputs+0outputs (0major+66minor)pagefaults 0swaps
```

Intelマシン


``` c
$ time ./main_simple
result of fib: 24157817
1.21user 0.00system 0:01.21elapsed 100%CPU (0avgtext+0avgdata 1488maxresident)k
$ time ./main_threaded
result of fib: 24157817
1.23user 0.00system 0:01.23elapsed 99%CPU (0avgtext+0avgdata 1560maxresident)k
$ time ./main_direct_threaded
result of fib: 24157817
1.22user 0.00system 0:01.22elapsed 99%CPU (0avgtext+0avgdata 1508maxresident)k
```

まとめてみます

.                           | Simple VM | Threaded VM | vs Simple | Direct Threaded VM | vs Simple |
:---------------------------|----------:|------------:|----------:|-------------------:|----------:|
マシン1/`-O3`               |      2.45 |        1.61 |       34% |               1.52 |       38% |
マシン1/`-O3 -march=native` |      1.66 |        1.60 |      3.6% |               1.50 |       10% |
マシン2/`-O3`               |      1.23 |        1.24 |     -0.8% |               1.19 |      3.2% |
マシン2/`-O3 -march=native` |      1.21 |        1.23 |     -1.7% |               1.22 |     -0.8% |

マシン1 = AMD/Ubuntu 18.10/gcc 8.2.0  
マシン2 = Intel/Ubuntu 18.04/gcc 7.3.0  


参考になれば。
