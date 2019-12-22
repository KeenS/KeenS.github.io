---
categories: [ML, SML, 言語実装]
date: 2015-05-29T20:26:41+09:00
title: 第一級ラベルを持たない言語におけるDirect Threaded VMの実装
---
こんにちは。κeenです。このブログでちょくちょく出てくるDirect Threaded VMについて。
SMLのようにgotoがない言語だとDT VMの実装出来ないよなー、と思ってた所、ふとアイディアが浮かんだのでそれについて。
<!--more-->
# 序論
DSL、例えば正規表現などの処理系を実装することを考えてみて下さい。

言語処理系において最も素朴な実装はインタプリタですが、速度面で不利なので一旦仮想命令にコンパイルして仮想命令実行器(VM)で実行することが一般的です。
コンパイラのように複雑な記号処理をするプログラムはCommon LispやMLのような記号処理に強い高級言語が得意とする分野です。
一方、ランタイムには低レベルなことが出来て処理速度の速いCommon LispやCを使いたくなるでしょう。

Common Lisp以外の言語ではコンパイラとランタイムを分離するのが妥当な選択肢のようですが、高級言語とcの間のブッジングが必要になり、少なからぬコストを支払う必要があります。
また、ffiを持たない言語ではブリッジ出来ないのでランタイムもその言語で実装する必要があります。つまり、高級言語でVMを実装する必要があるケースが存在します。

VMの実行を高速化する技術の一つとしてDirect Threadingというものがあります。
命令ディスパッチのループを短絡することで余計なオーバーヘッドが減り、また、命令毎にジャンプ命令を持つことで分岐予測も効きやすくなるのでVMが高速化します。
しかしDTの実装にはgotoのラベルを第一級オブジェクトとして保存する必要があり、gccやclangのように拡張されたcコンパイラなど、限られた言語でしか実現出来ません。まあ、Common Lispなら`eval`と`compile`を使えばJITが出来るので不要ですが。

そこで、gotoのない言語でDirect Threadingを実現してみたいと思います。

# Direct Threading
Direct Threaded *でない* VMは大抵次のような構造をしています。

```
loop {
  op = fetchNextOp
  switch(op) {
    case op1:
     ....
     break
    case op2:
     ....
     break
    ....
  }
}
```

つまり、

1. 次の命令を取得する
2. 命令でディスパッチする
3. 命令に対応するコードを実行する
4. ディスパッチを抜ける
5. 1.に戻る

という動きをします。自然に思えるかもしれませんが、

* 命令のディスパッチはlogオーダの時間が掛かる（可能性がある）。
* 4. 5. のステップが不要
* 2. で毎回違う命令にディスパッチするので分岐予測がほぼ意味を成さない

という無駄があります。それを改良したのがDirect Threaded VMで、オペコードではなくgotoのラベルを使うことで

```
label = fetchNextLabel
goto label
label1:
  ...
  label = fetchNextLabel
  goto label
label2:
  ...
  label = fetchNextLabel
  goto label
...
```

1. 次のラベルを取得する
2. ラベルにgotoする
3. 命令に対応するコードを実行する
4. 次のラベルを取得する
5. ラベルにgotoする(次の処理は3. 相当)

と、ループ内の2ステップを飛ばした他、ディスパッチもなくなるので高速になります。
また、ラベル毎にgotoがついていて、それぞれのgotoに分岐予測があるので普通のVMに比べて分岐予測がある程度効きます。

# 第一級ラベルを持たない言語におけるDirect Threaded VM
結論から言うと関数の配列を使います。ラベルの代わりに配列のインデックス、gotoの代わりに配列へのアクセスとcallを使います。
ランダムアクセスでアドレスの取得をするために配列を、任意コードへのジャンプのために関数を使えばエミュレート出来るよねって発想です。
# 実装
[ソースコード全体](https://github.com/KeenS/SML-VM)はGithuに上げてます。SML/NJで動きます。SML#向けのインターフェースファイルを書いていますが何故かコンパイルが通りません。

次のようなASTを実行するインタプリタ、VM、Direct Threaded VMを実装しました。但し、VMとDTVMはクロージャをサポートしていません。

```sml
datatype monoop
  = Not

datatype binop
  = Equal
  | GreaterThan
  | Add

datatype t
  = Int of int
  | Bool of bool
  | MonoOp of monoop * t
  | BinOp of binop * t * t
  | Bind of t * t
  | If of t * t * t
  | Var of string
  | Lambda of t list * t
  | Call of t * t list
  | Progn of t list
```

インタプリタはこのASTを解釈実行、VMはいくつかの中間表現を経てオペコードにコンパイルし、それを実行します。尚、最適化は行いません。


VMのディスパッチ部分は次のような実装になっています。

```sml
    fun aux () = (
        case  (Array.sub(ops, !pc)) of
            O.Not => (case pop vm of
                         V.Bool x => push vm (V.Bool (not x))
                       | _ => raise Type)
          | O.Add => (case (pop vm, pop vm) of
                         (V.Int x, V.Int y) => push vm (V.Int (x + y))
                       | _ => raise Type)
          | O.Eq => (case (pop vm, pop vm) of
                        (V.Int x, V.Int y) => push vm (V.Bool (x = y))
                      | (V.Bool x, V.Bool y) => push vm (V.Bool (x = y))
                      | _ => raise Type)
          | O.Gt => (case (pop vm, pop vm) of
                        (V.Int x, V.Int y) => push vm (V.Bool (x < y))
                      | _ => raise Type)
          | O.Jump label => pc := (label - 1)
          | O.Jtrue label => (case pop vm of
                                 V.Bool true => pc := (label - 1)
                               | V.Bool false => ()
                               | _ => raise Type)
          | O.Call i => (case (pop vm) of
                            V.Lambda label => (
                             pushCi vm;
                             fp := (!fp) - i;
                             pc := (label - 1))
                          | _ => raise Type)
          | O.Ret => (Array.update(stack, !fp, Array.sub(stack, (!sp) - 1));
                     popCi vm;
                     pc := (!pc))
          | O.Push v => push vm v
          | O.Pop => (pop vm;())
          | O.Lref i => push vm (Array.sub(stack, (!fp) + i))
          | O.Lset i =>  ((Array.update(stack, (!fp) + i, pop vm));
                         push vm (V.Bool true))
          | O.Gref i => push vm (Array.sub(pool, i))
          | O.Gset i =>  (Array.update(pool, i, pop vm); push vm (V.Bool true))
          | O.Nop => ()
          | O.End => raise Exit
      ;
        pc := (!pc) + 1;
      aux ())
```

DT VMではこれを次のように書き換えました。

```sml
fun next () = let
    val () = pc := (!pc) + 1;
    val (index, arg) = Array.sub(cops, !pc) in
    Array.sub(opArray, index) arg
end

Array.fromList [
            (* Not *)
            fn _ =>
                (case pop vm of
                    V.Bool x => push vm (V.Bool (not x))
                  | _ => raise Type;
                next ()),
            (* Add *)
            fn _ =>
                (case (pop vm, pop vm) of
                    (V.Int x, V.Int y) => push vm (V.Int (x + y))
                  | _ => raise Type;
                 next ()),
            (* Eq *)
            fn _ =>
                (case (pop vm, pop vm) of
                    (V.Int x, V.Int y) => push vm (V.Bool (x = y))
                  | (V.Bool x, V.Bool y) => push vm (V.Bool (x = y))
                  | _ => raise Type;
                  next ()),
            (* Gt *)
            fn _ =>
                (case (pop vm, pop vm) of
                    (V.Int x, V.Int y) => push vm (V.Bool (x < y))
                  | _ => raise Type;
                 next ()),
            (* Jump *)
            fn ({int = label, ...}: oparg) =>
               (pc := (label - 1);
               next ()),
            (* Jtrue *)
            fn ({int = label, ...}: oparg) =>
                (case pop vm of
                    V.Bool true => pc := (label - 1)
                  | V.Bool false => ()
                  | _ => raise Type;
                next ()),
            (* Call *)
            fn ({int = i, ...}: oparg) =>
                (case (pop vm) of
                    V.Lambda label => (
                     pushCi vm;
                     fp := (!fp) - i;
                     pc := (label - 1))
                  | _ => raise Type;
                next ()),
            (* Ret *)
            fn _ =>
                (Array.update(stack, !fp, Array.sub(stack, (!sp) - 1));
                 popCi vm;
                 pc := (!pc);
                next ()),
            (* Push *)
            fn ({vmvalue = v, ...}: oparg) =>
               (push vm v;
               next ()),
            (* Pop *)
            fn _ =>
               (pop vm;
                next ()),
            (* Lref *)
            fn ({int = i, ...}: oparg) =>
               (push vm (Array.sub(stack, (!fp) + i));
               next ()),
            (* Lset *)
            fn ({int = i, ...}: oparg) =>
                ((Array.update(stack, (!fp) + i, pop vm));
                 push vm (V.Bool true);
                next ()),
            (* Gref *)
            fn ({int = i, ...}: oparg) =>
               (push vm (Array.sub(pool, i));
               next ()),
            (* Gset *)
            fn ({int = i, ...}: oparg) =>
               (Array.update(pool, i, pop vm);
                push vm (V.Bool true);
               next ()),
            (* Nop *)
            (fn _ =>
                next ()),
            (fn _ =>
                raise Exit)
        ]

fun aux () = let val (index, arg) = Array.sub(cops, !pc) in
                 Array.sub(opArray, index) arg
             end
```

1引数を受け取ってunitを返す関数の配列としてVMを表しています。
1つ注意点として、前処理としてタグ付き共用体として表されている命令をタグ(配列のインデックス)と共用体に分解するのですが、SMLに共用体はないので構造体で代用しています。 `oparg` 型がそれにあたります。


ディスパッチを関数`next`に括り出していて、一見すると分岐予測に関する利点が失われるように思われますが、
`next`は小さいのでインライン化されるだろうと踏んでそのままにしています。実際、手動でインライン化しても速度に変化はありませんでした。

今回のメインの話はVMなのでインタプリタについては省略します。

# 実行速度
## 予測
普通のVMはディスパッチをlogオーダーの時間で行ないますがDT VMは定数オーダーの時間で行ないます。しかし配列の参照と関数呼び出しを挟むので定数倍の部分は大きくなります。
どちらが速いでしょうか。

## 計測
今回、次のようなフィボナッチ数列を計算するコードの実行速度を計測しました。

```sml
fun fib n = (Progn [
                Bind (Var "fib",
                      Lambda([Var "n"],
                               (If (BinOp(GreaterThan,
                                          (Int 2),
                                          (Var "n")),
                                    Int(1),
                                    BinOp(Add,
                                          Call(Var "fib", [BinOp(Add,
                                                                 Var "n",
                                                                 Int ~1)]),
                                          Call(Var "fib", [BinOp(Add,
                                                                 Var "n",
                                                                 Int ~2)])))))),
                Call(Var "fib", [Int n])])
```

コンパイラは以下のような命令列を吐きます。命令の内部表現が違うだけで命令列自体はVMとDTVMで共通です。
繰り返しますが、最適化はしてないのでL25で次の命令にジャンプしてるだとか目に見えて無駄なコードもあります。

```
0	Push Lambda 7
1	Gset 0
2	Pop
3	Push 35
4	Gref 0
5	Call 1
6	End
7	Push 2
8	Lref 0
9	Gt
10	Jtrue 12
11	Jump 14
12	Push 1
13	Jump 26
14	Lref 0
15	Push ~1
16	Add
17	Gref 0
18	Call 1
19	Lref 0
20	Push ~2
21	Add
22	Gref 0
23	Call 1
24	Add
25	Jump 26
26	Ret
```

計測には次のようなコードを使いました。コンパイラは実行効率を無視して書いたのでベンチマークには含めていません。


```sml
val target = (AST.fib 35)
val compiled = VM.compile target
val dtcompiled = DTVM.compile target
val vm = VM.new ()
val dtvm = DTVM.new ()

val _ = Benchmark.benchset "fib 35" 1 [
        ("Interpreter",
         fn () => (Interp.run target; ())),
        ("Normal VM",
         fn () => (VM.run vm compiled; ())),
        ("Direct Threaded VM",
         fn () => (DTVM.run dtvm dtcompiled; ()))
    ]
```

## 結果
Intel Core i5 M450 2.4GHz 2コア4スレッド、Ubunt 15.04、SML/NJ v110.77で実行しました。

```
-------------------+---------------------------------------------------
 Interpreter       |********************************************* 47170ms
 Normal VM         |****************** 19170ms
 Direct Threaded VM|************************************************** 51460ms
-------------------+---------------------------------------------------
```

ダントツで速いのがVMで、インタプリタに比べてかなりの性能向上が見られます。一方DT VMはインタプリタより遅いという結果になりました。

# 考察
冷静に考えたらインタプリタは毎回関数呼び出して遅いよねってことからループで処理を済ませるのがVMなのにVMで毎回関数を呼び出してたら遅いに決まってるじゃん。
というかこれ、Direct Threaded VMじゃないじゃん。死にたい。

# 余談

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr">ループ内でExceptionをhandleしてる所為だった。ループの外に出したら超速になってインタプリタの方が20倍遅くなった。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/604235677337714689">2015, 5月 29</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

尚、このつぶやきはfibの引数を小さくして繰り返しを増やした時のものです。多分コンパイルを外に出してるので繰り返しが多いとその分のオーバーヘッドの差が効いてくるのでしょう。


```sml
val target = (AST.fib 24)
val compiled = VM.compile target
val dtcompiled = DTVM.compile target
val vm = VM.new ()
val dtvm = DTVM.new ()

val _ = Benchmark.benchset "fib 24" 10 [
        ("Interpreter",
         fn () => (Interp.run target; ())),
        ("Normal VM",
         fn () => (VM.run vm compiled; ())),
        ("Direct Threaded VM",
         fn () => (DTVM.run dtvm dtcompiled; ()))
    ]
```

```
-------------------+---------------------------------------------------
 Interpreter       |************************************************** 2052ms
 Normal VM         |** 104ms
 Direct Threaded VM|***** 238ms
-------------------+---------------------------------------------------
```
