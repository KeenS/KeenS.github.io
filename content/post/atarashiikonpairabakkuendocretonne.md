---
categories: [Rust, 言語実装, Advent Calendar, Advent Calendar 2016]
date: 2016-12-02T14:00:11+09:00
title: 新しいコンパイラバックエンドcretonne
---

このエントリは[言語実装 Advent Calendar 2016 - Qiita](http://qiita.com/advent-calendar/2016/lang_dev)3日目の記事です。

κeenです。先日、Rustの開発者フォーラムに新しいバックエンドして[cretonne](https://github.com/stoklund/cretonne)が[提案されていました](https://internals.rust-lang.org/t/possible-alternative-compiler-backend-cretonne/4275)。

ちょっと気になったのでそれを紹介します。

<!--more-->


# 概略
* [GitHub](https://github.com/stoklund/cretonne)
* [ドキュメント](http://cretonne.readthedocs.io/en/latest/index.html)

フォーラムによると、WebAssemblyをエンコードするために作られたコンパイラバックエンドで、FirefoxのJSエンジンであるSpiderMonkeyに載せる予定だそうです。JITが主な目的？

LLVMのように最適化を頑張る方ではなくてコードを吐くまでのレイテンシを気にして作られているとのこと。なのでcretonne自身は最適化を行いません。

特徴はドキュメントの[LLVMとの比較](http://cretonne.readthedocs.io/en/latest/compare-llvm.html)によくまとまってますが、

* LLVMは複数種類の中間言語を持つのに対してcretonneは1種類
* アセンブラもディスアセンブラもない
  + コードジェネレータが吐く命令だけサポート
* 中間言語はISA非依存で、legalization / instruction selectionをするとISA固有のアノテーションが付く
* 最大抽象単位が関数(LLVMはモジュール)。cretonneは関数のインライン化とかもしない。
* Extended Basic Blockを使う(LLVMはBasic Block)。
  + ebbは分岐命令のfalse branchを指定せず、fall throughする。よくあるマシン語に近い。
  + 分岐の合流はebbに引数を持たせることで行う(LLVMはphi nodeを使う)。
* **undefined behaviorが存在しない**

なんとなくWebAssemblyを意識した仕様ですね。

# 雰囲気

以下のCのコードは

``` c
float
average(const float *array, size_t count)
{
    double sum = 0;
    for (size_t i = 0; i < count; i++)
        sum += array[i];
    return sum / count;
}
```

以下のcretonneのIRに落ちるそうです。

```
function average(i32, i32) -> f32 {
    ss1 = stack_slot 8, align 4   ; Stack slot for ``sum``.

ebb1(v1: i32, v2: i32):
    v3 = f64const 0x0.0
    stack_store v3, ss1
    brz v2, ebb3                  ; Handle count == 0.
    v4 = iconst.i32 0
    br ebb2(v4)

ebb2(v5: i32):
    v6 = imul_imm v5, 4
    v7 = iadd v1, v6
    v8 = heap_load.f32 v7         ; array[i]
    v9 = fext.f64 v8
    v10 = stack_load.f64 ss1
    v11 = fadd v9, v10
    stack_store v11, ss1
    v12 = iadd_imm v5, 1
    v13 = icmp ult v12, v2
    brnz v13, ebb2(v12)           ; Loop backedge.
    v14 = stack_load.f64 ss1
    v15 = cvt_utof.f64 v2
    v16 = fdiv v14, v15
    v17 = ftrunc.f32 v16
    return v17

ebb3:
    v100 = f32const qNaN
    return v100
```


まあ、実際はファイルヘッダとかも付きますがこんな感じです。


ebbに引数があったりebbの途中で平気でブランチしてたり中々やんちゃですね。

# 使ってみる

コードを吐くところまでやりたかったのですがまだ絶賛開発中ということもあってそのような操作が見当りませんでした。

ということでファイルの方を扱ってると面白みがないのでRust APIの方で関数を構築して正当性を検証してから中間言語を吐き出してみます。

## 一歩

まずはcretonneをcloneしてきます。

```
$ cargo new cretonne-sample --bin
$ cd cretonne-sample
```


んでCargo.tomlのdependenciesに

```
cretonne = {path = "path/cretonne/lib/cretonne/"}
```

を追加します。どうやらトップレベルのプロジェクトはただのユーティリティのようでした。本体は`lib/cretonne/`以下にいます。


そしてmainファイルの中身はこうです。

``` rust
extern crate cretonne;
use cretonne::ir::*;
use cretonne::ir::{types as ty};
use cretonne::ir::{immediates as imm};
use cretonne::isa;
use cretonne::settings::{self, Configurable};
use cretonne::{write_function, legalize_function, verify_function};

fn main() {
    let mut func = {
        let name = FunctionName::new("average");
        let mut sig = Signature::new();
        sig.argument_types.push(ArgumentType::new(ty::I32));
        sig.argument_types.push(ArgumentType::new(ty::I32));
        sig.return_types.push(ArgumentType::new(ty::F32));
        Function::with_name_signature(name, sig)
    };

    let isa = {
        let mut b = settings::builder();
        b.set("opt_level", "fastest").unwrap();
        let f = settings::Flags::new(&b);
        let builder = isa::lookup("intel").unwrap();
        builder.finish(f)
    };
    verify_function(&func).unwrap();
    legalize_function(&mut func, isa.as_ref());
    let mut o = String::new();
    write_function(&mut o, &func, Some(isa.as_ref())).unwrap();
    println!("{}", o);
}

```

``` rust
function average(i32, i32) -> f32 {
}
```

おおまかには`func`を構築して`isa`を決めて`verify_function`で検証、さらに`legalize_function`で今回のアーキテクチャ向けに微調整、`write_function`で書き出しです。

## スタックスロットの追加

まあ、スタック領域ですね。8byteの領域を確保します。

`func`にstack_slotsフィールドがあるのでそこにpushします。

``` rust
fn main() {
    let mut func = {
        ....
    };


    {
        let k = func.stack_slots.push(StackSlotData::new(8));
    }

    let isa = {
        ....
    };
    ....
}

```

```
function average(i32, i32) -> f32 {
    ss0 = stack_slot 8
}
```

`push`の返り値はスタック領域を指すキーです。

## EBBの追加

`func`構造体のフィールドに`dfg`がいて、そいつを色々いじります。

``` rust
fn main() {
    let mut func = {
        ....
    };


    {
        let k = func.stack_slots.push(StackSlotData::new(8));
        let cur = &mut Cursor::new(&mut func.layout);
        let ebb0 = func.dfg.make_ebb();
        let v1 = func.dfg.append_ebb_arg(ebb0, types::I32);
        let v2 = func.dfg.append_ebb_arg(ebb0, types::I32);

        cur.insert_ebb(ebb0);
    }

    let isa = {
        ....
    };
    ....
}

```

```
function average(i32, i32) -> f32 {
    ss0 = stack_slot 8

ebb0(vx0: i32, vx1: i32):
}
```

EBBの追加にはカーソルを決めてあげて、dfgにebbを作ってあげて、カーソルの位置にebbの追加という形になります。

EBBには任意に引数を追加出来ます。`append_ebb_arg`の返り値はSSAの変数です。

## 命令を追加する

命令はpythonのスクリプトによって生成されるので補完が効かず、中々扱いづらいですが頑張ります。

さて、今回コードの検証を入れているので以下のようなコードは検証に落ちてしまいます。

``` rust
    ....
    {
        let k = func.stack_slots.push(StackSlotData::new(8));
        let cur = &mut Cursor::new(&mut func.layout);
        let dfg = &mut func.dfg;
        let ebb0 = dfg.make_ebb();
        let v1 = dfg.append_ebb_arg(ebb0, types::I32);
        let v2 = dfg.append_ebb_arg(ebb0, types::I32);
        cur.insert_ebb(ebb0);

        let v3 = dfg.ins(cur).f64const(imm::Ieee64::new(0.0));
    }
    ....
```

```
thread 'main' panicked at 'called `Result::unwrap()` on an `Err` value: Error { location: Ebb(Ebb(0)), message: "block does not end in a terminator instruction!" }', ../src/libcore/result.rs:799
```

最後の命令が定数であるため、正常なブロックと見做されないのです。

ひとまずv3を返すことで凌ぎます。

```rust
    ....
    {
        ....
        let mut varg = VariableArgs::new();
        varg.push(v3);
        let _ = dfg.ins(cur).return_(varg);
    }
    ....
```

```
function average(i32, i32) -> f32 {
    ss0 = stack_slot 8

                    ebb0(vx0: i32, vx1: i32):
[-]                     v0 = f64const 0.0
[-]                     return v0
}

```

命令の頭に付いてる `[-]`はlegalizeすると付くようです。


## ブランチ

もう1つebbを追加してあげる必要があります。

``` rust
    {
        let k = func.stack_slots.push(StackSlotData::new(8));
        let cur = &mut Cursor::new(&mut func.layout);
        let dfg = &mut func.dfg;
        let ebb0 = dfg.make_ebb();
        let ebb3 = dfg.make_ebb();  // <- ebb3を作成
        let v1 = dfg.append_ebb_arg(ebb0, types::I32);
        let v2 = dfg.append_ebb_arg(ebb0, types::I32);
        cur.insert_ebb(ebb0);
        let v3 = dfg.ins(cur).f64const(imm::Ieee64::new(0.0));
        dfg.ins(cur).brz(v2, ebb3, VariableArgs::new()); // <-ebb3にジャンプ

        let mut varg = VariableArgs::new();
        varg.push(v3);
        let _ = dfg.ins(cur).return_(varg);

        cur.insert_ebb(ebb3); // <- ebb3を追加
    }
```


```
function average(i32, i32) -> f32 {
    ss0 = stack_slot 8

                    ebb0(vx0: i32, vx1: i32):
[-]                     v0 = f64const 0.0
[-]                     brz vx1, ebb1
[-]                     return v0

                    ebb1:
}
```

## 残り

``` rust
{
        let k = func.stack_slots.push(StackSlotData::new(8));
        let cur = &mut Cursor::new(&mut func.layout);
        let dfg = &mut func.dfg;

        let ebb0 = dfg.make_ebb();
        let v1 = dfg.append_ebb_arg(ebb0, types::I32);
        let v2 = dfg.append_ebb_arg(ebb0, types::I32);

        let ebb2 =  dfg.make_ebb();
        let v5 = dfg.append_ebb_arg(ebb0, types::I32);

        let ebb3 = dfg.make_ebb();

        cur.insert_ebb(ebb0);
        let v3 = dfg.ins(cur).f64const(imm::Ieee64::new(0.0));
        dfg.ins(cur).brz(v2, ebb3, VariableArgs::new());
        let v4 = dfg.ins(cur).iconst(types::I32, 0);
        let mut ebb2_arg = VariableArgs::new();
        ebb2_arg.push(v4);
        dfg.ins(cur).jump(ebb2, ebb2_arg);

        cur.insert_ebb(ebb2);
        let v6 = dfg.ins(cur).imul_imm(v5, 4);
        let v7 = dfg.ins(cur).iadd(v1, v6);
        dfg.ins(cur).jump(ebb3, VariableArgs::new());

        cur.insert_ebb(ebb3);
    }
```

こんな感じで進めていこうと思いましたがどうやらまだstack/heapを触る命令がない？？ようなので詰みました。


# まとめ

* コンパイラバックエンドcretonneについて紹介しました
* cretonneのRust APIを触ってみましたがダメでした。
