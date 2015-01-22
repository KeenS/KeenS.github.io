---
categories: [picrin, Lisp, Scheme]
date: 2015-01-10T09:40:51Z
title: picrinのcodegenプロセスを説明してみる
---
今、picrinのcodegenプロセスに手を入れる機運が高まってます。picrinはバイトコードインタプリタなのでS式を渡されたらそれをバイトコードに変換する作業が必要です。それがcodegen。本当はcompileなんだけどpicrinのCのソースコードのコンパイルと紛らわしいからcodegen。私も完全にはプロセスを理解してないので説明して理解度を試しつつ問題点を洗い出します。
<!--more-->
# 関連ファイル
* extlib/benz/codegen.c
* extlib/benz/macro.c
* (extlib/benz/read.c)
* (extlib/benz/vm.c)

# おおまかな流れ
1. (`read`する)
2. マクロ展開する
3. コードの解析をする
4. バイトコードを生成する
5. (実行する)

今回説明するのは2~4です。

# マクロ展開
入力:Schemeの式  
出力:マクロ展開済みのS式  
副作用:マクロテーブルの更新、ライブラリテーブルの更新、その他マクロ毎の副作用

extlib/benz/macro.cに処理があります。

メインの部分はこれです。

```c
static pic_value
macroexpand_node(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  switch (pic_type(expr)) {
  case PIC_TT_SYMBOL: {
    return macroexpand_symbol(pic, pic_sym(expr), senv);
  }
  case PIC_TT_PAIR: {
    pic_value car;
    struct pic_macro *mac;

    if (! pic_list_p(expr)) {
      pic_errorf(pic, "cannot macroexpand improper list: ~s", expr);
    }

    car = macroexpand(pic, pic_car(pic, expr), senv);
    if (pic_sym_p(car)) {
      pic_sym tag = pic_sym(car);

      if (tag == pic->rDEFINE_SYNTAX) {
        return macroexpand_defsyntax(pic, expr, senv);
      }
      else if (tag == pic->rLAMBDA) {
        return macroexpand_defer(pic, expr, senv);
      }
      else if (tag == pic->rDEFINE) {
        return macroexpand_define(pic, expr, senv);
      }
      else if (tag == pic->rQUOTE) {
        return macroexpand_quote(pic, expr);
      }

      if ((mac = find_macro(pic, tag)) != NULL) {
        return macroexpand_node(pic, macroexpand_macro(pic, mac, expr, senv), senv);
      }
    }

    return pic_cons(pic, car, macroexpand_list(pic, pic_cdr(pic, expr), senv));
  }
  default:
    return expr;
  }
}
```

S式を行きがけ順でマクロ展開していきます。

リストの先頭にあるものをマクロと疑って展開しようとしますが、見ての通り`define-syntax`、`lambda`、`define`、`quote`だけは特別扱いされています。このスペシャルフォーム達は引数に括弧つきのものを取ったり引数を全く触ってほしくなかったりするのでマクロ展開時に特別な処理がされるんですね。他はシンボルは変数名(identifier)として処理し、リストとシンボル以外のものはそのまま返します。

ここに1つ問題があります。`find_macro`です。[イシューにも挙げられて](https://github.com/picrin-scheme/picrin/issues/234)いますが、マクロを定義したあと同じ名前の手続を定義したら手続で上書きされる筈なのにマクロとして展開されてしまいます。これはマクロと手続きで別のテーブルを持っていることに起因します。ちゃんと上書きされるようにするにはマクロも変数束縛テーブルに登録すれば解決出来るのかなと思ってます。

## シンボル
変なことはしてません。スコープを遡っていき、最初に見付かったインターンされたシンボルを返します。見付からなかった(=未定義のシンボルだった)ときは新たなgensymを返します。

## `define-syntax`
```c
static pic_value
macroexpand_defsyntax(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  pic_value var, val;
  pic_sym sym, rename;

  if (pic_length(pic, expr) != 3) {
    pic_errorf(pic, "syntax error");
  }

  var = pic_cadr(pic, expr);
  if (! pic_sym_p(var)) {
    pic_errorf(pic, "binding to non-symbol object");
  }
  sym = pic_sym(var);
  if (! pic_find_rename(pic, senv, sym, &rename)) {
    rename = pic_add_rename(pic, senv, sym);
  } else {
    pic_warnf(pic, "redefining syntax variable: ~s", pic_sym_value(sym));
  }

  val = pic_cadr(pic, pic_cdr(pic, expr));

  pic_try {
    val = pic_eval(pic, val, pic->lib);
  } pic_catch {
    pic_errorf(pic, "macroexpand error while definition: %s", pic_errmsg(pic));
  }

  if (! pic_proc_p(val)) {
    pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", var);
  }

  define_macro(pic, rename, pic_proc_ptr(val), senv);

  return pic_none_value();
}
```
手を入れる候補。やってることは単純で、`(define-syntax <name> <definition>)`の形で引数を渡される筈だからnameをdefinitionに関連づけてマクロとして登録します。返り値はnone。返り値はnone。大事なことなので2回言いました。

一応実行時にはマクロ展開は全て終了してる筈なので`define-syntax`をマクロ展開時に消し去っても良い筈なんですが、気持ち悪いですよね。コアを小さくするという意味では正解なんですが…。あとファイルのコンパイルを考えた時に`define-syntax`をファイルの外に持ち出せなくなるという実用上の問題もあります。

あと、definitionがマクロ展開だけで止まってしまうと困るのでdefinitionだけはevalしているのもキモいポイントですね。

改善案は`define`と同じようにすること。そのためにはマクロ展開後の処理(analyze、codegen、VM命令)にも`define-syntax`用の処理を入れる必要があって、面倒。

## `lambda`
`lambda`の扱いは面倒です。一番分かり易い例は再帰マクロですかね。マクロの定義中に定義中のマクロが見えてる必要があります。この解決策として`lambda`だけはマクロ展開を遅延(defer)します。じゃあいつ遅延されたマクロが展開されるかというと

```c
pic_value
pic_macroexpand(pic_state *pic, pic_value expr, struct pic_lib *lib)
{
  struct pic_lib *prev;
  pic_value v;

#if DEBUG
  puts("before expand:");
  pic_debug(pic, expr);
  puts("");
#endif

  /* change library for macro-expansion time processing */
  prev = pic->lib;
  pic->lib = lib;

  lib->env->defer = pic_nil_value(); /* the last expansion could fail and leave defer field old */

  v = macroexpand(pic, expr, lib->env);

  macroexpand_deferred(pic, lib->env);

  pic->lib = prev;

#if DEBUG
  puts("after expand:");
  pic_debug(pic, v);
  puts("");
#endif

  return v;
}
```

`macroexpand`の後です。`macroexpand`は`macroexpand_node`の薄いラッパと思って下さい。

遅延されたマクロ展開は`senv->defer`にpushされ、`macroexpand_deferred`で1つずつ`macroexpand_lambda`されます。`macroexpand_lambda`はまあ、想像通りです。formalをα変換したあとformalとbodyをマクロ展開します。α変換は`senv`にシンボル -> gensymのキーペアを突っ込むだけです。返り値は`(lambda formal body)`。

## define
比較的単純です。`(define name value)`の他に`(define (name formal) body)`のMIT記法にも対応する必要があるのでマクロ展開時に特別扱いされてます。`(define name value)`を返り値として返します。MIT記法もこの形に正規化されます。

## quote
特に語る事はないです。

## 他のマクロ
Schemeのマクロ展開は簡単で、マクロ手続をbody、展開場所の環境、定義場所の環境の3つを引数として呼び出せば終りです。勿論`macroexpand_node`を見て分かるように最後まで展開します。picrinはCommon Lisp風の伝統的マクロもサポートしてるので伝統的マクロだった時はbodyだけを引数として呼び出します。

## 蛇足
`define-syntax`だけを指摘しましたが、マクロ展開後にnoneになるマクロは他にもあります。library関係全部です。個人的にはやつらもVM命令まで残すべきだと思ってます。

因みにもう1つlibrary関係の問題があります。`define-library`直下の`begin`についてです。

```scheme
(define-library (hoge)
  (begin
    (define foo ...)
    (define bar ...)
  )
)
```

のようなコードを考えます。`define-library`直下の`begin`は他とセマンティクスが違って、`begin`内のコードをトップレベルで定義されたかのように扱う必要があります。しかしpicrinは普通の`begin`と同じように扱っています。これがどのような違いを産むかというと、コンパイル順を見れば分かり易いです。

仕様: fooのマクロ展開->fooの解析->fooのコード生成->barのマクロ展開->barの解析->barのコード生成  
picrin: fooのマクロ展開->barのマクロ展開->fooの解析->barの解析->fooのコード生成->barのコード生成  

picrinのコンパイル順序だとマクロ展開を先にやってしまうのでマクロ展開中に同じ`begin`内で`define`されたシンボルが見えなくなってしまいます。尚、後述しますが今のpicrinはこのコンパイル順序のおかげで手続の相互参照が可能になってます。ここもどうにかしないといけない。

# コード解析
入力:マクロ展開済みのS式  
出力:中間表現  
副作用:無し

extlib/benz/codegen.cに処理があります。

ここの処理ではS式をVM命令に近いS式に書き換えます。`(+ 1 x)`が`(+ (quote 1) (gref x))`になるなど。この解析後のS式は特に呼び名はなさそうなので中間表現と呼んでおきます。

中間表現には`cons`、`car`などの16の基本的な手続に対応する命令や`if`、`define`などの6つのスペシャルフォームの他、`call`、`tailcall`、`call-with-values`、`tailcall-with-values`の4つの手続呼び出し命令、`gref`、`lref`、`cref`の3つの変数参照、`return`があります。合計30の命令です。JIT化も視野に入れてるので基本命令は少なくする方針なんでしょう。また、組み込みも意識しているのでその気になればコアを小さく出来るようにする意味もあるのかもしれません。

`gref`はグローバル変数、`lref`はローカル変数、`cref`はクcaptured変数を参照します。ここでピンときた方も居るかと思いますが、コード解析がスコープ周りを担当しています。

`analyze_*`系は`analyze_state`を持ち回ししますが、こいつはpicrin VMとスコープを表現する構造体の他に中間命令のシンボル(の一部)のキャッシュや中間命令にそのまま翻訳されるべき手続(`cons`や`car`)のキャッシュを持ちます。手続はライブラリに属するのでキャッシュを捜すときに少し特別な処理が入ってますね。中間命令のシンボルのキャッシュが一部だけなのは他は`pic_state`構造体が既に持ってるからですね。

さて、本体はやはり`analyze_node`ですが30命令分のifを持つので少し大きいですね。載せるのをやめて面白いやつだけ取り上げましょうか。どうでもいいけどこれ、caseじゃなくてif elseなので並び換えたら高速化しそうですね。

## 変数
`analyze_var`がスコープの深さでgref、lref、crefに振り分けます。`analyze_*_var`はgref、lref、crefを返すだけの関数。

スコープを遡って変数を捜していき、変数が見付かったスコープの深さ=今いるスコープの深さならグローバル変数、、今いるスコープならローカル変数、それ以外なら捕捉変数です。

```c
static pic_value
analyze_var(analyze_state *state, pic_sym sym)
{
  pic_state *pic = state->pic;
  int depth;

  if ((depth = find_var(state, sym)) == -1) {
    pic_errorf(pic, "unbound variable %s", pic_symbol_name(pic, sym));
  }

  if (depth == state->scope->depth) {
    return analyze_global_var(state, sym);
  } else if (depth == 0) {
    return analyze_local_var(state, sym);
  } else {
    return analyze_free_var(state, sym, depth);
  }
}
```

因みにこれ、変数が見付からなかったらエラー出してますけどSchemeは仕様上は

```scheme
(define (foo) (bar))
(define (bar) ...)
```
のように後から定義することも可能なので仕様に準拠してないことになります。ここが手を入れるポイント2つ目。

改善案は見付からなかった変数は一旦NOWHEREとかに束縛しておいてanalyze時にはエラーを出さない。実行時にNOWHEREに束縛された変数が出てくればエラー。

## define
```c
static pic_value
analyze_define(analyze_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  pic_value var, val;
  pic_sym sym;

  if (pic_length(pic, obj) != 3) {
    pic_errorf(pic, "syntax error");
  }

  var = pic_list_ref(pic, obj, 1);
  if (! pic_sym_p(var)) {
    pic_errorf(pic, "syntax error");
  } else {
    sym = pic_sym(var);
  }
  var = analyze_declare(state, sym);

  if (pic_pair_p(pic_list_ref(pic, obj, 2))
      && pic_sym_p(pic_list_ref(pic, pic_list_ref(pic, obj, 2), 0))
      && pic_sym(pic_list_ref(pic, pic_list_ref(pic, obj, 2), 0)) == pic->rLAMBDA) {
    pic_value formals, body_exprs;

    formals = pic_list_ref(pic, pic_list_ref(pic, obj, 2), 1);
    body_exprs = pic_list_tail(pic, pic_list_ref(pic, obj, 2), 2);

    val = analyze_defer(state, pic_sym_value(sym), formals, body_exprs);
  } else {
    if (pic_length(pic, obj) != 3) {
      pic_errorf(pic, "syntax error");
    }
    val = analyze(state, pic_list_ref(pic, obj, 2), false);
  }

  return pic_list3(pic, pic_symbol_value(pic->sSETBANG), var, val);
}
```

手続を`define`するときに上の後から定義(interreferencial definitionという)を一部の状況で実現するために束縛する値がlambdaかどうかで場合分けしています。lambdaの場合は`analyze_defer`を使います。そうすることで

```scheme
(begin
  (define (foo) (bar))
  (define (bar) ...))
```

と書けばbegin -> define foo -> define bar -> (analyze deferred) -> foo本体 -> bar本体
の順番に解析されることになるのでコンパイルが通ります。

また、`define`された手続に名前をつける(シンボルと手続を関連付けるのではない)役割もあります。

## lambda
また例の再帰定義とかで面倒なやつです。`analyze_lambda`が引数のチェックだけしたら`analyze_defer`に丸投げして、`analyze_defer`はモックのエントリーポイントだけ返してdeferリストに処理をpushします。

んで後から呼ばれる`analyze_deferred`が1つずつ`analyze_procedure`します。マクロ展開と微妙に名前が違うのが気になりますね。

`analyze_procedure`が本体になるのですが、スコープを作るので少しややこしいことになってます。

### スコープと変数
スコープの解説をしてませんでしたね。変数を参照するときはスコープの関係で4種類出てきます。

1. グローバル変数 : トップレベルで`define`されたもの
2. ローカル変数 : lambda内で`define`されたもの
3. 自由変数 : lambdaの外かつトップレベルでない変数
4. 引数 : lambdaの引数

captureについて: 変数を導入した側から見たら「捕捉された変数」で、変数を参照する側から見たら「自由変数」です。

さて、変数の種類を見たところで`analyze_procedure`の定義を見てみましょう。

```c
static pic_value
analyze_procedure(analyze_state *state, pic_value name, pic_value formals, pic_value body_exprs)
{
  pic_state *pic = state->pic;
  pic_value args, locals, varg, captures, body;

  assert(pic_sym_p(name) || pic_false_p(name));

  if (push_scope(state, formals)) {
    analyze_scope *scope = state->scope;
    pic_sym *var;
    size_t i;

    args = pic_nil_value();
    for (i = xv_size(&scope->args); i > 0; --i) {
      var = xv_get(&scope->args, i - 1);
      pic_push(pic, pic_sym_value(*var), args);
    }

    varg = scope->varg
      ? pic_true_value()
      : pic_false_value();

    /* To know what kind of local variables are defined, analyze body at first. */
    body = analyze(state, pic_cons(pic, pic_sym_value(pic->rBEGIN), body_exprs), true);

    analyze_deferred(state);

    locals = pic_nil_value();
    for (i = xv_size(&scope->locals); i > 0; --i) {
      var = xv_get(&scope->locals, i - 1);
      pic_push(pic, pic_sym_value(*var), locals);
    }

    captures = pic_nil_value();
    for (i = xv_size(&scope->captures); i > 0; --i) {
      var = xv_get(&scope->captures, i - 1);
      pic_push(pic, pic_sym_value(*var), captures);
    }

    pop_scope(state);
  }
  else {
    pic_errorf(pic, "invalid formal syntax: ~s", args);
  }

  return pic_list7(pic, pic_sym_value(pic->sLAMBDA), name, args, locals, varg, captures, body);
}
```

スコープを作ったあとは最初に引数をスコープに登録して本体内ででcapture出来るようにしてます。んで本体をanalyzeしてローカル変数と捕捉された変数を洗い出してます。

因みにvargというのはCommon Lispでいう&restや&optionalを引っ括めたものです。

## begin

```c
static pic_value
analyze_begin(analyze_state *state, pic_value obj, bool tailpos)
{
  pic_state *pic = state->pic;
  pic_value seq;
  bool tail;

  switch (pic_length(pic, obj)) {
  case 1:
    return analyze(state, pic_none_value(), tailpos);
  case 2:
    return analyze(state, pic_list_ref(pic, obj, 1), tailpos);
  default:
    seq = pic_list1(pic, pic_symbol_value(pic->sBEGIN));
    for (obj = pic_cdr(pic, obj); ! pic_nil_p(obj); obj = pic_cdr(pic, obj)) {
      if (pic_nil_p(pic_cdr(pic, obj))) {
        tail = tailpos;
      } else {
        tail = false;
      }
      seq = pic_cons(pic, analyze(state, pic_car(pic, obj), tail), seq);
    }
    return pic_reverse(pic, seq);
  }
}
```
そんなに大したコードじゃないんですけど末尾呼出最適化のためのコードが垣間見えたので載せました。

`begin`自身が末尾位置にいれば`begin`の最後の式は末尾位置になりますがそれ以外は必ず非末尾位置になるよねーってコードです。

# バイトコード生成
入力:中間表現  
出力:バイトコード  
副作用:無し  

extlib/benz/codegen.cに処理があります。

ここの処理では中間表現をスタックベースのpicrin VM命令に変換します。

## VM命令やlambdaの内部表現
### VM命令フォーマット
フォーマットは3種類あります。

```c
struct pic_code {
  enum pic_opcode insn;
  union {
    int i;
    char c;
    struct {
      int depth;
      int idx;
    } r;
  } u;
};
```

見ての通り命令番号(opcode)+

1. int
2. char
3. int2つ

になっています。因みに普通使われるのは1. intで、2. charはOP_PUSHCHARのみ、3. int2つはOP_CREFとOP_CSETのみに使われます。また、引数(?)を全く持たない命令もあります(スタックマシンなため)。

### 愉快なVM命令たち
```c
enum pic_opcode {
  OP_NOP,
  OP_POP,
  OP_PUSHNIL,
  OP_PUSHTRUE,
  OP_PUSHFALSE,
  OP_PUSHINT,
  OP_PUSHCHAR,
  OP_PUSHCONST,
  OP_GREF,
  OP_GSET,
  OP_LREF,
  OP_LSET,
  OP_CREF,
  OP_CSET,
  OP_JMP,
  OP_JMPIF,
  OP_NOT,
  OP_CALL,
  OP_TAILCALL,
  OP_RET,
  OP_LAMBDA,
  OP_CONS,
  OP_CAR,
  OP_CDR,
  OP_NILP,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_MINUS,
  OP_EQ,
  OP_LT,
  OP_LE,
  OP_STOP
}
```
大体名前から分かるかと思います。`quote`は`OP_PUSH*`系になります。`OP_STOP`はあるのに`OP_START`がないのは分かりますね。開始はユーザーの操作から始まるのに対して停止はプログラム側が行うからです。

### lambdaとか
lambdaとかのエントリポイント付きのコード1まとまりはirepと呼ばれています。

```c
struct pic_irep {
  PIC_OBJECT_HEADER
  pic_sym name;
  pic_code *code;
  int argc, localc, capturec;
  bool varg;
  struct pic_irep **irep;
  pic_valuel *pool;
  size_t clen, ilen, plen;
};
```

* `PIC_OBJECT_HEADER`: オブジェクトタイプのタグ
* `name`: 名前（あれば）
* `*code`: VM命令たち
* `argc`、 `localc`、 `capturec`: 引数数、ローカル変数数、捕捉された変数数
* `varg`: &rest引数を持つか
* `**irep`: irepプール
* `*pool`: 定数プール
* `clen`、`ilen`、`plen`: \*code、\*\*irep、\*poolの長さ

スコープを作るので変数を格納するpoolを持ちます。そしてlambdaだけは定数プールとは別にirepに格納します。

## コード生成
本体は`codegen`なんですがこれまた長いので掻い摘んで。帰りがけ順でコード生成します。

### cons
まずは肩馴らし。

```c
  else if (sym == pic->sCONS) {
    codegen(state, pic_list_ref(pic, obj, 1));
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_CONS;
    cxt->clen++;
    return;
  }
```
引数1と引数2を生成する命令を吐いてから`OP_CONS`命令を吐きます。引数1と引数2は自ら値をスタックにPUSHします。

### quote
少し長いですが。

```c
  else if (sym == pic->sQUOTE) {
    int pidx;

    obj = pic_list_ref(pic, obj, 1);
    switch (pic_type(obj)) {
    case PIC_TT_BOOL:
      if (pic_true_p(obj)) {
        cxt->code[cxt->clen].insn = OP_PUSHTRUE;
      } else {
        cxt->code[cxt->clen].insn = OP_PUSHFALSE;
      }
      cxt->clen++;
      return;
    case PIC_TT_INT:
      cxt->code[cxt->clen].insn = OP_PUSHINT;
      cxt->code[cxt->clen].u.i = pic_int(obj);
      cxt->clen++;
      return;
    case PIC_TT_NIL:
      cxt->code[cxt->clen].insn = OP_PUSHNIL;
      cxt->clen++;
      return;
    case PIC_TT_CHAR:
      cxt->code[cxt->clen].insn = OP_PUSHCHAR;
      cxt->code[cxt->clen].u.c = pic_char(obj);
      cxt->clen++;
      return;
    default:
      if (cxt->plen >= cxt->pcapa) {
        cxt->pcapa *= 2;
        cxt->pool = pic_realloc(pic, cxt->pool, sizeof(pic_value) * cxt->pcapa);
      }
      pidx = (int)cxt->plen++;
      cxt->pool[pidx] = obj;
      cxt->code[cxt->clen].insn = OP_PUSHCONST;
      cxt->code[cxt->clen].u.i = pidx;
      cxt->clen++;
      return;
    }
  }
```

リテラルや定数をPUSHする命令を吐いてますね。

### REF系
local、captureはirep構造体についてるのpoolを参照します。globalはVMのシンボルテーブルから直接引きます。因みにpicrinのシンボルはただのintです。

captureは上位のスコープで既に出てきた変数を参照するだけなので「n個上位のm番目のcaptured変数」と指定するだけなので簡単ですが、localはarg、普通のlocal、下位スコープにcaptureされたlocal、の3つに分けて置いているので少し面倒です。

```c
  if (sym == state->sGREF) {
    cxt->code[cxt->clen].insn = OP_GREF;
    cxt->code[cxt->clen].u.i = pic_sym(pic_list_ref(pic, obj, 1));
    cxt->clen++;
    return;
  } else if (sym == state->sCREF) {
    pic_sym name;
    int depth;

    depth = pic_int(pic_list_ref(pic, obj, 1));
    name  = pic_sym(pic_list_ref(pic, obj, 2));
    cxt->code[cxt->clen].insn = OP_CREF;
    cxt->code[cxt->clen].u.r.depth = depth;
    cxt->code[cxt->clen].u.r.idx = index_capture(state, name, depth);
    cxt->clen++;
    return;
  } else if (sym == state->sLREF) {
    pic_sym name;
    int i;

    name = pic_sym(pic_list_ref(pic, obj, 1));
    if ((i = index_capture(state, name, 0)) != -1) {
      cxt->code[cxt->clen].insn = OP_LREF;
      cxt->code[cxt->clen].u.i = i + (int)xv_size(&cxt->args) + (int)xv_size(&cxt->locals) + 1;
      cxt->clen++;
      return;
    }
    cxt->code[cxt->clen].insn = OP_LREF;
    cxt->code[cxt->clen].u.i = index_local(state, name);
    cxt->clen++;
    return;
  }
```

因みにset系もほぼ同じようなコードです。

### lambda


```c
  else if (sym == pic->sLAMBDA) {
    int k;

    if (cxt->ilen >= cxt->icapa) {
      cxt->icapa *= 2;
      cxt->irep = pic_realloc(pic, cxt->irep, sizeof(struct pic_irep *) * cxt->icapa);
    }
    k = (int)cxt->ilen++;
    cxt->code[cxt->clen].insn = OP_LAMBDA;
    cxt->code[cxt->clen].u.i = k;
    cxt->clen++;

    cxt->irep[k] = codegen_lambda(state, obj);
    return;
  }
```

`codegen_lambda`に投げているのでこの部分は簡単です。irepプールにコードを置いてそれを参照する命令を吐くだけ。

`codegen_lambda`はというとそこまで長くなくて

```c
static struct pic_irep *
codegen_lambda(codegen_state *state, pic_value obj)
{
  pic_state *pic = state->pic;
  pic_value name, args, locals, closes, body;
  bool varg;

  name = pic_list_ref(pic, obj, 1);
  args = pic_list_ref(pic, obj, 2);
  locals = pic_list_ref(pic, obj, 3);
  varg = pic_true_p(pic_list_ref(pic, obj, 4));
  closes = pic_list_ref(pic, obj, 5);
  body = pic_list_ref(pic, obj, 6);

  /* inner environment */
  push_codegen_context(state, name, args, locals, varg, closes);
  {
    /* body */
    codegen(state, body);
  }
  return pop_codegen_context(state);
}
```

単純に本体に対して`codegen`を呼んで出来た命令を取り出してるだけです。

### if
schemeはほとんどの制御を継続に任せているのでなんとjmp命令を吐くのはifだけです。

```c
  else if (sym == pic->sIF) {
    int s, t;

    codegen(state, pic_list_ref(pic, obj, 1));

    cxt->code[cxt->clen].insn = OP_JMPIF;
    s = (int)cxt->clen++;

    /* if false branch */
    codegen(state, pic_list_ref(pic, obj, 3));
    cxt->code[cxt->clen].insn = OP_JMP;
    t = (int)cxt->clen++;

    cxt->code[s].u.i = (int)cxt->clen - s;

    /* if true branch */
    codegen(state, pic_list_ref(pic, obj, 2));
    cxt->code[t].u.i = (int)cxt->clen - t;
    return;
  }
```

### call
思ったより短いです。単純に引数を評価する命令を吐いたあと引数の数を指定して呼ぶだけです。

```c
  else if (sym == state->sCALL || sym == state->sTAILCALL) {
    int len = (int)pic_length(pic, obj);
    pic_value elt;

    pic_for_each (elt, pic_cdr(pic, obj)) {
      codegen(state, elt);
    }
    cxt->code[cxt->clen].insn = (sym == state->sCALL) ? OP_CALL : OP_TAILCALL;
    cxt->code[cxt->clen].u.i = len - 1;
    cxt->clen++;
    return;
  }
```

# 最後に
これを書くのに半日掛かりましたがちゃんとコンパイルプロセスを理解して問題を洗い出すことが出来ました。良かった。

今のところ中間表現までS式なのでS式操作に長けたSchemeでコンパイラ書けないかなとか考えてますが現状マクロ展開で副作用があるので厳しいですね。

因みに気付いた方もいらっしゃるかと思いますが今のpicrinは最適化をtco以外は行っていません。行うとしたら

1. マクロ展開後にコンパイラマクロ展開を挟む
2. 中間表現から一旦ssa形式の第二中間表現に落して定数伝播や畳み込みなどの一般的な最適化を行なう
3. 生成されたvm命令に対して覗き穴最適化を行なう

あたりかなと思います。1. は楽しそうですけど普通のマクロ展開のバグとか問題に悩んでる今入れるべきではないですし、2. は効果高いですしjit化の布石にもなりますが実装コストが高いのでwasabiz次第かなといったところ、3. は今どの程度非効率な命令が吐かれてるのか分からないので効果が見えにくいですね。

いずれにせよ最適化は仕様準拠してからにしますか。

何か質問や突っ込みがあれば@blackenedgoldまで。
