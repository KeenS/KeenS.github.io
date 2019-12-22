---
categories: [ML, SML, SMLSharp]
date: 2015-04-19T13:51:33+09:00
title: SMLSharpでFFIバインディングを書く時の知見
---
κeenです。最近頻繁にSML#を使ってます。SML#のメイン機能の1つであるC連携ですが、ちょっと複雑なことをやろうとするとテクニックが必要になるので共有します。

Twitterとかにコメントや突っ込みお願いします。
<!--more-->
# 簡単な型
型が簡単な関数なら普通に`_import`で済みます。

```sml
val puts = _import "puts": string -> ()
val () = puts "Hello, C"
```

尚、簡単な型とは[公式ドキュメント](http://www.pllab.riec.tohoku.ac.jp/smlsharp/docs/2.0/ja/Ch9.S2.xhtml)にある通り、いわゆる即値、即値の組（タプル）、即値の配列、即値の参照、それらを引数、返り値に持つ関数などです。

又、以下のような制約もあります。

> C関数全体の型は，引数リストを組型とするMLの関数型に対応付けられます． ただし，C関数の引数や返り値に書ける相互運用型には，以下の制約があり ます．
>    配列型や組型など，Cのポインタ型に対応する相互運用型を，C関数の返り値の 型として指定することはできません．

恐らくGCとの兼ね合いでしょうがつらいですね。stringすら返り値で受け取れません。


それにこの制約がどこまで効いてるのかが不明で、同じ型でも型付けに成功したり失敗したりすることがあります。例えば上の例でstring型を引数にとる関数をインポートしましたが関数に依ってはstringが相互運用型でないとか怒られることがあります。タプルの配列やタプルの参照などは確実にダメみたいです。

尚、string型はCでいう `const char *` 、タプルは変更不能な構造体へのポインタになるそうです。構造体の即値は扱えないんですね…。また、参照とは別に `ptr` 型も存在します。SML#側からは作れず、Cとの相互運用のためだけに存在するようです。

# 魔法の `unit ptr`
じゃあ複雑な型はインポート出来ないのかというとそうでもなく、 `unit ptr` 型にしておけばとりあえずインポート出来ます。Cで言うところの `void *` です。
邪悪な雰囲気を感じますね。しかしそこは型安全言語、ちゃんと型安全に `unit ptr` を扱えます。

```sml
type file = unit ptr
val fopen = _import "fopen": (string, string) -> file
val fgetc = _import "fgetc": (file) -> int
val fclose = _import "fclose": (file) -> int
val () = let
      val f = fopen("test", "r")
      val c = fgetc(f)
    in
      print(str(chr c));
      fclose(f)
    end

```

はい。単に `type` で名前をつけてあげれば大丈夫です。SML#側ではポイント先が何であるかには関知せず、インポートしたC関数の間で完結してれば問題ありません。多くのライブラリはそのようなAPIになっているのではないでしょうか。

# ポインタを扱う
とはいえ時にポインタを扱う必要もあります。構造体の配列を扱えないのでその辺で。

そういった時に便利なのが `SMLSharp_Builtin.Pointer` と `Pointer` です。 `Pointer` の方は .smi ファイルの中で `_require "ffi.smi"` してから使います。

```sml
structure SMLSharp_Builtin
  structure Pointer =
  struct
    val identityEqual = _builtin val IdentityEqual : boxed * boxed -> bool
    val advance = _builtin val Ptr_advance : 'a ptr * int -> 'a ptr
    val deref = _builtin val Ptr_deref : 'a ptr -> 'a
    val store = _builtin val Ptr_store : 'a ptr * 'a -> unit

    val toUnitPtr = _builtin val Cast : 'a ptr -> unit ptr
    val fromUnitPtr = _builtin val Cast : unit ptr -> 'a ptr
    val toCodeptr = _builtin val BitCast : unit ptr -> codeptr
  end
  end

structure Pointer =
struct
  val advance = SMLSharp_Builtin.Pointer.advance

  val load =
      case 'a in 'a ptr -> 'a of
        int => SMLSharp_Builtin.Pointer.deref
      | word => SMLSharp_Builtin.Pointer.deref
      | SMLSharp_Builtin.Word8.word => SMLSharp_Builtin.Pointer.deref
      | char => SMLSharp_Builtin.Pointer.deref
      | real => SMLSharp_Builtin.Pointer.deref
      | SMLSharp_Builtin.Real32.real => SMLSharp_Builtin.Pointer.deref
      | 'b ptr => SMLSharp_Builtin.Pointer.deref

  val store =
      case 'a in 'a ptr * 'a -> unit of
        int => SMLSharp_Builtin.Pointer.store
      | word => SMLSharp_Builtin.Pointer.store
      | SMLSharp_Builtin.Word8.word => SMLSharp_Builtin.Pointer.store
      | char => SMLSharp_Builtin.Pointer.store
      | real => SMLSharp_Builtin.Pointer.store
      | SMLSharp_Builtin.Real32.real => SMLSharp_Builtin.Pointer.store
      | 'b ptr => SMLSharp_Builtin.Pointer.store

  val isNull : 'a ptr -> bool
  val NULL : unit -> 'a ptr

  val importBytes : SMLSharp_Builtin.Word8.word ptr * int
                    -> SMLSharp_Builtin.Word8.word vector
  val importString : char ptr -> string
end

```

`load` 、 `store` 、 `deref` 、 `advance` あたりを良く使いそうですね。

実際にあった話。 `struct header { const char *name; int name_len; const char *value; int value_len}` の配列(`struct header *`)を扱う必要がありました。
その配列をCの関数に渡して書き換えてもらって、後で値を取り出したいという状況がです。その時値を取り出すコードがこれです。

```sml
fun getHeader headers i =
      let
          val header_ptr : char ptr ptr = fromUnitPtr(headers)
          val header_ptr = advance(header_ptr, i * 2)
          val header_ptr : int ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val header_ptr = advance(header_ptr , i * 2)

          val header_ptr : char ptr ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val name = deref(header_ptr)
          val header_ptr = advance(header_ptr, 1)

          val header_ptr : int ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val nameLen = deref(header_ptr)
          val header_ptr = advance(header_ptr, 1)

          val header_ptr : char ptr ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val value = deref(header_ptr)
          val header_ptr = advance(header_ptr, 1)

          val header_ptr : int ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val valueLen = deref(header_ptr)
      in
          if isNull name
          then (NONE, String.substring(importString(value), 0, valueLen))
          else (SOME(String.substring(importString(name), 0, nameLen)),
                String.substring(importString(value), 0, valueLen))
      end

```


まず、タプルは構造体へのポインタなので今回の `struct header *` は `(string * int * string * int) ptr` ではありません。それは `struct header **` になってしまいます。
また、ポインタを扱う関数が `ptr` 型しか受け付けないので `string` ではなく `char ptr` にしておいて後から `importString` で文字列にする戦略をとります。

そして配列のi番目にアクセスしたかったら先述の通り `(string * int * string * int) ptr` ではないので地道に `char ptr ptr` 2*i個分、 `int ptr` 2*i個分ポインタを進めます。
ポインタの型を変える時はダイレクトには変換出来ないようなので一旦 `unit ptr` を経由してから変換。そして次のメンバにアクセスするために `advance` という形をとります。

そこまでしたら後は `deref` してあげれば欲しい値がとれます。


# REPLからのimportと `DynamicLink`
SML#のREPLからも勿論インポート出来ますが、SML#のランタイムにリンクされてないライブラリのものはインポート出来ないのでダイナミックリンクを使います。 `DynamicLink` にCの `dl*` と同じ関数群が用意されているのでそれらを使います。

```sml
structure DynamicLink =
struct
  type lib (= ptr)
  datatype scope = LOCAL | GLOBAL
  datatype mode = LAZY | NOW
  val dlopen : string -> lib
  val dlopen' : string * scope * mode -> lib
  val dlsym : lib * string -> codeptr
  val dlsym' : lib * string -> unit ptr
  val dlclose : lib -> unit
end
```

`val lib = dlopen("libawsome.so")` でライブラリのオープン、 `dlsym(lib, "awm_function"): _import () ->unit` で読み込みです。


これでインポートする関数は必要になった時に読み込んで欲しいのですがトップレベルで `val` でバインドすると即読み込まれてしまいます。その辺を上手くやるテクニックがSML#のソースにありました。MySQLのバインディングの部分です。

```sml
  fun lazy f =
      let
        val r = ref NONE
      in
        fn () =>
           case !r of
             SOME x => x
           | NONE =>
             let val x = f ()
             in r := SOME x; x
             end
      end

  val lib =
      lazy (fn _ =>
               DynamicLink.dlopen
                 (case OS.Process.getEnv "SMLSHARP_LIBMYSQLCLIENT" of
                    NONE => "libmysqlclient.16." ^ SMLSharp_Config.DLLEXT ()
                  | SOME x => x))

  fun find s = DynamicLink.dlsym(lib (), s)
  val mysql_init =
      lazy (fn _ => find "mysql_init"
                    : _import (MYSQL) -> MYSQL)


...

```

遅延評価してますね。これ。呼び出す時は `mysql_init () (mysql)` みたいに一旦lazyを剥がさないといけないので注意です。

# 問題とか
## Cの仕様

確かCの仕様上構造体のメモリ上の表現にはメンバ以外のものも置いていいことになっていた筈です。
上の方法では変なコンパイラでコンパイルしたコードだと動きそうにないですね。GCCやClangは大丈夫な筈。

そもそもSML#自体GCCとABI互換なコンパイラでコンパイルしたものじゃないとリンク出来なそうな気がするので杞憂ですかね。

## メモリ確保

Cの関数から構造体のポインタが返ってくるケースだと良いんですが自分で構造体を用意してCの関数に渡すケースだとメモリの確保が問題になります。現状、

```c
struct header
*prepare_headers(int n)
{
  return malloc(n * sizeof(struct header));
}
```

みたいなヘルパ関数を用意して凌いでますが欲しい構造体毎に書かないといけないのであまり嬉しくないです。 `sizeof` をSML#側でとれれば単に `malloc` をバインドするだけで済むのに。
もう少し欲を言うと `malloc` したらGCから外れそうな気がするので明示的に `free` する必要がありそうです。GCに載るメモリ確保関数も欲しいですね。
さらに欲を言うとスタックアロケートする版のメモリ確保関数も欲しい。もしかしたら `alloca` で大丈夫なんでしょうか。

## `#define`
ヘッダファイル内で定数を `#define` してあることが多々あります。それらは地道に手書きでSML#側に持ってくることになりますが気になるのが互換性の問題。
特にOSのヘッダファイルには名前は同じだけどOS毎に値が異なるものが存在します。シグナルとか。OSで条件分岐するか理想的にはプリプロセッサのサポートがあればどうにかなりそうなんですけどねぇ。
現状だとCで定数を返すgetter関数を書いてSML#側でインポートして…ってやればどうにか出来そうですけどやりたくないですね。

## 変数
私はまだ遭遇してないのですがライブラリによってはグローバル変数にアクセスしないといけないものが存在します。これもgetterとsetterを書いて…ってやるとどうにか出来そうですがどうせなら変数のインポートも出来ると良いですよね。


## まとめ
* SML#でCのバインドを書く時は少しテクニックが必要
* SML#にはポインタを直接扱える関数もある
* それでも機能が足りない時はCでヘルパ関数を書こう。
* ダイナミックリンクライブラリも扱えるよ
