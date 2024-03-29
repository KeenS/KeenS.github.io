---
categories: [ML, SML, SMLSharp]
date: 2017-07-17T21:04:57+09:00
description: null
title: 最近のSML#の話題
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# 最近のSML#の話題
----------------------
[ML勉強会 #2 - connpass](https://ml-lang.connpass.com/event/58151/)
<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# 話すこと
------

* SML# 3.0。0以降立て続けにリリースが続いてる
* 最近の変更を追いきれないのでまとめて追う

ver.  | date
------|-----
3.3.0 | 2017-06-20
3.2.0 | 2016-09-16
3.1.1 | 2016-07-15
3.1.0 | 2016-05-26
3.0.1 | 2016-04-04
3.0.0 | 2016-03-31
2.0.0 | 2014-04-04
1.2.0 | 2012-11-14


===

# そもそもSML#って？
-------------------

* SML '97 互換のコンパイラ
  + [The Definition of Standard ML, Revised](http://sml-family.org/sml97-defn.pdf)
  + Self Hosted
  + 2.0.0からLLVMベース
* 実用的な処理系を目指して作られてる
* 独自機能も多数
* コンパイラとして使いやすい
  + 分割コンパイル
  + REPL
* 速度は遅め

===
# SML#の独自機能
----------------
3.0.0以降の変更を追うのに一部前提知識として必要。  
とりあえず型を読めるようになって

* 第一級オーバーロード
* ランク1多相
* 多相レコード
* SQL連携
* C FFI
* マルチスレッドサポート

===
# ランク1多相
-------------

* SMLでは冠頭多相しかない
  ``` sml
  - fun pair x y = (x, y);
  val pair = fn : 'a -> 'b -> 'a * 'b
  (* pair 1: 'b -> (int, 'b) にはならない *)
  - pair 1;
  stdIn:4.1-4.7 Warning: type vars not generalized because of
     value restriction are instantiated to dummy types (X1,X2,...)
  val it = fn : ?.X1 -> int * ?.X1
  ```
* SML#では返り値の関数も多相にできる
  ``` sml
  # fun pair x y = (x , y);
  val pair = fn : ['a. 'a -> ['b. 'b -> 'a * 'b]]
  # pair 1;
  val it = fn : ['a. 'a -> int * 'a]
  ```
* `['a. ty]` が ${}^\forall a. ty$ の気分
* 因みに冠頭多相な型パラメータは`,`で並べる
  ``` sml
  val pair = fn : ['a, 'b. 'a -> 'b -> 'a * 'b]
  ```


===
# 第一級オーバーロード
---------------------

* SMLの演算子はオーバーロードされてるけど中途半端
  + 関数として多相になれなくて、推論できなければデフォルトint
  + `1.0 + 2.0: real` だけど `op+: int * int -> int`
* SML#はオーバーロードされた関数をサポート
  + `op+: ['a::{int, word, int64, word64,...}. 'a * 'a -> 'a]`
* `['a. ty]` に条件が付いた感じの型
* 自分でオーバーロードも書けるけど今回は触れない

===
# 多相レコード
-------------

* SMLでのレコードのフィールドの選択は多相になれない
  + アノテーションを与えないといけない

  ``` sml
  - #id;
  stdIn:1.2-1.5 Error: unresolved flex record
     (can't tell what fields there are besides #id)
  - #id: {id:int, name: string} -> int;
  val it = fn : {id:int, name:string} -> int
  ```
* SML#は多相になれる
  ``` sml
  # #id;
  val it = fn : ['a#{id: 'b}, 'b. 'a -> 'b]
  ```
* 適用可能なレコードの上限がついた感じの型



===
# SQL連携
---------

``` sml
(* DBの宣言。テーブルの型は全て手で書く *)
val server =
    _sqlserver "postgres://hoge:password@localhost/sample1"
    : {people: {name:string, age:int}};
(* `_sql` に続けてSMLでSQLっぽ書ける *)
val q = _sql db =>
         insert into #db.people (name, age)
         values ("Alice", 24);
(* コネクションを作って実行 *)
val conn = SQL.connect server;
val _ = _sqlexec q conn;
```

===

# SQL連携
---------
select 文は型に注目

``` sml
# _sql db =>
>    select #person.name as name,
>           #person.age as age
>    from #db.people as person
>    where  SQL.>=(#person.age, 25);
val it = fn
  : ['a#{people: 'b},
     'b#{age: int, name: 'c},
     'c::{int, intInf, word, char,...},
     'd::{int, intInf, word, char,...}.
       'a SQL.db -> {age: int, name: 'c} SQL.query]
```

===
# C FFI
--------

* `_import` 一行でCの関数を呼べる
* そのためにGCをnon-movingにしたり工夫している

``` sml
# val puts = _import "puts": (string) -> ();
val puts = fn : string -> unit
# puts "hello";
hello
val it = () : unit
```

===

# マルチスレッドサポート
-----------------------

* `_import` で`pthread_create`を呼べる

``` sml
type pthread_t = unit ptr;
val pthread_create =
    _import "pthread_create"
    : (pthread_t ref, unit ptr, unit ptr -> unit ptr, unit ptr) -> int
fun create f =
    let
      val ret = ref _NULL
      val err = pthread_create (ret, _NULL, f, _NULL)
      val ref th = ret
    in
      if err = 0 then () else raise Fail "pthread_create";
      th
    end
```

===

# 最近のSML#
-----------
Changesベース
<!-- .slide: class="center" -->

===

# 3.0.0
-------

* 64bit化
  + 逆に32bitサポートは怪しくなった
* pthreadサポート（あれ？今までは？）
  + デフォルトでサポートONになったらしい
* Fully Concurrent GC
  + [A Fully Concurrent Garbage Collector for Functional Programs on Multicore Processors. Katsuhiro Ueno, Atsushi Ohori. To appear in Proc. ICFP, 2016.](http://www.pllab.riec.tohoku.ac.jp/papers/icfp2016UenoOhori-preprint.pdf)
===

# Fully Concurrent GC
---------------------

* SML#のBit Map GCベース
* アロケーションはスレッド毎にセグメントを割り当てる
* コレクタはSnap Shotっぽいやつ
  1. Filledなセグメントは自動でルートセットに
  2. Currentなセグメントは各スレッドとハンドシェイク
     + コレクタとミューテータは別スレッド
     + スレッドが自分が管理しているルートセットを計算する
     + ライトバリアをonに
  3. 各ミューテータスレッドからルートセットが返ってくる
  4. あとはsnap shotと同じ
  5. ライトバリアをoffにできる
* 関数型言語なのでライトバリアが響かない


===

# 3.1.0
--------

* JSONサポート
  + [A Calculus with Partially Dynamic Records for Typeful Manipulation of JSON Objects. Atsushi Ohori, Katsuhiro Ueno, Tomohiro Sasaki, Daisuke Kikuchi. To appear in Proc. ECOOP Conference, 2016.](http://www.pllab.riec.tohoku.ac.jp/papers/ecoopPreversion.pdf)
===
# JSONサポート
-------------
コードは3.3.0より引用


``` sml
structure JSON =
struct
  exception AttemptToReturnVOIDValue
  exception RuntimeTypeError
  exception TypeIsNotJsonKind
  exception AttemptToViewNull

  exception NaturalJoin

  datatype null = datatype JSONTypes.null
  datatype void = datatype JSONTypes.void
  datatype jsonTy = datatype JSONTypes.jsonTy
  datatype json = datatype JSONTypes.json
  datatype dyn = datatype JSONTypes.dyn

  val view : 'a dyn -> 'a
  val import : string -> void dyn
  val importJson :JSONParser.utJson -> void dyn
  val typeOf : json -> jsonTy
  val printJsonTy : jsonTy -> unit
  val jsonToJsonDyn : json -> void dyn
  val jsonToString : json -> string
  val jsonDynToString : 'a dyn -> string
  val jsonDynToJson : 'a dyn -> json

  val toJson : ['a#dynamic.'a -> json]
  val dynamicToJson : PolyDynamic.dynamic -> json
  val toJsonDyn : ['a#dynamic.'a -> void dyn]
end
```


===
# JSONサポート
-------------

SML3.3に合わせてコードを変えてある

``` sml
val json = JSON.import "{\"name\":\"SML#\", \"version\":\"3.1\"}"
val name = _jsoncase json of
    {name=x:string, ...} => x;
val reco =  _json json as {name: string, version: string}
val versions =  _json JSON.import "{\"name\":\"SML#\", \"version\":\"3.1\"}" as {name: string, version: string}
val data = JSON.jsonToString (JSON.toJson {name = "SML#", version = "3.1"})
```

demo

===
# 3.1.1
-------
* inner join
* 実行されるsqlクエリの改善
  + 差分取りづらいので断念

===
# Inner Join
------------

``` sml
_sql db => select #P.name as name, #P.age as age, #S.salary
      from #db.Persons as P
      inner join #db.Salaries as S
      on #P.id = #S.person_id
      where SQL.>(#S.salary, 10000) ;
```

```
['a#{Persons: 'b list, Salaries: 'c list},
 'b#{age: 'f, id: 'j, name: 'h},
 'c#{person_id: 'j, salary: 'd},
 'd::{int, intInf, 'e option},
 'e::{int, intInf},
 'f::{int, intInf, word, char,...},
 'g::{int, intInf, word, char,...},
 'h::{int, intInf, word, char,...},
 'i::{int, intInf, word, char,...},
 'j::{int, intInf, word, char,...},
 'k::{int, intInf, word, char,...}.
   'a SQL.conn -> {3: 'd, age: 'f, name: 'h} list SQL.cursor]
```


===
# 3.2
------

* DBのnatural joinの型システム
  + [SML# with Natural Join - ICFP 2016](http://conf.researchr.org/event/icfp-2016/mlfamilyworkshop-2016-papers-sml-with-natural-join)
  + [A. Ohori and P. Buneman. Type inference in a database programming language. In Proc. ACM Conference on LISP and Functional Programming, pages 174–183, 1988.](http://www.pllab.riec.tohoku.ac.jp/~ohori/research/lfp88.pdf)の実現っぽい？
* jsonのNULLサポート

===
# Natural Join
--------------

``` sml
_sql db => select #P.name as name, #P.age as age, #S.salary
      from #db.Persons as P
      natural join #db.Salaries as S
      where SQL.>(#S.salary, 10000) ;
```

```
['a#{Persons: 'b list, Salaries: 'c list},
 'b#{age: 'f, name: 'h}#reify,
 'c#{salary: 'd}#reify,
 'd::{int, intInf, 'e option},
 'e::{int, intInf},
 'f::{int, intInf, word, char,...},
 'g::{int, intInf, word, char,...},
 'h::{int, intInf, word, char,...},
 'i::{int, intInf, word, char,...},
 'j#{}#reify. (* この辺がnatural join型 *)
   ('j = 'b join 'c) =>
   'a SQL.conn -> {3: 'd, age: 'f, name: 'h} list SQL.cursor]
```


===
# Nullable in JSON
------------------

``` sml
val json2 = JSON.import "{\"name\":\"SML#\", \"version\":\"3.1\", \"date\": null}"
val reco2 =  _json json2 as {name: string, version: string, date: string option}

```

===

# 3.3.0
------

* massive threadsサポート
  + [Making SML# a General-purpose High-performance Language. Atsushi Ohori, Kenjiro Taura, Katsuhiro Ueno.](http://www.pllab.riec.tohoku.ac.jp/papers/ml2017Preversion.pdf)
  + [massivethreads/massivethreads: Light weight thread library](https://github.com/massivethreads/massivethreads)
  + [細粒度マルチスレッド処理系 MassiveThreads](http://www.yl.is.s.u-tokyo.ac.jp/raw-attachment/wiki/GeneralMeeting/20110627-massive.pdf)
  + 100万個スレッド作れるらしい
  + IOスケジューリングも頑張るらしい
* Concurrent, Myth, CMLモジュールが増えた
  + ようやく処理系提供のスレッドライブラリ
  + CMLはMLtonとかと互換

===

# 3.3.0
------
* SQLの向上
  + 中置演算子がそのままな感じに
  + テーブルの型がlistに
  + sqlexcとsqlevalが廃止されてコネクションを渡されたら実行するように
  + リフレクション導入？（reifyできる型に対して`pp`が導入された）
* type reification
* ML式の自然結合


===
# Massive Threads
------------------

* Myth: ベースバインディング
  - [Thread](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/myth.smi#L7)
  - [Mutex](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/myth.smi#L19)
  - [Cond](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/myth.smi#L30)
  - [Barrier](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/myth.smi#L41)
* Concurrent: 便利ライブラリ
  - [MVar](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/concurrent.smi#L7)
  - [IVar](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/concurrent.smi#L15)
  - [Couple](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/concurrent.smi#L23)
  - [Thread](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/concurrent.smi#L31)
  - [Future](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/concurrent.smi#L37)
* [CML](https://github.com/smlsharp/smlsharp/blob/master/src/thread/main/cml.smi): Concurrent ML互換インタフェース
  - なんかCSPっぽい作りらしい？

===
# Massive Threads
------------------

* なんか[並列処理構文](https://github.com/smlsharp/smlsharp/blob/master/benchmark/benchmarks_massive/suffixsum.sml#L17-L38)も入ったらしい？
* 長い

``` sml
_foreach id in dataExp [where setUpExp]
with pat do iteratorExp while predicateExp
end
```


===
# 中置演算子
-----------

``` sml
_sql db => select #P.name as name, #P.age as age, #S.salary
      from #db.Persons as P inner join #db.Salaries as S on #P.id = #S.person_id
      where #S.salary > 10000 ;
```

===
# SQL式の分割
------------

``` sml
# val sel = _sql select #t.name;
val sel = _
  : ['a#{t: 'b},
     'b#{name: 'c},
     'c::{int, intInf, word, char,...},
     'd::{int, intInf, word, char,...},
     'e.
       ('a list, {1: 'c} list, 'e) SQL.select]
# val frm = fn db => _sql from #db.people as t
> ;
val frm = fn
  : ['a#{people: 'b list}, 'b, 'c.
       ('a, 'c) SQL.db -> ({t: 'b} list, 'c) SQL.from]
# val whr = fn () => _sql where (#t.age = 25 and #t.salary > 300);
val whr = fn
  : ['a#{t: 'b},
     'b#{age: 'd, salary: 'f},
     'c,
     'd::{int, intInf, 'e option},
     'e::{int, intInf},
     'f::{int, intInf, 'g option},
     'g::{int, intInf}.
       unit -> ('a list -> 'a list, 'c) SQL.whr]
# val q = _sql db => select...(sel) from...(frm db) where...(whr ());
val q = fn
  : ['a#{people: 'b list},
     'b#{age: 'e, name: 'c, salary: 'g},
     'c::{int, intInf, word, char,...},
     'd::{int, intInf, word, char,...},
     'e::{int, intInf, 'f option},
     'f::{int, intInf},
     'g::{int, intInf, 'h option},
     'h::{int, intInf}.
       'a SQL.conn -> {1: 'c} list SQL.cursor]

```

===
# Type Reification
------------------

* 値をreifyしてdatatypeとして取り出せる
* いわゆるリフレクション？
* [コード](https://github.com/smlsharp/smlsharp/blob/master/src/compiler-utils/reflection/main/ReifiedTerm.ppg#L59)

``` sml
structure ReifyTerm =
struct
  val toReifiedTerm : ['a#reify.'a -> ReifiedTerm.reifiedTerm]
  val sizeOf : ReifiedTy.reifiedTy -> word
  val setCdr : ['a#reify. 'a list * 'a list -> unit]
end

```


===

# Type Reification
------------------

``` sml
fun ('a#reify) pp (x:'a) = 
    (TextIO.print (ReifiedTerm.reifiedTermToString (ReifyTerm.toReifiedTerm x));
     TextIO.print  "\n")
```

```
# pp;
val it = fn : ['a#reify. 'a -> unit]
# pp (1 + 1);
2
val it = () : unit
# pp {name = "keen", age = 25};
{age = 25, name = "keen"}
val it = () : unit

```

===
# ML式の自然結合
----------------

``` sml
# _join(1, 1);
val it = 1 : int
# _join(1, 2);
uncaught exception NaturalJoin.NaturalJoin at (interactive):2
# _join([1, 2, 3], [2, 3, 4]);
val it = [2, 3] : int list
# val l1 = [{id = 1, age = 10}, {id = 2, age = 20}, {id = 3, age = 30}];
val l1 =
  [{age = 10, id = 1}, {age = 20, id = 2}, {age = 30, id = 3}] : {age: int, id: int} list
# val l2 = [{id = 2, salary = 200}, {id = 3, salary = 300}, {id = 4, salary = 400}];
val l2 =
  [{id = 2, salary = 200}, {id = 3, salary = 300}, {id = 4, salary = 400}] : {id: int, salary: int} list
# _join(l1, l2);
val it =
  [{age = 20, id = 2, salary = 200}, {age = 30, id = 3, salary = 300}] : {age: int, id: int, salary: int} list
```

===
# まとめ
--------

* SML# 3系の変更を追ったよ
* SQL、JSON、Concurrency
* みんなもSML# 使おうね

</textarea>
