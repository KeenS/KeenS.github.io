---
categories: ["正規表現", "SATySFi", "多段階計算"]
date: 2019-10-18T02:47:52+09:00
description: "Regex Festa での発表用。SATySFiの多段階計算を使って正規表現をコンパイルする話"
title: "その正規表現エンジン、インタプリタで満足してる？！"
---
<style>
.narrow {
  letter-spacing: -30px;
}

.narrow span {
  transform: scaleX(0.7);
  transform-origin: left;
  display: inline-block;
}
</style>

<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# その正規表現エンジン、<br /><span class="narrow"><span>イ</span><span>ン</span><span>タ</span><span>プ</span><span>リ</span><span>タ</span></span>で<ruby>満足<rp>(</rp><rt>サティスファイ</rt><rp>)</rp></ruby>してる？！
----------------------
[Regex Festa - connpass](https://opt.connpass.com/event/140566/)
<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# 今日話すこと
--------------

* SATySFi
* 部分評価と二村射影
* 多段階計算
* (正規表現のコンパイル)

===
# SATySFi
---------

* 政府の支援によりTeXを倒すために作られた組版処理系
  + ※多少の誇張を含みます
* 日本語が扱えて、PDFを直接吐ける
* ML系の言語で拡張ができる
  + 静的に型が付く
* ↓ 作者

<div style="display:grid;grid-template-columns: 50% 50%;">
<blockquote class="twitter-tweet" style="grid-column-start: 1;grid-column-end: auto;"><p lang="und" dir="ltr">？</p>&mdash; 画力・博士号・油田 (@bd_gfngfn) <a href="https://twitter.com/bd_gfngfn/status/1160573458016690177?ref_src=twsrc%5Etfw">August 11, 2019</a></blockquote>
<blockquote class="twitter-tweet" style="grid-column-start: 2;grid-column-end: auto;"><p lang="und" dir="ltr">！</p>&mdash; 画力・博士号・油田 (@bd_gfngfn) <a href="https://twitter.com/bd_gfngfn/status/1184828674404564992?ref_src=twsrc%5Etfw">October 17, 2019</a></blockquote>
</div>


===
# Hello SATySFi
---------

```text
@require: stdjareport

let-inline \show-int n =
  embed-string (arabic n)
in
document (|
  title = {サンプル文書};
  author = {組 版太郎};
|) '<
 +p {
   答え: \show-int(42);
 }
>
```

===
# Hello SATySFi
---------------

``` console
$ satysfi hello.saty
$ ls
hello.pdf  hello.saty  hello.satysfi-aux
```


===
# <span class="narrow"><span>イ</span><span>ン</span><span>タ</span><span>プ</span><span>リ</span><span>タ</span></span>と<span class="narrow"><span>コ</span><span>ン</span><span>パ</span><span>イ</span><span>ラ</span></span>の関係
------------------------------

* インタプリタとコンパイラって似てない？？
* `1 + 1` がきたときに
  + `1 + 1` を計算する
  + 「`1 + 1` を計算する」コードを生成する
* インタプリタの「計算をする」部分をそのまま吐き出せればコンパイラになりそう

===
# <span class="narrow"><span>イ</span><span>ン</span><span>タ</span><span>プ</span><span>リ</span><span>タ</span></span>と<span class="narrow"><span>コ</span><span>ン</span><span>パ</span><span>イ</span><span>ラ</span></span>の関係
------------------------------

* Q: インタプリタからコンパイラって作れないの？
* A: 実際にできるかは知らないけどそれっぽい原理ならあるよ

===
# (第一)二村射影
----------

* インタプリタはinputとソースコードを受け取る
* もしソースだけ先に渡されて最大限準備できたらどうなる？
  + = インタプリタをソースで部分評価

```text
+---------+ +--------+
| in/data | | source |
+---------+ +--------+
      |       |
   +-------------+
   | interpreter |
   +-------------+
         |
    +----------+
    | out/data |
    +----------+

```

===
# (第一)二村射影
----------

* 実行可能ファイルが作れる筈

```text
+---------+ +--------+         +---------+
| in/data | | source |-----+   | in/data |
+---------+ +--------+     |   +---------+
      |       |            |        |
   +-------------+       +----+  +-----+
   | interpreter |-------| F1 |--| exe |
   +-------------+       +----+  +-----+
         |                          |
    +----------+              +----------+
    | out/data |              | out/data |
    +----------+              +----------+
```

===
# 第二、第三二村射影
----------

* 補足程度に
* 第二: 第一二村射影をソースで部分評価するとコンパイラができる
* 第三: 第二二村射影を第一二村射影で部分評価するとコンパイラジェネレータができる

===
# 展望
---------------

* ソースだけ「先に計算」できたらコンパイルできそう
* つまり何かしらのインタプリタからコンパイラが作れそう
* でも「先に計算」、「後の計算」を考えると計算が多段階になるんじゃない？
* → 多段階計算

===
# 多段階計算
-----------

* プログラムの実行を多段階に行なう
  + ステージ0で「ステージ1で実行されるプログラム」の生成
  + ステージ1でプログラムの実行
* SATySFiがサポート！
* プリミティヴ
  + `~`
    - 気持はeval
    - 次のステージにいく
  * `&`
    - 気持はquasi-quote
    - 前のステージにいく
* `&ty` で 「次のステージで `ty` 型になる式」の意味

===
# 例0
-----

```ml
(* ステージ0 *)
(* 式の前に&をつけると&の型になる *)
&42: &int
```

```ml
(* ステージ1 *)
(* &の式の前に ~ をつけると評価する *)
~(&42): int
```

===
# 例1
-----

* 足し算を部分評価する例
* ステージ0でnを先に渡してしまう

```ml
let add n m = n + m
```

===
# 例1
-----

```ml
(* ステージ0 *)
let add n = &(fun m -> ~n + m)
```

```ml
(* ステージ1 *)
(* `1 +` を計算する関数をステージ0で生成する *)
let inc = ~(add &1)
(*  let inc = fun m -> 1 + m *)
(* ステージ1で生成した関数を使う *)
inc 3
```


===
# 例2
-----

* 指数を計算する関数を考える
* これを肩について部分評価したい
  + = $base^n$ を $base * base * \cdot * base$ に展開したい
* 普通のコード

```ml
let-rec power n base =
  if n <= 0 then 1 else s * (power (n - 1) base)
```

===
# 例2
-----

```ml
(* ステージ0 *)
let power n =
  let-rec aux n base =
    if n <= 0 then &1 else &(~base * ~(aux (n - 1) base))
  in
  &(fun base -> ~(aux n &base))
```

```ml
(* ステージ1 *)
(* b * b * b を計算する関数をステージ0で生成する*)
let cube = ~(power 3)
(* let cube = fn base => base * base * base * 1*)
(* ステージ1で生成した関数を使う *)
cube 9
```

===
# 例3
-----

* ショートサーキット
  + SATySFiにはショートサーキットする論理or/andがない

```ml
(* stage 0 *)
let ( &&& ) b1 b2 = &(if ~b1 then ~b2 else false)
```

```ml
(* stage 1 *)
~(&(1 + 1 = 2) &&& &(2 + 3 = 4))
```
↓
``` ml
~&(if ~(&(1 + 1 = 2)) then ~(&(2 + 3 = 4)) else false)
~&(if (1 + 1 = 2) then (2 + 3 = 4) else false)
if (1 + 1 = 2) then (2 + 3 = 4) else false
```

===
# 多段階計算の疑問
------------------

* マクロじゃないの？
  + 似てるけど違う
  + マクロと違って式から式への変換しかできない
  + アナフォリックマクロを書けない
* evalがあると破滅しない？
  + stage 0で生成したコードを評価するだとか
  + コードじゃない値をevalするだとか
  + うまい具合に制約が入って破天荒なことはできない
* 生成されたコードがコンパイルエラーになったりしないの？
  + 生成されるコードまで含めて型がつくので、ない。

===
# 今日の話
-----------

* SATySFiの多段階を使って正規表現をコンパイルする

===
# 正規表現<span class="narrow"><span>イ</span><span>ン</span><span>タ</span><span>プ</span><span>リ</span><span>タ</span></span>
--------------------

* 数十行で書ける
* 命令の列をリストで持つことにする
  * εと連接がfor freeでついてくる
* ↓ のプリミティヴをサポート

```ml
type vm-inst =
| CHAR of string
| KLEENE of vm-inst list
| OR of vm-inst list * vm-inst list

```

===
# 正規表現<span class="narrow"><span>イ</span><span>ン</span><span>タ</span><span>プ</span><span>リ</span><span>タ</span></span>
--------------------

```ml
let run iseq =
  let-rec loop iseq sp len s =
    (* ここで iseq の処理 *)
  in
  let f s =
      let sp = 0 in
      let len = string-length s in
      loop iseq 0 sp len s
  in f

```

===
# 正規表現<span class="narrow"><span>イ</span><span>ン</span><span>タ</span><span>プ</span><span>リ</span><span>タ</span></span>
--------------------

CHAR

さっきのショートサーキットを使う

```
| CHAR c ->
  if ~(&(sp < len) &&&
       &(string-same c (string-sub s sp 1)))
  then loop iseq (sp + 1) len s
  else None
```


===
# 正規表現<span class="narrow"><span>イ</span><span>ン</span><span>タ</span><span>プ</span><span>リ</span><span>タ</span></span>
--------------------

KLEENE(強欲マッチ)

```
| KLEENE code ->
  let-rec kleene sp =
    match loop code sp len s with
      | Some(sp) -> kleene sp
      | None -> sp
  in
  let sp = kleene sp in
  loop iseq sp len s
```

===
# 正規表現<span class="narrow"><span>イ</span><span>ン</span><span>タ</span><span>プ</span><span>リ</span><span>タ</span></span>
--------------------

```
| OR (code1, code2) ->
  (match loop code1 sp len s with
    | Some(v) -> Some(v)
    | None -> loop code2 sp len s)
```

===
# 正規表現<span class="narrow"><span>イ</span><span>ン</span><span>タ</span><span>プ</span><span>リ</span><span>タ</span></span>
--------------------

```ml
let matcher = (
  let iseq = [KLEENE([CHAR(`a`)]); CHAR(`b`); CHAR(`c`)] in
  run iseq)
in
let input = `aaaaaaaaaaaaabc` in
let result = matcher input in
match result with
  | Some(at) -> `matched to `# ^ (string-sub input 0 at)
  | None -> `not matched`
```

===
# 静的動的
----------

* これらを多段階計算を使ってコンパイルしたい。
* 基本は全部 `&` で、 静的なものだけを `~` するとよい
  + ここでは静的なものはiseqのみ
* ほとんどインタプリタと変わらない

===
# 正規表現の<span>コ</span><span>ン</span><span>パ</span><span>イ</span><span>ル</span></span>
-------------------

```ml
let compile iseq =
  let-rec loop iseq sp len s =
    (* ここでiseqの処理 *)
  in
  &(let f s =
      let sp = 0 in
      let len = string-length s in
      ~(loop iseq 0 &sp &len &s)
    in f)

```

===
# 正規表現の<span>コ</span><span>ン</span><span>パ</span><span>イ</span><span>ル</span></span>
-------------------
KLEENE

```ml
| KLEENE code ->
  &(let sp = ~(sp) in
    let-rec kleene sp =
      match ~(loop code &sp len s) with
        | Some(sp) -> kleene sp
        | None -> sp
    in
    let sp = kleene sp in
    ~(loop iseq &sp len s)
    )
```

===
# 正規表現の<span>コ</span><span>ン</span><span>パ</span><span>イ</span><span>ル</span></span>
-------------------

OR

```ml
| OR (code1, code2) ->
  &(let sp = ~(sp) in
    match ~(loop code1 &sp len s) with
      | Some(v) -> Some(v)
      | None -> ~(loop code2 &sp len s)
   )

```

===
# 正規表現の<span>コ</span><span>ン</span><span>パ</span><span>イ</span><span>ル</span></span>
-------------------
CHAR

```ml
| CHAR c ->
  &(let sp = ~(sp) in
    if ~(&(sp < ~len) &&&
                     (* ↓これ      *)
         &(string-same ~c (string-sub ~s sp 1)))
    then ~(loop iseq &(sp + 1) len s)
    else None
   )
```


===
# cross stage persistence
-------------------

* ここで問題が生じる
* stage 0の値 (`c`) をstage 1に持ち越せない
  + できるようにする機能をcross stage persistenceというらしい

```console
$ satysfi main.saty
! [Type Error] at "re.satyh", line 39, characters 50-51:
    invalid occurrence of variable 'c' as to stage;
    should be used at stage 0.
```


===
# cross stage persistence
-------------------------

* 開発用ブランチにCSPがあるらしい

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">一応 dev-macro-on-multi-stage というbranchで開発中のものには lift-int や lift-string などのリフト用のプリミティヴをいくつか入れています</p>&mdash; 画力・博士号・油田 (@bd_gfngfn) <a href="https://twitter.com/bd_gfngfn/status/1183787593755852810?ref_src=twsrc%5Etfw">October 14, 2019</a></blockquote>

===
# 正規表現の<span>コ</span><span>ン</span><span>パ</span><span>イ</span><span>ル</span></span>
-------------------
CHAR

```ml
| CHAR c ->
  &(let sp = ~(sp) in
    if ~(&(sp < ~len) &&&
                      (* ↓これ      *)
         &(string-same ~(lift-string c) (string-sub ~s sp 1)))
    then ~(loop iseq &(sp + 1) len s)
    else None
   )
```

===
# 正規表現の<span>コ</span><span>ン</span><span>パ</span><span>イ</span><span>ル</span></span>
--------------------

```ml
let matcher = ~(
  let iseq = [KLEENE([CHAR(`a`)]); CHAR(`b`); CHAR(`c`)] in
  compile iseq)
in
let input = `aaaaaaaaaaaaabc` in
let result = matcher input in
match result with
  | Some(at) -> `matched to `# ^ (string-sub input 0 at)
  | None -> `not matched`
```

===
# 比較
-------

* （パーサを適切に実装したら）インタプリタと違って静的に(stage 0で)エラーを検出できる
* インタプリタと違って、正規表現は静的に(stage 0で)与えられないといけない
  + stage 1の実行時にファイルから読んだりはできない
* インタプリタより少し速い
  + とあるワークロードで 4.70s vs 4.83s
  + サボって強欲マッチにしたのでpathological caseを作れなかった

===
# まとめ
--------

* 多段階計算があるとインタプリタを簡単にコンパイラに変換できる
* SATySFiを使ってそれをデモンストレーションした
* [コード](https://github.com/KeenS/satysfi-rejit)


</script>
</section>
