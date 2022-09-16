---
categories: ["言語処理系", "言語実装", "Lisp", "Common Lisp"]
date: 2018-11-15T22:55:10+09:00
description: "言語処理系勉強会での発表用"
title: "Common Lispの多値とその実装達"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Common Lispの多値とその実装達
----------------------
[言語処理系勉強会 Vol.1 - connpass](https://connpass.com/event/104863/)
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
# 今日話すこと
--------------

* 多値のこと
* Common Lispの多値のこと
* Common Lispの多値の実装のこと

===

# 引数と返り値の数
-----------------

<style>
.border-table {
  width: 100%;
  height: 100%;
}
.border-table tr, .border-table td, .border-table th {
  border: solid 1px #000;
}
</style>

<table class="border-table">
<tr><td colspan=2 rowspan=2></td><th colspan=2>引数</th></tr>
<tr>                                    <th>単</th><th>複</th></tr>
<tr><th rowspan=2>返り値</th><th>単</th><td>MLなど</td><td>多数</td></tr>
<tr>                        <th>複</th><td>???</td><td>Lispなど</td></tr>
</table>

===
# MLの引数
-----------------

* 複数引数に見えるがタプルを1つ取っている

``` standard-ml
fun foo(v1, v2) = v1 + v2
```
* 複数引数と違うの？
  + 違う
  + いい悪いではなく違う。

``` standard-ml
val tuple = (1, 2)
foo tuple (* -> 3 *)
```

===
# Lispの多値
------------

* 複数の値を返せる

``` common-lisp
CL-USER> (values 1 2)
1
2
```

* タプルと違うの？
  + 違う
  + いい悪いではなく違う。

``` common-lisp
CL-USER> (identity (values 1 2))
1
```

===
# 多値でないもの
----------------

* rubyの配列の分配束縛

``` ruby
irb(main):001:0> v1, v2 = 1, 2 # [v1, v2] = [1, 2]
=> [1, 2]
irb(main):002:0> v1
=> 1
irb(main):003:0> v2
=> 2
```

===
# 多値でないもの
----------------

* Pythonのタプルの分配束縛

``` python
>>> v1, v2 = 1, 2 # (v1, v2) = (1, 2)
>>> v1
1
>>> v2
2
```

===
# Common Lispの多値
-------------------

* 特徴的な挙動をみていく
* 非常に一貫した挙動になっている

===
# 多値を返す
-----------

* 標準関数の `VALUES` を使う

``` common-lisp
CL-USER> (values 1 2)
1
2
```

===
# 多値を受け取る
---------------

* 専用の構文(スペシャルフォーム)で関数の引数に渡せる

``` common-lisp
(multiple-value-call #'+ (values 1 2))
; -> 3
```

===
# 多値に束縛する
---------------

* マクロでできる

``` common-lisp
(multiple-value-bind (v1 v2) (values 1 2)
  (+ v1 v2))
; -> 3
```

===
# ダイナミックに多値
-------------------

* 動的な言語なので多値の数を変えるのも可能

``` common-lisp
(defun dynamic-mv (x)
  (case x
    ((1) (values 1))
    ((2) (values 1 2))
    ((3) (values 1 2 3))
    (otherwise (values 1 2 3 4))))
```


===
# ダイナミックに多値
-------------------


``` common-lisp
CL-USER> (dynamic-mv 1)
1
CL-USER> (dynamic-mv 2)
1
2
CL-USER> (dynamic-mv 3)
1
2
3
```

===
# 少なく受け取る
----------------

* 無言で無視できる

``` common-lisp
(multiple-value-bind (v1 v2) (values 1 2 3)
  (+ v1 v2))
```

===
# 単値で受け取る
----------------

* もちろんできる
  + 「少なく受け取る」の自然な流れ

``` common-lisp
(let ((v (values 1 2 3)))
  v)
; -> 1
```

===
# 多く受け取る
--------------

* nilが入る

``` common-lisp
(multiple-value-bind (v1 v2 v3) (values 1 2)
  (list v1 v2 v3))
; -> (1 2 NIL)
```

===
# 零値
------

* 値を0個返すこともできる

``` common-lisp
CL-USER> (values)
; No value
```

===
# 零値を受け取る
--------------

* nilが入る
  + 「多く受け取る」の自然な流れ

``` common-lisp
(let ((v (values)))
  v)
; -> NIL
```

===
# 標準ライブラリ
----------------

* ハッシュテーブル
* 2値目にキーが存在したかが入る
  + 1値目は存在しなかったらnil

``` common-lisp
(let ((hash (make-hash-table)))
  (setf (gethash :nil hash) nil)
  (gethash :nil hash))
; -> NIL
; -> T

```

===
# 標準ライブラリ
----------------


* ハッシュテーブルの次があるか、キー、値が多値で返る

``` common-lisp
(let ((hash (make-hash-table)))
  (setf (gethash :nil hash) nil)
  (with-hash-table-iterator (next hash)
    (loop
       ; ここ
       (multiple-value-bind (more key value) (next)
         (unless more (return nil))
         (print (list key value))))))
```

===
# 激しい例
----------

* `sb-unix:unix-getrusage`
* SBCLの処理系拡張のUNIX API
* まずは手元で `man 2 getrusage` してみて
* いくよ…

===
# 激しい例
----------

* 多値が16個返る

``` common-lisp
(sb-unix:unix-getrusage sb-unix:rusage_self)
;; T
;; 650886
;; 69591
;; 547536
;; 0
;; 0
;; 0
;; 35833
;; 0
;; 0
;; 16
;; 480
;; 0
;; 0
;; 0
;; 507
;; 78

```

===
# Common Lisp小話
------------------

* `eval` 関数がある
  + 実行時にインタプリタが呼べる
* `compile` 関数がある
  + 実行時にコンパイラが呼べる
* `disassemble` 関数がある
  + 実行時にディスアセンブラが呼べる
* ダイナミック!!

===
# Common Lisp小話
------------------

* 2行でJITが書ける

``` common-lisp
(eval '(defun inc (x) (1+ x)))
(compile 'inc)
```

===
# Common Lisp小話
------------------

* さらに即座にディスアセンブルできる

``` common-lisp
(disassemble #'inc)
; disassembly for INC
; Size: 35 bytes. Origin: #x100209ED64
; 64:       498B4D60         MOV RCX, [R13+96]                ; no-arg-parsing entry point
                                                              ; thread.binding-stack-pointer
; 68:       48894DF8         MOV [RBP-8], RCX
;;;; GCのタグがあるので1bitシフトされてる
; 6C:       BF02000000       MOV EDI, 2
; 71:       488BD3           MOV RDX, RBX
; 74:       FF1425A800B021   CALL QWORD PTR [#x21B000A8]      ; GENERIC-+
; 7B:       488B5DF0         MOV RBX, [RBP-16]
; 7F:       488BE5           MOV RSP, RBP
; 82:       F8               CLC
; 83:       5D               POP RBP
; 84:       C3               RET
; 85:       CC0F             BREAK 15                         ; Invalid argument count trap

```

===
# 多値の実装
------------

* disassemble関数を使って多値の実装を見てみよう
* 複数の処理系で比べるよ
* Steel Bank Common Lisp (SBCL): ネイティブコンパイル、レジスタマシン
* GNU CLISP (CLISP): バイトコード、スタックマシン

===
# 単値を返す
------------

``` common-lisp
(defun one-value ()
  (values 1))
```


===
# 単値を返す
------------
CLISP

``` text
Disassembly of function ONE-VALUE
(CONST 0) = 1
0 required arguments
0 optional arguments
No rest parameter
No keyword parameters
2 byte-code instructions:
0     (CONST 0)                           ; 1
1     (SKIP&RET 1)
```

===
# 単値を返す
------------
SBCL

``` common-lisp
; disassembly for ONE-VALUE
; Size: 21 bytes. Origin: #x2278662C
; 2C:       498B4D60         MOV RCX, [R13+96]                ; no-arg-parsing entry point
                                                              ; thread.binding-stack-pointer
; 30:       48894DF8         MOV [RBP-8], RCX
; 34:       BA02000000       MOV EDX, 2
; 39:       488BE5           MOV RSP, RBP
; 3C:       F8               CLC
; 3D:       5D               POP RBP
; 3E:       C3               RET
; 3F:       CC0F             BREAK 15                         ; Invalid argument count trap

```

===
# 多値を返す
-----------

``` common-lisp
(defun two-values ()
  (values 1 2))

(disassemble #'two-values)
```

===
# 多値を返す
-----------
CLISP

``` text
(CONST 0) = 1
(CONST 1) = 2
0 required arguments
0 optional arguments
No rest parameter
No keyword parameters
4 byte-code instructions:
0     (CONST&PUSH 0)                      ; 1
1     (CONST&PUSH 1)                      ; 2
2     (STACK-TO-MV 2)
4     (SKIP&RET 1)

```

===
# 多値を返す
-----------
SBCL

``` common-lisp
; disassembly for TWO-VALUE
; Size: 40 bytes. Origin: #x2278637C
; 7C:       498B4D60         MOV RCX, [R13+96]                ; no-arg-parsing entry point
                                                              ; thread.binding-stack-pointer
; 80:       48894DF8         MOV [RBP-8], RCX
;; 1
; 84:       BA02000000       MOV EDX, 2
;; 2
; 89:       BF04000000       MOV EDI, 4
; 8E:       488D5D10         LEA RBX, [RBP+16]
;; 多値の数
; 92:       B904000000       MOV ECX, 4
;; 恐らく多めに受け取ったときのためにNILで埋めてる
; 97:       BE17001020       MOV ESI, #x20100017              ; NIL
;; キャリーフラグで多値であることを伝える
; 9C:       F9               STC
; 9D:       488BE5           MOV RSP, RBP
; A0:       5D               POP RBP
; A1:       C3               RET
; A2:       CC0F             BREAK 15                         ; Invalid argument count trap
```

===
# 多値を返す
-----------
SBCL (3値)

``` common-lisp
; disassembly for THREE-VALUES
; Size: 40 bytes. Origin: #x2278640C
; 0C:       498B4D60         MOV RCX, [R13+96]                ; no-arg-parsing entry point
                                                              ; thread.binding-stack-pointer
; 10:       48894DF8         MOV [RBP-8], RCX
; 14:       BA02000000       MOV EDX, 2
; 19:       BF04000000       MOV EDI, 4
; 1E:       BE06000000       MOV ESI, 6
; 23:       488D5D10         LEA RBX, [RBP+16]
; 27:       B906000000       MOV ECX, 6
; 2C:       F9               STC
; 2D:       488BE5           MOV RSP, RBP
; 30:       5D               POP RBP
; 31:       C3               RET
; 32:       CC0F             BREAK 15                         ; Invalid argument count trap
```


===
# 多値を返す
-----------
SBCL (4値) レジスタが溢れた

``` common-lisp
; disassembly for FOUR-VALUES
; Size: 55 bytes. Origin: #x2278649C
; 9C:       498B4D60         MOV RCX, [R13+96]                ; no-arg-parsing entry point
                                                              ; thread.binding-stack-pointer
; A0:       48894DF8         MOV [RBP-8], RCX
; A4:       BA02000000       MOV EDX, 2
; A9:       BF04000000       MOV EDI, 4
; AE:       BE06000000       MOV ESI, 6
; B3:       48C745F008000000 MOV QWORD PTR [RBP-16], 8
; BB:       488D5D10         LEA RBX, [RBP+16]
; BF:       B908000000       MOV ECX, 8
; C4:       F9               STC
; C5:       488D65F0         LEA RSP, [RBP-16]
; C9:       488B6D00         MOV RBP, [RBP]
; CD:       FF73F8           PUSH QWORD PTR [RBX-8]
; D0:       C3               RET
; D1:       CC0F             BREAK 15                         ; Invalid argument count trap
```

===
# 多値を受け取る
---------------

``` common-lisp
(defun recieve2-two-values ()
  (multiple-value-bind (v1 v2) (two-values)
    (+ v1 v2)))


```

===
# 多値を受け取る
---------------
CLISP

``` text
Disassembly of function RECIEVE2-TWO-VALUES
(CONST 0) = TWO-VALUES
0 required arguments
0 optional arguments
No rest parameter
No keyword parameters
6 byte-code instructions:
0     (CALL0 0)                           ; TWO-VALUES
2     (NV-TO-STACK 2)
4     (LOAD&PUSH 1)
5     (LOAD&PUSH 1)
6     (CALLSR 2 55)                       ; +
9     (SKIP&RET 3)
```

===
# 多値を受け取る
---------------
SBCL


``` common-lisp
; disassembly for RECIEVE2-TWO-VALUES
; Size: 52 bytes. Origin: #x227867FC
; 7FC:       498B4D60         MOV RCX, [R13+96]               ; no-arg-parsing entry point
                                                              ; thread.binding-stack-pointer
; 800:       48894DF8         MOV [RBP-8], RCX
; 804:       4883EC10         SUB RSP, 16
; 808:       31C9             XOR ECX, ECX
; 80A:       48892C24         MOV [RSP], RBP
; 80E:       488BEC           MOV RBP, RSP
; 811:       E86217D8FD       CALL #x20507F78                 ; #<FDEFN TWO-VALUES>
;;  キャリーフラグで分岐
; 816:       7208             JB L0
; 818:       BF17001020       MOV EDI, #x20100017             ; NIL
; 81D:       488BDC           MOV RBX, RSP
; 820: L0:   488BE3           MOV RSP, RBX
; 823:       E818A237FF       CALL #x21B00A40                 ; GENERIC-+
; 828:       488BE5           MOV RSP, RBP
; 82B:       F8               CLC
; 82C:       5D               POP RBP
; 82D:       C3               RET
; 82E:       CC0F             BREAK 15                        ; Invalid argument count trap

```

===
# まとめ
--------

* 多値とタプルは違うよ
* Common Lispの多値はダイナミックだよ
* Common Lispはダイナミックだよ
* スタックマシンの多値は特別扱いされてるよ
* レジスタマシンの多値はハックが詰まってるよ
* [コード](https://gitlab.com/snippets/1779413)

</textarea>
