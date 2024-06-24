---
categories: [Lisp, Common Lisp, SBCL, Common Lisp処理系拡張]
date: 2015-06-20T18:30:52+09:00
description: "Lisp Meet Up #29での発表資料"
title: Common Lisp処理系拡張の探求 SBCLのマルチスレッドサポート
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
### Common Lisp 処理系拡張の探求
SBCLのマルチスレッドサポート

----------------------
Lisp Meet Up #29

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 渋谷のエンジニア
 + Lisp, ML, Shell Scriptあたりを書きます
===
# CLのマルチスレッド
----------------------------
## [bordeaux-threads](https://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation)
* 色々な処理系のマルチスレッドサポートの抽象レイヤー
* デファクトスタンダード
  + スレッド
  + ロック
  + コンディションヴァリアル

===
# SBCLのマルチスレッド

<!-- .slide: class="center" -->

===
# SBCLのマルチスレッド
----------------------------

* スレッド
  + スレッド内エラー
* アトミック操作<!-- .element: class="fragment highlight-red" data-fragment-index="1" -->
  + CAS<!-- .element: class="fragment highlight-red" data-fragment-index="1" -->
* 排他制御（ロック）
* セマフォ
* コンディションヴァリアル
* バリア
* キュー<!-- .element: class="fragment highlight-red" data-fragment-index="1" -->
* メールボックス<!-- .element: class="fragment highlight-red" data-fragment-index="1" -->
* ゲート<!-- .element: class="fragment highlight-red" data-fragment-index="1" -->
* frlock<!-- .element: class="fragment highlight-red" data-fragment-index="1" -->

===
# アトミック操作

<!-- .slide: class="center" -->
===
# アトミック操作
----------------
複雑な動作は同じデータに並行に動かすと壊れうる

```
        [var = 1]
[incf]      |      [decf]
 [1]<-------|
  |         |------>[1]
 [2]--->[var = 2]    |
            |        | 
        [var = 0]<--[0]
```
===
# アトミック操作
----------------

* `atomic-{incf, decf}`
  + 動作出来る場所が限られている
* `atomic-{pop, push, update}`
  + CASプロトコルを実装していればどこでも

===
# CASプロトコル
---------------
* compare and swap
* アトミック操作の基本中の基本
  + ハードウェアレベルのサポート
* ざっくり言うと並行版setf
* `(cas place old new env)`
  + もし`place`が`old`に等しければ`new`を代入
* `(defun (cas foo) (old new))`
  + `cas`版の`setf`定義
* 他にも`setf`相当の機能は揃ってる
===
# CASプロトコル
---------------

```lisp
(defvar *foo* nil)

(defun (cas foo) (old new)
  (cas (symbol-value '*foo*) old new))
```

===
# キュー
--------

* 普通のキュー
* スレッドセーフ
* `enqueue`, `dequeue`が基本操作
* `dequeue`が多値で、ブロックしない
  + 空なら第二値がnilになる

===
# メールボックス
---------------

* キューとほぼ同じ
* スレッドセーフ
* `send-message`, `recieve-message`が基本操作
* `recieve-message`はブロックする
  + タイムアウトも設定出来る
* `recieve-message-no-hangはdequeue`と同じ挙動
* `recieve-pending-messages`もある

===
# ゲート
--------

* 複数のスレッドが1つのイベントを待つ時に使う
* `wait-on-gate`, `open-gate`, `close-gate`が基本操作

===
```
[gate (closed)]  [T1] [T2] [T3]
     |            |    |    |
     |     wait   |    |    |
     |<----------------+----+
     |            |    .    .
     |     open   |    .    .
[gate (opened)]<--+    .    .
     +---------------->+--->+
         go            |    |
                       V    V
```

===
# frlock
--------

* Fast Read Lock
* またの名をRead-Write Lock
* Read Lockは多重に取れる。Write Lockは1つしか取れない。
* 基本操作は`frlock-read`と`frlock-write`
* 普通のlockと違って複数のReadが速くなる

===
# まとめ
--------

* SBCLのマルチスレッドサポートはbordeaux-threadsよりもリッチ
* 処理系の独自サポート面白い
* 処理系に依存してしまってもいいんじゃないだろうか
</textarea>
