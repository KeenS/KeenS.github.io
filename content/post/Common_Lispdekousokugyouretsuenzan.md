---
categories: [Lisp, Common Lisp]
date: 2016-02-04T23:47:34+09:00
title: Common Lispで高速行列演算
---

κeenです。Common Lispから線形代数ライブラリを使うポストで比較にCommon Lispのコードが出されていたのですがもう少し改良出来そうだったので少しばかり高速化してみました。

<!--more-->

そのポストはこちら。

[Common Lispで行列演算: LLA(Lisp Linear Algebra)を使う - 翡翠はコンピュータに卵を生むか](http://d.hatena.ne.jp/masatoi/20160204/1454519281)


圧倒的にCommon Lispが遅いですね。そのコードはこちらから。

[Python (NumPy) と Common Lisp (LLA) で行列積の実行速度を比較する - 不確定特異点](http://tanakahx.hatenablog.com/entry/2015/09/25/070000)

行列計算(GEMM)部分を抜き出すとこうなっています。


``` lisp
(defun simple-gemm (ma mb)
  "Common Lispのみを使った行列積の計算"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array single-float (* *)) ma mb))
  (let ((rows (array-dimension ma 0))
        (cols (array-dimension mb 1)))
    (declare (type fixnum rows cols))
    (let ((result (make-matrix rows cols)))
      (declare (type (simple-array single-float (* *)) result))
      (dotimes (row rows)
        (dotimes (col cols)
          (dotimes (k cols)
            (incf (aref result row col)
                  (* (aref ma row k) (aref mb k col))))))
      result)))
```


これを`*N*` = 256, で100回繰り返したら

```
;; Evaluation took:
;;   4.688 seconds of real time
;;   4.692000 seconds of total run time (4.688000 user, 0.004000 system)
;;   100.09% CPU
;;   13,564,728,093 processor cycles
;;   26,216,000 bytes consed

```

とのこと。まあ、まだ高速化の余地はありそうです。


# キャッシュする

内側のループをみるとこうなっています。

```lisp
          (dotimes (k cols)
            (incf (aref result row col)
                  (* (aref ma row k) (aref mb k col))))
```

`incf`をばらすとこうなります。

``` lisp
          (dotimes (k cols)
            (setf (aref result row col)
                  (+ (aref result row col) (* (aref ma row k) (aref mb k col)))))
```

値として参照している方の`(aref result row col)`は`k`に依存しないのでループの外に出せます。
書き込んでいる方は場所だから仕方ない。

これをキャッシュするようにすると

``` lisp
(defun caching-gemm (ma mb)
  "Common Lispのみを使った行列積の計算"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array single-float (* *)) ma mb))
  (let ((rows (array-dimension ma 0))
        (cols (array-dimension mb 1)))
    (declare (type fixnum rows cols))
    (let ((result (make-matrix rows cols)))
      (declare (type (simple-array single-float (* *)) result))

      (dotimes (row rows)
        (dotimes (col cols)
          (let ((cell (aref result row col)))
           (dotimes (k cols)
             (setf (aref result row col)
              (+ cell (* (aref ma row k) (aref mb k col)))))
           )))
      result)))
```

同じパラメータでベンチマークをとると

```
;; 3.971 seconds of real time
;; 3.972000 seconds of total run time (3.972000 user, 0.000000 system)
;; 100.03% CPU
;; 11,491,319,119 processor cycles
;; 26,216,000 bytes consed
```

2016-02-28 追記:

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> キャッシュのところ、resultのインデックスにkは使ってないけど、kが進むごとに対象の要素は変化してるはずなので、このキャッシュのしかたはまずいのでは?</p>&mdash; goskevich (@gos_k) <a href="https://twitter.com/gos_k/status/700622885933613056">2016年2月19日</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

ということでこれは間違ってました。

# ループアンローリング

さて、先程の内側のループ、仕事が少ないですね。小さな仕事をチマチマループしてるとイテレーションコストが嵩みます。
1回のイテレーションでの仕事を増やすべく、ループアンローリングをしましょう。

コピペはダルいのでまずはマクロを。

``` lisp
(defmacro dotimes-unroll ((i n unroll) &body body)
  (let ((n_      (gensym "n")))
    `(let ((,n_ ,n))
       (do ((,i 0))
           ((< ,n_ (the fixnum (+ ,unroll ,i)))
            (do ((,i ,i (the fixnum (1+ ,i))))
                ((< ,n_ (the fixnum (1+ ,i))))
              ,@body
              ))
         ,@(loop :repeat unroll :append (append body `((setq ,i (the fixnum (1+ ,i))))))))))
```

少し試してみましょう。

``` lisp
(dotimes-unroll (i 10 3)
  (format t "~%~a") i)
0
1
2
3
4
5
6
7
8
9
```


よしよし。

それではこれを使ってアンロールします。

``` lisp
(defun loop-unroll-gemm (ma mb)
  "Common Lispのみを使った行列積の計算"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array single-float (* *)) ma mb))
  (let ((rows (array-dimension ma 0))
        (cols (array-dimension mb 1)))
    (declare (type fixnum rows cols))
    (let ((result (make-matrix rows cols)))
      (declare (type (simple-array single-float (* *)) result))

      (dotimes (row rows)
        (dotimes (col cols)
          (let ((cell (aref result row col)))
           (dotimes-unroll (k cols 16)
             (setf (aref result row col)
              (+ cell (* (aref ma row k) (aref mb k col))))))))
      result)))
```

アンロール数は16が一番パフォーマンス出たようでした。

さて、ベンチマークをしてみます。

```
;; Evaluation took:
;;   3.847 seconds of real time
;;   3.848000 seconds of total run time (3.848000 user, 0.000000 system)
;;   [ Run times consist of 0.012 seconds GC time, and 3.836 seconds non-GC time. ]
;;   100.03% CPU
;;   11,128,993,432 processor cycles
;;   26,216,000 bytes consed
```

辛うじて速くなってる…。因みにこれのパフォーマンスはCPUの命令キャッシュの状況に依存するので何度か試すとこれより速いスコアが出ることもあります。


```
Evaluation took:
  3.338 seconds of real time
  3.340000 seconds of total run time (3.332000 user, 0.008000 system)
  [ Run times consist of 0.020 seconds GC time, and 3.320 seconds non-GC time. ]
  100.06% CPU
  9,657,259,219 processor cycles
  26,248,768 bytes consed
```


# メモリアクセスしない

さて、最初にキャッシュした時に書き込まれている方の `(aref result row col)` は場所だからキャッシュ出来ないといいました。まあ、それは正しいのですがループの中で毎回書き込む必要はありません。

レジスタ上で計算を済ませて最後に書き込んであげれば十分です。

```lisp
(defun on-register-gemm (ma mb)
  "Common Lispのみを使った行列積の計算"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array single-float (* *)) ma mb))
  (let ((rows (array-dimension ma 0))
        (cols (array-dimension mb 1)))
    (declare (type fixnum rows cols))
    (let ((result (make-matrix rows cols)))
      (declare (type (simple-array single-float (* *)) result))

      (dotimes (row rows)
        (dotimes (col cols)
          (let ((res (aref result row col)))
           (dotimes-unroll (k cols 16)
             (setf res
                   (the single-float (+ res (* (aref ma row k) (aref mb k col))))))
           (setf (aref result row col) res))))
      result)))
```

これを試してみます。


```
;; Evaluation took:
;;   2.302 seconds of real time
;;   2.304000 seconds of total run time (2.300000 user, 0.004000 system)
;;   [ Run times consist of 0.008 seconds GC time, and 2.296 seconds non-GC time. ]
;;   100.09% CPU
;;   6,662,273,812 processor cycles
;;   26,216,000 bytes consed
```


わお！急に速くなりました。 `(* 100 (- (/ 4.688 2.302) 1))` ≒ 103、 100%近い高速化です。


# おわりに

特に深い意味はなかったのですがパタヘネに載っていたやつを試してみたくて遊んでみました。

普通に行列計算したいなら線形代数ライブラリ使った方が良いと思います。

# ノート

* ループアンローリングの部分をイテレーションコストで説明しましたが他にも1ループ内の命令数が増えると組み合わせパズルのピースが増えるので最適化されやすくなります。まあ、SBCLは覗き穴最適化をほとんどしないのであまり意味ありませんが。気になる方はディスアセンブルしてみて下さい。
* このあと、 `(aref mb k col)` がCPUキャッシュを無駄にしているという話からキャッシュサイズ毎に行、列を分けて計算するやり方を紹介しようとしたのですが逆に遅くなってしまいました。付録Aにコードを置いておきます。うーむ。
* 本当はさらにこのセグメント毎にスレッドに計算を投げて並列化したかったのですがセグメントで高速化しなかったので萎えて諦めました。
* パタヘネ的にはSIMDも使うのですが深夜に近付いてきて面倒になったのでこの辺でやめました。Common Lisp(SBCL)からSIMD命令を使うには[私の過去のエントリ](http://keens.github.io/blog/2014/12/02/vopdeyou-bu/)を参照して下さい。
* 本気で行列計算をしたいなら今回の $O(n^3)$ のアルゴリズムだけでなくStrassenのアルゴリズムやCoppersmith-Winogradのアルゴリズムも検討すべきです。

# 付録A

``` lisp
(defun segmented-gemm (ma mb)
  "Common Lispのみを使った行列積の計算"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array single-float (* *)) ma mb))
  (let* ((segment 16)
         (rows (array-dimension ma 0))
         (cols (array-dimension mb 1))
         (colseg (floor cols segment))
         ktmp coltmp
         k col
         )
    (declare (type fixnum rows cols))
    (let ((result (make-matrix rows cols)))
      (declare (type (simple-array single-float (* *)) result))

      (dotimes (row rows)
        (dotimes (ci colseg)
          (setf coltmp (the fixnum (* ci segment)))
          (dotimes (ki colseg)
            (setf ktmp (the fixnum (* ki segment)))
           (dotimes (cs segment)
             (setf col (the fixnum (+ coltmp cs)))
             (let ((res (aref result row col)))
               (dotimes (ks segment)
                 (setf k (the fixnum (+ ktmp ks)))
                 (setf res
                       (the single-float (+ res (* (aref ma row k) (aref mb k col))))))
               (setf (aref result row col) res))))))
      result)))
```

