---
categories: [WebAssembly, 言語実装, GC, Advent Calendar, Advent Calendar 2017]
date: 2017-12-07T23:19:41+09:00
title: "WebAssemblyでGC"
---

κeenです。この記事は[WebAssembly Advent Calendar 2017](https://qiita.com/advent-calendar/2017/webassembly) 8日目の記事です。WebAssemblyでガーベジコレクションする話。
2017-12-08: ページサイズに関する誤りを訂正しました。その他加筆。
<!--more-->

はじめてWebAssemblyを聞いたとき、「ブラウザでアセンブラが動くのか、よし、コンパイラを作ろう」と思ったかと思います。
私もそのうちの一人で去年頃[こんなコンパイラ](https://github.com/KeenS/webml)を作り始めました。
しかしご覧のように進捗芳しくありません。
進捗が悪い理由の1つにGCがあります。最初はGCをCかRustで書いてemscriptenを通すつもりでしたがemscriptenを通したコードはどうにも扱いづらく、すぐに断念しました。
今でこそRustのwasm32-unknown-unknownのターゲットがありますが当時はemscriptenしか選択肢がありませんでした。

また、wasmには[GCサポートが追加される予定](https://github.com/WebAssembly/design/issues/1079)ですがまだ使えないので絵に書いた餅。

残る手段は1つ。アセンブラ手書きです。アセンブラ手書きでGCを書いていきます。

# リニアメモリ
GCの前にリニアメモリを押さえておきましょう。詳しくは[ドキュメント](http://webassembly.org/docs/semantics/#linear-memory)をみて下さい。ここで覚えておいてほしいのは

* アドレスは *0から始まって* 飛びがない
* データはナチュラルアラインされた方がアクセスは速いがアラインされていなくてもアクセス可能
* メモリは(wasm32では)Max 4GiB。ポインタも32bit
* メモリは`grow_memory`で~~16KiB~~64KiB単位で増やせる [tzikさん指摘ありがとうございます。](https://twitter.com/tzik_tack/status/938811214704009220)
* メモリはモジュール間でimport/exportできる
  + GCモジュールとミューテータ(言語)に分けられる


アドレスが0から始まるので保守的GCが少しやりづらそう？

# 肩慣らし
いきなりどんとGCが来ても困ると思うので軽くwasmのコードを書いてみましょう。
とはいっても生の.watは少々書きづらくマクロを使いたいので生成することを考えます。
ここは餅は餅屋、S式はLispで異論ないと思います。

まずは準備。外部コマンドを使うので`uiop`を使い、シンボルが大文字にならないようにread-caseを`:invert`にしておきます。

```lisp
(require 'uiop)
(setf (readtable-case *readtable*) :invert)
```

さて、生成自体は簡単で、

```lisp
(print '(module
  (func $i (import "imports" "imported_func") (param i32))
  (func $new_page
    (grow_memory (i32.const 3))
    (call $i))
  (func (export "exported_func")
    (i32.const 42)
    (call $new_page)
    (call $i))
  (memory 1 10)
))
```

のように.watのコードを`print`してあげればOKです。もう少し皮を被せてアセンブルまでやるようにすれば

```lisp
(defun write-wasm (filename wasm)
  (with-open-file (f filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (print wasm f)))

(let ((wasm ~~~~))
  (write-wasm "simple.wat" wasm)
  (uiop:run-program "wast2wasm simple.wat -o simple.wasm"))
```

こうなり、ファイルを実行すると`simple.wasm`の生成までやってくれます。

あとはこういうファイルを用意してブラウザで開けば動きます。

``` html
<!doctype html>
<html lang="en">
    <head>
        <meta charset="UTF-8"/>
        <title>Document</title>
    </head>
    <body>
        <script>
         var importObject = {
             imports: {
                 imported_func: function(arg) {
                     console.log(arg);
                 }
             }
         };
         fetch('simple.wasm').then(response =>
             response.arrayBuffer()
         ).then(bytes =>
             WebAssembly.instantiate(bytes, importObject)
         ).then(results => {
             results.instance.exports.exported_func();
         });
        </script>
    </body>
</html>
```

# GC
肩も馴れてもうwasmはいくらでも書けると思うのでGCを実装していきます。

## 戦略

* GCにも色々ありますがブラウザで動かす以上JSとのFFIもあるでしょうしnon-movingなものにしましょう
* grow_memoryでキリの良いアドレスが帰ってくるのでそれを使いましょう
* C言語と違って「このマクロをデータ型定義に埋め込めば何でもGCできます」とはいかないので(単純にマクロがないため)メタデータとデータは分離しましょう
* 地味にスタックとレジスタの走査ができないのでポインタは手でヒープに退避しましょう。これがルートセットになります。

ということでbitmap式のMark and Sweepを採用します。またかよと思った方、ええ、好きなんです。


## イメージ


このGCを使うときのイメージはこんな感じです。


``` html
(defwfun $main (&aux (gc i32) (state i32)) nil
  `(
    ; GCデータを初期化
    (set_local ,gc (call $new-gc))
    (call $gc-init (get_local ,gc))
    ; この時点ではまだデータは0
    (call $print (call $gc--allocated-data (get_local ,gc)))
    ; 今から確保するメモリのポインタをヒープに退避する準備。多くは関数の先頭でやる
    (set_local ,state (call $gc-save-state (get_local ,gc)))
    ; 4byteのメモリを確保し、ポインタを保護
    (call $gc-protect (get_local ,gc) (call $gc-alloc (get_local ,gc) (i32.const 4)))
    (call $gc-protect (get_local ,gc) (call $gc-alloc (get_local ,gc) (i32.const 4)))
    (call $gc-protect (get_local ,gc) (call $gc-alloc (get_local ,gc) (i32.const 4)))
    ; この時点で3つデータを確保している
    (call $print (call $gc--allocated-data (get_local ,gc)))
    ; GCを走らせる
    (call $gc-run (get_local ,gc))
    ; ポインタは退避されているので回収されない
    (call $print (call $gc--allocated-data (get_local ,gc)))
    ; 退避する前の状態に戻す。多くは関数の末尾でやる
    (call $gc-restore-state (get_local ,gc) (get_local ,state))
    ; ポインタを保護してない状態でGCを走らせる
    (call $gc-run (get_local ,gc))
    ; 回収される
    (call $print (call $gc--allocated-data (get_local ,gc)))))
```

さて、イメージも湧いたところで作って行きます。

## データレイアウト

2種類の構造体を使います。それぞれ1ページ、~~16KiB~~64KiB使います。
C言語風に書くとこうです。

``` C
struct gc {
  struct heap_page (*pages)[11], // 4 x 2^0 byte, 4 x 2^1 byte, ..., 4 x 2^10 byte
  size_t arena_top,
  // in the number of pointers
  size_t arena_size,
  void (*arena)[]
};
struct heap_page {
  // bitmaps enough to manage the 4byte-sized data pages
  uint64_t bitmaps[*page-size* / 4 /  64],
  struct page *next,
  // in bytes
  size_t data_size
  // in the number of cell
  size_t heap_size,
  void (*data)[]
};
```

図にするとこうです。

```
                         ポインタの一時退避場所↓
+---+- ... -+---+-----------+------------+- ... -+
| + | pages | + | arena_top | arena_size | arena | gc
+-|-+- ... -+-|-+-----------+------------+- ... -+
0 | 4        |   44                               2^16
  |
  |                       実際のデータ領域↓
+---------+---+-----------+-----------+- .. -+
| bitmaps | + | data_size | heap_size | data | heap_page
+---------+-|-+-----------+-----------+- .. -+
  +---------+
  .
  .
  |
+---------+---+-----------+-----------+- .. -+
| bitmaps | + | data_size | heap_size | data | heap_page
+---------+-|-+-----------+-----------+- .. -+
  +---------+
  |
  x

```

図には1種類しか書かれてませんが`heap_page`は4byteから4096byteまで11種類のサイズ毎にリンクトリストが用意されています。

### アリーナ
アリーナは代替スタック変数領域で、スタックとして使います。関数内でアロケートしたポインタはルートセットに含まれておらず、放っておくと回収されるのでアリーナに置いて保護します。
GC時にはアリーナはルートセットになります。

### heap_pageとbitmaps
宣言どおりデータとメタデータを分離してます。その他にもBitmap GCには色々利点があるので気になる方は調べてみて下さい。

bitmapsとデータ領域は連動していて、bitmapsのn bit目が1ならデータ領域のnセル目が使用中です。アロケートするときはbitmapsを舐めて0なbitに対応するセルを使います。もちろんbitmapsのbitも立てます。
GC時には一旦すべてのbitmapをクリアし、ルートセットから辿れるものにのみマークすればOKです。

### ポインタとマーク
さて、アロケートはマークからデータに飛ぶので問題ないのですが、マーク時はポインタからマークに飛ぶのでやや厄介です。
ここはページが~~16KiB~~64KiBアラインされていることを使います。

ページが~~16KiB~~64KiBアラインされているのでページのアドレスは下位~~14bit~~16bitが0です。なので単にポインタの下位~~14bit~~16bitをクリアしてあげると自分が所属しているページのアドレスが手に入るのです。

### 2017-12-08 追記: 限界
ここでは4096 byteまでのページしか用意していません。それ以上はこのGCではアロケートに失敗というか`unreachable`に入ってエラーになります。ちゃんと実装するならフリーリストなどで管理しましょう。
/ 追記
## 実装

淡々と実装していきます。Common Lispには解説を入れないので雰囲気で追って下さい


まず使いそうな定数とWASMの関数を便利に書くマクロを準備します。


``` common-lisp
; 訂正
; (defparameter *page-size* (* 16  1024))
(defparameter *page-size* (* 64  1024))
(defparameter *null-ptr* '(i32.const 0))
(defparameter *sizeof-i32* 4)
(defparameter *sizeof-i64* 8)
(defparameter *sizeof-ptr* 4)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun split-list (list sep)
    (labels ((recc (left rest)
               (cond
                 ((null rest) (cons (nreverse left) rest))
                 ((eql (car rest) sep)
                  (cons (nreverse left) (cdr rest)))
                 (t (recc
                     (cons (car rest) left)
                     (cdr rest))))))
      (recc () list))))

(defparameter *wasm-funs* nil)

(defmacro defwfun (name param-list result &body body)
  (let* ((param-list (split-list param-list '&aux))
         (params (car param-list))
         (auxs   (cdr param-list))
         (param-types (mapcar #'cadr params))
         (param-vars  (mapcar #'car params))
         (aux-types   (mapcar #'cadr auxs))
         (aux-vars    (mapcar #'car auxs)))
    (let ((bindings (loop
                       :for var :in (append param-vars aux-vars)
                       :for i := 0 then (1+ i)
                       :collect (list var i)))
          (param (if (null param-types) () (list (cons 'param param-types))))
          (local (if (null aux-types) () (list (cons 'local aux-types))))
          (result (if (eql nil result) () (list (list 'result result)))))
      `(push
        (let ,bindings
          (append `(func ,',name ,@',param ,@',result
                         ,@',local
                         )
                  ,@body))
        *wasm-funs*))))

```

このマクロで関数はこのように書けます。

``` common-lisp
(defwfun $add ((x i32) (y i32)) i32
  `((i32.add (get_local ,x) (get_local ,y))))

```

Common Lispの関数定義に倣って引数リストの`&aux`以降はローカル変数とします。

ユーティリティ

``` common-lisp
;;; utils
(defwfun $new-page () i32
  `((i32.mul (i32.const ,*page-size*) (grow_memory (i32.const 1)))))

```

先の構造体に対応する定数を手作業で定義していきます。
11のポインタの初期化などはCommon Lisp側の`loop`で自動生成します。

``` common-lisp
#|
struct gc {
  struct heap_page (*pages)[11], // 4 x 2^0 byte, 4 x 2^1 byte, ..., 4 x 2^10 byte
  size_t arena_top,
  // in the number of pointers
  size_t arena_size,
  void (*arena)[]
};
|#
(defparameter *sizeof-gc* (+ (* 11 *sizeof-ptr*) *sizeof-ptr* *sizeof-ptr*))
(defparameter *offset-gc-heap-page*  0)
(defparameter *offset-gc-arena-top*  (+ *offset-gc-heap-page*  (* 11 *sizeof-ptr*)))
(defparameter *offset-gc-arena-size* (+ *offset-gc-arena-top*  *sizeof-ptr*))
(defparameter *offset-gc-arena*      (+ *offset-gc-arena-size* *sizeof-ptr*))
#|
struct heap_page {
  // bitmaps enough to manage the 4byte-sized data pages
  uint64_t bitmaps[*page-size* / 4 /  64],
  struct page *next,
  // in bytes
  size_t data_size
  // in the number of cell
  size_t heap_size,
  void (*data)[]
};
|#

(defparameter *bitmap-size* (/ *page-size* 4 64))
(defparameter *sizeof-heap-page* (+ (* 8 *bitmap-size*) *sizeof-ptr* *sizeof-ptr*))
(defparameter *offset-heap-page-bitmaps* 0)
(defparameter *offset-heap-page-next*      (+ *offset-heap-page-bitmaps*   (* 8 *bitmap-size*)))
(defparameter *offset-heap-page-data-size* (+ *offset-heap-page-next*      *sizeof-ptr*))
(defparameter *offset-heap-page-heap-size* (+ *offset-heap-page-data-size* *sizeof-ptr*))
(defparameter *offset-heap-page-heap-data* (+ *offset-heap-page-heap-size* *sizeof-ptr*))

;;; struct gc;
(defwfun $new-gc () i32
  '((call $new-page)))

(defwfun $gc-init ((gc i32)) nil
  `(;; pages
    ,@(loop
         :for i :from 0 :below 11 :collect
           `(i32.store ,(intern (format nil "OFFSET=~a" (+ *offset-gc-heap-page* (* i *sizeof-ptr*))))
                       (get_local ,gc)
                       ,*null-ptr*))
    ;; arena_top
    (i32.store ,(intern (format nil "OFFSET=~a" *offset-gc-arena-top*))
               (get_local ,gc)
               (i32.const 0))
    ;; arena_size
    (i32.store ,(intern (format nil "OFFSET=~a" *offset-gc-arena-size*))
               (get_local ,gc)
               (i32.const ,(floor (- *page-size* *sizeof-gc*) *sizeof-ptr*)))))

;;; struct heap_page;
                                        ; heap data must be 8 byte aligned
(assert (zerop (rem *sizeof-heap-page* 8)))

(defwfun $new-heap-page () i32
  '((call $new-page)))

(defwfun $init-heap-page ((page i32) (size i32)) nil
  `(;; bitmaps
    ,@(loop :for i :from 0 :below *bitmap-size* :collect
           `(i64.store ,(intern (format nil "OFFSET=~a" (+ *offset-heap-page-bitmaps* (* i 8))))
                       (get_local ,page)
                       (i64.const 0)))
    ;; next
    (i32.store ,(intern (format nil "OFFSET=~a" *offset-heap-page-next*))
               (get_local ,page)
               ,*null-ptr*)
    ;; data-size
    (i32.store ,(intern (format nil "OFFSET=~a" *offset-heap-page-data-size*))
               (get_local ,page)
               (get_local ,size))
    ;; heap-size
    (i32.store ,(intern (format nil "OFFSET=~a" *offset-heap-page-heap-size*))
               (get_local ,page)
               (i32.div_u (i32.const ,(- *page-size* *sizeof-heap-page*)) (get_local ,size)))))

```

ちょっとトリッキーなのが`load`/`store`のオフセットの指定。 
`(i32.store offset=4 hoge fuga)`のように書くのですがCommon Lispでは`offset=4`一つでシンボル扱いになって動的生成が面倒です。`,(intern (format nil "OFFSET=~a" 4))`のように毎度`intern`で生成するゴリ押しでやってます。
本当は関数に纏めたかったけどアセンブラ感が減るのでやめました。

`(assert (zerop (rem *sizeof-heap-page* 8)))`ですが、`heap_page`から取得したポインタが全て64bitアラインになるように配慮です。


次にサイズから2の累乗に丸めあげる処理です。Cでいうマクロのように毎度インライン展開するようにCommon Lispの関数として定義します。単純な２分探索ですが、本当は4, 8 byteがよく使われるのでそれに合わせてアンバランスドな２分探索をした方がいいらしいです。

``` common-lisp
(defun calc-heap-index (size)
  ;; Simple binary search.
  ;; Should be optimized to common size (frequently 4 bytes and 8bytes)
  `(if i32 (i32.le_u ,size (i32.const 128))
                                        ; size <= 128
       (if i32 (i32.le_u ,size (i32.const 8))
                                        ; size <= 8
           (if i32 (i32.le_u ,size (i32.const 4))
                                        ; size <= 4
               (i32.const 0)
                                        ; 4 < size <= 8
               (i32.const 1))
                                        ; 8 < size <= 128
           (if i32 (i32.le_u ,size (i32.const 32))
                                        ; 8 < size <= 32
               (if i32 (i32.le_u ,size (i32.const 16))
                                        ; 8 < size <= 16
                   (i32.const 2)
                                        ; 16 < size <= 32
                   (i32.const 3))
                                        ; 32 < size <= 128
               (if i32 (i32.le_u ,size (i32.const 64))
                                        ; 32 < size <= 64
                   (i32.const 4)
                                        ; 64 < size <= 128
                   (i32.const 5))))
                                        ; 128 < size
       (if i32 (i32.le_u ,size (i32.const 1024))
                                        ; 128 < size <= 1024
           (if i32 (i32.le_u ,size (i32.const 512))
                                        ; 128 < size <= 512
               (if i32 (i32.le_u ,size (i32.const 256))
                                        ; 128 < size <= 256
                   (i32.const 6)
                                        ; 256 < size <= 512
                   (i32.const 7))
                                        ; 512 < size <= 1024
               (i32.const 8))
                                        ; 1024 < size
           (if i32 (i32.le_u ,size (i32.const 2048))
                                        ; 1024 < size <= 2048
               (i32.const 9)
               (if i32 (i32.le_u ,size (i32.const 4096))
                                        ; 2048 < size <= 4096
                   (i32.const 10)
                                        ; 4096 < size
                   (unreachable))))))
```

ヒーブを増やす内部処理。


``` common-lisp
(defwfun $gc--append-heap-page ((gc i32) (heap-page i32) (size i32)
                                &aux (heap-index i32) (heap-page-ptr i32) (heap-page-next-ptr i32)) nil
  `((set_local ,heap-index (i32.mul (i32.const 4) ,(calc-heap-index `(get_local ,size))))
    (set_local ,heap-page-ptr
               (i32.load ,(intern (format nil "OFFSET=~a" *offset-gc-heap-page*))
                         (i32.add (get_local ,gc) (get_local ,heap-index))))
    (if (i32.eqz (get_local ,heap-page-ptr))
        ;; if no page exist let it be the first page
        (i32.store ,(intern (format nil "OFFSET=~a" *offset-gc-heap-page*))
                   (i32.add (get_local ,gc) (get_local ,heap-index))
                   (get_local ,heap-page))
        ;; if at least 1 page exist, append it to the end
        (loop $loop
           (set_local ,heap-page-next-ptr
                      (i32.load ,(intern (format nil "OFFSET=~a" *offset-heap-page-next*))
                                (get_local ,heap-page-ptr)))
           (if (i32.eqz (get_local ,heap-page-next-ptr))
               (block
                   (i32.store ,(intern (format nil "OFFSET=~a" *offset-heap-page-next*))
                              (get_local ,heap-page-ptr)
                              (get_local ,heap-page))
                 (return))
               (set_local ,heap-page-ptr (get_local,heap-page-next-ptr)))
           (br $loop)))))

(defwfun $gc-extend-heap-page ((gc i32) (size i32) &aux (heap-page i32)) nil
  `((set_local ,heap-page (call $new-heap-page))
    (call $init-heap-page (get_local ,heap-page) (get_local ,size))
    (call $gc--append-heap-page (get_local ,gc) (get_local ,heap-page) (get_local ,size))))

```

アロケーション処理。ごちゃごちゃしてますがコメントだけ拾い読みすると何をやってるかはわかります。

GCからヒープのリストを指定して、ヒーブのリストから空いてるヒーブを探して、ヒーブの中から空いてるセルを探すので関数が3つに分かれてます。
また、大本の`alloc`関数はメモリが足りなければGCを起動したりメモリを増やしたりします。

``` common-lisp
(defwfun $gc--alloc-page ((page i32) (size i32) &aux  (bitmap i64) (i i32) (j i32) (index i32)) i32
  `(
                                        ; for i from 0 below *bitmap-size*
    (block $outer
     (loop $loop
        (if (i32.le_u (i32.const ,*bitmap-size*) (get_local ,i))
            (br $outer))
                                        ;   bitmap <- page.bitmap[i]
        (set_local ,bitmap (i64.load ,(intern (format nil "OFFSET=~a" *offset-heap-page-bitmaps*))
                                     (i32.add (get_local ,page) (get_local ,i))))
                                        ;   if bitmap == 1111...1111
        (if (i64.eq (i64.const -1) (get_local ,bitmap))
                                        ;     next
            (br $loop))
                                        ;   j <- index of the first 0 in bitmap
        (set_local ,j (i32.wrap/i64(i64.ctz (i64.sub (i64.const -1) (get_local ,bitmap)))))
                                        ;   index <- i*64+j
        (set_local ,index (i32.add (i32.mul (get_local ,i) (i32.const 64)) (get_local ,j)))
                                        ;   if page.heap_size <= index
        (if (i32.le_u (i32.load ,(intern (format nil "OFFSET=~a" *offset-heap-page-heap-size*))
                                (get_local ,page))
                      (get_local ,index))
                                        ;     return null
            (return ,*null-ptr*)
                                        ;   else
            (block
                                        ;     page.bitmap[i] <- bitmap | (1 << j)
                (i64.store ,(intern (format nil "OFFSET=~a" *offset-heap-page-bitmaps*))
                           (i32.add (get_local ,page) (get_local ,i))
                           (i64.or (get_local ,bitmap) (i64.shl (i64.const 1) (i64.extend_u/i32 (get_local ,j)))))
                                        ;     return &page.data[index*size]
                (return (i32.add (i32.add (get_local ,page) (i32.const ,*offset-heap-page-heap-data*))
                                 (i32.mul (get_local ,index) (get_local ,size))))))
        (br $loop)))
                                        ; return null
    ,*null-ptr*))

(defwfun $gc--alloc-pages ((pages i32) (size i32) &aux (ptr i32)) i32
  `(
                                        ;  loop
    (loop $loop
                                        ;   if (page.next is null)
       (if (i32.eqz (get_local, pages))
                                        ;     fail
           (return (i32.const 0)))
                                        ;   try alloc-page(page, size)
       (if (i32.eqz (tee_local ,ptr (call $gc--alloc-page (get_local ,pages) (get_local ,size))))
           (set_local ,ptr (i32.load ,(intern (format nil "OFFSET=~a" *offset-heap-page-next*))
                                     (get_local ,pages)))
           (return (get_local ,ptr)))
       (br $loop))
    (unreachable)
    )
  )

(defwfun $gc--alloc ((gc i32) (size i32) &aux (pages i32) (index i32)) i32
  `(
                                        ; index <- calc-index-by-size(size)
    (set_local ,index ,(calc-heap-index `(get_local ,size)))
                                        ; alloc-pages(gc.heaps[index], size)
    (set_local ,pages (i32.load ,(intern (format nil "OFFSET=~a" *offset-gc-heap-page*))
                                (i32.add (get_local ,gc) (get_local ,index))))
    (call $gc--alloc-pages
          (get_local ,pages)
          (get_local ,size))))

(defwfun $gc-alloc ((gc i32) (data-size i32) &aux (size i32) (ptr i32)) i32
  `(
                                        ; try alloc
    (if (i32.eqz (tee_local ,ptr (call $gc--alloc (get_local ,gc) (get_local ,data-size))))
        (block
                                        ; gc-run
            (call $gc-run (get_local ,gc))
                                        ; try alloc
          (if (i32.eqz (tee_local ,ptr (call $gc--alloc (get_local ,gc) (get_local ,data-size))))
              (block
                        ; FIXME: round up power of 2
                  (set_local ,size (get_local ,data-size))
                                        ; extend-page
                  (call $gc-extend-heap-page (get_local ,gc) (get_local ,size))
                                        ; alloc
                (if (i32.eqz (tee_local ,ptr (call $gc--alloc (get_local ,gc) (get_local ,data-size))))
                    (unreachable))))))
    (get_local ,ptr)))
```

ヒープにポインタを退避する関数群。ただのスタックなのでシンプル。

``` common-lisp
(defwfun $gc-save-state ((gc i32)) i32
  `((i32.load ,(intern (format nil "OFFSET=~a" *offset-gc-arena-top*)) (get_local ,gc))))
(defwfun $gc-protect ((gc i32) (ptr i32) &aux (arena-top i32)) nil
  `((set_local ,arena-top (i32.load ,(intern (format nil "OFFSET=~a" *offset-gc-arena-top*)) (get_local ,gc)))
    (i32.store ,(intern (format nil "OFFSET=~a" *offset-gc-arena*))
               (i32.add (get_local ,gc) (get_local ,arena-top))
               (get_local ,ptr))
    (i32.store ,(intern (format nil "OFFSET=~a" *offset-gc-arena-top*))
               (get_local ,gc)
               (i32.add (i32.const 4) (get_local ,arena-top)))))
(defwfun $gc-restore-state ((gc i32) (state i32)) nil
    `((i32.store ,(intern (format nil "OFFSET=~a" *offset-gc-arena-top*))
                 (get_local ,gc) (get_local ,state))))
```

マーク処理。
先程解説してなかったのですが、アロケートしたデータをどうマークするかはユーザに任せます。
動的型付き言語ならデータに埋め込まれたタグで型を判別してマークするでしょう。
静的型付き言語ならタグを入れるか保守的にポインタっぽい値を全部マークする手もあるでしょう。
ここではダミーの何もせずマーク完了を返す関数にしています。

``` common-lisp
(defwfun $gc--clear-marks-page ((page i32)) nil
  ;; clear the bitmaps of the page
  (loop :for i :from 0 :below *bitmap-size* :collect
       `(i64.store ,(intern (format nil "OFFSET=~a" (* i 8)))
                   (get_local ,page)
                   (i64.const 0))))
(defwfun $gc--clear-marks-pages ((current-page i32)) nil
  ;; call for $gc--clear-marks-page all the pages connected to this page
  `((loop $loop
       (if (i32.eqz (get_local ,current-page))
           (return))
       (call $gc--clear-marks-page (get_local ,current-page))
       (set_local ,current-page
                  (i32.load ,(intern (format nil "OFFSET=~a" *offset-heap-page-next*))
                            (get_local ,current-page)))
       (br $loop))))
(defwfun $gc--clear-marks ((gc i32)) nil
  ;; cal $gc--clear-marks-pages for all the size of pages
  (loop
     :for i :from 0 :below 11 :collect
       `(call $gc--clear-marks-pages
              (i32.load ,(intern (format nil "OFFSET=~a" (* i *sizeof-ptr*)))
                        (get_local ,gc)))))
(defwfun $gc--mark-data ((ptr i32)) i32
                                        ; user defined data marking function
  `((i32.const 1)))

(defun get-page (ptr)
  `(i32.and (i32.const ,(lognot(1- (ash 1 (round (log *page-size* 2))))))
            (get_local ,ptr)))

(defwfun $gc--mark-ptr
    ((ptr i32)
     &aux (page i32) (size i32) (bitmap i64) (mask i64)
     (index-in-heap i32) (index-in-array i32) (index-in-i64 i32)) i32
                                        ; mark the ptr and recursively the data
  `(
                                        ; page <- get_page(ptr)
    (set_local ,page ,(get-page ptr))
                                        ; size <- get_size(page
    (set_local ,size (i32.load ,(intern (format nil "OFFSET=~a" *offset-heap-page-data-size*))
                               (get_local ,page)))
                                        ; index <- calc index(page, ptr)
    (set_local ,index-in-heap
               (i32.div_u (i32.sub (get_local ,ptr)
                                   (i32.add (get_local,page)
                                            (i32.const ,*offset-heap-page-heap-data*)))
                          (get_local ,size)))
    (set_local ,index-in-array (i32.div_u (get_local ,index-in-heap) (i32.const 64)))
    (set_local ,index-in-i64   (i32.rem_u (get_local ,index-in-heap) (i32.const 64)))
                                        ; if (! is_marked(page, index))
    (set_local ,bitmap (i64.load ,(intern (format nil "OFFSET=~a" *offset-heap-page-bitmaps*))
                                 (i32.add (get_local ,page)
                                          (i32.mul (get_local ,index-in-array) (i32.const 8)))))
    (set_local ,mask (i64.shl (i64.const 1) (i64.extend_u/i32 (get_local ,index-in-i64))))
    (if (i64.eqz (i64.and (get_local ,bitmap) (get_local ,mask)))
                                        ;   mark(page, index)
        (block
            (i64.store ,(intern (format nil "OFFSET=~a" *offset-heap-page-bitmaps*))
                       (i32.add (get_local ,page)
                                (i32.mul (get_local ,index-in-array) (i32.const 8)))
                       (i64.or (get_local ,bitmap) (get_local ,mask)))
                                        ;   $gc--mark-data(ptr)
          (return (call $gc--mark-data (get_local ,ptr)))))
    (return (i32.const 1))))

(defwfun $gc--mark ((gc i32) &aux (arena-top i32) (i i32)) nil
  `(; for each arena data call $gc--mark-ptr
    (set_local ,arena-top (i32.load ,(intern (format nil "OFFSET=~a" *offset-gc-arena-top*))
                                    (get_local ,gc)))
    (loop $loop
       (if (i32.le_u (get_local ,arena-top) (get_local ,i))
           (return))
       (drop (call $gc--mark-ptr
                   (i32.load ,(intern (format nil "OFFSET=~a" *offset-gc-arena*))
                             (i32.add (get_local ,i) (get_local ,gc)))))
       (set_local ,i (i32.add (get_local ,i (i32.const 4))))
       (br $loop))))
```

GC実行。普通のMark and Sweepとは違ってclear and mark。

``` common-lisp
(defwfun $gc-run ((gc i32)) nil
  `((call $gc--clear-marks (get_local ,gc))
    (call $gc--mark (get_local ,gc))))
```

デバッグ用の何か。確保されているセルの数を数えます。

``` common-lisp
;;; other utils
(defwfun $gc--allocated-data-page ((page i32) &aux (sum i32)) i32
  `(,@(loop :for i :from 0 :below *bitmap-size* :collect
           `(set_local ,sum
                       (i32.add (get_local ,sum)
                                (i32.wrap/i64
                                 (i64.popcnt (i64.load ,(intern (format nil "OFFSET=~a" (* i 8)))
                                                       (get_local ,page)))))))
      (get_local ,sum)))

(defwfun $gc--allocated-data-pages ((current-page i32) &aux (sum i32)) i32
  `((loop $loop
       (if (i32.eqz (get_local ,current-page))
           (return (get_local ,sum)))
       (set_local ,sum (i32.add (get_local ,sum)
                                (call $gc--allocated-data-page (get_local ,current-page))))
       (set_local ,current-page
                  (i32.load ,(intern (format nil "OFFSET=~a" *offset-heap-page-next*))
                            (get_local ,current-page)))
       (br $loop))
    (unreachable)))
(defwfun $gc--allocated-data ((gc i32) &aux (sum i32)) i32
  `(,@(loop
         :for i :from 0 :below 11 :collect
           `(set_local ,sum
                       (i32.add (get_local ,sum)
                                (call $gc--allocated-data-pages
                                      (i32.load ,(intern (format nil "OFFSET=~a" (* i *sizeof-ptr*)))
                                                (get_local ,gc))))))
      (get_local ,sum)))
```

はい。まだFIXMEが残ってるなど甘いところがありますがWASMでBitMap GCを書きました。
多分他人の書いたよくわかんバグってそうなGCを使う人はいないと思うので自作するときの参考にして下さい。

所感としては`get_local`と`const`が面倒、`loop`が`br`を呼ばないと先頭に戻ってくれないことに気づいた、などがあります。

今回のソースコードは[ここ](https://github.com/KeenS/wasm-gc)にあります。

2017-12-08 追記:
今回のGCは2011年の上野先生の論文[^1]  に多大な影響を受けています。
また、そのBitMap GCは2016年に並行版も開発されています[^2]。
興味のある方はぜひご一読下さい。

[^1]: [An efficient non-moving garbage collector for functional languages, K. Ueno, A. Ohori, T. Otomo, In Proc. ACM ICFP Conference, pp:196-208, 2011.](https://dl.acm.org/citation.cfm?id=2034802)
[^2]: [A Fully Concurrent Garbage Collector for Functional Programs on Multicore Processors. Katsuhiro Ueno, Atsushi Ohori. To appear in Proc. ICFP, 2016.](https://dl.acm.org/citation.cfm?id=2951944)
/追記

# 付録 生成された .wat 全文
2017-12-08 編集: ページサイズを16KiB -> 64KiBの変更を取り入れました

``` wat

(module (func $print (import "imports" "print") (param i32))
 (func $new-page (result i32)
  (i32.mul (i32.const 65536) (grow_memory (i32.const 1))))
 (func $new-gc (result i32) (call $new-page))
 (func $gc-init (param i32) (i32.store offset=0 (get_local 0) (i32.const 0))
  (i32.store offset=4 (get_local 0) (i32.const 0))
  (i32.store offset=8 (get_local 0) (i32.const 0))
  (i32.store offset=12 (get_local 0) (i32.const 0))
  (i32.store offset=16 (get_local 0) (i32.const 0))
  (i32.store offset=20 (get_local 0) (i32.const 0))
  (i32.store offset=24 (get_local 0) (i32.const 0))
  (i32.store offset=28 (get_local 0) (i32.const 0))
  (i32.store offset=32 (get_local 0) (i32.const 0))
  (i32.store offset=36 (get_local 0) (i32.const 0))
  (i32.store offset=40 (get_local 0) (i32.const 0))
  (i32.store offset=44 (get_local 0) (i32.const 0))
  (i32.store offset=48 (get_local 0) (i32.const 16371)))
 (func $new-heap-page (result i32) (call $new-page))
 (func $init-heap-page (param i32 i32)
  (i64.store offset=0 (get_local 0) (i64.const 0))
  (i64.store offset=8 (get_local 0) (i64.const 0))
  (i64.store offset=16 (get_local 0) (i64.const 0))
  (i64.store offset=24 (get_local 0) (i64.const 0))
  (i64.store offset=32 (get_local 0) (i64.const 0))
  (i64.store offset=40 (get_local 0) (i64.const 0))
  (i64.store offset=48 (get_local 0) (i64.const 0))
  (i64.store offset=56 (get_local 0) (i64.const 0))
  (i64.store offset=64 (get_local 0) (i64.const 0))
  (i64.store offset=72 (get_local 0) (i64.const 0))
  (i64.store offset=80 (get_local 0) (i64.const 0))
  (i64.store offset=88 (get_local 0) (i64.const 0))
  (i64.store offset=96 (get_local 0) (i64.const 0))
  (i64.store offset=104 (get_local 0) (i64.const 0))
  (i64.store offset=112 (get_local 0) (i64.const 0))
  (i64.store offset=120 (get_local 0) (i64.const 0))
  (i64.store offset=128 (get_local 0) (i64.const 0))
  (i64.store offset=136 (get_local 0) (i64.const 0))
  (i64.store offset=144 (get_local 0) (i64.const 0))
  (i64.store offset=152 (get_local 0) (i64.const 0))
  (i64.store offset=160 (get_local 0) (i64.const 0))
  (i64.store offset=168 (get_local 0) (i64.const 0))
  (i64.store offset=176 (get_local 0) (i64.const 0))
  (i64.store offset=184 (get_local 0) (i64.const 0))
  (i64.store offset=192 (get_local 0) (i64.const 0))
  (i64.store offset=200 (get_local 0) (i64.const 0))
  (i64.store offset=208 (get_local 0) (i64.const 0))
  (i64.store offset=216 (get_local 0) (i64.const 0))
  (i64.store offset=224 (get_local 0) (i64.const 0))
  (i64.store offset=232 (get_local 0) (i64.const 0))
  (i64.store offset=240 (get_local 0) (i64.const 0))
  (i64.store offset=248 (get_local 0) (i64.const 0))
  (i64.store offset=256 (get_local 0) (i64.const 0))
  (i64.store offset=264 (get_local 0) (i64.const 0))
  (i64.store offset=272 (get_local 0) (i64.const 0))
  (i64.store offset=280 (get_local 0) (i64.const 0))
  (i64.store offset=288 (get_local 0) (i64.const 0))
  (i64.store offset=296 (get_local 0) (i64.const 0))
  (i64.store offset=304 (get_local 0) (i64.const 0))
  (i64.store offset=312 (get_local 0) (i64.const 0))
  (i64.store offset=320 (get_local 0) (i64.const 0))
  (i64.store offset=328 (get_local 0) (i64.const 0))
  (i64.store offset=336 (get_local 0) (i64.const 0))
  (i64.store offset=344 (get_local 0) (i64.const 0))
  (i64.store offset=352 (get_local 0) (i64.const 0))
  (i64.store offset=360 (get_local 0) (i64.const 0))
  (i64.store offset=368 (get_local 0) (i64.const 0))
  (i64.store offset=376 (get_local 0) (i64.const 0))
  (i64.store offset=384 (get_local 0) (i64.const 0))
  (i64.store offset=392 (get_local 0) (i64.const 0))
  (i64.store offset=400 (get_local 0) (i64.const 0))
  (i64.store offset=408 (get_local 0) (i64.const 0))
  (i64.store offset=416 (get_local 0) (i64.const 0))
  (i64.store offset=424 (get_local 0) (i64.const 0))
  (i64.store offset=432 (get_local 0) (i64.const 0))
  (i64.store offset=440 (get_local 0) (i64.const 0))
  (i64.store offset=448 (get_local 0) (i64.const 0))
  (i64.store offset=456 (get_local 0) (i64.const 0))
  (i64.store offset=464 (get_local 0) (i64.const 0))
  (i64.store offset=472 (get_local 0) (i64.const 0))
  (i64.store offset=480 (get_local 0) (i64.const 0))
  (i64.store offset=488 (get_local 0) (i64.const 0))
  (i64.store offset=496 (get_local 0) (i64.const 0))
  (i64.store offset=504 (get_local 0) (i64.const 0))
  (i64.store offset=512 (get_local 0) (i64.const 0))
  (i64.store offset=520 (get_local 0) (i64.const 0))
  (i64.store offset=528 (get_local 0) (i64.const 0))
  (i64.store offset=536 (get_local 0) (i64.const 0))
  (i64.store offset=544 (get_local 0) (i64.const 0))
  (i64.store offset=552 (get_local 0) (i64.const 0))
  (i64.store offset=560 (get_local 0) (i64.const 0))
  (i64.store offset=568 (get_local 0) (i64.const 0))
  (i64.store offset=576 (get_local 0) (i64.const 0))
  (i64.store offset=584 (get_local 0) (i64.const 0))
  (i64.store offset=592 (get_local 0) (i64.const 0))
  (i64.store offset=600 (get_local 0) (i64.const 0))
  (i64.store offset=608 (get_local 0) (i64.const 0))
  (i64.store offset=616 (get_local 0) (i64.const 0))
  (i64.store offset=624 (get_local 0) (i64.const 0))
  (i64.store offset=632 (get_local 0) (i64.const 0))
  (i64.store offset=640 (get_local 0) (i64.const 0))
  (i64.store offset=648 (get_local 0) (i64.const 0))
  (i64.store offset=656 (get_local 0) (i64.const 0))
  (i64.store offset=664 (get_local 0) (i64.const 0))
  (i64.store offset=672 (get_local 0) (i64.const 0))
  (i64.store offset=680 (get_local 0) (i64.const 0))
  (i64.store offset=688 (get_local 0) (i64.const 0))
  (i64.store offset=696 (get_local 0) (i64.const 0))
  (i64.store offset=704 (get_local 0) (i64.const 0))
  (i64.store offset=712 (get_local 0) (i64.const 0))
  (i64.store offset=720 (get_local 0) (i64.const 0))
  (i64.store offset=728 (get_local 0) (i64.const 0))
  (i64.store offset=736 (get_local 0) (i64.const 0))
  (i64.store offset=744 (get_local 0) (i64.const 0))
  (i64.store offset=752 (get_local 0) (i64.const 0))
  (i64.store offset=760 (get_local 0) (i64.const 0))
  (i64.store offset=768 (get_local 0) (i64.const 0))
  (i64.store offset=776 (get_local 0) (i64.const 0))
  (i64.store offset=784 (get_local 0) (i64.const 0))
  (i64.store offset=792 (get_local 0) (i64.const 0))
  (i64.store offset=800 (get_local 0) (i64.const 0))
  (i64.store offset=808 (get_local 0) (i64.const 0))
  (i64.store offset=816 (get_local 0) (i64.const 0))
  (i64.store offset=824 (get_local 0) (i64.const 0))
  (i64.store offset=832 (get_local 0) (i64.const 0))
  (i64.store offset=840 (get_local 0) (i64.const 0))
  (i64.store offset=848 (get_local 0) (i64.const 0))
  (i64.store offset=856 (get_local 0) (i64.const 0))
  (i64.store offset=864 (get_local 0) (i64.const 0))
  (i64.store offset=872 (get_local 0) (i64.const 0))
  (i64.store offset=880 (get_local 0) (i64.const 0))
  (i64.store offset=888 (get_local 0) (i64.const 0))
  (i64.store offset=896 (get_local 0) (i64.const 0))
  (i64.store offset=904 (get_local 0) (i64.const 0))
  (i64.store offset=912 (get_local 0) (i64.const 0))
  (i64.store offset=920 (get_local 0) (i64.const 0))
  (i64.store offset=928 (get_local 0) (i64.const 0))
  (i64.store offset=936 (get_local 0) (i64.const 0))
  (i64.store offset=944 (get_local 0) (i64.const 0))
  (i64.store offset=952 (get_local 0) (i64.const 0))
  (i64.store offset=960 (get_local 0) (i64.const 0))
  (i64.store offset=968 (get_local 0) (i64.const 0))
  (i64.store offset=976 (get_local 0) (i64.const 0))
  (i64.store offset=984 (get_local 0) (i64.const 0))
  (i64.store offset=992 (get_local 0) (i64.const 0))
  (i64.store offset=1000 (get_local 0) (i64.const 0))
  (i64.store offset=1008 (get_local 0) (i64.const 0))
  (i64.store offset=1016 (get_local 0) (i64.const 0))
  (i64.store offset=1024 (get_local 0) (i64.const 0))
  (i64.store offset=1032 (get_local 0) (i64.const 0))
  (i64.store offset=1040 (get_local 0) (i64.const 0))
  (i64.store offset=1048 (get_local 0) (i64.const 0))
  (i64.store offset=1056 (get_local 0) (i64.const 0))
  (i64.store offset=1064 (get_local 0) (i64.const 0))
  (i64.store offset=1072 (get_local 0) (i64.const 0))
  (i64.store offset=1080 (get_local 0) (i64.const 0))
  (i64.store offset=1088 (get_local 0) (i64.const 0))
  (i64.store offset=1096 (get_local 0) (i64.const 0))
  (i64.store offset=1104 (get_local 0) (i64.const 0))
  (i64.store offset=1112 (get_local 0) (i64.const 0))
  (i64.store offset=1120 (get_local 0) (i64.const 0))
  (i64.store offset=1128 (get_local 0) (i64.const 0))
  (i64.store offset=1136 (get_local 0) (i64.const 0))
  (i64.store offset=1144 (get_local 0) (i64.const 0))
  (i64.store offset=1152 (get_local 0) (i64.const 0))
  (i64.store offset=1160 (get_local 0) (i64.const 0))
  (i64.store offset=1168 (get_local 0) (i64.const 0))
  (i64.store offset=1176 (get_local 0) (i64.const 0))
  (i64.store offset=1184 (get_local 0) (i64.const 0))
  (i64.store offset=1192 (get_local 0) (i64.const 0))
  (i64.store offset=1200 (get_local 0) (i64.const 0))
  (i64.store offset=1208 (get_local 0) (i64.const 0))
  (i64.store offset=1216 (get_local 0) (i64.const 0))
  (i64.store offset=1224 (get_local 0) (i64.const 0))
  (i64.store offset=1232 (get_local 0) (i64.const 0))
  (i64.store offset=1240 (get_local 0) (i64.const 0))
  (i64.store offset=1248 (get_local 0) (i64.const 0))
  (i64.store offset=1256 (get_local 0) (i64.const 0))
  (i64.store offset=1264 (get_local 0) (i64.const 0))
  (i64.store offset=1272 (get_local 0) (i64.const 0))
  (i64.store offset=1280 (get_local 0) (i64.const 0))
  (i64.store offset=1288 (get_local 0) (i64.const 0))
  (i64.store offset=1296 (get_local 0) (i64.const 0))
  (i64.store offset=1304 (get_local 0) (i64.const 0))
  (i64.store offset=1312 (get_local 0) (i64.const 0))
  (i64.store offset=1320 (get_local 0) (i64.const 0))
  (i64.store offset=1328 (get_local 0) (i64.const 0))
  (i64.store offset=1336 (get_local 0) (i64.const 0))
  (i64.store offset=1344 (get_local 0) (i64.const 0))
  (i64.store offset=1352 (get_local 0) (i64.const 0))
  (i64.store offset=1360 (get_local 0) (i64.const 0))
  (i64.store offset=1368 (get_local 0) (i64.const 0))
  (i64.store offset=1376 (get_local 0) (i64.const 0))
  (i64.store offset=1384 (get_local 0) (i64.const 0))
  (i64.store offset=1392 (get_local 0) (i64.const 0))
  (i64.store offset=1400 (get_local 0) (i64.const 0))
  (i64.store offset=1408 (get_local 0) (i64.const 0))
  (i64.store offset=1416 (get_local 0) (i64.const 0))
  (i64.store offset=1424 (get_local 0) (i64.const 0))
  (i64.store offset=1432 (get_local 0) (i64.const 0))
  (i64.store offset=1440 (get_local 0) (i64.const 0))
  (i64.store offset=1448 (get_local 0) (i64.const 0))
  (i64.store offset=1456 (get_local 0) (i64.const 0))
  (i64.store offset=1464 (get_local 0) (i64.const 0))
  (i64.store offset=1472 (get_local 0) (i64.const 0))
  (i64.store offset=1480 (get_local 0) (i64.const 0))
  (i64.store offset=1488 (get_local 0) (i64.const 0))
  (i64.store offset=1496 (get_local 0) (i64.const 0))
  (i64.store offset=1504 (get_local 0) (i64.const 0))
  (i64.store offset=1512 (get_local 0) (i64.const 0))
  (i64.store offset=1520 (get_local 0) (i64.const 0))
  (i64.store offset=1528 (get_local 0) (i64.const 0))
  (i64.store offset=1536 (get_local 0) (i64.const 0))
  (i64.store offset=1544 (get_local 0) (i64.const 0))
  (i64.store offset=1552 (get_local 0) (i64.const 0))
  (i64.store offset=1560 (get_local 0) (i64.const 0))
  (i64.store offset=1568 (get_local 0) (i64.const 0))
  (i64.store offset=1576 (get_local 0) (i64.const 0))
  (i64.store offset=1584 (get_local 0) (i64.const 0))
  (i64.store offset=1592 (get_local 0) (i64.const 0))
  (i64.store offset=1600 (get_local 0) (i64.const 0))
  (i64.store offset=1608 (get_local 0) (i64.const 0))
  (i64.store offset=1616 (get_local 0) (i64.const 0))
  (i64.store offset=1624 (get_local 0) (i64.const 0))
  (i64.store offset=1632 (get_local 0) (i64.const 0))
  (i64.store offset=1640 (get_local 0) (i64.const 0))
  (i64.store offset=1648 (get_local 0) (i64.const 0))
  (i64.store offset=1656 (get_local 0) (i64.const 0))
  (i64.store offset=1664 (get_local 0) (i64.const 0))
  (i64.store offset=1672 (get_local 0) (i64.const 0))
  (i64.store offset=1680 (get_local 0) (i64.const 0))
  (i64.store offset=1688 (get_local 0) (i64.const 0))
  (i64.store offset=1696 (get_local 0) (i64.const 0))
  (i64.store offset=1704 (get_local 0) (i64.const 0))
  (i64.store offset=1712 (get_local 0) (i64.const 0))
  (i64.store offset=1720 (get_local 0) (i64.const 0))
  (i64.store offset=1728 (get_local 0) (i64.const 0))
  (i64.store offset=1736 (get_local 0) (i64.const 0))
  (i64.store offset=1744 (get_local 0) (i64.const 0))
  (i64.store offset=1752 (get_local 0) (i64.const 0))
  (i64.store offset=1760 (get_local 0) (i64.const 0))
  (i64.store offset=1768 (get_local 0) (i64.const 0))
  (i64.store offset=1776 (get_local 0) (i64.const 0))
  (i64.store offset=1784 (get_local 0) (i64.const 0))
  (i64.store offset=1792 (get_local 0) (i64.const 0))
  (i64.store offset=1800 (get_local 0) (i64.const 0))
  (i64.store offset=1808 (get_local 0) (i64.const 0))
  (i64.store offset=1816 (get_local 0) (i64.const 0))
  (i64.store offset=1824 (get_local 0) (i64.const 0))
  (i64.store offset=1832 (get_local 0) (i64.const 0))
  (i64.store offset=1840 (get_local 0) (i64.const 0))
  (i64.store offset=1848 (get_local 0) (i64.const 0))
  (i64.store offset=1856 (get_local 0) (i64.const 0))
  (i64.store offset=1864 (get_local 0) (i64.const 0))
  (i64.store offset=1872 (get_local 0) (i64.const 0))
  (i64.store offset=1880 (get_local 0) (i64.const 0))
  (i64.store offset=1888 (get_local 0) (i64.const 0))
  (i64.store offset=1896 (get_local 0) (i64.const 0))
  (i64.store offset=1904 (get_local 0) (i64.const 0))
  (i64.store offset=1912 (get_local 0) (i64.const 0))
  (i64.store offset=1920 (get_local 0) (i64.const 0))
  (i64.store offset=1928 (get_local 0) (i64.const 0))
  (i64.store offset=1936 (get_local 0) (i64.const 0))
  (i64.store offset=1944 (get_local 0) (i64.const 0))
  (i64.store offset=1952 (get_local 0) (i64.const 0))
  (i64.store offset=1960 (get_local 0) (i64.const 0))
  (i64.store offset=1968 (get_local 0) (i64.const 0))
  (i64.store offset=1976 (get_local 0) (i64.const 0))
  (i64.store offset=1984 (get_local 0) (i64.const 0))
  (i64.store offset=1992 (get_local 0) (i64.const 0))
  (i64.store offset=2000 (get_local 0) (i64.const 0))
  (i64.store offset=2008 (get_local 0) (i64.const 0))
  (i64.store offset=2016 (get_local 0) (i64.const 0))
  (i64.store offset=2024 (get_local 0) (i64.const 0))
  (i64.store offset=2032 (get_local 0) (i64.const 0))
  (i64.store offset=2040 (get_local 0) (i64.const 0))
  (i32.store offset=2048 (get_local 0) (i32.const 0))
  (i32.store offset=2052 (get_local 0) (get_local 1))
  (i32.store offset=2056 (get_local 0)
   (i32.div_u (i32.const 63480) (get_local 1))))
 (func $gc--append-heap-page (param i32 i32 i32) (local i32 i32 i32)
  (set_local 3
   (i32.mul (i32.const 4)
    (if i32
        (i32.le_u (get_local 2) (i32.const 128))
        (if i32
            (i32.le_u (get_local 2) (i32.const 8))
            (if i32
                (i32.le_u (get_local 2) (i32.const 4))
                (i32.const 0)
                (i32.const 1))
            (if i32
                (i32.le_u (get_local 2) (i32.const 32))
                (if i32
                    (i32.le_u (get_local 2) (i32.const 16))
                    (i32.const 2)
                    (i32.const 3))
                (if i32
                    (i32.le_u (get_local 2) (i32.const 64))
                    (i32.const 4)
                    (i32.const 5))))
        (if i32
            (i32.le_u (get_local 2) (i32.const 1024))
            (if i32
                (i32.le_u (get_local 2) (i32.const 512))
                (if i32
                    (i32.le_u (get_local 2) (i32.const 256))
                    (i32.const 6)
                    (i32.const 7))
                (i32.const 8))
            (if i32
                (i32.le_u (get_local 2) (i32.const 2048))
                (i32.const 9)
                (if i32
                    (i32.le_u (get_local 2) (i32.const 4096))
                    (i32.const 10)
                    (unreachable)))))))
  (set_local 4 (i32.load offset=0 (i32.add (get_local 0) (get_local 3))))
  (if (i32.eqz (get_local 4))
      (i32.store offset=0 (i32.add (get_local 0) (get_local 3)) (get_local 1))
      (loop $loop (set_local 5
                   (i32.load offset=2048 (get_local 4))) (if (i32.eqz
                                                              (get_local 5))
                                                             (block
                                                                 (i32.store
                                                                  offset=2048
                                                                  (get_local 4)
                                                                  (get_local
                                                                   1))
                                                               (return))
                                                             (set_local 4
                                                              (get_local
                                                               5))) (br
                                                                     $loop))))
 (func $gc-extend-heap-page (param i32 i32) (local i32)
  (set_local 2 (call $new-heap-page))
  (call $init-heap-page (get_local 2) (get_local 1))
  (call $gc--append-heap-page (get_local 0) (get_local 2) (get_local 1)))
 (func $gc--alloc-page (param i32 i32) (result i32) (local i64 i32 i32 i32)
  (block $outer
    (loop $loop (if (i32.le_u (i32.const 256) (get_local 3))
                    (br $outer)) (set_local 2
                                  (i64.load offset=0
                                   (i32.add (get_local 0)
                                    (get_local 3)))) (if (i64.eq (i64.const -1)
                                                          (get_local 2))
                                                         (br $loop)) (set_local
                                                                      4
                                                                      (i32.wrap/i64
                                                                       (i64.ctz
                                                                        (i64.sub
                                                                         (i64.const
                                                                          -1)
                                                                         (get_local
                                                                          2))))) (set_local
                                                                                  5
                                                                                  (i32.add
                                                                                   (i32.mul
                                                                                    (get_local
                                                                                     3)
                                                                                    (i32.const
                                                                                     64))
                                                                                   (get_local
                                                                                    4))) (if (i32.le_u
                                                                                              (i32.load
                                                                                               offset=2056
                                                                                               (get_local
                                                                                                0))
                                                                                              (get_local
                                                                                               5))
                                                                                             (return
                                                                                              (i32.const
                                                                                               0))
                                                                                             (block
                                                                                                 (i64.store
                                                                                                  offset=0
                                                                                                  (i32.add
                                                                                                   (get_local
                                                                                                    0)
                                                                                                   (get_local
                                                                                                    3))
                                                                                                  (i64.or
                                                                                                   (get_local
                                                                                                    2)
                                                                                                   (i64.shl
                                                                                                    (i64.const
                                                                                                     1)
                                                                                                    (i64.extend_u/i32
                                                                                                     (get_local
                                                                                                      4)))))
                                                                                               (return
                                                                                                (i32.add
                                                                                                 (i32.add
                                                                                                  (get_local
                                                                                                   0)
                                                                                                  (i32.const
                                                                                                   2060))
                                                                                                 (i32.mul
                                                                                                  (get_local
                                                                                                   5)
                                                                                                  (get_local
                                                                                                   1)))))) (br
                                                                                                            $loop)))
  (i32.const 0))
 (func $gc--alloc-pages (param i32 i32) (result i32) (local i32)
  (loop $loop (if (i32.eqz (get_local 0))
                  (return (i32.const 0))) (if (i32.eqz
                                               (tee_local 2
                                                (call $gc--alloc-page
                                                 (get_local 0) (get_local 1))))
                                              (set_local 2
                                               (i32.load offset=2048
                                                (get_local 0)))
                                              (return (get_local 2))) (br
                                                                       $loop))
  (unreachable))
 (func $gc--alloc (param i32 i32) (result i32) (local i32 i32)
  (set_local 3
   (if i32
       (i32.le_u (get_local 1) (i32.const 128))
       (if i32
           (i32.le_u (get_local 1) (i32.const 8))
           (if i32
               (i32.le_u (get_local 1) (i32.const 4))
               (i32.const 0)
               (i32.const 1))
           (if i32
               (i32.le_u (get_local 1) (i32.const 32))
               (if i32
                   (i32.le_u (get_local 1) (i32.const 16))
                   (i32.const 2)
                   (i32.const 3))
               (if i32
                   (i32.le_u (get_local 1) (i32.const 64))
                   (i32.const 4)
                   (i32.const 5))))
       (if i32
           (i32.le_u (get_local 1) (i32.const 1024))
           (if i32
               (i32.le_u (get_local 1) (i32.const 512))
               (if i32
                   (i32.le_u (get_local 1) (i32.const 256))
                   (i32.const 6)
                   (i32.const 7))
               (i32.const 8))
           (if i32
               (i32.le_u (get_local 1) (i32.const 2048))
               (i32.const 9)
               (if i32
                   (i32.le_u (get_local 1) (i32.const 4096))
                   (i32.const 10)
                   (unreachable))))))
  (set_local 2 (i32.load offset=0 (i32.add (get_local 0) (get_local 3))))
  (call $gc--alloc-pages (get_local 2) (get_local 1)))
 (func $gc-alloc (param i32 i32) (result i32) (local i32 i32)
  (if (i32.eqz (tee_local 3 (call $gc--alloc (get_local 0) (get_local 1))))
      (block (call $gc-run (get_local 0))
        (if (i32.eqz
             (tee_local 3 (call $gc--alloc (get_local 0) (get_local 1))))
            (block (set_local 2 (get_local 1))
              (call $gc-extend-heap-page (get_local 0) (get_local 2))
              (if (i32.eqz
                   (tee_local 3 (call $gc--alloc (get_local 0) (get_local 1))))
                  (unreachable))))))
  (get_local 3))
 (func $gc-save-state (param i32) (result i32)
  (i32.load offset=44 (get_local 0)))
 (func $gc-protect (param i32 i32) (local i32)
  (set_local 2 (i32.load offset=44 (get_local 0)))
  (i32.store offset=52 (i32.add (get_local 0) (get_local 2)) (get_local 1))
  (i32.store offset=44 (get_local 0) (i32.add (i32.const 4) (get_local 2))))
 (func $gc-restore-state (param i32 i32)
  (i32.store offset=44 (get_local 0) (get_local 1)))
 (func $gc--clear-marks-page (param i32)
  (i64.store offset=0 (get_local 0) (i64.const 0))
  (i64.store offset=8 (get_local 0) (i64.const 0))
  (i64.store offset=16 (get_local 0) (i64.const 0))
  (i64.store offset=24 (get_local 0) (i64.const 0))
  (i64.store offset=32 (get_local 0) (i64.const 0))
  (i64.store offset=40 (get_local 0) (i64.const 0))
  (i64.store offset=48 (get_local 0) (i64.const 0))
  (i64.store offset=56 (get_local 0) (i64.const 0))
  (i64.store offset=64 (get_local 0) (i64.const 0))
  (i64.store offset=72 (get_local 0) (i64.const 0))
  (i64.store offset=80 (get_local 0) (i64.const 0))
  (i64.store offset=88 (get_local 0) (i64.const 0))
  (i64.store offset=96 (get_local 0) (i64.const 0))
  (i64.store offset=104 (get_local 0) (i64.const 0))
  (i64.store offset=112 (get_local 0) (i64.const 0))
  (i64.store offset=120 (get_local 0) (i64.const 0))
  (i64.store offset=128 (get_local 0) (i64.const 0))
  (i64.store offset=136 (get_local 0) (i64.const 0))
  (i64.store offset=144 (get_local 0) (i64.const 0))
  (i64.store offset=152 (get_local 0) (i64.const 0))
  (i64.store offset=160 (get_local 0) (i64.const 0))
  (i64.store offset=168 (get_local 0) (i64.const 0))
  (i64.store offset=176 (get_local 0) (i64.const 0))
  (i64.store offset=184 (get_local 0) (i64.const 0))
  (i64.store offset=192 (get_local 0) (i64.const 0))
  (i64.store offset=200 (get_local 0) (i64.const 0))
  (i64.store offset=208 (get_local 0) (i64.const 0))
  (i64.store offset=216 (get_local 0) (i64.const 0))
  (i64.store offset=224 (get_local 0) (i64.const 0))
  (i64.store offset=232 (get_local 0) (i64.const 0))
  (i64.store offset=240 (get_local 0) (i64.const 0))
  (i64.store offset=248 (get_local 0) (i64.const 0))
  (i64.store offset=256 (get_local 0) (i64.const 0))
  (i64.store offset=264 (get_local 0) (i64.const 0))
  (i64.store offset=272 (get_local 0) (i64.const 0))
  (i64.store offset=280 (get_local 0) (i64.const 0))
  (i64.store offset=288 (get_local 0) (i64.const 0))
  (i64.store offset=296 (get_local 0) (i64.const 0))
  (i64.store offset=304 (get_local 0) (i64.const 0))
  (i64.store offset=312 (get_local 0) (i64.const 0))
  (i64.store offset=320 (get_local 0) (i64.const 0))
  (i64.store offset=328 (get_local 0) (i64.const 0))
  (i64.store offset=336 (get_local 0) (i64.const 0))
  (i64.store offset=344 (get_local 0) (i64.const 0))
  (i64.store offset=352 (get_local 0) (i64.const 0))
  (i64.store offset=360 (get_local 0) (i64.const 0))
  (i64.store offset=368 (get_local 0) (i64.const 0))
  (i64.store offset=376 (get_local 0) (i64.const 0))
  (i64.store offset=384 (get_local 0) (i64.const 0))
  (i64.store offset=392 (get_local 0) (i64.const 0))
  (i64.store offset=400 (get_local 0) (i64.const 0))
  (i64.store offset=408 (get_local 0) (i64.const 0))
  (i64.store offset=416 (get_local 0) (i64.const 0))
  (i64.store offset=424 (get_local 0) (i64.const 0))
  (i64.store offset=432 (get_local 0) (i64.const 0))
  (i64.store offset=440 (get_local 0) (i64.const 0))
  (i64.store offset=448 (get_local 0) (i64.const 0))
  (i64.store offset=456 (get_local 0) (i64.const 0))
  (i64.store offset=464 (get_local 0) (i64.const 0))
  (i64.store offset=472 (get_local 0) (i64.const 0))
  (i64.store offset=480 (get_local 0) (i64.const 0))
  (i64.store offset=488 (get_local 0) (i64.const 0))
  (i64.store offset=496 (get_local 0) (i64.const 0))
  (i64.store offset=504 (get_local 0) (i64.const 0))
  (i64.store offset=512 (get_local 0) (i64.const 0))
  (i64.store offset=520 (get_local 0) (i64.const 0))
  (i64.store offset=528 (get_local 0) (i64.const 0))
  (i64.store offset=536 (get_local 0) (i64.const 0))
  (i64.store offset=544 (get_local 0) (i64.const 0))
  (i64.store offset=552 (get_local 0) (i64.const 0))
  (i64.store offset=560 (get_local 0) (i64.const 0))
  (i64.store offset=568 (get_local 0) (i64.const 0))
  (i64.store offset=576 (get_local 0) (i64.const 0))
  (i64.store offset=584 (get_local 0) (i64.const 0))
  (i64.store offset=592 (get_local 0) (i64.const 0))
  (i64.store offset=600 (get_local 0) (i64.const 0))
  (i64.store offset=608 (get_local 0) (i64.const 0))
  (i64.store offset=616 (get_local 0) (i64.const 0))
  (i64.store offset=624 (get_local 0) (i64.const 0))
  (i64.store offset=632 (get_local 0) (i64.const 0))
  (i64.store offset=640 (get_local 0) (i64.const 0))
  (i64.store offset=648 (get_local 0) (i64.const 0))
  (i64.store offset=656 (get_local 0) (i64.const 0))
  (i64.store offset=664 (get_local 0) (i64.const 0))
  (i64.store offset=672 (get_local 0) (i64.const 0))
  (i64.store offset=680 (get_local 0) (i64.const 0))
  (i64.store offset=688 (get_local 0) (i64.const 0))
  (i64.store offset=696 (get_local 0) (i64.const 0))
  (i64.store offset=704 (get_local 0) (i64.const 0))
  (i64.store offset=712 (get_local 0) (i64.const 0))
  (i64.store offset=720 (get_local 0) (i64.const 0))
  (i64.store offset=728 (get_local 0) (i64.const 0))
  (i64.store offset=736 (get_local 0) (i64.const 0))
  (i64.store offset=744 (get_local 0) (i64.const 0))
  (i64.store offset=752 (get_local 0) (i64.const 0))
  (i64.store offset=760 (get_local 0) (i64.const 0))
  (i64.store offset=768 (get_local 0) (i64.const 0))
  (i64.store offset=776 (get_local 0) (i64.const 0))
  (i64.store offset=784 (get_local 0) (i64.const 0))
  (i64.store offset=792 (get_local 0) (i64.const 0))
  (i64.store offset=800 (get_local 0) (i64.const 0))
  (i64.store offset=808 (get_local 0) (i64.const 0))
  (i64.store offset=816 (get_local 0) (i64.const 0))
  (i64.store offset=824 (get_local 0) (i64.const 0))
  (i64.store offset=832 (get_local 0) (i64.const 0))
  (i64.store offset=840 (get_local 0) (i64.const 0))
  (i64.store offset=848 (get_local 0) (i64.const 0))
  (i64.store offset=856 (get_local 0) (i64.const 0))
  (i64.store offset=864 (get_local 0) (i64.const 0))
  (i64.store offset=872 (get_local 0) (i64.const 0))
  (i64.store offset=880 (get_local 0) (i64.const 0))
  (i64.store offset=888 (get_local 0) (i64.const 0))
  (i64.store offset=896 (get_local 0) (i64.const 0))
  (i64.store offset=904 (get_local 0) (i64.const 0))
  (i64.store offset=912 (get_local 0) (i64.const 0))
  (i64.store offset=920 (get_local 0) (i64.const 0))
  (i64.store offset=928 (get_local 0) (i64.const 0))
  (i64.store offset=936 (get_local 0) (i64.const 0))
  (i64.store offset=944 (get_local 0) (i64.const 0))
  (i64.store offset=952 (get_local 0) (i64.const 0))
  (i64.store offset=960 (get_local 0) (i64.const 0))
  (i64.store offset=968 (get_local 0) (i64.const 0))
  (i64.store offset=976 (get_local 0) (i64.const 0))
  (i64.store offset=984 (get_local 0) (i64.const 0))
  (i64.store offset=992 (get_local 0) (i64.const 0))
  (i64.store offset=1000 (get_local 0) (i64.const 0))
  (i64.store offset=1008 (get_local 0) (i64.const 0))
  (i64.store offset=1016 (get_local 0) (i64.const 0))
  (i64.store offset=1024 (get_local 0) (i64.const 0))
  (i64.store offset=1032 (get_local 0) (i64.const 0))
  (i64.store offset=1040 (get_local 0) (i64.const 0))
  (i64.store offset=1048 (get_local 0) (i64.const 0))
  (i64.store offset=1056 (get_local 0) (i64.const 0))
  (i64.store offset=1064 (get_local 0) (i64.const 0))
  (i64.store offset=1072 (get_local 0) (i64.const 0))
  (i64.store offset=1080 (get_local 0) (i64.const 0))
  (i64.store offset=1088 (get_local 0) (i64.const 0))
  (i64.store offset=1096 (get_local 0) (i64.const 0))
  (i64.store offset=1104 (get_local 0) (i64.const 0))
  (i64.store offset=1112 (get_local 0) (i64.const 0))
  (i64.store offset=1120 (get_local 0) (i64.const 0))
  (i64.store offset=1128 (get_local 0) (i64.const 0))
  (i64.store offset=1136 (get_local 0) (i64.const 0))
  (i64.store offset=1144 (get_local 0) (i64.const 0))
  (i64.store offset=1152 (get_local 0) (i64.const 0))
  (i64.store offset=1160 (get_local 0) (i64.const 0))
  (i64.store offset=1168 (get_local 0) (i64.const 0))
  (i64.store offset=1176 (get_local 0) (i64.const 0))
  (i64.store offset=1184 (get_local 0) (i64.const 0))
  (i64.store offset=1192 (get_local 0) (i64.const 0))
  (i64.store offset=1200 (get_local 0) (i64.const 0))
  (i64.store offset=1208 (get_local 0) (i64.const 0))
  (i64.store offset=1216 (get_local 0) (i64.const 0))
  (i64.store offset=1224 (get_local 0) (i64.const 0))
  (i64.store offset=1232 (get_local 0) (i64.const 0))
  (i64.store offset=1240 (get_local 0) (i64.const 0))
  (i64.store offset=1248 (get_local 0) (i64.const 0))
  (i64.store offset=1256 (get_local 0) (i64.const 0))
  (i64.store offset=1264 (get_local 0) (i64.const 0))
  (i64.store offset=1272 (get_local 0) (i64.const 0))
  (i64.store offset=1280 (get_local 0) (i64.const 0))
  (i64.store offset=1288 (get_local 0) (i64.const 0))
  (i64.store offset=1296 (get_local 0) (i64.const 0))
  (i64.store offset=1304 (get_local 0) (i64.const 0))
  (i64.store offset=1312 (get_local 0) (i64.const 0))
  (i64.store offset=1320 (get_local 0) (i64.const 0))
  (i64.store offset=1328 (get_local 0) (i64.const 0))
  (i64.store offset=1336 (get_local 0) (i64.const 0))
  (i64.store offset=1344 (get_local 0) (i64.const 0))
  (i64.store offset=1352 (get_local 0) (i64.const 0))
  (i64.store offset=1360 (get_local 0) (i64.const 0))
  (i64.store offset=1368 (get_local 0) (i64.const 0))
  (i64.store offset=1376 (get_local 0) (i64.const 0))
  (i64.store offset=1384 (get_local 0) (i64.const 0))
  (i64.store offset=1392 (get_local 0) (i64.const 0))
  (i64.store offset=1400 (get_local 0) (i64.const 0))
  (i64.store offset=1408 (get_local 0) (i64.const 0))
  (i64.store offset=1416 (get_local 0) (i64.const 0))
  (i64.store offset=1424 (get_local 0) (i64.const 0))
  (i64.store offset=1432 (get_local 0) (i64.const 0))
  (i64.store offset=1440 (get_local 0) (i64.const 0))
  (i64.store offset=1448 (get_local 0) (i64.const 0))
  (i64.store offset=1456 (get_local 0) (i64.const 0))
  (i64.store offset=1464 (get_local 0) (i64.const 0))
  (i64.store offset=1472 (get_local 0) (i64.const 0))
  (i64.store offset=1480 (get_local 0) (i64.const 0))
  (i64.store offset=1488 (get_local 0) (i64.const 0))
  (i64.store offset=1496 (get_local 0) (i64.const 0))
  (i64.store offset=1504 (get_local 0) (i64.const 0))
  (i64.store offset=1512 (get_local 0) (i64.const 0))
  (i64.store offset=1520 (get_local 0) (i64.const 0))
  (i64.store offset=1528 (get_local 0) (i64.const 0))
  (i64.store offset=1536 (get_local 0) (i64.const 0))
  (i64.store offset=1544 (get_local 0) (i64.const 0))
  (i64.store offset=1552 (get_local 0) (i64.const 0))
  (i64.store offset=1560 (get_local 0) (i64.const 0))
  (i64.store offset=1568 (get_local 0) (i64.const 0))
  (i64.store offset=1576 (get_local 0) (i64.const 0))
  (i64.store offset=1584 (get_local 0) (i64.const 0))
  (i64.store offset=1592 (get_local 0) (i64.const 0))
  (i64.store offset=1600 (get_local 0) (i64.const 0))
  (i64.store offset=1608 (get_local 0) (i64.const 0))
  (i64.store offset=1616 (get_local 0) (i64.const 0))
  (i64.store offset=1624 (get_local 0) (i64.const 0))
  (i64.store offset=1632 (get_local 0) (i64.const 0))
  (i64.store offset=1640 (get_local 0) (i64.const 0))
  (i64.store offset=1648 (get_local 0) (i64.const 0))
  (i64.store offset=1656 (get_local 0) (i64.const 0))
  (i64.store offset=1664 (get_local 0) (i64.const 0))
  (i64.store offset=1672 (get_local 0) (i64.const 0))
  (i64.store offset=1680 (get_local 0) (i64.const 0))
  (i64.store offset=1688 (get_local 0) (i64.const 0))
  (i64.store offset=1696 (get_local 0) (i64.const 0))
  (i64.store offset=1704 (get_local 0) (i64.const 0))
  (i64.store offset=1712 (get_local 0) (i64.const 0))
  (i64.store offset=1720 (get_local 0) (i64.const 0))
  (i64.store offset=1728 (get_local 0) (i64.const 0))
  (i64.store offset=1736 (get_local 0) (i64.const 0))
  (i64.store offset=1744 (get_local 0) (i64.const 0))
  (i64.store offset=1752 (get_local 0) (i64.const 0))
  (i64.store offset=1760 (get_local 0) (i64.const 0))
  (i64.store offset=1768 (get_local 0) (i64.const 0))
  (i64.store offset=1776 (get_local 0) (i64.const 0))
  (i64.store offset=1784 (get_local 0) (i64.const 0))
  (i64.store offset=1792 (get_local 0) (i64.const 0))
  (i64.store offset=1800 (get_local 0) (i64.const 0))
  (i64.store offset=1808 (get_local 0) (i64.const 0))
  (i64.store offset=1816 (get_local 0) (i64.const 0))
  (i64.store offset=1824 (get_local 0) (i64.const 0))
  (i64.store offset=1832 (get_local 0) (i64.const 0))
  (i64.store offset=1840 (get_local 0) (i64.const 0))
  (i64.store offset=1848 (get_local 0) (i64.const 0))
  (i64.store offset=1856 (get_local 0) (i64.const 0))
  (i64.store offset=1864 (get_local 0) (i64.const 0))
  (i64.store offset=1872 (get_local 0) (i64.const 0))
  (i64.store offset=1880 (get_local 0) (i64.const 0))
  (i64.store offset=1888 (get_local 0) (i64.const 0))
  (i64.store offset=1896 (get_local 0) (i64.const 0))
  (i64.store offset=1904 (get_local 0) (i64.const 0))
  (i64.store offset=1912 (get_local 0) (i64.const 0))
  (i64.store offset=1920 (get_local 0) (i64.const 0))
  (i64.store offset=1928 (get_local 0) (i64.const 0))
  (i64.store offset=1936 (get_local 0) (i64.const 0))
  (i64.store offset=1944 (get_local 0) (i64.const 0))
  (i64.store offset=1952 (get_local 0) (i64.const 0))
  (i64.store offset=1960 (get_local 0) (i64.const 0))
  (i64.store offset=1968 (get_local 0) (i64.const 0))
  (i64.store offset=1976 (get_local 0) (i64.const 0))
  (i64.store offset=1984 (get_local 0) (i64.const 0))
  (i64.store offset=1992 (get_local 0) (i64.const 0))
  (i64.store offset=2000 (get_local 0) (i64.const 0))
  (i64.store offset=2008 (get_local 0) (i64.const 0))
  (i64.store offset=2016 (get_local 0) (i64.const 0))
  (i64.store offset=2024 (get_local 0) (i64.const 0))
  (i64.store offset=2032 (get_local 0) (i64.const 0))
  (i64.store offset=2040 (get_local 0) (i64.const 0)))
 (func $gc--clear-marks-pages (param i32)
  (loop $loop (if (i32.eqz (get_local 0))
                  (return)) (call $gc--clear-marks-page
                             (get_local 0)) (set_local 0
                                             (i32.load offset=2048
                                              (get_local 0))) (br $loop)))
 (func $gc--clear-marks (param i32)
  (call $gc--clear-marks-pages (i32.load offset=0 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=4 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=8 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=12 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=16 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=20 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=24 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=28 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=32 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=36 (get_local 0)))
  (call $gc--clear-marks-pages (i32.load offset=40 (get_local 0))))
 (func $gc--mark-data (param i32) (result i32) (i32.const 1))
 (func $gc--mark-ptr (param i32) (result i32)
  (local i32 i32 i64 i64 i32 i32 i32)
  (set_local 1 (i32.and (i32.const -65536) (get_local 0)))
  (set_local 2 (i32.load offset=2052 (get_local 1)))
  (set_local 5
   (i32.div_u (i32.sub (get_local 0) (i32.add (get_local 1) (i32.const 2060)))
    (get_local 2)))
  (set_local 6 (i32.div_u (get_local 5) (i32.const 64)))
  (set_local 7 (i32.rem_u (get_local 5) (i32.const 64)))
  (set_local 3
   (i64.load offset=0
    (i32.add (get_local 1) (i32.mul (get_local 6) (i32.const 8)))))
  (set_local 4 (i64.shl (i64.const 1) (i64.extend_u/i32 (get_local 7))))
  (if (i64.eqz (i64.and (get_local 3) (get_local 4)))
      (block
          (i64.store offset=0
           (i32.add (get_local 1) (i32.mul (get_local 6) (i32.const 8)))
           (i64.or (get_local 3) (get_local 4)))
        (return (call $gc--mark-data (get_local 0)))))
  (return (i32.const 1)))
 (func $gc--mark (param i32) (local i32 i32)
  (set_local 1 (i32.load offset=44 (get_local 0)))
  (loop $loop (if (i32.le_u (get_local 1) (get_local 2))
                  (return)) (drop
                             (call $gc--mark-ptr
                              (i32.load offset=52
                               (i32.add (get_local 2)
                                (get_local 0))))) (set_local 2
                                                   (i32.add
                                                    (get_local 2
                                                     (i32.const 4)))) (br
                                                                       $loop)))
 (func $gc-run (param i32) (call $gc--clear-marks (get_local 0))
  (call $gc--mark (get_local 0)))
 (func $gc--allocated-data-page (param i32) (result i32) (local i32)
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=0 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=8 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=16 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=24 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=32 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=40 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=48 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=56 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=64 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=72 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=80 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=88 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=96 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=104 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=112 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=120 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=128 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=136 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=144 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=152 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=160 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=168 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=176 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=184 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=192 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=200 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=208 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=216 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=224 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=232 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=240 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=248 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=256 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=264 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=272 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=280 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=288 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=296 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=304 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=312 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=320 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=328 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=336 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=344 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=352 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=360 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=368 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=376 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=384 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=392 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=400 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=408 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=416 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=424 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=432 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=440 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=448 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=456 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=464 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=472 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=480 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=488 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=496 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=504 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=512 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=520 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=528 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=536 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=544 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=552 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=560 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=568 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=576 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=584 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=592 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=600 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=608 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=616 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=624 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=632 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=640 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=648 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=656 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=664 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=672 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=680 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=688 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=696 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=704 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=712 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=720 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=728 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=736 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=744 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=752 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=760 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=768 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=776 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=784 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=792 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=800 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=808 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=816 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=824 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=832 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=840 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=848 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=856 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=864 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=872 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=880 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=888 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=896 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=904 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=912 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=920 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=928 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=936 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=944 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=952 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=960 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=968 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=976 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=984 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=992 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1000 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1008 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1016 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1024 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1032 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1040 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1048 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1056 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1064 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1072 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1080 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1088 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1096 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1104 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1112 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1120 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1128 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1136 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1144 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1152 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1160 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1168 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1176 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1184 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1192 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1200 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1208 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1216 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1224 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1232 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1240 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1248 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1256 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1264 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1272 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1280 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1288 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1296 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1304 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1312 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1320 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1328 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1336 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1344 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1352 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1360 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1368 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1376 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1384 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1392 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1400 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1408 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1416 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1424 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1432 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1440 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1448 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1456 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1464 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1472 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1480 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1488 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1496 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1504 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1512 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1520 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1528 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1536 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1544 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1552 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1560 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1568 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1576 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1584 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1592 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1600 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1608 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1616 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1624 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1632 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1640 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1648 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1656 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1664 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1672 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1680 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1688 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1696 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1704 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1712 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1720 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1728 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1736 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1744 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1752 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1760 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1768 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1776 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1784 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1792 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1800 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1808 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1816 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1824 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1832 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1840 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1848 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1856 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1864 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1872 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1880 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1888 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1896 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1904 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1912 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1920 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1928 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1936 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1944 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1952 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1960 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1968 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1976 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1984 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=1992 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=2000 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=2008 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=2016 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=2024 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=2032 (get_local 0))))))
  (set_local 1
   (i32.add (get_local 1)
    (i32.wrap/i64 (i64.popcnt (i64.load offset=2040 (get_local 0))))))
  (get_local 1))
 (func $gc--allocated-data-pages (param i32) (result i32) (local i32)
  (loop $loop (if (i32.eqz (get_local 0))
                  (return (get_local 1))) (set_local 1
                                           (i32.add (get_local 1)
                                            (call $gc--allocated-data-page
                                             (get_local 0)))) (set_local 0
                                                               (i32.load
                                                                offset=2048
                                                                (get_local
                                                                 0))) (br
                                                                       $loop))
  (unreachable))
 (func $gc--allocated-data (param i32) (result i32) (local i32)
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=0 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=4 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=8 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=12 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=16 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=20 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=24 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=28 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=32 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=36 (get_local 0)))))
  (set_local 1
   (i32.add (get_local 1)
    (call $gc--allocated-data-pages (i32.load offset=40 (get_local 0)))))
  (get_local 1))
 (func $main (local i32 i32) (set_local 0 (call $new-gc))
  (call $gc-init (get_local 0))
  (call $print (call $gc--allocated-data (get_local 0)))
  (set_local 1 (call $gc-save-state (get_local 0)))
  (call $gc-protect (get_local 0) (call $gc-alloc (get_local 0) (i32.const 4)))
  (call $gc-protect (get_local 0) (call $gc-alloc (get_local 0) (i32.const 4)))
  (call $gc-protect (get_local 0) (call $gc-alloc (get_local 0) (i32.const 4)))
  (call $print (call $gc--allocated-data (get_local 0)))
  (call $gc-run (get_local 0))
  (call $print (call $gc--allocated-data (get_local 0)))
  (call $gc-restore-state (get_local 0) (get_local 1))
  (call $gc-run (get_local 0))
  (call $print (call $gc--allocated-data (get_local 0))))
 (start $main) (memory 1 10)) 
```

/編集
