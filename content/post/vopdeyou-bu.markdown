---
type: post
title: "VOPで遊ぶ"
date: 2014-12-02
comments: true
categories: [Lisp, Common Lisp, SBCL, VOP, Advent Calendar, Advent Calendar 2014, Lisp Advent Calendar]
---
(:meta  
 ((:this "[Lisp Advent Calendar 2014](http://qiita.com/advent-calendar/2014/lisp)の3日目の記事")  
  (:prev (:author "tk_riple" :title ["時の羅針盤＠blog: R7RSポータブルライブラリを書く際の落とし穴"](http://compassoftime.blogspot.jp/2014/12/r7rs.html)))  
  (:next (:author "nobkz" :tite "[Shenの基礎その1 基本的な型 - Qiita](http://qiita.com/nobkz/items/68ee2adbc13caf3eec6f)"))))


κeenです。さっきまでVOPで遊んでたので当初の予定を変更してVOPの話をします。
<!--more-->
# VOPとは何か
SBCLやCMU CLで使われているネイティブコードを吐くための機構、要はインラインアセンブラです。

# VOPとはどんなものか
とりあえずコードをば。x86-64用です。


```lisp
(in-package :cl-user)
(defpackage vop-sample
  (:use :cl :sb-ext :sb-c))
(in-package :vop-sample)

(defknown add (fixnum fixnum)          ; addのftypeを宣言
    fixnum
    (movable                            ; 副作用がない
     flushable                          ; デッドコードとして除去してよい
     foldable                           ; 定数畳み込みをしてよい
     always-translatable)               ; 必ずアセンブラコードになる
  :overwrite-fndb-silently t)           ; 関数上書きのエラーを出さない


(in-package "SB-VM")
(define-vop (vop-sample::add)          ; VOP名
  (:translate vop-sample::add)         ; 関数名
  (:policy :fast-safe)                  ; declare optimize的な
  (:args (x :scs (signed-reg))          ; 引数宣言。後述
         (y :scs (signed-reg)))
  (:arg-types fixnum fixnum)            ; 引数の型宣言
  (:results (r :scs (signed-reg)))      ; 返り値宣言。後述
  (:result-types fixnum)                ; 返り値の型宣言
  (:generator 4                         ; 翻訳するときのコスト
              (move r x)                ; 返り値レジスタにxを移動
              (inst add r y)))          ; 返り値レジスタにyを足し込む
(in-package :vop-sample)

(defun add (x y)                        ; 安全なバージョンでラップする
  (add x y))
```

ちょっとおまじないが多いですね。

すこし解説すると引数は基本レジスタ渡しで、レジスタが足りなければスタックも使います。で、レジスタやスタックの値には型があります。それがストレージクラス(sc)です。この場合、x、y、rは`signed-reg`と宣言されてますね。符号付きレジスタです。`:scs`の最後のsは複数形のsです。今回は1つしか指定してませんが複数指定することも可能なのです。

`move`というのはアセンブラ命令ではなく、マクロかなんかです（適当）。VOPなのかな？どのストレージクラスからどのストレージクラスに移動するかを認知して適切な命令を吐きます。

`inst`が付いてるのがアセンブラですね。

このコード、xとrが等しいときに最適化出来るのですがそれはまあおいといて、こいつをディスアセンブルしてみましょう。

```
VOP-SAMPLE> (disassemble #'add)
; disassembly for ADD
; Size: 43 bytes. Origin: #x1005C26416
; 16:       488BD3           MOV RDX, RBX                     ; no-arg-parsing entry point
; 19:       48D1FA           SAR RDX, 1
; 1C:       488BF9           MOV RDI, RCX
; 1F:       48D1FF           SAR RDI, 1
; 22:       4801FA           ADD RDX, RDI
; 25:       48D1E2           SHL RDX, 1
; 28:       488BE5           MOV RSP, RBP
; 2B:       F8               CLC
; 2C:       5D               POP RBP
; 2D:       C3               RET
; 2E:       CC0A             BREAK 10                         ; error trap
; 30:       02               BYTE #X02
; 31:       19               BYTE #X19                        ; INVALID-ARG-COUNT-ERROR
; 32:       9A               BYTE #X9A                        ; RCX
; 33:       CC0A             BREAK 10                         ; error trap
; 35:       04               BYTE #X04
; 36:       08               BYTE #X08                        ; OBJECT-NOT-FIXNUM-ERROR
; 37:       FE1B01           BYTE #XFE, #X1B, #X01            ; RDX
; 3A:       CC0A             BREAK 10                         ; error trap
; 3C:       04               BYTE #X04
; 3D:       08               BYTE #X08                        ; OBJECT-NOT-FIXNUM-ERROR
; 3E:       FE9B03           BYTE #XFE, #X9B, #X03            ; RDI
NIL
```

主要な部分はここです。

```
; 19:       48D1FA           SAR RDX, 1
; 1C:       488BF9           MOV RDI, RCX
; 1F:       48D1FF           SAR RDI, 1
; 22:       4801FA           ADD RDX, RDI
; 25:       48D1E2           SHL RDX, 1
; 28:       488BE5           MOV RSP, RBP
; 2B:       F8               CLC
; 2C:       5D               POP RBP
; 2D:       C3               RET
```

`ADD`の他に無駄な命令がいくつかありますね。

SBCLはintの下位1bitをGCのときのタグとして使ってるのでアセンブラに渡す前に算術右シフト(`SAR`)して渡してます。
そして返るときはまた左シフト(`SHL`)してます。

その後の

```
; 28:       488BE5           MOV RSP, RBP
; 2B:       F8               CLC
; 2C:       5D               POP RBP
; 2D:       C3               RET
```

は関数から返るときのイディオム(スタックポインタを復元してフラグをクリアして呼び出し元に戻る)です。

余談ですがLispマシンなどのタグマシンは下位ビットにあるタグを無視して計算出来るのでシフトが不要になります。なので速いんですね。

# シフトをなくす
さっきは`signed-reg`を指定しました。つまり「（アセンブラの）intをくれ」と要求した訳です。
intの下位1bitは0なので足し算する分には別にシフトされなくても問題ありませんよね。シフトを殺しましょう。

さっきのコードの下にこれを足します。`add`は再定義しないと反映されないようでした。

```lisp
(in-package "SB-VM")
(define-vop (vop-sample::add/v2)        ; vop名は異なる
  (:translate vop-sample::add)          ; 関数名は同じ
  (:policy :fast-safe)
  (:args (x :scs (any-reg))             ; any-regになってる
         (y :scs (any-reg)))            ; any-regになってる
  (:arg-types fixnum fixnum)
  (:results (r :scs (any-reg)))         ; any-regになってる
  (:result-types fixnum)
  (:generator 3                         ; コストをさっきより低くすると優先して使ってくれる
              (move r x)
              (inst add r y)))

(in-package :vop-sample)

(defun add (x y)
  (add x y))

```

んで、ディスアセンブルしてみると

VOP-SAMPLE> (disassemble #'add)

```
; disassembly for ADD
; Size: 31 bytes. Origin: #x1004C2AB83
; 83:       488BD1           MOV RDX, RCX                     ; no-arg-parsing entry point
; 86:       4801FA           ADD RDX, RDI
; 89:       488BE5           MOV RSP, RBP
; 8C:       F8               CLC
; 8D:       5D               POP RBP
; 8E:       C3               RET
; 8F:       CC0A             BREAK 10                         ; error trap
; 91:       02               BYTE #X02
; 92:       19               BYTE #X19                        ; INVALID-ARG-COUNT-ERROR
; 93:       9A               BYTE #X9A                        ; RCX
; 94:       CC0A             BREAK 10                         ; error trap
; 96:       04               BYTE #X04
; 97:       08               BYTE #X08                        ; OBJECT-NOT-FIXNUM-ERROR
; 98:       FE1B01           BYTE #XFE, #X1B, #X01            ; RDX
; 9B:       CC0A             BREAK 10                         ; error trap
; 9D:       04               BYTE #X04
; 9E:       08               BYTE #X08                        ; OBJECT-NOT-FIXNUM-ERROR
; 9F:       FE9B03           BYTE #XFE, #X9B, #X03            ; RDI
NIL
```

はい。見事にSARとSHLが消えましたね。


# もう少し複雑な型を扱う
アセンブラですし`(simple-array (unsigned-byte 8) (*))`(以下octets)を扱いたいですよね。とりあえず難しいことは考えずにoctetsの0番目の要素にアクセスしてみましょう。とはいっても`simple-array`は長さや要素の型の情報も持っていることが予想されるので少しデータを読み飛ばさないといけませんね。

その辺の計算が分からなかったのでsbclのソースからそれっぽいものを参考にしました。

```lisp
(defknown access-simple-array-0 ((simple-array (unsigned-byte 8) (*)))
    (unsigned-byte 8)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)
(in-package "SB-VM")
(define-vop (vop-sample::access-simple-array-0)
  (:translate vop-sample::access-simple-array-0)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:arg-types *)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
              (inst movzx r 
               (make-ea :byte :base x
                        :disp (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag)))))

(in-package :vop-sample)
(defvar *octets* (make-array 4
                             :element-type '(unsigned-byte 8)
                             :initial-contents '(10 11 12 13)))

(defun access-simple-array-0 (x)
  (access-simple-array-0 x))
```

こんな感じになります。境界チェックとかはやってませんが許して下さい。

`descriptor-reg`というのがポインタが入ってるレジスタっぽいです。`movzx`は8bitの値を64bitのレジスタに符号拡張しながらロードする命令です。

`make-ea`というのがアドレッシングですね。`x`レジスタを起点として`(- (* vector-data-offset n-word-bytes) other-pointer-lowtag)`バイト(？ワード？)先のメモリ1byteを指します。

ディスアセンブルしてみましょう。みなさんもう慣れてきたと思うので主要部だけ抜き出します。

```
; 65:       0FB65101         MOVZX EDX, BYTE PTR [RCX+1]      ; no-arg-parsing entry point
; 69:       48D1E2           SHL RDX, 1
```

短いですね。この`BYTE PTR [RCX+1]`が`make-ea`した値に対応します。`RCX`は`x`で`(- (* vector-data-offset n-word-bytes) other-pointer-lowtag)`の結果が1に現れてるんでしょう。補足しておくと、`EDX`と`RDX`は同じ場所を指します。32bitとして扱うときはE、64bitとして扱うときはRで指します。

さて、私はx86のアドレッシングモードなんて全然知らないのですがコードを見る限りもうちょっと複雑なアドレッシングが出来るようです。

配列のn番目にアクセスするコードが良い例のようです。

```lisp
(defknown access-simple-array-n ((simple-array (unsigned-byte 8) (*)) (unsigned-byte 64))
    (unsigned-byte 8)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)
(in-package "SB-VM")
(define-vop (vop-sample::access-simple-array-n)
  (:translate vop-sample::access-simple-array-n)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg))
         (i :scs (unsigned-reg)))
  (:arg-types * unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
              (inst movzx r 
               (make-ea :byte :base x
                        :scale 1
                        :index i
                        :disp (- (* vector-data-offset n-word-bytes)
                               other-pointer-lowtag)))))

(in-package :vop-sample)
(defun access-simple-array-n (x i)
  (access-simple-array-n x i))
```

新たに引数`i`をとるようになったのと`make-ea`の引数に`:scale 1 :index i`が加わってます。

ディスアセンブルしてみましょう。

```
; 98:       0FB6543901       MOVZX EDX, BYTE PTR [RCX+RDI+1]  ; no-arg-parsing entry point
; 9D:       48D1E2           SHL RDX, 1
```

アドレッシングに`+RDI`が加わりましたね。どうして`i`(`RDI`)を`RAS`しなくていいのか気になりますがまあ、とりあえず正常に動いてるようです。

おわかりかと思いますがアドレッシングが`x`をベースにして今までの定数オフセット+新たにレジスタで指定したオフセットになってます。

`make-ea`に渡した`:index`は何か分かるとしても`:scale`が気になりますよね。`scale`を2にしてディスアセンブルしてみます。

```
; 58:       0FB6547901       MOVZX EDX, BYTE PTR [RCX+RDI*2+1]  ; no-arg-parsing entry point
; 5D:       48D1E2           SHL RDX, 1
```

はい。インデックスを定数倍するようですね。

# SSEにチャレンジ
インテルアーキテクチャにあるSSEとはStreaming SIMD Extensionsの略です。じゃあSIMDは何かというとSimple Instruction Mulitple Dataの略で、1命令で複数のデータを処理出来ます。


この「複数のデータ」というのは64bit2つや32bit4つなどを128bitにまとめて渡します。128bitの値なんてどうやって作るんだよって感じですがsbcl-1.1.8から入った`sb-ext:%make-simd-pack-*`が存在します。

```
VOP-SAMPLE> (%make-simd-pack-ub32 1 2 3 4)
#<SIMD-PACK  01 00 00 00  02 00 00 00  03 00 00 00  04 00 00 00>
```

こんな感じです。x86_64がリトルエンディアンだったのを思い出させる表記ですね。ub32の他にub64、single、doubleが存在します。

ストレージクラスも`*-sse-reg`というものがあるのでこれを使いましょう。

```lisp
(defknown simd-add ((simd-pack (unsigned-byte 64)) (simd-pack (unsigned-byte 64)))
    (simd-pack (unsigned-byte 32))
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)
(in-package "SB-VM")
(define-vop (vop-sample::simd-add)
  (:translate vop-sample::simd-add)
  (:policy :fast-safe)
  (:args (x :scs (int-sse-reg))
         (y :scs (int-sse-reg)))
  (:arg-types simd-pack-int simd-pack-int)
  (:results (r :scs (int-sse-reg)))
  (:result-types simd-pack-int)
  (:generator 4
              (move r x)
              (inst padddw r y)))

(in-package :vop-sample)
(defun simd-add (x y)
  (simd-add x y))
```

はい。こんな感じですね。`paddw`はparallel add wordですかね。これをディスアセンブルすると思ったより大きな命令になったので全部貼っときますね。

    VOP-SAMPLE> (disassemble #'simd-add)
    
```
; disassembly for SIMD-ADD
; Size: 108 bytes. Origin: #x1006D64A21
; 21:       660F6FC2         MOVDQA XMM0, XMM2                ; no-arg-parsing entry point
; 25:       660FFDC1         PADDW XMM0, XMM1
; 29:       49896C2440       MOV [R12+64], RBP                ; thread.pseudo-atomic-bits
; 2E:       4D8B5C2418       MOV R11, [R12+24]                ; thread.alloc-region
; 33:       498D5320         LEA RDX, [R11+32]
; 37:       493B542420       CMP RDX, [R12+32]
; 3C:       7740             JNBE L2
; 3E:       4989542418       MOV [R12+24], RDX                ; thread.alloc-region
; 43:       498D530F         LEA RDX, [R11+15]
; 47: L0:   48C742F165030000 MOV QWORD PTR [RDX-15], 869
; 4F:       48C742F900000000 MOV QWORD PTR [RDX-7], 0
; 57:       660F7F4201       MOVDQA [RDX+1], XMM0
; 5C:       49316C2440       XOR [R12+64], RBP                ; thread.pseudo-atomic-bits
; 61:       7402             JEQ L1
; 63:       cc09             break 9                          ; pending interrupt trap
; 65: l1:   488be5           mov rsp, rbp
; 68:       f8               clc
; 69:       5d               pop rbp
; 6a:       c3               ret
; 6b:       cc0a             break 10                         ; error trap
; 6d:       02               byte #x02
; 6e:       19               byte #x19                        ; invalid-arg-count-error
; 6f:       9a               byte #x9a                        ; rcx
; 70:       cc0a             break 10                         ; error trap
; 72:       04               byte #x04
; 73:       32               byte #x32                        ; object-not-simd-pack-error
; 74:       fe1b01           byte #xfe, #x1b, #x01            ; rdx
; 77:       cc0a             break 10                         ; error trap
; 79:       04               byte #x04
; 7a:       32               byte #x32                        ; object-not-simd-pack-error
; 7b:       fe9b03           byte #xfe, #x9b, #x03            ; rdi
; 7e: l2:   6a20             push 32
; 80:       bac0854200       mov edx, 4359616                 ; alloc_tramp
; 85:       ffd2             call rdx
; 87:       5a               pop rdx
; 88:       80ca0f           or dl, 15
; 8b:       ebba             jmp l0
nil
```
なにやってるのやら。

# 出来なかったこと

`PCMPESTRI`を使ってみたかったのですが扱いがトリッキーなので断念しました。具体的に言うと操作する値を格納するレジスタがEAXとECX固定なようなのです。
`:temporary`節で内部で使うレジスタも要求出来るようなのですが名指しでもらえるんですかね。

SSEの使い方とsimple-arrayから要素の配列を取り出す方法までは示したので誰かやって下さい。

参考資料いっぱい置いときますね。

[How to Define New Intrinsics in SBCL - Paul Khuong mostly on Lisp](http://www.pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/)
: VOPの使い方。最初の方に出てきたaddの最適化のやつとかも出てくる。(en)

[Hacking SSE Intrinsics in SBCL (part 1) - Paul Khuong mostly on Lisp](http://pvk.ca/Blog/Lisp/hacking_SSE_intrinsics-part_1.html)
: SBCLのsimd-packの具体的解説(en)

[Fresh in SBCL 1.1.8: SSE Intrinsics! - Paul Khuong mostly on Lisp](http://www.pvk.ca/Blog/2013/06/05/fresh-in-sbcl-1-dot-1-8-sse-intrinsics/)
: SBCLでVOPとSSEを使ってマンデルブロ集合を計算する(en)

[Packed Compare Intrinsics](https://software.intel.com/en-us/node/514244)
: IntelのPCMPESTRIとかのマニュアル(en)

[_mm_cmpestri](http://msdn.microsoft.com/en-us/library/bb531465.aspx)
: MicrosoftのPCMPESTRIのマニュアル。こっちの方が分かりやすい(en)

[Intel optimization](http://homepage1.nifty.com/herumi/prog/intel-opt.html)
: PCMPISTRIを使った`strlen`の実装例(ja)

[Kazuho's Weblog: Improving Parser Performance using SSE Instructions (in case of PicoHTTPParser)](http://blog.kazuhooku.com/2014/12/improving-parser-performance-using-sse.html)
: PCMPESTRIを使ってHTTPパーサーを高速化した話。これをやりたかった。(en)


# おわりに

おつかれ様でした。たまには低レベルなことをやっても良いんじゃないでしょうか。

明日はnobkzさんで、Shenについてです。
