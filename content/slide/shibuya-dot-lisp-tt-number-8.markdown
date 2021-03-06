---
type: slide
title: "授業で半年間moclを使ってみた"
date: 2014-08-28
aliases:
    - /slide/shibuya-dot-lisp-tt-number-8.html
categories: [Common Lisp, Lisp, mocl, Android]
description: "Shibuya.lisp TT #8 LT用。<br>
moclを授業で半年扱ったときの雑感です。
"
---
<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">


# 授業で半年間moclを使ってみた
<hr />

Shibuya.lisp TT #8  
<br>
κeen(@blackenedgold)  

<!-- .slide: class="center" -->

## 自己紹介
<hr />

 + κeen
 + 東大数学科の4年生
 + ソーシャルアカウントは上のアイコン達から。
 + Lisp, Ruby, OCaml, Shell Scriptあたりを書きます

 一年前

<!-- .slide: class="center" -->

## [計算数学II](http://ks.ms.u-tokyo.ac.jp/)
![計算数学IIのホームページ](/images/shibuya_lisp_TT8/calculation_math.png)

自分でテーマを決めて半年間取り組む

チーム[人造エイリアン](https://sites.google.com/site/2013ks2/home)
![チーム人造エイリアン](/images/shibuya_lisp_TT8/android_alien.png)

## Common Lisp でAndroid?

<!-- .slide: class="center" -->


## [mocl](https://wukix.com/mocl)
![mocl home](/images/shibuya_lisp_TT8/mocl.png)

## What is mocl
<hr />

* iOS, Androidで動くCommon Lisp処理系
* LLVMをバックエンドに使う
* Networking, Unicode, CLOS, and More. Run Countless CL Libraries.

## 買ってもらった(・ω・)v
![mocl home](/images/shibuya_lisp_TT8/mocl_ut.png)

## 注意
<hr />

私が触ったのは1つ前のバージョンです。

一部5月のアップデートで改善されています。

## How it Works
<hr \>

<embed src="/images/shibuya_lisp_TT8/mocl_flowchart.svg" type="image/svg+xml" height="500px" />

## コードサンプル
<hr \>

授業で私が作ったオセロのAIのコード
```java
public BoardCanvas(Context context,int width, int height,int color ){
    ...
    CL.cl_init();
    CL.init_game(3 - color);
    ...
}
   
void nextHand(){
    String[] coord = CL.next().split(" ");
    CL.jput(Integer.parseInt(coord[0]), Integer.parseInt(coord[1]) , color);
    ...
}
```

## コードサンプル

```lisp
(declaim (call-in next))
(defun next ()
  (let ((dummy (make-board))
        (max most-negative-fixnum)
	    (x) (y) (score))
    ...
    ))
(declaim (call-in init-game))
(defun init-game (color)
  (setf *board* (make-board))
  (setf *ai* color))
(declaim (call-in jput))
(defun jput (x y color)
  (put *board* x y color))
```


## ライブラリについて
<hr />

* ASDFが使える
* `MOCL_HOME/systems/`以下に配置
* quicklispでインストールしたやつは`MOCL_HOME/systems/`にひたすら`ln -s`
* 大抵のライブラリは動かない

例えば`cl-annot`を使って
```lisp
(declaim (call-in jput))
(defun jput (x y color)
  (put *board* x y color))
```
を
```lisp
@call-in
(defun jput (x y color)
  (put *board* x y color))
```
としたい

私のときはCFFIを始め、ironclad、cl-annotなど、  
使おうとしたライブラリは全て動かなかった

現在の状態は不明


<!-- .slide: class="center" -->

## 作業フロー(Android)
<hr />

<ol>
<li>CLのコードを書いてSBCLで動かす</li>
<li>ある程度動いたらmoclでAndroid用のコードを生成</li>
<li>原因不明のエラーが出るので1に戻る</li>
<li>コンパイルが通れば生成されたCをコンパイル</li>
</ol>

<ol start="5">
<li>JavaからCLを呼び出すコードを書く</li>
<li>アプリをコンパイル</li>
<li>実機/エミュレータに転送</li>
</ol>

<!-- .slide: class="center" -->

<ol start="8">
<li>動かす</li>
<li>動かない</li>
<li>ひたすらlogcat眺める</li>
<li>1に戻る</li>
</ol>

<!-- .slide: class="center" -->

### ここがつらい
<hr />

* 作業が煩雑
* 例外のスタックトレースがJNIのせいで追いづらい
* logを出すには`rt:format`という関数のみ使える(ログレベルを設定出来ない)

※ 今はREPLが端末で動くので環境は良くなっています。

## 制限など
<hr />

* Java/Obj-CからCLを呼ぶことしか出来ない
  + プラットフォームのライブラリも呼べない
* やりとり出来るのは文字列、数値、真偽値のみ
  + 裏でタスクを回すworker的な役割or純粋関数的な役割のみ

※ 今はCLからObj-Cを呼ぶことが出来ます。

## 成果物
<hr />

* [コード(抜粋)](https://gist.github.com/KeenS/15e7bb35519818c88c13)
* [最終発表スライド](http://www.slideshare.net/blackenedgold/ss-30373688)
* [デモ動画](http://www.slideshare.net/blackenedgold/ss-30373688)

## おまけ

<!-- .slide: class="center" -->

## 5月のアップデート内容
<hr />

* OS Xアプリケーション対応
* 端末内でREPLが動く
* FFI強化
* Lisp内Obj-C構文
* iOSとOS XでのネイティブSSL対応
* ARM64サポート
* ドキュメントの改善
* 多くのバグフィクス

<span style="font-size:600%">以上</span>  
何か質問あればどうぞ

<!-- .slide: class="center" -->
</script>
</section>
