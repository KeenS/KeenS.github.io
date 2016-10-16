---
categories: [Lisp, Common Lisp, Arrows]
description: "Clack Meet Up #1 での発表資料<br>
Arrowsについて。
"
date: 2015-03-02T23:36:03+09:00
title: 既存のテンプレートエンジンの問題点と再設計
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
<style type="text/css">
.red {
color: red;
}
</style>
# 既存のテンプレートエンジンの問題点と再設計、あるいはArrowsについて
-----------------------------------------------------------------
Clack Meet Up #1  
2015-03-05 @サムライト

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 野生のLisper
 + Lisp, Ruby, OCaml, Shell Scriptあたりを書きます
===
# Template Engines

<!-- .slide: class="center" -->
===
# Existing Architeture
----------------------
0. (リクエスト)
1. アプリ
  0. 引数を計算
  1. テンプレートに引数を渡す
  2. レスポンスをレンダリング <!--.element: class="fragment grow"  -->
4. レスポンスをサーバに渡す
5. (レスポンス)
===
# Rendering?
----------------------
* 結合した文字列はクライアントに返ったらその後はゴミ
 + GCへ負荷がかかる (後述)
* クライアントにとって1つの文字列である必要はない
* むしろ返せる部分だけ先に返した方が得（後述）
===
# GC Pressure (SBCL)
-----------------
* 世代別Copy GC
* 結合した文字列は比較的大きい
* `alloc_space`に入らない大きさならアロケートが遅い
* GCを頻繁に起動してしまう
* 16KBを越えると特別扱いされて遅い/メモリを喰う
* LispのWebアプリはレスポンスタイムの分散が大きい（要出展）

参考: [SBCL GENCGC @ x86 Linux](http://cl-www.msi.co.jp/reports/sbcl-gc-memo.html)
===
# Split Response
---------------
例えば、こんなの

<pre><code class="html">&lt;!DOCTYPE html&gt;
&lt;html xmlns=&quot;http://www.w3.org/1999/xhtml&quot; xml:lang=&quot;en&quot; lang=&quot;en-us&quot;&gt;
    &lt;head&gt;
        &lt;link rel=&quot;stylesheet&quot; href=&quot;http://localhost:1313//reveal.js/lib/css/xcode.css&quot;&gt;
        &lt;script src=&quot;//ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js&quot;&gt;&lt;/script&gt;
        ....
        &lt;/script&gt;
    &lt;/head&gt;
    &lt;body class=&quot;li-body&quot;&gt;
    &lt;header&gt;{{ header }}&lt;/header&gt;
    {{ body }}
    ...
</code></pre>
===
# Split Response
---------------
1. `{{ header }}`の前に先頭から`<header>`までを返す
2. `{{ header }}`を返す
3. `</header>`を返す
4. `{{ body }}`を返す

...

===
# Pros of Split Response
------------------------
* `header`を計算してる間にクライアントにhead部分が渡る
  + 先に`<link>`や`<script>`を要求出来る
  + サーバ側のスループットやレスポンスタイムは変わらないが
    クライアントのレンダリング完了までの時間は大幅に短縮出来る
* 文字列を結合する必要がない
* 定数部分については長さが判ってるので最適化し易い
===
# How TEs Work 
---------------------------
## Compilation
1. テンプレート文字列
 + パーサ <!-- .element: style="list-style-image: url(/images/arrow_down.svg);" -->
2. 抽象構文木
 + コードエミット <!-- .element: style="list-style-image: url(/images/arrow_down.svg);" -->
3. レンダリング関数
===
# How TEs Work 
---------------------------
## Rendering
1. レンダリング関数
 + 引数 <!-- .element: style="list-style-image: url(/images/arrow_down.svg);" -->
2. 文字列
===
# Misc Problems
---------------
* サーバに渡すのは文字列なのにソケットに書き込む時はオクテット？
  + 文字列で返す？オクテットで返す？（デバッグがー）
  + オクテットの変換はいつ？
* ストリームが遅い？
  + POSIX APIが使える"なら"fdの方が速い？
* ユーザが用意したバッファに書き出したい？
* テンプレートに渡す引数が定数文字列なら畳み込める筈？
* リクエストの度にテンプレートパースするのは筋悪だけど開発中は毎回コンパイルするのは面倒？
===
<!-- .slide: data-background="/images/arrows.jpg" -->

# Arrows

Template flies like an arrow

<!-- .slide: class="center" -->
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# Arrows
--------
* [KeenS/arrows](https://github.com/KeenS/arrows)
* 現在開発中のテンプレートエンジン
* 複数のテンプレートが選べる（予定）
* 複数のバックエンドが選べる
* バックエンドに依ってはnon-consing
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# How Arrows Works
------------------
## Compilation
1. テンプレート文字列<span class="red"> + 定数引数</span>
  + パーサ<span class="red">(default, cl-emd互換...)</span> <!-- .element: style="list-style-image: url(/images/arrow_down.svg);" -->
2. 抽象構文木
  + <span class="red">最適化(const folding, concat, convert...)</span> <!-- .element: style="list-style-image: url(/images/arrow_down.svg);" -->
  + コードエミット <!-- .element: style="list-style-image: url(/images/arrow_down.svg);" -->
3. レンダリング関数<span class="red">(string, octets, stream, fast-io ...)</span>
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# How Arrows Works
------------------
## Rendering
1. レンダリング関数
  + 引数 <!-- .element: style="list-style-image: url(/images/arrow_down.svg);" -->
2. 文字列<span class="red">、オクテット列、ストリーム書き出し、fast-io…</span>
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# How compiled
--------------
```html
<h1>Hi {{var name}}!</h1>
```
を
```lisp
(compile-template-string :xxx
  "<h1>Hi {{var name}}!</h1>" ())
```
とコンパイル
===
# Stream backend
----------------
<!-- .slide: data-background="/images/arrows.jpg" -->
```lisp
(lambda (stream &key name)
  (write-string "<h1>Hi " stream)
  (write-string
    (encode-for-tt (princ-to-string name))
    stream)
  (write-string "!</h1>" stream))
```
* ほとんどアロケートしない
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# Octet backend
----------------
```lisp
(lambda (&key name)
  (with-fast-output (buffer)
    (fast-write-sequence
      #.(string-to-octets "<h1>Hi ")
       buffer)
    (fast-write-sequence
      (string-to-octets
        (encode-for-tt (princ-to-string name)))
                       buffer)
    (fast-write-sequence
      #.(string-to-octets "!</h1>")
      buffer)))
```
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# How optimized
---------------
```html
<h1>Hi {{var name}}!</h1>
```
を

```lisp
(compile-template-string :stream
   "<h1>Hi {{var name}}!</h1>"
   '(:known-args (:name "<κeen>")))
```
とコンパイル
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# How optimized
---------------
## variable folding
```lisp
(lambda (stream)
  (write-string "<h1>Hi " stream)
  (write-string (encode-for-tt "<κeen>") stream)
  (write-string "!</h1>" stream))
```
* 既知の引数は畳み込む
* 文字列なら`princ-to-string`しない
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# How optimized
---------------
## const folding
```lisp
(lambda (stream)
  (write-string "<h1>Hi " stream)
  (write-string "&lt;κeen&gt;" stream)
  (write-string "!</h1>" stream))
```
* 定数のエスケープはコンパイル時に済ませる
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# How optimized
---------------
## append sequence
```lisp
(lambda (stream)
  (write-string "<h1>Hi &lt;κeen&gt;!</h1>"
                stream))
```
* 複数シーケンスの書き出しは1つにまとめる
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# Further Ideas
---------------
* 引数計算の遅延
* 引数計算の並列化
* 非同期化
* HTML compction
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# Further Ideas
---------------
## 引数計算の遅延
* `name`の計算が重いときに先に`"<h1>Hi "`を返す。
* `name`は必要になったら値を計算する(Promise パターン)
```lisp
(lambda (stream &key name)
  (write-string "<h1>Hi " stream)
  (write-string
    (encode-for-tt (princ-to-string name))
    stream)
  (write-string "!</h1>" stream))
```
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# Further Ideas
---------------
## 引数計算の並列化
* `name`の計算が重いときに先に`"<h1>Hi "`を返す。
* `name`は並列に計算して必要になったら値を要求する(Futureパターン)
```lisp
(lambda (stream &key name)
  (write-string "<h1>Hi " stream)
  (write-string
    (encode-for-tt (princ-to-string name))
    stream)
  (write-string "!</h1>" stream))
```
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# Further Ideas
---------------
## 非同期化
* 単純にwriteを非同期にする
* 他にFutureもブロックするので非同期Futureを使う
===
<!-- .slide: data-background="/images/arrows.jpg" -->
# Further Ideas
---------------
## HTML compction
```html
<ol>
  <li> item 1 </li>
  <li> item 2 </li>
  <li> item 3 </li>
</ol>

```
を
```html
<ol><li>item 1</li><li>item 2</li><li>item 3</li></ol>

```
* DOM構造が変わってしまう

===
<!-- .slide: data-background="/images/arrows.jpg" -->
# TODOs
-------
* 設計上複数シンタックスをサポート可能だがまだしてない
* 既存のテンプレートエンジンとの比較ベンチマーク
* 高速化
* 多機能化
  + テンプレート
  + 最適化
  + バックエンド
* clackとの連携
  + clackのAPIはメモリアロケーションが多めに必要になる
===
# Summary
---------
* 既存のテンプレートエンジンは非効率
  + メモリを無駄遣いしていた
  + ユーザーのことを考えてなかった
* 新しいテンプレートエンジンを設計した
  + メモリアロケーションをあまりしない
  + ユーザー側の速度まで考慮した
  + 柔軟
</script>
</section>
