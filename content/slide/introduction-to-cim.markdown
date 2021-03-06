---
type: slide
title: "Introduction to CIM"
date: 2014-08-30
aliases:
    - /slide/introduction-to-cim.html
categories: [Common Lisp, Lisp, CIM]
description: "CIMの紹介スライド。<br>
もしかしたらShibuya.lisp TT #8で喋るかも。
"
---
<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">

# Introduction to CIM
<hr />

κeen(@blackenedgold)  

<!-- .slide: class="center" -->

## 突然ですが
Common Lispのスクリプトファイルの実行方法知ってますか？

sbcl

    sbcl --script $file

ecl

    ecl -shell $file

ccl

    ccl -e $file -e '(quit)'

...

<strong>シバン可搬性のあるスクリプトが書けない</strong>

## そこで、こんなの書きました
<hr />

```sh
case "$LISP_IMPL" in
    sbcl*)
        sbcl --script $1
        ;;
    clisp*)
        clisp -on-error exit -i $1
        ;;
    ecl*)
        ecl -shell $1
        ;;
    ccl*)
...
```

## でも問題がある
<hr />

* どの処理系をバックエンドに使うか
* 処理系のパスをどう捜すか
* そもそもどうやって使ってもらうか

## κeenの答え
<hr />

> 処理系の管理までやってしまおう。
> そうしたら一緒に使ってもらえる。

＿人人人人人人＿  
＞　CIMの誕生　＜  
￣Y^Y^Y^Y^Y￣

<!-- .slide: class="center" -->

## CIMとは
<hr />

* Common Lisp Implementation Manager
* ちむ
* Rubyのrvmを強く意識
* Common Lisp処理系のインストール、切り替え、コマンドラインインターフェースなど。
* `cim`、 `cl`、 `ql`コマンドから成る
* B Shell スクリプト製(マルチプラットフォーム対応のため)
* 2013/10くらいからぼちぼち開発

## `cim`コマンドについて
<hr />

インストール

    cim install sbcl

バックエンドの切り替え

    cim use sbcl

現在情報

    cim info

CIMのアップデート

    cim get

など。

## `cl`コマンドについて
<hr />

* 最初に紹介したシェルスクリプトベース
* コマンドラインからCommon Lispを使うことを重視
* シバン対応も。
* REPLは独自実装(デバッガを黙らせるのが主な目的)
* オプションはrubyを意識

## 少しシバンの話
<hr \>

### だめな例
```lisp
#!cl
(write-line "ok")
```
```lisp
#!/home/kim/.cim/bin/cl
(write-line "ok")
```
```lisp
#!/usr/bin/env cl -q
(write-line "ok")
```

### 推奨される例
```lisp
#!/bin/sh
#|
exec cl --  "$0" "$@"
|#
(write-line "ok")
```


## `ql`コマンドについて
<hr />

* quicklispのコマンドラインインターフェース
* まだ未熟
* rubyのgem的な。
* 最近`ql install`を高速化

## その他の話
<hr />

### 対応処理系
* 処理系はabcl, alisp, ccl, clisp, ecl, sbcl対応
* 基本ビルドする
* gclはANSIじゃないのでドロップ
* lispworks他商用ははインストール出来ないのでドロップ
* mkcl対応する？
* CMU CLはつらい…

### 実装
* B Shell スクリプトで書いたの失敗だった?
* つらい
* 開発速度に影響
* テスト（=品質）にも影響
* sbclがじゃじゃ馬でつらい

### 方針
* コマンドラインユーティリティである
* 基本POSIX以外に依存しない
* 極力環境に影響しない(rvmがひどいのを嫌って)
* clコマンドはあくまで薄いラッパ
* asdfも使わない

## [半年前](http://www.slideshare.net/blackenedgold/cim-common-lisp-implementation-manager?ref=http://keens.github.io/blog/2014/01/24/lisp-meet-up-number-13/)から何が変わったか
<hr />

* バグ潰し
* その他細かな改善
  + ダンプされたコアを読み込む機能
  + SIGINTをハンドル
  + ビルド時のログ制御
* ぼちぼちテスト書き始めた

## 将来の話
<hr />

* テストと品質の安定化
* バイナリ配布の処理系は大人しくバイナリ使う？
* 最適なビルドオプション
* コアダンプ機能
* Lispスクリプト(実行可能ファイル)インストーラ
* cl21との連携（バイナリ生成）
* もうちょっとquicklispとの連携
* ユーザー拡張

## 関連リソース
<hr />

* [The problem with Lisp ](http://pupeno.com/2007/08/26/the-problem-with-lisp/) : Lispはコマンドが作れないから流行らないという分析
* [CIMの解説をしてみる コマンド編 | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2014/01/27/cim-explanation/) : CIMの解説記事
* [shelly](https://github.com/fukamachi/shelly) : CIMを使うCLのコマンドラインインターフェース
* [lsp](https://github.com/snmsts/lsp) : CIMのC実装
* [qlot](https://github.com/fukamachi/qlot) : `ql`よりリッチなquicklispラッパ

<span style="font-size:600%">以上</span>  
何か質問あればどうぞ

<!-- .slide: class="center" -->
</script>
</section>
