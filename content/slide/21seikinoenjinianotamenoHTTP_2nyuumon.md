---
categories: []
date: 2015-05-18T00:24:08+09:00
draft: true
description: "第3会FRESH勉強会で発表予定のスライド。HTTPについて詳しくない人のために
HTTPの概要から先日RFC化されたHTTP/2の新機能、使いどころを解説します。
"
title: 21世紀のエンジニアのためのHTTP/2入門
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# {{ .Page.Titile }}
----------------------

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 渋谷のエンジニア
 + Lisp, ML, Shell Scriptあたりを書きます


# HTTPとは

# HTTP/1.1の限界

# HTTP/2の特徴

## セマンティクスの保持

## HPACK

# HTTP/2の新機能

# ストリーム

# サーバープッシュ

# 優先度

HTTP2 のフロー制御 - Qiita
http://qiita.com/Jxck_/items/622162ad8bcb69fa043d
具体的な状況はいくつか考えられます。

大きなファイルの通信が帯域を食いつぶし、他の通信を妨害する。
あるリクエストの処理にサーバがかかりっきりになり、他のリクエストをサーバが処理してくれなくなる。
    高速なアップロードを行うクライアントと、低速な書き込みをしているサーバとの間に挟まったプロキシが、調整のためにデータを貯めているバッファが溢れる。



HTTP2のヘッダ圧縮 Huffman Encode の原理とメリット・デメリット - Qiita
http://qiita.com/iwanaga/items/98f60003c0114e04095e


HTTP/1.1仕様
ハイパーテキスト転送プロトコル (HTTP) は、分散・共同体ハイパーメディ
ア情報システムのアプリケーションレベルプロトコルである。HTTP は、
World-Wide Web グローバル情報利用の先進として、1990 年から使われてい
る。
</script>
</section>
