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


# HTTPについて
-------------

* 1990年誕生の骨董仕様
* Human Readableなテキストベース
* パフォーマンスはあまり考慮してない


# HTTP/1.1の限界

* フォーマットがゆるふわでパースしづらい
* 何度も似たようなヘッダをる
  + 割とネットワーク負荷が高い
* Head of Line Blocking


```
GET / HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: ja
Accept-Encoding: gzip, deflate
Cookie: _ga=GA1.1.1989570020.1429589222; __utma=111872281.1989570020.1429589222.1430193585.1431477266.5; __utmz=111872281.1429589222.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __utmc=111872281
Connection: keep-alive
Cache-Control: max-age=0
```



```
GET /js/todo.js HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0
Accept: */*
Accept-Language: ja
Accept-Encoding: gzip, deflate
Referer: http://localhost:8080/
Cookie: _ga=GA1.1.1989570020.1429589222; __utma=111872281.1989570020.1429589222.1430193585.1431477266.5; __utmz=111872281.1429589222.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __utmc=111872281
Connection: keep-alive
Cache-Control: max-age=0
```



```
GET /style/main.css HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0
Accept: text/css,*/*;q=0.1
Accept-Language: ja
Accept-Encoding: gzip, deflate
Referer: http://localhost:8080/
Cookie: _ga=GA1.1.1989570020.1429589222; __utma=111872281.1989570020.1429589222.1430193585.1431477266.5; __utmz=111872281.1429589222.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __utmc=111872281
Connection: keep-alive
Cache-Control: max-age=0
```

# 涙ぐましい努力
--------------

* css/js concatenation
* image inlining
* image sprite


# HTTP/2

<!-- .slide: class="center" -->

# HTTP/2
--------

* 2015-05-15(先週の金曜)に[RFC化](http://jxck.hatenablog.com/entry/http2-rfc7540)
* HTTP/1.1に限界を感じたGoogleによって作られたSPDYがベース
* これから広まっていく


# HTTP/2の特徴
-------------

* バイナリベースになってパースが楽に
* セマンティクスはHTTP/1.xのものを保持
* ヘッタの圧縮も行なう([HPACK]())
* その他拡張も多数


## セマンティクスの保持
--------------------

* HTTP/2 -> HTTP/1.xへの変換が可能
* つまり、(リバース)プロキシの内側は1.x、外側は2が可能
  + アプリケーションはいじらずにフロント側が対応すればすぐに使える

CF [nghttpx](http://qiita.com/tatsuhiro-t/items/99a2fd61d0fb16d7241b)

## HPACK
HTTP2のヘッダ圧縮 Huffman Encode の原理とメリット・デメリット - Qiita
http://qiita.com/iwanaga/items/98f60003c0114e04095e

# HTTP/2の新機能
---------------

* ストリーム
* フロー制御
* サーバープッシュ


# ストリーム
----------

# フロー制御
HTTP2 のフロー制御 - Qiita
http://qiita.com/Jxck_/items/622162ad8bcb69fa043d
具体的な状況はいくつか考えられます。

大きなファイルの通信が帯域を食いつぶし、他の通信を妨害する。
あるリクエストの処理にサーバがかかりっきりになり、他のリクエストをサーバが処理してくれなくなる。
高速なアップロードを行うクライアントと、低速な書き込みをしているサーバとの間に挟まったプロキシが、調整のためにデータを貯めているバッファが溢れる。

# サーバープッシュ
----------------

# Availability
https://github.com/akamai/cl-http2-protocol




</script>
</section>
