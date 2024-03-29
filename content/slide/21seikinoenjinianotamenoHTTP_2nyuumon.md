---
categories: [HTTP, Web, FRESH勉強会]
date: 2015-05-18T00:24:08+09:00
description: "第3会FRESH勉強会で発表予定のスライド。HTTPについて詳しくない人のために
HTTPの概要から先日RFC化されたHTTP/2の新機能、使いどころを解説します。
"
title: 21世紀のエンジニアのためのHTTP/2入門
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# 21世紀のエンジニアのためのHTTP/2入門
----------------------
サイバーエージェントFresh勉強会

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
# HTTPについて
-------------

* 1990年誕生の骨董仕様
* Human Readableなテキストベース
* パフォーマンスはあまり考慮してない

===
# HTTP/1.1の限界
---------------

* フォーマットがゆるふわでパースしづらい
* 何度も似たようなヘッダをる
  + 割とネットワーク負荷が高い
* 基本1コネクションにつき1ファイルの送受信
  + 短命なコネクションをいくつも張ることになる
  + コネクションが"ウォームアップ"する前に切れる
* Head of Line Blocking
  + 遅いコンテンツをダウンロードしてると他のコンテンツがダウンロード出来なくなる
===

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

===

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

===

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
===
# 涙ぐましい努力
--------------
何度もリクエストをしないためにファイル数を減らす様々な努力がされてきた

* css/js concatenation
  + cssやjsを1つのファイルにまとめてアクセスを減らす
* image inlining
  + 画像をBase64エンコードしてCSS内に埋め込む
* image sprite
  + 複数の画像を1まとめにして表示する時に切り出して使う
* 並列アクセス
  + ブラウザは最大6並列でサーバにアクセスする
===
# HTTP/2

<!-- .slide: class="center" -->
===
# HTTP/2
--------

* 2015-05-15(先週の金曜)に[RFC化](http://jxck.hatenablog.com/entry/http2-rfc7540)
* HTTP/1.1に限界を感じたGoogleによって作られたSPDYがベース
  + 現実の問題を解決している
  + 新しいがある程度の信頼性もある
* これから広まっていく

===
# HTTP/2の特徴
-------------

* 接続開始はHTTP/1.1のUpgradeを使う。
  + HTTP/1.1と共存可能
* バイナリベースになってパースが楽に
* セマンティクスはHTTP/1.xのものを保持
* ヘッタの圧縮も行なう([HPACK](http://http2.github.io/http2-spec/compression.html#indexing.tables))
* プロキシやリバースプロキシの存在も織り込んだ仕様(Hop by Hop)
* その他拡張も多数

CF [HTTP/2の現状とこれから](http://www.slideshare.net/shigeki_ohtsu/http2-ohtsu-html5conf2015)
===
## セマンティクスの保持
--------------------

* HTTP/2 -> HTTP/1.xへの変換が可能
* つまり、(リバース)プロキシの内側は1.x、外側は2が可能
  + アプリケーションはいじらずにフロント側が対応すればすぐに使える

CF [nghttpx](http://qiita.com/tatsuhiro-t/items/99a2fd61d0fb16d7241b)
===
## セマンティクスの保持
--------------------

こういうことが可能

```
+------+           +-------+             +---+
|Client|-[HTTP/2]->|R.Proxy|-[HTTP/1.1]->|App|
+------+           +-------+             +---+
```

===
## HPACK
--------

* よく使うヘッダは数値で表わす
  + Static Table
* 以前送ったヘッダも数値で表わせる
  + Dynamic Table
* それ以外もハフマン符号で圧縮可能

===
# HTTP/2の新機能
---------------

* ストリーム
* フロー制御
* サーバープッシュ

===
# ストリーム
------------

* ストリームは論理的なもの
* 1つのコネクション内で複数のストリームを作れる
  + 1コネクション内で複数のファイルをやりとり出来る
  + さらに、1コネクション内で並列に複数のファイルをやりとり出来る
* 短命なコネクションをいくつも張るよりずっと効率的
  + コネクションの性能をほぼフルで使い切れるようになった
===
# フロー制御
------------

スライド略。

[HTTP2 のフロー制御 - Qiita](http://qiita.com/Jxck_/items/622162ad8bcb69fa043d)

>具体的な状況はいくつか考えられます。
>
> * 大きなファイルの通信が帯域を食いつぶし、他の通信を妨害する。
> * あるリクエストの処理にサーバがかかりっきりになり、他のリクエストをサーバが処理してくれなくなる。
> * 高速なアップロードを行うクライアントと、低速な書き込みをしているサーバとの間に挟まったプロキシが、調整のためにデータを貯めているバッファが溢れる。

===
# プライオーリティ制御
---------------------

* ストリームの存在が念頭にある
* 複数のストリームの内どれを優先させるかを決める
  + CSSは描画に必要だから優先度高、画像は後で良いから優先度低など
* ブラウザが要求する時に指定出来るし、サーバが指定することも出来る

CF [HTTP2 のプライオリティ制御 - Qiita](http://qiita.com/Jxck_/items/16a5a9e9983e9ea1129f)
===
# サーバープッシュ
----------------

* サーバが自発的にコンテンツを送れる
  + 今までは必ずクライアントがリクエストを送らないとレスポンスが返せなかった
* サーバがコンテンツの内容を分かってるなら先にコンテンツを送ることが可能
  + 例えばHTMLを生成する前に静的コンテンツを送ればレンダリング完了までの時間が速くなる
* いわゆるPush通知が可能
  + 今まではCommetやWebsocketなどで対応していた

CF [Service WorkerとHTTP/2が切り開く新しいWeb Pushの世界](http://d.hatena.ne.jp/jovi0608/20141204/1417697480)

===
# Availability
--------------
## ブラウザ

* Firefox 34以降（現38）
* Google Chrome 31以降 (現42)
* Opera
* IE 11 on Windows10

===
# Availability
--------------
## Servers
[Implementations · http2/http2-spec Wiki](https://github.com/http2/http2-spec/wiki/Implementations)
より抜粋。

![available servers according to http2 wiki](/images/http2_availability.png)

===
# Availability
--------------
## Servers

* Nginxを始めとして多くのサーバがHTTP/2を実装している
  + H2OのようにHTTP/2を念頭に置いて書かれたものもある
* アプリケーションサーバはそんなに多くない
  + 多分Rackなどの統一サーバインターフェースの問題
===
# Availability
--------------
少くともこういうことをすれば利用出来る

```
+--------+             +---------+             +-----+
|        |-[HTTP/2]--->|         |             |     |
| Client |             | R.Proxy |-[HTTP/1.1]->| App |
|        |-[HTTP/1.1]->|         |             |     |
+--------+             +---------+             +-----+
```

===
# まとめ
--------

* HTTP/2について紹介した
* HTTP/2は現状の問題を解決する
  + サーバ/クライアント共に幸せになれる
* HTTP/2には段階的に移行出来る
  + 普及はかなり速いかもしれない
* みなさんが配属された時はHTTP/2の存在を前提としてアプリを設計しましょう

</textarea>
