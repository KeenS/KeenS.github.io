---
categories: [socket, SML, smlsharp]
date: 2016-07-02T15:25:24+09:00
description: "
ML勉強会での発表用
"
title: Socket on SML#
---

<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# Socket on SML&#x23;
----------------------
[ML勉強会](http://connpass.com/event/32752/) 2016-07-09

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバーエージェントのエンジニア
 + Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# HTTP on SML&#x23;?
-------------

* SML#にJSONサポートが入った
  + REST API簡単に叩けるのでは？
* SML#にFully Concurrent GCが入った
  + HTTPサーバ書くとレイテンシ抑えられて嬉しいのでは？
  * スレッドも使えるしスループットも高い筈
* SML#でHTTPの機運
* しかしHTTPサポートはないので自前実装するしかない

===
# Socket on SML&#x23;?
------------------

* HTTPサポートに必要
* Basisにはある (Optional)
  + SML#のBasisはサポートしてない
* FFIあるしユーザランドで実装するか
* [KeenS/SmlSharpSocketSupport](https://github.com/KeenS/SmlSharpSocketSupport)

===
# TCP/IP、ソケットなど
---------------------------

* ソケットアドレス - 自分、相手の居場所。これがないと通信出来ない
  + 複数のアドレスファミリ(AF)がある
  + IP v4, IP v6, UNIXドメインなど。
* ソケット - 通信の仕組み。プログラムからはfdとして見える。
  + AFやtype(stream/diagram)の属性を持つ
* INet - インターネットドメインソケット
  + TCPとUDP
  + IP v4/v6アドレスで通信

===
# 主要なstructure
-----------------

* Socket
* NetHostDB
* INetSock
* (UnixSock)

===
# [NetHostDB](http://sml-family.org/Basis/net-host-db.html#SIG:NET_HOST_DB)
-----------

* ドメイン名からIPアドレスを調べたり逆をやったり
* Cの古い(現在では非推奨な)APIに合わせた設計
  + IPがv4/v6が混じることを考慮してない
  + 非推奨どころか脆弱性もあったり CF [glibc の脆弱性 CVE-2015-0235（通称：GHOST）についてまとめてみた - piyolog](http://d.hatena.ne.jp/Kango/20150128/1422409960)
* 新しいAPIで実装するのが面倒

===
# [Socket](http://sml-family.org/Basis/socket.html#SIG:SOCKET.accept:VAL:SPEC)
---------

* まあまあでかいけどsend/receive関数の変種が一杯あるだけ
* ドメインに依らないソケット操作
* typeには依る
  + stream socketとdgramソケットで送受信関数が別

===
# Socket.AF
------------

* ソケットのアドレスファミリ関連
* 仕様では具体的なファミリを定義しない

===
# Socket.SOCK
-------------

* ソケットにも種類がある
  + stream/diagram
* さらにstreamにはactive/passiveがある
* それらの型。

===
# [INetSock](http://sml-family.org/Basis/inet-sock.html#INetSock:STR:SPEC)
--------------

* IP - TCP/UDPのソケットを作る/操作するやつ。
* ここの関数はインターネットドメインソケットしか受け付けない

===
# Socketについて整理
----------------

* sockにはtypeとafの属性がある
* afは色々ある
* typeはstreamとdgramがある
* streamにはさらにactiveとpassiveがある
* 特定のaf, 特定のtypeしか受け付けない関数がある

===
# Socketについて整理
----------------

```
   (AF_UN)
        v
     [unix sock]...
         |                       (Active)
[socket]-+           [TCP(stream)]-+
         |             |         (Passive)
     [inet/inet6 sock]-+
        ^              |
   (AF_INET/INET6)   [UDP(dgram)]
```

===
<span style="font-size:500%;">幽霊型</sapn>

<!-- .slide: class="center" -->

===
# Socketの幽霊型
---------------

```sml
type ('af,'sock_type) sock
type 'af sock_addr
type dgram
type 'mode stream
type passive
type active
```
===
# サーバのSocket操作(stream)
---------------------------

* INetSock(UnixSock)でソケットを作る(af, sock typeの選択, プロセス側の準備)
* bindでソケットをsock_addrにバインドする(アドレスの割り当て)
* listenでソケットの特定のポートを開ける(TCPの開始)
  + passive streamしか受け付けない
* acceptでクライアントからの入力を受け付ける(接続)
  + passive streamしか受け付けない
  + passive streamがactive streamになる
  + 一度acceptしたsockはlisten/accept出来ない
===
# データの送受信
---------------

* `send` / `recv`
* それぞれArraySlice/VectorSliceの制御フラグ有り/無しがある
  + 実装がまあまあ面倒

===
# SML# でのバインディング
------------------------

* sock - fd = int
* sock_addr - AFによってサイズが違う。ヤバい。
 + 任意のsock_addrを格納出来るsockaddr_storageを使う
 + Solarisだとsockaddr_unを格納出来ないらしい（任意のaddrを格納出来るとは）
   - SML#はSolarisでは動かないので問題ない。
* AF_* とか - Cではただのint。SML#は関数しかインポート出来ない。
 + C側で定数関数でラップしてSML#で呼び出した値を束縛
* その他 - straitforward

===
# 非同期IO

<!-- .slide: class="center" -->

===
# 非同期IO
----------

* HTTPサーバは複数のクライアントとのコネクションを持つ
* それらのコネクションを要領良く扱わないといけない
* 相手の処理速度や通信速度によって即座にデータを読み書き出来ない時がある
  + 勿論クライアント毎に状況が異なる
* 読み書き出来ない時にやったらブロック(CPU時間の無駄遣い)する
* 「読み書き出来るならする」/「読み書き出来るクライアントを選ぶ」APIが必要

===
# 非同期API
----------

* select - 複数のクライアントを登録して、読み書き出来るやつを選ぶ
* \*NB - 読み書き出来るならやって、出来ないならブロックせずにリターンする
* poll - selectと同じような（ちょっと速い）API。SocketではなくIOにある。
  + pollがあるのに何故Socketにselectがあるの…。

===
===
# 非同期APIのFFI
----------------

* select - Cに対応する関数が。構造体の変換でメモリアロケーションが起きて遅い
* \*NB - O_NONBLOCKを付けたsend/recv。
* poll - SML#にある

===
# 非同期HTTPサーバ概略
---------------------

```sml
bind(sock, addr);
listen(sock, port);
create_thread(n, fn i => let
  val clientList = makeClientList()
  fun loop () = let
    val clientList =
      Option.map (acceptNB(sock))
      (fn client => addReadClient clientList client)
      handle SysErr => ...
    val {rds, wds, exs} = select (makeSelectList clientList)
  in
    recvAndParseHTTPThenCallHandler clientList rds;
    sendResponse clientList wds;
    loop ()
  end
end)
```

===
# Cバインディングの憂鬱

<!-- .slide: class="center" -->
===
# importとメモリ割り当て
-----------------------

* SML#にはポインタかワードサイズ以下の値しか渡せない
  + stringとかはポインタからインポートする関数がある
  + インポートしたものはSML#のヒープに **コピーされる**
* cでポインタを返すにはmallocが必要
* **すぐコピーされてfreeされるもののためにmalloc??**
* SML#からコールバックを渡してCのスタックの変数をインポート
* CF https://github.com/KeenS/SmlSharpSocketSupport/blob/master/lib/net_host_db.c#L74

===
# メモリ管理
------------

* SML#にインポート出来る型は限られている
  + array, string
* 他の型はポインタのまま扱う。
* ポインタはGCされない
* `sockaddr` は仕様的にファイナライザを持っていない
  + 現状 **メモリリークする**
* SML#側でどうにかしてもらわないとダメ？
  + パンドラの壷(sml_alloc)使う？

===
# 型隠蔽
-------

* sockは本来はioDescに変換出来ないといけない
 + SML#でioDescはsockと同じくint
* しかし型隠蔽のせいでintをioDescに変換出来ない
* 手詰まり

===
# まとめ
--------

* socketとかその辺を解説したよ
* SML#向けにSocket関連Basisのバインディング作ってるよ
* BasisのAPI使うと非同期HTTPサーバ作れるよ
* SML#のFFIはやっぱりつらいよ

</script>
</section>
