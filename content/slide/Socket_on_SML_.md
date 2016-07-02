---
categories: [socket, SML, smlsharp]
date: 2016-07-02T15:25:24+09:00
description: "
ML勉強会での発表用
"
draft: true
title: Socket on SML#
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# Socket on SML#
----------------------

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバーエージェントのエンジニア
 + Lisp, ML, Rust, Shell Scriptあたりを書きます



SML#にJSONサポートが入った
-> REST API叩けるのでは？

SML#にFully Concurrent GCが入った
-> HTTPサーバ書くとレイテンシ抑えられて嬉しいのでは？
   -> スレッドも使えてスループットも増

HTTPスタックを書きたい
-> SML#はサポートしてない
-> Basisにはある
-> FFIあるしユーザランドで実装するか

ソケット、アドレスに対する説明入れる

Socket, NetHostDB, Unix, INet

NetHostDB
-> Cの古い(現在では非推奨な)APIに合わせた設計。新しいAPIで実装するのが面倒

Socket
Socket Address Family (Unix Domain, INet, INet6)
Socket SOCK (Stream, Diagram)

幽霊型 af と sock type


bind, listen, accept
      connect
と幽霊型

send、sendTO、slice

非同期IO

select/NB
ioのpoll

つらい話
定数、マクロのマッピング
Cならスタック変数で済むものをSML#に渡すためにmallocするの嫌
-> import関数
sockaddrのメモリーリーク
-> SML#側でどうにかしてもらわないとダメ？
-> パンドラの壷(sml_alloc)

</script>
</section>
