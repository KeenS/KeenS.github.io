---
categories: [継続, 限定継続, Lisp, Common Lisp]
date: 2016-05-08T21:26:32+09:00
description: "継続勉強会向け"
draft: true
title: 高速な継続ライブラリに向けて
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# 高速な継続ライブラリに向けて
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

継続欲しい
  非同期プログラミング
  ゲームのコルーチン
  モナド記法
CPS変換
スペシャルフォーム25+funcall
関数定義とかもマクロだよって話
そのマクロを解くマクロ展開器もあるよって話
cl-cont
遅い
lambda多い。憎い。lambda禁止おじさんもびっくり
cl-fast-cont
  →完成させたい…
SSAコンパイラとCPSコンパイラ
SSAからの継続じゃだめなの？
 →gotoを使って自分でコールスタック組み立てる必要があった
Selective CPS
2pass transformation
※詳しい図解
load-time-value, catch, block, tagbody, special variableつらい
関数定義と引数の話
計測


</script>
</section>
