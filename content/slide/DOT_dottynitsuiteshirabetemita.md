---
categories: [Scala, 言語処理系, 社内, Scala Meet Up]
date: 2016-05-24T23:40:36+09:00
description: "Scala Meet Upでの発表用"
title: DOT/dottyについて調べてみた
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# DOT/dottyについて調べてみた
----------------------

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 基盤開発グループ
 + Lisp, ML, Rust, Shell Scriptあたりを書きます



System D<: -> System F<:
path dependent types
DOT extends System D<:
don't have generics (type members Suffice)
better explanation of existential types
structual subtyping rather than nominal subtyping
have intersection types rather than inheritance
(recursion)

subtyping constraint solver
implicit search
value classes

current DOT:
have union types -> useful to express nullable type


</script>
</section>
