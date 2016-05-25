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

all is object,
class is mere an (concrete) object type,
trait is mere an (abstract) object type,
package is mere an object.

path dependent type provides name space facility
path dependent type provides generics

subtyping constraint solver
implicit search -> contravariant implicit
nested value classes, arrays
literal singleton -> `final val i = 1`
phase fusion
non-blocking lazy

current DOT:
have union types -> useful to express nullable type
                    (maybe) appeard in infered types

>DOT  has  no  principal  types  and  no  global
>Hindley-Milner  style  type  inference  procedure.  But  as  in  Scala,
>local type inference based on subtype constraint solving [37, 39]
>is  possible,  and  in  fact  easier  than  in  Scala  due  to  the  existence
>of universal greatest lower bounds and least upper bounds through
>intersection and union types. 

from [^2]


scala is slow because:
type inference
+15 compile passes to java
generates many classes

from [^5]

[^1]: [The Essence of Dependent Object Types](https://infoscience.epfl.ch/record/215280/files/paper_1.pdf)
[^2]: [From F to DOT: Type Soundness Proofs with Definitional Interpreters](http://arxiv.org/pdf/1510.05216v2.pdf)
[^3]: [Dependent Object Types](http://www.cs.uwm.edu/~boyland/fool2012/papers/fool2012_submission_3.pdf)
[^4]: [Why is the Scala compiler so slow? - Quora](https://www.quora.com/Why-is-the-Scala-compiler-so-slow)
[^5]: [performance - Java compile speed vs Scala compile speed - Stack Overflow](http://stackoverflow.com/questions/3490383/java-compile-speed-vs-scala-compile-speed/3612212#3612212)
[^6]: [GHC doesn't do subtyping. I suspect that is the main reason why Scala is slow - ... | Hacker News](https://news.ycombinator.com/item?id=5008761)

</script>
</section>
