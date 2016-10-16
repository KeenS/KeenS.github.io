---
categories: [Lisp, Common Lisp, 小ネタ]
date: 2015-03-05T13:18:44+09:00
description: "Clack Meet Up #1で使うかもしれないLT資料"
title: caveman2、ningle…Common LispのWeb周りのフレームワークを快適に使うためのたった1つのコト
---

<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# caveman2、ningle、datafly…Common LispのWeb周りのフレームワークを快適に使うためのたった1つのコト
----------------------
Clack Meet Up #1  
2015-03-05 @サムライト

<!-- .slide: class="center" -->
===
<span style="font-size:300%">ずっと俺のターン</span>

<!-- .slide: class="center" -->
===
# 問題
------
Common Lispは case

* Sensitive <!--.element: class="fragment grow" data-fragment-index="1" -->  
* Insensitive <!--.element: class="fragment shrink" data-fragment-index="1" -->

どっち?
===
# 問題
-------

あれ？

```lisp
(eql? 'CaseInsensitive 'caseinsensitive) ;=> t
```

===
# 問題
-------

リーダがデフォルトで大文字にするだけ
```lisp
(eql? '|CaseSensitive| '|casesensitive|) ;=> nil
```

===
# Caveman2
---------

```lisp
@route GET "/hello"
(defun say-hello (&key (|name| "Guest"))
  (format nil "Hello, ~A" |name|))
```
===
# Ningle
--------

```lisp
(setf (ningle:route *app* "/login" :method :POST)
      #'(lambda (params)
          (if (authorize (getf params :|username|)
                         (getf params :|password|))
            "Authorized!"
            "Failed...Try again.")))
```
===
# 面倒…
----
デフォルトでそのままだたっらいいのに
===
# 魔法の`readtable-case`

<!-- .slide: class="center" -->
===
# 魔法の`readtable-case`
-----------------------
```lisp
(setf (readtable-case *readtable*) :invert)
```
を使えばOK  
参考: [SBCLでclispとかallegroのmodern mode的なことをする - wasabizの日記](http://wasabiz.hatenablog.com/entry/20120929/1348889601)
===
# 例
----
```lisp
CL-USER> :username
:USERNAME
CL-USER> (setf (readtable-case *readtable*) :invert)
:invert
CL-USER> :username
:username
```
===
# まとめ
--------
* `(setf (readtable-case *readtable*) :invert)`を使うと快適だよ


</script>
</section>
