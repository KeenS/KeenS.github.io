---
type: slide
title: "Get Clojure Faster"
date: 2014-10-28
aliases:
    - /slide/get-clojure-faster.html
categories: [Lisp, Clojure, JVM, 開発環境
]
draft: true
description: "Lisp Meet Up #21で発表するかもしれない
Clojureを外部から速くする話。
"
---
<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">

# Get Clojure Faster
--------------------
2014-10-28  
κeen(@blackenedgold)



# About Me
----------

 + κeen![κeenのアイコン](/images/icon.png)
 + 東大数学科の4年生
 + ソーシャルアカウントは上のアイコン達から。
 + Lisp, Ruby, OCaml, Shell Scriptあたりを書きます


# `lein repl`
--------------------

```
$ time  lein repl </dev/null
...
snip
...
lein repl < /dev/null  23.05s user 0.97s system 159% cpu 15.048 total
```

起動に20秒…


# drip
------

* JVMの起動時間を速くすくやつ
* 類似プロジェクトにCakeやNailgun
* でもCakeやNailgunの踏んだ地雷は避ける
  + JVMサーバを使い回すのではなく起動イメージを準備
  + Common Lispでのコアダンプに近い？


# drip
------
## 導入

```
curl -L http://drip.flatland.org > ~/bin/drip
chmod 755 ~/bin/drip
```
又は

```
git clone https://github.com/flatland/drip.git
cd drip && make prefix=~/bin install
```
そして
```
export LEIN_JAVA_CMD=drip
```

# drip
------
## 結果

```
$ time  lein repl </dev/null
lein repl < /dev/null  0.11s user 0.27s system 6% cpu 6.153 total
```



<span style="font-size:600%">以上</span>  
何か質問あればどうぞ
</script>
</section>
