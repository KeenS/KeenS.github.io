---
type: post
title: "PDFユーティリティ"
date: 2013-09-03T23:34:00Z
comments: true
sharing: true
categories: [PDF, Utility, CLI, 備忘録]
---
小ネタ。コマンドラインからPDFを編集した話の備忘録。

<!--more-->

[こちら](http://www.seeds-std.co.jp/seedsblog/181.html)に紹介されてるように、pdftkというツールである程度のことはできる。

    $sudo apt-get install pdftk

でインストール可能。  
今回は

    $for pdf in *.pdf
    >>do
    >> pdftk $pdf cat 1-endR output `basename $pdf .pdf`_rot.pdf
    >>done

で全てのPDFの全てのページを右周りに回転し、名前に\_rotを付けて保存した。

    pdftk *.pdf cat output all.pdf

とかで結合なんかもできて便利。


