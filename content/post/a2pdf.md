---
categories: [Command Line, PDF, 小ネタ]
date: 2015-01-24T10:23:00Z
title: 印刷フォマット済みのテキストファイルをPDF化する
---
小ネタ。古いドキュメントやらポータブルな仕様を落としてくると中身が印刷フォーマット済み(ページネーションやヘッダ、フッタをアスキーアートで表現してる)であることがあります。
それらを普通のテキスト形式だと読みづらいのでPDF化する話。
<!--more-->
結論から言うと[a2pdf - search.cpan.org](http://search.cpan.org/~jonallen/a2pdf-1.11/lib/Script/a2pdf.pm)を使いました。選んだ理由は

1. 余計な装飾を付与しない
2. `^L`(pagebreak)でちゃんと改ページする

です。使い方は

    $ a2pdf --noheader --noperl-syntax --noline-numbers  foo.txt -o foo.pdf

でok。本当は`--nofooter`も付けたいんですがそれつけるとエラー出たのでやむなくフッタは甘受。元々perlのソースコードをPDF化するためのものらしく、デフォルトでそのような設定になってるので無効化する必要があります。

生成速度なんですけどPDF化すると105ページあるテキストの変換に73秒かかりました。少し遅いですね。このくらいだったらサクっとCommon Lispで書いてしまった方が速かったのかなぁ。
