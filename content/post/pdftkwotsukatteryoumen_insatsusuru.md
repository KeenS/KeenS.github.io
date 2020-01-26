---
categories: [PDF, Utility, CLI, 備忘録, 小ネタ]
date: 2020-01-12T22:41:38+09:00
title: "pdftkを使って両面印刷する"
---

κeenです。
大きめ(130P程度)のPDFを印刷する用事があり、両面印刷したくなったのでそのときのメモ。

<!--more-->

高めのプリンタだと両面印刷の機能があることもありますし、プリンタドライバが優秀なら偶数ページのみ印刷や奇数ページのみ印刷のオプションがあって両面印刷できることもあります。今回はその両方ともが使えなかったときのお話。

方針としては奇数ページを印刷、偶数ページを印刷の順番に行えればどうにか両面にできるのでそうします。つまり、奇数ページのみを集めたPDFファイル、偶数ページのみを集めたPDFファイルを作ります。

まずは[pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)でページを1枚1枚バラします。pdftk自体は `apt install pdftk` とかでインストールしておいて下さい。

```console
$ pdftk file.pdf burst
```


するとカレントディレクトリに `pg_0001.pdf` 、 `pg_0002.pdf` 、 …とページ数分のPDFファイルが展開されます。これを奇数ページだけ、偶数ページだけ拾っていきます。

```console
$ pdftk $(ls pg_* | sort | xargs -n2 echo | cut -d\  -f1 ) cat output odd.pdf
$ pdftk $(ls pg_* | sort | xargs -n2 echo | cut -d\  -f2 ) cat output even.pdf
```

これで作業終わりです。

あとは

1. `odd.pdf` を印刷
2. 紙束の表裏を返す（紙束を左右に180°回す）
3. `even.pdf` を印刷

の流れで両面に印刷できます。印刷はEvinceかFirefoxで適当にやって下さい。
普通のプリンタならページ番号が若い方が下になるように印刷されていくので、奇数ページを印刷したあとにそのまま左右に返してセットしなおせば1pの裏に2p、3pの裏に4pが印刷されます。製本っぽいことしたいなら端に水糊を塗ってティッシュペーパをくっつけるとそれっぽくまとまるのであとはガムテープとかで保護してあげて下さい。

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">製本している <a href="https://t.co/D6ZL36eiwX">pic.twitter.com/D6ZL36eiwX</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1216359933198749700?ref_src=twsrc%5Etfw">January 12, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

pdftkにこういう($k \mod N$ ページ毎にファイルに振り分ける)機能あってもいいと思うんですけどねー。

という訳で小ネタでした
