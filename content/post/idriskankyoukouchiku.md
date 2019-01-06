---
categories: ["Idris"]
date: 2019-01-06T21:53:12+09:00
title: "idris環境構築"
---

κeenです。最近にわかにIdrisが話題になってるようですね。
私の知ってる範囲でIdrisの環境構築について書こうと思います。

<!--more-->

多分話題になったのは[Misreading Chat](https://misreading.chat/2018/12/25/episode-41-idris-systems-programming-meets-full-dependent-types/)が震源地でかね。私はまだ聞いてないです。

Idrisは依存型のある汎用プログラミング言語で、Haskellっぽい雰囲気の構文を持ちます。
バックエンドはいくつかありますが、デフォルトではCが選ばれるのでおおむねネイティブコンパイルされると思ってよさそうです。他にはJavaScriptなどもあります。
Idrisそのものについては[こちらのブログ](http://wkwkes.hatenablog.com/entry/2016/12/17/000000)を案内しておきます。
私からはインストールとかその辺を書きます。

# インストール

macOSではbrewがあるようですがLinuxだとバイナリがありません。
cabalでインストールすると失敗しがちなので[stack](https://docs.haskellstack.org/en/stable/README/)でインストールしましょう。
だいたい以下のような感じで入ると思います。

``` console
$ git clone https://github.com/idris-lang/Idris-dev.git
$ cd Idris-dev
$ git checkout v1.3.1
$ stack build
$ stack install
```

# Emacsの設定
[idris-mode](https://github.com/idris-hackers/idris-mode)があります。
処理系と対話しながらサポートしてくれるので中々便利です。
プログラムを書くのか証明を書くのかで結構変わりますが、 `C-c C-s` 、 `C-c C-c` 、 `C-c C-a` 、 `C-c C-l` あたりはよく使います。後述のipkgファイルもちゃんと認識してくれます。

[Vimのサポート](https://github.com/idris-hackers/idris-vim)と[atomのサポート](https://github.com/idris-hackers/atom-language-idris)もあるようなのでそちらもどうぞ。

# ビルド
原始的には1ファイル単位でコンパイルすればいいのですが[ビルドシステム](https://www.idris-lang.org/documentation/packages/)も一応あります。
ipkgファイルの書き方はあまりドキュメントがかかれてないので[この辺](https://github.com/idris-hackers/idris-crypto/blob/master/crypto.ipkg)とか[この辺](https://github.com/idris-hackers/software-foundations/blob/develop/software_foundations.ipkg)見ながら書くとよいでしょう。


# パッケージマネージャ
ない。お前がパッケージマネージャになるんだよ。

使いたいライブラリはダウンロードしてきて `idris --install hoge.ipkg` するとインストールされます。使うときにipkgファイルで `opts = "-p hoge"` とかするとリンクしてくれるので使えます。
使いたいライブラリは[Wiki](https://github.com/idris-lang/Idris-dev/wiki/Libraries)とかから探してきましょう。

新興の [elba](https://github.com/elba/elba) というパッケージマネージャ/ビルドツールがありますがまだ未熟なところがあり使いづらいようです(0.2.0時点)。
具体的には既存の `ipkg` と互換性がない点と、登録されているパッケージを検索するインターフェースがないので何が登録されているか分からない点です。


# その他

Idrisのメイン開発者が[Idris 2](https://github.com/edwinb/Blodwen)を開発しているらしい。
