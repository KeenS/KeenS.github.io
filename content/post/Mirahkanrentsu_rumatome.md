---
categories: [Mirha, 開発環境]
date: 2016-04-04T01:16:31+09:00
title: Mirah関連ツールまとめ
---
κeenです。寝付けないのでMirah関連ツールについてまとめようかと。
とはいってもそんなにないのですが。Mirahについて知らない人は適当にググって下さい。

<!--more-->
# コンパイラ
[Releases · mirah/mirah](https://github.com/mirah/mirah/releases)

本家のものを使うべし
# 開発支援
## NetBeansプラグイン
[shannah/mirah-nbm: Netbeans Module for Mirah](https://github.com/shannah/mirah-nbm)

このプラグインのためにパーサのAPIに変更を加える議論がされたりしていて恐らく現状一番まともに動くやつと思う。

## Emacs
[KeenS/mirah-mode.el:](https://github.com/KeenS/mirah-mode.el)

ruby-modeにやっつけで雑に機能を追加したやつ。コメントのハイライトにコーナーケース(`/*/*/**/*/*/` みたいなの)があったり雑。
まあ、とりあえずそれっぽくは動く

# ビルドツール統合
## Gradle
[ysb33r/gradle-mirah-plugin: Gradleplugin to compile Mirah source code](https://github.com/ysb33r/gradle-mirah-plugin)

最近作られたやつ。[軽く使てみた](https://github.com/KeenS/mirah_sample_gradle)ら簡単に動いたし良さげ。

## Maven
[mirah/maven-mirah-plugin: Plugin to compile Mirah source with Maven](https://github.com/mirah/maven-mirah-plugin)

公式サポートだし良さげ。

# その他
## コードトランスレート
[captn3m0/java2mirah: A java to mirah transpiler](https://github.com/captn3m0/java2mirah)

javaからMirahに変換してくれるツール。試したことはないけどそんなに期待はしてない。


-----------------

私が把握してるのはこれくらい。ビルドツール統合と開発支援があればどうにかなるっしょ。
