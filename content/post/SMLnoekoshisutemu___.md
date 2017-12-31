---
categories: [ML, SML, Advent Calendar, Advent Calendar 2017]
date: 2017-12-24T21:18:14+09:00
title: "SMLのエコシステム（？）"
---

κeenです。
どこかで紹介したつもりでしたが意外に書いてなかったのでSMLのツールとかそういうのを紹介します。

<!--more-->

# 仕様
通称SML '97の[The Definition of Standard ML Revised](https://dl.acm.org/citation.cfm?id=549659)が現行の最新仕様です。最終更新は通称どおり1997年ですね。
[本](https://mitpress.mit.edu/books/definition-standard-ml)もありますが[GitHub版](https://github.com/SMLFamily/The-Definition-of-Standard-ML-Revised)の方がアクセスが良いでしょう。

# ポータル
[SML Family](http://sml-family.org/)というところがあります。最終更新2016-04-04。大体の欲しい情報はここに書いてます。

# コンパイラ
上記SML Familyに列挙されてます。一応口コミを書いておくと、以下が有名どころでしょうか

MLton:
ソースはちょくちょく更新されてますが最終リリースは2013年。
実行速度が速い、拡張が多い、ランタイム表現も素敵と三拍子揃ったコンパイラですがwhole-program optimizationするためコンパイルはかなり遅いです。
Hello Worldするにも標準ライブラリごとコンパイルするので数秒かかります。

SML/NJ:
最終リリースは2017-10-16。
MLtonには劣りますがそれなりに速く拡張が（おそらく一番）豊富な処理系です。
ただしx86しかサポートしてないのとインタラクティブシステムがメインなので最近だとちょっと使いづらいかもです。

Poly/ML:
最終リリースは2917-11-27。
上記２つと比べて尖ったところはないですがピーキーなところもなく平均点を取るタイプのコンパイラです。
[Isabelle](http://www.polyml.org/)はこれを使っています。


Moscow ML:
最終リリースは2014-08-28。
Caml lightのフロントエンドをSMLに仕立てたコンパイラです。OCaml並みに使いやすいんじゃないですかね。

SML#:
最終リリースは2017-08-31。
東北大で開発されているコンパイラです。実用的な処理系を目指して作られています。PolyやMoscowと何が違うのと言われると苦しいが個人的によく使います。
たまに謎拡張が入ります。

# エディタ
EmacsとかVimとか古くからあるものはシンタックスハイライトとインデンティングくらいはサポートしてると思います。私はEmacsを使ってます。

# ドキュメント
標準ライブラリ扱いの[Basisのドキュメント](http://sml-family.org/Basis/index.html)があります。

# パッケージマネージャ
[smackage](https://github.com/standardml/smackage)というのがあります。最終更新 2016-03-07。
登録数は…21。正直smackageに頼るよりGitHub漁ったほうが良いです。

# ビルドツール

[smbt](https://github.com/finrod/smbt)というのがあります。最終更新 2015-09-16。
一応処理系に依存せずにビルドを宣言的に書けるはずですがあまり期待せずに各処理系推奨のビルド方法を使ったほうが無難。
昔[サンプルを書いた](https://github.com/KeenS/hello_smbt)ことがあるので適当に参考にして下さい。

# テスト
SML#付属の[SMLUnit](https://github.com/smlsharp/SMLUnit)がよさそう？

# Lint
[SML-Lint](https://github.com/nrnrnr/SML-Lint)というのがあります。最終更新2013-03-24。

# 未来
流石に仕様が1997年から変わらないのはきついので有志で仕様の拡張をしようと言うプロジェクトがあります。
その名も[Successor ML](https://github.com/SMLFamily/Successor-ML)。
最終更新2017-07-26。
細かなシンタックスシュガーの追加やプレリュードの追加などが入っているようです。

同じく[Basisを拡張しようとする動き](https://github.com/SMLFamily/BasisLibrary)もあります。最終更新 2016-08-16。
これは今見つけたので追えてませんが`Either`が入ってたりするよう。

# 終わりに
SMLのエコシステムらしいものはありませんでしたね。
自分のブログざっと漁った感じ過去にこういうの書いてないらしいんですが本当ですかね？すごい既視感が…
