---
categories: ["Rust", "GitLab"]
date: 2019-02-03T22:31:10+09:00
title: "GitLab CIでRustのCIとクロスコンパイル: お手軽編"
---
κeenです。ちょっとづつGitLabを使っていこうとしてます。
RustでCIとクロスビルドができたので紹介します。
<!--more-->

# GitLab CIとは
GitLabはGitHubと違って[^1]開発からデプロイまでの面倒をみるのでCIも付属しています。
外部サービスでない分コンフィグファイル置くだけで走り始めたり、CIの成果物をそのままバイナリリリースに置けたりします。

また、GitLabは利用形態にサービスとオンプレがありますが私は今回サービスの方で試しているのでオンプレで使いたい方は自分に合わせて読んでください。

[^1]: 最近だとGitHub Actionsもありますが…。

# 方針

* 想定ケースとしてはRustで書かれたCLIツールのCI
  + プラットホーム依存のコードはない想定
* ひと目で何してるか分かる範囲で頑張る
* 1ファイルで済ませる
* 素性のよく分からないDockerイメージとかは使わない

# コード

[これ](https://gitlab.com/blackenedgold/rust-simple-cross-build-gitlabci/blob/master/.gitlab-ci.yml)。バイナリリリースの例は[ここ](https://gitlab.com/blackenedgold/rust-simple-cross-build-gitlabci/releases)。適当にコピペしてお使い下さい。

残念ながらmacOS向けには簡単はできそうになかったのであきらめました。SaaSでもmacOSのrunner提供してくれないかなー。
一応Linuxからのクロスコンパイルは[成功例](https://mzp.hatenablog.com/entry/2017/05/02/122348)があるものの、よくわからないdockerイメージを使っているのと、ベースに使われているdockerプロジェクトが更新されていないようなので避けることにしました。手軽に出来る方法がありましたら情報、マージリクエストともにお待ちしております。


Windowsはmingwのみビルドしています。[クロスビルドのガイド](https://forge.rust-lang.org/cross-compilation/to-windows.html)を見るにMSVCは手軽にはビルドできなそうです。
それにバイナリを提供するという目的ではどちらか1つがあれば十分でしょう。

# 次回予告
GitHub + Travis CIなら[trust](https://github.com/japaric/trust)というおばけのようなプロジェクトがあります。
さまざまな環境でビルド/テストできるように整えられています。

それをGitLab CIに[移植しようとしている](https://gitlab.com/blackenedgold/trust-gitlab)のですが内部で使われている[ツールの問題](https://github.com/rust-embedded/cross/issues/52)で詰まってしまいました。
問題が解決できたらまた紹介記事を書こうと思います。

# ノート
GitLab 11.7からReleaseが[リッチになりました](https://about.gitlab.com/2019/01/22/gitlab-11-7-released/)。
しかしまだAPIレベルでのサポートで、CI/CDからスムーズに登録できるよう鋭意開発中とのことです。
今はリリースノートにリンクを手で書いてますがいずれ自動化されると思われます。
