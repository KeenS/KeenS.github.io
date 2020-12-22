---
categories: [Idris, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-22T23:33:53+09:00
title: "Idris面白機能：文芸的Idris"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の21日目の記事です。
前はmock_beginnerさんで[Idrisとはじめる型駆動開発](https://kengo-yamashita.hatenablog.com/entry/2020/12/20/Idrisとはじめる型駆動開発)でした。

κeenです。大ネタが続いたのでそろそろ小ネタでも。文芸的プログラミングの機能です。

<!--more-->

[文芸的プログラミング](https://ja.wikipedia.org/wiki/%E6%96%87%E8%8A%B8%E7%9A%84%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0)はドナルド・クヌースの提唱したプログラミングスタイルです。
普通のプログラムがプログラムの中にコメントとして文章を埋め込むのに対して文芸的プログラミングでは文章の中にプログラムを埋め込みます。
さらに加えてプログラムは断片に分解でき、ラベルの参照を使って自由に組み合わせることができます。

断片の参照の機能はありませんがIdrisには多少の文芸的プログラミングのサポートがあります。

````literate-idris
文芸的Idrisでは普通の文章の中にIdrisのコードを埋め込みます。

`>` ではじまる行がIdrisのコードとして扱われます。

例：Idrisで2数の和をとるコード

> add : Integer -> Integer -> Integer
> add x y = x + y

`main` なども書けます。

> main : IO ()
> main = printLn $ add 1 2

拡張子は `.lidr` を使います。
このファイルを `LiterateIdris.lidr` に保存したとしましょう。
以下のように普通にコンパイル・実行できます。


```text
$ idris -o LiterateIdris LiterateIdris.lidr
$ ./LiterateIdris
3
```

プログラムの中に自信のコンパイル方法を書いたりできるの楽しいですよね。

これをどう使うかというと、例えば証明をするときなんかには証明の流れや文章での証明の方針の補助などが必要なので向いてますね。

````


私の使っているハイライトエンジンにliterate idrisのサポートがなかったので残念なことになってますが、エディタなどはちゃんと対応しているようです。

以上小ネタでした。
