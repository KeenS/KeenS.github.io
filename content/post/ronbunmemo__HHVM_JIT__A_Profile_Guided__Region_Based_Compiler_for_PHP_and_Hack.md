---
categories: ["論文メモ"]
date: 2019-02-16T21:45:31+09:00
title: "論文メモ: HHVM JIT: A Profile-Guided, Region-Based Compiler for PHP and Hack"
---

G. Ottoni. "HHVM JIT: A Profile-Guided, Region-Based Compiler for PHP and Hack," PLDI 2018 を読んだメモ。

<!--more-->

PLDIのaccepted paper。FacebookよりHHVMの第2世代JITエンジンで使われている手法の紹介。
Method JITでもTracing JITでもなくRegionベースでJITする話。

HHVMはHackとPHPを動かすが、Hackの型情報は捨ててしまう。Hackの型システムはunsoundらしい。世知辛い。

バイトコードインタプリタとJITでOSRして相互に実行を交代できる「よくある」JITエンジン。

HHVMのJITでは以下の4つをしている。

* type specialization
* profile-guided optimizations
* side exits
* region-based compilation

このうちside exitsとregion-based compilationが私は初見だった。

side exitsはコンパイルされたコードの途中で抜けるやつ。インタプリタに戻ったり、さらに最適化されたコードに跳んだりする。
TraceMonkeyとかHotSpotとかで使われているらしい。

region-based compilationはタイトルにもなっているように設計上重要な部分。JITコンパイルするときの粒度。
メソッド単位、基本ブロック単位、トレース単位など色々にある。
リージョンベースコンパイルについては以下を参照とのこと Richard E Hank, Wen-Mei W Hwu, and B Ramakrishna Rau. 1995. Region-based compilation: An introduction and motivation. In Pro- ceedings ofthe International Symposium on Microarchitecture. 158–168 。リージョンがなんなのか具体的な説明がないまま進んでいく…。
恐らく任意の粒度のバイトコードかな？例えば 「`a` の型が `int` と分かっている区間」のような単位でバイトコードを切り取っていそう。

Javaなどの静的な言語に比べて動的な言語ではよりリージョンの恩恵が大きいらしい。

面白いのが最適化のところ。

* 1つのトリガで全部のコンパイルをするらしい。Whole Program Optimizaionが可能になる。
* 関数をよく呼び出す組同士で並べるとTLBミスが減ってパフォーマンスが改善するらしい
* Huge Pageによく呼ばれるコードを置くとやはりI-TLBミスが減って速くなるらしい。
* code bloatを防ぐために前提条件ゆるくしてコンパイルする

自分にとって目新しい部分だけ抜き出したので気になった人は論文読んで。
