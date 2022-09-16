---
categories: [Scala, Scala Meet Up, テスト, 社内]
date: 2016-04-12T22:31:51+09:00
description: "社内勉強会での発表用。
社内だとSpecs2を使ってるプロダクトも多いのでScalaTestの紹介からテスト手法、Gatlingの改造なんかについて
"
title: テストについて、Scalaと。
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# テストについて、Scalaと。
----------------------
サイバーエージェント アドテクスタジオ  
ScalaMeetUp テスト回

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + サイバーエージェントのエンジニア
   + 基盤開発グループでScalaで基盤開発してます
 + Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# Scalaのテスト
---------------

* ScalaTest
  + 機能豊富
  + 複数のテストスタイルを選べる
  + 他のフレームワークとの統合がある
* Specs2
  + 普通に使いやすい
* 他にもScalaCheck、JUnit、Gatlingなどなど

===
# ScalaTestのスタイルたち
-----------

* FunSuite
  + xUnitっぽいらしい
* FlatSpec
  + xUnitからBDDに移行した人向けらしい
* FunSpec
  + RSpecっぽいBDD用の

===
# ScalaTestのスタイルたち
-----------

* WordSpec
  + Specs2に似てる。
* FreeSpec
  + 何段にでもネスト出来る。自由。
* Spec
  + テスト=メソッド。速いらしい。

===
# ScalaTestのスタイルたち
-----------
* PropSpec
  + プロパティベースのテスト。ScalaCheckとの統合。
* FeatureSpec
  + シナリオテスト。
===
# その他の機能
-------------

* mockitoのシュガー
* Matcher DSL
* 複数のランナー（複数のツールとの統合）

===
# Axionのテスト
コード紹介を多めに
<!-- .slide: class="center" -->
===
# 単体テスト
---------------

* 普通の単体テストはWordSpec
* ホーアの3つ組に基いて事前条件、コマンド、事後条件(不変条件)に分ける
  + 「{事前条件}が成り立つ時{コマンド}を実行し、停止するなら{事後条件(不変条件)}が成り立つ」の列挙
* [テストについて考えてみた | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2016/03/01/tesutonitsuitekangaetemita/)
* コマンドの実行と事後条件への表明を明確に分離
* テストの分け方に試行錯誤

===
# 事前条件
----------

* 基本的にはmockitoをフルに使う
* テストは並行に走るのでテストケース毎にモックを準備
* コンテキストの共有は基本的にはしない
  + 情報の局所性を上げて何をテストするかを理解しやすくする
  + 逆にノイズが乗ってわかりにくい？
* メソッドの実行に必要な引数と環境全てを構築する

===
# コマンドの実行
---------------

* 例外が起きないことだけ表明する
* 結果はvarに保存して外に持ち出す。

===
# 事後条件
----------

* コマンドを実行した結果に対する表明
* 環境に対する表明
  + 環境が変化した/しなかった
  + 依存オブジェクトのメソッドが呼ばれた/なかった

===
メソッド毎にテストケースをグルーピング

``` scala
"Class#method" when {
  "precondition" should {
    ...
    var ret: Type = null
    "method invocation" in {
      ret = ...
    }

    "post conditions" in {
      ...
    }
  }
}

```

===
# Tips
------

* エディタでテンプレートを用意しておくと楽
  + ensimeのテストテンプレートとか
* やや冗長でも頭を使わずに書ける/理解出来るテストを書く

===
# ユーティリ
---------------

* ユーティリティのテストはPropSpec
* ユーティリティ = 汎用的(文脈に依存しない)、副作用がない、小さい
  + まさにプロパティベースのテストに向く
* ほとんどGenerator Driven Test
  + 半分くらいScalaCheckの機能を使う
* 一部Tebale Driven Test
  + Generatorがコーナーケースを生成してくれない時がある

===
# 他のテスト
------------

* インテグレーションテスト/パフォーマンステストはGatling(の予定)
* Gatlingでエラーレート、パーセンタイル、レスポンスなどをテスト出来る
* パフォーマンステストは実行インスタンスを固定しないといけない問題が…
* シナリオテストはない(APIがReadのみなため)

===
# 他のテスト
---------------

* AxionはThriftプロトコルだがGatlingのAPIをいくつか実装すればGatlingを独自プロトコルに対応出来る
  + 実装量はほとんどない
  + 2~3個DSLを作るだけ
  + 簡単とはいってない(ドキュメントがないつらさ)
* Gatlingのおかげでパフォーマンスに対する表明やリクエスト毎のチェックなどが簡単
* そのうちノウハウを公開するかも？
  + 2.1.Xと2.2.XでAPIが違う問題も…

===
# まとめ
--------

* ScalaTestを紹介したよ
* ScalaTestの実用例を紹介したよ
* Gatlingについて紹介したよ


</textarea>
