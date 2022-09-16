---
categories: [SQL, DB, Apache Drill, Drill Meet up]
date: 2016-03-06T14:54:30+09:00
description: "Tokyo Apache Drill Meetupでの発表用
自分なりのdrillの使い方とsql-drill.elについて
"
title: ビッグデータしないDrill、ローカルで快適に使うために
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# ビッグデータしないDrill、ローカルで快適に使うために
----------------------
[Tokyo Apache Drill Meetup](http://drill.connpass.com/event/27414/)  
2016-03-22

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + Lisp, ML, Rust, Shell Scriptあたりを書きます
 + サイバーエージェントのエンジニア
   + アドテクスタジオ所属
   + データエンジニアじゃなくてもデータを触ることはある

===
# なぜローカルか
---------------

* ビッグデータ重い
* 使いたいデータが絞れる時には大袈裟
  + 「売り上げ上位1000のユーザの行動」とか
* 同じようなクエリの重複
  + 新しいテーブル作れないDBとかview作れないDBとか
* 結果をRに渡したりとかが面倒
  + CSVダウンロードがGUI
===
# キャッシュ的な
----------------

```
+---------------------------------+
| BIG DATA (BigQuery, Spark, etc) |
+---------------------------------+
        ^ |  ^  |
        | |  |  |
        | v  |  v
   +-------------------+
   | ローカルキャッシュ | <- ???
   +-------------------+
     ^ | ^ | ^ | ^ |
     | v | v | v | |
     +----------+  |
     |   自分   |  |
     +----------+  v
     +---+    +-----+
     | R | <--| CSV |
     +---+    +-----+
```

===
# やりたいこと
--------------

* パパッっと処理を始められる
  + だいたい「CSVを簡単に読める」に同じ
  + たまにJSONとか
* 簡単に処理出来る
  + 自分が馴れているかどうか
* CSVを吐ける
  + 最後はRに渡したい

===
# 候補
-------

* ローカルMySQL
  + 事前のScheme定義が必要
* R
  + 扱い馴れない
* SQLite
  + 事前のScheme定義が必要
* (Python)
  + あまり向いてない？

===

<pre style='font-size: 200%'>
＿人人人人人人人＿
＞　突然のDrill　＜
￣Y^Y^Y^Y^Y^Y￣
</pre>

===
# なぜ Drill
------------

* (デーモン立ち上げておけば)サクっと始められる
* CSVをそのまま読める
  + Schema定義がいらない!
* ついでにJSONも読める
* 馴れたSQLで操作出来る
* CSV吐ける

===
# Drillを便利にするために
-------------------------

* いくつかのシェル関数
* Drillのデーモン/クライアントの起動を便利に

```sh
drill-start() {
    ~/compile/zookeeper-3.4.8/bin/zkServer.sh start
    drillbit.sh start
}

drill-cli() {
    sqlline -u jdbc:drill:zk=localhost:2181
}

drill-web() {
   firefox http://localhost:8047
}
```

===
# もっと便利に
--------------

* REPLでの操作が面倒
  + ヒストリ遡るのとか
  + ミスった時の訂正が地味に不便
* シンタックスハイライト欲しい
* 馴れたツールで編集したい

===
# sql-drill.el
--------------

* [KeenS/sql-drill.el](https://github.com/KeenS/sql-drill.el)
* Emacsのsql-modeのDrillサポート
  + emacsのsql-modeは拡張可能
* シンタックスハイライト
* REPL

===
# DEMO
------
<!-- .slide: class="center" -->

===
# まとめ
--------

* ローカルでデータ分析したい時があるよ
* その時にDrillは便利だよ
* Emacs向けのDrillプラグイン作ったよ

</textarea>
