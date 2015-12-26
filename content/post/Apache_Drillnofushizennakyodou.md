---
categories: [SQL, DB, Apache Drill, Advent Calendar, Advent Calendar 2015]
date: 2015-12-20T23:04:37+09:00
title: Apache Drillの不自然な挙動
---
この記事は[Apache Drill Advent Calendar 2015](http://qiita.com/advent-calendar/2015/drill)21日目のエントリです

最初は備忘録がてら普段使うMySQL、BigQuery、SQLite、Apache DrillのSQLシンタックスの違いでも書こうかと思ったのですが調査が思ったより大変だったのでやめました。ANSI SQLも色々変わってるということは理解出来ました。

ということでApache Drillを使ってて「ん？」と思った挙動をいくつか出してみようかと。1.4時点での情報です。

<!--more-->

# `SELECT 1`
シェルで手持ち無沙汰に`ls`や`pwd`を打つようにSQLプロンプトで`SELECT 1;`を打つ訳ですが、Apache Drillはこれを受け付けてくれません。

まあ、これは不自然というかそういう仕様なのですが正しくは

``` sql
SELECT 1 FROM (VALUES(1));
```

です。手持ち無沙汰に打つには少し面倒ですね。そうでなくても日付関数の挙動を確かめたい時にも

``` sql
SELECT NOW();
```

などとやりたい訳ですがそれにも` FROM (VALUES(1));`とダミーの`FROM`句を付けてあげる必要があります。面倒ですね。


# `UNION`と`UNION ALL`
`UNION`はテーブル同士のレコード間で重複を取り除いた集合を、`UNION ALL`は重複を取り除かない集合を返します。Apache Drillでも試してみましょう。

``` sql
SELECT * FROM (VALUES (1, 2, 3) UNION VALUES(4, 5, 6) UNION VALUES(7, 8, 9) UNION VALUES(1, 2, 3));
SELECT * FROM (VALUES (1, 2, 3) UNION ALL VALUES(4, 5, 6) UNION ALL VALUES(7, 8, 9) UNION ALL VALUES(1, 2, 3));
```

これらのクエリはどういう結果になるでしょうか。答えはこうです。

```
0: jdbc:drill:zk=local> SELECT * FROM (VALUES (1, 2, 3) UNION VALUES(4, 5, 6) UNION VALUES(7, 8, 9) UNION VALUES(1, 2, 3));
+---------+---------+---------+
| EXPR$0  | EXPR$1  | EXPR$2  |
+---------+---------+---------+
| 1       | 2       | 3       |
+---------+---------+---------+
1 row selected (0.416 seconds)
0: jdbc:drill:zk=local> SELECT * FROM (VALUES (1, 2, 3) UNION ALL VALUES(4, 5, 6) UNION ALL VALUES(7, 8, 9) UNION ALL VALUES(1, 2, 3));
+---------+---------+---------+
| EXPR$0  | EXPR$1  | EXPR$2  |
+---------+---------+---------+
| 7       | 8       | 9       |
| 7       | 8       | 9       |
| 7       | 8       | 9       |
| 7       | 8       | 9       |
+---------+---------+---------+
4 rows selected (0.115 seconds)
```

うーん。これはバグだと思うんですよね。定数テーブルをキャッシュするようにしてたら全てのレコードで同じ値を参照しちゃって値を書き換えてしまったとか。
`UNION`が`UNION ALL`の結果から`DISTINCT`相当の処理をするなら2番目の結果を見れば最初の結果が従うのもうなずけますし。

# ヘッダ付きCSV
Drill 1.4からヘッダ付きCSVの扱いが楽になりましたね。拡張子が`csv`のままヘッダ付きCSVにクエリを投げられます。ちょっと試しましょう。

次のようなCSVファイルを用意します。

```
$ cat ~/Csv/test.csv
foo,bar,baz
1,1.0,"a"
2,2.0,"b"
3,3.0,"c"
4,4.0,"d"
5,5.0,"e"
```

クエリを投げてみましょう。

```
0: jdbc:drill:zk=local> SELECT * FROM TABLE(dfs.`/home/kim/Csv/test.csv`(type => 'text', fieldDelimiter => ',', extractHeader => true));
+------+------+------+
| foo  | bar  | baz  |
+------+------+------+
| 1    | 1.0  | a    |
| 2    | 2.0  | b    |
| 3    | 3.0  | c    |
| 4    | 4.0  | d    |
| 5    | 5.0  | e    |
+------+------+------+
5 rows selected (0.12 seconds)
```

よしよし。じゃあ今度は`baz`カラムだけ取り出してみましょう。ここで、会社のMacだと


```
0: jdbc:drill:zk=local> SELECT baz FROM TABLE(dfs.`/home/kim/Csv/test.csv`(type => 'text', fieldDelimiter => ',', extractHeader => true));
+------+------+------+
| foo  | bar  | baz  |
+------+------+------+
| 1    | 1.0  | a    |
| 2    | 2.0  | b    |
| 3    | 3.0  | c    |
| 4    | 4.0  | d    |
| 5    | 5.0  | e    |
+------+------+------+
5 rows selected (0.12 seconds)

```

のようにカラムを指定しているのにも関らず全てのデータが返ってきていました。その旨を書こうと思ったのですが手元のLinuxマシンだと正しい結果が返ってくるようです。

```
0: jdbc:drill:zk=local> SELECT `bar` FROM TABLE(dfs.`/home/kim/Csv/test.csv`(type => 'text', fieldDelimiter => ',', extractHeader => true));
+------+
| bar  |
+------+
| 1.0  |
| 2.0  |
| 3.0  |
| 4.0  |
| 5.0  |
+------+
5 rows selected (0.088 seconds)
```

ちょっと会社で見たのが幻覚だったかもしれないので要調査ですね。


# MySQLへの接続
以下のようなSQLを投げた所、ひどく時間が掛かりました。投げた対象はプロダクションのデータが入ってる分析用のDBで、結構な量のデータが入ってます。

``` sql
SELECT * FROM mysql.mydb.`test` LIMIT 20;
```

Apache Drill自体レスポンスが悪いのでクエリによってはそんなものかな、と思って最初はそのままにしたのですがふと気になって`EXPLAIN`してみました。


```
0: jdbc:drill:zk=localhost:2181> EXPLAIN PLAN FOR SELECT * FROM mysql.mydb.`test` LIMIT 20;
+------+------+
| text | json |
+------+------+
| 00-00    Screen
00-01      Project(columns....)
00-02        SelectionVectorRemover
00-03          Limit(fetch=[20])
00-04            Limit(fetch=[20])
00-05              Jdbc(sql=[SELECT * FROM `mydb`.`test`])
| {
....
```

`Jdbc(sql=[SELECT * FROM mydb.test])` とあるのでなんとMySQLに`LIMIT`をつけずにクエリを投げてます。そしてさらに何故か`Limit(fetch=[20])`を2回やっています。

まあ、内部でLimitを2回やってるのは良いとしてもMySQLにLimit無しで投げるのはいただけないですね。
分散モデルのDrillからしたら「分散してる俺らの方が処理が速いから全部俺らでやってやるよ」という気概なのかもしれませんがこちらとしてはDBへの負荷も考えて欲しいものです。
まあ、かといって`WHERE`句にインデックスが付く/付かないだとかその他の句によってMySQLのへの負荷も変わるのでなんとも言えないといえばそうなんですが簡単な場合にpush downしてくれるか
あるいは手動でpush down出来る仕組みが欲しいですね。`FORCE INDEX`みたいに。


さて、色々気になる点はありましたがApache Drillは面白いプロダクトです。
主にビッグデータ分析に使われるようですが私は大きなテーブルからアドホックに抽出したCSVとMySQLにあるマスターデータをJOINするなどの目的で使っていきたいなと思っています。

皆様良いデータ分析ライフを！
