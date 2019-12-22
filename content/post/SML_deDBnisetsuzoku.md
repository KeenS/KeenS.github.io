---
categories: [ML, SML, SMLSharp, Advent Calendar, Advent Calendar 2015, ML Advent Calendar]
date: 2015-12-05T22:04:25+09:00
title: SML#でDBに接続
---
このエントリは[ML Advent Calendar](http://www.adventar.org/calendars/848)6日目の記事です。  
次はdico_leque先生で、「ML中心にしつつ何か」です。

κeenです。SMl#はDB接続及びSQLを言語レベルでサポートしていることが大きな特徴ですが詳細な方法についてはあまり情報がなく、
実際に利用するのは難しい状況にあります。そこで私がなんとかソースコードを読んで得られた知識を少し共有したいと思います。
<!--more-->

# 接続サーバ設定
DBへの接続サーバ設定には

```
_sqlserver serverLocation : τ
```

式を使います。このserverLocationはDB毎に指定方法が異なります。公式のドキュメントにはデフォルトDBであるPostgreSQLへの接続方法しか書かれていませんが、いくつかのDBをサポートします。

SML#はDB接続時にテーブルの型を要求します(上でいう型注釈の`τ`がそれです。文法的に型注釈がないといけません。)。今回の例では公式ドキュメントにある通り

``` sql
CREATE DATABASE mydb;
CREATE TABLE Persons (
  name text not null,
  age int not null,
  salary int not null
);
USE mydb;
INSERT INTO Persons VALUES ('Joe', 21, 10000);
INSERT INTO Persons VALUES ('Sue', 31, 20000);
INSERT INTO Persons VALUES ('Bob', 41, 30000);
```

で作ったDBへ接続することにします。

## テーブルの型
深追いしてないので分かりません。とりあえず今回使う`mydb`は`{Persons:{name:string, age:int, salary :int}`で接続出来るみたいです。これも追わねば…

一応ソースを読む限りだとint, decimal, real32, float, real, string, bool, timestampがあるようです。NULLableなカラムにはoptionを使うようです。


SMLで見慣れないdecimal, float, timestampはSML#が実装を持っているようです。

float, decimalは内部表現は文字列で、文字列との相互及びrealとIEEERealへの変換が出来るようです。

timestampは文字列との相互変換及び`now()`関数, `defaultTimestamp`が存在します(スキーマに`DEFAULT`指定があった時用)。内部表現はunix timeなのか、intです。

それぞれ`SQL.Float`, `SQL.Decimal`, `SQL.TimeStamp`モジュールに入っています。

## PostgreSQL
デフォルト設定のpgのmydbというDBへ接続したいならこれで接続出来ると公式ドキュメントに書いてあるのですが

```
val myServer = _sqlserver (dbname="mydb") : {Persons:{name:string, age:int, salary :int}
```

これだと最後の `}`が1つ抜けている上、それを補っても

```
(interactive):2.27-2.32 Error: (name evaluation "190") unbound variable: dbname
```

と返してきます。正しくは(?)

```
val myServer = _sqlserver ("dbname=mydb") : {Persons:{name:string, age:int, salary :int}}
```

です。他のパラメータ、例えばホストやポート、パスワードを渡したい場合はスペース区切で渡します。

```
val myServer = _sqlserver ("dbname=mydb host=localhost password=hogehoge") : {Persons:{name:string, age:int, salary :int}}
```

この文字列はそのままPostgreSQLの[`PGconnectdb`](http://www.fireproject.jp/feature/postgresql/programing_libpq/connect.html#2)という関数に渡っているのでそのドキュメントに完全な受付可能なキーが書いてあります。


* host: DBサーバ名 
* hostaddr: DBサーバのIPアドレス 
* user: DB接続時のユーザ名 
* password: userに対するパスワード 
* dbname: DB名 

とのことです。


## MySQL
ドキュメントはありませんがソースコードを読んである程度勘を効かせた上で色々書いて試行錯誤すれば接続方法は分かります。

MySQLに接続するには以下のように`SQL.mysql`を使います。


``` sml
val myServer = _sqlserver (SQL.mysql "dbname=mydb") : {Persons:{name: string, age: int, salary: int}}
```

余談ですが`_sqlserver`はオーバーロードされていて、backend型とstring型両方を受け付けます。
文字列が来た時には`SQL.default`に渡されます。そして `val default = postgresql`です。つまり、PostgreSQLの例は`_sqlserver (SQL.postgresql "dbname=mydb")`
としているのと同じです。

閑話休題。MySQLの時に渡せるパラメータの話をしましょう。こちらはバックエンドの接続関数にそのまま渡してる訳ではないのですが、
SML#側でパーサを持っているのでPostgreSQLの時のように書けます。

``` sml
SQL.mysql "dbname=mydb host=localhost"
```

ここで使えるキーは

```sml
val availableKeys = ["host", "port", "user", "password", "dbname", "unix_socket", "flags"]
```

と定義されています[^1]。因みにこれはパースエラーにならない文字列の集合であって、`unix_socket`を使おうとするとコネクションエラーになるようです(`raise Connect "unix_socket is not supported"`)。

[^1]: https://github.com/smlsharp/smlsharp/blob/master/src/sql/main/MySQLBackend.sml#L184

文法に関して深掘りすると、一応パーサ([src/sql/main/KeyValuePair.sml](https://github.com/smlsharp/smlsharp/blob/master/src/sql/main/KeyValuePair.sml))を読む限りだと

* keyは `[a-zA-Z0-9_]`(`isAlphanum` + `#'_'`)
* valueはスペース以外の文字列(e.g. `hogehoge`)又は`'`で囲まれた文字列(e.g. `'hoge hoge'`)
* valueでは`\`によるエスケープが可能(e.g. `hoge\ hoge`, `'a \'quote\''`)

`\`については文字列の中なので`\`自身のエスケープが必要なことに注意して下さい。

## unixODBC
ご存知ない方に説明するとODBCはOpen Database Connectivityの略です。Microsoftが主導となって定めたDB接続のインターフェースです。また、そのインターフェースに従うドライバマネージャのことも指します。
一旦抽象化レイヤを挟むのでパフォーマンス面では生の接続に負けますが、豊富な接続先が魅力です。
MicroSoftのSQL ServerやMicroSoft Accessを始めとし、Oracle, MySQL, PostgreSQL, SQLite3, 果てはMongoDBなどにまで接続出来ます。
まあ、接続出来るからといって投げたクエリを正しく解釈、実行してくれるとは限りませんが…

unixODBCはODBCのunix実装のようです。つまりODBCドライバを持つDBにunixから接続出来ます。

さてさて、odbcに繋ぐには以下のようにします。[^2]

[^2]: https://github.com/smlsharp/smlsharp/blob/master/src/sql/main/ODBCBackend.sml#L270


``` sml
val myServer = (SQL.odbc "mydb username mypassword") : {Persons:{name: string, age: int, salary: int}}
```

なぜ記法変えたし…。~~しかも必ずスペースで区切るのでクォートも出来なければ空のユーザパスワードを渡すことも出来ません。もしかしたら`"''"`としたら後側で空の文字列と扱ってくれるかもしれませんが。~~
私が`String.fields`の挙動を勘違いしてました。スペースで区切って空にしておけば空パスワードを渡せます(e.g. `"mydb username "`)。あるいは空ユーザ名も(`"mydb  "`)。


私自身ODBCに詳しくないのですが、ODBCに接続するときはData Source Nameと呼ばれるものがシステムの特定の場所に存在するので他の接続情報は必要ないみたいです。

# 接続
さっきまでは接続サーバの設定の話でした。今度は実際に接続する話です。DBMS固有の話はサーバ設定までなのでこれからは総のDBに共通です。

接続するには公式のドキュメント通り

```sml
val conn = SQL.connet myServer
```

で繋げます。ここで各クライアントのダイナミックリンクライブラリがないとエラーになります。64dbit環境でSML#を32bitでビルドしている人はライブラリも32bitでビルドされていることを確認しましょう。

# クエリ

これもドキュメント通りです。

``` sml
val myQuery = _sql db => select #P.name as name, #P.age as age
      from #db.Persons as P
      where SQL.>(#P.salary, 10000)
val rel = _sqleval myQuery conn
val result = SQL.fetchAll rel; (* => [{age=32, name="Sue"}, {age=41, name="Bob"}] *)
val () = SQL.closeRel
```

です。

可能な文法については主に[公式ドキュメント](http://www.pllab.riec.tohoku.ac.jp/smlsharp/docs/2.0/ja/Ch10.S5.xhtml)を参考にしましょう。

insert, update, deleteをサポートしている他、トランザクション(`begin`, `commit`, `rollback`)をサポートしているようです。詳しくは[BNF](https://github.com/smlsharp/smlsharp/blob/master/src/compiler/parser2/main/iml.grm#L727)を読んで下さい。

SQLを投げて返ってきたリレーションに対しては結果を総取得する`SQL.fetchAll`、結果をoption型で1つ取得する`SQL.fetch`、結果を1つ取得するか例外になる`SQL.fetchOne`を使います。`fetchOne`の上げる例外は`SQL.NotOne`です。

クエリ結果を使い終わったら必ず`SQL.closeRel`で開放しましょう。そしてコネクションも、終わったら`SQL.closeCon`で開放しましょう。




# おわりに
本当は実際に試してから記事を書きたかったのですが、32bitの壁に阻まれて出来ませんでした。しかしソースを読んで裏を取って書いてあるのでまあまあ信憑性のある内容だと思います。
SML# からデータベースに接続したい方の一助になれば幸いです。
