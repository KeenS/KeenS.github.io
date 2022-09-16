---
categories: [Scala, Scala Meet Up]
date: 2015-12-13T22:42:39+09:00
description: "社内勉強会のScala Meet Upで話す内容。
Tagless Finalについて。あまりScala関係ない…
"
title: DSLとTagless Final
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# DSLとTagless Final
----------------------
サイバーエージェント アドテクスタジオ  
Scala Meet Up 2015-12-18

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + AMoAdの新卒エンジニア
 + Lisp, ML, Rust, Shell Scriptあたりを書きます
   - Scalaは初心者 ※ [Scala初心者の新卒が頑張ってLispを作ってみた](http://adtech.cyberagent.io/scalablog/2015/12/05/scala-lisp/)
===
# 注意
------

* Scala初心者なのでScalaではなくScalaっぽい疑似コードで説明します
  + ~~コード例動かす時間が無かっただけ~~
* Scala初心者なのでScalaのコードは少なめに説明します

===
# DSLを作る
-----------

以下のようなHTTPなユーザをエミュレートするDSLを考える

``` scala
val scenario = and(
  get("/"),
  get("/users").andThen(res =>
    post("/follow", first(users.getJsonData))))
val Right(_) = runScenario(scenario, "user")
```

===
# DSLを作る
-----------

普通は以下のようなAST作ってをラップする

``` scala
trait ScenarioDSL
sealed case class Get(url: String) extends ScenarioDSL
sealed case class Post(url: String, data: JSON) extends ScenarioDSL
sealed case class And(first, ScenarioDSL, second ScenarioDSL) extends ScenarioDSL
sealed case class AndThen(first: ScenarioDSL,
   callBack: (Response) => ScenarioDSL) extends ScenarioDSL

def get(url: String): ScenarioDSL= Get(url)
....

```

===
# DSLを作る
-----------

そして`runScenario`を作る

``` scala
def runScenario(scenario: ScenarioDSL, user: String) = {
  scenario match {
    Get(url) => Client.getInstance(user).get(url)
    Post(url) => Client.getInstance(user).post(url)
    And(first, second) => {runSenario(first, user); runSenario(second, user)}
    ....
  }
}


```



===
# DSLを便利に
-------------

例えば、このDSLを`curl`コマンド関数を追加する

``` scala
def toCurl(scenario: ScenarioDSL, user: String) =
  val auth = makeAuth(user)
  scenario match {
    Get(url) => "curl ${auth} ${SERVICE_HOST}/url"
    Post(url) => "curl ${auth} -XPOST ${SERVICE_HOST}/url"
    And(first, second) => toCurl(first) + "\n" + toCurl(second)
    ....
  }
}

```

===
# DSLの拡張
-----------

このDSLに新たに`select`を追加する

``` scala
val scenario = and(
  get("/"),
  select(get("/users"),
         post("/update")))
val _ = runScenario(scenario, "user")
```

===
# Expression Problem
-------------------

この時に問題が出る

* DSLを使う全てのコードに変更が必要
  + `runScenario`と`toCurl`で`select`に対応する
* DSLそのものに手を加える必要がある
  + DSLがsealed traitされてる
  + そもそも、DSLがライブラリだったら変更出来るの？
* 実際には使ってなくても全ての関数で新しい機能に対応しないといけない
  + `toCurl`では`select`に対応しないとか
  + 逆に、`toCurl`がプラグインでコードいじれなかったらどうしよう
===
# Tagless Finalで解決出来るよ
----------------------------

* 元のコードをいじらず
* 必要な所だけを記述して
* しかも元々の実装よりも速い

DSLの作り方があります。それは型クラスを使ったやり方です。

===
# 型クラスの復習
---------------

型クラスは

* 型の振る舞うインターフェースを定めて
* インスタンスの型ごとに「後付けで」実装を与えると
* 多相的に扱えるアドホックポリモーフィズム

でした

===
# 型クラスの復習
---------------

``` rust
trait Hello { //型クラス
  fn hello(&self) -> String;
}

impl Hello for isize { //isize型のインスタンス
  fn hello(&self) -> String {format!("I'm {} years old", self)}
}
impl <'a>Hello for &'a str {//&str型のインスタンス
  fn hello(&self) -> String {format!("My name is {}", self)}
}
fn main() {
  println!("{}", 23.hello());     // => "I'm 23 years old"
  println!("{}", "κeen".hello()); // => "My name is κeen"
}
```

===
# DSLをRustに翻訳
-------------

Scalaで型クラスを説明するとややこしいので一旦先のDSL例をRustに翻訳します

``` rust
let scenario = and(
  get("/".to_string()),
  get("/users".to_string()).andThen(|req|
    post("/follow".to_string(), req.getJsonData().first)));
runScenario(scenario, "user").unwrap();
```

===
# Rustに翻訳
-------------
DSLのASTはこうなります。

``` rust
enum ScenarioDSL {
  Get{url: String},
  Post{url: String, data: RequestData},
  And{first: Box<ScenarioDSL>, second: Box<ScenarioDSL>},
  AndThen{first: Box<ScenarioDSL>,
   callBack: FnOnce<(Scenario, Request) -> ScenarioDSL>},
}
```
===
# Rustに翻訳
-------------
runScenarioはこうなります。

``` rust
fn runScenario(scenario: &ScenarioDSL, user: &str)-> Result<()> {
  match scenario {
    &ScenarioDSL::Get{ref url} => Client::instance(user).get(url),
    &ScenarioDSL::Post{ref url} => Client::instance(user).post(url),
    &ScenarioDSL::And{ref first, ref second} => {
      try!(runSenario(first));
      runSenario(second)
    },
   ...
  };
}
```

===
# Rustに翻訳
-------------
`toCurl`はこうなります

``` rust
fn toCurl(scenario: &ScenarioDSL, user: &str)-> String {
  let auth = makeAuth(user)
  match scenario {
    &ScenarioDSL::Get{ref url} => format!("curl {} {}{}", auth, SERVICE_HOST, url)
    &ScenarioDSL::Post{ref url} => format!("curl {} -X POST {}{}", auth, SERVICE_HOST, url)
    &ScenarioDSL::And{ref first, ref second} => {
      let first_str = toCurl(first, user);
      let second_str = toCurl(second, user);
      format!("{}\n{}", first_str, second_str)
    },
   ...
  }
}
```

===
# Tagless Final
---------------

Rustの準備が終わったのでTagless Finalの説明に入ります。

===
# Tagless Final
----------------

* ASTをデータではなく関数で表わす
* 同じ関数でも欲しい結果によって計算を変えるために型クラスを使う
* どの型クラスのインスタンスとして扱うかで結果を変える
  + そもそもASTは計算を抽象化してデータにしたもの。
  + 抽象的な計算があるならデータにする必要はない

* 型クラスでジェネリックに作って
* 欲しい型を伝えるだけで挙動が変わる

===
# 型クラス
----------
DSLの文法の型クラスを定義する。  
DSLっぽく見せるため、ラッパを書く(Rust特有)。


``` rust
trait ScenarioSYM {
  fn get(url: String) -> Self;
  fn post(url: String) -> Self;
  fn and(first: Self, second: Self) -> Self;
  ...
}

fn get<C: ScenarioSYM>(url: String) -> C {C::get(url)}
fn post<C: ScenarioSYM>(url: String) -> C {C::post(url)}
fn and<C: ScenarioSYM>(first: C, second: C) -> C {C::and(first, second)}
...
```


===
# `runScenario`
------
結果に`Result`型が結果として欲しいなら`Result`型に`ScenarioSYM`を実装する

```rust
impl ScenarioSYM for Result<()> {
  fn get(self,url: String) -> Self {...}
  fn post(self, url: String) -> Self {...}
  fn and(self, url: String) -> Self {...}
}

fn runScenario(res: Result<()>) -> Result<()> {
  res
}
```

===
# `toCurl`
----------
`String`が欲しいなら`String`に実装すれば良い。

``` rust
impl ScenarioSYM for String {
  fn get(self, url: String) -> Self {...}
  fn post(self, url: String) -> Self {...}
  fn and(self, url: String) -> Self {...}
}

fn toCurl(str: String) -> String {
  str
}
```

===
# 完成形
--------

``` rust
let scenario = and(
  get("/".to_string()),
  get("/users".to_string()).andThen(|req|
    post("/follow".to_string(), req.getData().iter().next())));
  // ↑ここまではジェネリックなScenarioSYM型
  // ↓ここで呼ばれることでResult<()>型にインスタンス化される
runScenario(scenario, "user").unwrap();
```

===
# DSLの拡張
-----------

拡張したい文法のDSLを定義して、欲しいものに実装するだけ。

`runScenario`に変更は要らないしサポートしない`toCurl`は気にしなくて良い。

``` rust
trait SelectSYM {
  fn select(self, first: Self, second: Self) -> Self;
}

impl SelectSYM for Result<()> {
  fn select(self, first: Self, second: Self) -> Self {...}
}
```

===
# Tagless Finalまとめ
---------------------

* この方式だと実行時にタグでパターンマッチしないので速い
* DSLの拡張も機能の拡張も容易
* 型クラス便利

===
# ScalaでのTagless Final
------------------------
Rustで分かりやすく解説したのでScalaでどうなるか見ていきます。

まずは型クラスを定義

``` scala
trait ScenarioSYM[C] {
  def get(self: C, url: String): C
  def post(self: C, url: String): C
  ...
}
def get(self: C, url: String)(implicit i: ScenarioSYM[C]): C = i.get(self, url)
def post(self: C, url: String)(implicit i: ScenarioSYM[C]): C = i.post(self, url)
...
```

===
# `runScenario`
------

``` scala
implicit val resultScenario = new ScenarioSYM[Either[Err, ()]] {
  def get(self: Either[Err, ()], url: STring): Either[Err, ()] = ...
  def post(self: Either[Err, ()], url: STring): Either[Err, ()] = ...
  ...
}

def runScenario(ast: Either[Err, ()], user: String)
      (implicit i: ScenarioSYM[Either[Err, ()]]) = ast
```

===
# 完成形
--------
Scalaの型クラスの制約上最初の例と少し異なる
(`val`じゃなくて`implicit`を取る`def`になる)


``` scala
def scenario(implict i: ScenarioSYM[C]) = Scenario
   get("/")
   get("/users") andThen((scenario, res) =>
    scenario post("/follow", first(users.getJsonData)))
val Right(_) = runScenario(scenario, "user")

```

===
# まとめ
--------

* Expression Problemについて説明した
* Tagless Finalを紹介した
* 型クラス便利
* Rust便利
* [参考](http://okmij.org/ftp/tagless-final/course/)

</textarea>
