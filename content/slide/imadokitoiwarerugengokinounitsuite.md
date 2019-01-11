---
categories: []
date: 2019-01-11T00:34:20+09:00
description: "産業界で比較的最近導入されるようになった言語や言語機能は学術界では古くから研究されていたものも多くあるようです。
例えばRustやScalaにあるトレイト、Swiftにあるプロトコル、Goのgoroutineやチャネルなどは遡ると1970年代、80年代頃に提案された機能にあたります。
本発表では産業界でイマドキと言われる言語機能を紹介し、最先端の研究から実用されはじめるまでのギャップを覗こうと思います。 "
title: "イマドキと言われる言語機能について"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# イマドキと言われる言語機能について
----------------------
第60回プログラミングシンポジウム
<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
   + 情報科学の教育は受けていない純粋なエンジニア
 * 実際に仕事で使った(ている)のはJava, Scala, Rust

===

# 最近っぽい言語
----------------

言語              | 1.0リリース | 特徴
:-----------------------------------------------|------------|:------
 [Go](https://golang.org/)                      |   2012     | goroutineが使えるシンプルな言語
 [Rust](https://www.rust-lang.org/)             |   2015     | 安全なシステムプログラミング言語
 [Swift](https://developer.apple.com/jp/swift/) |   2014     | iOSアプリが作れる
 [Scala](https://www.scala-lang.org/)           |   2004     | 関数型パラダイムを取り入れたJVM言語
 [Kotlin](https://kotlinlang.org/)              |   2016     | 整理されたJava


===

# 最近のトレンド
---------------

* 静的型付言語が増えてきた
  + 動的型付言語が主流だった反動？
* 静的コンパイルする言語が増えてきた
* インタプリタ言語にも速度が求められるようになってきた
* 関数型言語の機能を取り入れるようになってきた
* 速度やマルチコア対応が気にされ始めた
* マルチタスクのサポートが増えてきた

===
# 静的型(解析)
--------

* 動的型付言語に(部分的に)静的型を入れるのが増えてきた
  + [漸進的型付け (2006)](http://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/)かな？
  + [TypeScript](https://www.typescriptlang.org/)
  + Pythonの[Type Hints](https://www.python.org/dev/peps/pep-0484/)
  + Ruby 3に型を入れたいらしい
* Null値の静的解析が増えた
  + Scala, Rustの`Option` 型
  + Swift, Kotlinのnullable/non-nullableの区別
* 型推論

===

# TypeScript
------------

* TypeScript is a typed superset of JavaScript that compiles to plain JavaScript.

```typescript
function greeter(person: string) {
    return "Hello, " + person;
}
```

* 型がかなり多機能 CF [TypeScriptで最低一つは必須なオプションオブジェクトの型を作る](https://qiita.com/uhyo/items/583ddf7af3b489d5e8e9)
* 型のないコードも許容する

===
# Rustの `Option`
-------

``` rust
pub enum Option<T> {
    None,
    Some(T),
}

let x: Option<u32> = Some(2);
assert_eq!(x.is_some(), true);

let x: Option<u32> = None;
assert_eq!(x.is_some(), false);
```

===
# Rustの `Option`
-------

* 代数的データ型で定義される
* 特に `Option` が特別扱いされているわけではない


===
# KotlinのNull許容型
--------

``` kotlin
val listWithNulls: List<String?> = listOf("Kotlin", null)
for (item in listWithNulls) {
    item?.let { println(it) } // prints A and ignores null
}
```

===
# KotlinのNull許容型
--------

* Nullable Typeのための構文が用意されている

===
# 継承に依らないポリモーフィズム
------------------------------

* 今まで主流の言語は継承によるポリモーフィズムが多かった
  + Ruby, Perl, Python, Java, C++
* それ以外の方法が増えてきた
 + [型クラス(1988)](https://people.csail.mit.edu/dnj/teaching/6898/papers/wadler88.pdf)とか構造的ポリモーフィズムとか
 + Scalaの貧者の型クラス, Rustのトレイト
 + Goのインターフェース


===
# Rustのトレイト
------

``` rust
struct Sheep { naked: bool, name: &'static str }

trait Animal {
    // Instance method signatures; these will return a string.
    fn name(&self) -> &'static str;
    fn noise(&self) -> &'static str;
}

// Implement the `Animal` trait for `Sheep`.
impl Animal for Sheep {
    fn name(&self) -> &'static str {
        self.name
    }

    fn noise(&self) -> &'static str {
        if self.is_naked() {
            "baaaaah?"
        } else {
            "baaaaah!"
        }
    }
}
```

===
# Rustのトレイト
------

* 割と普通の型クラス

===
# Scalaの型クラス
-------


``` scala
trait Show[A] {
  def show(a: A): String
}
implicit val intCanShow: Show[Int] =
  new Show[Int] {
    def show(int: Int): String = s"int $int"
}
def showExp(implicit sh: Show[A]) = sh.show(a)
```

===
# Scalaの型クラス
-------

* dictinary passing方式のimplicit parameterを生で実装

===
# Goのインターフェース
-----

``` go
type I interface {
	M()
}

type T struct {
	S string
}

// This method means type T implements the interface I,
// but we don't need to explicitly declare that it does so.
func (t T) M() {
	fmt.Println(t.S)
}

```

===
# Goのインターフェース
-----

* 構造的ポリモーフィズムっぽい？

===

# 所有権
---------

* GCを使わないメモリ管理
 + [線形型 (1990?)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.31.5002)
 + Rust, (部分的に)C++のムーブセマンティクス

===
# Rustの所有権
--------------

``` rust
let s1 = String::from("hello");
// s1はここでs2に所有権が移った
let s2 = s1;

// ここでs1を使おうとするとエラー
println!("{}, world!", s1);

// s2はスコープの終わりで解放される
```

===
# Rustの所有権
--------------

* Rustの最大の特徴とされるが難しいという声も

===

# 非同期処理
-----------------

* async/await
  + C#, JavaScript, (Scala), (Rust)
* コルーチン
  + kotlin, (Java)
* goroutine + [CSP(1978)](https://www.cs.cmu.edu/~crary/819-f09/Hoare78.pdf)
  + goroutineはコルーチンではない
  + goroutineは軽量スレッドではない
  + Go

===
# C#の `async` / `await`
-----

``` c#
private readonly HttpClient _httpClient = new HttpClient();

downloadButton.Clicked += async (o, e) =>
{
    // This line will yield control to the UI as the request
    // from the web service is happening.
    //
    // The UI thread is now free to perform other work.
    var stringData = await _httpClient.GetStringAsync(URL);
    DoSomethingWithData(stringData);
};
```

===

# C#の `async` / `await`
-----

* `async` / `await` を最初に発明したのが C# らしい？
* `async` ブロック内で `await` を呼ぶことでIOでブロックしなくなる
  + シンタックスシュガーで内部ではステートマシンになる
  + コルーチンと違ってスタックレス
* ユーザが非同期タスクを作ることも出来る

===
# Goのgoroutine
-----------------

``` go
func sum(s []int, c chan int) {
	sum := 0
	for _, v := range s {
		sum += v
	}
	c <- sum // send sum to c
}

func main() {
	s := []int{7, 2, 8, -9, 4, 0}

	c := make(chan int)
	go sum(s[:len(s)/2], c)
	go sum(s[len(s)/2:], c)
	x, y := <-c, <-c // receive from c

	fmt.Println(x, y, x+y)
}
```

===
# Goのgoroutine
-----------------

* goroutineはマルチスレッドで動く
* goroutineはIOなどのタイミングで他のgoroutineに制御を移す
* goroutineはnon-preemptive
* goroutine同士は(基本は)チャネルで通信する


===

# 開発支援ツール
----------------

* コードフォーマッタ
  + コードを自動整形してくれる
  + 自動インデントより多くをする
* [Language Server Protocol](https://langserver.org/)
  + 古くはLispのSWANK?
  + 開発支援のためにクエリに答える
  + 型、定義箇所、ドキュメント、名前変更など

===
# フォーマッタ
---------

``` rust
fn main()
 {
    let name   = "keen";
 println!("Hello, {}", name);
}

```

``` rust
fn main() {
    let name = "keen";
    println!("Hello, {}", name);
}
```

===
# フォーマッタ
---------

* インデントを調整する
* 改行や途中にある空白も変更する
* CLIから起動できるので誰が書いても最終的に同じ見た目になる
  + チーム開発で便利

===
# Language Server
-----

``` text
client-notification Fri Jan 11 03:54:52 2019:
(:jsonrpc "2.0" :method "initialized" :params
          (:__dummy__ t))

client-notification Fri Jan 11 03:54:52 2019:
(:jsonrpc "2.0" :method "textDocument/didOpen" :params
          (:textDocument
           (:uri "file:///home/shun/Rust/WebAssembler-rs/src/lib.rs" :version 0 :languageId "rust" :text "mod util;\nmod module;\nmod types;\nmod ops;\npub mod builder;\n\npub use types::*;\npub use module::*;\npub use ops::*;\n\npub trait Dump {\n    fn dump(&self, buf: &mut Vec<u8>) -> usize;\n}\n")))

client-notification Fri Jan 11 03:54:52 2019:
(:jsonrpc "2.0" :method "workspace/didChangeConfiguration" :params
          (:settings nil))

```

===
# Language Server
-----

* クライアント(エディタなど)からのクエリに答える
  + 型情報、補完情報などなど
  + オンラインにクエリに答えないといけない
* プロトコルが統一化されたので言語に依存せずに使えるようになった
* 割と処理系開発元と同じところが提供することが多くなった
* 静的解析よりなので動的型付き言語には少し不満

===
# まとめ
--------

* 静的解析
* 継承から離れつつある
* 所有権がきてる
* 開発支援ツールも言語に求められるように


</script>
</section>
