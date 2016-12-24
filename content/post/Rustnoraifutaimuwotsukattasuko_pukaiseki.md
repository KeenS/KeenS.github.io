---
categories: [Rust, Advent Calendar, Advent Calendar 2016]
date: 2016-12-24T22:51:46+09:00
title: Rustのライフタイムを使ったスコープ解析
---

このエントリは[Rust その2 Advent Calendar 2016 - Qiita](http://qiita.com/advent-calendar/2016/rust-lang-2)の25日目の記事です。

κeenです。大トリに小ネタで申し訳ないのですが最近書いたコードでちょっと便利だなと思ったテクニックを紹介します。

<!--more-->


次のような言語のインタプリタを作ることを考えます。意味論はエスパーして下さい。
```
x = 1
y = 2
println("x = ", x)
println("y = ", y)

println("--")

{
    x = 3
    println("x = ", x)
    println("y = ", y)
}

println("--")

println("x = ", x)
println("y = ", y)
```

この実行結果は

```
x = 1
y = 2
--
x = 3
y = 2
--
x = 1
y = 2
```

になりますね。このインタプリタをRustで作りましょう。


# 初期実装

雛形はこんなもんでしょうか。後でリファレンスが出てきてややこしくなるので値は全て所有権を貰っています。

``` rust
#[derive(Debug, Clone)]
struct AST(Vec<Stmt>);

#[derive(Debug, Clone)]
enum Stmt {
    Subst(String, Expr),
    Print(Vec<Expr>),
    Block(Vec<Stmt>),
}


#[derive(Debug, Clone)]
enum Expr {
    Str(String),
    Num(isize),
    Var(String),
}


struct Interpreter {
    // これを実装する
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter{}
    }

    fn run(&mut self, ast: AST) {
        for stmt in ast.0 {
            self.run_stmt(stmt);
        }
    }

    fn run_stmt(&mut self, stmt: Stmt) {
        // これを実装する
    }
}

fn main() {
    let ast = {
        use Stmt::*;
        use Expr::*;
        AST(
            vec![
                Subst("x".to_string(), Num(1)),
                Subst("y".to_string(), Num(2)),
                Print(vec![Str("x = ".to_string()), Var("x".to_string())]),
                Print(vec![Str("y = ".to_string()), Var("y".to_string())]),
                Print(vec![Str("--".to_string())]),
                Block(vec![
                    Subst("x".to_string(), Num(3)),
                    Print(vec![Str("x = ".to_string()), Var("x".to_string())]),
                    Print(vec![Str("y = ".to_string()), Var("y".to_string())]),
                ]),
                Print(vec![Str("--".to_string())]),
                Print(vec![Str("x = ".to_string()), Var("x".to_string())]),
                Print(vec![Str("y = ".to_string()), Var("y".to_string())]),
            ]
        )
    };
    let mut interpreter = Interpreter::new();

    interpreter.run(ast);
}
```

(コンストラクタ関数作っときゃよかったかも)

今回の本質ではないのでパーサは用意してません。

さて、とりあえずのところ変数の管理のためにシンボルテーブルが必要です。実装しましょう。


``` rust
//...

use std::collections::HashMap;
struct Interpreter {
    symbol_table: HashMap<String, Expr>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter{
            symbol_table: HashMap::new(),
        }
    }

    fn add_to_scope(&mut self, name: String, expr: Expr) {
        self.symbol_table.insert(name, expr);
    }

    fn find_scope(&self, name: &str) -> Expr {
        self.symbol_table
            .get(name)
            .expect("reference to unknown variable")
            .clone()
    }

  //...
}

```

簡略化のためにcloneやpanicしてますがご愛嬌。

これがあれば`run_stmt`のそれっぽい実装は出来ます。


``` rust
    fn run_stmt(&mut self, stmt: Stmt) {
        use Stmt::*;

        match stmt {
            Subst(name, expr) => {
                let expr = self.eval(expr);
                self.add_to_scope(name, expr);
            },
            Print(exprs) => {
                for expr in exprs {
                    self.print_expr(expr);
                }
                println!("");
            },
            Block(stmts) => {
                for stmt in stmts {
                    self.run_stmt(stmt);
                }
            },
        }

    }

    fn eval(&self, expr: Expr) -> Expr {
        use Expr::*;
        match expr {
            Var(v) => self.find_scope(&v),
            e @ Str(_) |
            e @ Num(_) => e
        }
    }

    fn print_expr(&self, expr: Expr) {
        use Expr::*;
        match self.eval(expr) {
            Str(ref s) => print!("{}", s),
            Num(ref n) => print!("{}", n),
            Var(_) => panic!("reference to unknown variable") ,
        }
    }
```

さて、これを走らせてみましょう。

```
$ cargo run
x = 1
y = 2
--
x = 3
y = 2
--
x = 3
y = 2
```


スコープを実装していないので最後の`x`が3になっていますね。スコープを実装しましょう。

スコープを実装するには簡単にはハッシュテーブルを複数持てば良さそうです。
3段にネストしたスコープなら3つのハッシュテーブルを持ちます。
そしてスコープを抜けた時にそのスコープに対応するハッシュテーブルを消せば（忘れれば）良さそうです。

``` rust
struct Interpreter {
   // スコープに対応したテーブル達。
   symbol_tables: Vec<HashMap<String, Expr>>,
   // 今どのスコープを指しているか。
   // スコープを抜けてもベクトルは縮まないのでこれが必要。
   pos: usize,
}

impl Interpreter {
    pub fn new() -> Self {
        // 新しいフィールドの初期化
        Interpreter{
            symbol_tables: Vec::new(),
            // ややこしくなるが、0で初期化しているのでpos-1番目のハッシュマップが現スコープのテーブルになる
            pos: 0,
        }
    }

    // スコープに入る処理。
    fn in_scope(&mut self) {
        let pos = self.pos;
        // スコープから抜けてもハッシュマップは消えないので
        // 入る → 出る → 入るをしたときに既にテーブルがある場合が出てくる。
        // 必要なテーブルが既にある場合とない場合で条件分岐
        if self.symbol_tables.len() <= pos {
            self.symbol_tables.push(HashMap::new())
        } else {
            // 既にテーブルがある場合は古い情報を消す
            self.symbol_tables[pos - 1].clear()
        }
        // ポジションは忘れずインクリメント
        self.pos += 1;
    }

    // スコープから抜ける処理
    fn out_scope(&mut self) {
        // スコープから抜けるのはデクリメントのみでOK
        self.pos -= 1;
    }

    pub fn run(&mut self, ast: AST) {
        // in_scope が増えた
        self.in_scope();
        for stmt in ast.0 {
            self.run_stmt(stmt);
        }
        // out_scope が増えた
        self.out_scope();
    }

    fn add_to_scope(&mut self, name: String, expr: Expr) {
        // 現在のスコープに変数を入れる
        let pos = self.pos - 1;
        self.symbol_tables[pos].insert(name, expr);
    }

    fn find_scope(&self, name: &str) -> Expr {
        let pos = self.pos;
        // 一番内側(現在)のスコープから外側のスコープの順に調べていく。
        // `[0..pos]` や `rev()` に注意。
        for table in self.symbol_tables[0..pos].iter().rev() {
            if let Some(e) = table.get(name) {
                return e.clone()
            }
        }
        // どこにもなければ未定義変数
        panic!("reference to unknown variable")
    }

    fn run_stmt(&mut self, stmt: Stmt) {
        use Stmt::*;

        match stmt {
            // ..
            Block(stmts) => {
                // in_scope が増えた
                self.in_scope();
                for stmt in stmts {
                    self.run_stmt(stmt);
                }
                // out_scope が増えた
                self.out_scope();
            },
        }

    }

    // ..
}
```

これで走らせてみます。

```
$ cargo run
x = 1
y = 2
--
x = 3
y = 2
--
x = 1
y = 2
```

はい。動きました。

# `Scope` の導入

さて、上のコードでは`in_scope`と`out_scope`が対で現れました。というか現れないといけません。
でもプログラミングエラーで`out_scope`を忘れるなんてことはよくある話です（現に私はこのサンプルを書いてて間違えかけました）。これをRustのライフタイムの仕組みに乗っけて自動で解決しようというのが今回のお話。

手法は簡単で、とりあえず`Scope`という型を用意します。そして`new`する時に`in_scope`を呼んであげて、`drop`する時に`out_scope`を呼んであげます。

``` rust
use std::ops::Drop;
struct Scope<'a>(&'a mut Interpreter);

impl <'a>Scope<'a> {
    fn new(inpr: &'a mut Interpreter) -> Self {
        inpr.in_scope();
        Scope(inpr)
    }
}

impl <'a>Drop for Scope<'a> {
    fn drop(&mut self) {
        self.0.out_scope()
    }
}
```

さて、この`&mut Interpreter`で参照しているというのが丁度良くて、Scopeの分岐というのを防ぐことが出来ます。こういうやつですね。

```
let scope1 = Scope::new(&mut intr);
let scope2 = Scope::new(&mut intr); // error: 既に&mut されている変数は&mut出来ない
```

では、これを使ってみましょう。

``` rust
    pub fn run(&mut self, ast: AST) {
        // in_scopeをScope::new()に置き換えた
        let scope = Scope::new(self);
        for stmt in ast.0 {
            // scopeに渡してしまったのでselfはsope.0で参照する必要がある
            scope.0.run_stmt(stmt);
        }
        // out_scopeは自動で呼ばれる
    }

    // run_stmtのBlockも同様に書き換える
```

```
$ cargo run
x = 1
y = 2
--
x = 3
y = 2
--
x = 1
y = 2
```

動きました。でも、あまり`scope.0`なんて使いたくないですよね。出来れば透過的に扱いたい。
そんなときのために`Deref` と `DerefMut` です。Rustは型が合わなければ自動で`deref`を呼んでくれます。

`Deref` と `DerefMut` を実装しましょう。

``` rust
use std::ops::{Deref, DerefMut};
impl <'a> Deref for Scope<'a> {
    type Target = Interpreter;
    fn deref(&self) -> &Interpreter {
        self.0
    }
}

impl <'a> DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Interpreter {
        self.0
    }
}

impl Interpreter {
    // ..

    pub fn run(&mut self, ast: AST) {
        // deref_mutするために mutをつける
        let mut scope = Scope::new(self);
        for stmt in ast.0 {
            // scope.0しなくてもInterpreterのメソッドが呼べる
            scope.run_stmt(stmt);
        }
    }

   // ..
}
```

# まとめ

* 生成と消滅が対になるものはRustのオブジェクトで管理出来るよ
* シンプルなラッパー型は`Deref`や`DerefMut`を実装しておくと便利だよ

# 参考

* [Drop](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/drop.html)
* [`Deref` による型強制](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/deref-coercions.html)


# 付録A
最終的なソースコード全文を掲載します。


``` rust
#[derive(Debug, Clone)]
struct AST(Vec<Stmt>);

#[derive(Debug, Clone)]
enum Stmt {
    Subst(String, Expr),
    Print(Vec<Expr>),
    Block(Vec<Stmt>),
}


#[derive(Debug, Clone)]
enum Expr {
    Str(String),
    Num(isize),
    Var(String),
}


use std::collections::HashMap;
struct Interpreter {
   symbol_tables: Vec<HashMap<String, Expr>>,
   pos: usize,
}

use std::ops::{Drop, Deref, DerefMut};
struct Scope<'a>(&'a mut Interpreter);

impl <'a>Scope<'a> {
    fn new(inpr: &'a mut Interpreter) -> Self {
        inpr.in_scope();
        Scope(inpr)
    }
}

impl <'a>Drop for Scope<'a> {
    fn drop(&mut self) {
        self.0.out_scope()
    }
}

impl <'a> Deref for Scope<'a> {
    type Target = Interpreter;
    fn deref(&self) -> &Interpreter {
        self.0
    }
}

impl <'a> DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Interpreter {
        self.0
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter{
            symbol_tables: Vec::new(),
            pos: 0,
        }
    }

    fn in_scope(&mut self) {
        let pos = self.pos;
        if self.symbol_tables.len() <= pos {
            self.symbol_tables.push(HashMap::new())
        } else {
            self.symbol_tables[pos - 1].clear()
        }
        self.pos += 1;
    }

    fn out_scope(&mut self) {
        self.pos -= 1;
    }

    pub fn run(&mut self, ast: AST) {
        let mut scope = Scope::new(self);
        for stmt in ast.0 {
            scope.run_stmt(stmt);
        }
    }

    fn add_to_scope(&mut self, name: String, expr: Expr) {
        let pos = self.pos - 1;
        self.symbol_tables[pos].insert(name, expr);
    }

    fn find_scope(&self, name: &str) -> Expr {
        let pos = self.pos;
        for table in self.symbol_tables[0..pos].iter().rev() {
            if let Some(e) = table.get(name) {
                return e.clone()
            }
        }
        panic!("reference to unknown variable")
    }

    fn run_stmt(&mut self, stmt: Stmt) {
        use Stmt::*;

        match stmt {
            Subst(name, expr) => {
                let expr = self.eval(expr);
                self.add_to_scope(name, expr);
            },
            Print(exprs) => {
                for expr in exprs {
                    self.print_expr(expr);
                }
                println!("");
            },
            Block(stmts) => {
                let scope = Scope::new(self);

                for stmt in stmts {
                    scope.0.run_stmt(stmt);
                }
            },
        }

    }

    fn eval(&self, expr: Expr) -> Expr {
        use Expr::*;
        match expr {
            Var(v) => self.find_scope(&v),
            e @ Str(_) |
            e @ Num(_) => e
        }
    }

    fn print_expr(&self, expr: Expr) {
        use Expr::*;
        match self.eval(expr) {
            Str(ref s) => print!("{}", s),
            Num(ref n) => print!("{}", n),
            Var(_) => panic!("reference to unknown variable") ,
        }
    }
}

fn main() {
    let ast = {
        use Stmt::*;
        use Expr::*;
        AST(
            vec![
                Subst("x".to_string(), Num(1)),
                Subst("y".to_string(), Num(2)),
                Print(vec![Str("x = ".to_string()), Var("x".to_string())]),
                Print(vec![Str("y = ".to_string()), Var("y".to_string())]),
                Print(vec![Str("--".to_string())]),
                Block(vec![
                    Subst("x".to_string(), Num(3)),
                    Print(vec![Str("x = ".to_string()), Var("x".to_string())]),
                    Print(vec![Str("y = ".to_string()), Var("y".to_string())]),
                ]),
                Print(vec![Str("--".to_string())]),
                Print(vec![Str("x = ".to_string()), Var("x".to_string())]),
                Print(vec![Str("y = ".to_string()), Var("y".to_string())]),
            ]
        )
    };
    let mut interpreter = Interpreter::new();

    interpreter.run(ast);
}
```

