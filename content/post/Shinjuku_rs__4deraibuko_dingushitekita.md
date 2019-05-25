---
categories: [Rust, Shinjuku.rs]
date: 2019-05-22T00:04:52+09:00
title: "Shinjuku.rs #4 でライブコーディングしてきた"
---
κeenです。勉強会参加報告を書くのも久しぶりですね。[RustのLT会 Shinjuku.rs #4 @FORCIA ](https://forcia.connpass.com/event/124988/)でライブコーディングしてきたのでそれを書き留めておきます。

<!--more-->

当日になって発表枠が2つ分くらい余ってるとのことだったので尺とりそうなライブコーディングをしてきました。
題材は拙著(共著)[実践Rust入門](https://gihyo.jp/book/2019/978-4-297-10559-4)の「9章 パーサを作る」より。
9章の締めの文にはこう書かれています。

> ここで構成したインタプリタは簡素なものではありますが、骨子は同じまま複雑な処理をするインタプリタへの応用もできるでしょう。変数、関数、制御構造、ユーザ定義型、と徐々に拡張していけば 1 つ 1 つのステップは難しくないでしょう。

これを証明するために変数の追加をライブコーディングしました。何の準備もないぶっつけ本番のやつです。あまり褒められたものではないですね。

9章を通して作るコードはGitHubで[公開されている](https://github.com/ghmagazine/rustbook)のでこれをベースに作業をします。

# 元の電卓と新しい仕様

元のコードは以下のようにして式を打ち込むとパースして計算して結果を出してくれる対話環境です。

```console
> 1 + 1
Annot { value: BinOp { op: Annot { value: Add, loc: Loc(2, 3) }, l: Annot { value: Num(1), loc: Loc(0, 1) }, r: Annot { value: Num(1), loc: Loc(4, 5) } }, loc: Loc(0, 5) }
2
1 1 +
```

デバッグプリントが出てますが気にせず進めます。2行目にちゃんと `2` と出ているのが見えるかと思います。こうやって結果を出してくれます。

これにRust風の `let <変数> = <式>;` を加えるというのが今回の題材です。

# レキサまで

新しく `let` 、変数、 `=` 、 `;` がトークンに加わります。これはそのまま `TokenKind` に加えればよいでしょう。

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TokenKind {
    // ...
    /// let
    Let,
    /// =
    Equal,
    /// ;
    Semicolon,
    /// x
    Var(String),
}

```

`Var` の引数に `Stringが` 入った影響で `Copy` が自動導出から消えてます。
この定義変更から直ちに作業的にヘルパ関数ができあがります。

```rust
impl Token {
    // ...

    fn let_(loc: Loc) -> Self {
        Self::new(TokenKind::Let, loc)
    }

    fn equal(loc: Loc) -> Self {
        Self::new(TokenKind::Equal, loc)
    }

    fn semicolon(loc: Loc) -> Self {
        Self::new(TokenKind::Semicolon, loc)
    }

    fn var(s: impl Into<String>, loc: Loc) -> Self {
        Self::new(TokenKind::Var(s.into()), loc)
    }
}
```

さて、これからレキサを書いていきますが、その前にテストを準備しましょう。ライブコーディング中はロケーション情報の修正を忘れてたりしましたが、このコードは修正後のものです。

``` rust
#[test]
fn test_lexer2() {
    assert_eq!(
        lex("let x = 1;"),
        Ok(vec![
            Token::let_(Loc(0, 3)),
            Token::var("x", Loc(4, 5)),
            Token::equal(Loc(6, 7)),
            Token::number(1, Loc(8, 9)),
            Token::semicolon(Loc(9, 10)),
        ])
    )
}

```

これでレキサを書き始められます。
まずはエントリポイントの `lex` 関数に処理を加えましょう。今回加わったのは `=` 、 `;` 、 `let` 、 変数ですが、変数と `let` は最初の文字が被ってる可能性があるので一緒に処理します。なので都合3つの節が新たに加わります。
ここでaからzまでの範囲の記法を間違えて会場に助けてもらったりしてました。

``` rust
/// 字句解析器
fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    // ...
    while pos < input.len() {
        // ここでそれぞれの関数に`input`と`pos`を渡す
        match input[pos] {
            // ...
            b'=' => lex_a_token!(lex_equal(input, pos)),
            b';' => lex_a_token!(lex_semicolon(input, pos)),
            // ...
            b'a'...b'z' => lex_a_token!(lex_str(input, pos)),
            // ...
        }
    }
    Ok(tokens)
}

```

このうち `=` と `;` はそのまま実装が決まります。

``` rust
fn lex_equal(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'=').map(|(_, end)| (Token::equal(Loc(start, end)), end))
}
fn lex_semicolon(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b';').map(|(_, end)| (Token::semicolon(Loc(start, end)), end))
}
```

変数または `let` は複数のトークンを読む必要があるので、 `lex_number` をコピペして作りましょう。
ひとまずアルファベットの塊を抜き出してみて `let` だったら `let` キーワードに、それ以外だったら変数にするようにします。

``` rust
fn lex_str(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;

    let start = pos;
    let end = recognize_many(input, start, |b| b"abcdefghijklmnopqrstuvwxyz".contains(&b));
    let s = from_utf8(&input[start..end])
        // start..posの構成から `from_utf8` は常に成功するため`unwrap`しても安全
        .unwrap();
    if s == "let" {
        Ok((Token::let_(Loc(start, end)), end))
    } else {
        Ok((Token::var(s, Loc(start, end)), end))
    }
}

```

これでレキサ部分は実装を終えましたが後の方でコンパイルエラーが出ているので直します。
`TokenKind` のDisplayに新たに加えたものの実装を足します。この実装不足をコンパイラが教えてくれるのはいい所ですね。

``` rust
impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenKind::*;
        match self {
            // ...
            Let => write!(f, "let"),
            Semicolon => write!(f, ";"),
            Equal => write!(f, "="),
            Var(s) => write!(f, "{}", s),
        }
    }
}

```

そして `TokenKind` から `Copy` を消した影響で `parse_expr1` に変更が必要になります。1行目の `tok.value` に `clone` がつきます。


``` rust
// expr1
fn parse_expr1<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek().map(|tok| tok.value.clone()) {
    ...
}
```

これでテストが通るようになった(会場ではテストのロケーションの修正をやった)のでレキサは完成です。

``` console
test test_lexer2 ... ok
```

# パーサ
パーサも作業の流れはレキサと同じです。 `let` 文を表わすバリアントを追加します。

``` rust
/// ASTを表すデータ型
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AstKind {
    // ...
    /// Let文
    Let { var: String, body: Box<Ast> },
}

// ヘルパメソッドを定義しておく
impl Ast {
    fn let_(var: String, body: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::Let {
                var,
                body: Box::new(body),
            },
            loc,
        )
    }
}
```

そして対応するテストを追加します。

``` rust
#[test]
fn test_parser2() {
    // let x = 1;
    let ast = parse(vec![
        Token::let_(Loc(0, 3)),
        Token::var("x", Loc(4, 5)),
        Token::equal(Loc(6, 7)),
        Token::number(1, Loc(8, 9)),
        Token::semicolon(Loc(9, 10)),
    ]);
    assert_eq!(
        ast,
        Ok(Ast::let_(
            "x".to_string(),
            Ast::num(1, Loc(8, 9)),
            Loc(0, 10)
        ))
    )
}
```

テストができました。パーサを作りましょう。

いままでは式だけでしたが、`let` は文なので新たに `parse_stmt` という関数を追加します。これは `parse_expr1` からコピペして作ったと思います。長いですが `let` 、 変数 、 `=` 、 式 、 `;` を順番に取り出してるだけです。もし先頭が `let` でなかったら `parse_expr3` のパースにフォールバックします。

``` rust
fn parse_stmt<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek().map(|tok| tok.value.clone()) {
        Some(TokenKind::Let) => {
            // ("+" | "-")
            let loc_start = match tokens.next() {
                Some(Token {
                    value: TokenKind::Let,
                    loc,
                }) => loc,
                _ => unreachable!(),
            };
            let var = match tokens.next() {
                Some(Token {
                    value: TokenKind::Var(s),
                    loc,
                }) => s,
                _ => unreachable!(),
            };
            match tokens.next() {
                Some(Token {
                    value: TokenKind::Equal,
                    loc,
                }) => (),
                _ => unreachable!(),
            };
            let body = parse_expr(tokens)?;
            let loc_end = match tokens.next() {
                Some(Token {
                    value: TokenKind::Semicolon,
                    loc,
                }) => loc,
                _ => unreachable!(),
            };
            let loc = loc_start.merge(&loc_end);
            //let loc = op.loc.merge(&e.loc);
            Ok(Ast::let_(var, body, loc))
        }
        _ => parse_expr3(tokens),
    }
}

```

会場では `=` を取り出すのを忘れていました。また、 `unreachable` をところどころ使っていますがよく考えると先頭の `let` 以外は到達可能で、 unexpected tokenのエラーを出すべきですね。
煩雑なので `consume_token` 的な関数を定義しておけばよかったんですがアドリブではそこまで機転が利きませんでした。

`loc` のところでコメントアウトが見られますが最初混乱して書けなくて後回しにしたせいです。


レキサと同じくこれで動作はしますが、後の方でコンパイルエラーが発生してるので直します。 `Interpreter` の `eval` です。ここでは `unimplemented` を使ってコンパイルを通します。


``` rust
impl Interpreter {
    // ...
    pub fn eval(&mut self, expr: &Ast) -> Result<i64, InterpreterError> {
        use self::AstKind::*;
        match expr.value {
            // ...
            Let { .. } => unimplemented!()
        }
    }

```

同様に、 `RpnCompiler` でもエラーが出ますが、 `unimplemented!()` にします。


これでテストを走らせてみましょう。

```console
---- test_parser2 stdout ----
thread 'test_parser2' panicked at 'assertion failed: `(left == right)`
  left: `Err(NotExpression(Annot { value: Let, loc: Loc(0, 3) }))`,
 right: `Ok(Annot { value: Let { var: "x", body: Annot { value: Num(1), loc: Loc(8, 9) } }, loc: Loc(0, 10) })`', src/main.rs:658:5
note: Run with `RUST_BACKTRACE=1` environment variable to display a backtrace.

```

おや、エラーになります。。どうやら `let` のパースに失敗してます。よく考えたら `parse_stmt` を定義したいいですが、使うのを忘れてました。 `parse` 関数内で `parse_expr` の代わりに `parse_stmt` を呼ぶようにしましょう。


``` rust
fn parse(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    // 入力をイテレータにし、 `Peekable` にする
    let mut tokens = tokens.into_iter().peekable();
    // その後 `parse_expr` を呼んでエラー処理をする
    let ret = parse_stmt(&mut tokens)?;
    match tokens.next() {
        Some(tok) => Err(ParseError::RedundantExpression(tok)),
        None => Ok(ret),
    }
}

```

今度こそ。

``` console
test test_parser2 ... ok
```

通りました。会場では `=` の処理を忘れていたり、ロケーション情報を修正したりしましたが概ねこのまま動きました。

# インタプリタ
RPNコンパイラはそもそもバックエンドの実行器が変数に対応してないのでインタプリタを作ります。
`Interpreter` 構造体に変数と値の対応を保持するハッシュマップを持っておきましょう。


``` rust
use std::collections::HashMap;

/// 評価器を表すデータ型
struct Interpreter(HashMap<String, i64>);

impl Interpreter {
    pub fn new() -> Self {
        Interpreter(HashMap::new())
    }
}
```

これであとは `eval` の `unimplemented` の部分を埋めるだけです。
が、文の値というものがここで発生します。普通は文の値は `void` ですね。
綺麗にやるなら `enum Value {Int(i64), Void}` を定義するところですが、時間がないので `0` を返すことにしました。
会場ではライフタイムエラーなどありましたがこのコードが出来上がりました。

``` rust
    pub fn eval(&mut self, expr: &Ast) -> Result<i64, InterpreterError> {
        use self::AstKind::*;
        match expr.value {
            Let {
                ref var, ref body, ..
            } => {
                let e = self.eval(body)?;
                self.0.insert(var.clone(), e);
                Ok(0)
            }
```


これで動くのですが、RPNコンパイラを実装してないのでそれはコメントアウトしておきましょう。

``` rust
fn main() {
    // ...
        // let rpn = compiler.compile(&ast);
        // println!("{}", rpn);
}

```


それでは試してみます。

``` console
> let x = 1;
Annot { value: Let { var: "x", body: Annot { value: Num(1), loc: Loc(8, 9) } }, loc: Loc(0, 10) }
0
```

ちゃんと0が返ってきました。めでたしめでたし。ここまで20分ほどだったようです。

…と会場では締めたのですがこれでは変数を定義するだけで利用できないですね。ついでなのでそれも実装しましょう。

# 変数の参照
懇親会中に一人でやってました。レキサは変数を既に扱えるのでパーサ以降をいじります。
ASTにバリアントを追加してヘルパ関数を定義。

``` rust
/// ASTを表すデータ型
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AstKind {
    // ...
    /// 変数
    Var(String),
}

// ヘルパメソッドを定義しておく
impl Ast {
    // ..
    fn var(var: String, loc: Loc) -> Self {
        Self::new(AstKind::Var(var), loc)
    }
}
```

変数はアトムなので `parse_atom` をちょこちょこっといじります。

``` rust
fn parse_atom<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            // ...
            // Var
            TokenKind::Var(s) => Ok(Ast::new(AstKind::Var(s), tok.loc)),
            // ...
        })
}

```

インタプリタに取り掛かりますが、その前に存在しない変数の参照はエラーなので `InterpreterErrorKind` を増やします。 `description` の実装要らない気がするけどなんで残ってるんだろう。

``` rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum InterpreterErrorKind {
    DivisionByZero,
    UnboundVariable(String),
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::InterpreterErrorKind::*;
        match self.value {
            DivisionByZero => write!(f, "division by zero"),
            UnboundVariable(ref v) => write!(f, "variable {} is not bound", v),
        }
    }
}

impl StdError for InterpreterError {
    fn description(&self) -> &str {
        use self::InterpreterErrorKind::*;
        match self.value {
            DivisionByZero => "the right hand expression of the division evaluates to zero",
            UnboundVariable(_) => "variable is not bound",
        }
    }
}

```

それではインタプリタをば。 ハッシュマップからの `get` で `Option` が返るので `ok_or` で `None` を `UnboundVariable` エラーに変換して返ります。

``` rust
    pub fn eval(&mut self, expr: &Ast) -> Result<i64, InterpreterError> {
        use self::AstKind::*;
        match expr.value {
            // ...
            Var(ref s) => self.0.get(s).cloned().ok_or(InterpreterError::new(
                InterpreterErrorKind::UnboundVariable(s.clone()),
                expr.loc.clone(),
            )),
        }
    }

```

RPNコンパイラの方は相変わらず `unimplemented` のスタブを刺しておいて下さい。

これで実行できます。

``` console
> let x = 1 + 1;
Annot { value: Let { var: "x", body: Annot { value: BinOp { op: Annot { value: Add, loc: Loc(10, 11) }, l: Annot { value: Num(1), loc: Loc(8, 9) }, r: Annot { value: Num(1), loc: Loc(12, 13) } }, loc: Loc(8, 13) } }, loc: Loc(0, 14) }
0
> x + x * 2
Annot { value: BinOp { op: Annot { value: Add, loc: Loc(2, 3) }, l: Annot { value: Var("x"), loc: Loc(0, 1) }, r: Annot { value: BinOp { op: Annot { value: Mult, loc: Loc(6, 7) }, l: Annot { value: Var("x"), loc: Loc(4, 5) }, r: Annot { value: Num(2), loc: Loc(8, 9) } }, loc: Loc(4, 9) } }, loc: Loc(0, 9) }
6
> x + y
Annot { value: BinOp { op: Annot { value: Add, loc: Loc(2, 3) }, l: Annot { value: Var("x"), loc: Loc(0, 1) }, r: Annot { value: Var("y"), loc: Loc(4, 5) } }, loc: Loc(0, 5) }
variable y is not bound
x + y
    ^
variable y is not bound
```

変数の扱いもunbound variableも扱えます。めでたしめでたし。
