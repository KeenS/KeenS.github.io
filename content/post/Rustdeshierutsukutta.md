---
categories: [Rust, Shell]
date: 2016-09-04T16:26:20+09:00
title: Rustでシェル作った
---

κeenです。
先日、先輩社員と話してるとシェルを作る話になりました。
だいたいのコンピュータサイエンスの学生なら学部生の頃に課題でシェルを作りますが数学科にいた私は作ったことありませんでした。
でも、その時「今ならシェルなんて作ろうと思えばすぐに作れますよ」なんて言っちゃったのでなんか作りました。
まあ、習作程度の雑なものです。

<!--more-->
作ったのはこれ、[KeenS/igaguri: my toy shell written in Rust](https://github.com/KeenS/igaguri)。
名前は、Shellは木の実を覆う殻のようにカーネルを覆う殻とのことなのでRustのロゴに似てる毬栗を選びました。

# パーサ
パーサコンビネータライブラリの[nom](https://github.com/Geal/nom)を使いました。

雰囲気こんな感じ。

``` rust
named!(token<String>, map!(map_res!(is_a!("abcdefghijklmnopqlrstuvwxyzABCDEFGHIJKLMNOPQLRSTUVWXYZ-!$%^&@/1234567890"), str::from_utf8), |i: &str| i.to_string()));
named!(pipe, tag!("|"));
named!(redirect_out<String>, chain!(tag!(">") ~ opt!(multispace) ~ filename: token, || filename));

named!(command<Ast>, chain!(
    opt!(multispace)
        ~ cmd: separated_list!(multispace, token)
        ~ outfile: opt!(chain!(multispace ~ out: redirect_out, || out))
        ~ opt!(multispace), || Ast::Command{cmd: cmd, out: outfile}));
named!(parse<Ast>, map!(separated_list!(pipe, command), Ast::Pipe));

```

# 入力ハンドリング
readlineのRust実装、[rustyline](https://github.com/kkawakam/rustyline)を使いました。

それっぽいからとりあえず、で選んだものの、複数行入力（履歴）とか考えるともうちょっとリッチなものを選んだ方が良かったかもしれません。

# 実行
最初、[libcバインディング](https://github.com/rust-lang/libc)の `pipe` や `fork` 、 `execve` なんかを考えてましたが、標準ライブラリの [`std::process::Command`](https://doc.rust-lang.org/std/process/struct.Command.html)で実現出来そうだったのでそれを使いました。

こんな感じ。

``` rust
Command::new(cmd)
    .args(&terms)
    .stdin(stdin)
    .stdout(stdout)
    .stderr(stderr)
    .spawn()
```

## パイプ/リダイレクト

一応、パイプも標準ライブラリでサポートされてるのでそれを使いました。

ただ、普通のCとは違って子プロセスと親プロセスの間に一旦パイプを作って、子プロセスの出力の生のfdを取り出して、次の子プロセスの入力に与えて、と案外面倒でした。 `unsafe` も出てくれば `unreachable` も出てくるのでもう少し書き直したい。

``` rust
let mut si = stdin;
let mut itr = commands.into_iter().peekable();
unsafe {
    while let Some(command) = itr.next() {
        if itr.peek().is_some() {
            let process =
                try!(self.run(command, si, Stdio::piped(), Stdio::inherit()));
            si = Stdio::from_raw_fd(process.stdout.unwrap().into_raw_fd());
        } else {
            return self.run(command, si, stdout, stderr);
        }
    }
}
unreachable!()
```

リダイレクトも似たようなもので、ファイルを作って生のfdを取り出して、とやります。

``` rust
let stdout = out.map(|f| {
    unsafe {
        let file = File::create(f)
        // FIXME: do not panic
            .unwrap();
        Stdio::from_raw_fd(file.into_raw_fd())
    }

}).unwrap_or(stdout);
```

# おわりに

昨日の夜からちょろっと作り始めてさっきそれっぽく動くようになった程度なのでまだ適当にしか動きません。
パイプと標準出力のリダイレクトだけで、 `cd` なんかのシェルコマンドもなければシェル変数もありません。
が、そろそろ飽きたのでこの辺で。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">そろそろ飽きてきた</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/772334565264232454">2016年9月4日</a></
blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

シェルコマンドとシェル変数くらいはいつか実装しようかな。
