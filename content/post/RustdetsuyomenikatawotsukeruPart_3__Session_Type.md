---
categories: ["Rust", "Advent Calendar", "Advent Calendar 2018", "Rustで強めに型をつける", "Rust Advent Calendar"]
date: 2018-12-15T05:48:48+09:00
title: "Rustで強めに型をつけるPart 3: Session Type"
---
このエントリは[Rustその2 Advent Calendar 2018](https://qiita.com/advent-calendar/2018/rust2) 8日目の記事を時空を遡って書いています。

κeenです。寝れないので空いてる日の分を埋めに行きます。次はSession Type。完全に趣味に走ったので実用性はないです。

<!--more-->

前回のType Level State Machineの発展形のようなものです。通信に型を付けます。
まず、ステートマシンは概ねオートマトンくらいの表現力があります。パッと考えて連接、選択、繰り返しはできそうですね。前回の例で連接と繰り返しはやったのであとはメソッドを複数用意して分岐できるようにしたら選択もできそうです。
これを双方向通信にも応用しようとすると少し複雑さが増します。
通信になると送信/受信の区別が必要になるのと、「選択」が送信か受信かで扱いが変わるためです。「選択」は送信側にとってはどちらか好きな方を選べます(積極的選択)が受信側にとってはどちらが来るかわからない(消極的選択)からどちらにも対応できるようにしなければなりません。あ、あと通信終了も必要です。

まとめると、以下のプリミティブが存在します。

* 送信を表す `Send`
* 受信を表す `Recv`
* 積極的選択を表す `Choose`
* 消極的選択を表す `Offer`
* ループ(再帰)を表す `Rec` 。これはラベル相当。
* ループ(再帰)を表す `Var` 。これはジャンプ相当。
* 通信の終了(ε)を表す `Eps`

これらを組み合わせると自由に通信を記述できます。
例えば「数値を受け取って真偽値を返す」通信はこう書けます。

```rust
Recv<i64, Send<bool, Eps>>

```

また、サーバとクライアントの型は一対一に対応するので片方の型が決まったら自動的にもう片方の型も決まります。今回はこうですね。

```rust
Send<i64, Recv<bool, Eps>>
```


それではではライブラリを使って遊んでみましょう。
[`session_types`](https://crates.io/crates/session_types) を使います。
「数値を送って100以上だったらtrueを返してさらに数値を受け取る(以下ループ)、100未満だったらfalseを返しそこで通信終了」を実装してみましょう。


```
サーバ:
 +------------------+
 |                  |
 V   <i64     >bool |
(S)------>[]--------+-->(E)


クライアント:
 +------------------+
 |                  |
 V   >i64     <bool |
(S)------>[]--------+-->(E)

```

選択の消極/積極が表れてませんがこういうステートマシンです。
これを型に起こします。



```rust
use session_types::*;

type Server = Rec<Recv<i64, Send<bool, Choose<Var<Z>, Eps>>>>;
```

終了するかはサーバが選びます。
クライアントの型は自動的に求められます。

``` rust
type Client = <Server as HasDual>::Dual;
```

まあ、双対になりますよね。


これに基づいたサーバの実装はこれです。`sel1`、`sel2`などと積極的選択をしているのが分かるかと思います。

```rust
fn srv(c: Chan<(), Server>) {
    let mut c = c
        // Rec<Recv<i64, Send<bool, Choose<Var<Z>, Eps>>>> -> Recv<i64, Send<bool, Choose<Var<Z>, Eps>>>
        // 環境にラベルを追加
        .enter();
    loop {
        let (c_, n) = c.
            // Recv<i64, Send<bool, Choose<Var<Z>, Eps>>> -> Send<bool, Choose<Var<Z>, Eps>>
            recv();
        if 100 <= n {
            c = c_
                // Send<bool, Choose<Var<Z>, Eps>> -> Choose<Var<Z>, Eps>
                .send(true)
                // Choose<Var<Z>, Eps> -> Var<Z>
                .sel1()
                // Var<Z> -> Recv<i64, Send<bool, Choose<Var<Z>, Eps>>>
                .zero();
        } else {
            c_
                // Send<bool, Choose<Var<Z>, Eps>> -> Choose<Var<Z>, Eps>
                .send(false)
                // Choose<Var<Z>, Eps> -> Eps
                .sel2()
                // Eps → 終了
                .close();
            break;
        }
    }
}
```

クライアントはこうです。終了するかどうかはサーバが選ぶのでクライアント側にはサーバの判断を確認する分岐が必要になります。


``` rust
fn cli(c: Chan<(), Client>) {
    let mut c = c
        // Rec<Send<i64, Recv<bool, Offer<Var<Z>, Eps>>>> -> Send<i64, Recv<bool, Offer<Var<Z>, Eps>>>
        // 環境にラベルを追加
        .enter();
    for n in &[101, 100001, 42, 200] {
        let c_ = c
            // Send<i64, Recv<bool, Offer<Var<Z>, Eps>>> -> Recv<bool, Offer<Var<Z>, Eps>>
            .send(*n);
        let (c_, b) = c_
            // Recv<bool, Offer<Var<Z>, Eps>> -> Offer<Var<Z>, Eps>
            .recv();
        if b {
            println!("{} is big", n);
        } else {
            println!("{} is small", n);
        }
        c = match c_
            // Offer<Var<Z>, Eps> ->
            .offer()
        {
            // Var<Z>
            Left(c) => c
                // Var<Z> -> Send<i64, Recv<bool, Offer<Var<Z>, Eps>>>
                .zero(),
            // Eps
            Right(c) => {
                println!("server stopped");

                c.close();
                break;
            }
        };
    }
}
```

あとはmainを書いてあげれば完成です。

``` rust
use std::thread;
fn main() {
    let (server_chan, client_chan) = session_channel();

    let srv_t = thread::spawn(move || srv(server_chan));
    let cli_t = thread::spawn(move || cli(client_chan));

    let _ = (srv_t.join(), cli_t.join());
}

```

さらっとしか触れませんでしたが1つのチャネルで`int`と`bool`を混ぜて通信しています。
普通にやるとunsafeになりそうですがsession typeをつけることでAPI上はunsafeが現れずにプログラミングできました。

session typeすごい。
