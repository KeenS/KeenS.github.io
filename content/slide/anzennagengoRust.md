---
categories: [Rust]
date: 2020-06-25T09:31:42+09:00
description: "フィンテックエンジニア養成勉強会9での発表用。Rustの紹介。"
title: "「安全」な言語Rust"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# 安全な言語Rust
----------------------
[【オンライン】フィンテックエンジニア養成勉強会9（最新技術特集） - connpass](https://fintech-engineer.connpass.com/event/178015/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa2_vest.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# Rustとは？
------------

* 2015年にリリースされた **システムプログラミング言語**
  + CとかC++とかのライバル
* MozillaやAWS、Microsoft Azureなどが[支援](https://www.rust-lang.org/sponsors)
* 安全なシステムプログラミング言語として注目
* 5年連続で[最も愛された言語](https://insights.stackoverflow.com/survey/2020)

===

# Rustの利用事例
----------------

* Firefox: レンダラの[WebRenderer](https://github.com/servo/webrender)（a.k.a. Quantum Renderer）やCSSエンジンの[Stylo](https://hacks.mozilla.org/2017/08/inside-a-super-fast-css-engine-quantum-css-aka-stylo/)プロジェクト（a.k.a. Quantum CSS）
* AWS: Lambda/Fargateのランタイムの[Firecraker](https://github.com/firecracker-microvm/firecracker/)
* Microsoft: Azure IoTの[エッジセキュリティデーモン](https://msrc-blog.microsoft.com/2019/09/30/building-the-azure-iot-edge-security-daemon-in-rust/)
* 暗号通貨: Ethereumクライアントの[OpenEthereum](https://github.com/openethereum/openethereum)

===

# 何故Rust?
-----------

* 今までなかった新しいシステムプログラミング言語
* 安全性が特徴
* 所有権などの新しい概念を導入
* 関数型言語の特徴を取り入れた


===
# Rustのパフォーマンス
----------------------

* 基本性能はC/C++と[同等](https://benchmarksgame-team.pages.debian.net/benchmarksgame/which-programs-are-fastest.html)
* 所有権のおかげで自動でメモリ管理をするが、GCがない
* ゼロコスト抽象化のおかげで綺麗なコードがそのまま速い

===
# 例1 平均二乗誤差
-----

平均二乗誤差 [[asm](https://gcc.godbolt.org/z/ffwvT2)]

``` rust
// データの参照 `&` をとれる
fn mean_square_error(v1: &[f32], v2: &[f32]) -> f32 {
    let len = v1.len() as f32;
    // 繰り返し処理はイテレータで一発
    let square_sum = v1
        .iter()
        .zip(v2)
        .map(|(x1, x2)| (x1 - x2).powi(2))
        .sum::<f32>();
    square_sum / len
}
```

===
# Rustの安全性
--------------

* 一言で言ってしまえば未定義動作が起きない
  + ヌルポ、SEGV、境界外アクセスなど
* ポインタでトラブルが起きない設計
  + [「Chrome」の深刻なセキュリティ脆弱性、70％はメモリー安全性の問題 - ZDNet Japan](https://japan.zdnet.com/article/35154338/)
* 強い静的型で不正な入力はコンパイルエラー

===

# 例2 参照
-----

これはちゃんとコンパイルエラー [[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1c348e83914bc400a7dcab4b136e5549)]

``` rust
fn int_ref() -> &i32 {
    // ローカル変数を用意。
    // スコープは関数末尾で終わる
    let i = 1;
    // ローカル変数の参照を返す
    // （dangling pointer）
    &i
}
```

===
# 例3 RAII
----

* Cの `malloc` だと返り値は `void *`
  + `struct point *` や `double *` など好きなデータにキャストできてしまう
    ``` c
    void *ptr;
    struct point *data = (struct point*) ptr;
    double *d = (double*) ptr;
    ```
* `malloc` してから値を入れるまでに、有効な値が入ってない期間がある
* `malloc` した値を `free` し忘れたり、 `free` したあとに参照してしまう問題がある


===
# 例3 RAII
----

RAII [[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=7ddb0fb13500f2e181be913869aa64dc)]

``` rust
struct Point(i32, i32);
{
    // メモリ確保は値による初期化と一緒
    // `data` は `Box::<Point>` 型
    let data = Box::new(Point(0, 0));
    // スコープを抜けると確保したメモリは開放される
}
```

===
# 所有権
---------

* 値にはただ1つの所有者がいる
* 所有者がいなくなると値は破棄される
  + このおかげでGCレス
  + ものによってはデストラクタが走る
* 値の所有者を移動（ムーブ）することができる
* 値は参照で貸し借りできる
* 所有権をスレッド間でやりとりできる

===
# 例4 所有権
-----

所有権と開放 [[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=2898bcb8d4ce3104672418dcf6f7fa3b)]

``` rust
// data は Stringの所有者
let data = "owned".to_string();
// Stringの所有権がdata2にムーブした
let data2 = data;

// ムーブ済みの変数にアクセスするとエラー
println!("{}", data);
```

===
# 例5 ライフタイム
-----------------

* 多くのGC付き言語は値の破棄をGCに任せる
* でもファイルディスクリプタなどの破棄は手でやる
  + GCでやるとタイミングが遅い
* 手でやるとバグるので専用構文がある
* でも完全じゃない

===
# 例5 ライフタイム
-----------------

これはエラー

``` ruby
file = nil
File.open("file.rb") do |f|
  file = f
end
puts file.read
```


===
# 例5 ライフタイム
-----------------

値の開放はライフタイムに基く [[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=80c761adee6b5eeeaa37af730934a8d1)]

``` rust
let file;
{
    let tmp = File::open("hoge.txt")?;
    // fileに`File`をムーブ
    file = tmp
}
// ここでfileは有効

// 末尾でfileのライフタイムが終わる
// それと同時にFileが閉じられる
```

===
# 例6 並行と所有権
-----------------

[[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=12c992d81dcab3a16a024712b06d8c72)]
``` rust
let mut data = "single thread".to_string();
// 別スレッドに `data` を送る
let handle = thread::spawn(move || {
    // スレッドを使いつつ値を変更しているが、
    // マルチスレッドバグは起きない
    data.push_str(" in another thread");
    println!("{}", data);
});
// `data` の所有権を別スレッドに送ったので
// `data` にアクセスできない
// data.push_str("hoge")
handle.join();
```


===
# 関数型言語の機能
------------------

* Rustは堅牢性が高いと言われる関数型言語の機能を取り入れた
* デフォルトイミュータブル
* 型推論
* 代数的データ型とパターンマッチ
* などなど

===
# 型推論
--------

* 型を書かなくても推論してくれる仕組み
  + ただしRustは設計上関数の型はユーザに書かせる
* だたの略記だけではなく「逆向き」の推論ができる

===
# 例7 型推論
------------

[[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=9960dde1904599b960a78f4afc068e0d)]
``` rust
fn bind(addr: IpAddr) { /* do something */}

// この時点では何にパースするか分からない
let addr = "127.0.0.1".parse().unwrap();

// 後で `IpAddr` 型として使われているので `IpAddr` 型として
// パースすべきであることが分かる
bind(addr);
```

===
# 例8 パターンマッチ
-------------------

値の生成と同じ構文で分解できる [[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1a6d1c7bd8d9543b54722f0a366a7b95)]

``` rust
enum Command {
    Move(u32),
    Left,
    Right,
}
use Command::*;
let commands = vec![Left, Move(10), Right, Move(5)];
for command in commands {
    match command {
        Move(amount) => println!("move {}", amount),
        Left => println!("Turn left"),
        Right => println!("Turn right"),
    }
}
```

===
# 例9 代数的データ型
-------------------

* Rustに例外はない
* 組み込みのNullable型もない
* どちらも代数的データ型で表現できる
  + 標準ライブラリで定義

``` rust
enum Option<T> {
    Some(T),
    None
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

===
# 例9 代数的データ型
-------------------

``` rust
struct DivByZero;
let x = 3;
let y = 0;

if y == 0 {
    Err(DivByZero)
} else {
    Ok(x/y)
}
```

===
# その他
---------

* インストーラやビルドツール、パッケージマネージャ完備
* ジェネリクス（パラメトリック多相）やトレイト（アドホック多相）
* わかりやすいエラーメッセージ
* 豊富なクレート（パッケージ）
* …などなど

===
# まとめ
---------

* Rustは新しいシステムプログラミング言語
* Rustはメモリエラーが起きないという意味で「安全」
* Rustは所有権などの目新しい概念を取り入れてる
* Rustはロジックエラーが起きづらいという意味で「安全」
* Rustは色々な言語から便利な機能を取り入れている
* Rustは意識外の例外が飛んできたりしないという意味で「安全」

</script>
</section>
