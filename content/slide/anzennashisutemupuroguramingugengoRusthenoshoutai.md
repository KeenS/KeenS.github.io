---
categories: ["Rust"]
date: 2019-11-18T15:28:52+09:00
description: "IIJ Labでの発表用。主にシステムプログラミング言語経験者向けの内容"
title: "安全なシステムプログラミング言語Rustへの招待"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# 安全なシステムプログラミング言語Rustへの招待
----------------------
[IIJ Labセミナー](https://iijlab-seminars.connpass.com/event/152079/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
 * [Idein Inc.](https://idein.jp/)のエンジニア
  + 2年半くらい仕事でRustを書いている
 * Lisp, ML, Rust, Shell Scriptあたりを書きます
 * [実践Rust入門](https://gihyo.jp/book/2019/978-4-297-10559-4)の共著者

===
# Rustって言語があるよ
---------------------

* システムプログラミング言語
* 2015年に1.0.0がリリース
 + リリース前に破壊的変更しまくったので1.0以降はかなり安定してる
 + 0.x時代はもちろんのこと、1.0-alphaのあとに1.0-alpha2が出たりもした
* 6週間毎にコンパイラのリリースで、今は1.39.0
* C++03, C++11 みたいなノリでRust 2015とRust 2018の2つの仕様がある
  + コンパイラはずっと両方サポート

===

# 事例
------

* [firecracker](https://github.com/firecracker-microvm/firecracker): AWS Lambdaのセキュアコンテナ
* [Magick Pocket](https://www.wired.com/2016/03/epic-story-dropboxs-exodus-amazon-cloud-empire/): DropBoxのストレージマネージャ。
* [Servo](https://servo.org/): Mozillaの新ブラウザエンジンの実験プロジェクト。一部の成果がFirefoxに反映されている。
* [Redox](https://redox-os.org/): OS
* [TiKV](https://github.com/tikv/tikv): KVS


===
# システムプログラミング言語っぽさ
-------------------------------

* ランタイムレス
  + でもメモリは自動管理
* Cと相互に連携できる
  + C FFIだけでなくCからRustも呼べる
* それっぽいプロジェクトもいくつか
  + [libpnet](https://github.com/libpnet/libpnet): 生パケット扱うライブラリ
  + [awesome-embedded-rust](https://github.com/rust-embedded/awesome-embedded-rust): 組み込み系のキュレーション
  + [tokio](https://tokio.rs/): 非同期イベントループのライブラリ

===
# メモリ配置
------------

この構造体のサイズは？

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=fd32ac47011842c8f12c0fc1425131c1)


```rust
use std::mem;

struct Hoge {
    c1: u8,
    i2: u32,
    c2: u8,
}

println!("{}", mem::size_of::<Hoge>());
```


===
# メモリ配置
------------

デフォルトでサイズが最小になるように詰める

```console
+----+----+----+---------+
| c1 | c1 |\\\\|   i2    |
+----+----+----+---------+
```

===
# メモリ配置
------------

この構造体のサイズは？

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d4290de0065ba2951a486676fafe2ecf)

```rust
use std::mem;

#[repr(C)]
struct Hoge {
    c1: u8,
    i2: u32,
    c2: u8,
}

println!("{}", mem::size_of::<Hoge>());
```


===
# メモリ配置
------------

`repr(C)` をつけると見慣れたメモリ配置になる

```console
+----+----+---------+----+----+
| c1 |\\\\|   i2    | c1 |\\\\|
+----+----+---------+----+----+
```

===

<blockquote class="twitter-tweet" data-conversation="none" data-dnt="true"><p lang="ja" dir="ltr">構造体をそのままchar*にキャストしてsocketに突っ込むことで通信ができるという幻想こそがC支持者の心の拠り所なわけで、その間違った考えにのっとるなら48bit整数とかが欲しくなるのはそこまで不自然な話ではないと思う。</p>&mdash; 7594591200220899443 (@shyouhei) <a href="https://twitter.com/shyouhei/status/1174519106864697344?ref_src=twsrc%5Etfw">September 19, 2019</a></blockquote>

===

# 普通のRust
------------

* 便利なイテレータ
* 素数最初の25個を列挙

[run](https://is.gd/Hh0H42)
[asm](https://godbolt.org/z/JC-DRx)


```rust
fn main() {
    (2..)
        .filter(|&n| (2..n).all(|i| n % i != 0))
        .take(25)
        .for_each(|n| println!("{}", n))
}
```


===
# 普通のRust
------------

* トレイト便利
  + C++のconcept相当らしい
* ポリモーフィズムは大抵トレイト経由で実現
  + 静的ディスパッチ
  + 演算子のオーバーローオ
  + 動的ディスパッチ
* メタプログラミングでいくつかのトレイトは自動で実装できる

===
# 普通のRust
------------
トレイト

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2015&gist=e9ba56aa24da63eed8eb3787f2b10ac5)

```rust
// `derive(トレイト)` アトリビュートをつけると
// 自動でトレイトを実装してくれる
#[derive(Debug, Clone, PartialEq, Eq)]
struct Cartesian(f64, f64);
#[derive(Debug, Clone, PartialEq, Eq)]
struct Polar(f64, 664);

// トレイトを定義
trait ToCartesian {
    fn to_cartesian(self) -> Cartesian;
}

それぞれの型に実装
impl ToCartesian for Cartesian {
    fn to_cartesian(self) -> Cartesian {
        self
    }
}

impl ToCartesian for Polar {
    fn to_cartesian(self) -> Cartesian {
        Cartesian(self.0 * self.1.cos(), self.0 * self.sin)
    }
}

// ToCartesianを実装している型のみ渡せる
fn print_point<P: ToCartesian>(p: P) {
    let p = p.to_cartesian();
    println!("({}, {})", p.0, p.1);
}

fn main() {
    let p = Polar(1.0, 0);
    // Debugをderiveしたので印字できる
    println!("{:?}", p);
    // PartialEqをderiveしたので比較できる
    p == p;
    // ToCartesianを実装したので `print_point` に渡せる
    print_point(p)
}
```


===
# 普通のRust
------------

* 割と型検査で事前に不正なコードを弾く
* テンプレートと違ってジェネリクス定義時点で型が合ってないとコンパイルできない
  + 以下のコードはこれを書いた時点でコンパイルエラー

```
fn print_point<P>(p: P) {
    let p = p.to_cartesian();
    println!("({}, {})", p.0, p.1);
}
```


===
# 普通のRust
------------

* 便利な `enum` (代数的データ型)
* [`Ordering`](https://doc.rust-lang.org/std/cmp/enum.Ordering.html) も便利
* `impl` ブロックでデータ型にメソッドを生やせる


[run](https://is.gd/Cpgdpo)


```rust
enum Color {
    Red,
    Black,
}

enum Tree<T> {
    Leaf,
    Node {
        color: Color,
        l: Box<Tree<T>>,
        v: T,
        r: Box<Tree<T>>,
    },
}

use std::cmp::Ordering;

impl<T: Ord> Tree<T> {
    fn is_member(&self, t: &T) -> bool {
        use Tree::*;
        match self {
            Leaf => false,
            Node { l, v, r, .. } => match t.cmp(v) {
                Ordering::Less => l.is_member(t),
                Ordering::Equal => true,
                Ordering::Greater => r.is_member(t),
            },
        }
    }
}
```


===
# 普通のRust
------------

* [`Result`](https://doc.rust-lang.org/std/result/enum.Result.html) 型とパターンマッチはエラーハンドリングに便利
  + Rustに例外はない。
* 「例外が上がる」という概念ではなくてデータ型という第一級の値で表現することで扱いやすさが向上
  + メソッドを生やしたりできる


```
use std::io::{self, Write};
use std::fs::File;

fn write_string(filename: &str, content: &str) -> io::Result<()> {
    let mut file = match File::create(filename) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("an error occured when opening file: {}", e);
            return
        }
    };
    match file.write_all(&content.as_bytes())? {
        Ok(file) => (),
        Err(e) => {
            eprintln!("an error occured when writing to file: {}", e);
            return
        }
    }
    Ok(())
}
```

===
# 普通のRust
------------

* 早期リターンする記法もある
  * `Result` (または`Option`) 型に `?` 演算子でエラーなら関数から返る

```
use std::io::{self, Write};
use std::fs::File;

fn write_string(filename: &str, content: &str) -> io::Result<()> {
    let mut file = File::create(filename)?;
    file.write_all(&content.as_bytes())?;
    Ok(())
}
```


===
# 普通のRust
------------

* UNIX APIの便利ラッパー
* パターンマッチに便利な仕組み

```rust
use nix::unistd::{fork, ForkResult};

fn main() {
    match fork() {
       Ok(ForkResult::Parent { child, .. }) => {
           println!("Continuing execution in parent process, new child has pid: {}", child);
       }
       Ok(ForkResult::Child) => println!("I'm a new child process"),
       Err(_) => println!("Fork failed"),
    }
}
```

===
# 普通のRust
------------

* Cargo
  + 便利なビルドツール/パッケージマネージャ
  + プラグインの仕組みもある
  + 今回は `cargo-edit` を使ってる (`cargo install cargo-edit` で入る)

```console
$ cargo new fork-example
$ cargo add nix
$ cd fork_example
$ edit src/main.rs
$ cargo run
```

===
# Why Rust (over C/C++)?
------------------------

* 安全
  + セキュリティ的な嬉しさ
  + 開発面での余計なデバッグの不要
* 生産性が高い
  + 便利な機能があることと低レイヤが扱えることは両立する
  + 例えば最近入った `async` / `await` はOSがなくても動く
* Cargo(ビルドツール) + crates.io(パッケージレジストリ)が便利
* 活発なコミュニティ

===
# 速度と機能の話
--------------

* [Why is Rust slightly slower than C?](https://github.com/ixy-languages/ixy-languages/blob/master/Rust-vs-C-performance.md)
  + ネットワークドライバを各言語で実装してみる実験
* RustはCより少しだけ遅い。でもIPCはRustの方が断然いい。
* → CはCPUを使ってる気になってるけど使いきれてないのでは？
* → CPUも進化してるんだから言語も進化しましょう

===
# Rustの安全性について
-----------------------------
安全 ≒ 未定義動作を踏まない

* NULL Pointerはない。
  + 令和にもなってセグフォのデバッグはしたくない
* use after freeができない
* 配列の範囲外アクセスが検査される
* データ競合(data race)が起きない
  + ≒ 多数のスレッドから1つのデータに同時にアクセスできない
* 競合状態(race condition) は防げないので注意
  + デッドロックとか

===
# 範囲外アクセス
----------------

* (他の安全性とは違って)範囲外アクセスは実行時に検査される
  + 範囲外アクセスを静的に弾くのはかなり難しいことが知られている


```rust
let v = vec![1, 2, 3];
// コンパイルは通るけど実行時にパニック
v[3]
```


<pre>
```console
thread 'main' panicked at 'index out of bounds: the len is 3 but the index is 3', /rustc/4560ea788cb760f0a34127156c78e2552949f734/src/libcore/slice/mod.rs:2717:10
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace.
```
</pre>


===
# 範囲外アクセス
----------------

* ただし固定長配列に定数でアクセスする場合はコンパイルエラーにしてくれる

```rust
let v = [1, 2, 3];
// コンパイルエラー
v[3]
```

<pre>

```console
error: index out of bounds: the len is 3 but the index is 3
 --&gt; src/main.rs:5:1
  |
5 | v[3];
  | ^^^^
  |
  = note: `#[deny(const_err)]` on by default
```

</pre>

===
# 所有権
--------

* 値にはライフタイムがある

```rust

{
  let data = Data::new();
  // `data` のライフタイムはこのスコープ
} // ← ここで `data` がfreeされる
// ここでは `data` にアクセスできない
```

===
# 所有権
--------

* 所有権は移動する

```rust

let data = Data::new();
// ここで `data` の所有権が `tmp` に移る
let tmp = data;
// 以降 `data` にアクセスするとコンパイルエラー
// tmpが死ぬと `data` はfreeされる
```

===
# 所有権
--------

* 関数に渡しても移動する

```rust
fn take_data(_: Data) {}


let data = Data::new();

take_data(data);

// 以降 `data` にアクセスするとコンパイルエラー
// take_data(data);
```

===
# 所有権
--------

* 関数に渡しても移動する
* …えっ

===
# 借用
--------

* データを一時的に「貸す」こともできる
* `&` で参照を取ると貸すことになる

```rust
fn borrow_data(_: &Data) {}

let data = Data::new();

// 貸す
borrow_data(&data);

// 返してもらったらまた使える
borrow_data(&data);
```

===
# 借用
--------

* `&` で参照を取ると貸すことになる
* 参照はポインタの意味もあるけど普段はあんまりポインタとしては意識してない
  + カジュアルに構造体の値渡しをする
  + 「借用するかどうか」で使い分ける
  + 所有するポインタ (`Box`) もあるけどたまにしか使わない

===
# 借用とライフタイム
-------------------

* 借りたデータを元のデータより長く生かせない
  + 要するにdangling pointer禁止
* 長く生かそうとするとコンパイルエラー


```rust
// このコードはコンパイルエラー
fn new_data() -> &Data {
    let data = Data::new();
    &data
    // data はここで死ぬので関数から返せない
}
```

===
# 借用と変更
-------------------

* 以下の関数の返り値は？

```C
int
func(int *a, int *b)
{
  *a = 0;
  *b = 1;
  return *a;
}
```


===
# 借用と変更
-------------------

* 以下の関数の返り値は？
  + `&mut` は可変ら参照を表わす
  + Cでいう普通の `&`

```rust
fn func(a: &mut i32, b: &mut i32) -> i32 {
    *a = 0;
    *b = 1;
    *a
}
```

===
# 借用と変更
-------------------

* Rustでは1つの変数の可変な参照を複数作れない
  + 以下はコンパイルエラー
  ```
  let mut a = 1;
  func(&mut a, &mut a)
  ```
  + Rustの `memcpy` は簡単になる
* RustはPointer Aliasの制約が強い
  + → 挙動が分かりやすい
  + → コンパイラが最適化しやすい
* 不変な借用があるときに可変な参照も作れない
  + 要するにコンパイル時 Read-Write Lock

===
# 借用と変更
-------------------

* 不変な借用があるときに可変な参照も作れない

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2015&gist=2d2d3e2fbb67247147ef1cbde75fcbbc)


```rust
let v = vec![1, 2, 3];
for e in &v {
    v.push(*e);
}
```

<pre>

```console
error[E0596]: cannot borrow `v` as mutable, as it is not declared as mutable
 --&gt; src/main.rs:4:9
  |
2 |     let v = vec![1, 2, 3];
  |         - help: consider changing this to be mutable: `mut v`
3 |     for e in &v {
4 |         v.push(*e);
  |         ^ cannot borrow as mutable
```

</pre>

===
# Nullableな値
---------------

* 全てのポインタがNullableなのは酷いけどNullがないのも不便な気がする
  + findして結果がなかったらNullを返したい
* ポインタとは関係なくNullableであることを表わすデータ型を用意して解決
  + `i32` とかもnullableにできる
  + メソッドを生やせる
* [Option](https://doc.rust-lang.org/std/option/enum.Option.html)
* `Option<Pointer>` は最適化でただのポインタになる

```rust
pub enum Option<T> {
  None,
  Some(T),
}
```


===
# 所有権の例外
--------------

* `i32` とか小さい型をいちいち貸し借りしたくない
  - 湯水のごとくじゃぶじゃぶ使いたい
* そういう型は無制限に使える仕組みがある
* [`Copy`](https://doc.rust-lang.org/std/marker/trait.Copy.html)トレイトを実装した型は勝手にコピーしてくれる
  + よくRustで所有権を試そうとしてる人がはまりがち

```rust
fn take_i32(_: i32) {}
let a = 1;
take_i32(a);
// 何度でも呼べる
take_i32(a);
take_i32(a);
```


===
# 所有権とか
---------------------------

* 正直スライドだけでは伝えきれない
* 理解しようとすると[公式ドキュメント](https://doc.rust-jp.rs/book/second-edition/ch04-00-understanding-ownership.html)を読むのがよい
  + あとは手を動かさないと分からない。
* 雑にまとめると
  + データには所有者がいる
  + ポインタは実体があると保証できる範囲でしか作れない
    - NULL poinerやdangling pointerは存在しない
  + Writeは排他

===
# 所有権とか
---------------------------

* 結構アプリケーションの設計に関わってくる
  + 雑な設計だとすぐに破綻する
  + それゆえ難しいといわれがち
* 所有者を意識すると綺麗になりがち
  + 長寿のデータ型に持たせる
    - `App` とか `Config` とか
  + データ構造は所有者になりがち
    - `HashMap` とか
  + 処理のフローを考えると余計なコピーを省ける
    - HTTPの場合は `Request` の生存期間で十分だったり

===
# 所有権小話1
-------------

* `HashMap` はデータを所有するので `get` / `get_mut` だとデータを借りることしかできない
  + 取り出したいのが不変の参照か可変の参照かでメソッドが分かれてるのが普通
* データを取り出したいときは `remove` を使う

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=c633e1e2a6935a12238795f68c22b156)


```rust
use std::collections::HashMap;
let mut map = vec![
    (1, "one".to_string()),
    (2, "two".into()),
    (3, "three".into()),
]
.into_iter()
.collect::<HashMap<_, _>>();

match map.remove(&3) {
    None => println!("no data"),
    Some(data) => println!("got data: {}", data),
}
```


===
# 所有権小話2
-------------

* Rustにリソースを開放するAPIはない
  + `File` の `open` はあるけど `close` はない
  + `Lock` の `lock` はあるけど `free` はない
* RAIIで管理されるのでデータの `free` のときに一緒に開放される

[run](https://is.gd/KKm9Vb)

```run
use std::io::{self, Write};
use std::fs::File;

fn write_string(filename: &str, content: &str) -> io::Result<()> {
    // Fileをwriteモードでopen
    let mut file = File::create(filename)?;
    file.write_all(&content.as_bytes())?;
    Ok(())
    // fileがスコープを抜けるときに自動でcloseされる
}
```


===
# Rustの進歩
------------

* Rustは問題がある「かもしれない」コードを弾く
  + 不思議な力で安全になる訳ではなくて、養成ギプス的にユーザに安全なコードを書かせる
* 安全にはなるが窮屈
  + 極端な話、全てのコードを弾けば実行時エラーは出ない
* Rustの進歩でコンパイルが通る範囲もちょっとづつ広がっている

===
# Rustの進歩
------------

* 昔は以下のコードがコンパイルできなかった
  - 昔 = 1年前
* 最近は制御フローまで見て問題なければコンパイルを通す

[run](https://is.gd/ALWpec)


```rust
use std::collections::HashMap;
fn insert_or_update(map: &mut HashMap<i32, i32>, key: i32, value: i32)  {
    // get_mutで可変の参照
    match map.get_mut(&key) {
        Some(v) => *v = value,
        None => {
          // その参照が生きている間に更新
          map.insert(key, value);
        },
    }
}
```


===
# 所有権をopt out
----------------

* 所有権は便利だけどそれだと書けないデータ構造が発生する
  + グラフとか
* そういう場合に実行時に所有権/ミュータビリティ検査をするAPIがある
  + [`Rc`](https://doc.rust-lang.org/std/rc/struct.Rc.html) (参照カウント) … 複数人でデータを共有できる [doc](https://doc.rust-jp.rs/book/second-edition/ch15-04-rc.html)
  + [`RefCell`](https://doc.rust-lang.org/std/cell/struct.RefCell.html) … 実行時に借用検査をする [doc](https://doc.rust-jp.rs/book/second-edition/ch15-05-interior-mutability.html)

===
# 所有権をopt out
----------------

* Rustだけど「何でもあり」にできてしまう例

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2015&gist=f7b7e132be264a3de565669565c4e454)

```rust
use std::rc::Rc;
use std::cell::RefCell;

let data = Rc::new(RefCell::new(1));
let data2 = data.clone();

// data2を変更。 data2はイミュータブルだが
// `RefCell` なので変更できる
*data2.borrow_mut() = 2;

// `Rc` なのでdata2の変更がdataにも反映される
assert_eq!(*data.borrow(), 2);
```


===
# 並列
-------

* Rustはスレッドセーフでないプログラムをマルチスレッドで使うとエラーにする
* 例えば `Rc` はスレッドアンセーフ(裏でcountの変更があるため)

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2015&gist=9198d0f301cc24e503992ed9b127ef59)

```rust
use std::rc::Rc;
use std::thread;
use std::cell::RefCell;

let data = Rc::new(RefCell::new(1));
let data2 = data.clone();
let handle = thread::spawn(move || {
    *data2.borrow_mut() = 2;
});
handle.join().unwrap();

assert_eq!(*data.borrow(), 2);
```

<pre>

```console
   --&gt; src/main.rs:9:14
    |
9   | let handle = thread::spawn(|| {
    |              ^^^^^^^^^^^^^ `std::rc::Rc<std::cell::RefCell<i32>>` cannot be shared between threads safely
    |
    = help: the trait `std::marker::Sync` is not implemented for `std::rc::Rc<std::cell::RefCell<i32>>`
    = note: required because of the requirements on the impl of `std::marker::Send` for `&std::rc::Rc<std::cell::RefCell<i32>>`
    = note: required because it appears within the type `[closure@src/main.rs:9:28: 14:2 data2:&std::rc::Rc<std::cell::RefCell<i32>>]`
```

</pre>


===
# 並列
-------

* スレッドセーフなAPIにしたり `Mutex` を使ったりするとコンパイルが通る
  + `Arc` = Atomic Reference Count
  + `Mutex` = mutual exclution、要はロック

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2015&gist=13757c7015d10f5954a4978baa8d17e5)

```rust
use std::sync::{Arc, Mutex};
use std::thread;

let data = Arc::new(Mutex::new(1));
let data2 = data.clone();
let handle = thread::spawn(move || {
    *data2.lock().unwrap() = 2;
});
handle.join().unwrap();

assert_eq!(*data.lock().unwrap(), 2);
```


===
# 並列の安全性の舞台裏
----------------------

* トレイトで制御している
* `thread::spawn` に渡せるのは `Send` トレイトを実装した型のみ。
* `Rc` や `RefCell` は `Send` トレイトを実装していない
* → `Rc` や `RefCell` を渡そうとするとコンパイルエラーになる
* ドキュメントを読まなくてもスレッドセーフか分かるの素敵

===
# 安全性を捨てるとき
-------------------

* Rustを使ってても安全性を捨てないといけないケースがある
  + Cと連携するとき
    + Rustから見たらCはデフォルトで危険
  + データ構造を実装するとき
    + ハッシュテーブルみたいに未初期化かもしれないメモリを扱うとき
* そういうときのエスケープハッチがある
  + その名も `unsafe`
* `unsafe` な部分とsafeな部分を区別する仕組みがある

===
# 安全性を捨ててみる
-------------------

* `unsafe` で囲むとやりたい放題できる

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2015&gist=6a04ca87dc616a2dfece0aae00e9e981)


```rust
use std::ffi::c_void;
use std::ptr::null_mut;

extern "C" {
  // FFIの関数のプロトタイプ宣言
  // 参照とは別のマジのポインタ型
  fn free(p: *mut c_void);
}

fn main() {
  // unsafeで囲むとやりたい放題
  unsafe {
    // ヌルポが作れる！！
    let p: *mut i32 = null_mut::<i32>();
    // ヌルポに書き込める！！
    *p = 1;
    // freeできる！！
    free(p.cast());
    // use after freeできる！！
    println!("{}", *p);
  }
}
```


===
# `unsafe` の仕組み
-------------------

* Rustはいくつかの操作や関数を `unsafe` とみなす
  + 関数は自分で `unsafe` とマークできる
* `unsafe` な操作は `unsafe` の内側でしかできないようになっている
  + `unsafe` の境界の安全性はユーザが保証する

[run](https://play.rust-lang.org/?version=stable&mode=debug&edition=2015&gist=3e1c98392da16fea45ed23e0d945ff72)

```rust
use std::ptr::null_mut;

fn main() {
    // ヌルポインタを作るだけならsafe
    let p: *mut i32 = null_mut::<i32>();
    // ポインタに触るのはunsafe
    *p = 1;
}
```

<pre>


```console
error[E0133]: dereference of raw pointer is unsafe and requires unsafe function or block
 --&gt; src/main.rs:7:5
  |
7 |     *p = 1;
  |     ^^^^^^ dereference of raw pointer
  |
  = note: raw pointers may be NULL, dangling or unaligned; they can violate aliasing rules and cause data races: all of these are undefined behavior

```

</pre>


===
# `unsafe` の使いどころ
----------------------

* 基本は使わない。
* どう頑張っても `unsafe` を使わないと実装できないものは仕方なく使う
  + データ構造の実装に多い
  + 標準ライブラリの [`Vec`](https://doc.rust-lang.org/beta/src/alloc/raw_vec.rs.html#315-359) とか
* めちゃくちゃ速度が重要で、 `unsafe` を使うとすごく高速化できる場合にはトレードオフを考えて使う
  + スライスの境界外アクセスを無視して行なう [`get_unchecked`](https://doc.rust-lang.org/beta/std/primitive.slice.html#method.get_unchecked)とか
* C FFIのラッパを書くときはまあ、仕方ない
  + でも最小限に

===
# 活発なコミュニティ
-------------------

* 「技術的投資」というならその資産のグロースも考えよう
* stack overflowの[最も愛されている言語](https://insights.stackoverflow.com/survey/2019#most-loved-dreaded-and-wanted)
  + 2016年から4年連続1位
* [Microsoft](https://msrc-blog.microsoft.com/2019/11/07/using-rust-in-windows/)も導入に乗り気
* パッケージのセントラルレポジトリ([crates.io](https://crates.io))がある
  + 30,000+ クレート
  + 参考: rubygemsは150,000+ gems

===
# コミュニティ中心
-------------------

* コミュニティベースの開発
  + 「Mozillaの言語」ではない。
  + Mozilaが初期から支援しているだけ。今は[AWS](https://www.atmarkit.co.jp/ait/articles/1910/17/news088.html)とかもサポート。
* 開発はチームによる。チームの会議もDiscordなどで公開
  + やさしい終身の独裁者的な人はいない
* コミュニティから意見を吸い上げる→開発チームがロードマップを出すのサイクル
  + [サーベイ](https://blog.rust-lang.org/2018/11/27/Rust-survey-2018.html) とか [#rust2020](https://blog.rust-lang.org/2019/10/29/A-call-for-blogs-2020.html)
  + 機能追加の提案も [GitHub](https://github.com/rust-lang/rfcs) から誰でもできる

===
# まとめ
--------

* Rustは安全なシステムプログラミング言語だよ
  + 安全とはUBが起きないことだよ
* 普通にプログラミング言語としても便利だよ
* コミュニティに勢いがあるよ
* お試しとしても新天地としても良い言語なんじゃないでしょうか


</textarea>
