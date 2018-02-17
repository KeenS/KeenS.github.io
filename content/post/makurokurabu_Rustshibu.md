---
categories: [Rust, マクロ]
date: 2018-02-17T15:15:07+09:00
title: "マクロクラブ Rust支部"
---

マクロ・クラブのルール

1. マクロを書くな
2. それがパターンをカプセル化する唯一の方法ならば、マクロを書け
3. 例外: 同等の関数に比べて、 呼び出し側が楽になるならば、マクロを書いても構わない

<!--more-->
κeenです。レーシックを受けようとしたら角膜が薄くて手術拒否されました。予定が狂って時間が空いたのでブログを書きます。

冒頭のルールは[プログラミングClojure](https://www.ohmsha.co.jp/book/9784274069130/)に出てくるマクロの書き方の指南です。
Rustのマクロって色々できるんだよという記事を書くにあたってマクロに一日の長があるLispの知見を引用されていただきました。
ルールにあるとおり、マクロは関数と違ってRustの第一級オブジェクトでないので扱いづらいですし本体がコピーされるのでコードサイズも膨らんでしまいます。
よく考えながら使いましょう。とはいってもRustの構文はLispに比べると複雑ですし型やパターンマッチなどLispに存在しない構文要素もあるのでLispより使いどころは多くありそうです。

# マクロって何？
簡潔に言うとRustのプログラムをプログラミングする仕組みです。
関数がデータを受け取ってデータを返すのに対してマクロはRustの構文の一部(構文木)を受け取ってRustの構文の一部を返します。

マクロはRustコンパイラがコンパイル中に実行するので関数とは全然違うタイミングで動きます。
初めのうちはマクロも意識せずに使えますがまれにマクロについて理解してないと使えないケースもあります。
必要になったらマクロを意識しはじめましょう。

マクロのドキュメントは[TRPL](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/macros.html)や[TRPL 2nd](https://doc.rust-lang.org/book/second-edition/appendix-04-macros.html)、[リファレンスマニュアル](https://doc.rust-lang.org/stable/reference/macros-by-example.html)、非公式ですが[The Little Book of Macros](http://danielkeep.github.io/tlborm/book/)などがあるので詳しく見たい方はそちらを当たるといいかもしれません。ここではあまり難しいことは言わずにマクロを使っていきます。ポジションとしてはTRPLのマクロの章を実際のユースケースに合わせて補完できたらなというところです。

また、Rustにはここで扱う宣言的マクロの他に手続き的マクロもあります。
手続きによってRustの構文を作るので生成されるコードがわかりづらく難しい反面、自由度は高いので宣言的マクロで出来ないことも手続き的マクロでは可能になります。
proc_macroはここでは扱わないので気になる方は適当に調べてみて下さい。

# マクロでできること
大抵のことはできます。

## 値
要は関数のようにも扱えるわけですね

``` rust
macro_rules! add {
    ($e1: expr, $e2: expr) => {
        $e1 + $e2
    };
}

fn main() {
    let ret = add!(1, 2);
    println!("{}", ret); // -> 3
}
```

## 関数呼び出し

関数名を受け取ってそれを呼び出したりもできます。

``` rust
fn print2(i: i32, j: i32) {
    println!("{}, {}", i, j);
}

macro_rules! call_by_double {
    ($name: ident, $e: expr) => {
      $name($e, $e)
    }
}

fn main() {
    call_by_double!(print2, 1); // -> 1, 1
}
```

## 変数束縛
新たな束縛を導入したりもできます。
詳しくは触れませんがRustのマクロは衛生的なのでユーザが指定した以外の変数を導入することはできません。

``` rust
macro_rules! bind {
    ($var: ident, $val: expr) => (let $var = $val;)
}

fn main() {
    bind!(x, 1 + 2);
    println!("{}", x); // -> 3
}
```


## パターン
パターンも書けます。今試したら`|`パターンは書けないようでした。残念ですね。

``` rust
macro_rules! exact_one {
    () => (Some(1))
}

macro_rules! two {
    ($x: ident) => (Some($x @ 2))
}

macro_rules! three {
    ($x: ident) => (Some($x @ 3))
}

macro_rules! many_or_none {
    () => (_)
}

fn main() {
    match Some(2) {
        exact_one!() => println!("exact one"),
        two!(x) | three!(x) => println!("{}", x),
        many_or_none!() => (),
    }
}
```

## 型
型の位置にもマクロが使えます。残念ながらトレイト制約の一には書けませんでした

``` rust
macro_rules! result {
    () => {
        Result<i32, ()>
    }
}

fn function() -> result!() {
    Ok(0)
}
```

## アイテムの定義
`struct`や`impl`や`trait`などの定義もできます。

``` rust
macro_rules! define_id_type {
    ($name: ident) => {
        pub struct $name(pub u64);
        impl $name {
            pub fn new(id: u64) -> Self {
                $name(id)
            }
        }
    }
}

define_id_type!(UserId);
```

---

マクロをどこで使えるかはイマイチドキュメントがなく、実験的に確かめるしかありません。

# マクロを定義する
## 基本の「き」
マクロで何が書けるかを見てきたので次はマクロを書いてみましょう。とはいってもいままで定義してきましたね。
最初の`add!`マクロを取り出してみましょう。

``` rust
macro_rules! add {
    ($e1: expr, $e2: expr) => {
        $e1 + $e2
    };
}
```

これの引数のところを取り出してみましょう

```
($e1: expr, $e2: expr)
```

`$e1`, `$e2` などはいいですね。引数の変数です。
マクロは引数でパターンマッチできるので変数はそれとの違いを明確にするために`$`をつけます。

`:expr`の方です。マクロの引数にも型があります。構文を受け取るので型は構文の型です。ここでは`expr`型の構文を受け取ると言っています。
`expr` 型はRustの構文でいう式、つまりは計算すると値を返す構文の型です。

他にも構文の型はいくつかあります。


* `item`: `fn`定義`struct`定義などのトップレベルに置かれるものです
* `block`: ブロック。雑に言うと波括弧で括られた式/文ですね。`{let x = 1; x * 2}`など。
* `stmt`: 文。雑に言うと`;`で終わるやつです。`let x = 1;`など。
* `pat`: パターン。`match`や`if let`、`let`などで使えるやつです。
* `expr`: 式。先程説明したとおり、計算すると値を返すものです。`1 + 2`など。
* `ty`: 型。
* `ident`: 識別子。関数名、変数名などですね。
* `path`: パスです。`std::collections::HashMap`などです。
* `tt`: トークンツリー。1塊になってる構文要素です。まあ、なんかなんでもアリみたいなやつです。
* `meta`: メタアイテムです。雑にいうと`#[]`の中身です。

これを覚えておけばマクロの引数には困りません。
あと比較的最近、可視性(`pub`や`pub(crate)`など)も導入された筈ですがドキュメントが見当たらないので流します。

よく使うのは`expr`, `ty`, `ident`あたりかなーといったところです。


## パターン
マクロの引数の位置にはパターンが書けます。試しに`Vec`を作る`vec!`マクロのように`HashMap`を作る`map!`マクロを作ってみましょう。

``` rust
macro_rules! map {
    ($($k: expr => $v: expr),*) => {{
        let mut map = ::std::collections::HashMap::new();
        $(map.insert($k, $v);)*
        map
    }};
}

fn main() {
    let nums = map!{1 => "one", 2 => "two"};
    println!("{}", nums[&2]);
}
```

一気に詰め込みすぎましたかね？ひとつづつ見ていきます。

まずはパターンの`$k: expr => $v: expr`の部分。
`1 => "one"`や`2 => "two"`にマッチしています。
それっぽくパターンを書くとrustコンパイラがいい感じにマッチしてくれます。
ここでの`=>`のようにマクロを使うとRustにない構文でも自由に受け取れるようになります。マクロ便利ですね。
ところで`add!`マクロで`($e1: expr, $e2:expr)`のようにカンマで区切っていたのもカンマに対するマッチだった訳ですね。

この新しい記号の導入はある程度制限があります。たとえばここで`+`を区切りに使うと怒られます。
`map![1+1+1]`と渡した場合に`+`がマクロの区切りなのか足し算をしているのか区別が付かないからです。
これはどの記号が許容されるかはドキュメントに書いてあるので気になる人は確認して下さい。
ドキュメントを読まない人はRustの構文解析器の気持ちになって察して下さい。間違ってたらコンパイラが教えてくれるのでそれでもいいと思います。

次に`$(...),*`の部分。これは0回以上のパターンの繰り返しです。
複数個使おうと思ったら必要ですね。
地味に`,`の扱いが特別で、`map!{1 => "one", 2 => "two"}`の使用からみても分かるとおり区切りの位置にのみマッチします。`map!{1 => "one", 2 => "two",}`と最後にカンマを書くとエラーになります。


パターンはいいのですが、使用箇所も見ておきましょう。`$(map.insert($k, $v);)*`です。
使用時にも`$(...)*`包んでいます。ここでは`$()`の中に`;`を入れているので区切りを使ってませんね。
区切りを外に出して`$(map.insert($k, $v));*;`と書くことも可能ですが、最後のセミコロンに注意しましょう。

最後に、マクロの本体が複文になったので全体が`{{ }}`で囲まれています。
外側の`{}`がマクロの本体を括る括弧で、内側の`{}`がマクロの返り値を1つの構文要素にするための括弧です。

さて、これでだいたいマクロは説明した気がするので実例をみていきましょう。



# マクロ実例集
## `match_vec!`
説明していませんでしたが、マクロのパターンは複数書けます。関数定義よりは`match`を書いている気分に近いですね。


``` rust
macro_rules! ignore {
    ($pat: tt) => {}
}

macro_rules! match_vec {
    // `vec![]`パターンの末尾カンマに対応するために末尾カンマを取り除いて本体に渡すための節
    (let vec![$($pat:pat),*,] = $v:expr;) => (
        match_vec!(let vec![$($pat),*] = $v;)
    );
    // 本体
    (let vec![$($pat:pat),*] = $v:expr;) => (
        let ($($pat),*) = {
            // * exprで受けたので`vec![1, 2, 3]`などのまだ評価されていない式も来うる。
            //   一旦変数に格納して評価させる。
            // * ついでに`mut`をつけたりイテレータを取り出したり。
            let mut i = $v.into_iter();
            // * `$()*`を使いたいが`$pat`は使わないので`ignore`を使って無視する
            // * お粗末だが`next()`に対して`unwrap()`している。
            //   実行時のマッチ失敗panicを投げる余裕があるなら投げるべき。
            let ret = ($({ignore!($pat); i.next().unwrap()}),*);
            // 同じくvecが余った場合の検査を`assert!`に丸投げしている。
            assert!(i.next().is_none());
            ret
        };
    )
}

```

使い方


```rust
fn main() {
    let v = vec![1, 2, 3];
    match_vec! {
        let vec![x, y, z] = v;
    }
    // 上の式を展開するとこうなるはず。
    //
    // // 複数のパターンマッチをタプルのマッチに落とし込んでいる。
    // let (x, y, z) = {
    //   // (マニアックな話):Rustのマクロは衛生的なのでマクロ内で定義した`i`がgensym(rename)される。
    //   let mut i_xxx = v.into_iter();
    //   let ret = (
    //     {ignore!(x);i_xxx.next().unwrap()},
    //     {ignore!(y);i_xxx.next().unwrap()},
    //     {ignore!(z);i_xxx.next().unwrap()},
    //   );
    //   assert!(i_xxx.next().is_none());
    //   ret
    // };
    println!("x: {}, y: {}, z: {}", x, y, z); // -> x: 1, y: 2, z: 3
}
```

興味があるなら`HashMap`向けのものも書くと練習になるかもしれません。



## `err!`

定義自体はシンプルです。

``` rust
macro_rules! err {
    ($e: expr) => {
        return Err($e.into())
    }
}

```

使うのもシンプル

``` rust
fn fact(n: i32) -> Result<i32, String> {
    if n < 0 {
        err!("n is negative")
    }
    let mut ret = 1;
    for i in 1..(n + 1) {
        ret *= i;
    }

    Ok(ret)
}

fn main() {
    println!("{:?}", fact(-3)); // -> Err("n is negative") 
}
```

シンプルですが`return`のようにコントロールフローに干渉するようなものは関数では書けなくて、マクロが必要になります。


## `define_error_enum!`

エラーハンドリングのときに複数のエラーを束ねるenumの実装を生成するやつです。
現実的には[error_chain](https://github.com/rust-lang-nursery/error-chain)などのちゃんとしたものを使うべきですが、似たようなケースに遭遇したときに自分でボイラープレートを削減出来るマクロを書けると便利です。

``` rust
macro_rules! define_error_enum {
    // 普通のenumっぽく書けるようにパターンを工夫してある。
    // 無骨に型名、列挙子と対応するエラー型名だけを受け取っても本質は変わらない。
    ($(#[$meta:meta])*
     pub enum $name: ident { $($variant: ident ($ty: ty),)* } ) => {
        // この`$(#[$meta])*`を用意しておくとユーザは好きにderiveなどを書ける。
        $(#[$meta])*
        pub enum $name {
            // 本当はvariantにもmetaをつけれるようにすべきだが、面倒なので読者の課題とする
            $($variant($ty),)*
        }

        // 各型毎に`From`を実装
        $(
            impl From<$ty> for $name {
                fn from(e: $ty) -> Self {
                    $name::$variant(e)
                }
            }
        )*


        // この型自体もErrorになるように諸々実装。
        // 実装は内部のエラーにdelegateするだけ。
        impl ::std::fmt::Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                use self::$name::*;
                match self {
                    $(&$variant(ref e) => e.fmt(f),)*
                }
            }
        }

        impl ::std::error::Error for $name {
            fn description(&self) -> &str {
                use self::$name::*;
                match self {
                    $(&$variant(ref e) => e.description(),)*
                }
            }

            fn cause(&self) -> Option<&::std::error::Error> {
                use self::$name::*;
                match self {
                    $(&$variant(ref e) => Some(e),)*
                }
            }
        }
    }
}
```

結構大きいですがほぼボイラープレートなのでマクロとしてはあんまりおもしろいことはやってないです。

使い方ですが、まずエラー型を適当に用意しましょう。

``` rust
use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub struct Error1;

impl fmt::Display for Error1 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "this is error1")
    }
}

impl Error for Error1 {
    fn description(&self) -> &str {
        "error1 occurred"
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

#[derive(Debug)]
pub struct Error2;

impl fmt::Display for Error2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "this is error2")
    }
}

impl Error for Error2 {
    fn description(&self) -> &str {
        "error2 occurred"
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

```

そして

``` rust

//ほぼenumの定義っぽく書ける
define_error_enum! {
    #[derive(Debug)]
    pub enum GenericError {
        E1(Error1),
        E2(Error2),
    }
}

fn main() {
    // intoを自動で実装した
    let e: GenericError = Error1.into();
    // displayを自動で実装した
    println!("{}", e); // -> this is error1
}

```

便利ですね。


---

こういった、「もうちょっとすっきり書けないかな」「ほぼコピペなコードが量産されるんだけどどうにかならないの」な状況にマクロが便利です。


# マクロプログラミング
ここからはプログラミングの余興、サーカス的プログラミングの世界です。

Rustのマクロには再帰(ループ)とパターンマッチ(分岐)があるのでチューリング完全です。
そう訊いたらとりあえずプログラミングしてみたくなりますよね？

# 最初の試み

そう聞いたあなたはすぐさま`(1, ...)`で数値を表すことにして以下のようなマクロを書き下すでしょう。

``` rust
macro_rules! add {
    (($($l: tt),*), ()) => {
        ($($l),*)
    };
    (($($l: tt),*), (1)) => {
        (1, $($l),*)
    };
    (($($l:tt),*), (1, $($r: tt),*)) => {
        add!((1, $($l),*), ($($r),*))
    };
}

macro_rules! sub {
    (($($l: tt),*), ()) => {
        ($($l),*)
    };
    ((1 $(, $l: tt)*), (1)) => {
        ($($l),*)
    };
    ((1 $(, $l:tt)*), (1 $(, $r: tt)*)) => {
        sub!(($($l),*), ($($r),*))
    };
}

macro_rules! fib {
    (()) => {(1)};
    ((1)) => {(1)};
    (($($n:tt),*)) => {
        add!(fib!(sub!(($($n),*), (1))), fib!(sub!(($($n),*), (1, 1))))
    };
}

fn main() {
    let n = fib!((1, 1, 1));
    println!("{:?}", n);
}
```

しかしこれはうまく動作しません。

```
error: no rules expected the token `fib`
  --> macro.rs:29:14
   |
29 |         add!(fib!(sub!(($($n),*), (1))), fib!(sub!(($($n),*), (1, 1))))
   |              ^^^
...
34 |     let n = fib!((1, 1, 1));
   |             --------------- in this macro invocation

error: aborting due to previous error

error: Could not compile `macro`.

To learn more, run the command again with --verbose.
internal error: cargo failed with status 101
```

これはマクロの展開の順序が絡んできます。
普通の関数だと内側から展開されますが、マクロは外側から展開するので問題が起きます。
`fib!`の第3節の本体は`add!(sub!($($n),*, (1)), sub!($($n),*, (1, 1)))`ですね。
このマクロ式の呼び出しは外側から、つまり`add!`から展開されます。
`add!`は`(sub!($n, (1)), sub!($n, (1, 1)))`に対してパターンマッチしようとして失敗しているのです。

さて、どうしましょう。普通にはプログラミングできません。外側から展開していたものを内側から展開するようにうまいことやる手段はあるでしょうか。

そう、みなさんならおわかりですね。「CPSするとlazyをeagerに変換できる」というやつです。正確にはCPSじゃありませんが。

# `ck!`
コードと継続からなる抽象マシン、CKマシンをマクロで適当にエミュレートするのが`ck!`マクロです。詳しくはこの記事を→ [syntax-rulesズンドコキヨシ、またはマクロ展開時ズンドコキヨシ](https://qiita.com/dico_leque/items/e2c7a88df2e9dfe9a446)。
雑に言うとコールスタックを自前で実装したらネストしたマクロ呼び出しもできるよねってやつです。
ただちょっと面倒なのが引数が複数あるので呼び出しの深さに応じてスタックを縦に積むのと引数の数に応じて中身を横に広げるの2つの操作がある点ですね。これはスタックにタプルを積むことで解決します。


また、rust特有の問題として、マクロ呼び出しが`name ! (args...)`と3トークンに分かれてしまうのでやや扱いに気をつける必要があります。
もう一つ、値をマクロと同じように扱わないと対応するパターンが爆発してしまうのでLispでいうquote相当のものとして`q!`という目印を使います。
これは`ck!`マクロ内で目印として使っているだけのものなので特に実体は必要ありません。


``` rust
// ck(スタック, 式)という形で評価していく。なんとなく継続を起動してるイメージ
// 評価が終わったら`q!(値)`という形で目印を付ける
//
// スタックは以下のような形。
// [(マクロ名, (まだ評価していない引数...), (評価が終わった引数 ...)), ...]
//
// ここで受け取るマクロは全てスタックを第一引数に取るものとする。CPSでいう継続っぽいもの。
macro_rules! ck {
    // スタックが空で値が出来ているなら値を返す
    ([], q!($value:tt)) => {
        $value
    };
    // 評価対象がマクロ呼び出しならスタックに積んで第一引数から評価を始める
    ([$($stack:tt),*], $name:ident ! ($argn:ident ! $arga:tt, $($args: tt)*)) => {
        ck!([($name, ($($args)*), ()) $(,$stack)*], $argn!$arga)
    };
    // コンマの扱いのために1引数の場合も別途定義
    ([$($stack:tt),*], $name:ident ! ($argn:ident ! $arga:tt)) => {
        ck!([($name, (), ()) $(,$stack)*], $argn!$arga)
    };
    // 無引数マクロは即時評価
    ([$($stack:tt),*], $name:ident ! ()) => {
        name!([$($stack),*])
    };
    // 1つの引数の評価が終わって、まだ評価していない引数があるなら
    // 評価が終わったリストにその値を加えてまだ評価していない引数を評価する
    ([($name:ident, ($todon:ident ! $todoa:tt, $($todos:tt)*), ($($dones:tt),*)) $(, $stack:tt)*], q!($value:tt)) => {
        ck!([($name, ($($todos)*), ($($dones,)* $value)) $(, $stack)*], $todon ! $todoa)
    };
    // コンマの扱いのためにtodoが1つの場合も別途定義
    ([($name:ident, ($todon:ident ! $todoa:tt), ($($dones:tt),*)) $(, $stack:tt)*], q!($value:tt)) => {
        ck!([($name, (), ($($dones,)* $value)) $(, $stack)*], $todon ! $todoa)
    };
    // 全ての引数を評価したなら満を持してマクロを呼ぶ。コールスタックを渡しているので帰ってこれる。
    ([($name:ident, ( ), ($($dones:tt),*)) $(, $stack:tt)*], q!($value:tt)) => {
        $name!([$($stack),*], $($dones,)* $value)
    };
}
```

このCKマクロを使うと次のようにマクロを定義できます。

``` rust
macro_rules! add {
    ($s:tt, ($($l:tt),*), ()) => {
        ck!($s, q!(($($l),*)))
    };
    ($s:tt, ($($l:tt),*), (1)) => {
        ck!($s, q!((1, $($l),*)))
    };
    ($s:tt, ($($l:tt),*), (1 $(, $r:tt)*)) => {
        ck!($s, add!(q!((1, $($l),*)), q!(($($r),*))))
    };
}

macro_rules! sub {
    ($s:tt, ($($l: tt),*), ()) => {
        ck!($s, q!(($($l),*)))
    };
    ($s:tt, (1 $(, $l: tt)*), (1)) => {
        ck!($s, q!(($($l),*)))
    };
    ($s:tt,  (1 $(, $l:tt)*), (1 $(, $r: tt)*)) => {
        ck!($s, sub!(q!(($($l),*)), q!(($($r),*))))
    };
}

macro_rules! fib {
    ($s: tt, ()) => {ck!($s, q!((1)))};
    ($s: tt, (1)) => {ck!($s, q!((1)))};
    ($s: tt, ($($n:tt),*)) => {
        ck!($s, add!(fib!(sub!(q!(($($n),*)), q!((1)))), fib!(sub!(q!(($($n),*)), q!((1, 1))))))
    };
}
```


これで念願の`fib!`マクロが動きます。

``` rust

fn main() {
    let n = ck!([], fib!(q!((1, 1, 1, 1, 1))));
    println!("{:?}", n); // -> (1, 1, 1, 1, 1, 1, 1, 1)
}

```

ただしマクロの再帰が深くなるので`#![recursion_limit = "256"]`が必要となります。


# おわりに
マクロは便利な使い方もアホな使い方もできるよって話でした。
