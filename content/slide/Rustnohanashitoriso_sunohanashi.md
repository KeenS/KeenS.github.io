---
categories: [Rust]
date: 2016-11-20T14:08:07+09:00
description: "<a src='https://rust.connpass.com/event/41467/'>RustのLT会！ Rust入門者の集い - connpass</a>での発表用"
title: Rustの話とリソースの話
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Rustの話とリソースの話
----------------------
[RustのLT会！ Rust入門者の集い](https://rust.connpass.com/event/41467/)  
2016-11-21

<!-- .slide: class="center" -->

===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * 若者
 * サイバーエージェントのエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます
 * [プログラミング言語Rust](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/)の翻訳
 * [Join rust-jp on Slack!](http://rust-jp.herokuapp.com/) にもいます。
 * [Rust その2 Advent Calendar 2016 - Qiita](http://qiita.com/advent-calendar/2016/rust-lang-2)

===

# 注意
------

* たまにマニアックな話が出ますが気にせず無視して下さい
* 「なんかRustすげー」って思ってもらえれば幸いです


===

# Rustとの出会い
----------------

* 元々Lisperだった
* 他にはML, Ruby, JVM言語など

===

![lis books](/images/rust-and-resource/lisp.jpg)<!-- .element: height="640px" -->

===

<span style="font-size: 150%">「Lisperは自分で処理系作って一人前」</span>

<!-- .slide: class="center" -->

===

![compiler books](/images/rust-and-resource/compiler.jpg) <!-- .element: height="640px"" -->

===

# 苦悩
-------

* Cは低級すぎる
* Javaは向いてない
* C++は怖そう
  + 闇の軍団
  + 縄文土器飛んできそう
  + あと **nullあるしメモリ破壊あるし**
* MLは向いてるけどシステムプログラミングしづらい
* ATS2, Golang, D...

===

# Rustと出会う
----------------

* 正直最初は色物言語と思ってた
* 調べてみたら気に入った

===

# ゼロコスト抽象化
----------------------------

``` rust
fn sum_pos(v: &Vec<i32>) -> i32 {
    v.iter().filter(|i| **i > 0).sum()
```

===

高階関数が消えた…！？

``` asm
_ZN21higher_order_function7sum_pos17h2f9de4f69306ec0aE:
	.cfi_startproc
	movq	(%rdi), %rcx
	movq	16(%rdi), %rax
	leaq	(%rcx,%rax,4), %rdx
	xorl	%eax, %eax
	jmp	.LBB0_1
.LBB0_3:
	addl	%esi, %eax
	.p2align	4, 0x90
.LBB0_1:
	cmpq	%rcx, %rdx
	je	.LBB0_4
	movl	(%rcx), %esi
	addq	$4, %rcx
	testl	%esi, %esi
	jle	.LBB0_1
	jmp	.LBB0_3
.LBB0_4:
	retq
```

===


# スタックとヒープ
------------------


* Rustは手で割り当てを変えられる
* 「ヒープを使ったら負け」ゲームが出来る
* ループの中でヒープアロケートしたくないよね

===

# マクロとコンパイラプラグイン
-----------------------------

例えば正規表現リテラルを作れるか、とか。

1. Common Lisp
2. 衛生的マクロ + 手続的マクロ <- Rust
2. 衛生的マクロ + コンパイラプラグイン <- Rust
3. 手続的マクロ
4. コンパイラプラグイン
4. 衛生的マクロ

===

# トレイト
----------

* 型クラス
  + 継承を駆逐する
  + MLにも欲しい超便利機能
* 関連型
  + 表現力が高いことが知られている
  + CF [Rustの型レベルLispっぽいの作った | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2016/11/06/rustnokatareberulispppoinotsukutta/)

===

# パターンマッチ、代数的データ型
-------------------------------

* 便利ってかないと困る

``` rust
    match expr {
        &Expr::Nil |
        &Expr::EOF |
        &Expr::Str(_) |
        &Expr::Int(_) |
        &Expr::Float(_) |
        &Expr::Proc(_) => Ok(expr.clone()),
        &Expr::Sym(ref name) => {
            match env.find(&name.to_owned()) {
                Ok(v) => Ok(v.clone()),
                Err(m) => {
                    if name.deref() == "t" {
                        Ok(ksym("t"))
                    } else {
                        Err(m)
                    }
                }
            }
        }
        &Expr::Cons(ref car, ref cdr) => {
        ....
```


===

# C インターフェース
---------------------

* C FFI, C API共に1行
* 構造体なんかも互換

``` rust
extern {
    fn snappy_max_compressed_length(source_length: size_t) -> size_t;
}
```

``` rust
#[no_mangle]
pub extern fn hello_rust() -> *const u8 {
    "Hello, world!\0".as_ptr()
}
```


===

# ランタイムなし
---------------

* [RustでベアメタルRaspberry PiのLチカ | κeenのHappy Hacκing Blog](http://keens.github.io/blog/2016/05/04/rustdebeametaruraspberry_pinolchika/)
* [Writing an OS in Rust](http://os.phil-opp.com/)
* [Redox - Your Next(Gen) OS](http://www.redox-os.org/)

===

# 生ポインタ
------------

* Cとのやりとりで大事
* あるいはパフォーマンスチューニングに

``` rust
pub struct Vec<T> {
    ptr: *mut T,
    cap: usize,
    len: usize,
}
```

``` rust
unsafe fn from_raw_parts(ptr: *mut T, length: usize, capacity: usize) -> Vec<T>
```


===

# エラー処理
------------

* 例外じゃない
* [`Result`](https://doc.rust-lang.org/std/result/enum.Result.html)
* `?` (元 `try!`)も便利
* Erro as a data
* 巻き戻し例外って扱い難しいよね
  + 値継続と例外継続の使い分けつらい

===

# リソースの話（本題）
<!-- .slide: class="center" -->

===

# リソースの重要性
-------------------

* プログラムはIOの塊、すなわりリソースの塊
* 従来は手動で管理するかGCで管理するかしていた
* RustはGCを使わず自動で管理する
* 所有権の概念が「ただの自動」以上に便利

===

# メモリ管理
------------

* Cでいう`free`を自動で挟んでくれる
* 基本

===

# Vecとslice
-------------

* sliceを`Vec`のviewとして使える
  + zero copy
  + 例えばJavaとかだと出来ない
* zero copy parserとかも書ける

===

# `File` 、 `Lock`
------------------

* `Drop`があるので自動
* ところでGCがあるのに手動で管理する言語があるらしいですね
  + 例えばRubyの`File.open(..) do ... end` も半手動
  + GCで処理すべきなのに手で`do ... end`を書いてる

===

# Rust
-------

``` rust
let file = File::open("text.txs").unwrap();
let mut br = BufReader::new(file);
...
```


===

# Java
-------
※try-with-resourceを使うともっと簡単に書けます。極端な比較のためにこう書いてます

``` java
BufferedReader br = null;
try {
    br = new BufferedReader(new FileReader("test.txt"));
    ...
} catch (FileNotFoundException e) {
    e.printStackTrace();
} finally {
    if (br != null)
        try {
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
}
```

===

# Use after close
------------------

* プログラミングエラーの一種
* あらゆるリソースで起きうる
* GCのある言語ではメモリでのエラー **のみ** 防げる
* 他のリソースだとダメ
* Rustは **全ての** リソースで防げる

===

# Ruby
------

``` ruby
file = nil
File.open("file.rb") do |f|
  file = f
end
puts file.read

```

```
#<File:file.rb (closed)>
file.rb:6:in `read': closed stream (IOError)
        from file.rb:6:in `<main>'
```


===

# Rust
-------

* 所有権のある限り勝手に`close`されない

``` rust
let mut file = None;
{
  let f = File::open("file.rs").unwrap();
  file = Some(f);
}
let mut s = String::new();
file.unwrap().read_to_string(&mut s).unwrap();
println!("{}", s);
```

===

# ラッパーオブジェクト
--------------------

* 所有権便利って話

===

# Scala
------

``` scala
val kc = new KafkaProducer(...)
val sk = new SimpleKafkaClient(kc)
kc.close // <- !?!?!?
...
```

===

# Rust
-------

``` rust
let kc = KafkaClient(...);
val mut sk = SimpleKafkaClient(kc);
kc.close(); // <- Compile error because kc has been moved
```

===

# 責任者問題
-----------

* ラッパーオブジェクトの続き
* ラップされたオブジェクトは誰が閉じるの？


===

# Scala
------

``` scala
val kc = new KafkaProducer(...)
val sk = new SimpleKafkaClient(kc)
...
sk.close
kc.close // 本当に必要？
```

===

# Rust
-------

``` rust
let kc = KafkaClient(...);
val mut sk = SimpleKafkaClient(kc); // ここでmove
...
sk.close(); // moveされたskがcolseすることが型で分かる
```

===

# まとめ
--------

* Rust = 便利機能詰め合わせ + 所有権
* さらにランタイムもないしCとの相互連携も出来る
* リソース管理って大事
* 所有権は制限だけじゃないよ



</textarea>
