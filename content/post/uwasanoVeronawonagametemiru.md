---
categories: ["言語処理系", "Verona"]
date: 2020-01-26T17:28:42+09:00
title: "ウワサのVeronaを眺めてみる"
---
κeenです。
先日、Microsoft Research (MSR)からVeronaという言語が公開されました。

* [マイクロソフト、Rustベースのセキュアなプログラミング言語を探求中 - ZDNet Japan](https://japan.zdnet.com/article/35146475/)
* [マイクロソフト、「Rust」に基づくプログラミング言語プロジェクト「Project Verona」がGitHubに - ZDNet Japan](https://japan.zdnet.com/article/35148191/)

これを見た人達の反応が様々で（婉曲表現）面白いな（婉曲表現）ーと思いました。

最近は企業や大きな団体の作った言語がメインストリームで使われることも増えたことから、本来の目的以上に期待を背負ってしまった感じがします。
プログラミング言語は思ったよりも気軽に作られているものです。
例えばGoogleの開発した[ohmu](https://github.com/google/ohmu)という言語はほとんど誰も知りませんよね。
そういうもんです。

以下には私がドキュメントとかコードとか読んだ感想を書きます。
主に読んだのはdocsの[FAQ](https://github.com/microsoft/verona/blob/master/docs/faq.md)や[Explore](https://github.com/microsoft/verona/blob/master/docs/explore.md)、そしてテストスイートです。
残念ながら実装までは踏み込めませんでした。
VMとか結構面白いらしいので解説してくれる人がいたら嬉しいですね。

# このプロジェクトの立ち位置

MSRのプロジェクトということからも分かるように、研究目的の言語です。
決して既存の言語を置き換えようとかそういうものではないです。FAQにも明記されています：

> # When will this be a product?
> This is a research project not a product.

。なのでMicrosoftがC/C++/Rustを使わなくなるということでもないですし、既存の言語を拡張するのは目的が違うというのも理解できると思います。

MSRは色々な言語を作っていて、[P](https://github.com/p-org)や[F*](https://www.fstar-lang.org/)など色々なコンセプトの言語があります。
Veronaもその1つです。

研究目的で開発されている機能がメインストリームに降りてくるのは10年、20年先のことでしょう。
例えばRustは実績のある言語機能を採用するという方針もあり、Rustで使われている所有権やトレイトなどのシステムは20年以上前に論文が出ています。
現段階では「これがあるとしたらどんなことができるかな」くらいの代物と受け取っておくとよいと思います。


# Veronaの目指すもの

「Rustベースの」なんて言われている通り、システムプログラミングをサポートしたいです。
システムプログラミングに求められるものを分解すると以下の2要素が挙げられます。

* 予測可能性
  + レイテンシ
  + リソース使用量
* "生の"アクセス
  + メモリをバイト/ビット単位で扱える
  + ハードウェアを抽象化しない/あまりしない

こういうものを *安全に* 扱いたいよね、というのがRustから続くモチベーションです。
VeronaもおおむねRustと同じく、型システムで安全性を保証しようというアプローチです。
しかし型システムで安全性を保証しようとするのは生のアクセスの方は無理がありそうです（Rustも結局 `unsafe` を使いますしね）。
ところが予測可能性の方はできるんじゃないか、というのが仮説で、それを検証していうプロジェクトのようです。

なのでVeronaは性格にはシステムプログラミング言語ではなくインフラプログラミング言語を謳っています。
性能が安定してるだとか急にレイテンシが跳ねないだとかの方を目指しているようです。
なのでVeronaでOSを書くことはできないですが、データベースなら（将来的には）できるんじゃないでしょうか。

つまるところ、Veronaを評価するときは「どうやってメモリを管理してるか」、「どうやって並行処理に関連する困難を扱っているか」に着目すると良さそうです。
FAQを見るとC++とのFFIは重要視してるようなので生のアクセスの方はC++に任せる肚なのかもしれません。

実際、Veronaがどういう目的（どういう研究）のために作られたかというと、これもFAQに3つ挙げられています。

* 並行な破壊的変更をしない言語を設計したらメモリ管理をスケーラブルにできるか？
* 線型リージョンを使うとメモリ管理は残しつつオブジェクト単位の線形性を緩められるか？
* 言語レベルでリージョンをサポートすると区分け（compartmentalisations）をサポートするのに使えるか？

です。並行な破壊的変更、リージョン、などなどいくつかキーワードが出てきましたね。これはExploreで解説されています。
キーワードと絡めつつVeronaの設計を見ていきましょう。

# Veronaの設計
## 並行な破壊的変更とメモリ安全性

メモリの管理は難しいです。
早めに開放するとdangling pointerになりますし、開放しなかったらメモリリークになります。
とくに並行性が絡むと全てのスレッドを検査してオブジェクトが使われているか調べなければなりません。
要するにGCのStop the Worldですね。
Stop the Worldはアプリケーション全体を止めますし、スレッド数が増えるとパフォーマンスが落ちがちなことが知られています。

一方でRustなどの言語はGCなしでメモリを管理しています。
これは複数スレッドからのアクセスを抑制して1スレッドからのみアクセスできる（所有権）ようにすることで実現しています。

1スレッドからしかアクセスできないと一見不便ですね。これは所有権を別のスレッドに送るれるようにすることで解決しています。
結局スレッド間の相互作用が入ってしまうのですが、2スレッドしか絡まないので全てのスレッドを検査するGCよりは複雑でない動作です。

というのが前提知識。
VeronaはRustやPonyのように所有権を使ってメモリを管理しています。
ところがVeronaは並行な破壊的変更を排除する方向に舵を切りました。
これでは並行キュー（チャネル）などの並行ライブラリをユーザランドで実装することができません。
なので言語にもう少し機能を加えて並行処理を便利にできるようにしたいです。
このとき、2つの方針があります。

* Rustの `unsafe` のように限定的な状況で安全でない操作を許す
  + Rustの並行ライブラリ（ `std::thread::spawn` や `std::sync::mpsc::channel` はユーザレベルで実装されている）
* Ponyのように言語側で安全に操作できるモデルを構築してユーザにはその枠組みの中でプログラムさせる

ここでPonyという言語が出てきました。
4年前のスライドなので古い部分もあるかと思いますが私は以下のスライドが参考になりました。

[Pony concurrency built into the type system](https://www.slideshare.net/matsu_chara/pony-concurrency-built-into-the-type-system-59778750)

capabilityというのが6種類あって、それで並行なアクセスを細かく制御しています。
Rustでいう所有権にあたる `iso` からはじまり `trn` 、 `box` 、 `val` 、 `ref` 、最後はポインタの同値比較のみができる `tag` などがあるようです。

余談ですが上記のPonyの発表のあった会で私はRustの発表をしました。
該当勉強会の参加者は4年越しにピースが繋がった感じがするんじゃないでしょうか。


話を戻すとVeronaはPony方式、つまり言語の規定した並行モデルの上でプログラミングさせる方式のようです。
ただしPonyのモデルそのものではなくて色々アレンジが入っています。それが線形性やリージョンなどです。

## 並行所有権

Rustでいう所有権は型理論では線型型と呼ばれています。
なので所有権システムを持つことを線形性があるなんて言ったりします。


Veronaには並行所有権があります。Concurrent owners、略してcown（コウン）です。
並行所有権ということは複数のスレッドが所有権を共有している訳です。
複数のスレッドから同時にアクセスすると結局何でもありになってしましそうですね。
しかしVeronaのcownは「同時に」アクセスすることはできないようです。
Exploreにあるコード片を眺めてみます。


``` text
// x is some isolated object graph
var c = cown.create(x)
// c is a cown that mediates access to x.
// We have lost direct access to x here
```

コメントにある通り、cownを作るとその変数への直接アクセスを失ってしまいます。
アクセスするときは `when` ブロックを使ってアクセスを得てから実行します。

``` text
when (var x = c)
{
  // Access internals of cown(c) using name x in here
  Builtin.print("Hello\n")
}
Builtin.print("Goodbye\n")
```

さて、面白いことに `when` ブロックは非同期に実行されます。つまり、上記のコードを実行するとHelloをGoodbyeのどちらが先に実行されるか分かりません。

なんとなく[モニタ](https://ja.wikipedia.org/wiki/%E3%83%A2%E3%83%8B%E3%82%BF_(%E5%90%8C%E6%9C%9F))に似ていますが、モニタと違ってブロックしないところが違います。

また、実装レベルの話をすると `when` の中は実質クロージャ相当なのでクロージャをキューに入れてスケジューラで順に実行してあげればランタイムをロックフリーにできます。
よく考えたなーという感じですね。

さらに言うとこのcownはただのアクセス制御だけでなく、リージョンでも重要な役割を果たすので「ただのロックの亜種」と思わない方がいいでしょう。

## リージョン

プログラミング言語の文脈でリージョンというといくつかあってややこしいのですが、メモリ管理の方のリージョンです。
RustやVeronaが参考にしているCycloneで導入されている概念です。
拙いですが以前（もう5年前）ブログに書いたので気になる人は読んでみて下さい。

[リージョンについて | κeenのHappy Hacκing Blog](/blog/2015/12/09/ri_jonnitsuite/)

軽くリージョンの特徴をまとめておきます。

* リージョンはメモリを管理する単位。
  リージョンにメモリをアロケートし、リージョンが終わるときにメモリが開放される
* リージョンは入れ子にできる。
* メモリをアロケートするときはどのリージョンにアロケートするかを指定できる
  + ここがスタック変数などとは違うところ

元々のリージョンは1つの木になっています。
すなわち、プログラム終了まで生きるリージョンを根としてその他のリージョンは全てそのリージョンにぶら下がってる形になります。

翻ってVeronaのリージョンは森になります。木々の根になるのはスタック変数やcownです。cownがリージョンに密接に絡んでるのです。
これを見たときになるほどなーと思いました。
雑に解釈するとスタックをルートとするリージョンはスレッドローカルな値を、cownをルートとするリージョンはスレッド間で使われる値を管理するリージョンという訳ですね。

ところでVeronaのリージョンは私のようにRustを普段使いしてる人からすると驚きの機能があります。
同じリージョン内のオブジェクト同士なら相互参照できるのです。
つまり、グラフを簡単に作れます。
別の解釈をするとRustは1オブジェクト1リージョンに制限されているとも考えられます。
この意味で、VeronaはRustにあった制限を緩めた言語とも捉えられます。


# コードを眺める

Veronaのキーとなる機能を理解したところで、これらの機能を実際に試してみましょう。

## ビルド

[building](https://github.com/microsoft/verona/blob/master/docs/building.md)に書かれてます。
CMakeとninjaを使います。

``` console
$ git clone --recursive https://github.com/microsoft/verona
$ mkdir build_ninja
$ cd build_ninja
$ cmake .. -GNinja -DCMAKE_BUILD_TYPE=Debug
$ ninja install
```

これを走らせると `build_ninja/dist/veronac` が出来上がります。



## 食事する哲学者

並行処理といえばまず最初に挙がる例ですね。
セッティングを知らない方は[Wikipediaの記事](https://ja.wikipedia.org/wiki/%E9%A3%9F%E4%BA%8B%E3%81%99%E3%82%8B%E5%93%B2%E5%AD%A6%E8%80%85%E3%81%AE%E5%95%8F%E9%A1%8C)を読んで下さい。

コードは[こちら](https://github.com/microsoft/verona/blob/master/testsuite/demo/run-pass/dining_phil.verona)にあります。
以下ではエッセンスだけ眺めていきます。

まずは `Philosopher` クラス。cownな `Fork` をimmutableに保持しています。

``` text
class Philosopher
{
  // id used for printing the trace of what happened.
  id: U64 & imm;
  // The two forks this Philosopher is using to eat
  fork1: cown[Fork] & imm;
  fork2: cown[Fork] & imm;
  // The door is used, so we can synchronise the finish to eating.
  door:  cown[Door] & imm;
  // The number of times left for this philosopher to eat.
  hunger: U64 & imm;
}
```

後で `Fork` を変更しているので恐らくここの `imm` は `cown` にのみ適用されて内側の `Fork` の管理はまた別なんだと思います。
`Philosopher` のコンストラクタは以下。

``` text
  /**
   * This static method creates a Philosopher
   *
   * It returns the Philosopher with the capability `iso`.  This is linear
   * capability that expresses unique ownership of this object (and potentially
   * other objects in the same regions). 
   **/
  create(
    n: U64 & imm,
    f1: cown[Fork] & imm,
    f2: cown[Fork] & imm,
    d: cown[Door] & imm): iso & Philosopher
  {
    var p = new Philosopher;
    p.hunger = 10;
    p.fork1 = f1;
    p.fork2 = f2;
    p.door = d;
    p.id = n;
    p
  }

```

`iso & Philosopher` と、 isoなPhilosopherを返しています。
isoは恐らくPony由来の名前でしょう。

そして肝心の `request_eat`。

``` text
  /**
   * This instance method perform the requests to acquire the forks for this 
   * Philosopher.
   * 
   * The Philosopher is passed as an `iso`, so that its linear capability can be
   * sent into the closure of the when expression.
   **/
  request_eat(self: iso)
  {
    // Request the philosophers forks
    // This captures the self parameter in the closure that it schedules.
    when (var f1 = self.fork1, var f2 = self.fork2) 
    {
      // mut-view is an annotation to coerce the `iso` capability to a `mut`
      // capability for this call. When we have more inference for capabilities
      // this will be inferred.
      (mut-view self).eat(f1, f2);

      if (self.hunger)
      {
        // Not zero hunger, so recurse.
        // Though, this is not technically recursion, as this call is actually
        // in the closure created and scheduled by `request_eat`.
        self.request_eat();
      }
      else
      {
        // This Philosopher is finished, so leave the room through the door.
        Builtin.print1("philosopher {} leaving\n", self.id);
        Door.leave(self.door);
      }
    };
    // Accessing self here is an error as it has been captured by the closure
    // Uncommenting the following line illustrates this:
    // self.fork1;
  }
```


`when` でフォーク2つを取得するスケジュールを開始しています。
取得できたら`eat` メソッドを呼んでいます。
その後にまだお腹が空いてたらまた `self.request_eat` を呼んでもう1度スケジューラに自身を積みます。

さて、問題はこれの実行部分。

``` text
class Main
{
  main()
  {
    var f1 = Fork.create();
    Builtin.print1("fork 1: {}\n", f1);
    var f2 = Fork.create();
    Builtin.print1("fork 2: {}\n", f2);
    var f3 = Fork.create();
    Builtin.print1("fork 3: {}\n", f3);
    var f4 = Fork.create();
    Builtin.print1("fork 4: {}\n", f4);

    var d = Door.create(f1, f2, f3, f4);

    var p1 = Philosopher.create(1, f1, f2, d);
    var p2 = Philosopher.create(2, f2, f3, d);
    var p3 = Philosopher.create(3, f3, f4, d);
    var p4 = Philosopher.create(4, f4, f1, d);

    p1.request_eat();
    p2.request_eat();
    p3.request_eat();
    p4.request_eat();
  }
}
```


`Philosopher.create` にそれぞれ左手、右手の順にフォークを渡しています。
これだと全員同時に左手から食べ始めるデッドロックが起きそうな気がしますね。
ところがこれで動いているようです。
恐らく `when` ブロックに渡す `cown` はスケジューラがいい感じに賢く管理してくれて、デッドロックが起きない仕組みなんでしょう。


## リージョン

コードは[ここ](https://github.com/microsoft/verona/blob/master/testsuite/demo/run-pass/region101.verona)にあります。
循環のある連結リストを作る例。

デストラクタでプリントするノードを定義しています。

``` text
class Node
{
  id: U64;
  field: (Node & mut) | (None & imm);

  // Self should really be read-only, but not implemented yet.
  // This is called when the runtime deallocates this object.
  final(self: mut)
  {
    Builtin.print1("Deallocating id {}\n", self.id);
  }
}

```

`(Node & mut) | (None & imm)` のように、 `&` や `|` の記号が使われていますね。
それぞれ交差型と合併型だと思います。


これらを使って閉路を作っていきます。
まずはノードを作るところ。

``` text
// Allocate a new node in its own region.
var r1 = new Node;
// Allocate two nodes in the same region as r1.
var r2 = new Node in r1;
var r3 = new Node in r1;

// Give nodes an id for logging 
r1.id = 1;
r2.id = 2;
r3.id = 3;
```


注目してほしいのは `r2` 、 `r3` の `in r1` の部分です。
この記法で `r1` と同じリージョンにアロケートしています。

同じリージョンにノードを確保できたら閉路を作ります。

``` text
// Create a little graph that has a cycle
r1.field = r2;
r2.field = r3;
r3.field = mut-view(r1);
```

最後の `r3` だけそのままの代入ではなく `mut-view` を噛ませていますね。
流石に所有権があるので `iso` では渡せなくてviewで渡しているようです。
実際、最後の `mut-view` を外すとエラーになります。

``` text
$ ./dist/veronac --run ../testsuite/demo/run-pass/region101.verona
../testsuite/demo/run-pass/region101.verona:42:3: error: Inference failed for method test1
  test1()
  ^~~~~~~
1 error generated
```


`mut-view` を外す前の世界線に戻って、 `r1` を別のノードで上書きしてみます。

``` text
r1 = new Node;
r1.id = 4;
Builtin.print("Update\n");
```

こうすると `r1` が開放されるのはもちろんのこと、 `r2` 、 `r3` も開放されます。
実際、この行のあとに `r2` を使おうとするとコンパイルエラーです。

``` text
Builtin.print1("r2.id = {}\n", r2.id);
```

``` text
$ ./dist/veronac --run ../testsuite/demo/run-pass/region101.verona
../testsuite/demo/run-pass/region101.verona:79:36: error: Cannot use variable '4'
    Builtin.print1("r2.id = {}\n", r2.id);
                                   ^~
../testsuite/demo/run-pass/region101.verona:73:5: note: Its parent, '3', was overwitten here
    r1 = new Node;
    ^~~~~~~~~~~~~
```


これまたコンパイルが通る時の世界線に戻って実行してみると、確かに id 1のノードを開放したらすぐさまid 2、id 3も開放されて、関数の最後にid 4のオブジェクトが開放されているのが分かります。

``` text
$ ./dist/veronac --run ../testsuite/demo/run-pass/region101.verona
Deallocating id 1
Deallocating id 3
Deallocating id 2
Update
Deallocating id 4
```

## その他

型システムは構造的部分型で交差型と合併型があるようです。
例えば以下のように返るのが `A` だか `B` だか分からないようなものもちゃんと（不動点をとって？） `A | B` と判定できます。


``` text
class A { f: B & mut; }
class B { f: A & mut; }

class Main {
  main() { }

  fixpoint(a: A & mut) : (A | B) & mut
  {
    var current = a;
    while 1
    {
      current = current.f;
    };
    current
  }
}

```


コードは [これ](https://github.com/microsoft/verona/blob/master/testsuite/features/compile-pass/fixpoint.verona)。


あとはテスト用にスケジューラの挙動を制御できるだとかの面白い機能もあります。

# 結びに

MSRの研究プロジェクト、Veronaを紹介しました。
まだ論文も出ていない、はじまりかけですが十分面白いプロジェクトだと思います。

個人的にはユーザが陽に扱えるリージョンやリージョンとcownの関係、リージョン内での相互参照は可能な設計など、リージョンを上手く使ってるなーと感心しながら見てました。

今回はさらっとドキュメントやコードを眺めただけなので複雑なコードやコンパイラ、ランタイムには踏み込めませんでした。
ランタイムもメッセージパッシング向けのアロケータ[nmalloc](https://github.com/microsoft/snmalloc)を使っているなど、探せば話題に事欠かなそうです。
興味のある方は是非実装を眺めてみて下さい。

