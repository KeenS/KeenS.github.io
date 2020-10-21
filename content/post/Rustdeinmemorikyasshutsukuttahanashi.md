---
categories: ["Rust"]
date: 2020-10-20T03:37:17+09:00
title: "Rustでインメモリキャッシュ作った話"
---

κeenです。先日発表した[Rustで作るインメモリキャッシュ](https://keens.github.io/slide/rustdetsukuruinmemorikyasshu/)が全然話し足りなかったので色々補足します。
<!--more-->

実は今回の話題はソフトウェアというよりはハードウェアの仕組みに由来するものなのですが、ソフトウェアにも応用できるだろうということで試してみた結果です。

ハードウェアでもソフトウェアでもアクセスするデータは時間局所性、つまり短期間にアクセスするデータには偏りがあるというのが仮定としてあります。
よくある例が[冪乗則](https://ja.wikipedia.org/wiki/%E5%86%AA%E4%B9%97%E5%89%87)、あるいは80:20の法則とよばれるもので、アクセス数上位20%のものが全体の80%のアクセスを占めたりします。
データ全部はメモリに乗らないけど20%くらいだったら載るようなケースではその20%を上手く選んでメモリに載せられる仕組みがあればパフォーマンスがあがるはずです。
ということでそのような仕組み、キャッシュがほしいよねというのがモチベーションです。

# キャッシュの話

まずはキャッシュの一般論から。
Wikipediaなんかも参考にして下さい。
[キャッシュメモリ - Wikipedia](https://ja.wikipedia.org/wiki/%E3%82%AD%E3%83%A3%E3%83%83%E3%82%B7%E3%83%A5%E3%83%A1%E3%83%A2%E3%83%AA)


キャッシュとは容量制限のあるハッシュマップのようなものです。
ハッシュマップとの違いは、アイテムを新たに挿入するときに十分なスペースが足りなければ容量を増やすのではなく既存のアイテムを削除する点です。

このときにどのアイテムを削除するかがパフォーマンスに直結する工夫点です。

## 場所

削除と密接に絡む、「どのデータをどこに挿入するか」の話から。

ハッシュマップのようなものなので、まずはキーのハッシュ値を計算して挿入したい場所をみつけます。

```text
hash(k) = 1

    insert(k, v)
      v
+---+---+---+--
|   |   |   | ...
+---+---+---+--
```

このときそこが空いていれば何も問題なく挿入できます。

```text
hash(k) = 1

    insert(k, v)
      v
+---+---+---+--
|   |???|   | ...
+---+---+---+--
```

一杯だった場合にとれる方法がいくつかあります。

```text
hash(k) = 1

    insert(k, v)
      v
+---+---+---+--
|   |///|   | ...
+---+---+---+--
```

ひとつはその場にあるデータをそのまま追い出して、置き換えてしまう方式。

```text
hash(k) = 1

    insert(k, v)
      v
+---+---+---+--
|   |???|   | ...
+---+---+---+--
      ↓
     ///
```

これは **ダイレクトマップ** 方式とよばれます。愚直で速そうですね。
しかし他に領域が空いていてもそれを使わないので空間効率は悪そうです。

もう1つは本来の場所を起点にして空いているところを探していく方式。
ずっとみつからなければ配列を一周します。


```text
hash(k) = 1

    insert(k, v)
      ........v
+---+---+---+---+--
|   |///|///|???|...
+---+---+---+---+--
```

これは **フルアソシアティブ** 方式と呼ばれます。
ダイレクトマップ方式の真逆で空間効率は最高ですが、最悪のケースで配列を全て舐めるので速度は悪いです。

ダイレクトマップとフルアソシアティブの折衷案として、衝突したら数個だけ近辺をみてまわるというのがあります。
例えば3つだけ近辺をみてまわるとしましょう。

```text
hash(k) = 1

    insert(k, v)
  +........+|
  v   .....+|
+---+---+---+---+--
|???|///|///|???|...
+---+---+---+---+--
```

配列を3つごとのブロックに区切って、その中で一周するように探索します。
その中に空きがあればそこを、なければ何かしらのデータを追い出します。

これは **セットアソシアティブ** 方式と呼ばれます。
1セットにn要素ある方式を n-ウェイセットアソシアティブ方式と呼びます。
上の例は3-ウェイセットアソシアティブ方式ですね。

n = 1の場合はダイレクトマップ方式になりますし、n = 配列の長さの場合はフルアソシアティブ方式になります。
そういう意味で両者の折衷案となっています。

両者の折衷案ということで、ほどほどの空間効率とほどほどの速度が期待できます。
もちろん、nが小さければ特徴がダイレクトマップ方式に寄りますし、nが大きければフルアソシアティブ方式に近い特徴になります。


## 削除

ダイレクトマップ方式はそのまま削除するデータまで指定するのでフルアソシアティブ方式とセットアソシアティブ方式が対象です。

「将来再度使われるのが最も遅いデータ」を削除できると効率的にメモリを使えるのですが、未来予知はできないので絵に描いた餅。
次善の策として「将来一番使われなそうな」データを削除するのが目標になります。

輪番削除などの方法もあるのですが、あまり効率はよくないようです。
似たような気がするランダム削除は実はそこまで悪くないらしいです。
実装も簡単そうですしいざとなったらこれで実装するのもよさそうです。

ある程度効率がいいとされているのがLeast Recently Used（LRU）、最後に使ったのが最も古いデータを削除する方法です。
時間局所性の仮定下では最近使われていないデータは将来も使われる可能性が低いので合理的な気がします。

LRUの基本的な動きを説明しておきます。

まず `1` というデータがきたらリストの先頭に置きます。

<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#new-1" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#new-1" />
    <use xlink:href="#arrow-top" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-1" />
</svg>

次に `2` というデータがきたら `1` のデータを1つ下にずらして2のデータを置きます。

<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-1" />
    <use xlink:href="#new-2" />
</svg><svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-1" />
    <use xlink:href="#new-2" />
    <use xlink:href="#arrow-top" />
    <use xlink:href="#arrow-1-2" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-2-1" />
</svg>

`3` がきたら同様に `1` 、 `2` をずらして3を置きます。

<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-2-1" />
    <use xlink:href="#new-3" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-2-1" />
    <use xlink:href="#new-3" />
    <use xlink:href="#arrow-top" />
    <use xlink:href="#arrow-1-2" />
    <use xlink:href="#arrow-2-3" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-3-2-1" />
</svg>

次にデータが一杯の状態で `4` がきたら `1` 、 `2` 、 `3` をずらしてデータを置きます。
このとき溢れた `1` は削除されます。

<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-3-2-1" />
    <use xlink:href="#new-4" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-3-2-1" />
    <use xlink:href="#new-4" />
    <use xlink:href="#arrow-1-2" />
    <use xlink:href="#arrow-2-3" />
    <use xlink:href="#arrow-3-out" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item--3-2-1out" />
    <use xlink:href="#new-4" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item--3-2-1out" />
    <use xlink:href="#new-4" />
    <use xlink:href="#arrow-top" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-4-3-2" />
</svg>

最後に、既に保持されている `2` というデータがきたらそれより後にきたデータを1つずらし、 `2` を先頭にもってきます。

<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-4-3-2" />
    <use xlink:href="#new-2" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-4-3-2" />
    <use xlink:href="#new-2" />
    <use xlink:href="#arrow-1-2" />
    <use xlink:href="#arrow-2-3" />
    <use xlink:href="#arrow-3-1" />
</svg>
<svg width="160" height="120" xmlns:xlink="http://www.w3.org/1999/xlink">
    <use xlink:href="#base-box" />
    <use xlink:href="#item-2-4-3" />
</svg>


というのがキャッシュの一般論でした。これを踏まえて、実装についてみていきましょう。

# 今回の実装の話

今回は16-ウェイセットアソシアティブ方式を実装しました。
コードは以下にあります。

[κeen / chechire · GitLab](https://gitlab.com/blackenedgold/chechire)

これについて色々話題があるので拾っていきます。

## LRUの実装

LRUの説明でリストのようなものがでてきたことからも分かるように、LRUはリスト（又はベクタ）があれば実装できます。
教科書的には順番の入れ替えが得意なリストの出番ですが、残念ながら最近のCPUではベクトルの処理は速くリストのようなポインタ処理は遅いです。
Rustの[LinkedListのドキュメント](https://doc.rust-lang.org/std/collections/struct.LinkedList.html)にもこう書かれています。

> NOTE: It is almost always better to use Vec or VecDeque because array-based containers are generally faster, more memory efficient, and make better use of CPU cache.

> 訳 注: 一般に配列ベースのコンテナの方が速く、メモリ効率的でかつCPUキャッシュをより利用できるので、ほぼ常に `Vec` や `VecDeque` を使った方がよいです。

今回必要な操作は先頭にデータを追加する処理、リストの途中にあるデータを先頭にもってくる処理と、一番古いデータを捨てる処理です。
`Vec` は末尾にデータを挿入する処理が得意なので末尾に新しいデータを挿入し、古いデータを捨てるときだけ先頭から取り出すようにすればそこまで効率は悪くないはずです。

というのがわかりやすい実装です。

実はLRUは少数であればbit演算でも管理できることが知られています。
主にセットアソシアティブ方式を想定したアルゴリズムですね。

上の例ではキャッシュのキーを直接管理してましたが、こちらはそのキーが置かれる場所、バケットのインデックスで管理します。

```text
  0   1   2
+---+---+---+
|   |   |   |
+---+---+---+
```

「0番目のバケットが使われた」「2番目のバケットの内容は古いから捨ててもよい」のように管理すれば数値を扱うことになるのでbit演算ができます。

さて、肝心のアルゴリズムですが、まずはN x Nのビットテーブルを用意します。今回は3x3ですね。
ひとまず0で埋めておきましょうか。

```text
   0 1 2

0  0 0 0
1  0 0 0
2  0 0 0
```


N番目が使われたとき、 N列目を全て0にして、N行目を全て1にします。

例えば0、2、1、2の順で使われたとしましょう。

まず0が使われたので0列目を全て0にします。

```text
 0列目
   v
   0 1 2

0  0 0 0
1  0 0 0
2  0 0 0
```

そして0行目を全て1にします。

```text
   0 1 2

0  1 1 1 < 0列目
1  0 0 0
2  0 0 0
```

次に2が使われたので2列目を全て0にします。

```text
      2列目
       v
   0 1 2

0  1 1 0
1  0 0 0
2  0 0 0
```

そして2行目を全て1にします。

```text
   0 1 2

0  1 1 0
1  0 0 0
2  1 1 1 < 0列目
```

次に1が使われたので1列目を全て0にします。

```text
    1列目
     v

   0 1 2

0  1 0 0
1  0 0 0
2  1 0 1
```

そして1行目を全て1にします。

```text
   0 1 2

0  1 0 0
1  1 1 1 < 1行目
2  1 0 1
```

最後に再度2が使われたので2列目を全て0にします。

``` text
    2列目
       v
   0 1 2

0  1 0 0
1  1 1 0
2  1 0 0
```

そして2行目を全て1にします。

``` text
   0 1 2

0  1 0 0
1  1 1 0
2  1 1 1 < 2行目
```

こうやって管理したbitを行単位で1つの数値と思うと、その数値の大小が最後に使われた順序になっています。
上の例だと

```
0: 100 = 4
1: 110 = 6
2: 111 = 7
```

なので 0 < 1 < 2 と、最後に使われた順序と等しいです。

数値の大小が最後に使われた順ということは、数値が最小のものがLRUとなります。
あとはLRUを削除するときにそこの行を全て0にしてあげれば完成です。

今回16-wayを選んだのはRust的に `u16` を16個の配列であれば効率的に実装できるかなと思ったからです。

私が実装したのも、ほぼ上記のままです。行を数値として比較したいので1行を `u16` で表現することにして、それを16行分まとめたものを `[u16;16]` として表現しています

``` rust
pub struct LeastRecentlyUsed {
    bit_table: [u16; 16],
}

impl LeastRecentlyUsed {
    pub const SIZE: usize = 16;

    pub fn refer(&mut self, index: usize) {
        debug_assert!(index < Self::SIZE);
        let mask = !(1 << index);
        for b in &mut self.bit_table {
            *b &= mask;
        }
        self.bit_table[index] = !0;
    }

    pub fn delete(&mut self, index: usize) {
        debug_assert!(index < Self::SIZE);
        self.bit_table[index] = 0;
    }

    pub fn least(&self) -> usize {
        let mut min = u16::MAX;
        let mut min_index = 0;
        for (i, &b) in self.bit_table.iter().enumerate() {
            // ignoring b == 0 because hashbrown assumes there is at least 1 blank bucket
            // thus there is at least 1 unavailable bucket.
            if 0 < b && b < min {
                min = b;
                min_index = i;
            }
        }
        min_index
    }
}
```


## テーブル側

LRUでテーブルの内部実装にある配列の各インデックスにアクセスできるという仮定を置きました。
しかし標準ライブラリの `HashMap` ではそういう機能はありません。

ところで標準ライブラリの `HashMap` と同じものがcrates.ioに公開されています。
[hashbrown](https://crates.io/crates/hashbrown)といいます。
というか元々は標準ライブラリ互換の速いハッシュマップとして公開されていたものが標準ライブラリ側が取り込んだ形になります。
普通の言語だとそもそもユーザレベルでハッシュマップを効率的に実装できなかったり、できたとしても自動で標準ライブラリに依存してしまうので標準ライブラリに取り込めなかったりするのですが `no_std` などの仕組みが整ったRustではこういうことができてしまう訳です。

ちょっと話が逸れましたね。
このhashbrowですが実験的機能として[`raw`](https://docs.rs/hashbrown/0.9.1/hashbrown/raw/index.html)というモジュールを公開しています。
これはハッシュマップの実装に使われているデータ構造の内部実装を公開するAPIで、先程言及した内部で使ってる配列へのインデックスアクセスも含みます。
あるいは `insert_no_grow` のように領域が足りなくてもメモリを拡張しない（領域が足りなかったら未定義動作）APIなどのキャッシュの実装にはおあつらえ向きなAPIがあります。
これらをありがたく使わせてもらうことにします。

一方で、ハッシュマップを前提にした実装なので困った点などもいくつかあるのですが、それは後ろの方で紹介したいと思います。



## 全体の実装

キャッシュポリシーとテーブが出揃ったので全体の話をします。
hashbrownの `raw` を使っている関係上、また設計を真似る意味でも低レベルな `RawCache` とその上に作られた `CacheTable` の2つを作っていくことにします。
`RawCache` のAPIをhashbrownの `RawTable` に似せれば `CacheTable` 部分はほぼコピペで実装できるだろうという魂胆です。

今回の実装は16-ウェイアソシアティブなキャッシュなので、小さなキャッシュテーブルを複数束ねた形になります。これはRustのコードを見た方が話が早いですかね。

``` rust
pub struct Block<T> {
    pub(crate) table: RawTable<T>,
    policy: RefCell<LRU>,
}

pub struct RawCache<T> {
    blocks: Vec<Block<T>>,
}
```


これを使って、気になる `insert` は以下のように実装されています。

``` rust
pub fn insert(&mut self, hash: u64, value: T) -> Bucket<T> {
    let block_index = self.block_index(hash);
    let cache_hash = self.block_hash(hash);
    let block = self.block_mut(hash);
    unsafe {
        // もしテーブルが満杯だったら
        if (block.table.capacity() - block.table.len()) == 0 {
            // Least Recently Usedなデータを取得して
            let least = block.policy.borrow().least();
            let least_bucket = block.table.bucket(least);
            // それを消して
            block.table.erase(least_bucket);
            // LRUの方からも消す
            block.policy.borrow_mut().delete(least);
        }
        debug_assert!(0 < block.table.capacity() - block.table.len());
        // 必ず空きがあるはずなので `insert_no_grow`
        let bucket = block.table.insert_no_grow(cache_hash, value);
        // LRU を更新する
        let bucket_index = block.table.bucket_index(&bucket);
        block.policy.borrow_mut().refer(bucket_index);
        Bucket {
            bucket,
            block: block_index,
        }
    }
}
```

ほぼ理論通りの実装です。 `find` などの参照系の実装でもLRUを更新する処理が入っています。

さて、ここでブロックとハッシュの話をしましょう。
`Block` の `Vec` があって、さらに `Block` の中のハッシュテーブルの中に配列（のようなもの）があるので2段階のインデックスアクセスが必要になります。

``` text
RawCache
    +-------+--
    | Block | ...
    +-------+--
       |
       |
      / \
Block
    +--------+--
    | Bucket | ...
    +--------+--
```

ブロックを選択する方を `block_index` 、ブロック内でデータにアクセスする方は普通のハッシュテーブルに任せてハッシュ値をそのまま使うのでて `block_hash` （あるいは cache hash）と呼んでいます（もっと良い名前が欲しい）。
`insert` の先頭部分で計算しているのがそれです。

``` rust
pub fn insert(&mut self, hash: u64, value: T) -> Bucket<T> {
    let block_index = self.block_index(hash);
    let cache_hash = self.block_hash(hash);
    // ...
}
```


これらの計算には普通のハッシュテーブルのようにハッシュ値を使うのですが、注意が必要です。
安直に思いつくのは `block_index` にハッシュ値を `Vec` の長さで割った余り、 `block_hash` にハッシュ値そのままというものです。

``` rust
fn block_index(&self, hash: u64) -> usize {
    let num_blocks = self.blocks.len();
    (hash as usize) % num_blocks
}

fn block_hash(&self, hash: u64) -> u64 {
    hash
}
```

しかしこれだとよくないです。
`hash % num_blocks` でブロックを振り分けているということは、1ブロック内でハッシュ値の下位bitが揃ってしまいます。

``` text
RawCache
    +-------+--
    | Block | ...
    +-------+--
       | hash % num_block = 0 を振り分け
       |
      / \
Block
    +--------+--------+--
    | Bucket | Bucket |
    +--------+--------+--
      0100     1100
         ^        ^
    下2bitが全部00で揃ってしまう
```

ハッシュテーブルはハッシュ値がきれいにバラけたときに効率的なので、これだとパフォーマンスが下がってしまいます。

もう1つ、hashbrown特有の罠もあります。
下位bitを使い回すのがダメなら下のbitを捨ててしまえという発想もありえます。

``` rust
fn block_index(&self, hash: u64) -> usize {
    let num_blocks = self.blocks.len();
    (hash as usize) % num_blocks
}

fn block_hash(&self, hash: u64) -> u64 {
    let num_blocks = self.blocks.len();
    // block_indexの計算に使ったbitを捨てる
    hash / (num_blocks as u64)
}
```

あるいは下位bitが駄目なら上位bitで計算すればいいじゃないかという案もあるでしょう。
しかし、hashbrownは簡単に（false positiveありで）キーが一致するかを確認するために上位7bitをテーブル内に保持しています。
下位bitを捨てて上位bitに0を埋めたり、上位bitでブロックを振り分けたりするとまたハッシュ値が衝突します。

つまり、上位7bitと下位数bit（今回は16-wayなので4bit）は予約済みで動かせないと考えてよいです。


``` text
  上位7bit                   下位4bit
 ________________             _______
/                \           /       \
+--+--+--+--+--+--+--....-+--+--+--+--+
63 62 61 60 59 58 57      4  3  2  1  0
```


ということで `block_index` の方で工夫するとして、効率のいい実装は以下です。

``` rust
fn block_index(&self, hash: u64) -> usize {
    let num_blocks = self.blocks.len();
    // ハッシュで使う分のbitを除けて計算
    ((hash as usize) / LRU::SIZE) % num_blocks
}

fn block_hash(&self, hash: u64) -> u64 {
    // ハッシュ値そのまま
    hash
}
```



## ベンチマーク

効率とかの話をしたのでベンチマークをとりましょう。

アクセス頻度にばらつきのあるベンチマークをとることになります。
そういう偏りのある乱数を得るのに、[rand_distr](https://crates.io/crates/rand_distr)クレートにある[WeightedIndex](https://rust-random.github.io/rand/rand_distr/weighted/struct.WeightedIndex.html)がまさしくな機能を提供しています。
例えば `WeightedIndex::new([6, 3, 1])` とすれば `0` が60%、`1` が30%、 `2` が10%の確率ででてくる分布になります。

これを使ってベンチマークのデータ生成の方をこう実装します。

``` rust
impl WorkLoad {
    fn next<R: Rng>(&mut self, rng: &mut R) -> usize {
        self.weights.sample(rng)
    }
}
```

それぞれの重みは適当に計算しています。
本当はべき乗則に従った実装をしたつもりだったのですが、よく調べずに書いたので間違ってました（ $x^n$ を計算しないといけないところを、 $a^x$ を計算してました）。
はずかしいので実装は載せません。まあ、ベンチマークには最終的なキャッシュヒット率の方が大事なので問題ないでしょう。

ベンチマーク対象は以下のようになっています。

``` rust
struct Subject<C> {
    cache: C,
    hits: u64,
}

impl<C> Subject<C>
where
    C: Cache,
{
    fn new(cache: C) -> Self {
        Self { cache, hits: 0 }
    }
    fn test(&mut self, data: usize) {
        match self.cache.get(&data) {
            Some(&data) => {
                self.hits += 1;
                data
            }
            None => {
                thread::sleep(Duration::from_micros(500));
                self.cache.insert(data, 0);
                0
            }
        };
    }
}
```

キャッシュがヒットしなかったら500μsスリープする実装です。
500μsというのは速めのDBアクセスがこのくらいかなという感覚値です。
また、キャッシュヒット率計算のために `hits` を保持しています。

そしてベンチマーク本体はおおむね以下の形です。

``` rust
fn run_bench<C: Cache>(name: &str, s: &mut Subject<C>) {
    let count = 1_000_000;
    let mut w = WorkLoad::new(100_000_000_000).unwrap();
    let mut rng = thread_rng();

    // ...

    for _ in 0..count {
        s.test(w.next(&mut rng))
    }

    // ...
}
```

このベンチマークコードで以下の3つをベンチマークしてみました。

* chechire: 今回実装したコード
* easy cache (vec): LRUにstdの `Vec`、テーブルにstdの `HashTable`
* easy cache (deque): LRUに `VecDeque`、テーブルにstdの `HashTable`

LRUをベクタで実装する際に先頭と末尾両方の操作があるので念のため `Vec` と `VecDeque` 両方を試してみました。
本当は `LinkedList` も比較したかったのですが [`remove` がnightlyのみ](https://doc.rust-lang.org/std/collections/struct.LinkedList.html#method.remove) なのであきらめました。コレクションで削除ってかなり基本的な気がするんですが、それが安定化していないあたり `LinkedList` の需要の少なさが伺えますね。


さてさて、余談は置いておいてベンチマークをしましょう。
chechireは16-ウェイセットアソシアティブでeasy cacheはフルアソシアティブなのでキャッシュヒット率ではchechireが劣りますが、基礎パフォーマンスは高いはずです。

ベンチマークした結果が以下です。

<style type="text/css">
.graph{
  background:#aaa;
  border-radius:5px;
  white-space: nowrap;
  text-align: left;
}
td {
  white-space: nowrap;
}
</style>


| subject             | hit rate   |  time
|:--------------------|------------|-----------------------------------------------------------------
| chechire            |  98.9537%  | <div class="graph" style="width:calc( 5958px * 0.05 );">5958ms</div>
| easy cache (vec)    |  99.1176%  | <div class="graph" style="width:calc( 6266px * 0.05 );">6266ms</div>
| easy cache (deque)  |  99.1117%  | <div class="graph" style="width:calc( 6469px * 0.05 );">6469ms</div>



キャッシュのヒット率は劣るものの、全体的な速度ではchechireが上回ってます。
乱数要素があるのですが、くりかえし回数（ `count` ）が十分大きいので何度か計測してみてもおおむねこのような結果になりました。

という訳で速い実装ができました。
めでたしめでたし…としたいところなのですが、これはまやかしです。
LTするときに綺麗な落ちがほしかったのでこういう結果を載せました。
ちょっと事情を詳しく説明しましょう。

キャッシュのパフォーマンスはほぼキャッシュヒット率で決まります。
もうちょっと正確にいうと、(1 - キャシュヒット率)をキャッシュミス率とすると

$a$ をキャッシュミス率 、 $X$ をキャッシュミスしたときにかかる時間 、 $Y$ をキャッシュヒットしたときにかかる時間 とすると、平均パフォーマンスは

\\\[
aX + (1 - a)Y
\\\]

となります。ここでほとんどの場合では $X >> Y$ なので、キャシュの性能はほぼ以下となります。

\\\[
aX
\\\]

$X$ はコントロールできないのでキャッシュミス率がほぼそのままキャッシュのパフォーマンスを左右するのです。

この原則に従わないのは $a$ が極端に小さいか、 $X$ と $Y$ の差がそこまで大きくないかの場合になります。

上記のベンチマークでchechireが速いように見えたのはまさにそれです。
キャッシュミス率が1%前後と小さいですし、 $Y$ が $X$ より十分小さくなかったからです。
実際、スリープの時間を500μsから1msにすると結果が逆転します。

| subject             | hit rate   |  time
|:--------------------|------------|-----------------------------------------------------------------
| chechire            |  98.9549%  | <div class="graph" style="width:calc( 11159px * 0.025 );">11159ms</div>
| easy cache (vec)    |  99.1131%  | <div class="graph" style="width:calc( 10767px * 0.025 );">10767ms</div>
| easy cache (deque)  |  99.1094%  | <div class="graph" style="width:calc( 11040px * 0.025 );">11040ms</div>

みなさん数値に騙されないようにしましょうね。


# こぼれ話とか

話の流れをスムーズにするために省いた話題を拾っていきます。

## プランAが失敗した話

今回のキャッシュは、なんとなくアイディアだけあってLTに申し込んで、その後に実装をはじめました。
そのとき思い浮かんでいた実装がプランA、それが失敗したときの代替案のプランBとCがありました。
A、B、Cの順で実装難易度が高いです。
そしてベンチマークのところででてきたchechireがプランB、easy cacheがプランCです。

ではAはというと、 `RawTable` をカスタマイズする案でした。
カスタマイズするのはいくつかの理由があります。

### データ構造的効率

1つはデータ構造的効率です。

`RawCache` は同じサイズの `RawTable` を複数個 `Vec` に格納しています。
`RawTable` 内にポインタが1つ、 `Vec` 内にポインタが1つでデータにアクセスするのに都合2つのポインタを経由します。これが無駄なので1つにまとめられないかと考えていました。
メモリアロケーション的にも1回でドカンとメモリを確保した方が効率がいいはずです。
また、それぞれのハッシュマップはサイズ情報を持ちますが、今回は16で固定なのでその情報も無駄ですので、これを省こうという意図もありました。

### アルゴリズム的効率

もう1つはアルゴリズム的効率です。

ハッシュマップは場合によっては巨大にもなる要素数に対応しています。
ハッシュマップの空き部分が少なくなると極端に効率が悪くなるので、hashbrowでは持っているデータ領域の85%が埋まるとデータ領域を拡張するようにできています。
バケットを走査するときに「空きバケットがみつかるまで続ける」などの処理をしているので適度に空き領域がないといけません。そして最低でも1つは空きがないと無限ループしてしまいます。

一方でキャッシュの実装では16個と決めてたので巨大なデータは想定しなくていいですし、空きバケットなく完全に埋めてしまっても問題ありません。


こういった部分で妥協したくなかったのですが、ちょっと難しかったです。
`RawTable` と `Vec` のポインタを統合するということは2つのデータ構造を同時に実装するということでもあります。さらにはポインタを減らすために `RawTable` に保持しているフィールドもどこかのメモリ領域に埋め込んだりなど、メモリレイアウトの工夫も必要でした。これは考えるのも実装するのもかなり大変でした。

また、 `RawTable` にはイテレータなど関連するデータ型が多かったのも障壁の一つでした。
実装を変えるときにどこをいじったらどのデータ型に影響があるのかがわかりづらかったので途中で思考を放棄してあきらめてしまいました。

## `ProbeSeq` が面白い

上述のようにhashbrownのコードを読んでいたのですが、その中で使われている `ProbeSeq` が面白かったので紹介します。

`ProbeSeq` はあるバケットに着地したあと、そこに要素があったら他のバケットを探しにいくときに使うイテレータです。次の候補となるインデックスを返してくれます。

愚直にやるならすぐ隣をみにいけばよさそうです。


``` text
  1   2   3   4   5
  v   v   v   v   v
+---+---+---+---+---+
|   |   |   |   |   |
+---+---+---+---+---+
  0   1   2   3   4
```

しかしこれだと一度要素が埋まってるカタマリが産まれると、周辺のアクセス効率がひどく悪くなります。
例えば下の例だと0、1、2、3のどれからスタートしても隣が埋まってるので右に辿りつづけて、全て4に落ち着きます。そしてそこにデータを挿入するとさらにカタマリが大きくなってしまします。


``` text
      1   2   3   4   5
      v   v   v   v   v
          1   2   3   4   5
          v   v   v   v   v
  1   2   3   4   5
  v   v   v   v   v
+---+---+---+---+---+
|///|///|///|///|???|
+---+---+---+---+---+
  0   1   2   3   4
```

これは隣に限らず、規則的なアクセスだと大抵起こってしまう問題です。
だからといえって不規則にアクセスしていたらどれにアクセスしてどれにアクセスしてないかが分からなくなります。
そこで「不規則っぽくアクセスする規則」があるとうれしいです。
特に全ての要素を丁度1度づつ訪問できると最高です。

hashbrownの `ProbSeq` はそれを実現しています。しかもめちゃくちゃ簡単なアルゴリズムで。

使っているのは[三角数](https://ja.wikipedia.org/wiki/三角数)です。
これは一辺がnの正三角形に含まれる○の数で、具体的には1、3、6、10、15…となっています。n番目の三角数を $T_n$ で表わします。

``` text
1  3    6     10
O  O    O      O
  O O  O O    O O
      O O O  O O O
            O O O O
```

hashbrowのデータを保持する配列（のようなもの）は2のべき乗なのですが、最初の $2^n$ 個の三角数をそれぞれ $2^n$ で割ったあまりの集合がちょうど $\\{0, 1, \cdots, 2^n - 1\\}$ となっているそうです。これなら全ての要素をちょうど一回ずつ訪問できますね。

実装上は $T\_n - T\_\\{n - 1\\} = n$ を利用して、1つ隣、そこから2つ隣、そこから3つ隣…と1つずつアクセス幅を増やしていけばよいです。


``` text
  1   2       3           4
  v   v       v           v
+---+---+---+---+---+---+---+--
|   |   |   |   |   |   |   | ...
+---+---+---+---+---+---+---+--
  0   1   2   3   4   5   6
```

実はこれ、Art of Compute Programming, Volume 3の6.4章に載っているらしいです。持ってないので知りませんでした。

また、2の羃と三角数のあまりについての証明はこちらの記事にあります。

[Triangular numbers mod 2^n | The ryg blog](https://fgiesen.wordpress.com/2015/02/22/triangular-numbers-mod-2n/)


ところで、三角数は無限にあるのでこのイテレータは終わりません。
実装をみても常に `Some(result)` を返しています。

``` rust
struct ProbeSeq {
    bucket_mask: usize,
    pos: usize,
    stride: usize,
}

impl Iterator for ProbeSeq {
    type Item = usize;

    #[inline]
    fn next(&mut self) -> Option<usize> {
        // We should have found an empty bucket by now and ended the probe.
        debug_assert!(
            self.stride <= self.bucket_mask,
            "Went past end of probe sequence"
        );

        let result = self.pos;
        // SIMD幅単位でアクセスしてるので
        // self.stride += 1
        // でない
        self.stride += Group::WIDTH;
        self.pos += self.stride;
        self.pos &= self.bucket_mask;
        Some(result)
    }
}
```

これが上の方で説明した、バケットを走査するときに「空きバケットがみつかるまで続ける」などの処理をしているの部分です。
イテレータ側ではずっとインデックスを返し続けていて、それを使う側で空きバケットがあったら終了という処理をいれています。

外部に公開してないデータ構造だから許される攻め具合ですね。

## Criterionが使えなかった話

ベンチマークに素朴な繰り返しと素朴な `Instant::now` を使いました。
しかしベンチマークライブラリに[`criterion`](https://crates.io/crates/criterion)があります。
こちらの方がこなれてますし、誤差とかもうまく処理してくれます。
最初はこれを使ったのですが、正しくベンチマークがとれなかったのでやめました。

使えなかったのは外れ値処理の問題です。
集団から大きくはずれた値を計算に入れると平均などの統計値が大きく狂ってしまいます。
なので統計的処理をするにあたって外れ値を除外する処理は重要です。
実際、[criterionも外れ値処理をします](https://bheisler.github.io/criterion.rs/book/analysis.html#outlier-classification)。

しかし残念ながらこの外れ値処理が問題でcriterionがベンチマークに使えませんでした。
今回のキャッシュのパフォーマンス測定では、大多数のキャッシュヒットと少数のキャッシュミスからなります。
さらにキャッシュヒットはとても速く、キャッシュミスは極端に遅いです。
その結果をみてcriterionはキャッシュミスした場合の計測値を外れ値とみなして除外してしまってたのです。
criterionのAPIやCLIを見ても外れ値処理をスキップする機能はなさそうだったのでcriterionをあきらめました。

キャッシュのパフォーマンス測定で本質的に外れ値のようなものがでてしまって、統計的ベンチマークが役に立たないのは面白いなと思いました。

<svg
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   version="1.1"
   style="display:none"
   id="lru"
   inkscape:version="0.92.5 (2060ec1f9f, 2020-04-08)">
  <defs id="defs2">
    <marker
       inkscape:stockid="Arrow1Send"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="marker3501"
       style="overflow:visible;"
       inkscape:isstock="true">
      <path
         id="path3499"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.2) rotate(180) translate(6,0)" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible;"
       id="marker3185"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Send"
       inkscape:collect="always">
      <path
         transform="scale(0.2) rotate(180) translate(6,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path3183" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible;"
       id="marker1967"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Send">
      <path
         transform="scale(0.2) rotate(180) translate(6,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path1965" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Send"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Send"
       style="overflow:visible;"
       inkscape:isstock="true"
       inkscape:collect="always">
      <path
         id="path918"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.2) rotate(180) translate(6,0)" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible;"
       id="marker1337"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Mend">
      <path
         transform="scale(0.4) rotate(180) translate(10,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path1335" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible;"
       id="marker1269"
       refX="0.0"
       refY="0.0"
       orient="auto"
       inkscape:stockid="Arrow1Mend">
      <path
         transform="scale(0.4) rotate(180) translate(10,0)"
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         id="path1267" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Mend"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Mend"
       style="overflow:visible;"
       inkscape:isstock="true"
       inkscape:collect="always">
      <path
         id="path912"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.4) rotate(180) translate(10,0)" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Lend"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Lend"
       style="overflow:visible;"
       inkscape:isstock="true">
      <path
         id="path906"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.8) rotate(180) translate(12.5,0)" />
    </marker>
    <marker
       inkscape:stockid="Arrow1Lstart"
       orient="auto"
       refY="0.0"
       refX="0.0"
       id="Arrow1Lstart"
       style="overflow:visible"
       inkscape:isstock="true">
      <path
         id="path903"
         d="M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z "
         style="fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1"
         transform="scale(0.8) translate(12.5,0)" />
    </marker>
    <marker
       inkscape:isstock="true"
       style="overflow:visible"
       id="marker1967-2"
       refX="0"
       refY="0"
       orient="auto"
       inkscape:stockid="Arrow1Send">
      <path
         inkscape:connector-curvature="0"
         transform="matrix(-0.2,0,0,-0.2,-1.2,0)"
         style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:1.00000003pt;stroke-opacity:1"
         d="M 0,0 5,-5 -12.5,0 5,5 Z"
         id="path1965-7" />
    </marker>
    <symbol
       inkscape:label="base-box"
       inkscape:groupmode="layer"
       id="base-box"
       style="display:inline">
      <g id="g3621">
        <rect
           y="17.709580000000017"
           x="46.238255"
           height="22.753609"
           width="22.753609"
           id="rect10"
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.32291663;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
        <rect
           y="40.4632"
           x="46.238255"
           height="22.753609"
           width="22.753609"
           id="rect10-3"
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.32291663;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
        <rect
           y="63.216800000000006"
           x="46.238255"
           height="22.753609"
           width="22.753609"
           id="rect10-6"
           style="opacity:1;fill:none;fill-opacity:1;stroke:#1a1a1a;stroke-width:1.32291663;stroke-linecap:round;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1" />
      </g>
    </symbol>
    <symbol
       inkscape:groupmode="layer"
       id="arrow-top"
       inkscape:label="arrow-top"
       style="display:inline">
      <path
         style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.32291663;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#Arrow1Mend)"
         d="m 128.20608,25.01997 c 0,0 -10.23865,-10.579456 -25.22088,-10.782835 C 88.002972,14.033757 71.916025,26.664809 71.916025,26.664809"
         id="path96"
         inkscape:connector-curvature="0"
         sodipodi:nodetypes="czc" />
    </symbol>
    <symbol
       inkscape:label="new-1"
       id="new-1"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="133.08879"
         y="32.506424"
         id="text149"><tspan
           sodipodi:role="line"
           x="133.08879"
           y="32.506424"
           id="tspan147"
           style="stroke-width:0.26458332px"><tspan
             x="133.08879"
             y="32.506424"
             id="tspan145"
             style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="new-2"
       inkscape:label="new-2">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="133.18018"
         y="32.597801"
         id="text155"><tspan
           sodipodi:role="line"
           x="133.18018"
           y="32.597801"
           id="tspan153"
           style="stroke-width:0.26458332px"><tspan
             x="133.18018"
             y="32.597801"
             id="tspan151"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:label="item-1"
       id="item-1"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.56303"
         y="32.68935"
         id="text161"><tspan
           sodipodi:role="line"
           x="57.56303"
           y="32.68935"
           id="tspan159"
           style="stroke-width:0.26458332px"><tspan
             x="57.56303"
             y="32.68935"
             id="tspan157"
             style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:groupmode="layer"
       id="arrow-1-2"
       inkscape:label="arrow-1-2"
       style="display:inline">
      <path
         style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.32291663;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#Arrow1Send)"
         d="m 46.055496,28.401028 c 0,0 -11.587836,2.282937 -11.605254,11.879395 -0.01742,9.596458 9.869036,10.508692 9.869036,10.508692"
         id="path1327"
         inkscape:connector-curvature="0"
         sodipodi:nodetypes="czc" />
    </symbol>
    <symbol
       inkscape:label="new-3"
       id="new-3"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="133.63707"
         y="32.323662"
         id="text167"><tspan
           sodipodi:role="line"
           x="133.63707"
           y="32.323662"
           id="tspan165"
           style="stroke-width:0.26458332px"><tspan
             x="133.63707"
             y="32.323662"
             id="tspan163"
             style="stroke-width:0.26458332px">3</tspan></tspan></text>
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="item-2-1"
       inkscape:label="item-2-1">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.654408"
         y="32.780727"
         id="text173"><tspan
           sodipodi:role="line"
           x="57.654408"
           y="32.780727"
           id="tspan171"
           style="stroke-width:0.26458332px"><tspan
             x="57.654408"
             y="32.780727"
             id="tspan169"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.554268"
         y="56.455193"
         id="text179"><tspan
           sodipodi:role="line"
           x="57.554268"
           y="56.455193"
           id="tspan177"
           style="stroke-width:0.26458332px"><tspan
             x="57.554268"
             y="56.455193"
             id="tspan175"
             style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       style="display:inline"
       inkscape:label="arrow-2-3"
       id="arrow-2-3"
       inkscape:groupmode="layer">
      <path
         sodipodi:nodetypes="czc"
         inkscape:connector-curvature="0"
         id="path1961"
         d="m 46.055496,28.401028 c 0,0 -11.587836,2.282937 -11.605254,11.879395 -0.01742,9.596458 9.869036,10.508692 9.869036,10.508692"
         style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.32291663;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker1967)" />
      <path
         sodipodi:nodetypes="czc"
         inkscape:connector-curvature="0"
         id="path1961-0"
         d="m 45.964117,54.992593 c 0,0 -11.587837,2.282937 -11.605255,11.879394 -0.01742,9.596458 9.869037,10.508693 9.869037,10.508693"
         style="display:inline;fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.32291663;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker1967-2)" />
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="new-4"
       inkscape:label="new-4">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="132.6319"
         y="32.415043"
         id="text185"><tspan
           sodipodi:role="line"
           x="132.6319"
           y="32.415043"
           id="tspan183"
           style="stroke-width:0.26458332px"><tspan
             x="132.6319"
             y="32.415043"
             id="tspan181"
             style="stroke-width:0.26458332px">4</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:label="item-3-2-1"
       id="item-3-2-1"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.471649"
         y="33.054871"
         id="text191"><tspan
           sodipodi:role="line"
           x="57.471649"
           y="33.054871"
           id="tspan189"
           style="stroke-width:0.26458332px"><tspan
             x="57.471649"
             y="33.054871"
             id="tspan187"
             style="stroke-width:0.26458332px">3</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.37151"
         y="56.729336"
         id="text197"><tspan
           sodipodi:role="line"
           x="57.37151"
           y="56.729336"
           id="tspan195"
           style="stroke-width:0.26458332px"><tspan
             x="57.37151"
             y="56.729336"
             id="tspan193"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.462891"
         y="79.848465"
         id="text203"><tspan
           sodipodi:role="line"
           x="57.462891"
           y="79.848465"
           id="tspan201"
           style="stroke-width:0.26458332px"><tspan
             x="57.462891"
             y="79.848465"
             id="tspan199"
             style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:groupmode="layer"
       id="arrow-3-out"
       inkscape:label="arrow-3-out"
       style="display:inline">
      <path
         style="fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.05833328;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker3185)"
         d="M 57.112471,85.69626 V 99.129113"
         id="path3175"
         inkscape:connector-curvature="0" />
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="item--3-2-1out"
       inkscape:label="item--3-2-1out">
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="57.554268"
       y="56.089676"
       id="text209"><tspan
         sodipodi:role="line"
         x="57.554268"
         y="56.089676"
         id="tspan207"
         style="stroke-width:0.26458332px"><tspan
           x="57.554268"
           y="56.089676"
           id="tspan205"
           style="stroke-width:0.26458332px">3</tspan></tspan></text>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="57.645649"
       y="79.208801"
       id="text215"><tspan
         sodipodi:role="line"
         x="57.645649"
         y="79.208801"
         id="tspan213"
         style="stroke-width:0.26458332px"><tspan
           x="57.645649"
           y="79.208801"
           id="tspan211"
           style="stroke-width:0.26458332px">2</tspan></tspan></text>
    <text
       xml:space="preserve"
       style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
       x="57.581059"
       y="111.54848"
       id="text221"><tspan
         sodipodi:role="line"
         x="57.581059"
         y="111.54848"
         id="tspan219"
         style="stroke-width:0.26458332px"><tspan
           x="57.581059"
           y="111.54848"
           id="tspan217"
           style="stroke-width:0.26458332px">1</tspan></tspan></text>
    </symbol>
    <symbol
       style="display:inline"
       inkscape:groupmode="layer"
       id="item-4-3-2"
       inkscape:label="item-4-3-2">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.106129"
         y="32.597969"
         id="text227"><tspan
           sodipodi:role="line"
           x="57.106129"
           y="32.597969"
           id="tspan225"
           style="stroke-width:0.26458332px"><tspan
             x="57.106129"
             y="32.597969"
             id="tspan223"
             style="stroke-width:0.26458332px">4</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.005989"
         y="56.272434"
         id="text233"><tspan
           sodipodi:role="line"
           x="57.005989"
           y="56.272434"
           id="tspan231"
           style="stroke-width:0.26458332px"><tspan
             x="57.005989"
             y="56.272434"
             id="tspan229"
             style="stroke-width:0.26458332px">3</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.09737"
         y="79.391563"
         id="text239"><tspan
           sodipodi:role="line"
           x="57.09737"
           y="79.391563"
           id="tspan237"
           style="stroke-width:0.26458332px"><tspan
             x="57.09737"
             y="79.391563"
             id="tspan235"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:label="item-2-4-3"
       id="item-2-4-3"
       inkscape:groupmode="layer"
       style="display:inline">
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.014748"
         y="32.597969"
         id="text245"><tspan
           sodipodi:role="line"
           x="57.014748"
           y="32.597969"
           id="tspan243"
           style="stroke-width:0.26458332px"><tspan
             x="57.014748"
             y="32.597969"
             id="tspan241"
             style="stroke-width:0.26458332px">2</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="56.914608"
         y="56.272434"
         id="text251"><tspan
           sodipodi:role="line"
           x="56.914608"
           y="56.272434"
           id="tspan249"
           style="stroke-width:0.26458332px"><tspan
             x="56.914608"
             y="56.272434"
             id="tspan247"
             style="stroke-width:0.26458332px">4</tspan></tspan></text>
      <text
         xml:space="preserve"
         style="font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:12.69999981px;line-height:134.00000334%;font-family:'源ノ角ゴシック JP';-inkscape-font-specification:'源ノ角ゴシック JP';text-align:center;letter-spacing:0px;word-spacing:0px;text-anchor:middle;display:inline;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
         x="57.005989"
         y="79.391563"
         id="text257"><tspan
           sodipodi:role="line"
           x="57.005989"
           y="79.391563"
           id="tspan255"
           style="stroke-width:0.26458332px"><tspan
             x="57.005989"
             y="79.391563"
             id="tspan253"
             style="stroke-width:0.26458332px">3</tspan></tspan></text>
    </symbol>
    <symbol
       inkscape:groupmode="layer"
       id="arrow-3-1"
       inkscape:label="arrow-3-1"
       style="display:inline">
      <path
         style="display:inline;fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:1.05833328;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker-end:url(#marker3501)"
         d="m 69.814287,75.735843 c 0,0 16.357008,-8.589715 16.265628,-24.398447 C 85.988536,35.528663 69.631526,28.401028 69.631526,28.401028"
         id="path3475"
         inkscape:connector-curvature="0"
         sodipodi:nodetypes="czc" />
    </symbol>
  </defs>
</svg>
