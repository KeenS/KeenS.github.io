---
categories: [ML, ATS2, 依存型, 型]
date: 2015-12-29T23:40:45+09:00
title: ATS2の依存型を使ってAVL木
---

κeenです。少しばかりATS2を触ってみたので成果報告でも。

AVL木は左右のノードの高さが高々1しか違わない平衡二分木です。OCamlやSMLでナイーブに実装すると本当に1しか違わないことを保証するのは難しく、精々テストなどで部分的に検査するだけです。

ところがSMLに似た文法を持つATS2には依存型があり、左右のノードの高さが高々1しか違わないことを型で保証出来ます。
つまり、左右のノードの高さが2以上違う木を作ろうとしてもコンパイルエラーになるのでコンパイルが通れば高さについてはバグがないこと保証されます。

そういうAVL木を使ってTreeSetを作ってみたので紹介します。
<!--more-->
私のブログ(のこの記事)の読者ならATS2も依存型もAVL木も知ってそうですが一応説明します。

# ATS2って何?
詳しい説明は[日本ATSユーザグループ](http://jats-ug.metasepi.org/#document)に譲るとして、この記事にて重要な点を挙げます。

* SMLに似た文法を持つML方言
* 型の部分に型以外の項（例えば整数とか）が使える[依存型](https://ja.wikipedia.org/wiki/%E4%BE%9D%E5%AD%98%E5%9E%8B)を持つ

他にももっと素晴しい型システムや機能、特徴があるのですがとりあえずのところは上記の特徴を押えておいて下さい。


# 依存型って何?
先程も述べた通り、ざっくり言うと型の部分に型以外の項が使える型システムです。

例えば、次のSMLのコードを考えてみましょう。

``` sml
Array.get(arr: int array, index: int): int
```

型から読み取れるのは「`int`の配列`arr`と整数`index`を受け取って`int`の値を返す」関数です。
しかしこの関数は`index`が`arr`の長さよりも大きい時に実行時例外を上げます。内部でメモリを護るために境界チェックをして、チェックに落ちた時に例外を挙げている筈です。。
配列アクセスの度に実行時例外をハンドルするのはコード上もパフォーマンス上も避けたいですよね。
こういうものをコンパイル時に防ぎたくありませんか？無駄な境界チェックを省きたくありまんか？依存型を使えばそれが出来ます。

例えば

``` ats2
array_get{n, i: nat| i < n}(arr: arrayref(int, n), index: int(i)): int
```

これは「長さ`n`の`int`配列`arr`と`n`以下の自然数`index`を受け取って`int`の値を返す」関数です。
正しい入力しか受け取らないので実行時例外が起きませんし内部で境界チェックも必要ありません。面白くないですか？

私も大して型システムに詳しくないのでこのくらいの例しか挙げることが出来ませんがきっとさらに面白い世界が広がっているのでしょう。

# AVL木って何？
[Wikipedia](https://ja.wikipedia.org/wiki/AVL%E6%9C%A8)を参照すると詳しいことが載っています。前の説明を繰り返すと、左右のノードの高さが高々1しか違わない平衡二分木です。自身の高さは左右の高さの高い方+1です。
データ構造は左右の木、ノードの保持する値の他にそのノードの高さを保持します。


なので平衡度は赤黒木より高く、検索が高速な一方、要素数を変更する操作の度に平衡を取らないといけないので挿入や削除は赤黒木に負けるそうです。
OCamlのstdlibのMap/Setの実装に使われています。(OCamlは左右の高さが高々2違う木なので厳密にはAVL木ではないのですがアルゴリズムは同じです。)

因みにAVLは人の名前の頭文字みたいです。

# 作ってみよう
今回、ATS2の依存型に関係ない機能、関数テンプレートなどは理解（読解）の妨げになると判断してそれらを使わないコード例を出します。
(関数テンプーレートを使わなくても多相型を使えば良さそうですが、ATS wayではなさそうです。)

ということで、整数を格納するSetを作ってみましょう。

実装に先立って、[OCamlのSet](https://github.com/ocaml/ocaml/blob/trunk/stdlib/set.ml)や[GADTを使ったAVLの実装](https://tech.bezoomnyville.com/2015/12/07/A-GADT-implementation-of-AVL-tree.html)を読んでおくと比較が出来るかと思います。

今回OCamlのSetを参考にして作ったので比較のためにコードを載せたいのですがOCamlは確かコードの再配布に厳しいライセンスだった気がするのでやめておきます。(スニペットくらいなら著作権は発生せず、ライセンス関係なしに使えるのですが念のため。)

## おまじない
標準的な操作を取り込むため、以下をファイルの先頭に書きます。

``` ats2
#include "share/atspre_staload.hats"
```


## データ型
高さnのAVL木を表わすデータ型を考えます。このデータ型は、"型パラメータとして"木の高さnを取ります。

書き出しはこうです。

``` ats2
datatype avlt(n: int) =
 ...
```

続けて、Emptyを書きます。Emptyは高さ0です。


``` ats2
datatype avlt(n: int) =
    Empty(0)
    ...
```

続いて、Nodeですが、左の木の高さと右の木の高さを表わす変数を導入します。


``` ats2
  | {l, m: nat} Node of ...
```

このクルクル括弧`{}`で囲まれた部分は「任意の`nat`なる`l`,`m`に対して」と読めば読み易いです。

データ構造は、左右の木、値、高さを持つのでした。ここで、高さは`n`であると型で制約されてます。ということでこうです。


``` ats2
  | {l, m: nat} Node of (avlt(l), int, avlt(m), int(n))
```

最後の高さの部分で`int(n)`となっているのは、`n`は種`int`であって型ではないので「整数`n`の`int`"型"」にする為に`int()`を適用する必要があるのです。依存型だと「ただの整数値」だけでなく「整数3」などの型も作れるのです。


さて、このままだと左右の木の高さが高々1しか違わないということを表わせていません。さらにいうと、自身の高さについての制約も書けていません。これらの制約はこう書きます。

``` ats2
  | {l, m: nat | <制約> }
```

今回の制約はこうなりますね。


``` ats2
  | {l, m: nat |
    (l + 2 == n && m + 1 == n) || // 右が左より1高い 又は
    (l + 1 == n && m + 1 == n) || // 左右同じ高さ 又は
    (l + 1 == n && m + 2 == n)    // 左が右より1高い
    }
```

結果、データ型はこうです。


``` ats2
datatype avlt(n: int) =
    Empty(0)
  | {l, m: nat |
    (l + 2 == n && m + 1 == n) ||
    (l + 1 == n && m + 1 == n) ||
    (l + 1 == n && m + 2 == n)
} Node of (avlt(l), int, avlt(m), int(n))

```

中々複雑ですね。


## `height`

軽い関数から作っていきましょう。まずは木の高さを返す関数`height`です。高さ`n`の木を受け取って整数`n`を返します。

書き出しはこうです。

``` ats2
fun height...
```

そして`n`を導入します。

``` ats2
fun height{n: nat}...
```

引数は「高さ`n`の木」ですね。


``` ats2
fun height{n: nat}(tree: avlt(n))...
```

返り値は「整数`n`」です。


``` ats2
fun height{n: nat}(tree: avlt(n)): int(n) = ...
```

関数本体は普通です。

``` ats2
fun height{n: nat}(tree: avlt(n)): int(n) = 
  case+ tree of
  | Empty () => 0
  | Node (_, _, _, n) => n
```

ここで使っている`case+`ですが、普通の`case`より強くて網羅性検査に落ちるとコンパイルエラーになります。

## `create`
これから`insert`を実装していく訳ですが、ユーティリティから少しづつ実装していきます。まずは高さが高々1しか違わない木2つと値を1つ受け取って新たな木を作る`create`です。`Node`を生で使うのに比べて高さの計算が必要なくなります。

これは先に実装を与えて、後で型を考えましょう。先程の`height`を使います。


``` ats2
fun create??(l: ??, v: int, r: ??): ?? = let
  val hl = height l
  val hr = height r
in
  if hl >= hr
  then Node(l, v, r, hl + 1)
  else Node(l, v, r, hr + 1)
end
```

さて、型を考えていきます。まずは`l`と`r`はある高さを持つ木です。


``` ats2
fun create{l, m: nat} (l: avlt(l), v: int, r: avlt(m)): ?? = ...
```


次に、左右で高さが高々1しか違わないことを制約したいです。これは`Node`の定義を参考にすると書けるでしょうか。

``` ats2
fun create{
  l, m: nat |
  l + 1 == m ||
  l == m + 1 ||
  l == m
  } (l: avlt(l), v: int, r: avlt(m)): ?? = ...
```

最後に返り値の型を考えます。返り値は「`l`か`m`どちらか大きい方+1の高さを持つ木」です。私は最初、それをこう表現していました。


``` ats2
fun create...(...): [n: nat | n == m + 1 || n == l + 1] avlt(n) = ...
```

`[n: nat | n == m + 1 || n == l + 1] avlt(n)`の部分を読み下すと、「`n = m + 1`又は`n = l + 1`を満たす`n`に対して`avlt(n)`」という型になります。これは、間違ってはいないのですが正確ではありませんでした。なぜならこの型に忠実に従うと`l = m + 1`の時に`n = m + 1`、すなわち`n = l`もありえてしまいます。勿論そんなことはなく、`l = m + 1`ならば`n = l + 1`です。

よって正しい型は`[n: nat | (m > l && n == m + 1) || (l >= m && n == l + 1)] avlt(x, n)`です。やたら長いですね。


全体を載せると、


``` ats2
fun create{
  l, m: nat |
  l + 1 == m ||
  l == m + 1 ||
  l == m
  } (l: avlt(l), v: int, r: avlt(m)): [n: nat | (m > l && n == m + 1) || (l >= m && n == l + 1)] avlt(n) = let
  val hl = height l
  val hr = height r
in
  if hl >= hr
  then Node(l, v, r, hl + 1)
  else Node(l, v, r, hr + 1)
end
```

になります。

## `rotate`

OCamlのSetでは1まとめに`bal`としていますが、条件分岐が多すぎて読みづらいので`right`と`left`に分離することにします。

`rotate_xxx`は、左右で高さがちょうど2違う木と値を受け取って、バランスのとれた木を作って返す関数です。
`rotate_right`なら左の木が右の木より2高い木を受け取って上手い具合に木の付け替えをして新たな木を返します。

返り値の木は左の木と同じ高さかあるいはそれより1高い木です。

そろそろ慣れてきたと思うので型までを一気に書きます。


``` ats2
fun rotate_right{hl, hr: nat |
  hl == hr + 2
}(l: avlt(hl), v: int, r: avlt(hr)): [n: nat| n == hl || n == hl + 1] avlt(n) = ...
```

さて、さらに左側の木の左右の高さでも場合分けが必要になるのでまずは色々バラします。


``` ats2
fun rotate_right{hl, hr: nat |
  hl == hr + 2
}(l: avlt(hl), v: int, r: avlt(hr)): [n: nat| n == hl || n == hl + 1] avlt(n) = let
  val+ Node(ll, lv, lr, _) = l
  val hll = height ll
  val hlr = height lr
in
  ...
end
```

さて、ここが一番面白い所です。`val+ Node(ll, lv, lr, _) = l`に注目して下さい。 OCamlでは`match l with Empty -> invalid_arg "Set.bal" | Node (ll, lv, lr, _) -> ...`となっていた部分です。
型制約から、`l`は高さ2以上の木であることが分かっているので`Empty`でないことが保証されます。なので **`Node`だけで網羅出来ているとコンパイラが理解してくれ** ます。マッチが1節しかないので`val`のパターンマッチで代用出来ますね。
`val+`は`case+`と同じく網羅性検査に落ちるとコンパイルエラーになるバージョンの`val`です。

残りの実装をまとめると、こうです。


``` ats2
fun rotate_right{hl, hr: nat |
  hl == hr + 2
}(l: avlt(hl), v: int, r: avlt(hr)): [n: nat| n == hl || n == hl + 1] avlt(n) = let
  val+ Node(ll, lv, lr, _) = l
  val hll = height ll
  val hlr = height lr
in
  if  hll >= hlr
  then create(ll, lv, create(lr, v, r))
  else let
       val+ Node(lrl, lrv, lrr, _) =  lr
  in
    create(create(ll, lv, lrl), lrv, create(lrr, v, r))
  end
end
```

`rotate_left`はこうです。


``` ats2
fun rotate_left{hl, hr: nat |
  hl + 2 == hr
}(l: avlt(hl), v: int, r: avlt(hr)): [n: nat| n == hr || n == hr + 1] avlt(n) = let
  val+ Node(rl, rv, rr, _) = r
  val hrl = height rl
  val hrr = height rr
in
  if  hrr >= hrl
  then create(create(l, v, rl), rv, rr)
  else let
    val+ Node(rll, rlv, rlr, _) =  rl
  in
    create(create(l, v, rll), rlv, create(rlr, rv, rr))
  end
end
```

## `bal`
`rotate_right`, `rotate_left`, `create`を1まとめにして左右の高さの差が高々2の2つの木と値を受け取ってバランスのとれた木を作って返す`bal`を作りましょう。

型がエグいことになります。


``` ats2
fun bal{hl, hr: nat |
  ~2 <= hl - hr  && hl - hr <= 2
}(l: avlt(hl), v: int, r: avlt(hr)): [n: nat|
 (hl == hr - 2 && n == hr    ) ||
 (hl == hr - 2 && n == hr + 1) ||
 (hl == hr - 1 && n == hr + 1) ||
 (hl == hr     && n == hl + 1) ||
 (hl == hr + 1 && n == hl + 1) ||
 (hl == hr + 2 && n == hl + 1) ||
 (hl == hr + 2 && n == hl    )
] avlt(n) = let
  val hl = height l
  val hr = height r
in
  if hl = hr + 2
  then rotate_right(l, v, r)
  else if hl = hr - 2
  then rotate_left(l, v, r)
  else create(l, v, r)
end
```

## `cmp`

さて、本来のOCamlのSetはファンクタで出来てますがATS2には原始的なファンクタしかないのでファンクタを使わずにこれまで実装してきました。
あまりよろしくありませんが`cmp`もベタっと実装してしまいましょう。

``` ats2
fun cmp(x: int, y: int): int = x - y
```

## `empty`, `singleton`

``` ats2
fun empty(): avlt(0) = Empty()
fun singleton(x: int): avlt(1) = Node(Empty, x, Empty, 1)
```
## `insert`

ここまで辿りついたらもう難しいところはありません。`insert`した結果、木の高さが変わらないか1増えるだけです。


``` ats2
fun insert{m: nat}(x: int, tree: avlt(m)): [n: nat | n == m || n == m + 1]avlt(n) =
  case+ tree of
  | Empty () => singleton(x)
  | t as Node(l, v, r, _) => let
    val c = cmp(x, v)
  in
    if c = 0 then t
    else if c < 0
    then bal(insert(x, l), v, r)
    else bal(l, v, insert(x, r))
  end
```

## `mem`

``` ats2
fun mem{m: nat}(x: int, tree: avlt(m)): bool =
  case+ tree of
  | Empty () => false
  | Node(l, v, r, _) => let
    val c = cmp(x, v)
  in
    if c = 0
    then true
    else if c < 0
    then mem(x, r)
    else mem(x, l)
  end
```

# 遊ぶ

さて、少しばかり遊んでみましょう

``` ats2

implement
main0 () =  {
  val tree = Empty
  val tree = insert(1, tree)
  val tree = insert(2, tree)
  val tree = insert(4, tree)
  val b = mem(2, tree)
  val c = mem(3, tree)
  val () = fprintln!(stdout_ref, b)
  val () = fprintln!(stdout_ref, c)
}
```

これまでのファイルを`avlset_int.dat`に保存して、以下のようにコンパイル/実行します。


```
$ patscc avlset.dats -DATS_MEMALLOC_LIBC
$ ./a.out
true
false
```

ちゃんと動いているようです。(GCをリンクしてないのでメモリリークしてる気がしますが、GCのリンクの仕方が分からなかったのでこのままにします。)

追記: GCのリンクの仕方を教えてもらいました。

<blockquote class="twitter-tweet" data-conversation="none" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> GC付きコンパイルには -DATS_MEMALLOC_GCBDW -lgc を使うようです。僕も今知りました。。。 / master · githwxi/ATS-Postiats-contrib <a href="https://t.co/4DPotHeUJ1">https://t.co/4DPotHeUJ1</a></p>&mdash; Myu-Myu- ATS-tan! (@masterq_mogumog) <a href="https://twitter.com/masterq_mogumog/status/682142334704717824">2015, 12月 30</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

# まとめ

依存型を持つATS2を使うことでAVL木の高さに関する要件を必ず満たす木を作ることが出来ました。
これは変な入力がないことを保証するばかりでなく、自らの実装がバグっていないことの確認にも使えます。
また、`val+`の例で見た通り、パターンマッチについてもより厳格な検査が出来るので無意味なコードを書かなくて済みました。

今回のコードは[こちら](https://github.com/KeenS/avlset)に置いておきます。また、関数テンプレートを使ったよりジェネリックな実装もあります。


# ノート

* 今回型レベルで保証したのは高さに対する要件だけでした。
  「ノードの左側の木にはそのノードの保持する値より小さい値しか入っていない」「一度`insert`した値は`mem`すると`true`が返ってくる」などの要件については無保証のままです。
  実はATS2には[定理証明器](http://jats-ug.metasepi.org/doc/ATS2/INT2PROGINATS/c2849.html)も付いており、そのような振舞に関しても保証出来ます。
* ATS2には依存型の他にも[線形型](http://jats-ug.metasepi.org/doc/ATS2/INT2PROGINATS/p3301.html)を持っていて、GCに頼らないメモリ管理が可能です。
  今回のコードも線形型を使ってメモリリークしないように出来るでしょう。
* 関数テンプレートを使ったAVL木の実装には若干危い点があります。関数テンプレートは実装をアドホックに変更出来るので
  比較関数を差し替えれてしまいます。`cmp(x, y) = x - y`で木を作った後に`cmp(x, y) = y - x`にしてさらに要素を挿入したら大惨事になるでしょう。
  そこまで気にするようなら[ファンクタ](http://jats-ug.metasepi.org/doc/ATS2/INT2PROGINATS/x1974.html)を使うべきです。


# その他

* CoqやAgdaのように依存型も定理証明器もある言語がありますが、それらと違ってATS2は「汎用プログラミング言語に表現力の高い依存型を付けて、ついでに定理証明器もついてる」くらいです。
  本格的な証明には向かない一方汎用プログラミングにはATS2の方が向いているでしょう。
* しかしCoqと似ている面もあり、ATS2の世界で依存型や線形型、証明などで強い保証を付けた後は一旦Cのコードをエクストラクトして、それをコンパイルしています。
* Cにエクストラクトされるということで、ATS2はCとの相互連携が出来ます。

さて、ATS2には心躍る特徴がたくさんあります。依存型で実行時検査をコンパイル時検査に持ち上げ、定理証明器でバグも潰せて、
線形型でボックス/アンボックデータの扱いやGCに頼らないリソース管理やデータ競合の回避が出来、
並列プログラミングもサポート、低レベルなCのコードを呼べてバイナリサイズも小さいなどなど。

しかし、これらの機能を得るのにATS2は1つだけ代償を払いました。学習コストです。
私はCも書きますしSMLも書きます。依存型のあるCoqや線形型より少し弱いアフィン型のあるRustも書いたことがあります。そんな私にもATS2は難解です。
最近難解な言語として名高くなってきたRustですら足元に及ばないでしょう。

それでも、急峻な学習曲線の崖をよじ登った先には素晴しい世界が広がっているのです。1つ、修行だと思ってトライしてみてはいかがでしょうか。


参考: [プログラマの区分](http://fumieval.tumblr.com/post/28324791101/%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9E%E3%81%AE%E5%8C%BA%E5%88%86)

