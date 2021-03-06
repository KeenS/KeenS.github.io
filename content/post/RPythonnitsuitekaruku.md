---
categories: [言語実装, RPython, Interpreter, Advent Calendar, Advent Calendar 2017, 言語実装 Advent Calendar]
date: 2017-12-12T20:25:00+09:00
title: "RPythonについて軽く"
---
κeenです。これは [言語実装 Advent Calendar 2017](https://qiita.com/advent-calendar/2017/lang_dev)13日目の記事です。
RPythonやPyPyについて勘違いしてる人向けに誤解を解こうかと。あんまコードは出てこないやつです。

<!--more-->


いきなり話が逸れますが、PyPyとRPythonの話前に二村射影を知っておくと理解が深まるかもしれないので触れて起きます。

# 二村射影と部分評価

詳しくは[Wikipedia](https://ja.wikipedia.org/wiki/%E9%83%A8%E5%88%86%E8%A9%95%E4%BE%A1)を見て下さい。
ここでは直感的な話をします。

## 第一: インタプリタとExecutable

インタプリタは抽象的にはソースコードと入力データを受け取って出力データを出しています。

```
+---------+ +--------+
| in/data | | source |
+---------+ +--------+
      |       |
   +-------------+
   | interpreter |
   +-------------+
         |
    +----------+
    | out/data |
    +----------+
```

このインタプリタをソースコードについて部分評価するとどうなるでしょう。
この部分評価する関数が第一二村射影なので`F1`と呼びますね。


```
+---------+ +--------+         +---------+
| in/data | | source |-----+   | in/data |
+---------+ +--------+     |   +---------+
      |       |            |        |
   +-------------+       +----+  +-----+
   | interpreter |-------| F1 |--| ??? |
   +-------------+       +----+  +-----+
         |                          |
    +----------+              +----------+
    | out/data |              | out/data |
    +----------+              +----------+
```

この第一二村射影の結果出来上がったものは何でしょうか。


```
  +---------+
  | in/data |
  +---------+
       |
    +-----+
    | ??? |
    +-----+
       |
 +----------+
 | out/data |
 +----------+

```

これはただのプログラム、実行可能ファイルですね。

## 第二: コンパイラとインタプリタ

さて、もう一度第一二村射影を眺めましょう。

```
+---------+ +--------+         +---------+
| in/data | | source |-----+   | in/data |
+---------+ +--------+     |   +---------+
      |       |            |        |
   +-------------+       +----+  +-----+
   | interpreter |-------| F1 |--| exe |
   +-------------+       +----+  +-----+
         |                          |
    +----------+              +----------+
    | out/data |              | out/data |
    +----------+              +----------+
```

この`F1`に注目して下さい。

```
+--------+ +-------------+
| source | | interpreter |
+--------+ +-------------+
      |       |
   +-------------+
   |      F1     |
   +-------------+
          |
       +-----+
       | exe |
       +-----+
```

見覚えのある構図ですね。先程と同じく`F1`を`interpreter`で部分評価してみましょう。この部分評価が`F2`です。

```
+--------+ +-------------+          +--------+
| source | | interpreter |---+      | source |
+--------+ +-------------+   |      +--------+
      |       |              |          |
   +-------------+         +----+  +----------+
   |      F1     |---------| F2 |--| compiler |
   +-------------+         +----+  +----------+
          |                             |
       +-----+                       +-----+
       | exe |                       | exe |
       +-----+                       +-----+
```

答え書いちゃってますが`F2`の結果生まれるのはコンパイラです。

## 第三: コンパイラジェネレータ

もう馴れたと思うので一気に`F3`まで書きますね。結論も書くと`F3`の結果コンパイラジェネレータが出来ます。


```
+-------------+ +----+            +-------------+
| interpreter | | F1 |---+        | interpreter |
+-------------+ +----+   |        +-------------+
         |       |       |              |
     +-------------+    +----+  +--------------------+
     |      F2     |----| F3 |--| compiler generator |
     +-------------+    +----+  +--------------------+
            |                           |
       +----------+                +----------+
       | compiler |                | compiler |
       +----------+                +----------+
```

`F1`、`F2`、`F3`さえ実装できればインタプリタからコンパイラが生成できるという魔法のような話でした。
とはいっても`F1`の時点でもかなり無理があるので実現可能性は低いでしょう。

ようやくPyPyの話に移ります。

# PyPyって？

[公式](http://pypy.org/)。
そこそこ有名なのであまり説明は必要ないかもしれませんが、Python処理系の1つです。
JITをすることで[かなり高速に動作](http://speed.pypy.org/)します。


# RPythonって？
[ドキュメント](https://rpython.readthedocs.io/en/latest/)。ひとまず、PyPyのサブプロジェクトです。
「RPython」が指す範囲が広いので慎重にいきます。

言語仕様はPythonのサブセットです。PyPyのインタプリタを実装する目的で策定されました。
面白いのは処理系実装の方で、RPythonのプログラムをC(など)に変換するコンパイラになってます。

この説明だとPythonをコンパイルする夢の処理系かと早合点する人が多いですが、Cに変換できる程に制限したサブセットなので実際はPython風に書ける小さな言語処理系記述DSL程度です。

RubyのJIT処理系を目指して作られた[Topaz](https://github.com/topazproject/topaz)もこのRPythonで書かれていたので色々勘違いされて「Rubyはあまりに遅くて、RubyインタプリタをPythonで書き直したら5倍速くなった」なんて冗談じみた話も出回ってましたが完全に誤解です。

## もうちょっとRPython
「RPython」からC言語に至るまでのツーリングが素敵にややこしいので少し触れます。

まず、RPython処理系はPythonで実装されています。

```
+---------------+
| `rpython.py`  |
+---------------+
|   `python`    |
+---------------+
```

そしてRPytnonのプログラムはPythonのソースコードからやってきます。
どういうことかというと、「RPythonとして有効な関数を返すPythonの関数」の実行結果がRPythonのプログラムとして処理されます。
普通のPythonの普通の関数オブジェクト(RPythonのドキュメントでは _コードオブジェクト_ )がRPythonの仕様の範囲内ならRPythonのプログラムになるわけです。
「ソースコードじゃなくてコードオブジェクト？Pythonのメモリにあるただのデータでいいの？」と思うかもしれませんがRPythonの処理系はコンパイルされたPythonのバイトコードを解析(正確には[抽象解釈](https://ja.wikipedia.org/wiki/%E6%8A%BD%E8%B1%A1%E8%A7%A3%E9%87%88))して、同じセマンティクスを持つCのソースコードを吐いているので大丈夫です。むしろソースコードは扱えません。

ところでなぜ一旦Pythonのインタプリタを噛ませてるかというと、1つにはPythonをメタ言語として使えるからです。

[ドキュメント](https://rpython.readthedocs.io/en/latest/getting-started.html)の例を引用すると、

``` python
def generator(operation):
    if operation == 'add':
       def f(a, b):
           return a + b
    else:
       def f(a, b):
           return a - b
    return f

add = generator('add')
sub = generator('sub')

def entry_point(argv):
    print add(sub(int(argv[1]), 3) 4)
    return 0
```

上記の`entry_point`とそこから辿れる`add`、`sub`はRPythonですが`generator`はそうではありません。しかし

``` python
def target(*args):
    return entry_point, None
```

のようにRPythonのエントリーポイントを`entry_point`にすれば、別に`generator`はRPythonでなくともよいのです。
つまりは最終的にコンパイルされる部分以外ではPythonでメタプログラミングが出来るのです。

最後に、もちろんRPythonに吐かれたCのコードはCコンパイラを通して実行可能ファイルになります。

これら関係を図に加えると、

```
+-[source.py]--+
| Python       |
|    +---------+    +---+       +-----+
|    | RPython | => | C |   =>  |     |
|  +-+---------+    +---+       |     |
|  |+-----------------++-------+| exe |
+--+|   `rpython.py`  ||       ||     |
+---------------------+| `gcc` ||     |
|         `python`    ||       ||     |
+---------------------++-------++-----+
```

こうなります。


さて、PyPyの処理系はRPythonで書かれていたわけですからそれも書いてみましょう。

```
                                +--------+
+-[pypy.py]----+                | Python |
| Python       |                +--------+
|    +---------+    +---+       +--------+
|    | RPython | => | C |   =>  |        |
|  +-+---------+    +---+       |        |
|  |+-----------------++-------+| `pypy` |
+--+|   `rpython.py`  ||       ||        |
+---------------------+| `gcc` ||        |
|         `python`    ||       ||        |
+---------------------++-------++--------+
```

PythonとRPythonが入り乱れて楽しいですね。
因みに簡単な方のアーキテクチャで、後述のようにJITも入れるともっと複雑です。

さて、少し脇道に逸れるとよくある勘違いの1つに「RPythonをPyPyで動かしたらもっとPyPyが速くなるんじゃないか」というのがあります。
しかし図をみたら分かるようにRPythonを速く動かしたところで速くなるのは`pypy`処理系の生成で、`pypy`そのものが速くなる訳ではないことが見て取れると思います。

閑話休題。ここまでだと結局はCで書かれたPythonインタプリタが出てくるのでCPythonとそう変わらなそうですね。
こっちは多少言語処理系に特化した分少し速いくらいでしょうか。
しかしJITが入ってくるとRPythonで抽象的にインタプリタの意味論を与えたことが効いてきて、「効率的に」効率的な実行ができるようになります。

# RPythonとJIT
さて、シンプルな話が終わったのでJITの話をします。

インタプリタが遅かったらJITをしますね？でもJITは実装が大変ですし、「普通のインタプリタ」と「JITで生成するコード用のインタプリタと同じ挙動をするアセンブリ」で実質2回同じインタプリタを実装しないといけません。
面倒なだけでなく、両者で挙動に違いがあればそれはバグです。大変ですね。

これをうまいことするのがRPythonのJIT Optimization。
なんとRPythonで書かれたインタプリタからJITエンジンを生成してしまうのです。
実行時情報を取りながらという条件付きですが第一二村射影を実現してしまった感じですね。

私の理解が足りてないので間違ってるかもしれませんがこんな感じでしょうか。

```
                     [JIT Lib]-+
                               |  +--------+
+-[pypy.py]----+    +--------+ |  | Python |
| Python       | => |  JIT   | |  +--------+
|    +---------+    +--------+ V  +--+-----+
|    | RPython | => |    C   | => |  | JIT |
|  +-+---------+    +--------+    |  +-----+
|  |+-----------------++-------+  | `pypy` |
+--+|   `rpython.py`  ||       |  |        |
+---------------------+| `gcc` |  |        |
|         `python`    ||       |  |        |
+---------------------++-------+  +--------+
```

# 終わりに
締まらないですが終わりです。
本当はRPythonでBFのJITインタプリタを作るチュートリアル[1](https://morepypy.blogspot.jp/2011/04/tutorial-writing-interpreter-with-pypy.html)と[2](https://morepypy.blogspot.jp/2011/04/tutorial-part-2-adding-jit.html)
をやりたかったのですが2の方がAPIが変わったらしく動かないので断念しました。2011年のものですしまだRPythonがPyPyから切り出されてない頃のもののようなので当然っちゃ当然ですね。

明日は続けて似たようなフレームワーク(と思ってる)JVMのGraalとTruffleについて調べようと思います。
