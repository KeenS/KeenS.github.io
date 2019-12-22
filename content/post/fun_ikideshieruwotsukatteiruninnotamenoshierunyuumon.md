---
categories: [Shell, Shell Script, CLI]
date: 2017-10-17T22:36:14+09:00
title: "雰囲気でシェルを使っている人のためのシェル入門"
---
κeenです。雰囲気でシェルを使ってる人が多いとのことだったので少しばかり込み入った知識を。
あと一応POSIX準拠かどうかも気にしながらやっていきます。

<!--more-->
# 基礎知識編
## シェルの種類
まず、POSIXにシェルが[定義されています](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html)。

これに最低限の機能で準拠しているものをPOSIXシェルと呼ぶことにします。いわゆる`/bin/sh`です。具体的な実装はbsh、ash、dashあたりでしょうか。
最低限の機能以上に色々拡張されているシェルを拡張POSIXシェルと呼ぶことにします。具体的な実装はbash、zsh、kshなどでしょうか。
ここでは触れませんがPOSIX準拠でないシェルも存在してcshやtcshなどのシェルがあります。あと確か最近話題のfishも違ったような。

さて、1つ問題になるのは普段使いのコマンドラインはおおむね拡張POSIXシェルでしょうが、サーバで使うシェルやデプロイスクリプトで呼び出すシェルなどは拡張でないPOSIXシェルだったりすることです。なので普段のコマンドラインで使える機能とシェルスクリプトで使える機能を分けて覚えなければなりません。ということでここではPOSIX準拠かどうかを気にしながらやっていきます。

面倒ならデプロイスクリプトを `.sh` じゃなくて `.bash` にしてshebangもbashにしてBashスクリプトにすることで罠を避ける方法もあります。そのときはちゃんとサーバにBashが入っているか確認しておきましょう。ついでにBashのバージョンも。4.x系からの機能もちょいちょいあるので3.x系だと動かないとかたまにあります。

## 変数
シェルにも変数があります。代入するときは名前のまま、使うときは `$` を前置して使います。`[0-9a-zA-Z_]+` が変数名だった気がするのでそれ以外の文字で区切られます。

``` shell
version=1.0
echo /path/to/lang/$version/bin/lang-$version
# => /path/to/lang/1.0/bin/lang-1.0
```

因みに代入の `=` の前後に空白を入れるとエラーです。

展開する変数名がアレな場合や変数名へ区切が必要な場合は `{変数名}` とすることで任意の名前の変数を展開できます。

``` shell
echo lang_$version_date
# "version_date" という変数名と認識される
# => lang_
echo lang_${version}_date
# これだとちゃんと`version`という変数名で認識される
# => lang_1.0_date
```

さらに[変数置換](http://d.hatena.ne.jp/ozuma/20130928/1380380390)などの複雑な記法もありますが、複雑なシェルスクリプトを読むときくらいしか要らない知識なのでやめておきます。一言触れておくと、POSIX準拠のものと拡張シェルにのみ存在するものがあるので気をつけましょう。


## 環境変数
シェル変数より環境変数をよく使うと思います。
シェルからみたらシェル変数も環境変数もあまり変わりませんが他のコマンドを起動したときに引き継がれるかが異なります。

``` shell
# シェル変数を環境変数に
export hoge
# 新たに環境変数を定義
export hoge=fuga
```

あるいは1つのコマンド実行時にだけ環境変数を設定できればいいのであれば

``` shell
hoge=fuga command1
```

という構文で設定しつつ実行できます。
ややこしいのですが、同様のことをする外部コマンド`env`もあって、

``` shell
env hoge=fuga command1
```

としても実行できます。まあ、前者がよく使われますかね。

## Stringly Typed

POSIXシェルには文字列しかありません。
たまに数値計算をするコマンドがありますがあれは数字だけが並んだ文字列を内部で数値に変換して計算、文字列にして返しているだけです。

拡張POSIXシェルでは配列変数や連想配列変数があるようですが私は使わないので知りません。

## 文字列
特にクォートしなければ空白文字区切で文字列と認識されます(重要)。

クォートしたければ記法は2種類あって、それぞれ意味が異なります。

`"文字列"` は内部でエスケープ記号や変数の展開が行なわれます。

``` shell
hoge=world
echo "hello\n$hoge"
# -> hello
# -> world
```

`'文字列'` は一切のエスケープ処理や変数展開を行いません

``` shell
hoge=world
echo 'hello\n$hoge'
# -> hello\n$hoge
```

※この結果はbashでのものです。後述のechoコマンドの移植性の問題でzshなどを使っているとこの結果になりません

 `'文字列'` 内では `'` のエスケープが行われないので `'` を入れられません。どうしても入れたい場合は一旦文字列を終了させてからシェルのエスケープを使って`'`を打ち、また`'`を始めることになるでしょう。

``` shell
$ echo 'this contains a single quote('\'') mark'
this contains a single quote(') mark
```

クォート単位が複数になっていてもスペースさえ空いていなければ1つの文字列として認識されちゃうんですね。

おおまかな指針として、特に何もなければ `'文字列'` を、変数展開したい場合は `"文字列"` を使うとよいでしょう。
拡張シェルを使っていると`$`以外の記号(例えばzshで`!`など)も展開対象になるので気をつけましょう。

2017-10-30 追記
<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">hoge=world<br>echo &quot;hello\n$hoge&quot;<br># -&gt; hello<br># -&gt; world<br>これ↑ですが、\n を解釈するのはある種の echo が行なっていることです。<br>printf &#39;%s&#39; &quot;hello\n$hoge&quot; |od -tcx1<br>などとして確認を。</p>&mdash; ふみやす＠シェルまおう(自称でない)🚲 (@satoh_fumiyasu) <a href="https://twitter.com/satoh_fumiyasu/status/924884773423628288?ref_src=twsrc%5Etfw">2017年10月30日</a></blockquote>

/追記

## ヒアドキュメント
ヒアドキュメントがあります。Rubyとかにあるやつですね。これは文字列ではなく標準入力として扱われます。

クォートの有無で変数展開の有無が変わるので気をつけましょう。

クォートなし
``` shell
hoge=fuga
cat <<EOF
This is $hoge
EOF
# -> This is fuga
```

クォートあり
``` shell
hoge=fuga
cat <<'EOF'
This is $hoge
EOF
# -> This is $hoge
```

パイプをつなげるときはこう書きます。

``` shell
cat <<EOF | tr a-z A-Z
hello
EOF
# -> HELLO
```

## コマンド置換
コマンドは基本的には標準出入力でやり取りしますが、たまに結果を変数に格納したい、引数に渡したいなどの需要が発生します。
そういうときにはコマンド置換で出力を文字列にしてあげます。記法が2つありますが、ネストの扱い以外振る舞いはおなじです。

バッククォート記法
``` shell
echo `echo ok`
# -> ok

# ネストはバックスラッシュでエスケープ
echo `echo \`echo ok\``
# -> ok
```

`$()`記法

``` shell
echo $(echo ok)
# -> ok

# ネストは自然に
echo $(echo $(echo ok))
```

確か`$()`記法はPOSIX標準ではないけど事実上ほとんどのシェルで使えるとかだったきがします。

## 組み込みコマンド
`echo`や`cd`などいくつかのコマンドはシェルの組み込みコマンドとして実装されています。
これらは外部コマンドとして実行出来ないので例えば`xargs`や`sudo`に渡しても実行できなかったりします。
しかしここでややこしいことに組み込みコマンドであるはずのものでも利便性のために外部コマンドとしても用意されていることもあります。

``` console
$ which echo
echo: shell built-in command
$ ls /bin/echo
/bin/echo
```

まあなので「`echo`は基本的には`xargs`や`sudo`に渡せないけど渡せる可能性もある」くらいにおぼえておいて下さい。

あとは互換性問題。組み込みコマンドということは実装ごとに挙動が違う訳です。
例えば[この記事](https://fumiyas.github.io/2013/12/08/echo.sh-advent-calendar.html)にあるように`echo`に非互換があります。 
*echoコマンドが* `\n` などのエスケープシーケンスを解釈するかで違いがあります。
なので上の方の例は`''`リテラルにはエスケープは解釈されなくてもzshの`echo`には解釈されて改行されてしまいます。

その他`time`は出力フォーマットがバラバラです。というか`time`はPOSIXに定義されてない組み込みコマンドですね。
あとやっぱり外部コマンドも存在します。`time`の結果をパースするときは注意しましょう。

``` console
$ bash -c 'time expr 1 + 1' 
2

real    0m0.001s
user    0m0.001s
sys     0m0.000s
$ zsh -c 'time expr 1 + 1'
2
expr 1 + 1  0.00s user 0.00s system 89% cpu 0.001 total
$ /usr/bin/time expr 1 + 1
2
0.00user 0.00system 0:00.00elapsed 100%CPU (0avgtext+0avgdata 2096maxresident)k
0inputs+0outputs (0major+79minor)pagefaults 0swaps
```

2017-10-30 追記
<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">time(1)コマンドについても、POSIXで定義されています: <a href="https://t.co/tf1IHCfgOz">https://t.co/tf1IHCfgOz</a><br>記述にある通り、デフォルトの出力形式がバラバラですが、イマドキのOSなら「-p」オプションつければ最低限の互換性は確保できるかと</p>&mdash; SODA Noriyuki (@n_soda) <a href="https://twitter.com/n_soda/status/924895533629505536?ref_src=twsrc%5Etfw">2017年10月30日</a></blockquote>

とのことですが、普通にUbuntu(の少なくとも14.10)に入っていないので実用上気をつけましょう。
/追記


## シェルとコマンドの区別
[昔のエントリ](/blog/2015/06/17/shell_scriptwokakutokinikiwotsuketaikoto/)でも触れましたがシェルレベルとコマンドレベルを区別しましょう。

root権限でファイルに書くつもりで

``` shell
command1 | sudo command2 > file
```

と書いたとき

``` shell
o | o > file
|   |
|   +- sudo command2
|
+- command
```

と解釈されてsudoの範囲が `> file` にまで及びません。

``` shell
command1 | command2 | sudo tee file
```

ならば

``` shell
o | o | o
|   |   |
|   |   +- sudo tee file
|   +- command2
+- command1
```

と解釈されるので意図通りです。



さて、基本を終えたのでコマンドラインで便利なもの、シェルスクリプトで便利なものに分けて紹介していきましょう。

# コマンドライン編
## リダイレクト
ちょっと細かく説明します。

プログラムを箱に例えると、箱には外部とやり取りするための穴が必要です。
さもなくば我々にできることはプログラムを実行してCPUが熱くなるのを眺めるくらいしかありません。
ということでプログラムには穴が空いています。最大1024個くらい。これをfd(ファイルディスクリプタ)といいます。
ファイルを開いたりソケットに繋いだりするのに使われます。
0, 1, 2番のfdは標準で開いていて、それぞれ標準入力、標準出力、標準エラー出力です。

シェルはデフォルトでターミナルからの入力を0に、1と2をターミナルへの出力につないでいます。
このfdと出入力先の繋ぎ変えをするのがリダイレクトです。

例えば下記はrubyでfd 9に書いてシェルで9を1に繋ぎ変えてターミナルに表示する例です。

``` console
$ ruby -e 'IO.open(9) {|out| out.puts "Hello, fd 9"}' 9>&1
Hello, fd 9
```

あるいはよくあるのはこういうやつですね。標準出力、標準エラー出力を `/dev/null/` に捨てる例です。

``` console
$ some_command > /dev/null 2>&1
```

どこにどの数値や記号を書くのか混乱しがちですが下記のよな構文になってます。

* [`[n]> file`](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_07_02) で fd `n` を `file` にリダイレクト、 `n` が省略されたら標準出力です。
* [`[n]>& m`](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_07_06) で fd `n` を fd `m` とおなじものに。 `n` が省略されたら標準出力です。

そして罠なのがいかにも宣言的っぽい見た目をしていながら書いた順に処理されます。上の例は

1. 標準入力を `/dev/null` に
2. `2` (標準エラー出力)を `1` と同じもの、つまり `/dev/null` に

という処理をします。イメージは手続き型言語で`fd_1=/dev/null; fd_2=fd_1`としている感じですね。

ちなみにパイプは標準出力のみを次のコマンドに繋ぎ変えます。

リダイレクトの愉快な仲間は[ここ](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_07)に色々乗っていますが上の２つと`>>`くらいしか使わないでしょう。
雑学として基礎知識のところで出てきたヒアドキュメントもリダイレクトの一種だったりします。

さて、これがPOSIX全般のの話で、拡張POSIXシェルにはもうちょい機能があります。
zshとかは複数リダイレクトなど色々拡張してるのですがひとまず覚えるのはこれ。

``` console
$ some_command >& /dev/null
```

[`>&` あるいは `&>`](http://zsh.sourceforge.net/Doc/Release/Redirection.html) は大抵の拡張POSIXシェルで使えるリダイレクトで、標準出力と標準エラー出力を同時にリダイレクトします。
上述のように `/dev/null` にリダイレクトすることが多いでしょうか。前者の記法はfdのリダイレクトと被ってますが数値かそれ以外で分けてるらしいです。

因みにリダイレクトでなくてパイプに繋ぎたいなら `|&` があります。

## for
シェルで繰り返しをしたいなら一応 `for` があります。

カレントディレクトリの.jpgの拡張子を.jpegに書き換えたければ

``` console
$ for f in $(ls *.jpg); do mv "$f" "$(basename $f .jpg).jpeg" ; done
```

です(実行してないので怪しいですが)。
セミコロンの位置が覚えづらいかもしれませんが

``` shell
for 変数 in スペース区切りの列
do
    本体
done
```

を1行で書くために改行をセミコロンにしているだけです。普通に改行して書いても構いません。

繰り返し対象に`*`を指定すると死ぬとか繰り返しが多いとプロセスフォークのオーバーヘッドで死ぬとかは自分でぐぐっておいてください。

## while read

割と評判が悪いのですが他にも繰り返しの手段はあります。
シェル組み込みの`while`と`read`を組み合わせた方法です。
さきほどのものと同じコードを書くと、

``` console
$ ls *.jpg | while read f; do
    mv "$f" "$(basename $f .jpg).jpeg"
done
```

となります。`for`と似たようなものですが`for`が`$(ls *.jpg)`と一旦繰り返し対象の文字列を作ってるのに対しこちらはパイプなので効率的です。
また、`read`は分配束縛ができるなどのメリットもあります。

デメリットは`read`に罠が多い(らしい)点です。

## xargs

おそらく繰り返しで一番有名なのが `xargs` でしょう。コマンドを並列実行してくれたり頼れるコマンドです。
しかし`xargs`だと先程のコードは正しく書き換えられません。

``` console
ls *.jpg | xargs -I@ mv @ "$(basename @ .jpg).jpeg"
```

と書いても意図通りにならないのです。これもシェルレベルとコマンドレベルの話です。


```
 o | o
 |   |
 |   +- xargs -I@ mv @ "$(o).jpeg"
 |                        |
 |                        +- basename @ .jpg
 +- ls *.jpg
```

このような構造になっているので `basename` の方の`@`が `basename` を評価したあとにプレースホルダ展開されるのです。

このように置換のプレースホルダが変数でないために不便が生じることもあります。
大抵はxargsが適当でしょうがこのようなケースや複数のコマンドを叩きたいケースでは上記の`for`や`while read`を使うことになるでしょう。

## ブレース展開

``` console
$ echo 1{0..9}
10 11 12 13 14 15 16 17 18 19
```

とかですね。これは拡張POSIXシェルの機能なので気をつけましょう。
あとこれはすべてメモリに展開されるのにも気をつけましょう。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">echo {000000000..9999999999}ってどうなるんだっけ？<br><br>桁は適当 <a href="https://twitter.com/hashtag/%E6%83%85%E5%A0%B1%E6%BC%8F%E6%B4%A9?src=hash&amp;ref_src=twsrc%5Etfw">#情報漏洩</a></p>&mdash; Ryuichi Ueda (@ryuichiueda) <a href="https://twitter.com/ryuichiueda/status/659026276032712705?ref_src=twsrc%5Etfw">2015年10月27日</a></blockquote>


## プロセス置換
拡張POSIXシェルでは実行したコマンドをファイルのように扱えます。

人によって直感的かが結構違うようですが、`>()` で書き込み可能な、`<()`で読み出し可能なファイルを作ります。

``` console
$ echo pohe > >(cat)
pohe
```

大抵のコマンドが標準入力からもコマンドに渡された引数からも入力を受け付けるのでありがたみが分かりづらいかもしれませんが複数の入力を渡すときに便利です。

``` console
diff <(command1) <(command2)
```

シェルスクリプトならファイルに書き出せばいいのでこれはコマンドラインで複雑なことをしようとするときに使われるようです。

因みに実体は `/proc` にあるfdです。

``` console
$ echo >(cat)
/proc/self/fd/12
```

## バックグラウンド実行とnohup, disown, supend
詳しくは[技術/UNIX/なぜnohupをバックグランドジョブとして起動するのが定番なのか？(擬似端末, Pseudo Terminal, SIGHUP他) - Glamenv-Septzen.net](https://www.glamenv-septzen.net/view/854)とか[HUPシグナルとnohupとdisownとバック/フォアグラウンドジョブの理解 - Qiita](https://qiita.com/yushin/items/732043ee23281f19f983)とかを読んで下さい。

シェルから実行されるコマンドにはひとまず3つのステータスがあって、フォアグラウンド、サスペンド、バックグラウンドがある。んでそれらを操作する組み込みコマンドもある。状態を確認する組み込みコマンドもある。
それらがややこしいしみなさん雰囲気で使ってますよねーって話です。私も説明できるほど詳しくないのでみなさん手元で実験しながら覚えて下さい。状態の確認コマンドは`jobs`で、状態変化コマンドは下図の通りです。


```
F: フォアグラウンド
S: サスペンド
B: バックグラウンド

`cmd`      `cmd &`      `setsid cmd`
   |           |              |
  +---+  `fg` +---+ `disown`  |
  |   |<------|   |--------+  |
+-| F |       | B |        |  |
| |   |       |   |        v  v
| +---+       +---+        vvvv
|  ^ |         ^          >解脱<
|  | | Ctrl+z  | `bg`      ^^^^
|  | |         |            ^
|  | |  +---+  |            |
|  | +->|   |--+            |
|  +----| S |---------------+
|   `fg`|   |      `disown`
|       +---+
| Ctrl+c   vv
+-------->>死<
           ^^
```

* ここには載ってませんが`kill %jobid`で殺すことも可能です。
* `disown`は拡張POSIXシェルの機能のようです。
* `setsid` はPOSIXコマンドでないどころか多分Linux固有です。
* `setsid`の解脱と`disown`の解脱は多分違う機能です。disownの処理のソース読んでないですが。
* `nohup`は解脱せずに不死属性つける感じです。多分。

まあつまり何を言いたいかというとκeenも雰囲気で使ってます。

2017-10-30 追記
<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">setsidコマンドにがPOSIX外なのはその通りですが、Linux固有というわけでもなくて、たぶんSVR4発祥だったと思います。</p>&mdash; SODA Noriyuki (@n_soda) <a href="https://twitter.com/n_soda/status/924897473377665024?ref_src=twsrc%5Etfw">2017年10月30日</a></blockquote>
/追記

あとzshやbash 4.0以降にコプロセスというのがありますが詳しくないです。

# シェルスクリプト編
シェルスクリプトは雰囲気で書いてる人が多いでしょう。
普通にやるとただのシェル入門になるので危なげなところだけひろっていきます。

## ifとtestと論理演算

``` shell
if test_command
then
    then_command
else
    else_command
fi
```

else節はオプショナルです。

test_command のexit statusが0ならthen節が、それ以外ならelse節が実行されます。例えば以下のように使います。

``` shell
if grep pohe /etc/password > /dev/null 2>&1; then
    echo "Hello, pohe"
else
    echo "pohe is absent"
fi
```

上の例では`grep`を使いましたが、test_commandに使われる代表的なコマンドが`test`、別名`[`コマンドです。`[`として呼んだときは最後の引数が`]`でないといけません。
いいですか、最後の引数がです。`[ 1 = 1]` は最後の引数が`1]`なので不正です。

さて、2通り書き方があるとどちらが推奨かという話になりますが、`[]`が多いようです。
確か随分古いシステムで`[]`が使えないとかで`test`の方を使うスクリプトもありますが多くはないです。

肝心の書き方ですが、

* `[ A = B ]` `[ A != B ]` 文字列比較
* `[ N -eq M ]` `[ N -ne M ]` 数値比較(数値も実際は文字列なので上記コマンドでも比較可能な点に注意)
* `[ N -lt M ]` `[ N -gt M ]` &lt; と &gt;
* `[ N -le M ]` `[ N -ge M ]` &lt;= と &gt;=
* `[ -z A ]` `[ -n A ]` Aが空文字列か非空文字列か
* `[ -e P ]` `[ -s P ]` `[ -d P]` Pにファイルが存在するか、存在して中身があるか、Pがディレクトリか

などなどかなり沢山[定義されています](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html)。
ここでちょっとややこしいのが`test`コマンドはシェルスクリプトで多用されるため外部コマンドにしておくと遅いので多くの場合シェル組み込みになっています。
まれに互換問題が発生するので怖い人はちゃんとPOSIXの定義をみておきましょう。

この`test`コマンド、ある操作が足りないのに気づいたでしょうか。論理演算です。それはシェル組み込みの`&&`、`||`、`!`を使います。
ちゃんとショートサーキットもやってくれます。

``` shell
# 前述のとおりzshなどでは`!`の扱いに注意

echo 1 && echo 2 | cat
# => 1
# => 2
echo 1 || echo 2 | cat
# => 1
! echo 1 && ! echo 2 | cat
# => 1
! echo 1 || ! echo 2 | cat
# => 1
# => 2
```

これで一通りの条件式が書けますね。

``` shell
n=1
if [ "$n" -lt 0  ]; then
    echo "n is negative"
elif [ 0 -le "$n" ] && [ "$n" -lt 10 ]; then
    echo "n is small"
else
    echo "n is big"
fi
```


そしてまた拡張POSIX Shellの話です。奴らは独自に `[[]]` というコマンドも持っていて、書ける内容は大体`[]`のスーパーセットになってるようです。
論理和や論理積が書けるなどちゃんと考えて設計した?という気になるものから`[[ str =~ regex ]]` など便利なものもあるようですが例によって私は使わないので知りません。
1つ苦言を呈しておくとこの`[[]]`は無自覚に行われるbashismの代表格(κeen脳内調べ)であり、「よくわかんないけど強いらしいからこれ使う」でもいいとは思いますが流石にどのシェルで動かすつもりなのか意識して書きましょうね。
Shell Shockのときに分かったようにいざというときに困ります。

## サブシェルとコマンドグループ
複数のコマンドを1つに纏めたいとき、2種類の方法があります。

1つはサブシェル起動の`()`、もう一つはコマンドグループの`{}`です。

サブシェルは新たにシェルを立ち上げるので環境を汚し放題です。典型的には`cd`を使うでしょうか。あとは変数も親のシェルに影響しません。

``` shell
(
    cd build
    ./configure && make && make install
)
# ここではbuildディレクトリから抜けている
```

上級な使い方にサブシェルを使うことでfdをまとめて色々したりと使いみちは様々です。 CF [標準入力同士の diff - Qiita](https://qiita.com/bsdhack/items/55d5eced2fb3e6625d74)

コマンドグループはただコマンドをまとめるだけです。サブシェルのコストがないのと`cd`や変数への変更が残るなどの違いがあります。複数の出力をつなげたいときとか次のパイプに行く前にごにょごにょしたいときに使うでしょうか。

``` shell
{ echo header; cat nullpo.csv; } | ...
```

まあ、別にこれを`()`でやってもいいんですけどね。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">難しい話をするとね、AT&amp;T ksh や zsh では (command) はサブシェルではあるが fork(2) はしない実装もあるんだ。(条件に依るが詳しくは知らね) <a href="https://t.co/R4yhLLpMzC">https://t.co/R4yhLLpMzC</a></p>&mdash; ふみやす＠シェルまおう(自称でない)🚲 (@satoh_fumiyasu) <a href="https://twitter.com/satoh_fumiyasu/status/923519885904265216?ref_src=twsrc%5Etfw">2017年10月26日</a></blockquote>
<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">どうでもいい話をすると ( ) は文字として特別な意味があるので ( echo 1 )のように ; が必要ないけど { } の方は { echo 1; } のように ; が必須だったりする（そのぶん { } はクォートせずに書ける</p>&mdash; でこれき (@dico_leque) <a href="https://twitter.com/dico_leque/status/923528213657030658?ref_src=twsrc%5Etfw">2017年10月26日</a></blockquote>


## 関数
`関数名() コマンド` で定義します。大抵はコマンドグループを使って

``` shell
関数名() {
  コマンド
}
```

の形で使うでしょうか。拡張POSIXシェルで`function 関数名() コマンド`の構文も使えたりしますが私は使わないので違いは知りません。ローカル変数の扱いが違ったりするんですかね。

有名な関数はこれでしょうか。

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="und" dir="ltr">$ :(){: | : &amp;};: <a href="https://twitter.com/hashtag/%E5%8D%B1%E9%99%BA%E3%82%B7%E3%82%A7%E3%83%AB%E8%8A%B8?src=hash&amp;ref_src=twsrc%5Etfw">#危険シェル芸</a></p>&mdash; Ryuichi Ueda (@ryuichiueda) <a href="https://twitter.com/ryuichiueda/status/502070331616423937?ref_src=twsrc%5Etfw">2014年8月20日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

試さないでくださいね。PCがフリーズします。余談ですが`:`というコマンドは存在していて、何もしないのでpyhtonにおける`pass`みたいな使われ方をします。

閑話休題。POSIXシェルでのローカル変数ですが、そんなものはありません。頑張って下さい。関数の入れ子呼び出しで変数を上書きされて死んだことあります。頑張って下さい。
拡張POSIXシェルには流石にありますが、シェル毎に構文が違うらしいです。頑張って下さい。ステートレスプログラミングを推奨する感じですね。

あと基本的な考え方の話をすると、関数の返り値 = 標準出力への書き出しです。あとは一応exit statusも。`return n`です。`exit n`にするとシェル(スクリプト)ごと終了します。
入力は標準入力と引数。引数はスクリプトと同じく`$n`で参照できます。唯一ローカルに使える変数なので賢く使いましょう。

あとはシェル組み込みコマンドと同じく`xargs`や`sudo`には渡せないので注意。
bashならShell Shockで一躍有名になった関数export構文でどうにかできそうな気がしますが、やめときましょう。

## exprと$(())
シェル組み込み構文の`$(())` で計算できます。`expr`コマンドでもできます。

``` shell
echo $((1+1))
# => 2
expr 1 + 1
```

`$(())`の方はシェル組み込みなのでこれまた実装ごとの拡張があります。
例えば`$(())`の方はシェル拡張で`0x`の表記が使えるなど。
互換性を意識して`expr`を使うべきか速度をとって`$(())`を使うべきかわからないので雰囲気で使っていきましょう。

# 最後に
普段雰囲気で使ってるシェルが身近になったでしょうか。それととも余計怖くなったでしょうか。
ご覧の通りシェルを深堀すると互換性問題、複数の実装の知識、OSへの理解、ステートレスプログラミングの経験など様々なものが求められます。
普段使いするもの、ちょっとずつでいいので理解してあげてください。

本をお求めならこれがおすすめです[O'Reilly Japan - 詳解 シェルスクリプト](https://www.oreilly.co.jp/books/4873112672/)。
ここには書いてないシェルの評価規則とかが載ってます。

# メタ
この記事を書き始めたタイミングでOSアップデートしたらuim-skk+dvorak配列が使えなくなってしまいました。
この機にとdvorakもSKKも捨て、qwerty配列にibusの *かな入力* を使い始めました。
そしたらまあ全然書けなくて執筆開始が10-17なのに公開が10-30になってしまいました。
まだかな入力には馴れきってないです。
