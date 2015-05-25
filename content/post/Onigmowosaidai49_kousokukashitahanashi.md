---
categories: [正規表現]
date: 2015-05-26T01:14:54+09:00
title: Onigmoを最大49%高速化した話
---

κeenです。Rubyでも使われてる高速な正規表現エンジン、Onigmo(鬼雲)を高速化したのでその話を。
<!--more-->
先日、[正規表現技術入門を読んだ](http://keens.github.io/blog/2015/05/10/seikihyougengijutsunyuumonwoyonda/)というエントリの中で

> ところで本に載ってた鬼雲のコードはDT(編注: Direct Threaded)にしてなかったけど簡単のためなのかな？あるいは厳格にC89に準拠するため？picrinみたくプリプロセッサで分岐すれば使えるのに。

と書いたところ、鬼雲の作者、K.Takataさんから

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/k_takata">@k_takata</a> 「picrinみたくプリプロセッサで分岐すれば使えるのに。」これも知らなかった。</p>&mdash; K.Takata (@k_takata) <a href="https://twitter.com/k_takata/status/597690447499108352">2015, 5月 11</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

という反応を頂きました。そして[イシュー](https://github.com/k-takata/Onigmo/issues/51)にも乗ったので言い出しっぺとして実装してみました。[こちらのプルリク](https://github.com/k-takata/Onigmo/pull/52)です。

Direct Threaded VM自体の解説はRubyist Magazineに載っている笹田さんのものが詳しいようです [Rubyist Magazine - YARV Maniacs 【第 3 回】 命令ディスパッチの高速化](http://magazine.rubyist.net/?0008-YarvManiacs)


実装は少し技巧的ですが`while`, `switch`, `case`, `break`, `continue`などをマクロでラップしつつDT VMが有効ならそれらと互換性のあるDT用のコード（`goto`やラベル）に展開します。元は[picrin](https://github.com/picrin-scheme/picrin/blob/master/extlib/benz/vm.c#L583)で使われていたテクニックです。
このコードは [@wasabiz](https://twitter.com/wasabiz)が書いたものなのでpicrinがどこを参考にして書かれたかは@wasabizに聞いて下さい。もしかしたらわさびずの発明かもしれませんね。

で、パフォーマンスの方ですが、最初、素直に制御命令を1つづつマクロで書き換えたのですが、こうなりました。

master

<table>
<tr><td>パターン</td><td>時間</td></tr>
<tr><td class="pattern">Twain</td><td class="time">47 ms</td></tr>
<tr><td class="pattern">^Twain</td><td class="time">47 ms</td></tr>
<tr><td class="pattern">Twain$</td><td class="time">47 ms</td></tr>
<tr><td class="pattern">Huck[a-zA-Z]+|Finn[a-zA-Z]+</td><td class="time">127 ms</td></tr>
<tr><td class="pattern">a[^x]{20}b</td><td class="time">1172 ms</td></tr>
<tr><td class="pattern">Tom|Sawyer|Huckleberry|Finn</td><td class="time">151 ms</td></tr>
<tr><td class="pattern">.{0,3}(Tom|Sawyer|Huckleberry|Finn)</td><td class="time">497 ms</td></tr>
<tr><td class="pattern">[a-zA-Z]+ing</td><td class="time">4032 ms</td></tr>
<tr><td class="pattern">^[a-zA-Z]{0,4}ing[^a-zA-Z]</td><td class="time">96 ms</td></tr>
<tr><td class="pattern">[a-zA-Z]+ing$</td><td class="time">4175 ms</td></tr>
<tr><td class="pattern">^[a-zA-Z ]{5,}$</td><td class="time">1770 ms</td></tr>
<tr><td class="pattern">^.{16,20}$</td><td class="time">1757 ms</td></tr>
<tr><td class="pattern">([a-f](.[d-m].){0,2}[h-n]){2}</td><td class="time">1849 ms</td></tr>
<tr><td class="pattern">([A-Za-z]awyer|[A-Za-z]inn)[^a-zA-Z]</td><td class="time">656 ms</td></tr>
<tr><td class="pattern">"[^"]{0,30}[?!\.]"</td><td class="time">115 ms</td></tr>
<tr><td class="pattern">Tom.{10,25}river|river.{10,25}Tom</td><td class="time">260 ms</td></tr>
</table>


DT版

<table>
<tr><td>パターン</td><td>時間</td></tr>
<tr><td class="pattern">Twain</td><td class="time">100 ms</td></tr>
<tr><td class="pattern">^Twain</td><td class="time">99 ms</td></tr>
<tr><td class="pattern">Twain$</td><td class="time">100 ms</td></tr>
<tr><td class="pattern">Huck[a-zA-Z]+|Finn[a-zA-Z]+</td><td class="time">246 ms</td></tr>
<tr><td class="pattern">a[^x]{20}b</td><td class="time">2182 ms</td></tr>
<tr><td class="pattern">Tom|Sawyer|Huckleberry|Finn</td><td class="time">288 ms</td></tr>
<tr><td class="pattern">.{0,3}(Tom|Sawyer|Huckleberry|Finn)</td><td class="time">847 ms</td></tr>
<tr><td class="pattern">[a-zA-Z]+ing</td><td class="time">6278 ms</td></tr>
<tr><td class="pattern">^[a-zA-Z]{0,4}ing[^a-zA-Z]</td><td class="time">203 ms</td></tr>
<tr><td class="pattern">[a-zA-Z]+ing$</td><td class="time">6430 ms</td></tr>
<tr><td class="pattern">^[a-zA-Z ]{5,}$</td><td class="time">3603 ms</td></tr>
<tr><td class="pattern">^.{16,20}$</td><td class="time">3596 ms</td></tr>
<tr><td class="pattern">([a-f](.[d-m].){0,2}[h-n]){2}</td><td class="time">3239 ms</td></tr>
<tr><td class="pattern">([A-Za-z]awyer|[A-Za-z]inn)[^a-zA-Z]</td><td class="time">1039 ms</td></tr>
<tr><td class="pattern">"[^"]{0,30}[?!\.]"</td><td class="time">327 ms</td></tr>
<tr><td class="pattern">Tom.{10,25}river|river.{10,25}Tom</td><td class="time">487 ms</td></tr>
</table>

はい。DT版の方が2倍ちょっと遅いです。そりゃないわー。最適化オプションとかも確認したのですがダメでした。

諦めて布団に入った後、ふと思い当たる節がありました。

元のコードで


```c
case OP_XXX:
   ...
   continue;
   break;
```

というパターンが割と現れます。`continue`の後の`break`は本来なら不要ですが`case`を書く際の作法というか癖というか
とにかく`break`を付けるスタイルもあります。これもそうなのでしょう。こいつらをマクロで書き換える時に愚直に

```c
CASE(OP_XXX)
   ...
   JUMP;
   NEXT;
```

としてました。ここで、

```c
#if USE_DIRECT_THREADED_VM
#define NEXT sprev = sbegin; JUMP
#define JUMP goto *oplabels[*p++]
#else
#define NEXT break
#define JUMP continue
#endif
```

です。これはUSE_DIRECT_THREADED_VMが定義されてる時は

```c
L_OP_XXX:
   ...
   goto *oplabels[*p++];
   sprev = sbegin;goto *oplabels[*p++];
```

と展開され、`goto`が2つ現れることになります。どうせ無用コードだし最適化で消えるだろと思ってたらそうでもないらしく、

```c
CASE(OP_XXX)
   ...
   JUMP;
   NEXT;
```
を

```c
CASE(OP_XXX)
   ...
   JUMP;
```

にし、マクロの方も

```c
#if USE_DIRECT_THREADED_VM
#define NEXT sprev = sbegin; JUMP
#define JUMP goto *oplabels[*p++]
#else
#define NEXT break
#define JUMP continue; break
#endif
```

としたらようやく本領発揮してくれました。その結果がこれです。

<table>
<tr><td></td><td>Master</td><td>This PR</td><td>Improve Rate</td></tr>
<tr><td class="pattern">Twain</td><td class="time">47 ms</td><td class="time">47 ms</td><td>0%</td></tr>
<tr><td class="pattern">^Twain</td><td class="time">47 ms</td><td class="time">47 ms</td><td>0%</td></tr>
<tr><td class="pattern">Twain$</td><td class="time">47 ms</td><td class="time">47 ms</td><td>0%</td></tr>
<tr><td class="pattern">Huck[a-zA-Z]+|Finn[a-zA-Z]+</td><td class="time">127 ms</td><td class="time">127 ms</td><td>0%</td></tr>
<tr><td class="pattern">a[^x]{20}b</td><td class="time">1172 ms</td><td class="time">889 ms</td><td>31%</td></tr>
<tr><td class="pattern">Tom|Sawyer|Huckleberry|Finn</td><td class="time">151 ms</td><td class="time">153 ms</td><td>-1%</td></tr>
<tr><td class="pattern">.{0,3}(Tom|Sawyer|Huckleberry|Finn)</td><td class="time">497 ms</td><td class="time">449 ms</td><td>10%</td></tr>
<tr><td class="pattern">[a-zA-Z]+ing</td><td class="time">4032 ms</td><td class="time">2705 ms</td><td>49%</td></tr>
<tr><td class="pattern">^[a-zA-Z]{0,4}ing[^a-zA-Z]</td><td class="time">96 ms</td><td class="time">98 ms</td><td>-2%</td></tr>
<tr><td class="pattern">[a-zA-Z]+ing$</td><td class="time">4175 ms</td><td class="time">2797 ms</td><td>49%</td></tr>
<tr><td class="pattern">^[a-zA-Z ]{5,}$</td><td class="time">1770 ms</td><td class="time">1623 ms</td><td>9%</td></tr>
<tr><td class="pattern">^.{16,20}$</td><td class="time">1757 ms</td><td class="time">1637 ms</td><td>7%</td></tr>
<tr><td class="pattern">([a-f](.[d-m].){0,2}[h-n]){2}</td><td class="time">1849 ms</td><td class="time">1670 ms</td><td>11%</td></tr>
<tr><td class="pattern">([A-Za-z]awyer|[A-Za-z]inn)[^a-zA-Z]</td><td class="time">656 ms</td><td class="time">607 ms</td><td>8%</td></tr>
<tr><td class="pattern">"[^"]{0,30}[?!\.]"</td><td class="time">115 ms</td><td class="time">93 ms</td><td>24%</td></tr>
<tr><td class="pattern">Tom.{10,25}river|river.{10,25}Tom</td><td class="time">260 ms</td><td class="time">262 ms</td><td>-1%</td></tr>
</table>

最大49%の高速化です。素晴しいですね。


因みに2つめの`goto`は実際には実行されないのに何故遅くなったかというとgotoはコンパイラにとってはコントロールフログラフを乱す厄介な奴なので
無用コード除去に引っ掛からなかったどころか最適化ルーチンを引っ掻き回したんじゃないかと思います。


このコード、私の手元の環境でしかテストしてないのでC89なら須くサポートするOnigmoにマージされるかは分かりませんがマージされると嬉しいですね。
