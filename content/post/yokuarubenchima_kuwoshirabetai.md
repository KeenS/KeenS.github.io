---
categories: [言語実装]
date: 2022-01-03T02:35:58+09:00
title: "よくあるベンチマークを調べたい"
---

κeenです。よくある言語同士や言語実装同士の速さの比較に使われるベンチマークが実際にどんなところの性能を調べているのかを理解してないので軽く調べてみます。

<!--more-->

色々調べようかと思ったんですが、一番よく見るのは[Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)の結果かなと思うのでここで使われているベンチマーク10種について調べていきます。
調べるとはいっても説明を読んで、ベンチマークに使われてるプログラムを読んで感想を言っていくだけです。実際にプロファイルをとってどこがボトルネックになるとかまではやらずに想像で喋ってるので参考程度に見ていって下さい。


## fannkuch-redux
[fannkuch-redux](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/fannkuchredux.html#fannkuchredux)は数字列を操作するプログラムです。

求める計算は以下です

1. ${1,\cdots,n}$の並べ替えを受け取る 例: ${4,2,1,5,3}$
2. 最初の要素を見て（ここでは4）、先頭からその数の要素分だけ順序を反転させる: ${5,1,2,4,3}$
3. これを先頭の要素が1になるまで繰り返す: ${3,4,2,1,5}$ → ${2,4,3,1,5}$ → ${4,2,3,1,5}$ → ${1,3,2,4,5}$
4. 何回反転させたかを数える: 5
5. 以下のどちらかの式でチェックサムを計算する（どちらでも等価）:
   - $checksum = checksum + (\mathrm{if}\;{}permutation\\_index\;\mathrm{is}\;\mathrm{even}\;\mathrm{then}\;{}flips\\_count\;\mathrm{else}-flips\\_count)$
   - $checksum = checksum + (toggle\\_sign\_{-1\\_1} * flips\\_count)$
6. これを全ての $1$ から $n$ までの数の並べ替えで計算する
7. チェックサムと、最大の反転回数を出力する

個々の数字列に対しては並列に計算できるので、処理系の並列計算能力や配列操作、場合によっては数値の扱いあたりが効いてきそうですね。

因みにこれはベンチマーク用のプログラムみたいで、あんまり意味のあることはしてなさそうです。fannkuchはパンケーキ（ホットケーキ）の意味で、ホットケーキを裏返すのを繰り返してる…らしいです。

## n-body
[n-body](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/nbody.html#nbody)は[木星型惑星](https://ja.wikipedia.org/wiki/木星型惑星)の軌道を計算するプログラムです。木星型惑星は太陽系でいうと木星、土星、天王星、海王星です。日本語でも普通に[N体問題](https://ja.wikipedia.org/wiki/N体シミュレーション)で知られていますね。

プログラムの中身はそこまで難しいものではなく、主にループと浮動小数点数計算で構成されています。
ベンチマークプログラムとして浮動小数点数計算がメインなので数値計算屋さんの戦場になっておりSIMD命令などを使った最適化の戦いが繰り広げられています。
SIMDを使えない言語ではシンプルな演算の性能のベンチマークになるようです。


## spectal-norm
[spectral-norm](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/spectralnorm.html#spectralnorm)は行列のスペクトルノルムの近似値を[べき乗法](https://ja.wikipedia.org/wiki/べき乗法)で計算するプログラムです。スペクトルノルムについては[Wikipediaの行列ノルムのページ](https://ja.wikipedia.org/wiki/%E8%A1%8C%E5%88%97%E3%83%8E%E3%83%AB%E3%83%A0)や[特異値のページ](https://ja.wikipedia.org/wiki/%E7%89%B9%E7%95%B0%E5%80%A4)も参考にして下さい。
計算の元になる行列の要素は以下の式で与えられています。

\\\[
a\_{ij} = \frac{1}{(i+j)(i+j+1)/2+i+1}
\\\]

添字を渡されたらどんなサイズの行列でも要素を計算できるのでベンチマーク向けにサイズを増やすのに向いてます。

具体的なスペクトルノルムの計算は $A^TA$ を20回掛けて求めています。

行列計算なので粗粒度並列性（≒スレッド）と細粒度並列性（≒SIMD）の両方が使えるところであり、かつキャッシュの有効活用なんかも重要なのでそれぞれを使える/使えないが大きく点数に響きます。


## mandelbrot
[mandelbrot](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/mandelbrot.html#mandelbrot)は[マンデルブロ集合](https://ja.wikipedia.org/wiki/マンデルブロ集合)を描画するプログラムです。マンデルブロ集合は投げ付けたら痛そうなアレですね。

<img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAMgAAADIAQAAAACFI5MzAAAABGdBTUEAALGPC/xhBQAAAAJiS0dEAAHdihOkAAAAB3RJTUUH5gEDCxMZKs3S0gAAAt9JREFUWMPlmDFv01AQx89NSoDFbBlAChIfoB1YmNxv0JWJma3dkbBRFWVBYmRC8DmQoJ mRkDoyIMVbN7CgUi2I/bCfG+fOvv+FZq07JOovd+/ee353/3vkek/RfJBDz00mISRTRMoTaLMFuRghklGskzJBxBHtQTIAJKERGIcohN6AzR9os4Q2OLbC8nZ/i5lOAJlDklKkkNPqL0W7EOVBeb7ok+LzMdHdWLN5u09Eh5c9UpTTCtDurEvK T2UNKDjpkuKJ84RmVYyS0K8DT8LeOEne2Ax6hJ43ZCd+dyrIX2qcEb0cxoLklFyRZyS9LWn1PAg682nJ7aAb24oMuyRrjTrjVPvZPjEYp54rJ7/XZFcSNkwkSJyvSSjIj8drMhYkfbgmdwT5Rii274zsCHLBScTJkhG5BmwJKIBEruhHTl5wkn Oyx8lTTh4xIoahgNskAsWICJuUkwEf5wMn4xX5Kd+Peus8WbjFe5cdcDJZ2cw6C7qymdYFRM7nKoL5+euvzgkyasirM3qjkjJJKr+5ZlPFe6sTQdAS0slZ9fUoE4Si1maizaesVzlUI/Bfpbdmpl/8kJnizf9voHlrtuxYIc2vpbdg7a1j0+zp XCEhI/sKSSHRbA4ZuSdIfX7Ko0Qh9Wksg4T6j/Nk3gf+NC4VC/KZTyU+U+Qa8Rkp1UjUef14aJaNMQ6ODc8Hr4GxbnCt8f5Ye5oqZNO705BjhWQKGW94r1PFZrTh/OAzh8+pcbbrCan5YEMOgRlJz1VGfoM50cijOPfifG3leFQXrFpi1h+jZg nC65xRG2E9NWowrtu41mN9gDUF1iFYu2C9gzUS1lVYi2H9Zmg+rBOxtjT0aLvfw//WvVgrY32NNTnW8Vj7434B9xi4L8G9jMP9j9UzoT7L7M1AP9fs6wQQ3DfCXhP3p4XVVWNv1+63cY9u9PXJ9Xt0fOdg3FPAuw3jPmSb2xXjrgbe7xh3Qu7G kX+EUbjwioSYYgAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMi0wMS0wM1QxMToxMjoyNCswMDowMO9uj7gAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjItMDEtMDNUMTE6MTI6MjQrMDA6MDCeMzcEAAAAAElFTkSuQmCC">

画素ごとの計算が可能で自由な並列化ができるのでそのあたりの速さが必要そうです。あと地味にバイナリデータを出力するのでバイナリデータの出入力が弱いとマイナスポイントかもしれません。

## pidigits

[pidigits](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/pidigits.html#pidigits)は[円周率](https://ja.wikipedia.org/wiki/円周率)πの10進表記を求め、表示するプログラムです。アルゴリズムに指定があります。内部で多倍長整数を使うので多倍長整数ライブラリの出来の良さが効いてきます。

## regex-redux
[regex-redux](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/regexredux.html#regexredux)は正規表現でDNA列のフォーマットの[FASTA](https://en.wikipedia.org/wiki/FASTA_format)を処理するプログラムです。

入力の改行を正規表現処理したり、特定の塩基列とその逆相補配列をさがしたり、特定のパターンを置き換えたりします。

正規表現エンジンの速度や文字列確保の効率などが効いてきます。

## fasta
[fasta](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/fasta.html#fasta)は先述のDNA列のフォーマットであるFASTAを生成するプログラムで、アルゴリズムに指定があります。DNA列を繰り返したりランダム生成したりします。

ループの中で分岐や（プログラムの作りによっては）文字列確保をするので基礎スペックが効いてきます。ランダムとはいってもアルゴリズム指定があり、結果の一致も求められるのですが、工夫すると並列化もできるようなので並列化性能も効くみたいです。

## k-nucleotide
[k-nucleotide](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/knucleotide.html#knucleotide)はハッシュマップを使ってFASTAフォーマットで与えられる塩基を数えるプログラムです。各塩基の単体、2つ組の全てのカウントを出力し、3、4、6、12、18つ組の全てをカウントして特定の組のカウントを出力することが求められます。

ハッシュマップ自作が認められない、容量も手動確保せずに自動スケーリングに任せるなどの制約があり、組み込み/ライブラリのハッシュマップの速度を競う内容になっています。


## reverse-complement
[reverse-complement](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/revcomp.html#revcomp)はFASTAフォーマットで与えられたDNAの逆相補配列を出力するプログラムです。アルゴリズムの指定があり、入力を1行ずつ自動バッファリングに任せて読み込むなどの制約があります。

ナイーブな出入力の速度と反転に必要な文字列処理が効いてくるベンチマークになります。…が、入力と反転処理を並行でやるために工夫したり反転操作は上手くスレッド並列やSIMD化したりもできるので、やっぱり並列処理に強い言語が強くなります。

## binary-trees
[binary-trees](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/binarytrees.html#binarytrees)は完全二分木を操作するプログラムです。アロケーションに指定があり、最低でも参照実装のプログラムと同じ回数だけのアロケーションが求められます。また、独自のメモリ管理手段を用意することは禁止されています。

アロケーションに指定がある通り、メモリの扱いが効いてくるベンチマークです。あと地味にポインタを行ったり来たりするのでポインタの効率が悪いとマイナスポイントになります。アロケーションの指定で自作アロケータは禁止されているものの、外部ライブラリのアロケータを使うのは許されているようで、そういった手動メモリ管理ができる言語が大幅に有利になっています。

---

ざっと眺めてみましたが、結構大変ですね。前半で数値計算が多くて「大丈夫か？」とも思ったのですが後半で文字列処理やハッシュマップ、メモリアロケーションなどが登場したのでそれなりにバランスの取れたベンチマークなんじゃないでしょうか。

余談ですがベンチマークスイートのネタとして[ypsilonのベンチマーク](https://github.com/imrehg/ypsilon/tree/master/bench/gambit-benchmarks)なんかもあったのですが、大変なのでやめました。

とりあえず個人的にはどんなベンチマークを取ってるのか納得できたのですっきりしました。
