---
categories: [日記, 番外編]
date: 2018-05-13T21:11:39+09:00
title: "ツクモで自作マシン買った"
---
κeenです。ただの日記。初心者が自作マシンを組む時のメモ。

<!--more-->


なぜ自作マシンか。一番大きな理由はやってみたかったから。工作って楽しいよね。
あとはRyzenが評判いいので試してみたかったから。
Rustのコンパイルが遅くて困ってるんですよ。

# 秋葉原へ
知り合いに相談したらとりあえずドスパラに行くといいよと言われた。
ゴールデンウィークにドスパラへいくとRyzenどこに置いてるの、とかあんまよくわかんなくて、一旦店を出る。
他の店も見ようと思ってソフマップ、ツクモときてツクモに自作PC相談カウンターがあったのでそこで相談して買うことにした。

Ryzen 7のマシン組みたいですといったらはいはいと誘導してくれた。
どうせ7までしかないだろと思ったらThreadripperがあったので迷って一旦Threadripperで見積もりを取ることに。

そのまま店員さんとパーツを見て回る。要件は決まってたので割とサクサク決まっていった。
電源の選び方教えてらったり(電力容量は知ってたけどピンの数とか変換効率とかあるんだね。)した。
あとThreadripperは空冷だと冷却が間に合わないらしく、簡易水冷になった。
ゲームはしない人なのでGPUにこだわりはなく知人から余ってるのをもらった、OSはUbuntuなのでフリー。
ディスプレイもどうせ黒い画面しかみないので解像度だけ高い適当なものを見繕った。
SSDはNVMeにしたかったけど予定外のThreadripperで完全に予算オーバーなのでSATAで。
Threadripperをamazon.comで買うと大分安くなって差分でNVMeにできたけどここまで相談しといて買わないのもなんなのでツクモで買ってしまった。
いつかNVMeにして今のやつをバックアップ用にしよう。


<img style="width:100%;" alt="買ったパーツ" src="/images/my-machine/IMAG1362.jpg" />

# 組み立て

数学科の授業でPCを一回組み立てたことがあったのであまり心配はしてなかった。

困ったのは

* マニュアルには電源は24pin + 8pinしか書いてなかったけど実際は + 4pin必要だった。
* ケースが両面から作業するタイプで、扱いづらかった。特に電源へのアクセスが非常に悪い。
* マザボの X399 Taichi が C0 エラー(メモリ関連)で起動しない。
  + 先述の電源pinの問題やBIOSアップデートなどを試すが解決せず
  + 結局CPUを取り付け直すと解決した。そこかー。


<img style="width:100%;"  alt="組んだ状態" src="/images/my-machine/IMAG1368.jpg" />

# 性能とか

元のマシンもコンパイル速度重視で買った[T440p](https://www3.lenovo.com/jp/ja/notebooks/thinkpad/t-series/ThinkPad-T440p/p/22TP2TT440P)。Core i7 4910MQの4コア 8スレッド。
今度のマシンがAMD Ryzen threadripper 1950x 16コア32スレッド。

Rustのプロジェクトのコンパイルをしてみると思ったよりコアを使い切れず1.6倍程度。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">新マシンで業務開発してるやつのcargo clean &amp;&amp; cargo buildが1.6倍くらいに高速化してる。今のマシンの1.5倍くらいしたしこんなものかな。</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/995256685794025472?ref_src=twsrc%5Etfw">2018年5月12日</a></blockquote>
<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr">cargo testだったら並列性能が効いて1.8倍くらいになる</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/995258092731351040?ref_src=twsrc%5Etfw">2018年5月12日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

Rustのコンパイル単位はクレートという大きめなもので、依存順にコンパイルされる。
最初こそ32スレッド全部使い切るもののすぐに並列度が落ちてコアを半分くらいしか使えない状態に。
まあ、CPUが余ってるので他の作業はできるけど。
特にコンパイルしながらEmacsで編集してもracerやrlsが重くならないので生産性への影響は大きい。

# 最後に

日記なのでまとめとかはない。

デスクトップマシンにするにあたってキーボードを買う必要が出たのでHappy Hacking Keyboardを買った。
5年くらいこのタイトルでブログをやってるけどようやくHappy Hacκingするようになった。
