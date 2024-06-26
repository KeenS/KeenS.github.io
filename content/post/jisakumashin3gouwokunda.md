---
categories: [番外編, 日記]
date: 2024-05-29T23:47:30+09:00
title: "自作マシン3号を組んだ"
---
κeenです。ゴールデンウィークに買ったマシンがややトラブルありつつも組み上がって安定稼動しはじめたのでメモを残します。

<!--more-->
[前回](/blog/2020/02/16/jisakumashin2goukunda/)に引き続き新しいThreadripperが出たので買いました。3990Xから7980Xです。コア数やスレッド数は変わってませんがキャッシュが増えたりzen4アーキテクチャになったりで40%くらいの性能向上があるらしいです。他にもPCIe5やDDR5メモリのサポートがあり、全体的に色々アップグレードされます。

## ハードウェア

パーツは以下になりました。いくつかミスがあったので真似する人はこのまま買うのをお勧めしません。

* マザーボード: [ASRock TRX50 WS](https://shop.tsukumo.co.jp/goods/4710483944529)
* CPU: [AMD Ryzen Threadripper](https://shop.tsukumo.co.jp/goods/0730143315753)
* メモリ: [Samsung 64GB DDR5 4800MHz PC5-38400 ECC RDIMM 2Rx4 (EC8 10x4)](https://www.amazon.co.jp/gp/product/B0CFG7THWM) x 4
* ストレージ:
    + [Crucial T700　CT2000T700SSD3JP M.2 NVMe 内蔵SSD / 2TB / PCIe Gen5x4 / ヒートシンク無](https://shop.tsukumo.co.jp/goods/0649528937551) x 4
    + [Asrock Blazing Quad M.2 Card](https://shop.tsukumo.co.jp/goods/4710483942808) (PCIeスロットに4枚M.2ストレージ挿せるやつ)
* GPU: [Radeon RX 7800 XT](https://shop.tsukumo.co.jp/goods/4710483943683)
* CPUクーラー: [SilverStone IceGem 240P](https://shop.tsukumo.co.jp/goods/4589657258355)
* 電源ユニット: [MSI MAG A1000GL PCIE5](https://shop.tsukumo.co.jp/goods/4526541048319)
* ケース: [FSP CUT593P-B](https://shop.tsukumo.co.jp/goods/4713224529108)

前回からの主なアップデートは以下です

* Threadripper 3990X → Threadripper 7980X
  + zen2 → zen4 でAVX512が使えるようになったりIPCの向上など
* DDR4メモリ128GB → DDR5メモリ256GB
  + 1HTあたり1GBだと足りないケースがあった
  + DDR5になって速度向上
* PCIe 4 NVMe 500GB x 6 → PCIe 5 NVMe 2TBx4
  + カメラを持ちはじめてストレージが足りなくなったので増強
  + 本数が多いと管理が面倒だったので1枚あたりの容量を増やして本数は抑えた
    + マザボについてるスロットは一旦GPU外さないといけないので取り回しが悪い。
  + とりあえずPCIe 5にしてみたけどどれくらい違うのやら

メモリやストレージの増量は分かりやすいですね。CPUに関しては今後AVX512で遊んで実感しようと思います。あと後述しますがファイルシステムが変わった関係でPCIe5のストレージはあんまり活かせてなさそう。

ところで、はじめてネットで買ったら色々ミスりました。不適切な組み合わせのパーツがいくつかあります。

* ThreadripperがサポートするDDR5メモリはECCのついたRegisterd DIMM (RDIMM)と呼ばれるもので、ECCのない普通のDIMMはサポートしない
    + 知らずに一度ECCなしのメモリを買って買い直した
    + それが届くのに3週間くらいかかり、GWに買ったはずなのに月末に組み上がった
* マザーボードのTRX50のフォームファクタはE-ATXで、ネジの位置などはATXと共通だが、基盤が大きい。
    + ケースがaTX向けだったのでケーブルを通す穴などが合わなかった。無理矢理鉄板をペンチで曲げてケーブルを通した
    + 規格間違いがなければ両面の蓋がドライバーなしで開けられて良いケースだっただけに残念

その他トラブルだと何度試してもPCIeエラーで電源が点かず途方にくれてたら実はマザボに挿す電源が1本挿さってなかったなどがありました。普通の電源の他にグラフィックカード用の電源もマザボに挿すことになっているようで(CPU本体が電力食いすぎてグラボに回す電力足りないのかな？)、見落していました。



## ソフトウェア

話が遡るんですが、GW直前くらいに2号機が壊れました。
恐らくメモリ故障→ストレージのデータ破損の流れでRAIDが崩壊しました。
btrfsのRAID 5を使っているので1本の故障なら耐えられるはずでしたが、メモリが完全に故障する前に中途半端に動いて2本同時に認識できなくなったり(恐らく)復旧コマンドを間違えたりして結局復旧はかないませんでした。
トラブルを経験するためにあまり使ってる人がいなそうなbtrfsのRAID 5を使っていたので壊れたことはいいんですが、どうせならちゃんと復旧するところまでやりたかったですね。


不幸中の幸いで壊れる数時間前にフルバックアップが走っていた([昔の記事でやったやつ](https://keens.github.io/blog/2022/04/30/tarwotsukattabakkuappuwomaishuutoru/)です)のでOSをインストールし直すことにしました。
ここで実はbtrfsのRAID 5サポートが非推奨になっていることを知ります。そこで仕方ないのでzfsにしてRAIDもRAID Zにしました。そのときのログは以下に残してあります。

[Ubuntuをzfsルートにしたときのメモ](https://zenn.dev/blackenedgold/scraps/7fcb8e9b2790bb)

バックアップの仕組みもzfsに合わせたものにしました。あとbtrfsになかった機能として暗号化があるのでそれも試してみました。

暗号化付きZFSを使ってみると、結構メモリを食ったり(デフォルトの設定でメモリの半分まで使う、つまり128GBメモリを食う)、IOが遅かったりと性能面で不満が出ます。
ハードウェアの性能は向上しているにもかかわらずIO性能は目にみえて低下しました。
btrfsの頃はフルバックアップが30分未満で終わっていたものが100分以上かかるようになってます。
そもそも律速段階がデータの圧縮(pixz)だったのがファイルの読み出し(tar)に移っていますしね。
復号がマルチスレッドで早くなってくれたらうれしいんですが難しそうなのでtarの方がなんか良い感じにマルチスレッドでファイルを読み出すようになればいいんですかね。今後の課題とします。
まあ、zfsは速さよりも安心感を求める人向けでしょうか。次にストレージが破損したときは無事にRAIDが機能するといいですね。メモリもECCつきですしメモリがアキレス腱になることもないでしょう。

----

Threadripperはコア数は増えてないのにドル建ての段階でも日本円でもかなり値上がりしてお財布に優しくないですね。特に今回はメモリを間違って買って買い直したりした関係で2号マシンの倍以上の価格になりました。これから元がとれるくらいガンガン使っていくぞ
