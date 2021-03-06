
---
categories: ["ELF", "compiler"]
date: 2020-04-12T17:30:06+09:00
title: "最小限のELF"
---

κeenです。
[先日の記事](/blog/2020/04/01/taneakirakashi__whitespacekonpairawotsukuttahanashinouragawa/)で言及した最小限のELFが需要ありそうだったので解説してみます。
コンパイラを作るときの参考にしてみて下さい

<!--more-->

ELFはExecutable and Linkable Formatを表わし、Linuxなどで標準の実行可能ファイルのフォーマットとなっています。
なんらかの形でこのファイルを生成するのがコンパイラの最終目標となります。
一般にはアセンブラまで吐いて外部のアセンブラやリンカにELFファイルの生成を任せることが多いのですが、自作するなら最後までやりたいという人向けにとっかかりになる最小限のELFファイルを解説します。
まずはこのファイルを生成するのを目指して、後から色々追加していけばインクリメンタルにできるよね、という訳です。

一部、「どうしてかは分からないけどこうしたら動く」みたいな箇所もあるので詳しい人がいたら教えて下さい。

# 全体構成

プログラムそのものの他に、プログラムのメタデータ2種類とファイルのメタデータが1つついています。

フォーマットはおおむね以下のような形になっています。

```text
+------------------------+
|       ELF header       |
+------------------------+
|+----------------------+|
|| Program header table ||
|+----------------------+|
||+--------------------+||
|||                    |||
|||       data         |||
|||                    |||
||+--------------------+||
|+----------------------+|
|| Setion header table  ||
|+----------------------+|
+------------------------+
```

ELFヘッダファイル全体のメタデータで、ELFのバージョンだとか、アーキテクチャだとか、プログラムヘッダテーブルやセクションヘッダテーブルの位置だとかを保持しています。

プログラムヘッダは実行時に関係するメタデータを持っています。プログラムをメモリアドレスのどこにロードするだとかを指定します。

セクションヘッダはリンク時に使う情報を保持しています。プログラムのここからここまでが何々という関数で〜のような情報を指定します。リンクの他にはGDBでデバッグするときの情報なんかも入ってるのかな？

プログラムヘッダとセクションヘッダはそれぞれ独立しているので1つのプログラムに対して別々にメタデータを指定します。極端な話、全然違う領域にメタデータをつけたり片方がなかったりしても問題ないのですが、だいたいのELFはプログラムヘッダが指定した領域を細分するようにセクションヘッダがついているようです。

プログラムヘッダ「テーブル」、セクションヘッダ「テーブル」とあるように、ELFファイル内でそれぞれのヘッダは複数あります。テーブルというかプログラム的な見方をすれば配列ですね。


今回は実行だけできればいいのでセクションヘッダは利用せずにプログラムヘッダだけでいきます。つまり、こうなります。

```text
+------------------------+
|       ELF header       |
+------------------------+
|+----------------------+|
|| Program header table ||
|+----------------------+|
||+--------------------+||
|||                    |||
|||       data         |||
|||                    |||
||+--------------------+||
|+----------------------+|
+------------------------+
```


# 実際のデータ

べんっと貼ります。Linux on amd64向けのELFファイルです。


```text
00000000: 7f45 4c46 0201 0100 0000 0000 0000 0000  .ELF............
00000010: 0200 3e00 0100 0000 b000 4000 0000 0000  ..>.......@.....
00000020: 4000 0000 0000 0000 0000 0000 0000 0000  @...............
00000030: 0000 0000 4000 3800 0200 4000 0000 0000  ....@.8...@.....
00000040: 0600 0000 0400 0000 4000 0000 0000 0000  ........@.......
00000050: 4000 4000 0000 0000 4000 4000 0000 0000  @.@.....@.@.....
00000060: 7000 0000 0000 0000 7000 0000 0000 0000  p.......p.......
00000070: 8000 0000 0000 0000 0100 0000 0500 0000  ................
00000080: 0000 0000 0000 0000 0000 4000 0000 0000  ..........@.....
00000090: 0000 4000 0000 0000 bc00 0000 0000 0000  ..@.............
000000a0: bc00 0000 0000 0000 0000 2000 0000 0000  .......... .....
000000b0: b83c 0000 00bf 0000 0000 0f05            .<..........
```

思ったより小さいですね。

`xxd` で出力しているので、 `xxd -r` でバイナリに逆変換できます。

```bash
$ xxd -r <<EOF > elf

data

EOF
$ chmod +x elf
$ ./elf
```

さて、これだけだと分かりずらいのでもうちょっと分解してみましょう。
ELFヘッダ、プログラムヘッダ2つ、実際のプログラムが入っています。

```text
         +-----------------------------------------+
00000000:| 7f45 4c46 0201 0100 0000 0000 0000 0000 |
00000010:| 0200 3e00 0100 0000 b000 4000 0000 0000 |  ELFヘッダ
00000020:| 4000 0000 0000 0000 0000 0000 0000 0000 |
00000030:| 0000 0000 4000 3800 0200 4000 0000 0000 |
         +-----------------------------------------+
         +-----------------------------------------+
00000040:| 0600 0000 0400 0000 4000 0000 0000 0000 |
00000050:| 4000 4000 0000 0000 4000 4000 0000 0000 | プログラムヘッダ1
00000060:| 7000 0000 0000 0000 7000 0000 0000 0000 |
00000070:| 8000 0000 0000 0000 +-------------------+
         +---------------------+
                             +---------------------+
         +-------------------+ 0100 0000 0500 0000 |
00000080:| 0000 0000 0000 0000 0000 4000 0000 0000 | プログラムヘッダ 2
00000090:| 0000 4000 0000 0000 bc00 0000 0000 0000 |
000000a0:| bc00 0000 0000 0000 0000 2000 0000 0000 |
         +-----------------------------------------+
         +-----------------------------------------+
000000b0:| b83c 0000 00bf 0000 0000 0f05           | プログラム
         +-----------------------------------------+
```

これをそれぞれ見ていきます。

[Wikipedia](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format)も併読書としてご利用下さい。


## ELFヘッダ

ほとんど固定値が入ります。全部で0x40バイトあります。

意味を持つデータ単位で区切るとこんな感じです。

``` text
           1                   2    3    4    5    6    7
         +-------------------+----+----+----+----+----+----------------------------------+
00000000:| 7f   45   4c   46 | 02 | 01 | 01 | 00 | 00 | 00   00   00   00   00   00   00 |
         +-------------------+----+----+----+----+----+----------------------------------+
           8         9         10                  11
         +---------+---------+-------------------+---------------------------------------+
00000010:| 02   00 | 3e   00 | 01   00   00   00 | b0   00   40   00   00   00   00   00 |
         +---------+---------+-------------------+---------------------------------------+
           12                                      13
         +---------------------------------------+---------------------------------------+
00000020:| 40   00   00   00   00   00   00   00 | 00   00   00   00   00   00   00   00 |
         +---------------------------------------+---------------------------------------+
           14                  15        16        17        18        19        20
         +-------------------+---------+---------+---------+---------+---------+---------+
00000030:| 00   00   00   00 | 40   00 | 38   00 | 02   00 | 40   00 | 00   00 | 00   00 |
         +-------------------+---------+---------+---------+---------+---------+---------+
```

それぞれの番号で説明していきます。

1. マジックナンバー。ELFファイルであることを表わす。7f 45 4c 46固定。
2. クラス。32bit = 0x01、64bit = 0x02。ここでは64向けなので0x02。
3. エンディアン。little endian = 0x01、big endian = 0x02。ここではlittle endianで0x01。以降のマルチバイトの数値はリトルエンディアンで解釈される。
4. バージョン。0x01。
5. OS ABI。どれが何に対応するかはWikipediaの通りだが、多くの場合で0x00（要出典）らしいので0x00にする。
6. ABIの細かなバージョン。LinuxかつOS ABIに0x00を指定していれば無視されるらしい。
7. 空き領域。
8. オブジェクトのタイプ。今回はシンプルな実行可能ファイルなので0x02。libcをダイナミックリンクしたりするなら0x03かな？
9. マシンタイプ。amd64なので0x3e。
10. バージョン。0x01。バージョンが登場するの2回目な気がするんですけどなんででしょうね。
11. プログラムのエントリポイント。ここから実行がはじまる。プログラムヘッダでどこにロードするかに依るが、今回は0x4000b0（後述）
12. ファイル内でのプログラムヘッダの位置。ELFヘッダの直後に始まっているので0x40。
13. ファイル内でのセクションヘッダの位置。今回はセクションヘッダはないので0x00。
14. フラグ。ターゲットアーキテクチャ依存でなんか読まれるらしい。よくわからないので0x00。
15. このヘッダのサイズ。0x40。
16. プログラムヘッダのサイズ。64bitなので0x38。
17. プログラムヘッダの数。今回は0x02。
18. セクションヘッダのサイズ。64bitなので0x40。
19. セクションヘッダの数。今回は0x00。
20. セクション名を管理するセクションヘッダのセクションヘッダテーブル内のインデックス。今回は存在しないので0x00を入れといた。

11のエントリポイントについて補足します。今回はこのあとに続くプログラムヘッダでELFファイル全体を0x400000にロードしています。そしてELFファイル内でプログラムは0xb0から始まっているので0x400000に0xb0を足して0x4000b0という訳です。

これらの値は `readelf -h` で確認できます。

``` text
$ readelf -h elf
ELF ヘッダ:
  マジック:   7f 45 4c 46 02 01 01 00 00 00 00 00 00 00 00 00
  クラス:                            ELF64
  データ:                            2 の補数、リトルエンディアン
  Version:                           1 (current)
  OS/ABI:                            UNIX - System V
  ABI バージョン:                    0
  型:                                EXEC (実行可能ファイル)
  マシン:                            Advanced Micro Devices X86-64
  バージョン:                        0x1
  エントリポイントアドレス:               0x4000b0
  プログラムヘッダ始点:          64 (バイト)
  セクションヘッダ始点:          0 (バイト)
  フラグ:                            0x0
  Size of this header:               64 (bytes)
  Size of program headers:           56 (bytes)
  Number of program headers:         2
  Size of section headers:           64 (bytes)
  Number of section headers:         0
  Section header string table index: 0
```

## プログラムヘッダ1

続いてプログラムヘッダ1です。これはプログラムヘッダのある領域のメタデータです。
ぶっちゃけ、このプログラムヘッダが必要なのかよく分かってないのですが、ないと動かなかったので置いています。
詳しい方、どうしてか教えて下さい。


``` text
           1                   2                   3
         +-------------------+-------------------+---------------------------------------+
00000040:| 06   00   00   00 | 04   00   00   00 | 40   00   00   00   00   00   00   00 |
         +-------------------+-------------------+---------------------------------------+
           4                                       5
         +---------------------------------------+---------------------------------------+
00000050:| 40   00   40   00   00   00   00   00 | 40   00   40   00   00   00   00   00 |
         +---------------------------------------+---------------------------------------+
           6                                       7
         +---------------------------------------+---------------------------------------+
00000060:| 70   00   00   00   00   00   00   00 | 70   00   00   00   00   00   00   00 |
         +---------------------------------------+---------------------------------------+
           8
         +---------------------------------------+
00000070:| 80   00   00   00   00   00   00   00 |
         +---------------------------------------+
```

プログラムヘッダは構成がおおぶりで解説が楽ですね。

1. プログラムヘッダのタイプ。プログラムヘッダそのものの領域を指示する(PT_PHDR)ので0x00000006。
2. セグメントに依存したフラグ。Linuxのヘッダファイルを読む限り0x01 = executable、0x02 = writable、0x04 = readableで今回はread onlyの指定で0x04。
3. ファイル内でのセグメントのオフセット。プログラムヘッダはELFヘッダの直後（0x40）から始まってるので0x40。
4. メモリ内での仮想アドレス。セマンティクスがよく分からないが、恐らく「メモリ内のどこに存在するか」。ELFファイルを0x400000にロードして、プログラムヘッダは0x40から開始するので0x400040。
5. メモリ内での物理アドレス。物理アドレスが関係するOS向けらしく、Linuxには関係ない。他のELFを見たら仮想アドレスと同じ値を設定していたのでそれに倣った。
6. セグメントのファイル内のサイズ。プログラムヘッダが2つあるので 0x38 + 0x38 = 0x70。
7. セグメントのメモリ内のサイズ。ファイル内のサイズと同じく0x70。
8. アラインメント。2の羃な値を指定する。どの値が適当か分からなかったので適当に0x80を入れた。0x08を指定してしているファイルもあったしもっと小さくてもよさそう。

## プログラムヘッダ2

1と同様に2も解説していきます。2は、プログラムをメモリにロードする方法を指示します。

``` text
                                                   1                   2
                                                 +-------------------+-------------------+
                                                 | 01   00   00   00 | 05   00   00   00 |
                                                 +-------------------+-------------------+
           3                                       4
         +---------------------------------------+---------------------------------------+
00000080:| 00   00   00   00   00   00   00   00 | 00   00   40   00   00   00   00   00 |
         +---------------------------------------+---------------------------------------+
           5                                       6
         +---------------------------------------+---------------------------------------+
00000090:| 00   00   40   00   00   00   00   00 | fe   00   00   00   00   00   00   00 |
         +---------------------------------------+---------------------------------------+
           7                                       8
         +---------------------------------------+---------------------------------------+
000000a0:| fe   00   00   00   00   00   00   00 | 00   00   20   00   00   00   00   00 |
         +---------------------------------------+---------------------------------------+
```

バイト数や構成は1と変わらないですね。

1. プログラムヘッダのタイプ。ロードする領域を指示する(PT_LOAD)ので0x00000001。
2. セグメントに依存したフラグ。Linuxのヘッダファイルを読む限り0x01 = executable、0x02 = writable、0x04 = readableで今回はread + executableの指定で0x05。
3. ファイル内でのセグメントのオフセット。ファイルの先頭から読むので0x00。
4. メモリ内での仮想アドレス。セマンティクスがよく分からないが、恐らく「メモリ内のどこにロードするか」。
5. メモリ内での物理アドレス。物理アドレスが関係するOS向けらしく、Linuxには関係ない。他のELFを見たら仮想アドレスと同じ値を設定していたのでそれに倣った。
6. セグメントのファイル内のサイズ。ファイルサイズに等しく、今回は0xbc。
7. セグメントのメモリ内のサイズ。ファイル内のサイズと同じく0xbc。
8. アラインメント。2の羃な値を指定する。他のELFファイルに倣って0x200000を入れた。


プログラムヘッダは `readelf -l` で読み取ることができます。

``` text
$ readelf -l

Elf ファイルタイプは EXEC (実行可能ファイル) です
エントリポイント 0x4000b0
There are 2 program headers, starting at offset 64

プログラムヘッダ:
  タイプ        オフセット          仮想Addr           物理Addr
                 ファイルサイズ        メモリサイズ         フラグ 整列
  PHDR           0x0000000000000040 0x0000000000400040 0x0000000000400040
                 0x0000000000000070 0x0000000000000070  R      0x80
  LOAD           0x0000000000000000 0x0000000000400000 0x0000000000400000
                 0x00000000000000bc 0x00000000000000bc  R E    0x200000
```

libcをダイナミックリンクする際はタイプに `PT_DYNAMIC` や `PT_INTERP` を指定するプログラムヘッダが入るっぽいです。


## プログラム

ただのアセンブラなんですが、解説しますね。
手作りのELFファイルでは通常Cを書くときに使う `main` のような高級なものはありません。
そういうのはcrt1.oというファイルに既述されており、Cのファイルをコンパイルするときに暗黙的にリンクされます。これをリンクしていないので開始から終了まで全て自分の手書きです。

つまり、プログラムを終了させるのも自分で書かなければなりません。それをアセンブラで書いたのがこれです。
`exit(0)` 相当のアセンブラです。

``` text
           mov rax, 60              mov rdx, 0               syscall
         +------------------------+------------------------+---------+
000000b0:| b8   3c   00   00   00 | bf   00   00   00   00 | 0f   05 |
         +------------------------+------------------------+---------+
```


ということでこのファイルを実行すると「無事」exit status 0で終了します。

# まとめ

ひとまず、最小限に動くELFファイルの全てのバイトを解説しました。
まだよく分かってないところもあったり、セクションヘッダを扱ってなかったりするので不十分なところもあるでしょうが、何かのとっかかりになれば幸いです。
