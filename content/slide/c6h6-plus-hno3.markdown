---
type: slide
title: "C6H6+HNO3"
date: 2014-10-24
aliases:
    - /slide/c6h6-plus-hno3.html
categories: [Lisp, picrin]
description: "関数型なんたらの集いでの内容です。<br>
picrinの紹介とnitro、今開発中のsulfuricについてです。<br>
"
---
<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# $\mathbf{C_6H_6+HNO_3}$
------------------------
関数型なんたらの集い <2014-10-25>  
κeen(@blackenedgold)

# picrinについて
---------------
![ピクリン酸の構造式](/images/picrin/picrin-structure.png)  
化学式 $\mathbf{C_6H_3N_3O_7}$、示性式 $\mathbf{C_6H_2(OH)(NO_2)_3}$ で表される芳香族のニトロ化合物。  
一般にはフェノールのニトロ化によって得られる。

Note:
ニトロ化に硫酸を使うことを触れる。

![picrinのロゴ](/images/picrin/picrin-logo-fin01-02.png)  

# About Me
----------

 + κeen
 + 東大数学科の4年生
 + ソーシャルアカウントは上のアイコン達から。
 + Lisp, Ruby, ML, Shell Scriptあたりを書きます
 + [picrin](https://github.com/picrin-scheme/picrin)のコミッタです


# picrinについて
----------------

* github: picrin-scheme/picrin
* R7RS-small scheme 処理系
* 速い、軽い、高機能を目指す
* 組み込み向けを意識
* 1年くらい開発
* κeenがコミットし始めてから半年弱


# picrinの プロジェクト構成
---------------------------
コア(benz)はC、ビルドツールはCMake。

```
├── contrib (nitros)
   .......
├── docs
├── etc
├── extlib
│   └── benz
├── piclib
   ....
└── src
```

Note:
contribとextlib/benzを強調。
srcはほぼ空であることにも触れる。

# nitrosの紹介
---------------

* schemeやCで書かれている
* picrinのコンパイル時に一緒にlibpicrinに入る
* picrinと共にCMakeで管理されている

Note:
schemeファイルもlibpicrinにCの文字列として入ってることに触れる。
バイトコンパイルプロジェクトにも触れる。
nitrosも一緒にコンパイルされることを強調する。


# nitroの紹介
---------------
## [R7RS](https://github.com/picrin-scheme/picrin/tree/master/contrib/05.r7rs/scheme)

* picrinではr7rsもただのライブラリ
* 組み込みのときに必要なければ外せばよい

Note:
リンクを辿る必要はない。
スライドは下に続く。


# nitroの紹介
---------------
## [partcont](https://github.com/picrin-scheme/picrin/blob/master/contrib/10.partcont/piclib/partcont.scm)

* ただの限定継続ライブラリ
* ねこはるさんが詳しく話すと思うのでそちらに譲ります

Note:
リンクを辿ってソースを見せる。

# nitroの紹介
---------------
## [for](https://github.com/picrin-scheme/picrin/blob/master/contrib/20.for/piclib/for.scm)

* 非決定性計算ライブラリ
* またの名をListモナド

Note:
リンクを辿ってソースを見せる。

# nitroの紹介
---------------
## [regexp](https://github.com/picrin-scheme/picrin/blob/master/contrib/10.regexp/src/regexp.c)

* Unixのregexライブラリのバインディング
* Cで書かれている

Note:
リンクを辿ってソースを見せる。
詳細に説明する。

 + `regexp_t`型
 + dtor
 + GCに乗ることは強調


# nitroの依存関係の話
---------------------

* nitro同士に依存関係がある
* forはpartcontに依存する
* REPLはeditlineに依存する
* etc...


# picrinの依存関係解決法
-----------------------

[picrin/contrib](https://github.com/picrin-scheme/picrin/tree/master/contrib)

Note:

* リンクを辿る
* ディレクトリ名が数字で始まることを説明
* 03まできてそろそろ限界であることを説明


どう見ても<span style="font-size:150%">手動(ディレクトリ名)ソート</span>ですね。  
本当にありがとうございました。


* さすがにどうにかしたい
* 依存関係解決ツールが欲しい
* なんかmrubyのmrbgemとかみたいにしたら楽しいんじゃね？
* 個々のnitroを別プロジェクトにも出来る!


# sulfuricプロジェクト

# sulfuricプロジェクト
---------------------

* sulfuric acid = 硫酸
* nitro定義、依存解決、ビルドコンフィグ etc...
* 最終的にはCMakeと連携
* 定義ファイルはschemeで書きたい

Note:
個人案なことに触れる。
chikenのeggsやRacketのPlanetにも触れる。


# sulfuricプロジェクト
---------------------

* あれ？R7RSってnitroじゃね？
* そもそもschemeをビルドするためにschemeを書く…？
* てかCMakeとの連携どうするよ

Note:
進捗はまだinitial commitすら出来てない。
書いてる内に大きくなっていったことも説明。
最初は他のschemeでビルドする案やフェノールでビルドする案も話す。
solutionディレクトリに入れたいよねーも話す。
フェノールの合成にベンゼンスルホン酸が使われる


# picrinにコミットしよう
-----------------------

* picrinはまだ若い
  + ちょろっと覗くだけでコミット出来るところが見付かる
* 開発者全員日本人


# コミットはとっても簡単
------------

1. [issue](https://github.com/picrin-scheme/picrin/issues)を覗く
2. 簡単そうなのをサクっと実装
3. PR

Note:
イシューの中から

  + \#224 feature request: data structures
  + \#210 Add MQTT support

を見せる


# Q. なぜpicrin?
------------

# A. picrinの独自拡張
-----------------

* [ドキュメント](http://picrin.readthedocs.org/en/latest/)に色々書いてある
* C拡張の書き方も書いてある

Note:
arrayとdictionaryに触れる。
マクロに触れる
C APIの項目があることにも触れる。


# picrinのマクロ
----------------

* `syntax-rules` (R7RS)
* `sc-macro-transformer`
* `rsc-macro-transformer`
* `ir-macro-transformer`
* `er-macro-transformer`
* `define-macro` (Common Lisp)

参考: [様々な Hygienic Macro - 月の塵](http://d.hatena.ne.jp/leque/20080528/p1)


# picrinにコミットしよう
-----------------------

* issueに上がってなくても大歓迎
* Schemeの練習に
* 「便利なライブラリ書いたけどどこ置いたら良いの」



# picrinにコミットしよう


<span style="font-size:600%">以上</span>  
何か質問あればどうぞ
</script>
</section>
