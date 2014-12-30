---
type: post
title: "require, ASDF, quicklispを正しく使う"
date: 2014-11-30
comments: true
categories: [Lisp, Common Lisp,  Quicklisp, ASDF]
---
κeenです。最近のCommon Lispのパッケージ管理は`ql:quickload`しか知らないという方も多いのではないでしょうか。しかしそれだけでは機能が足りないこともあります。Common Lispには様々な管理システムがあるので整理しましょう。
<!--more-->

# `provide`, `require`
同じファイルを読み込まないための原始的なシステムです。Common Lispの標準の機能です。`(require 'foo)`がファイルをロードし、ロードされたファイル内で`(provide 'foo)`しておくと2回目以降の`(require 'foo')`はファイルを読まずにすぐさま返ります。

ここで問題なのが`require`がどこのファイルを捜しにいくかは処理系依存なところですね。なので生の`require`は使えないと思っておいた方が良いでしょう。

# ASDF 3
Another System Definition Facility。過去にはAnotherじゃないSystem Definition Facilityもあった模様。最新版は3系です。結構APIが変っているので必ず3の情報を捜しましょう。

この「System」というのが聞き慣れませんが、ASDFの`defsystem`で

1. systemに含まれるファイルの定義
2. 依存systemの記述
3. その他作者、ライセンス、バージョンなどの記述
4. systemのコンパイルやロード、テストなどの操作

が可能です。ASDFはCにとってのmake + ldを標榜しています。Makefile的なものは(systemname).asdになります。

`require`との統合もされており、ASDFを適切に設定することで`defsystem`されたsystemを`require`でロードすることが出来ます。このときASDFは処理系に依存せずにASDFのパスに従って.asdファイルを捜しにいきます。

また、ほとんどの処理系はASDFを標準添付していて、大抵`(require 'asdf)`すれば使えるようになっています。

尚、コンパイル後のファイルは処理系、バージョン毎に互換性がないので本来なら適切に管理する必要がありますが、ASDFが適切に~/.cache/common-lisp以下に管理してくれます。優秀ですね。
# quicklisp
lispプロジェクトのインストーラと微妙にパッケージマネージャ的な役割をします。

http://beta.quicklisp.org/quicklisp.lisp をダウンロードしてきてそのファイルを読み込んで`(quicklisp-quickstart:install)`すれば~/quicklisp/以下にquicklispがインストールされ、ロードされます。処理系を再起動したあとまたquicklispをロードするには~/quicklisp/setup.lispを`load`します。が、毎回それをやるのが面倒な人は`(ql:add-to-init-file)`しておけば処理系の初期化ファイルに~/quicklisp/setup.lispを読み込む処理が書き加えられます。

さて、このquicklispを扱う上で3つの概念を覚えておくと良いです。

dist
:    releaseの配布元。普通は'quicklisp'のみだがほかのdistを使うことも可能。自前のやつとか。新たなdistを追加したり削除したり出来る。gitのremoteをイメージすればよい。

release
:    quicklispがダウンロードしてくる単位。ライブラリの作者が登録するときの単位。複数のsystemをもつこともある。

system
:    ASDFのsystem。ユーザーが使うときの単位。

さて、quicklispを使うときは通常使いたいシステムを`ql:quickload`すればそのシステムと依存システムの含まれているリリースをダウンロード、展開、ロードまでしてくれます。

開発中はREPLを立ち上げているので「あ、このライブライリ使おう」と思ったら`ql:quickload`するだけで即座に使えるようになりますね。

quicklispのリリースは毎月下旬にアップデートされていて、毎月ちゃんと全てのライブラリが対応処理系で動くことがテストされています。なのでメンテナが居なくなったライブラリはドロップします。代わりにquicklispの作者がテスト出来ないライブラリは登録させてもらえなかったりします（cl-cudaとか）

# quicklispの問題
既にCommon Lispのライブラリマネージャとしてデファクトスタンダード的位置を築きつつあるquicklispですが、私は結構不満があります。

* ~/quicklisp/setup.lispを読み込むとquicklispの全てを読み込んでしまう: これには過去全てのバージョンのdistとreleaseとsystemのデータベース、HTTPクライアント、圧縮ファイルの解凍ライブラリなども含まれており、処理系の起動が非常に遅くなる
* 特にデータベースはファイルに書かれていても高速に読める形式なのに初期化時に毎回全ての内容をLispのハッシュに変換するという愚行をする。
* `(ql:add-to-init-file)`が推奨されているが、前述の通り処理系の起動が遅くなってしまう。特に、（インストーラではなく、インストール済みの）ライブラリマネージャとして使いたくてもインストーラの機能までロードしてしまう。結構メモリを食うしアプリケーションには要らない。
* 処理系に依っては初期化ファイルはlispファイルをスクリプトとして使うときは読み込まれないこともあるのでスクリプトには使えない。まあ、前述の通り使いたくもない。

のでライブラリマネージャにはASDFが向いてるのですが、

* quicklispでインストールしたシステムはquicklispを一旦ロードするかASDFの設定をいじるかしなければASDFからは使えない
* quicklispはシステムのダウンロード/インストールだけすれば良いものをロードまでする
* 要はASDFを隠す
* じゃASDFのラッパーかというとロードしかせず、コンパイルやテストなどはしない

など様々な問題があります。

一応quicklispの弁護をしておくと、Common Lispは他のスクリプト言語とはちょっと使い方が違って、

* 初期化ファイルは大抵オレオレライブラリで埋め尽くされていてそもそも起動には時間がかかる
* Emacsのように一度REPLを立ち上げたら滅多に落とさない
* アプリケーションも基本的にはREPLの中で使う
* 起動時間を気にするなら初期化ファイルを全てロードした、コアファイルをダンプしておいて使う

といった使い方をされることが多いのです。そのような人からしてみたら細々処理されるよりも起動時間とメモリは気にしないから速いやつをくれ、となるのです。

じゃあそれなりにCommon Lispを使う私が何故そうしないかというと

* 立ち上げっぱなしというのが性に合わない。立ち上がっているものを見るとすぐに落としたくなる。EmacsのヘビーユーザーだがEmacsもすぐ落とす。
* コマンドラインアプリケーションを作りたいため、起動時間が命になる。
* 私はSBCLの開発版を使っていて、コアファイルはすぐに無効になるためコアダンプはしない（意味がない）

といった理由があります。これは初心者にも共通する部分があるんじゃないでしょうか。REPLを立ち上げっぱなしにはしないし「コアダンプ？なにそれ？エラー出してんじゃん」状態でしょう。

# ASDFを正しく使う
ということでquicklispをあまり使わない方法を紹介します。

前述の通り、quicklispでインストールしたシステムはquicklispを一旦ロードするかasdfの設定をいじるかしなければasdfからは使えません。具体的に言うと、

* ASDFのデフォルトパスは~/common-lisp以下
* quicklispのシステムのインストールパスは~/quicklisp/dists/(dist name)/software/以下

なので~/quicklisp/dists/以下にパスを通します。少し無駄なディレクトリもパスに含まれてしまうので気にする人はそれぞれのdistについて~/quicklisp/dists/(dist name)/software/にパスを通しましょう。

因みに拙作の[CIM](https://github.com/KeenS/CIM)は~/.cim/quicklisp以下にquicklispをインストールするのでCIM使いの方は~/.cim/quicklisp/dists/ですね。~/quicklispの方が混乱少ないかなあ。

さて、ASDFの設定は[公式マニュアル](http://common-lisp.net/project/asdf/asdf.html#Configuring-ASDF-to-find-your-systems)を参照すれば良いのですが一応実際のものを書いておくと

~/.config/common-lisp/source-registry.conf に

```lisp
(:source-registry
  (:tree "~/quicklisp/dists/")
  (:tree (:home "Lisp/"))
  :INHERIT-CONFIGURATION)
```

を書けばOKです。2つめのLisp/は私がCommon Lispファイルを置いているディレクトリですね。この書き方だとquicklispより自分のLispライブラリを優先して読みにいきます。開発版を使いたいときとかむしろ開発をするときとかに必須です。この設定をミスるといくら開発してもロードされてるのはリリース版で、機能が動かないとかの地獄を見ます。

ここまでくれば

```lisp
(require 'asdf)
(require 'hoge)
```

とすればquicklispでインストールしたライブラリをロード出来ます。

## require以外のASDFの使い方
バージョンに依って使い方が異なるのですが、3系だと`(asdf:xxx-system 'hoge)`などとします。具体的には

* `(asdf:load-system 'hoge)` ( = `(require 'hoge)`)
* `(asdf:test-system 'hoge)` (テストがあるときのみ)
* `(asdf:compile-system 'hoge)`

を覚えておけば良いでしょう。他にもいくつか操作があるようですが私は使ったことがないです。`asdf:locate-system`は名前的に便利そうではあるんですけどね。

開発するときはまず.asdファイルを書いておいて(あるいはcl-projectから生成して)、`(require 'hoge)`で始めて書いていき、ある程度進んだらテストを書いてテストの項目を.asdファイルに書き足して`(asdf:test-system 'hoge)`、それなりに動くようになったら`(asdf:compile-system 'hoge)`してコンパイルの様子を見たりコンパイル後のベンチマークを取ったりします。

## quickloadを成仏させる
これでロードの処理はASDFで全てカバー出来るようになりました。じゃあquickloadのロード機能は邪魔ですね。quicklispをインストーラとしてのみ使いましょう。

まず処理系の初期化ファイル(sbclなら~/.sbclrc)から

```lisp
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "~quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init :verbose nil)))

```

を削除します。quicklispを使いたいときだけ`(load "~/quicklisp/setup.lisp")`します。そして欲しいシステムをインストールするには

```lisp
(ql::recursively-install "foo")
```

します。quickloadに比べて爆速です。ここにきて名前が文字列な上にexportされてないシンポル参照してますね。気持ち悪いという方は大人しくquickloadしましょう。

拙作のCIMにはこれをコマンドラインから行なう`ql install`なるコマンドが存在します。

# 最後に
quicklispが嫌いなのは私の好みですがASDFを正しく使えて損はないと思うので知らなかった方々は是非試してみて下さい。
