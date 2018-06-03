---
categories: [Haskell]
date: 2018-06-03T12:08:45+09:00
title: "haskell stackのインストールが難しかった"
---
詳しい経緯は省きますがHaskellを書こうとしたらghc-modが新しいマシンに入ってなく、新規に入れようとしたら躓いたので解決方法をネットの海に投げておきます。

cabalを使うと解決しそうな気がしますがdependency hellはもう懲り懲りなのでstackしか使わない縛りでやってます。
<!--more-->

大本は[これ](https://github.com/DanielG/ghc-mod/pull/922#issuecomment-353896120)なのですが[このパッチ](https://github.com/DanielG/ghc-mod/pull/927)の影響でさらにで1行だけ変更が入ります。

まず、コマンド一発でインストールとかはあきらめましょう。ソースコードをビルドします。しかしそれですら難易度が高いのでここで解説してます。


## 1. ghc-modの`ghc802`ブランチを用意

```console
$ git clone git@github.com:ariskou/ghc-mod.git -b ghc802
```

## 2. cabal-helperをチェックアウト

えーcabalーと思いましたがライブラリとして使ってるみたいなので問題なさそう？

```console
$ git clone https://github.com/DanielG/cabal-helper.git
```

## 3. ghc-modのディレクトリに移動

```console
$ cd ghc-mod
```

## 4. `stack.yaml`を作成
内容はこれ。

```
resolver: lts-10.0

packages:
- location: .
- location: ./core # <- この行が元のコメントとの差分
- location: ../cabal-helper
  extra-dep: true

extra-deps: 
- extra-1.5.3
- monad-journal-0.7.2
- optparse-applicative-0.13.2.0
- unix-compat-0.4.3.1
- either-4.4.1.1


flags: {}

extra-package-dbs: []
```

## 5. インストール

```console
$ stack install
```

これで完了。因みにstackのバージョンは1.7.1です。
