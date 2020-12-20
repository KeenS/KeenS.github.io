---
categories: [Idris, GitHub, GitLab, Idris Advent Calendar, Advent Calendar 2020, Advent Calendar]
date: 2020-12-20T16:45:01+09:00
title: "IdrisのテストとCI"
---
このエントリは[Idris Advent Calendar 2020](https://qiita.com/advent-calendar/2020/idris)の16日目の記事です。

κeenです。今回はipkgを使ったIdrisのテストとCIを紹介していきます。CIはGitLab CIとGitHub Actions両方の設定を紹介します。

<!--more-->

# Idrisのテストフレームワーク

フレームワークというほど大層なものではないですが、テストができる仕組みがあります。
contribに `Test.Unit` というモジュールがあり [^1]、それらを使ってテストが書けるのです。

[^1]: [contribのdoc](https://www.idris-lang.org/docs/current/contrib_doc/)には載っていません。更新が追い付いてないようです。

テストの本体となるのは `genericTest` です。

```text
*Test/Unit> :doc genericTest
Test.Unit.Generic.genericTest : Show a => (title : Maybe String) -> (given : a) -> (expected : a) -> (tFunc : a -> a -> Bool) -> IO Bool
    Run a generic test.
    Arguments:
        title : Maybe String  -- Optional Test title

        given : a  -- The given string to parse

        expected : a  -- The expected result

        tFunc : a -> a -> Bool  -- The testing function to compare the results.

    The function is: Total & export
```

とはいえ、これだけだと使いづらいので `assertXxx` などの便利関数も用意されています。

```idris
assertEquals : Eq a => Show a => a -> a -> IO Bool
assertFalse : Bool -> IO Bool
assertJust : Show a => Maybe a -> IO Bool
assertLeft : Show a => Show b => Either a b -> IO Bool
assertNotEquals : Eq a => Show a => a -> a -> IO Bool
assertNothing : Show a => Maybe a -> IO Bool
assertRight : Show a => Show b => Either a b -> IO Bool
assertTrue : Bool -> IO Bool
```

例えば `assertTrue` であれば以下のように動作します。

``` text
*Test/Unit> :exec assertTrue True
Test: Assert True
True
*Test/Unit> :exec assertTrue False
Test: Assert True
++++++++++++++++++++++++++++++++++++++++
An error occured:
  Given:
    False
  Expected:
    True
++++++++++++++++++++++++++++++++++++++++
False
```

ちょっと手札が少ないですがこれでどうにかテストは書けそうです。

# ipkgとテスト

ipkgにもテストのサポートがあります。
`tests =` に `IO ()` の値を書き連ねておけば、 `idris --testpkg IPKG` でテストしてくれます。

例えばipkgに以下を書いたとします。

``` text
tests = Tests.Hoge.test
      , Tests.Fuga.test
```


すると裏で以下のようなファイルを生成、実行します。

``` idris
module Test_______

-- 必要ならばここにimportが並ぶ

namespace Main
  main : IO ()
  main = do Tests.Hoge.test
            Tests.Fuga.test
```

このプログラムを実行した終了ステータスでそのまま `idris --testpkg` も終了します。


# テストを書く

先日のanagramにテストを書いてみましょう。

anagramパッケージは以下のようなディレクトリ構成なのでした。

``` text
$ tree
.
├── anagram.ipkg
└── src
    ├── Anagram.idr
    └── AnagramMain.idr
```

そして `anagram.ipkg` は以下のような内容なのでした。

``` text
package anagram

version = "0.1.0"
author = Your name

sourcedir = src
modules = Anagram
        , AnagramMain
main = AnagramMain
executable = anagram
pkgs = contrib
```

これにテストを加えてみましょう。

## 失敗するテスト

まずは失敗するテストを書いてみます。

anagram.ipkgを以下のように書き換えます。

``` text
package anagram

version = "0.1.0"
author = Your name

sourcedir = src
modules = Anagram
        , AnagramMain
        , Tests.Anagram

main = AnagramMain
executable = anagram

tests = Tests.Anagram.test

pkgs = contrib
```

`modules` に `Tests.Anagram` が増えたのと、 `tests = Tests.Anagram.test` の行が増えました。

これに対応して `src/Tests/Anagram.idr` に以下の内容を書きます。

``` idris
module Tests.Anagram

import Test.Unit
import Anagram

export
test : IO ()
test = runTests [ assertTrue False ]
```

ここで、 `runTests` は以下のような関数です。

``` text
*Test/Unit> :doc runTests
Test.Unit.Runners.NonReporting.runTests : List (IO Bool) -> IO ()
    Run the given set of tests, but don't return the results.
```


これを走らせてみましょう。 `idris --testpkg` です。

``` text
$ idris --testpkg anagram.ipkg
Entering directory `./src'
Type checking /tmp/idris192645-0.idr
Test: Assert True
++++++++++++++++++++++++++++++++++++++++
An error occured:
  Given:
    False
  Expected:
    True
++++++++++++++++++++++++++++++++++++++++
All Tests have been performed.
=========================
$ echo $?
0
```

期待通りテストが失敗しましたね。

しかし終了ステータスが0です。
実装を読んだところ、どうやら `runTests` は終了ステータスには無関心のようです。
人間が目で確認する分にはこれでもいいのですが、今回はCIでテストをしたいので失敗したら終了ステータスも0以外になってほしいです。ちょっとだけ工夫しましょう。

### 失敗するテストを失敗させる

実は `runTests` にはもう1つの（オーバーロードされた）実装があります。

``` text
*Test/Unit> :doc runTests
Test.Unit.Runners.Reporting.runTests : List (IO Bool) -> IO (List Bool)
    Run the given set of tests and return the results.
```


こちらは返り値が `IO (List Bool)` とテストの成否を返すようになっています。
これを利用して1つでも失敗したテストがあれば異常終了するようにしてみましょう。

まずはこのようなヘルパ関数を用意します。

``` idris
-- exitをインポートする
import System

exitIfFail : IO (List Bool) -> IO ()
exitIfFail action = do
  results <- action
  if not (all id results)
  then do
    putStrLn "Some tests failed"
    exit 1
  else pure ()
```

`test` 側もこれを使うようにしましょう。

``` idris
export
test : IO ()
test = exitIfFail $ runTests [ assertTrue False ]
```


再度これで走らせてみます。


``` text
$ idris --testpkg anagram.ipkg
Entering directory `./src'
Type checking ./Tests/Anagram.idr
Type checking /tmp/idris193751-0.idr
Test: Assert True
++++++++++++++++++++++++++++++++++++++++
An error occured:
  Given:
    False
  Expected:
    True
++++++++++++++++++++++++++++++++++++++++
All tests have been performed.
========================================
Some tests failed
Leaving directory `./src'
$ echo $?
1
```

今度はちゃんと終了ステータスが1になりましたね。
それではこれを使ってテストを書いていきます。

## anagramのテスト

準備が整ったのでテストを書いていきましょう。こんな感じになるんじゃないでしょうか。


``` idris
import Data.SortedSet

testEmptyQuery : IO Bool
testEmptyQuery = do
  let result = query emptyDB "hoge"
  assertTrue $ contains "hoge" result

testRegisterQuery : IO Bool
testRegisterQuery = do
  let db = register emptyDB "eat"
  let result = query db "tea"
  assertTrue $ contains "tea" result

export
test : IO ()
test = exitIfFail $ runTests [
  testEmptyQuery,
  testRegisterQuery
]
```

ベストプラクティスがある訳ではないので難しいんですが、ipkgの `tests` に書くのは1ファイル1テストにして1ファイル内のテストは `runTests` で1まとめにすることにします。

テストを走らせてみましょう。

``` text
$ idris --testpkg anagram.ipkg
Entering directory `./src'
Type checking ./Tests/Anagram.idr
Type checking /tmp/idris195171-0.idr
Test: Assert True
Test: Assert True
All tests have been performed.
========================================
Leaving directory `./src'
```

無事通っているようですね。

今回は `assertTrue` しか使ってませんが、様々なテストするにあたって `assertXxx` が色々ほしくなるはずです。そういうときは `genericTest` を使って `assertXxx` を自作しましょう。

ひとまずテストが書けたということでCIを設定していきます。

# CI

CIでテストを走らせつつmasterにpushするときはIdrisdocで生成したドキュメントを更新するようにしましょう。

多様性に配慮してGitLab CIとGitHub Actionsの両方を紹介します。
設定の簡潔さではGitLab CIの方が勝るので個人的にはGitLab CIをおすすめします。

## Gitリポジトリの準備

どちらのCIを使うにせよ、まずはGitリポジトリとしての準備をしましょう。

こんな感じで初期化します。

``` text
$ git init .
$ cat <<EOF > .gitignore
*.ibc
anagram
EOF
$ git add .
$ git commit -m'inital commit'
```

## 余談: CIで使えるIdrisのDockerイメージ

GitLab CIでもGitHubActionsでもIdrisコンパイラが入ったDockerイメージを使うことになります。
IdrisのDockerイメージは公式配布のものがないので個人で配布しているものを使うか、自分でビルドすることになります。

私は[mmhelloworld/docker-idris](https://github.com/mmhelloworld/docker-idris)を使っていますが、残念ながら現時点では1.3.2までしか対応しておらず、最新版の1.3.3がありません。
自分でビルドする手もあるのですが話がややこしくなるので一旦1.3.2に甘んじて設定を書きます。

Dockerイメージの設定はどのみち1行だけなので、別のイメージを使うのも特段難しいことはないでしょう。

## GitLab CI

GitLab CIは `.gitlab-ci.yml` に設定ファイルを置くだけで勝手にセットアップされます。

`.gitlab-ci.yml` に以下のファイルを置きます。

``` yaml
# インターネットに転がっていたイメージ
# 気にする人は自分でイメージを作ると良い
image: mmhelloworld/idris:1.3.2

test:
  script:
    - idris --testpkg anagram.ipkg

pages:
  script:
    - idris --mkdoc anagram.ipkg
    # docker内で生成するとパーミッションの問題が発生するらしく、一旦作り直すと解決する
    - mkdir public
    - cp -R anagram_doc/* public
  artifacts:
    paths:
      - public
  only:
    - master
```

あとはGitLabにリポジトリを作ってpushするだけです。

pushすると私が作ったリポジトリの[GitLabのパイプライン](https://gitlab.com/blackenedgold/idris-anagram/-/pipelines/232417601)のようにジョブが走ります。[生成されたドキュメント](https://blackenedgold.gitlab.io/idris-anagram/)も確認できます。


GitLab CIについて詳しくは[GitLab CIのドキュメント](https://docs.gitlab.com/ce/ci/)を参照して下さい。


## GitHub Actions

Actionsは `.github/workflows/` 以下に設定ファイルを置くと勝手にセットアップされます。

Actionsではテストとドキュメントの生成でファイルが分かれます。
トリガー条件が異なるからですね。

まずはテストの方。名前はなんでもいいんですが、 `ci.yml` という名前で作りました。

``` yaml
name: Run tests

on:
  push:
  pull_request:

jobs:
  test:
    runs-on: ubuntu-latest
    container: mmhelloworld/idris:1.3.2
    steps:
      - name: Checkout
        uses: actions/checkout@v2
      - run: idris --testpkg anagram.ipkg
```

次はドキュメントの方。`doc.yml` という名前で作りました。

``` yaml
name: Generate doc

on:
  push:
    branches:
      - master

jobs:
  doc:
    runs-on: ubuntu-latest
    container: mmhelloworld/idris:1.3.2
    steps:
      - name: Checkout
        uses: actions/checkout@v2
      - run: |
          idris --mkdoc anagram.ipkg
          mkdir public
          cp -R anagram_doc/* public
      - name: Deploy to GitHub Pages
        if: success()
        uses: crazy-max/ghaction-github-pages@v2
        with:
          target_branch: gh-pages
          build_dir: public
          jekyll: false
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

この2つを置いたらGitHubにリポジトリを作ってpushするとCIが走ります。

pushすると私が作ったリポジトリの[GitHub Actions](https://github.com/KeenS/idris-anagram/actions)のようにジョブが走ります。

GitHub Pagesはもうちょっと設定が必要です。リポジトリのSettingsのGitHub Pagesの項目で、Sourceを `gh-pages` ブランチの `/` にしてSaveします。すると[生成されたドキュメント](https://keens.github.io/idris-anagram/)を確認できます。


GitHub Actionsについて詳しくは[GitHub Actionsのドキュメント](https://docs.github.com/actions/learn-github-actions)を参照して下さい。

# まとめ

Idrisのテストの書きかた、テストの走らせかた、CIのセットアップ方法を紹介しました。
