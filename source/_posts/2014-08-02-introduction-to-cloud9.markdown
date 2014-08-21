---
layout: post
title: "Cloud9の紹介とLispのセットアップ"
date: 2014-08-02 01:02:18 +0900
comments: true
categories: [Lisp,Common Lisp, IDE, 開発環境, Cloud9]
---
κeenです。先日のLisp Meet Upで紹介した[Cloud9](https://c9.io)の詳解でも書きます。まあ、そんなに詳しくないんですけどね。
<!-- more -->
# Cloud9について
先日のスライドを見てない方はまず見て下さい。

[Cloud 9の紹介 | κeenのHappy Hacκing Blog](http://keens.github.io/slide/cloud-9.html)

公式の詳解ムービー(en)もあります。まあ、一般的なIDEの機能は一通り揃ってるようです。git連携とかも。
[CloudNine Ide - YouTube](https://www.youtube.com/user/c9ide/videos?flow=grid&view=1)

スライドでバックエンドは一人一つDockerが与えられるって書きましたが自分のVMをバックエンドにも出来るようです。

省略しましたが他のユーザーとの連携機能も多数あって、チャットだとかワークスペースの共有だとかソーシャルコーディング関連も多数機能があります。全社導入の折には役立つ機能なんじゃないでしょうか。

ローカルファイルをアップロードしたり逆にローカルにダウンロードとかも出来るようです。

尚、IDE本体は[ACEという名前でオープンソースで開発されてる](https://github.com/ajaxorg/ace)のでプラグインとかのドキュメントはそっちもあたると良いようです。
# Cloud9事始め
スクショとかは適当にスライドを参照して下さい(手抜き)
## アカウントを作る
[c9.io](https://c9.io)にアクセスしてGithubかBitbucketのアカウントでログイン出来ます(以後、Githubでログインしたものとします。Bitbucketの方は適宜読み替えて下さい)。Cloud9で書いたコードをHerokuやらなんやらに直接上げるつもりなら普通にCloud9アカウント作っても良いと思いますが、多くの場合どちらかとの連携を使うと思うので素直にGithubでログインしましょう。

ログインしたらダッシュボードに飛ばされます。demo-projectと自分のGithubのプロジェクトがサイドバーにある筈です。
## 細かな設定
demo-projectでも開きましょうか。demo-projectを選択してstart editingです。

IDEというかEditorの設定が出てくる筈です(多分)。キーバインディングとかタブ幅とか設定しましょう。そして、`C-h`で`backward-delete-char`出来ないと死んでしまう方のための設定はこちらです。Preferencesのkeybindingsのedit your keymap fileからkeymap fileに飛んで、
```json
[
    {"command": "backspace", "keys":["Ctrl-H", "Backspace"]},
    {"command": "replace", "keys":["Alt-Shift-5"]}
]
```
と編集、保存しましょう。どうも、キーにコマンドを割り当てるのではなく、コマンドにキーを割り当ててる(?)っぽいのでCtrl-Hを持ってる`replace`を別のキーにしないと動かないようでした。尚、これはファイルをいじらなくてもsearch keybindingsのところから目的のコマンドを捜して、keystrokeのところをクリックして、割り当てたいキーストロークをすれば割り当て出来るようです。その際、自分のkeymap fileも対応して書き変わります。

## Common Lispのセットアップ
処理系のインストールはいくつか方法があります。

1. `apt-get`で入れる
2. バイナリを落してくる
3. [CIM](https://github.com/KeenS/CIM)を使う

のいずれかの方法で処理系を入れて下さい。`apt-get`で入れる場合はかなり古いバージョンが入ります。CIMで入れる場合は`sudo apt-get install time`しておかないとSBCLのビルドが出来ませんでした。他の処理系は確認してないです。どの処理系でも良いんですがとりあえずSBCLを入れたってことで話進めますね。私がそれしか確認してないので。

また、便利のためrlwrapを入れましょう。`sudo apt-get install rlwrap`です。

そこまでいったらrun scriptを作りましょう。上のメニューバーから[Run]>[Run With]>[New Runner]と進みます。で、run scriptの内容はどの方法で処理系を入れたかに依るのですが、

1の場合

```json
{
    "cmd" : ["sudo", "rlwrap", "sbcl", "--load", "$file", "$args"],
    "info" : "Started SBCL :: $project_path$file_name",
    "env" : {},
    "selector" : "source.ext"
}
```

だそうです([引用元](http://cjelupton.wordpress.com/2014/07/24/hacking-lisp-in-the-cloud-pt-3/))。`sudo`がついてるのはWEBアプリの場合0.0.0.0で待つためにroot権限が必要だからかな？

これは実行した後でREPLに入ってデバッガとか扱う感じの設定ですね。  
単に実行したいだけののきは`--load`の部分を`--script`に変えれば良さそうです(未確認)  
2の場合も同じ設定でいけそうな気がします。PATHぐらい必要かな?(未確認)

3の場合

```json
{
    "cmd" : ["sudo", "cl", "$file", "$args"],
    "info" : "Started cl :: $project_path$file_name",
    "env" : {"CIM_HOME":"$HOME/.cim","PATH": "$CIM_HOME/bin:$PATH", "LISP_IMPL": "sbcl"},
    "selector" : "source.ext"
}
```

です。本来CIMは`$CIM_HOME/init.*`を呼ばないと動かないのですが、ショートカットする環境変数を設定することで無理矢理動かしてます。Cloud9のinitスクリプト(jsファイル)があるようなのでそこでCIMをinitしておけば無理せずとも出来そうですがあまり深く踏み込んでないです。

尚、この設定は実行即終了のものです。実行してREPLに入りたい場合は`"cmd"`を`["cl","-rf", "$file", "$args"]`にすると出来ます。

## 動かしてみる
上記のrun scriptを設定、保存したらメニューバーから[File]>[New File]と進み、新しいファイルを作ります。  
一番簡単に
```lisp
(write-line "Hello Cloud9")
```
とでも書きましょうか。そして[>Run]してちょいと待つと実行出来る筈です。動かなかったら手動で[Run]>[Run With]>[My Runner]とかしてみましょう。ここまで来ればセットアップ完了です。

# 今後の課題というか要調査というか
## シンタックスハイライト
Lisp Meet Upでも喋りましたがハイライトが最悪です。ユーザー定義ハイライターも(少くともACEレベルでは)出来そうなのでどうにかする必要があります。
## インデント
Lispのインデントはタブn個とかで済むようなものじゃなくてどのオペレータの中にいるかに依って変わります。その辺どうにかなって欲しいですね。
もしかしたらClojureは上手くいってるかもしれないのでそこからインポートしてくれると嬉しいですね。
## swankインターフェース
initスクリプトがあるって事はもしかしたらIDEもプログラマブルなのかもしれないのでswankインターフェースを作れるかもしれません。
## CIMインターフェース
initスクリプトがあるって事は(ry。最終目標はメニューバーからどの処理系を使うか選択可能とかで。

# まとめ
 + Cloud9を紹介した
 + Common Lispのセットアップ方法を示した
 + 誰か諸問題解決して下さい
