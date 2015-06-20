---
categories: [Shell Script]
date: 2015-06-17T22:12:25+09:00
title: Shell Scriptを書くときのテクニック10選
---
κeenです。LLで書かれた動作のもっさりしてるコマンドラインツールよりシェルスクリプトが好きです。

しかしシェルスクリプトを書く時にはハマり所も多いです。ということでハマりそうなところと対処法を共有しますね。
<!--more-->
# 1. 変数は基本クォート
変数には空白が入り得ます（特にパス名とか）。あるいは空である可能性もあります。

```sh
mv file1 file2 $target_dir
```

で`$target_dir`が空だった時は

```sh
mv file1 file2
```

となってしまいます。file2が上書きされますね。恐ろしい。

クォートで括っておけば

```sh
mv file1 file2 "$target_dir"
```

```sh
mv file1 file2 ""
```

となり辺なディレクトリに書き込もうとしてエラーになります。まだマシですね。

基本、と書いたのはクォートしたくない場合もあるからです。例えばこんな時ですね。

```sh
items="foo bar baz"
for item in $items
do
    echo "$item"
done
```

変数展開の後にトークン分割が行なわれるので空白で区切れば複数のトークンを1つの変数に入れられます。

# 2. -eオプションをつけよう

シェルには例外がありません。何かが失敗しても走り続けます。これは時に迷惑な挙動です。しかし `-e`オプションをつけると0以外の終了ステータスが出た瞬間スクリプトが止まるようになります。

`-e`オプションを有効にするには

```sh
#!/bin/sh
set -e
```

とすれば良いです。0以外のステータスで終了し得るコマンドの例外ハンドル的なことをしたければ

```sh
trying_command || true
```

とすれば確実に0で終了します。

また、スクリプトの途中で`-e`を切り替えたければ


```sh
#!/bin/sh
set -e
# -eが有効
set +e
# -eが無効
set -e
# -eが有効
```

のように`set -e`と`set +e`で制御出来ます。

# 3. カレントディレクトリはスクリプトを起動した場所

例えば ~/Shell/hoge.shにこう書いたとします。

```sh
#!/bin/sh
cat ./hoge.sh
pwd
```

それを実行するとこうなります

```
$ cd ~/Shell
$ sh hoge.sh
#!/bin/sh
cat ./hoge.sh
pwd
/home/kim/Shell
$ cd ../
$ sh Shell/hoge.sh
hoge.sh: no such file or directory
/home/kim
```

なのでシェルスクリプト内での相対パスは基本的に信用出来ません。

スクリプトの相対でパスを指定したいならこうしましょう。

```sh
#!/bin/sh
ROOT="$(cd $(dirname $0); pwd)"
cat "$ROOT/hoge.sh"
pwd
```

`$(cd $(dirname $0); pwd)` がキモです。 $0には起動スクリプトが入ってます。 `$ ./hoge.sh`と起動したなら `./hoge.sh`が、 `$ ../hoge.sh` なら `../hoge.sh`が。そのスクリプトがあるディレクトリに移動(`cd $(dirname $0)`) して`pwd`するとスクリプトのあるディレクトリの絶対パスがとれます。

因みにこのイディオムはスクリプトにリンクを張られると困ります。 `readlink(1)` を使う流儀もあるのですがreadlinkがMacとLinuxで全然違い、互換性を保てないのでおすすめしません。

# 4. sudo command >> fileでパーミッションエラー
`>>` (`>`) を使った時にファイルに書き込んでるのは `command`ではなくてシェルなのでシェルのアクセス権限でパーミッションエラーが出ます。sudoして書き込みたいなら

`sudo command > file` ではなく

```
$ command | sudo tee file
```

を、

`sudo command >> file` ではなく

```
$ command | sudo tee -a file
```

を使いましょう。


関連して、`echo`は外部コマンドではなくシェルの組込みコマンドなのでsudoに渡すことが出来ません。これもファイルに書き込む目的ならechoとsudo teeをパイプで繋ぎましょう。


# 5. sudoのパスワードを渡せない
スクリプト内でsudoを扱うのは少しテクニックが必要になります。sudoに-Sを付けると標準入力からパスワードを読むようになります。かといってソースにベタ書きする訳にはいきません。
ということで、こうなります。

```sh
printf "password: "
read password
echo "$password" | sudo -S command
```

ユーザにパスワードを要求し、メモリに保存。必要な時にsudoに渡すという流れです。

シェルに依ってはreadに-sをつけることでエコーバックしなくなるのでディスプレイにパスワードが流れてくることはありません。未確認ですがdash(Debian系の/bin/sh)では出来なくてB shell(BSD系の/bin/sh)やBash、Zshなどの拡張POSIXシェルだと出来そうです。

余談ですが改行なしの印字に`echo -n`は使えません。`-n` シェルによってはオプションを認識しないので。printf(1)はPOSIXにあるのでポータブルに使えます。

因みにパスワードが初期化されてないならユーザに訊く、というのはこういう関数を実装すれば良さそうです。

```sh
password(){
    if ! ${password+:} false
    then
        printf "password: "
        read -s password
    fi
}
```

# 6. sudoのパスワードを渡しつつ標準出力も渡したい

```sh
{ echo "$password" ; cat } | sudo -S command
```

しましょう。


# 7. リモートでスクリプトを実行したい
一旦scpでスクリプトを送ってから実行？そんな面倒なことしたくありません。

```sh
ssh user@remote <<SHELL
#  some script
SHELL
```

で実行出来ます。これはログインシェルで実行します。ログインシェル如何に関わらずshで実行したいなら(ログインシェルがcshとかnologinとかは割とありえる)

```sh
ssh user@remote sh <<SHELL
#  some script
SHELL
```

としましょう。

# 8. リモートでsudoのパスワードを渡したい

先のテクニックがそのまま使えます。

```sh
printf "password: "
read password
ssh user@remote sh <<SHELL
echo "$password" | sudo -S command
SHELL
```

クォート無しのヒアドキュメントのシェル変数の展開はローカルで行なわれるのでローカルにある変数がそのまま参照出来ます。

# 9. リモートにローカルに置いてあるファイルを送りたい
scpでも良いですがroot loginを許可してないと設定ファイルを/etcに置けないなどと不都合が生じます。


```sh
ssh user@remote sh <<SHELL
cat <<'EOF' >> some_remote_file
$(cat local_file)
EOF
SHELL
```

これは少し解説が必要でしょうか。

まず、先程も出てきた

```sh
ssh user@remote sh <<SHELL
SHELL
```

ですが普通のヒアドキュメントなので中の変数やコマンド置換を展開します。ということでリモートで実行されるのは

```sh
cat <<'EOF' >> some_remote_file
# the content of
# local_file
EOF
```

となります。そしてcatのヒアドキュメントはクォート付きなのでlocal_fileの中身がさらに変数展開されることはありません。

勿論、root権限で書き込みたかったら先程までのテクニックを組み合わせて

```sh
ssh user@remote sh <<SHELL
{ echo "$password" ; cat <<'EOF' } | sudo -S tee  some_remote_file
$(cat local_file)
EOF
SHELL
```

として下さい.

# 10. リモートにあるファイルを編集したい

sed(1)を使いましょう。あるいは、sedだと辛いならローカルでファイルを編集、diffをとってpatch(1)を使いましょう。diffの送り方はもう分かりますね。

```sh
ssh user@remote sh <<SHELL
cat <<'EOF' |  patch
$(cat file.diff)
EOF
SHELL
```

です。


------

いかがでしょうか。テクニックさえ抑えればシェルスクリプトは料理人だか板前だか知りませんが流行に流されるツールにも負けない力があると思います。動作も速い。
みなさんシェルスクリプト書きましょうね。
