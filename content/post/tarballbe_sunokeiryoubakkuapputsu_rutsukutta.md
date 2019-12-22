---
categories: [Shell, CLI]
date: 2015-05-06T20:45:53+09:00
title: tarballベースの軽量バックアップツール'Sheltar'を作った
---
κeenです。みなさんバックアップは何使ってますか？大抵のOSには標準でバックアップツールが付いてますがそれ使ってます？私も使ってます。
しかしやつらはそのOSでしか動かない/動かす方法があまり知られていないのでマシンがクラッシュしたりマシンを失くした時に困ります。

そこで候補に上がるのが安心と信頼のtarballですが今度は増分バックアップが取りづらいという問題があります。なのでtarballベースの差分バックアップツールを作りました。
<!--more-->
まあ、捜せば同じようなものが色々あるでしょうが捜し回るのが面倒だし色々インストールするのも嫌なのでえいやと作ってしまいました。


タイトルにある通り[Sheltar](https://github.com/KeenS/sheltar)といいます。ポータブルなB Shellスクリプト製です。どこでも動くと思います。バックアップ（避難）だしshellだしtarだしって適当に付けたら意外にもコンフリクトがありませんでした。しかも[ウルトラ怪獣](http://ultra.wikia.com/wiki/Sheltar)にも居るという奇跡的な名前です。

# 目的
* [増分バックアップ](http://ja.wikipedia.org/wiki/%E5%A2%97%E5%88%86%E3%83%90%E3%83%83%E3%82%AF%E3%82%A2%E3%83%83%E3%83%97)をとる
* UbuntuとMac間でのデータ同期にも使いたい(家のルータがしょぼいのでrsyncを使いたくなく、USBメモリ経由とかが良い)
* なのでファイル形式はポータブルなtarballで
* メタデータとかは極力置かず、最悪他のツールからでもバックアップデータをサルベージ出来るようにしたい

です。 GNU tarには増分バックアップ用のオプションがありますがファイル抽出は手作業でやる必要があります。
また、(多分)BSD tarには増分バックアップ用のオプションがないのでそこもカバーする必要があるかなと思って作りました。

あとオプションがあるとはいっても一々覚えてられないのでスクリプト化してしまいたかったってのもあります。

# 使い方
## STEP0
[ここ](https://github.com/KeenS/sheltar)からSheltarを入手しましょう。`sheltar`スクリプト1つで完結しているのでcloneしなくてもrawをwgetしてきても大丈夫です。
## STEP1
増分バックアップなので複数のtarballが作られます。バックアップ用のディレクトリを用意しましょう。

```sh
$ mkdir /mnt/dev1/backup
```

## STEP2
バックアップしたいファイルをリストアップしましょう。バックアップは継続的に行なうものなので1回限りのコマンドライン引数で渡す訳にはいきませんね。

pngファイルをバックアップしたいとしましょうか。

```sh
$ find Pictures -name '*.png' >> list.txt
```

これで良さそうです。ディレクトリの中身全部をバックアップしたいなら

```sh
$ echo Pictures/ >> list.txt
```

でも構いません。最後の'/'重要です。最初のバックアップの時には問題ありませんが'/'がないと増分バックアップの時にディレクトリ自体のlast modified timeを見て変更を判断します。

## STEP3
あとは実行するだけですね。

```sh
$ sheltar backup /mnt/dev1/backup list.txt
```

その後、増分バックアップを取るのも同じく

```sh
$ sheltar backup /mnt/dev1/backup list.txt
```

で可能です。前回のアーカイブファイルのlast modified time より新しいファイルだけをバックアップしてくれます。

因みに今のところバックアップディレクトリに余計なものがあると正しく動作してくれないので注意です。

## 復元
全部のファイルをカレントディレクトリにぶちまけたいなら

```sh
$ sheltar extract /mnt/dev1/backup
```

です。抽出するファイルを指定したいなら

```sh
$ sheltar extract /mnt/dev1/backup file1 file2 ...
```

とします。

カレントディレクトリに同名ファイルが既にある場合は新しい方を残します。

# その他
まだまだ未完成です。欲しい機能があったら[issue](https://github.com/KeenS/sheltar/issues)に投げてくれると実装するかもしれません。[Pull Request](https://github.com/KeenS/sheltar/pulls)もお待ちしております
あと、ドックフードはまだ食べてません。一応テストはしてますが…。Macをまともに使う気になったら使い始めると思います。


あ、そうそう。今回比較的コメントを多く書いたのでシェルスクリプトを勉強したい方はソース読んでみて下さい。
こんな感じです。Doxygenがシェルスクリプトに対応してないので疑似ドキュメント形式のコメントですが。

```sh
# Extract the all files from backups in $ST_BACKUP_DIR.
# A file with the same name as extracting file will be preserved if it is newer than its counterpart.
## @filesystem extract all the backup files to CWD
st_extract_all()
{
    # This is preferable to `for TARBALL in $(ls)` because the names of listed files
    # can contain whilespaces
    ls --sort=time -r "${ST_BACKUP_DIR}" | while read TARBALL
    do
        tar xf "${ST_BACKUP_DIR}/${TARBALL}" \
            --keep-newer-files               \
            --preserve-permissions           \
            --preserve-order
    done
}
```

