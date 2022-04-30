---
categories: [Linux]
date: 2022-04-30T17:54:15+09:00
title: "Dropboxにtarを使ったバックアップを毎週取る"
---
κeenです。最近バックアップを見直したのですが、その一環でtarでフルバックアップを毎週取るようにしたのでその報告です。

<!--more-->

## 背景

今までUbuntuに標準でついてきたdeja-dupを使っていたのですが、リモートストレージにバックアップを取れない、リストアが遅い、使用するストレージ容量の上限を設定できない、などの理由で乗り換えを検討しました。
調査の結果、1つのツールで条件を満たすのは難しそうなので用途別にバックアップを使い分けることにしました。ここではその用途の1つ、「家が燃えるなどしてPCのデータが一切ダメになった場合に備えた完全なバックアップ」を取るための方法です。

検討したときのスクラップはこちらにあります。

[バックアップを考えたい](https://zenn.dev/blackenedgold/scraps/40c2acb437b355)

## やりたいこと

データが一切ダメになって、復元するときも全てのデータを丸ごと復元するケースを想定しているので割り切った要求になります

* 毎回フルバックアップを取る
  + インクリメンタルだと復元とかデータのローテション管理とかが面倒かなって
* できるだけ整ってない環境から復元できる
* バックアップをできるだけ1つのファイルにまとめられる
  + インターネット経由でダウンロードするときに速い
* バックアップを圧縮できる
  + ストレージ料金を安く済ませたい
* 長期保存/ダウンロードが安いクラウドストレージに保存できる

ツールはbtrfsのバックアップツールとかrcloneだとかdeja-dupのバックエンドになってるduplicityも検討しましたが、私の環境では素直にtar.xzを作るのが一番かなと結論づけました。
多少GNU tarに依存する部分はあるものの、どうせLinux使ってるならGNU tarは使ってるでしょうし汎用性も申し分なさそうです。
同じような理由で昔tarballベースのバックアップツールを作っていたりしますが、今回はインクリメンタルバックアップをしないので出番はありませんでした。

[tarballベースの軽量バックアップツール'Sheltar'を作った | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2015/05/06/tarballbe_sunokeiryoubakkuapputsu_rutsukutta/)

ストレージも多少検討したのですが、ディレクトリに放り込むだけという手軽さと、既に有料プランを契約していて数百GB分の容量を持て余してるDropboxを使うのが一番だろうという結論になりました。

他のクラウドストレージのコスト感については以下のブログが参考になります。

[NAS のクラウドバックアップのコスト (Amazon S3 vs Backblaze B2) – ikata.co::blog](https://ikata.co/archives/393)

## やること

フルバックアップなので基本は `tar` を使って `$HOME` をアーカイブするだけですが、古いバックアップをどこまで保存するかとか、静止点取れるのかとか色々あります。

* 古いバックアップは1つ前まで保存。
* btrfsを使っているので静止点はスナップショットから取る

をやります。おおむね以下のような処理をします。

```sh
# 1つ前のアーカイブを.oldにリネーム。2つ前のアーカイブを上書く。
mv $HOME/Dropbox/backup/home.tar.xz $HOME/Dropbox/backup/home.old.tar.xz
# (毎時撮っている)btrfsスナップショットの最新版を取得。これは私のPC固有の処理。
latest_snapshot="$(ls -rt /pool/backups/ | grep @home | head -n1)"
# バックアップを取る
# cvf                      -- おなじみのtarを作るオプション
# --sparse                 -- 中身がほとんど0だがメタデータ上サイズだけ大きいファイルを適切に処理する
# --use-compress-prog=pixz -- xz圧縮に並列圧縮ツールを使う(要apt install)
# -p --xattrs              -- 権限や属性などを保存
# --exclude=HOGE           -- HOGEをアーカイブに入れない
#                          -- 保存先のDropboxを入れると悲惨なことになるし、.cacheは保存しなくても構わない。
# -C DIR                   -- ファイル名をDIRからの相対パスとして解釈する
#                          -- ここではbtrfsのスナップショットをベースにした相対パスにしてるので
#                          -- 復元したときに普通のhomeのように見える
# 並列圧縮すると重いのでnice値を上げておく
nice tar cvf $HOME/Dropbox/backup/home.tar.xz \
    --sparse \
    --use-compress-prog=pixz  \
    -p --xattrs \
    --exclude=Dropbox \
    --exclude=.cache \
    -C "/pool/backups/$latest_snapshot/$USER/" \
    .
```

この処理を一旦手で叩いてみて動くか確認しましょう。 `mkdir -p $HOME/Dropbox/backup` とかが事前に必要かもしれませんし、 `pixz` がちゃんと動くかとかも調べておきたいですね。

よさそうならスクリプトにまとめてcronを設定してあげます。
以下の内容を `backup-home.sh` に保存して実行権限をつけましょう。cronだといくか手元と環境が違うのでそこだけ注意しながら清書します。

```shell
#!/bin/sh
# templated by http://qiita.com/blackenedgold/items/c9e60e089974392878c8
usage() {
    cat <<HELP
NAME:
   $0 -- backup home

SYNOPSIS:
  $0 [-h|--help]
  $0 [--verbose]

DESCRIPTION:
   Backup home from the latest snapshot. It logs to $LOG_FILE

  -h  --help      Print this help.
      --verbose   Enables verbose mode.
HELP
}

main() {
    SCRIPT_DIR="$(cd $(dirname "$0"); pwd)"

    while [ $# -gt 0 ]; do
        case "$1" in
            --help) usage; exit 0;;
            --verbose) set -x; shift;;
            --) shift; break;;
            -*)
                OPTIND=1
                while getopts h OPT "$1"; do
                    case "$OPT" in
                        h) usage; exit 0;;
                    esac
                done
                shift
                ;;
            *) break;;
        esac
    done

    /usr/bin/mv /home/name/Dropbox/backup/home.tar.xz /home/name/Dropbox/backup/home.old.tar.xz
    /usr/bin/chown name:name /home/name/Dropbox/backup/home.old.tar.xz
    latest_snapshot="$(/usr/bin/ls -rt /pool/backups/ | /usr/bin/grep @home | /usr/bin/head -n1)"
    /usr/bin/nice /usr/bin/tar cvf /home/name/Dropbox/backup/home.tar.xz \
        --sparse \
        --use-compress-prog=/usr/bin/pixz  \
        -p --xattrs \
        --exclude=Dropbox \
        --exclude=.cache \
        -C "/pool/backups/$latest_snapshot/name/" \
        .
    /usr/bin/chown name:name /home/name/Dropbox/backup/home.tar.xz
}
set -e
LOG_FILE=/var/log/backup-home.log
main "$@" 2>&1 > $LOG_FILE
```

あとは `sudo mv backup-home.sh /etc/cron.weekly/` としてあげれば設定終わりです。
ログだけ出しっぱなしだと心配なのでlogrotateを設定してあげましょう。以下の内容を `/etc/logrotate.d/backup-home` に保存します。

```text
/var/log/backup-home.log {
        weekly
        missingok
        rotate 7
        compress
        notifempty
        copytruncate
}
```

因みに xzの圧縮率はかなり高く、元のデータの40%ほどになりました。

```console
$ du -h --exclude=Dropbox --exclude=.cache  -s $HOME
317G    /home/name
$ du -h $HOME/Dropbox/backup/home.tar.xz
129G    /home/name/Dropbox/backup/home.tar.xz
```

シンプルなツールの組み合わせで実現できてよかったです。

