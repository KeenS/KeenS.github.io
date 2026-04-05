---
categories: [zfs]
date: 2026-04-05T14:43:30+09:00
title: "ZFSのバックアップの設定"
---
κeenです。一昨年ファイルシステムをbtrfsからZFSに移行したあとそういやバックアップの話を書いてなかったので書き残します。
<!--more-->

前提として、デスクトップで使っているUbuntuで、ルートにZFSを使っています。

ここではバックアップを2系統に分けて考えます。

* 軽~中程度の破損
  + 例: ファイルをうっかり削除してしまったので復元したい
* 重程度の破損
  + 例: PCが燃えたので一から組み直すことになった

軽~中はZFSのスナップショットを、重はファイルを丸ごとクラウドに同期する戦略を取ります。
因みに私はRAID Zを組んでるのでディスク破損は想定してませんが、そうでない方はディスク破損も重に入ると思います。

## スナップショット
軽~中程度の破損の対策です。
ZFSにスナップショットの仕組み自体はあるのですが、それを定期的に取ったり古いスナップショットを退役させたりするのは一手間かかるので管理ツールを導入します。簡素な仕組みで動くzfs-auto-snapshotというツールです。

[zfsonlinux/zfs-auto-snapshot](https://github.com/zfsonlinux/zfs-auto-snapshot)

インストールすると `/etc/cron.{d,daily,hourly,monthly,weekly}` にこんな感じの `zfs-auto-snapshot` というファイルが作られます(なかったらどこかにあるはずなのでコピーしてきて下さい)

```sh
#!/bin/sh

# Only call zfs-auto-snapshot if it's available
which zfs-auto-snapshot > /dev/null || exit 0

exec zfs-auto-snapshot --quiet --syslog --label=weekly --keep=8  -r bpool/BOOT rpool/ROOT rpool/USERDATA
```

あとは好みに応じて `man zfs-auto-snapshot` をみながら細かい設定を調整していきます。

`/etc/cron.d/` は定期実行で有名ですね。それ以外の `cron.weekly` とかはそこにシェルスクリプトを置いておくと勝手に時次・日次・週次・月次でのそスクリプトを実行してくれます。 `/etc/cron.d/` と違って冒頭の実行間隔指定がないので書きやすいですね。

↑のスクリプトは `/etc/cron.weekly/` に置いてあるもので、週次のスナップショットを最大8つまで取る設定になっています。

因みに `/etc/cron.d/` のものはこうなっています。

```sh
PATH="/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin"

*/15 * * * * root which zfs-auto-snapshot > /dev/null || exit 0 ; zfs-auto-snapshot --quiet --syslog --label=frequent --keep=4 -r bpool/BOOT rpool/ROOT rpool/USERDATA
```

15分に一回、最大4つまで取る設定になってます。1時間経ったら時次のスナップショットに引継ぎですね。

私はそんな感じで15分ごとを4つ、1時間ごとを24つ、1日ごとを7つ、1週間ごとを8つという設定にしています。デフォルトだと1月ごとを12つとかの設定もあるので長すぎるなと感じる人は調整して下さい。

撮ったスナップショットの一覧を取得するのは `zfs list -t snapshot` なので動作してるか確認したい人は活用して下さい。

正しく動くとこんな感じでファイルシステムに `.zfs` という隠し(というか `ls -a` に出てこない)ディレクトリが作られて、その中にスナップショットが見えるようになります。

```console
$ ls ~/.zfs/snapshot
zfs-auto-snap_daily-2026-03-30-0106     zfs-auto-snap_hourly-2026-04-02-1217  zfs-auto-snap_hourly-2026-04-04-1417
zfs-auto-snap_daily-2026-03-31-0109     zfs-auto-snap_hourly-2026-04-02-1317  zfs-auto-snap_hourly-2026-04-04-1517
zfs-auto-snap_daily-2026-04-01-0029     zfs-auto-snap_hourly-2026-04-02-1417  zfs-auto-snap_hourly-2026-04-05-0317
zfs-auto-snap_daily-2026-04-02-0152     zfs-auto-snap_hourly-2026-04-02-1517  zfs-auto-snap_hourly-2026-04-05-0417
zfs-auto-snap_daily-2026-04-02-2237     zfs-auto-snap_hourly-2026-04-02-1617  zfs-auto-snap_hourly-2026-04-05-0517
zfs-auto-snap_daily-2026-04-04-1356     zfs-auto-snap_hourly-2026-04-02-2217  zfs-auto-snap_weekly-2026-02-02-0017
zfs-auto-snap_daily-2026-04-05-0303     zfs-auto-snap_hourly-2026-04-02-2317  zfs-auto-snap_weekly-2026-02-09-0336
zfs-auto-snap_frequent-2026-04-05-0515  zfs-auto-snap_hourly-2026-04-03-0017  zfs-auto-snap_weekly-2026-02-16-0120
zfs-auto-snap_frequent-2026-04-05-0530  zfs-auto-snap_hourly-2026-04-03-0117  zfs-auto-snap_weekly-2026-02-24-0329
zfs-auto-snap_frequent-2026-04-05-0545  zfs-auto-snap_hourly-2026-04-03-0217  zfs-auto-snap_weekly-2026-03-12-1308
zfs-auto-snap_frequent-2026-04-05-0600  zfs-auto-snap_hourly-2026-04-03-0317  zfs-auto-snap_weekly-2026-03-19-0158
zfs-auto-snap_hourly-2026-04-02-0817    zfs-auto-snap_hourly-2026-04-03-0417  zfs-auto-snap_weekly-2026-03-25-2337
zfs-auto-snap_hourly-2026-04-02-0917    zfs-auto-snap_hourly-2026-04-03-0517  zfs-auto-snap_weekly-2026-04-02-0251
zfs-auto-snap_hourly-2026-04-02-1017    zfs-auto-snap_hourly-2026-04-03-0617
zfs-auto-snap_hourly-2026-04-02-1117    zfs-auto-snap_hourly-2026-04-03-0717
```

それぞれがスナップショットされたツリーになっているので、普通に `cat` やら `cp` やらで簡単にファイルを取り出せます。


## クラウド同期
重程度の破損の対策です。割り切ってホームディレクトリ以下を `tar` にまとめて圧縮してDropboxに放り込みます。ZFSのアーカイブ機能もありますが、復元するときもZFSじゃないといけないのでもっと可搬性のある `tar` を採用しました。
ZFSのスナップショットで静止点を取れるので最新のスナップショットをそのままtarにまとめれば済みますね。

そういうシェルスクリプトを書きました。

``` sh
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

    latest_snapshot="$(ls --color=never -t .zfs/snapshot | head -n1)"
    echo "Taking backup of $latest_snapshot"
    ls -l .zfs/snapshot | grep "$latest_snapshot"
    mv -f Dropbox/backup/home.tar.xz Dropbox/backup/home.old.tar.xz || true
    mv -f Dropbox/backup/home.tar.xz.sha1 Dropbox/backup/home.old.tar.xz.sha1 || true
    nice tar cJvf Dropbox/backup/home.tar.xz \
        --sparse \
        -p --xattrs \
        --exclude=./Dropbox \
        --exclude=./.cache \
        --exclude=./.steam \
        -C ".zfs/snapshot/$latest_snapshot/" \
        .
    sha1sum Dropbox/backup/home.tar.xz > Dropbox/backup/home.tar.xz.sha1
}
set -e
cd $HOME
export PATH=/usr/bin:/usr/sbin
start_time="$(date +%s)"
main "$@"
end_time="$(date +%s)"
echo "$(($end_time - $start_time)) seconds"
date
```

バックアップの話題は何度かしてるので過去に登場したことがありますが、細かいアップデートがあります。ユーザのホームディレクトリをバックアップするのでユーザ権限のSystemdで動かすようにしたり、エラーハンドリングとかログとかもちょっとだけ増やしました。

あとはSystemdの設定です。毎週走るようにタイマーを設定しましょう。 `~/.config/systemd/user/backuphome.service` に以下を置き、

```text
[Unit]
Description="Backup home"

[Service]
ExecStart=/path/to/backup-home.sh
```

`~/.config/systemd/user/backuphome.timer` に以下を置きます。

```text
[Unit]
Description="run backup-home weekly"

[Timer]
OnCalendar=weekly
Persistent=true

[Install]
WantedBy=timers.target
```

多分置いただけだと設定が反映されないのでリロードしときましょう。

``` console
$ systemctl --user daemon-reload
```

そしたら `systemctl --user status backuphome.service` や `systemctl --user status backuphome.timer` などで様子をみます。

``` console
$ systemctl --user status backuphome.service
● backuphome.service - "Backup home"
     Loaded: loaded (/home/shun/.config/systemd/user/backuphome.service; linked; preset: enabled)
     Active: active (running) since Sun 2026-04-05 15:18:06 JST; 29min ago
 Invocation: 7cbd43ee1aa1487ebe6d75ac62f6491e
   Main PID: 2455894 (backup-home.sh)
      Tasks: 132 (limit: 303067)
     Memory: 19.3G (peak: 19.3G)
        CPU: 1d 39min 25.655s
     CGroup: /user.slice/user-1000.slice/user@1000.service/app.slice/backuphome.service
$ systemctl --user status backuphome.timer
● backuphome.timer - "run backup-home weekly"
     Loaded: loaded (/home/shun/.config/systemd/user/backuphome.timer; enabled; preset: enabled)
     Active: active (waiting) since Sun 2026-04-05 16:22:21 JST; 2h 12min ago
 Invocation: 00f1c20be510416eaff2802540f6825b
    Trigger: Mon 2026-04-06 00:00:00 JST; 5h 25min left
   Triggers: ● backuphome.service

 4月 05 16:22:21 mini systemd[6953]: Started backuphome.timer - "run backup-home weekly".
```

設定したときの記憶が薄いので他にも操作が必要かもしれません。`systemctl --user enable backuphome.timer` とかを叩きましょう。 `systemctl --user list-timers --all` とかでタイマーが設定されていることを確認できればOKです。

ユーザモードでSystemdのタイマーを設定できれば毎週ユーザ権限でバックアップを走らせてくれます。パスとかファイルのownerとかの問題が解決して嬉しいですね。
