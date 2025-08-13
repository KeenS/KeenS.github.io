---
categories: [Ubuntu]
date: 2025-08-13T21:44:28+09:00
title: "Ubuntu 25.04へのアップグレードが過去一難しかった"
---
κeenです。この時期にアップグレードしてるあたりからも分かるとおり、Ubuntu 25.04へのアップグレードが過去一難しかったので情報を書き留めておきます。

<!--more-->

私はzfsルートでUbuntuを使っています。

[Ubuntuをzfsルートにしたときのメモ](https://zenn.dev/blackenedgold/scraps/7fcb8e9b2790bb)

UbuntuはLTSじゃなくて最新版を追い掛けるスタイルで使っているので半年に一回のアップグレードがあります。LTSじゃないバージョンは9ヶ月しかサポートしないので出たら3ヶ月以内にアップグレードしないといけません。

今回も同じようにやっていました。新バージョンが出て間もなく、具体的には4/24にアップグレード。しかしいくつかトラブルがあり、アップグレードできませんでした。

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">Ubuntu 25.04に上げようとしたら依存が大量に壊れてて失敗する。どうしよ。 <a href="https://t.co/RiTDk9Ez1d">pic.twitter.com/RiTDk9Ez1d</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1915250984202498503?ref_src=twsrc%5Etfw">April 24, 2025</a></blockquote>

## アップグレードの問題
第一段は今回のアップグレードのタイミングでaptの依存が複数種類の壊れ方をしていました。詳細は以下のリンクにありますが、私が影響を受けたのが

* 外部リポジトリからのaptパッケージを勝手に削除してしまう
* KDE関連のパッケージが入っていると依存が壊れる
  + = アップグレードの途中でエラーで止まる

という問題でした。

[Status of Oracular to Plucky upgrades - Project Discussion / Release - Ubuntu Community Hub](https://discourse.ubuntu.com/t/status-of-oracular-to-plucky-upgrades/59652)

私はKubuntuではない素のUbuntuを使っていましたが、digiKamなどKDE系のGUIアプリケーションを入れていたので依存が壊れていました。依存が壊れるとアップグレードの進行も巻き戻しもできないので、zsysが撮ったスナップショットから復元して、原因っぽいところを直したらまたトライして失敗して復元して、を繰り返していました。

この問題はすぐに認知され、一度アップグレードの配布が停止になって問題解決が急がれました。
これは幸い早め(5/15)に解消さてアップグレードも再開されました。

## ZFSの問題

ところが、まだアップグレードしてもエラーが出ます。第二段はZFSの問題でした。これは根が深いです。
Ubuntuのアップグレードはカーネルやらユーザランドやらのパッケージをまとめて更新します。このパッケージの更新はユーザランドは更新するとすぐに変更が反映されますが、カーネルは再起動したあとに変更が反映されます。この変更の反映のタイミングのずれのせいで、あるタイミングでZFSのバージョンがユーザランドとカーネルで違うものになってしまうのです。
今までは *たまたま* ZFSのツールに互換性があったので問題にならなかったのですが、今般その眠っていた互換性問題が火を吹きました。アップグレードの最中にZFSの操作でフリーズする現象が確認されました。

[Bug #2110891 “System freeze on release upgrade 24.10 oracular to...” : Bugs : ubuntu-release-upgrader package : Ubuntu](https://bugs.launchpad.net/ubuntu/+source/ubuntu-release-upgrader/+bug/2110891)

中途半端にアップグレードした状態でフリーズするので強制シャットダウンして再起動しても古いカーネルのままで、システムの起動もできない状態です。またzsysの撮ったスナップショットから復元する作業をします。zsysのスナップショットがなかったら即死でした。

この問題も先のaptの依存の問題が解消してすぐの5/15に報告され、そのまま確認されました。
ですが問題への対処が難しく、根本的な解決はされないままZFSを使っているシステムではアップグレードを中止するよう応急措置がとられました。

最初に書いたとおり、24.10のサポート期間は9ヶ月、具体的には7/10までですので修正を急がないとアップグレードできないままサポート期間が終了してしまいます。サポート期間が終了するといかなる理由であっても更新は配布されないのでそのままアップグレードできないOSができあがります。

で、今になってブログを書いていることから分かるとおり、サポート終了までに解決策は見出せず、ZFSを使っている人たちはアップグレード手段を奪われたまま24.10に囚われたままになってしまいました。

## 自助救済

サポート終了しているということはセキュリティアップデートもないのでこのまま使い続けるのは無理があります。どうにかして自力でアップグレードをします。

↑のバグ報告のスレッドにも何人か自力でアップグレードを試みた人がいます。
新しいzfsツールとdkmsを同時に用意して〜とか、一度アップグレードしてフリーズするとこまで持っていったあとに手動コマンドで新しいカーネルを起動して〜とか、普通は推奨されなそうな方法がバンバン飛び交っています。

私はというと、 `debootstrap` でシステム部分だけ入れ直しました。
pluckyのインストールメディアから起動して `/mnt` に元使っていたシステムをマウントして、 `debootstrap` …でできたらよかったんですが競合するファイルがあるので `/usr` と `/etc` を消してしてから `debootstrap` しました。後になって考えたらこれがよくなかった。
BSDとかならユーザの変更は `/usr/local/` にあるんですがUbuntuだとシステムの変更もユーザの変更もまとめて `/usr` や `/etc` にあるので設定が中途半端になったパッケージが続出しました。

`debootstrap` して `ubuntu-desktop` したあとは普通に起動…と思いきや上手くいきませんでした。`/usr` と `/etc` を消し飛ばしたのが原因なのか、あるいは後述するようにzsysが死んでて正常な動作が阻害されてたのかは分かりません。とりあえずやばそうなところを直したりして起動までもっていけました。

`systemctl enable xxx` で設定が消えたサービスを復元したり、 `snap disable firefox && snap enable firefox` でsnapの設定を復元したり、色々やりました。

`/home/` 以下は無傷だったのでそこは楽でしたね。

で、最終的に普通に使えるように復元できました。

<blockquote class="twitter-tweet" data-conversation="none"><p lang="ja" dir="ltr">インストールメディアからdebootstrapで強制的にアップグレードかけた。色々事故ったけど、多分どうにかなってる。 <a href="https://t.co/ttDirPEk3Q">pic.twitter.com/ttDirPEk3Q</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1955520471992270918?ref_src=twsrc%5Etfw">August 13, 2025</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

## zsysの死

自力アップデート作業の途中で、ZFSの自動スナップショットなどをやるzsysがPluckyで上手く動いていないことをに気付きました。

[Bug #2106501 “zsysd failing to start: panic: runtime error: inde...” : Bugs : zsys package : Ubuntu](https://bugs.launchpad.net/ubuntu/+source/zsys/+bug/2106501)

zsys自体更新があまりされていないので新しいZFSに対応していないのは納得できます(でもaptで入るパッケージなら動作確認くらいしててほしかったな)。これは原因が判明していてzsysが依存している `go-libzfs` のバグのようです。修正PRも出ていますが、取り込まれていません。

[Ignore unknown zpool properties by tilgovi · Pull Request #2 · ubuntu/go-libzfs](https://github.com/ubuntu/go-libzfs/pull/2)


zsysはaptでパッケージを更新する度に顔を出して、都度エラーを吐くのでアンインストールしてしまいました。今はスナップショットの仕組みがないまま使っています。
調べたらZFSには定期スナップショットをやってくれるパッケージが複数あるようなので移行していきたいですね。
