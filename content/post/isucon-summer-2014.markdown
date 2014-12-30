---
type: post
title: "isucon夏期講習2014に参加してきた"
date: 2014-08-21
comments: true
categories: [ISUCON]
---
レポート締切間近のκeenです。ISUCON夏期講習2014に参加してきたのでレポートです。とはいっても去年の夏期講習もISUCONも参加してるので目新しいことはないんですが。主な目的はメンバー集めです。
<!--more-->
# 流れ
+ LINEの採用情報:
   ISUCONがきっかけでLINEに内定貰った人がいるよう。因みに自分はLINE落ちた。
+ ISUCONの概要の説明:
  うん。知ってる。

   <iframe src="//www.slideshare.net/slideshow/embed_code/38160090" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="https://www.slideshare.net/tagomoris/isucon2014" title="ISUCONの話(夏期講習2014)" target="_blank">ISUCONの話(夏期講習2014)</a> </strong> from <strong><a href="http://www.slideshare.net/tagomoris" target="_blank">SATOSHI TAGOMORI</a></strong> </div>
+ 過去問を解く:
   去年の予選の問題だった。うん。知ってる。
+ パフォーマンス向上に関しての解説:
   これもISUCON反省会とかそんなんでkazeburoさんが話してた気がする
   
<iframe src="//www.slideshare.net/slideshow/embed_code/38162132" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="https://www.slideshare.net/kazeburo/isucon-summerclass2014action1" title=" Webアプリケーションの パフォーマンス向上のコツ 概要編" target="_blank"> Webアプリケーションの パフォーマンス向上のコツ 概要編</a> </strong> from <strong><a href="http://www.slideshare.net/kazeburo" target="_blank">Masahiro Nagano</a></strong> </div>
<iframe src="//www.slideshare.net/slideshow/embed_code/38163927" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="https://www.slideshare.net/kazeburo/isucon-summerclass2014action2" title=" Webアプリケーションの パフォーマンス向上のコツ 実践編" target="_blank"> Webアプリケーションの パフォーマンス向上のコツ 実践編</a> </strong> from <strong><a href="http://www.slideshare.net/kazeburo" target="_blank">Masahiro Nagano</a></strong> </div>

+ morisさんによるライブチューニング:
   失敗してた
+ 懇親会

# 自分の作業
前年度参加者がハイパフォーマンス叩き出して俺TUEEEEEEしても意味がないので次回のISUCONで試したいことを試して地雷踏むことにした。

## sshのRSAAuthenticationの設定
何故かハマる。

id_rsa.pubをauthorized\_keysに突っ込んでUserPAMの設定まで確認して`sudo service sshd restart`したのにパスワードを求められる。確か色々確認してもう一回sshdリスタートしたら通った気がする。原因不明。

そういや
    curl https://github.com/KeenS.keys >> .ssh/authorized_keys
のワンライナー使えば良かった。
## アプリケーションをgitで管理する
やらかす。

最初isucon/webapp/ruby/だけをgitで管理した。それだと足りないので次にisucon/全体をgitで管理しようとしたらisucon/webapp/ruby/がサブモジュール扱いされて困った。サブモジュール分かんね。色々試すもついぞisucon/のgitでisucon/webapp/ruby/を管理出来なかった。結局.git吹っ飛ばして再度`git init .`した。これは次回までの宿題ですね。

あと地味にisucon/init.shが.gitignoreされてて困った。

## サーバーマシンをgitサーバーにする
前回はbitbucketで管理してたがしばらく使わない内にパスワードを忘れてしまったのと(もしかしたら以前のバイト先のメールアドレスで登録してた?)pingが400msのbitbucketサーバー使うのに気が引けたのでサーバーマシンをgitサーバーにしようと決意。ハマる。

gitのマニュアルを適当に読んでたのが悪かった。ローカルで`git clone iscon@machine:isucon`してもなんか怒られてしばらく悩む。正解は、先ずリモートで`git clone --bare isucon isucon.git`してから`git clone iscon@machine:isucon.git`だった。

次にリモートで
    mv isucon isucon.back
    git clone isucon.git isucon
したら.gitignoreとかその辺の関係でベンチマークやら何やらが動かなくなる。自分でベンチマークをビルドする羽目に。

新たにcloneせずに
    cd isucon
    git remote add origin ../isucon.git
すれば良かったのだろうか。要調査。

## 細かなチューニング
markdownのバックエンドをrdiscountにしたり。これもGemfileで困る。結局あれはローカルで`bundle install --no-deploy`してから`git push`したら良かったのだろうか。これも宿題。

## インデックスを張る
isucon/config/init.sqlに

```sql
CREATE INDEX users_idx_username ON users (username);
CREATE INDEX memos_idx_id_is_private_created_at_id ON memos (is_private, created_at, id);
CREATE INDEX memos_idx_id_user_created_at ON memos (`user`, created_at);
```
を書いてisucon/init.shに

```sh
#!/bin/sh
set -e
mysql -u isu-user isucon < ./config/init.sql > log 2>&1
#データ投入後になにかしらの作業をしたい場合はこのシェルスクリプトに書いてください
```
を書くもエラーが出る。結局絶対パスで書いて2〜3回実行したら通ったのでパスの問題だったかと。しかし1回で通らなかったのは謎。

あとデバッグ大変だった。ベンチマークツールさん、初期化スクリプトでエラー出したらエラーログ吐いて下さいよ。

そういやリダイレクト先のlogファイルは結局行方不明のままだった。

## N+1クエリの解決
SQL書けなすぎてJOINとかFORCE INDEX書くのに戸惑った。一々書いてはmysqlのコンソールに貼ってエラーメッセージ読んでってやってた。すんなりやりたい。

## リバースプロキシをNginXにする
まずは

    sudo yum install nginx
    mv /etc/nginx/nginx.conf ./config/
    git add ./config/nginx.conf
    git commit -m 'add nignx.conf'
    sudo ln -s /home/isu-user/isucon/config/nginx.conf /etc/nginx/
    sudo service httpd stop
    sudo service nginx start
だったかな?最初`ln`に絶対パスを使ってなくてエラー出たりしたけど。

んで
```nginx
location / {
   proxy_pass   http://localhost:5000;
}
```
だけ書くも`404 http://localhost:5000/なんちゃら Not Found`が出る。そういや、とプロキシヘッダを追加。それでも画像などはとれない。多分rootをいじらなかったのが原因。次の手で静的ファイルをNginXでキャッシュするように設定したら直った。

あまり記録残してないけどこの辺で2900点くらいで首位だった気がする。因みに初期値は1700くらい。

## Varnishを噛ませる
前回、ベンチマークツールのチェックが甘くてVarnishを使うだけでスコアが跳ね上がるというのを知っていたのでちょっとチート臭いけどどうせ本番でも必要だしということで練習もかねて使う。

    sudo yum install varnish
    mv /etc/sysconfig/varnish ./config
    git add ./config/varnish
    git commit -m 'varnish'
    sudo ln -s /home/isu-user/isucon/config/varnish /etc/sysconfig/varnish
    mv /etc/varnish/default.vcl ./config
    git add ./config/default.vcl
    git commit -m 'default.vcl'
    sudo ln -s /home/isu-user/isucon/config/default.vcl /etc/varnish/default.vcl

的なことをやった。あとはvarnishを80番にしてバックエンドを4000番にして、NginXを4000番にした。それだけでスコアが14000くらいになった。varinish簡単だし優秀。

## my.cnfのチューニング
もはやvarnishが仕事してる所為でボトルネックはベンチマークツールなのだけど一応。

とはいってもinnodb\_buffer\_pool\_sizeを2GBから6GBにしただけ。因みに与えられたマシンのメモリは8GB。スコアは一応微増。でもまあ誤差の範囲内。

この辺でタイムオーバー。

# 懇親会
目の前の人がClojure推しの人でだいたいそんな話してた。

チームメンバー捜しは席の周りの人が当日出れない人ばかりだったのと席替えがなかったので失敗に終わる。誰か学生枠で一緒に出ましょうよ。学生賞目指しましょうよ。

# やり残したこととか反省とか
* ローカルで変更→commit→push→リモートでpull→再起動→ベンチマークのワークフローが結構面倒だったしpull忘れ、再起動忘れがあったのでpushより後を自動化したい。出来ればgitのコミットログとベンチマークのスコアをペアで保存しながら。hook使ったら割と出来そうな気はするんだけどなあ。
* 折角サーバー与えられたんだしMySQLのslow logを吐かせるの一回練習しとけば良かった。
* 結局capistrano使わなかった。複数台構成なら必須だし復習しておきたかったのに。
* メンバー捜したかった。

マジでメンバー捜してます。学生枠で出たい人@blackenedgoldまでお願いします。
