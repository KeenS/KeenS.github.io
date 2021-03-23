---
categories: [Yubikey, OpenPGP, GPG]
date: 2021-03-23T12:18:07+09:00
title: "Yubikeyを使う：OpenPG編"
---

κeenです。最近Yubikeyを買ったので色々試しています。今回はそのうちのPGP回です。

<!--more-->

# Yubikeyについて
[Yubikey](https://securitykey.yubion.com)は米瑞企業のYubico社が販売している認証デバイスです。FIDOやらWebAuthnやらの文脈で耳にした方も多いんじゃないしょうか。Yubikeyは日本ではソフト技研社が販売代理店をしています。

Yubikeyはラインナップがいくつかありますが私が買ったのはYubikey 5 NFCです。

Yubikeyでできることは色々あります。

* FIDO U2F
* FIDO/WebAuthn
* Challenge and Response
* OATH-TOTP / OATH-HOTP
* Yubico OTP
* PIV
* OpenPGP
* 静的パスワード

参考：[Yubikey 5をArchLinuxで使う - Qiita](https://qiita.com/onokatio/items/15108b9c199ccf378f91)

このうち今回はOpenPGPサポートの機能を使います。

# OpenPGPについて

OpenPGPはデータの完全性を提供するためのプロトコルです。[RFC4880](https://tools.ietf.org/html/rfc4880)で標準化されています。主な用途は電子署名や暗号化などでしょうか。その他頑張ればSSHの認証にも使えるようです。私は以下の用途に使っています。

* [Gitのコミットの署名](https://git-scm.com/book/ja/v2/Git-のさまざまなツール-作業内容への署名)
* メールの署名
  + CF [「Thunderbird」がメール暗号化・デジタル署名「OpenPGP」を標準サポートへ - 窓の杜](https://forest.watch.impress.co.jp/docs/news/1211933.html)
* [keybase](https://keybase.io)での紐付け

Gitのコミットの署名について補足しておきましょう。Gitに設定するユーザ名やメールアドレスは自己申告なので簡単に詐称可能です。さらにGitLabやGitHubはそのメールアドレスでコミットをアカウントに紐付けるので偽装コミットが簡単に作れます。署名をしておけばそういう偽装を防げます。GitLabやGitHubもOpenPGPの公開鍵を登録しておけばコミットが本人により署名されたものかを確認してくれる機能がついています。

OpenPGPの用途は他にも私が公開している鍵を使って他の人が私に暗号化したファイルを送ったり（現実的には暗号化メールかな）できるのですが、まだ暗号化ファイルを受け取った経験はありません。

OpenPGPを使うときのツールはLinuxなら[Gnu Privacy Guard](https://gnupg.org)（GPG）を使うことになるでしょう。この記事でもGPGを前提として紹介していきます。

# YubikeyとOpenPGPについて

OpenPGPでは公開鍵暗号を使うので公開鍵と秘密鍵があります。公開鍵はインターネットに公開し、秘密鍵を自分しか知らないように大切に保管します。SSHと雰囲気が似てますね。ですがOpenPGPの鍵は所有者本人に結びつくのでSSHの秘密鍵以上に扱いに注意が必要です。OpenPGPの鍵はそれが本人と結び付いていることを周りに信用してもらってはじめて意味を持ちます。そのためには利用実績の積み重ねと本人による適切な秘密鍵の管理が不可欠です。そして秘密鍵は電脳空間で本人そのものになるので秘密鍵が流出すると自身の人格が乗っ取られることに等しいです。そういった点で流出したら接続先マシンから公開鍵を削除して鍵ペアを作り直せばいいSSH鍵とは扱いが異なります。

なのでOpenPGPの秘密鍵を安全に保管したいというモチベーションがあり、そのための機能を実際に提供するのがYubikeyのOpenPGPサポートです。Yubikeyのデバイス内にOpenPGPの鍵を焼き込み、二度と外に出さない運用が可能です。ということでYubikeyにOpenPGPの鍵を焼き込みましょう。

# OpenPGPの機能と鍵

鍵を焼く前にOpenPGPにどんな鍵があり、どう使うのか押えましょう。OpenPGPを使うときにはいくつかの種類の公開暗号鍵対を使い分けます。

## 鍵の機能

OpenPGPでは鍵に機能を割り当てることができます。仕様上は細かくいくつか分かれているようですが、現実には以下の三つが使われます。

* 認証
* 署名
* 暗号化

## 鍵の失効と有効期限

もし万が一秘密鍵が流出したときのために鍵を失効することができます。そのときは秘密鍵で失効証明書というのを発行し、それを配ることで周りの人に以前公開した公開鍵を失効させることを宣言できます。

また、鍵には有効期限が設定できます。もし万が一秘密鍵を紛失した場合には失効証明書が発行できないので既に発行した公開鍵をどうすることもできなくなります。そういうときに有効期限があれば勝手に失効してくれるので知らずに意味のない鍵を使われる可能性が減ります。有効期限はあとから延長することもできるので、1年くらいに設定して毎年延長する運用するスタイルもあるようです。

## 主鍵と副鍵

私も最初混乱したのですが、OpenPGPには主鍵と副鍵があるようです。副鍵は主鍵からいくらでも作ることができます。副鍵は主鍵に紐付いていて、主鍵とは独立して鍵の有効期限を設定したり失効したりできます。主鍵も副鍵も公開暗号鍵対です。なので厳密には主鍵"対"、副鍵"対"ですし、主鍵の公開鍵、主鍵の秘密鍵、副鍵1の公開鍵、副鍵1の秘密鍵、副鍵2の…と色々な鍵が登場します。

## 鍵の実際の運用

よくある運用としては主鍵に認証と署名の機能を持たせて有効期限を無期限にし、副鍵に暗号化の機能を持たせて有効期限を設定することが多いようです。
認証と署名は本人に紐付くため一生に一つあれば十分なので有効期限は無期限で問題ありません。

主鍵の失効証明書を事前に発行して保管しておきます。
もし鍵が流出したときは気付いた時点で失効証明書をみんなに配れば被害は最小限で済みます。
秘密鍵が流出しても、流出前に行った署名が無効になったり偽造されるようになったりましませんからね。

一方、暗号化は[前方秘匿性](https://ja.wikipedia.org/wiki/Forward_secrecy)の観点から流出した時点で被害が最大になります。
すなわち、秘密鍵を手にした人は過去に自分向けに暗号化されたデータ全てを復号できるようになってしまいます。
そこで鍵の有効期限は1年程度にして毎年鍵を交換することで被害を小さくできます。
副鍵は主鍵に紐付いていて、鍵の信用は主鍵が担保してくれるので副鍵はある程度頻繁に替えても問題ないという寸法です。

色々書きましたが以下の2つ（4つ）の鍵があること押えて下さい

* 署名と認証に使う主鍵（の秘密鍵と公開鍵）
* 暗号化に使う副鍵（の秘密鍵と公開鍵）

### 運用と保管

鍵は盗まれてはならないし失くしてもならないけど普段気軽に使えるようにしないといけません。
この辺の匙加減は個々人に任されるころではあります。
例えばローカルマシン内は安全としてそのまま保存してしまう場合もあります（そのままとはいってもパスワードで保護されていますが）。
あるいはインターネットに繋ったマシンは全て危険として主鍵の秘密鍵を紙、あるいはUSBメモリなどに書いて家の中で大切に保管し、ローカルマシンには副鍵のみ置く場合もあります。そういう場合は一度もインターネットに繋がったことのないマシン（Live CDとか）の上でOpenPGP鍵を生成するようです。

今回の我々にはYubikeyがあるので考えることが少なくて済みます。
インターネットに繋っているマシンは安全でないとして、普段使いにはYubikeyに保存した主鍵と副鍵（の秘密鍵）を、バックアップとしてUSBメモリに主鍵と副鍵の秘密鍵のコピーを保存します。
これでインターネットから秘密鍵がアクセスされることはなく安全に使えるようになります。
まあ、作業は普段使ってるインターネットにバリバリで繋がってるマシンでやるので徹底はしてませんけどね。気になる方は上手いことやって下さい。
秘密鍵を保存したUSBメモリはインターネットでの自身の人格に相当するので門外不出に安全に管理し、火事で焼け出されるときも通帳や印鑑よりも優先して持ち出します。


# 鍵の生成からYubikeyへの焼き込みまで

ちょっと前置きが長かったのでやることをおさらいしておきましょう。

* (事前準備)
* 署名・認証の主鍵と暗号化の副鍵を生成する
  + このとき副鍵の有効期限を1年にする
* 主鍵、副鍵のバックアップをUSBメモリに置く
* 主鍵、副鍵をYubikeyに焼き込む

これらの作業をやっていきます。Yubicoの公式ドキュメントも併読しながら進んで下さい。

[Using Your YubiKey with OpenPGP – Yubico](https://support.yubico.com/hc/en-us/articles/360013790259-Using-Your-YubiKey-with-OpenPGP)

## 事前準備

YubikeyをOpenPGP鍵のストアとして使うためにscdaemonをインストールしておきます。

```shell
$ sudo apt install scdaemon
```

どうやらOpenPGP鍵を安全に保管できるデバイスはスマートカードと呼称されるらしいです。


## 鍵の生成

OpenPGP鍵は `gpg` コマンドで作成できます。このとき生成する鍵のアルゴリズムはYubikeyに保存できるものを選ばないといけません。Yubikey 5ならRSA 4096 をサポートしている他、楕円曲線暗号も使えます。

See also: [YubiKey 5.2.3 Enhancements to OpenPGP 3.4 Support – Yubico](https://support.yubico.com/hc/en-us/articles/360016649139-YubiKey-5-2-3-Enhancements-to-OpenPGP-3-4-Support)


ここではセキュリティビット数が256bit相当（RSA 4096より多い）ある `nistp521` を使うことにします。以下のようにダイアログを進めていきます。

1. `gpg --expert --full-gen-key` で鍵の生成を開始
2. `9` で鍵の種類に楕円曲線暗号（ECC）を選択
3. `5` で楕円曲線にNIST P-521を選択
4. `0` で有効期限に無期限を選択
5. `y` で了承
6. 本名、メールアドレス、コメントなどを入力
7. `O` で了承
8. マウスやキーボードを動作させてエントロピーを生成
9. 鍵のパスワードを設定

鍵のパスワードは長いものを設定しておきましょう。コンソールではこうなります。

```text
$ gpg --expert --full-gen-key
gpg (GnuPG) 2.2.20; Copyright (C) 2020 Free Software Foundation, Inc.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

ご希望の鍵の種類を選択してください:
   (1) RSA と RSA (デフォルト)
   (2) DSA と Elgamal
   (3) DSA (署名のみ)
   (4) RSA (署名のみ)
   (7) DSA (機能をあなた自身で設定)
   (8) RSA (機能をあなた自身で設定)
   (9) ECC と ECC
  (10) ECC (署名のみ)
  (11) ECC (機能をあなた自身で設定)
  (13) 既存の鍵
  (14) カードに存在する鍵
あなたの選択は? 9
ご希望の楕円曲線を選択してください:
   (1) Curve 25519
   (3) NIST P-256
   (4) NIST P-384
   (5) NIST P-521
   (6) Brainpool P-256
   (7) Brainpool P-384
   (8) Brainpool P-512
   (9) secp256k1
あなたの選択は? 5
鍵の有効期限を指定してください。
         0 = 鍵は無期限
      <n>  = 鍵は n 日間で期限切れ
      <n>w = 鍵は n 週間で期限切れ
      <n>m = 鍵は n か月間で期限切れ
      <n>y = 鍵は n 年間で期限切れ
鍵の有効期間は? (0) 0
鍵は無期限です
これで正しいですか? (y/N) y

GnuPGはあなたの鍵を識別するためにユーザIDを構成する必要があります。

本名: 田中太郎
電子メール・アドレス: taro@tanaka.town
コメント:
あなたは文字集合'utf-8'を使っています。
次のユーザIDを選択しました:
    "田中太郎 <taro@tanaka.town>"

名前(N)、コメント(C)、電子メール(E)の変更、またはOK(O)か終了(Q)?
たくさんのランダム・バイトの生成が必要です。キーボードを打つ、マウスを動か
す、ディスクにアクセスするなどの他の操作を素数生成の間に行うことで、乱数生
成器に十分なエントロピーを供給する機会を与えることができます。
たくさんのランダム・バイトの生成が必要です。キーボードを打つ、マウスを動か
す、ディスクにアクセスするなどの他の操作を素数生成の間に行うことで、乱数生
成器に十分なエントロピーを供給する機会を与えることができます。
gpg: 鍵83AD9B71D78B7AA7を究極的に信用するよう記録しました
gpg: 失効証明書を '/home/shun/.gnupg/openpgp-revocs.d/094ACC65CE981E96AA063B0483AD9B71D78B7AA7.rev' に保管しました。
公開鍵と秘密鍵を作成し、署名しました。

pub   nistp521 2021-03-23 [SC]
      094ACC65CE981E96AA063B0483AD9B71D78B7AA7
uid                      田中太郎 <taro@tanaka.town>
sub   nistp521 2021-03-23 [E]
```


これで署名・認証の主鍵と暗号化の副鍵が生成されました。しかし有効期限は両方とも無期限です。副鍵を編集して有効期限を1年にします。今生成された鍵の指紋が `094ACC65CE981E96AA063B0483AD9B71D78B7AA7` なのでこれを指定して編集します。

1. `gpg --edit-key 094ACC65CE981E96AA063B0483AD9B71D78B7AA7` で鍵の編集を開始
2. `key 1` で暗号化用の副鍵を選択
3. `expire` で有効期限の編集を開始
  + `1y` で1年を指定、`y` で了承
4. `save` で編集を保存

コンソールではこうなります。

```text
$ gpg --edit-key 094ACC65CE981E96AA063B0483AD9B71D78B7AA7
gpg (GnuPG) 2.2.20; Copyright (C) 2020 Free Software Foundation, Inc.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

秘密鍵が利用できます。

sec  nistp521/83AD9B71D78B7AA7
     作成: 2021-03-23  有効期限: 無期限       利用法: SC
     信用: 究極        有効性: 究極
ssb  nistp521/21D1788C632258F9
     作成: 2021-03-23  有効期限: 無期限       利用法: E
[  究極  ] (1). 田中太郎 <taro@tanaka.town>

gpg> key 1

sec  nistp521/83AD9B71D78B7AA7
     作成: 2021-03-23  有効期限: 無期限       利用法: SC
     信用: 究極        有効性: 究極
ssb* nistp521/21D1788C632258F9
     作成: 2021-03-23  有効期限: 無期限       利用法: E
[  究極  ] (1). 田中太郎 <taro@tanaka.town>

gpg> expire
副鍵の有効期限を変更します。
鍵の有効期限を指定してください。
         0 = 鍵は無期限
      <n>  = 鍵は n 日間で期限切れ
      <n>w = 鍵は n 週間で期限切れ
      <n>m = 鍵は n か月間で期限切れ
      <n>y = 鍵は n 年間で期限切れ
鍵の有効期間は? (0) 1y
鍵は2022年03月23日 09時46分25秒 JSTで期限切れとなります
これで正しいですか? (y/N) y

sec  nistp521/83AD9B71D78B7AA7
     作成: 2021-03-23  有効期限: 無期限       利用法: SC
     信用: 究極        有効性: 究極
ssb* nistp521/21D1788C632258F9
     作成: 2021-03-23  有効期限: 2022-03-23  利用法: E
[  究極  ] (1). 田中太郎 <taro@tanaka.town>

gpg> save
```


有効期限の編集くらいならGUIクライアントからでもできます。そっちが好みの方はseahorseとかで編集してみて下さい。

また、ユーザ情報は複数登録できます。プライベートと仕事などで複数のメールアドレスを使い分けたいケースなどで有用ですね。ユーザ情報を追加で登録する場合は  `gpg --edit-key <key id>` から `adduid` を選択して下さい。あるいはGUIからやるならseahorseでポチポチと追加できます。

これで鍵の生成は完了です。

## 鍵のバックアップをとる

OpenPGP鍵をYubikeyに焼くと二度とYubikeyの外に取り出せなくなります。あらかじめバックアップをとっておきましょう。この作業は一時的に鍵が書き出されるので細心の注意を払いながら進めます。

まず念のためにtmpfsを用意します。見た目上ファイルを消してもディスクにはデータが残っていることがあります。特に私の使っているbtrfsだとCopy on Writeなので `shred` コマンドなどで消しても実際にはディスクに残ります。すると安全な場所に保管しているはずの秘密鍵が実は安全でない場所に残り続けていたという事態になりかねません。そういった問題を避けるためにtmpfs上で作業してデータをディスク上に残さないようにします。

tmpfsをマウントします。

```shell
$ mkdir workspace
$ sudo mount -t tmpfs tmpfs workspace
$ cd workspace
```

tmpfs上で秘密鍵をエクスポートします。

```shell
$ gpg --armor --export-secret-keys 094ACC65CE981E96AA063B0483AD9B71D78B7AA7 > secret_key.gpg
```

一旦この鍵をインポートできるか確認しておきます。

```shell
$ gpg --import secret_key.gpg
gpg: 鍵83AD9B71D78B7AA7:"田中太郎 <taro@tanaka.town>"変更なし
gpg: 鍵83AD9B71D78B7AA7: 秘密鍵をインポートしました
gpg: 処理数の合計: 1
gpg:              変更なし: 1
gpg:       秘密鍵の読み込み: 1
gpg:  無変更の秘密鍵: 1
```

よさそうなら手持ちのUSBメモリに保存しておいて下さい。鍵はパスワードで保護されているのでこのまま保管して大丈夫でしょう。USBメモリの不良セクタとかが怖ければ同じファイルをUSB内の2箇所に保存しておけばリスクヘッジできるんじゃないですかね。これは他人が触れない場所に大切に保管します。

終わったら後片づけをします。

```shell
$ shred secret_key.gpg
$ cd ../
$ sudo umount workspace
```

これで鍵のバックアップは完了です

## 秘密鍵をYubikeyに焼く

それでは秘密鍵をYubikeyに焼きます。
と、その前にYubikeyのPINを変更しておきましょう。PINがデフォルトのままだと危ないですからね。
PINはデフォルトで123456、管理者PINはデフォルトで12345678だそうです。
これを変更します。
この操作も `gpg` でできるようです。
YubikeyをPCに挿して以下の操作を行います。

1. `gpg --edit-card` でYubikeyの編集を開始
2. `passwd` でPINの変更
  + 現在のPIN（123456）、新しいPIN、新しいPINの確認を入力
3. `admin` で管理者用の編集モードに入る
4. `passwd` でPINの変更。管理者用なので複数のメニューが出る。
5. `3` で管理者用のPINを変更
  + 現在の管理者PIN（12345678）、新しい管理者PIN、新しい管理者PINの確認を入力
6. `Q` でパスワードの変更完了
7. `quit` でYubikeyの編集完了

コンソールだとこうなります。


```shell
$ gpg --edit-card
gpg/card> passwd
gpg: OpenPGPカードno. D2760001240103040006120841120000を検出
PIN changed.

gpg/card> admin
管理者コマンドが許可されています

gpg/card> passwd
gpg: OpenPGPカードno. D2760001240103040006120841120000を検出

1 - change PIN
2 - unblock PIN
3 - change Admin PIN
4 - set the Reset Code
Q - quit

あなたの選択は? 3
PIN changed.

1 - change PIN
2 - unblock PIN
3 - change Admin PIN
4 - set the Reset Code
Q - quit

あなたの選択は? Q

gpg/card> quit
```

その他Reset PINもあるので必要に応じて設定しましょう。


それでは準備の万端が整ったので本当に秘密鍵をYubikeyに焼きます。Yubikeyには署名、認証、暗号化それぞれにスロットがあるようなので個別に焼きます。
YubikeyをPCに挿したまま以下の操作を行います。

1. `gpg --edit-key` で鍵の編集を開始
2. `keytocard` で主鍵をYubikeyに移動させる
  + `1` を選んで署名鍵を選択
  + （たしか）管理者PINが求められる
3. `keytocard` で主鍵をYubikeyに移動させる
  + `3` を選んで認証鍵を選択
  + （たしか）管理者PINが求められる
4. `key 1` で暗号化用の副鍵を選択
5. `keytocard` で副鍵をYubikeyに移動させる
  + `2` を選んで認証鍵を選択
  + （たしか）管理者PINが求められる
6. `quit` で終了
  + `y` 了承

コンソールだとこうなります。昔焼いたときのログなので上記とは鍵の指紋やアルゴリズムが異なりますがやることは変わらないので別にいでしょう。

```shell
$ gpg --edit-key 01FEF2424389F000BE9A7DA529B9340D65C60646
gpg (GnuPG) 2.2.20; Copyright (C) 2020 Free Software Foundation, Inc.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

秘密鍵が利用できます。

sec  rsa4096/29B9340D65C60646
     作成: 2021-02-28  有効期限: 無期限       利用法: SC
     信用: 究極        有効性: 究極
ssb  rsa4096/A7F529BF0FCD8B54
     作成: 2021-02-28  有効期限: 無期限       利用法: E
[  究極  ] (1). Peter George Phill <pgp@example.com>

gpg> keytocard
この主鍵を本当に移動しますか? (y/N) y
鍵を保管する場所を選択してください:
   (1) 署名鍵
   (3) 認証鍵
あなたの選択は? 1

sec  rsa4096/29B9340D65C60646
     作成: 2021-02-28  有効期限: 無期限       利用法: SC
     信用: 究極        有効性: 究極
ssb  rsa4096/A7F529BF0FCD8B54
     作成: 2021-02-28  有効期限: 無期限       利用法: E
[  究極  ] (1). Peter George Phill <pgp@example.com>

gpg> keytocard
この主鍵を本当に移動しますか? (y/N) y
鍵を保管する場所を選択してください:
   (1) 署名鍵
   (3) 認証鍵
あなたの選択は? 3

sec  rsa4096/29B9340D65C60646
     作成: 2021-02-28  有効期限: 無期限       利用法: SC
     信用: 究極        有効性: 究極
ssb  rsa4096/A7F529BF0FCD8B54
     作成: 2021-02-28  有効期限: 無期限       利用法: E
[  究極  ] (1). Peter George Phill <pgp@example.com>

gpg> key 1

sec  rsa4096/29B9340D65C60646
     作成: 2021-02-28  有効期限: 無期限       利用法: SC
     信用: 究極        有効性: 究極
ssb* rsa4096/A7F529BF0FCD8B54
     作成: 2021-02-28  有効期限: 無期限       利用法: E
[  究極  ] (1). Peter George Phill <pgp@example.com>

gpg> keytocard
鍵を保管する場所を選択してください:
   (2) 暗号化鍵
あなたの選択は? 2

sec  rsa4096/29B9340D65C60646
     作成: 2021-02-28  有効期限: 無期限       利用法: SC
     信用: 究極        有効性: 究極
ssb* rsa4096/A7F529BF0FCD8B54
     作成: 2021-02-28  有効期限: 無期限       利用法: E
[  究極  ] (1). Peter George Phill <pgp@example.com>

gpg> quit
変更を保存しますか? (y/N) y
```


ちゃんと焼けているか確認しましょう。適当なファイルに署名をしてみます。Yubikeyが挿さっているときに署名できること、Yubikeyが挿さっていないときに署名ができないことを確認します。

```shell
$ echo test > test.txt
$ gpg --sign test.txt
$ rm test.txt.gpg
```

これでYubikeyが挿さっているときにPINが求められ、Yubikeyが挿さっていないときにカードがないよってエラーが出たら成功です。


# 運用

私はOpenPGPを多少は使っていますが扱いに失敗すると財産や生命が危険に晒されるような用途には使っていません。遊びというか、失敗しても笑えるくらいの用途でのみ使っています。内容については一切の責任を負わないので参考にする際は自己責任でお願いします。
私のブログより信頼できる情報としてGPGのハンドブックを挙げておきます。

[The GNU Privacy Handbook](https://gnupg.org/gph/en/manual.html)

## 鍵サーバ

OpenPGPには鍵サーバというのがあって、みんなが公開鍵を登録できるらしいです。が、私は特に登録してないです。興味のある方はGPGが運営している鍵サーバを覗いてみて下さい。

[OpenPGP Keyserver](http://keys.gnupg.net/)

## Keybase

コマンドで鍵を登録できます。

```shell
$ keybase pgp update --all
```

## Thunderbird

Thunderbird 78からPGPによる署名に対応しました。メールアカウントの \[設定\] → \[エンドツーエンド暗号化\]のOpenPGPの項目からGnuPG経由で外部の鍵を取り込んで、署名に取り込んだ鍵を指定します。

署名する鍵に登録したメールアドレスの中にメールアカウントのメールアドレスが入ってないといけないことに注意して下さい。

## GIT

私はgitのコミットに署名しているので以下の設定を `~/.gitconfig` に書いています。`signingkey` に渡す値は自分の鍵のIDか指紋を指定して下さい。

```text
[user]
    # ...
    signingkey = 2F3944FD72D3F1BD590C7309F88CE432DCC5FCBC
[credential]
    helper = seahorse
[commit]
    gpgsign = true
```

これを設定しておくとPCにログインして最初の `git commit` のときにPINが求められます。以後は入力しなくてもコミットできます。

GitLabやGitHubには公開鍵を登録できるので手動で登録します。まず `gpg` コマンドで公開鍵を出力します。

```shell
$ gpg --armor --export 2F3944FD72D3F1BD590C7309F88CE432DCC5FCBC > public_key.gpg
```

この `public_key.gpg` の中身をそれぞれのサービスに登録します。先述の鍵サーバからとってきてくれたら嬉しかったんですがそういう機能はないので手でコピペです。

* GitLab: [preferencesのGPGキー](https://gitlab.com/-/profile/gpg_keys)に鍵を貼って登録
* GitHub: [settingsのSSH and GPG Keys](https://github.com/settings/keys)からNew GPG Keyで登録


## Yubikeyの扱い

Yubikeyを普段どう使っているかについて紹介します。

まずYubikeyはどのくらい取り扱い注意が必要か考えます。
ソフトウェア的には秘密鍵を取り出せないので、離席して一瞬同僚が触れる環境に置いたり、うっかり落して後ろの人に「落としましたよ」って声をかけられるくらいなら大丈夫じゃないですかね。
しかし秘密鍵が何らかの形でICに焼き込まれてるはずなので悪い人の手に渡って被膜を剥がれてなんか特殊な機械で読み取られたらマズいんじゃないかなと思ってます。
つまり、「他人に触られてもいいけど失くすのはダメ」のルールで取り扱います。
実際の運用としては家の鍵と同じくらいの扱いかなと思ったのでキーホルダーで家の鍵とまとめて使っています。

そもそもYubikeyはOpenPGP秘密鍵の絶対的に安全な保管場所なのか何なのかですが、私は「比較的安全かつ便利に使える方法でOpenPGP秘密鍵を持ち運ぶツール」として使っています。
Yubikeyを家の中でしか使わないという人なら安全な保管場所としても使えるんじゃないですかね。

次に使い方です。私のデスクトップPCの筐体が微妙に遠くにあって抜き差しがちょっと手間なので机の上にUSBを延長してそこに挿しています。具体的にはこれを使っています。

[ヨドバシ.com - Digio デジオ UH-C3143BK \[USB3.1 Type-C 3ポートハブ 120cm ブラック\] 通販【全品無料配達】](https://www.yodobashi.com/product/100000001004019325/)

USB Type-Aの延長はType-CからType-Aでないとできないようだったのでそういうケーブルを探しました。1つだけUSB Type-CのポートがあるPCでよかった。

## Yubikeyを失くしたら

もし鍵が家の中で失くなった、海に落とした、あるいはYubikeyが壊れて使えなくなったならYubikeyを買い直してバックアップの秘密鍵を再度Yubikeyに焼けばよいです。

誰かの手に渡ったら先述のとおり悪い人に鍵を読み出されてしまうかもしれません。
そうでなくてもたまたまPINを当てられて勝手に使われる可能性もあります。
安全のため失効させましょう。

もしOpenPGP鍵を生成したPCをそのまま使っているなら `~/.gnupg/openpgp-revocs.d/` 以下に失効証明書が残っているはずです。それを `gpg --import` します。
残っていないならバックアップの秘密鍵から `gpg --gen-revoke` で失効証明書を発行してから `gpg --import` します。
どちらかのアクションを行うとOpenPGP鍵が失効状態になるはずなので公開鍵を公開したときと同じフローで失効状態の公開鍵を公開すればよいようです。

そのあとでYubikeyは買い直して再度セットアップをしましょう。


# まとめ

YubikeyとOpenPGPについて紹介しました。そしてYubikeyにOpenPGP鍵を登録して運用するようにしたときのログを公開しました。

諸々のリアルタイムな作業や調べもののログは下記のscrapに残してあるので必要な方は参考にして下さい。

[yubikeyを買ったので色々セットアップのメモ](https://zenn.dev/blackenedgold/scraps/1c44b845a6aa68)

# おまけ（鍵の移行）

今回私は新しい鍵を生成したのですが、Yubikeyを買う前から使っていたOpenPGP鍵も持っていました。しかしこれは一時期安全でない方法で保管していたので念のため退役させることにしました。
こういう、古い鍵も新しい鍵も揃っている場合に鍵の移行をする際にはtransition statementというのを発行することがあるようです。

[Creating a new GPG key with subkeys | Into.the.Void.](https://www.void.gr/kargig/blog/2013/12/02/creating-a-new-gpg-key-with-subkeys/)

私も発行してみました。

[κeenのtransition statement一式](https://keybase.pub/blackenedgold/transition-2021-03-23/)

ここには本体である `transition_statement.txt` と新旧鍵での署名 `sig1.txt` 、 `sig2.txt` があります。このtransition statementを私が書いたものであると確認するには以下のコマンドを使います（新旧鍵両方を持っている人向け）。

``` shell
$ gpg --verify sig1.txt transition_statement.txt
gpg: 2021年03月23日 12時10分31秒 JSTに施された署名
gpg:                RSA鍵866D76462E1140F5C6744CE90DB75EDC93CA52CBを使用
gpg: "κeen <3han5chou7@gmail.com>"からの正しい署名 [究極]
$ gpg --verify sig2.txt transition_statement.txt
gpg: 2021年03月23日 12時10分51秒 JSTに施された署名
gpg:                ECDSA鍵2F3944FD72D3F1BD590C7309F88CE432DCC5FCBCを使用
gpg: "Shimura Rin <3han5chou7@gmail.com>"からの正しい署名 [究極]
```


因みに署名は以下のコマンドで作成しました。

```shell
$ gpg --armor -b -u 866D76462E1140F5C6744CE90DB75EDC93CA52CB -o sig1.txt transition_statement.txt
$ gpg --armor -b -u 2F3944FD72D3F1BD590C7309F88CE432DCC5FCBC -o sig2.txt transition_statement.txt
```

