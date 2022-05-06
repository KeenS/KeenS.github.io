---
categories: [Ubuntu]
date: 2022-04-23T19:38:42+09:00
title: "Ubuntu 22.04に上げたときに起きた問題と解決"
---

κeenです。昨日Ubuntu 22.04に上げたら色々問題が起きたので私がやった解決策を残しておきます。

<!--more-->


## グラフィックドライバの問題(多分)
### 問題
dist upgrade時にグラフィックドライバ系のパッケージでインストールエラーが発生しました。
これは恐らく私がUbuntu標準でないAMDが配布しているドライバをインストールしていたからだと思います。
ただ、そのせいか再起動するとGUIが立ち上がらず、画面が黒いままになってしまいました。

### 解決
一旦AMDのグラフィックドライバをアンインストールして、GUIに関係ありそうな `gdm3` や `ubuntu-session` 、`mutter` などを再インストールしました。

アンインストールは、黒い画面で `Ctrl + Alt + F1~F12`` のどれかを押すとコンソールに行けるのでそこでログインしてターミナルで操作しました。
以下のようなコマンドで洗い出せると思うので目視判断しつつ削除しました。

```sh
LANG=C apt search amdgpu | grep installed  
```

そのあと再起動すると無事GUIが起動しました。
AMDのグラフィックドライバのアンインストールとデスクトップ環境の最インストールを両方やっているのでどっちが効いたのか分かりませんが、恐らくドライバの問題だと思います。

## Firefoxの設定が吹き飛んでた
### 問題

Firefoxを起動すると前世の記憶を忘れてようこそ画面を表示し、ブックマークもcookieもアドオンもなくなっていました。

### 解決

これはFirefoxの管理がsnapに移行した関係で設定ファイルの場所が変わっていたからです。
パスが `~/.mozilla` から ` ~/snap/firefox/common/.mozilla` に移動しているので、一旦Firefoxを落として設定ファイルを移植してあげれば前世の記憶を取り戻します。

```sh
rm -rf ~/snap/firefox/common/.mozilla
cp -R ~/.mozilla ~/snap/firefox/common/.mozilla
```

最初シンボリックリンクを貼ったのですが多重起動判定されてFirefoxが立ち上がりませんでした。まあ、気持は分からんでもない。

## Firefoxでファイルをダウンロードできない
### 問題

Firefoxでファイルのダウンロード系の操作をしても無言で何も動作しませんでした。syslogを見たりCLIから起動して標準出力を見たりすると以下のようなメッセージが出ています。

``` sh
Can't open portal file chooser: GDBus.Error:org.freedesktop.DBus.Error.ServiceUnknown: The name org.freedesktop.portal.Desktop was not provided by any .service files
```

### 解決

あんまりちゃんと把握してないですが、 `xdg-desktop-portal` のパッケージがないとこうなるらしいです。パッケージをインストールして再起動すると解決しました。

``` sh
sudo apt install xdg-desktop-portal-gnome
```

## Alacrittyでxremapが効かない
### 問題

xremapによるキーリマップから外してるはずのAlacrittyでキーリマップがされてしまい、思ったように操作されませんでした。

### 解決

恐らくですがAlacrittyがWaylandで正しくウィンドウクラスを設定できておらず(タイトルも空で真っ白)、ウィンドウクラスでアプリケーションを判別しているxremapが動かなかったようです。
これは一旦WaylandをやめてXorgを使うことにして解決しました。

ウィンドウシステムをWaylandからX11にするにはログイン時にユーザを選択したあと、パスワード入力画面で右下にちっちゃく表示されている歯車アイコンをクリックしてUbuntu on Xorgを選択するとできます。

## uimで日本語が打てない
### 問題
ほとんどのアプリケーションで日本語が打てず、アルファベットのみになる。キーを押しても有効にならない。

### 解決
Ubuntu 22.04で多くのアプリケーションがGTK4に移行した関係でした。
[uimはGTK4サポートがまだ](https://github.com/uim/uim/issues/173)なので動かないという訳です。
ちゃんとissueとかは調べてませんがibusも動かなかったので同様だと思います。

fcitx5を使うようにすると解決しました。

## Joplinが起動しない
### 問題

アプリケーションからJoplinを指定して実行しても何も起こらずに起動をやめてしまう。

### 解決

Joplinに限らずですが、AppImage系アプリの問題のようです。AppImageの展開が変わったっぽいです。以下のURLの手順に従うと動きました。

https://github.com/AppImage/AppImageKit/wiki/FUSE
