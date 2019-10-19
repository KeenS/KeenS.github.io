---
categories: ["SKK", "Ubuntu"]
date: 2019-10-20T00:21:47+09:00
title: "パッチを当ててuim-skkをUTF-8化する"
---
κeenです⸜( ¯⌓¯ )⸝。

ずっとuim-skkを使っていての困り事がユニコードの扱いでした。
uimの内部コーディングはEUC-JPなんですが、「⸜( ¯⌓¯ )⸝」に含まれる「⌓」がEUJ-JP外なので入力できないのです。そこでuim-skk内部や辞書をUTF-8化して入力できるようにしたお話です。

<!--more-->

偉そうに書いてますが本体はこのブログによるものです： [「𠁣」の文字をどのようにキーボード入力するか](http://harakire.tripod.com/junkies/non-bmp-keyb.html)

ここに書いてある通りでほぼ動くのですが、地味にパッチがコケたりですんなりとは動かなかったのでその作業メモです。

既に `sudo apt install uim uim-skk skkdic skkdic-extra` くらいは済んでる想定です。

# ブログのパッチを当てる

`/usr/share/uim` 以下にuimのSchemeファイルがあるのでそれらにパッチを当てます。
しかしパッチはuimの1.8.6用に作られてますが、Ubuntu 19.10だと1.8.8が入るので綺麗にパッチが当たりません。
そこで私がパッチを修正して1.8.8に当たるようにしたものを出しておきます。

これです。 https://gitlab.com/snippets/1905594

これを使って以下のスクリプトでえいやと変換できるはずです。

```sh
cd /usr/share/uim
for f in japanese-act.scm japanese-azik.scm japanese-custom.scm japanese-kzik.scm japanese.scm skk.scm skk-custom.scm
 do
  new="$(echo $f | sed 's/\.scm$/-utf8.scm/')"
  iconv -f EUC-JP -t UTF-8 < "$f" | sudo tee "$new"
done
wget -O /tmp/uim-skk-1.8.8-utf8.patch https://gitlab.com/snippets/1905594/raw
cat /tmp/uim-skk-1.8.8-utf8.patch | patch -p0 -b --follow-symlink
sudo mv skk.scm skk.scm.orig && sudo mv skk-utf8.scm skk.scm
sudo mv skk-custom.scm skk-custom.scm.orig && sudo mv skk-custom-utf8.scm skk-custom.scm
sudo rm japanese-custom-utf8.scm.orig japanese-utf8.scm.orig skk-utf8.scm.orig
```

ここで一旦再起動して、UIMでひらがなが入力できるかくらいは確認した方が良いでしょう。

# 辞書のUTF-8化

次に、既存のSKK辞書をUTF-8化したいです。

ここで問題になるのが私が使っているyaskkservです。
yaskkservはEUC-JPにハードコードされている[らしい](https://github.com/wachikun/yaskkserv/issues/2)のでyaskkservはやめて辞書ファイルに移行することにします。
uimは1つの辞書ファイルしかサポートしていないので辞書ファイルを結合する処理もします。

uim-のGUIから設定する人は[SKK 辞書]からskkservを使わないようにし、、テキストで設定する人は`~/.uim.d/customs/custom-skk-dict.scm` の `skk-use-skkserv` を `#f` に設定しましょう。

さて、これで辞書を変換にかかります。
`/usr/share/skk` に辞書ファイルが並んでる筈なので、一旦これらを `*.utf8` 変換したあと、全部入りの辞書ファイル `SKK-JISYO.all.utf8` を作ります。
そしてデフォルトの辞書を `/usr/share/skk/SKK-JISYO.all.utf8` にしておきましょう。

以下のスクリプトで一気にできるはずです。

```sh
cd /usr/share/skk
for f in SKK-JISYO.*; do
  iconv -f EUC-JP -t UTF-8 < "$f" | sudo tee "$f.utf8" > /dev/null
done
skkdic-expr2 *.utf8 | sudo tee SKK-JISYO.all.utf8 > /dev/null
sudo ln -sf /usr/share/skk/SKK-JISYO.all.utf8 /etc/alternatives/SKK-JISYO
```

`/usr/share` に書き込むのが嫌な人は `/usr/local/share` に作ってもいいでしょう。

あとは別途、 `~/.skk-jisyo` や `~/.skk-uim-jisyo` も変換しておいて下さい。

# 絵文字辞書のインストール

ここまで来ると絵文字辞書が使えるようになります。[skk-emoji-jisyo](https://github.com/uasi/skk-emoji-jisyo)とか[SKK-JISYO.emoji-ja](https://github.com/ymrl/SKK-JISYO.emoji-ja)とか。

これはさっきの辞書を変換するコマンドの引数に絵文字辞書を加えてあげると実現できます。

```sh
skkdic-expr2 /usr/share/skk/*.utf8 /path/to/other_dicks | \
  sudo tee /usr/share/skk/SKK-JISYO.all.utf8 > /dev/null
```

これでSKKユーザでも絵文字が使えるようになりました✌

# トラブルシューティング
## uimのログ

どこにあるのか分かりませんでした。詳しい人教えて下さい。

## uimの再起動

どうやってやるのか分かりませんでした。
GNOME Shellが管轄してる気がするので多分ですが再起動が一番確実だと思います。

## 辞書のリロード

これはログアウトでできるみたいです。

## その他絵文字などを追加したい

`\u` とコードポイントで新しい文字を入力できます。
普段、シフトによる漢字変換や `/` によるアルファベット変換ばかり使ってるかと思いますが、 `\` によるコードポイントの入力もあるのです。そこで `u` に続いてユニコードのコードポイントを入れると入力できます。
例えば ✌ であれば `\u270c` と入力すると入ります。
