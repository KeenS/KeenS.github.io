---
categories: [SKK]
date: 2022-04-24T00:21:58+09:00
title: "fcitx-skkで変換/無変換キーで日本語/ラテンを切り替える"
---

κeenです。fcitxユーザになってfcitx-skkの設定頑張ってるので記録に残します。

<!--more-->

fcitxの設定をみてもSKKには大した設定はなさそうに見えますが、内部でlibskkを使っているのでそちらのドキュメントを見ながら設定すると自由度が上がります。

https://github.com/ueno/libskk

ドキュメントは[rules](https://github.com/ueno/libskk/tree/master/rules)のREADMEにあります。あるいは `/usr/share/libskk/rules` にも `README.rules` があるのでそれを読んでもいいです。

``` sh
$ ls /usr/share/libskk/rules
README.rules  act  act09  azik  default  kzik  nicola  tcode  trycode  tutcode  tutcode-touch16x
```

私の場合はデフォルトのキーマップでどの場合も以下のキーバインドにできればよいです。

* 変換キー: ひらがなに切り替え
* 無変換キー: ラテンに切り替え

そこで `/usr/share/libskk/rules/default/keymap/default.json` を編集して `Henkan` と `Muhenkan` に割り当てればよいです。末尾二行に `Henkan` と `Muhenkan` を足します。

``` json
{
    "define": {
        "keymap": {
            // ...
            "Henkan": "set-input-mode-hiragana",
            "Muhenkan": "set-input-mode-latin"
        }
    }
}
```

あとはなんかコンフィグを読み直させれば反映されます。
