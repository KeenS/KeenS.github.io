---
categories: [SML, SMLSharp, emacs, flymake]
date: 2015-04-23T23:01:17+09:00
title: SMLSharpを使ってSMLのon-the-flyエラーチェック
---
κeenです。最近SMLを結構書いてるのですが中置演算子が乱立する言語はLisperにはつらくて、しょっちゅうコンパイルエラーを出します。
そこでSML#をflymakeで動かしてOn The Flyにエラーチェックをします。
<!--more-->

なぜSML#かというと `-ftypecheck-only` オプションがあって、シンタックスと型エラーのチェックだけを行なえるからです。

色々試したのですが設定はこれだけで済みました。

```sml
(eval-after-load 'flymake
  '(progn 
    (add-to-list 'flymake-allowed-file-name-masks '(".+\\.sml$"
                                                    (lambda ()
                                                      (list "/usr/local/bin/smlsharp" (list "-ftypecheck-only" (buffer-file-name))))
                                                    (lambda () nil)))
    (add-to-list 'flymake-err-line-patterns '("^\\([^: ]*\\):\\([0-9]+\\)\\.\\([0-9]+\\)-[0-9]+\\.[0-9]+ \\(Error\\|Warning\\):"
                                              1 2 3 4))))
(add-hook 'sml-mode-hook #'flymake-mode)
```

`"/usr/local/bin/smlsharp"` のところは各自書き換えて下さい。尚、SML#はエラーメッセージを複数行に跨って出すのですがそれがflymakeと相性が悪いのでエラーメッセージの取得は諦めました。

多くの場合、 `flymake-simple` という枠組みの中で設定を書くのですがテンポラリファイルを作る構造がどうしてもインターフェースファイルと相性が悪かったので生のflymakeを使ってます。
というかflymakeは元々 `-ftypecheck-only` みたいなのを前提に作られてたのに実際にコンパイル走らせないとエラーメッセージ取得出来ない言語が多過ぎてフレームワークが出来たのでこれが本来の使い方です。


# おまけ
[SML-Lint](https://github.com/nrnrnr/SML-Lint)というものがあって、スタイルワーニングを出してくれます。ただ、これはパッチを当てないと使えなかったりスタイルワーニングのみしか出さなかったりするので気が向いた時に紹介します



