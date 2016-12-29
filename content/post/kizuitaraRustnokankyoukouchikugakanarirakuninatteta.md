---
categories: [Rust, rustup, 開発環境, Emacs]
date: 2016-12-29T11:52:55+09:00
title: 気付いたらRustの環境構築がかなり楽になってた
---

κeenです。ここのところRustの開発ツール回りでリリースが続いてセットアップが楽になってるようだったので最新の情報をお届けします。


<!--more-->

とりあえず私は既存の環境があるので一旦全て破棄してから再度セットアップしてみます。

# Rustのインストール

[rustup.rs - The Rust toolchain installer](https://rustup.rs/)がリリースされたので以下で一発です。

```
$ curl https://sh.rustup.rs -sSf | sh
```

そして

```shell
source $HOME/.cargo/env
```

をシェルのrcファイルに書き加えたらOKです。
今のシェルにも反映するには同じく上記のコマンドを打ちます。

# 周辺ツールのインストール

cargoがあるので楽ですね

```
# コードフォーマッタ
$ cargo install rustfmt
# 補完、定義ジャンプなど
$ cargo install racer
```

まあまあ時間がかかります。

# エディタ(Emacs)の設定
私がEmacsを使ってるのでEmacsの設定を。

まず、必要パッケージをインストールします。

~~`racer` `rustfmt` `flycheck-rust` をインストールします。~~

`racer` `flycheck-rust` をインストールします。rust-modeにrustfmtの機能が含まれているようでした。

<blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> rustfmtを emacsから使うの rust-modeが現在サポートしているので, rustfmt.elパッケージは不要ですね. rust-format-on-saveが non-nilなら保存時に rustfmtが実行されます.</p>&mdash; Syohei YOSHIDA (@syohex) <a href="https://twitter.com/syohex/status/814354653672415232">2016年12月29日</a></blockquote>

``` emacs-lisp
;;; racerやrustfmt、コンパイラにパスを通す
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
;;; rustのファイルを編集するときにracerとflycheckを起動する
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-rust-setup)))
;;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)
;;; racerの補完サポートを使う
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)
                             ;;; この辺の設定はお好みで
                             (set (make-variable-buffer-local 'company-idle-delay) 0.1)
                             (set (make-variable-buffer-local 'company-minimum-prefix-length) 0)))
```

とりあえずこれでセットアップ完了です。racerの最新版でrustのソースコードの位置を設定しなくてよくなったので簡潔になりましたね。

racer-modeは補完をしてくれる他、`M-.`で定義元ジャンプ、`M-,`でジャンプ元に戻れます。
あとは`M-x racer-describe`でポイント位置にあるシンボルのrustdocを(Emacs内で)表示してくれます。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">おお! racerでEmacs内でrustdocレンダ出来るようになってる！便利！！<br><br>Rustdoc Meets The Self-Documenting Editor<a href="https://t.co/gDEHl2fSfx">https://t.co/gDEHl2fSfx</a> <a href="https://t.co/sdrX9OoEnr">pic.twitter.com/sdrX9OoEnr</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/770796719185408000">2016年8月31日</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

その他は放っておくと勝手に機能してくれるので考えることが少なくていいですね。


# その他便利ツール
セットアップがあっけなかったのでついでに入れておくと便利なツールをいくつか紹介します。

## cargo-edit
インストールは

```
$ cargo install cargo-edit
```

「libcクレートの最新版に依存したい」なんて時に一々libcの最新版を確認するのは面倒ですよね。そんなときにcargo-editがあれば

```
$ cargo add libc
```

で `Cargo.toml` に追記してくれます。

## cargo-script
インストールは

```
$ cargo install cargo-script
```

ちょっとした実験スクリプトとかをコンパイル→実行のステップを踏まずにいきなり実行出来ます。
`cargo script FILE`でファイルを実行出来ます。

```
$ cat <<'EOF' > hello.rs
fn main() {
    println!("hello, script")
}
EOF
$ cargo script hello.rs
   Compiling hello v0.1.0 (file:///home/kim/.cargo/.cargo/script-cache/file-hello-f6b1735a519bb659)
    Finished release [optimized] target(s) in 0.15 secs
hello, script
```

というふうに使います。

シェルのrcファイルで

```sh
alias rust='cargo-script'
```

としておくと吉。

## cargo-update
インストールは

```
$ cargo install cargo-update
```

今までインストールしたバイナリのアップデートを出来ます。

```
$ cargo install-update -a
```

以上
