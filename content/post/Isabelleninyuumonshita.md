---
categories: [Isabelle, 定理証明]
date: 2016-01-12T22:23:45+09:00
title: Isabelleに入門した
---
κeenです。連休中にIsabelleを少しばかり触ったのでその時のまとめを。

<!--more-->
# Isabelleって？
[公式](https://isabelle.in.tum.de/)。定理証明支援系の一つ。SML処理系の1つ、Poly/MLで書かれている。
どうやらHOL(Higher Order Logic)に焦点を当てているようだが詳しいことは分かっていない。

あと、Linuxが正式にはGNU/LinuxというようにIsabelleもIsabelle/Isar/HOLと言うべきらしいが、まだよく分かっていない。

# インストール
公式サイトからダウンロード出来るが、後述のProofGeneralの関係上Isabelle2014をインストールする。

# エディタ設定
[ProofGeneral](https://github.com/ProofGeneral/PG)を使う。恐らくGitHub版を使った方がいい。設定は1行

``` lisp
(load (expand-file-name "/path/to/ProofGeneral/generic/proof-site") nil t)
```

と書くだけ。多分`prettify-symbol-mode`も自動でonになる。(なってなかったらなんか適当に設定しておこう。global-prettify-symbols-modeとかで。)
Isabelleの古いドキュメントを見てるとX-Symbolsモードなるものが出てくるが、恐らく現在の`prettify-symbol-mode`だと思う。

# はじめる
[この](isabelle.in.tum.de/doc/tutorial.pdf)チュートリアルを進めている。まだ途中。タクティックも`tactic_ind`と`auto`しか知らない。

とりあえず拡張子は.thyにしとくとEmacsが認識してくれる。

# 所感
Coqを触ったことあるとそれなりにスムーズに入れた。ただしダブルクォートはキモい。
Coqも3つくらいの言語が混ってるらしいのでその内のGallinaの部分をクォートで囲っている感じなのだろうか。
クォートの部分はHOLがどうのこうの言ってた気がする。

まだどんなことが出来るのか分からないのでCoqの方がマシじゃんという感想しかない。

# 困ったところ
チュートリアル中に数学記号が出てくる。∀とか∃とか¬とか。断りもなしにソースコード中にも。
最初、組版の都合上置き換えたのだろうとCoqのようにforall, exists, notと打っていたがエラーになった。じゃあ、とユニコードで打ってみてもエラーになった。
結局、TeXのようにコマンドを入力するとWYSIWYGのようにシンボルになるらしい。
さらにややこしいことにEmacsに特定のシーケンスでキーを入力するとそのコマンドを挿入してくれる機能がついている。

とりあえず今のところ分かっているものをまとめる

見た目 |  Emacsでの入力 | 実際のタグ
-------|---------------|-----
∀     | \forall       | \<forall>
∃     | \exists       | \<exists>
¬      | \not          | \<not>
∧     | \and, /\      | \<and>
∨     | \or, \/       | \<or>
⇒     | \Rightarrow, => | \<Rightarrow>
→     | \rightarrow, -> | \<rightarrow>
≡     | \equiv, ==    | \<equiv>
≠      | \noteq, ~=    | \<noteq>


なんかそれっぽい記号を連ねてみたら色々出てきた。けどユニコード入力出来ないから載せれないw。

逆引きしたい。見た目からコマンドを知りたい。

なんかまとまりないけどそんな所で。頭痛い。
