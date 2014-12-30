---
type: post
title: "Emacs便利機能/Tips紹介"
date: 2014-08-13
comments: true
categories: [Emacs, 開発環境, IDE]
---
κeenです。このブログのアクセスログを見てるとEmacsの記事が好評なようなのでここは一つ私の知ってる便利機能を全列挙してみますね。
<!--more-->
どの辺から常識でどの辺からTipsかは私の主観です。だいたいるびきちさんの本に載ってるのは常識扱いです。

※`-!-`でカーソル位置、`-¡-`でマーク位置を表すものとします。  
※私はEmacsのtrunkをビルドして使ってるので最新のリリース版のEmacsにも含まれない機能もあるかもしれません。

# 改行/インデント系
## `C-m` `RET` `C-j` `C-o`
最初の頃は混乱してました。

| key       | lisp function
|:----------|:-------------------
| C-m = RET | `newline`
| C-j       | `newline-and-indent`
| C-o       | `open-line`

です。`C-m`と`C-o`の違いはカーソルが現在行に残るか残らないかです。

```
aaa-!-aaa
```
↓ `C-m`
```
aaa
-!-aaa
```
ですが、
```
aaa-!-aaa
```
↓ `C-o`
```
aaa-!-
aaa
```
です。

`C-o`の改行後にインデントする版を捜したのですが`C-M-o (split-line)`という近いコマンドはあるもののまさにというのはありませんでした。

※trunkではデフォルトで`electric-indent-mode`がonになってるので`C-m/RET`と`C-j`が入れ替わります。  
※現在行と改行した先の行両方をインデントする`indent-then-newline-and-indent`という関数もあります。

## `M-^`(`delete-indentation`)
直感的には`newline-and-indent`の逆関数です。今の行を上の行にくっつけます。そのとき邪魔になるインデントは取り除いてくれます。`M-- M-^`とすると下の行を今の行にくっつけてくれます。

## `C-M-j`(`indent-new-comment-line`)
コメント内で使う`C-j`です。
```lisp
;;; This is a comment-!-
```
↓ `C-M-j`
```lisp
;;; This is a comment
;;; -!-
```
です。

## `M-m`(`back-to-indentation`)
現在行の空白文字でない最初の文字に移動します。viでいう`^`です。

```
    aaa-!-bbb
```
↓
```
    -!-aaabbb
```
です。

## `electric-indent-mode`
さっきちらっと触れました。`C-m`が`newline-and-indent`になって`C-j`が`newline`になるモードです。

# 削除/kill-ring系
## `<C-S-backspace>`(`kill-whole-line`)
おなじみ `C-k`(`kill-line`)は現在位置から行末までを削除しますが、それだとぬるいといって`C-k`で現在行をまるごと削除する設定(`(setq kill-whole-line t)`)してる人もいますが、実は`<C-S-backspace>`で可能なのでした。鍛えられた左手の小指の先と腹でCaps LockとShiftを同時押しすれば難易度もそんなに高くない！

## `C-M-w`(`append-next-kill`)
nextのkillコマンドでkillした内容をkill-ringにappendします。チュートリアルにも載ってた気がするんですがすっかり忘れてました。

# 編集系
## `electric-pair-mode`
開き括弧を挿入すると自動で閉じ括弧を挿入してくれます。リージョンを選択して括弧を挿入するとリージョンを囲むように括弧を挿入してくれます。これ大事。挿入後のカーソルは開き/閉じのどちらを入力したかに依ります。まあ、想像付きますね。地味に鉤括弧にも対応。

`ruby-mode`はこの挙動を上書きしてるので殴り殺したくなります。

## `subword-mode`
私はキャメルケースが嫌いという超個人的理由で使ってないのですが、キャメルケースがコーディング規約な言語を使う人には便利でしょう。キャメルケースの1こぶを1単語と見做します。例えば"SubWord"は"Sub" "Word"と認識されます。


    -!-SubWord 
で`M-f`(`forward-word`)すると通常は
    SubWord-!-
となりますが`subword-mode`内だと
    Sub-!-Word
となります。その他word系の操作が全部こぶ単位になります。

因みにモードラインの表示が", "と、微妙な感じです。

## `superword-mode`
`subword-mode`の逆です。Lispの"this-is-a-symbol"といったシンボルが"this" "-is" "-a" "-symbol"と認識されていたのが"this-is-a-symbol"と認識されます。

因みにモードラインの表示は "$^2$" です。

## `C-x C-u`(`upcase-region`)
リージョンを選択した状態で使うとリージョンを大文字にしてくれます。
これを使ったTipsです。

シチュエーションは全部大文字の単語(定数とかで良く出てくる)を入力したいが、Caps Lockは既にCtrlにしてあるので使えないって状態です。  
先ず、`C-@`します。そして目的の単語を小文字のまま入力します。そして`C-x C-u`します。すると全て大文字になります。便利。`C-x C-l`(`lowercase-region`)でも同じこと出来ますが、使いたいシチュエーションは少ないでしょう。

尚、このコマンドはデフォルトで無効になっているので普段使いするには一回使ってダイアログを出さないようにするか`init.el`に
```lisp
(put 'upcase-region 'disabled nil)
```
を追加しておく必要があります。

## `M-- M-u`(`upcase-word`)
`upcase-word`はカーソルの次の単語を大文字にしますが、負の前置引数を与えると直前の単語を大文字にしてくれます。ちょっと押し辛いですが一応覚えておきましょう。`M-- M-l`(`capitalize-word`)もそれなりに使えるかもしれません。

# complete系
`icomplete-mode`するとミニバッファでの入力時に補完候補が随時表示されますが、実はあの候補に色々操作出来るのです。

## `M-p` / `M-n`
ヒストリを辿れます

## `C-j`
先頭の候補に決定します。

## `C-,` / `C-.`
候補を後/先に送ります。キーボードを見て&lt; / &gt;を押せば良いので簡単ですね。

## `C-M-i`
先頭の候補を補完します。続けて`C-M-i`すると順次次候補になります。

## `C-r`
ヒストリを検索します。

## `C-s`
ミニバッファ内を前方検索します。あまり需要なさそうですね。

# isearch
isearchはカーソルをバッファに残したままミニバッファを編集するのでキーバインドが特殊です。

## `C-s`(`isearch-repeat-forward`)
検索文字列が空の状態で`C-s`すると前回の検索文字列で検索します。

## `C-j`(`isearch-printing-char`)
RETが決定に使われてるので改行はC-jになります。

## `C-w`(`isearch-yank-word-or-char`)
カーソル下の単語又は文字を検索文字列にします。連続して押すと範囲を広げます。

## `C-M-w`(`isearch-del-char`)
1文字削除します。`DEL`(`isearch-delete-char`)とは違って削除してもバッファのポイントは戻りません。

## `C-M-y`(`isearch-yank-char`)
カーソル下の文字を検索文字列に足します。

## `M-s C-e`(`isearch-yank-line`)
カーソルから行末までを検索文字列にします。

## `M-s `(`isearch-toggle-`)系
isearchには実は文字列検索だけでなく、さまざまな検索方法があります。
`M-s _`(`isearch-toggle-symbol`)、`M-s c`(`isearch-toggle-case-fold`)、`M-s r`(`isearch-toggle-regexp`)、`M-s w`(`isearch-toggle-word`)、あたりを覚えておくと良いでしょう。

# バッファ/ウィンドウ系
## split-threshold
Emacsはデフォルトの状態では何かしらのウィンドウを分割するアクション(e.g. `list-buffers`)でウィンドウを上下に分割します。しかしワイドディスプレイを使っていると左右に分割して欲しいものです。そこで、この設定です。
```lisp
(setq split-height-threshold nil)
(setq split-width-threshold 100)
```
この設定で、幅が100桁以上なら左右、100桁未満なら上下に分割するようになります。100という数字は私が試行錯誤して出した最適解です。`split-height-threshold`と`split-width-threshold`を同時に設定したら先に`split-height-threshold`が判定されるようです。

## `C-x 4 `(`-other-window`)系
ウィンドウを分割して隣のウィンドウに別のファイルを開きたい、あるいは既に分割してある隣のウィンドウに別のバッファを持ってきたいといったときに使うのが`C-x 4 `(`-other-window`)系のコマンドです。`C-x 4 C-f`(`find-file-other-window`)、`C-x 4 b`(`switch-to-buffer-other-window`)、`C-x 4 0`(`kill-buffer-and-window`)あたりを覚えておけば良いでしょう。詳しくは`C-x 4 C-h`すると見れます。

似たようなのに、`C-x 5 `(`-other-frame`)系のコマンドがありますが、私はEmacsのフレームを2つ以上出したいという状況に遭遇したことがないので省略します。

## `find-alternate-file`
ほぼ`find-file`と同じ挙動ですが、現在のバッファをkillしてから新たなファイルを開きます。あるファイルを開こうとして間違ったファイルを開いてしまったときに使います(バッファの内容が空なので間違ったことが直ぐ分かる)。地味に便利なのですがデフォルトでキーが割り当てられていません。`C-x C-a`あたりが妥当でしょうか。

# rectangle(矩形選択)系
最近機能が強化されてるrectangle系です。プリフィクスが`C-x r`と押し辛いのが難点ですが、使用頻度が高い訳ではないのでまあ妥当でしょう。

region-rectangleはカーソルとマークで成す長方形を範囲とします。
```
ab-!-cdefg
hijklmn
opqrs-¡-tu
```
のとき、
```
cde
jkl
qrs
```
がregion-rectangleに入ります。

しかしこれだと分り辛いものです。そこで、最近`C-x SPC`でregion-rectangleを視覚表示出来るようになりました。るびきちさんの本に載ってる`sense-region`に近いです。

## `C-x r k`(`kill-rectangle`) / `C-x r d`(`delete-rectangle`) / `C-x r M-w`(`copy-rectangle-as-kill`)
名前のままですね。`kill-rectangle`や`copy-rectangle-as-kill`が保存するkill-ringは通常のkill-ringとは異なります。

region-rectangleを視覚表示した状態で`C-w`すると`kill-rectangle`になるようです(他のコマンドは未確認)

## `C-x r y`(`yank-rectangle`)
`kill-rectangle`や`copy-rectangle-as-kill`が保存したものを吐きます。どのように挿入されるかというと、  
rectangleのkill-ringが
```
cde
jkl
qrs
```
で、バッファが
```
-!-foo
bar
baz
```
のとき、`C-x r y`すると
```
cdefoo
jklbar
qrs-!-baz
```
となります。

## `C-x r t`(`string-rectangle`)
rectangle-regionを文字列で置換します。ちょっと実用的な例を出しましょうかね。

バッファが
```C
pic_-¡-list_first(pic_state *);
pic_list_second(pic_state *);
pic_list-!-_third(pic_state *);
```
で`C-x r t RET vector`すると
```C
pic_vector_first(pic_state *);
pic_vector_second(pic_state *);
pic_vector_third(pic_state *);
```
となります。最近プレビュー機能が入ったので入力しながらリアルタイムでバッファが書き換わります。最小設定主義のemacsにしては珍しい変更ですね。

尚、幅0のregion-rectangleに使うとプリフィクスを付けられることも覚えておきましょう。

```
-¡-This sentence is
-!-a quotation
```
に`C-x r t RET > `すると
```
> This sentence is
> a quotation.
```
と出来ます。

# register系
register系も`C-x r`のブリフィクスを持ちます。非常に高機能なのに押し辛く、rectangleと紛らわしいキーバインドなのは残念です。

registerの概念は分り辛いのですが、「何でも保存出来る箱」で、しかも「キー1つにつき箱一つ」です。viの名前付きバッファと似ていますが何でも保存出来る点で異なります。

それぞれの機能を見てちょっと自分で試してみて理解して下さい。

## `C-x r SPC`(`point-to-register`)
レジスタにポイントを登録します。`C-x r C-SPC`でも`C-x r C-@`でも同じです。

`C-x r SPC`すると"Point to Register: "とプロンプトが出るので、現在のポイントを登録させたいレジスタを選びます。レジスタを選ぶというのは好きなキーを押せばよいです。私は考えるのが面倒なのでaから順番に使っていってます。

これだけだと分かりにくいので次のコマンドも参考にして下さい。

## `C-x r j`(`jump-to-register`)
`point-to-register`でレジスタに登録したポイントに飛びます。例えばaレジスタにポイントを登録したなら`C-x r j RET a`でそこに飛べます。最近、現在登録されてるレジスタ一覧が出るようになったので然程迷わないかと思います。

## `C-x r x`(`copy-to-register`)
レジスタに選択範囲の文字列を登録します。`C-x r s`でも同じです。

rectangleと同じプリフィクスを使っているがために非常に覚え辛いキーバインドになってしまっています。しかもkill-ringとの差別化がイマイチ分からないので需要なさそうですね。

## `C-x r r`(`copy-rectangle-to-register`)
region-rectangleをレジスタに登録します。

## `C-x r i`(`insert-register`)
レジスタに登録した文字列/rectangle/数字をバッファに挿入します。`C-x r g`でも同じです。

ポイントを挿入しようとすると数字が入るようです(ポイントは内部的には数字)。

## `C-x r n`(`number-to-register`)
レジスタに数字を登録します。登録した数字は`C-x r +`(`increment-regiser`)で増やしたり(負の前置引数を与えれば減らすことも可能)`insert-register`でバッファに挿入することも可能。キーボードマクロなんかで役に立つのかな？

## `C-x r w`(`window-configuration-to-register`)
現在のフレームのwindow-configuration(ウィンドウの分割や表示されるバッファ)をレジスタに登録します。復元は`C-x r j`(`jump-to-register`)です。

同様の機能を提供するために数々のプラグインが作られてきましたが標準で提供されることになりました。

これと次の`frameset-to-register`は最近入った機能だった気がします。

## `C-x r f`(`frameset-to-register`)
frameset(フレームの数や位置、サイズ、そしてそれぞれのフレームのwindow-configuration)をレジスタに登録します。復元は`C-x r j`(`jump-to-register`)です。


## 蛇足
キーは割り当てられてませんがキーボードマクロの保存/実行やレジスタにappend/prependするコマンドもあるようです。

同じ`C-x r`のプリフィクスの機能にブックマークがありますが、るびきちさんの本に載ってるので割愛します。

# vc(バージョン管理)系
詳しい使い方は多数の記事があると思うのでそちらに任せるとして、よく使うものを紹介します。尚、るびきちさんの本の頃からは大分進化してるのでvcに関しては参考にしない方が良いかと思います。例えば`git init`相当をするコマンド(`vc-create-repo`)は追加されてます。その他mergeやpull/pushなども。

各vcsの対応状況は

> ;; Supported version-control systems presently include CVS, RCS, GNU  
> ;; Arch, Subversion, Bzr, Git, Mercurial, Monotone and SCCS  
> ;; (or its free replacement, CSSC).

とあります。かなり多いですね。というかこんなにvcsあったんですね。

私が最近gitしか使ってないので他のvcsでどうなるかは分かりません。一応vc.el自体vcsの差異を吸収するものなのですが、vcs固有の挙動もあるのです。

## `C-x v =`(`vc-diff`)
`git diff <バッファのファイル>`相当です。大体、しばらく作業してそろそろコミットするかーって時に使います。表示されるバッファはdiff-modeになっているのでdiffの部分でRETすると変更箇所にジャンプ出来ます。

## `C-x v l`(`vc-print-log`) / `C-x v L`(`vc-root-log`)
`C-x v l`(`vc-print-log`) で現在ファイルの、 `C-x v L`(`vc-root-log`) でバージョン管理下全体のログ(コミットグラフ)を見れます。TABでログ間を移動出来たりRETでログを展開したりDでそのログのdiffを見たり色々なことが出来ます。diffを表示させたら勿論RETで変更箇所に飛べます。変更履歴を追うときは圧倒的に便利。

私もそこまで深追いしてないのでlogバッファでの操作の調査は読者の課題とする。

## `C-x v v`(`vc-next-action`)
vcの中で一番有名なコマンドでしょうから詳細は省きます。私が強調したいのはコミットログ書くときのlog-editモードです。地味に機能強化されてます。`C-c C-d`(`log-edit-show-diff`)でdiffを表示したり`C-c C-e`(`vc-git-log-edit-toggle-amend`)でamendにしたり出来ます。詳しくは`C-c ?`(`log-edit-mode-help`)して下さい。

## `C-x v d`(`vc-dir`)
ディレクトリ単位での機能を使えます。というかファイル単位でない機能(つまりvcsの多くの機能)を使えます。最近はvcを使うときはこれがメインになってます。色々機能があるので`<f1> b`(`describe-bindings`)して確認して下さい。

## `C-x v u`(`vc-revert`)
これは逆に使ってはいけないコマンドです。バッファのファイルのみをリバートする(`git checkout -- <バッファのファイル>`相当)と思いきや、`git reset --hard`するようです。no more被害者。

# view-mode
かなりキーバインドが特殊ですが割と便利な機能です。ここで紹介したもの以外にもキーはバインドされてますが非自明なのは大体こんなもんでしょう。

## `RET`(`View-scroll-line-forward`) / `y`(`View-scroll-line-backward`)
上下に1行スクロールします。ありそうで他にない機能です。

## `/`(`View-search-regexp-forward`) / `\`(`View-search-regexp-backward`)
まさかのviバインド。ちゃんと`n`(`View-search-last-regexp-forward`)と`p`(`View-search-last-regexp-backward`)も用意されてます。

## `s`(`isearch-forward`) / `r` (`isearch-backward`)
`C-`がなくなっただけですね。他にも`C-`がなくなっただけのコマンドはいっぱいありますが`/`/`\`との比較で出しました。

## `m`(`point-to-register`) / `'`(`register-to-point`)
`m`でマークして`'`でジャンプです。

## やめる系
なぜか一杯あります。前提として、view-modeは別のファイルから「view-modeで(時に別ウィンドウで)ファイルを開く」系のコマンドで呼ばれることが多いというのがあります。

| コマンド                   | 説明
|:---------------------------|:----
| `e`(`View-exit`)           | view-modeを抜ける。
| `E`(`View-exit-and-edit`)  | view-modeを抜けてバッファを編集出来るようにする。つまり、read-only-modeも抜ける。
| `c`(`View-leave`)          | view-modeを抜けてバッファを切り替えるがバッファはkillしない。
| `C`(`View-kill-and-leave`) | view-modeを抜けてバッファをkillして以前のバッファに切り替る。
| `q`(`View-quit`)           | view-modeを抜けてウィンドウ内の状態を元に戻し、フォーカスも戻す。大抵バッファをkillする。
| `Q`(`View-quit-all`)       | view-modeを抜けてウィンドウ構成を元に戻す。大抵バッファをkillする。

うん。覚えられね。

# dired系
便利なのに情報が少ないdired系。私はUbuntuのUnityを捨ててStumpwmを使っているのでファイルエクスプローラは専らEmacsですからよく使います。[以前のエントリ](http://localhost:4000/blog/2013/10/04/emacs-dired/)以外の便利機能を紹介します。

因みに呆れるほどキーがバインドされてるのでここで紹介するのはほんの一部です。画像のサムネイル関連の機能なんかもあります。

## `+`(`dired-create-directory`)
ディレクトリを作ります。

## `(`(`dired-hide-ditail-mode`)
初期状態では`ls -l`っぽい内容が表示されてますが`ls`に変更出来ます。

## `v`(`dired-view-file`)
ポイント下のファイルをview-modeで開きます。view-modeでは`q`でバッファを閉じれるので、あるディレクトリ下のファイルを連続して読みたいときに便利です。

## `o`(`dired-find-file-other-window`)
別ウィンドウでファイルを開きます。

## `C-o`(`dired-display-file`)
別ウィンドウでファイルを開きますが、フォーカスは移りません。

diredに限らず、ファイルへのリンクをリスト表示するバッファでは大抵`v`と`o`と`C-o`が使えます。

## `i`(`dired-maybe-insert-subdir`)
バッファの下にポイント下のディレクトリの内容を追加します。ツリー表示にする機能はないようです。

## `%`(`dired--regexp`)系
`% C`(`dired-do-copy-regexp`)など色々あるのですがとりあえず使いそうなのは`% g`(`dired-mark-files-containing-regexp`)と`% m`(`dired-mark-files-regexp`)ですかね。

拡張子でマークするコマンドがあった気がしたのですが見当りませんでした。

## 検索/置換系
これも色々ある上にキーバインドが覚えづらいです。ポイント下又はマークしたファイル全てに操作を行います。キーを覚えるより`M-x`で実行した方が速そうですね。

| キー          | コマンド
|:--------------|:-------------------------------
| `Q`           | `dired-do-query-replace-regexp`
| `M-s a C-s`   | `dired-do-isearch`
| `M-s a C-M-s` | `dired-do-isearch-regexp`

# eww (Emacs Web WOWer)
elispで書かれたブラウザ(正確にはWOWerらしい)です。レンダリングエンジンにはかつてgnusで使われて今は別プロジェクトになったshr.el(Simple HTML Renderer)が使われています。HTMLの他に簡単なCSSも理解します。

libxsltなどのSGML解析系のライブラリを有効にしてビルドしないと使えないのでEmacsを野良ビルドする人は要注意です。

## `M-x eww`
URL又はキーワードで検索します。検索エンンジンは`eww-search-prefix`で制御出来ます。

googleで検索したかったら
```lisp
(setq-default eww-search-prefix "https://www.google.co.jp/search?q=")
```
です。

## キーバインド
infoライクに設定されてます。そんなに多くないのですが、一部抜粋すると`n`(`eww-next-url`)、`p`(`eww-previous-url`)、`l`(`eww-back-url`)、`r`(`eww-forward-url`)、`H`(`eww-list-histories`)、`&`(`eww-browse-with-external-browser`)、`b`(`eww-add-bookmark`)、`B`(`eww-list-bookmarks`)、`q`(`quit-window`)です。

`n` / `p`と`l` / `r`の違いは、`l` / `r`はヒストリを辿る機能、`n` / `p`はヘッダに`<link rel="Next/Prev" ...>`が設定されているときにそれを辿ります。あまり馴染がないかもしれませんが広告料稼ぐためにページ分割しているサイトが多いので結構便利ですよ。

# newsticker
最後に最近見付けたRSS/Atomリーダーの紹介です。

先ず、こんな感じの設定をします。ニュースサイトは自分の好きなものを選んで下さい。

```lisp
(setq-default newsticker-url-list '(("産経" "http://sankei.jp.msn.com/rss/news/points.xml")
                                    ("朝日-IT/Sci" "http://rss.asahi.com/rss/asahi/science.rdf")
                                    ("技術評論社" "http://rss.rssad.jp/rss/gihyo/feed/rss2?rss")
                                    ("Planet Lisp" "http://planet.lisp.org/rss20.xml")
                                    ("Hacker News" "https://news.ycombinator.com/rss")))
(setq-default newsticker-url-list-defaults
              '(("LWN (Linux Weekly News)" "http://lwn.net/headlines/rss")))
(setq-default newsticker-retrieval-interval 0)
(setq newsticker-html-renderer #'shr-render-region)
```
で、`M-x newsticker-show-news`でニュースを取ってきてくれます。

3ペインの画面(treeview)になるかと思いますが、基本操作を覚えれば`C-x o`で移動する必要はありません。

## `f`(`newsticker-treeview-next-feed`) / `F`(`newsticker-treeview-prev-feed`)
ニュースサイト間を移動します。

## `n`(`newsticker-treeview-next-item`) / `p`(`newsticker-treeview-prev-item`)
ニュース間を移動します。

## `SPC`(`newsticker-treeview-next-page`)
ニュースのページを送ります。戻すキーは無いようです。

## `v`(`newsticker-treeview-browse-url`)
今開いているニュースをブラウザで開きます。RSSは概要しか送られてこないのでこの機能は必須ですね。

## `q`(`newsticker-treeview-quit`)
画面を閉じます。

# 最後に
いかがだったでしょうか。ちょっと長いので途中読み飛ばし気味だったかもしれませんが1つでも覚えていただけたら幸いです。

因みに、このエントリを書き始めたのは8/13だったのですが、あまりに長く、途中合宿を挟んだりしたので公開は8/20になってしまいました。
