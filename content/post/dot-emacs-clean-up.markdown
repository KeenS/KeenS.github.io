---
type: post
title: ".emacsの整理をした話 + EmacsとViとShellとLispを悪魔合体させたら超絶便利だった"
date: 2013-12-13
comments: true
sharing: true
categories: [Emacs, Emacs Lisp, Lisp, Shell, vi]
---
やや長いタイトルですが・・・年末になって大掃除がやってきましたね。みなさんもそろそろ.emacsの大掃除をしましょう。

<!--more-->

私の.emacsは元々1300行ちょいあってEmacsの起動に7~8秒(体感)かかってましたが大掃除&高速化をした結果800行弱、起動に1秒(体感)ほどになったので整理の仕方を共有しますね。

前提ですが、私はinitローダーとかは使ってません。全部`init.el`に書いてます。で、機能毎にページを作って(`C-q C-l`)ます。ただ、それだけだと視認性が悪いので見出しとして`C-u C-u C-u ;`で`;`を64個挿入して次の行にコメントで`#`付きのタイトルを付けてます。

具体的には

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
</pre></td>
<td class="code"><pre><code class="common-lisp"><span class="line"><span class="nv">^L</span>
</span><span class="line"><span class="c1">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
</span><span class="line"><span class="c1">;; #Lisp</span>
</span></code></pre></td>
</tr></table></div></figure>

こんな感じのものが機能毎に書かれてます。これで`C-s`や`M-x occur`でハッシュタグのように検索することも`C-v`でスクロールしていって目grepすることも`C-x ]`で機能毎にジャンプすることもできます。

# 1. Emacsの最新版を使う

結構重要です。「標準のやつだと欲いこの機能がないから拡張パッケージ入れた」なんてのも最新版では改善されていたりします。例えば私は`emacs-w3m`を使っていましたが、Emacsのmasterブランチには`eww`なるEmacs Lisp製のブラウザが入っているのでそれを使うようにしました。

<s>ただ、これが絶対的正義かというとそうでもなく、パッケージで入れてない分
Ubuntuのインプットメソッドとの連携部分がなかったので<code>uim.el</code>を入れる
必要が出てきたりと、面倒な部分もありました。Emacs標準のインプットメソッドはどうにも使いものにならず、
<code>ddskk</code>もuim-skkとコンフリクトする(というか<code>C-j</code>上書きとかありえない)ので使いません。インプットメソッドの切り替え部分は</s>

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
</pre></td>
<td class="code"><pre><code class="common-lisp"><span class="line"><span class="p">(</span><span class="nv">global-set-key</span> <span class="p">(</span><span class="nv">kbd</span> <span class="s">"&lt;hiragana-katakana&gt;"</span><span class="p">)</span> <span class="nf">#'</span><span class="nv">uim-mode</span><span class="p">)</span>
</span><span class="line"><span class="p">(</span><span class="nv">global-set-key</span> <span class="p">(</span><span class="nv">kbd</span> <span class="s">"&lt;zenkaku-hankaku&gt;"</span><span class="p">)</span> <span class="nf">#'</span><span class="nv">uim-mode</span><span class="p">)</span> <span class="p">(</span><span class="nv">autoload</span> <span class="nf">#'</span><span class="nv">uim-mode</span> <span class="s">"uim"</span> <span class="no">nil</span> <span class="no">t</span><span class="p">)</span>
</span></code></pre></td>
</tr></table></div></figure>

になりました。

2013-12-16追記  
これは私が`~/.Xresources`に`Emacs*useXIM: false`を書いていたのが原因でした。`Emacs*useXIM: true`に書き換え、`xrdb ~/.Xresources`すると直りました。

# 2. 普段使わない設定は全部消す

基本ですね。私は`summary-edit.el`だとか`multiverse.el`だとかるびきちさんの本を読んで便利そうだから入れたものの、結局使わなかったものの設定&elispをごっそり削除。あとかなりの言語に対してデフォルトで`auto-mode-alist`が設定されていたので`auto-mode-alist`の設定も全部消して、必要になったら書き足すようにしました。

# 3. 普段使っていても代替の効くものは削除

これは高速化の意味と自分の環境に依存しない意味があります。最近、自分のラップトップ以外でもEmacsを触ることが多くあって、デフォルトのキーを上書きして使ってる部分で何度も誤操作したのでそれを減らす目的です。`bm.el`は`C-x r SPC`の`register`系や`C-x C-SPC`で対応(registerは覚えれば使い出がありそうなのでいつか解説書くかもです)、`open-junk-file.el`は`~/tmp`を作って対応、`recentf-ext.el`は`helm-file-buffers`だとか。

あと全て`helm.el`に置き換えて`helm.el`と`anything.el`が混在してる状態をどうにかしたかったのですが、`php-completion.el`かなにかが依存しててトドメを刺せませんでした。

あと、`viewer`の代替を探していたらタイトルにあるように悪魔合体が起きたので後で書きますね。

# 4. できる限り標準のものを使う

標準で提供されているパッケージは`autoload`が`emacs`バイナリに組込まれてる(と思う)ので起動時のオーバーヘッドはありません。`flymake.el`や`ruby-mode.el`が標準で提供されてるのに気付いたのでそれを使ったりなど。一度`(emacsroot)/lisp`以下を眺めてみることをお勧めします。結構発見があるものです。

# 5.`autoload`を使う

`autoload`とはファイルの読み込みを必要になるまで遅らせる仕組みです。「必要になる」ってのはそのファイルで定義されている関数が呼ばれたときです。賢い`require`と思えば良いでしょう。

    (autoload #'関数名 "関数が呼ばれたときに読むファイル名" nil interactivep)

みたいに使います。`interactivep`の部分は`M-x`で呼ぶものなら`t`、そうでなければ`nil`です。`require`を`autoload`で書き換えていけば理論上起動時の読み込み0にできるのでかなり高速化できます。

が、実際は一々`autoload`書くのはしんどいので次です。

# 6.できる限り`package.el`を使う

`package.el`は必要な関数の`autoload`を自動生成して読み込んでおいてくれるのでかなりの手間が省けます。そして`autoload`があるのに`require`してると折角の`package.el`の配慮が無駄になります。

自動生成された`autoload`は`elpa/パッケージのディレクトリ/パッケージ-autoloads.el`にあるので確認しながら`init.el`の邪魔なものを消していきます。これでかなり`init.el`の行数が減ります。今まで無駄な設定していたんだなと気付きます。

# 7. `eval-after-load`を使う

8割程の設定は`autoload`で対応できるのですが、踏み込んだ設定をしているとパッケージの内部の関数を使ってしまってどうしてもその式が評価される前にパッケージが読み込まれている必要があることがあります。

そんなときは`eval-after-load`を使います。名前のまんま、ロードした後で`eval`してくれます。

    (eval-after-load 'ファイル名
        '式)

の形で使います。複数の式を使いたい場合は`progn`を使って

    (eval-after-load 'ファイル名
        '(progn
             式1
             式2...))

のように使います。あるパッケージの拡張パッケージなんかもここで読むと良いかもしれません。

# 8.その他

メールクライアントを標準のものにしようとしましたが、`gnus.el`はちょっと受け付けなくてその他はimapを喋らないので断念。でも色々調べてたら`mew`より`wanderlust`の方が良いようなので使い初めました。表示が綺麗で良いですね。HTMLのレンダリングも`emacs-w3m`に頼らず標準の`shr.el`を使っているのも◎。

同じような経緯で`JDEE`をやめて`malabar.el`を使うようにしました。ただ、私は`maven`使いではないので微妙ではあります。まあ、そもそもプロジェクト単位でJavaを書くことがないってのもあるんですが。Androidのスケルトンがantなのでantでできたら嬉しいなーって。

# EmacsとViとShellとLispを悪魔合体させた話

私はEmacsの狂信者ですが読み専のときはちょいちょいviを使うこともあります。片手で操作できるのは便利です。Emacsで読み専といえば`view-mode`です。そこでもhjklを使うべく`view-mode-map`に手を加えてましたが、大掃除ということで全部削除。

その後で`emacsroot/lisp/emulate/`以下を読んでいるとなんかviのエミュレーターが3つも見付かりました。`vi.el`、`vip.el`、`viper.el`です。後者になるほどviとの互換性が高くなります。とりあえずは`hjkl`が使えれば良いので`vi.el`を使ってみたところ、ん〜…といったところ。`vip.el`と試して結局`viper.el`に落ち着きました。

    (global-set-key (kbd "C-x C-q") #'(lambda ()
                        (interactive)
                        (toggle-viper-mode)
                        (force-mode-line-update)))

設定はこんな感じです。`toggle-viper-mode`してもモードラインの表示が変わらないことがあったので`force-mode-line-update`を加えました。

`viper.el`は単なるviのエミュレートだけではなく、Levelに応じて良い感じにemacsと悪魔合体してくれます。私は最高レベルの5にしました。”C-x C-s”など基本的なコマンドはそのまま使えるようになってます。`:`で始まるvi(ex)のコマンドも使えます。`C-z`でemacs<–>viを切り替えたり。非常に便利です。

尚、私はvi使いであってvim使いではないのでevilは使いません。

もう一つ、shellの話。今までは`multiterm` × `zsh`な感じでしたが、「できる限り標準のものを使う」方針で`eshell`に切り替えました。`eshell`はEmacs Lispで書かれたshellです。これが思った以上に便利です。るびきちさんの本では標準出力とエラー出力の切り分けができてないと書かれてましたがそれは修正されてるようです。

あとは`/dev/kill`だとか`>>>`だとかバッファへのリダイレクトだとか`grep`の上書きだとか色々楽しい拡張もあるのですが、一番は`eshell`がLispであること。`eshell`上で任意のEmacs Lisp式を実行できます。最近Emacs LispやLispに精通してきたので非常に有り難いです。  
それにファイルを開くときもその後で同じディレクトリのファイルを開くことが多いので一旦 `cd`してから`find-file`をするとアクセスし易くて捗ります。もう起動時に`eshell`が立ち上がるようにして、基本そこから操作するようにしてます。guakeもそんなに使わなくなりました。他の環境でも使えるので安心して依存できます。

で、ファイルを開くときはどうしてるかというと実はemacsの`find-file`ではなくviの`:e file-name`です。Emacs上でLispで出来たShellを使いつつviを動かしてます。かなり人を選びますが「EmacsのヘビーユーザーでLispに精通しててviを便利だと思ってる人」は試してみてはいかがでしょうか。


