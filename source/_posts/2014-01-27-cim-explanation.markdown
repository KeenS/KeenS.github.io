---
layout: post
title: "CIMの解説をしてみる コマンド編"
date: 2014-01-27 13:55
comments: true
sharing: true
categories: [CIM, Command Line, Common Lisp, Lisp]
---
先日LTで紹介したCIMが思いの外反響を呼んでる(Githubのstarが15)ので解説でもしてみます。

コマンド編の次は実装編です。これでバグにエンカウントしたときの調査とか完璧ですね。

<!-- more -->
# インストーラ

READMEにあるように

    curl https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | /bin/sh

で`~/.cim/`にインストールできます。<s>パスを変えたければ

    CIM_HOME=/path/to/cim curl https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | /bin/sh

でOK。Cシェル系なら

    env CIM_HOME /path/to/cim curl https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | /bin/sh

でしょうか。</s>

2014-10-20更新  
CIM_HOMEの設定方法が間違ってました。正しくは

    curl https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | CIM_HOME=/path/to/cim /bin/sh
で、Cシェル系なら

    curl https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | env CIM_HOME /path/to/cim /bin/sh
です  
更新ここまで

ここでは`curl`を使ってますが`wget -O -`とか`fetch -o -`など標準出力に吐ければなんでも良いです。

あ、でも`CIM_HOME`を指定してインストールしたときにバグがある気がしてきました。今から直します。

CIMを使うにはシェルの起動時に初期化が必要ですが、インストーラが初期化ファイルを読みにいく処理を`.*rc`に書き出してくれます。Bourne Shellは`.profile`ですね。  
書き出すファイルは`$SHELL`を見てます。`csh`、`tcsh`、`sh`、`bash`、`zsh`に対応してます。

あと`~/.emacs.d/eshell/`が存在したら`~/.emacs.d/eshell/profile`にも初期化処理を書き出します。一時期パスが間違ってて`~/.emacs.d/profile`に書き出してました。ごめんなさい。  
vimshell(だっけ？)でも初期化さえしてしまえば使えると思うので誰か初期化処理書いて下さい。

# `cim`

とりあえず`help`見ておきましょう。

<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
<span class="line-number">19</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help
</span><span class="line">CIM -- Common Lisp Implementation Manager
</span><span class="line">
</span><span class="line">Commands currently available
</span><span class="line">install &lt;impl[-version]&gt; ;; Install the impl.
</span><span class="line">use &lt;impl[-version]&gt; ;; Use specified impl as `cl' command's backend.
</span><span class="line">resume [rm] [resume..] ;; Resume suspended installation(s) or give it up.
</span><span class="line">list &lt;command&gt; ;; List available target of &lt;command&gt;
</span><span class="line">reinstall &lt;impl[-version]&gt; ;; Reinstall the impl.
</span><span class="line">clean &lt;impl&gt;|all ;; Clean downloaded archive and src of the impl.
</span><span class="line">distclean &lt;impl&gt;|all ;; Clean built dists.
</span><span class="line">uninstall &lt;impl[-version]&gt; ;; Uninstall the impl.
</span><span class="line">purge &lt;impl&gt;|all ;; Just clean and uninstall the impl.
</span><span class="line">info ;; Display current state.
</span><span class="line">get ;; Upgrade cim itself.
</span><span class="line">version ;; Show the version
</span><span class="line">help &lt;command&gt; ;; Show help for &lt;command&gt;. If no commands are specified, show this help.
</span><span class="line">
</span><span class="line">If you want detailed help, type `cim help &lt;command&gt;'.</span></code></pre></td>
</tr></table></div></figure>
## `cim install`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
<span class="line-number">19</span>
<span class="line-number">20</span>
<span class="line-number">21</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help install
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim install &lt;impl[-version]&gt;
</span><span class="line"> 
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Install the given lisp implementation to cim directory.
</span><span class="line">If version is not specified, install the latest version.
</span><span class="line">If you want to pass specific options to 'configure' script,
</span><span class="line">prefix 'flags='.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> # install sbcl-1.1.14
</span><span class="line"> cim install sbcl-1.1.14
</span><span class="line">  
</span><span class="line"> # install latest clisp with option.
</span><span class="line"> flags='--with-libsigsegv-prefix=/usr/local' cim install clisp
</span><span class="line">
</span><span class="line">To show available lisp implementations, type 'cim list install'.
</span></code></pre></td>
</tr></table></div></figure>

インストールします。はい。`<impl[-version]>`ってのは`sbcl`とか`clisp-2.48`とかですね。バージョンを指定しなかったら最新版になります。`cim list install`でインストール可能なものを表示しますが載ってない古いバージョンとかもインストールできる筈です。多分。

`configure`スクリプトに渡すフラグを指定したいときは`flags='flag....' cim install`とします。ヘルプにあるように`libsigsegv`のパスを指定しないとFreeBSDでclispをインストールできなかったので用意しました。

Allegroはライセンス表示した方が良いのかなーとか考えてます。

## `cim use`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
<span class="line-number">19</span>
<span class="line-number">20</span>
<span class="line-number">21</span>
<span class="line-number">22</span>
<span class="line-number">23</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim use &lt;impl[-version]&gt; [--default]
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Use &lt;impl&gt; as background for 'cl' command. It also affects bare lisp command.
</span><span class="line">If version is not given, use latest version.
</span><span class="line">If --default is given, use the specified implementation at initial state.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> # use the latest sbcl
</span><span class="line"> cim use sbcl
</span><span class="line"> sbcl --version
</span><span class="line"> -&gt; SBCL 1.1.14
</span><span class="line"> # use old sbcl
</span><span class="line"> cim use sbcl-1.1.10
</span><span class="line"> sbcl --version
</span><span class="line"> -&gt; SBCL 1.1.10
</span><span class="line">
</span><span class="line"> # use ccl-1.9 and set it default
</span><span class="line"> cim use ccl-1.9 --default
</span></code></pre></td>
</tr></table></div></figure>

`cl`コマンドのバックエンドの切り替えと処理系のバージョンの両方を切り替えます。処理系の指定の仕方は`install`に同じ。じゃなかった。`install`に加えて`/bin:/usr/bin:/usr/local/bin`にあるものを`xxx-system`として指定できます。`sbcl-system`とか。

例えば

    cim use clisp-2.48

ってやると`cl`コマンドがバックエンドとしてclispのバージョン2.48を使うようになりますし、`clisp`コマンドのバージョンも2.48になります。この`clisp`の部分はリンクを貼り替えてて、一つのシェルで変更すると他のシェルにも影響が及ぶ問題があるのでそのうち直します。これ結構面倒なのでそのうちです。  
clisp = clisp-2.48な状態で`cim use sbcl`するとclisp = clisp-2.48なまま`cl`のバックエンドがsbclになります。上手く使って下さい。

`--default`を付けると次回からのデフォルト値を指定したものにしつつ`cim use`します。

## `cim resume`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help resume
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim resume [rm] &lt;impl[-version]&gt; | all
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Resume interrupted installation of &lt;impl&gt;. If &lt;version&gt; is not given, the latest version is used.
</span><span class="line">If 'rm' is given, remove &lt;impl&gt; from 'resume' list.
</span><span class="line">If target is 'all', do on all the available target.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> # resume the installation of clisp with configure flags
</span><span class="line"> flags='--with-libsigsegv-path=/usr/local' cim resume clisp
</span><span class="line">
</span><span class="line"> # remove all the available target.
</span><span class="line"> cim resume rm all
</span></code></pre></td>
</tr></table></div></figure>

`cim install`がエラー終了したり`Ctrl-C`で中断したりしたやつを再開してくれるコマンドです。`cim install sbcl`を中断したなら`cim resume sbcl`で再開できます。`cim resume all`で中断された全てのインストールを再開します。終了ステータスは再開してもまたエラーで終了したやつの数になります。

`cim resume rm sbcl`だと`cim list resume`の結果から`sbcl`を削除できます。`resume`はしませんよ。`cim resume rm all`とかも可能です。

## `cim list`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
<span class="line-number">19</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help list
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim list &lt;subcommand&gt;
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">List available target for subcommand.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> # list available lisp implementation to install
</span><span class="line"> cim list install
</span><span class="line">
</span><span class="line"> # show available target for list
</span><span class="line"> cim list list
</span><span class="line">
</span><span class="line"> # targets for subsubcommands are also available
</span><span class="line"> cim list resume rm
</span></code></pre></td>
</tr></table></div></figure>

さっきから出てきてるので分かるかと思いますが、可能なターゲットを表示します。将来的には`zsh`の補完候補にも使うつもりです。そのときは`-b, --batch`オプションでhuman readableかmachine readableかを分けようかなと。

## `cim reinstall`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
<span class="line-number">19</span>
<span class="line-number">20</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help reinstall
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim reinstall &lt;impl[-version]&gt; | all
</span><span class="line"> 
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Force install already installed implementaion(s). Downloaded archives are resued if available
</span><span class="line">If version is not specified, install the latest version.
</span><span class="line">If target is 'all', do on all the available targets.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> # reinstall sbcl-1.1.14
</span><span class="line"> cim reinstall sbcl-1.1.14
</span><span class="line">  
</span><span class="line"> # reinstall all installed lisp impls
</span><span class="line"> cim reinstall all
</span><span class="line">
</span><span class="line">To show available lisp implementations, type 'cim list reinstall'.
</span></code></pre></td>
</tr></table></div></figure>

`cim install`は既にインストールされているものはインストールできないのですが、`reinstall`を使えば可能です。ダウンロードしたアーカイブがあればそれを使います。なければダウンロードから始めます。`all`は察しのようにインストール済のものを全て`reinstall`します。

## `cim clean`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
<span class="line-number">19</span>
<span class="line-number">20</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help clean
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim clean &lt;impl[-version]&gt; | all
</span><span class="line"> 
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Remove donwloaded archives and extracted sources of the given lisp implementations.
</span><span class="line">If version is not specified, install the latest version.
</span><span class="line">If target is 'all', do on all the available targets.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> # clean sbcl-1.1.14
</span><span class="line"> cim clean sbcl-1.1.14
</span><span class="line">  
</span><span class="line"> # clean all installed lisp impls
</span><span class="line"> cim clean all
</span><span class="line">
</span><span class="line">To show available lisp implementations, type 'cim list clean'.
</span></code></pre></td>
</tr></table></div></figure>

`cim install`は基本的に不要なものも削除しないので、それが嫌なら`clean`を使いましょうというスタンスです。アーカイブとソースを削除します。

## `cim distclean`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
<span class="line-number">19</span>
<span class="line-number">20</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$cim help distclean
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim distclean &lt;impl[-version]&gt; | all
</span><span class="line"> 
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Clean built files of the given lisp implementation. It doesn't mean uninstall.
</span><span class="line">If version is not specified, install the latest version.
</span><span class="line">If target is 'all', do on all the available targets.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> # distclean sbcl-1.1.14
</span><span class="line"> cim distclean sbcl-1.1.14
</span><span class="line">  
</span><span class="line"> # distclean all installed lisp impls
</span><span class="line"> cim distclean all
</span><span class="line">
</span><span class="line">To show available lisp implementations, type 'cim list distclean'.
</span></code></pre></td>
</tr></table></div></figure>

`cim clean`より控えめなコマンドです。コンパイルするときに`make`で生成されたファイルを削除します。`make clean`的な。

## `cim uninstall`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
<span class="line-number">19</span>
<span class="line-number">20</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help uninstall
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim uninstall &lt;impl[-version]&gt; | all
</span><span class="line"> 
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Uninstall the given lisp implementaion(s). It does't remove donwloaded archives and extracted sources.
</span><span class="line">If version is not specified, install the latest version.
</span><span class="line">If target is 'all', do on all the available targets.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> # uninstall sbcl-1.1.14
</span><span class="line"> cim uninstall sbcl-1.1.14
</span><span class="line">  
</span><span class="line"> # uninstall all installed lisp impls
</span><span class="line"> cim uninstall all
</span><span class="line">
</span><span class="line">To show available lisp implementations, type 'cim list uninstall'.
</span></code></pre></td>
</tr></table></div></figure>

アンインストールします。はい。ただしダウンロードしたアーカイブとソースは削除しません。

## `cim purge`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help purge
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim purge &lt;impl[-version]&gt; | all
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Uninstall and clean up downloaded files of the given lisp implementation.
</span><span class="line">This command is equivalent to 'clean' and 'uninstall'.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> # purge the latest sbcl
</span><span class="line"> cim purge sbcl
</span><span class="line">
</span><span class="line">To show available target for 'purge', type 'cim list purge'
</span></code></pre></td>
</tr></table></div></figure>

`uninstall`のアーカイブとソースを削除する版です。

## `cim info`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help info
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim info
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Display information for current cim. It includes environment variables that cim uses, what current and default lisp implementation is, what version of each implementation is used, and where system lisp is.
</span><span class="line">
</span><span class="line">Example:
</span><span class="line">
</span><span class="line"> # display info
</span><span class="line"> cim info
</span></code></pre></td>
</tr></table></div></figure>

現在の状態を表示します。こんな感じ。

<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim info
</span><span class="line">CIM_ID = 22176
</span><span class="line">CIM_HOME = /home/kim/.cim
</span><span class="line">
</span><span class="line">current lisp = sbcl-system
</span><span class="line">default lisp = sbcl-system
</span><span class="line">
</span><span class="line">abcl = abcl-1.2.1
</span><span class="line">alisp = alisp-9.0
</span><span class="line">ccl = ccl-1.9
</span><span class="line">clisp = clisp-2.49
</span><span class="line">ecl = ecl-13.5.1
</span><span class="line">gcl = gcl-2.6.9
</span><span class="line">sbcl = sbcl-system
</span><span class="line">
</span><span class="line">sbcl-system = /usr/local/bin/sbcl
</span><span class="line">gcl-system = /usr/bin/gcl
</span></code></pre></td>
</tr></table></div></figure>

`CIM_ID`ってのは`cim`がそれぞれのシェルの状態を管理するためのIDです。まあ、そのシェルのプロセスIDですね。逆にプロセスIDの取得と環境変数の設定さえできれば`cim`は動かせるのでEmacsでも簡単に動きます。

## `cim get`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help get
</span><span class="line">Usage:
</span><span class="line"> cim get
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Upgrade to the latest cim.
</span><span class="line">
</span><span class="line">Examples:
</span><span class="line">
</span><span class="line"> cim get
</span></code></pre></td>
</tr></table></div></figure>

cim自身のアップデートです。なんでこんな名前かというと`rvm`がそうだからです。現状はgithubのKeenS/CIMのmasterからとってきます。stable版ができたらstableに変更しますね。実はブランチを指定してとってくる方法もあったりはします。`cim get --branch stable`か `CIM_INSTALL_BRANCH=stable cim get`です。まだブランチが無いので意味ないんですけどね。

この辺で気付いたかもしれませんが結構環境変数で制御できるようにデザインされてます。rubyがそんな感じだからです。

## `cim version`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help version
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> cim version
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Show current cim's version
</span><span class="line">
</span><span class="line">Example:
</span><span class="line">
</span><span class="line"> cim version
</span></code></pre></td>
</tr></table></div></figure>

めちゃくちゃバグがあるのにversionが1.0.0に見えるの気のせいです。きっと100点中の1点なんですよ。

## `cim help`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cim help help
</span><span class="line">Help for 'help' is not prepared, sorry.</span></code></pre></td>
</tr></table></div></figure>

あー。`help`のhelp用意してなかったー。まあ、上で使ったのが全てです。

# `cl`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
<span class="line-number">16</span>
<span class="line-number">17</span>
<span class="line-number">18</span>
<span class="line-number">19</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ cl -h
</span><span class="line">Usage: cl [switchs] [--] [programfile] [argumensts]
</span><span class="line">
</span><span class="line">-C DIR set *default-pathname-defaults* DIR.
</span><span class="line">-d, --debug set debugging flags (push :debug into *features*)
</span><span class="line">-e, --eval SEXP one line of script. Several -e's are allowed. Omit [programfile]
</span><span class="line">-f, --load FILE load the FILE
</span><span class="line">-i EXT edit *argv* files in place and make backup with the extension .EXT
</span><span class="line">-l LIBRARY quickload the LIBRARY
</span><span class="line">-L LIBRARY quickload and use-package the LIBRARY
</span><span class="line">-r, --repl run repl
</span><span class="line">-q, --no-init do not load $CIM_HOME/init.lisp
</span><span class="line">--no-rl do not use rlwrap. This is effective only when --repl is specified
</span><span class="line">--no-right do not display right prompt. This is effective only when --repl is specified
</span><span class="line">--no-color do not use color. This is effective only when --repl is specified
</span><span class="line">-h, --help print this help
</span><span class="line">-v, --version print the version
</span><span class="line">
</span><span class="line">If neither programfile, -e (--eval) nor -r (--repl) are specified, cl reads scripts from the standard input and then eval them.
</span></code></pre></td>
</tr></table></div></figure>

まあ、見たら分かりますね。ruby由来のオプションとShelly由来のオプションと私の好みのオプションがあります。`cl`を起動する方法は

- `cl script.lisp`
- `cl < script.lisp`
- `cl -e sexp`
- `cl -r`

です。`cl script.lisp`はシェバン(最初の行が`#!`なもの)を無視します。

また、これも環境変数`LISP_IMPL`でバックエンドを制御できます。これはShelly由来ですね。

もう一つ、デバッガは起動しません。スクリプトやワンライナーはエラーを吐いて終了して欲しいですし次に述べますがREPLも目的からしてデバッガは相応しくないと思うからです。因みにですが`-d`オプションをつけてもデバッガは起動しません。理由はオプション解析のほとんどをLispに任せているので起動時のオブションである`--disable-debugger`を制御できないからです。需要が高そうならシェルスクリプト内で解析して頑張ってみます。

あ、`-c, --compile FILE`オプションつけ忘れてた。いつか入れます。`(compile-file FILE)`で可能でしょうからすぐに入るはずです。

## `cl -r`

一応開発向けではなくて、ちょっと試したいときや初心者が本を片手に動かすときを想定して作ってます。エラーはメッセージのみ表示。ちょっと試したいときはあんまりデバッガを必要としませんしスタックトレースもどうせ深さ1~2なのであまり必要でないからです。なにより初心者にとってデバッガは何をして良いか分かりませんし覚えても処理系毎に操作が違います。その辺を分り易い統一インターフェースを作ることも考えたのですが「それって`swank`」って結論に至ったのでこのままです。

# `ql`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">ql -- Command line interface for quicklisp
</span><span class="line">
</span><span class="line">Commands currently available
</span><span class="line">deps &lt;system&gt; ;; Install dependencies of &lt;system&gt;.
</span><span class="line">help [command] ;; Show help.
</span><span class="line">install[quickload] &lt;system&gt;... ;; Install given system.
</span><span class="line">list {remote | local} ;; List available/installed systems.
</span><span class="line">search[system-propos] &lt;keyword&gt; ;; Search quicklisp.
</span><span class="line">uninstall &lt;system&gt; ;; Uninstall system.
</span><span class="line">update [system] ;; Update dist info and installed systems.</span></code></pre></td>
</tr></table></div></figure>

一番未完成なコマンドです。主な理由は私が`quicklisp`の使い方を分かってないからです。

## `ql deps`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
<span class="line-number">14</span>
<span class="line-number">15</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ ql help deps
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> ql deps [--path &lt;path&gt;] &lt;system&gt;
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Install dependencies of given system. If path is specified, install dependencies there.
</span><span class="line">Once you run 'ql --path &lt;path&gt; deps', you don't need to specify '--path' because the path is written out to '.quicklisp-path' in current directory.
</span><span class="line">You can run 'ql deps' for a local system if '.asd' file is in current directory.
</span><span class="line">This command is useful when you want to separate quicklisps for local project.
</span><span class="line">
</span><span class="line">Example:
</span><span class="line">
</span><span class="line"> ql deps --path ./quicklisp my-project
</span></code></pre></td>
</tr></table></div></figure>

rubyの`bundler`を参考に作りました。`Gemfile`に対応する`QLfile`でも読もうかと思いましたが良く考えたら`.asd`で十分じゃんとの思いに至ったのでカレントパスの`asd`ファイルを読みます。  
が、奴は依存システムのバージョンまでは指定できなかった気がするのでやっぱり必要な気がします。

`ql deps --path ./ql myapp`とすると依存システム(+新たな`quicklisp`本体)を`./ql`にインストールしてくれます。`quicklisp`本体のインストールは不要ですがどうやったら回避できるかは今模索中です。  
`--path`の位置が`cim use --default`と違うのでご注意下さい。これは主に内部で使ってる`parse-option`の欠陥に由来します。  
一度指定すると`.quicklisp-path`にパスを書き出してくれるので次回からは必要なくなります。カレントディレクトリに複数の`asd`ファイルがあってそれぞれでパスを使い分けたいとかはできません。  
あと、最大の欠陥として`./ql`にシステムをインストールしてもそれを読み込みにいくコマンドがないので実用性ゼロです← まあ、`cl`あたりを弄ってカレントパスに`.quicklisp-path`があったら読みにいくようにします。その辺は`.lisp-impl`とかも含めて考えます。

## `ql help`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ ql help help
</span><span class="line">Help for 'help' is not prepared, sorry.
</span></code></pre></td>
</tr></table></div></figure>

こいつもhelp用意してなかった。まあ、良いや。

## `ql install`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
<span class="line-number">13</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ ql help install
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> ql install &lt;system&gt;...
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Install given system(s) to $CIM_HOME/quicklisp.
</span><span class="line">
</span><span class="line">Example:
</span><span class="line">
</span><span class="line"> # install clack
</span><span class="line"> ql install clack
</span></code></pre></td>
</tr></table></div></figure>

なんでこいつがhelpの先頭に居ないんでしょうね。`cim`に比べて適当ですね。

`ql quickload`っていう別名も持ってます。`ql help`には`quickload`が本名で`install`がエイリアスって書いてありましたが実装ミスりました。

名前の通り`ql:quickload`します。`system`は複数指定できます。

## `ql list`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ ql list
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> ql list {remote | [local]}
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">List installed systems. if 'remote' is given, list all the available systems. 'ql list local' is equivalent to 'ql list'.
</span><span class="line">
</span><span class="line">Example:
</span><span class="line">
</span><span class="line"> ql list remote
</span></code></pre></td>
</tr></table></div></figure>

`cim list`と使い方が違ってアレですね。現状`quicklisp`に登録されているもの全てを表示する`ql list remote`とインストールしたものだけを表示する`ql list [local]`があります。`ql deps`でパスを指定したやつらにも使いたかったのですが方法が分かんないのでとりあえず先送りです。

## `ql search`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ ql help search
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> ql search &lt;keyword&gt;
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Search quicklisp for &lt;keyword&gt;.
</span><span class="line">
</span><span class="line">Example:
</span><span class="line">
</span><span class="line"> ql search http
</span></code></pre></td>
</tr></table></div></figure>

別名`system-apropos`。別名と本名が`ql help`と違うのも単に`ql:system-apropos`を呼ぶのも`ql install`と同じです。出力結果をもうちょっと制御したいなと思ってます。

## `ql uninstall`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ ql help uninstall
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> ql uninstall &lt;system&gt;...
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Uninstall given system(s).
</span><span class="line">
</span><span class="line">Example:
</span><span class="line">
</span><span class="line"> ql uninstall caveman clack
</span></code></pre></td>
</tr></table></div></figure>

`ql:uninstall`に同じ。以上。

## `ql update`
<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
<span class="line-number">10</span>
<span class="line-number">11</span>
<span class="line-number">12</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">$ ql help update
</span><span class="line">Usage:
</span><span class="line">
</span><span class="line"> ql update [client]
</span><span class="line">
</span><span class="line">Description:
</span><span class="line">
</span><span class="line">Update installed systems. If target 'client' is given, update quicklisp itself.
</span><span class="line">
</span><span class="line">Example:
</span><span class="line">
</span><span class="line"> ql update client
</span></code></pre></td>
</tr></table></div></figure>

`ql:update-all-dists`か`ql:update-client`かを選べます。最初はsystem毎にアップデートしたかったのですが無理っぽいので止めました。

# おわりに

とりあえずCIMの機能はこれで全部です。あ、開発用に`cim reset`とかありますけど使わないで下さい。とういうか使うなよ。絶対使うなよ。

これら使ってみてバグがあったらgithubのissueに投げるなりtwitterで報告するなりして下さい。

ところでCIMは私はちむと読んでます。しー・あい・えむと読む人も居るようです。何でも良いです。プロジェクトとしてのちむとコマンドとしてのちむを区別するためにコマンドは小文字で`cim`、そして`cim`、`cl`、`ql`を含んだプロジェクトは大文字でCIMと書くことにします。今考えました。


