---
type: post
title: "ここ１ヶ月くらいの近況"
date: 2013-11-13
comments: true
sharing: true
categories: [CIM, Common Lisp, Emacs, Octomacs, fluentd, mpd, Command Line]
---
 #isucon 参戦記がフォロワーの多い@tagomorisさんとか@nitro\_idiotさんとかにツイートされて普段の数十倍のアクセスが来てテンション上がったので近況書きますね。

<!--more-->
## isuconの勉強

件の記事でも書いてますけどMySQLとNginXとCapistranoの勉強しました。SQLって書けると案外楽しいですね。あとN+1問題を学んだり、`IF EXISTS TRRIGER`的な文がなくて殺そうかと思ったり。

NginXはキャッシュとかですね。キャッシュキーに`$cookie_isucon_session`とか入れてログインしてるユーザーが来ても対応できたので良かったです（小並感

Capistranoはどうせだからと3.0を使ったのですが調べても2.x時代の情報ばっかり引っ掛かって苦労しました。その代わり、isucon本戦では複数サーバーへのデプロイ機能を存分に使わせて頂きました。ほぼログインしなくても問題ないですね。

例えば

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
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
<span class="line-number">24</span>
<span class="line-number">25</span>
<span class="line-number">26</span>
</pre></td>
<td class="code"><pre><code class="ruby"><span class="line"><span class="n">set</span> <span class="ss">:application</span><span class="p">,</span> <span class="s1">'my app'</span>
</span><span class="line"><span class="n">set</span> <span class="ss">:repo_url</span><span class="p">,</span> <span class="s1">'git@bitbucket.org:me/myrepo.git'</span>
</span><span class="line"><span class="n">set</span> <span class="ss">:deploy_to</span><span class="p">,</span> <span class="s1">'/home/me/app'</span>
</span><span class="line"><span class="n">set</span> <span class="ss">:scm</span><span class="p">,</span> <span class="ss">:git</span>
</span><span class="line"><span class="n">set</span> <span class="ss">:deploy_via</span><span class="p">,</span> <span class="ss">:remote_cache</span>
</span><span class="line">
</span><span class="line">
</span><span class="line"><span class="n">namespace</span> <span class="ss">:nginx</span> <span class="k">do</span>
</span><span class="line"> <span class="n">task</span> <span class="ss">:reload</span> <span class="k">do</span>
</span><span class="line"> <span class="n">on</span> <span class="n">roles</span><span class="p">(</span><span class="ss">:web</span><span class="p">)</span> <span class="k">do</span>
</span><span class="line"> <span class="n">execute</span> <span class="ss">:sudo</span><span class="p">,</span> <span class="s1">'service nginx reload'</span>
</span><span class="line"> <span class="k">end</span>
</span><span class="line"> <span class="k">end</span>
</span><span class="line">
</span><span class="line"> <span class="n">task</span> <span class="ss">:restart</span> <span class="k">do</span>
</span><span class="line"> <span class="n">on</span> <span class="n">roles</span><span class="p">(</span><span class="ss">:web</span><span class="p">)</span> <span class="k">do</span>
</span><span class="line"> <span class="n">execute</span> <span class="ss">:sudo</span><span class="p">,</span> <span class="s1">'service nginx restart'</span>
</span><span class="line"> <span class="k">end</span>
</span><span class="line"> <span class="k">end</span>
</span><span class="line">
</span><span class="line"> <span class="n">task</span> <span class="ss">:start</span> <span class="k">do</span>
</span><span class="line"> <span class="n">on</span> <span class="n">roles</span><span class="p">(</span><span class="ss">:web</span><span class="p">)</span> <span class="k">do</span>
</span><span class="line"> <span class="n">execute</span> <span class="ss">:sudo</span><span class="p">,</span> <span class="s1">'service nginx start'</span>
</span><span class="line"> <span class="k">end</span>
</span><span class="line"> <span class="k">end</span>
</span><span class="line"><span class="k">end</span>
</span></code></pre></td>
</tr></table></div></figure>

こんな感じです。で、もう一つ

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
<span class="line-number">8</span>
<span class="line-number">9</span>
</pre></td>
<td class="code"><pre><code class="ruby"><span class="line"><span class="n">set</span> <span class="ss">:stage</span><span class="p">,</span> <span class="ss">:production</span>
</span><span class="line"><span class="n">server</span> <span class="s1">'123.4.5.67'</span><span class="p">,</span> <span class="ss">user</span><span class="p">:</span> <span class="s1">'me'</span><span class="p">,</span> <span class="ss">roles</span><span class="p">:</span> <span class="s1">'web'</span>
</span><span class="line"><span class="n">server</span> <span class="s1">'123.4.5.68'</span><span class="p">,</span> <span class="ss">user</span><span class="p">:</span> <span class="s1">'me'</span><span class="p">,</span> <span class="ss">roles</span><span class="p">:</span> <span class="s1">'web'</span>
</span><span class="line"><span class="n">server</span> <span class="s1">'123.4.5.69'</span><span class="p">,</span> <span class="ss">user</span><span class="p">:</span> <span class="s1">'me'</span><span class="p">,</span> <span class="ss">roles</span><span class="p">:</span> <span class="s1">'web'</span>
</span><span class="line"><span class="n">set</span> <span class="ss">:ssh_options</span><span class="p">,</span> <span class="p">{</span>
</span><span class="line"> <span class="ss">keys</span><span class="p">:</span> <span class="sx">%w(~/.ssh/id_rsa)</span><span class="p">,</span>
</span><span class="line"> <span class="n">forward_agent</span><span class="p">:</span> <span class="kp">true</span><span class="p">,</span>
</span><span class="line"> <span class="n">auth_methods</span><span class="p">:</span> <span class="sx">%w(publickey)</span>
</span><span class="line"><span class="p">}</span>
</span></code></pre></td>
</tr></table></div></figure>

を書いておけばこの3台のサーバーに一斉にデプロイしてくれます。(上の例だとnginxですが、まあいいでしょう。)

## Shibuya.lisp

Lisp Meet Up #10で発表してきました。内容は`fluentd`をCommon Lispから使う話。  
スライド

<iframe src="http://www.slideshare.net/slideshow/embed_code/27444839" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC;border-width:1px 1px 0;margin-bottom:5px" allowfullscreen> </iframe>

 **[Common Lisp でビッグデータを作ろう](https://www.slideshare.net/blackenedgold/common-lisp-27444839 "Common Lisp でビッグデータを作ろう")** from **[blackenedgold](http://www.slideshare.net/blackenedgold)** 

Ustreamは見つかりませんでした。てへペロ

恐らく、`Log4CL`はやりたいことと目的が違いますね。全く別のロギングフレームワークを作るのが良さげな気がします。isuconの懇親会で開発者の@tagomorisさんにアドバイスも頂きましたし作るかもしれません。

## mpd

[以前、mplayer2が正常に動作しないとかぼやいて](/blog/2013/09/29/mplayer/)ましたが、結局あきらめました。で、代わりに [Music Player Daemon(MPD)](http://www.musicpd.org/)なるものを見付けて、しかもRaspberry Piで動き、iPod/Androidから操作できるとのことで、導入しました。

MPDはまさしくRaspberry Piのような機器向で、音楽を鳴らすサーバーと操作するクライアントが完全に分かれてます。

    $ sudo apg-get install mpd mpc

とかでRSPiに入れた気がします。クライアントはAndroidは`MPDroid`、iPodは`MPoD`っていうアプリです。

`scp`で手元の曲をRSPiコピーし、iPodから繋ぐも曲が見えない。色々試した結果、`scp`でコピーしたときにパーミッションの問題が出てたようなのでそこを解決したらちゃんと動きました。

動画(iPod)

<iframe width="560" height="315" src="//www.youtube.com/embed/x5CWtXbCkqo" frameborder="0" allowfullscreen></iframe>

黒ばっかで見づらくて申し訳ありません。iPodからではなくスピーカから音が出てるのが分かりますかね？思ったより音が入ってないので分りづらいですね（汗

## CIM

[Shelly](https://github.com/fukamachi/shelly/)を使って [Common Lispスクリプトをexecutableにしよう](https://gist.github.com/KeenS/7059301)ってのをやってましたが少しshellyが求めているのと違うようだったので「シェルスクリプトでshelly的な物を実装しよう！ついでにrvmみたいにバージョン管理できたら嬉しいな！！」って思い付きで [Common Lisp Implementation Manager(CIM)](https://github.com/KeenS/CIM)を作り始めました。まだCLISP, ECL, GCLのインストールぐらいしかできてません。

初めてシェルスクリプトを書き、初めてBourne Shellを触り(普段はzsh)、早くも挫折しそうです。今めっちゃシェルスクリプトの勉強してます。目的はImplementationの管理じゃなくて統一インターフェースの`cl`コマンドなのにそこが全然進んでないですね。`ql`に至っては1行も書いてない。まあ、`cl`を使って書くつもりなので`cl`が出来ないことにはどうしようもないんですが。

`syset`とかネーミングセンスが無かったり(発音しずらい。シセット…サイセットって読もうかな)ダウンロードしたアーカイブのチェックサムとか全く見てなかったり色々ツッコみたくなりますが温かい目で見て下さい。気に入ったらpull-reqとかして下さい。

## Octomacs

Octomacsに一回pull-req送ってからコミッタ気取りのκeenですが、`octomacs-preview`と`octomacs-deploy`を実装しました。`C-u`をつけると`generate`が付いてきます。テストをしてない（できない？）+サーバーを殺す(`octomacs-preview-quit`とかの名前かなあ。現状は`*octomacs preview*`バッファを`kill-buffer`すればいい)コマンドを実装してないのでpull-reqは投げてませんが使ってみたい方は [こちら](https://github.com/KeenS/octomacs)をどうぞ。まあ、今からpushするんですがね。ちゃんとこの記事も`octomacs-new-post`から始まり、`octomacs-preview`で確認しつつ`octomacs-deploy`でデプロイしました。あとは過去の記事を編集する`octomacs-edit`を実装すれば完璧ですね。

こんなかんじです。`CIM`がんばります。


