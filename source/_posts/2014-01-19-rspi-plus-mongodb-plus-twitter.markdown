---
layout: post
title: "RSPiでMongoDBを動かし、Twitter Post Alertを作った話"
date: 2014-01-19 18:17
comments: true
sharing: true
categories: [MongoDB, Raspberry Pi, Ruby, Twitter]
---
κeenです。ここのところ寒い日が続いてますね。寒いと家に扃(ひきこも)ってTweet数が急上昇します。 すると一日に何postしてるか気になるのでカウンタを作りました。  
そのときの苦労話です。

<!-- more -->

ただ、どうせ記録取るなら色々やりたいのでRspberry Piで一日のpost数、fav数、そしてメンヘラなことにフォロー/フォロワー全員のidを毎日記録することにしました。これで誰にリムられたかブロられたか一目瞭然ですね。

ここで一つ問題が。post, fav数は良いのですがフォロー/フォロワーのid一覧は毎日変わるのでmysqlで管理するのはちょっと面倒かなと。なのでMongoDBで管理することにしたいんですが、raspbianの`apt`にないんですよね。仕方ないので手動ビルドで対応しました。

## MongoDBのビルド

[ここ](http://c-mobberley.com/wordpress/index.php/2013/10/14/raspberry-pi-mongodb-installation-the-working-guide/)を参考に

    cd ~/compile
    sudo apt-get install build-essential libboost-filesystem-dev libboost-program-options-dev libboost-system-dev libboost-thread-dev scons libboost-all-dev python-pymongo git
    git clone https://github.com/skrabban/mongo-nonx86
    cd mongo-nonx86
    sudo scons

としました。が、`scons`に一晩かかるとのことなので放置してたら何故かraspberry piが落ちてる…  
気をとりなおして再起動。もう一度`sudo scons`すると今度はエラー終了。`sig 9 killed`…いや、私なにもしてないですよ?ダメ元で今度は

    sudo scons install

したら完走。良ク分カラン。狂想曲感が出てますね。

## MongoDBのインストール

さっきのでインストール終わったと思うでしょ？違うんですよ。`init.d`だとか`mongodb.conf`だとかは配置されてないんですよ。`debian/`以下に色々入ってるので

    cd debian
    sudo cp mongodb.conf /etc/mongodb.conf

までは良いのですが、`init.d`と`mongodb.upstart`の`mongod`のパスが`/usr/bin/mongod`でハードコードされてるんですよ。上の`sudo scons install`だと`/usr/local/bin/mongod`です。さらに起動オプションに`--dbpadh /var/lib/mongodb`と`--logpath /var/log/mongodb/mongodb.log`が指定されていて、`mongodb.conf`で上書きしているにも係らずそこまでのパスが存在しないとエラー吐いて起動してくれないので修正。  
`mongodb.upstart`は直ぐ分かるので良いとして、`init.d`は

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
<td class="code"><pre><code class=""><span class="line">48d47
</span><span class="line">&lt; 
</span><span class="line">50c49
</span><span class="line">&lt; DAEMON=/usr/bin/mongod
</span><span class="line">---
</span><span class="line">&gt; DAEMON=/usr/local/bin/mongod
</span><span class="line">100c99
</span><span class="line">&lt; DAEMON_OPTS="$DAEMON_OPTS --config $CONF"
</span><span class="line">---
</span><span class="line">&gt; DAEMON_OPTS="run --config $CONF"
</span></code></pre></td>
</tr></table></div></figure>

な感じの修正です。なんか空行が一つ減ってますが気にしない。  
ここまでしたらようやく

    chmod +x init.d mongodb.upstart
    sudo cp init.d /etc/init.d/mongodb
    sudo cp mongodb.upstart /etc/init/mongodb

で完了です。`man`とかもあるのですが配置の仕方が分からないので放置←

    sudo service mongodb start
    mongo

で接続確認できます。

## Twitterのpostを集約する

ビックリするくらい簡単。twitterのAPI keyを持ってる前提ですが。

    sudo gem install twitter mongo

で必要なgemをインストールし、`~/.twitter.rb`に

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
</pre></td>
<td class="code"><pre><code class="ruby"><span class="line"><span class="vg">$client</span> <span class="o">=</span> <span class="ss">Twitter</span><span class="p">:</span><span class="ss">:REST</span><span class="o">::</span><span class="no">Client</span><span class="o">.</span><span class="n">new</span> <span class="k">do</span> <span class="o">|</span><span class="n">conf</span><span class="o">|</span>
</span><span class="line"> <span class="n">conf</span><span class="o">.</span><span class="n">consumer_key</span> <span class="o">=</span> <span class="s2">"yours"</span>
</span><span class="line"> <span class="n">conf</span><span class="o">.</span><span class="n">consumer_secret</span> <span class="o">=</span> <span class="s2">"yours"</span>
</span><span class="line"> <span class="n">conf</span><span class="o">.</span><span class="n">access_token</span> <span class="o">=</span> <span class="s2">"yours"</span>
</span><span class="line"> <span class="n">conf</span><span class="o">.</span><span class="n">access_token_secret</span> <span class="o">=</span> <span class="s2">"yours"</span>
</span><span class="line"><span class="k">end</span>
</span></code></pre></td>
</tr></table></div></figure>

と保存しておきます。昔と形式が変わりましたね。この形式になってからスレッドセーフになり、Twitter Stream APIにも対応してるようです。いつか扱ってみたい

集計ですが

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
<td class="code"><pre><code class="ruby"><span class="line"><span class="c1">#! /usr/local/bin/ruby</span>
</span><span class="line"><span class="c1"># coding: utf-8</span>
</span><span class="line"><span class="nb">require</span> <span class="s1">'twitter'</span>
</span><span class="line"><span class="nb">require</span> <span class="s1">'mongo'</span>
</span><span class="line"><span class="nb">require</span> <span class="s1">'time'</span>
</span><span class="line"><span class="nb">load</span> <span class="s2">"~/.twitter.rb"</span>
</span><span class="line">
</span><span class="line"><span class="n">doc</span> <span class="o">=</span> <span class="p">{</span> <span class="s2">"tweets"</span> <span class="o">=&gt;</span> <span class="vg">$client</span><span class="o">.</span><span class="n">user</span><span class="o">.</span><span class="n">tweets_count</span><span class="p">,</span>
</span><span class="line"> <span class="s2">"favs"</span> <span class="o">=&gt;</span> <span class="vg">$client</span><span class="o">.</span><span class="n">user</span><span class="o">.</span><span class="n">favorites_count</span><span class="p">,</span>
</span><span class="line"> <span class="s2">"followers"</span> <span class="o">=&gt;</span> <span class="vg">$client</span><span class="o">.</span><span class="n">friend_ids</span><span class="o">.</span><span class="n">to_a</span><span class="p">,</span>
</span><span class="line"> <span class="s2">"friends"</span> <span class="o">=&gt;</span> <span class="vg">$client</span><span class="o">.</span><span class="n">follower_ids</span><span class="o">.</span><span class="n">to_a</span><span class="p">,</span>
</span><span class="line"> <span class="s2">"timestamp"</span> <span class="o">=&gt;</span> <span class="no">Time</span><span class="o">.</span><span class="n">now</span><span class="o">.</span><span class="n">strftime</span><span class="p">(</span><span class="s2">"%Y%m%d%H%M%S"</span><span class="p">)}</span>
</span><span class="line"><span class="n">col</span> <span class="o">=</span> <span class="ss">Mongo</span><span class="p">:</span><span class="ss">:Connection</span><span class="o">.</span><span class="n">new</span><span class="o">.</span><span class="n">db</span><span class="p">(</span><span class="s2">"twitter"</span><span class="p">)</span><span class="o">.</span><span class="n">collection</span><span class="p">(</span><span class="s2">"tweets"</span><span class="p">)</span>
</span><span class="line"><span class="n">last</span> <span class="o">=</span> <span class="n">col</span><span class="o">.</span><span class="n">find</span><span class="o">.</span><span class="n">sort</span><span class="p">(</span><span class="o">[</span><span class="s2">"timestamp"</span><span class="p">,</span><span class="ss">:desc</span><span class="o">]</span><span class="p">)</span><span class="o">.</span><span class="n">first</span>
</span><span class="line">
</span><span class="line"><span class="n">tweets_sub</span> <span class="o">=</span> <span class="n">doc</span><span class="o">[</span><span class="s2">"tweets"</span><span class="o">]</span> <span class="o">-</span> <span class="n">last</span><span class="o">[</span><span class="s2">"tweets"</span><span class="o">]</span>
</span><span class="line"><span class="n">favs_sub</span> <span class="o">=</span> <span class="n">doc</span><span class="o">[</span><span class="s2">"favs"</span><span class="o">]</span> <span class="o">-</span> <span class="n">last</span><span class="o">[</span><span class="s2">"favs"</span><span class="o">]</span>
</span><span class="line"><span class="n">followers_sub</span> <span class="o">=</span> <span class="n">doc</span><span class="o">[</span><span class="s2">"followers"</span><span class="o">].</span><span class="n">length</span> <span class="o">-</span> <span class="n">last</span><span class="o">[</span><span class="s2">"followers"</span><span class="o">].</span><span class="n">length</span>
</span><span class="line"><span class="n">friends_sub</span> <span class="o">=</span> <span class="n">doc</span><span class="o">[</span><span class="s2">"friends"</span><span class="o">].</span><span class="n">length</span> <span class="o">-</span> <span class="n">last</span><span class="o">[</span><span class="s2">"friends"</span><span class="o">].</span><span class="n">length</span>
</span><span class="line"><span class="n">col</span><span class="o">.</span><span class="n">insert</span> <span class="n">doc</span>
</span><span class="line"><span class="n">message</span> <span class="o">=</span> <span class="s2">"前回計測(</span><span class="si">#{</span><span class="no">Time</span><span class="o">.</span><span class="n">parse</span><span class="p">(</span><span class="n">last</span><span class="o">[</span><span class="s2">"timestamp"</span><span class="o">]</span><span class="p">)</span><span class="o">.</span><span class="n">strftime</span> <span class="s2">"%Y-%m-%d %H:%M:%S"</span><span class="si">}</span><span class="s2">)以降</span><span class="se">\n</span><span class="s2">"</span>
</span><span class="line"><span class="n">message</span> <span class="o">+=</span> <span class="s2">"</span><span class="si">#{</span><span class="n">tweets_sub</span><span class="si">}</span><span class="s2">回ツイートし</span><span class="se">\n</span><span class="s2">"</span>
</span><span class="line"><span class="n">message</span> <span class="o">+=</span> <span class="s2">"</span><span class="si">#{</span><span class="n">favs_sub</span><span class="si">}</span><span class="s2">回favり</span><span class="se">\n</span><span class="s2">"</span>
</span><span class="line"><span class="n">message</span> <span class="o">+=</span> <span class="s2">"友達は</span><span class="si">#{</span><span class="n">friends_sub</span><span class="o">.</span><span class="n">abs</span><span class="si">}</span><span class="s2">人</span><span class="si">#{</span><span class="n">friends_sub</span> <span class="o">&gt;=</span> <span class="mi">0</span> <span class="o">?</span> <span class="s2">"増え"</span> <span class="p">:</span> <span class="s2">"減り"</span><span class="si">}</span><span class="se">\n</span><span class="s2">"</span>
</span><span class="line"><span class="n">message</span> <span class="o">+=</span> <span class="s2">"フォロワーは</span><span class="si">#{</span><span class="n">followers_sub</span><span class="o">.</span><span class="n">abs</span><span class="si">}</span><span class="s2">人</span><span class="si">#{</span><span class="n">followers_sub</span> <span class="o">&gt;=</span> <span class="mi">0</span> <span class="o">?</span> <span class="s2">"増え"</span> <span class="p">:</span> <span class="s2">"減り"</span><span class="si">}</span><span class="s2">ました"</span>
</span><span class="line"><span class="vg">$client</span><span class="o">.</span><span class="n">update</span> <span class="n">message</span>
</span></code></pre></td>
</tr></table></div></figure>

で全部です。Rubyのハッシュ+配列がそのままinsert/selectできるのでぜんぜんMongoDBを意識しなくて良いですね。

ところで`datetime`型が無さげだったので`timestamp`をソートキー兼idにするために`YYYYMMDDHHMMSS`形式の文字列で格納してます(整数は8bitまでっぽかったです)。他に良い方法があると思うのですが思いつきませんでした。


