---
layout: post
title: "Redmineのインストール"
date: 2013-09-05 16:35
comments: true
sharing: true
categories: [Install, Redmine, Ruby, Ubuntu, rvm, 備忘録]
---
私はバイトではプロジェクト管理に [Backlog](http://backlog.jp)のサービスを利用してますが、同様のOSSに [Redmine](http://redmine.jp)なるものがあると聞いて動かしてみたときのメモ。

<!-- more -->

以下、公式の引用。

> Redmineはオープンソースのプロジェクト管理ソフトウェアです。
> 
> プロジェクトのタスク管理、進捗管理、情報共有が行えます。SubversionやGitなどのバージョン管理システムとの連携機能も備えており、ソフトウェア開発やwebサイト制作などのITプロジェクトで特に威力を発揮します。

Redmineの管理団体(？)が [MyRedmine](http://hosting.redmine.jp/)というホスティングサービスを提供してたりしますが、今回はこれをローカルで動かします。

[公式ドキュメント](http://redmine.jp/guide/RedmineInstall/)を参考に作業していきます。

まずはダウンロード。現在の最新版は2.3.2だそうです。

    $cd ~/Ruby
    $svn checkout http://svn.redmine.org/redmine/branches/2.3-stable redmine

んで、グローバルの空間を汚したくないので新たにGemsetを作ります。本来ならbundler仕事なのですが、railsのバージョンとか仕組みの問題が面倒臭そうだったのでGemsetで管理。

    $rvm use --create 1.9.3@redmine
    $cd redmine
    $echo ruby-1.9.3-p448 > .ruby-version
    $echo redmine > .ruby-gemset

これで~/Ruby/redmineにcdすると自動的にrvmが1.9.3@redmineに切り替えてくれます。  
※注意  
`.rvmrc`に`rvm use 1.9.3@redmine`と書いてたら怒られます。`.ruby-version`と`.ruby-gemset`がバージョン管理システムに依存しない新しい書き方だそうです。

データベースはデフォルトだとMySQLを使う設定になってますがこんなお試しにのためにMySQLを動かしたくないのでSQLiteに変更。

    $cp config/{database.yml.example, database.yml}

のあと、database.ymlのproductionの部分を以下のように変更。

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
</pre></td>
<td class="code"><pre><code class="yaml"><span class="line"><span class="l-Scalar-Plain">production</span><span class="p-Indicator">:</span>
</span><span class="line"><span class="err"> </span><span class="l-Scalar-Plain">adapter</span><span class="p-Indicator">:</span> <span class="l-Scalar-Plain">sqlite3</span>
</span><span class="line"><span class="err"> </span><span class="l-Scalar-Plain">database</span><span class="p-Indicator">:</span> <span class="l-Scalar-Plain">db/redmine.sqlite3</span>
</span></code></pre></td>
</tr></table></div></figure>

あとは`bundle install`。(bundlerはインストールされてる前提です)

    $bundle install --without development test

すると…あれ？RMagickでコケる…。公式ドキュメントには `bundle`の`--without`に`rmagick`を加えろと書いてますが画像扱えないのは悔しいので色々調べた結果、

    $sudo apt-get install libmagic-dev libmagickwand-dev

で依存関係を満せるようです。これでようやく`bundle install`が通ります。

ここまできたらあとはドキュメント通り。

    $rake generate_secret_token
    $RAILS_ENV=production rake db:migrate
    $RAILS_ENV=production rake redmine:load_default_data
    $rails s

…あれ？`rails s`でコケる。  
これは自分がrailsに慣れてなかったのが問題のようで、正しくは

    $rails s -e production

だそうです。

    $RAILS_ENV=production rails s

としても同じ。これで [localhost:3000](http://localhost:3000)にアクセスするとRedmineが開きます。

![Redmineのホーム画面](/images/redmine-home.png "redmine-home")

因みにAdminのID、パスワードはadminだそうです。

データベースの設定のexample部分にjenkinsとかいう文字列があったけど連携できるのかなぁ。要調査。

* * *

### 補足

デフォルトのWEBrickだと遅いのでGemfileに

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
</pre></td>
<td class="code"><pre><code class="ruby"><span class="line"><span class="n">gem</span> <span class="s2">"thin"</span>
</span></code></pre></td>
</tr></table></div></figure>

を加えて、`bundle install`し直すと自動的にThinを使ってくれるそうです。


