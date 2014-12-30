---
type: post
title: "Octopressのgistプラグインが動かなかったので修正した"
date: 2013-09-26T22:57:00Z
comments: true
sharing: true
categories: [Github, Octopress, Ruby]
---
どうもGithubのgistのURLが変わってる(?)っぽくてOctopressのgistプラグインが動いてくれなかったので修正しました。

<!--more-->

Octopressの最新版でも修正されてなかったっぽいです。自分の使い方が悪かったのかなぁ…

原因は生のソースコードを取ってくるときに`"https://raw.github.com/gist/#{gist}/#{file}"`にアクセスするんですがそいつが404返すんですね。ブラウザからgistにいって”view raw”をクリックすると`"https://gist.github.com/#{gist_id}/#{gist}/raw/#{file}"`でした。gist\_idってかGithubアカウントです。この辺、挙動が怪しくて、gist\_idがなくてもリダイレクトしてくれたりしてくれなかったりします。APIによって違うようです。問題の`raw`APIはリダイレクトしてくれないので割と大きめにプラグインを書き換える必要がありました。

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
<span class="line-number">24</span>
<span class="line-number">25</span>
<span class="line-number">26</span>
<span class="line-number">27</span>
<span class="line-number">28</span>
<span class="line-number">29</span>
<span class="line-number">30</span>
<span class="line-number">31</span>
<span class="line-number">32</span>
<span class="line-number">33</span>
<span class="line-number">34</span>
<span class="line-number">35</span>
<span class="line-number">36</span>
<span class="line-number">37</span>
<span class="line-number">38</span>
<span class="line-number">39</span>
<span class="line-number">40</span>
<span class="line-number">41</span>
<span class="line-number">42</span>
<span class="line-number">43</span>
<span class="line-number">44</span>
<span class="line-number">45</span>
<span class="line-number">46</span>
<span class="line-number">47</span>
<span class="line-number">48</span>
<span class="line-number">49</span>
<span class="line-number">50</span>
<span class="line-number">51</span>
<span class="line-number">52</span>
<span class="line-number">53</span>
<span class="line-number">54</span>
<span class="line-number">55</span>
<span class="line-number">56</span>
<span class="line-number">57</span>
<span class="line-number">58</span>
<span class="line-number">59</span>
<span class="line-number">60</span>
<span class="line-number">61</span>
<span class="line-number">62</span>
<span class="line-number">63</span>
<span class="line-number">64</span>
<span class="line-number">65</span>
<span class="line-number">66</span>
<span class="line-number">67</span>
<span class="line-number">68</span>
<span class="line-number">69</span>
<span class="line-number">70</span>
<span class="line-number">71</span>
<span class="line-number">72</span>
<span class="line-number">73</span>
<span class="line-number">74</span>
<span class="line-number">75</span>
<span class="line-number">76</span>
<span class="line-number">77</span>
<span class="line-number">78</span>
<span class="line-number">79</span>
<span class="line-number">80</span>
<span class="line-number">81</span>
<span class="line-number">82</span>
<span class="line-number">83</span>
<span class="line-number">84</span>
<span class="line-number">85</span>
<span class="line-number">86</span>
<span class="line-number">87</span>
<span class="line-number">88</span>
<span class="line-number">89</span>
<span class="line-number">90</span>
<span class="line-number">91</span>
<span class="line-number">92</span>
<span class="line-number">93</span>
<span class="line-number">94</span>
<span class="line-number">95</span>
<span class="line-number">96</span>
<span class="line-number">97</span>
<span class="line-number">98</span>
<span class="line-number">99</span>
<span class="line-number">100</span>
<span class="line-number">101</span>
<span class="line-number">102</span>
<span class="line-number">103</span>
<span class="line-number">104</span>
<span class="line-number">105</span>
<span class="line-number">106</span>
<span class="line-number">107</span>
<span class="line-number">108</span>
<span class="line-number">109</span>
<span class="line-number">110</span>
<span class="line-number">111</span>
<span class="line-number">112</span>
<span class="line-number">113</span>
<span class="line-number">114</span>
<span class="line-number">115</span>
<span class="line-number">116</span>
<span class="line-number">117</span>
<span class="line-number">118</span>
<span class="line-number">119</span>
<span class="line-number">120</span>
<span class="line-number">121</span>
<span class="line-number">122</span>
<span class="line-number">123</span>
<span class="line-number">124</span>
<span class="line-number">125</span>
<span class="line-number">126</span>
<span class="line-number">127</span>
<span class="line-number">128</span>
<span class="line-number">129</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">diff --cc plugins/gist_tag.rb
</span><span class="line">index 1620345,0000000..58acd2a
</span><span class="line">mode 100644,000000..100644
</span><span class="line">--- a/plugins/gist_tag.rb
</span><span class="line">+++ b/plugins/gist_tag.rb
</span><span class="line">@@@ -1,105 -1,0 +1,105 @@@
</span><span class="line"> +# A Liquid tag for Jekyll sites that allows embedding Gists and showing code for non-JavaScript enabled browsers and readers.
</span><span class="line"> +# by: Brandon Tilly
</span><span class="line"> +# Source URL: https://gist.github.com/1027674
</span><span class="line"> +# Post http://brandontilley.com/2011/01/31/gist-tag-for-jekyll.html
</span><span class="line"> +#
</span><span class="line"> +# Example usage: //embeds a gist for this plugin
</span><span class="line"> +
</span><span class="line"> +require 'cgi'
</span><span class="line"> +require 'digest/md5'
</span><span class="line"> +require 'net/https'
</span><span class="line"> +require 'uri'
</span><span class="line"> +
</span><span class="line"> +module Jekyll
</span><span class="line"> + class GistTag &lt; Liquid::Tag
</span><span class="line"> + def initialize(tag_name, text, token)
</span><span class="line"> + super
</span><span class="line"> + @text = text
</span><span class="line"> + @cache_disabled = false
</span><span class="line"> + @cache_folder = File.expand_path "../.gist-cache", File.dirname( __FILE__ )
</span><span class="line"> + FileUtils.mkdir_p @cache_folder
</span><span class="line"> + end
</span><span class="line"> +
</span><span class="line"> + def render(context)
</span><span class="line">- if parts = @text.match(/([a-zA-Z\d]*) (.*)/)
</span><span class="line">- gist, file = parts[1].strip, parts[2].strip
</span><span class="line">- script_url = script_url_for gist, file
</span><span class="line">- code = get_cached_gist(gist, file) || get_gist_from_web(gist, file)
</span><span class="line">++ if parts = @text.match(/([a-zA-Z]*) ([a-zA-Z\d]*) (.*)/)
</span><span class="line">++ gist_id, gist, file = parts[1].strip, parts[2].strip, parts[3].strip
</span><span class="line">++ script_url = script_url_for gist_id, gist, file
</span><span class="line">++ code = get_cached_gist(gist_id, gist, file) || get_gist_from_web(gist_id, gist, file)
</span><span class="line"> + html_output_for script_url, code
</span><span class="line"> + else
</span><span class="line"> + ""
</span><span class="line"> + end
</span><span class="line"> + end
</span><span class="line"> +
</span><span class="line"> + def html_output_for(script_url, code)
</span><span class="line"> + code = CGI.escapeHTML code
</span><span class="line"> + &lt;&lt;-HTML
</span><span class="line"> +&lt;div&gt;&lt;script src='#{script_url}'&gt;&lt;/script&gt;
</span><span class="line"> +&lt;noscript&gt;&lt;pre&gt;&lt;code&gt;#{code}&lt;/code&gt;&lt;/pre&gt;&lt;/noscript&gt;&lt;/div&gt;
</span><span class="line"> + HTML
</span><span class="line"> + end
</span><span class="line"> +
</span><span class="line">- def script_url_for(gist_id, filename)
</span><span class="line">- url = "https://gist.github.com/#{gist_id}.js"
</span><span class="line">++ def script_url_for(gist_id, gist, filename)
</span><span class="line">++ url = "https://gist.github.com/#{gist_id}/#{gist}.js"
</span><span class="line"> + url = "#{url}?file=#{filename}" unless filename.nil? or filename.empty?
</span><span class="line"> + url
</span><span class="line"> + end
</span><span class="line"> +
</span><span class="line">- def get_gist_url_for(gist, file)
</span><span class="line">- "https://raw.github.com/gist/#{gist}/#{file}"
</span><span class="line">++ def get_gist_url_for(gist_id, gist, file)
</span><span class="line">++ "https://gist.github.com/#{gist_id}/#{gist}/raw/#{file}"
</span><span class="line"> + end
</span><span class="line"> +
</span><span class="line">- def cache(gist, file, data)
</span><span class="line">- cache_file = get_cache_file_for gist, file
</span><span class="line">++ def cache(gist_id, gist, file, data)
</span><span class="line">++ cache_file = get_cache_file_for gist_id, gist, file
</span><span class="line"> + File.open(cache_file, "w") do |io|
</span><span class="line"> + io.write data
</span><span class="line"> + end
</span><span class="line"> + end
</span><span class="line"> +
</span><span class="line">- def get_cached_gist(gist, file)
</span><span class="line">++ def get_cached_gist(gist_id, gist, file)
</span><span class="line"> + return nil if @cache_disabled
</span><span class="line">- cache_file = get_cache_file_for gist, file
</span><span class="line">++ cache_file = get_cache_file_for gist_id, gist, file
</span><span class="line"> + File.read cache_file if File.exist? cache_file
</span><span class="line"> + end
</span><span class="line"> +
</span><span class="line">- def get_cache_file_for(gist, file)
</span><span class="line">++ def get_cache_file_for(gist_id, gist, file)
</span><span class="line"> + bad_chars = /[^a-zA-Z0-9\-_.]/
</span><span class="line"> + gist = gist.gsub bad_chars, ''
</span><span class="line"> + file = file.gsub bad_chars, ''
</span><span class="line">- md5 = Digest::MD5.hexdigest "#{gist}-#{file}"
</span><span class="line">- File.join @cache_folder, "#{gist}-#{file}-#{md5}.cache"
</span><span class="line">++ md5 = Digest::MD5.hexdigest "#{gist_id}-#{gist}-#{file}"
</span><span class="line">++ File.join @cache_folder, "#{gist_id}-#{gist}-#{file}-#{md5}.cache"
</span><span class="line"> + end
</span><span class="line"> +
</span><span class="line">- def get_gist_from_web(gist, file)
</span><span class="line">- gist_url = get_gist_url_for gist, file
</span><span class="line">++ def get_gist_from_web(gist_id, gist, file)
</span><span class="line">++ gist_url = get_gist_url_for gist_id, gist, file
</span><span class="line"> + raw_uri = URI.parse gist_url
</span><span class="line"> + proxy = ENV['http_proxy']
</span><span class="line"> + if proxy
</span><span class="line"> + proxy_uri = URI.parse(proxy)
</span><span class="line"> + https = Net::HTTP::Proxy(proxy_uri.host, proxy_uri.port).new raw_uri.host, raw_uri.port
</span><span class="line"> + else
</span><span class="line"> + https = Net::HTTP.new raw_uri.host, raw_uri.port
</span><span class="line"> + end
</span><span class="line"> + https.use_ssl = true
</span><span class="line"> + https.verify_mode = OpenSSL::SSL::VERIFY_NONE
</span><span class="line"> + request = Net::HTTP::Get.new raw_uri.request_uri
</span><span class="line"> + data = https.request request
</span><span class="line"> + if data.code.to_i != 200
</span><span class="line"> + raise RuntimeError, "Gist replied with #{data.code} for #{gist_url}"
</span><span class="line"> + end
</span><span class="line"> + data = data.body
</span><span class="line">- cache gist, file, data unless @cache_disabled
</span><span class="line">++ cache gist_id, gist, file, data unless @cache_disabled
</span><span class="line"> + data
</span><span class="line"> + end
</span><span class="line"> + end
</span><span class="line"> +
</span><span class="line"> + class GistTagNoCache &lt; GistTag
</span><span class="line"> + def initialize(tag_name, text, token)
</span><span class="line"> + super
</span><span class="line"> + @cache_disabled = true
</span><span class="line"> + end
</span><span class="line"> + end
</span><span class="line"> +end
</span><span class="line"> +
</span><span class="line"> +Liquid::Template.register_tag('gist', Jekyll::GistTag)
</span><span class="line"> +Liquid::Template.register_tag('gistnocache', Jekyll::GistTagNoCache)</span></code></pre></td>
</tr></table></div></figure>

なんかOctopressのブランチが面倒だったり`magit.el`の使い方がよく分らなかったのでアレですが伝えたいことは伝わるdiffだと思います。

見ての通り`gist_id`というパラメーターを追加してリクエストURLをちょこっと書き換えただけです。この修正を加えたあとは

    {% gist KeenS 6688683 script.lisp %}

で使えます。

本当にこれでいいのかなあ…なんか違う気がするなぁ。表示もイマイチだし。けどこれしかないのだから仕方がないですね。正確な情報を持ってる方いらっしゃいましたらコメントお願いします。

## 追記

http://rcmdnk.github.io/blog/2013/05/06/blog-octopress/ にあるエントリを見て変更加えました。やり方は貼られてあるdiffを`*scratch*`バッファにコピー、`M-x ediff-patch-file`でパッチ適用しました。

しかしどうもパッチの元のバージョンが古いらしく、何度もエラー出しながら手でパッチファイルを修正しました。もうちょっとパッチに慣れないとな…

しかも苦労してパッチ適用したのに表示変わらないなーって思ってたらどうもCleanpressは`sass/partial`を読み込まないようなので無駄骨でした。`sass/parts/_syntax.sass`をひたすら様子見ながら修正しました。

diffは…いいや。希望があったら晒します。


