---
layout: post
title: "Shell Scriptでオプションをパースするときの必勝法"
date: 2014-02-19 04:13
comments: true
sharing: true
categories: [Command Line, Shell Script]
---
κeenです。CIMの解説 実装編の執筆はもうちょい掛かりそうです。だいたいコードが落ち着いたら書きます。

今回は長いオプションと短いオプションをシェルスクリプトでパースするときの話です。

<!-- more -->

シェルスクリプトでは`case`を使う方法と`getopts`を使う方法があります。`case`だと長いオプションと短いオプションを扱えるものの短いオプションをまとめるのに苦労し、`getopts`だと短いオプションをまとめられるものの長いオプションを扱えません。

そこで解決策。`--foo ARG` `-f ARG` `-h`をパースし、他のオプションも許可（無視）、`--`でパースを止めるとします。

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
</pre></td>
<td class="code"><pre><code class="sh"><span class="line"><span class="k">while</span> <span class="o">[</span> <span class="s2">"$#"</span> -gt 0 <span class="o">]</span>;<span class="k">do</span>
</span><span class="line"><span class="k"> </span><span class="nv">ARG</span><span class="o">=</span><span class="s2">"$1"</span>;<span class="nb">shift</span>
</span><span class="line"><span class="nb"> </span><span class="k">case</span> <span class="s2">"$ARG"</span> in
</span><span class="line"> --foo<span class="o">)</span> do_something_with <span class="s2">"$ARG"</span>;;
</span><span class="line"> --<span class="o">)</span> <span class="nb">break</span>;;
</span><span class="line"> --*<span class="o">)</span> ;;
</span><span class="line"> -*<span class="o">)</span>
</span><span class="line"> <span class="nv">OPTIND</span><span class="o">=</span>1
</span><span class="line"> <span class="k">while </span><span class="nb">getopts</span> :f:h OPT <span class="s2">"$ARG"</span>; <span class="k">do</span>
</span><span class="line"><span class="k"> case</span> <span class="s2">"$OPT"</span> in
</span><span class="line"> f<span class="o">)</span> do_something_with <span class="s2">"$OPTARG"</span>;;
</span><span class="line"> h<span class="o">)</span> do_something;;
</span><span class="line"> <span class="k">esac</span>
</span><span class="line"><span class="k"> done</span>
</span><span class="line"> ;;
</span><span class="line">  
</span><span class="line"> <span class="k">esac</span>
</span><span class="line"><span class="k">done</span>
</span></code></pre></td>
</tr></table></div></figure>

単純に`case`と`getopts`を組み合わせます。ポイントは`-*)`の節の`OPTIND=1`と`getopts`の第3引数でしょうか。

因みにこの例だと引数を消費します。引数を消費したくなければ

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
</pre></td>
<td class="code"><pre><code class="sh"><span class="line"><span class="k">while</span> <span class="o">[</span> <span class="s2">"$#"</span> -gt 0 <span class="o">]</span>;<span class="k">do</span>
</span><span class="line"><span class="k"> </span><span class="nv">ARG</span><span class="o">=</span><span class="s2">"$1"</span>;<span class="nb">shift</span>
</span></code></pre></td>
</tr></table></div></figure>

を

<figure class="code"><figcaption><span></span></figcaption><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
</pre></td>
<td class="code"><pre><code class="sh"><span class="line"><span class="k">for </span>ARG;do
</span></code></pre></td>
</tr></table></div></figure>

に書き換えれば良く、`--`でパースを止めたくなければ`--) break;;`の節を無くせば良く、

他のオプションを許したくなければ`--*);;`の節を`--*)exit 1;;`などにし、`getopts`の引数文字列`:f:h`の`:`をとって`getopts fh ...`とすれば良いです。


