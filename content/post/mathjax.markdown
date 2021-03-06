---
type: post
title: "MathJaxを使って数式を埋め込む"
date: 2014-02-21
comments: true
sharing: true
categories: [MathJax, Octopress, 小ネタ]
---
小ネタです。ブログに数式を埋め込むときの話。

<!--more-->

[MathJax](http://www.mathjax.org/)はHTMLに$\LaTeX$の式を埋め込んでJavaScriptでMathMLに変換するプロジェクトみたいです。

Octopressで使うには`(ocotpressroot)/source/_includes/custom/head.html`に

<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
<span class="line-number">4</span>
<span class="line-number">5</span>
<span class="line-number">6</span>
<span class="line-number">7</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">&lt;script type="text/x-mathjax-config"&gt;
</span><span class="line"> MathJax.Hub.Config({ tex2jax: { inlineMath: [['$','$'], ["\\(","\\)"]] } });
</span><span class="line">&lt;/script&gt;
</span><span class="line">&lt;script type="text/javascript"
</span><span class="line"> src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"&gt;
</span><span class="line">&lt;/script&gt;
</span><span class="line">&lt;meta http-equiv="X-UA-Compatible" CONTENT="IE=EmulateIE7" /&gt;</span></code></pre></td>
</tr></table></div></figure>

を追加するだけです。

数式を埋め込むには、上の設定だと`$数式$`又は`\\\(数式\\\)`でインライン、

<figure class="code"><div class="highlight"><table><tr>
<td class="gutter"><pre class="line-numbers"><span class="line-number">1</span>
<span class="line-number">2</span>
<span class="line-number">3</span>
</pre></td>
<td class="code"><pre><code class=""><span class="line">\\\[
</span><span class="line">数式
</span><span class="line">\\\]</span></code></pre></td>
</tr></table></div></figure>

で別行立ての数式が書けます。markdownとコンフリクトするのでバックスラッシュのエスケープ大変ですね…。`\begin{eqnarray*}\end{eqnarray*}`は無理なのかなあ

インライン\\\(e^{i\pi} = -1\\\)

別行\\\\[\lim\_{n \to \infty} P(Z=l) = e^{–\lambda} \frac{ \lambda ^l}{l!}\\\]


