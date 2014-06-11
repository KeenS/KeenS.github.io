---
layout: post
title: "octomacs.elを修正しました"
date: 2013-09-16 21:24
comments: true
sharing: true
categories: [Emacs, Emacs Lisp, Octopress]
---
EmacsからOctopressに投稿できるOctomacs.elが最新版のrvm.elに対応してないようだったので修正してみました。

<!-- more -->

原因はOctomacs.elがrvm.elの内部で使われている関数を使っていたのですが、その関数が最新版で削除されていたからだったみたいです。

結論として、

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
</pre></td>
<td class="code"><pre><code class=""><span class="line">*** /home/kim/.emacs.d/lisp/octomacs/octomacs.el 2013-09-16 22:29:14.498296762 +0900
</span><span class="line">--- /home/kim/.emacs.d/elpa/octomacs-20121026.1649/octomacs.el 2013-08-31 23:54:13.044373498 +0900
</span><span class="line"> ***************
</span><span class="line"> ***4,10****
</span><span class="line"> ;;
</span><span class="line"> ;; Author: Jacob Helwig &lt;jacob@technosorcery.net&gt;
</span><span class="line"> ;; Homepage: http://technosorcery.net
</span><span class="line">! ;; Version: 0.0.1
</span><span class="line"> ;; URL: https://github.com/jhelwig/octomacs
</span><span class="line"> ;;
</span><span class="line"> ;;; License:
</span><span class="line">--- 4,11 ----
</span><span class="line"> ;;
</span><span class="line"> ;; Author: Jacob Helwig &lt;jacob@technosorcery.net&gt;
</span><span class="line"> ;; Homepage: http://technosorcery.net
</span><span class="line">! ;; Version: 20121026.1649
</span><span class="line">! ;; X-Original-Version: 0.0.1
</span><span class="line"> ;; URL: https://github.com/jhelwig/octomacs
</span><span class="line"> ;;
</span><span class="line"> ;;; License:
</span><span class="line"> ***************
</span><span class="line"> ***133,146****
</span><span class="line"> (defun octomacs-format-rake-task-with-args (task &amp;optional arguments)
</span><span class="line"> "Build a shell suitable string of the rake TASK name with the specified ARGUMENTS."
</span><span class="line"> (let ((arguments-string (if arguments
</span><span class="line">! (format "[%s]" (if (listp arguments) (mapconcat 'octomacs-shell-escape-string arguments ", ") arguments))
</span><span class="line"> "")))
</span><span class="line"> (format "'%s%s'" task arguments-string)))
</span><span class="line">  
</span><span class="line"> (defun octomacs-rake-with-rvm (directory task &amp;optional arguments)
</span><span class="line"> "Run rake task TASK with specified ARGUMENTS in DIRECTORY using rvm"
</span><span class="line"> (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
</span><span class="line">! (rvmrc-info (or (rvm--load-info-rvmrc) (rvm--load-info-ruby-version) (rvm--load-info-gemfile)))
</span><span class="line"> (rvm-command (if rvmrc-info
</span><span class="line"> (concat "rvm " (mapconcat 'identity rvmrc-info "@") " do ")
</span><span class="line"> "")))
</span><span class="line">--- 134,148 ----
</span><span class="line"> (defun octomacs-format-rake-task-with-args (task &amp;optional arguments)
</span><span class="line"> "Build a shell suitable string of the rake TASK name with the specified ARGUMENTS."
</span><span class="line"> (let ((arguments-string (if arguments
</span><span class="line">! (format "[%s]" (mapconcat 'octomacs-shell-escape-string arguments ", "))
</span><span class="line"> "")))
</span><span class="line"> (format "'%s%s'" task arguments-string)))
</span><span class="line">  
</span><span class="line"> (defun octomacs-rake-with-rvm (directory task &amp;optional arguments)
</span><span class="line"> "Run rake task TASK with specified ARGUMENTS in DIRECTORY using rvm"
</span><span class="line"> (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
</span><span class="line">! (rvmrc-path (rvm--rvmrc-locate directory))
</span><span class="line">! (rvmrc-info (if rvmrc-path (rvm--rvmrc-read-version rvmrc-path) nil))
</span><span class="line"> (rvm-command (if rvmrc-info
</span><span class="line"> (concat "rvm " (mapconcat 'identity rvmrc-info "@") " do ")
</span><span class="line"> "")))
</span></code></pre></td>
</tr></table></div></figure>

こんな感じです。これでちゃんと`.rvmrc`やら`.ruby-version`やら`.ruby-gemset`やらを反映してくれます。 [pull-req投げてみました](https://github.com/jhelwig/octomacs/pull/2)。初pull-reqです。英語心配です。

俺、pull-reqとりこまれたら`rake gen_deploy`にも挑戦するんだ…

あ、因みにこの投稿は`init.el`に

    (setq octomacs-workdir-alist '(("default" . "~/Ruby/octopress")))

して`octomacs-new-post`から投稿してます。


