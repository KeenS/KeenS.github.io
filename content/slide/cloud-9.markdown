---
type: slide
title: "Cloud 9の紹介"
date: 2014-07-29
aliases:
    - /slide/cloud-9.html
categories: [Lisp Meet Up, Lisp, IDE, Editor, 開発環境]
description: "Lisp Meet Up #19 での発表資料です。<br>
WEBベースのIDE Cloud 9でのLisp対応について。

"
---
<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# Cloud 9の紹介
<hr />

Lisp Meet Up #19 presented by Shibuya.lisp  
κeen(@blackenedgold)  

<!-- .slide: class="center" -->

## 自己紹介
<hr />

 + κeen
 + 東大数学科の4年生
 + ソーシャルアカウントは上のアイコン達から。
 + Lisp, Ruby, OCaml, Shell Scriptあたりを書きます

## Lispがあまり使われない原因
<hr />

<ul>
<li class="fragment">認知度が低い</li>
<li class="fragment"><strong>開発環境構築の難易度高い</strong></li>
</ul>

## Cloud 9について
<hr />

 + WEBベースのIDEサービス
 + 40以上の言語に対応（らしい）
 + この間正式リリース
 + 1アカウント毎に1Docker環境与えられる
 + IDEはオープンソース
 + よく分かってない

## こんなん
<img src="/images/cloud9/login.png" alt="ログイン画面" />

<!-- .slide: class="center" -->

<img src="/images/cloud9/dashboard.png" alt="ダッシュボード" />

<!-- .slide: class="center" -->

<img src="/images/cloud9/workspace.png" alt="ワークスペース" />

<!-- .slide: class="center" -->

<img src="/images/cloud9/lookandfeel.png" alt="こんなかんじ" />

<!-- .slide: class="center" -->

<img src="/images/cloud9/CIMworks.png" alt="CIMも動く" />

<!-- .slide: class="center" -->

<img src="/images/cloud9/clworks.png" alt="CLもRun出来る" />

<!-- .slide: class="center" -->

## デモ

## 良いところ
<hr />

 + 難易度低い
 + Lisp環境動く
 + Githubとの連携
 + どこからでも同じ環境にアクセス
 + キーバインドはうれしい各宗教対応の  
 {Mac, Windows}x{Default, Emacs, Vim, Sublime}


## 微妙なところ
<hr />

 + シンタックスハイライト微妙  
 （せめてキーワード引数はどうにか）
 + 補完微妙
 + インデント微妙
 + swank使いたい

<span style="font-size:600%">以上</span>  
何か質問あればどうぞ

<!-- .slide: class="center" -->
</script>
</section>
