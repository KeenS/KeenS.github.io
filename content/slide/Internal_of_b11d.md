---
categories: [社内, AdTech, Lisp, Clojure]
date: 2015-10-19T21:33:00+09:00
description: "AdTech Studioの新卒研修の最終発表用。
私の作ったDSP、'b11d'について。
"
title: Internal of b11d
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# Internal of b11d
----------------------
サイバーエージェント新卒研修  
ひとりDSP最終発表会

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + AMoAdのサーバーサイド（？）エンジニア
 + 趣味でLisp, ML, Shell Scriptあたりを書きます
 + 仕事でScalaとShell Scriptあたりを書きます


# 今回作ったもの
----------------

* b11d (ぶらっくんどごーるど)
* Clojure (compojure + middleware)製
  + + NginX + MySQL + Supervisord
* 出来る限りシンプルになるように作った
  + 310行
* デプロイその他はシェルスクリプト
* 監視はmackerel


# シンプルということ
-------------------

* アプリケーションは状態を持たない
* IO以外副作用を持たない
* つまり、キャッシュを持たない
  + キャッシュはパフォーマンス上の意味しかない
  + 早過ぎる最適化は諸悪の根源
* スケールアウトが容易
* compojureのミドルウェアを上手く使った


# シンプルということ2
--------------------

* データベースはだいたい第4正規形（ﾀﾌﾞﾝ）
  + 全てのカラムは`NOT NULL`
  + 広告主の所を拡張性を持たせるために第1正規形に落とした
  + 1広告主に対して複数の広告が持てるようになる
* 外部キー制約も全部付ける
* データが綺麗なのでアプリケーション側でのやることが少ない
* 分析する時にも活きる筈


# デプロイ
---------

* 全てシェルスクリプトでオペレーション出来る
* 手作業を無くしてミスを無くす
  + 「あれ？設定書き換えたのに反映されてない」 → 「再起動忘れてた」とかを防ぐ
* ☆秘伝☆のシェルテクニック満載！ 
* アプリケーションが1jarに収まるのでデプロイが楽。


# 監視
------

* mackerelで色々モニタリング
* Linux, NginX, MySQL, JVM, レスポンスステータス
* ほとんどMackerelに乗っかることで本質的でないことに労力を割かない
  + 本質 = 業務ロジックを理解すること
* アプリケーションの死活監視にはsupervisord
  + 本当はsystemdでやりたかったけどUbuntuのバージョンが古かった


# 落穂拾い
----------

* DBへの書き込みは非同期。Clojureなら簡単に書ける
  + `(go ...)` で囲むだけ
* だいたい600qps。データが増えると多分もうちょい下がる。
  + キャッシュすればデータに依存しない速度。もう少しパフォーマンスも出る。
* アプリケーションサーバには組込みjettyを使った。
  + War + Tomcatでも出来るがデプロイ/運用が楽なのでこちらを採用
* コア数に対してスケールするのでスケールアップも出来る
* CTR予測は訓練データを 広告主xサイトxUA で統計を取って使う
  + 動的に変化しない


# 改善点とか今後とか
---------

* win rate, 入札/落札価格, CTRとかをmackerelでモニタリングしたかった
* 管理画面的なの必要？
* フォールトトラレンス性はないのでその辺
  + MySQLが死ぬとヤバい。
* ホットデプロイしたかった
* 折角JVMだし機械学習したかった
* Unix Domain Socket使うの忘れてた
* データベースにタイムスタンプ入れるの忘れてた………orz

</script>
</section>
