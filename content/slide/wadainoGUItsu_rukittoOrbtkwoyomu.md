---
categories: ["Rust"]
date: 2019-12-10T23:00:58+09:00
description: "Rust LT #7 での発表用。GUIツールキットOrbtkのコードの軽い解説"
title: "話題のGUIツールキットOrbtkを読む"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# 話題のGUIツールキットOrbtkを読む
----------------------
[Rust LT #7 - connpass](https://rust.connpass.com/event/156436/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# Orbtkとは
-----------
* [これ](https://github.com/redox-os/orbtk)
* SNSで[話題沸騰中](https://twitter.com/vaaaaanquish/status/1203618077557981184)
* [Redoxプロジェクト](https://redox-os.org)発
* RedoxのWindow System、[Orbital](https://gitlab.redox-os.org/redox-os/orbital)で使われている
* マルチプラットフォーム対応
  + Cライブラリに依存してないっぽい
* 最近stableコンパイラで動くようになった

===
# デモ
------

* calcurator
* canvas
* canvas wasm
* widgets
* widgets wasm

===
# ある機能
---------

* widgetいくつか
* レイアウトいくつか
* ちゃんとマウスやキーボードも扱える
* Webでも動く

===

# Orbtk解剖
-----------
* Application
* Window
  + WindowShell
* Widget
 + Layout
 + Behaviour
 + Cursor
 + ...

===
# Orbtkがマルチプラットフォームで動く仕組み <!-- .element: style="font-size: 120%;" -->
----------------------------------

* フルスクラッチしてるのでOS依存は少なめ
  + そもそも色々動かないRedox上で動いてるし
* GUIに必要なものは意外と少ない
  + ウィンドウ/マウス/キーボードを扱う仕組み
  + グラフィック
* あとはボタンなどは自分で描く

===
# 互換レイヤ
-------------

* ネイティブ
  + ウィンドウ/マウス/キーボード: [minfb](https://github.com/emoon/rust_minifb)
  + グラフィック: [raqote](https://github.com/jrmuizel/raqote)
* Web:
  + Canvasとイベントハンドラでどうにかなる

===

# アーキテクチャ
<!-- .slide: class="center" -->

===

# 他のGUIツールキット
---------------------
* 継承ベースで記述
  + [GTK](https://developer.gnome.org/gtk3/stable/GtkCellAreaBox.html)
  + [Qt](https://doc.qt.io/qt-5/qtabwidget.html)
  + [Swing](https://docs.oracle.com/javase/jp/8/docs/api/javax/swing/JToolTip.html)
* まあ、合理的
  + だいたい似たような振舞
  + だいたい似たようなフィールド
* でもRustは？

===

# ECS
------

* Entity Componet System
  + EntityとComponentとSystemからなる
  + "[Composition over inheritance](https://en.wikipedia.org/wiki/Entity_component_system)"
* Entity: もの。通常中身はIDだけ
* Component: Entityのアスペクト。
* System: とあるコンポーネントを持つEntityを動作させる

===


# Example (1)
----------

from [decs](https://gitlab.redox-os.org/redox-os/dces-rust)

[view](https://gitlab.redox-os.org/redox-os/dces-rust/blob/develop/examples/minimal.rs)


===

# Example (2)
----------

from [decs](https://gitlab.redox-os.org/redox-os/dces-rust)

[view](https://gitlab.redox-os.org/redox-os/dces-rust/blob/develop/examples/basic.rs)

===

# OrbtkとECS
------------

* アプリケーションはECSで動いてる
  + レンダリングループが[Systemの組み合わせ](https://github.com/redox-os/orbtk/blob/develop/crates/api/src/application/mod.rs#L165-L216)
* [ウィジェットはEntity](https://github.com/redox-os/orbtk/blob/develop/crates/api/src/macros.rs#L321)
  + EntityStoreは木構造も管理するようになってる
* ウィジェットの[フィールドはComponentとしても登録される](https://github.com/redox-os/orbtk/blob/develop/crates/api/src/macros.rs#L375-L395)
* [docs.rs](https://docs.rs/orbtk)

===
# メインループ
-------------

* Functional Reactive Programming
   1. イベント入力（内部状態の変更）
   2. 内部状態とテーマなどに基いてステートの更新（追従）
   3. ステートに基いてレイアウト計算/描画
* Reactとかを参考にしたらしい
* 「表示の書き換え」はしない
  + ステートの更新と描画が分離されてる

===
# CSS
-----

* スタイルの指定にCSSが使える
  + セレクタ
  + プロパティ
* もちろんHTMLにあるやつと中身は別
  + タグ名→ウィジェット名
    - 正確にはウィジェットの定義時に自分で指定
  + プロパティ→コンポーネント

===
# Exampleを読む
-----

* [calculator](https://github.com/redox-os/orbtk/blob/develop/examples/calculator.rs)

===
# Widgetを読む
--------------

* [button](https://github.com/redox-os/orbtk/blob/develop/crates/widgets/src/button.rs)
* [toggle_button](https://github.com/redox-os/orbtk/blob/develop/crates/widgets/src/toggle_button.rs)
  + buttonのコピペ
* [switch](https://github.com/redox-os/orbtk/blob/develop/crates/widgets/src/switch.rs)
  + 動くUIの描画
* [scroll_indicator](https://github.com/redox-os/orbtk/blob/develop/crates/widgets/src/scroll_indicator.rs)

===
# まとめ
---------

* Orbtkを紹介した
* 案外GUIツールキットに必要なものは少ない
* OrbtkはECSをベースに作られていた
* OrbtkはFRPで動いていた
* 今後に期待


</textarea>
