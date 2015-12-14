---
categories: [Scala, Scala Meet Up]
date: 2015-12-13T22:42:39+09:00
draft: true
description: "社内勉強会のScala Meet Upで話す内容。
Tagless Finalについて。あまりScala関係ない…
"
title: DSLとTagless Final
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# DSLtとTagless Final
----------------------
サイバーエージェント アドテクスタジオ
Scala Meet Up 2015-12-13

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + AMoAdの新卒エンジニア
 + Lisp, ML, Rust, Shell Scriptあたりを書きます


# DSLを作る
-----------

以下のようなASTで表されるDSLを考える

``` scala
なんか実用的な例
```


# DSLを作る
-----------

普通はASTをラップして以下のようにする

``` scala

```


# DSLを便利に
-------------

例えば、このDSLを文字列にする関数を追加する

``` scala

```


# DSLの拡張
-----------

このDSLに新たに    を追加する

``` scala

```


# Expression Prolem
-------------------

この時に問題が出る

* DSLそのものに手を加える必要がある
  + DSLを使う全てのコードに変更が必要
  + そもそも、ライブラリだったら変更出来るの？
* 実際には使ってなくても全ての関数で新しい機能に対応しないといけない


# それ、Tagless Finalで解決出来るよ
----------------------------------

* 元のコードをいじらず
* 必要な所だけを修正して
* しかも元々の実装よりも速い

ASTの作り方があります。


# 型クラスの復習
---------------

型クラスは

* 型の振る舞うインターフェースを定めて
* インスタンスの型ごとに「後付けで」実装を与えると
* 多相的に扱えるアドホックポリモーフィズム

でした


# 型クラスの復習
---------------

``` rust

```


# Rustに翻訳
-------------

Scalaで説明するとややこしいので一旦先の例をRustに翻訳します

``` rust
DSLのAST
```


# Rustに翻訳
-------------

``` rust
DSLのラッパ
```


# Rustに翻訳
-------------

``` rust
DSLのstringify
```

# Tagless Final
----------------


initialなアプローチとfinalなアプローチ、taglessness
ScalaでのTagless Final(Scala)

</script>
</section>
