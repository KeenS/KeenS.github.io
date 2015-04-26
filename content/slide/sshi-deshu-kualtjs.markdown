---
type: slide
title: "(S式で書く(altJS)達)"
date: 2014-07-13
aliases:
    - /slide/sshi-deshu-kualtjs.html
categories: [Lisp, altJS]
description: "天下一altJS武闘大会でLTしたときの資料です。<br>
元々はorg-modeでEmacsで発表してましたがこのブログに載せる為に変換しました。
"
---
<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# (S式で書く(altJS)達)
<2014-06-08 日>  
κeen(@blackenedgold)  
天下一altJS武闘会(#tenka1altJS)  
@渋谷プライムプラザ四階

<!-- .slide: class="center" -->

## 自己紹介
 + κeen
 + 東大数学科の4年生
 + Twit : @blackenedgold
 + Github : KeenS
 + Blog : [κeenのHappy Hacκing Blog](http://KeenS.github.io/)
 + Shibuya.lispにいます。(2014-06〜運営になりました)
 + Lisp, Ruby, OCaml, Shell Scriptあたりを書きます

## お品書き
 - (ボツ) Shen.js
 - (ボツ) Embedable Common Lisp with Emscripten
 - (ボツ) Picrin with Emscripten
 - (ボツ) オレオレaltJSの作り方の話
 - ParenScript

## Shen.js
Shenの公式ページより

  + パターンマッチ
  + λ計算ベース
  + マクロ
  + 遅延評価も可能
  + 静的型検査
  + 関数的なPrologの統合
  + ビルトインコンパイラコンパイラ

これのJS実装もある。

## 例
```lisp
(define factorial
  0 -> 1
  X -> (* X (factorial (- X 1))))
```

そもそもaltJSじゃない…

<!-- .slide: class="center" -->

ボツ

<!-- .slide: class="center" -->

## ECL with Emscripten
  + ANSI Common Lisp準拠のCommon Lisp処理系
  + LispをコンパイルしてCを吐く
  + Emscriptenでコンパイルすれば…

`#include <ecl/ecl-cmp.h>`
が悪さをしてコンパイル出来ない

<!-- .slide: class="center" -->

ボツ

<!-- .slide: class="center" -->

##  picrin
  + R7RS small準拠を目指すScheme処理系
  + R7RS準拠の中では唯一Emscriptenで
    コンパイル出来るらしい
  + SDLもEmscriptenで動くらしいから
    組み合わせればウハウハじゃね？

コンパイル出来ない…
( `va_args` は64bit x Clang3.3では
コンパイル出来ないとか言われる。回避策も効かない)

<!-- .slide: class="center" -->

ボツ

<!-- .slide: class="center" -->

##  オレオレaltJSの作り方の話
escodegenを使う
```javascript
{
    type: 'BinaryExpression',
    operator: '+',
    left: { type: 'Literal', value: 40 },
    right: { type: 'Literal', value: 2 }
}
→40 + 2
```

オレオレaltJS(S式)  
 ↓ read  
リスト  
 ↓ 変換  (find-file "~/Lisp/translate.lisp")  
リスト  
 ↓ cl-json  
JSON  
 ↓ escodegen  
JS

escodegenのドキュメントがあんまりない…

<!-- .slide: class="center" -->

ボツ

<!-- .slide: class="center" -->

## ParenScript
  + Common Lisp製
  + Weblocks(WAF)に採用されるなどの実績/伝統
  + Common LispのサブセットをJSにコンパイル
  + 実際はCommon Lispのマクロなので
    Common Lispに組み込んで使える
  + ランタイムライブラリは必要ない
  + Lispを無理矢理変換するというより
    LispっぽくJSを書ける感じ

### 例
~/Lisp/parenscript.lisp

 + `(@ obj property)` でプロパティ参照
 + `(chain obj function/property)` でメソッドチェーン
など

以上

<!-- .slide: class="center" -->
</script>
</section>
