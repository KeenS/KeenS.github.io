---
categories: [言語処理系]
date: 2015-05-17T01:33:39+09:00
draft: true
description: "<p>新人研修でクロージャで躓いてる人が多いようだったので手助けになればと思って作ったもの。
特に発表予定はない。</p>
<p>分かりやすくなると思って作ったけどむしろ混乱する気しかしない</p>
"
title: 実装から理解するクロージャ
---

<section data-markdown
    data-separator="\n\n"
    data-vertical="\n\n"
    data-notes="^Note:">
<script type="text/template">
# 実装から理解するクロージャ
----------------------

<!-- .slide: class="center" -->

# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 + κeen
 + [@blackenedgold](https://twitter.com/blackenedgold)
 + Github: [KeenS](https://github.com/KeenS)
 + 渋谷のエンジニア
 + Lisp, ML, Shell Scriptあたりを書きます


# クロージャとは？
-----------------

* 日本語にすると（関数）閉包
* 関数が外側のローカル変数を補足する
* 補足されたローカル変数は無限の生存期間を持つ
  + ローカル変数は本来スコープを抜けると生存期間が終わる
  + 言い換えるとグローバル変数みたいになる
  + でもあくまでスコープはローカル

# コード例
---------

```js
function genpower(n){
    var x = 1;
    return function(){
        x *= n;
        return x;
    };
}
var p = genpower(2);
```

# コード例
----------

```js
p()  // => 2
p()  // => 4
p()  // => 8
x * 2 // x is not defined
```


# コード例
----------

* `p`が`n`と`x`を補足しているので関数を抜けた後も`x`と`n`は使える。
  + 関数の仮引数もローカル変数。
* でもローカル変数なので外からは見えない。


図が入る予定

# コード例2
----------

```js
function incdec(){
    var x = 0;
    return [function(){ return ++x;},
            function(){ return --x;}];
}
var fs = incdec();
var inc = fs[0];
var dec = fs[1];
```

# コード例2
----------

```js
inc() // => 1
inc() // => 2
dec() // => 1
inc() // => 2
```

# コード例2
----------

* 同じタイミングで作られたクロージャ群は捕捉変数を共有する


図が入る予定

# コード例3
----------

```js
function genpower(n){
    var x = 1;
    return function(){
        x *= n;
        return x;
    };
}
var p1 = genpower(2);
var p2 = genpower(2);
```

# コード例3
----------

```js
p1()  // => 2
p1()  // => 4
p2()  // => 2
p2()  // => 4
```

# コード例3
----------

* 逆に、同じ関数から生まれても違うタイミングなら共有しない。


図が入る予定

# 実装方法
----------

* ここでは複数ある実装方法のうちの1つを紹介する。
* 言語はVM型のインタプリタ（大抵のインタプリタの実装に同じ）を仮定する


# 用語整理
----------
* `outer`から見たら`x`は捕捉(Captured)変数
  + `inner`から捕捉されてるから
* `inner`から見たら`x`は自由(Free)変数
  + `inner`からしたら`x`は知らない子だから

```js
function outer(x) {
    function inner(y){
        return x * y;
    }
}
```


# 実装概要
----------

* **クロージャとは捕捉変数の集まり**
  + つまり、捕捉した側ではなくされた側が作る
  + 捕捉した側は作られたものを参照するだけ

# 変数の話
---------

* グローバル変数はヒープ領域に置かれる
  + グローバル DB（大抵巨大なハッシュテーブル）に登録される
* ローカル変数はコールスタックに置かれる
  + 配列が作られ、インデックスでアクセスされる感じ。
  + ローカル変数の数は関数定義時に決定するので配列で管理出来る
  + 関数の実引数も同じように置かれる

# 捕捉変数の話
-------------

* 捕捉変数はヒープ領域に置かれる
  + 簡単には小さなハッシュテーブルに登録される
    - つまり、グローバル変数と同じ
    - 捕捉変数も関数定義時に決定するので配列でも管理出来る
  + ハッシュテーブル/配列はクロージャ毎に作られる

※ [本気出した実装](http://practical-scheme.net/docs/stack-j.html)だとコールスタックでどうにかすることもある

```js
var global = 1;
function sample(arg) {
    var local = 2;
    var captured = 3;
    return function(){ return captured;};
}
```

<!-- .slide: class="center" -->


図が入る予定

# まとまってないけどまとめ
-------------------------

* クロージャの正体は変数の集まり
* 外からは見えないグローバル変数のようなもの
</script>
</section>
