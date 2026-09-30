---
categories: [Rust, CFG, compiler]
date: 2026-09-29T20:49:59+09:00
description: コンパイラのコンパの部分での発表用。自作しているパーサライブラリについて
title: "パーサをもっと便利にしたい"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# パーサをもっと便利にしたい
----------------------
[コンパイラのコンパの部分 #03 - connpass](https://compiler.connpass.com/event/401886/)
<!-- .slide: class="center" -->

===
# About Me
---------
![κeenのアイコン](/images/kappa2_vest.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

* κeen
* [@blackenedgold](https://twitter.com/blackenedgold)
* GitHub: [KeenS](https://github.com/KeenS)
* GitLab: [blackenedgold](https://gitlab.com/blackenedgold)
* コンパイラエンジニア
* Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# みんな…
---------------

* 正規表現使ってる？
* パーサ使ってる？
  + yaccとか

===
# なんで？
----------

* CFGはRegexの上位互換
  + 正規言語 ⊂ 文脈自由言語
* 全部CFGでよくない？

===
# なぜ使われない？
---------------

* APIが違う
  + 文字列 vs トークン列
  + `match`, `find`, `capture`, …
  + マッチ位置 vs AST構築
* ツールが違う
  + プログラム内vsコードジェネレータ

===
# パーサを便利にしたい
-------------------------

* Regexの手法を取り入れられない？
* API真似したい
  + 文字列受け取りたい
  +  `match`, `find`, `capture`
  + マッチ位置だけあればいい
* ツール真似したい
  * プログラム内で動かしたい

===
# 作ってる
--------

[KeenS/regext](https://github.com/KeenS/regext)
(まだプロトタイプ)

===
# 使い方
-------

```rs
use RegExt::Regex;

let re = Regex::new("a(b)+c").unwrap();
assert!(re.is_match("abbbc"));
```

===

```
let sign = Pattern::lit("+")
    .or(Pattern::lit("-"));
let re = Regex::from_pattern(
    sign.opt()
        .then(Pattern::digit()
        .between(1, 3))
    ).unwrap();

```

===

```rs
CfgParser::new(r#"S -> "(" S ")" | "a""#).unwrap();
parser.is_match("((a))")
```

===
# 他にも…
---------

* 文字列じゃない式
* `find`, `capture`

[composable_api.rs](https://github.com/KeenS/regext/blob/master/examples/composable_api.rs)  
[innermost_div.rs](https://github.com/KeenS/regext/blob/master/examples/innermost_div.rs)

===
# 裏話
------

* 100%ローカルLLMで実装した
  + 正直つらかった
* 教科書レベルの知識の組み合わせは以外といける
* 今後課金LLMで高速化とかやるかも？


</textarea>
