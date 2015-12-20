---
categories: [Lisp, SKK, Rust, Advent Calendar, Advent Calendar 2015]
date: 2015-12-20T22:40:16+09:00
title: 最近作ってるLispの話
---
このエントリは[Lisp Advent Calendar 2015](http://qiita.com/advent-calendar/2015/lisp)21日目の記事です。

κeenです。最近あまりLispを書いてなくてネタがないので最近作ってるLispの話でもしようかと。

<!--more-->
[κLisp](https://github.com/KeenS/kappaLisp)といいます。まだまだ完成には程遠いです。現在、[ここら辺](https://github.com/KeenS/kappaLisp/blob/master/src/eval.rs#L378)くらいの式なら評価出来ます。

このLispはSKK辞書に使われているS式を評価するために作られました。
SKKは様々な場所に移植されているのでマルチプラットフォームで動いて他の言語と協調が出来る言語で作るのが望ましいのですが、今回はRustを選びました。
最初はCにしようかと思ったのですがちょいとRustを試してみようかと(当時Rustほとんど書いたことがない初心者)。


SKKに書かれているS式を処理出来るLisp処理系はあるのですがGPLでライセンスされており、それを使ったiOSアプリをAppStoreに登録出来ないという問題があるそうです。
なのでMITライセンスで作り始めました。

さて、Lisp処理系くらい簡単に作れそうなものですが、いくつかの点で詰まっています。

* そもそもRustが難しい
* 関数ポインタが思うように扱えない
* やろうとした事がborrow checkに引っ掛かって出来なかった

この詰まった点はRustが悪い訳ではなくて単に私がRustに合わせた設計が出来なかっただけですね。そろそろRustにも少しづつですが慣れて来たのでどこかのタイミングでリファクタリングして進めていきたいですね。

一応現状でも進めようと思えば進めれるのですが[ここ](https://github.com/KeenS/kappaLisp/blob/master/src/eval.rs#L232)のように明らかにイケてないコードが続くので進めるモチベーション起きませんよね。

設計とかの話をすると、最終的にはC APIでS式の文字列を受け取ったらそれを評価した結果の文字列を返す関数を晒せばいいかな、と思ってます。
ちゃんとiOSへのクロスコンパイルも出来ますしObj-C, Swift双方C FFIがあるのでそれで十分でしょう。(CF http://qiita.com/moriturus/items/1190614dcbbe31ecfc2f , http://safx-dev.blogspot.jp/2014/06/rustios.html)

パーサは手書きで、実行はインタプリタ、Emacs Lispに則ってLisp2のダイナミックスコープです。設計がクソなのでメモリ管理らしいメモリ管理はやってませんがRustのメモリ管理のお陰でメモリリークはしてません。
ちゃんと設計してReference Countなメモリ管理でもしようかと。フットプリントの軽さ大事。


全然まとまりませんがいつか使えるようになってAquaSKKやFlickSKKに取り込まれるかもしれないのでその時には裏でこのLispが動いてるかもと思い出してあげて下さい。


