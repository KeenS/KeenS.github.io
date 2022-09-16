---
categories: ["非同期", "Future"]
date: 2018-10-05T19:55:29+09:00
description:
title: "Futureとその周辺"
---
<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Futureとその周辺
----------------------
[情報科学若手の会 #51](https://wakate.org/2018/07/28/51th-general/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます
 * 言語処理系と継続が好き
 * 科学っぽい話はできないです ><

===
# 背景
-------

* 非同期計算を上手く扱いたい
* 色々あるけど難しい
* 似て非なるものを同じ名前で呼んでて紛らわしい
* 全体を俯瞰したい

===
# 同期計算
-------------

* 同期IO処理はその処理が終わるまで待つ
* 待ってる間が無駄

![同期計算](/images/sync.png) <!-- .element: width="100%" -->

===

# 非同期計算
-------------

* 待ってる間別のことをやりたい
* 処理の切り替えどうするの

![非同期計算](/images/async.png) <!-- .element: width="100%" -->


===

# 非同期計算
-------------

* (限定)継続が取り出せればいい
* 解決！

![限定継続](/images/cont.png) <!-- .element: width="100%"  -->

===
# 限定継続
----------

* 多くの言語では限定継続は扱えない
  + Schemeなら簡単なんだけどねー
* CPS変換すれば限定継続じみたことができる
  + 要はコールバック形式

===
# 問題意識
----------

* コールバック地獄
* デッドロック


===
# コールバック地獄
--------------

* コールバックがどんとんネストしていく問題
* 非同期呼び出しする度に深くなる
* 視認性が悪くなる

===
# コールバック地獄
--------------

```javascript
fs.readdir(source, function (err, files) {
  if (err) {
    console.log('Error finding files: ' + err)
  } else {
    files.forEach(function (filename, fileIndex) {
      console.log(filename)
      gm(source + filename).size(function (err, values) {
        if (err) {
          console.log('Error identifying file size: ' + err)
        } else {
          console.log(filename + ' : ' + values)
          aspect = (values.width / values.height)
          widths.forEach(function (width, widthIndex) {
            height = Math.round(width / aspect)
            console.log('resizing ' + filename + 'to ' + height + 'x' + height)
            this.resize(width, height).write(dest + 'w' + width + '_' + filename, function(err) {
              if (err) console.log('Error writing file: ' + err)
            })
          }.bind(this))
        }
      })
    })
  }
})
```

http://callbackhell.com/


===
# デッドロック
------------

* 個人的経験
* [Aerospike](https://www.aerospike.jp/)のJavaクライアント
 + 非同期IO
 + コールバックが設定できる
* readしてコールバックでwriteした
* → デッドロックした
* 非同期処理はIOスレッドが実行していた
* コールバックもIOスレッドて呼ばれていた

===
# デッドロック
------------


1. 非同期ReadでIOスレッドを専有
2. IOスレッドでコールバック発火
3. 非同期Write発行
4. WriteはIOスレッド待ち & IOスレッドはWrite待ち
5. デッドロック


===

# 色々な視点
------------

* 実行モデル
  + デッドロックの件は実行モデルの知識が足りなかったから起きた
* ユーザインタフェース ← メイン
  + コールバック地獄は主にユーザインタフェースの問題
* 実装方式
  + 処理系の中身みんな知りたいよね！

===
# 実行モデル
-----------

* 多分無数にある
* ただの遅延計算
* IOスレッド1つ
* スレッドプール
  + ブロックするスレッドとさせたくないスレッドを分離
  + スケジューラに無数のバリエーション
* イベントループ
  + スレッドをブロックせずに一杯タスクをこなす
* その他応用

===
# ユーザインタフェース
---------------------

* コールバック
* Future
  + futureにも色々
  + 少し実装も絡む
* `async`/`await`
* `do` 式 / `for` 式
* coroutine
  + coroutineにも色々
* goroutine

===
# 実装方式
----------

* 完全ユーザレベル
* 完全処理系レベル
* 処理系レベルだけど一部ユーザレベル
* ユーザレベルだけど特殊な処理系の機能を使う

===
# コールバック
-------------

* ユーザインタフェース: コールバック
  + 扱いづらい
* 実行モデル: ものによる
* 実装方式: 完全ユーザレベル

===

# Future
--------

* ユーザインタフェース: Future
  + 少しマシになった
* 実行モデル: ものによる
* 実装方式: 完全ユーザレベル

===
# Future
--------

* 並行デザインパターン
* 計算を非同期実行
* 値の引換券(先物 = future)を返す
* 実行した値を受け取れる
* みなさん知ってますよね

===

# diff to コールバック
-----------------------------

* 値になる
 + 「あとで呼ばれる」という暗黙の文脈が「値」という一級市民になった
* 続けて処理を書ける
  + map, andThen, ...
  + 要はモナド
* そのままだとコールバック地獄は変わらない

===
Scala標準ライブラリ

``` scala
val purchase = usdQuote flatMap {
  usd =>
  chfQuote
    .withFilter(chf => isProfitable(usd, chf))
    .map(chf => connection.buy(amount, chf))
}
```

https://docs.scala-lang.org/ja/overviews/core/futures.html


===
# Future x 実行モデル
--------------------

* Futureは基本的にはコールバックの抽象化
* 特定の実行モデルとは結びつかない
* …とでも思ったか！
* 実装によって千差万別

===
# Future x 実行モデル
--------------------

* Scala: 標準ライブラリの`Future`
  + Futureそのものは実行モデルと結びつかない
  + `Future` の作成にスレッドプールが必要
  + 実装レベルでは分離
  + APIレベルでは結合してる
* Clojure: clojure.coreの`future`
  + 雑にスレッドプールに処理を投げる
  + 完全に密結合

===
# Future x 実行モデル
--------------------

* Scala: TwitterUtilの`Future`
  + Futureそのものは実行モデルと結びつかない
  + APIレベルでも分離
  + 別途スレッドプールも用意される
* Rust: futures-rs
  + Futureそのものは実行モデルと結びつかない
  + APIレベルでも分離
  + 別途スレッドプールも用意される

===
# Future x 実行モデル
--------------------

* Futureとスレッドプールが密結合
  + 手軽に並列化できる
* Futureと実行モデルは粗結合
  + 計算を合成してからスレッドプールに投げられる
   - 計算と実行を別々のライブラリにできる

===
# Futureはいつ実行される
--------------------

* 前のタスクが終了した直後に実行
* ジョブキューに積まれていつか実行
* 値をgetするとき
* イベントループがpollする
* …


===
# Futureの構文糖衣
------------------

* Futureを使ってもコールバック地獄は変わらない
  + 構文糖衣でどうにかする
* Futureはモナド
  + 普通のプログラムっぽい書き方ができるはず
* 普通は構文糖衣は処理系のサポートが必要

===
# Futureの構文糖衣
------------------

* `async`/`await`
  + 大半処理系、一部ユーザ
  + ジェネレーターが必要
* `do` 式/`for`式
  + 処理系の機能に乗っかる
  + 高階多相が必要
* Lispだとマクロでユーザレベルで可能

===
# `async`/`await`
-----------------

``` c#
// Signature specifies Task<TResult>
async Task<int> TaskOfTResult_MethodAsync()
{
    int hours;
    // . . .
    // Return statement specifies an integer result.
    return hours;
}

// Calls to TaskOfTResult_MethodAsync
Task<int> returnedTaskTResult = TaskOfTResult_MethodAsync();
int intResult = await returnedTaskTResult;
// or, in a single statement
int intResult = await TaskOfTResult_MethodAsync();
```

===
# `do` 式
------

``` haskell
   do a1 <- async (getURL url1)
      a2 <- async (getURL url2)
      page1 <- wait a1
      page2 <- wait a2
      ...
```
http://hackage.haskell.org/package/async-2.2.1/docs/Control-Concurrent-Async.html

===
# `for` 式
-----------

``` scala
val usdQuote = Future { connection.getCurrentValue(USD) }
val chfQuote = Future { connection.getCurrentValue(CHF) }

val purchase = for {
  usd <- usdQuote
  chf <- chfQuote
  if isProfitable(usd, chf)
} yield connection.buy(amount, chf)
```

https://docs.scala-lang.org/ja/overviews/core/futures.html


===
# Lisp
--------

```common-lisp
(alet ((x (grab-x-from-server1))
       (y (grab-y-from-server2)))
  (format t "x + y = ~a~%" (+ x y)))
```

http://orthecreedence.github.io/blackbird/


===

# 発展的話題
------------

* コールバック形式でなくて直接形式で書きたい
* もっと直接限定継続を取得したい
* 1回しか実行しなくていいので限定継続より軽いモデルでいい
* → コルーチン

===
# コルーチン
-----------

* 対称/非対称コルーチンがある
  + 対称: 各コルーチンが対等。 `transfer` で切り替える
  + 非対称: コルーチンに親子関係がある。 `yield` で親に、 `resume` で子に切り替える
* stack full/stack lessがある
  + stack full: 呼び出された関数内から外のコルーチンを `transfer` / `yield` できる
  + stack less: コルーチン直下でしか `transfer` / `yield` できない
* fiber = 準コルーチン = 非対称コルーチン

===

``` ruby
f = Fiber.new do
  n = 0
  loop do
    Fiber.yield(n)
    n += 1
  end
end
```
https://docs.ruby-lang.org/ja/latest/class/Fiber.html

===

``` ruby
require 'fiber'

fr1 = Fiber.new do |v|
  :fugafuga
end

fr2 = Fiber.new do |v|
  fr1.transfer
  :fuga
end

fr3 = Fiber.new do |v|
  fr2.resume
  :hoge
end

p fr3.resume # => :fugafuga
```

https://docs.ruby-lang.org/ja/latest/method/Fiber/i/transfer.html

===

![対称コルーチン](/images/coroutine.png) <!-- .element: width="100%" -->

===

![非対称コルーチン](/images/semicoroutine.png) <!-- .element: width="100%" -->

===
# Futureとの関係
----------------
* `async`/`await`はstack less非対称コルーチン(=generator)の上に実装されることが多い
  + jsの`async`/`await`とか
  + Rustの`async`/`await`もそうなる予定
* `do`式/`for`式はstack less非対称コルーチンを実装できる
  http://hackage.haskell.org/package/monad-coroutine-0.9.0.4/docs/Control-Monad-Coroutine.html

===

![async/awaitと機能の依存](/images/async-await.png) <!-- .element: width="100%" -->

===

![do式と機能の依存](/images/do-async.png) <!-- .element: width="100%" -->


===

# goroutine
------------

* Goのあれ
  + 他の言語にもある
* 完全処理系レベルの実現
* stackfullな対称coroutine(多分) + IOをトリガとした自動スケジュール
  + 直接形式で書ける
* 自分でtransferを書かないのでスレッドに近い見た目

===

``` go
func say(s string) {
	for i := 0; i < 5; i++ {
		time.Sleep(100 * time.Millisecond)
		fmt.Println(s)
	}
}

func main() {
	go say("world")
	say("hello")
}
```
https://tour.golang.org/concurrency/1

===
# スレッドとの違い
----------------
* スレッドはプリエンプティブ
  + スケジューラが勝手に止めたり起動したりする
* goroutineはノンプリエンプティブ
  + 自身でIOする直前に別のgoroutineにtransferする
  + スケジューラは次にどのgoroutineを起動するか選ぶだけ

===
# goroutine
-----------

* Goのgoroutine
  + mainもgoroutine
  + ランタイムにスケジューラが組込
* JavaのProject Loom
  + 処理系に手を入れることで既存ライブラリもサポート
  + 用語が変
* Clojureのcore.async
  + なんとユーザレベルで実現
  + マクロでSSA変換

===
# goroutine
-----------

* プログラマ的には直接形式で書けるので扱いやすい
* 処理系的には処理系全体でサポートする必要がある
  + IOのタイミングを全部掌握
  + FFIとかも
* 実行モデルは決め打ちになる


===
# 結論
--------

* 非同期に対して色々なアプローチがある
* 同じ名前で中身が違うことが多々有る
* 結局抽象化を解いて中身を見るしかない

===
# まとめ
--------

* コールバックを値にしたのがFuture
* Futureに構文糖衣を被せたのが`async`/`await`とか
* Futureと実行モデルはほぼ直行する
* 別のアプローチとしてgoroutineがある
* goroutineは実行モデルと密結合
* Lispはなんでもできる


</textarea>

