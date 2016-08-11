---
categories: [mirah]
date: 2016-08-11T15:32:39+09:00
title: mirah 0.2がリリースされました。
---

κeenです。久しぶりに[mirah](mirah.org)のリリースが出ましたので新機能の紹介をしたいと思います。

リリース版のダウンロードは [こちら](https://github.com/mirah/mirah/releases/tag/0.2.0)から。
<!--more-->
# `==` のセマンティクス変更
Rubyに合わせて、 `==` が等価性、 `===` が同値性の比較になりました。

```mirah
import java.net.URL

puts "1 == 1 => #{1 == 1}"
puts "1 === 1 => #{1 === 1}"
x = 1; puts "x = 1"
puts "x == x => #{x == x}"
puts "x === x => #{x === x}"
puts "URL.new(\"http://hoge.com\") == URL.new(\"http://hoge.com\") => #{URL.new("http://hoge.com") == URL.new("http://hoge.com")}"
puts "URL.new(\"http://hoge.com\") === URL.new(\"http://hoge.com\") => #{URL.new("http://hoge.com") === URL.new("http://hoge.com")}"
x = URL.new("http://hoge.com"); puts "x = URL.new(\"http://hoge.com\")"
puts "x == x => #{x == x}"
puts "x === x => #{x === x}"
```

```
1 == 1 => true
1 === 1 => true
x = 1
x == x => true
x === x => true
URL.new("http://hoge.com") == URL.new("http://hoge.com") => true
URL.new("http://hoge.com") === URL.new("http://hoge.com") => false
x = URL.new("http://hoge.com")
x == x => true
x === x => true
```


# ビルトインMacroの追加

## キャスト
今まで、Mirahのキャスト構文は

``` mirah
int(0.5)
```

のように関数呼び出しっぽい形式でしたが、この度 `as!` マクロが追加されたので以下のように書けるようになりました。

``` mirah
puts "0.5.as!(int) => #{0.5.as!(int)}"
```

```
0.5.as!(int) => 0
```


## Synchronize

javaの `Lock` の `lock` , `unlock` を `sychronize` マクロでラップしました。Javaのビルトインの `synchronized` ではない模様。

``` mirah
import java.util.concurrent.locks.ReentrantLock
lock = ReentrantLock.new
counter = 0
th1 = Thread.new do
  100.times do
    lock.synchronize do
      counter += 1
    end
  end
end

th2 = Thread.new do
  100.times do
    lock.synchronize do
      counter -= 1
    end
  end
end

th1.run
th2.run
th1.join
th2.join
puts "counter = #{counter}"

```

```
counter = 0
```

## `private`

``` mirah
private def my_method
  puts "foo"
end
```

みたいに書けるようになりました。


## ラッパークラスの演算子
`int` に対する `Integer` のようなラッパークラスにも演算子マクロが定義されるようになました。

``` mirah
b = Integer.new(1); puts "b = Integer.new(1)"
b += 1;puts "b += 1"
puts "a < b => #{a < b}"
```

```
a < b => true
```


他にも色々と定義されているようです。

# 改善

* Javaのバイトコードを吐くライブラリasmのバージョンが5になりました。
* コンパイラのの内部で使っているロガーをマクロで書き直すことでDebug出力しない時のコンパイラの速度が速くなりました(確かこの変更が入った時に20%速くなるって言ってたような)。
* マクロが[サービスプロバイダ](http://www.ne.jp/asahi/hishidama/home/tech/java/jar.html#h_Service_Provider)の仕組みに乗っかるようになりました。
  マクロを使うには今までマクロを定義したmirahのソースコードを一緒にコンパイルしないといけなかったのが、Mirahコンパイラがクラスパスから捜すようになったのでJarによる配布が現実的になりました。



# 最後に

マクロ周りが便利になるなど、興味深い変更がありましたね。
みなさんもこれを機にmirahを使ってみて下さい。
