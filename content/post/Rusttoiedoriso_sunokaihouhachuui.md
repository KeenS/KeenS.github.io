---
categories: [Rust]
date: 2016-01-08T00:50:31+09:00
title: Rustといえどリソースの解放は注意
---

κeenです。深夜にですが小ネタを。
<!--more-->

Rustを使っているとついついリソースの解放のことは忘れてしまうのですが、注意しないといけない類のリソースがあります。

その1つが`BufWriter`です。
バッファリングライター全般に言えることですが、奴らはメモリ上にまだ書き込まれてない値を保持しているので解放する前にそれらを掃き出す必要があります。
その時に書き込み例外が起き得るので、解放も安全な処理ではないのです。Javaのtry-with-resource文を使わないファイルの扱いについては悪名高いですが、あれは仕方ない話なのです。

ところでRustのリソースの解放は`Drop`トレイトの`drop`が担っていますが、返り値は`void`です。基本的には裏で動くので当然ですね。
そして、`drop`はpanicを起こさないことが望ましいです。Rustは今のところ(1.5時点)panicから回復出来ないのでそれはそうでしょう。では、エラーを返り値でも返せない、panicも起こせない中`BufWrite`はどうエラーを扱っているのでしょう。それは実装を見ると分かります。


``` rust
impl<W: Write> Drop for BufWriter<W> {
    fn drop(&mut self) {
        if self.inner.is_some() {
            // dtors should not panic, so we ignore a failed flush
            // 訳: dropはパニックすべきではない、だからflushの失敗は無視する
            let _r = self.flush_buf();
        }
    }
}

```


…はい。中々アレなことをやってくれますね。


ということで`BufWrite`を使う時は

``` rust
{
  let file = File::create("some_file.txt").unwrap();
  let br = BufWrite::new(&file);

  // do something

  match br.flush_buf() {
    Ok(()) => (),
    Err(e) => // handle errors
  }

}
```

のようにライフタイムの終わりで`flush_buf`を呼ぶのが作法的な方法です。
`unwrap`と同じく掃き出せない時に無視されてもいいならそこまでする必要はありませんが、それでもスコープの終わりに意図的に`flush_buf`を呼んでない旨を書くと丁寧でしょう。

因みにスコープが大きすぎる時は

``` rust
{
  let file = File::create("some_file.txt").unwrap();
  {
    let br = BufWrite::new(&file);
  
    // do something with br
  
    match br.flush_buf() {
      Ok(()) => (),
      Err(e) => // handle errors
    }
  }
  
  // do other things

}
```

のようにスコープを作って不要なライフタイムを切り詰めるイディオムもあるので併せてどうぞ。

余談ですが`std::fs::File`の実体はCの`FILE`構造体ではなくファイルディスクリプタなのでバッファリングはしてません。のでこちらは気にする必要はありません。


という訳で小ネタでした。
