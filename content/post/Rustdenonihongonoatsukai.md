---
categories: [Rust, Advent Calendar, Advent Calendar 2015, 小ネタ]
date: 2015-12-13T17:14:18+09:00
title: Rustでの日本語の扱い
---

このエントリは[Rust Advent Calendar](http://qiita.com/advent-calendar/2015/rust-lang) 14日目の記事です  
前:gifnksmさんの [kcovを使ってRustプログラムのカバレッジを測定する - gifnksmの雑多なメモ](http://gifnksm.hatenablog.jp/entry/2015/12/13/204655)  
後:nacika_insさんのtimeこわい  

κeenです。当初の予定より小ネタになってしまいましたがRustでの日本語の扱いについて書こうかと。
<!--more-->
ご存じの通りRustはマルチバイト文字をサポートしていますが、その殆どがUTF-8だけです。しかし日本人にはUTF-8だけでは少し辛いものがありますのでどうにかして対応しようかと。


ソースコード中でもマルチバイト文字を扱えますが、UTF-8である必要があります。

例えば

``` rs
fn main() {
  println!("あああ");
}
```


これをISO-2022-JPで保存してrustcに食わせても、

``` rust
error: couldn't read "src/main.rs": stream did not contain valid UTF-8
Could not compile `japanese`.
```

と無下もないエラーが出てしまいます。

内部エンコーディングにもUTF-8が使われているのでRustの文字列をUTF-8以外で出力するにはなんとなく変換が必要そうなのは理解出来ますが、変に出入力をラップしてる関数を使うとまた意図せぬエラーが出ます。


``` rust
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
  let file = File::open("japanese.txt");
  let br = BufReader::new(&file);
  for line in br.lines() {
    matich line {
      Ok(l) => println!("Ok: {}", l),
      Err(e) => println!("{:?}", e)
    };
  }
}
```

これもまた、`stream did not contain valid UTF-8`エラーを出します。

ReadもBufReadもbyte orientedって言ってるのに何故か文字列を扱えるのが若干気になりますがまあ、そういうもんなんでしょう。

# encoding
そこで救世主となるライブラリが[lifthrasiir/rust-encoding](https://github.com/lifthrasiir/rust-encoding)です。文字列のエンコーディングをサポートします。
READMEに詳しい使い方が載っているのですが、このライブラリは与えられたスライスに対して指定されたエンコーディングでエンコード/デコードします。

なので先の例のようにISO-2022-JPで`"あああ"`と出力したいなら

``` rust
extern crate encoding;
use encoding::{Encoding, EncoderTrap};
use encoding::all::ISO_2022_JP;
use std::io::Stdout;

fn main() {
  let bytes = ISO_2022_JP.encode("あああ", EncoderTrap::Ignore);
  Stdout.write_all(&bytes[..]);
}

```

と出来るでしょう。(即興で書いてコンパイルすらしてないので本当に出来るかは分かりませんが)

読み取りに関しては…………。そう、このライブラリはバッファに対してしか動作せず、ストリームに使えないのです。提供してくれたら良さそうなのですが、作者の方が忙しいようです。

<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/blackenedgold">@blackenedgold</a> stream encoding/decodingは計画してはいるのですが、私が忙しくてなかなか作業をすることができません... orz</p>&mdash; Kang Seonghoon (@senokay) <a href="https://twitter.com/senokay/status/666516796942319616">2015, 11月 17</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

余談ですがこの方は韓国の方のようですが、日本語もある程度分かるそうです。すごいですね。


さて、ストリームに対して使えないと分かったなら自分で対応するまで。幸いにも我々が使っているのはRustです。新しいものを既存の仕組みに載せるのは造作もありません。
ストリームを良い感じにデコードするには[std::io::Read](https://doc.rust-lang.org/std/io/trait.Read.html)を実装した何かを作れば十分です。作っていきましょう。`Read`トレイトを実装するには最低`read`だけ実装してしまえば十分です。

この`read`ですが、ドキュメントを読むと中々制限が緩いようです。

> Pull some bytes from this source into the specified buffer, returning how many bytes were read.

>This function does not provide any guarantees about whether it blocks waiting for data, but if an object needs to block for a read but cannot it will typically signal this via an Err return value.

>If the return value of this method is Ok(n), then it must be guaranteed that 0 <= n <= buf.len(). A nonzero n value indicates that the buffer buf has been filled in with n bytes of data from this source. If n is 0, then it can indicate one of two scenarios:

>    This reader has reached its "end of file" and will likely no longer be able to produce bytes. Note that this does not mean that the reader will always no longer be able to produce bytes.
>    The buffer specified was 0 bytes in length.

>No guarantees are provided about the contents of buf when this function is called, implementations cannot rely on any property of the contents of buf being true. It is recommended that implementations only write data to buf instead of reading its contents.

です。

> No guarantees are provided about the contents of buf when this function is called, implementations cannot rely on any property of the contents of buf being true.

「関数が呼ばれる際に`buf`に対しては何如なる保証もなく、呼出側はバッファの内容に対して何如なる不変条件も期待してはいけません」とあります。つまり、4096 byteのバッファが与えられても1呼び出しにつき1バイトしか返さないような実装でも許容されます。
また、非同期ベースのIOのようにキャッシュにデータがなければIOブロッキングせずにエラーだとかの挙動もありえます。非常にシステムプログラミング言語らしい仕様ですね。


今から作ろうとしているデコーディングストリームも読み出したバイト列がデコード後に何バイトになるのか予測がつかないのでこの仕様は有難いですね。


さて、作っていきましょうか。まずは必要そうなライブラリを読み込みます。

``` rust
extern crate encoding;
use self::encoding::{EncodingRef, DecoderTrap};
use std::io::Read;
use std::io;
```


そしてメインとなる構造体です。とある`Read`を実装した型をラップする形にしましょう。

``` rust
pub struct DecodingReader<R> {
    inner: R,
    encoding: EncodingRef,
}
```

`Read`はトレイトなので`inner`のところを`&R`としなくて大丈夫です。`Read`の実装側が調整します。


そしてコンストラクタですね。


``` rust
impl <R: Read> DecodingReader<R> {
    pub fn new(coding: EncodingRef, read: R)-> Self {
        EncodingReader {
            inner: read,
            encoding: coding
        }
    }
}
```

そのままですね。

`Read`の実装です。まずは`inner`の`read`を呼んで、その結果を一旦デコードしてからまたバイト列に変換します。

``` rust
impl <R: Read> Read for DecodingReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize>{
        // TODO set appropriate buffer size
        let len = buf.len() / 2;
        let n = try!(self.inner.read(&mut buf[..len]));
        // TODO don't ignore input code
        let str = self.encoding.decode(&mut buf[..n], DecoderTrap::Ignore).unwrap();
        let mut i = 0;
        for (d, s) in buf.iter_mut().zip(str.bytes()) {
            *d = s;
            i+=1;
            if i == 0 {
                break
            };
        }
        assert!(i <= buf.len());
        Ok(i)
    }
}
```

先程も言った通りデコード読み込んだバイト列をUTF-8にした時にどれくらいバイト数が変わるか分からないので山勘で2倍くらいに膨らむだろうとして与えられたバッファ長の半分を使います。
ここはもう少し調整が必要ですね。エンコーディング毎に適切なバッファ長を設定したい…

バッファを確保出来たら`let n = try!(self.inner.read(&mut buf[..len]));`してそれっぽいサイズ読み込んで、読み込んだサイズをデコードにかけます。

現在`DecoderTrap`を`Ignore`にして`unwrap`してますが、これは少し雑すぎますね。例えば入力が全てvalidな文字を含んでいたとしても`read`が多バイト文字の真ん中までしか読まなかった時にそれが無効なエンコーディングと判定されて無視されるのでユーザからしたら「合ってる筈なのに何故か1文字欠損するバグ」として現れます。今回即席で作ったので勘弁して下さい。

変換は文字列にしか出来ないようなので一旦文字列に落してからまたバイト列として取り出し、バッファに書き出します。デコード後のバイト列が`read`したバイト列より小さかったら一部`read`したデータが残ってしまいそうですが、「何如なる不変条件も…」とあるので問題ないでしょう。返り値にデコード後のバイト列の長さを返していれば良い筈です。


さて、エンコードのエラーハンドリングしてないだとか本当に読み込みのバッファ長が適切なのかとかの疑問はあるものの、一応ストリームのデコーダが出来ました。使ってみましょう。


``` rust
extern crate encoding;
extern crate japanese;

use encoding::{Encoding, DecoderTrap};
use encoding::all::ISO_2022_JP;

use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;
use std::str::from_utf8;
use japanese::buf::DecodingReader;


fn main() {
    let file = File::open("src/japanese.txt").unwrap();
    let e = DecodingReader::new(ISO_2022_JP, &file);
    let mut br = BufReader::new(e);
    for line in br.lines() {
        println!("{}", line.unwrap());
    }
}
```

これでちゃんとISO-2022-JPのファイルを読めます。

Javaに慣れてる人ならこういうIOに対するデコレータパターンは見覚えがあるでしょう。

今更ですがiso-2022-jpはいわゆるjisエンコーディングです。rust-encodingはshift_jisの拡張にあたるwindows31jだとかeuc-jpも扱えます([参考](https://lifthrasiir.github.io/rust-encoding/encoding/codec/japanese/index.html))。


# 今後
ライブラリとして公開するか、あるいは作者の方がやる気はあるようなのでプルリクを送るかが終着点かな、と思っていますが如何せん雑にしか実装してないので微妙ですね。
例の境界とマルチバイトの問題は`Read`じゃなく`BufRead`を要求して1文字分読み切るまで来るまで1バイトずつ読むとかになると思います。ダルい。


# まとめ
* Rust標準だけでUTF-8以外の文字列を使うのはつらいよ
* Rustのエンコーディングライブラリを紹介したよ
* エンコーディングライブラリを使ってストリームデコーダを実装してみたよ。

今回のコードは[こちら](https://github.com/KeenS/japanese)にあります
