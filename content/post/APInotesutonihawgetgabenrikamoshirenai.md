---
categories: [CLI]
date: 2015-10-20T00:30:42+09:00
title: REST APIのテストにはwgetが便利かもしれない
---
寝れずに悶々としていたので書いてみる。REST APIを叩くときはデフォルトで結果を標準出力に吐くcURLがよく使われるがテストにはwgetが向いてるかもしれない。
<!--more-->
理由は簡単。exit statusだ。シェルのテストをする時は[shunit2](https://github.com/kward/shunit2)を使うことになるかと思うが、exit statusでテストをしたくなる。
cURLはHTTPサーバーがエラーステータスを返してもexit status 0で終了するのに対してwgetは所定の値を返す。

```
EXIT STATUS
       Wget may return one of several error codes if it encounters problems.

       0   No problems occurred.

       1   Generic error code.

       2   Parse error---for instance, when parsing command-line options, the .wgetrc or .netrc...

       3   File I/O error.

       4   Network failure.

       5   SSL verification failure.

       6   Username/password authentication failure.

       7   Protocol errors.

       8   Server issued an error response.
```

少し試してみよう。


```
$ curl -sL google.com/teapot > /dev/null
$ echo $?
0
$ wget -qO /dev/null google.com/teapot
$ echo $?
8
```

ちゃんと違いが出ている。余談だが `wget -O /dev/null`は便利なので覚えておくと幸せになれる。

# おまけ
HTTPのエラーを全部1つのステータスに纏めるのはちょっと乱暴な気がする。wgetはHTTP以外のプロトコルにも対応してるのでまあ仕方がないのだろうが。
ということで任意のHTTPレスポンスを抜き出してみる。

```
$ curl -sIL google.com/teapot | grep -E '^HTTP/[0-9](\.[0-9])? [0-9]{3}'
HTTP/1.1 301 Moved Permanently
HTTP/1.1 418 I'm a Teapot
```

本当にstatusだけ抜き出したかったらこうだ。

```
$ curl -sIL google.com/teapot | grep -E '^HTTP/[0-9](\.[0-9])? [0-9]{3}' | grep -o -E '[0-9]{3}'
301
418
```

複数回HTTPリクエストを飛ばしているので複数statusが返ってくるのは諦めよう。最後のやつだけ欲しければ `tail -n 1`すればいい。

HTTPの仕様を読まずに書いたのでバージョンのマッチの部分が冗長かもしれないがとりあえず動く。というかcURLを使った。

# まとめ

cURL使おう。
