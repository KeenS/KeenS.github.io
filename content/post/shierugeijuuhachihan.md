---
categories: [shell, シェル芸]
date: 2016-07-13T23:34:03+09:00
title: シェル芸十八般
---

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">武芸十八般のひとつシェル芸ってワードがふと湧いてきたけどどちらかというとシェル芸十八般とかの方がよさそう。grep, cut, xargs,...</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/753211663927513088">2016年7月13日</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>
<!--more-->

# `cat`
ファイルの内容を吐くのに使う。 -n とか使えそうなオプションがあるが滅多に使わない

## 例

``` shell
cat file
```


# `head`

標準入力の先頭のほうだけを切り取る。大きなファイルに対して処理する前にちょろっと試すのに使う

## 例

```shell
cat large_file | head -n 10 | ...
```

# `tail`

標準入力の末尾のほうだけを切り取る。`-f` オプションでよく使う

## 例

``` shell
tail -f log_file | grep -ie error -e '$'
```

# `grep`
globally regexp print. 汎用フィルタとして様々に活躍する他飛び道具的な使い方もある。

## 例

``` shell
cat file | grep some_word
```

``` shell
cat file | grep -v "^[ \n\t]+$"
```

``` shell
echo hello | grep -o .
```

# `awk`
プログラミング言語awk。だいたい `cut` の上位互換として使われる。

## 例

``` shell
ls -l | awk '{print $3}'
```

``` shell
seq 1 100 | awk 'BEGIN{sum=0}{sum+=$0}END{print sum}'
```

# `sed`
プログラミング言語sed。だいたい `s/RE/replace/` が使われるが使いこなすと深い。

## 例

``` shell
cat log_file | sed 's/error/ERROR/gi'
```

``` shell
cat some.csv | sed 1d
```

# `xargs`
標準入力を横に並べる、コマンドを並列に実行する、繰り返し実行をするなどストリーム内での制御機構的働きをする

## 例

```shell
seq 1 10 | xargs echo
```

```shell
find . -type f | xargs -P $(nproc) grep FIXME
```

``` shell
inc(){ echo $(( 1 + $1 ));}
export -f sum
seq 1 10 | xargs -I@ bash -c 'sum @'
```

# `find`
複雑なクエリ式を書けるコマンド。(シェルに依存してしまう)ワイルドカード展開の肩代わりなんかも出来る。xargsに繋げることが多い。

## 例

``` shell
find . -type f -name '*.md' -exec basename '{}' .md \;
```

# `sort`
ほぼ次のuniqに繋げるために使うことが多い。あるいはuniqした結果を使う。

## 例

``` shell
cat .histfile | grep '|' | sed 's/|/\n/g' | awk '{print $1}' | LANG=C sort | uniq -c | LANG=C sort
```

# `uniq`
多くは頻度のカウントに使う。

# `tar`
リモートホストとファイルをやりとりするのに使う。

## 例

``` shell
ssh stepper ssh host1 tar czf - -C '/var/log/somedir/' stdout.log stderr.log | tar xzf - -C /tmp
```

# `zcat`

圧縮ファイル扱うのに使う。よくtarと組み合わせる。
プロセスが分かれる分マルチコアを使えるのでtarのzオプション使うより速いかもしれない。

## 例

``` shell
zcat some.tar.gz | tar tf - | xargs -I@ -n 1 basename @ .java
```


# `date`
日付操作用コマンド。BSD系とGNU系で全然違う。つらい。

## 例

``` shell
find logs -type f -name '*.log' | xargs -I@ dirname @ | sed 's|logs/||' | tr / - | xargs -I@ date --date=@ +%w | sort | uniq -c
```

# `tr`
文字置換、削除。`sed`で出来るじゃんとか言わない。

## 例

``` shell
cat data.list | tr -c '[a-zA-Z0-9\n]' _ | xargs touch
```

# `curl`
なんでもプロトコル喋る人。結果を標準出力に吐くのでREST APIを叩いた後シェル芸に繋げやすい。

## 例

```shell
curl http://example.io/status | jq -r '.status'
```

# `jq`
チューリング完全言語jq。REST API叩いてjsonが返ってきた時に使うと便利。


# `od`
バイナリデータを扱うのに使う。

## 例

``` shell
cat /dev/urandom | od -x | head -n 10 | ...
```

# `tee`
T字管のようにパイプを分岐させるのに使う。あるいはシェルの機能であるリダイレクトをコマンドとして実行するのに使う。

## 例

``` shell
cat nginx.conf | ssh stepper ssh host1 sudo tee /etc/nginx/nginx.conf > /dev/null
```

