---
type: post
title: "mirahについて"
date: 2014-12-04
comments: true
categories: [Advent Calendar, Mirah]
---
このエントリーは[マイナー言語 Advent Calendar 2014 - Qiita](http://qiita.com/advent-calendar/2014/minor-language)4日目の記事です。  
前: ksmakotoさんで[マクロ、拙作のテキストマクロ言語m55について主に - ksmakotoのhatenadiary](http://ksmakoto.hatenadiary.com/entry/2014/12/03/002435)

κeenです。進捗ダメです。マイナー言語Advent Calendarが空いてるようだったのでMirahの話を捩じ込みますね。
<!--more-->
# Mirahとは？
[以前の記事](http://keens.github.io/blog/2014/08/07/mirah-0-dot-1-3/)を参照していただければだいたい分かるかと思いますが、「Ruby風の文法で書けるJava」です。JRubyのようにRubyをJavaで実装したのでもなくGroovyのようにJVM上で動く別の言語でもなく、あくまでJavaそのものです。

JRubyとの違いは理解頂けるかと思いますが、Groovyとの違いを説明するとしたら一番はランタイムですかね。Groovyはコンパイル後のソースもGroovyを必要としますがMirahはコンパイルされたら普通のJavaで書いたのと同じ.classファイルになります。

かといってくるくる括弧(`{}`)を`end`で書けるだけかというとそうでもなく、多くのシンタックスシュガーがマクロとして実装されています。

例えば

```ruby
10.times do |i|
  puts i
end
```

はJavaの

```java
for(int i = 0; i < 10; i++)
    System.out.println(i);
```

と同じバイトコードを生成します。

# v0.1.4リリースおめでとうございます。
2014-11-14にv0.1.4がリリースされました。ラムダ式について少し進展があったようです。

```ruby
(lambda Runnable do
 1000.times { puts "Hello"}
end).run
```
とか書けます。が、

```ruby
(lambda Runnable do
 1000.times { puts "Hello"}
end).start
```

とは書けませんでした。やりたいのはこっちなのにね。ちょっと突っついてみます。

[ここ](https://github.com/mirah/mirah)からそれっぽい情報入手出来ます。

[ここ](https://github.com/shannah/mirah-nbm)からNetBeansのプラグインを入手出来ます。


