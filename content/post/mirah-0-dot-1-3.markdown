---
type: post
title: "Mirah 0.1.3がリリースされました"
date: 2014-08-07T23:52:15Z
comments: true
categories: [Mirah, Java, Ruby]
---
本日2回目のκeenです。次はRuby-likeな文法でJavaが書けるMirah言語の0.1.3がリリースたのでこれを期にMirahの紹介をします。
<!--more-->
# Mirahとは？
[公式ページ](http://mirah.org)。JRubyの開発者がJRubyのためにJavaを書くのが嫌になったという理由で作り始めた言語です。2008年にスタートだったかな？JRubyの開発の片手間に開発してるのでコミットペースはゆっくりです。

[github](https://github.com/mirah/mirah)のREADMEから引用すると、

* Ruby-like シンタックス
* .classにコンパイルされる
* Javaと同じスピード
* ランタイムライブラリ必要なし

な言語です。私的には

* 型推論がある
* マクロがある
* 面倒な部分はコンパイラが補ってくれる

、Javaです。多くのJVM上の言語は独自言語+Java FFIって感じですが、Mirahは文法をRuby風にしただけで、吐かれるバイトコードはJavaコンパイラが吐くのものと等価です。

# サンプルコード
一番Javaっぽいコードを見せましょう。ファイル名は`HelloWorld.mirah`の他に`hello_world.mirah`でも構いません。

```ruby
class HelloWorld
  def self.main(args:String[]):void
    System.out.println("Hello World")
  end
end
```

まあ、Javaですね。JavaのstaticメソッドとRubyのクラスメソッドが対応しています。これをMirahの機能を使って書き換えていきます。

## 暗黙のクラス
ファイル名からクラス名が推測出来るので省略することが出来ます。すなわち、トップレベルのメソッド定義は推測されたクラス内でのメソッド定義として扱われます。

```ruby
def self.main(args:String[]):void
  System.out.println("Hello World")
end
```

これでも動きます。

## 暗黙のmain
トップレベルの式はmain内のものとして扱われます。

```ruby
System.out.println("Hello World")
```

こうも書けることになります。ただし、argsにアクセス出来ないので複雑なことをやりたかったら大人しく`self.main`を書きましょう。

## putsマクロ

mirahには組込みで`puts`というマクロが定義されています。これはコンパイル時に`System.out.println`に展開されます。

```ruby
puts "Hello World"
```

こう書けます。ここまでくるとRubyと同じコードになりますね。あ、()が省略可能なのは良いですよね。Ruby系の言語ではよくあることです。

# もうちょっとサンプル

mirah/exmpleから面白いのを拾ってきます。だいたいRubyです。

## リテラル

ほぼRubyです。つまり、`[]`が配列ではなくArrayListになってます。尚、自動でintがIntegerに変換されてます。
また、hashはHashMapです。

```ruby
str = 'non-interpolated string'
str2 = "interpolated is better than #{str}"
heredoc = <<EOS
this is a here doc
EOS
int = 42
char = ?a
float = 3.14159265358979323846264
regex = /\d(cow)+\w\\/  # in Java, this would be "\\\\d(cow)+\\\\w\\\\\\\\"
regex2 = /interpolated #{regex}/
list = [1, 2, 3]
list[2] = 4
array = byte[5]
array[0] = byte(0)
hash = { "one" => 1, "two" => 2 }
hash["three"] = 3
```

## 修飾import

```ruby
import java.util.HashMap as H
```
って書けます

## 型推論

Swingの例です。変数の型を書いてないことに注目して下さい。

```ruby
import javax.swing.JFrame
import javax.swing.JButton

# FIXME blocks need to be inside a MethodDefinition, but main doesn't
# have one.
def self.run
  frame = JFrame.new "Welcome to Mirah"
  frame.setSize 300, 300
  frame.setVisible true

  button = JButton.new "Press me"
  frame.add button
  frame.show

  button.addActionListener do |event|
    JButton(event.getSource).setText "Mirah Rocks!"
  end
end

run
```

## 暗黙のInterface及び暗黙のabstractメソッド

先のSwingの例を良く見て下さい。この部分です。

```ruby
  button.addActionListener do |event|
    JButton(event.getSource).setText "Mirah Rocks!"
  end
```

Javaだと

```java
button.addactionlistener(new ActionListener(){
    public void actionPerformed(ActionEvent event){
        JButton(event.getSource).setText("Mirah Rocks!");
    }
});
```
となっていたところが、

1. 引数の型がインターフェースだったときはブロックで`new Class(){}`と同じ働きになる
2. abstractメソッドが一つのときはそれも省略出来る

というルールにより簡潔に書けます。これで引数の中に文が現れるという最悪の事態を回避出来ます。Java8のlambda式に近いのかな？Java8に詳しくなくてゴメンなさい。

似たようなので、Threadも

```ruby
Thread.new do
 # do something
end.start
```
と書けます。

## マクロによる既存クラスの拡張
マクロは展開後のASTがJavaとして有効であれば良いのでJavaでは出来ない芸当が可能です。

シンプルだけど強力な例

```ruby
10.times{ puts "Hi"}
```
intを`times`マクロで拡張してます。その他、`each`などの便利マクロや`attr_accessor`(getterとsetterを自動生成する)など色々あります。ユーザー定義のマクロで拡張も可能ですが、今シンプルに書けるシンタックスが議論中です。

# Javaとの互換性とか完成度とか
まだ未実装機能はいっぱいあります。`final`とか`synchronized`とか。あとスコープもRuby風に`private`以下で定義されたものはprivateですがメソッド/フィールド単位では制限出来ません[^1]。ジェネリクスの構文もまだサポートされてません[^2]のでジェネリクスの定義は不可能、使用も型推論で型を明示的に書かなくても良いときのみ可能です。インターフェースやアノテーションはあります。

[ここ](https://docs.google.com/spreadsheets/d/1t7NVsyysIlj6OF6E26OrVJ1AVrL8i2yzbMv92cdHN8c/edit#gid=0)にTODOがありますが、inner classやlambda(多分Java8のlambda式とは別もの)が弱いようです。

でもまあ、Mirah自体Mirahでセルフホスティングされてますし一つ言語を作れる程度には機能は揃ってます。遊んでみる分には十分使えると思います。

# マクロの話
Mirahはオブジェクト指向で静的型付けの言語でマクロを実装してます。Lisper的には割と面白かったのでちょいと触れますね。

まずは簡単な例から。

```ruby
macro def puts(node)
  quote {System.out.println(` [node] `)}
end
```

Lisperなら

* `macro def`で`defmacro`
* `quote block`でquasiquote
* バックスラッシュで囲んでunquote

などが読み取れると思います。

今のはASTは陽には出てこない簡単な例でしたが、次はちょっと飛躍しますよ？

```ruby
macro def self.abstract(klass:ClassDefinition)
  anno = Annotation.new(@call.name.position, Constant.new(SimpleString.new('org.mirah.jvm.types.Modifiers')),
                        [HashEntry.new(SimpleString.new('flags'), Array.new([SimpleString.new('ABSTRACT')]))])
  klass.annotations.add(anno)
  klass.setParent(nil)
  klass
end
```

* macroにも型がある。その型はASTの型。
* というかClassDefinitionとかいう型がある
* ASTをいじるときにASTのNodeオブジェクトのメンバをゴニョゴニョするという手段がある

などが読み取れると思います。また、ClassDefinitionを受け取ってClassDefinitionを返しているのでmacro chainが可能ですね。

次はASTを自分で組み立てる例です。

```ruby
  macro def self.attr_reader(hash:Hash)
    methods = NodeList.new
    i = 0
    size = hash.size
    while i < size
      e = hash.get(i)
      i += 1
      method = quote do
        def `e.key`:`e.value`  #`
          @`e.key`
        end
      end
      methods.add(method)
    end
    methods
  end
```

NodeListがprognみたいなものでその中にMethodDefinitionを突っ込んでいってますね。中々楽しい。

余談:ところでgetterメソッド名がgetKeyじゃなくてkeyになってますよね。コンパイル後は変換してくれるのかなと思い、
```ruby
@foo = "a"
attr_accessor :foo => :String
```
をコンパイル、ディスアセンブルしてみました。すると、`foo`と`set_foo`というメソッドが定義されてましたorz。Ruby的にはまあ良いんですがコンパイル後はJavaなのでそこはgetKey/setKeyにしてほしかったですね。もしかしたら今はシンボルをキャメルケースに変換出来ないのかもしれません

閑話休題。また、呼び出し元の情報もとれます。これはStringの+マクロです。

```ruby
macro def +(arg)
  quote { "#{`@call.target`}#{`arg`}" }
end
```

`@call`に呼び出し元の情報が入ってるのでそれを使って情報をとれます。

gensymなんかもあります。これはintのtimesマクロです。

```ruby
macro def times(block:Block)
  i = if block.arguments && block.arguments.required_size() > 0
    block.arguments.required(0).name.identifier
  else
    gensym
  end
  last = gensym
  quote {
    while `i` < `last`
      init { `i` = 0; `last` = `@call.target`}
      post { `i` = `i` + 1 }
      `block.body`
    end
  }
end
```
whileの中にある`init`と`post`はRubyでいう`BEGIN`と`END`、JVM的には`for`の実装のために使われているのでしょうか。

# まとめとか雑感とか
0.1.3で一番大きな変更はセルフホストされたことですね。今まではJRubyで書かれてたのでHello Worldのコンパイルに16秒とか掛かってました。今のmirahc.jarはかなり小さく、1MBちょっとしかありません。

入手法はgithubから良いかんじにダウンロード出来るんじゃないですかね？（適当）

古いバージョンにはMirahのコードと等価なJavaのソースを吐くオプションがありましたがコンパイラが変わってなくなりました。かつてheadiusはこの機能を使って吐いたコードをJRubyにコミットしたことがあるそうです。今新しいコンパイラが安定してきたのでそろそろ再実装されそうです。

exampleですが一応私のコミットも入ってます。`grep keen NOTICE`ってやってみて下さい。

mirah-mode.elをちまちま書いてますが道程は通そうです。ブロックコメント(`/* .. */`)がネスト可能なのですが、正規表現だと`/*/*`を`/*`2つと`*/`1つと認識しちゃってつらいです。

今回細かいところは省きましたがexampleとかあと公式ページとかgithubのwikiとか見て下さいね。

ある程度の完成度になってきてるのでみなさんも遊んでみて下さい。
[^1]: pull-reqは入ってるのですが、まだマージされてません

[^2]: まだリテラルの議論が終わってないだけで、内部はジェネリクスに対応してるので近い内に入りそうな気はします。
