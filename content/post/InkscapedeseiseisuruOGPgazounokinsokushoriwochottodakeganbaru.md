---
categories: [Inkscape, NLP]
date: 2020-12-07T03:15:20+09:00
title: "Inkscapeで生成するOGP画像の禁則処理をちょっとだけ頑張る"
---

κeenです。
以前雑な実装でブログにOGP画像を生成するようにしましたが、禁則処理が気になったのでそれを工夫したときの記録です。

<!--more-->

# モチベーション

以前の作業はこれです。

[Inkscapeを使ってSVGからOGPイメージを半自動生成する | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2020/11/09/inkscapewotsukattesvgkaraogpime_jiwohanjidouseiseisuru/)

読んでない方に説明すると、SVGのERBテンプレートを作って記事データをあてはめたものからInkscapeでPNG画像を生成しているのでした。
その際に改行が自動で入らないので自分で改行をいれる必要があります。
今回はその改行を入れる位置を工夫したというお話。

文書に改行をいれるにあたって[禁則処理](https://ja.wikipedia.org/wiki/禁則処理)というのがあります。
恐らく小学校でも習ったと思いますが、行末に開き鉤括弧（「）を置かない、句点（。）読点（、）を行頭に置かない、などです。

もうちょっというと、今回の対象は文章ではなく中央揃えのタイトルなので禁則事項に触れないだけでなく「ちょうどいい」場所で改行してほしいです。

何故改行を入れる位置にこだわるかというと、このツイートがわかりやすいかもしれません。

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">ブログのOGPの生成をちょっと工夫した。それっぽいかたまりをトークンとしてまとめて、トークンの境目でしか改行しないようにした。<br><br>←before after→ <a href="https://t.co/kbRUwag7sp">pic.twitter.com/kbRUwag7sp</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/1335608625172103168?ref_src=twsrc%5Etfw">December 6, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

「使  
って」

と格好悪い見た目になってますね。これを防ぎたい訳です。他にも英単語の途中で改行されたくないだとか色々と条件があります。

本気出してやるなら自然言語処理して文節単位で区切って〜となるのですが、あんまり頑張りたくないので適当なヒューリスティックで近似します。


# 方針

最初に細かい実装上の都合なんですが、文字の正確な横幅の情報を取り出せるのがInkscapeに渡して画像としての準備ができてからになります。
そこからデータを取り出すのが少し手間ですし、遅いです。なので数回Inkscapeに横幅を問い合わせるだけでアルゴリズムを完了させたいです。

幸いにもInkscapeには1度のクエリで複数の結果を返す機能がついています。
なので細切れにした情報を送り込めれば一度に色々な情報を取り出せます。
ということで以下の方針で実装することにします。

1. タイトルを改行しない塊（トークン）に区切る
2. トークン単位でタグ付けしたタイトルのSVGを生成する
  + `<title><tsapn>トークン1</tspan><tspan>トークン2</tspan> ... </title` のような見た目
3. 生成したSVGをInkscapeに読み込み、タイトルの全体の長さとトークンの長さを取得する
4. タイトル全体の長さが画像の横幅より大きければトークンのどれかに改行文字を入れる

2、3は私の書いたOGP画像生成スクリプトの都合です。
4は先頭から1つ1つトークンを取得して横幅を足していって、規定の長さを越えたトークンの1つ前で改行すればいいだけなので難しいことはありません。

なので1について解説します。

# トークン分割処理

まず、対象データがどういうものかを確認します

* 漢字かなカナ英字記号交じり
* 漢字かなカナ英字は同じくらいの割合で交じっている
* 約物（。や！など）は少ないが、ないことはない

ここから、字種（漢字かなカナ英字）が変わるタイミングで区切ればそれっぽいのではないかと仮説を立てます。
ただしいくつか例外規定も設けます。

* 鉤括弧の行末禁止などは別途実装する
* 漢字からひらがなへの変化は区切りとしない
  * 「使ってる」を「使」と「ってる」に区切りたくない
* その他細則

この規則を実装していきましょう。

まず、字種を判別するメソッドから。


``` ruby
def char_class_of(c)
  case c
  when /[a-zA-Zα-ωΑ-Ω]/
    :alphabetic
  when /[0-9０−９]/
    :numeral
  when /\s/
    :spaces
  when /[「（(\["'※『{〔〘〈《【〖]/
    :yaku_start
  when /[。、…)）」!?？！\]ー』}〕〙》〉】〗]/
    :yaku_end
  when /[\p{Hiragana}]/
    :hiragana
  when /[\p{Katakana}]/
    :katakana
  when /[\p{Han}]/
    :kanji
  else
    :other
  end
end
```

私のブログのタイトルに使われそうなものしか考慮してないので雑ですね。

`:alphabetic` にギリシャ文字も含んでいるのは私のハンドルネームのκeenのκがギリシャ文字だからです。たまにはブログタイトルに自分の名前を使うこともあるだろうという判断です。

正規表現の `\p{Hogehoge}` というのはUnicode Propertyというやつです。

参考: [正規表現 \p{...} メモ - Qiita](https://qiita.com/dico_leque/items/08b6c5aa523216376b78)

探せば色々プロパティがあるのでしょうが、探すのが面倒だったのでパッとみつかった漢字かなカナにのみ使ってます。

次に2つの文字種を受け取って、その文字種の間に改行を入れるべきかを返す `split_point` メソッドです。

``` ruby
def japanese_classes
  [:hiragana, :katakana, :kanji]
end

def split_point?(before, after)
  # 行頭の場合
  if before == nil
    return false
  end

  if after == :yaku_start
    return true
  end

  if before == :yaku_start
    return false
  end

  if after == :yaku_end
    return false
  end

  if japanese_classes.map{|j| [:kanji, j]}.include?([before, after])
    return false
  end

  if [[:alphabetic, :hiragana]].include?([before, after])
    return false
  end

  before != after
end

```

いくつかの禁則処理や漢字+ひらがな、英字+ひらがなで例外が入ってますが、基本は `before != after` の文字種が違ったら分割するという処理です。

あとはこれらを使って `split_point?` がtrueだった場合に分割するメソッドを書くだけです。


``` ruby
def tokenize(str)
  result = []
  current = ""
  last_char_class = nil

  str.each_char do |c|
    char_class = char_class_of(c)
    if split_point?(last_char_class, char_class)
      result << current
      last_char_class = nil
      current = c
    else
      current += c
    end
    last_char_class = char_class
  end
  result << current if current != ""
  result
end

```



これはirbなどにロードして動作を確認できます。

``` text
irb(main):079:0> tokenize("Inkscapeを使ってSVGからOGPイメージを半自動生成する")
=> ["Inkscapeを", "使って", "SVGから", "OGP", "イメー", "ジ", "を", "半自動生成する"]
irb(main):080:0> tokenize("Inkscapeで生成するOGP画像の禁則処理をちょっとだけ頑張る")
=> ["Inkscapeで", "生成する", "OGP", "画像の", "禁則処理をちょっとだけ", "頑張る"]
```

まあ、悪くないんじゃないでしょうか。

一応既知の問題として長音符（ー）が約物になるのでカタカナ語の分割が下手というのはあります。

``` text
irb(main):082:0> tokenize("ロード時間を短くする")
=> ["ロー", "ド", "時間を", "短くする"]
```


あとは2, 3, 4を実装してあげれば完成です。

雑にそれっぽい成果が欲しい方は真似してみて下さい。
