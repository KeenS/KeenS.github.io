---
categories: ["番外編"]
date: 2018-12-31T16:24:18+09:00
title: "2018年注目していきたかった技術の振り返りと個人的振り返り"
---

κeenです。[2018年注目していきたい技術](https://keens.github.io/blog/2018/01/04/2018nenchuumokushiteikitaigijutsu/)とかをベースに1年を振り返ります。

<!--more-->
# 2018年注目していきたかった技術
## Swagger

思ったより進展がなかった。業務でいえばスキーマ定義から実装されたAPIの妥当性の検査とかも入ったしだんだん浸透してきている。
拙作の[chema](https://github.com/KeenS/chema)も少しづつ進化してる。


## diesel
2018年の大きなイベントとして[1.0がリリースされた](https://github.com/diesel-rs/diesel/releases/tag/v1.0.0)。
記述できるSQLの表現力も豊かになったしデータとのマッピングも柔軟になった。


## Gotham

思ったより奮わなかった。開発がそこまで活発ではなく、他のフレームワークに押され気味。[isucon7予選のアプリをRustに移植したから解説するね](https://keens.github.io/blog/2018/09/02/isucon7yosennoapuriworustniishokushitakarakaisetsusurune/)で試した時もイマイチな印象だったので今後どうなるんですかね。

## Coq

全然書いてない…。

## Idris

あんまり書いてない気がする。というか最近趣味でコードを書く機会が減った

## 線形論理
論文読んでない(完)

## ベクトル最適化
読書会[やってる](https://compiler-dev.connpass.com/)。
ドラゴンブックみたいな入門書とは違ってかなり実践的なことが書いてある。
依存関係をディオファントス方程式に落として解析したりとかもやってるが、ちゃんとその基礎の整数論とか線形代数とかもカバーしてるのでself-containedになってる。
ただし誤植が多すぎるので１人じゃ読めなそう。

## 量子コンピュータ

ひとまず[量子コンピュータと量子アルゴリズム II](https://shop.ohmsha.co.jp/shopdetail/000000002568/) を読んだ。
過去記事を参照 [その1](https://keens.github.io/blog/2018/05/01/ryoushikonpyu_taninyuumonshiteru/) [その2](https://keens.github.io/blog/2018/05/18/ryoushikonpyu_taninyuumonshiterusono2_ryoushifu_riehenkan/) [その3](https://keens.github.io/blog/2018/06/10/ryoushikonpyu_taninyuumonshiterusono3_ryoushitansakuarugorizumu/)


# 個人的振り返り
## メトリクス
ブログ記事33本、スライド10綴、mendeleyに突っ込んだ論文156綴、amazonで買った技術書12冊、GitHubのcontribution 2758。

論文と本は全て読んだわけじゃない(むしろ本は読んだ方が少ない)けどだいたいこんな感じ。


## 総括

仕事ではずっと作ってた[サービス](https://actcast.io)が世に出た。 CF [Actcast α版をリリースしました](https://blog.idein.jp/post/180982005915/alpharelease)。
それ以外では会社も(また)引っ越して大きくなったしactcastの開発メンバーもかなり増えた。
確か去年末時点から比べて社員数もactcastの開発メンバー数も倍増くらいしてるんじゃないかな。
環境が変化しつつ(=仕事が増える)いままでやってた業務も変わらずやるのは辛くて開発体制変えてもらったりした。
来年からはactcastをやるのは変わらないけど別のサブシステムを一から書くことになりそう。

プライベートではあまり活動できなかった。 一応メトリクスでは2017年を全部上回ってるので2018年の目標「もうちょっと進捗を出す」は達成した。
しかし2018年頭に終わらせる予定だったクローズドプロジェクトがまだ終わってないのでつらい。今年こそ年度末までには完遂したい。
あ、あと自作パソコン[買った](https://keens.github.io/blog/2018/05/13/tsukumodejisakumashinkatta/)。そろそろノートパソコンを軽量化したい(現在2.1 Kg)。
因みに論文が多いのは春頃にarXiveのRSSを購読してたせいなんだけど結局消化が間に合わなくて購読をやめた。

# 来年は

プライベートであまり活動できてないせいで「2019年注目していきたい技術」みたいなのが書けない。のでここで書いてしまう。
来年の抱負は言語処理系をもう少し書く。
作りかけでとまってる[SMLコンパイラ](https://github.com/KeenS/webml)や[Prologインタプリタ](https://bitbucket.org/blackenedgold/prolog-sml/src/default/)を進めたい。
というかPrologの方はWAMの論文ちゃんと読んでWAMベースの実装にしたい。今の所雰囲気で書いてる。
あとはなんだかんだLLVMをちゃんと使ったことがないのでそれ使ったプロジェクトも1つやりたい。シンプルにCコンパイラ(実はまだCコンパイラ書いたこと無い)かSchema(or ISLisp)のコンパイラかな？
地味にGitLabが便利なのとGitHub一極集中を避けるためにGitLabも使っていきたい。
