---
categories: [git, pijul]
date: 2016-02-14T17:12:01+09:00
title: 分散VCSのモデル、あるいはPijulについて
---

先日、[Pijul](pijul.org)という分散VCSについて知って、それについて調べてみたら少し面白かったのでメモ。
<!--more-->
DVCSで一番有名なのは間違いなくGitだろう。あれは分散グラフ理論木モデルに基いているらしい。ベースになったモデルがあることに驚いたが、調べても出てこなかった。
Gitは高速で信頼性が高い一方、コミット同士をチェーンのように繋げてしまうので柔軟性を欠き、例えばCherry Pickなんかがやりづらい。
あるいはリモートのmasterを取り込まずにローカルのmasterにコミットすると互いに独立した変更であっても一旦remote masterをマージしないとプッシュ出来ず、コミットグラフが汚れてしまう。
また、CUIが直感的でなく、理解しづらいという声もある。それはこういう皮肉にも現れている

![gitの皮肉](http://imgs.xkcd.com/comics/git.png)

まあ、言われてみれば私もこのスライドを見てようやく理解した。

<iframe src="//www.slideshare.net/slideshow/embed_code/key/l0beYVXFDsBY3P" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="//www.slideshare.net/ktateish/git-concept1" title="コンセプトから理解するGitコマンド" target="_blank">コンセプトから理解するGitコマンド</a> </strong> from <strong><a href="//www.slideshare.net/ktateish" target="_blank">ktateish</a></strong> </div>

他のVCSにも色々特色はあって、歴史は神聖なるmercurial、履歴にアクセスしてこそのVCSなFossil、レポジトリとはパッチの集合であるDarcsなどなど。

この中でもDarcsは大きく毛色が違うように思える。レポジトリは依存関係のあるパッチの集合で、互いに独立なパッチは相互作用しない。なので上に挙げたcherry pickだとかremote vs localの問題とかが起きない。
それにパッチベースの管理なのでメールでパッチを送り合うような古いシステムでも困らない。少なくともforkしてpull requestよりはパッチを送った方がなんぼか筋は良い気がする。他のVCSがsnapshot-basedなのに対してdarcsはpatch-basedになる。

さて、このDarcs、そこまで知らない人も多いかと思うが古くはGHCの開発に使われていたり(gitに移行した)、common-lisp.netで使われていたり(リニューアルの時に内部がgitlabになってdarcsサポートはドロップされた)した。
Haskellで書かれていたのとモデルが(理論的には)綺麗なので函数型な人達には人気があったようだ。


Darcsを使っていたプロジェクトがGitに移行したことから分かるように、Darcsにも問題があった(らしい)。1つはパフォーマンスの問題で、Haskellで書かれていたので流石にCで書かれたGitには勝てなかった。
さらには、勘の良い人は気付いたかもしれないが、パッチ同士の依存関係だけで管理してるとマージの時に最悪計算量が $O(2^n)$ になる(多くの場合は $O(n)$ で済むらしい)。

その他のDarcsの問題についてはこちらを参考されれば。

[GHC の開発の darcs から git への移行 - Togetterまとめ](http://togetter.com/li/120640)



# Pijul

今回話題に出したPijulはDarcsに影響を受けている。レポジトリとは依存関係を持ったパッチの集合で、パッチ同士の関係はGitに比べれば薄いが、snapshot-basedとpatch-basedの両方の良い所を取り入れている。
Pijulの特色は[A Categorical Theory of Patches](http://arxiv.org/abs/1311.3903)の成果を取り入れていて、Darcsにあった計算量の問題が改善している。
この理論はファイルを対象、パッチを射とした圏を考えると、互いにコンフリクトしないパッチは同時に適用出来てかつ適用の順序に依らないことは圏論に於ける「押し出し」に一致することに着目している。
さらにこのパッチ圏を拡張(余完備化)することでコンフリクトが起きないように出来る。

さて、Pijulにすると何が嬉しいのかというと、色々ある。
Darcsのようにパッチベースな点は言わずもがな、パッチベースなのでパッチを作った後でブランチを切ることが出来る。
実際に作業をしてみてブランチの名前が不適切だったな、とかブランチを切る前に名前を考えるのに一瞬手が止まったりすることがない。
尚、Pijulの現バージョン(0.2)では内部構造的にはブランチを扱えるものの、インターフェースのコマンドがないため事実上ブランチは作れないらしい。
また、データの持ち方的にgit blame相当の機能が速いなどそれなりの特色を出している。


開発言語もOCaml、Scala、Haskell、Cなどを試して今はRustがベストフィットだと落ち着いたらしい。 `cargo install pijul` でインストール出来る。
Rustで開発したお陰もあってDarcsより大分速いらしい。

Darcsユーザには受けがいいようで、darcs to pijulブリッジも作られようとしているらしい。

個人で使うには困らない程度にまで安定したら楽しみだ。
