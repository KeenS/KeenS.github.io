---
categories: [DB, RockDB, Rust]
date: 2018-02-08T04:22:30+09:00
title: "改めましてRocksDBさん"
---

κeenです。寝れない。[前回のエントリ](https://keens.github.io/blog/2018/02/05/hello__rocksdb/)でRocksDBのことを勘違いしていたようだったので改めてRocksDBについて書きます。

個人的理解をメモした雑多な記述です。

<!--more-->

# LSM-Tree
まず、私は勝手にRocksDBはロックフリーな[スキップリスト](https://ja.wikipedia.org/wiki/%E3%82%B9%E3%82%AD%E3%83%83%E3%83%97%E3%83%AA%E3%82%B9%E3%83%88)を使って並列性能をあげたDBだと思ってたんですが間違ってました。
使っているのは[Log-Structured_Merge-Tree](https://en.wikipedia.org/wiki/Log-structured_merge-tree)のようです。
[Log Structured File System](https://en.wikipedia.org/wiki/Log-structured_file_system)に名前が似てるな〜思ったらやはりそこ由来のデータ構造のようです。
ファイルシステムも一貫性が必要なKVSですからまあ、参考になりますよね。
LSM-Treeについては[元論文](https://www.cs.umb.edu/~poneil/lsmtree.pdf)の他[このスライド](https://www.slideshare.net/ssuser7e134a/log-structured-merge-tree)が多少参考になります。

重要なのは

* 削除や更新は論理操作。ストレージのデータは基本はイミュータブル
* メモリのデータとストレージのデータ2つ合わせて完全
* 読み取りはメモリとストレージなど複数箇所から読んで複数値をマージする
* ストレージのデータは時々コンパクションされる

この辺がRocksDBのAPIに反映されています。

# RocksDB

LSM-Treeベースの実装が色々ある中RocksDBはかなり実装上の工夫を加えたらしいです。
[ここ](https://github.com/facebook/rocksdb/wiki/Features-Not-in-LevelDB)にすごい量載っています。これはLevelDBとの差分だけなのでLevelDBでの工夫も加えたらすごいことになるんですかね。
RangeやPrefixクエリにbloom filterが使えたりトランザクションをサポートしていたりRDBのバックエンドとして使うには嬉しそうな機能が一杯ですね。

# 勘違いとか

前回の記事の勘違いを指摘していきます。

> RocksDBの[`Delete`](https://github.com/facebook/rocksdb/blob/master/db/db_impl.h#L86)自体は `Status` を返すので判別可能

これはDelete操作がエラーになったかどうかしかわかりません。多分論理削除なので元の値が存在したかどうかは高速には判断できないんでしょう。putが存在したかどうか判定できないのも恐らく同じ理由。

> [`keyMayExist`](https://github.com/facebook/rocksdb/blob/master/db/db_impl.h#L135)のラッパも存在しないので

KeyMayExistは`May`があるようにtrueを返したところで値が存在することは保証しません。falseだったら存在しないのは確実。恐らくbloom filterしか見ないんでしょう。因みに実装見に行ったら常にtrue返すように見えたんですが気のせいですかね？だとしても仕様には違反しませんが。

> * どうやら非同期クエリもできるらしい？だったらTokioと相性よさそう

これは `WriteOptions` の `sync` を見て言ったのですがこの `sync` の意味はOSのバッファリングもストレージに同期する、つまりシステムコールの `sync` の意味でした。

# その他APIについて

APIでなんかよくわからない概念があったりこれなんだろみたいな関数があったりしたのがよく調べたらわかったのでメモ

* ColumnFamilyはただの名前空間
* SnapShotが気軽に取れるのに納得。
* SingleDeleteは追記されていない場合にのみ使える。論理削除じゃなくて物理削除？
* megrgeは恐らくreadの副産物
