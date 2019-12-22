---
categories: [Rust, Diesel]
date: 2018-02-26T15:51:08+09:00
title: "DieselでSELECT NULLする"
---

κeenです。個人メモ。dieselでリテラルの`NULL`を`SELECT`したいケースでの書き方。

<!--more-->

```rust
table
.select(None::<String>.into_sql::<Nullable<Text>>())
```

結果だけみると当たり前っぽいけど `into_sql` とか `None` と `into_sql` 両方に型書かないとコンパイル通らないとか色々はまった。
