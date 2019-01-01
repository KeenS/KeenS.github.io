---
categories: ["Rust"]
date: 2019-01-01T20:50:13+09:00
title: "My wishilist for Rust 2019"
---
The rest of this article is written in Japanese. For those who don't understand Japanese, auto translation tools may help you.
I'm writing my blog in Japanese because I put importance on Japanese community and the situation "information is available in mother tongue".

κeenです。 #rust2019 の投稿として自分の考えをまとめます。自動翻訳で翻訳しやすいように1文1文を短めに書きます。

<!--more-->


2018年は大きな変更が多かった。言語がかなり進化した。特にRust 2018 Editionが出た。
ライブラリも大きく変わった。Tokio 0.1が出たし、Hyper 0.12も出た。
これらの大きな変更にコミュニティが追いついていない。コミュニティに時間が必要だと思う。
アプリケーションに必要なのはフレームワークやそのエコシステムなので今出来上がりつつあるコアライブラリからさらにステップが必要だ。

2019年のスローガンは "Ecosystem" で

* 既存のプロジェクトを完遂する (async/await, chalkify)
* 言語は大きく変えない
  + 代わりに未実装のRFCを実装する
* ユーザにフレームワークや周辺ライブラリを書きやすくする
  + 基盤となるライブラリを安定化させる
* ユーザに新機能を定着させる
  + "This library does not work with the new feature..." をなくす

が必要だと思う。もちろん、コンパイラが速くなると嬉しいし、実行も速くなると嬉しい。

# その他の問題

私が最近気づいた細々した問題を挙げる。

## リンク時間

リンクが遅い。テストファイルが多いときに顕著になる。
以下のようにテストファイルが多いとリンクにすごく時間がかかる。

```
Cargo.toml
tests/
  test1.rs
  test2.rs
  test3.rs
```

テストの数だけリンクが走るのでリンク時間が問題になる。さらに並列にビルドされるのでメモリが溢れる。

## ツールとマクロの相性が悪い

マクロを使うとエラーメッセージが分かりづらい。
例えば以下ようなマクロを書いたとする。

```rust
macro_rules! my_struct {
    (struct $name:ident {
        $($field: ident : $ty: ty),*
    }) => {
        struct $name {
            $($field: $ty),*
        }
    }
}
```



これを以下のように使うとエラーになる。

``` rust
my_struct! {
    struct MyStruct {
        hoge: i32,
        fuga: i64,
    }
}
```

メッセージはこうだ

```text
error: no rules expected the token `}`
  --> macro_struct.rs:15:5
   |
15 |     }
   |     ^

error: aborting due to previous error

error: Could not compile `macro_struct`.
```

何が悪いのか分かりづらい。

もう1つの問題は、このマクロが正しい `struct` 定義の構文を受け取らない点にある。
マクロ毎に「`struct` 定義っぽい構文」があることになる。

さらにrustfmtも効かなくなる。
以下のように書いてもrustfmtは何も変更しない。

``` rust
my_struct! {
    struct MyStruct {
      hoge: i32,
        fuga: i64,
    }
}

```

これらの問題の解決策として、高レベルなマクロパターンがあると良さそうに思う。
例えば `struct_def` パターン。

``` rust
macro_rules! my_struct {
    ($struct: struct_def) => {
        $struct;
        // some items (like name, fieles, arrtibutes, etc) is accessible via `#` notation
        impl $struct#name {
            // ..
        }
    }
}
```

## 手続きマクロが扱いづらい

手続きマクロのためにCargoバッケージを1つ作るのは面倒だ。
1バッケージ内でライブラリ/バイナリと手続きマクロを同居させたい。
エントリーポイントに `macro.rs` を増やすのはどうだろう。

``` rust
src/
  macro.rs  // entry point for proc_macro crate
  lib.rs    // entry point for lib crate
  bin.rs    // entry point for bin crate
```

