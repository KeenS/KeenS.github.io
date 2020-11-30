---
categories: [SML, compiler, 言語実装, 言語実装 Advent Calendar, Advent Calendar, Advent Calendar 2020]
date: 2020-11-30T01:47:29+09:00
title: "自作コンパイラをブラウザ上で動かす"
---
このエントリは[言語実装 Advent Calendar 2020](https://qiita.com/advent-calendar/2020/lang_dev)の1日目の記事です。
次はsisshiki1969さんで「RustでつくるRuby、その後の進捗」です。

κeenです。
昔からちまちま作ってるSML処理系をブラウザ上で動かすことができたのでその進捗報告です。

<!--more-->

ちまちま作ってるSML処理系とはこれのことです。

[KeenS/webml: A Standard ML Compiler for the Web](https://github.com/KeenS/webml)

今回実装したオンラインコンパイラは以下で試すことができます：

[WebML Online Compiler](https://keens.github.io/webml/online-compiler.html)

まあ、ブラウザで動くのでここに貼ることもできるんですけどね。

<div>
    <form id="form">
        <textarea id="program" cols="25" rows="10">
fun fib 0 = 1
|   fib 1 = 1
|   fib n = fib (n - 1) + fib (n - 2)
val _ = print (fib 10)
</textarea>
        <button id="button" type="button" disabled>Loading the compiler</button>
    </form>
    <div>
        <div id="error" style="display:none;">
            <p>Error:</p>
            <p id="error-message", class="message-box error"></p>
        </div>
        <div id="output" style="display:none;">
            <p>Output:</p>
            <p id="output-message" class="message-box correct"></p>
        </div>
    </div>
</div>

こういうのどうやって作ったの？っていうのを話していけたらなと思います。

# 設計

そもそもの話、WebMLはブラウザで動かすために設計、実装されています。
スタートから違うじゃんと思うかもしれませんが、スタートから違うとして、どう違えばいいのかも必要な情報でしょう。
では、ブラウザで動かすための設計とはというと、以下のことに気をつけて書かれています。

1. 実装言語にブラウザ上で動く仕組みをもっているものを選ぶ
2. コンパイルターゲットにブラウザ上で動く仕組みをもっているものを選ぶ
3. コンパイル中にファイルや外部プロセスに依存しない
4. コンパイル結果やランタイムがWebでも動くように配慮する

2以外は[emscripten](https://emscripten.org)でゴリ押しするという手もあるのですが、色々な理由で私は選択肢から外しました。

emscriptenを選択肢から外すとWeb上で言語を動かす仕組みはJavaScriptか[WebAssembly](https://webassembly.org)（WASM）になります。
私はWASMに全振りする選択をとりました。というかWASMで遊ぶためにこのプロジェクトをはじめました。

というのを踏まえて上記の条件をWebMLにあてはめてみましょう。

## 実装言語

Rustで書きました。
RustはWASMへのコンパイルターゲット（`wasm32-unkonwn-unknown`）をもっているので容易にブラウザ上で動かすことができます。

WebMLを始めた頃はまだ実験的機能扱いでしたが、今やRustがWASMを使うときの最有力候補になっています。

## コンパイルターゲット

WASMを選びました。
これは前述のように「WASMをターゲットにするコンパイラを作ればブラウザ上でコンパイルできるのでは」というアイディアに基いて始めたプロジェクトなので当たり前っちゃ当たり前ですね。

## コンパイルプロセス

全てメモリ内で完結するようにしました。
ここでネックになるのがアセンブラです。
WASMアセンブラは[binaryen](https://github.com/WebAssembly/binaryen)などの公式CLIツールが配られてますが、CLIツールなので外部プロセスを起動しなければなりません。さらに入力も出力もファイルです。
しかしながらブラウザ上で外部プロセスやファイルなどは扱えません。

仕方ないので全てメモリ内で完結するWASMアセンブラ、[WebAssembler-rs](https://github.com/KeenS/WebAssembler-rs)を作りました。WASMが正式勧告になる前から作ってたので途中でバイナリフォーマットが変わって泣きの目をみたなんて開発秘話もあります。

## Webで動くオブジェクトやランタイム

まあ、要するに `libc` に依存しないようにすればいいわけです。

ランタイムはいくつかのパーツに分かれます。GCと標準ライブラリ。
GCと標準ライブラリさえどうにかすれば吐いたバイナリはそれらだけに依存するように作れば話は済みます。
なのでGCと標準ライブラリをどうするかが問題です。

### GC

GCはRustで書いてWASMにコンパイルしています。
ただしlibcを使えないので `#![no_std]` にして、[intrinsic](https://doc.rust-lang.org/core/arch/wasm32/index.html)の[memory_grow](https://doc.rust-lang.org/core/arch/wasm32/fn.memory_grow.html)や[memory_size](https://doc.rust-lang.org/core/arch/wasm32/fn.memory_size.html)を使ってイチからメモリ管理しようとしています。…が実態は実装がおいついてなくてゴミを回収しない、いわゆるZeroGCになっています。

私は[過去にWASM生書きでGCを実装したことがある](/blog/2017/12/07/webassemblynogc/)のですが、アロケータAPIとかの兼ね合いで流用はできないので再実装することになる見込です。

### 標準ライブラリ

標準ライブラリはWASMで表現できるもの（`+` など）はコンパイラの組み込みにして、それ以外はFFIとして実装しています。
FFIはWASMの `import` の仕組みを利用して実装されています。
`print` を例に採ると `prelude.sml` で以下のように定義されています。

```sml
fun print x = _externcall("js-ffi"."print": (int) -> unit)(x)
```

そしてブラウザ上で動かすときに以下のようにして `print` の実装を与えています。

``` javascript
let buffer = compile_string(str).buffer;
let importObj = {
    "js-ffi": {
        print: showOutput
    },
    // ...
};
WebAssembly.instantiate(buffer, importObj)
```

SML側で型さえ与えられれば実行時に自由にJS側で実装を与えられるので `print` も簡単に実装できてしまえます。
ついでにいうと `print` をどう実装するかに裁量が残るので今回のようにページ内に結果を出力するなんてこともできます。

# オンラインコンパイラの実装のためにしたこと

上記のようなコンパイラの設計をしたとして、オンラインコンパイラの実装のためにやったことを書いておきます。

1. コンパイラをWASMにコンパイル
2. ブラウザでリンクする仕組みの開発
3. WASMをサーバやブラウザ内から取得して実行

## コンパイラをWASMにコンパイル

ちょっとWASMがごちゃごちゃするのでIとTの記法を使って状況を説明します。


以下のような図を書いたときに

``` text
+-----+----------+--------+
| src | compiler | target |
+-----+----------+--------+
      | runtime  |
      +----------+
```

runtime上で動くcompilerを使ってsrcからtargetに変換するということを表わします。

また以下のような図を書いたときに

``` text
+--------+
|  code  |
+--------+
| interp |
+--------+
```

interpを使ってcodeを解釈するということを表わします。


これらの記法を使うと、以前は以下のような状況でした。

``` text

+------------+-------+---------------+
| webml(src) | rustc | webml(x86_64) |
+------------+-------+---------------+
             |  OS   |
             +-------+

+---------------+-------+----------------+
| webml-rt(src) | rustc | webml-rt(wasm) |
+---------------+-------+----------------+
                |  OS   |
                +-------+

+-----+-------+------+
| SML | webml | WASM |
+-----+-------+------+
      |  OS   |
      +-------+


+-----------------+
| WASM + webml-rt |
+-----------------+
|     browser     |
+-----------------+
|       OS        |
+-----------------+

```

手元のPC上で動くWebMLコンパイラを使って事前にSMLのコードをコンパイルしておき、それをブラウザにロードしていました。


今回のオンラインコンパイラの実装でコンパイラもブラウザ上で動くようになりました。

``` text

+------------+-------+-------------+
| webml(src) | rustc | webml(wasm) | <-- ターゲットがwasmになった
+------------+-------+-------------+
             |  OS   |
             +-------+

+---------------+-------+----------------+
| webml-rt(src) | rustc | webml-rt(wasm) |
+---------------+-------+----------------+
                |  OS   |
                +-------+

+-----+---------+------+
| SML |  webml  | WASM |
+-----+---------+------+
      | borwser | <---------------------- ブラウザで動くようになった
      +---------+
      |   OS    |
      +---------+

+-----------------+
| WASM + webml-rt |
+-----------------+
|     browser     |
+-----------------+
|       OS        |
+-----------------+

```

## ブラウザでリンクする仕組みの開発

上を見てもらったら分かるように、WASMとランタイムはそれぞれ別でコンパイルされ、ブラウザ上でリンクしています。
ブラウザ上でWASMファイルをパースしてリンクした新しいWASMファイルを生成するのは骨ですが、幸いにもブラウザ上でWASMモジュールを上手く組み合わせる仕組みがあります。

WASMはブラウザ上ではモジュールという単位で管理されます。
上記例でいうとwebml、webml-rt、SMLのコンパイル結果などがそうですね。
モジュールにはexportsといってJavaScriptから関数のように見えるAPIが生えています。
一方でモジュールを作るときにはimportsといってJavaScriptから関数や値などを渡せます。
これらを使ってexportsとimportsをJavaScriptで繋げてあげることでリンクを実現しています。

面白いことに、WASMのモジュールのexportsにはメモリ（メモリ全体）もあります。
importsで他のモジュールのメモリを読み込むこともできます。
現時点ではメモリ空間は1つしかないのでモジュールは自分でメモリを作ってexportするかメモリを作らずにimportするかしかありません。
この仕組みを上手く使ってるのがGCです。
GCのモジュールはメモリと `alloc` 関数をexportしていて、SMLのコードの方でそれらをインポートして使っています。

冒頭の方で `print` の例として載せたコードの省略した部分をちゃんと載せるとこうなっています。


``` javascript
let buffer = compile_string(str).buffer;
let importObj = {
    "js-ffi": {
        print: showOutput
    },
    "webml-rt": {
        alloc: rtObj.alloc,
        init: rtObj.init,
        memory: rtObj.memory
    },
};
WebAssembly.instantiate(buffer, importObj)
```

`importObj` にランタイム（`rtObj`） のメモリ（`memory`）や `alloc` 関数を渡しています。

## WASMを取得して実行

WASMを取得して実行するにあたっていくつか解決しないといけない問題がありました

1. 謎のセキュリティ機構でロードできない問題
2. WASMのプリミティブ型以外は受け渡せない問題
3. WASMのコンパイル/実行

1は未だもって不明です。JavaScriptの文法で `import "wasm=_ile.wasm"` と書けば自然とロードしてくれるはずだったのですが、MIMEタイプが許可されてないとかでロードエラーが起きました。
どうやらサーバの設定の問題（開発に使ったのはRubyのWEBrickです）のようなのですが、それっぽい設定をしてもダメでした。

2は問題としては分かりやすいです。WASMには先程説明したとおり、 `i32` 、 `i64` 、 `f32` 、 `f64` の型しかありません。
独自定義の構造体はおろか、文字列なども存在しません。もちろん、imports/exportsでもこれらの型しか扱えません。
これでは今回のように文字列を渡してコンパイルしてもらってバイト列を返してもらうような関数を定義できません。
これには一応解決策があります。メモリはJavaScriptから読めるのでポインタだけ関数の引数でやりとりして、実データメモリ経由でやりとりする方法で解決できます。
とはいえ、これを1つ1つやるのは人間のやることではないので自動でやってくれる仕組みを導入します。

3はまあ分かりやすいです。JS APIの `WebAssembly` を実行するだけです。
ただし（ロードやコンパイルに時間がかかることを想定して）APIが非同期に設計されているので少しだけ面倒です。

3は言ってしまえばやるだけなので1、2の解決策だけ紹介します。とはいってもツールを使うだけですが。

### wasm-bindgen/wasm-pack

[wasm-bindgen](https://github.com/rustwasm/wasm-bindgen)がRustとWASMの橋渡しをするためのRustライブライで、[wasm-pack](https://github.com/rustwasm/wasm-pack)がRustのコードをブラウザで扱いやすいようにビルドしてくれるビルドツールです。
おおむねwasm-bindgenが2を、wasm-packが1、2を解決してくれます。

#### wasm-bindgen

使い方はシンプルです。
WASMビルドの際にJS側に晒したいAPIに `#[wasm_bindgen]` を付与するだけです。
今回の私のコンパイラではこのようなコードを書きました。

``` rust
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;


#[cfg(target_arch = "wasm32")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn compile_string(input: String) -> Result<Vec<u8>, JsValue> {
    let mut prelude = include_str!("../ml_src/prelude.sml").to_string();
    prelude.push_str(&input);

    let config = Config::default();
    compile_str(&prelude, &config).map_err(|e| format!("Compile failed: {}", e).into())
}

```

関数の返り値に `Result` を指定したかったら `Result<Vec<u8>, JsValue>` のように `Err` 側に必ず `JsValue` を指定しないといけないだとか、いくつかのデータ型はJSに渡せないなどの制約はありますがおおむね素直に動いてくれます。

#### wasm-pack

こっちも使い方はシンプルです。Cargoの代わりに `wasm-pack` を使ってビルドします。

``` text
$ wasm-pack build --target web
```

wasm-packにはいくつかビルドモードがありますが、私はnpmツール群を避けて通りたいのでピュアなWeb向けの成果物を出してくれる `web` を選択しています。デフォルトでは `bundler` になっています。

ビルドが終わると `pkg/` 以下にいくつかのファイルが生成されます。

``` text
$ ls pkg
README.md  package.json  webml.d.ts  webml.js  webml_bg.wasm  webml_bg.wasm.d.ts
```

このうちブラウザ上では `webml.js` のみをロードすればよいことになっています。
`*.wasm` が本体、`*.ts` は（TypeScript向けの）補助ファイル、 `*.js` がWASM <-> JSのグルーコードです。

グルーコードについて言及しましょう。
例えば今回の `compile_string` は `webml.js` で以下のように定義されています。
`wasm.compile_string` をラップする形で文字列やバイト列の操作を請け負ってるのが見てとれるかと思います。

``` javascript
/**
* @param {string} input
* @returns {Uint8Array}
*/
export function compile_string(input) {
    try {
        const retptr = wasm.__wbindgen_export_0.value - 16;
        wasm.__wbindgen_export_0.value = retptr;
        var ptr0 = passStringToWasm0(input, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        var len0 = WASM_VECTOR_LEN;
        wasm.compile_string(retptr, ptr0, len0);
        var r0 = getInt32Memory0()[retptr / 4 + 0];
        var r1 = getInt32Memory0()[retptr / 4 + 1];
        var v1 = getArrayU8FromWasm0(r0, r1).slice();
        wasm.__wbindgen_free(r0, r1 * 1);
        return v1;
    } finally {
        wasm.__wbindgen_export_0.value += 16;
    }
}
```

ところでこのグルーコドはロード問題なども請け負ってくれます。
コードを読んだところ `import` でWASMファイルとして読み込むとエラーになりますが、 `fetch` でバイナリ列として取り出して `WebAssembly` APIでコンパイルするとエラーにならなようです。

# まとめ

ブラウザでコンパイラを動かすためのコンパイラの設計や、その設計をした上でどのようなことをすれば動くのかを紹介しました。

設計としてはコンパイラやコンパイル結果がWeb上で動くように配慮する必要があります。
実装としてはRustを使って開発した上でwasm-bindgen/wasm-packを使ってコンパイルし、ブラウザ上でランタイムとコンパイル結果をリンクする形になります。

今回実装したオンラインコンパイラはtextareaの内容をそのままコンパイルしただけでした。
将来[ace](https://ace.c9.io)などのWebエディタと統合したり1行づつ評価するREPLを作ったりをやっていきたいですね。


# 付録
## A: WebAssemblyとは

ここまで話題に出てきたWebAssemblyとは何かという話を軽くします。

WebAssemblyはブラウザ上で（も）動く言語です。
[2019年12月にW3C正式勧告](https://www.w3.org/TR/wasm-core-1/)となり主要ブラウザで実装されています。
フォーマットはバイナリで、スタックマシンです。
機械語を直接ブラウザで動かせる仕組みと勘違いしてる人がたまにいますが、名前に反してアセンブリではありません。
中間言語、JVMでいうバイトコードのようなポジションですね。

以下のような見た目をしています。

``` webassembly
(module
  (func $i (import "imports" "imported_func") (param i32))
  (func $new_page
    (grow_memory (i32.const 3))
    (call $i))
  (func (export "exported_func")
    (i32.const 42)
    (call $new_page)
    (call $i))
  (memory 1 10))
```

モジュールや関数などがあり、機械語とは程遠いですね。

## WebAssemblyの生い立ち

1. CをJavaScriptにコンパイルするempscriptenが登場する
2. どうせ機械生成ならと、特殊記法で高速に実行できるasm.jsが登場する
  + JavaScriptのサブセットなのでasm.jsをサポートしてないブラウザでも動く
3. asm.jsは実行は高速だがロードが遅い問題があった
4. JavaScriptの文法を捨ててバイナリにしたらコンパクトになってダウンロードもパースも速くなるんじゃね？
5. →WebAssemblyの誕生

その生い立ちから、まずはC/C++ to WASMのコンパイラを満足に書ける程度の仕様が策定されました。
つまり、最小限のWASMはJavaScriptに表現力で勝ることはないです。
じゃあ何故WASMを選ぶのかというと1つはバイナリが動いて面白いから。もう1つが将来への期待です。

最小限のWASMはJavaScriptに及ばないのですが、その後の方向性としてJSにはないSIMDやスレッドなどのAPIが足される予定です。

[Roadmap - WebAssembly](https://webassembly.org/roadmap/)

そういった意味で将来が期待されるプロジェクトです。

## WebAssemblyの使いどころ

基本的にはブラウザで速度が欲しいときに使います。
例えばゲームエンジンのUnityは[WebAssemblyへの出力が可能](https://blogs.unity3d.com/2018/08/15/webassembly-is-here/)です

あとはブラウザで動く関係上サンドボックス機構がついてるのでそれを利用するケースがあります。
[Kebernetes上でDockerコンテナの代わりにWASMを動かす](https://cloudblogs.microsoft.com/opensource/2020/04/07/announcing-krustlet-kubernetes-rust-kubelet-webassembly-wasm/)だとか。
名前に反してブラウザで動くとは限らないんですね。

## コンパイルターゲットとしてのWebAssembly

ブラウザで動かすにしてもJavaScriptやemscripten経由じゃだめなの？という疑問に答えておこうと思います。

1つはセマンティクスの問題。
WebAssemblyには数値型が `i32` 、 `i64` 、 `f32` 、 `f64` と4つあります。
一方でJavaScriptの数値型は`number` と `BigInt` のみです。
Cのような32bit整数型がJavaScriptにないんですね。
なのでCの `1 + 1` を素直に `1 + 1` にコンパイルできないのです。
これはJavaScriptに限らず高級言語から高級言語にコンパイルすると常にこういったセマンティクスの違いの問題がつきまといます。
なので比較的低級で意味論も素直なWebAssemblyにコンパイルするのは理に適っています。

1つは利用ケース。
上で紹介したように、WebAssemblyはブラウザ以外でも利用例があります。
なんなら、ローカルマシンで実行できるバイナリを吐く[コンパイラ](https://github.com/bytecodealliance/lucet)もあります。「ポータブルなバイナリフォーマット」としてのWebAssemblyは価値があります。

1つはWebAssemblyでしかできないことを狙って。
先程言及したように、将来WebAssemblyでしかできない機能が導入される予定です。
そういったものを見越してWebAssemblyを使ってみるのも面白いでしょう。

### emscripten問題

直接JavaScriptにコンパイルするのはつらくても、細かなところまで面倒をみてくれるemscriptenならどうなのという疑問にも答えておきます。
数年前にちょろっと調べたときの記憶で書いてるので色々間違ってるかもしれませんが、当時私がemscriptenでなくWebAssemblyを選んだ理由を書いておきます。

#### ホスト環境の再現はほしくない

emscriptenは既存のコードをそのままWebブラウザで動かせるように色々気を使ってます。
特にlibcもブラウザで動くように再実装しています（ファイルシステムとかも実装されてます）。

ですが私がやりたいのはWeb向けに設計された言語の設計と実装です。
`print` の例のように裏はそのままJavaScriptと繋がっていてほしいのです。
そういった点で、emscriptenの方向性は私の目指すものと違いました。

#### バイナリサイズが大きい

上記のlibcを移植してる話に繋がるのですが、バイナリサイズ[^1]がとても大きいです。
Hello Worldをするのにしばらく待たされるくらい重いです。
一方でWebMLのランタイムとHello Worldをコンパイルした程度のコードなら両者合わせても1KiBにも満たないサイズなのでロードは一瞬です。

[^1]: 厳密にはJavaScriptなのでバイナリではなくテキスト

ゲームのようにそもそも重いものを動かすなら我慢できるかもしれませんが、私のようにちょろっと動くプログラミング言語を作るのには向いてなさそうでした。

#### カスタマイズ性が分からなかった

チュートリアルをやって少しだけ調べた状態での印象です。
`puts("Hello, World");` するとアラートで画面にメッセージが表示されたりと期待しない挙動をよくしていました。
また、特定のAPIを叩くと勝手にロード、実行される仕組みになってるのでGCのようにライブラリとして使いたいものの動かし方が分かりませんでした。

ちゃんと調べれば出てくるのでしょうが、前述のいくつかの理由もあいまってそこまで調べずにあきらめてしまいました。

現時点ではWebAssembly周りのエコシステムが発達してきているのでemscriptenは使わなくてよかったのかな、と振り返ってます。



## B: オンラインコンパイラのコード

``` html
<div>
    <form id="form">
        <textarea id="program" cols="25" rows="10">
fun fib 0 = 1
|   fib 1 = 1
|   fib n = fib (n - 1) + fib (n - 2)
val _ = print (fib 10)
</textarea>
        <button id="button" type="button" disabled>Loading the compiler</button>
    </form>
    <div>
        <div id="error" style="display:none;">
            <p>Error:</p>
            <p id="error-message", class="message-box error"></p>
        </div>
        <div id="output" style="display:none;">
            <p>Output:</p>
            <p id="output-message" class="message-box correct"></p>
        </div>
    </div>
</div>

<style>
 .message-box {
     padding: 5px;
 }
 .correct {
     background: #eeeebb;
 }
 .error {
     background: #eebbbb;
 }
</style>
<script type="module">
 import { default as compilerInit, compile_string } from 'https://KeenS.github.io/webml/compiler/webml.js';
 import { default as rtInit } from 'https://KeenS.github.io/webml/compiler/webml_rt.js';

 let input = document.getElementById("program");
 let output = document.getElementById("output");
 let outputMessage = document.getElementById("output-message");
 let error = document.getElementById("error");
 let errorMessage = document.getElementById("error-message");
 let button = document.getElementById("button");

 function showOutput(str) {
     outputMessage.innerHTML = str;
     output.style.display = "inline";
     errorMessage.innerHTML = "";
     error.style.display = "none";
 }

 function showError(str) {
     outputMessage.innerHTML = "";
     output.style.display = "none";
     errorMessage.innerHTML = str;
     error.style.display = "inline";
 }

 async function init() {
     let [compilerObj, rtObj] = await Promise.all([
         compilerInit(),
         rtInit()
     ]);
     function compileAndRun(str, output) {
         try {
             let buffer = compile_string(str).buffer;
             let importObj = {
                 "js-ffi": {
                     print: showOutput
                 },
                 "webml-rt": {
                     alloc: rtObj.alloc,
                     init: rtObj.init,
                     memory: rtObj.memory
                 },
             };
             WebAssembly.instantiate(buffer, importObj)
                        .catch(err => showError("Browser returned an error: " + err))
         } catch (e) {
             showError(e)
         }
     }
     return compileAndRun;
 }
 init().then(compileAndRun => {
     button.removeAttribute("disabled");
     button.onclick = arg => compileAndRun(input.value, output);
     button.innerHTML = "Run";
 });
</script>
```


<section>
<style>
 .message-box {
     padding: 5px;
 }
 .correct {
     background: #eeeebb;
 }
 .error {
     background: #eebbbb;
 }
</style>
<script type="module">
 import { default as compilerInit, compile_string } from 'https://KeenS.github.io/webml/compiler/webml.js';
 import { default as rtInit } from 'https://KeenS.github.io/webml/compiler/webml_rt.js';

 let input = document.getElementById("program");
 let output = document.getElementById("output");
 let outputMessage = document.getElementById("output-message");
 let error = document.getElementById("error");
 let errorMessage = document.getElementById("error-message");
 let button = document.getElementById("button");

 function showOutput(str) {
     outputMessage.innerHTML = str;
     output.style.display = "inline";
     errorMessage.innerHTML = "";
     error.style.display = "none";
 }

 function showError(str) {
     outputMessage.innerHTML = "";
     output.style.display = "none";
     errorMessage.innerHTML = str;
     error.style.display = "inline";
 }

 async function init() {
     let [compilerObj, rtObj] = await Promise.all([
         compilerInit(),
         rtInit()
     ]);
     function compileAndRun(str, output) {
         try {
             let buffer = compile_string(str).buffer;
             let importObj = {
                 "js-ffi": {
                     print: showOutput
                 },
                 "webml-rt": {
                     alloc: rtObj.alloc,
                     init: rtObj.init,
                     memory: rtObj.memory
                 },
             };
             WebAssembly.instantiate(buffer, importObj)
                        .catch(err => showError("Browser returned an error: " + err))
         } catch (e) {
             showError(e)
         }
     }
     return compileAndRun;
 }
 init().then(compileAndRun => {
     button.removeAttribute("disabled");
     button.onclick = arg => compileAndRun(input.value, output);
     button.innerHTML = "Run";
 });
</script>
</section>
