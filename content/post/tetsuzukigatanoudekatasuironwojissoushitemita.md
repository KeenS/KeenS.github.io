---
categories: ["型", "型推論", "言語実装","Advent Calendar", "Advent Calendar 2019"]
date: 2019-12-08T16:58:42+09:00
title: "手続き型脳で型推論を実装してみた"
---

このエントリは[型 Advent Calendar 2019 - Qiita](https://qiita.com/advent-calendar/2019/type) 2日目に遡って投稿しているエントリです。
担当に遅刻した訳ではなくて空いてたので前から詰めて投稿しただけです。

κeenです。世の中に型推論アルゴリズムは色々知られていると思いますが、それを一切無視して型推論を実装してみたので報告します。

<!--more-->

# 型推論の基本的理解

式に型変数を割り当てて、既に分かっている型から制約条件をつけてその制約の連立方程式を解けばいいです。

例えば以下のようなSMLの式を考えましょう。

```sml
val printLn = fn s => (print (s ^ "\n"))
```

まずはそれぞれの式に変数を割り当てます。

``` text
printLn: 'a
s: 'b
print: 'c
op^ :'d
"\n": 'e
(s ^ "\n"): 'f
print (s ^ "\n"): 'g
```

使われ方から以下の等式が出ます。

``` text
'a = 'b -> 'g
(* 一時的な変数 'h, 'i を導入 *)
'c = 'h -> 'i
(* 一時的な変数 'j, 'k, 'l を導入 *)
'd = 'j * 'k -> 'l
'h = 'f
'i = 'g
'j = 'b
'k = 'e
'l = 'f
```

これから変数の値を全て確定させる （= 全ての変数の制約について、 `'x = type` という形に変形する）のが目標です。
ひとまずそれぞれの変数の制約状況を見てみましょう。


``` text
'a = 'b -> 'g
'b = ???
'c = 'h -> 'i
'd = 'j * 'k -> 'l
'e = ???
'f = ???
'g = ???
'h = 'f
'i = 'g
'j = 'b
'k = 'e
'l = 'f
```


ここで `print: string -> unit` 、 `op^: string * string -> string` 、 `"\n": string` という制約が事前に分かっていたとします。
この式を1つ1つあてはめていきます。

まずは `print: string -> unit` より `'c = string -> unit` 。
`'c = 'h -> 'i` でもあるので **構造的に比較** して、 `'h = string` 、 `'i = unit` も得られます。
さらに `'h = 'f` や `'i = 'g` などからいくつかの変数の値が **自動的に決まります**


``` text
'a = 'b -> unit
'b = ???
'c = string -> unit
'd = 'j * 'k -> string
'e = ???
'f = string
'g = unit
'h = string
'i = unit
'j = 'b
'k = 'e
'l = string
```


同様に `op^: string * string -> string` から `'j = string` 、 `'k = string` 、 `string = string` などが得られます。
最後の `string = string` は恒真なのでスルーします。ここで `string = unit` などが出ると、矛盾としてエラーを出します。

``` text
'a = string -> unit
'b = string
'c = string -> unit
'd = string * string -> string
'e = string
'f = string
'g = unit


'h = string
'i = unit
'j = string
'k = string
'l = string
```

これで既に変数が全部埋まりましたが、これが残りの制約と矛盾してないかの確認も必要なので推論を続けます。 `"\n": string` より `'e = string` 。
これは矛盾しないですね。

ということで型推論が終わりました。特に、 `printLn` には `string -> unit` 型が付くことが分かりました。

この手計算を **手続型脳で** プログラムにしていきたいと思います。

# ゆにふぁい！

プログラムに移る前に、先程の例で、ちょっと非自明なことを2つしてました。
そこを掘り下げておきましょう。

1つは構造的比較です。
`string -> unit = 'h -> 'i` という制約から `'h = string` 、 `'i = unit` という式を導き出しました。
この導出はユニフィケーション（unification、単一化とも）と呼ばれ、いくつかのルールにより動作しています。

1. `string = string` のように恒真式なら正常終了
2. `string = unit` のように矛盾していたら異常終了
3. `'x = ty` のように、変数と具体的な型のユニフィケーションなら `'x` に `string` を代入する
4. `'x = 'y` のように、変数同士のユニフィケーションなら将来どちらかに代入された場合に他方も同じ値になるように、結び付ける
5. `'a -> 'b = 'x -> 'y` のように同じ種類の合成型同士のユニフィケーションなら、構成要素のそれぞれでユニフィケーションする。
   この例なら `'a` と `'x` 、 `'b` と `'y` でユニフィケーションする。
6. それ以外 (`'a -> 'b = string` のように合成型と基本型、あるいは `'a -> 'b = 'a * 'b`のように違う合成型同士)の場合は異常終了

もう1つは、1つの制約式の追加でいくつかの変数の値が自動的に決まった点です。自動的というか推移的というか。
例えば `'a = 'b -> 'c` という制約があったところに、 `'a = string -> unit` という制約を加えると、`'b` や `'c` は直接は登場してないのに `'b = string` 、 `'c = unit` という制約が得られます。
数式としては至極自然な挙動なのですが、実装は1つ1つ辿っていくとすると大変そうです。
しかしまあ、なんかポインタで1箇所指してれば実現できそうな挙動ではあります。

# 方針

型同士をユニファイするコードと、制約を推移的に適用する部分を別々に実装しましょう。

型同士を単一化するやつは簡単なパターンマッチで書けそうです。推移的に適用する部分をもう少し掘り下げましょう。

## ユニフィケーションプール

制約を推移的に適用する部分はポインタにしとくと、どうにかなりそうという観測がありました。
もう少し物理的なイメージを膨らませましょう。


### 表現

まずは型は全てポインタとします。ポイント先は型の実体の配列です。
例えば `string` 型はこうです。

``` text
string
  |
  v
+---+---
| * | ...
+-|-+---
  |
  S
```

ここで `S` は `string` 型を表わす内部表現とします。


`string -> unit` 型はこうです。

``` text
        string -> unit
               |
     unit      |
string |       |
  |   ++  +----+
  v   v   v
+---+---+---+
| * | * | * |
+-|-+-|-+-|-+
  |   |   +----+
  S   U        |
         F(string, unit)
```

ここで `U` は `unit` 型を表わす内部表現、 `F` は関数型を表わす内部表現とします。
`F` の引数が `S` や `U` ではなく `string` や `unit` になっていることに注意して下さい。
`F` はポインタを引数にとっているので、ポイントの中身が変化したら自動で追従してくれます。

### ゆにふぁい！（１）

変数と `string` をユニファイしてみましょう。ゆにふぁい！

まずは `'a` と `string` があります。変数 `'x` の内部表現は `V("x")` とします。

``` text
 'a  string
  |   |
  v   v
+---+---+
| * | * |
+-|-+-|-+
  |   |
  |   S
V("a")
```

ユニフィケーションすると変数の内部表現がポインタに変わります。


``` text
 'a  string
  |   |
  v   v
+---+---+
| * | * |
+-|-+-|-+
  |   |
  |   S
string
```

これで出てくるポインタをどんどん辿っていけば `'a` から `S` に到達するので `'a` が `S` になることが分かります。無事ユニフィケーションできているようです。

### ゆにふぁい！（２）

もう1例、 件の `'b -> 'c = 'a = string -> unit` のユニフィケーションをしてみましょう。ゆにふぁい！

まず、変数 'a, 'b, 'c があります。


``` text
 'a  'b  'c
  |   |   |
  v   v   v
+---+---+---+
| * | * | * |
+-|-+-|-+-|-+
  |   |   |
V("a")| V("c")
   V("b")
```

`'a = 'b -> 'c` です。

``` text
 'a  'b  'c
  |   |   |
  v   v   v
+---+---+---+
| * | * | * |
+-|-+-|-+-|-+
  |   |   |
  |   | V("c")
  | V("b")
F('b, 'c)
```

`string -> unit` があります。

``` text
                    string -> unit
                           |
                 unit      |
 'a  'b  'c string |       |
  |   |   |   |   ++  +----+
  v   v   v   v   v   v
+---+---+---+---+---+---+
| * | * | * | * | * | * |
+-|-+-|-+-|-+-|-+-|-+-|-+
  |   |   |   |   |   +----+
  |   | V("c")S   U        |
  | V("b")           F(string, unit)
F('b, 'c)
```

`'a` と `string -> unit` をユニファイします。

`'a` を辿ると `F('b, 'c)` 、 `string -> unit` を辿ると `F(string, unit)` があります。
ユニフィケーションのルールに則って、それぞれの構成子どうしでユニフィケーションします。
つまり、 `'b` と `string` 、 `'c` と `unit` でユニフィケーションします。

結果

``` text
                    string -> unit
                           |
                 unit      |
 'a  'b  'c string |       |
  |   |   |   |   ++  +----+
  v   v   v   v   v   v
+---+---+---+---+---+---+
| * | * | * | * | * | * |
+-|-+-|-+-|-+-|-+-|-+-|-+
  |   |   |   |   |   +----+
  |   | unit  S   U        |
  | string           F(string, unit)
F('b, 'c)
```


これで `'a` を辿っていくと `F('b, 'c)` 、さらに `'b` と `'c` を辿ると `string` と `unit` に到達するので `'a = string -> unit` になってますし、 `'b = string` 、 `'c = unit` もでてきました。

この表現でうまくいきそうです。

### 簡約

このままでも問題ないのですが、少しだけ懸案事項があるので解消しておきましょう。

`'a = 'b = 'c = 'd = string` のように、長い制約の連鎖があったとします。

``` text
 'a  'b  'c  'd
  |   |   |   |
  v   v   v   v
+---+---+---+---+
| * | * | * | * |
+-|-+-|-+-|-+-|-+
  |   |   |   |
 'b  'c  'd   S
```


このときに `'a = string` であることは間違いなく表現できているのですが、少し経由するポインタの数が多いですね。
何度も参照すると遅そうです。
もうちょっと定量的に言うと読み取りがセルの数の $O(n)$ に比例する表現はできるだけ避けたいです。 $O(\log n)$ かそれ以下くらいに抑えたいものです。

これには簡単な解決策があります。中間のポインタを省いて値を保持している型に書き換えてしまえばいいのです。先程の例でいくとこうです。

``` text
 'a  'b  'c  'd
  |   |   |   |
  v   v   v   v
+---+---+---+---+
| * | * | * | * |
+-|-+-|-+-|-+-|-+
  |   |   |   |
 'd  'd  'd   S
```

この状態なら $O(1)$ です。
あとは操作する度にポイタが変わるので、こういう簡約をいつ行うかという問題だけです。
参照を作る度に神経質にやってては遅そうです。読み取るときに参照を辿っていく道すがらにあるポインタを変換するのがよくある手法らしいです。
しかし今回はRustを使うので、読み取り操作で値を書き換えるのはできるだけ避けたいです。ということで私の実装はユニフィケーションを呼んだときについでに簡約することになってます。
私の実装とは違いますが道すがらのポインタをまとめて最短のポインタに変換する最適化を入れれば全体としては $O(1)$ に近いくらいの速度で動くんじゃないかなと思ってます。

# 実装

それでは実装していきましょう。
というか、私が[開発中のコンパイラ](https://github.com/KeenS/webml)でのコードを紹介していきましょう。
Rustで書いているStandard MLのコンパイラです。

全体的に、式は 型なし → 型付け中 → 型あり の2段階の変化をします。

最終的にはこういう型になります。
型変数は気の迷いで入ってますが今のところ多相をサポートしてないので使うと後の方で「多相はサポートしてないよ」のエラーが出ます。

``` rust
pub enum Type {
    Variable(u64),
    Int,
    Real,
    Fun(Box<Type>, Box<Type>),
}
```

そして型付け中の型を表わすのがこのデータ型の定義です。型付け中には変数が必要なのでこっちの変数は気の迷いじゃないです。

``` rust
enum Typing {
    Variable(u64),
    Int,
    Real,
    Fun(NodeId, NodeId),
}
```

`NodeId` はあとで出てきますが、「型はポインタとします」といったときのポインタ相当です。 `Typing` は内部表現相当です。

それでは型のユニフィケーションと、制約式のユニフィケーションプールを見ていきましょう。

まずはユニフィケーションプールから。

``` rust
pub struct UnificationPool<T> {
    pool: Vec<Node<T>>,
}

pub struct NodeId(usize);
```

セルの列はベクタで表現します。
セルに保持するデータは `Node` という型をあとで用意することにします。
`NodeId` は要するに先程の説明でいう「型はポインタとします」のポインタのことです。

セルの中の値は実際に値を持っているか、それとも別のポインタを持っているかなのでした。
それを表現するコードがこちら。

``` rust
enum Node<T> {
    Value(T),
    Refer(NodeId),
}
```

この `T` には実際には型の内部表現、 `Typing` が入るのですがここでは抽象化しておきます。

この `NodeId` 、 `UnificationPool` 、 `Node` と先程の図による説明、対応がとれますかね？

```text
 'a    <- NodeId
  |
  v    ~~~~~~~
+---+
| * |  <- UnificationPool
+-|-+
  |    ~~~~~~~
  |
V("a") <- Node
```


さて、いくつかのユーティリティメソッドを生やしましょう。

``` rust
impl<T> Node<T> {
    fn new(t: T) -> Self {
        Node::Value(t)
    }
}


impl<T> UnificationPool<T> {
    pub fn new() -> Self {
        Self { pool: vec![] }
    }

    fn register(&mut self, node: Node<T>) -> NodeId {
        self.pool.push(node);
        NodeId(self.pool.len() - 1)
    }

    pub fn node_new(&mut self, t: T) -> NodeId {
        self.register(Node::new(t))
    }
    /// idが指しているノードを返す
    fn at(&self, node_id: NodeId) -> &Node<T> {
        &self.pool[node_id.0]
    }

    fn at_mut(&mut self, node_id: NodeId) -> &mut Node<T> {
        &mut self.pool[node_id.0]
    }

    /// idが指しているノードから辿っていって値を返す
    pub fn value_of(&self, mut id: NodeId) -> &T {
        loop {
            match self.at(id) {
                Node::Value(t) => return t,
                Node::Refer(new_id) => id = *new_id,
            }
        }
    }

}
```

これでもうユニフィケーションまであと一息です。
簡約を定義します。

``` rust
impl<T> UnificationPool<T> {
    fn value_id(&self, mut id: NodeId) -> NodeId {
        loop {
            match self.at(id) {
                Node::Value(_) => return id,
                Node::Refer(new_id) => id = *new_id,
            }
        }
    }
    fn reduction(&mut self, mut start: NodeId) {
        // 一旦値のあるノードまで辿ってIDを取得して、
        // もう一度舐めて道すがらの参照を変換していくサボり実装
        let value_id = self.value_id(start);
        loop {
            match self.at_mut(start) {
                Node::Value(_) => {
                    return;
                }
                Node::Refer(ref mut id) => {
                    start = *id;
                    *id = value_id;
                }
            }
        }
    }
}
```

これでもう制約のユニフィケーションを定義できます。
今回型単体のユニフィケーションと制約式を跨いだ処理は分けて書く方針なので引数で型単体のユニフィケーション関数を受け取ります。

``` rust
impl<T> Node<T> {
    // 値なら取り出し、別セルの参照なら捨てる
    fn take(&mut self) -> Option<T> {
        // 所有権の関係でダミー用の値と差し替えることでデータを取り出す
        // NodeIdはダミーになっているのでこのNodeは使えなくなっている
        let node = std::mem::replace(self, Node::Refer(NodeId(std::usize::MAX)));
        match node {
            Node::Value(t) => Some(t),
            Node::Refer(_) => None,
        }
    }
}

impl<T> UnificationPool<T> {
    pub fn try_unify_with<E>(
        &mut self,
        id1: NodeId,
        id2: NodeId,
        // 型単体のユニフィケーションは外部からもらう
        try_unify: impl FnOnce(&mut Self, T, T) -> Result<T, E>,
    ) -> Result<NodeId, E> {
        // それぞれの値を保持するノードのIDを取得して
        let lid = self.value_id(id1);
        let rid = self.value_id(id2);
        // それぞれの値を取り出す
        let l = self.at_mut(lid).take().unwrap();
        let r = self.at_mut(rid).take().unwrap();
        // 値をゆにふぁい！
        // 結果が新しい値となる。
        let new = try_unify(self, l, r)?;
        // ユニファイした左の方のノードに値を持たせる。
        // これは別にどっちでもいい。
        *self.at_mut(lid) = Node::Value(new);
        *self.at_mut(rid) = Node::Refer(lid);
        // ついでに簡約
        self.reduction(id1);
        self.reduction(id2);
        Ok(lid)
    }
}

```

これで制約式部分が完成です。
…といいたいところですがこのコード（`try_unify_with`）にはバグがあります。どこだか分かりますか？
関数内の2-4行目です。左右の参照先が同じ場合は最初の `self.at_mut(lid).take()` で値が無になったあと、 続く `self.at_mut(rid).take().unwrap()` が既に無になったデータを取り出そうとしてパニックになってしまいます。
正しくは2-3行目の間にガードを入れた以下のコードです。

``` rust
impl<T> UnificationPool<T> {
    pub fn try_unify_with<E>(
        &mut self,
        id1: NodeId,
        id2: NodeId,
        try_unify: impl FnOnce(&mut Self, T, T) -> Result<T, E>,
    ) -> Result<NodeId, E> {
        let lid = self.value_id(id1);
        let rid = self.value_id(id2);
        // 左右が同じ場合はもうすることがない
        if lid == rid {
            return Ok(lid);
        }
        let l = self.at_mut(lid).take().unwrap();
        let r = self.at_mut(rid).take().unwrap();
        let new = try_unify(self, l, r)?;
        *self.at_mut(lid) = Node::Value(new);
        *self.at_mut(rid) = Node::Refer(lid);

        self.reduction(id1);
        self.reduction(id2);
        Ok(lid)
    }
}

```

これで本当に制約式部分が完成です。


最後に型単体のユニフィケーション。
型単体のユニフィケーションの途中で変数が出てくると推移的なユニフィケーションが走るので両者は相互再帰します。

``` rust
fn try_unify<'b, 'r>(
    pool: &'b mut UnificationPool<Typing>,
    t1: Typing,
    t2: Typing,
) -> Result<'r, Typing> {
    use Typing::*;
    match (t1, t2) {
        // 両者が等価ならそれで終わり
        (t1, t2) if t1 == t2 => Ok(t1),
        // 片方が変数ならもうに合わせる
        (Variable(_), ty) | (ty, Variable(_)) => Ok(ty),
        // 構造的型は構造が同じ場合は構成子それぞれでゆにふぁい！
        (Fun(p1, b1), Fun(p2, b2)) => {
            let p = pool.try_unify_with(p1, p2, try_unify)?;
            let b = pool.try_unify_with(b1, b2, try_unify)?;
            Ok(Fun(p, b))
        }
        /// それ以外はエラー
        (t1, t2) => Err(TypeError::MisMatch {
            expected: conv_ty(pool, t1),
            actual: conv_ty(pool, t2),
        }),
    }
}
```

はい、これでユニフィケーションの仕組みが整いました。
これらをベースに `NodeId` 同士でユニフィケーションする `unify` メソッドを定義するのは容易いでしょう。
また、型の内部表現(= `Typing`)を受け取って、裏側で一旦 `UnificationPool` に登録して `NodeId` を取得してから別の型とユニフィケーションする関数 `give` なども簡単に書けます。

そこまで用意できたらあとは式にあわせて推論していくだけです。例えばApplyは以下のように書かれています。

``` rust
App { ty, fun, arg } => {
    self.infer_expr(fun)?;
    self.infer_expr(arg)?;
    self.give(fun.ty(), Typing::Fun(arg.ty(), *ty))?;
    Ok(())
}
```

再帰的なアルゴリズムなので `infer_expr` を再帰呼び出してます。
制約式に順序はないので `self.give` と `self.infer_expr` の順序は（多分）どうでもいいですが、なんとなくこの順番で呼んでます。

さて、最終的に型付けが終わったら `Typing` から `Type` に変換します。
これはプールから取得していくだけなので簡単ですね。

``` rust
fn resolve(pool: &UnificationPool<Typing>, id: NodeId) -> Type {
    conv_ty(pool, pool.value_of(id).clone())
}

fn conv_ty(pool: &UnificationPool<Typing>, ty: Typing) -> Type {
    use Typing::*;
    match ty {
        Variable(id) => Type::Variable(id),
        Int => Type::Int,
        Real => Type::Real,
        Fun(param, body) => Type::Fun(
            Box::new(resolve(pool, param)),
            Box::new(resolve(pool, body)),
        ),
    }
}
```

ということで型推論のコードの紹介は断片的でしたが腕力で実装したユニフィケーションの実装でした。

# ノート

* 既存のアルゴリズムを（うっすらとは知ってますが）一切ちゃんと調べずに書いたコードなので間違ってる可能性もあります。
* このコードを書こうと思ったのは「型推論って[Union-Find木（森）](https://ja.wikipedia.org/wiki/%E7%B4%A0%E9%9B%86%E5%90%88%E3%83%87%E3%83%BC%E3%82%BF%E6%A7%8B%E9%80%A0)の挙動に似てない？」と思ったからです。
  途中で簡約とか計算量の話がでてきたのはそのためです。
  + 真面目に分析してませんが、「ユニフィケーションするときにノードの大きい方を根にする」「ノードから値を読み出すときに毎度簡約する」をちゃんとやればUnion-Find木と同じく $O(α(n))$ （ $α(n)$ はアッカーマン関数の逆関数）になるんじゃないかと思ってますが定かではないです。$α(n)$ は実用の範囲ではほぼ定数なので 「$O(1)$ に近いくらいの速度」と言った訳です。
* 実装してる途中で「これ、Prologの実装ってこんな感じじゃなかったっけ？」という気持になりました。まあ、当たり前っちゃあたりまえなんですけど。


# おまけ: オーバーロードのサポート

SMLには困った仕様としてオーバーロードがあります。
組み込み関数 `+` はオーバーロードされているので `int` にも `real` にも使えます。

```text
- 1 + 1;
val it = 2 : int
- 1.1 + 1.0;
val it = 2.1 : real
```

しかし型システム上はオーバーロードは存在しないので「`+` は `int` にも `real` にも使えるけど、どっちかに決まらなかったら `int` 決め打ちになる」という仕様があります。


``` text
(* x + y では型が決まらないのでintにフォールバックする *)
- fun add x y = x + y;
val add = fn : int -> int -> int
(* なんなら単体でも型は決まらない *)
- op+;
val it = fn : int * int -> int
```

しかし型推論は式全体全体を見てくれるので局所的に型が分からなくても全体で定まっていれば `real` にも推論してくれます。

``` text
- fun add x y = x + y + 0.0;
val add = fn : real -> real -> real
```


この対応面倒ですね。Prolog風にバックトラックを実装しようかとも思いましたが `+` が連鎖すると多分死ぬのでやめました。

面倒なのでやっつけで実装します。
こいつは `Typing` に数値演算向けのオーバーロードされた型を入れておきます。

``` rust
enum Typing {
    // ...
    OverloadedArith,
}
```


これのユニフィケーション規則はこう。

``` rust
(Int, OverloadedArith) | (OverloadedArith, Int) => Ok(Int),
(real, overloadedarith) | (overloadedarith, real) => ok(real),
```

もちろん、数値演算は `OverloadedArith` として推論されます。

``` rust
BinOp { op, ty, l, r } => {
    if ["+", "-", "*"].contains(&op.0.as_str()) {
        self.infer_expr(l)?;
        self.infer_expr(r)?;
        self.unify(l.ty(), r.ty())?;
        self.unify(l.ty(), overloaded_arith)?;
        self.unify(*ty, l.ty())?;
        Ok(())
    } // ...
}
```

最終的に `Typing` から `Type` に変換するところで、オーバーロードが残ったままだったら `Int` にフォールバックするという処理を加えます。

``` rust
fn conv_ty(pool: &UnificationPool<Typing>, ty: Typing) -> Type {
    use Typing::*;
    match ty {
        // ...
        OverloadedArith => Type::Int,
    }
}

```

今のところこれで動いてるみたいです。

ちゃんとやるならオーバーロードには型の集合を定義して、ユニフィケーションのときは交差集合をとって…と実装することになるでしょうが、そもそも仕様がやっつけ感あるのでこのくらいで十分でしょう。
