---
categories: [Rust, Transaction]
date: 2017-06-06T18:20:52+09:00
title: Rustのトランザクション抽象化ライブラリ作った
---

κeenです。最近[KeenS/transaction-rs: The transaction abstraction library and its executors for rust](https://github.com/KeenS/transaction-rs)というライブラリをリリースしたのでそれについて。

<!--more-->

# モチベーション

Rustでドメインロジックを書いていると以下のようなコードが出てきました。
(実際はもうちょっと複雑ですが本質ではないので簡略化します)

``` rust
struct GroupPgDao(r2d2::Pool<ConnectionManager<PgConnection>>);
impl GroupPgDao {
  fn get_conn(&self) -> &PgConnection { /*... */ }
  fn delete_user(&self, user: &User, group: &Group) -> Result<()> {
      let cn = self.get_conn();
      //...
  }
  fn add_user(&self, user: &User, group: &Group) -> Result<()> {
      let cn = self.get_conn();
      //...
  }
}

trait GroupService: HaveGroupDao {
  fn change_group(&self, user: &User, from: &Group, to: &Group) -> Result<()> {
     let service = self.group_dao();
     dao.delete_user(user, from)?;
     dao.add_user(user, to)
  }
}
```


この`change_group`はDBのトランザクション内で実行しないと困ります。`delete_user`が成功して`add_user`が失敗するとuserがどちらにも所属しなくなるからです。じゃあ`change_group`の中でトランザクションを作って実行すればいいかというとそうでもなく、いくつか問題があります。

1. もっと外側でトランザクションが必要な操作が発生するかもしれない
2. 同一トランザクション内で実行するにはコネクションを共有しないといけないが、関数個々でプールからコネクションを取得している

1に関しては誰がトランザクションを実行するのかという責任問題と、どの操作がトランザクションを必要とするのかという契約問題の2つが複合しています。
dieselのトランザクションはネストをサポートしているので必要そうな部分至るところトランザクションを作るというのも考えられますが、プログラマが目で責任と契約を追うことになりバグりかねません

2つ目はインジェクションの問題ですね。これについてはシンプルに引数にコネクションを渡すというのも考えられますが持ち回るのは非常に大変です。

これらを解決するために作ったのが冒頭に挙げたtransaction-rsです。

# transaction-rs
transaction-rsは非常に[future-rs](https://github.com/alexcrichton/futures-rs)に似ています。

``` rust
pub trait Transaction<Ctx> {
    type Item;
    type Err;
    fn run(&self, ctx: &mut Ctx) -> Result<Self::Item, Self::Err>;

    fn map<F, B>(self, f: F) -> Map<Self, F>
    where
        F: Fn(Self::Item) -> B,
        Self: Sized,
    { ... }
    fn and_then<F, B>(self, f: F) -> AndThen<Self, F, B>
    where
        B: Transaction<Ctx, Err = Self::Err>,
        F: Fn(Self::Item) -> B,
        Self: Sized,
    { ... }
    fn map_err<F, B>(self, f: F) -> MapErr<Self, F>
    where
        F: Fn(Self::Err) -> B,
        Self: Sized,
    { ... }
    fn or_else<F, B>(self, f: F) -> OrElse<Self, F, B>
    where
        B: Transaction<Ctx, Item = Self::Item>,
        F: Fn(Self::Err) -> B,
        Self: Sized,
    { ... }

    // ...
}
```

`map`や`and_then`のある「よくあるパターン」です。`futures`の他`Result`や`Option`にもありますね。
`Transaction`を作った時点ではまだ計算は実行されません。DBへのクエリも実行されてません。`run`が呼ばれた時点でようやく計算が実行され、結果を取り出すことができます。`future-rs`と同じくこれは *ゼロコスト抽象化* をしていて、トランザクションの関数を積んでいくと実行時にはオートマトンにまで落ちます。 詳しくはこちらをどうぞ[Zero-cost futures in Rust · Aaron Turon](https://aturon.github.io/blog/2016/08/11/futures/)。

# トランザクションの合成と要求

話を冒頭のコードに戻しすと、transaction-rsを使うとDAOはこう書けるでしょう。

``` rust
struct GroupPgDao;
impl GroupPgDao {
  fn delete_user(&self, user: &User, group: &Group) -> Box<Transaction<... ()> {
      with_conn(|cn| {
          //...
      })
      .boxed()
  }
  fn add_user(&self, user: &User, group: &Group) -> Box<Transaction<... ()> {
      with_conn(|cn| {
          //...
      })
      .boxed()
  }
}
```

返り値の型がやや残念になるものの、中身はそれほど変わらないです。
`Transaction`を返しているのでこのコードはトランザクション下で実行されることを要求しているのが分かりますね。

そして自分で持っていたコネクションプールと`self.get_conn()`が消え、変わりに`with_conn`関数でコネクションを取得しています。この関数は付属の`transaction-diesel`にて定義されています。何やら虚空からコネクションが沸いてるようにも見えますがちゃんと正規のルートで渡ってきています。これについては後で説明します。

さらにサービスの方も書き換えてみます。

``` rust
trait GroupService<Ctx>: HaveGroupDao<Ctx {
  fn change_group(&self, user: &User, from: &Group, to: &Group) -> Box<Transaction<Ctx, ... ()>>> {
     let service = self.group_dao();
     dao.delete_user(user, from)
         .and_then(move|()| dao.add_user(user, to))
         .boxed()
  }
}
```

疑似コードなのでコンパイルが通るか分かりませんがまあ、こんなところになるでしょう。
このコードで分かるように、トランザクション同士の合成も可能なのです。
そして、`Transaction`を返しているのでこのコード自身もまたトランザクションを要求していることが分かります。

さて、気付いたでしょうか。これで上記の契約問題、責任問題、インジェクション問題が解決しています。

契約問題については`Transaction`を返す関数はトランザクションを要求するというシグナルですね。非常に分かりやすいです。
責任問題についても`Transaction`を`run`する関数にトランザクションの責任があります。`run`しないと結果を取り出せないので結果が欲しい人がそのまま責任を持つ形になります。

なんとなく、`unsafe`に似ているのが分かりますか？`unsafe fn`は内部でアンセーフなことをやるし、呼び出す関数に契約を満たすことを要求します。`unsafe`ブロックはアンセーフな処理の契約に責任を持ちます。同じく`Transaction`を返す関数は内部でトランザクションを要求する処理をするし、呼び出し側に契約を満たすことを要求します。`run`がトランザクションという契約に責任を持ちます。

# インジェクションの仕組み

先のコードでマジカルにコネクションが外からやってきていました。この仕組みについて説明します。とはいっても複雑ではないです。

`Transaction`のコードを思い出してみましょう。

``` rust
pub trait Transaction<Ctx> {
    fn run(&self, ctx: &mut Ctx) -> Result<Self::Item, Self::Err>;
    // ...
}
```

`run`すると結果が取り出せるのでした。そして`run`は引数`Ctx`をとります。
これは抽象的なトランザクションなので抽象的な「コンテキスト」ですが、[STM](https://github.com/Marthog/rust-stm)ならSTM、データベースならコネクションがコンテキストです。
コンテキストを受け取って計算を実行するということは、`Transaction`は見方を変えると`Ctx`を引数にとる関数になっています。

なので`with_conn`関数は`run`で受け取った引数を取り出しているだけです。

# 小まとめ

このtransaction-rsは

* トランザクションの契約と責任を型で表現している
* コネクションをインジェクトしている

さらに、`run`するまでは実際にはトランザクションは実行されていないので

* 「トランザクションを必要とする計算とその合成」と「トランザクションの実行」を分離している

というのが特徴です。大筋はこれがメインなのですが、実用の話をいくつか。

# つらい話と救いの話

`Transaction`で契約と責任は明確になりました。
では、便利になったかというとそうでもありません。例えばUserをCRUDしてみましょう。

``` rust
fn crud_user(dao: UserDao, ctx: Ctx) -> Result<()> {
    let tx = dao.create("name", 24)
      .and_then(move |id|
          dao.find(id)
              .and_then(move |user|
                  dao.update(None, user.age + 1)
                      .and_then(move |()|
                          dao.delete(user))));
    tx.run(ctx)
}
```

このように、`and_then`がネストしてしまいます。コードが不要に複雑になっただけですね。つらい。

しかしながら救いはあって、[`mdo`](https://github.com/TeXitoi/rust-mdo)というライブラリがあります。
mがなんなのかやdoがどこからきたのかは気にしてはいけません。
`transaction-rs`もこれをサポートしていて、マクロを使うことで以下のように書き換えられます。

``` rust
fn crud_user(dao: UserDao, ctx: Ctx) -> Result<()> {
    let tx = mdo! {
        id =<< dao.create("name", 24);
        user =<< dao.find(id);
        () =<< dao.update(None, user.age + 1);
        ret dao.delete(user)
    };

    tx.run(ctx)
}
```

本質的にはネストを代入っぽく書き換えただけですが、絶大な可読性の向上があります。
なので実際に`transaction`を使うときは`mdo`を併用することになるでしょう。

# `futures`との違い

`futures`に非常によく似ていると言いましたが、逆に何が違うのかという話です。大きな違いは2つあります。

1つには結果を取り出すメソッドの違いです。`Future`の`poll`は引数を取りませんが、`Transaction`の`run`は取ります。また、それに合わせてトレイトがジェネリクスになっています。

``` rust
pub trait Future {
    fn poll(&mut self) -> Poll<Self::Item, Self::Error>;
}

pub trait Transaction<Ctx> {
    fn run(&self, ctx: &mut Ctx) -> Result<Self::Item, Self::Err>;
}
```

もう1つはクロージャを取るメソッドの違いで、`Future`は`FnOnce`を取るのに対して`Transaction`は`Fn`を取ります。

``` rust
pub trait Future {
    fn map<F, U>(self, f: F) -> Map<Self, F>
        where
            F: FnOnce(Self::Item) -> U,
            Self: Sized,
}

pub trait Transaction<Ctx> {
    fn map<F, B>(self, f: F) -> Map<Self, F>
    where
        F: Fn(Self::Item) -> B,
        Self: Sized,
}
```

`Future`は単純に計算の合成と実行を分離しているのに対して`Transaction`はSTMのように失敗した計算のリトライにまで責任を持つことがあるので再実行可能でないといけません。さらに、再実行するということは羃等性の確保も必要です。羃等性を保つためコンテキスト以外への副作用も禁止する必要があって、`FnOnce`でも`FnMut`でもなく`FnOnce`を要求します。

実はこのことが若干問題になるケースもあります。データベースのトランザクションなら別に再実行せずにロールバックするだけなので`FnOnce`で十分なケースもあります。`FnOnce`の方が所有権に寛容なので`FnOnce`なら書けるのに`Fn`が要求されて、実際には`FnOnce`しか必要ない、というケースに何度か直面しました。いくつかのハックで乗り越えられましたが本質的ではない問題なので今後何か変更があるかもしれません。

# 他の問題点
ちょっと説明するのが面倒なのでコードを出さないのですが、計算の合成と実行を分離すると合成の時点では生きているけど実行するときに生きてるか分からないオブジェクトが出てきたりします。
概ねライフタイム境界を明示的に書いてあげると解決するのですが、一部`join`などを駆使してハックしないとコンパイルが通らないことがあり面倒です。

返り値型が複雑なのも問題で、ひとまず[`impl Trait`](https://github.com/rust-lang/rust/issues/34511)がstableに降ってくるまではそもそも型を書けません（クロージャが匿名なせい）。
`Box`に包むにしても少なくとも`Ctx`が型引数に増えますし、`diesel`と一緒に使うならコネクションのライフタイムも型に入れる必要があったりと`Result`や`BoxFuture`に比べてやや重くなっています。

記法についても問題があります。`mdo`を使うとある程度は解決しますが、早期リターンがしづらく、頑張って分岐のネストを書かないといけません。これは`futures`も抱える問題です。
`futures`についてはのジェネレータのパッチを当てることで[`Result`のように書けるデモ](https://github.com/alexcrichton/futures-await)があるのですが、先述の通り`transaction`では`FnOnce`ではなく`Fn`を取っているのでジェネレータにはエンコードできないのではないかと思っています（あまり調査してないです）。

# まとめ

* `transaction-rs`を作ったよ
* トランザクションの契約と責任を型で明示するよ
* コネクションをインジェクトするよ
* コードは複雑になるかもね
* `mdo`を使うと複雑さを抑えられるよ
* でもライフタイムとかの問題もあるよ
* 将来どうにかするかもね

# 零れ話

これを作ったあとに[【ScalaMatsuriセッション当選御礼】ドワンゴ秘伝のトランザクションモナドを解説！ - Qiita](http://qiita.com/pab_tech/items/86e4c31d052c678f6fa6)を思い出して読んでみたらほぼ同じものを作ってましたね。
Scalaのpoor-man's type classだと読み辛いですがほぼ同じです。

相違点は1つには返り値が`Result`か`Future`か。実際、トランザクションモナドは成功と失敗両方に対してモナドになっている型であれば（こういうの名前ついてないのかな、重モナドとかそんなの）何にでも定義できます。要はトランスフォーマーとして定義可能です。
しかしながらRustには高階多相がなくてトランスフォーマーは書けないのでどちらかを選ぶ必要があり、`transaction-rs`では`Result`を選びました。外部ライブラリへの依存を減らしたいだとかウェブアプリケーションフレームワークにFutureを受け付けてくれるものがなくてあまり意味がないとかその辺です。
需要が発生したらトランスフォーマーマクロとか作るかもしれません。

もう1つ相違点があって、fujitaskの方はRead/Writeを型で判別するようになっていますが`transaction-rs`にはそういう機能はありません。
fujitaskを読み返すまではそれには思い至りませんでした。
で、存在を知った後に実装したかというと別にしてません。
Rustでも`Ctx`に幽霊型を付ければ実装自体は可能なのですがデータベースライブラリがトランザクションの分離レベルの変更をサポートしていないのであまり意味がないからです。
データベース側でサポートされたらやるかもしれません。

Scalaのfujitaskは便利そうで、概ねこちらもそのようなものなのですが、Rust特有の所有権/ライフタイムによる問題とScalaの`for`式相当のものがないという理由でちょっとつらい感じになってます。つらい。
