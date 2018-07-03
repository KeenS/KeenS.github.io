---
categories: [Rust]
date: 2018-07-01T21:58:14+09:00
description: "Rust Workshopでの発表用"
title: "Rustと3種のDSL"
---
<section data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
<script type="text/template">
# Rustと3種のDSL
----------------------
[Running Rust in Production](https://d-cube.connpass.com/event/90317/)

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# DSLとは
>  Lispでは，プログラムをただプログラミング言語に従って書くことはしない． プログラミング言語を自分の書くプログラムに向けて構築するのだ
> -- Paul Graham (On Lispより)


<!-- .slide: class="center" -->

===
# DSLとは
----------

* ドメイン特化言語
* 自分の解きたい問題に合わせてミニ言語を作る
  + 関心に集中
  + 言語を設計することでドメインへの考察が深まる
* 2種類ある
  + 内部DSL: ホスト言語の式でそれっぽく作る
  + 外部DSL: 外部化してパーサから作る
* まずは内部DSLを検討しよう

===

# DSLの使いどころ
----------------

* だいたい以下の条件
 + 素直に書くと記述が多い
 + 細かなバリエーションがあって関数一つにはまとめられない
 + → スイートスポットがDSL
* 典型的にはビジネスロジックとか

===
# Actcast
---------

* Idein社で開発中のサービス
* IoTデバイスを管理する
  + 人、管理グループ、デバイス、アプリケーションなどエンティティが多め
* APIはOpen API(旧Swagger)を使う
* まだ開発中
  + 一緒に開発してくれる人募集してます!
* 今回はこれで使ったDSLの話

===

# 珠玉の内部DSL

<!-- .slide: class="center" -->

===

# アクセス権限チェック


``` rust
fn update_device(operator: User, device: Device) -> Result<Device> {
    use dsl::*;

    let conn = get_conn();
    if !is_user_readable_to_device(operator, device, &conn)? {
        return Err(Error::ReadPrivilege);
    }

    if !is_user_writeable_to_device(operator, device, &conn)? {
        return Err(Error::WritePrivilege);
    }

    // do update
    Ok(device.clone())
}

```

===

# アクセス権限チェック


```rust
fn update_device(operator: User, device: Device) -> Result<Device> {
    use dsl::*;

    let conn = get_conn();
    check_if(
        &conn,
        // 権限チェックのDSL
        operator.is_readable().is_writable().to(device),
    )?;
    // do update
    Ok(device.clone())
}
```

===

# モデリング
------------

``` rust
// これを実装しているものがDSLになる気持ち
trait Precondition {
    fn check(&self, conn: &DbConn) -> Result<()>;
}

struct And<P, Q>(P, Q);

impl<P, Q> Precondition for And<P, Q>
where
    P: Precondition,
    Q: Precondition,
{
    fn check(&self, conn: &DbConn) -> Result<()> {
        self.0.check(conn).and_then(|()| self.1.check(conn))
    }
}

struct IsReadable<A, B>(A, B);
struct IsWritable<A, B>(A, B);

impl Precondition for IsReadable<User, Device> {
    fn check(&self, _conn: &DbConn) -> Result<()> {
        // do ckeck
        Ok(())
    }
}

impl Precondition for IsWritable<User, Device> {
    fn check(&self, _conn: &DbConn) -> Result<()> {
        // do ckeck
        Err(Error::WritePrivilege)
    }
}


```

===
# DSLの構築
-----------

``` rust
mod dsl {
    use super::*;

    pub struct IncompleteIsReadable<T>(T);

    pub trait IsReadableDsl {
        type Out;
        fn is_readable(self) -> Self::Out;
    }

    impl IsReadableDsl for User {
        type Out = IncompleteIsReadable<User>;
        fn is_readable(self) -> Self::Out {
            IncompleteIsReadable(self)
        }
    }
   // 長いので略
}

```

===

# 内部DSL
---------

* [コード全体](https://gist.github.com/KeenS/d8ef8c95110742d31c74c750ed456ecb)
* 正道
* トレイトをうまく使う
  + メソッド記法でそれっぽく
  + 演算子オーバーロードはあまり悪用しない
* DSLはあくまで略記のための手法
  + 何がしたいかよくモデリングする
* モデルと記法分離することで内部実装を変更できたりも

===

# 特殊化
-------

``` rust
impl Precondition for IsReadableWritable<User, Device> {
    fn check(&self, _conn: &DbConn) -> Result<()> {
        // 本当はAnd<IsReadable<_, _>, IsWritable<_, _>>でも可能
        // DBアクセスを減らすために特殊化
        Err(Error::WritePrivilege)
    }
}

mod dsl {
    impl ToDsl<Device> for IncompleteIsReadableWritable<User> {
        type Out = IsReadableWritable<User, Device>;
        fn to(self, to: Device) -> Self::Out {
            // ユーザインタフェースを(見た目レベルでは)変えずに内部実装を変更可能
            // And(IsReadable(self, to), IsWritable(self, to))
            IsReadableWritable(self.0, to)
        }
    }
}
```



===


# 諸刃の剣のマクロDSL
<!-- .slide: class="center" -->


===

# API定義
---------

``` rust
{
    #[derive(Serialize, Deserialize, Debug, Clone)]
    pub struct PathParameters {
        app_id: api::AppId,
    }

    app = app.route(
        "/apps/{app_id}",
        Method::GET,
        | (req, path, sess):
          (HttpRequest<Context>, Path<PathParameters>, Session)|-> FutureResponse<HttpResponse> {
            let _user: model::User = match sess.get::<model::User>(SESSION_KEY) {
                Ok(Some(user)) => user,
                Ok(None) => return Box::new(Err(hoge).into_future()).responder(),
                Err(e) => return Box::new(Err(e).into_future()).responder(),
            };
            let path = path.into_inner();
            // 関心があるのはここ
            service::api::find_app(&*ctx, path.app_id)
            // and do more
            let fut = fut.map(Into::into) .from_err();
            let fut: Box<Future<Item = HttpResponse, Error = Error>> = Box::new(fut);
            fut.responder()
    });
}
```

===

# API定義
---------

``` rust
def_api!{
    Method: GET,
    // ルーティングと引数を同時記述
    Path: { /apps/{app_id:api::AppId} },
    Session,
    Response: api::api::apps_::get::Response,
    // この`path`に`app_id`が入る。
    // 引数もマクロの書き方で増減する
    Handler: |ctx, path, _user| {
        service::api::find_app(&*ctx, path.app_id)
        // and do more
    }
};
```

===

# マクロDSL
-------------

* マクロの実装は出せない
  + 出しても多分読めない
  + 合計300行くらいのマクロ群
* 可能なら使わない方が良い
  + マクロは第一級でない(関数の引数に渡せない)
* マクロにしかできないこともある
  * arityの調整、シンボルから文字列の生成、構造体の定義の生成などなど
* まあまあトリッキー
  + [The Little Book of Rust Macros](https://danielkeep.github.io/tlborm/book/index.html)読んで

===
# マクロと型
------------

``` rust
macro_rules! take_tt {
    ($t:tt) => {
        stringify!($t)
    };
}
// エラー: `::std::string::String`はそのままだと複数のttになる
let _ = take_tt!(::std::string::String);

macro_rules! wrap_ty {
    ($t:ty) => {
        take_tt!($t)
    };
}
// OK: 一旦tyとしてパースすると1つのttになる
let _ = wrap_ty!(::std::string::String);
```

===

# マクロのCPS変換
---------

``` rust
macro_rules! id_ty {
    ($t:ty) => {
        $t
    };
}
// エラー: マクロの入れ子呼び出しはできない
let _ = take_tt!(id_ty!(::std::string::String));

macro_rules! id_ty_cps {
    ($t:ty, $callback:ident) => {
        $callback!($t)
    };
}

// OK: コールバックとして受け取ればよい
let _ = id_ty_cps!(::std::string::String, take_tt);

```



===

# みぞの鏡の外部DSL
> erised stra ehru oyt ube cafru oyt on wohsi -- The Mirror of Erised
<!-- .slide: class="center" -->



===

# JSON Schema
-------------

``` json
{
  "type":"object",
  "properties": {
     "id": {"type": "integer"},
     "name": {"type": "string"}
  }
  "required": ["id", "name"]
}
```
===

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">jsonで型付きデータスキーマを定義するときによくある<br>{name: &quot;hoge&quot;, type: &quot;typename&quot;, required: false}<br>とかやるやつダサくない？<br>hoge: typename option<br>って書きたくない？</p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/920484211961630720?ref_src=twsrc%5Etfw">2017年10月18日</a></blockquote>


===

# JSON Schema
-------------
略記できるツールを作った

```
struct {
  id: integer,
  name: string,
}
```

===
# 外部DSL
---------

* [KeenS/chema](https://github.com/KeenS/chema)
* ほしいものがなんでも手に入る
* 最後の手段
* 文字列からRustのデータ型を構成する
  + 今回はJSON SchemaがターゲットなのでJSONにダンプ
  + ある意味ではコンパイラ
* 最後の手段
* 開発支援ツールやエラーメッセージが壊滅的
* 最後の手段

===
# まとめ
--------

* DSLは色々なケースで役に立つよ
* 3種のDSLを使いこなそう
* 珠玉の内部DSL
  + 普段はこれを使おう
* 諸刃の剣のマクロDSL
  + デメリットをよく考えて使おう
* みぞの鏡の外部DSL
  + なんでも出来るけど溺れるな

</script>
</section>
