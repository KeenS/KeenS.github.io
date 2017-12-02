---
categories: [Rust, Advent Calendar, Advent Calendar 2017]
date: 2017-12-01T21:10:52+09:00
title: "RustのDI"
---
κeenです。[Rustその2 Advent Calendar 2017](https://qiita.com/advent-calendar/2017/rust-lang-2)が空いてたので小ネタをば。1日目の記事です。
[Dependency Injection](https://ja.wikipedia.org/wiki/%E4%BE%9D%E5%AD%98%E6%80%A7%E3%81%AE%E6%B3%A8%E5%85%A5)の話。
<!--more-->

例えば[３層アーキテクチャ](https://ja.wikipedia.org/wiki/%E5%A4%9A%E5%B1%A4%E3%82%A2%E3%83%BC%E3%82%AD%E3%83%86%E3%82%AF%E3%83%81%E3%83%A3#.E4.B8.89.E5.B1.A4.E3.82.A2.E3.83.BC.E3.82.AD.E3.83.86.E3.82.AF.E3.83.81.E3.83.A3)で組む時には以下のような依存関係が発生します。

```
[user interface(web)]
  |
  V
[logic(service)]
  |
  V
[database access(dao)]
```

これを他の実装に依存せずにそれぞれの層を書きたい、どうしようという問題設定です。

# ナイーブな実装

簡単に考えたらRustならトレイトで抽象化してあげればよさそうです。

コードにするなら共通インターフェイスにトレイトを定義してあげて

``` rust
pub trait UserDao {
  fn find_user(&self, id: i32) -> Result<Option<User>>;
}
```

Dao側は実際のDBに合わせた実装、

``` rust
struct UserPgDao(PgConnection);

impl UserDao for UserPgDao {
  fn find_user(&self, id: i32) -> Result<Option<User>> {
    // ...
  }
}
```

そしてサービスレイヤはトレイトにのみ依存した実装をかけば１丁あがりです。

``` rust
struct UserService<U: UserDao>(U);

impl<U:UserDao> UserService<U> {
  pub fn get_user_by_id(&self, id: i32) -> Result<Option<User>> {
    self.0.find_user(id)
  }
}
```

## 問題点

シンプルなものなら上のもので問題ありません。問題は複雑な依存関係が発生したときに起こります。

``` rust
            [Web]
            /  \
           /    \
[GroupService] [UserService]
    |   \          |
    |     \        |
    |       \      |
    |         \    |
    |           \  |
[GroupDao]     [UserDao]
```

ここでのUserDaoへの依存のように複数からの依存関係があるとRustの所有権機能によって簡単にはコードを書けなくなります。



解決策は色々とあろうかと思いますが、ここではその一つ、Cake Patternの亜種によって解決したいと思います。

# RustでCake Pattern

Cake PatternはScala由来のDIパターンで、Scala界隈ではそれなりに使われているように思います。
詳しくは [実戦での Scala: Cake パターンを用いた Dependency Injection (DI) ](http://eed3si9n.com/ja/real-world-scala-dependency-injection-di) をあたって下さい。以下、Cake Patternは既知のものとして話を進めます。

幸いなことにCake Patternで必要とされる言語機能はRustにも対応するものがある程度揃っているのでRustに翻訳できます。

Scala | Rust
------|------
トレイト | トレイト
自分型アノテーション | トレイトの継承
class in class | モジュール
抽象メンバー | getterメソッド

この対応で翻訳してあげるとDaoとそのComponentはこうなり、

```rust
// 上述記事中のUserRepository相当
pub trait UserDao {/* ... */}
// 上述記事中のUserRepositoryComponent相当
pub trait HaveUserDao {
  type UserDao: UserDao;
  fn user_dao(&self) -> Self::UserDao;
}

```

ServiceとそのComponentはこうなります。

```rust
// 上述記事中のUserService相当
trait UserService: HaveUserDao {
  pub fn get_user_by_id(&self, id: i32) -> Result<Option<User>> {
    self.user_service().find_user(id)
  }
}
// UserServiceはHaveUserDaoにのみ依存するのでそれさえ実装していれば自動で実装を与えられます。
// もちろんテストなどで挙動を上書きしたければ具体的な型での実装で上書きできます。
impl<T:HaveUserDao> UserService for T {}

// 上述記事中のUserServiceComponent相当
trait HaveUserService {
  type UserService: UserService;
  fn user_service(&self) -> Self::UserService;
}
```

これで `UserService` が `UserDao` を専有しなくなりました。
同じように `Group{Dao,Service}` も同じようにつくってあげます。

するとサーバが以下のようにかけます。

```
struct Server {
  user_dao: UserPgDao,
  group_dao: GroupPgDao,
}

impl HaveUserDao for Server {
  type UserDao = UserPgDao;
  fn user_dao(&self) -> Self::UserDao {
    &self.user_dao
  }
}
impl HaveUserService for Server{
  type UserService = Self;
  fn user_service(&self) -> Self::UserService {
    self
  }
}

// 同じくGroupも
```

`Server` の依存する `user_dao` と `gropu_dao` も型パラメータで抽象化できますが、実際にその抽象化が必要になることはないでしょう。

DaoからServerまで矛盾なくコードが書けたので晴れて複依存問題が解決出来ました。以上小ネタでした。

# 他の解決策

## Daoをコピーする
複数回必要なら複数用意すればいいじゃないという発想。
悪くはないんですが例えばDaoでクエリ結果のキャッシュを持ちたい場合などに不都合です。

## Daoを参照で持つ
悪くはないんですがライフタイムパラメータが増えて型が煩雑になります。

## Daoを`Arc`で持つ
HTTPフレームワークが大抵`Sync`を要求してくるので`Rc`ではだめで、`Arc`です。
これでもいいと思います。

# 結びに
業務でDIするにあたっていい案もなかったので前職でのScalaの経験からCake Patternを使ってみたらとりあえずできたという感じです。
めちゃくちゃ便利という訳でもないですが今の所問題もないのでそのまま使っているのが現状で、誰か他の知見を下さい。
