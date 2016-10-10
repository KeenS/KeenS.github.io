---
categories: [SML]
date: 2016-10-10T11:05:38+09:00
title: SMLでモナド
---

κeenです。ML Workshop 2016でなんかSMLで型クラスする発表があったらしいので追ってみます。

implicit parameterをexplicitに渡すのではなく、モジュールとファンクタを使った実装です。

<!--more-->
# 前置き
Haskell風の中置演算子を使うので


``` standard-ml
infix 4 <$> <$ $>  <*> <* *>
infix 1 >>= >>

```

を予め宣言しておきます。

# 最初の試み

さて、(MLの機能の方ではない)functorを考えましょうか。functorには `fmap` が要求され、`<$>` なんかが実装されます。それを素直に表したらこうなるでしょうか。

``` standard-ml
signature FUNCTOR_MIN = sig
    type 'a t
    val fmap: ('a -> 'b) -> 'a t -> 'b t
end

signature FUNCTOR = sig
    include FUNCTOR_MIN
    val <$> : ('a -> 'b) * 'a t -> 'b t
    val <$ : 'a *  'b t ->  'a t
    val $> : 'a t *  'b  ->  'b t
    val void: 'a t -> unit t
end

functor MkFunctor(Fun: FUNCTOR_MIN): FUNCTOR = struct
    open Fun

    fun f <$> fa = fmap f fa
    fun a <$ fb =  (fn _ => a) <$> fb
    fun fa $> b = b <$ fa
    fun void fa = () <$ fa
end
```

さて、さらにそのサブクラスのapplicativeも作ります。 `include` を使うことで `FUNCTOR` を継承することを表します。


``` standard-ml
signature APPLICATIVE_MIN = sig
    include FUNCTOR

    val pure: 'a -> 'a t
    val ap : ('a -> 'b) t -> 'a t -> 'b t
end

signature APPLICATIVE = sig
    include APPLICATIVE_MIN

    val <*> : ('a -> 'b) t * 'a t -> 'b t
    val *> : 'a t * 'b t -> 'b t
    val <* : 'a t * 'b t -> 'a t
end

functor MkApplicative(App: APPLICATIVE_MIN): APPLICATIVE = struct
    open App

    fun af <*> aa = ap af aa
    fun aa *> ab = pure (fn _ => fn x => x) <*> aa <*> ab
    fun aa <* ab = pure (fn x => fn _ => x) <*> aa <*> ab
end
```

さらにmonadも作りましょうか。

``` standard-ml
signature MONAD_MIN = sig
    include APPLICATIVE

    val bind : 'a t -> ('a -> 'b t) -> 'b t
end

signature MONAD = sig
    include MONAD_MIN

    val return: 'a -> 'a t
    val >>= : 'a t * ('a -> 'b t) -> 'b t
    val >> : 'a t * 'b t -> 'b t
end


functor MkMonad(M: MONAD_MIN): MONAD = struct
    open M

    val return = pure
    fun ma >>= f = bind ma f
    fun ma >> mb = ma >>= (fn _ => mb)

end
```

ちょっと使ってみましょう。1つ1つインスタンスを作ってあげます。

``` standard-ml
structure OptionFunc = MkFunctor(
  struct
      type 'a t = 'a option
      fun fmap f = Option.map f
  end)

structure OptionApp = MkApplicative(
  struct
      open OptionFunc
      fun pure x = SOME(x)
      fun ap af aa = case (af, aa) of
                         (SOME(f), SOME(a)) => SOME(f a)
                       | _ => NONE
  end)

structure OptionMonad = MkMonad(
  struct
      open OptionApp
      fun bind ma f = case ma of
                          SOME(a) => f a
                        | NONE => NONE
  end)


local
    open OptionMonad
in
    fun add x y = x + y
    fun println s = print (s ^ "\n")

    val ret1 = add <$> SOME(1) <*> SOME(2)
    val ret2 = SOME(1) >>= (fn x =>
               SOME(2) >>= (fn y =>
               return (x + y)))
    val () = Option.app (println o Int.toString) ret1 (* => 3 *)
    val () = Option.app (println o Int.toString) ret2 (* => 3 *)
end

```

動きました。

さて、一見これで良さそうですがML Workshopでの発表では問題点を2点指摘しています。

* Monadのインスタンスが欲しいだけなのにFunctorまで遡って作らないといけない
* ダイアモンド継承が起きた時に共通の親クラスの実装の一貫性を保障出来ない

とのことです。私はこの問題点自体割と疑問なのですがまあ、問題らしいです。

# 解決案
継承のやり方をちょいと変えます。`Functor` までは先程と同じなのですが、Applicativeは
APPLICATIVE_MINにFUNCTORをincludeしません。また、衝突の問題から型が `applicative`になります（同じと言っちゃいましたがFunctorの方も `functor_` になってます）。

``` standard-ml
signature APPLICATIVE_MIN = sig
    type 'a applicative
    val pure: 'a -> 'a applicative
    val ap : ('a -> 'b) applicative -> 'a applicative -> 'b applicative
end
```

そしてAPPLICATIVEの方でincludeします。ここで `sharing` 制約を付けます。


``` standard-ml
signature APPLICATIVE = sig
    include FUNCTOR APPLICATIVE_MIN
    sharing type functor_ = applicative

    val <*> : ('a -> 'b) applicative * 'a applicative -> 'b applicative
    val *> : 'a applicative * 'b applicative -> 'b applicative
    val <* : 'a applicative * 'b applicative -> 'a applicative
end
```


で、 `APPLICATIVE_MIN` から `FUNCTOR_MIN` を生成するファンクタを作ります。

``` standard-ml
functor ApplicativeMinToFunctorMin(A: APPLICATIVE_MIN): FUNCTOR_MIN = struct
    open A
    type 'a functor_ = 'a applicative
    fun fmap f = ap (pure f)
end
```

applicativeからfunctor????感ありますがとりあえず進みます。

で、 `MkApplicative` は一旦内部で `Functor` を作ってそれを `open` します。


``` standard-ml
functor MkApplicative(App: APPLICATIVE_MIN): APPLICATIVE = struct
    type 'a applicative = 'a App.applicative

    structure FunctorMin = ApplicativeMinToFunctorMin(App)
    structure Functor = MkFunctor(FunctorMin)
    open App Functor

    fun af <*> aa = ap af aa
    fun aa *> ab = pure (fn _ => fn x => x) <*> aa <*> ab
    fun aa <* ab = pure (fn x => fn _ => x) <*> aa <*> ab
end

```

モナドも同じですね。ただ、 `MONAD_MIN` から `APPLICATIVE_MIN` を生成する必要があるので `MONAD_MIN` のシグネチャに `return` が増えてます。


``` standard-ml
signature MONAD_MIN = sig
    type 'a monad
    val return: 'a -> 'a monad
    val bind : 'a monad -> ('a -> 'b monad) -> 'b monad
end

signature MONAD = sig
    include APPLICATIVE MONAD_MIN
    sharing type applicative = monad
    val >>= : 'a monad * ('a -> 'b monad) -> 'b monad
    val >> : 'a monad * 'b monad -> 'b monad
end

functor MonadMinToApplicativeMin(M: MONAD_MIN): APPLICATIVE_MIN = struct
    open M
    type 'a applicative = 'a monad
    val pure = return
    fun ap mf ma =  bind mf (fn f =>
                    bind ma (fn a =>
                    return (f a)))
end

functor MkMonad(M: MONAD_MIN): MONAD = struct
    type 'a monad = 'a M.monad
    structure AppMin = MonadMinToApplicativeMin(M)
    structure App = MkApplicative(AppMin)
    open M App

    fun ma >>= f = bind ma f
    fun ma >> mb = ma >>= (fn _ => mb)

end
```

ちょっと `ap` の実装イケてないなぁと思いつつ先に進みます。


さて、使ってみましょう。今度はファンクタの適用は一回で済みます。


``` standard-ml
structure ListMonad = MkMonad(struct
                                   type 'a monad = 'a list
                                   fun return x = [x]
                                   fun bind ma f = List.concat (List.map f ma)
                              end)
local
    open ListMonad
in
    val ret = [1, 2, 3] >>= (fn x =>
              [4, 5, 6] >>= (fn y =>
              return (x + y)))
    fun println s = print (s ^ "\n")

    val () = List.app (println o Int.toString) ret
end
```

動きます。
ご覧の通り、ファンクタの適用は1回で済んでますし、親クラスのインスタンス全て自分で生成しているので実装の一貫性も保障出来ます。ダイアモンド継承が来ても安心ですね！

# 感想
私は最初のアプローチの方が好きですね（率直）。

それぞれのクラスのインスタンスを作るのは自然だと思いますし実はWorkshopのアプローチは表層上の問題でダイアモンド継承を解決出来ていません。名前衝突が起きます。モジュールの名前衝突ならまだ気合で( `val >>= = Monad.>>=` のように全て手で)解決出来ますが、シグネチャの方は解決策はなさそうです(いや、シグネチャを使わずに全て手で書き直すとかは可能ですが)。どうせ完全には解決出来てない問題のために実装が曲がるのは好みではないです。

# 付録A ダイアモンド継承に強そうなアプローチ

単純に親クラスのインクルードをやめます。んで親クラスは名前空間の汚染を防ぐためにモジュールに入ったまま受け取ることにします。

``` standard-ml
infix 4 <$> <$ $>  <*> <* *>
infix 1 >>= >>

signature FUNCTOR_MIN = sig
    type 'a t
    val fmap: ('a -> 'b) -> 'a t -> 'b t
end

signature FUNCTOR = sig
    include FUNCTOR_MIN
    val <$> : ('a -> 'b) * 'a t -> 'b t
    val <$ : 'a *  'b t ->  'a t
    val $> : 'a t *  'b  ->  'b t
    val void: 'a t -> unit t
end

functor MkFunctor(Fun: FUNCTOR_MIN): FUNCTOR = struct
    open Fun

    fun f <$> fa = fmap f fa
    fun a <$ fb =  (fn _ => a) <$> fb
    fun fa $> b = b <$ fa
    fun void fa = () <$ fa
end

signature APPLICATIVE_MIN = sig
    type 'a t
    val pure: 'a -> 'a t
    val ap : ('a -> 'b) t -> 'a t -> 'b t
end

signature APPLICATIVE = sig
    include APPLICATIVE_MIN

    val <*> : ('a -> 'b) t * 'a t -> 'b t
    val *> : 'a t * 'b t -> 'b t
    val <* : 'a t * 'b t -> 'a t
end

functor MkApplicative(App: sig
                          structure Functor: FUNCTOR
                          include APPLICATIVE_MIN
                          sharing type Functor.t = t
                         end): APPLICATIVE = struct
    open App
    type 'a t = 'a t

    fun af <*> aa = ap af aa
    fun aa *> ab = pure (fn _ => fn x => x) <*> aa <*> ab
    fun aa <* ab = pure (fn x => fn _ => x) <*> aa <*> ab
end


signature MONAD_MIN = sig
    type 'a t
    val return: 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
end

signature MONAD = sig
    include MONAD_MIN
    val >>= : 'a t * ('a -> 'b t) -> 'b t
    val >> : 'a t * 'b t -> 'b t
end


functor MkMonad(M: sig
                    structure Applicative: APPLICATIVE
                    include MONAD_MIN
                    sharing type Applicative.t = t
                end): MONAD = struct
    open M
    type 'a t = 'a t

    fun ma >>= f = bind ma f
    fun ma >> mb = ma >>= (fn _ => mb)

end


structure ListFunctor = MkFunctor(struct
                                       type 'a t = 'a list
                                       val fmap = List.map end)
structure ListApplicative = MkApplicative(struct
                                               structure Functor = ListFunctor
                                               type 'a t = 'a list
                                               fun pure x = [x]
                                               fun ap af aa = List.concat (List.map (fn a => List.map (fn f => f a) af) aa)
                                           end)
structure ListMonad = MkMonad(struct
                                   structure Applicative = ListApplicative
                                   type 'a t = 'a list
                                   fun return x = [x]
                                   fun bind ma f = List.concat (List.map f ma)
                               end)
local
    open ListFunctor
    open ListApplicative
    open ListMonad
in
    fun add x y = x + y
    val ret1 = [1, 2, 3] >>= (fn x =>
               [4, 5, 6] >>= (fn y =>
               return (x + y)))
    val ret2 = add <$> [1, 2, 3] <*> pure 1
    fun println s = print (s ^ "\n")

    val () = List.app (println o Int.toString) ret1
    val () = List.app (println o Int.toString) ret2
end
```

# 付録B do記法
ちょっと `>>=` が入り乱れるのはつらいですね。[PreML](https://github.com/br0ns/PreML)というSMLのプリプロセッサがあって、do記法（など）のシンタックスシュガーを提供してくれます。


``` standard-ml
val ret = do with ListMonad;
    x <- [1, 2, 3];
    y <- [4, 5, 6];
    return (x + y)
end
fun println s = print (s ^ "\n")

val () = List.app (println o Int.toString) ret
```

生成されるコードはこれです。

``` standard-ml
val ret = let infix 0 >>= val op>>= = ListMonad.>>= val return = ListMonad.return in ( 
         [1, 2, 3] ) >>= (fn  x => ( 
         [4, 5, 6] ) >>= (fn  y => 
    return (x + y) ) ) end 
```

マシになりましたね。
