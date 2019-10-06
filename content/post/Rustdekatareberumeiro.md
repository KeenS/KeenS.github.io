---
categories: ["Rust", "型"]
date: 2019-10-06T21:55:42+09:00
title: "Rustで型レベル迷路"
---

κeenです。型レベル迷路作ったよ！コンパイルが通ったらゴール！！

<!--more-->

Rustで型レベルステートマシン定義できるじゃないですか [^1]。
ということは1マス単位で移動できる方向を使い分けられるし迷路くらい作れるんじゃね？というお話。

[^1]: blog/2018/12/15/rustdetsuyomenikatawotsukerupart_2__type_level_state_machine/


サクっといきます。

マス型。

``` rust
use std::marker::PhantomData;

pub struct Cell<Maze, X, Y> {
    m: PhantomData<Maze>,
    x: PhantomData<X>,
    y: PhantomData<Y>,
}

impl<Maze, X, Y> Cell<Maze, X, Y> {
    const fn new() -> Self {
        Self {
            m: PhantomData,
            x: PhantomData,
            y: PhantomData,
        }
    }
}
```

迷路間で定義を使い回したいので迷路もパラメータに持ってます。
また、任意のマスが作られては困るのでフィールドも `new` もプライベートです。
あと `static` 変数にバインドしたい関係で `const` にしてます。


次、自然数型。

``` rust
pub trait Nat {
    fn to_n() -> u32;
}

pub struct Z;
pub struct S<N>(N);

type _0 = Z;
type _1 = S<_0>;
type _2 = S<_1>;

impl Nat for Z {
    fn to_n() -> u32 {
        0
    }
}
impl<N: Nat> Nat for S<N> {
    fn to_n() -> u32 {
        N::to_n() + 1
    }
}
```

ついでにマスを座標でプリントできるようにする。

``` rust
impl<Maze, X, Y> Cell<Maze, X, Y>
where
    X: Nat,
    Y: Nat,
{
    fn to_point() -> (u32, u32) {
        (X::to_n(), Y::to_n())
    }
}
```


ここまできたらあとは迷路を作るだけ。こういう迷路を作りましょう。

``` text
  0   1
+   +---+
| g     |  0
+---+   +
|   | s |  1
+---+   +

```


すなわち、

* (1, 1)がスタートである
* (1, 1)から(0, 1)へ移動できる
* (0, 1)から(1, 1)へ移動できる
* (0, 1)から(0, 0)へ移動できる
* (0, 0)から(0, 1)へ移動できる
* (0, 0)はゴールである

とします。

素直にステートマシンで実装しましょう。

``` rust
pub mod maze1 {
    use super::{_0, _1};

    pub struct Maze1;
    pub type Cell<X, Y> = super::Cell<Maze1, X, Y>;

    impl Cell<_1, _1> {
        pub fn up(self) -> Cell<_1, _0> {
            println!("{:?}", Self::to_point());
            Cell::new()
        }
    }

    impl Cell<_1, _0> {
        pub fn left(self) -> Cell<_0, _0> {
            println!("{:?}", Self::to_point());
            Cell::new()
        }
    }
    impl Cell<_1, _0> {
        pub fn down(self) -> Cell<_1, _1> {
            println!("{:?}", Self::to_point());
            Cell::new()
        }
    }

    impl Cell<_0, _0> {
        pub fn down(self) -> Cell<_0, _1> {
            println!("{:?}", Self::to_point());
            Cell::new()
        }
    }

    pub type Start = Cell<_1, _1>;
    pub type Goal = Cell<_0, _0>;
    pub const START: Start = Cell::new();
}

```

モジュール内で `Maze1` と `Cell<X, Y>` を定義しているので、内部では自然にステートマシンを定義できます。
各移動メソッドは `self` を消費しているので前の状態が消えるようになっていて、ちゃんとステートマシンになってますね。
そして表示用に `println!` を挟んでます。これはちょっと汚ないので経路は別に返せるようにしたいですね。

さてさて、この迷路を遊ぶには以下のような関数を定義します。


``` rust
fn solve_maze1(start: maze1::Start) -> maze1::Goal {
    // 迷路の回答
}
```

迷路のステートマシンを巧みに操ってゴールまでいけたらコンパイルが通るので迷路が解けました。

あとはこのように `main` を書いてあげると経路が表示されます。

``` rust
fn main() {
    println!("starting from {:?}", maze1::Start::to_point());
    let _goal = solve_maze1(maze1::START);
    println!("goal!");
}
```



``` console
starting from (1, 1)
(1, 0)
(0, 0)
goal!
```

やったね！

# 迷路を生成する

これで迷路を遊べるようにはなりましたが、ちょっと定義が面倒ですね。
大体機械的に生成できるのでマクロでやっつけてしまいましょう。
マクロは長くなるので付録に回しますが、以下のように迷路を定義できるようになります。

``` rust
mod maze2 {
    maze! {
        Maze2,
        [g .],
        [# s]
    }
}

```

これも先程と同じように解けます。

``` rust
fn solve_maze2(start: maze2::Start) -> maze2::Goal {
    start.up().left()
}
```


これで大規模迷路もできますね！
例えばこういうものを置いてておきます。

``` rust
mod maze3 {
    maze! {
        Maze3,
        [. . . . . . . . . # . . g],
        [. # # # . # # # # # . # .],
        [. # . # . . . . . . . # .],
        [. # . # # # # # # # . # .],
        [. . . # . . . . . # . # .],
        [# # # # . # . # . # # # .],
        [. . . . . # . # . . . . .],
        [. # # # # # . # # # # # #],
        [. . . . . # . # . . . . .],
        [. # # # . # . # . # # # .],
        [. # . . . # . # . . . # .],
        [# # . # # # . # # # # # .],
        [s . . # . . . . . . . . .]
    }
}
```

是非チャレンジしてみて下さい。

今回のコードは[こちら](https://gitlab.com/blackenedgold/typelevel-maze)。

# 付録A. マクロ

``` rust
macro_rules! gen_cell {
    ($ty: ty, . , ($upty: ty, $up:tt), ($rightty: ty,  $right:tt), ($downty:ty, $down:tt), ($leftty:ty, $left: tt)) => {
        gen_cell! {$ty, ($upty, $up), ($rightty, $right), ($downty, $down), ($leftty, $left)}
    };
    ($ty: ty, s , ($upty: ty, $up:tt), ($rightty: ty,  $right:tt), ($downty:ty, $down:tt), ($leftty:ty, $left: tt)) => {
        pub type Start = $ty;
        pub const START: $ty = Cell::new();
        gen_cell! {$ty, ($upty, $up), ($rightty, $right), ($downty, $down), ($leftty, $left)}
    };
    ($ty: ty, g , ($upty: ty, $up:tt), ($rightty: ty,  $right:tt), ($downty:ty, $down:tt), ($leftty:ty, $left: tt)) => {
        pub type Goal = $ty;
        gen_cell! {$ty, ($upty, $up), ($rightty, $right), ($downty, $down), ($leftty, $left)}
    };

    ($ty: ty, # , ($upty: ty, $up:tt), ($rightty: ty,  $right:tt), ($downty:ty, $down:tt), ($leftty:ty, $left: tt)) => {};
    ($ty: ty, ($upty: ty, g), ($rightty: ty,  $right:tt), ($downty:ty, $down:tt), ($leftty:ty, $left: tt)) => {
        gen_cell! {$ty, ($upty, .), ($rightty, $right), ($downty, $down), ($leftty, $left)}
    };
    ($ty: ty, ($upty: ty, $up:tt), ($rightty: ty,  g), ($downty:ty, $down:tt), ($leftty:ty, $left: tt)) => {
        gen_cell! {$ty, ($upty, $up), ($rightty, .), ($downty, $down), ($leftty, $left)}
    };
    ($ty: ty, ($upty: ty, $up:tt), ($rightty: ty,  $right:tt), ($downty:ty, g), ($leftty:ty, $left: tt)) => {
        gen_cell! {$ty, ($upty, $up), ($rightty, .), ($downty, .), ($leftty, $left)}
    };
    ($ty: ty, ($upty: ty, $up:tt), ($rightty: ty,  $right:tt), ($downty:ty, $down:tt), ($leftty:ty, g)) => {
        gen_cell! {$ty, ($upty, $up), ($rightty, .), ($downty, $down), ($leftty, .)}
    };

    ($ty: ty, ($upty: ty, .), ($rightty: ty,  $right:tt), ($downty:ty, $down:tt), ($leftty:ty, $left: tt)) => {
        impl $ty {
            #[allow(dead_code)]
            pub fn up(self) -> $upty {
                println!("{:?}", <$upty>::to_point());
                <$upty>::new()
            }
        }
        gen_cell! {$ty, ($upty, #), ($rightty, $right), ($downty, $down), ($leftty, $left)}
    };

    ($ty: ty, ($upty: ty, $up:tt), ($rightty: ty, .), ($downty:ty, $down:tt), ($leftty:ty, $left: tt)) => {
        impl $ty {
            #[allow(dead_code)]
            pub fn right(self) -> $rightty {
                println!("{:?}", <$rightty>::to_point());
                <$rightty>::new()
            }
        }
        gen_cell! {$ty, ($upty, #), ($rightty, #), ($downty, $down), ($leftty, $left)}
    };

    ($ty: ty, ($upty: ty, $up:tt), ($rightty: ty, $right:tt), ($downty:ty, .), ($leftty:ty, $left: tt)) => {
        impl $ty {
            #[allow(dead_code)]
            pub fn down(self) -> $downty {
                println!("{:?}", <$downty>::to_point());
                <$downty>::new()
            }
        }
        gen_cell! {$ty, ($upty, #), ($rightty, #), ($downty, #), ($leftty, $left)}
    };

    ($ty: ty, ($upty: ty, $up:tt), ($rightty: ty, $right:tt), ($downty:ty, $down:tt), ($leftty:ty, .)) => {
        impl $ty {
            #[allow(dead_code)]
            pub fn left(self) -> $leftty {
                println!("{:?}", <$leftty>::to_point());
                <$leftty>::new()
            }
        }
        gen_cell! {$ty, ($upty, #), ($rightty, #), ($downty, #), ($leftty, #)}
    };

    ($ty: ty, ($upty: ty, $up:tt), ($rightty: ty, $right:tt), ($downty:ty, $down:tt), ($leftty:ty, $left:tt)) => {};
}

macro_rules! gen_maze_row {
    ($x: ty, $y: ty, [$up_left:tt $up:tt $($up_rest:tt)*], [$left:tt $cell:tt $right:tt $($rest:tt)*], [$down_left:tt $down:tt $($down_rest:tt)*]) => {
        gen_cell! {Cell<$crate::S<$x>, $crate::S<$y>>, $cell, (Cell<$crate::S<$x>, $y>, $up), (Cell<$crate::S<$crate::S<$x>>, $crate::S<$y>>, $right), (Cell<$crate::S<$x>, $crate::S<$crate::S<$y>>>, $down), (Cell<$x, $crate::S<$y>>, $left)}
        gen_maze_row! {$crate::S<$x>, $y, [$up $($up_rest)*], [$cell $right $($rest)*], [$down $($down_rest)*]}
    };
    ($x: ty, $y: ty, [$up_left:tt $up:tt], [$left:tt $cell:tt], [$down_left:tt $down:tt]) => {
        gen_cell! {Cell<$crate::S<$x>, $crate::S<$y>>, $cell, (Cell<$crate::S<$x>, $y>, $up), (Cell<$crate::S<$crate::S<$x>>, $crate::S<$y>>, #), (Cell<$crate::S<$x>, $crate::S<$crate::S<$y>>>, $down), (Cell<$x, $crate::S<$y>>, $left)}
    };
    ($x: ty, $y: ty, [$up_left:tt $up:tt $($up_rest:tt)*], [$left:tt $cell:tt $right:tt $($rest:tt)*], @bottom) => {
        gen_cell! {Cell<$crate::S<$x>, $crate::S<$y>>, $cell, (Cell<$crate::S<$x>, $y>, $up), (Cell<$crate::S<$crate::S<$x>>, $crate::S<$y>>, $right), (Cell<$crate::S<$x>, $crate::S<$crate::S<$y>>>, #), (Cell<$x, $crate::S<$y>>, $left)}
        gen_maze_row! {$crate::S<$x>, $y, [$up $($up_rest)*] , [ $cell $right $($rest)* ], @bottom }
    };
    ($x: ty, $y: ty, [$up_left:tt $up:tt], [$left:tt $cell:tt], @bottom) => {
        gen_cell! {Cell<$crate::S<$x>, $crate::S<$y>>, $cell, (Cell<$crate::S<$x>, $y>, $up), (Cell<$crate::S<$crate::S<$x>>, $crate::S<$y>>, #), (Cell<$crate::S<$x>, $crate::S<$crate::S<$y>>>, #), (Cell<$x, $crate::S<$y>>, $left)}
    };
    ($x: ty, $y: ty, @top, [$left:tt $cell:tt $right:tt $($rest:tt)*], [$down_left:tt $down:tt $($down_rest:tt)*]) => {
        gen_cell! {Cell<$crate::S<$x>, $crate::S<$y>>, $cell, (Cell<$crate::S<$x>, $y>, #), (Cell<$crate::S<$crate::S<$x>>, $crate::S<$y>>, $right), (Cell<$crate::S<$x>, $crate::S<$crate::S<$y>>>, $down), (Cell<$x, $crate::S<$y>>, $left)}
        gen_maze_row! {$crate::S<$x>, $y, @top, [$cell $right $($rest)*], [$down $($down_rest)*]}
    };
    ($x: ty, $y: ty, @top, [$left:tt $cell:tt], [$down_left:tt $down:tt]) => {
        gen_cell! {Cell<$crate::S<$x>, $crate::S<$y>>, $cell, (Cell<$crate::S<$x>, $y>, #), (Cell<$crate::S<$crate::S<$x>>, $crate::S<$y>>, #), (Cell<$crate::S<$x>, $crate::S<$crate::S<$y>>>, $down), (Cell<$x, $crate::S<$y>>, $left)}
    };
    ($x: ty, $y: ty, @top, [$left:tt $cell:tt $right:tt $($rest:tt)*], @bottom) => {
        gen_cell! {Cell<$crate::S<$x>, $crate::S<$y>>, $cell, (Cell<$crate::S<$x>, $y>, #), (Cell<$crate::S<$crate::S<$x>>, $crate::S<$y>>, $right), (Cell<$crate::S<$x>, $crate::S<$crate::S<$y>>>, #), (Cell<$x, $crate::S<$y>>, $left)}
        gen_maze_row! {$crate::S<$x>, $y, @top, [$cell $right $($rest)*], @bottom}
    };
    ($x: ty, $y: ty, @top, [$left:tt $cell:tt], @bottom) => {
        gen_cell! {Cell<$crate::S<$x>, $crate::S<$y>>, $cell, (Cell<$crate::S<$x>, $y>, #), (Cell<$crate::S<$crate::S<$x>>, $crate::S<$y>>, #), (Cell<$crate::S<$x>, $crate::S<$crate::S<$y>>>, #), (Cell<$x, $crate::S<$y>>, $left)}
    };
}

macro_rules! gen_maze {
    ($x: ty, $y: ty,  @top, [$($row:tt)*], [$($down:tt)*] $(,[$($rest:tt)*])*, @bottom) => {
        gen_maze_row!{$x, $y, @top, [$($row)*], [$($down)*]}
        gen_maze!{$x, $crate::S<$y>, [$($row)*], [$($down)*] $(,[$($rest)*])*, @bottom}
    };

    ($x: ty, $y: ty,  [$($up:tt)*], [$($row:tt)*], [$($down:tt)*] $(,[$($rest:tt)*])*, @bottom) => {
        gen_maze_row!{$x, $y, [$($up)*], [$($row)*], [$($down)*]}
        gen_maze!{$x, $crate::S<$y>, [$($row)*], [$($down)*] $(,[$($rest)*])*, @bottom}
    };
    ($x: ty, $y: ty,  [$($up:tt)*], [$($row:tt)*],  @bottom) => {
        gen_maze_row!{$x, $y, [$($up)*], [$($row)*], @bottom}
    };

    ($x: ty, $y: ty,  @top, [$($row:tt)*],  @bottom) => {
        gen_maze_row!{$x, $y, @top, [$($row)*], @bottom}
    };
}

macro_rules! maze {
    ($name: ident, $([$($row:tt)*]),*) => {
        pub struct $name;
        pub type Cell<X, Y> = $crate::Cell<$name, X, Y>;
        gen_maze! {$crate::Z, $crate::Z, @top $(, [# $($row)*])*, @bottom}
    };
}

```
