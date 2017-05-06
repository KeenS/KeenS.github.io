---
categories: [Rust, デザインパターン]
date: 2017-05-06T21:59:30+09:00
title: Rust風にデザインパターン23種
---

κeenです。
GoFのデザインパターンは有名ですが、言語機能によっては単純化できたりあるいは不要だったりするのでRust風に書き換えたらどうなるか試してみます。
<!--more-->
発端はこのツイート。

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">デザインパターン、古いJavaの機能の足りなさのワークアラウンド的なテクニックも含まれてるからあまり宜しくないんだよね。enumやクロージャで十分なのもいくつかある。<br><br>Rustで写経、デザインパターン23種 - Qiita<a href="https://t.co/MhpS3Z2OlF">https://t.co/MhpS3Z2OlF</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/860635079600513024">2017年5月5日</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

一応誤解のないように説明しておくと、該当のQiitaの記事に不満がある訳ではなくてGoFのデザインパターンついての言及です。

リンク先のコードで十分な時にはここでは流すのでリンク先も同時に参照下さい。
また、比較しやすいようにサンプルコードはリンク先のものに則って書きます。

一応デザインパターンの教科書として結城先生の本を参照しつつ書いていきます。

# Command パターン

列挙型を使うところです。
Javaにまともなenumがないのでクラスが使われていますが、列挙型があるならそちらの方がデータとコードの分離ができて見通しがいいです。また、コマンドが1つの型に収まるのでトレイトオブジェクトを作る必要がなくなります。

比較的マイナーな変更です。

Lispだとクロージャで解決したりしますが、Rustだと列挙型の方がしっくりくるかなと思います。


``` rust
trait Command<T> {
    fn execute(&self, &mut T);
    fn undo(&self, &mut T);
}



struct Invoker<'a, Cmd, T: 'a> {
    commands: Vec<Cmd>,
    target: &'a mut T,
    current_index: usize,
}


impl<'a, Cmd, T> Invoker<'a, Cmd, T> {
    fn new(t: &'a mut T) -> Self {
        Invoker {
            commands: Vec::new(),
            target: t,
            current_index: 0,
        }
    }


    fn target(&self) -> &T {
        self.target
    }

    fn append_command(&mut self, c: Cmd) {
        self.commands.push(c);
    }
}

impl<'a, Cmd, T> Invoker<'a, Cmd, T>
    where Cmd: Command<T>
{
    fn execute_command(&mut self) {
        if self.commands.len() <= self.current_index {
            // Nothing to do.
            return;
        }

        let c = &self.commands[self.current_index];
        let t = &mut *self.target;
        c.execute(t);

        self.current_index += 1;
    }

    fn execute_all_commands(&mut self) {
        for _ in self.current_index..self.commands.len() {
            self.execute_command();
        }
    }

    fn undo(&mut self) {
        if 0 == self.current_index {
            return;
        }

        self.current_index -= 1;

        let c = &self.commands[self.current_index];
        let t = &mut *self.target;
        c.undo(t);
    }
}


#[derive(Debug, Eq, PartialEq)]
struct Robot {
    x: i32,
    y: i32,
    dx: i32,
    dy: i32,
}


impl Robot {
    fn new() -> Robot {
        Robot {
            x: 0,
            y: 0,
            dx: 0,
            dy: 1,
        }
    }

    fn move_forward(&mut self) {
        self.x += self.dx;
        self.y += self.dy;
    }

    fn set_direction(&mut self, d: (i32, i32)) {
        self.dx = d.0;
        self.dy = d.1;
    }

    fn get_direction(&self) -> (i32, i32) {
        (self.dx, self.dy)
    }
}

enum RoboCommand {
    MoveForward,
    TurnRight,
    TurnLeft,
}

impl Command<Robot> for RoboCommand {
    fn execute(&self, r: &mut Robot) {
        use RoboCommand::*;
        match *self {
            MoveForward => r.move_forward(),
            TurnRight => {
                let (dx, dy) = r.get_direction();
                r.set_direction((dy, -dx))
            }
            TurnLeft => {
                let (dx, dy) = r.get_direction();
                r.set_direction((-dy, dx));
            }

        }
    }
    fn undo(&self, r: &mut Robot) {
        use RoboCommand::*;
        match *self {
            MoveForward => {
                let c1 = TurnRight;
                c1.execute(r);
                c1.execute(r);
                self.execute(r);
                c1.execute(r);
                c1.execute(r);
            }
            TurnRight => {
                let c = TurnLeft;
                c.execute(r);
            }
            TurnLeft => {
                let c = TurnRight;
                c.execute(r);

            }

        }
    }
}



fn main() {
    let mut r = Robot::new();

    let mut invoker = Invoker::new(&mut r);
    assert_eq!(*invoker.target(),
               Robot {
                   x: 0,
                   y: 0,
                   dx: 0,
                   dy: 1,
               });

    {
        use RoboCommand::*;
        invoker.append_command(TurnRight);
        invoker.append_command(TurnLeft);
        invoker.append_command(MoveForward);
    }

    invoker.execute_all_commands();
    assert_eq!(*invoker.target(),
               Robot {
                   x: 0,
                   y: 1,
                   dx: 0,
                   dy: 1,
               });

    invoker.undo();
    assert_eq!(*invoker.target(),
               Robot {
                   x: 0,
                   y: 0,
                   dx: 0,
                   dy: 1,
               });

    invoker.undo();
    assert_eq!(*invoker.target(),
               Robot {
                   x: 0,
                   y: 0,
                   dx: 1,
                   dy: 0,
               });
}

```

# Stateパターン
参照先のままです。トレイトオブジェクトを使う代表的なケースだと思います。。
あるいは列挙型を使う可能性もあります。

# Strategyパターン

参照先でも説明されていますが、Rustにはクロージャがあるので不要です。

# Template Methodパターン

そもそもトレイトを使った普通のプログラミングなのでRustでわざわざ名前をつけるほどかな？と個人的には思いますがあえて書くなら参照先のままです。
あるいはものによっては高階関数でも。

個人的にはトレイトオブジェクトを作るより関連型を使った方が好みです。

``` rust
trait AbstractFactory<'a> {
    type ProdX: ProductX;
    type ProdY: ProductY;
    fn create_product_x(&self) -> Box<ProdX + 'a>;
    fn create_product_y(&self) -> Box<ProdY + 'a>;
}

// ...
```


# Mementoパターン
参照先のままです

# Observerパターン
参照先のままです

# Visitorパターン
参照先では簡単な例なので分かりづらいのですが、列挙型を使うところです。
まともな列挙型がない+シングルディスパッチしかなく引数はオーバーロードという二重苦によって生まれたパターンであり、まともな列挙型か多重ディスパッチがあれば複雑怪奇なプログラムは書かなくて済みます。

ここでは参照先とは違ってもう少し複雑な例を出します。

``` rust
trait Visitor<T> {
    fn visit(&mut self, &T);
}

enum Entity {
    File(String),
    Dir(String, Vec<Entity>),
}

struct ConcreteFileVisitor;

impl Visitor<Entity> for ConcreteFileVisitor {
    fn visit(&mut self, e: &Entity) {
        use Entity::*;
        match *e {
            File(ref name) => println!("file: {}", name),
            Dir(ref name, ref files) => {
                println!("dir: {}", name);
                for file in files {
                    self.visit(file)
                }
            }
        }
    }
}

fn main() {
    use Entity::*;
    let e = Dir("/".to_string(),
                vec![File("etc".to_string()), File("usr".to_string())]);
    let mut visitor = ConcreteFileVisitor;
    visitor.visit(&e);
}
```

特段パターンというほどの処理をしている感じがしませんね。

# Iteratorパターン
参照先のままです。

# Mediatorパターン
だいたい参照先のままです。複雑なことをしようと思うと`Colleague`が複数種類出てきて列挙型かトレイトオブジェクトが必要になりそうな気がします。

# Interpreterパターン
略

# Builderパターン
あまりここまで抽象化してるのは見たことありませんがやるとしたら参照先のままです。
そもそもRustには継承がないので抽象化する意義があまりなく、型固有のBuilderパターンで十分です。

# Prototypeパターン
参照先のままです。

# Factoryパターン
クロージャで十分です。

``` rust
trait Product {
    fn convert(&self, String) -> String;
}


struct Factory;

impl Factory {
    fn convert<P, F>(&self, s: String, create_product: F) -> String
        where P: Product,
              F: FnOnce() -> P
    {
        create_product().convert(s)
    }
}


struct ConcreteProductX;
impl Product for ConcreteProductX {
    fn convert(&self, s: String) -> String {
        s.to_uppercase()
    }
}


fn main() {
    let f = Factory;
    println!("{}",
             f.convert("hogehoge piyopiyo".to_string(), || ConcreteProductX))
}
```


# AbstractFactoryパターン
そもそもここまでやる？というのは置いといてやるとしたら参照先のままかなぁと思います。


TemplateMethodパターンでも述べた通り個人的には関連型を使う方が好みです。

# Chain of Responsibility/CoR パターン
参照先のままです。

# Singletonパターン
そもそもアンチパターンです。分かりづらいグローバル変数なだけです。あえてRustでやるとしたら[lazy_static](https://github.com/rust-lang-nursery/lazy-static.rs)かなと思います。

# Adapterパターン
これは捉え方が2種類あるかなーと思います。。

1つにはクラスの定義時にしかインターフェースを実装できない窮屈な言語仕様へのワークアラウンドとして。
この捉え方では参照先のコードようにただトレイトを実装してしまえば終わりです。

もう1つにはラッパーオブジェクトとして。`std::fs::File`の実装とかがそれっぽいと思います。。

``` rust
pub struct File {
    inner: fs_imp::File,
}
```


# Bridgeパターン
そもそも機能の追加とAPIの抽象化をどちらも継承に押し込める言語仕様が悪い。
それにRustでは継承しないので関係ないです。

# Proxyパターン
参照先のままです。

# Facadeパターン
参照先のままです。Rustにはモジュールや可視性の制御があって特別意識することなく普段からやっていることなのであまり名前をつけるほどのことではないと思ってます。

# Flyweightパターン
所有権があるのでRustだとちょっと難しいパターンです。
参照だけなら参照先のように`HashMap`にいれるか、オブジェクトを区別しないなら`Vec`にいれるかなと思います。

因みにLispとかでは`intern`という名前で呼ばれてると思います。

# Compositeパターン
ただの列挙型の再実装です。

# Decoratorパターン
よくあるやつです。参照先のコードの他、`std::io::WriteBuf`のようなものが代表的です。

# おわりに
デザインパターンをdisろうと思って書いたのですが案外多くのケースで便利でした。Rustで不要なものは10本指で数えられる程度でしたね。すごい。

因みにLisperでAIの研究者（確か今GoogleのAI研究所の所長）のPeter Norvigは[Design Patterns in Dynamic Languages](http://www.norvig.com/design-patterns/design-patterns.pdf)で16個はLispの機能を使えばパターンという程のものではなくなると言ってます。
それぞれどの機能でどれが不要になるかを解説しているのですが、Rustは高階関数とモジュールの分に加えて列挙型の分で不要になってるかなと思います。
