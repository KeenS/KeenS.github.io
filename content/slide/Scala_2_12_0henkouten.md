---
categories: [Scala, ScalaMeetUp, 社内]
date: 2016-11-04T13:19:01+09:00
description: "
社内勉強会用
"
title: Scala 2.12.0変更点
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# Scala 2.12.0変更点
----------------------
サイバーエージェント ScalaMeetUp

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/icon.png) <!-- .element: style="position:absolute;right:0;z-index:-1" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * AI Studio Dev Group
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===

# サマリー
----------
全部[公式ページ](http://www.scala-lang.org/news/2.12.0)に詳細あるよ

* Java8向けの互換性向上したよ
  + トレイトがinterfaceに
  + 無名関数がlambdaに
  + SAMとScalaの関数が統合
* コンパイラが少し賢くなったよ
  + いくつかでinvokedynamicを使うように
  + 中間状態をやめた
  + 最適化をするように
* ライブラリ改善した
  + EitherがRight Biasedに
  + Futureが強化
* 非互換いくつか


===

# トレイトがinterfaceに
-----------------------

``` scala
trait Foo {
  def foo(): String = "foo"
  def bar(): String
}
```

===

# トレイトがinterfaceに
-----------------------


```
$ scalac-2.11 Trait.scala
$ ls
Foo$class.class  Foo.class  Trait.scala
$ javap Foo
Compiled from "Trait.scala"
public interface Foo {
  public abstract java.lang.String foo();
  public abstract java.lang.String bar();
}
$ javap 'Foo$class'
Compiled from "Trait.scala"
public abstract class Foo$class {
  public static java.lang.String foo(Foo);
  public static void $init$(Foo);
}

```

===

# トレイトがinterfaceに
-----------------------

```
$ scalac-2.12 Trait.scala
$ ls
Foo.class  Trait.scala
$ javap Foo
Compiled from "Trait.scala"
public interface Foo {
  public static java.lang.String foo$(Foo);
  public java.lang.String foo();
  public abstract java.lang.String bar();
  public static void $init$(Foo);

```

===

# 無名関数がlambdaに
-----------------------

```
class Foo {
  val f = () => "string"
}
```

===

# 無名関数がlambdaに
-----------------------


```
$ scalac-2.11 Trait.scala
$ ls
Foo$$anonfun$1.class  Foo.class  Trait.scala
$ javap Foo
Compiled from "Trait.scala"
public class Foo {
  public scala.Function0<java.lang.String> f();
  public Foo();
}
$ javap  'Foo$$anonfun$1
Compiled from "Trait.scala"
public final class Foo$$anonfun$1 extends scala.runtime.AbstractFunction0<java.lang.String> implements scala.Serializable {
  public static final long serialVersionUID;
  public final java.lang.String apply();
  public final java.lang.Object apply();
  public Foo$$anonfun$1(Foo);
}

```

===

# 無名関数がlambdaに
-----------------------

```
$ scalac-2.12 Trait.scala
$ ls
Foo.class  Trait.scala
$ javap -p Foo
Compiled from "Trait.scala"
public class Foo {
  private final scala.Function0<java.lang.String> f;
  public scala.Function0<java.lang.String> f();
  public static final java.lang.String $anonfun$f$1();
  public Foo();
  private static java.lang.Object $deserializeLambda$(java.lang.invoke.SerializedLambda);
$ javap -c -p Foo
Compiled from "Trait.scala"
public class Foo {
  private final scala.Function0<java.lang.String> f;

  public scala.Function0<java.lang.String> f();
    Code:
       0: aload_0
       1: getfield      #19                 // Field f:Lscala/Function0;
       4: areturn

  public static final java.lang.String $anonfun$f$1();
    Code:
       0: ldc           #25                 // String string
       2: areturn

  public Foo();
    Code:
       0: aload_0
       1: invokespecial #29                 // Method java/lang/Object."<init>":()V
       4: aload_0
       5: invokedynamic #49,  0             // InvokeDynamic #0:apply:()Lscala/Function0;
      10: putfield      #19                 // Field f:Lscala/Function0;
      13: return

  private static java.lang.Object $deserializeLambda$(java.lang.invoke.SerializedLambda);
    Code:
       0: aload_0
       1: invokedynamic #61,  0             // InvokeDynamic #1:lambdaDeserialize:(Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;
       6: areturn
}
```

===

# SAMとScalaの関数が統合
-----------------------

関数をSingle Abstract Methodに出来る

```
scala> val runRunnable: Runnable = () => println("Run!")
runRunnable: Runnable = $$Lambda$1073/754978432@7cf283e1
scala> runRunnable.run()
Run!
```

JavaでLambdaが期待される箇所にScalaの無名関数を書ける

===

# SAMとScalaの関数が統合
-----------------------

逆に、JavaのLambdaでScalaの関数を作れる

```
public class A {
  scala.Function1<String, String> f = s -> s.trim();
}
```

===

# 最適化
--------
とりあえずDead Code Elimination

```
class Foo {
  def add1(i: Int) = {
    val unused = 1 + i
    1 + i
  }

}

```

===

# 最適化
--------

```
$ scalac Trait.java
$ javap -c Foo
...
  public int add1(int);
    Code:
       0: iconst_1
       1: iload_1
       2: iadd
       3: istore_2
       4: iconst_1
       5: iload_1
       6: iadd
       7: ireturn
...
}

```

===

# 最適化
--------

```
$ scalac -opt:l:method Trait.java
$ javap -c Foo
...
  public int add1(int);
    Code:
       0: iconst_1
       1: iload_1
       2: iadd
       3: ireturn
...
```

===

# 非互換
--------

* Object initialization locks and lambdas
  + 無名関数が元のクラスのメソッドになったので変にデッドロックするかも
* Lambdas capturing outer instances
  + 無名関数が元のクラスのメソッドになったのでスコープが少し変わる
  + シリアライズに影響出るかも
* SAM conversion precedes implicits
  + SAM conversionとimplicit comversionだとSAMが優先される

===

# 非互換
--------

* SAM conversion in overloading resolution
  + なんか型が面倒になった
* Inferred types for fields
  + なんかよく分かんなかった
* Changed syntax trees (affects macro and compiler plugin authors)
  + 変わったらしい

</textarea>
