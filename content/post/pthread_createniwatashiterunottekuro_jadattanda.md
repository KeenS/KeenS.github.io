---
categories: ["小ネタ"]
date: 2019-02-17T12:31:45+09:00
title: "pthread_createに渡してるのってクロージャだったんだ"
---
κeenです。いわれてみれば当たり前なんですが最近気づいたこと
`pthread_create` に渡してるのって要するにクロージャなんですね。

<!--more-->

`pthread_create` は以下のようなシグネチャです。

```C
int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                          void *(*start_routine) (void *), void *arg);
```

ここの `void *arg` が `void *(*start_routine) (void *)` に引数として渡されます。
`void *` だと情報量がないですが、要はジェネリクスですよね。ML風に型を付けるとこうなるでしょうか。

``` ml
val pthread_create: pthread_t * pthread_attr_t * ('a -> ()) * 'a -> int
```


しかしどちらかというと `start_routine` が引数に受け取る型を知っていて、それに合わせてデータを渡すので存在型をつけたくなります。
引数内の存在型なのでML多相と同値ですし問題ないでしょう。


``` ml
exists 'a, ('a -> ()) * 'a
```

この形、どこかで見覚えがあります。 [Closure Conversion as CoYoneda](http://prl.ccs.neu.edu/blog/2017/08/28/closure-conversion-as-coyoneda/) に出てくるクロージャの型とそっくりですね。


\\\[
A \to B \cong \exists \Gamma.\Gamma \times (\Gamma \times A \to B)
\\\]

よく考えたら高級な言語(クロージャのある言語)だとspawnの引数に渡すクロージャは `() -> ()` なことが多いですね。

実装上の解釈をすると、クロージャは関数とそれが捕捉した環境の組で実装されることが多いです。その関数と環境を2つに分けて引数に渡していると考えれば納得できます。
なるほどー。
