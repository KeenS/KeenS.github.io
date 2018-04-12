---
categories: [rust, rayon, アルゴリズム]
date: 2018-04-08T22:50:43+09:00
title: "rayonの真価は分割統治にアリ"
---

κeenです。やや釣りっぽいタイトルですがRustのデータ並列ライブラリの[rayon](https://github.com/rayon-rs/rayon)について。イテレータを並列に計算できるだけでなく`join`で自分で並列処理を書くこともできるんだよという記事です。

<!--more-->

# rayonと`par_iter`
rayonはRustのデータ並列ライブラリです。あの化学繊維の[レーヨン](https://ja.wikipedia.org/wiki/%E3%83%AC%E3%83%BC%E3%83%A8%E3%83%B3)です。Thread(糸)にちなんだ名前なんですかね。

rayonで一番良く使われるのは`par_iter`でしょう。このように使えます。

``` rust
use rayon::prelude::*;
let vec = vec![1, 2, 3, 4, 5, 6];
// _par_iter でデータ並列計算
let max = vec.par_iter().max();
assert_eq!(max, Some(6));
```

このようにほぼRustのイテレータを置き換える形で使えます。

その反面、イテレータらしい処理でないと`par_iter`は使えません。
たとえば次のminとmaxを同時に求めるアルゴリズムはどうでしょう。
要素を同時に2つ取って、大小の区別をつけてからmin、maxと比較するのでminとmaxを個別に求めるのに比べて比較回数が3/4くらいになります。
しかし同時に2つ取るのでidiomaticなイテレータからは外れてしまいます。


``` rust
fn main() {
    let vec = vec![1, 2, 3, 4, 5, 6];
    let (min, max) = min_max(&vec);
    assert_eq!(min, Some(&1));
    assert_eq!(max, Some(&6));
}

fn min_max<T: Ord>(v: &[T]) -> (Option<&T>, Option<&T>) {
    if v.is_empty() {
        return (None, None);
    } else {
        let (min, max) = min_max_(v);
        (Some(min), Some(max))
    }
}

fn min_max_<T: Ord>(v: &[T]) -> (&T, &T) {
    use std::cmp;
    debug_assert!(0 < v.len());
    let mut iter = v.iter();
    let mut min;
    let mut max;
    if v.len() % 2 == 0 {
        min = iter.next().unwrap();
        max = iter.next().unwrap();
    } else {
        min = iter.next().unwrap();
        max = min;
    }
    while let Some(a) = iter.next() {
        let b = iter.next().unwrap();
        let (small, large) = if a < b { (a, b) } else { (a, b) };
        min = cmp::min(min, small);
        max = cmp::max(max, large);
    }
    (min, max)
}
```

しかしこのアルゴリズムはrayonのイテレータでは記述出来ないでしょう。
こういうときにデータ並列操作を自分で書けるのが`join`です。

# `join`
`par_iter`が高レベルAPIなのに対して[`join`](https://docs.rs/rayon/1.0.1/rayon/fn.join.html)はカスタムjobを書くためのAPIとされています。以下のような型シグネチャを持つ関数です。

```pub
pub fn join<A, B, RA, RB>(oper_a: A, oper_b: B) -> (RA, RB) where
    A: FnOnce() -> RA + Send,
    B: FnOnce() -> RB + Send,
    RA: Send,
    RB: Send,
```

少しややこしいですが、普通のスレッドの`spawn`のように新たなタスクを始める関数です。ただし引数に処理関数を2つ取ります。このシグネチャから「タスクを半分に分割しなさい。そうすればそれぞれを並列に解くことが出来ます。」というメッセージが伝わりますね。

今回のmin_maxの例でいくとまずスライスを半分に分割し、それぞれでmin_maxを求めたあとでmin同士、max同士を比べるとよさそうです。こうなるでしょうか。

``` rust
fn main() {
    let vec = vec![1, 2, 3, 4, 5, 6];
    let (min, max) = min_max_rayon(&vec);
    assert_eq!(min, Some(&1));
    assert_eq!(max, Some(&6));
}

fn min_max_rayon<T: Ord + Send + Sync>(v: &[T]) -> (Option<&T>, Option<&T>) {
    match v.len() {
        0 => (None, None),
        1 => (Some(&v[0]), Some(&v[0])),
        _ => {
            let (min, max) = min_max_rayon_(v);
            (Some(min), Some(max))
        }
    }
}
fn min_max_rayon_<T: Ord + Send + Sync>(v: &[T]) -> (&T, &T) {
    use std::cmp;
    debug_assert!(1 < v.len());

    match v.len() {
        2 => {
            let a = &v[0];
            let b = &v[1];
            if a < b {
                (a, b)
            } else {
                (b, a)
            }
        }
        _ => {
            fn doit<T: Ord + Send + Sync>(v: &[T]) -> (&T, &T) {
                let mid = match v.len() % 4 {
                    0 => v.len() / 2,
                    2 => v.len() / 2 + 2,
                    _ => unreachable!(),
                };
                let ((min1, max1), (min2, max2)) =
                    // ここで `rayon::join` を用いている
                    rayon::join(|| min_max_rayon_(&v[..mid]), || min_max_rayon_(&v[mid..]));
                (cmp::min(min1, min2), cmp::max(max1, max2))
            }
            if v.len() % 2 == 1 {
                let t = &v[0];
                let v = &v[1..];
                let (min, max) = doit(v);
                (cmp::min(t, min), cmp::max(t, max))
            } else {
                doit(v)
            }
        }
    }
}
```

コメントに書きましたが `rayon::join(|| min_max_rayon_(&v[..mid]), || min_max_rayon_(&v[mid..]));` が今回の核心です。ほとんど並列を意識させずにコードを書けていますね。
事実 `(min_max_rayon_(&v[..mid]), min_max_rayon_(&v[mid..]))` のようにただのタプルにしても結果は変わりません。

# joinと再帰とWork Stealing
`rayon::join(|| min_max_rayon_(&v[..mid]), || min_max_rayon_(&v[mid..]));`をよく見ると再帰呼び出ししていますね。すると再帰先でもまた`join`を呼び、今回の基底ケースは`v.len() == 2`なので 100万要素のスライスに対して50万個のタスクができることになります。
素直に50万個のスレッドを作るわけにもいきませんしシンプルなジョブキューを作っても50万回の排他ロックはとてつもなく遅いでしょう


``` rust
// 1タスクスレッドは重すぎる
[Thread][Thread][Thread][Thread][Thread]
[Thread][Thread][Thread][Thread][Thread]
[Thread][Thread][Thread][Thread][Thread]
[Thread][Thread][Thread][Thread][Thread]
....
```

``` rust
// キューでもロックが遅い

                 |---|',
                 |   |  |
                 |---|  |
                 | . |  50万
                 | . |  |
                 |---|  |
                 |   |,'
                 +---+
                 >lock<
                   .
               .   .   .
            .    . .  .   .
         .     .   .   .     .
      .      .     .     .      .
   .       .       .       .       .
[Thread][Thread][Thread][Thread][Thread]

```

rayonが採用しているのはWork Stealingです。他の言語でも使われているのでご存知の方も多いでしょう。
私は[元論文](http://supertech.csail.mit.edu/papers/ft_gateway.pdf)は読んだことはないのですが興味のある方は目を通してみて下さい。ここではスピリチュアルに説明します。
まず、各スレッドが各自のDequeを持っています。

``` rust
 |   |   |   |   |   |   |   |
 +---+   +---+   +---+   +---+
 |   |   |   |   |   |   |   |
 +---+   +---+   +---+   +---+
 |   |   |   |   |   |   |   |
 +---+   +---+   +---+   +---+
   |       |       |       |
[Thread][Thread][Thread][Thread]
```

このうちの1つのスレッドの動きに着目しましょう。
最初は大きなJobがいます。

``` rust
 +---+
 |100|
 +---+
   |
[Thread]

```

このタスクを実行すると先程のmin_maxのように`join`を使うと半分に分割されますね。
それは1つは即座に実行、もう一つはDequeにpush_frontされます。

``` rust
 +---+
 |50 |
 +---+
   |
 +---+
 |50 |
 +---+
[Thread]

```

これを何度か繰り返します。再帰しているので小さなタスクが手前にきます。

``` rust
 +---+
 |50 |
 +---+
 |26 |
 +---+
 |12 |
 +---+
 |6  |
 +---+
   |
 +---+
 |6  |
 +---+
[Thread]

```


ここまでは全て自分のスレッド内での作業なのでロックは必要ありません。
ここで何か他のスレッドが全てのタスクを終えて空いているとしましょう


``` rust
 +---+
 |50 |
 +---+
 |26 |
 +---+
 |12 |
 +---+
 |6  |
 +---+
   |
 +---+     |   |
 |6  |     +---+
 +---+       |
[Thread1] [Thread2]

```

するとThread2はThread1からJobを奪います。奪うのは一番後ろ、`50`です。これは他のたとえばThread3も同じように奪いに来る可能性もあるのでロックを取ります。

``` rust
 >lock< -----+
 +---+       |
 |26 |       |
 +---+       |
 |12 |       |
 +---+       |
 |6  |       |
 +---+       v
   |       +---+
 +---+     |50 |
 |6  |     +---+
 +---+       |
[Thread1] [Thread2]

```

これでロックを出来る限り少なく、一番大きなタスクを他のスレッドに渡すことができました。
このようにrayonは`rayon::join`と再帰呼出しに向いたスケジューリングを採用しています。


# `join` と `scope`
rayonにはカスタムjobのためのAPIとして`join`の他に [`scope`](https://docs.rs/rayon/1.0.1/rayon/fn.scope.html)も用意されています。こちらは2つとは限らずに好き勝手タスクを`spawn`できるので自由度が高いです。なんなら`scope`を用いて`join`を実装することも可能です。

しかし可能な限り`join`の方を使えとあります。`join`の方がWork Stealingに併せたAPIなのでこっちの方が内部実装が高速だとのこと。
上記の通り恐らくrayonはWork Stealingを前提に作られているので`join`が本体で`scope`は副産物なのでしょう。

# ベンチマーク
さて、並列というものはby definitionで速くないといけません。ベンチマークをとりましょう。

nightlyの機能のベンチマークを使います。また、適当な配列を用意するのに[`rand`](https://github.com/rust-lang-nursery/rand)を使い、このような関数で実装します。

``` rust
extern crate rand;
fn random_vec(n: usize) -> Vec<i32> {
    let mut rng = rand::thread_rng();
    (0..n).into_iter().map(|_| rng.gen::<i32>()).collect()
}
```

これを用いて以下のようにベンチマークをとります。実行するのは4コア8スレッドのIntel(R) Core(TM) i7-4910MQ CPU @ 2.90GHzです。

``` rust
#[bench]
fn bench_min_max(b: &mut Bencher) {
    let v = random_vec(1024 * 1024 * 32);
    b.iter(|| min_max(&v))
}

#[bench]
fn bench_min_max_rayon(b: &mut Bencher) {
    let v = random_vec(1024 * 1024 * 32);
    b.iter(|| min_max_rayon(&v))
}

```
結果は

``` rust
$ cargo +nightly bench
test bench_min_max           ... bench:   2,001,679 ns/iter (+/- 236,508)
test bench_min_max_rayon     ... bench:   5,608,823 ns/iter (+/- 1,225,619)
```

なんとrayonを使ったほうが遅いです。

詳細なグラフはこちら。

<canvas id="vanilla_vs_rayon"></canvas>



これは第一にrayon化するだけでどうやらオーバーヘッドが乗るから。もう一つに、いくらwork stealingとはいえ毎度jobを作っているとバカにならないコストが発生するからです。
なのである程度のサイズ以下になったら`min_max_rayon`にスイッチするとよいでしょう。

`min_max_rayon_`の

``` rust
    match v.len() {
        2 => {
            let a = &v[0];
            let b = &v[1];
            if a < b {
                (a, b)
            } else {
                (b, a)
            }
        }
        _ => {

```

の部分を

``` rust
    if v.len() <= 8192 {
        min_max_(v)
    } else {

```

に書換えます。この8192というのは私の手元のベンチマークに合わせてチューニングした結果です。
どうしてもrayonに載せるだけでそれなりにオーバーヘッドがかかるので並列化がペイするためにはそれなりに大きくないといけなくなります。

こうするとぐっと速くなって

``` rust
test bench_min_max           ... bench:   1,982,283 ns/iter (+/- 124,381)
test bench_min_max_rayon     ... bench:     412,036 ns/iter (+/- 89,327)
```

と大体(/ 1982283.0 412036.0) = 4.9倍速くなってますね。

<canvas id="vanilla_vs_rayon_tuned"></canvas>


# `join` と分割統治

`join`の素晴らしい点は思考のフレームワークにもなっている点ですね。
小さな問題に分割して解いて、その解を組み合わせて全体の問題を解く。分割統治って言うらしいです。
クイックソートやマージソートがよく知られる例ですね。私はアルゴリズムに詳しいわけでは無いので細かな点は近日中にネットに出没するらしい詳しい記事に譲るとします。

たとえば最大上位者数問題、つまり全ての要素の中での「自身より右側にあって自身より大きい要素の数(上位者数)」の最大値を求める問題は素朴にはこう書けます。

``` rust
fn msc<T: PartialOrd + Send + Sync>(v: &[T]) -> usize {
    v.iter()
        .enumerate()
        .map(|(n, t)| scount(t, &v[n..]))
        .max()
        .unwrap()
}

fn scount<T: PartialOrd + Send + Sync>(t: &T, v: &[T]) -> usize {
    v.iter().filter(|s| t < s).count()
}
```

これは $O(n^2)$ の計算量を持ちますがこれをうまく分割統治統治すると $O(n \log n)$ で求められます。


``` rust
fn msc_divide<T: PartialOrd + Send + Sync>(v: &[T]) -> usize {
    table(v).into_iter().map(|(_, n)| n).max().unwrap()
}

fn table<T: PartialOrd + Send + Sync>(v: &[T]) -> Vec<(&T, usize)> {
    if v.len() == 1 {
        vec![(&v[0], 0)]
    } else {
        let mid = v.len() / 2;
        let xs = &v[0..mid];
        let ys = &v[mid..];
        let xs = table(xs);
        let ys = table(ys);
        join(&xs, &ys)
    }
}
fn join<'a, 'v, T: PartialOrd + Send + Sync>(
    mut xs: &'a [(&'v T, usize)],
    mut ys: &'a [(&'v T, usize)],
) -> Vec<(&'v T, usize)> {
    let mut v = Vec::new();
    loop {
        if ys.is_empty() {
            v.extend(xs);
            return v;
        } else if xs.is_empty() {
            v.extend(ys);
            return v;
        } else {
            let xt = &xs[0];
            let yt = &ys[0];
            if xt.0 < yt.0 {
                v.push((xt.0, xt.1 + ys.len()));
                xs = &xs[1..];
            } else {
                v.push(yt.clone());
                ys = &ys[1..];
            }
        }
    }
}
```

このコードはほぼストレートに`table`をrayon化できるのは見て取れるでしょう。
良いアルゴリズムと良い並列化が両立するのは素晴らしいですね。

# まとめ

* rayonには`par_iter`以外にも`join`があるよ
* `join`を使うといろんなアルゴリズムを並列化出来るよ
  + ただし無闇に並列化すると遅くなるから注意
* `join`と分割統治は相性がいいよ

# ノート

* このブログの大半の内容はrayonの[作者による解説](http://smallcultfollowing.com/babysteps/blog/2015/12/18/rayon-data-parallelism-in-rust/)でカバーされています。
* `min_max` の例は[アルゴリズムイントロダクション](https://www.amazon.co.jp/dp/476490408X)から採りました
* 最大上位者数問題は[関数プログラミング 珠玉のアルゴリズムデザイン](https://www.amazon.co.jp/dp/4274050645/)から採りました
* 今回用いたコードは[こちら](https://github.com/KeenS/rayon-playground)
* 以下にチューニングの結果を示します。閾値を小さくするとオーバーヘッドが無視出来ず、大きくすると並列性能が出ないことが見て取れると思います。
  データが多くて見づらいですがラベルをクリックするとデータ毎に表示/非表示を切り替えられるので色々比べてみて下さい。

<canvas id="rayon_thresholds"></canvas>

# 余談
因みにですが個別に`min`と`max`で求めたものと比べると


``` rust
const N: usize = 1024 * 1024;

#[bench]
fn bench_min_and_max(b: &mut Bencher) {
    let v = random_vec(N);
    b.iter(|| (v.iter().min(), v.iter().max()))
}

#[bench]
fn bench_min_max(b: &mut Bencher) {
    let v = random_vec(N);
    b.iter(|| min_max(&v))
}

#[bench]
fn bench_min_and_max_rayon(b: &mut Bencher) {
    use rayon::prelude::*;
    let v = random_vec(N);
    b.iter(|| (v.par_iter().min(), v.par_iter().max()))
}

#[bench]
fn bench_min_max_rayon(b: &mut Bencher) {
    let v = random_vec(N);
    b.iter(|| min_max_rayon(&v))
}
```

なんと全体的にはminとmaxを個別に求めた方が速い結果になりました。

``` rust
test bench_min_and_max       ... bench:     593,406 ns/iter (+/- 21,954)
test bench_min_and_max_rayon ... bench:     416,664 ns/iter (+/- 68,554)
test bench_min_max           ... bench:   1,982,283 ns/iter (+/- 124,381)
test bench_min_max_rayon     ... bench:     412,036 ns/iter (+/- 89,327)
```

min_max_rayonは辛勝…。

<canvas id="min_and_max_vs_min_max"></canvas>


色々試すとどうやらループの中で`if`を使ってるのが遅いらしく、ループ内の`if`を取り除いたら、つまり比較演算の削減を諦めてmin, maxを個別に求めるのと変わらないループを書くと速くなりました。不思議…。
別にイテレータの`min`や`max`がSIMDを生成していたとかではなくて、単に生成されるコードの品質の問題でした。


<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.4.0/Chart.min.js"></script>

<script type="application/javascript">
var labels = [];
for(var i = 0; i <= 19; i++) {
    labels[i] = 2**(i+1);
}
function genConfig(title, datasets) {
    let config = {
	type: 'line',
	data: {
	    labels: labels,
	    datasets: []
	},
	options: {
	    responsive: true,
	    title: {
		display: true,
		text: title
	    },
	    tooltips: {
		mode: 'index',
		intersect: false,
	    },
	    hover: {
		mode: 'nearest',
		intersect: true
	    },
	    scales: {
		xAxes: [{
		    display: true,
		    scaleLabel: {
			display: true,
			labelString: 'vector size'
		    }
		}],
		yAxes: [{
		    display: true,
                    type: 'logarithmic',
		    scaleLabel: {
			display: true,
			labelString: 'time (ns)'
		    }
		}]
	    }
	}
    };
    datasets.forEach(data => {
        config.data.datasets.push({
	    label: data.label,
	    backgroundColor: data.color,
	    borderColor: data.color,
	    data: data.data,
	    fill: false
	});
    })
    return config
}

var vanilla_vs_rayon = document.getElementById('vanilla_vs_rayon').getContext('2d');
new Chart(vanilla_vs_rayon, genConfig("min_max vs min_max_rayon", [
    {
        label: 'min_max_rayon',
        color: 'rgb(256, 128, 128)',
        data: [1, 4969, 6263, 8260, 10238, 11591, 13227, 15180, 17157, 23007, 33231, 47599, 75734, 133570, 241004, 421043, 804201, 1572861, 3136177, 5995852],
    },
    {
        label: 'min_max',
        color: 'rgb(128, 128, 256)',
        data: [0, 2, 5, 9, 21, 39, 73, 144, 289, 686, 3740, 8591, 17501, 42219, 69543, 135583, 275131, 530968, 1019684, 1976327],
    }
]));

var vanilla_vs_rayon_tuned = document.getElementById('vanilla_vs_rayon_tuned').getContext('2d');
new Chart(vanilla_vs_rayon_tuned, genConfig("min_max vs min_max_rayon(チューニング後)", [
    {
        label: 'min_max_rayon',
        color: 'rgb(256, 128, 128)',
        data: [1, 2, 6, 10, 18, 35, 70, 139, 277, 632, 2252, 6758, 15635, 46270, 47309, 51649, 83701, 152410, 275619, 532008],
    },
    {
        label: 'min_max',
        color: 'rgb(128, 128, 256)',
        data: [0, 2, 5, 9, 21, 39, 73, 144, 289, 686, 3740, 8591, 17501, 42219, 69543, 135583, 275131, 530968, 1019684, 1976327],
    }
]));

var rayon_thresholds = document.getElementById('rayon_thresholds').getContext('2d');
new Chart(rayon_thresholds, genConfig("thresholdを変えたときのmim_max_rayonのパフォーマンス", [
    {
        label: '2',
        data: [1, 4969, 6263, 8260, 10238, 11591, 13227, 15180, 17157, 23007, 33231, 47599, 75734, 133570, 241004, 421043, 804201, 1572861, 3136177, 5995852]
    },

    {
        label: '4',
        data: [1, 2, 5377, 6789, 7559, 9979, 11886, 13038, 14988, 18823, 23705, 31624, 53671, 92329, 161459, 279781, 547332, 1029873, 2043434, 3989602]
    },

    {
        label: '0000008',
        data: [2, 2, 6, 5718, 7489, 8650, 11075, 12323, 13605, 15674, 18473, 25299, 34824, 55843, 99329, 188902, 341966, 648656, 1258574, 2354102]
    },

    {
        label: '0000016',
        data: [2, 3, 6, 11, 5370, 6605, 8975, 11009, 11961, 13482, 16042, 19543, 27623, 42773, 73452, 136375, 242573, 449895, 894581, 1711311]
    },

    {
        label: '0000032',
        data: [2, 3, 7, 16, 23, 8106, 7789, 10858, 11720, 12659, 13847, 16564, 22043, 31661, 54789, 93711, 162402, 311356, 599225, 1264995]
    },

    {
        label: '0000064',
        data: [1, 3, 6, 13, 25, 42, 7565, 8450, 11010, 12113, 13547, 15418, 19406, 28498, 45491, 74264, 130119, 229457, 423164, 816093]
    },

    {
        label: '0000128',
        data: [1, 2, 6, 13, 19, 37, 72, 5739, 9677, 11677, 12904, 14111, 17413, 23945, 36427, 56699, 105557, 187037, 355136, 652626]
    },

    {
        label: '0000256',
        data: [1, 3, 7, 13, 22, 58, 110, 184, 12032, 11574, 12861, 14554, 16853, 22186, 32045, 49191, 90558, 163303, 307767, 593919]
    },

    {
        label: '0000512',
        data: [1, 2, 6, 12, 19, 37, 73, 141, 284, 7833, 10931, 12553, 15002, 20304, 29863, 47826, 79071, 144858, 272874, 531286]
    },

    {
        label: '0001024',
        data: [1, 2, 5, 11, 20, 35, 95, 184, 390, 698, 18329, 19275, 18980, 23640, 34736, 56762, 96810, 164784, 302074, 584485]
    },

    {
        label: '0002048',
        data: [1, 3, 5, 13, 23, 41, 81, 162, 347, 645, 2011, 17156, 16931, 19250, 28485, 44241, 76107, 132698, 252528, 491360]
    },

    {
        label: '0004096',
        data: [1, 2, 5, 9, 19, 51, 86, 173, 343, 687, 3225, 7011, 29425, 29276, 31519, 48384, 81598, 150722, 277016, 540963]
    },

    {
        label: '0008192',
        color: 'rgb(256, 128, 128)',
        data: [1, 2, 6, 10, 18, 35, 70, 139, 277, 632, 2252, 6758, 15635, 46270, 47309, 51649, 83701, 152410, 275619, 532008]
    },

    {
        label: '0016384',
        data: [1, 2, 6, 11, 20, 42, 84, 157, 312, 617, 2341, 6095, 14035, 30036, 64901, 68928, 76370, 138835, 243741, 473987]
    },

    {
        label: '0032768',
        data: [1, 2, 5, 11, 18, 35, 70, 139, 282, 588, 2380, 7300, 16692, 37347, 73819, 182132, 168316, 155573, 353786, 565423]
    },

    {
        label: '0065536',
        data: [1, 3, 6, 10, 21, 40, 79, 162, 294, 611, 2040, 6633, 13929, 30203, 63107, 121916, 226008, 246603, 244836, 482809]
    },

    {
        label: '0131072',
        data: [1, 2, 5, 11, 19, 35, 69, 137, 274, 624, 2241, 6736, 14389, 30630, 63357, 124594, 250942, 395238, 466010, 481440]
    },

    {
        label: '0262144',
        data: [1, 2, 5, 10, 18, 36, 71, 140, 280, 559, 2211, 6056, 14412, 30083, 61307, 125313, 251839, 519617, 751774, 979082]
    },

    {
        label: '0524288',
        data: [1, 2, 5, 12, 21, 38, 77, 144, 294, 609, 2311, 6007, 13846, 30177, 69691, 144964, 275105, 545024, 1060201, 1353068]
    },

    {
        label: '1048576',
        data: [1, 2, 5, 10, 18, 36, 77, 156, 303, 606, 1740, 5905, 14085, 29457, 63666, 130469, 260893, 518881, 986104, 2013271]
    },
]));
var min_and_max_vs_min_max = document.getElementById('min_and_max_vs_min_max').getContext('2d');
new Chart(min_and_max_vs_min_max, genConfig("min_and_max, min_and_max_rayon, min_max, min_max_rayon", [
    {
        label: 'min_max_rayon',
        color: 'rgb(256, 128, 128)',
        data: [1, 2, 6, 10, 18, 35, 70, 139, 277, 632, 2252, 6758, 15635, 46270, 47309, 51649, 83701, 152410, 275619, 532008],
    },
    {
        label: 'min_max',
        color: 'rgb(128, 128, 256)',
        data: [0, 2, 5, 9, 21, 39, 73, 144, 289, 686, 3740, 8591, 17501, 42219, 69543, 135583, 275131, 530968, 1019684, 1976327],
    },
    {
        label: 'min_and_max_rayon',
        color: 'rgb(256, 64, 64)',
        data: [9116, 11592, 16563, 20167, 22646, 33490, 37236, 38469, 41873, 44973, 47956, 51669, 59828, 69602, 82043, 107234, 150433, 203012, 309735, 496755],
    },
    {
        label: 'min_and_max',
        color: 'rgb(64, 64, 256)',
        data: [2, 5, 7, 12, 20, 36, 68, 136, 279, 559, 1102, 2180, 4366, 9030, 18182, 35873, 71861, 144179, 293551, 580270],
    }
]));

</script>
