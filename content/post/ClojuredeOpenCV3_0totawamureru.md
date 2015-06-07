---
categories: [Lisp, Clojure, OpenCV]
date: 2015-06-07T16:56:51+09:00
title: ClojureでOpenCV 3.0と戯れる
---
κeenです。先日OpenCV 3.0がリリースされましたね。
ちょっと触ってみようと思ったのですが公式バインディングがC++、C、Java、Pythonと中々つらい言語ばっかりなので扱いやすいClojureから触った時のメモです。

2系とはAPIが変わってる部分もあるらしく、苦労しました。
UbuntuでやってるのでMacの人は適当に読み替えて下さい。
<!--more-->

# 準備
## OpenCV
[公式](http://opencv.org/)からOpenCV 3.0をダウンロードしてきましょう。展開してからは

```
$ cd opencv-3.0.0
$ cmake .
$ make -j4
$ sudo make -j4 install
```

でインストールまでしてくれます。



## ImShow
どうも3.0からHighguiというQtベースのGUIツール群のJavaバインディングが作られなくなったそうです。Swing使えと。

ということでOpenCVとSwingのブリッジしてくれる軽いライブラリが[ImShow-Java-OpenCV](https://github.com/master-atul/ImShow-Java-OpenCV)
です。.javaファイル1枚なので本当に軽いです。

.jarもレポジトリに入ってるのですがソースコードより古いらしく、自分でビルドする必要があります。

```
$ git clone git@github.com:master-atul/ImShow-Java-OpenCV.git
$ cd ImShow-Java-OpenCV/ImShow_JCV/src
$ javac com/atul/JavaOpenCV/Imshow.java
$ jar -cf Imshow.jar com/atul/JavaOpenCV/Imshow.class
```

## lein プロジェクト
Clojureのプロジェクト管理ツールの[Leiningen](http://leiningen.org/)を使います。知らない人は適当にググって下さい。

まずプロジェクトを作ります。

```
$ lein new opencv-play
```

んで、mavenでいうところのpom.xmlにあたるproject.cljをいじります。

```
$ cd opencv-play
$ cat project.clj
(defproject opencv-play "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]])
$ edit project.clj
$ cat project.clj
(defproject opencv-play "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [cider/cider-nrepl "0.9.0-SNAPSHOT"]]
  :jvm-opts ["-Djava.library.path=./lib"]
  :resource-paths ["./lib/opencv-300.jar"
                   "./lib/Imshow.jar"]
  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)])
```

そしたら先程のライブラリ達を配置します。

```
$ mkdir lib
$ cp /usr/local/share/OpenCV/java/* lib
$ cp /path/to/ImShow-Java-OpenCV/ImShow_JCV/src/Imshow.jar lib
```

ついでにlenaも呼びましょう。

```
$ mkdir img
$ cp /path/to/src/of/opencv-3.0.0/samples/data/lena.jpg img
```

# 遊ぶ
準備完了ってことでREPLを起動しましょう。ちょっと遅いですが我慢。

```
$ lein repl
Picked up JAVA_TOOL_OPTIONS: -javaagent:/usr/share/java/jayatanaag.jar 
Picked up JAVA_TOOL_OPTIONS: -javaagent:/usr/share/java/jayatanaag.jar 
nREPL server started on port 52301 on host 127.0.0.1 - nrepl://127.0.0.1:52301
REPL-y 0.3.5, nREPL 0.2.6
Clojure 1.6.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_25-b17
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> 
```

このままREPLを使ってもいいですが私はEmacsから[CIDER](https://github.com/clojure-emacs/cider)でnREPLにつなぎます。
REPLでも補完は効くのでEmacsやらのエディタの設定が面倒ならこのままでも十分ですよ。

まずは画像のロードから。2系とは違ってImagecodecを使うようです。

```clojure
user> (import [org.opencv.imgcodecs Imgcodecs])
org.opencv.imgcodecs.Imgcodecs
user> (def lena (Imgcodecs/imread "img/lena.jpg"))
#'user/lena
```

いじる前に表示してみましょう。

```clojure
user> (import [com.atul.JavaOpenCV Imshow])
com.atul.JavaOpenCV.Imshow
user> (def is (Imshow. "Lena"))
#'user/is
user> (.showImage is lena)
nil
```

![lena](/images/clojure-opencv/show-lena.png)

よしよし。表示されましたね。ではこの辺を参考に顔認識してみましょう。

[Clojure - 二次元絵の顔を検出する - Qiita](http://qiita.com/woxtu/items/bf39e3d53cbf60396d2c)

まずは必要なパッケージの読み込み

```clojure
user> (import [org.opencv.core Mat CvType])
org.opencv.core.CvType
user> (import [org.opencv.imgproc Imgproc])
org.opencv.imgproc.Imgproc
```

前処理のグレースケール変換からヒストグラムの均一化まで一気にやってしまいましょう。

```clojure
user> (def buffer (Mat. 512 512 CvType/CV_8UC3))
#'user/buffer
user> (Imgproc/cvtColor lena buffer Imgproc/COLOR_RGB2GRAY)
nil
user> (Imgproc/equalizeHist buffer buffer)
nil
```

ここで一旦画像の確認。

```clojure
user> (.showImage is buffer)
nil
```

![gray-hist-lena](/images/clojure-opencv/gray-hist-lena.png)

ふむふむ。ではでは顔を認識しますか。

まずは色々準備します。

```clojure
user> (import [org.opencv.core MatOfRect])
org.opencv.core.MatOfRect
user> (import [org.opencv.objdetect CascadeClassifier])
org.opencv.objdetect.CascadeClassifier
user> (def faces (MatOfRect.))
#'user/faces
user> (def classifier (CascadeClassifier.))
#'user/classifier
user> (.load classifier "/usr/local/share/OpenCV/haarcascades/haarcascade_frontalface_default.xml")
true
```

因みに分類器のデータのロードは成功がtrueで失敗がfalseだそうです。例外投げないんですね。ハマった。パス名に`~`を使うと(ホームを省略表記すると)ロードに失敗するようなのでお気をつけて。

それでは実行。

```clojure
user> (.detectMultiScale classifier buffer faces)
nil
```

さて、顔の位置がとれたので画像に重ねるのですがここではSwing(AWT)の描画を使ってみましょう。
OpenCVの描画は画像に描画するのでファイルに書き出しても残っているのに対してSwingのだと表示しているフレームに描画するので書き出した画像には残りません。また、リフレッシュすれば消えるのでインタラクティブに実験するのに向いてます。

一旦準備。フレームをレナの元画像に戻しておきましょう。

```clojure
user> (import [java.awt Rectangle Color])
java.awt.Color
user> (.showImage is lena)
nil
```

ImshowのWndowというパブリックなメンバにJFrameが入ってるのでそれを操作します。

```clojure
user> (def g2 (.getGraphics (.Window is)))
#'user/g2
user> (.setColor g2 Color/GREEN)
nil
user> (doseq [face (.toList faces)]
  (let [rect (Rectangle.)]
    (do
      (.setRect rect (.width face) (.height face) (.x face) (.y face))
      (.draw g2 rect))))
nil
```

はい。

![face recognized lena](/images/clojure-opencv/face-recognized-lena.png)

パチパチパチ

因みに四角形を消すには
```clojure
user> (.showImage is lena)
nil
```

で十分です。ミスっても何回でもやり直せますね。

# まとめ
因みに3.0のドキュメントが探しづらいのですが、masterにあるようです。

[OpenCV: OpenCV](http://docs.opencv.org/master/)

OpenCVをインタラクティブに扱えると楽しいのでみなさんClojure使いましょう。

