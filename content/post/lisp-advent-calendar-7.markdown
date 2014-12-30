---
type: post
title: "ClackのHandlerの書き方"
date: 2013-12-06
comments: true
sharing: true
categories: [Advent Calendar, Clack, Common Lisp, Lisp]
---
(この記事は [Lisp Advent Calendar](http://qiita.com/advent-calendar/2013/lisp) 7日目のためのエントリです。  
 ( [6日目](http://meymao.hatenablog.com/entry/2013/12/06/140029) meymaoさんより「Lisperがクリスマスに贈るべきプレゼント三選」)  
 ( [8日目](http://www.principia-m.com/ts/0081/index-jp.html) athos0220さんよりマクロとクロージャで作る並行プログラミング言語))

ClackのHandlerの書き方についてちゃちゃっと解説します。

<!--more-->

さっくりゆるふわな感じで行くので [clack公式](http://clacklisp.org/)だとか [チュートリアル](http://clacklisp.org/tutorial/ja/)だとかも参考にして下さい。とは言ってもCommon Lispな方なら知ってるでしょう。

## Clackって？

PerlのPlackやRubyのRackと同じくCommon Lispの統一HTTPサーバーインターフェースです。開発時はHunchentootで、本番はFastCGIでみたいなことが簡単にできます。

## Handlerって？

HunchentootやFastCGIといったバックエンドとClackとの間でリクエストやレスポンスの受け渡しを担当します。さっくり言うとHandlerを書けばClackで使えるサーバーが増える訳です。今(2013-12現在)のところ、Hunchentoot、FastCGI、Apache + mod\_lisp2があるようです。

じゃあ、実際に軽量サーバーの [toot](https://github.com/gigamonkey/toot)のハンドラを書きながら解説しますね

## 準備

`git clone git@github.com:fukamachi/clack.git`してclackのソースコードを持ってきます。他のHanderがそうしてるようなので

- `clack/clack-handler-toot.asd`
- `clack/src/core/handler/toot.lisp`

を作ります。`core`じゃなくて`contrib`だろとかそもそも自分のリポジトリに作れよとかは自由にやって下さい。`clack/clack-handler-toot.asd`は適当に似た名前のやつをコピーすれば良いんじゃないですかね？（適当

## 書き方

いたって簡単で、`run (app &key debug (port 5000)) -> acceptor`と`stop (acceptor) -> 多分決まってない`を実装すれば良いです。

とはいっても`stop`は1行で終わりますが`run`は大きく分けて

1. . サーバーを立ち上げる
2. . サーバーから渡されたリクエスト(多くの場合`request`オブジェクト)をplistにして`app`に渡す
3. . `app`のlist形式の返値を適切な形(多くの場合`response`オブジェクト)にしてサーバーに返す
4. . エラーハンドリング

の4つの作業があります。一つ一つ説明していきますね。

### 1 サーバーを立ち上げる

tootの場合は`toot:start-server (&key handler port)`を使いました。スレッド立てるのはclackがやってくれます。はい。

### 2 サーバー渡されたリクエスト(多くの場合`request`オブジェクト)をplistにして`app`に渡す

tootの場合、`key`の`handler`が`requesut`オブジェクトを受け取って`response`オブジェクトを返せば良いので

    (lambda (req)
        .....
        (call app (handle-request req))
        .....
    )

って感じで`handle-request`に実装を書きます。

`handle-request`の内部は`req`を [clack公式のチュートリアル](http://clacklisp.org/tutorial/ja/04-the-environment.html)に載っているプロパティに一つ一つ変換していきます。このプロパティとバックエンドのオブジェクトのスロット名と実際のHTTPヘッダの名前が必ずしも一致しないのが泣き所です。さらに、大抵のサーバーの場合リクエストオブジェクトのスロットの詳細まではドキュメントに載ってないのでソースを参照しながら書くことになります。また、そもそも対応する値が無くて、他の情報を切り貼りして作らないといけなかったり、どうしようもなくて空にしないといけないこともあります。

tootハンドラは

    (defun handle-request (req)
      "Convert Request from server into a plist
    before pass to Clack application."
      (let ((content-length (and (request-header :content-length req)
                                 (parse-integer (request-header :content-length req) :junk-allowed t)))
        (port-and-host (get-port-and-host req)))
        (append
         (list
          :request-method (request-method req)
          :script-name ""
          :path-info (url-decode (request-path req))
          :server-name (car port-and-host)
          :server-port (cdr port-and-host)
          :server-protocol (server-protocol req)
          :request-uri (request-uri req)
          :url-scheme :HTTP;(request-scheme req)
          :remote-addr (remote-addr req)
          :remote-port (remote-port req)
          :query-string (request-query req)
          :content-length content-length
          :content-type (request-header :content-type req)
          :raw-body (let ((stream (toot::request-body-stream req)))
                      ;(when content-length
                        ;(setf (flex:flexi-stream-bound stream) content-length))
                      stream)
          :clack.uploads nil
          :clack.handler :toot)
    
    
         (loop for (k . v) in (toot::request-headers req)
               unless (find k '(:request-method :script-name :path-info :server-name :server-port :server-protocol :request-uri :remote-addr :remote-port :query-string :content-length :content-type :accept :connection))
                 append (list (intern (format nil "HTTP-~:@(~A~)" k) :keyword)
                              v)))))

こんな感じに実装されてます。tootからインポートしたものの他、一部ヘルパー関数も使ってますが挙動はまあ、名前から察して下さい。

### 3 `app`の返値のplistを適切な形(多くの場合`response`オブジェクト)にしてサーバーに返す

`app`を`call`してやると`(status headers body)`という形式のlistが返ってきます。`status`は数値、`headers`はplist、`body`はパスネーム又は文字列のリストです。

さっきはこんな感じで呼んだのでした。

    (lambda (req)
        .....
        (call app (handle-request req))
        .....
    )

もうお分かりかと思いますが

    (lambda (req)
        (handle-response
            (call app (handle-request req)))
    )

として、実装は`handle-response`に書きます。…が、tootは`response`オブジェクトではなく`request`オブジェクトに変更を加えたものを返すようなので

    (lambda (req)
        (handle-response
            req
            (call app (handle-request req)))
    )

として、`app`の返値を元に`req`を書き換えます。こちらもあまりドキュメントが無いので頑張ってソース読むしかないです。因みに`body`はパスネームならそのファイルの内容を、文字列のリストならそれぞれを改行(`<br>`ではなく`\n`)で連結したものを返す必要があります。

tootハンドラの実装載せときますね。

    (defun handle-response (req res)
      (destructuring-bind (status headers body) res
        (etypecase body
          (pathname
           (multiple-value-call #'serve-file
         (values req body (parse-charset (getf headers :content-type)))))
          (list
           ;; XXX: almost same as Clack.Handler.Hunchentoot's one.
           (setf (status-code req) status)
           (loop for (k v) on headers by #'cddr
                 with hash = (make-hash-table :test #'eq)
                 if (gethash k hash)
                   do (setf (gethash k hash)
                            (format nil "~:[~;~:*~A, ~]~A" (gethash k hash) v))
                 else if (eq k :content-type)
                   do (multiple-value-bind (v charset)
                          (parse-charset v)
                        (setf (gethash k hash) v)
                        (setf (toot::response-charset req) charset))
                 else do (setf (gethash k hash) v)
                 finally
              (loop for k being the hash-keys in hash
                    using (hash-value v)
                    do (setf (response-header k req) v)))
           (toot::send-response req (with-output-to-string (s)
          (format s "~{~A~^~%~}" body)))))))

です。

    (destructuring-bind (status headers body) res
      (etypecase body
        (pathname ...)
        (list ...)))

がテンプレートですね。`pathname`のときはtootに丸投げして`list`のときは`headers`とかを真面目に処理してます。

### 4 エラーハンドリング

早い話が500 internal server errorです。普通、Lispはエラーが起きるとデバッガに落ちますがサーバーは走り続ける必要がるのであらゆるエラーを無視する必要があります。が、しかしデバッグするとき(`run`のキーワード引数に`debug`がありましたね)はデバッガに落ちると嬉しいです。なので例のラムダを少し変更します。

    (lambda (req)
                  (handle-response
                   req
                   (if debug (call app (handle-request req))
               (aif (handler-case (call app (handle-request req))
                  (condition () nil))
                it
                '(500 nil nil)))))

これで完成です。

## テスト

Clackにはテストが付いてます。テストケース自体は`clack/src/core/test/suite.lisp`に書かれてます。

`clack/t/core/handler/hunchentoot.lisp`を参考に`clack/t/core/handler/toot.lisp`を作り、`clack/clack-test.asd`の`hunchentoot`を`toot`に書き換えたら準備完了です。`(ql:quickload :clack-test)`しましょう。テストが走ります。そして恐らくエラーが出て止まるでしょう。変な値を返すとサーバーじゃなくてテストの方がエラーで死ぬんですね。

そしたら`clack/src/core/test/suite.lisp`を開いて期待された値がなんだったかを調べてサーバーを修正します。で、また`(ql:quickload :clack-test)`の繰り返し。でも一回`quickload`しちゃうと読み直してくれないので私は一々`M-x slime-restart-inferior-lisp`してました。なんか違う気がする。これだけじゃなくてテスト全般。溢れるバッドノウハウ感。

とりあえずバックエンドにバグがあるとかのどうしようもない場合を除いてテストに全部合格すれば完成です

## 完成したら

どうしたらいいんでしょうね。分かんないです。clackにpull-req送るんでしょうか。あるいはquicklispに登録?私はとりあえず [ブランチに置いてます](https://github.com/KeenS/clack)が恐らく誰も使ってないですし知らないと思います。

## で、何作ればいい？

[clackのissue](https://github.com/fukamachi/clack/issues?state=open)にまだ作られてないものがリストされてます。「自分では需要はないけどなんか作ってみたい」といった奇特な方はそこから試すと良いんじゃないでしょうか。GAEとかmongrel2とか需要ありそうな気がします。

## まとめ

誰得


