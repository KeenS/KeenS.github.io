---
categories: [network]
date: 2016-02-24T23:57:31+09:00
title: Bind Addressの意味がようやく分かった
---
κeenです。小ネタを。サーバを立ち上げる時に指定するBind Addressについてよく分かっていなかったのがようやく分かるようになったのでそれについて。
<!--more-->
Bind Addressはサーバ(特にHTTPサーバ)を立ち上げる時によく指定することになりますが、Usageを見ても「バインドするアドレスを指定する」などとトートジーめいたことしか書いてありませんでした。
実際に使ってみても127.0.0.1を指定すればローカルホストから、0.0.0.0を指定すれば外部からでも参照出来るな、くらいの認識しかありませんでした。
意味も分からず使っていると気持ち悪いもので、2つの疑問が湧いてきます。

* 0.0.0.0と127.0.0.1は正確に何を意味するのか
* 127.0.0.1と0.0.0.0以外のアドレスを指定するのはどのようなケースか


もちろん、127.0.0.1がlocalhostを指すことは知っていますが0.0.0.0というアドレスはBind Addressでしか見たことありませんし、127.0.0.1が私の知っているlocalhostの意味でない可能性も十分あるな、と思っていました。

さて、この疑問に答えるには前提知識が必要なのでそこから始めましょう。

# マシンは複数のIPアドレスを持ちうる

1つのマシンに、複数のIPアドレスが割り当てられることがあります。
典型的なのはルータのようにインターネットとローカルエリアネットワーク両方に参加しているもので、LANには「192.168.1.1」、インターネットには「www.xxx.yyy.zz」という2つのIPアドレスを持つでしょう。
また、どのマシンにも先程述べたlocalhost、127.0.0.1という内部から自身を指すIPアドレスもあります。
図にするとこんな感じでしょうか。


```
  [インターネット]
       |
       |
       | www.xxx.yyy.zz
[x 127.0.0.1] ルータ
       | 192.168.1.1
       |
   ...-+--+
          | 192.168.1.2
         [ 127.0.0.1]マシンA

```

さて、この時に例えばルータの管理WebUIを提供したいとしましょう。このUIは勿論LAN内にいるマシンAからは参照したいですがインターネット全てに公開する訳にはいきません。
こういう時のアクセス制御に使うのがBind Addressです。

# 呼ばれ方で反応を変える
bind addressは、他のマシンから何という名前で呼ばれたかでメッセージを受け取る/受け取らない（というか見える見えない）を変えるものです。
街中で「田中さん」と呼ばれたら返事したいけど「†漆黒の天翅†さん」と呼ばれたら返事したくないですよね。
少し実験してみましょう。

さて、私の手元のマシンはifconfigによると127.0.0.1というアドレスと192.168.1.6というアドレスを持っているようです。

```
$ ifconfig
lo        Link encap:ローカルループバック  
          inetアドレス:127.0.0.1  マスク:255.0.0.0
          inet6アドレス: ::1/128 範囲:ホスト
          UP LOOPBACK RUNNING  MTU:65536  メトリック:1
          RXパケット:5132 エラー:0 損失:0 オーバラン:0 フレーム:0
          TXパケット:5132 エラー:0 損失:0 オーバラン:0 キャリア:0
          衝突(Collisions):0 TXキュー長:0 
          RXバイト:665331 (665.3 KB)  TXバイト:665331 (665.3 KB)
wlp3s0    Link encap:イーサネット  ハードウェアアドレス cc:3d:82:38:21:e2  
          inetアドレス:192.168.1.6  ブロードキャスト:192.168.1.255  マスク:255.255.255.0
          inet6アドレス: fe80::ce3d:82ff:fe38:21e2/64 範囲:リンク
          UP BROADCAST RUNNING MULTICAST  MTU:1500  メトリック:1
          RXパケット:85578 エラー:0 損失:0 オーバラン:0 フレーム:0
          TXパケット:64002 エラー:0 損失:0 オーバラン:0 キャリア:0
          衝突(Collisions):0 TXキュー長:1000 
          RXバイト:85393847 (85.3 MB)  TXバイト:10710856 (10.7 MB)
```

HTTPサーバをそれぞれにbindしてみます。今回使うのはRubyのコマンドラインから使えるHTTPサーバです。

まずは127.0.0.1。

```
$ ruby -run -e httpd -- ./ --port 8080 --bind 127.0.0.1
```


こんな感じですかね。

```
+-------+
|       |
|     +-+ 127.0.0.1
|ruby-+ |
|       + 192.168.1.6
+-------+
```


さて、これにcurlでアクセスしてみます。

```
$ curl 127.0.0.1:8080
結果が返ってくる
```

```
$ curl 192.168.1.6:8080
curl: (7) Failed to connect to 192.168.1.6 port 8080: 接続を拒否されました
```

次に192.168.1.6を試します。

```
$ ruby -run -e httpd -- ./ --port 8080 --bind 192.168.1.6
```

これもこんな感じですかね。

```
+-------+
|       |
|       + 127.0.0.1
|ruby-+ |
|     +-+ 192.168.1.6
+-------+
```


同じくcurlでアクセスしてみます。

```
$ curl 127.0.0.1:8080
curl: (7) Failed to connect to 127.0.0.1 port 8080: 接続を拒否されました
```

```
$ curl 192.168.1.6:8080
結果が返ってくる
```

見事にbindしたアドレスに投げた時にしか結果が返ってきません。
また、無関係なアドレス、例えば192.168.1.2などにbindしようとするとこうなります。

```
$ ruby -run -e httpd -- ./ --port 8080 --bind 192.168.1.2
[2016-02-25 00:36:17] INFO  WEBrick 1.3.1
[2016-02-25 00:36:17] INFO  ruby 2.1.5 (2014-11-13) [x86_64-linux-gnu]
/usr/lib/ruby/2.1.0/socket.rb:206:in `bind': Cannot assign requested address - bind(2) for 192.168.1.2:3000 (Errno::EADDRNOTAVAIL)
```

どうやら正確に自分のアドレスでないとbind出来ないようです。



# 0.0.0.0はワイルドカード
さて、これで正確に何という名前で呼ばれたかでアクセスを制御出来るようになりました。しかし公開サーバのように「どこからでも」アクセスさせたい場合にこと細かく指定させるのは不便です（というか複数のネットワークから参照することが出来なくなる？）。
その「どこからでも」を表すのが0.0.0.0です。

試してみます。

```
$ ruby -run -e httpd -- ./ --port 8080 --bind 0.0.0.0
```

こうなっているのでしょうか。


```
+-------+
|       |
|     +-+ 127.0.0.1
|ruby-+ |
|     +-+ 192.168.1.6
+-------+
```

```
$ curl 127.0.0.1:8080
結果が返ってくる
```

```
$ curl 192.168.1.6:8080
結果が返ってくる
```

```
$ curl 0.0.0.0:8080
結果が返ってくる
```

このようにどのような指定方法でも返ってきます。
curl 0.0.0.0:3000で結果が返ってきた(0.0.0.0が自身を指す)ことに驚きましたがワイルドカードアドレスにbindされた時にだけ結果が返ってきて欲しい時に便利なんですかね。

# まとめ

Bind Addressは本当にBindするAddressでした。足りないのはBind Addressに対する知識ではなくネットワークに対する知識でした。
世の中知らないといけないことが多い。

