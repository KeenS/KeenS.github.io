---
categories: [DB, RockDB, Rust]
date: 2018-02-05T00:00:00+09:00
title: "Hello, RocksDB"
---

κeenです。進捗ダメです。最近全然コード書いてないのでたまには手を動かすかということでRocksDBにHello Worldしてみます。
<!--more-->
# RocksDBとは
[RocksDB](http://rocksdb.org/)はアプリケーション組み込み向けに作られた永続化KVSです。
主に高速性、SSDなどの高速なストレージに合わせたアーキテクチャ、アプリケーションとの親和性、基本操作の他マージなどの高度なオペレーションを特徴としているようです。

FacebookのDBエンジニアリングチームが作っていて、[MyRocks](https://github.com/facebook/mysql-5.6)なんかのバックエンドに使われています。
Googleの[LevelDB](https://github.com/google/leveldb)を下敷きに作られている模様。

私が知ったきっかけはやはりMySQL互換プロトコルを喋る分散DBの[TiDB](https://github.com/pingcap/tidb)がバックエンドに使っているからでした。

# Hello, RocksDB
いくら新しいとはいえ、ただのKVSなので使い方はそんなの難しくないはず。とりあえず使ってみましょう。
RocksDB自体はC++で書かれているのですが私はC++はわからないのでRustから叩いてみます。

色々ラッパはあるようですがひとまず一番ダウンロードされている[rocksdb](https://crates.io/crates/rocksdb)を使います。
これはあまりラッピングが上手くなく、先述のTiDBは[フォークしている](https://github.com/pingcap/rust-rocksdb)など不穏なのですが問題があるなら直せばいいの気持ちでやっていきます。

まずはクレートドキュメントのサンプルコードを動かしてみましょう。

``` rust
use rocksdb::DB;
 // NB: db is automatically closed at end of lifetime
 let db = DB::open_default("path/for/rocksdb/storage").unwrap();
 db.put(b"my key", b"my value");
 match db.get(b"my key") {
    Ok(Some(value)) => println!("retrieved value {}", value.to_utf8().unwrap()),
    Ok(None) => println!("value not found"),
    Err(e) => println!("operational problem encountered: {}", e),
 }
 db.delete(b"my key").unwrap();
```

`open_default` したあとは `put` , `get` , `delete` とわかりやすいですね。
実行してみます。


``` console
$ cargo run
retrieved value my value
```

はい。動きました。

# memcached
このままだと面白くないのでmemcachedのプロトコルを喋ってみましょう。

プロトコルのドキュメントは[ここ](https://github.com/memcached/memcached/blob/master/doc/protocol.txt)にあります。

## エンジン
たくさん実装するのはだるいのでひとまずは `set`, `get`, `delete` を実装します。no_replyとcasは無視しましょう。
プロトコルを読むに、以下のデータ型を用意すればよさそう。

``` rust
type Result<T> = ::std::result::Result<T, rocksdb::Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Value {
    pub data: Vec<u8>,
    pub flags: u32,
    pub exptime: i64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Command<'a> {
    Set { key: &'a [u8], value: Value },
    Get { keys: &'a [&'a [u8]] },
    Delete { key: &'a [u8] },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CommandRet<'a> {
    Stored,
    Got(Vec<(&'a [u8], Value)>),
    Deleted,
    NotFound,
}
```

getのキーを複数にした人の気がしれませんが仕様にそう書いてあるのでこれでいきます。

まずは実行部分から。
イメージ、これが動くようにします。

``` rust
fn main() {
    let engine = Engine::new("path/for/rocksdb/storage").unwrap();
    let commands = vec![
        Command::Set {
            key: b"key",
            value: Value {
                data: b"value".to_vec(),
                exptime: 0,
                flags: 0,
            },
        },
        Command::Get { keys: &[b"key"] },
        Command::Get { keys: &[b"no such key"] },
        Command::Delete { key: b"key" },
        Command::Get { keys: &[b"key"] },
        Command::Delete { key: b"key" },
    ];
    for cmd in commands {
        println!("query: {:?}", cmd);
        match engine.exec(cmd) {
            Ok(ret) => println!("{}", ret),
            Err(e) => println!("error: {}", e),
        }

    }
}
```

まあ、ただのインタプリタですね。ディスパッチ部分まではサクッと作れます。

と、その前にvalueのシリアライザ/デシリアライザを作っておきましょう。
データの末尾8byteを使って `flags` と `exptime` を保存すればいいですかね。
`exptime` はクライアントとのやりとりのために `i64` にしてますが負値の場合は保存せずにそのまま捨てるので32bitで十分です。

``` rust
fn encode_be(b: u32) -> [u8; 4] {
    [
        ((b >> 24) & 0xff) as u8,
        ((b >> 16) & 0xff) as u8,
        ((b >> 8) & 0xff) as u8,
        (b & 0xff) as u8,
    ]
}

fn decode_be(bytes: [u8; 4]) -> u32 {
    (bytes[0] as u32) << 24 + (bytes[1] as u32) << 16 + (bytes[2] as u32) << 8 + bytes[3]
}

impl Value {
    fn pack(self) -> Vec<u8> {
        let mut data = self.data;
        data.extend_from_slice(&encode_be(self.flags));
        data.extend_from_slice(&encode_be(self.exptime as u32));
        data
    }

    fn from_vec(mut data: Vec<u8>) -> Value {
        let len = data.len();
        assert!(len > 8);
        let flags = decode_be([data[len - 8], data[len - 7], data[len - 6], data[len - 5]]);
        let exptime = decode_be([data[len - 4], data[len - 3], data[len - 2], data[len - 1]]);
        let exptime = exptime as i64;
        data.truncate(len - 8);
        Value {
            data,
            flags,
            exptime,
        }
    }
}

```

はい、ではディスパッチ部分まで。

``` rust
struct Engine {
    db: DB,
}

impl Engine {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        let db = DB::open_default(path)?;
        Ok(Self { db: db })
    }

    pub fn exec<'a>(&self, cmd: Command<'a>) -> Result<CommandRet<'a>> {
        match cmd {
            Command::Set { key, value } => {
                let () = self.set(key, value)?;
                Ok(CommandRet::Stored)
            }
            Command::Get { keys } => {
                let v = self.get(keys)?;
                Ok(CommandRet::Got(v))
            }
            Command::Delete { key } => {
                if self.delete(key)? {
                    Ok(CommandRet::Deleted)
                } else {
                    Ok(CommandRet::NotFound)
                }
            }
        }
    }
    fn set<'a>(&self, key: &'a [u8], value: Value) -> Result<()> { ... }
    fn get<'a>(&self, keys: &'a [&'a [u8]]) -> Result<Vec<(&'a [u8], Value)>> { ... }
    fn delete<'a>(&self, key: &'a [u8]) -> Result<bool> { ... }
}
```


この `set`, `get`, `delete` のそれぞれの関数を実装していきます。

まずは `set` 。
exptimeが負値ならばその場でexpireとのことなのでそうします。
`put` のセマンティクスがcreate or updateらしいのでそれを使えばOK。

``` rust
    /// create, update or delete the kv pair
    fn set<'a>(&self, key: &'a [u8], value: Value) -> Result<()> {
        if value.exptime < 0 {
            self.db.delete(&key)
        } else {
            self.db.put(&key, &value.pack())
        }
    }
```

つぎは`get`。

取得したデータが期限切れしてたら消します。時刻を扱うのに [chrono](https://github.com/chronotope/chrono)を使いましょう。

返り値はベクトルです。keysのうち、見つからないものはスルーするらしいです。すごい仕様ですね。
`Result` と `Vec` と `Option` が出てきて煩雑ですが [`Iterator`](https://doc.rust-lang.org/std/iter/trait.Iterator.html) のテクニックを弄することなく地道に手続き的にがんばりましょう。
因みにRocksDB自体には[`MultiGet`](https://github.com/facebook/rocksdb/blob/master/db/db_impl.h#L110)がありますがラッパは作られていません。


``` rust
    /// find data and collect only found data
    fn get<'a>(&self, keys: &'a [&'a [u8]]) -> Result<Vec<(&'a [u8], Value)>> {
        let mut ret = Vec::new();
        for &key in keys {
            match self.db.get(&key)? {
                None => (),
                Some(v) => {
                    let entry = Value::from_vec(v.to_vec());
                    if entry.exptime == 0 {
                        ret.push((key, entry))
                    } else {
                        let now = Utc::now();
                        if entry.exptime < now.timestamp() {
                            self.db.delete(&key)?
                        } else {
                            ret.push((key, entry))
                        }
                    }
                }
            }

        }
        Ok(ret)
    }
```

次に `delete` です。残念なことに `delete` のラッパはキーの有無を区別してくれないので削除前に区別する必要があります(RocksDBの[`Delete`](https://github.com/facebook/rocksdb/blob/master/db/db_impl.h#L86)自体は `Status` を返すので判別可能)。
さらに残念なことに[`keyMayExist`](https://github.com/facebook/rocksdb/blob/master/db/db_impl.h#L135)のラッパも存在しないので `get` をつかってチェックしましょう。

``` rust
    fn delete<'a>(&self, key: &'a [u8]) -> Result<bool> {
        let exists = self.db.get(key)?.is_some();
        self.db.delete(key).map(|()| exists)
    }
```

あとは出力のために `CommandRet` に `to_vec` を用意し、

``` rust
impl<'a> CommandRet<'a> {
    fn to_vec(&self) -> Vec<u8> {
        use self::CommandRet::*;
        match *self {
            Stored => b"STORED\r\n".to_vec(),
            Got(ref results) => {
                use std::str::from_utf8;
                let mut ret = Vec::new();
                for &(ref key, ref value) in results {
                    ret.extend(
                        format!(
                            "VALUE {} {} {}\r\n",
                            from_utf8(key).unwrap(),
                            value.flags,
                            value.data.len()
                        ).as_bytes(),
                    );
                    ret.extend(&value.data);
                    ret.extend(b"\r\n");
                }
                ret.extend(b"END\r\n");
                ret
            }
            Deleted => b"DELETED\r\n".to_vec(),
            NotFound => b"NOT_FOUND\r\n".to_vec(),
        }
    }
}

```

`Display` を実装してあげます。

行儀が悪いですがデバッグでしか使わないので `from_utf8` して `unwrap` しちゃいます。

``` rust
impl<'a> fmt::Display for CommandRet<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::str::from_utf8;
        write!(f, "{}", from_utf8(&self.to_vec()).unwrap())
    }
}
```

これで先程の `main` が動きます。

``` rust
$ cargo run
query: Set { key: [107, 101, 121], value: Value { data: [118, 97, 108, 117, 101], flags: 0, exptime: 0 } }
STORED

query: Get { keys: [[107, 101, 121]] }
VALUE key 0 5
value
END

query: Get { keys: [[110, 111, 32, 115, 117, 99, 104, 32, 107, 101, 121]] }
END

query: Delete { key: [107, 101, 121] }
DELETED

query: Get { keys: [[107, 101, 121]] }
END

query: Delete { key: [107, 101, 121] }
NOT_FOUND


```

よし。

## プロトコル
`set key 0 0 value\r\n` や `get key*` なんかのプロトコルをTCP経由で喋ります。
やや大仰ですがTCPの扱いに `tokio` を、パーサに `nom` を使います。`tokio`を使うとバッファリングを考えなくてよくなります。

実行部分は作ったのでサービス部分はこのコードで十分です。

``` rust
use tokio_service::Service;
use futures::{Future, IntoFuture};

struct MemcachedServer {
    engine: Engine,
}

impl Service for MemcachedServer {
    type Request = Command;
    type Response = CommandRet;
    type Error = io::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error>>;

    fn call(&self, cmd: Self::Request) -> Self::Future {
        Box::new(
            self.engine
                .exec(cmd)
                .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
                .into_future(),
        )
    }
}
```

### Parser
実際のパーサを作ります。これはRocksDBへのHello, Worldなので適当に。

``` rust
use std::str;
use std::str::FromStr;
use nom::{digit, IResult, alphanumeric, space};

named!(parse_cmd<&[u8], Command>, alt!(parse_set | parse_get | parse_delete));
named!(parse_set<Command>, do_parse!(
    tag!(b"set") >> space >>
        key: alphanumeric >> space >>
        flags: parse_u32 >> space >>
        exptime: parse_i64 >> space >>
        len: parse_u32 >>
        tag!(b"\r\n") >>
        data: take!(len) >>
        tag!(b"\r\n") >>
        ({
            let data: &[u8] = data;
            Command::Set {
                key: key.to_vec(),
                value: Value {
                    flags: flags,
                    exptime: exptime,
                    data: data.to_vec(),
                }
            }})
));

named!(parse_get<Command>, do_parse!(
    tag!(b"get") >>
        space >>
        keys: separated_nonempty_list!(space, alphanumeric) >>
        tag!(b"\r\n") >>
        (Command::Get {
            keys: keys.iter().map(|k| k.to_vec()).collect(),
        })
));

named!(parse_delete<Command>, do_parse!(
    tag!(b"delete") >>
        space >>
        key: alphanumeric >>
        tag!(b"\r\n") >>
        (Command::Delete {
            key: key.to_vec(),
        })
));

named!(parse_u32<u32>, map_res!(map_res!(digit, str::from_utf8), FromStr::from_str));
named!(parse_i64<i64>, map_res!(map_res!(recognize!(
    do_parse!(opt!(tag!(b"-")) >> digit >> ())),
                                         str::from_utf8),
                                FromStr::from_str));

#[test]
fn test_parser() {
    assert_eq!(parse_cmd(b"delete key\r\n"),
               IResult::Done(&b""[..], Command::Delete{key: b"key".to_vec()}));
    assert_eq!(parse_cmd(b"get key1 key2\r\n"),
               IResult::Done(&b""[..], Command::Get{keys: vec![b"key1".to_vec(), b"key2".to_vec()]}));
    assert_eq!(parse_cmd(b"set key 1 0 5\r\nhello\r\n"),
               IResult::Done(&b""[..], Command::Set {key: b"key".to_vec(), value: Value {
                   exptime: 0,
                   flags: 1,
                   data: b"hello".to_vec(),
               }}));
}
```

はい、できました。

### Codec

パーサを使ってtokioのCodecとProtoを実装します。multiplexはしたくないのでpipelineで。


``` rust
use bytes::BytesMut;
use tokio_io::codec::{Encoder, Decoder};
use tokio_proto::pipeline::ServerProto;

struct MemcachedCodec;
impl Encoder for MemcachedCodec {
    type Item = CommandRet;
    type Error = io::Error;

    fn encode(&mut self, cmd: Self::Item, buf: &mut BytesMut) -> io::Result<()> {
        Ok(buf.extend(cmd.to_vec()))
    }
}

impl Decoder for MemcachedCodec {
    type Item = Command;
    type Error = io::Error;

    fn decode(
        &mut self,
        buf: &mut BytesMut,
    ) -> ::std::result::Result<Option<Self::Item>, Self::Error> {
        let (read, cmd) = match parse_cmd(buf) {
            IResult::Done(rest, cmd) => {
                let read = buf.len() - rest.len();
                (read, cmd)
            }
            IResult::Incomplete(_) => return Ok(None),
            IResult::Error(_) => {
                return Err(io::Error::new(io::ErrorKind::Other, "invalid protocol"))
            }
        };
        buf.advance(read);
        Ok(Some(cmd))
    }
}

struct MemcachedProto;
impl<T: AsyncRead + AsyncWrite + 'static> ServerProto<T> for MemcachedProto {
    // For this protocol style, `Request` matches the `Item` type of the codec's `Decoder`
    type Request = Command;

    // For this protocol style, `Response` matches the `Item` type of the codec's `Encoder`
    type Response = CommandRet;

    // A bit of boilerplate to hook in the codec:
    type Transport = Framed<T, MemcachedCodec>;
    type BindTransport = ::std::result::Result<Self::Transport, io::Error>;
    fn bind_transport(&self, io: T) -> Self::BindTransport {
        Ok(io.framed(MemcachedCodec))
    }
}
```

### Main

最後にmainをサーバに書き換えます。

``` rust
fn main() {
    // Specify the localhost address
    let addr = "0.0.0.0:12345".parse().unwrap();

    // The builder requires a protocol and an address
    let server = TcpServer::new(MemcachedProto, addr);

    // We provide a way to *instantiate* the service for each new
    // connection; here, we just immediately return a new instance.
    server.serve(|| {
        Ok(MemcachedServer {
            engine: Engine::new("path/for/rocksdb/storage").unwrap(),
        })
    });
}
```

動きます。

``` rust
$ cargo run
# 別ターミナルで
$ telnet localhost 12345
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
set key 0 0 5
hello
STORED
get key
VALUE key 0 5
hello
END
delete key
DELETED
get key
END
```


良かったですね

# 課題
へっぽこmemcachedサーバは置いとくとして、RustのRocksDBラッパには問題がありましたね。

* keyMayExistがない
* deleteがstatusを見ない

さらに、他の問題もあります。

* どうやら非同期クエリもできるらしい？だったらTokioと相性よさそう
* 複数コマンドを発行しているが、スレッドセーフでない。[トランザクション](https://github.com/facebook/rocksdb/wiki/Transactions)が必要だが、Rustのラッパが存在しない


今回はHello Worldなのでまだラッパの方の問題にばかり目が行きますがもう少し掘ると何か出てくるかもしれません。
今日はこの辺で。

