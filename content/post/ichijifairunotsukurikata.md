---
categories: [Rust, Linux]
date: 2015-11-08T21:40:14+09:00
title: 一時ファイルの作り方
---
Rustに[tempfile](https://github.com/Stebalien/tempfile)というライブラリがある。その名の通りテンポラリファイルを作るライブラリだ。
必要に迫られてそのライブラリにPRを送ろうとして実装を読んだのだが普段あまり意識しなかったテンポラリファイルの作り方を知ったのでちょっと解説してみる。

Rustはほぼそのままの使い心地でCのライブラリを呼べるのでCが分かる人になら伝わると思う。
<!--more-->
ライブラリの構成として、src/*.rsにOS非依存なコードやユーザ向けのAPIがあり、src/imp/{windows,unix}.rsにOS依存なコードがある。
私はWindowsの実装には興味ないし詳しくもないのでunix向けの実装だけを見る。

imp以下を見る前にこのライブライが提供するAPIを解説しておくと、大きく分けて`TempFile`と`NamedTempFile`がある。無名、Namedどちらにもデフォルトの一時ファイルディレクトリ下に一時ファイルを作るAPIと指定したディレクトリ以下に作るAPIとがある。
`TempFile`が作るファイルは完全に匿名で、ファイルシステムに残らない。I/Oが出来るストリームだけを返す。定期的に/tmp以下を削除するジョブが走っていたとしても安全だし、他のプロセスに中身を読まれる危険性もない。
一方`NamedTempFile`の方は予測不可能なファイル名でファイルを作るものの、ファイルシステムに残るので安全ではない。しかしファイル名を要求する関数や外部プロセスと協調する時に使える。

それぞれで実装を見ていく。

# TempFile
これはLinuxとその他で実装が分かれる。ユーザーAPI側でテンポラリファイルを作るディレクトリの指定は吸収しているのでこちらではただ指定されたディレクトリ下にテンポラリファイルを作ればいい。

## Linux

```rust
#[cfg(target_os = "linux")]
pub fn create(dir: &Path) -> io::Result<File> {
    const O_TMPFILE: libc::c_int = 0o20200000;
    match unsafe {
        libc::open(try!(cstr(dir)).as_ptr(), O_CLOEXEC | O_EXCL | O_TMPFILE | O_RDWR, 0o600)
    } {
        -1 => create_unix(dir),
        fd => Ok(unsafe { FromRawFd::from_raw_fd(fd) }),
    }
}
```

Linux 3.11から`O_TMPFILE`が入ったので実装は直接的だ。`O_TMPFILE`はテンポラリファイル向けにファイルシステムに名前が登録されない匿名のファイルを作る。Lispのgensymに似ている。
O_EXCLでリンクによる別名での参照を回避し、Linux 2.6.23以後で入ったO_CLOEXECで`exec(2)`した時にfdを閉じるようにする。そうすれば関係のないプログラムからファイルが参照されるのを防げる。

このライブラリコールが成功すればそのままFileを返す。

Linux 3.11未満やO_TMPFILEをサポートしないファイルシステムだったら失敗し、-1が返るのでLinuxに依存しない汎用の`create_unix`にフォールバックする。

## Unix

Unix向けの実装は恐らくPOSIX互換なシステムなら全てで動く。


```rust
#[cfg(not(target_os = "linux"))]
pub fn create(dir: &Path) -> io::Result<File> {
    create_unix(dir)
}

fn create_unix(dir: &Path) -> io::Result<File> {
    for _ in 0..::NUM_RETRIES {
        let tmp_path = dir.join(&tmpname());
        return match create_named(&tmp_path) {
            Ok(file) => {
                // I should probably tell the user this failed but the temporary file creation
                // didn't really fail...
                let _ = fs::remove_file(tmp_path);
                Ok(file)
            },
            Err(ref e) if e.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(e) => Err(e),
        }
    }
    Err(io::Error::new(io::ErrorKind::AlreadyExists,
                       "too many temporary directories already exist"))
}
```

汎用の`create_unix`では完全に無名のテンポラリファイルを作るのを諦めて、一旦ランダム生成な名前のファイルを作って開き、成功したらそのファイルを削除する。
1つでも開いてるプロセスがある時に削除されるとUnixのファイルはそのプロセスからは見れるが他のプロセスからは見れない状態になるので色々便利に使われている。

`create_named`は後程見るとして、ランダム生成なファイル名だと稀に既に存在するファイル名と被る可能性があるので何度かリトライしている。
リトライ回数はsrc/lib.rsで定義されていて、

```rust
const NUM_RETRIES: u32 = 1 << 31;
```

となっている。思ったよりえけつない回数リトライしている。もちろん、ファイル名が被った以外の理由でファイル生成が失敗したのならリトライに意味はないので大人しくreturnしている。

で、`create_named`だが

```rust
pub fn create_named(path: &Path) -> io::Result<File> {
    match unsafe {
        libc::open(try!(cstr(&path)).as_ptr(), O_CLOEXEC | O_EXCL | O_RDWR | O_CREAT, 0o600)
    } {
        -1 => Err(io::Error::last_os_error()),
        fd => Ok(unsafe { FromRawFd::from_raw_fd(fd) }),
    }
}
```

となっている。単に`create`のフラグの`O_TMPFILE`が`O_CREAT`になっただけ。

# NamedTempFile

こちらはユーザーAPIの中から直接OS互換用コードを呼んでいる。src/named.rsに実装がある。

```rust
    /// Create a new temporary file in the specified directory.
    pub fn new_in<P: AsRef<Path>>(dir: P) -> io::Result<NamedTempFile> {
        for _ in 0..::NUM_RETRIES {
            let path = dir.as_ref().join(&util::tmpname());
            return match imp::create_named(&path) {
                Ok(file) => Ok(NamedTempFile(Some(NamedTempFileInner { path: path, file: file, }))),
                Err(ref e) if e.kind() == io::ErrorKind::AlreadyExists => continue,
                Err(e) => Err(e),
            }
        }
        Err(io::Error::new(io::ErrorKind::AlreadyExists,
                           "too many temporary directories already exist"))
    }
```

返り値をユーザAPI向けにラップしている以外は無名の一時ファイルと変わらない。余談だがここで使われているファイル名にユーザが干渉することが出来ず、完全にランダムなファイル名になっている。
ファイルの末尾(例えば拡張子)や先頭(例えばapp_nameなどの識別子)を挟めない。私が今作業してるパッチはその辺をもう少し自由にするものだ。

閑話休題。似たような実装が2つもあるのは気になるので一応windows向けの実装も読んでみる。


```rust
const ACCESS: DWORD     = winapi::FILE_GENERIC_READ
                        | winapi::FILE_GENERIC_WRITE;
const SHARE_MODE: DWORD = winapi::FILE_SHARE_DELETE
                        | winapi::FILE_SHARE_READ
                        | winapi::FILE_SHARE_WRITE;
const FLAGS: DWORD      = winapi::FILE_ATTRIBUTE_HIDDEN
                        | winapi::FILE_ATTRIBUTE_TEMPORARY;


fn to_utf16(s: &Path) -> Vec<u16> {
    s.as_os_str().encode_wide().chain(Some(0).into_iter()).collect()
}

fn win_create(path: &Path,
                     access: DWORD,
                     share_mode: DWORD,
                     disp: DWORD,
                     flags: DWORD) -> io::Result<File> {

    let path = to_utf16(path);
    let handle = unsafe {
        CreateFileW(
            path.as_ptr(),
            access,
            share_mode,
            0 as *mut _,
            disp,
            flags,
            ptr::null_mut())
    };
    if handle == winapi::INVALID_HANDLE_VALUE {
        Err(io::Error::last_os_error())
    } else {
        Ok(unsafe { File::from_raw_handle(handle as RawHandle) })
    }
}

pub fn create_named(path: &Path) -> io::Result<File> {
    win_create(path, ACCESS, SHARE_MODE, winapi::CREATE_NEW, FLAGS)
}

pub fn create(dir: &Path) -> io::Result<File> {
    for _ in 0..::NUM_RETRIES {
        return match win_create(
            &dir.join(&tmpname()),
            ACCESS,
            SHARE_MODE,
            winapi::CREATE_NEW,
            FLAGS | winapi::FILE_FLAG_DELETE_ON_CLOSE)
        {
            Ok(f) => Ok(f),
            Err(ref e) if e.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(e) => Err(e),
        };
    }
    Err(io::Error::new(io::ErrorKind::AlreadyExists,
                       "too many temporary directories already exist"))
}
```

Windows向けの実装は必ずファイル名を指定する必要があり、リトライが必要そう？`NamedTempFile`を作るには(理論的に)どの実装でもリトライが必要だからユーザAPI側で纏めたのかな？


# まとめ

* テンポラリファイルを作るならまずは`O_TMPFILE`を試す
* ダメならランダム生成なファイル名で成功するまでリトライ、成功したら即座にファイルを削除

作業中のコードをpushし忘れて土日に進捗出来なかったのでまたお茶を濁すはめになった。
