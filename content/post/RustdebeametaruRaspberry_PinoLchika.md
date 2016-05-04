---
categories: [Rust, Raspberry Pi, ベアメタル]
date: 2016-05-04T11:43:26+09:00
title: RustでベアメタルRaspberry PiのLチカ
---

κeenです。GWもくもく会で机の片隅で埃被ってたRSPiのベアメタルを触ってみたのでその成果報告を。
ベアメタルについて知らない人でも読めるように書きます。そしてアセンブラもCも出てきませんし、半田付けも必要ありません。
Rustしか使いません。

Rustは分かるけどLチカは全く知らない、けど興味のある方は是非読んでみて下さい。
<!--more-->

# ベアメタルって？
普段我々が使っているパソコンはOSの上で動いています。
OSはざっくりいうとハードウェアへのアクセスを抽象化し、プロセスという単位でプログラムの実行を分離します。
ベアメタルはそのOSがない状態、ハードウェアの初期化やプロトコルなどは自分でやるしメモリ管理だとかも自分でやる環境です。
丁度ハードウェアが抽象化されてなくて機械が生で見えているようなのでこう呼ぶようです。

OSがないのは中々イメージつきづらいですが、標準出力なんてものはないのでHello Worldも動きませんしメモリも仮想化されていないのでmallocも発効出来ません。
さらにはスタックとヒープみたいな区別もOSが与えてくれるものなので、自分で作らない限りスタック領域も使えません。
自分で書いたプログラムが唯一走るプログラムなのでプロセスの感覚でプログラムを終了させてはいけません。

なんでそんな環境でプログラムしたいかというといくつかモチベーションがありそうです。1つはOSそのものを作ってみたい人が練習がてら。
もう1つにはOSが動かないような小さな環境だとそれしか選択肢がない。

RSPiの場合はOSが動くので選択肢がないという訳ではないのですが私はそういう小さなデバイスを持っていないので実験台としてRSPiを使いました。

さて、Hello Worldすら動かないシステムでの入門的なプログラムは信号のON/OFFを切り替えてあげるだけで出来るLEDをチカチカさせるプログラムという訳です。

# Raspberry Pi
言わずと知れた名刺サイズの小型コンピュータ、Raspberry Piです。
Raspberry Piと一口にいっても[色々種類がある](https://ja.wikipedia.org/wiki/Raspberry_Pi)のですが、私が持っているのはRaspberry Pi 1 Model Bの後期モデルです(Raspberry Pi 1 Model Bは販売途中で仕様がアップグレードしてメモリが増えた)。

ARMv6アーキテクチャのチップ(ARM1176JZF-S)で動いていて、SDカードから起動します。特別なライターなどは必要なく、PCからSDカードに書き込んでRSPiに挿して電源を繋ぐだけで簡単に起動出来る訳です。
さらに配線とかが必要なGPIOに加えてボードにLEDもついているので物理の知識必要なくて今回のようにLチカだけをやりたいなら手軽に遊べます。

# 準備

* Raspberry Pi 1 Model B
* USB - micro USBケーブル(Raspberry Piへの給電用)
* SDカード(FAT32でフォーマットしておく)
* GCCのARM向けクロスコンパイラ(後述)
* RustのARM向けクロスコンパイラ(後述)
* ブートローダ(後述)

## GCCのクロスコンパイラ
GCCのクロスコンパイラは[GCC ARM Embedded in Launchpad](https://launchpad.net/gcc-arm-embedded)というプロジェクトがあるので簡単に用意出来ます。Ubuntu環境なら

```
sudo apt install gcc-arm-linux-gnueabihf
```

でインストールできます。どうやらRSPiには不動小数点数計算ユニット(FPU)がついているようなのでhf(Hardware Float)で大丈夫みたいです。
HomebrewやPacmanにもパッケージはあるようです。

## Rustのクロスコンパイラ
次にRustのクロスコンパイラですが、[multirust](https://github.com/brson/multirust)を使うと簡単にインストール出来ます。今回はunstable機能も使うのでnightlyコンパイラを使います。
因みに私が昔インストールしたmultirustだと古くて以下のコマンドが実行出来なかったので出来ない方はmultirustをアップデートして試してみて下さい。

```
# インストール可能なターゲットを確認
$ multirust list-available-targets nightly
aarch64-apple-ios
aarch64-unknown-linux-gnu
arm-linux-androideabi
arm-unknown-linux-gnueabi
arm-unknown-linux-gnueabihf
armv7-apple-ios
armv7-unknown-linux-gnueabihf
armv7s-apple-ios
i386-apple-ios
i586-pc-windows-msvc
i586-unknown-linux-gnu
i686-apple-darwin
i686-pc-windows-gnu
i686-pc-windows-msvc
i686-unknown-linux-gnu
mips-unknown-linux-gnu
mips-unknown-linux-musl
mipsel-unknown-linux-gnu
mipsel-unknown-linux-musl
powerpc-unknown-linux-gnu
powerpc64-unknown-linux-gnu
powerpc64le-unknown-linux-gnu
x86_64-apple-darwin
x86_64-apple-ios
x86_64-pc-windows-gnu
x86_64-pc-windows-msvc
x86_64-rumprun-netbsd
x86_64-unknown-freebsd
x86_64-unknown-linux-gnu
x86_64-unknown-linux-musl
x86_64-unknown-netbsd
# インストール
$ multirust add-target nightly arm-unknown-linux-gnueabihf
```

## ブートローダ
まず、ブートローダが何をするものなのかから説明しましょう。興味がない人は飛ばして下さい。

### 説明
Raspberry PiにはCPUとGPUが両方ついているのでやや面倒です。

CPU/GPUは電源がONになってすぐは何の機能もない、ただの命令を実行する機械です。メモリにすらアクセス出来ません。RSPiにSDカードを挿したところでSDカードのプロトコルを喋らないとそこに書いてあるプログラムが読めません。
SDカードのプロトコルを喋るプログラム(BIOS)は、RSPiに組込みで入っています。これは普段我々が使うメモリ(RAM)とは別の場所(ROM)に入っています。

最初はGPUがそいつを起動し、(この時点ではCPUは起動してない)SDカードを読みにいきます。名前決め打ちでbootcode.binという名前のプログラムを、RAMではなくGPU(のL2キャッシュ)に直接ロードして実行します(メモリはまだ使えません)。
GPUのキャッシュは非常に小さいのでbootcode.binは小さなプログラムでないといけません。


bootcode.binはRAMを有効にしてstart.elfをRAMに読み込み、GPUがそれを実行します。start.elfはRAMを使えるのである程度大きくても構いません。

start.elfはconfig.txt、cmdline.txtとkernel.imgを読みにいき、kernel.imgを0x8000番地のメモリに配置します。そしてconfgやcmdlineに基いてCPUを設定してkernel.imgに引数を渡しつつ実行します。

このkernel.imgは普段はLinuxカーネルなどのカーネル、今回は我々が作るLチカのプログラムです。

参考: [Raspberry Piのブートプロセスメモ - φ(・・*)ゞ ｳｰﾝ　カーネルとか弄ったりのメモ](http://kernhack.hatenablog.com/entry/2014/01/11/102237)
昔はloader.binもあったようですが今はなくて大丈夫なようです。


### インストール
bootcode.bin, start.elfは[こちら](https://github.com/raspberrypi/firmware/tree/master/boot)から入手出来ます。config.txtとcmdline.txtはなくていいようです。

こいつらはSDカードに放り込んでおきます。

# 環境確認

まずはコンパイラツールチェーンが正常に動くか確認しましょう。
以下にrustコードを。


```rust
#![feature(lang_items, asm)]
#![crate_type = "staticlib"]
#![no_std]

#[no_mangle]
pub extern fn main() {
    loop{}
}


#[no_mangle]
pub extern fn _sbrk() {}
#[no_mangle]
pub extern fn _exit() {}
#[no_mangle]
pub extern fn _kill() {}
#[no_mangle]
pub extern fn _getpid() {}


#[lang = "eh_personality"]
extern fn eh_personality() {}

#[lang = "panic_fmt"]
extern fn panic_fmt() {}

```

さて、今回作るのは `#![crate_type = "staticlib"]` を使って静的リンクライブラリを作れと指定しています。そして `#[no_mangle] pub extern fn main() {` でC言語と同じような規則でmain関数を定義する(アセンブラからmain関数を実行出来るようにする)と指定しています。
ベアメタル環境なのでファイルだとか諸々のものはなく、stdを使えないので `#![no_std]` を指定してstdを使わずにcoreライブラリを使うようにしました。その代わり`#[lang = "eh_personality"] extern fn eh_personality() {}` と `#[lang = "panic_fmt"] extern fn panic_fmt() {}` を実装してあげないといけません(この実装はあまりよろしくないのですが今回はとりあえずコンパイルが通るようにということでこうしてます)。

そしてよく分かってないのですがRustが `_sbrk` 、 `_exit` 、 `_kill` 、 `_getpid` のシンボルを捜しにいくのでとりあえずスタブを挿してます。

no_mangle, no_std, lang_itemsについて詳しくは[ドキュメント](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/)をご覧下さい。
[他言語関数インターフェイス](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/ffi.html)、[No stdlib](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/no-stdlib.html)、[言語アイテム](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/lang-items.html)あたりが参考になるかと思います。



以下のようにコンパイル出来ればRustはOKです。

```
rustc --target arm-unknown-linux-gnueabihf --emit=obj test.rs
```

次に、elfファイルを作ります。これは以下のコマンドで出来ます。Cのクロスコンパイラを使っていますね。

```
$ arm-none-eabi-gcc -O2 -mfpu=vfp -mfloat-abi=hard -march=armv6zk -mtune=arm1176jzf-s -nostartfiles test.o -o kernel.elf
/usr/lib/gcc/arm-none-eabi/4.9.3/../../../arm-none-eabi/bin/ld: warning: cannot find entry symbol _start; defaulting to 0000000000008000
```

-mで始まるオプションはRaspberry Piのチップ固有の指定、 -nostartfiles はベアメアルで動かすのに必要な指定ですね。

最後にelfファイルからimgファイルを作ります。

```
arm-none-eabi-objcopy kernel.elf -O binary kernel.img
```

私もよく分かってないのですがobjファイルが不完全なバイナリファイル、elfが完全なバイナリファイル、imgがelfを実際のメモリに展開した時のものそのままなんですかね。
逆にいうとOSのない環境で動かすには生のimgが欲しいけどそれを作るのは難しいから一旦elfを作ってそこからimgを生成している？


さて、とりあえずここまで来れば環境は整いました。あとはプログラムを書くだけです。

# Lチカ
RSPiにくっついているLEDはデバイスとしてはGPIOのように見えるみたいです。
そしてGPIOはMemory Mapped IO、特定の位置のメモリを読み書きすることでデバイスをいじれるようになってるみたいです。
このセットアップは誰がやったんだって気になりますがBIOSなんですかね。

さて、この「特定の位置のメモリ」ですが、Raspberry Pi 1だと `0x20200000` から始まるアドレス、のようです。2だと `3F200000` のよう。

そして0x20200000からいくつかの32bitレジスタが配列のように並んでいる訳ですがこれまたRSPi+やRSP 2とRSPi 1はLEDに対応するレジスタがやや違うようです。

面倒なのでコピペで済ませると(私はCのコードからRustに移植しました。)


``` rust
// Raspberry Pi2
//pub const GPIO_BASE: u32 = 0x3F200000;
// other
pub const GPIO_BASE: u32 = 0x20200000;



// Raspberrp Pi+ or Raspberry Pi2
//pub const LED_GPFSEL: isize   = GPIO_GPFSEL4;
//pub const LED_GPFBIT: i32     = 21;
//pub const LED_GPSET: isize    = GPIO_GPSET1;
//pub const LED_GPCLR: isize    = GPIO_GPCLR1;
//pub const LED_GPIO_BIT: isize = 15;
//  other
pub const LED_GPFSEL: isize = GPIO_GPFSEL1;
pub const LED_GPFBIT: i32 = 18;
pub const LED_GPCLR: isize = GPIO_GPCLR0;
pub const LED_GPSET: isize = GPIO_GPSET0;
pub const LED_GPIO_BIT: i32 = 16;

pub const GPIO_GPFSEL0: isize   = 0;
pub const GPIO_GPFSEL1: isize   = 1;
pub const GPIO_GPFSEL2: isize   = 2;
pub const GPIO_GPFSEL3: isize   = 3;
pub const GPIO_GPFSEL4: isize   = 4;
pub const GPIO_GPFSEL5: isize   = 5;
pub const GPIO_GPSET0: isize    = 7;
pub const GPIO_GPSET1: isize    = 8;
pub const GPIO_GPCLR0: isize    = 10;
pub const GPIO_GPCLR1: isize    = 11;
pub const GPIO_GPLEV0: isize    = 13;
pub const GPIO_GPLEV1: isize    = 14;
pub const GPIO_GPEDS0: isize    = 16;
pub const GPIO_GPEDS1: isize    = 17;
pub const GPIO_GPREN0: isize    = 19;
pub const GPIO_GPREN1: isize    = 20;
pub const GPIO_GPFEN0: isize    = 22;
pub const GPIO_GPFEN1: isize    = 23;
pub const GPIO_GPHEN0: isize    = 25;
pub const GPIO_GPHEN1: isize    = 26;
pub const GPIO_GPLEN0: isize    = 28;
pub const GPIO_GPLEN1: isize    = 29;
pub const GPIO_GPAREN0: isize   = 31;
pub const GPIO_GPAREN1: isize   = 32;
pub const GPIO_GPAFEN0: isize   = 34;
pub const GPIO_GPAFEN1: isize   = 35;
pub const GPIO_GPPUD: isize     = 37;
pub const GPIO_GPPUDCLK0: isize = 38;
pub const GPIO_GPPUDCLK1: isize = 39;
```


こうなります(cfgを使ってないです。ごめんなさい。)。

次にLチカですが、
LEDに対応するGPIOをwriteに初期化する→ループ{LEDに対応するGPIOをセットする → スリープ → LEDに対応するGPIOをクリアする → スリープ}

のプログラムを書きます。

色々試したのですが簡単なプログラムだと最適化で消えてしまうので消えないように工夫する必要があります。

スリープはとりあえずタイマーを使わずに空ループを回すのですが、ループの中身は空インラインアセンブラを使うと消えないようです。

また、メモリへの書き込みは[intrinsic](https://doc.rust-lang.org/core/intrinsics/)の[volatile_store](https://doc.rust-lang.org/core/intrinsics/fn.volatile_store.html)を使います。



ということでコードは以下です。


``` rust
#![feature(lang_items, asm, core_intrinsics)]
#![crate_type = "staticlib"]
#![no_std]

use core::intrinsics::volatile_store;

// Raspberry Pi2
//pub const GPIO_BASE: u32 = 0x3F200000;
// other
pub const GPIO_BASE: u32 = 0x20200000;



// Raspberrp Pi+ or Raspberry Pi2
//pub const LED_GPFSEL: isize   = GPIO_GPFSEL4;
//pub const LED_GPFBIT: i32     = 21;
//pub const LED_GPSET: isize    = GPIO_GPSET1;
//pub const LED_GPCLR: isize    = GPIO_GPCLR1;
//pub const LED_GPIO_BIT: isize = 15;
//  other
pub const LED_GPFSEL: isize = GPIO_GPFSEL1;
pub const LED_GPFBIT: i32 = 18;
pub const LED_GPCLR: isize = GPIO_GPCLR0;
pub const LED_GPSET: isize = GPIO_GPSET0;
pub const LED_GPIO_BIT: i32 = 16;

pub const GPIO_GPFSEL0: isize   = 0;
pub const GPIO_GPFSEL1: isize   = 1;
pub const GPIO_GPFSEL2: isize   = 2;
pub const GPIO_GPFSEL3: isize   = 3;
pub const GPIO_GPFSEL4: isize   = 4;
pub const GPIO_GPFSEL5: isize   = 5;
pub const GPIO_GPSET0: isize    = 7;
pub const GPIO_GPSET1: isize    = 8;
pub const GPIO_GPCLR0: isize    = 10;
pub const GPIO_GPCLR1: isize    = 11;
pub const GPIO_GPLEV0: isize    = 13;
pub const GPIO_GPLEV1: isize    = 14;
pub const GPIO_GPEDS0: isize    = 16;
pub const GPIO_GPEDS1: isize    = 17;
pub const GPIO_GPREN0: isize    = 19;
pub const GPIO_GPREN1: isize    = 20;
pub const GPIO_GPFEN0: isize    = 22;
pub const GPIO_GPFEN1: isize    = 23;
pub const GPIO_GPHEN0: isize    = 25;
pub const GPIO_GPHEN1: isize    = 26;
pub const GPIO_GPLEN0: isize    = 28;
pub const GPIO_GPLEN1: isize    = 29;
pub const GPIO_GPAREN0: isize   = 31;
pub const GPIO_GPAREN1: isize   = 32;
pub const GPIO_GPAFEN0: isize   = 34;
pub const GPIO_GPAFEN1: isize   = 35;
pub const GPIO_GPPUD: isize     = 37;
pub const GPIO_GPPUDCLK0: isize = 38;
pub const GPIO_GPPUDCLK1: isize = 39;

#[no_mangle]
pub extern fn main() {
    // GPIO_BASEを符号無し32bit整数を指すポインタにキャストする
    let gpio = GPIO_BASE as *const u32;
    // offsetを使うことでCの配列のようにポインタの何番地か先のメモリを指せる
    let init   = unsafe {gpio.offset(LED_GPFSEL) as *mut u32};
    let led_on = unsafe { gpio.offset(LED_GPSET) as *mut u32 };
    let led_off = unsafe { gpio.offset(LED_GPCLR) as *mut u32 };

    // LEDのGPIOを書き込みに設定
    unsafe{
        volatile_store(init, *(init) | 1 << LED_GPFBIT);
    }

    loop {
        // LEDをOFFにする
        unsafe {
            volatile_store(led_off, 1 << LED_GPIO_BIT);
        }
        // 適当な長さbusy loopで時間を空ける
        for _ in 1..500000 {
            unsafe { asm!(""); }
        }

        // LEDをONにする
        unsafe {
            volatile_store(led_on, 1 << LED_GPIO_BIT);
        }
        // また適当な長さ空ける
        for _ in 1..500000 {
            unsafe { asm!(""); }
        }
    }
}


#[no_mangle]
pub extern fn _sbrk() {}
#[no_mangle]
pub extern fn _exit() {}
#[no_mangle]
pub extern fn _kill() {}
#[no_mangle]
pub extern fn _getpid() {}


#[lang = "eh_personality"]
extern fn eh_personality() {}

#[lang = "panic_fmt"]
extern fn panic_fmt() {}
```

これをkernel.rsとして保存し、

```
rustc  -O --target arm-unknown-linux-gnueabihf --emit=obj kernel.rs
```

でコンパイルします。どうやら使っていないのにrustが余計なシンボルを付けるようで、-Oをつけてそいつらをoptimize outさせないとコンパイルが通りませんでした。
この辺、コンパイラのバージョンに依存しそうで怖いですね。

さて、あとは先程と同じく

```
$ arm-none-eabi-gcc -O2 -mfpu=vfp -mfloat-abi=hard -march=armv6zk -mtune=arm1176jzf-s -nostartfiles kernel.o -o kernel.elf
$ arm-none-eabi-objcopy kernel.elf -O binary kernel.img
```

でイメージを作ってあげます。

そしたらこのkernel.imgをSDカードに放り込みます。

SDカードの中身はこうなっている筈です。

```
$ ls  /media/kim/6F6F-DCD9
bootcode.bin  kernel.img  start.elf
```



このSDカードをRSPiに挿して、電源を繋いであげるとLEDがすごい勢いでチカチカする筈です。そうなったら成功です。おめでとうございます。


# おわりに
今回のコードは[ここ](https://github.com/KeenS/RaspPi)に置いておきます。とはいってもそのままではなくてCargo化したりMakefileを書いたり色々しています。

今回の作業をするにあたって参考にした資料のURLを挙げておきます。

* [Step01 – Bare Metal Programming in C Pt1 – Valvers](http://www.valvers.com/open-software/raspberry-pi/step01-bare-metal-programming-in-cpt1/)
* [Step02 – Bare Metal Programming in C Pt2 – Valvers](http://www.valvers.com/open-software/raspberry-pi/step02-bare-metal-programming-in-c-pt2/)
* [Raspberry Pi Bare Metal Programming with Rust](http://blog.thiago.me/raspberry-pi-bare-metal-programming-with-rust/)
* [piでベアメタルプログラミング - bobuhiro11's diary](http://blog.bobuhiro11.net/2014/01-13-baremetal.html)
