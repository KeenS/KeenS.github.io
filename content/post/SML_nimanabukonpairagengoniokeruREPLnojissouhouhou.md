---
categories: [SML, SMLSharp, Advent Calendar, Advent Calendar 2015, 言語処理系]
date: 2015-12-12T19:42:52+09:00
title: SML#に学ぶコンパイラ言語におけるREPLの実装方法
---

このエントリは[言語実装 Advent Calendar 2015](http://qiita.com/advent-calendar/2015/lang_dev)16日目の記事です。  
前: [SML#でJITコンパイラを作る軽い話]()  

κeenです。かねてより気になっていたREPLの実装方法について、SML#のソースコードを読んだのでその話でもします。
<!--more-->
さて、ソースコードを読みながらやっていきましょう。SML# 2.0.0のソースコードです。

REPLのエントリポイントは[src/compiler/main/main/SimpleMain.smlL949](https://github.com/smlsharp/smlsharp/blob/master/src/compiler/main/main/SimpleMain.sml#L949)です。

```
      | Interactive (options, context) =>
        let
          val newContext =
              Top.loadInteractiveEnv
                {stopAt = Top.NoStop,
                 stdPath = [#systemBaseDir options],
                 loadPath = nil}
                context
                (Filename.concatPath
                   (#systemBaseDir options, Filename.fromString "prelude.smi"))
          val context =
              let
                val context = Top.extendContext (context, newContext)
                val context = Top.incVersion context
              in
                context
              end
          val _ = ReifiedTermData.init (#topEnv context)
                  handle e => raise e
        in
          RunLoop.interactive options context;
          SUCCESS
        end

```

いろいろごちゃごちゃやってますがオプションと共に`RunLoop.interactive`を呼んでるだけです。


さて、RunLoop.smlはSimpleMain.smlと同じディレクトリにあります。`interactive`関数は次のように定義されています。


``` sml
  fun interactive options context =
      let
        ...
        val state = initInteractive ()
        fun loop context input =
            if !(#eof state) then ()
            else
              (Counter.reset();
               NameEvalEnv.intExnConList();
               case run options context input of
                 SUCCESS newContext =>
                 ....
               | FAILED =>
                 loop (Top.incVersion context) (interactiveInput state)
              )
      in
        loop context (interactiveInput state)
      end
```

何やら状態を初期化した後は`interactiveInput`で得られた結果を`run`に渡しているようです。`run`を見ましょう。


``` sml
  fun run ({stdPath, loadPath, LDFLAGS, LIBS, errorOutput, llvmOptions,
            ...}:options)
          context input =
      let
        fun puts s = TextIO.output (errorOutput, s ^ "\n")
        val options = {stopAt = Top.NoStop,
                       baseFilename = NONE,
                       stdPath = stdPath,
                       loadPath = loadPath}
        val ({interfaceNameOpt, ...}, result) =
             Top.compile options context input
             handle e =>
             (
               case e of
                 UserError.UserErrors errs =>
                 app (fn e => puts (userErrorToString e)) errs
               | UserError.UserErrorsWithoutLoc errs =>
                 app (fn (k,e) => puts (userErrorToString (Loc.noloc,k,e))) errs
               | Bug.Bug s => puts ("Compiler bug:" ^ s)
               | exn => raise exn;
               raise CompileError
            )
        val (newContext, module) =
            case result of
              Top.RETURN (newContext, module) => (newContext, module)
            | Top.STOPPED => raise Bug.Bug "run"
      in
        let
          val objfile = TempFile.create ("." ^ SMLSharp_Config.OBJEXT ())
          val asmfile = TempFile.create ("." ^ SMLSharp_Config.ASMEXT ())
          val _ = #start Counter.llvmOutputTimeCounter()
          val _ = LLVM.compile llvmOptions (module, LLVM.AssemblyFile,
                                            Filename.toString asmfile)
          val _ = LLVM.compile llvmOptions (module, LLVM.ObjectFile,
                                            Filename.toString objfile)
          val _ = #stop Counter.llvmOutputTimeCounter()
          val _ = LLVM.LLVMDisposeModule module
          val sofile = TempFile.create (SMLSharp_Config.DLLEXT ())
          val ldflags =
              case SMLSharp_Config.HOST_OS_TYPE () of
                SMLSharp_Config.Unix => nil
              | SMLSharp_Config.Cygwin =>
                ["-Wl,-out-implib,"
                 ^ Filename.toString (Filename.replaceSuffix "lib" sofile)]
              | SMLSharp_Config.Mingw =>
                ["-Wl,--out-implib="
                 ^ Filename.toString (Filename.replaceSuffix "lib" sofile)]
          val libfiles =
              case SMLSharp_Config.HOST_OS_TYPE () of
                SMLSharp_Config.Unix => nil
              | SMLSharp_Config.Cygwin =>
                map (fn x => Filename.toString (Filename.replaceSuffix "lib" x))
                    (!loadedFiles)
              | SMLSharp_Config.Mingw =>
                map (fn x => Filename.toString (Filename.replaceSuffix "lib" x))
                    (!loadedFiles)
          val _ = BinUtils.link
                    {flags = SMLSharp_Config.RUNLOOP_DLDFLAGS () :: LDFLAGS
                             @ ldflags,
                     libs = libfiles @ LIBS,
                     objects = [objfile],
                     dst = sofile,
                     useCXX = false,
                     quiet = not (!Control.printCommand)}
          val so = DynamicLink.dlopen' (Filename.toString sofile,
                                        DynamicLink.GLOBAL,
                                        DynamicLink.NOW)
                   handle OS.SysErr (msg, _) => raise DLError msg
          val {mainSymbol, stackMapSymbol, codeBeginSymbol, ...} =
              GenerateMain.moduleName (interfaceNameOpt, #version context)
          val smap = DynamicLink.dlsym' (so, stackMapSymbol)
                     handle OS.SysErr (msg, _) => raise DLError msg
          val base = DynamicLink.dlsym' (so, codeBeginSymbol)
                     handle OS.SysErr (msg, _) => raise DLError msg
          val _ = sml_register_stackmap (smap, base)
          val ptr = DynamicLink.dlsym (so, mainSymbol)
                    handle OS.SysErr (msg, _) => raise DLError msg
          (*
           * Note that "ptr" points to an ML toplevel code. This toplevel code
           * should be called by the calling convention for ML toplevels of
           * ML object files.  __attribute__((fastcc,no_callback)) is an ad
           * hoc way of yielding this convention code; no_callback avoids
           * calling sml_control_suspend.  If we change how to compile
           * attributes in the future, we should revisit here and update the
           * __attribute__ annotation.
           *)
          val mainFn =
              ptr : _import __attribute__((fastcc,no_callback)) () -> ()
        in
          loadedFiles := sofile :: !loadedFiles;
          mainFn () handle e => raise UncaughtException e;
          SUCCESS newContext
        end
        handle e =>
          (
            case e of
              UserError.UserErrors errs =>
              app (fn e => puts (userErrorToString e)) errs
            | UserError.UserErrorsWithoutLoc errs =>
              app (fn (k,e) => puts (userErrorToString (Loc.noloc,k,e))) errs
            | DLError s =>
              puts ("failed dynamic linking. Perhaps incorrect name in _import declaration: " ^ s)
            | UncaughtException exn =>
              puts ("uncaught exception " ^ exnMessage exn)
            | CoreUtils.Failed {command, message} =>
              (puts ("command failed: " ^ command); puts message)
            | _ => raise e;
            FAILED
          )
      end
      handle CompileError => FAILED
```

デカいですね。どうやら`run`がREPLの本体のようです。少しずつ見ていきます。


まずはコンパイルしているようです。

``` sml
        val ({interfaceNameOpt, ...}, result) =
             Top.compile options context input
```

そして結果からcontextとmoduleを取り出します。

``` sml
        val (newContext, module) =
            case result of
              Top.RETURN (newContext, module) => (newContext, module)
            | Top.STOPPED => raise Bug.Bug "run"
```

恐らくcontextが変数名とかを持っているのでしょう。
moduleに関しては`Top.compile`が最後にLLVMEmitをしてるのでLLVMのmodule(コンパイルされる最大単位)のことだと思います。だとするとここまではまだコードはメモリ上にあるはずです。


moduleを取り出した後どうしてるかというとオブジェクトファイルを作っているようです。ここでファイルに書き出されました。


```
          val objfile = TempFile.create ("." ^ SMLSharp_Config.OBJEXT ())
          val asmfile = TempFile.create ("." ^ SMLSharp_Config.ASMEXT ())
          val _ = #start Counter.llvmOutputTimeCounter()
          val _ = LLVM.compile llvmOptions (module, LLVM.AssemblyFile,
                                            Filename.toString asmfile)
          val _ = LLVM.compile llvmOptions (module, LLVM.ObjectFile,
                                            Filename.toString objfile)
          val _ = #stop Counter.llvmOutputTimeCounter()
          val _ = LLVM.LLVMDisposeModule module
```

最後に呼んでるDisposeModuleはデストラクタですかね。ちゃんとお片付けしてます。

さらに、ダイナミックリンクファイルを作るようです。

``` sml
          val sofile = TempFile.create (SMLSharp_Config.DLLEXT ())
          val ldflags =
              case SMLSharp_Config.HOST_OS_TYPE () of
                SMLSharp_Config.Unix => nil
              | SMLSharp_Config.Cygwin =>
                ["-Wl,-out-implib,"
                 ^ Filename.toString (Filename.replaceSuffix "lib" sofile)]
              | SMLSharp_Config.Mingw =>
                ["-Wl,--out-implib="
                 ^ Filename.toString (Filename.replaceSuffix "lib" sofile)]
          val libfiles =
              case SMLSharp_Config.HOST_OS_TYPE () of
                SMLSharp_Config.Unix => nil
              | SMLSharp_Config.Cygwin =>
                map (fn x => Filename.toString (Filename.replaceSuffix "lib" x))
                    (!loadedFiles)
              | SMLSharp_Config.Mingw =>
                map (fn x => Filename.toString (Filename.replaceSuffix "lib" x))
                    (!loadedFiles)
          val _ = BinUtils.link
                    {flags = SMLSharp_Config.RUNLOOP_DLDFLAGS () :: LDFLAGS
                             @ ldflags,
                     libs = libfiles @ LIBS,
                     objects = [objfile],
                     dst = sofile,
                     useCXX = false,
                     quiet = not (!Control.printCommand)}
```

この時、Windowsでは何かしらの制約があるのか過去にリンクしたダイナミックリンクファイル(libfiles)も今回作るダイナミックリンクファイルにリンクしているようです。


さて、ダイナミックリンクファイルを作った後は`dlopen`を使ってダイナミックリンクします。


``` sml
          val so = DynamicLink.dlopen' (Filename.toString sofile,
                                        DynamicLink.GLOBAL,
                                        DynamicLink.NOW)
                   handle OS.SysErr (msg, _) => raise DLError msg
```

その後はエントリポイントを捜してインポート、

``` sml
          val {mainSymbol, stackMapSymbol, codeBeginSymbol, ...} =
              GenerateMain.moduleName (interfaceNameOpt, #version context)
          val smap = DynamicLink.dlsym' (so, stackMapSymbol)
                     handle OS.SysErr (msg, _) => raise DLError msg
          val base = DynamicLink.dlsym' (so, codeBeginSymbol)
                     handle OS.SysErr (msg, _) => raise DLError msg
          val _ = sml_register_stackmap (smap, base)
          val ptr = DynamicLink.dlsym (so, mainSymbol)
                    handle OS.SysErr (msg, _) => raise DLError msg
```

そして実行します。

``` sml
          val mainFn =
              ptr : _import __attribute__((fastcc,no_callback)) () -> ()
        in
          loadedFiles := sofile :: !loadedFiles;
          mainFn () handle e => raise UncaughtException e;
          SUCCESS newContext
```

結果を印字しているところが見当らないのですが吐かれたコードに印字部分も入っているんですかね。

あとは`interactive`に返ってコンテキストを拡張したりバージョンをインクリメントしたりします。

``` sml
                   val context = Top.extendContext (context, newContext)
                   val context = Top.incVersion context
```

バージョンによってシンボル名を変えることで何度もsoファイルをロードしても大丈夫なようにしてるんですかね。

ということでREPLの実装は入力を一旦ダイナミックリンクとしてコンパイルしてからロードしているようでした。

# おまけ
REPLの入力部分は次のようになっています。

``` sml

  fun initInteractive () =
      let
        val lineCount = ref 1
        val eof = ref false
        fun read (isFirst, _:int) =
            let
              val prompt = if isFirst then "# " else "> "
              val _ = TextIO.output (TextIO.stdOut, prompt)
              val _ = TextIO.flushOut TextIO.stdOut
              val line = TextIO.inputLine TextIO.stdIn
              val _ = lineCount := !lineCount + 1
            in
              case line of NONE => (eof := true; "") | SOME s => s
            end
      in
        {lineCount = lineCount, eof = eof, read = read}
      end

```

結構小さいので色々Hack出来ます。例えば[hskさんのように](https://github.com/smlsharp/smlsharp/pull/13)REPLを付けることも出来ます。

このパッチで外部コマンドを呼ぶのが少し嫌な人はさらに`ioctl(2)`を使ってrawモードにするパッチを当てるかlibeditなんかを使ってリッチな入力を実現するかをすればいいでしょう。
個人利用なら(他人に配布しないなら)GPLv3のreadlineも使えるんですかね（ライセンスに詳しくない）。

# まとめ

* REPLの実装にはダイナミックリンクを使う方法があるよ
* SML#のREPLはHack出来そうだよ
