---
categories: [言語実装, JVM, Advent Calendar, Advent Calendar 2017, 言語実装 Advent Calendar]
date: 2017-12-13T20:32:41+09:00
title: "Graal/Truffleについて軽く"
---
κeenです。これは[言語実装 Advent Calendar 2017](https://qiita.com/advent-calendar/2017/lang_dev)14日目の記事です。
JVMのコンパイラエンジンGraalと高速インタプリタ作成フレームワークのTruffleについて。
<!--more-->
この記事の前に[昨日の記事](/blog/2017/12/12/rpythonnitsuitekaruku/)を読んでおくと理解の助けになるかもしれません。

# Graalについて
[公式ページ](http://www.oracle.com/technetwork/oracle-labs/program-languages/overview/index.html)/[GitHub](https://github.com/graalvm/graal)。

Oracle Labで開発されているJavaのJITエンジンの1つです。
[JVMのコンパイラインタフェース](http://openjdk.java.net/jeps/243)(JVMCI)を利用してJavaでコンパイラを書いたものです。
従来はコンパイル部分はC++で書かれてましたが曰くJavaも十分速くなったし高級で安全なJavaでコンバイラを書いてもいいだろとのこと。
この絶妙にランタイムが拡張可能でかつパフォーマンスを損なわない感じはJavaならではですね。
また、Javaで書かれているのでユーザがJavaで拡張可能でもあり、最適化や機械語生成を拡張できます。

Graalはピーク性能の改善を目標としていて、Scalaアプリケーションなど一部のケースで既に従来のJVMの性能を上回ってるそうです。

GraalをデフォルトでonにしたGraalVMというのもOracle Labから配布されているようです。

# Truffleについて
[GitHub](https://github.com/graalvm/graal/tree/master/truffle)。

JavaのASTインタプリタフレームワークです。
多少言語実装をかじった人ならASTインタプリタは遅いことをご存知かと思いますがTruffleはそのままASTで解釈するだけではありません。
昨日のRPythonのようにASTインタプリタは意味論を与えるのが主な目的で、実行時にはGraalと連携して最適化していきます。

Truffleが行う最適化は[自己最適化](https://dl.acm.org/citation.cfm?doid=2384577.2384587)です。
Javaで実装された各ASTノードが`execute`メソッドを持っていて実行される訳ですが、ASTを実行中に書き換えることで高速化していきます。
抽象的なインタプリタからセマンティクスを得て高速化していく点では似てますが、昨日のRPythonのJITとは違って全てがJVMの実行中に起きるのでちょっと図には書けないですね…。
このゲスト言語のソースコードに合わせた(部分評価した)最適化と評価器そのものの最適化が表裏一体な感じがなんともいえませんね。

さて、Truffleの目的は高速化以外にもあります。polyglot、つまり同じVM上で複数の言語を動かすのです。
現状[Ruby](https://github.com/graalvm/truffleruby)、[Python](https://bitbucket.org/ssllab/zippy)、JavaScript、[R](https://github.com/graalvm/fastr)、[LLVM IR](https://github.com/graalvm/sulong)などが動きます。
なにやらCLI/CLRとIronプロジェクトが去来しますがRuby、Python、JS、RはJVM実装の実績がありますしまあ大丈夫でしょう。
[Truffle Tutorial: Embedding Truffle Languages in Java](http://graalvm.github.io/graal/truffle/javadoc/com/oracle/truffle/tutorial/embedding/package-summary.html)を見ても分かるように相互連携はそれなりに出来るようです。


# Substrate VM
[資料](https://github.com/graalvm/truffleruby/blob/master/doc/user/svm.md)。
あまり情報がないのですが、どうやらTruffleで書かれたインタプリタをコンパイルしてくれるようです。
出来上がるバイナリはJVMやJava Byte Codeを含まないらしいので面白いですね。

# Truffleチラ見

[simplelanguage](https://github.com/graalvm/simplelanguage)というレポジトリがあって、おもちゃ言語をTruffleで実装するデモが置いてあります。

## エントリポイント

日本語コメントはκeenによる。

``` java
// SLMain.java
public final class SLMain {

    /**
     * The main entry point.
     */
    public static void main(String[] args) throws IOException {
        Source source;
        if (args.length == 0) {
            // @formatter:off
            source = Source.newBuilder(new InputStreamReader(System.in)).
                name("<stdin>").
                // MIME Typeを指定することでその言語のインタプリタを指定する
                mimeType(SLLanguage.MIME_TYPE).
                build();
            // @formatter:on
        } else {
            source = Source.newBuilder(new File(args[0])).build();
        }

        executeSource(source, System.in, System.out);
    }

    private static void executeSource(Source source, InputStream in, PrintStream out) {
        out.println("== running on " + Truffle.getRuntime().getName());

        PolyglotEngine engine = PolyglotEngine.newBuilder().setIn(in).setOut(out).build();
        assert engine.getLanguages().containsKey(SLLanguage.MIME_TYPE);

        try {
            // ここで実行
            Value result = engine.eval(source);

            if (result == null) {
                throw new SLException("No function main() defined in SL source file.");
            } else if (result.get() != SLNull.SINGLETON) {
                out.println(result.get());
            }

        }
        ...
    }
    ...
}
```

## Nodeと解釈

``` java
// nodes/expression/SLMulNode.java

package com.oracle.truffle.sl.nodes.expression;

import java.math.BigInteger;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.sl.nodes.SLBinaryNode;

/**
 * This class is similar to the extensively documented {@link SLAddNode}.
 */
@NodeInfo(shortName = "*")
// 辿っていくと、`com.oracle.truffle.api.nodes.Node`の子クラスになっている
public abstract class SLMulNode extends SLBinaryNode {

    @Specialization(rewriteOn = ArithmeticException.class)
    protected long mul(long left, long right) {
        return Math.multiplyExact(left, right);
    }

    @Specialization
    @TruffleBoundary
    protected BigInteger mul(BigInteger left, BigInteger right) {
        // 本当に計算するだけ。
        return left.multiply(right);
    }
}
```

## Builtin

ビルトイン関数も定義されます。

``` java
// builtins/SLPrintlnBuiltin
@NodeInfo(shortName = "println")
public abstract class SLPrintlnBuiltin extends SLBuiltinNode {
   ...

    @TruffleBoundary
    private static void doPrint(PrintWriter out, Object value) {
        out.println(value);
    }
}
```

# まとめ

またしても締まりがないですが

* GraalというJavaで拡張可能なJITエンジンがあるよ
* Truffleという簡単にインタプリタを書けるフレームワークがあるよ
* TruffleはGraalを使いながら裏ですごい最適化をするよ
* Truffleで書いたインタプリタはSVMというツールでバイナリにもできるよ。

# 参考資料
* [Graal/Truffleの資料](https://github.com/graalvm/graal/blob/master/docs/Publications.md)
* [Understanding How Graal Works - a Java JIT Compiler Written in Java](http://chrisseaton.com/rubytruffle/jokerconf17/)
* [Graal Project](http://openjdk.java.net/projects/graal/)
* [Truffle Tutorial](http://graalvm.github.io/graal/truffle/javadoc/com/oracle/truffle/tutorial/package-summary.html)
* [TruffleRuby on the Substrate VM](http://nirvdrum.com/2017/02/15/truffleruby-on-the-substrate-vm.html)
* [One VM to Rule Them All](https://lafo.ssw.uni-linz.ac.at/pub/papers/2016_PLDI_Truffle.pdf) スライド
* [One VM to Rule Them All, One VM to Bind Them](https://www.youtube.com/watch?v=FJY96_6Y3a4&feature=youtu.be) YouTube
