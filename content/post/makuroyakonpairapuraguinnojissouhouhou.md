---
categories: [言語実装, マクロ, Advent Calendar, Advent Calendar 2016, 言語実装 Advent Calendar]
date: 2016-12-01T13:29:52+09:00
title: マクロやコンパイラプラグインの実装方法色々
---

このエントリは[言語実装 Advent Calendar 2016 - Qiita](http://qiita.com/advent-calendar/2016/lang_dev)2日目の記事です。

κeenです。マクロなどのコンパイル時に何か処理をしてコード生成する機構の実現方法が言語によって様々にあるなぁと思ったのでちょっと探ってみます

<!--more-->

なんか文字だけ並んでても味気ないのでサンプルマクロも付けときますね。

# コンパイラにマクロ専用言語のインタプリタ機能を付けるやつ
Rustの`macro_rules`やSchemeの`syntax-rules`が該当します。

``` scheme
(define-syntax define-protocol
  (syntax-rules ()
    ((define-protocol (name type ...) (method arg ...) ...)
     (begin
       (define method
         (make-generic))
       ...
       (define name
         (lambda (type ...)
           (lambda methods
             (add-methods methods (list (list method arg ...) ...)))))))))
```

制限が強い上に無駄にコンパイラの機能が増えるので個人的にはあまり好きじゃないですね…。

# コンパイラでもホスト言語を動かすやつ
コンパイラとランタイムが一緒になっているCommon Lispなんかで使われる方法です。

``` common-lisp
(defmacro dotimes-unroll ((i n unroll) &body body)
  (let ((n_      (gensym "n")))
    `(let ((,n_ ,n))
       (do ((,i 0))
           ((< ,n_ (the fixnum (+ ,unroll ,i)))
            (do ((,i ,i (the fixnum (1+ ,i))))
                ((< ,n_ (the fixnum (1+ ,i))))
              ,@body
              ))
         ,@(loop :repeat unroll :append (append body `((setq ,i (the fixnum (1+ ,i))))))))))

```

マクロがなくなるまで再帰的にマクロ展開をします。
ユーザが好き勝手書けてしかも手軽に使えるので割と好きです。まあ、でも言語を選びますね。

# コンパイラプラグインとしてdlopenするやつ

Rustのコンパイラプラグインが相当します。

``` toml
[lib]
crate-type = ["dylib"]
plugin = true
```


``` rust
fn codegen<'cx>(cx: &'cx mut ExtCtxt, text: String, file: String)
        -> Box<MacResult + 'cx> {
    let mut output = Vec::new();
    let doc = Document::parse(&text)
        .expect("failed to parse thrift file")
        .expect("EOF while parsing thrift file");
    {
        let ns = find_rust_namespace(&doc).expect("cannot find namespace");
    output.write_all(format!("mod {} {{", ns.module).as_ref()).expect("internal error failed to write the vec");
    }
    compile(doc, &mut output).expect("failed to generate code");
    output.write_all(format!("}}").as_ref()).expect("internal error failed to write the vec");
    let output = match std::str::from_utf8(&output) {
        Ok(s) => s,
        Err(_) => "",
    };

    trace!("{}", output);


    let parser = new_parser_from_source_str(cx.parse_sess(), file, output.to_string());

    struct ExpandResult<'a> {
        p: parse::parser::Parser<'a>,
    }
    impl<'a> base::MacResult for ExpandResult<'a> {
        fn make_items(mut self: Box<ExpandResult<'a>>)
                      -> Option<SmallVector<ptr::P<ast::Item>>> {
            let mut ret = SmallVector::zero();
            while self.p.token != token::Eof {
                match panictry!(self.p.parse_item()) {
                    Some(item) => ret.push(item),
                    None => panic!(self.p.diagnostic().span_fatal(self.p.span,
                                                                  &format!("expected item, found `{}`",
                                                                           self.p.this_token_to_string())))
                }
            }
            Some(ret)
        }
    }

    Box::new(ExpandResult { p: parser })

}


fn macro_thrift<'cx>(cx: &'cx mut ExtCtxt, sp: Span, tts: &[TokenTree])
                     -> Box<MacResult + 'cx> {

    let text = match get_single_str_from_tts(cx, sp, tts, "thrift!") {
        Some(f) => f,
        None => return DummyResult::expr(sp),
    };

    codegen(cx, text, "trift!".to_string())
}


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("thrift", macro_thrift);
    reg.register_macro("thrift_file", macro_thrift_file);
}

```

一旦dllを作ってそれをプラグインとしてロードするという手間もありますし、ユーザが触れるASTも複雑なのであまり好きではないです。
まあ、これは準クオートだとかのユーザインターフェースの問題だったりするんですが。
ただ表現力はホスト言語が使えるので自由に使えます。

# 言語の仕組みを使ってライブラリをコンパイラにロードする

私が地味に好きな言語に[mirah](http://www.mirah.org/)というのがあります。その言語での実現方法が少し好きでした。
mirahはJVMで動くRuby風言語で、コンパイラは既にセルフホストされています。つまりコンパイラもJVMで動く訳です。

Javaには[SPI](https://docs.oracle.com/javase/tutorial/sound/SPI-intro.html)というものがあって明示的にクラスやインスタンスを指定しなくてもJVMが実装を捜してくれる機能があります。
その機能にのっかることでマクロを.classファイルにコンパイルしてクラスパスに置いておくだけでマクロが使えるようになります。

``` mirah
$ExtensionsRegistration[['java.util.Map']]
class MapExtensions
  macro def [](key)
    quote { `@call.target`.get(`key`) }
  end
end

class Builtins implements ExtensionsProvider

  def register(type_system:ExtensionsService):void
    type_system.macro_registration(MapExtensions.class)
  end
end
```

中々面白いですね。

# おわりに

なんか実装コード1つない雑な記事で申し訳ありませんがmirahのマクロの仕組みがちょっと気に入ったので書こうと思った次第です。

あとはOCamlのppxやHaskellのTHも気になるのですが調べきれませんでした。はい。
