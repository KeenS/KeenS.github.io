---
type: post
title: "H2Oにpicrinを溶かす"
date: 2014-12-07
comments: true
categories: [Lisp, Scheme, picrin, H2O, Advent Calendar]
---
このエントリーは  
[Lisp Advent Calendar](http://qiita.com/advent-calendar/2014/lisp) 7日目  
兼  
[H2O Advent Calendar](http://qiita.com/advent-calendar/2014/h2o) 7日目  
の記事です。


κeenです。タイトルの通りです。
<!--more-->
# H2Oとは
水。
# picrinとは
[Wikipedia](http://ja.wikipedia.org/wiki/%E3%83%94%E3%82%AF%E3%83%AA%E3%83%B3%E9%85%B8)にあるように、フェノールのトリニトロ化合物で、水溶性があります。

# ではなくて
H2OはHTTP1, HTTP2, WebsocketをサポートするNginXより速いHTTPサーバです。[Github](https://github.com/h2o/h2o)で開発されています。開発者は@kazuhoさん。

picrinは「速い、軽い、高機能」を目指して作られているScheme処理系です。[Github](https://github.com/picrin-scheme/picrin)で開発されています。開発者は@wasabizさん。

# 混ぜる
picrinのように組込み向けで開発されている処理系は

* picrinからH2Oを使えるようにする
* H2Oにpicrinを埋め込む

と、2種類考えられますが、今回は後者です。H2Oにpicrinを溶かしてる感じしますね。

まあ、Apatch HTTPDやNginX宜しくmod_picrinを作れば済むでしょう。

# 絶望
<blockquote class="twitter-tweet" lang="ja"><p><a href="https://twitter.com/blackenedgold">@blackenedgold</a> モジュラーにできるようにはしてるけど、まだsoをロードする仕組みはないです。というか、APIがまだunstableだし</p>&mdash; Kazuho Oku (@kazuho) <a href="https://twitter.com/kazuho/status/540692011003559936">2014, 12月 5</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

つらい

# solシステムの導入
mod\_xxxにしようと思ったんですけど水だし溶液ってことでsol\_xxxにします。

H2Oにこんな感じのパッチ当てて

```diff
diff --git a/src/main.c b/src/main.c
index 7fc4680..57804a6 100644
--- a/src/main.c
+++ b/src/main.c
@@ -30,6 +30,7 @@
 #include <signal.h>
 #include <stdio.h>
 #include <unistd.h>
+#include <dlfcn.h>
 #include <sys/stat.h>
 #include <sys/socket.h>
 #include <sys/types.h>
@@ -76,6 +77,8 @@ struct config_t {
     } state;
 };
 
+typedef int(*sol_init_fn)(h2o_configurator_command_t *, h2o_configurator_context_t *, const char *, yoml_t *);
+
 static unsigned long openssl_thread_id_callback(void)
 {
     return (unsigned long)pthread_self();
@@ -381,6 +384,43 @@ static int on_config_num_threads(h2o_configurator_command_t *cmd, h2o_configurat
     return h2o_config_scanf(cmd, config_file, config_node, "%u", &conf->num_threads);
 }
 
+static int on_config_use(h2o_configurator_command_t *cmd, h2o_configurator_context_t *ctx, const char *config_file, yoml_t *config_node)
+{
+  /* struct config_t *conf = H2O_STRUCT_FROM_MEMBER(struct config_t, global_config, ctx->globalconf); */
+  char *sol_name;
+  sol_init_fn init_fn;
+  void *handle;
+
+  /* fetch solution name */
+  switch (config_node->type) {
+  case YOML_TYPE_SCALAR:
+    sol_name = config_node->data.scalar;
+    break;
+  default:
+    h2o_config_print_error(cmd, config_file, config_node, "value must be a string or a mapping (with keys: `port` and optionally `host`)");
+    return -1;
+  }
+
+  char dl_name[strlen("sol_.so") + strlen(sol_name) + 1];
+  char init_fn_name[strlen("init_sol_") + strlen(sol_name) + 1];
+
+  sprintf(dl_name, "./sol_%s.so", sol_name);
+  sprintf(init_fn_name, "init_sol_%s", sol_name);
+  handle = dlopen(dl_name, RTLD_LAZY);
+  if (! handle){
+    h2o_config_print_error(cmd, config_file, config_node, "cannot load the solution");
+    return -1;
+  }
+
+  init_fn = dlsym(handle, init_fn_name);
+  if(dlerror()){
+    h2o_config_print_error(cmd, config_file, config_node, "cannot find the initialize function");
+    return -1;
+  }
+  return (*init_fn)(cmd, ctx, config_file, config_node);
+
+}
+
 static void usage_print_directives(h2o_globalconf_t *conf)
 {
     h2o_linklist_t *node;
@@ -606,6 +646,10 @@ int main(int argc, char **argv)
             c, "num-threads", H2O_CONFIGURATOR_FLAG_GLOBAL,
             on_config_num_threads,
             "number of worker threads (default: 1)");
+        h2o_config_define_command(
+            c, "use", H2O_CONFIGURATOR_FLAG_GLOBAL,
+            on_config_use,
+            "use the solution");
     }
 
     h2o_access_log_register_configurator(&config.global_config);
```

こんな感じのソリューション用意して

```C
#include <stdio.h>
#include <pthread.h>
#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/error.h"
#include "h2o.h"

pic_state *pic;
struct pic_lib *PICRIN_BASE;


void pic_init_contrib(pic_state *);
void pic_load_piclib(pic_state *);

static pic_value
pic_features(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic->features;
}

static pic_value
pic_libraries(pic_state *pic)
{
  pic_value libs = pic_nil_value(), lib;

  pic_get_args(pic, "");

  pic_for_each (lib, pic->libs) {
    libs = pic_cons(pic, pic_car(pic, lib), libs);
  }

  return libs;
}

void
pic_init_picrin(pic_state *pic)
{
  const char *scheme =
    "(import (scheme base)"
    "        (scheme write))"
    "(define-syntax call-with-output-to-string"
    "  (syntax-rules ()"
    "    ((_ proc)"
    "     (let ((s (open-output-string)))"
    "       (proc s)"
    "       (get-output-string s)))))"
    "(define (->string e)"
    "  (call-with-output-to-string"
    "   (lambda (s)"
    "     (display e s))))";


  pic_add_feature(pic, "r7rs");

  pic_deflibrary (pic, "(picrin library)") {
    pic_defun(pic, "libraries", pic_libraries);
  }

  pic_deflibrary (pic, "(scheme base)") {
    pic_defun(pic, "features", pic_features);

    pic_init_contrib(pic);
    pic_load_piclib(pic);
  }
  pic_deflibrary (pic, "(picrin base)") {
    pic_load_cstr(pic, scheme);
  }
}

const char *
pic_eval_cstr_into_cstr(pic_state *pic, const char *input)
{
  pic_value v;


  v = pic_read_cstr(pic, input);
  v = pic_eval(pic, v, PICRIN_BASE);
  v = pic_funcall(pic, PICRIN_BASE, "->string", pic_list1(pic, v));
  return pic_str_cstr(pic_str_ptr(v));
}



int
on_picrin(h2o_configurator_command_t *cmd, h2o_configurator_context_t *ctx, const char *config_file, yoml_t *config_node)
{
  const char *sexp;
  
  switch (config_node->type) {
  case YOML_TYPE_SCALAR:
    sexp = config_node->data.scalar;
    break;
  default:
    h2o_config_print_error(cmd, config_file, config_node, "value must be a string");
    return -1;
  }

  pic_try{
    puts(pic_eval_cstr_into_cstr(pic, sexp));
  }
  pic_catch{
    pic_print_backtrace(pic);
    return -1;
  }
  return 0;

}


int
init_sol_picrin(h2o_configurator_command_t *cmd, h2o_configurator_context_t *ctx, const char *config_file, yoml_t *config_node)
{


  h2o_configurator_t *c = cmd->configurator;

  pic = pic_open(0, NULL, NULL);

  pic_init_picrin(pic);

  PICRIN_BASE = pic_find_library(pic, pic_read_cstr(pic, "(picrin base)"));

  h2o_config_define_command(
      c, "picrin", H2O_CONFIGURATOR_FLAG_GLOBAL | H2O_CONFIGURATOR_FLAG_EXPECT_SCALAR,
      on_picrin,
      "run picrin");


  return 0;

}
```

んでコンフィグは

```yaml
# to find out the configuration commands, run: h2o --help

use: picrin
listen:
  port:
listen:
  port: 8081
  ssl:
    certificate-file: examples/h2o/server.crt
    key-file: examples/h2o/server.key
picrin: "(string-append \"Hello, \" \"World\")"
hosts:
  default:
    paths:
      /:
        file.dir: examples/doc_root
    access-log: /dev/stdout
```

そして魔法のコマンドを叩いて

```
$ cp ~/compile/picrin/src/{init_contrib.c,load_piclib.c} ./
$ gcc -c sol_picrin.c -std=c99   -o sol_picrin.o -I ~/compile/picrin/extlib/benz/include -I ~/compile/h2o/include -I ~/compile/h2o/deps/picohttpparser -I ~/compile/h2o/deps/yoml -DH2O_USE_LIBUV=0 -fPIC
$ gcc sol_picrin.o load_piclib.o init_contrib.o -L ~/compile/picrin/build/lib/ -l picrin -fPIC -shared -o sol_picrin.so
$ cp sol_picrin.so ~/compile/h2o
```

実行すると

```
$ cd ~/compile/h2o
$ ./h2o --conf example/h2o/h2o.conf
Hello, Wold
```

ヤッタ！

# 標準出力かよ
ううっ。ごめんなさい。

# で、HTTPレスポンス版は？
ごめんなさい、まだです。

`file.c`をベースにして

```yaml
    paths:
      /:
        picrin.exp: "(string-append \"Hello, \" \"World\")"

```

とかしたかったんですけど間に合いませんでした。

ソリューションなりモジュールなりのシステムが出来たらまたトライします
