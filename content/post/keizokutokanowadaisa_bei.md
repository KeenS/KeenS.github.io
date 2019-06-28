---
categories: ["継続"]
date: 2019-06-27T01:16:40+09:00
title: "継続とかの話題サーベイ"
---
日夜CPSに励む紳士淑女のみなさなまこんにちは、κeenです。CPS変換について掘り返してたら発散していったので適当に書き留めておきます。
主に自分向けのメモで、あとで思い出すために書いてるので私自身以外には分かりづらいと思います。

<!--more-->
# CPS
最初はcall-by-nameをcall-by-valueでエミュレーションするために導入されたっぽい。

Plotkin, G. D. "Call-by-name, call-by-value, and the λ-calculus," Theoretical Computer Science, Vol. 1, No. 2,pp. 125–159 (December 1975).

変換規則は以下。

\\\[
\begin{align}
\[x\] &= \lambda\kappa.\kappa x \\\\\\
\[\lambda x.M\] &= \lambda\kappa.\kappa (\lambda{}x.\[M\]) \\\\\\
\[M N\] &= \lambda\kappa\[M\] (\lambda{}m.\[N\] (\lambda{}n.(m n) \kappa))
\end{align}
\\\]

存外シンプル。CPSと呼ばれだしたのはSchemeのRabbitコンパイラ（Steele Jr., G. L. 1978）からっぽい？

ただしこれは動くだけで便利じゃない。例えば以下のような式を考える。


\\\[
\lambda{}f.\lambda{}x.\lambda{}y.(f y) x
\\\]

これをCPSすると以下のようになる。

\\\[
\lambda\kappa.\kappa (\lambda{}f.\lambda\kappa.k (\lambda{}x.\lambda\kappa.k(\lambda{}y.\lambda\kappa.(\lambda\kappa.(\lambda\kappa.\kappa f)(\lambda{}m.(\lambda\kappa.k y)(\lambda{}n.(m n)\kappa)))
                                                                                                 (\lambda{}m.(\lambda\kappa.\kappa x)(lambda{}n.(m n) \kappa)))))
\\\]

これには簡約基(redex)があるので簡約するとこうなる（post-reduction）。

\\\[
\lambda\kappa.\kappa (\lambda{}f.\lambda\kappa.k (\lambda{}x.\lambda\kappa.k(\lambda{}y.\lambda\kappa.(f y)(\lambda{}m.(m x) k))))
\\\]

これで変換したものが得られたが、途中で簡約を挟んでいる。今回は元の式に簡約基がなかったがもし簡約基を持つ式をCPSするとpost-reductionで元の式の簡約基まで潰れてしまう。

これでは困るのでCPSに工夫を加える。
CPSの結果として残る項と変換処理の都合上導入された項を区別する。
それが

Danvy, O and Filinski, A. "Representing contril: a study of the CPS transformation," Math. Struct. in Comp. Science (1992), vol. 2, pp. 361-391.

関数適用にもアノテーションが必要なため、記法の都合で適用に $@$ 印を付けることにすると、こういうような規則になる。

\\\[
\begin{align}
\[x\] &= \overline{\lambda}\kappa.\overline{@}\kappa x \\\\\\
\[\lambda{}x.M\] &= \overline{\lambda}\kappa.\overline{@}\kappa (\underline{\lambda}x.\underline{\lambda}\kappa.\overline{@}\[M\] (\overline{\lambda}m.\underline{@}\kappa m)) \\\\\\
\[@M N\] &= \overline{\lambda}\kappa.\overline{@}\[M\] (\overline{\lambda}m.\overline{@}\[N\] (\overline{\lambda}n.\underline{@}(\underline{@}m n) (\underline{\lambda}a.\overline{@}\kappa a)))
\end{align}
\\\]

上線がstaticな項で、CPS変換中に簡約してよい。下線がdynamicな項で、CPS変換後も残らないといけない。

というのが基本。

理論的なCPS変換としては上記でいいが、実用上はやっぱり不便、というか遅い。 $\lambda$ 抽象が多すぎる。
そもそもなんのためにCPS変換するの、というのを考えないといけないが自分の場合は `call/cc` や `shift` / `reset` を実装したいから。
となると継続が必要になる式だけCPSしたら速いんじゃねというのがSelective CPS。

色々な切り口から色々な手法が出てるけどこれ出しとけばいいのかな。

Nielsen, L.R. "A Selective CPS Transformation," Electronic Notes in Theoretical Computer Science 45 (2001)

これはeffect分析を用いて変換する。
雑にいうと `callcc` や `throw` を内包する式を内側から順にマークしていって、マークのついている式だけを変換する。
多分みんなの思ってる通りのやつ。

あとは型主導だと浅井研のこれが好き。

Asai, K. and Uehara, C. "Selective CPS transformation for shift and reset," PEPM '18 Proceedings of the ACM SIGPLAN Workshop on Partial Evaluation and Program Manipulation Pages 40-52 (2018)

型主導なのでShift/Resetに型が付く言語という縛りが出るけどやっぱ型主導翻訳って楽しいよね。


# 型

あんまり型には興味ない。浅井先生の `printf` が有名。

Asai, K. "On Typing Delimited Continuations: Three New Solutions to the Printf Problem," Higher-Order and Symbolic Computation, Vol. 22, No. 3, pp. 275-291, Springer (2009).

これはCPSとか型付けとかShift/ResetとかAnswer Typeとかを丁寧に解説してるのでオススメ。

元はDanvyがCPSで書いたコードだけどそれを限定継続で再実装してみるとFilinskiのshift/resetの実装で型チェック通らないという話。
俎上に上がってる `(s)printf` は以下のように使う。

```ocaml
(* string *)
sprintf (lit "Hello world!" ++ eol)
(* -> "Hello world!\n" *)

(* string -> string *)
sprintf (lit "Hello " ++ % str ++ lit "!" ++ eol)

(* string *)
sprintf (lit "Hello " ++ % str ++ lit "!" ++ eol) "world"
(* -> "Hello world!\n" *)

(* string -> int -> string *)
sprintf (lit "The value of " ++ % str ++ lit " is " ++ % int ++ eol)


(* string *)
sprintf (lit "The value of " ++ % str ++ lit " is " ++ % int ++ eol) "x" 3
(* -> "The value of x is 3\n" *)
```

見てのとおり引数に応じて型が変わる仕組みになってる。

元々はCPSだけど少し複雑なのでshift/resetで再実装したものだけ載せるとこう。
全体的にパターンは文字列を受け取って文字列を返す関数になってる。

``` ocaml
(*  lit : string -> string -> string  *)
let lit x = fun s -> s ^ x

(*  eol : string -> string  *)
let eol = fun s -> s ^ "\n"

(*  (++) : (’a -> ’b) -> (’b -> ’c) -> ’a -> ’c  *)
let (++) f1 f2 = fun s -> f2 (f1 s)

(*  int : int -> string  *)
let int x = string_of_int x

(*  str : string -> string  *)
let str (x : string) = x

(* ??? *)
let % to_str = fun s -> shift (fun k -> fun x -> k (s ^ to_str x))

(* ??? *)
let sprintf p = reset (fun () -> p "")
```

`%` が一旦継続を破棄して `reset` まで返り、必要な引数を受け取ってから継続を再起動する設計。
素直に `k` をtail callしていないのがポイントで、このせいで型付けが難しくなっている。

`%` と `reset` の型は一旦棚上げしておいて、もし動的型付言語などで実行すると以下のように簡約が進む。

``` text
   sprintf (lit "Hello " ++ % str ++ lit "!" ++ eol)
→ reset (fun () -> (lit "Hello " ++ % str ++ lit "!" ++ eol) "")
→ reset (fun () -> eol (lit "!" (% str (lit "Hello " ""))))
→ reset (fun () -> eol (lit "!" (% str "Hello ")))
→ fun x -> reset (fun () -> eol (lit "!" ("Hello " ^ str x)))
```

パッと見 `lit` とか使ってるので `string` が出てきそうなbodyから `fun x -> ...` が出てきちゃいました、というのが型の問題。

ここで一旦Answer Typeが出てくる。Answer Typeは初期継続の返り値型で、要するに `reset` の返り値の型になる。
Answer Typeの概念を用いてCPSを説明すると、直接形式のプログラム `S -> T` をCPS変換すると `(T -> Ans) -> S -> Ans` になる。対偶っぽい見た目。

直接形式のプログラムだけならそれでいいのだが、 `shift` が出てくると話がややこしくなる。
先の例のように、元々 `string` 型が出てくる予定だった `reset` から `fun x -> ...` が出てきた訳で、 `shift` を呼ぶとAnswer Typeが変化することが分かる。
Filinskiのshift/resetの実装はこのAnswer Type Modificationをサポートしていなかったので問題が起きた。

元々のshift/resetの理論ははAnswer Type Modificacionをサポートしていたらしい。
`S -> T` をCPSすると `(T -> A) -> S -> A` になっていたが、Answer Typeが変わりうることを考慮すると `(T -> A) -> S -> B` になる。
これを `shift` / `reset` の体系で（陽に継続を扱わない体系で） 表現しようとするとAnswer Type Modificationをする関数は `S/A -> T/B` と表記することになる。
`shift` を呼ばないpureな関数はAnswer Typeに影響がないので `S/'p -> T/'p` と型が付く。

これを導入すると `%` に型が付く。

``` ocaml
(*  % : (’b -> string) -> (string / ’a -> string / (’b -> ’a))  *)
let % to_str = fun s -> shift (fun k -> fun x -> k (s ^ to_str x))
```

answer typeが `'a` から `'b -> 'a` に変わっているのが分かる。

同様に `sprintf` にも型が付く。

``` ocaml
(*  sprintf : (string / string -> string / ’a) -> ’a  *)
let sprintf p = reset (fun () -> p "")
```

ちゃんと「`string` が出てきそうな `body` から `fun x -> ...` (`'a`) が出てきちゃった」が表現されている。

# 計算体系
初期の $\lambda_v-C$ とかもあるっぽいけどよく分かんないのでSLCから。
浅井先生のスライドが分かりやすい。

A Reinvestigation of Filinski’s Symmetric Lambda Calculus [PDF](http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/27/slides/kenichi1.pdf)


Filinskiの論文はこれ。

Filinski, A. "Declarative Continuations and Categorical Duality," Master's thesis, DIKU Report89/11, University of Copenhagen (1989).

Symmetric Lambda Calculsは「継続って値の双対だよね」という話から始まる（あとcall-by-valueとcall-by-nameも双対）。
なので継続も値と同じように第一級に扱える計算体系になっている。
関数も値 `A` を 値 `B` に変換する `A -> B` と `B` の継続を `A` の継続に変換する `¬A <- ¬B` がある。

SLCが何故そのまま研究されなかったかは知らないけど（見た目がキモいから？）、そのあと $\lambda$ 計算に `call/cc` を入れた $\lambda\mu$ 計算が登場した。

Parigot, M. "λμ-calculus: An Algorithmic Interpretation of Classical Natural Deduction,"" In A.Voronkov, editor,Logic Programming and Automated Reasoning (LNCS 624), pp. 190–201 (1992).

これ、Classical Natural Deductionといってシーケント計算が使われてるんですがそういうもんですかね（注意書きでProofs of usual natural deduction are easely translated in this systemとは書かれている）。
 $\lambda\mu$ 計算は自然演繹。

さらにその後計算に対称性を持たせて自然演繹ではなくLKのシーケント計算に対応させた $\overline{\lambda}\mu\tilde\mu$ (ラムダ・バー・ミュー・ミュー・チルダ) 計算が出た。

Curien, P.-L., and H. Herbelin "The Duality of Computation," Proceedings of the ACM SIGPLANInternational Conference on Functional Programming (ICFP'00), pp. 233–243 (2000).

これは読みやすいのでおすすめ。ただしシーケント計算を知ってないと文脈が分かりづらい。
$\overline{\lambda}$ になるとシーケント計算になって、それに $\mu$ と $\tilde\mu$ を加えたっぽい？
$\tilde\mu$ はcall-by-nameの上でcall-by-valueをするための演算子で、 $\mu$ の双対になる。実質 `let`。

以下が $\overline{\lambda}\mu\tilde\mu$ の構文。

\\\[
\begin{align}
c &::= \langle{}v|e\rangle \\\\\\
v &::= x ‖ \mu\beta.c ‖ \lambda{}x.v \\\\\\
e &::= \alpha ‖ \tilde\mu{}x.c ‖ v\cdot{}e \\\\\\
\end{align}
\\\]

普通の $\lambda$ 計算に慣れてると戸惑うが、値に継続（評価文脈）を当て嵌めるとそれはコマンドになる。
$\mu$ が継続を受け取ってコマンドを返す項で、 $\tilde\mu$ が値を受け取ってコマンドを返す項。
$\lambda{}x.v$ は何かしらの値を受け取りある値を返す項で、 $v\cdot{}e$ はある値 $v$ を供出し、何かしらの値を $e$ で受け取る項。

評価規則は以下のようになる。

\\\[
\begin{align}
(\to^\prime) \quad & \langle\lambda{}x.v_1|v_2\cdot{}e\rangle & \to \quad & \langle{}v_2|\tilde\mu{}x.\langle{}v_1|e\rangle\rangle \\\\\\
(\mu)        \quad & \langle\mu\beta.c|e\rangle               & \to \quad & c\[\beta \leftarrow e\] \\\\\\
(\tilde\mu ) \quad & \langle{}v|\tilde\mu{}x.c\rangle         & \to \quad & c\[x \leftarrow v\]
\end{align}
\\\]

これの $(\mu)$ を先に適用すればCBVになり、 $(\tilde\mu)$ を先に適用すればCBNになる。

型判断が初見だと分かりづらい。記法に以下の3種類がある。

\\\[
c: (\Gamma \vdash \Delta) \\\\\\
\Gamma \vdash v : A | \Delta \\\\\\
\Gamma | e : A \vdash \Delta
\\\]

コマンドは値と継続が揃ってるので判断式の型がつく。 $\Gamma$ が値の型環境で、 $\Delta$ が継続の型環境。
値の型判断は $\Gamma \vdash v : A | \Delta$ で、 $\Delta$ は使ってないので $|$ で隔離されている。
継続の型判断は $\Gamma | e : A \vdash \Delta$ で、 $\Gamma$ は使ってないので $|$ で隔離されている。

継続の方、確かに値と双対な書き方なんだけどそう書くのかーという感じ。

この記法を用いて型付け規則はこう書かれる。

\\\[
\frac{\Gamma \vdash v : A | \Delta  \qquad \Gamma | e : A \vdash \Delta}{\langle{}v|e\rangle : (\Gamma \vdash \Delta)} \\\\\\
\frac{}{\Gamma | \alpha : A \vdash \alpha : A, \Delta} \quad
\frac{}{\Gamma, x : A \vdash x : A | \Delta} \\\\\\
\frac{c : (\Gamma \vdash \beta : B, \Delta)}{\Gamma \vdash \mu\beta.c : B | \Delta} \quad
\frac{c : (\Gamma , x : A \vdash \Delta)}{\Gamma | \tilde\mu{}x.c : B \vdash \Delta} \\\\\\
\frac{\Gamma \vdash v : A | \Delta \qquad \Gamma | e : B \vdash \Delta}{\Gamma | v\cdot{}e : A \to B \vdash \Delta} \quad
\frac{\Gamma, x : A \vdash v : B | \Delta}{\Gamma \vdash \lambda{}x.v : A \to B | \Delta}
\\\]

なんか最後の2つだけ対称性が悪い。なので値に $e\cdot{}v$ を、継続に $\beta\lambda.e$ を加えて対称にする拡張もある。

これより先は知らない。浅井先生のスライドによるとWadlerのやつがあるらしいけど未読。

先というか応用例だとSequent Coreがある。

Downen, P. Maurer, L. Ariola, Z. M. and Peyton Jones, S. "Sequent calculus as a compiler intermediate language," Proceedings of the 21st ACM SIGPLAN International Conference on Functional Programming, ICFP 2016, Nara, Japan, September 18-22, 2016, pages 74–88 (2016).  
Maurer, L. Downen, P. Ariola, Z. M. and Peyton Jones, S. "Compiling without continuations," PLDI 2017 Proceedings of the 38th ACM SIGPLAN Conference on Programming Language Design and Implementation, Pages 482-494 (2017)

# 実行
継続を陽に扱える仮想機械にCEKマシンがある。あんまり情報がない。
Felleisen, M. Flatt, M. "Programming languages and lambda caluculi" で紹介されているのは確認した。
元論文は以下？

M. Felleisen and D. P. Friedman. "Control Operators, the SECD-Machine, and the λ-Calculus," Indiana University, Computer Science Department (1986).

継続を陽に扱う処理系作るならSelective CPSかCEKマシンかなーと思いつつCEKマシンの情報が少ない。

# その他

そういえばshift/resetってどこが発祥なんだろう。というのが気になった。これかな？

shift/reset: Danvy, O. and Filinski, A. "A Functional Abstraction of Typed Contexts," Technical Report89/12, DISK, University of Copenhagen, (1989).

control/prompt: Felleisen, M. "The Theory and Practice of First-Class Prompts," proceedings of the Fifteenth Annual ACM SIGACT-SIGPLAN Symposium on Principles of Programming Languages pp 180-190, San Diego, California (1988).
