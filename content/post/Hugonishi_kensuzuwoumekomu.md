---
categories: [Hugo, PlantUML, Ditaa]
date: 2017-08-26T15:20:21+09:00
title: "Hugoにシーケンス図を埋め込む"
---
κeenです。ブログに図を入れるお話です。
このブログの長らくの読者ならお気付きかもしれませんが、アスキーアートを多用していました。inkscapeを開いて図を生成してコピーして手で画像へのリンクを張るのがだるいためです。特段自動生成とかはしてなくて、全て手書きです。
流石にそろそろどうにかしようと思い立っていじった次第です。

<!--more-->
手で画像を書くのはやりたくないのでアスキーアートか何かしらの記法から画像を自動生成する方針です。ここで

1. ブログアップロード前にmarkdownを解析して自動で生成、リンクを埋め込む
2. ブラウザでレンダリングしてもらう
3. emacsプラグインでどうにかする

の3つの方針があり、上の方が優先度が高いかなと思って調べ始めました。
多少調べましたが、1、2が難しそうだったので諦めて3にしました。1についてはhugoがスニペットにフックを掛けさせてくれたらよかったのですが、まだ出来ないようでした（[#796](https://github.com/gohugoio/hugo/issues/796)）。2はあってもよさそうなので何かご存知の方は教えて下さい。

と、結局3のemacsプラグインで外部のコマンドに投げる方針で実装します。外部コマンドは今のところ[ditaa](http://ditaa.sourceforge.net/)と[PlantUML](http://plantuml.com/)をサポート。文化的にそうなのか、両方ともJava実装なのでもっさりしてます。一応両者`apt`で入ります。

で、書いたemacs lispはこちら。

``` emacs-lisp
(defun current-line-string ()
  (save-excursion
    (let (start end)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq end (point))
      (buffer-substring start end))))


(defun ditaa-region (start end filename)
  (interactive (progn
                 (unless (mark)
                   (user-error "The mark is not set now, so there is no region"))
                 (list (region-beginning) (region-end) (read-file-name))))
  (call-shell-region start end "ditaa -" nil (list :file filename)))

(defun plantuml-region (start end filename)
  (interactive (progn
                 (unless (mark)
                   (user-error "The mark is not set now, so there is no region"))
                 (list (region-beginning) (region-end) (read-file-name))))
  (call-shell-region start end "plantuml -pipe" nil (list :file filename)))


(defun markdown-ext-parse-header (header-line)
  (if (string-match "<pre +style=\"display:none;\">\\([[:alnum:]]+\\)[[:space:]]*,[[:space:]]*\\([[:alnum:]]+\\)" header-line)
      (list (match-string 1 header-line) (match-string 2 header-line))
    nil))

(defun markdown-ext-region (start end)
  "
<pre style=\"display:none;\">{format},{filename}
...
...
</pre>
"
  (interactive (progn
                 (unless (mark)
                   (user-error "The mark is not set now, so there is no region"))
                 (list (region-beginning) (region-end))))
  (save-excursion
                                        ; trim trailing blanks
    (goto-char end)
    (skip-chars-backward "\n\t ")
    (setq end (point))

    (narrow-to-region start end)
    (let ((prefix (file-name-base (buffer-file-name)))
          (header-line (save-excursion (beginning-of-buffer) (current-line-string)))
           (text-start (save-excursion
                          (beginning-of-buffer)
                          (next-line)
                          (point)))
           (text-end (save-excursion
                        (end-of-buffer)
                        (beginning-of-line)
                        (point))))
     (destructuring-bind (format filename) (markdown-ext-parse-header header-line)
       (make-directory (format "../../static/images/%s" prefix) t)
       (cond
        ((string= format "ditaa") (ditaa-region text-start text-end (format "../../static/images/%s/%s.png" prefix filename)))
        ((string= format "plantuml") (plantuml-region text-start text-end (format "../../static/images/%s/%s.png" prefix filename))))
       (end-of-buffer)
       (widen)
       (forward-char)
       (if (markdown-link-p)
           (kill-line))
       (insert (format "![%s](/images/%s/%s.png)" filename prefix filename))
       ))))
```


かなり雑実装なのでコピペして使わずに参考実装程度にしてください。画像の保存場所なんか適当も適当。あと`plantuml`と`ditaa`には`exec-path`を通しておいて下さい。

使い方は特定のフォーマットに従って書いたらそのリージョンを選択して`M-x markdown-ext-region`。するとそのリージョンを解析して図を生成、リンクを挿入してくれます。

フォーマットはかなり苦しかったのですが`display:none`な`<pre>`で囲むようにしてあって

```
<pre style="display:none;">{format},{filename}
...
...
</pre>
```

というようになっています。HTMLのコメントだと記法中に`-->`とかカジュアルに出てきてダメでした。

# 例1 ditaa
このテキスト

```
<pre style="display:none">ditaa,file
    +--------+   +-------+    +-------+
    |        | --+ ditaa +--> |       |
    |  Text  |   +-------+    |diagram|
    |Document|   |!magic!|    |       |
    |     {d}|   |       |    |       |
    +---+----+   +-------+    +-------+
        :                         ^
        |       Lots of work      |
        +-------------------------+
</pre>
```

をリージョン選択して`markdown-ext-region`を呼ぶと`![file](/images/Hugonishi_kensuzuwoumekomu/file.png)`のようなリンクを生成してくれて、実際はこのような見た目になります。

<pre style="display:none;">ditaa,file
    +--------+   +-------+    +-------+
    |        | --+ ditaa +--> |       |
    |  Text  |   +-------+    |diagram|
    |Document|   |!magic!|    |       |
    |     {d}|   |       |    |       |
    +---+----+   +-------+    +-------+
        :                         ^
        |       Lots of work      |
        +-------------------------+
</pre>
![file](/images/Hugonishi_kensuzuwoumekomu/file.png)

# 例2 PlantUML
こう書くと

```PlantUML
<pre style="display:none;">plantuml,file2
@startuml
[*] --> Active

state Active {
  [*] -> NumLockOff
  NumLockOff --> NumLockOn : EvNumLockPressed
  NumLockOn --> NumLockOff : EvNumLockPressed
  --
  [*] -> CapsLockOff
  CapsLockOff --> CapsLockOn : EvCapsLockPressed
  CapsLockOn --> CapsLockOff : EvCapsLockPressed
  --
  [*] -> ScrollLockOff
  ScrollLockOff --> ScrollLockOn : EvCapsLockPressed
  ScrollLockOn --> ScrollLockOff : EvCapsLockPressed
}

@enduml
</pre>
```

こうなります

<pre style="display:none;">plantuml,file2
@startuml
[*] --> Active

state Active {
  [*] -> NumLockOff
  NumLockOff --> NumLockOn : EvNumLockPressed
  NumLockOn --> NumLockOff : EvNumLockPressed
  --
  [*] -> CapsLockOff
  CapsLockOff --> CapsLockOn : EvCapsLockPressed
  CapsLockOn --> CapsLockOff : EvCapsLockPressed
  --
  [*] -> ScrollLockOff
  ScrollLockOff --> ScrollLockOn : EvCapsLockPressed
  ScrollLockOn --> ScrollLockOff : EvCapsLockPressed
}

@enduml
</pre>
![file2](/images/Hugonishi_kensuzuwoumekomu/file2.png)

ブログを見易くしていきたい。
