---
type: post
title: "COOKPADのアイコン"
date: 2013-09-07
comments: true
sharing: true
categories: [COOKPAD, Octopress]
---
クックパッドにレシピ初投稿の記念として左上のソーシャルアイコンにクックパッドを追加しました。

<!--more-->

もともとソーシャルアイコン自体今使ってるOctopressのテーマ、CleanPressにはないのですが、別のテーマからもらってきました。

加えた変更は  
source/\_includes/header.htmlに

```html
<nav id="sub-nav">{% include custom/social.html %}</nav><br>
```

を、  
source/\_includes/custom/social.htmlのdivの中に

```html
{% if site.cookpad_user %}
<a class="cookpad" href="http://cookpad.com/kitchen/3303629" title="COOKPAD">COOKPAD</a>
{% endif %}

```

を、  
sass/parts/\_header.scssの&.githubとかが並んでるところに

```sass
&.cookpad{
    background: image-url('social/cookpad.png') center no-repeat #FF9933;
    border: 1px solid #FF9933;
        &:hover{
            border: 1px solid darken(#FF9933, 10%);
        }
}
```

を、  
\_config.ymlに

```yaml
#COOKPAD
cookpad_user: your_ID
```

を加え、  
source/images/social/に [この画像](/images/social/cookpad.png)をつっこみました。16x16なのでめっちゃちっちゃいです。

画像はクックパッドの公式のバナーのピクセル数を数えながら作ったのですが、アンチエイリアスのかけかたとか分らなかったので質はかなり低いです。かといって公式のやつ勝手に改造はマズいでしょうし。

16x16で白い部分のみ、背景透過でだれか作って下さい←  
因みに私の作った画像は自由にご使用下さい。


