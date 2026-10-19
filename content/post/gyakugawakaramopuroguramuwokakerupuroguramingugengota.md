---
categories: [継続, プログラミング]
date: 2026-10-18T22:26:17+09:00
title: "逆側からもプログラムを書けるプログラミング言語作った"
---
κeenです。最近はAIで自作プログラミング言語を作るのが流行ってますね。私も前から温めていたアイディアを形にしてみたので紹介します。

<!--more-->

# 設計思想
作ったのは [SLC](https://github.com/KeenS/slc)という言語です。一応純粋関数型言語になるのかな？ならないかも。

プログラムというのは計算です。開始状態から始め、値を評価して条件分岐をしたり合流したりしながら終了状態へと向かっていき、最後には値となります。

<svg xmlns="http://www.w3.org/2000/svg" width="800" height="380" viewBox="0 0 800 380" font-family="Helvetica, Arial, sans-serif">
<defs>
<marker id="m0" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#1f55df"/></marker>
<marker id="m1" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#1fa4df"/></marker>
<marker id="m2" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#1fdfca"/></marker>
<marker id="m3" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#1fdf7b"/></marker>
<marker id="m4" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#1fdf2c"/></marker>
<marker id="m5" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#62df1f"/></marker>
<marker id="m6" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#b1df1f"/></marker>
<marker id="m7" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#dfbd1f"/></marker>
<marker id="m8" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#df6e1f"/></marker>
<marker id="m9" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#df1f1f"/></marker>
<marker id="m10" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#c3c8ce"/></marker>
</defs>
<rect width="800" height="380" fill="#fff"/>
<path d="M63.0,179.0 L134.8,118.0" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M63.0,201.0 L134.8,262.0" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M165.6,98.2 L246.7,63.0" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M164.5,113.8 L247.9,164.6" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M162.8,263.8 L249.9,188.1" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M166.6,278.6 L245.5,295.8" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M280.5,62.1 L366.8,101.7" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M279.9,166.9 L367.4,119.5" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M279.4,184.0 L368.0,239.4" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M280.7,293.5 L366.5,257.7" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M399.4,119.0 L488.0,174.4" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M402.0,109.3 L605.0,100.8" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M399.9,241.9 L487.4,194.5" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M399.7,258.6 L487.7,309.9" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M518.9,175.2 L608.7,111.6" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M519.7,193.6 L607.7,244.9" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M519.9,311.9 L607.4,264.5" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M639.1,109.6 L733.5,173.8" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M639.8,246.7 L732.5,194.8" fill="none" stroke="#c3c8ce" stroke-width="2" marker-end="url(#m10)"/>
<path d="M616.6,269.8 Q505,465 394.7,267.5" fill="none" stroke="#c3c8ce" stroke-width="2" stroke-dasharray="5 4" marker-end="url(#m10)"/>
<path d="M63.0,201.0 L134.8,262.0" fill="none" stroke="#1fa4df" stroke-width="4" marker-end="url(#m1)" pathLength="1" stroke-dasharray="1 1" stroke-dashoffset="1" opacity="0"><animate attributeName="stroke-dashoffset" values="1;1;0;0" keyTimes="0;0.0286;0.0762;1" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1" keyTimes="0;0.0286" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></path>
<path d="M162.8,263.8 L249.9,188.1" fill="none" stroke="#1fdfca" stroke-width="4" marker-end="url(#m2)" pathLength="1" stroke-dasharray="1 1" stroke-dashoffset="1" opacity="0"><animate attributeName="stroke-dashoffset" values="1;1;0;0" keyTimes="0;0.1048;0.1524;1" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1" keyTimes="0;0.1048" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></path>
<path d="M279.4,184.0 L368.0,239.4" fill="none" stroke="#1fdf7b" stroke-width="4" marker-end="url(#m3)" pathLength="1" stroke-dasharray="1 1" stroke-dashoffset="1" opacity="0"><animate attributeName="stroke-dashoffset" values="1;1;0;0" keyTimes="0;0.1810;0.2286;1" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1" keyTimes="0;0.1810" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></path>
<path d="M399.9,241.9 L487.4,194.5" fill="none" stroke="#1fdf2c" stroke-width="4" marker-end="url(#m4)" pathLength="1" stroke-dasharray="1 1" stroke-dashoffset="1" opacity="0"><animate attributeName="stroke-dashoffset" values="1;1;0;0" keyTimes="0;0.2571;0.3048;1" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1" keyTimes="0;0.2571" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></path>
<path d="M519.7,193.6 L607.7,244.9" fill="none" stroke="#62df1f" stroke-width="4" marker-end="url(#m5)" pathLength="1" stroke-dasharray="1 1" stroke-dashoffset="1" opacity="0"><animate attributeName="stroke-dashoffset" values="1;1;0;0" keyTimes="0;0.3333;0.3810;1" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1" keyTimes="0;0.3333" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></path>
<path d="M616.6,269.8 Q505,465 394.7,267.5" fill="none" stroke="#b1df1f" stroke-width="4" marker-end="url(#m6)" pathLength="1" stroke-dasharray="1 1" stroke-dashoffset="1" opacity="0"><animate attributeName="stroke-dashoffset" values="1;1;0;0" keyTimes="0;0.4095;0.4571;1" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1" keyTimes="0;0.4095" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></path>
<path d="M399.7,258.6 L487.7,309.9" fill="none" stroke="#dfbd1f" stroke-width="4" marker-end="url(#m7)" pathLength="1" stroke-dasharray="1 1" stroke-dashoffset="1" opacity="0"><animate attributeName="stroke-dashoffset" values="1;1;0;0" keyTimes="0;0.4857;0.5333;1" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1" keyTimes="0;0.4857" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></path>
<path d="M519.9,311.9 L607.4,264.5" fill="none" stroke="#df6e1f" stroke-width="4" marker-end="url(#m8)" pathLength="1" stroke-dasharray="1 1" stroke-dashoffset="1" opacity="0"><animate attributeName="stroke-dashoffset" values="1;1;0;0" keyTimes="0;0.5619;0.6095;1" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1" keyTimes="0;0.5619" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></path>
<path d="M639.8,246.7 L732.5,194.8" fill="none" stroke="#df1f1f" stroke-width="4" marker-end="url(#m9)" pathLength="1" stroke-dasharray="1 1" stroke-dashoffset="1" opacity="0"><animate attributeName="stroke-dashoffset" values="1;1;0;0" keyTimes="0;0.6381;0.6857;1" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1" keyTimes="0;0.6381" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></path>
<circle cx="50" cy="190" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"><animate attributeName="fill" values="#eef0f2;#1f55df" keyTimes="0;0.0000" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="stroke" values="#c3c8ce;#1f55df" keyTimes="0;0.0000" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="r" values="17;17;22;17" keyTimes="0;0.0000;0.0143;0.0381" dur="10.5s" repeatCount="indefinite"/></circle>
<text x="50" y="195" font-size="14" font-weight="bold" text-anchor="middle" fill="#555">S</text>
<circle cx="150" cy="105" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"/>
<circle cx="150" cy="275" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"><animate attributeName="fill" values="#eef0f2;#1fa4df" keyTimes="0;0.0762" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="stroke" values="#c3c8ce;#1fa4df" keyTimes="0;0.0762" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="r" values="17;17;22;17" keyTimes="0;0.0762;0.0905;0.1143" dur="10.5s" repeatCount="indefinite"/></circle>
<circle cx="265" cy="55" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"/>
<circle cx="265" cy="175" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"><animate attributeName="fill" values="#eef0f2;#1fdfca" keyTimes="0;0.1524" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="stroke" values="#c3c8ce;#1fdfca" keyTimes="0;0.1524" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="r" values="17;17;22;17" keyTimes="0;0.1524;0.1667;0.1905" dur="10.5s" repeatCount="indefinite"/></circle>
<circle cx="265" cy="300" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"/>
<circle cx="385" cy="110" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"/>
<circle cx="385" cy="250" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"><animate attributeName="fill" values="#eef0f2;#1fdf7b;#b1df1f" keyTimes="0;0.2286;0.4571" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="stroke" values="#c3c8ce;#1fdf7b;#b1df1f" keyTimes="0;0.2286;0.4571" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="r" values="17;17;22;17" keyTimes="0;0.2286;0.2429;0.2667" dur="10.5s" repeatCount="indefinite"/></circle>
<circle cx="505" cy="185" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"><animate attributeName="fill" values="#eef0f2;#1fdf2c" keyTimes="0;0.3048" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="stroke" values="#c3c8ce;#1fdf2c" keyTimes="0;0.3048" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="r" values="17;17;22;17" keyTimes="0;0.3048;0.3190;0.3429" dur="10.5s" repeatCount="indefinite"/></circle>
<circle cx="505" cy="320" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"><animate attributeName="fill" values="#eef0f2;#dfbd1f" keyTimes="0;0.5333" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="stroke" values="#c3c8ce;#dfbd1f" keyTimes="0;0.5333" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="r" values="17;17;22;17" keyTimes="0;0.5333;0.5476;0.5714" dur="10.5s" repeatCount="indefinite"/></circle>
<circle cx="625" cy="100" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"/>
<circle cx="625" cy="255" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"><animate attributeName="fill" values="#eef0f2;#62df1f;#df6e1f" keyTimes="0;0.3810;0.6095" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="stroke" values="#c3c8ce;#62df1f;#df6e1f" keyTimes="0;0.3810;0.6095" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="r" values="17;17;22;17" keyTimes="0;0.3810;0.3952;0.4190" dur="10.5s" repeatCount="indefinite"/></circle>
<circle cx="750" cy="185" r="17" fill="#eef0f2" stroke="#c3c8ce" stroke-width="2"><animate attributeName="fill" values="#eef0f2;#df1f1f" keyTimes="0;0.6857" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="stroke" values="#c3c8ce;#df1f1f" keyTimes="0;0.6857" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/><animate attributeName="r" values="17;17;22;17" keyTimes="0;0.6857;0.7000;0.7238" dur="10.5s" repeatCount="indefinite"/></circle>
<text x="750" y="190" font-size="14" font-weight="bold" text-anchor="middle" fill="#555">T</text>
<circle r="6" fill="#fff" stroke="#1fa4df" stroke-width="3" opacity="0"><animateMotion path="M63.0,201.0 L134.8,262.0" keyPoints="0;0;1;1" keyTimes="0;0.0286;0.0762;1" calcMode="linear" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" keyTimes="0;0.0286;0.0762" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></circle>
<circle r="6" fill="#fff" stroke="#1fdfca" stroke-width="3" opacity="0"><animateMotion path="M162.8,263.8 L249.9,188.1" keyPoints="0;0;1;1" keyTimes="0;0.1048;0.1524;1" calcMode="linear" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" keyTimes="0;0.1048;0.1524" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></circle>
<circle r="6" fill="#fff" stroke="#1fdf7b" stroke-width="3" opacity="0"><animateMotion path="M279.4,184.0 L368.0,239.4" keyPoints="0;0;1;1" keyTimes="0;0.1810;0.2286;1" calcMode="linear" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" keyTimes="0;0.1810;0.2286" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></circle>
<circle r="6" fill="#fff" stroke="#1fdf2c" stroke-width="3" opacity="0"><animateMotion path="M399.9,241.9 L487.4,194.5" keyPoints="0;0;1;1" keyTimes="0;0.2571;0.3048;1" calcMode="linear" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" keyTimes="0;0.2571;0.3048" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></circle>
<circle r="6" fill="#fff" stroke="#62df1f" stroke-width="3" opacity="0"><animateMotion path="M519.7,193.6 L607.7,244.9" keyPoints="0;0;1;1" keyTimes="0;0.3333;0.3810;1" calcMode="linear" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" keyTimes="0;0.3333;0.3810" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></circle>
<circle r="6" fill="#fff" stroke="#b1df1f" stroke-width="3" opacity="0"><animateMotion path="M616.6,269.8 Q505,465 394.7,267.5" keyPoints="0;0;1;1" keyTimes="0;0.4095;0.4571;1" calcMode="linear" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" keyTimes="0;0.4095;0.4571" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></circle>
<circle r="6" fill="#fff" stroke="#dfbd1f" stroke-width="3" opacity="0"><animateMotion path="M399.7,258.6 L487.7,309.9" keyPoints="0;0;1;1" keyTimes="0;0.4857;0.5333;1" calcMode="linear" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" keyTimes="0;0.4857;0.5333" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></circle>
<circle r="6" fill="#fff" stroke="#df6e1f" stroke-width="3" opacity="0"><animateMotion path="M519.9,311.9 L607.4,264.5" keyPoints="0;0;1;1" keyTimes="0;0.5619;0.6095;1" calcMode="linear" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" keyTimes="0;0.5619;0.6095" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></circle>
<circle r="6" fill="#fff" stroke="#df1f1f" stroke-width="3" opacity="0"><animateMotion path="M639.8,246.7 L732.5,194.8" keyPoints="0;0;1;1" keyTimes="0;0.6381;0.6857;1" calcMode="linear" dur="10.5s" repeatCount="indefinite"/><animate attributeName="opacity" values="0;1;0" keyTimes="0;0.6381;0.6857" calcMode="discrete" dur="10.5s" repeatCount="indefinite"/></circle>
</svg>

はじめは値がなくて計算だけ、実行中の状態をみると計算途中の値と残りの計算があり、おわりは計算がなくて値だけです。よく考えてみると値と計算は表裏の関係にあるのに気付きます。

我々が関数を書くときも値を受け取ったところからはじめて値を使った計算を書いたり値に基いた条件分岐を書いたりしながら書き進めますよね？

<svg xmlns="http://www.w3.org/2000/svg" width="410" height="380" viewBox="10 0 410 380" font-family="Helvetica, Arial, sans-serif">
<defs><marker id="m" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#8a9099"/></marker></defs>
<rect x="10" width="410" height="380" fill="#fff"/>
<path d="M63.0,179.0 L134.8,118.0" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M63.0,201.0 L134.8,262.0" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M165.6,98.2 L246.7,63.0" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M164.5,113.8 L247.9,164.6" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M162.8,263.8 L249.9,188.1" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M166.6,278.6 L245.5,295.8" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M280.5,62.1 L366.8,101.7" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M279.9,166.9 L367.4,119.5" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M279.4,184.0 L368.0,239.4" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M280.7,293.5 L366.5,257.7" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<circle cx="50" cy="190" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<text x="50" y="195" font-size="14" font-weight="bold" text-anchor="middle" fill="#444">S</text>
<circle cx="150" cy="105" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="150" cy="275" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="265" cy="55" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="265" cy="175" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="265" cy="300" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="385" cy="110" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="385" cy="250" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
</svg>


このスタイル、アンバランスじゃないですか？値と残りの計算は表裏の関係だったので値を中心にプログラムを書くだけでなく残りの計算を中心にプログラムを書けてもいいと思いませんか？
残りの計算側から書くというのは要は終了状態から書きはじめるということです。

<svg xmlns="http://www.w3.org/2000/svg" width="440" height="380" viewBox="350 0 440 380" font-family="Helvetica, Arial, sans-serif">
<defs><marker id="m" viewBox="0 0 10 10" refX="8" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#8a9099"/></marker></defs>
<rect x="350" width="440" height="380" fill="#fff"/>
<path d="M399.4,119.0 L488.0,174.4" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M402.0,109.3 L605.0,100.8" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M399.9,241.9 L487.4,194.5" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M399.7,258.6 L487.7,309.9" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M518.9,175.2 L608.7,111.6" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M519.7,193.6 L607.7,244.9" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M519.9,311.9 L607.4,264.5" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M639.1,109.6 L733.5,173.8" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M639.8,246.7 L732.5,194.8" fill="none" stroke="#8a9099" stroke-width="2.5" marker-end="url(#m)"/>
<path d="M616.6,269.8 Q505,465 394.7,267.5" fill="none" stroke="#8a9099" stroke-width="2.5" stroke-dasharray="5 4" marker-end="url(#m)"/>
<circle cx="385" cy="110" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="385" cy="250" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="505" cy="185" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="505" cy="320" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="625" cy="100" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="625" cy="255" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<circle cx="750" cy="185" r="17" fill="#ffffff" stroke="#6b7178" stroke-width="2.5"/>
<text x="750" y="190" font-size="14" font-weight="bold" text-anchor="middle" fill="#444">T</text>
</svg>

プログラムの始点と終点、両端から書くことで散らばりがちな条件分岐もスマートに書ける気がします。分かりやすい例だと「このプログラムが終了するときは正常終了して値を返すか異常終了してメッセージを返すかだ」のように関数の終了条件を記述できると便利だと思いませんか？
そういうプログラミング言語SLCを作りました。

ところでさっきから「残りの計算」と呼んでいたものは継続という名前で知られていますね。以後、継続と呼ぶことにします。

継続の話を続けましょう。継続というのは残りの計算、もっとちゃんと言うと終了状態に到達するまでの残りの計算です。値と継続を合成するとプログラムが完成し、そのままプログラムが終了するまで実行が進みます。いくつかのプログラミング言語には `exit` という関数があって呼んだらその場でプログラムが終了してしまいますが、そんなイメージです。値が `v` 、 継続が `k` だとすると、SLCでは `<v | k>` という記法で値と継続を合成します。

値でも継続でもない存在として関数があります。みなさん関数を値に適用しますよね？関数を `f` とするとその記法はちょっと見馴れないですが、 `<v | f` と書きます。

値に関数を適用できるということは表裏の関係にある継続も関数に適用できます。その記法は `f | k>` になります。継続を関数に適用するというのがイメージつきづらいですかね？例えば `k` の型が `i32` だったとします。これは数値を受け取ってプログラムが終了するまで計算する継続です。例えば先に例に挙げた `exit` なんかが該当します。
そして `f` の型が `String -> i32` だとします。これは例えば `"OK"` を受け取ったら `0` を、それ以外の文字列を受け取ったら `-1` を返す関数だったとしましょう。この2つを `f | k>` のように合成すると「`"OK"` を受け取ったら終了ステータス `0` で、それ以外の文字列を受け取ったら終了ステータス `-1` でプログラムを終了させる継続」という意味になります。元々 `i32` の継続だったのが `String` の継続になっているので私は `String -> i32` のアダプターのようなものを噛ませてるイメージを持っています。

記法からちょっと予想がついたかもしれませんが、値→関数→継続と連続して合成することもできます。

```
<v | f | k>
```

これは `v` に `f` を適用して得られた値と `k` を合成すると捉えてもいいですし、 `f` に `k` を適用して得られた継続と `v` を合成すると捉えても構いません。どちらでもプログラムの意味は変わりません。 `f1`, `f2`, `f3`のように複数の関数があれば一気に合成することもできます。

```
<v | f1 | f2 | f3 | k>
```

# コード例

## ハローワールド
基本アイディアが分かったところでHello Worldをみてみましょう。まだ紹介していない機能もありますが、一旦スルーして下さい。



```text
proc main | (exit: i32) / {IO} {
    <"Hello, SLC!" | println;
    <0 | exit>
}

```

いきなり継続がでてきました。 `(exit: i32)` と書いてあるのがこの `main` 手続の継続を受け取っている箇所です。 `|` の左側が値の引数、右側が継続の引数なのですが、 `main` は値の引数がないので `|` の左側は空です。 `/ {IO}` の部分は今回説明しないのでスルーしましょう。

関数本体の1行目は `<"Hello, SLC!" | println;` で関数を値に適用して返り値は特に使っていません。この辺はRust風に `;` で複数行のプログラムを書けるようになっています。
そして2行目がさきほど説明した `<0 | exit>` と値と継続の合成です。値と継続の合成のことはカットと呼んでいます。ちゃんと `main` がカットで終わっていますね。

## 関数
さっき定義したのは値も継続も受け取る `proc` でしたが、値だけしか受け取らない、あるいは継続だけしか受け取らないものはちょっとだけ省略した記法で `func` で書けます。

```text
func double(n: i64) -> i64 {
    <(n, 2) | mul
}
```

これは値を受け取って値を返す関数です。親しみやすいでしょう。因みにSLCには中置演算子がないので四則演算も2引数関数になっています。


逆に、継続を受け取って継続を返す関数も書けます。そのときは返り型の前にある矢印を `<-` にして記述します。

```text
func stringify(out: String) <- Bool {
  mu Bool {
    True => <"True" | out>,
    False => <"False" | out>,
  }
}
```

`String` 継続を受け取って `Bool` 継続を返す関数です。みなさんの馴れた言い方だと `Bool` 値を受け取って `String` 値を返す関数とも言えます。 `mu` はもうちょっとあとで説明するので一旦スルーして下さい。

`->` の関数と `<-` の関数は値と継続のどちらに名前をつけるかだけでとくに振舞の違いはありません。

## データ型

個人的に気に入ってるのはデータ型です。Rustを出発点としてるのでRustでいう `struct`、SLCでは `data` と呼ぶものと `enum` があります。

```text
data Point {
    x: i64,
    y: i64,
}

enum Size {
    Small,
    Big(i64), // how far over the limit
}
```

ですがこれらは値のためのデータ型ですよね？継続のためのデータ型、 `menu` と `form` もあります。

`form` は分かりやすいですね。 継続を束ねた継続、データを受け取れます。
`data` が値を作るときにデータを入れてあげて使うときにパターンマッチで分解してどうやって使うか書きますよね？その逆をやるのが `form` です。

こういうフォームを用意したとします。

```text
form User {
    id: i32,
    name: String,
}
```

このフォームのインスタンスは `i32` と `Stirng` を受け取る継続です。インスタンスを作るときは `id: i32` と `name: String` を受け取ったら何をするかを記述します。

```text
// exit: i32は継続とする

let f = mu User {
    // idとnameを受け取ったらやることを書く
    User {id, name} => {
        of <(id, 0) | gt {
            // exitで早期リターン
            False => <-1 | exit>,
            True => (,),
        };
        of <(name, "anonymous") | eq {
            True => (,),
            // exitで早期リターン
            False => <-2 | exit>
        };
        // formは継続なので最後の計算まで書く、
        // つまり最後はカット
        <0 | exit>
    }
};
```

このインスタンスを計算に使うときはデータを入れてあげます。

```text
<User{ id: 1, name: "anonymous"} | f>
```

`menu` も `form` と同様に継続を束ねるデータ型ですが、 `enum` のようにどれか1つの継続が使えます。

```text
menu Drink {
    cola: String,
    beer: String,
}
```

この `Drink` メニューのインスタンスを作るには、 `cola`, `beer` をそれぞれどう提供するか記述してあげます。 `cola`, `beer` それぞれが継続なのでその継続に提供する値を食わせてあげて下さい。

```text
let m = mu Drink {
    cola <= <"Cola" | cola>,
    beer <= <"🍺" | beer>,
};
```

このメニューから選ぶには `m.field` の構文を使います。

```text
<m.cola | println; // "Cola"
// あるいは
<m.beer | println; // "🍺"
```

値を保持する `data` や `enum` と違って計算を保持しているのでフィールドを参照する度に計算が走ります。

## パターンマッチと2つの `mu`

この言語でのパターンマッチには `of` を使います。とはいっても既に出てきていますがね。

```text
func not(b: Bool) -> Bool {
    of b {
        True => False,
        False => True,
    }
}
```

`of EXPR { PAT => EXPR, PAT => EXPR}` の構文です。

もう1つ、 `mu` という変な構文がありましたね。あれもパターンマッチができますが、どちらかというとパターンマッチはオマケみたいなものです。本質的にはSchemeにあるような `call/cc` に近い存在です。

先の `not` を `<-` で実装してみましょう。

```text
func not(out: Bool) <- Bool {
    // ...
}
```

これ、ちょっと困りません？あるのは出力する継続だけで条件分岐する対象の値がありません。値が渡されるのは分かっているのにそれを呼ぶ名前がありません。そこで `mu` という構文で「渡されるはずの値」 を取り出すことができます。

```text
func not(out: Bool) <- Bool {
    mu Bool {
        b => of b {
            True => <False | out>,
            False => <True | out>,
        }
    }
}
```

さらにそのままパターンマッチに入れるのでこういう書き方もできるのです。

```text

func not(out: Bool) <- Bool {
    mu Bool {
        True => <False | out>,
        False => <True | out>,
    }
}

```

`mu` は `mu TYPE { PAT => CUT, PAT => CUT, ...}` の構文です。 `=>` の左側が値のパターン、`=>`の右側がカットです。 `mu` と `{` の間が型ですし、マッチした腕が式でなくカットだったりとちょっとクセがありますね。イメージとしては「継続を組み立ててる中で急に値を取り出す構文、全体としては継続になる」と思っておいて下さい。

`mu` にはもう1つの構文があります。`menu` を組み立てたときの構文を覚えていますか？ `mu` ですが、腕が `<=` になっています。

```text
let m = mu Drink {
    cola <= <"Cola" | cola>,
    beer <= <"🍺" | beer>,
};
```

これは先の `mu` とは逆の機能で、主に `menu` を組み立てるのに使います。 `<=` の左側が継続のパターン、`<=`の右側がカットです。イメージとしては「値を組み立ててる中で急に継続を取り出す構文、全体としては値になる」と思っておいて下さい。


## タプルの親戚
SLCにはテンソル(タプル) `(T1, T2)` がありますが、それ以外の無名データ型もあります。`data`, `enum`, `menu`, `form` それぞれに対応するものがあります。


| 有名   | 無名    | 名称 |
|--------|--------|-----|
| `data` | `(T1, T2)` | Tensor |
| `enum` | `(T1 \| T2)` | Sum |
| `menu` | `(K1 & K2)` | With |
| `form` | `(K1 ; K2)` | Par |

値を複数受け取る関数は `(T1, T2)` を引数にとり、継続を複数受け取る関数は `(K1 & K2)` を引数にとります。全体として「`T1` と `T2` を使って `K1` または `K2` を実現できる」のような意味を型から読み取れます。

```text
// 引数として継続を複数とる。
// ok: 成功したときの継続
// failed: 失敗したときの継続
proc parse_literal(input: String, pos: i64, text: String) | (ok: i64 & failed: String) {
    let end = <(pos, <text | str_len) | add;
    // The slice is taken only once it is known to be in range.
    let matches = of (<(end, <input | str_len) | le) {
        True => <(<(input, pos, end) | substring, text) | eq,
        False => False,
    };
    of matches {
        // 成功したらok継続を呼ぶ
        True => <end | ok>,
        // 失敗したらfailed継続を呼ぶ
        False => <"invalid JSON literal" | failed>,
    }
}
```

他の言語で例外や `Either`, `Result` などで処理していたエラーの扱いを継続でできます。ここでは `ok`, `failed` と名前をつけましたが、 `return`, `throw` なんかにしても面白いかもしれません。

## menuの真価
`data` と `enum` はRustをはじめとして色々な言語でよく知られているとして、 `menu` (`enum` の双対なので co data)はあまり知られていないんじゃないでしょうか。これはかなり面白い機能を提供してくれます。フィールドを選んではじめて計算が行なわれるので遅延計算ができます。

```text
menu Lazy<*T> {
    force: T,
}
```

遅延計算を上手く使えば、無限の数列なんかも作れます。

```text
menu Stream<+T> {
    head: T,
    tail: Stream<T>,
}

// 例えばこう構築できる
func repeat<+T>(x: T) -> Stream<T> {
    mu Stream {
        head <= <x | head>,
        tail <= <x | repeat | tail>,
    }
}

// 1,1,1,1,1,…と無限に続く数列
let s = <1 | repeat;
<s.head | println;                // 1
<s.tail.head | println;           // 1
<s.tail.tail.tail.head | println; // 1

```

各フィールドは `enum` よろしく引数もとれます。それを上手く使うとオブジェクト指向のようなこともできるのです。

```text
menu StringBuilder {
    append(part: String): StringBuilder,
    finish: String,
}

// 中間状態を作る関数
func from(text: String, part: String) -> StringBuilder {
    let text = <(text, part) | add;
    mu StringBuilder {
        append(part) <= <(text, part) | from | append>,
        finish <= <text | finish>,
    }
}
// 初期状態を作る関数
func new() -> StringBuilder {
    mu StringBuilder {
        append(part): out <= <("", part) | from | out>,
        finish <= <"" | finish>,
    }
}

// こう使う
let start = new();
let left = <"left: " | start.append;
let left = <"42" | left.append;
let right = <"right" | start.append;
<left.finish | println;   // left: 42
<right.finish | println;  // right
```

純粋関数型言語ですがこのような手続型っぽいコードも書けるのが面白いですね。

## 継続を使った大域脱出
継続には色々な使い方があります。

早期リターンの進化形として大域脱出なんかもあります。木の探索を考えましょう。関数呼出がネストするのでコールスタックが深くなります。その状態で目的の値がみつかったら何が起きるでしょうか？普通なら今まで積み重ねてきたコールスタックを1つずつreturnして回る必要があります。しかし、あらかじめ継続を受け取っておけばその継続を起動することで一気にコールスタックを飛び越えて値を返せます。

```text
enum Tree {
    Leaf,
    Node(Tree, i64, Tree),
}

proc walk(t: Tree, target: i64) | (
    jump: i64
    & done: (unit / {IO})
) / {IO} {
    of t {
        Leaf => <(,) | done>,
        Node(left, value, right) => {
            <("visiting ", <value | int_to_str) | add | println;
            of (<(value, target) | eq) {
                // 大域脱出する。これでネストしたwalk全てを飛び越えて呼出元に返れる
                True => <value | jump>,
                False => (,),
            };
            // walkの再帰呼び出し1
            <(left, target) | walk | (jump & mu unit {
                // walkの再帰呼び出し2
                finished_left => <(right, target) | walk | (jump & done)>,
            })>
        },
    }
}

let tree = ...;
let hit = mu {
    k <= {
        let not_found = mu unit { exhausted => <-1 | k> };
        // 呼出元
        <(tree, 2) | walk | (k & not_found)>
    }
};

```

## 継続でファイルを必ず閉じる

ファイルをopenしたあと、必ず閉じる処理って難しいですよね。言語ごとに専用の構文が用意されていると思いますが、SLCならイディオムでカバーできます。
関数から返る手段は `exit` などの継続しかないので `exit` 変数をシャドウイングしてしまえば関数から返るときに必ずファイルを閉じるようにできます。

```text
// exit: i32は継続とする

// 一旦ファイルを開く
let file = <"examples/basics/hello.sl" | fs::open;
// exit継続をシャドウイング
let exit = mu i32 {
    status => {
        <file | fs::close;
        <status | exit>
    },
};
// ファイルにアレコレする
```

Cの `goto FAIL;` とかGoの `deferred` とかに近いパラダイムですね。ですが `goto FAIL;` よりもユーザがミスする余地が減りますし、 `deferred` のように専用の構文も必要ありません。なんとなく継続の有用さが伝われば嬉しいです。


# 雑多な話題

この記事は継続に焦点をあてたので色々紹介できてない機能があります。例えば副作用の扱いは代数的エフェクトを導入しています。構文は `hook` / `hand` でインタフェースと実ハンドラを定義して `do` でハンドリングをします。ハンドラをインラインで定義する構文は `hn` です。

Rustのようにデータ型にメソッドを定義する構文 `impl` もあります。トレイト相当のものは `spec` です。

名前空間は `sect` で作って `cite` で利用します。

なんとなく想像ついたかもしれませんが、トップレベルの宣言のキーワードは全部4文字、式は全部2文字で統一しています。 `menu` とか `form` とかを含め、いくつかのものはAIが考えてくれました。特に `menu` は `enum` のアナグラムですし気に入ってます。

中置演算子がないのは他の機能をたくさん入れるためです。例えば`(T1 | T2)`や`(T1 & T2)`が実現してるのは中置演算子がないおかげもあります。その他にも全体的に糖衣構文は少なめです。

純粋関数型なのはミュータブルな機能を入れ忘れたからです。でも無限ループがあるので純粋関数型警察がきたら怒られるかも。ここでいう純粋関数型というのは値は全てイミュータブルで、副作用はEffectという専用の仕組みで管理できるという意味です。

`if` 式はありません。全部 `of` でやります。これは `if` を言語に入れると `Bool` を特別扱いしないといけなくなるのが嫌だったからです。`of` だけだと `if else` 相当の式がないのでちょっと不便です。

SLCの名前のとおりSymmetric Lambda Calculusから着想を受けてますが、コア言語はλ̄μμ̃(ラムダ・バー・ミュー・ミュー・チルダ)計算を採用しています。カリーハワードでいうと古典シーケント計算LKに対応します。

え？じゃあ排中律とか書けるの？書けます。

```
enum Choice {
    Holds(i64),
    // -TはTの継続の意味
    Refutes(-i64),
}

func lem() -> Choice {
    mu { k <= <Choice::Refutes(mu i64 { a => <Choice::Holds(a) | k> }) | k> }
}
```



SLCのインタプリタ、コンパイラ、ランタイム、標準ライブラリ、ドキュメントは全部AIに書かせてコードは一切読んでないです。このブログやドキュメントに書いてあることはそうなるはずの内容ですが本当に正しいかは私も知りません。特に静的検査回りはけっこう怪しいです。

# 結びに

値と同じくらい継続を主役にするプログラミング言語を作ってみました。もっと詳しく知りたい方は[ドキュメント](https://keens.github.io/slc/introduction.html)や[examples](https://github.com/KeenS/slc/tree/master/examples)をみてみて下さい。

継続の強力さが伝われば幸いです。

継続は力なり


