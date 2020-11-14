---
categories: [JavaScript, Web Audio API]
date: 2020-11-14T10:23:27+09:00
title: "ブラウザ上で音楽を演奏する"
---

κeenです。めずらしくWebの話題でも。ブラウザ上で音出したいときってどうすればいいんだっけとなって調べた結果です。

<!--more-->

# Web Audio API

個人的にはSVGのオーディオ版の、楽譜っぽいものをテキストで入力したら勝手に音が鳴ってくれるフォーマットをさがしてたのですがみつかりませんでした（辛うじて[MusicXML](https://ja.wikipedia.org/wiki/MusicXML)が近いくらい？）。MIDIとかにも期待したんですがあれはバイナリ

代わりに、Web Audio APIというのを使えばブラウザ上で音が出せそうというのがみつかりました。

* [Web Audio API - Web APIs | MDN](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API)

この中の [`OscillatorNode`](https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode) を使えば手で音が出せそうです。

## OscillatorNode

`OscillatorNode` を使ったサンプルを実装してみたのがこれ（クリックすると大きめの音が鳴ります）。

<div id="oscPlayer">▶️ Play</div>

<script><!--
(function() {
    const audioCtx = new (window.AudioContext || window.webkitAudioContext)();
    let oscillator = null;
    const playButton = document.getElementById("oscPlayer");

    playButton.addEventListener("mousedown", function(event) {
      if (oscillator === null) {
          playButton.innerHTML = "⏸ Stop";
          oscillator = audioCtx.createOscillator();
          oscillator.type = 'square';
          oscillator.frequency.setValueAtTime(440, audioCtx.currentTime); // value in hertz
          oscillator.connect(audioCtx.destination);
          oscillator.start();
      } else {
          playButton.innerHTML = "▶️ Play";
          oscillator.stop();
          oscillator.disconnect(audioCtx.destination);
          oscillator = null
      }
    }, false);
})()
-->
</script>

コードは以下。



```html
<div id="oscPlayer">▶</div>
<script>
const audioCtx = new (window.AudioContext || window.webkitAudioContext)();
let oscillator = null;
const playButton = document.getElementById("oscPlayer");

playButton.addEventListener("mousedown", function(event) {
  if (oscillator === null) {
      playButton.innerHTML = "⏸ Stop";
      oscillator = audioCtx.createOscillator();
      oscillator.type = 'square';
      oscillator.frequency.setValueAtTime(440, audioCtx.currentTime); // value in hertz
      oscillator.connect(audioCtx.destination);
      oscillator.start();
  } else {
      playButton.innerHTML = "▶️ Play";
      oscillator.stop();
      oscillator.disconnect(audioCtx.destination);
      oscillator = null
  }
}, false);
</script>
```

ノードをコネクトしていって有向グラフを作り、音源から `audioCtx.destination` まで到達できたら音が鳴る仕組みです。
その音源にあたるのが `OscillatorNode` で、コード中のこの部分で作って実行してます。

``` javascript
      oscillator = audioCtx.createOscillator();
      oscillator.type = 'square';
      oscillator.frequency.setValueAtTime(440, audioCtx.currentTime); // value in hertz
      oscillator.connect(audioCtx.destination);
      oscillator.start();
```

440Hzの矩形波を鳴らしています。440Hzというのはおなじみラの音です。

音源をstopする手段はあるのですが、一度stopすると再開できないのでこういう再生/停止を繰り返す場合は音源を削除、disconnectするのが常套手段のようです。

今回は使ってませんが、 `OscillatorNode` には `frequency` の他に `detune` というパラメータがあります。
これは[平均律](https://ja.wikipedia.org/wiki/平均律)での[セント](https://ja.wikipedia.org/wiki/セント_(音楽))を指定します。
12音階での隣の音がちょうど100セントで、1オクターブ上がるとちょうど1200セントです。
上記のラが440Hzなのでこれに100セントずつ+-していけば音階を表現できるという仕組みです。

## 演奏時間を指定する

周波数と演奏する長さがあればひとまず演奏できそうですね。
上記のAPIで周波数は指定できるようになったので演奏時間を指定するのが目標です。

`OscillatorNode` などの [`AudioScheduledSourceNode`](https://developer.mozilla.org/en-US/docs/Web/API/AudioScheduledSourceNode) の [`start`](https://developer.mozilla.org/en-US/docs/Web/API/AudioScheduledSourceNode/start) （と呼応する [`stop`](https://developer.mozilla.org/en-US/docs/Web/API/AudioScheduledSourceNode/stop)）はオプショナルな引数 `when` でいつ操作をするか選べるのでそれを使います。
すなわち、1秒演奏するなら `start(startTime)` と `stop(startTime + 1)` を呼んであげればいい訳です。
ただし演奏が終わったら `disconnect` したり `null` を代入したりする処理が必要になります。そういうのは `onended` にコールバックで指定できるのでそうします。

それを実装したのが以下。

<div id="oscPlayer2">▶️ Play</div>

<script><!--
(function() {
    const audioCtx = new (window.AudioContext || window.webkitAudioContext)();
    let oscillator = null;
    const playButton = document.getElementById("oscPlayer2");

    playButton.addEventListener("mousedown", function(event) {
      if (oscillator === null) {
          playButton.innerHTML = "⏸ Stop";
          oscillator = audioCtx.createOscillator();
          oscillator.type = 'square';
          oscillator.frequency.setValueAtTime(440, audioCtx.currentTime); // value in hertz
          oscillator.connect(audioCtx.destination);

          // 1秒だけ鳴らしたあとお片付けする
          let currentTime = audioCtx.currentTime;
          oscillator.onended = function() {
              oscillator.disconnect(audioCtx.destination);
              oscillator = null
          }
          oscillator.start(currentTime);
          oscillator.stop(currentTime + 1);
      } else {
          playButton.innerHTML = "▶️ Play";
          oscillator.stop();
          oscillator.disconnect(audioCtx.destination);
          oscillator = null
      }
    }, false);
})()
-->
</script>

コードは以下。



```html
<div id="oscPlayer">▶</div>
<script>
const audioCtx = new (window.AudioContext || window.webkitAudioContext)();
let oscillator = null;
const playButton = document.getElementById("oscPlayer2");

playButton.addEventListener("mousedown", function(event) {
  if (oscillator === null) {
      playButton.innerHTML = "⏸ Stop";
      oscillator = audioCtx.createOscillator();
      oscillator.type = 'square';
      oscillator.frequency.setValueAtTime(440, audioCtx.currentTime); // value in hertz
      oscillator.connect(audioCtx.destination);

      // 1秒だけ鳴らしたあとお片付けする
      let currentTime = audioCtx.currentTime;
      oscillator.onended = function() {
          oscillator.disconnect(audioCtx.destination);
          oscillator = null
      }
      oscillator.start(currentTime);
      oscillator.stop(currentTime + 1);
  } else {
      playButton.innerHTML = "▶️ Play";
      oscillator.stop();
      oscillator.disconnect(audioCtx.destination);
      oscillator = null
  }
}, false);
</script>
```

## 音量を調整する

[`GainNode`](https://developer.mozilla.org/en-US/docs/Web/API/GainNode) というのでできます。
音源とスピーカの間に挟まるエフェクト扱いです。

こんな感じ（動作例略）

``` javascript
const gainNode = audioCtx.createGain();
gainNode.gain.value = 0.025;
gain.connect(audioCtx.destination);
osc.connect(gain)
```

## 演奏する

まあ、あとはプログラマなら適当にコード書けば演奏できるようになるでしょう。

実装したのがこちら。今度はボリュームが調整できます。

<div class="controls">
    <div class="left">
        <div id="playButton" class="button">
            ▶️ play
        </div>
    </div>
    <div class="right">
        <span>Volume: </span>
        <input type="range" min="0.0" max="1.0" step="0.01"
               value="0.8" name="volume" id="volumeControl">
    </div>
</div>

<script><!--

 class Note {
     constructor(step, octave, duration) {
         const noteTable = {
             "C" : -9,
             "C#": -8, "Cs": -8, "Df": -8,
             "D" : -7,
             "D#": -6, "Ds": -6, "Ef": -6,
             "E" : -5,
             "F" : -4,
             "F#": -3, "Fs": -3, "Gf": -3,
             "G" : -2,
             "G#": -1, "Gs": -1, "Af": -1,
             "A" :  0,
             "A#":  1, "As":  1, "Bf":  1,
             "B" :  2
         };
         this.detune = noteTable[step] * 100 + (octave - 4) * 1200;
         this.duration = duration;
     }

     realDuration(bpm) {
         bpm = bpm || 60;
         return 1  * (60 / bpm) / this.duration * 4;
     }
 }

 class Tone {
     ctx
     concertPitch
 }

 class SimpleTone extends Tone {
     type = 'sine';
     constructor(type) {
         super()
         this.type = type;
         this.osc = null;
     }

     play(detune, start, duration, output) {
         const osc = this.ctx.createOscillator();
         osc.type = this.type;
         osc.frequency.value = this.concertPitch;
         osc.detune.value = detune;
         osc.connect(output);
         osc.onended = function() {
             osc.disconnect(output)
         };
         osc.start(start);
         osc.stop(start + duration * 0.9);
         return osc;
     }
 }

 class PianoTone extends Tone {
     createOsc(detune, gain, start, duration, output) {
         const attack = 0.2;
         const decay = 0.1;
         const sustain = gain * 0.7;
         const release = 0.3;

         const osc = this.ctx.createOscillator();
         osc.frequency.value = this.concertPitch;
         osc.detune.value = detune;
         osc.type = 'sine';

         const gainNode = this.ctx.createGain();
         const t0 = start;
         const t1 = t0 + attack;
         const t2 = t1 + decay;
         const t3 = t0 + duration;
         const t4 = t3 + release;
         gainNode.gain.linearRampToValueAtTime(gain, t1);
         gainNode.gain.setTargetAtTime(sustain, t1, decay)
         gainNode.gain.setTargetAtTime(0, t3, release);

         osc.connect(gainNode);
         gainNode.connect(output);
         osc.onended = function() {
             osc.disconnect(gainNode);
             gainNode.disconnect(output);
         }

         osc.start(t0);
         osc.stop(t4);
         return osc;
     }

     play(detune, start, duration, output) {
         let o1 = this.createOsc(detune - 3600, 0.512, start, duration * 0.064, output);
         let o2 = this.createOsc(detune - 2400, 0.64 , start, duration * 0.16 , output);
         let o3 = this.createOsc(detune - 1200, 0.8  , start, duration * 0.4  , output);
         let o4 = this.createOsc(detune       ,   1  , start, duration        , output);
         let o5 = this.createOsc(detune + 1200, 0.4  , start, duration * 0.8  , output);
         let o6 = this.createOsc(detune + 2400, 0.16 , start, duration * 0.64 , output);
         let o7 = this.createOsc(detune + 3600, 0.064, start, duration * 0.512, output);
         return {
             oscs: [o1, o2, o3, o4, o5, o6, o7],
             stop: function(when) {
                 for(let osc of this.oscs) {
                     osc.stop(when)
                 }
             }
         };
     }
 }

 class Music {
     constructor(notes) {
         this.notes = notes;
     }

     [Symbol.iterator]() {
         return this.notes[Symbol.iterator]();
     }

     static parse(input) {
         const notes = [];

         let octave = 4;
         let duration = 4;
         for(let c of input.split(' ')) {
             if (c === '|' || c === "" || c === "\n" ) {
                 continue;
             }

             let ret = c.match(/([A-G][#sf]?)([-+]*)(16|8|4|2|1)?/);
             if (ret) {
                 let step = ret[1];
                 for(let o of ret[2]) {
                     switch (o) {
                         case "+": octave++;
                         case "-": octave--;
                     }
                 }
                 if(ret[3]) {
                     duration = parseInt(ret[3]);
                 }
                 notes.push(new Note(step, octave, duration));
             } else {
                 throw "parse error";
             }
         }
         return new this(notes);
     }
 }

 class Player {
     constructor(bpm, tone, concertPitch) {
         this.ctx = new (window.AudioContext || window.webkitAudioContext)();

         const gain = this.ctx.createConstantSource();
         const gainNode = this.ctx.createGain();
         gainNode.gain.value = 0.025;
         gain.connect(gainNode.gain);
         gain.start();
         gain.offset.value = 0.25;
         gainNode.connect(this.ctx.destination);

         this.output = gainNode;
         this._gain = gain;
         this.bpm = bpm;
         this.tone = tone || new SimpleTone('square');
         this.tone.ctx = this.ctx;
         this.tone.concertPitch = concertPitch || 440;
         this.playingNotes = [];
         this.isPlaying = false;
     }

     get gain() {
         this._gain.offset.value;
     }

     set gain(gain) {
         this._gain.offset.value = gain;

     }

     playNote(note, at) {
         at = at || 0;
         const duration = note.realDuration(this.bpm);
         let playingNote = this.tone.play(note.detune, at, duration, this.output);
         return playingNote;
     }

     playMusic(music) {
         this.isPlaying = true;
         let start = this.ctx.currentTime;
         for (let note of music) {
             let playingNote = this.playNote(note, start);
             start += note.realDuration(this.bpm);
             this.playingNotes.push(playingNote);
         }
     }

     cancel() {
         this.isPlaying = false;
         for(let note of this.playingNotes) {
             note.stop()
         }
     }
 }

 function setup() {

     const music = Music.parse("\
     C  C  G  G  | A  A  G2 |\
     F4 F  E  E  | D  D  C2 |\
     G4 G  F  F  | E  E  D2 |\
     G4 G  F  F  | E  E  D2 |\
     C4 C  G  G  | A  A  G2 |\
     F4 F  E  E  | D  D  C2 |");
     const player = new Player(90, new SimpleTone('sine'));
     function togglePlay(event) {
         if (player.isPlaying) {
             playButton.innerHTML = "▶️ play";
             player.cancel();
         } else {
             playButton.innerHTML = "◼ cancel";
             player.playMusic(music);
         }
     }

     function changeVolume(event) {
         player.gain = volumeControl.value;
     }

     const playButton = document.querySelector("#playButton");
     const volumeControl = document.querySelector("#volumeControl");
     volumeControl.value = player.gain;

     playButton.addEventListener("click", togglePlay, false);
     volumeControl.addEventListener("input", changeVolume, false);

 }

 setup();

-->
</script>

コードは後程gistを貼りますが、だいたい以下のようなコードを書いたら演奏できるようになってます。

``` javascript
const music = Music.parse("\
    C  C  G  G  | A  A  G2 |\
    F4 F  E  E  | D  D  C2 |\
    G4 G  F  F  | E  E  D2 |\
    G4 G  F  F  | E  E  D2 |\
    C4 C  G  G  | A  A  G2 |\
    F4 F  E  E  | D  D  C2 |");
const player = new Player(90, new SimpleTone('sine'));
player.playMusic(music)
```


ひとまずやりたいことができました。

## ピアノの音色？

ところで、演奏したときの音色気になりませんでしたか？
正弦波なのでちょっと機械っぽい音色になります。

しかしあらゆる波は正弦波の組み合わせで表現できますし、ピアノの鍵盤を叩いたときの音の起伏もプログラムで表現できるので今までにでてきた道具でピアノの音色っぽいものも作れます。ちょっと挑戦してみましょう。

やってみたのがこれです。

<div class="controls">
    <div class="left">
        <div id="playButton2" class="button">
            ▶️ play
        </div>
    </div>
    <div class="right">
        <span>Volume: </span>
        <input type="range" min="0.0" max="1.0" step="0.01"
               value="0.8" name="volume" id="volumeControl2">
    </div>
</div>

<script><!--
 function setup2() {

     const music = Music.parse("\
     C  C  G  G  | A  A  G2 |\
     F4 F  E  E  | D  D  C2 |\
     G4 G  F  F  | E  E  D2 |\
     G4 G  F  F  | E  E  D2 |\
     C4 C  G  G  | A  A  G2 |\
     F4 F  E  E  | D  D  C2 |");
     const player = new Player(90, new PianoTone());
     function togglePlay(event) {
         if (player.isPlaying) {
             playButton.innerHTML = "▶️ play";
             player.cancel();
         } else {
             playButton.innerHTML = "◼ cancel";
             player.playMusic(music);
         }
     }

     function changeVolume(event) {
         player.gain = volumeControl.value;
     }

     const playButton = document.querySelector("#playButton2");
     const volumeControl = document.querySelector("#volumeControl2");
     volumeControl.value = player.gain;

     playButton.addEventListener("click", togglePlay, false);
     volumeControl.addEventListener("input", changeVolume, false);

 }

 setup2();

-->
</script>

どうですか？少なくとも先程の正弦波よりは楽器っぽくきこえないですか？

実装は以下のようなコードです。


``` javascript
class PianoTone extends Tone {
    createOsc(detune, gain, start, duration, output) {
        const attack = 0.2;
        const decay = 0.1;
        const sustain = gain * 0.7;
        const release = 0.3;

        const osc = this.ctx.createOscillator();
        osc.frequency.value = this.concertPitch;
        osc.detune.value = detune;
        osc.type = 'sine';

        const gainNode = this.ctx.createGain();
        const t0 = start;
        const t1 = t0 + attack;
        const t2 = t1 + decay;
        const t3 = t0 + duration;
        const t4 = t3 + release;
        gainNode.gain.linearRampToValueAtTime(gain, t1);
        gainNode.gain.setTargetAtTime(sustain, t1, decay)
        gainNode.gain.setTargetAtTime(0, t3, release);

        osc.connect(gainNode);
        gainNode.connect(output);
        osc.onended = function() {
            osc.disconnect(gainNode);
            gainNode.disconnect(output);
        }

        osc.start(t0);
        osc.stop(t4);
        return osc;
    }

    play(detune, start, duration, output) {
        let o1 = this.createOsc(detune - 3600, 0.512, start, duration * 0.064, output);
        let o2 = this.createOsc(detune - 2400, 0.64 , start, duration * 0.16 , output);
        let o3 = this.createOsc(detune - 1200, 0.8  , start, duration * 0.4  , output);
        let o4 = this.createOsc(detune       ,   1  , start, duration        , output);
        let o5 = this.createOsc(detune + 1200, 0.4  , start, duration * 0.8  , output);
        let o6 = this.createOsc(detune + 2400, 0.16 , start, duration * 0.64 , output);
        let o7 = this.createOsc(detune + 3600, 0.064, start, duration * 0.512, output);
        return {
            oscs: [o1, o2, o3, o4, o5, o6, o7],
            stop: function(when) {
                for(let osc of this.oscs) {
                    osc.stop(when)
                }
            }
        };
    }
}
```


ポイントは2つ。1つは[ADSR](https://ja.wikipedia.org/wiki/ADSR)で、もう1つは複数の波の組み合わせです。
どっちも私は詳しい訳じゃないんですがちょっと解説してみます。


### ADSR

ADSRはAttack、Decay、Sustain、Releaseの略でそれぞれ立ち上がり、減衰、減衰後の保持、余韻と訳されるようです。

ピアノ的に解釈すると鍵盤に触れてからを叩き終わるまでがAttack、叩いた瞬間から音が落ち着くまでがDecay、鍵盤を押しっぱなしの時間がSustain、鍵盤を離したあとにも鳴ってる時間がReleaseですかね？

音を作る人はこういうパラメータで音（エンベロープ）を作るらしいです。

上記のコードでは以下の部分ですね。


``` javascript
const attack = 0.35;
const decay = 0.2;
const sustain = gain * 0.7;
const release = 0.3;

// ...

const gainNode = this.ctx.createGain();
const t0 = start;
const t1 = t0 + attack;
const t2 = t1 + decay;
const t3 = t0 + duration;
const t4 = t3 + release;
gainNode.gain.linearRampToValueAtTime(gain, t1);
gainNode.gain.setTargetAtTime(sustain, t1, decay)
gainNode.gain.setTargetAtTime(0, t3, release);
```


`duration` が全体の演奏時間（鍵盤に触れてから離すまで）で、そのうち物理特性で `attack` と `decay` に使われる時間が決まります。
本当は鍵盤の叩き方で `attack` の方は変わると思いますがこの人は同じ強さで叩く人と思いましょう。

### 波の組み合わせ

コードでいうとこの部分です。 `detune` が1200で1オクターブなので、対象の音の3オクターブ下から3オクターブ上までの音を合成しています。

``` javascript
let o1 = this.createOsc(detune - 3600, 0.512, start, duration * 0.064, output);
let o2 = this.createOsc(detune - 2400, 0.64 , start, duration * 0.16 , output);
let o3 = this.createOsc(detune - 1200, 0.8  , start, duration * 0.4  , output);
let o4 = this.createOsc(detune       ,   1  , start, duration        , output);
let o5 = this.createOsc(detune + 1200, 0.4  , start, duration * 0.8  , output);
let o6 = this.createOsc(detune + 2400, 0.16 , start, duration * 0.64 , output);
let o7 = this.createOsc(detune + 3600, 0.064, start, duration * 0.512, output);
```

これは数学的に解釈すればフーリエ変換的に複数の周波数の正弦波を組み合わせてピアノの音色を再現しようとしています。
物理的に解釈すれば固有振動数が倍数関係にあるピアノ線が共鳴するはずなので、それを表現しています。
本当はピアノ線の長さ（音の高さ）で物理特性が変わるはずなのでADSRも変わるはずですが面倒なので無視しましょう。

# まとめ

Web Audio APIを使って音を鳴らしてみました。
Web Audio APIは周波数を指定して定音を鳴らす機能しかないので、自分で演奏機能や音色などを再現してみる試みをしました。その過程でADSRや波の合成などを知りました。


# 参考文献

* [Example and tutorial: Simple synth keyboard - Web APIs | MDN](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Simple_synth)
* [Controlling multiple parameters with ConstantSourceNode - Web APIs | MDN](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Controlling_multiple_parameters_with_ConstantSourceNode)
* [エンベロープジェネレータ | Web Audio APIの基本処理 | WEB SOUNDER - Web Audio API 解説 -](https://weblike-curtaincall.ssl-lolipop.jp/portfolio-web-sounder/webaudioapi-basic/envelope-generator)
* [Web Audio API でピアノの音色に近づけたい | q-Az](https://q-az.net/web-audio-api-piano-sound/)

# 付録: コード

今回実装したPlayerのコードはこちらに置いておきます。
楽譜部分を書き換えたら他の楽曲も演奏できるので気に入った方は試してみて下さい。

<script src="https://gist.github.com/KeenS/d5935e69e7b96275de469aa3283e1cb2.js"></script>

ライセンスはMIT/Apache-2.0のデュアルライセンスとします。

