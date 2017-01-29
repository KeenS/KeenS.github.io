---
categories: [Lisp, Common Lisp, CIM]
date: 2017-01-29T22:18:24+09:00
title: Deprecating CIM
---
I'm κeen. I have a sad news, I'll stop the development of [CIM](https://github.com/KeenS/CIM) and no more updates for new lisp impls will be provided.
<!--more-->
It has been months from when I stopped developping CIM and only providing updates for new lisp impl versions.
From that point, CIM is a half-dead product. I know there are still someone using CIM though, so I kept updating.
For present CIM users, I recomend to migrate to [roswell](https://github.com/roswell/roswell), also a lisp installer and manager, which is well-maintained.

CIM has been developed in my studenthood for my practice in shell scripts. This is my first OSS.
The reason why I wrote CIM in shell script other than for practice is that shell scripts are the most portable and available script language on Unix-like systems.
The original purpose of writing a new manager is to provide a consistent way of launching CLs from CLI.
To realize that, managing lisp impls, including installing is needed so CIM has started to manage impls.
At some point, CIM was used such products like [cl-travis](https://github.com/luismbo/cl-travis).
But CIM lacked flexibility of development as it is written in ugly shell script, and lacked Windows support.
As I got a job and got interests other than lisp such as Rust, I have no more time to spend for maintaining CIM.

Thus [@snmsts](https://github.com/snmsts), the author of roswell, started to develop roswell to support Windows.
Roswell is written mainly in Lisp though booting from C, so many of Lispers may be able to hack it.
Roswell is well-maintained because stmsts is, unlike me, a hobby Lisper, professional lisper and he uses roswell in daily work.
Besides roswell is well-maintained, roswell is more feature-rich than CIM.
It searchs for new lisp versions for itself so there is no need to update manager itself to update your local lisp impls.
It is distributed from package managers such like homebrew and AUR so you don't need to care about updating it.
Using `dump` feature of roswell, you can create an executable binary from a lisp script in a command.
And so on. There are many reasons to use roswell even if CIM would be kept maintained.

Anyway I wish CIM users to keep using lisp via roswell. Thank you for staying with me for a long time.
