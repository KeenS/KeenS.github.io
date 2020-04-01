---
categories: [Whitespace, compiler]
date: 2020-04-01T00:00:00+09:00
title: "Whitespaceのセルフホストコンパイラ作ったよ！"
---

ハ〜イ、κeenさんだよー。Whitespaceのセルフホストコンパイラを作ったから紹介するねー。

<!--more-->

Whitespace[^1]ってのはあまり知名度がないけどこういう感じの言語。

[^1]: Edwin BradyとChris Morrisにより2003年 **4月1日** に発表された言語

``` text
    
   	  	   
		    	
   		  	 	
		    	 
   		 		  
		    		
   		 		  
		    	  
   		 				
		    	 	
   	 		  
		    		 
   	     
		    			
   			 			
		    	   
   		 				
		    	  	
   			  	 
		    	 	 
   		 		  
		    	 		
   		  	  
		    		  
   	     
		    		 	
   		 				
		    			 
   		  		 
		    				
   	     
		    	    
   			  		
		    	   	
   			    
		    	  	 
   		    	
		    	  		
   		   		
		    	 	  
   		  	 	
		    	 	 	
   			  		
		    	 		 
   	    	
		    	 			
    
		     

 	 			 			 			  	  		 	  	 			 	   		  	 	

 	 		 			  		  	 	 			 			 		 		   		 	  	 		 			  		  	 	




   		    	 		  	   		  	  
	   
	

   			 			 			  	  		 	  	 			 	   		  	 	
 
 			 
 
	  			 			 			  	  		 	  	 			 	   		  	 	 	 					 		  	 	 		 			  		  	  
	
     	
	   
 
 			 			 			  	  		 	  	 			 	   		  	 	

   			 			 			  	  		 	  	 			 	   		  	 	 	 					 		  	 	 		 			  		  	  
 

 


	

   			  	  		  	 	 		    	 		  	  
 
  
 	
	 			 
    	 	 
	  	
	  			  	  		  	 	 		    	 		  	   	 					 		  	 	 		 			  		  	  
 

   	
	   
 
 			  	  		  	 	 		    	 		  	  

   			  	  		  	 	 		    	 		  	   	 					 		  	 	 		 			  		  	  
 

   	
	       
		 
	

   		 			  		  	 	 			 			 		 		   		 	  	 		 			  		  	 	
   	 	 
   		 	
	
  	
  
	

```

見ての通りクリアな構文が売りの言語で、簡単に覚えられるのが特徴。
文法が綺麗だとパースも簡単だろうということで、セルフホストコンパイラを書いてみた。

こんな感じでコンパイラに自身のソースを食わせると完全に一致するバイナリを吐くよ。

``` console
$ whitelie < whitelie.ws > whitelie2
$ md5sum whitelie whitelie2
418b0b9a58caaa9e99a2d5e3649f6faf  whitelie
418b0b9a58caaa9e99a2d5e3649f6faf  whitelie2
```

一応注意点を挙げておくと、オリジナルの言語とは多少の非互換がある。一番大きいのは
<pre class="chroma"><code class="language-text" data-lang="text">	
  </code></pre>
で、バイナリを扱いたいので非ASCII文字もそのまま通すようにした。この非互換の関係で恐らく既存の処理系では動かないので注意（自分は手元でパッチを当てて動かした）。あとは数値が多倍長整数でないだとか、<pre class="chroma"><code class="language-text" data-lang="text">	
 	</code></pre>と<pre class="chroma"><code class="language-text" data-lang="text">	
		</code></pre>が符号に対応してないとか。
一応全部ちゃんと実装したつもりだけど一部ちゃんと動かないソースコードもみつかってるので粗は大目に見てもらえるとありがたい。


実装は、おおむねそのまま書けばいいんだけどいくつか詰まりがちだった所を紹介するね。

実装にとりかかってまず最初に目につくのは

<pre class="chroma"><code class="language-text" data-lang="text"> 
	</code></pre>


 。セマンティクスが分かりづらい。「スタックのトップを残したまま、その後のn個のアイテムを捨てる」という動作をする。
一見すると何をしたいのか分かりづらいけど、いくつかのローカル変数をスタックに残したま関数を走らせるときに便利。
最後に関数から抜けるときに、スタックトップに値を残したいけどそのままだとスタックにローカル変数が残ってしまうからこれで消すと都合がいい。

次が

<pre class="chroma"><code class="language-text" data-lang="text">
  </code></pre>

。まあ、目に見えて面倒。今回は機械語を一旦生成して、ポジションを確定したあとにラベルのポジションを抽出して、その上でもう一度機械語を生成することで対応した。ジャンプは面倒だったので全部nearにした。



あとは地味に大変だったのが

<pre class="chroma"><code class="language-text" data-lang="text">	
 	</code></pre>

と

<pre class="chroma"><code class="language-text" data-lang="text">	
		</code></pre>

。これはどうしようもないので気合でアセンブリで実装した。

あ、書き忘れてたけど今回はlibcなどは使わずに全て自分で実装した。

終わった感想としてはまあ、大変でしたw。
とくにELFを吐きはじめたあたりで苦しくなって、gdbとにらめっこしたりバイナリを目diffしたりしてデバッグがつらかったです。


よかったら見てみて下さい。

[KeenS/whitelie: A self hosted whitespace compiler](https://github.com/KeenS/whitelie)
