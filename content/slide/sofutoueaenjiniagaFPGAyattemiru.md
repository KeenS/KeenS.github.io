---
categories: [FPGA, PYNQ, sotomuki]
date: 2017-03-28T09:21:33+09:00
description: "おもしろLT"
title: ソフトウェアエンジニアがFPGAやってみる
---

<textarea data-markdown
    data-separator="\n===\n"
    data-vertical="\n---\n"
    data-notes="^Note:">
# ソフトウェアエンジニアがPYNQでFPGAやってみる
----------------------

<!-- .slide: class="center" -->
===
# About Me
---------
![κeenのアイコン](/images/kappa.png) <!-- .element: style="position:absolute;right:0;z-index:-1" width="20%" -->

 * κeen
 * [@blackenedgold](https://twitter.com/blackenedgold)
 * Github: [KeenS](https://github.com/KeenS)
 * [Idein Inc.](https://idein.jp/)のエンジニア
 * Lisp, ML, Rust, Shell Scriptあたりを書きます

===
# よくある話
------------

<div class="fragment" data-fragment-index="1">「Lisperって自分のLispを作るらしいよ」 </div>
<div class="fragment" data-fragment-index="2">「Lispって昔は専用ハードウェアで動いてたらしいよ」 </div>
<div class="fragment" data-fragment-index="3">「じゃあハードウェア作んなきゃ」 </div>
<div class="fragment" data-fragment-index="4">「FPGAやるかー」 </div>

===

# FPGAって？
------------

* Field-Programmable Gate Array
* プログラマブルな集積回路
  + オレオレ設計のCPU作ったり
  + HDMI入力を直でmpg4にエンコードするデバイスとかも
* クロックはASIC(ふつうのCPU)より大分遅い
* でも上手く嵌れば100倍高速化とか

===

# PYNQって？
------------

* [PYNQ-Z1 Python Productivity for Zynq - Digilent](http://store.digilentinc.com/pynq-z1-python-productivity-for-zynq/)
* xilinxのFPGAとARMのチップが載ったSoC
* ARMで動いてるPython(Jupyter)からFPGAにロジック焼ける
  + 手軽にロジックを試せる
* FPGAとの通信はMMIO
  + 特定のアドレスのメモリにデータを書くとFPGAに送られる
  + メモリのデータを読むとFPGAからデータが送られる

===

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">会社にPYNQがきたよ！！azure倒したら遊ぶ！！！！ <a href="https://t.co/ohJg7z4lxd">pic.twitter.com/ohJg7z4lxd</a></p>&mdash; κeen (@blackenedgold) <a href="https://twitter.com/blackenedgold/status/828834126572589056">2017年2月7日</a></blockquote>

===

# ハードウェアロジック
---------------------

* ハードウェア記述言語(HDL)を使う
  + メジャーなのはVHDLとVerilog
  + 今回はVerilogの話
* クロックとかレジスタとかワイヤーとか駆使する
* 電気信号なのでロジックが並列で走る
* vivadoというツールでコンパイル
  + Tclで制御も出来る
* 回路設計とかも
* コンパイルは結構遅い…
* CPUと通信するときの規格とかも書かなきゃいけない
  * AXI LiteとかAXI Fullとか…
  * 一応自動で生成はしてくれる

===

![vivadoのスクショ](/images/fpga/vivado.png) <!-- .element: width="100%" -->

===

# なんか作ってみる1
------------------

* Lチカ
* [Vivado and zybo linux勉強会資料3](https://www.slideshare.net/marsee101/vivado-and-zybo-linux3)
* ↑ロジック焼くとこ以外ほぼこれのまま
* ロジックで作ったワイヤーと実際のLEDのワイヤー結んだりして楽しい
* AXI LiteでCPUと繋がって間隔変えたり出来る

===

``` verilog
reg [3:0] LED_Display_Counter;
reg [31:0] LED_Interval_Counter;
reg slv_reg_wren_1d;

// slv_reg_wren_1d generate
always @(posedge S_AXI_ACLK) begin
   if (~S_AXI_ARESETN)
       slv_reg_wren_1d <= 1'b0;
   else
       slv_reg_wren_1d <= slv_reg_wren;
end

// LED_Interval_Counter
always @(posedge S_AXI_ACLK) begin: proc_LED_Interval_Counter
   if (~S_AXI_ARESETN) begin
       LED_Interval_Counter <= 32'd0;
   end else begin
       if (slv_reg0[0]) begin // Enable
           if (LED_Interval_Counter == 32'd0)
               LED_Interval_Counter <= slv_reg3;
           else
               LED_Interval_Counter <= LED_Interval_Counter - 32'd1;
       end else
           LED_Interval_Counter <= slv_reg3;
   end
end

/// Counters
// LED_Display_Counter
always @(posedge S_AXI_ACLK) begin: proc_LED_Display_Counter
   if (~S_AXI_ARESETN) begin
       LED_Display_Counter <= 4'd0;
   end else begin
       if (slv_reg_wren_1d && axi_awaddr[ADDR_LSB+OPT_MEM_ADDR_BITS:ADDR_LSB] == 2'h1) // Counter Load
           LED_Display_Counter <= slv_reg1[3:0];
       else if (slv_reg0[0]) begin  // Enable
           if (LED_Interval_Counter == 32'd0)
               LED_Display_Counter <= LED_Display_Counter + 4'd1;
       end
   end
end

assign LED4bit = LED_Display_Counter;
```

===

![jupyterのスクショ](/images/fpga/led4bit.png)<!-- .element: width="100%" -->

===

# なんか作ってみる2
------------------

* 簡易計算機
* 渡された2値で演算
  + 四則演算とか論理演算とか
* AXI Liteで通信
  + AXI Liteはレジスタのみ通信可能

===

```verilog
reg [C_S_AXI_DATA_WIDTH-1:0] res;

always @(posedge S_AXI_ACLK) begin
   if (S_AXI_ARESETN == 1'b0) begin
      res <= 32'h0;
   end else if (~ slv_reg_wren)
     begin
        if (slv_reg0[0]) begin
           case (slv_reg1)
             32'h0: res <= slv_reg2 + slv_reg3;
             32'h1: res <= slv_reg2 - slv_reg3;
             32'h2: res <= slv_reg2 * slv_reg3;
             default: res <= res;
           endcase // case (slv_reg1)
        end
     end
end // always @ (posedge S_AXI_ACLK)

always @(posedge S_AXI_ACLK) begin
   slv_reg4 <= res;
   slv_reg0 <= 32'b0;
end

```

===

![jupyterのスクショ](/images/fpga/alu.png) <!-- .element: width="100%" -->


===

# なんか作ってみる3
------------------

* 命令列を受け取って計算
* 内部で16のレジスタ、プログラムカウンタなど
* CPUっぽい動き
* CPUとの通信はAXI Full
  + メモリに触れる

===

``` verilog
generate
   if (USER_NUM_MEM >= 1)
 begin
    assign mem_select  = 1;
    assign mem_address = (axi_arv_arr_flag? axi_araddr[ADDR_LSB+OPT_MEM_ADDR_BITS:ADDR_LSB]:(axi_awv_awr_flag? axi_awaddr[ADDR_LSB+OPT_MEM_ADDR_BITS:ADDR_LSB]:0));
 end
endgenerate

function [3:0] fop;
   input reg[C_S_AXI_DATA_WIDTH-1:0] opcode;
   fop = opcode[C_S_AXI_DATA_WIDTH-1 -: 4];
endfunction

function [3:0] fret;
   input reg[C_S_AXI_DATA_WIDTH-1:0] opcode;
   fret = opcode[C_S_AXI_DATA_WIDTH-5 -: 4];
endfunction

function [3:0] freg1;
   input reg[C_S_AXI_DATA_WIDTH-1:0] opcode;
   freg1 = opcode[C_S_AXI_DATA_WIDTH-9 -: 4];
endfunction

function [3:0] freg2;
   input reg[C_S_AXI_DATA_WIDTH-1:0] opcode;
   freg2 = opcode[C_S_AXI_DATA_WIDTH-13 -: 4];
endfunction

function [15:0] fconst1;
   input reg [C_S_AXI_DATA_WIDTH-1:0] opcode;
   fconst1 = opcode[C_S_AXI_DATA_WIDTH-13 -: 16];
endfunction

// implement Block RAM(s)
wire                                 mem_rden;
wire                                 mem_wren;
wire [C_S_AXI_DATA_WIDTH-1:0]        data_out;
wire [C_S_AXI_DATA_WIDTH-1:0]        start;
reg [C_S_AXI_DATA_WIDTH-1:0]         result;
reg [C_S_AXI_DATA_WIDTH-1:0]         r[0:15];
reg [C_S_AXI_ADDR_WIDTH-1:0]         pc;
reg [1:0]                            finish;
reg [C_S_AXI_DATA_WIDTH-1:0]         byte_ram [0 : 31];
reg [3:0]                            state;
reg [C_S_AXI_DATA_WIDTH-1:0]         opword;
reg [3:0]                            op;
reg [3:0]                            ret;
reg                                  reten;
reg [C_S_AXI_DATA_WIDTH-1:0]         retreg;
reg [C_S_AXI_DATA_WIDTH-1:0]         reg1;
reg [C_S_AXI_DATA_WIDTH-1:0]         reg2;
reg [15:0]                           const1;

assign mem_wren = axi_wready && S_AXI_WVALID ;

assign mem_rden = axi_arv_arr_flag ; //& ~axi_rvalid

assign data_out = byte_ram[mem_address];

assign start = byte_ram[1];

localparam integer PC_START = 2;

always @(posedge S_AXI_ACLK)
  begin
     if (S_AXI_ARESETN == 0 || start == 32'b0)
       begin
          finish <= 0;
          pc <= PC_START;
          result <= 0;
          state <= 0;
          reten <= 0;
          retreg <= 0;
          ret <= 0;
          reg1 <= 0;
          reg2 <= 0;
          const1 <= 0;
          r[0] <= 0; r[1] <= 0;
          r[2] <= 0; r[3] <= 0;
          r[4] <= 0; r[5] <= 0;
          r[6] <= 0; r[7] <= 0;
          r[8] <= 0; r[9] <= 0;
          r[10] <= 0; r[11] <= 0;
          r[12] <= 0; r[13] <= 0;
          r[14] <= 0; r[15] <= 0;
       end
     else if (finish != 2'd2)
       begin
          case (state)
            // fetch
            4'd0: begin
               opword <= byte_ram[pc];
               state <= 4'd1;
            end
            // decode
            4'd1: begin
               op <= fop(opword);
               ret <= fret(opword);
               reg1 <= r[freg1(opword)];
               reg2 <= r[freg2(opword)];
               const1 <= fconst1(opword);
               state <= 4'd2;
            end
            //execute
            4'd2: begin
               case (op)
                 // add
                 4'b0000: begin
                    retreg <= reg1 + reg2;
                    reten <= 1;
                 end
                 // sub
                 4'b0001: begin
                    retreg <= reg1 - reg2;
                    reten <= 1;
                 end
                 // mul
                 4'b0010: begin
                    retreg <= reg1 * reg2;
                    reten <= 1;
                 end
                 // or
                 4'b0011: begin
                    retreg <= reg1 | reg2;
                    reten <= 1;
                 end
                 // and
                 4'b0100: begin
                    retreg <= reg1 & reg2;
                    reten <= 1;
                 end
                 // xor
                 4'b0101: begin
                    retreg <= reg1 ^ reg2;
                    reten <= 1;
                 end
                 // add imm
                 4'b1000: begin
                    retreg <= reg1 + const1;
                    reten <= 1;
                 end
                 // sub imm
                 4'b1001: begin
                    retreg <= reg1 - const1;
                    reten <= 1;
                 end
                 // mul imm
                 4'b1010: begin
                    retreg <= reg1 * const1;
                    reten <= 1;
                 end
                 // or imm
                 4'b1011: begin
                    retreg <= reg1 | const1;
                    reten <= 1;
                 end
                 // and imm
                 4'b1100: begin
                    retreg <= reg1 & const1;
                    reten <= 1;
                 end
                 // jz
                 4'b1101: begin
                    if (reg1 == 32'b0)
                      pc <= const1 - 1 + PC_START;
                    reten <= 0;
                 end
                 // j
                 4'b1110: begin
                    pc <= const1 - 1 + PC_START;
                    reten <=0;
                 end
                 // exit
                 4'b1111: begin
                    pc <= PC_START - 1;
                    result <= reg1;
                    finish <= 1;
                    reten <= 0;
                 end
                 // error
                 default: begin
                    pc <= PC_START - 1;
                    result <= op;
                    finish <= 1;
                    reten <= 0;
                 end
               endcase
               state <= 3;
            end
            // write back
            4'd3: begin
               pc <= pc + 1;
               if (reten)
                 r[ret] <= retreg;
               if (finish == 1)
                 finish <= 2;
               state <= 0;
            end

          endcase
       end
  end

always @( posedge S_AXI_ACLK )
  begin
 if (mem_rden)
   begin
          if (mem_address == 0)
        mem_data_out[0] <= result;
          else
            mem_data_out[0] <= data_out;
   end
  end


generate
   for(mem_byte_index=0; mem_byte_index<= (C_S_AXI_DATA_WIDTH/8-1); mem_byte_index=mem_byte_index+1)
 begin:BYTE_BRAM_GEN
    wire [8-1:0] data_in ;
    integer      j;

    //assigning 8 bit data
    assign data_in  = S_AXI_WDATA[(mem_byte_index*8+7) -: 8];

    always @( posedge S_AXI_ACLK )
      begin
         if (mem_wren && S_AXI_WSTRB[mem_byte_index])
           begin
              byte_ram[mem_address][(mem_byte_index*8+7) -: 8] <= data_in;
           end
      end
 end
endgenerate
//Output register or memory read data

always @( mem_data_out, axi_rvalid)
  begin
 if (axi_rvalid)
   begin
      // Read address mux
      axi_rdata <= mem_data_out[0];
   end
 else
   begin
      axi_rdata <= 32'h00000000;
   end
  end
```

===


![jupyterのスクショ](/images/fpga/alu2.png) <!-- .element: width="100%" -->


===

# まとめ
--------

* FPGA楽しいよ
* CPUっぽいの作れるよ
* PYNQ便利だよ


</textarea>
