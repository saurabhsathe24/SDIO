// CLK ENABLE IS LEFT.

package sdio_cmd;

	import CRC7 ::*;
	import ConcatReg ::*;
	//import Semi_FIFOF        :: *;
	import FIFOLevel::*;
	//import AXI4_Types:: *;
	//import AXI4_Fabric:: *;
	//import AXI4_Lite_Types   :: *;
	//import AXI4_Lite_Fabric  :: *;
	import Connectable ::*;
	import FIFO::*;
	import FIFOF::*;
	import Clocks::*;
	import SpecialFIFOs::*;
	import ClientServer::*;
	//import MIMO_MODIFY::*;
	import DefaultValue :: *;
//	`include "defined_parameters.bsv"
	`include "sdio.defines"
	import ConfigReg::*;
	import Vector::*;
	import UniqueWrappers :: * ;
	import DReg::*;
	import BUtils::*;
	
	typedef enum {Byte=0, HWord=1, Word=2, DWord=3} AccessSize deriving(Bits,Eq,FShow);
	
	// Enum to define the SDIO Transaction transfers
	typedef enum{
			IDLE,
			WAIT,
			SETUP_PHASE,
			ACTIVE,
			HOLD_PHASE
	   } Sdio_state deriving(Bits, Eq, FShow);

	interface SDIO;
	
		//CLK 	
		method bit clk;
		
		//Bi-directional DATA 8 bits (IP and OP)
		method Bit#(8) data_out; 
		method Bit#(8) data_enable;
		method Action  data_i ( Bit#(8) data_in); 
		
		//Bi-directional CMD 1 bit (IP and OP)
		method Bit#(1) cmd_out; 
		method Bit#(1) cmd_enable;
		method Action  cmd_i ( Bit#(1) cmd_in);
		
	endinterface
	
	interface Ifc_sdio_controller;
   		interface SDIO io;
   		
   		/*doc : method : method to receive write requests from AXI */
		method ActionValue#(Bool) write_req(Bit#(64) addr, Bit#(64) data, AccessSize size);
		
		/*doc : method : method to receive read requests from AXI */
		method ActionValue#(Tuple2#(Bool, Bit#(64))) read_req(Bit#(64) addr, AccessSize size);
		
		  
    endinterface
    
    
	
	module mksdio_controller(Ifc_sdio_controller);
	
	/*************** List of SDIO defined Registers *****************/
	
	Ifc_Crc crc7 <- mk_Crc_Generator;
	
	/*doc : Wire : Wire which holds the input from MOSI IO pin */
	Wire#(bit) wr_sdio_cmd_in <- mkWire();
	Wire#(Bit#(8)) wr_sdio_data_in <-mkDWire(0);
	Reg#(Bit#(8)) sd_data_output <-mkReg(0);
	
	
	
	// SDMMC_POWER 
	
	Reg#(Bit#(2)) pwrctrl <- mkReg(0);
	Reg#(Bit#(32)) power = concatReg2(readOnlyReg(30'd0),pwrctrl);
	
	//SDMMC_CLKCR
	
	Reg#(Bit#(1)) clkcr_hwfc_en <- mkReg(0);
	Reg#(Bit#(1)) clkcr_negedge <- mkReg(0);
	Reg#(Bit#(2)) clkcr_widbus <- mkReg(0);
	Reg#(Bit#(1)) clkcr_bypass <- mkReg(0);
	Reg#(Bit#(1)) clkcr_pwrsav <- mkReg(0);
	Reg#(Bit#(1)) clkcr_clken <- mkReg(0);
	Reg#(Bit#(8)) clkcr_clkdiv <- mkReg(0);
	Reg#(Bit#(32)) clkcr = concatReg8(readOnlyReg(17'd0),clkcr_hwfc_en,clkcr_negedge,clkcr_widbus,clkcr_bypass,clkcr_pwrsav,clkcr_clken,clkcr_clkdiv);
	
	//SDMMC_ARG
	Reg#(Bit#(32)) arg <- mkReg(0);
	
	//SDMMC_CMD
	Reg#(Bit#(1)) cmd_sdiosuspend <- mkReg(0);
	Reg#(Bit#(1)) cmd_cpsmen <- mkReg(0);
	Reg#(Bit#(1)) cmd_waitpend <- mkReg(0);
	Reg#(Bit#(1)) cmd_waitint <- mkReg(0);
	Reg#(Bit#(2)) cmd_waitresp <- mkReg(0);
	Reg#(Bit#(6)) cmd_cmdindex <- mkReg(0);
	Reg#(Bit#(32)) cmd = concatReg7(readOnlyReg(20'd0),cmd_sdiosuspend,cmd_cpsmen,cmd_waitpend,cmd_waitint,cmd_waitresp,cmd_cmdindex);
	
	//SDMMC_RESPCMD
	Reg#(Bit#(6)) respcmd_respcmd <- mkReg(0);
	Reg#(Bit#(32)) respcmd = concatReg2(readOnlyReg(26'd0),respcmd_respcmd);
	
	//SDMMC_RESP1
	Reg#(Bit#(32)) resp1 <- mkReg(0);
	
	//SDMMC_RESP2
	Reg#(Bit#(32)) resp2 <- mkReg(0);
	
	//SDMMC_RESP3
	Reg#(Bit#(32)) resp3 <- mkReg(0);
	
	//SDMMC_RESP4
	Reg#(Bit#(32)) resp4 <- mkReg(0);
	
	//SDMMC_DTIMER
	Reg#(Bit#(32)) dtimer <- mkReg(0);
	
	//SDMMC_DLEN
	Reg#(Bit#(25)) datalength <- mkReg(0);
	Reg#(Bit#(32)) dlen = concatReg2(readOnlyReg(7'd0),datalength);
	
	//SDMMC_DCTRL
	Reg#(Bit#(1)) dctrl_sdioen <- mkReg(0);
	Reg#(Bit#(1)) dctrl_rwmod <- mkReg(0);
	Reg#(Bit#(1)) dctrl_rwstop <- mkReg(0);
	Reg#(Bit#(1)) dctrl_rwstart <- mkReg(0);
	Reg#(Bit#(4)) dctrl_dbblocksize <- mkReg(0);
	Reg#(Bit#(1)) dctrl_dmaen <- mkReg(0);
	Reg#(Bit#(1)) dctrl_dtmode <- mkReg(0);
	Reg#(Bit#(1)) dctrl_dtdir <- mkReg(0);
	Reg#(Bit#(1)) dctrl_dten <- mkReg(0);
	Reg#(Bit#(32)) dctrl = concatReg10(readOnlyReg(20'd0),dctrl_sdioen,dctrl_rwmod,dctrl_rwstop,dctrl_rwstart,dctrl_dbblocksize,dctrl_dmaen,dctrl_dtmode,dctrl_dtdir,dctrl_dten);
	
	//SDMMC_DCOUNT
	Reg#(Bit#(25)) datacount <- mkReg(0);
	Reg#(Bit#(32)) dcount = concatReg2(readOnlyReg(7'd0),datacount);
	
	//SDMMC_STA
	Reg#(Bit#(1)) sta_sdioit <- mkReg(0);
	Reg#(Bit#(1)) sta_rxdavl <- mkReg(0);
	Reg#(Bit#(1)) sta_txdavl <- mkReg(0);
	Reg#(Bit#(1)) sta_rxfifoe <- mkReg(0);
	Reg#(Bit#(1)) sta_txfifoe <- mkReg(0);
	Reg#(Bit#(1)) sta_rxfifof <- mkReg(0);
	Reg#(Bit#(1)) sta_txfifof <- mkReg(0);
	Reg#(Bit#(1)) sta_rxfifohf <- mkReg(0);
	Reg#(Bit#(1)) sta_txfifohe <- mkReg(0);
	Reg#(Bit#(1)) sta_rxact <- mkReg(0);
	Reg#(Bit#(1)) sta_txact <- mkReg(0);
	Reg#(Bit#(1)) sta_cmdact <- mkReg(0);
	Reg#(Bit#(1)) sta_dbckend <- mkReg(0);
	//1 bit reserved readOnlyReg(1'd0)
	Reg#(Bit#(1)) sta_dataend <- mkReg(0);
	Reg#(Bit#(1)) sta_cmdsent <- mkReg(0);
	Reg#(Bit#(1)) sta_cmdrend <- mkReg(0);
	Reg#(Bit#(1)) sta_rxoverr <- mkReg(0);
	Reg#(Bit#(1)) sta_txunderr <- mkReg(0);
	Reg#(Bit#(1)) sta_dtimeout <- mkReg(0);
	Reg#(Bit#(1)) sta_ctimeout <- mkReg(0);
	Reg#(Bit#(1)) sta_dcrcfail <- mkReg(0);
	Reg#(Bit#(1)) sta_ccrcfail <- mkReg(0);
	Reg#(Bit#(32)) sta = concatReg24(readOnlyReg(9'd0),sta_sdioit,sta_rxdavl,sta_txdavl,sta_rxfifoe,sta_txfifoe,sta_rxfifof,sta_txfifof,sta_rxfifohf,sta_txfifohe,sta_rxact,sta_txact,sta_cmdact,sta_dbckend,readOnlyReg(1'd0),sta_dataend,sta_cmdsent,sta_cmdrend,sta_rxoverr,sta_txunderr,sta_dtimeout,sta_ctimeout,sta_dcrcfail,sta_ccrcfail);
	
	
	//SDMMC_ICR
	Reg#(Bit#(1)) icr_sdioitc <- mkReg(0);
	//11 bit reserved readOnlyReg(11'd0)
	Reg#(Bit#(1)) icr_dbckendc <- mkReg(0);
	//1 bit reserved readOnlyReg(1'd0)
	Reg#(Bit#(1)) icr_dataendc <- mkReg(0);
	Reg#(Bit#(1)) icr_cmdsentc <- mkReg(0);
	Reg#(Bit#(1)) icr_cmdrendc <- mkReg(0);
	Reg#(Bit#(1)) icr_rxoverrc <- mkReg(0);
	Reg#(Bit#(1)) icr_txunderrc <- mkReg(0);
	Reg#(Bit#(1)) icr_dtimeoutc <- mkReg(0);
	Reg#(Bit#(1)) icr_ctimeoutc <- mkReg(0);
	Reg#(Bit#(1)) icr_dcrcfailc <- mkReg(0);
	Reg#(Bit#(1)) icr_ccrcfailc <- mkReg(0);
	Reg#(Bit#(32)) icr = concatReg14(readOnlyReg(9'd0),icr_sdioitc,readOnlyReg(11'd0),icr_dbckendc,readOnlyReg(1'd0),icr_dataendc,icr_cmdsentc,icr_cmdrendc,icr_rxoverrc,icr_txunderrc,icr_dtimeoutc,icr_ctimeoutc,icr_dcrcfailc,icr_ccrcfailc);
	
	
	//SDMMC_MASK
	Reg#(Bit#(1)) mask_sdioitie <- mkReg(0);
	Reg#(Bit#(1)) mask_rxdavlie <- mkReg(0);
	Reg#(Bit#(1)) mask_txdavlie <- mkReg(0);
	Reg#(Bit#(1)) mask_rxfifoeie <- mkReg(0);
	Reg#(Bit#(1)) mask_txfifoeie <- mkReg(0);
	Reg#(Bit#(1)) mask_rxfifofie <- mkReg(0);
	Reg#(Bit#(1)) mask_txfifofie <- mkReg(0);
	Reg#(Bit#(1)) mask_rxfifohfie <- mkReg(0);
	Reg#(Bit#(1)) mask_txfifoheie <- mkReg(0);
	Reg#(Bit#(1)) mask_rxactie <- mkReg(0);
	Reg#(Bit#(1)) mask_txactie <- mkReg(0);
	Reg#(Bit#(1)) mask_cmdactie <- mkReg(0);
	Reg#(Bit#(1)) mask_dbckendie <- mkReg(0);
	//1 bit reserved readOnlyReg(1'd0)
	Reg#(Bit#(1)) mask_dataendie <- mkReg(0);
	Reg#(Bit#(1)) mask_cmdsentie <- mkReg(0);
	Reg#(Bit#(1)) mask_cmdrendie <- mkReg(0);
	Reg#(Bit#(1)) mask_rxoverrie <- mkReg(0);
	Reg#(Bit#(1)) mask_txunderrie <- mkReg(0);
	Reg#(Bit#(1)) mask_dtimeoutie <- mkReg(0);
	Reg#(Bit#(1)) mask_ctimeoutie <- mkReg(0);
	Reg#(Bit#(1)) mask_dcrcfailie <- mkReg(0);
	Reg#(Bit#(1)) mask_ccrcfailie <- mkReg(0);
	Reg#(Bit#(32)) maskreg = 				concatReg24(readOnlyReg(9'd0),mask_sdioitie,mask_rxdavlie,mask_txdavlie,mask_rxfifoeie,mask_txfifoeie,mask_rxfifofie,mask_txfifofie,mask_rxfifohfie,mask_txfifoheie,mask_rxactie,mask_txactie,mask_cmdactie,mask_dbckendie,readOnlyReg(1'd0),mask_dataendie,mask_cmdsentie,mask_cmdrendie,mask_rxoverrie,mask_txunderrie,mask_dtimeoutie,mask_ctimeoutie,mask_dcrcfailie,mask_ccrcfailie);
	
	//SDMMC_FIFOCNT
	Reg#(Bit#(24)) fifocount <- mkReg(0);
	Reg#(Bit#(32)) fifocnt = concatReg2(readOnlyReg(8'd0),fifocount);
	
	
	//SDMMC_FIFO
	Reg#(Bit#(32))  fifo_reg  <- mkReg(0);
	
	//Register required for rules are defined here.
	Reg#(Bit#(48))  rg_packet_tx  <- mkReg(0);
	Reg#(Bit#(136))  rg_packet_rx  <- mkReg(0);
	Reg#(Bit#(1))  rg_transmit  <- mkReg(0);
	Wire#(Bit#(1))  wr_transmit  <- mkWire();
	
	Reg#(Bit#(4))  rg_state  <- mkReg(10);
	Reg#(Bit#(6))  rg_counter <- mkReg(0);
	Reg#(Bit#(1))  rg_clock  <- mkReg(0);
	Reg#(Bit#(1))  rg_clk_old  <- mkReg(0);
	Reg#(Bit#(8))  rg_clock_counter  <- mkReg(0);
	Reg#(Bit#(7))  rg_wait_counter  <- mkReg(0);
	
	Reg#(Bit#(8))   rg_rx_counter  <- mkReg(0);
	Reg#(Bit#(48))  rg_receive_data  <- mkReg(0);
	
	Reg#(Bit#(6))	send_count <- mkReg(0);
	
	Reg#(Bit#(1))      rg_start <- mkReg(0);
	Reg#(Bit#(1))      rg_status <- mkReg(0);
	
	rule cmd_idle (rg_state == 0);          
         crc7.crc_input({1'b0,1'b1,cmd[5:0],arg});
	     //rg_packet<= {1'b0,1'b1,cmd[5:0],arg,}
	     if (crc7.status() == 0) begin
	       rg_state <= 1;
	     end			
	endrule
	
	rule cmd_crc_cal (rg_state == 1);
	
	  	let lv_crc = crc7.crc_output();
	  	$display("CRC_Generated = %b \n",lv_crc); 	    
	   	rg_packet_tx  <= {1'b0,lv_crc,arg,cmd[5:0],1'b1,1'b0};
	   	//$display("cmd_cpsmen = %b",cmd_cpsmen);
	    //$display("cmd_waitpend = %b",cmd_waitpend);
	    //$display("CMD Register = %b",cmd);
	    //$display("ARG Register = %b",arg);
	    //$display("CLKCR  = %b",clkcr); 
		if ((cmd_cpsmen == 1'b1) && (cmd_waitpend==0)) begin      // Transition to Send state  
		    rg_state <= 3;
		    //$display("state = %b",rg_state); 
		end
		else if ( (cmd_cpsmen== 1'b1) && (cmd_waitpend == 1'b1) ) begin // Transition to Pend state
			rg_state <= 2;
		end
	
	endrule
	
	rule cmd_pending (rg_state == 2); 
	
		if (cmd_cpsmen == 0) begin       // Transition to Idle state
		    rg_state <= 0;
		end
		else begin
		// Some conditions are required like wait for last data bit to get transferred and internal signal cmdpend.
		rg_state <= 3;
		end
		
	endrule
	
	rule cmd_sending (rg_state == 3); 
          	
		if ((rg_clk_old & ~ rg_clock) == 1 ) begin
		    rg_transmit <= rg_packet_tx[0];		          
		    rg_packet_tx  <= rg_packet_tx  >> 1;
		    send_count <= send_count + 1;
		    $display("rg_transmit = %b",rg_transmit);
		    $display("rg_packet_tx = %b \n",rg_packet_tx); 
		end          
	    if (send_count == 48) begin   
		  if ((cmd_waitresp == 2'b01) || (cmd_waitresp == 2'b11 ))
		   begin 
		   	$display("state = %b",rg_state);
		  	 rg_state <= 4;
		  	 $display("CMD SENT SUCCESSFULLY AND WAITING FOR RESPONSE.");
		  end 
		  else begin
		  	 	
		     rg_state <= 10; 
		     $display("CMD SENT SUCCESSFULLY");
		  end
	    end 
	   
	endrule
	
	
	rule cmd_wait (rg_state == 4); 
		// Note: The command timeout has a fixed value of 64 SDMMC_CK clock periods.
		// Wait for start bit of response'
		
		rg_wait_counter <= rg_wait_counter + 1;
		
		if ((rg_wait_counter == 64) || (cmd_cpsmen == 0)) begin
			$display("TIMEOUT , SADLY RESPONSE DID NOT CAME.");
		    rg_state <= 10;  // Move to Idle state either when timeout or cpsm is disabled
		      				// Set ERROR bit in Status Register
		end
		else if (wr_sdio_cmd_in == 1'b0) begin
			$display("wr_sdio_cmd_in");
		    	rg_state <= 5;  // Move to Receive state
		end

	endrule
	
	// Rule for Receive state
	rule cmd_receive (rg_state == 5);
	$display("cmd_receive");
      if ((rg_clk_old & ~ rg_clock) == 1 ) begin
		if ((cmd_cmdindex == 6'b000010) || (cmd_cmdindex == 6'b001010) || (cmd_cmdindex == 6'b001001 ))	
		begin
        	rg_packet_rx <= { rg_packet_rx[134:0],wr_sdio_cmd_in};
        	rg_rx_counter <= rg_rx_counter + 1 ;
        end
        else begin
        	rg_packet_rx <= zeroExtend({ rg_packet_rx[46:0],wr_sdio_cmd_in});
        	rg_rx_counter <= rg_rx_counter + 1 ;
        end
		// Wait for all response bits to be received
		if ( ((cmd_cmdindex == 6'b000010) || (cmd_cmdindex == 6'b001010) || (cmd_cmdindex == 6'b001001 )) && (rg_rx_counter == 135)) begin
		    resp1 <= rg_packet_rx[127:96];
		    resp2 <= rg_packet_rx[95:64];
		    resp3 <= rg_packet_rx[63:32];
		    resp4 <= rg_packet_rx[31:0];
		    respcmd_respcmd <= 6'b111111 ;
		    
		end
		else if ( ((cmd_cmdindex != 6'b000010) || (cmd_cmdindex != 6'b001010) || (cmd_cmdindex != 6'b001001 )) && (rg_rx_counter == 46) ) 			begin
		    // Verify CRC
		    crc7.crc_input(rg_packet_rx[47:8]);
		    rg_state <= 6;
		end
	  end
	endrule
	
	rule cmd_final (rg_state == 6);
	
			let expected_crc = crc7.crc_output();
		    if (rg_packet_rx[7:1] == expected_crc) begin
		    	resp1 <= rg_packet_rx[31:0];
		        respcmd_respcmd <= cmd_cmdindex ; // With Command Index of Previous command for which response was received.
		        rg_state <= 0;      // Return to Idle state //Status Register
		    end
		    else begin
		        rg_state <= 0;      // Move to Idle state
		        //sr_error <= 1;      // Set ERROR bit in SR
		    end
	endrule
	
	rule idle (rg_state == 10);
		$display("NOW IN STATE 10");
		
		//Waiting for SDMMC_CMD register to written.
		if (rg_start == 1 ) begin
		rg_status <= 1; 
		rg_state <= 0; 
		rg_start<= 0; end
		 else begin 
		  rg_status <= 0; end
	endrule
	
	
	// Rule for SDIO clock management
	
	rule rl_sdio_clock_generator;
	
	rg_clk_old <= rg_clock;
	    
		if(rg_clock_counter == clkcr_clkdiv) begin
			rg_clock_counter <= 0 ;
			rg_clock <= ~rg_clock;
			
		end
		
		else begin
			rg_clock_counter <= rg_clock_counter + 1;
		end
		
		
	endrule

	// Write Request and Read Request
	
	method ActionValue#(Bool) write_req(Bit#(64) addr, Bit#(64) data, AccessSize size);
		Bool succ = True;
		if(addr[7:0] == `POWER && size == Word) //32 bits thats why size == 2 , 16 size == 1 , 8 size == 0
			power <= truncate(data);
		else if(addr[7:0] == `CLKCR && size == Word)
			clkcr <= truncate(data);
		else if(addr[7:0] == `ARG && size == Word)
			arg <= truncate(data);
		else if(addr[7:0] == `CMD && size == Word) begin 
			cmd <= truncate(data); 
			rg_start <= 1;  end
		//RESPCMD and CMDRESP1234 is readonly since r is written
		else if(addr[7:0] == `DTIMER && size == Word)
			dtimer <= truncate(data); 
		else if(addr[7:0] == `DLEN && size == DWord)
			dlen <= truncate(data);
		else if(addr[7:0] == `DCTRL && size == Word)
			dctrl <= truncate(data); 
		//DCOUNT , STA is readonly since r is written
		else if(addr[7:0] == `ICR && size == Word)
			icr <= truncate(data);
		else if(addr[7:0] == `MASK && size == Word)
			maskreg <= truncate(data); 
		//FIFOCNT is readonly since r is written		
		else if(addr[7:0] == `DATAFIFO && size == Word)
			fifo_reg <= truncate(data);
		else
			succ = False;
		//`logLevel( sdio, 0, $format("SDIO : write req : addr = %h, data = %h, succ = %h \n",addr,data,succ))   
		return succ;		
	endmethod
	
	method ActionValue#(Tuple2#(Bool, Bit#(64))) read_req(Bit#(64) addr, AccessSize size);
		Bool succ = True;
		Bit#(64) data = 0;
		if(addr[7:0] == `POWER && size == Word) //32 bits thats why size == 2 , 16 size == 1 , 8 size == 0
			data = duplicate(power);
		else if(addr[7:0] == `CLKCR && size == Word)
			data = duplicate(clkcr);
		else if(addr[7:0] == `ARG && size == Word)
			data = duplicate(arg);
		else if(addr[7:0] == `CMD && size == Word) 
			data = duplicate(cmd);
			
		//RESPCMD and CMDRESP1234 is readonly since r is written
		else if(addr[7:0] == `RESPCMD && size == Word) 
			data = duplicate(respcmd);
		else if(addr[7:0] == `RESP1 && size == Word) 
			data = duplicate(resp1);
		else if(addr[7:0] == `RESP2 && size == Word) 
			data = duplicate(resp2);
		else if(addr[7:0] == `RESP3 && size == Word) 
			data = duplicate(resp3);
		else if(addr[7:0] == `RESP4 && size == Word) 
			data = duplicate(resp4); 
		
		else if(addr[7:0] == `DTIMER && size == Word)
			data = duplicate(dtimer);
		else if(addr[7:0] == `DLEN && size == Word)
			data = duplicate(dlen);
		else if(addr[7:0] == `DCTRL && size == Word)
			data = duplicate(dctrl); 
		//DCOUNT , STA is readonly since r is written
		
		else if(addr[7:0] == `DCOUNT && size == Word)
			data = duplicate(dcount);
		else if(addr[7:0] == `STA && size == Word)
			data = duplicate(sta);
			
		else if(addr[7:0] == `ICR && size == Word)
			data = duplicate(icr);
		else if(addr[7:0] == `MASK && size == Word)
			data = duplicate(maskreg);
			
		//FIFOCNT is readonly since r is written
		else if(addr[7:0] == `FIFOCNT && size == Word)
			data = duplicate(fifocnt);
						
		else if(addr[7:0] == `DATAFIFO && size == Word)
			data = duplicate(fifo_reg);
			
		else if(addr[7:0] == `FINISH && size == Word)
			data = duplicate(rg_status);
		else
			succ = False;
		//`logLevel( sdio, 0, $format("SDIO : read req : addr = %h, data = %h, succ = %h \n",addr,data,succ))   
		return tuple2(succ,data);
		
	endmethod
	
	interface SDIO io;
	
		method Action  cmd_i (Bit#(1) val); //CMD INPUT
			wr_sdio_cmd_in <= val;
		endmethod
		
		method Bit#(1) cmd_out; //CMD OUTPUT
			return rg_transmit;
		endmethod
		
		method Action  data_i (Bit#(8) data_in); //8 bit Data Input
			wr_sdio_data_in <= data_in;
		endmethod
		
		method Bit#(8) data_out; //8 bit Data Output
			return sd_data_output;
		endmethod 
	
		method bit clk;
			return rg_clk_old;		
		endmethod
		
		//method Bit#(8) data_enable;
		
		//endmethod
		
		//method Bit#(1) cmd_enable;
		
		//endmethod
		
	endinterface
	
	
	endmodule : mksdio_controller
	endpackage
