package testbench;

import sdio_cmd::*;

module tbsdio();      
          Ifc_sdio_controller sdio <- mksdio_controller();            
          Reg#(Bit#(32)) rg_state <- mkReg(0);
          Reg#(Bit#(32)) rg_count <- mkReg(0); 
          rule cmd;
          sdio.io.cmd_i(1);
          endrule
          
	    rule step1(rg_state == 0);
	      	 $display("CLKCR REGISTER VALUE UPDATED : %b ",sdio.write_req(64'h0000_0004, 64'h0000_0000_0000_010A,Word));
	      	 $display("ARGUMENT REGISTER VALUE UPDATED : %b",sdio.write_req(64'h0000_0008, 64'h5555_5555_5555_5555,Word)); //argument register
	      	 $display("CMD REGISTER VALUE UPDATED : %b \n",sdio.write_req(64'h0000_000C, 64'h0000_0000_0000_0400,Word));
	      //	 $display("CMD REGISTER VALUE UPDATED : %b \n",sdio.write_req(64'h0000_000C, 64'h0000_0000_0000_044D,Word)); //CMD13 Expect Response.
		 	 rg_state <= 1 ;
             //$display("state = ",rg_state);
        endrule 

        rule step2(rg_state == 1);
             
             //$display("%b",sdio.read_req(64'h0000_0004,Word));
	      	 //$display("%b",sdio.read_req(64'h0000_0008,Word));
	      	 //$display("%b",sdio.read_req(64'h0000_000C,Word));
             //rg_count <= rg_count + 1;
             //$display("cycel = ",rg_count);
             rg_state <= 2 ;
             //$display("tb_state = ",rg_state);
         endrule


		rule step3(rg_state == 2);
          	let {x,y} <- sdio.read_req(64'h0000_0084,Word) ;  
              //$display("status = %b",y); 
              
            if( y[0] == 0 ) begin
             	   $display("finish_testbench "); 
             	  $finish();
             end
             else begin
            	rg_count <= rg_count + 1;
    		 end
            // rg_state <= 3 ;
             //$display("tb_state = ",rg_state);
           
         endrule
     
 endmodule 
 
 endpackage
