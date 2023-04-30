
package CRC7;

// Define interface for CRC Generator
interface Ifc_Crc;
	// Method to input data for CRC generation
	method Action crc_input (Bit#(40) inp);
	// Method to output the calculated CRC value
	method Bit#(7) crc_output ();
	method Bit#(1) status(); 
endinterface 
// Define CRC generator module
module mk_Crc_Generator (Ifc_Crc);
    
        // Define shift register to hold input data
        Reg#(Bit#(8)) shift_reg <- mkReg(0);
        // Define register to hold input data
        Reg#(Bit#(40)) input_reg <- mkReg(0); 
        // Define register to keep track of state
        Reg#(Bit#(2)) state <- mkReg(0);
        // Define register to count number of bits processed
        Reg#(Bit#(8)) count <- mkReg(0);
        
        // CRC generation rule
        rule crc if (state == 1);
                
            // Shift input register to left by 1 bit
            input_reg <= input_reg << 1;  
           
            // Calculate XOR of MSB of input register and LSB of shift register
            Bit#(1) xor_val = (input_reg[39] ^ shift_reg[6]);
        
            // If XOR value is 1, perform CRC operation on shift register
            if (xor_val == 1) begin
                shift_reg <=((shift_reg << 1) ^ 8'b10001001);  // SDMMC CRC7 polynomial: x^7 + x^3 + 1 = 10001001
            end
            // If XOR value is 0, shift the shift register to left by 1 bit
            else begin 
                shift_reg <= (shift_reg << 1);
            end 
         
            // If count equals 39 (i.e. all 40 bits have been processed), set state to 2
            if (count ==  39) begin 
               state <= 2; 
            end     
            // Else increment count
            else begin  
               count <= count + 1; 
            end  
              
        endrule 
             
        // Method to input data for CRC generation
        method Action crc_input (Bit#(40) inp);            
            input_reg <= inp; 
            state <= 1 ;             
        endmethod
          
          method Bit#(1) status();
           if(state == 1) begin 
               return(1); end 
             else begin
               return(0); end 
          endmethod  
        // Method to output the calculated CRC value
        method Bit#(7) crc_output() if (state == 2);
        	//$display("CRC_Generated = %b",shift_reg[6:0]);
            return shift_reg[6:0] ;
        endmethod 
    
endmodule: mk_Crc_Generator

// Define top-level testbench module
module mkTbtop(Empty);
  
    // Instantiate CRC generator module
    Ifc_Crc crc_gen <- mk_Crc_Generator();
    
    // Define register to keep track of state
    Reg#(Bit#(2)) state <- mkReg(0);

    // Test rule to input data for CRC generation
    rule test1 (state == 0);
        crc_gen.crc_input(40'b1010101010101010101010101010101010101010);   
        state <= 1;  
    endrule 
   
    // Test rule to output calculated CRC value
    rule test2 (state == 1);    
        $display("CRC_Generated = %b", crc_gen.crc_output());
        $finish();
    endrule 
   
endmodule : mkTbtop
   
endpackage

