-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2015-2 trabalho semestral, autor: Roberto Hexsel, 27out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- ESTE ARQUIVO NAO PODE SER ALTERADO

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- le um arquivo, um inteiro por vez
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;
use work.p_wires.all;

entity read_data_file is
  generic (INPUT_FILE_NAME : string := "input.data");
  port (rst, clk : in    bit;
        data  : out   bit_vector;
        eof   : out   boolean);
end entity read_data_file;

architecture behavioral of read_data_file is

  type uint_file_type is file of integer;
  file input_file: uint_file_type open read_mode is INPUT_FILE_NAME;
  
begin

  U_read: process
    variable datum : integer := 0;
  begin

    eof <= FALSE;
    
    while not endfile(input_file) loop

      wait until rising_edge(clk);

      read( input_file, datum );

      -- assert FALSE report "fileRD: " & integer'image( datum ); -- DEBUG
      
      data <= SLV2BV32( std_logic_vector(to_signed(datum,32)) );

    end loop;
    
    eof <= TRUE;
    wait;
    
  end process U_read;

end behavioral;
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- escreve um inteiro na saida padrao do simulador
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;
use work.p_wires.all;

entity write_int is
  port (rst, clk : in    bit;
        sel   : in    bit;              -- active in '1'
        data  : in    reg32);
end write_int;

architecture behavioral of write_int is

  file output : text open write_mode is "STD_OUTPUT";
  
begin

  U_write_uint: process(clk, sel)
    variable msg : line;
  begin
    
    if rising_edge(clk) and sel = '1' then
      write ( msg, string'(BV32HEX(data)) );
      writeline( output, msg );
    end if;
      
  end process U_write_uint;

end behavioral;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- escreve sete inteiros na saida padrao do simulador
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;
use work.p_wires.all;

entity write_int7 is
  port (rst,clk : in  bit;
        sigma   : in  integer;
        epsilon : in  integer;
        delta   : in  integer;
        prop    : in  integer;        
        integr  : in  integer;
        deriv   : in  integer;
        lambda  : in  integer);
end write_int7;

architecture behavioral of write_int7 is

  file output : text open write_mode is "STD_OUTPUT";
  
begin

  U_write_uint: process(clk)
    variable msg : line;
  begin
    
    if rising_edge(clk) then
      write ( msg, integer'image(sigma));
      write ( msg, ' ');
      write ( msg, integer'image(epsilon));
      write ( msg, ' ');
      write ( msg, integer'image(delta));
      write ( msg, ' ');
      write ( msg, integer'image(prop));
      write ( msg, ' ');
      write ( msg, integer'image(integr));
      write ( msg, ' ');
      write ( msg, integer'image(deriv));
      write ( msg, ' ');
      write ( msg, integer'image(lambda));
      writeline( output, msg );
    end if;
      
  end process U_write_uint;

end behavioral;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
