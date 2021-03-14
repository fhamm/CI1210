-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2014-2 trabalho semestral, autor: Roberto Hexsel, 26out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- testbench para circuito de controle do Urubu
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.p_wires.all;

entity tb_pid is
end tb_pid;

architecture TB of tb_pid is

  component read_data_file is
    generic (INPUT_FILE_NAME : string := "input.data");
    port (rst,clk : in    bit;
          data    : out   reg32;
          eof     : out   boolean);
  end component read_data_file;

  component urubu is
    port (rst,clk : in    bit;
          lambda  : in    reg32;
          epsilon : out   reg32);
  end component urubu;

  component pid is
    port (rst,clk : in    bit;
          sigma   : in    reg32;
          epsilon : in    reg32;
          lambda  : out   reg32);
  end component pid;

  signal clk,rst : bit;
  signal r_nxt, r_rdy, w_sel, w_rdy: bit;
  signal set_point, epsilon, lambda : reg32;
  signal r_eof : boolean := FALSE;
  
begin  -- TB

  U_read_input: read_data_file generic map ("input.data")
    port map (rst,clk, set_point, r_eof);

  U_pid: pid port map (rst,clk, set_point, epsilon, lambda);

  U_urubu: urubu port map (rst,clk, lambda, epsilon);


  U_clock: process
  begin
    clk <= '1';      -- executa e
    wait for t_clock_period / 2;  -- espera meio ciclo
    clk <= '0';      -- volta a executar e
    wait for t_clock_period / 2;  -- espera meio ciclo e volta ao topo
  end process;

  U_reset: process
  begin
    rst <= '1';      -- executa e
    wait for t_clock_period * 2.0;  -- espera por 2 ciclos
    rst <= '0';      -- volta a executar e
    wait;            -- espera para sempre
  end process;

  assert r_eof =FALSE
    report " --###--fim da simulacao--###-- "
    severity failure;

  
end TB;
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


----------------------------------------------------------------
configuration CFG_TB of tb_pid is
	for TB
        end for;
end CFG_TB;
----------------------------------------------------------------
