-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2015-2 trabalho semestral, autor: Roberto Hexsel, 04mar21
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- controlador proporcional-integral-derivativo
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
library ieee; use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity pid is
  port (rst,clk : in    bit;
        sigma   : in    reg32;
        epsilon : in    reg32;
        lambda  : out   reg32);
end pid;

architecture functional of pid is

  component write_int7 is
    port (rst,clk : in  bit;
          sigma   : in  integer;
          epsilon : in  integer;
          delta   : in  integer;
          prop    : in  integer;
          integr  : in  integer;
          deriv   : in  integer;
          lambda  : in  integer);
  end component write_int7;
  
  constant k_prop   : integer := 4;
  constant k_integr : integer := 2;
  constant k_deriv  : integer := 4;

  -- valor será deslocado para a direita para reduzir peso de cada fator
  constant k_contrib : integer := k_prop + k_integr + k_deriv;
  
  -- declaracao dos componentes
  -- registradores, somadores

  

  -- declaracao dos sinais internos INTEIROS
  signal i_sigma, i_epsilon, i_delta : integer := 0;
  signal i_prop, i_integr, i_deriv, i_lambda : integer := 0;


  -- declaracao dos bit-vectors equivalentes (se necessário)
  signal delta : reg32; -- como exemplo 
  
  
begin  -- functional

  i_sigma   <= to_integer(signed(to_stdlogicvector(sigma)));  -- bit2integer
  i_epsilon <= to_integer(signed(to_stdlogicvector(epsilon)));

  -- essas expressoes devem ser trocadas para circuitos
  i_delta   <=  i_sigma - i_epsilon;

  i_prop    <= i_delta * k_prop;

  -- esse processo devemser trocado para circuito(s)
  U_integral: process(clk, rst)
    variable sum: integer := 0;
  begin
    if rst = '1' then
      sum := 0;
    elsif rising_edge(clk) then
      sum := sum + (i_delta * k_integr)/8; -- minimizar efeito da integral
    end if;
    i_integr <= sum;
  end process U_integral;


  -- esse processo deve ser trocado para circuito(s)
  U_derivada: process(clk, rst)
    variable old, diff: integer := 0;
  begin
    if rst = '1' then
      old  := 0;
      diff := 0;
    elsif rising_edge(clk) then
      diff := (i_sigma - old) * k_deriv;
      old  := i_sigma;
    end if;
    i_deriv <= diff;
  end process U_derivada;

  -- ameniza o efeito das contribuicoes de P, I, D
  i_lambda <= (i_prop + i_integr + i_deriv) / k_contrib;

  -- lambda e a saida do circuito e deve ser implementada (veja U_write)
  lambda <= SLV2BV32(std_logic_vector(to_signed(i_lambda, 32)));


  
  -- NAO ALTERE ESTA PARTE
  -- sinais para depuracao, usados para gerar o grafico
  -- estes sinais com valores inteiros devem ser gerados pelo seu modelo,
  --  a partir dos bits das saidas dos circuitos
  U_write: write_int7 port map (rst, clk,
                                i_sigma,i_epsilon,i_delta,
                                i_prop,i_integr,i_deriv,i_lambda);
  
end functional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



-- ----------------------------------------------------------------------
--  modelo do Urubu
--  nao altere esse modelo
-- ----------------------------------------------------------------------
library ieee; use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.p_wires.all;

entity urubu is
  port (rst,clk : in    bit;
        lambda  : in    reg32;
        epsilon : out   reg32);
end urubu;

architecture functional of urubu is

  constant linear_limit : integer := 1;
  constant linearity_factor : integer := 0;  -- deve ser >= 0

  component registerN is
    generic (NUM_BITS: integer; INIT_VAL: bit_vector);
    port(clk, rst, ld: in  bit;
         D:            in  bit_vector;
         Q:            out bit_vector);
  end component registerN;

  signal dly_1, dly_2, dly_3, dly_4 : integer := 0;
  signal dly_5, dly_6, dly_7, dly_8, i_lambda : integer := 0;
  signal val_1, val_2, val_3, val_4 : integer := 0;
  signal val_5, val_6, val_7, val_8 : integer := 0;
  signal i_linear, i_error : integer := 0;
begin

  -- sistema linear, com atraso

  i_lambda <= to_integer(signed(to_stdlogicvector(lambda)));

  -- simula nao-linearidade
  --   altere linear_limit e linearity_factor para experimentar
  U_linear: process(i_lambda)
  begin
    if i_lambda >= 0 then
      if i_lambda < linear_limit then
        i_linear <= i_lambda;
      else
        i_linear <= i_lambda + (i_lambda * linearity_factor);
      end if;

    else  -- i_lambda < 0

      if i_lambda > (0 - linear_limit)   then
        i_linear <= i_lambda;
      else
        i_linear <= i_lambda + (i_lambda * linearity_factor);
      end if;

    end if;
  end process;


  -- simula atraso/inercia na resposta do Urubu
  U_dly1: process(clk)
  begin
    if rising_edge(clk) then dly_1 <= i_linear; end if;
  end process;

  U_dly2: process(clk)
  begin
    if rising_edge(clk) then dly_2 <= dly_1; end if;
  end process;

  U_dly3: process(clk)
  begin
    if rising_edge(clk) then dly_3 <= dly_2; end if;
  end process;

  U_dly4: process(clk)
  begin
    if rising_edge(clk) then dly_4 <= dly_3; end if;
  end process;

  U_dly5: process(clk)
  begin
    if rising_edge(clk) then dly_5 <= dly_4; end if;
  end process;

  U_dly6: process(clk)
  begin
    if rising_edge(clk) then dly_6 <= dly_5; end if;
  end process;

  U_dly7: process(clk)
  begin
    if rising_edge(clk) then dly_7 <= dly_6; end if;
  end process;

  U_dly8: process(clk)
  begin
    if rising_edge(clk) then dly_8 <= dly_7; end if;
  end process;

  val_1 <= (dly_1) / 4;  -- 1/4
  val_2 <= (dly_2) / 4;  -- 1/4
  val_3 <= (dly_3) / 8;  -- 1/8
  val_4 <= (dly_4) / 8;  -- 1/8
  val_5 <= (dly_5) / 16;  -- 1/16
  val_6 <= (dly_6) / 16;  -- 1/16
  val_7 <= (dly_7) / 16;  -- 1/16
  val_8 <= (dly_8) / 16;  -- 1/16
  
  i_error <= val_1 + val_2 + val_3 + val_4 + val_5 + val_6 + val_7 + val_8;
  
  epsilon <= SLV2BV32(std_logic_vector(to_signed(i_error, 32)));

end architecture functional;
-- ----------------------------------------------------------------------

