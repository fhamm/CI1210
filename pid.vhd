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

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador completo de um bit, modelo estrutural
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity addBit is
  port(bitA, bitB, vem : in bit;    -- entradas A,B,vem-um
       soma, vai       : out bit);  -- saida C,vai-um
end addBit;

architecture estrutural of addBit is 
  component and2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component and2;

  component or3 is generic (prop:time);
                      port (A,B,C: in bit; S: out bit);
  end component or3;

  component xor3 is generic (prop:time);
                      port (A,B,C: in bit; S: out bit);
  end component xor3;

  signal a1,a2,a3: bit;
begin
  U_xor:  xor3 generic map ( t_xor3 ) port map ( bitA, bitB, vem, soma );

  U_and1: and2 generic map ( t_and2 ) port map ( bitA, bitB, a1 );
  U_and2: and2 generic map ( t_and2 ) port map ( bitA, vem,  a2 );
  U_and3: and2 generic map ( t_and2 ) port map ( vem,  bitB, a3 );
  U_or:   or3  generic map ( t_or3  ) port map ( a1, a2, a3, vai );

end estrutural;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- adiantamento de vai-um de 4 bits
--  P&H,2ndEd,sec4.5, RH sec1.6+8.3.2
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity adianta4 is
  port(a,b : in reg4;           -- entradas A(i),B(i)
       vem : in bit;            -- vem-um
       c: out reg4              -- vai(i)
       );
end adianta4;

architecture adianta4 of adianta4 is 
  component and2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component and2;
  component or2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component or2;

  signal p,g : reg4;
begin

  U_a0: and2 generic map ( t_and2 ) port map ( a(0), b(0), g(0) );
  U_a1: and2 generic map ( t_and2 ) port map ( a(1), b(1), g(1) );
  U_a2: and2 generic map ( t_and2 ) port map ( a(2), b(2), g(2) );
  U_a3: and2 generic map ( t_and2 ) port map ( a(3), b(3), g(3) );  

  U_o0: or2 generic map ( t_or2 ) port map ( a(0), b(0), p(0) );
  U_o1: or2 generic map ( t_or2 ) port map ( a(1), b(1), p(1) );
  U_o2: or2 generic map ( t_or2 ) port map ( a(2), b(2), p(2) );
  U_o3: or2 generic map ( t_or2 ) port map ( a(3), b(3), p(3) );

  c(0) <= g(0) or (p(0) and vem) after t_and2+t_or2;
  c(1) <= g(1) or (p(1) and g(0)) or (p(1) and p(0) and vem)
          after t_and3+t_or3;
  c(2) <= g(2) or (p(2) and g(1)) or (p(2) and p(1) and g(0)) or
          (p(2) and p(1) and p(0) and vem) after t_and4+t_or4;
  c(3) <= g(3) or (p(3) and g(2)) or (p(3) and p(2) and g(1)) or
          (p(3) and p(2) and p(1) and g(0)) or
          (p(3) and p(2) and p(1) and p(0) and vem)
          after t_and5+t_or5;

end adianta4;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- adiantamento de vai-um de 16 bits
--  P&H,2ndEd,sec4.5, RH sec1.6+8.3.2
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity adianta16 is
  port(a,b : in reg16;          -- entradas A(i),B(i)
       vem : in bit;            -- vem-um
       c: out reg4              -- vai(i), de 4 em 4 bits
       );
end adianta16;

architecture adianta16 of adianta16 is 
  signal p,g : reg16;
  signal pp,gg,cc : reg4;
begin

  gen: for i in 15 downto 0 generate
    g(i) <= reject (contam * t_and2) inertial (a(i) and b(i)) after t_and2;
    p(i) <= reject (contam * t_or2)  inertial (a(i) or  b(i)) after t_or2;
  end generate gen;


  pp(0) <= p(3) and p(2) and p(1) and p(0) after t_and4;
  pp(1) <= p(7) and p(6) and p(5) and p(4) after t_and4;
  pp(2) <= p(11) and p(10) and p(9) and p(8) after t_and4;
  pp(3) <= p(15) and p(14) and p(13) and p(12) after t_and4;

  gg(0) <= g(3) or (p(3) and g(2)) or (p(3) and p(2) and g(1)) or
           (p(3) and p(2) and p(1) and g(0)) after t_or4+t_and4;

  gg(1) <= g(7) or (p(7) and g(6)) or (p(7) and p(6) and g(5)) or
           (p(7) and p(6) and p(5) and g(4)) after t_or4+t_and4;

  gg(2) <= g(11) or (p(11) and g(10)) or (p(11) and p(10) and g(9)) or
           (p(11) and p(10) and p(9) and g(8)) after t_or4+t_and4;

  gg(3) <= g(15) or (p(15) and g(14)) or (p(15) and p(14) and g(13)) or
           (p(15) and p(14) and p(13) and g(12)) after t_or4+t_and4;

  cc(0) <= gg(0) or (pp(0) and vem) after t_or2+t_and2;
  cc(1) <= gg(1) or (pp(1) and gg(0)) or (pp(1) and pp(0) and vem)
           after t_or3+t_and3;
  cc(2) <= gg(2) or (pp(2) and gg(1)) or (pp(2) and pp(1) and gg(0)) or
           (pp(2) and pp(1) and pp(0) and vem) after t_or4+t_and4;
  cc(3) <= gg(3) or (pp(3) and gg(2)) or (pp(3) and pp(2) and gg(1)) or
           (pp(3) and pp(2) and pp(1) and gg(0)) or
           (pp(3) and pp(2) and pp(1) and pp(0) and vem)
           after t_or5+t_and5;

  c <= cc;

end adianta16;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador de 16 bits, com adiantamento de vai-um de 16 bits
--  P&H,2ndEd,sec4.5, RH sec1.6+8.3.2
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

-- entrada vem deve estar ligada em '0' para somar, em '1' para subtrair
entity adderAdianta16 is
  port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;             -- '0' soma, '1' subtrai    
       vai  : out bit
       );
end adderAdianta16;

architecture adderAdianta16 of adderAdianta16 is 
  component addBit port(bitA, bitB, vem : in bit;
                        soma, vai       : out bit);       
  end component addBit;

  component adianta4 port(a,b : in reg4;
                          vem : in bit;
                          c: out reg4);
  end component adianta4;
  
  component adianta16 port(a,b : in reg16;
                          vem : in bit;
                          c: out reg4);
  end component adianta16;
  
  signal v : reg16;                     -- cadeia de vai-um
  signal r : reg16;                     -- resultado parcial
  signal c : reg16;
  signal cc : reg4;                     -- cadeia de adiantamento de vai-um
begin

  U_a15_0:
    adianta16 port map (inpA,inpB,vem,cc); 
  
  U_a3_0: adianta4 port map
    (inpA(3 downto 0),inpB(3 downto 0),vem,c(3 downto 0)); 

  U_b0: addBit port map ( inpA(0),inpB(0),vem, r(0),open );
  U_b1: addBit port map ( inpA(1),inpB(1),c(0),r(1),open );
  U_b2: addBit port map ( inpA(2),inpB(2),c(1),r(2),open );
  U_b3: addBit port map ( inpA(3),inpB(3),c(2),r(3),open );

  U_a4_7: adianta4 port map
    (inpA(7 downto 4),inpB(7 downto 4),cc(0),c(7 downto 4));

  U_b4: addBit port map ( inpA(4),inpB(4),cc(0),r(4),open );
  U_b5: addBit port map ( inpA(5),inpB(5), c(4),r(5),open );
  U_b6: addBit port map ( inpA(6),inpB(6), c(5),r(6),open );
  U_b7: addBit port map ( inpA(7),inpB(7), c(6),r(7),open );

  U_a8_11: adianta4 port map
    (inpA(11 downto 8),inpB(11 downto 8),cc(1),c(11 downto 8)); 

  U_b8: addBit port map ( inpA(8), inpB(8), cc(0), r(8),open );
  U_b9: addBit port map ( inpA(9), inpB(9),  c(8), r(9),open );
  U_ba: addBit port map ( inpA(10),inpB(10), c(9),r(10),open );
  U_bb: addBit port map ( inpA(11),inpB(11),c(10),r(11),open );

  U_a12_15: adianta4 port map
    (inpA(15 downto 12),inpB(15 downto 12),cc(2),c(15 downto 12)); 

  U_bc: addBit port map ( inpA(12),inpB(12),cc(0),r(12),open );
  U_bd: addBit port map ( inpA(13),inpB(13),c(12),r(13),open );
  U_be: addBit port map ( inpA(14),inpB(14),c(13),r(14),open );
  U_bf: addBit port map ( inpA(15),inpB(15),c(14),r(15),open );
  
  vai <= cc(3);
  outC <= r;
  
end adderAdianta16;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador 32 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
architecture structural of adderCSA32 is

  component adderAdianta16 is port(inpA, inpB : in reg16;
                          outC : out reg16;
                          vem  : in bit;
                          vai  : out bit);
  end component adderAdianta16;

   signal x,y,z : bit;
   signal outE0, outE1: reg16;
   
begin

 Uadd16_1: adderAdianta16 port map(inpA(15 downto 0), 
 inpB(15 downto 0), outC(15 downto 0), vem, x);
 
 Uadd16_2: adderAdianta16 port map(inpA(31 downto 16), 
 inpB(31 downto 16), outE0, '0', y);
 
 Uadd16_3: adderAdianta16 port map(inpA(31 downto 16), 
 inpB(31 downto 16), outE1, '1', z);
 
---- MUX 2x32 ----



 outC(31 downto 16) <= outE0 when (x = '0') else outE1;
 vai <= y when (x = '0') else z;
 
end adderCSA32;

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- pid
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

