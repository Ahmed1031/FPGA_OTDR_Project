--$Id: pulse_gen.vhd 1.4 2007/05/12 02:07:27Z vtzann Exp $

-- Description:
--==============
-- Upon "trig" going high a pulse of a given width (in clock cycles) is generated at pulseout.
-- the width of the pulse is determined by "pwidth"+1.
-- The circuit is regtriggerable, and it will skip incoming triggers during the pulse.
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity pulse_gen is
	generic ( pw_bits : natural:=10);
	port (
		areset	: in std_logic;
		clk		: in std_logic;				   -- 100MHz -- this drives the PLL as well
		pwidth	: in std_logic_vector(pw_bits-1 downto 0);					-- 10 bits -- 10 ns to 10us wide
		trig	   : in std_logic;					-- trigger, already synchronous to the clock (clk)
		pulseout : out std_logic					-- delayed varible witdh pulse
	);
end pulse_gen;

--

architecture behavior of pulse_gen is

	signal s_trig_del		: std_logic;
	signal s_start			: std_logic;
	signal s_pulseout		: std_logic;
	signal s_pw			   : std_logic_vector(pw_bits-1 downto 0);
	constant c_zeropw		: std_logic_vector(pw_bits-1 downto 0):=(others =>'0');
	
begin

pulseout <= s_pulseout;

pulse: process (areset, clk, s_pulseout, s_trig_del, trig, s_pw, pwidth)
	begin
	if areset = '1' then
		s_pulseout <= '0';
		s_trig_del <= '0';
		s_pw <= pwidth;
	elsif clk'event and clk = '1' then
		s_trig_del <= trig; 								-- create delayed trigger pulse
		if s_pulseout = '0' and s_trig_del = '0' and trig = '1' then			-- start condition
			s_pw <= pwidth;
		 elsif s_pw = c_zeropw then
		 	s_pulseout <= '0';
		 else
		 	s_pulseout <= '1';
			s_pw <= s_pw - '1';
		end if;
	end if;
end process;




end behavior;
