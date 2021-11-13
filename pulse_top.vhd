-- $Id: pulse_top.vhd 1.2 2007/04/09 01:48:10Z Vtzann Exp $

-- This is what the AE top level will do: combine the PLL, pulse_gen and pulse delay
--


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity pulse_top is
	generic (pulse_width_bits : natural:= 10);
	port (
		areset	: in std_logic;
		clk		: in std_logic;				-- 100MHz -- this drives the PLL as well
		pwidth	: in std_logic_vector(pulse_width_bits-1 downto 0);					-- 10 bits -- 10 ns to 10us wide
		pdelay	: in std_logic_vector(3 downto 0);					-- 16 steps
		trig	: in std_logic;					-- trigger, already synchronous to the clock (clk)
		pulseout: out std_logic						-- delayed varible witdh pulse
	);
end pulse_top;
--

architecture behaviour of pulse_top is
-------------------------------
component pulse_gen is
	generic ( pw_bits : natural);
	port (
		areset	: in std_logic;
		clk		: in std_logic;				-- 100MHz -- this drives the PLL as well
		pwidth	: in std_logic_vector(pw_bits-1 downto 0);					-- 10 bits -- 10 ns to 10us wide
		trig	: in std_logic;					-- trigger, already synchronous to the clock (clk)
		pulseout: out std_logic						-- delayed varible witdh pulse
	);
end component;


-------------------------------

component pulse_delay is
	port (
		areset	: in std_logic;
		clk_400_000		: in std_logic;				-- 400MHz phase 0
		clk_400_180		: in std_logic;				-- 400MHz phase 180
		pdelay	: in std_logic_vector(3 downto 0);					-- 16 steps
		pulsein	: in std_logic;					-- trigger, already synchronous to the clock (clk)
		pulseout: out std_logic						-- delayed varible witdh pulse
	);
end component;


---- // PLL //------------------------------
component PLL
	PORT
	(
		areset	: IN STD_LOGIC;
		inclk0	: IN STD_LOGIC;
		c0		: OUT STD_LOGIC ;
		c1		: OUT STD_LOGIC ;
		c2		: OUT STD_LOGIC ;
		locked	: OUT STD_LOGIC 
	);
end component;

--
signal s_areset			: std_logic;
signal s_clk			: std_logic;
signal s_pwidth			: std_logic_vector(pulse_width_bits-1 downto 0);	
signal s_pdelay			: std_logic_vector(4-1 downto 0);	-- 16 steps

signal s_trig			: std_logic;					-- delayed varible witdh pulse
signal s_pulse			: std_logic;					-- delayed varible witdh pulse
signal s_pulseout			: std_logic;					-- delayed varible witdh pulse

signal c0			: std_logic; -- not used in this design bu it will be used in AE_top
signal c1			: std_logic;
signal c2			: std_logic;
signal locked			: std_logic;

begin -------------------------------------------------------------------------------------

s_clk <= clk;
s_areset <= areset;
s_pwidth <= pwidth;
s_pdelay <= pdelay;
s_trig <= trig;
pulseout <= s_pulseout;

Pulser:pulse_gen
	generic map(pw_bits => pulse_width_bits)
	port map(
		areset	=> s_areset,
		clk	=> s_clk,
		pwidth	=> s_pwidth,
		trig	=> s_trig,
		pulseout	=> s_pulse
	);

Delay: pulse_delay
	port map(
		areset	=> s_areset,
		clk_400_000 => c1,
		clk_400_180 => c2,
		pulsein	=> s_pulse,
		pdelay	=> s_pdelay,
		pulseout	=> s_pulseout
	);

--Instantiate a PLL
PLL_inst : PLL
	PORT MAP (
		areset	=> s_areset,
		inclk0	=> s_clk,
		c0	 => c0,
		c1	=> c1,
		c2	=> c2,
		locked	=> locked
)	;

end behaviour;

--use work.all;
---- Select on the different impementation of "pulse_delay"
--configuration select_stage_CFG of pulse_top is
--	for behaviour
--		for all: pulse_delay
--		-----------------------------------------------
--	    use entity work.pulse_delay(rtl_selsr);
--		-----------------------------------------------
--		end for;
--	end for;
--	
--end select_stage_CFG;
--
--configuration varible_sr_CFG of pulse_top is
--	for behaviour
--		for all: pulse_delay
--		-----------------------------------------------
--	    use entity work.pulse_delay(rtl_varsr);--<<==
--		-----------------------------------------------
--		end for;
--	end for;
--end varible_sr_CFG;
--