-- $Id: pulse_delay.vhd 1.5 2007/04/17 15:42:32Z Vtzann Exp $

-- Description:
--==============
-- This entity delays an incoming pulse by up to 7.5 cycles of the 400MHz clock
-- or,  over 0 - 18.75ns in 1.25ns increments.

-- pdelay[3:0] :
--	Determines delay. The upper 3 bits delay the pulse by up to 7 cycles, while the LSB
-- 	adds 0.5 cycles to the chosen delay, increasing the stepping resolution to 0.5 cycles (1.25ns).
--
-- There are two implementations, reflected by the two architecures. Either one can be selected via a
-- configuration statement in the instatiating file.
-------------------------------------------------------------------------
--NOTE: At the moment I have not figured out how to use configurations.
--		Therefore the last configuration (rtl_varsr) is the default one.
--		(Actually I think that it is a more robust one, anyway).
-------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;

entity pulse_delay is
	port (
		areset	: in std_logic;
		clk_400_000		: in std_logic;				-- 400MHz phase 0
		clk_400_180		: in std_logic;				-- 400MHz phase 180
		pdelay	: in std_logic_vector(3 downto 0);	-- 16 steps
		outsel	: in std_logic_vector(2 downto 0);	-- select output
		pulsein	: in std_logic;						-- trigger, already synchronous to the clock (clk)
		pulseout: out std_logic_vector(2 downto 0) 	-- delayed varible witdh pulse
	);end pulse_delay;
--
-- ARCHITECTURE: rtl_selsr
-- ========================
-- In this architecture the two 400MHz, 180 degrees out of phase, clocks are multiplexed 
-- to produce clk_400 of selectable phase.
--
-- The incoming pulse is first registered using clk_400_000 and the fed into a
-- shift register, clocked by clk_400.
-- A multiplexes selects one of the stages as the output, also registered using clk_400.

-- Selecting SR stage creates a delay in steps of 2.5ns, while inverting the phase of clk_400
-- splits these into 1.25ns increments.
--

architecture rtl_selsr of pulse_delay is

-- // CLK MUX //------------------------------
component clkctrl
	PORT
	(
		clkselect	: IN STD_LOGIC;
		inclk0x		: IN STD_LOGIC ;
		inclk1x		: IN STD_LOGIC ;
		outclk		: OUT STD_LOGIC 
	);
end component;

--CLOCK SELECTOR SIGNALS-------------------
signal clkselect: std_logic;
signal inclk0x	: std_logic;
signal inclk1x	: std_logic;
signal outclk	: std_logic;

--======================================================================================================


--DELAY PIPLINE SELECTOR SIGNALS-----------
signal dly_p	: std_logic_vector(7 downto 0);
signal dly_p_sel: std_logic;

-----------------------------------------------------------------------
-- other design signals:
signal pulsein_reg	: std_logic;	-- registered input pulse
signal clk_400		   : std_logic;	-- registered input pulse
signal pulseout_drv	: std_logic;	-- registered input pulse

-----------------------------------------------------------------------
begin

--
--Instantiate a clock mux
clkctrl_inst : clkctrl PORT MAP (
		clkselect	 => clkselect,
		inclk0x	 => inclk0x,
		inclk1x	 => inclk1x,
		outclk	 => outclk
	);

--MUX phase selector (400MHz)
clkselect	<= not pdelay(0);--400MHz phase selector is the LSB of delay parameter

-- clocks used in the design:
inclk0x <= clk_400_000;
inclk1x <= clk_400_180;

clk_400 <= outclk;	-- output of MUX is clk_400, phase selectable clock

-- REGISTER pulsein using the 400MHz clock -------------------------------
reg_pulsein: process(clk_400_000)
	begin
	if clk_400_000'event and clk_400_000='1' then
		pulsein_reg <=  pulsein;
	end if;
end process;

--CREATE DELAY PIPLINE----------------------------
delay4: process(clk_400, pulsein_reg, dly_p)
begin
	if clk_400'event and clk_400='1' then
		dly_p(dly_p'left downto 1) <= dly_p(dly_p'left-1 downto 0);
		dly_p(0)	<= pulsein_reg;
	end if;
end process;
----SELECT & REGISTER PIPELINE STAGE TO OUPTUT-----------------
	dly_p_sel <=	
		dly_p(0) when pdelay(3 downto 1)="000" else
		dly_p(1) when pdelay(3 downto 1)="001" else
		dly_p(2) when pdelay(3 downto 1)="010" else
		dly_p(3) when pdelay(3 downto 1)="011" else
		dly_p(4) when pdelay(3 downto 1)="100" else
		dly_p(5) when pdelay(3 downto 1)="101" else
		dly_p(6) when pdelay(3 downto 1)="110" else
		dly_p(7);
-- RE-TIME the selected output to the 400MHz clock and put it out to the pin
sel_n_reg_out: process(clk_400, pdelay, dly_p)
begin
	if clk_400'event and clk_400='1' then
		pulseout_drv <= dly_p_sel;
	end if;
end process;

-- now select output to route pulse to
sel_out: process(clk_400, pulseout_drv, outsel)
begin
	if clk_400'event and clk_400='1' then
		for i in pulseout'range loop
			pulseout(i) <= pulseout_drv and outsel(i);
		end loop;
	end if;
end process;

end rtl_selsr;

-- ===========================================================================================
-- ===========================================================================================

--
-- ARCHITECTURE: rtl_varsr
-- ========================

-- In this architecture the two 400MHz, 180 degrees out of phase, clocks are multiplexed 
--
-- The incoming pulse is first registered using clk_400_000 and the fed into a
-- shift register, clocked by clk_400.
-- The pulse to be delayed is injected into a selected stage of a shift register, creating
-- in essence a variable length shift register.
-- The shift register is clocked by the 0 phase 400MHz clock (clk_400_000).
-- Finally, the output of the shift register is further registered using the multiplexed cloc
-- (clk_400). By changing the phse of the clk_400, the output delay changes by 1.25ns.
--
architecture rtl_varsr of pulse_delay is

-- // CLK MUX //------------------------------
component clkctrl
	PORT
	(
		clkselect	: IN STD_LOGIC;
		inclk0x		: IN STD_LOGIC ;
		inclk1x		: IN STD_LOGIC ;
		outclk		: OUT STD_LOGIC 
	);
end component;

--CLOCK SELECTOR SIGNALS-------------------
signal clkselect: std_logic;
signal inclk0x	: std_logic;
signal inclk1x	: std_logic;
signal outclk	: std_logic;

--======================================================================================================


--DELAY PIPLINE SELECTOR SIGNALS-----------
signal dly_p	: std_logic_vector(7 downto 0);
signal dly_p_sel: std_logic;

-----------------------------------------------------------------------
-- other design signals:
signal pulsein_reg	: std_logic;	-- registered input pulse
signal clk_400		   : std_logic;	-- registered input pulse
signal pulseout_drv	: std_logic;	-- registered input pulse

-----------------------------------------------------------------------
begin

--
--Instantiate a clock mux
clkctrl_inst : clkctrl PORT MAP (
		clkselect	 => clkselect,
		inclk0x	 => inclk0x,
		inclk1x	 => inclk1x,
		outclk	 => outclk
	);

--MUX phase selector (400MHz)
clkselect	<= not pdelay(0);--400MHz phase selector is the LSB of delay parameter

-- clocks used in the design:
inclk0x <= clk_400_000;
inclk1x <= clk_400_180;

clk_400 <= outclk;	-- output of MUX is clk_400, phase selectable clock

-- REGISTER pulsein using the 400MHz clock -------------------------------
reg_pulsein: process(clk_400_000)
	begin
	if clk_400_000'event and clk_400_000='1' then
		pulsein_reg <=  pulsein;
	end if;
end process;


-- This is a shift register, where the input of each stage is either the ouput of the previous stage
-- or the input signal (pulse to be delayed). Essentially, this creates a variable length Shift register
-- Selection is accomplished by the upper 3 bits of the selecting signal (pdelay).
-- It is clocked by phase 0 of the 400MHz clock

varlen_sr: process(clk_400_000, pulsein_reg, dly_p)
	begin
	if clk_400_000'event and clk_400_000 = '1' then
		if pdelay(3 downto 1)="111" then dly_p(0) <= pulsein_reg; else dly_p(0) <= '0'; end if;
		if pdelay(3 downto 1)="110" then dly_p(1) <= pulsein_reg; else dly_p(1) <= dly_p(0); end if;
		if pdelay(3 downto 1)="101" then dly_p(2) <= pulsein_reg; else dly_p(2) <= dly_p(1); end if;
		if pdelay(3 downto 1)="100" then dly_p(3) <= pulsein_reg; else dly_p(3) <= dly_p(2); end if;
		if pdelay(3 downto 1)="011" then dly_p(4) <= pulsein_reg; else dly_p(4) <= dly_p(3); end if;
		if pdelay(3 downto 1)="010" then dly_p(5) <= pulsein_reg; else dly_p(5) <= dly_p(4); end if;
		if pdelay(3 downto 1)="001" then dly_p(6) <= pulsein_reg; else dly_p(6) <= dly_p(5); end if;
		if pdelay(3 downto 1)="000" then dly_p(7) <= pulsein_reg; else dly_p(7) <= dly_p(6); end if;
	end if;
	end process;


-- Clock the final stage output by the inverted clock to create output dly_p_180 further delayed by T/2=1.25ns.
process(clk_400, dly_p(7))
begin
	if clk_400'event and clk_400 ='1' then
		pulseout_drv <= dly_p(7);
	end if;
end process;

-- now select output to route pulse to
sel_out: process(clk_400, pulseout_drv, outsel)
begin
	if clk_400'event and clk_400='1' then
		for i in pulseout'range loop
			pulseout(i) <= pulseout_drv and outsel(i);
		end loop;
	end if;
end process;

end rtl_varsr;
