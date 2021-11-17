-- $Id: LAN91C96.vhd 1.4 2007/09/14 16:32:20Z Vtzann Exp $
-- Functional Description:
-- 		Connects the Ehternet IC on the test PCB to the OMAP.
--

library ieee;
use ieee.std_logic_1164.all;
-------------------------------------

entity LAN91C96 is
port(
		-- OMAP I/O signals defined first
		Sys_RST	: in std_logic;
		Sys_ADDR	: in std_logic_vector ( 2 downto 0 );
   	Sys_DATA	: inout std_logic_vector ( 15 downto 0 );
		Sys_CSn	: in std_logic;
		Sys_RDY	: inout std_logic;
   	Sys_nRD	: in std_logic;
   	Sys_nWE	: in std_logic;
  	 	Sys_INT 	: out std_logic;
		-- LAN CHIP I/O signals defined next
		LAN_ADDR	 : out std_logic_vector ( 2 downto 0 );
		LAN_DATA	 : inout std_logic_vector ( 15 downto 0 );
 	  	LAN_nIORD : out std_logic;
	   LAN_nIOWR : out std_logic;
		LAN_INTR0 : in std_logic;
		LAN_RST	 : out std_logic;
		LAN_nREG	 : out std_logic;	-- Chip Sel 0 for valid transaction
		LAN_IOCHRDY : in std_logic
);
end LAN91C96;
--

architecture structure of LAN91C96 is

	signal sWR_STB	: std_logic; 
	signal sRD_STB	: std_logic; 

begin 
	--Qualified Read & Write strobes (for bus direction control).	
 	sRD_STB <= '1' when ( Sys_RST = '0' and Sys_CSn = '0' and Sys_nWE ='1' and Sys_nRD = '0') else '0';
 	sWR_STB <= '1' when ( Sys_RST = '0' and Sys_CSn = '0' and Sys_nWE ='0' and Sys_nRD = '1') else '0';
	--

	LAN_nREG	   <= Sys_CSn;		-- CS to LAN91C96
	LAN_nIORD	<= Sys_nRD;		-- Read strobe to LAN91C96
	LAN_nIOWR	<= Sys_nWE;  	-- Write Strobe to LAN91C96
	LAN_ADDR	   <= Sys_ADDR;	-- Address line passthrough
	LAN_RST  	<= Sys_RST;		-- Reset line passthrough
	Sys_INT		<= LAN_INTR0;	-- Interrupt line from the LAN91C96 to the OMAP

	-- Bus control logic:
	Sys_RDY <= '0' when LAN_IOCHRDY='0' else 'Z'; -- effect open drain.
	Sys_DATA <= LAN_DATA when sRD_STB = '1' else (others => 'Z');
	LAN_DATA <= Sys_DATA when sWR_STB = '1' else (others => 'Z');

end structure;
