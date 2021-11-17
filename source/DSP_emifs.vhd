-- $Id: DSP_emifs.vhd 1.2 2007/09/14 13:47:38Z Vtzann Exp Aghouri $
-- Module that interfaces the FPGA to the DSP IFFS
--
library ieee;
use ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;

--NOTE: all CS connected to the FPGA (FLASH_CS1L,FLASH_CS1U & FLASH_CS2L) address 32MB of memory
entity CS_gen is
	generic (
	-- Constant parameters
		base		: std_logic_vector( 25 downto 1);	-- base address of AE
		addrw	: natural								-- how many address lines go straight to the AE
														-- NOTE: the lower "addrw" of "base" are ignored during decoding
	);
	port (
	--DSP side ports
		emifs_addr	:    in std_logic_vector( 25 downto 1);
		emifs_CSn	:    in std_logic;
	--FPGA (inside) Chip Selects
		CSn			:   out std_logic
	);
end CS_gen;

		
architecture rtl of CS_gen is
begin
	CSn	<= '0' when emifs_CSn='0' and emifs_addr(25 downto addrw+1) = base(25 downto addrw+1) else '1';
	-- when creating CSn for other entities inside the FPGA, make sure that the address spaces do not overlap.
end rtl;
