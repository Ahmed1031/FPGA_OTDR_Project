-- Van Tzannidakis .
-- AExlatRR : Translate RR (resolution register)
-- Dated : 2006.01.17

--
-- Functional Description:
-- See 535XT Acquisition Engine Interface Control.doc
--

--Rev:1.01
--(535XT Acquisition Engine Interface Control.doc REV. -- 1.01)
--	2.3	R2 - Resolution Register (RR) translates to this:
-- ILVS		HALFSPD
-- 1-16		0		for RR = 0- 4
-- 1		1		for RR = 5- 7

-- The interleaves also determine the initial delay and delay step:
-- ILVS		DLY0	DLYSTEP
--	 1		0		0
--	 2		16		8
--	 4		16		4
--	 8		16		2
--	16		16		1
-------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
-------------------------------------


entity AExlatRR is
port(
	RR			:  in std_logic_vector( 2 downto 0);
--	2.3	R2 - Resolution Register (RR) translates to this:
	ILVS		: out std_logic_vector( 4 downto 0);--this is alway 1-16
	HALFSPD		: out std_logic;					--this is 1 when we go at half speed (ILVS= 1)
	DLY0		: out std_logic_vector( 4 downto 0);--delay for interleave 0
	DLYSTEP		: out std_logic_vector( 4 downto 0) --delay step (decerement by)
	);
end AExlatRR;

architecture behaviour of AExlatRR is
begin
process (RR)
begin
	case RR is
		when "000" => ILVS <= "00001"; HALFSPD <= '0'; DLY0 <= "00000"; DLYSTEP <= "00000";
		when "001" => ILVS <= "00010"; HALFSPD <= '0'; DLY0 <= "00000"; DLYSTEP <= "01000";
		when "010" => ILVS <= "00100"; HALFSPD <= '0'; DLY0 <= "00000"; DLYSTEP <= "00100";
		when "011" => ILVS <= "01000"; HALFSPD <= '0'; DLY0 <= "00000"; DLYSTEP <= "00010";
		when "100" => ILVS <= "10000"; HALFSPD <= '0'; DLY0 <= "00000"; DLYSTEP <= "00001";
		when others=> ILVS <= "00001"; HALFSPD <= '1'; DLY0 <= "00000"; DLYSTEP <= "00000";
	end case;		
end process;
----
end behaviour;