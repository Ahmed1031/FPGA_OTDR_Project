-- Van Tzannidakis .
-- AExlatAR : Translate AR (averages register)
-- Dated : 2006.01.17

--
-- Functional Description:
-- See 535XT Acquisition Engine Interface Control.doc
--

--Rev:1.01
--(535XT Acquisition Engine Interface Control.doc REV. -- 1.01)
--
--	2.5	R4 -  Averages Register (AR) translates to this:
-- AVTRACES = 2^AR		for AR = 0- 20
-- AVTRACES = 2^20		for AR = 21-31

library ieee;
use ieee.std_logic_1164.all;
--use ieee.std_logic_arith.all;
--use ieee.std_logic_unsigned.all;
--use ieee.NUMERIC_STD.all;
-------------------------------------


entity AExlatAR is
port(
	AR			:  in std_logic_vector( 4 downto 0);
	--	2.3	R2 - Resolution Register (RR) translates to this:
	AVTRACES	: out std_logic_vector( 20 downto 0)-- =2^AR up to 1048576
	);
end AExlatAR;
--
--
architecture behaviour of AExlatAR is
-------------------------------------------------------------------
-- convert AR to integer and use as index to lookup table
-------------------------------------------------------------------
begin

process (AR)
begin
	case AR is
	
		when "00000" => AVTRACES <= "000000000000000000001";-- 0 --> 1
		when "00001" => AVTRACES <= "000000000000000000010";-- 1 -->       2
		when "00010" => AVTRACES <= "000000000000000000100";-- 2 -->       4
		when "00011" => AVTRACES <= "000000000000000001000";-- 3 -->       8
		when "00100" => AVTRACES <= "000000000000000010000";-- 4 -->      16
		when "00101" => AVTRACES <= "000000000000000100000";-- 5 -->      32
		when "00110" => AVTRACES <= "000000000000001000000";-- 6 -->      64
		when "00111" => AVTRACES <= "000000000000010000000";-- 7 -->     128
		when "01000" => AVTRACES <= "000000000000100000000";-- 8 -->     256
		when "01001" => AVTRACES <= "000000000001000000000";-- 9 -->     512
		when "01010" => AVTRACES <= "000000000010000000000";--10 -->    1024
		when "01011" => AVTRACES <= "000000000100000000000";--11 -->    2048
		when "01100" => AVTRACES <= "000000001000000000000";--12 -->    4096
		when "01101" => AVTRACES <= "000000010000000000000";--13 -->    8192
		when "01110" => AVTRACES <= "000000100000000000000";--14 -->   16384
		when "01111" => AVTRACES <= "000001000000000000000";--15 -->   32768
		when "10000" => AVTRACES <= "000010000000000000000";--16 -->   65536
		when "10001" => AVTRACES <= "000100000000000000000";--17 -->  131072
		when "10010" => AVTRACES <= "001000000000000000000";--18 -->  262144
		when "10011" => AVTRACES <= "010000000000000000000";--19 -->  524188
		when others  => AVTRACES <= "100000000000000000000";--31 --> 1048176
		end case;		
	

end process;

end behaviour;