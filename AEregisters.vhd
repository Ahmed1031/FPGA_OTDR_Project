-- $Id: AEregisters.vhd 1.15 2008/01/15 21:53:01Z Aghouri Exp $
-- Van Tzannidakis .
-- AEregisters : Acess registers Entity for Aquisition Engine to be implemented in Cyclone-II FPGA .
-- Dated : 2006.01.17

--
-- Functional Description:
-- See 535XT Acquisition Engine Interface Control.doc
--
-- These registers are asynchronously interfaced to the outside world. They can be read and/or written to
-- as per specification.
-- We'll synchronize them to the clock... somehow
-- The registers are written to when the AE is not running; so, they can probably be just latched.

--Rev:1.01
--(535XT Acquisition Engine Interface Control.doc REV. -- 1.01)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
-------------------------------------


entity AEregisters is
port(
	clk : in std_logic;	-- 100MHz System Clock

-- System Interface signals and buses -- tot signals : 22 

	rst			: in std_logic;
	ADDR		: in std_logic_vector ( 3 downto 0 );
   DATA		: inout std_logic_vector ( 15 downto 0 );
--    INT			: out std_logic;
--	RWn			: in std_logic;
   Rn			: in std_logic;
   Wn			: in std_logic;
	CEn		: in std_logic;

--NOTE: more signals may be needed to support DR
    Drr 	: out std_logic; 
	Trrd	: out std_logic; 
		
	DR		: in std_logic_vector( 15 downto 0);		--	2.1	R0 - Data Read Register (DR)
	ER		: out std_logic_vector( 15 downto 0);		--	2.2	R1 - End Register (ER)
														--	2.3	R2 - Resolution Register (RR) translates to this:
	ILVS	: out std_logic_vector( 4 downto 0);		--		this is always 1-16
	HALFSPD	: out std_logic;							--		this is 1 when we go at half speed (ILVS= 1)
	DLY0	: out std_logic_vector( 4 downto 0);		--		delay for interleave 0
	DLYSTEP	: out std_logic_vector( 4 downto 0); 		--		delay step (decerement by)
	PR		: out std_logic_vector( 9 downto 0);		--	2.4	R3 -  Pulsewidth Register (PR)
															--	2.5	R4 -  Averages Register (AR) translates to this:
	AVTRACES: out std_logic_vector( 20 downto 0);		-- 		=2^AR up to 1048576
	TLR		: in  std_logic_vector ( 15 downto 0 );		--	2.6	R5 -  Traces Accumulated Register - low word (TLR)
	THR		: in  std_logic_vector ( 15 downto 0 );		--	2.6	R5 -  Traces Accumulated Register - low word (TLR)
	CR		: inout  std_logic_vector ( 15 downto 0 )		--	2.8	R7 -  Control/Status Register (CR)

);
end AEregisters;


architecture structure of AEregisters is
attribute keep : boolean;
    
component AExlatRR
port(
	RR			:  in std_logic_vector( 2 downto 0);
--	2.3	R2 - Resolution Register (RR) translates to this:
	ILVS		: out std_logic_vector( 4 downto 0);--this is alway 1-16
	HALFSPD		: out std_logic;					--this is 1 when we go at half speed (ILVS= 1)
	DLY0		: out std_logic_vector( 4 downto 0);--delay for interleave 0
	DLYSTEP		: out std_logic_vector( 4 downto 0) --delay step (decerement by)
	);
end component;
--
component AExlatAR
port(
	AR			:  in std_logic_vector( 4 downto 0);
													--	2.3	R2 - Resoltion Register (RR) translates to this:
	AVTRACES	: out std_logic_vector( 20 downto 0)-- 		=2^AR up to 1048576
	);
end component;

	--signal sig_DR	: std_logic_vector( 15 downto 0);		--	2.1	R0 - Data Read Register (DR)
	signal sig_ER	: std_logic_vector( 15 downto 0);		--	2.2	R1 - End Register (ER)
	signal sig_RR	: std_logic_vector( 2 downto 0);		--	2.3	R2 - Resolution Register (RR)
	signal sig_PR	: std_logic_vector( 9 downto 0);		--	2.4	R3 -  Pulsewidth Register (PR)
	signal sig_AR	: std_logic_vector( 4 downto 0);		--	2.5	R4 -  Averages Register (AR
	--signal sig_TLR	: std_logic_vector ( 15 downto 0 );		--	2.6	R5 -  Traces Accumulated Register - low word (TLR)
	--signal sig_THR	: std_logic_vector ( 15 downto 0 );		--	2.6	R6 -  Traces Accumulated Register - low word (TLR)
	signal sig_CR	: std_logic_vector ( 15 downto 0 );		--	2.8	R7 -  Control/Status Register (CR)

	-- Resolution Register (RR) translates to these:
	--signal sig_ILVS		: std_logic_vector( 4 downto 0);	--this is alway 1-16
	--signal sig_HALFSPD	: std_logic;						--this is 1 when we go at half speed (ILVS= 1)
	--signal sig_DLY0		: std_logic_vector( 4 downto 0);	--delay for interleave 0 + delay step,
	--signal sig_DLYSTEP	: std_logic_vector( 4 downto 0) 	--delay step (decerement by)
	--
	signal sig_AVTRACES	: std_logic_vector( 20 downto 0);	-- =2^AR up to 1048576
	signal sig_CR0 : std_logic;
			
	
begin
    
           
ER	<= sig_ER;    
PR	<= sig_PR;        
CR(12 downto 0) <= sig_CR(12 downto 0); -- bits 13, 14 & 15 of CR come straing form the CR pins      

TRanslateRR: AExlatRR
	port map(
		RR		=> sig_RR,
		ILVS	=> ILVS,
		HALFSPD	=> HALFSPD,
		DLY0	=> DLY0,
		DLYSTEP	=> DLYSTEP

		);
--
TRanslateAR: AExlatAR
	port map(
		AR			=> sig_AR,
		AVTRACES	=> sig_AVTRACES
		);
AVTRACES <= sig_AVTRACES;
-- Write to Registers
wr_ops: process ( rst, ADDR, DATA, Wn, Rn, CEn )
begin
	if (rst = '1') then
		--sig_DR	<= (others => '0'); -- this is read only register.
		sig_ER	<= (others => '0');
		sig_RR	<= (others => '0');
		sig_PR	<= (others => '0');
		sig_AR	<= (others => '0');
		sig_CR	<= (others => '0');-- this may have to be initialized differently
		-- sig_CR		<= (others => '1');-- just for testing
	elsif (CEn ='0' and Wn ='0' and Rn = '1') then -- WRITE registers
		case ADDR is
			--when "0000" => sig_DR <= DATA(sig_DR'high downto 0);		--	2.1	R0 - Data Read Register (DR)
			when "0001" => sig_ER <= DATA(sig_ER'high downto 0);		--	2.2	R1 - End Register (ER)
			when "0010" => sig_RR <= DATA(sig_RR'high downto 0);		--	2.3	R2 - Resolution Register (RR)
			when "0011" => sig_PR <= DATA(sig_PR'high downto 0);		--	2.4	R3 -  Pulsewidth Register (PR)
			when "0100" => sig_AR <= DATA(sig_AR'high downto 0);		--	2.5	R4 -  Averages Register (AR)
			--when "0101" sig_TLR <= DATA(sig_TLR'high downto 0);	--	2.6	R5 -  Traces Accumulated Register - low word (TLR)
			--when "0110" sig_THR <= DATA(sig_THR'high downto 0);	--	2.6	R6 -  Traces Accumulated Register - low word (TLR)
			when "0111" => sig_CR <= DATA(sig_CR'high downto 0);		--	2.8	R7 -  Control/Status Register (CR)
			                             
			when others =>
				-- Registers 8-15 are not used --
		end case;		
	end if;
end process;


rd_ops: process ( rst, ADDR, Wn, Rn, CEn, sig_ER, sig_RR, sig_PR, sig_AR, TLR, THR, sig_CR, DR, CR)
begin
	
	DATA 	<= (others => 'Z');
	Drr <= '0';
	Trrd <= '0'; 
	if (rst = '0' and CEn ='0' and Rn='0' and Wn ='1' ) then -- READ registers
		DATA <= (others => '0'); -- so that the upper bits of short registers read '0'
		case ADDR is
			when "0000" => DATA(DR'high downto 0) <= DR ;			--	2.1	R0 - Data Read Register (DR)
						   				      Drr <= '1'; 
							
			when "0001" => DATA(sig_ER'high downto 0)	<= sig_ER;		--	2.2	R1 - End Register (ER)
			when "0010" => DATA(sig_RR'high downto 0)	<= sig_RR;		--	2.3	R2 - Resolution Register (RR)
			when "0011" => DATA(sig_PR'high downto 0)	<= sig_PR;		--	2.4	R3 -  Pulsewidth Register (PR)
			when "0100" => DATA(sig_AR'high downto 0)	<= sig_AR;		--	2.5	R4 -  Averages Register (AR)
			---------------------------------------------------------------------------------------------------------    
			when "0101" => DATA(TLR'high downto 0)	<= TLR;			--	2.6	R5 -  Traces Accumulated Register - low word (TLR)
			when "0110" => DATA(THR'high downto 0)	<= THR;			--	2.6	R6 -  Traces Accumulated Register - High word (THR)
											          Trrd <= '1';  --<< This is done intentionally for the System to Read 
											                        -- the TLR fist and then THR .     
			----------------------------------------------------------------------------------------------------------								   
			when "0111" => DATA(sig_CR'high downto 0) 	<= sig_CR;			--	2.8	R7 -  Control/Status Register (CR)
						   DATA(15 downto 13) <= CR(15 downto 13);
			when others =>   
		end case;
		
	end if;
end process;





end structure;
