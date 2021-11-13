-- File name : AEtop.vhd 
-- version : 1.54 
-- date created : 2007/10/26 
-- Ahmed Asim Ghouri
-- AETop : Top-level Entity for Aquisition Engine to be implemented in Cyclone-II FPGA .
-- 
-- Description : FPGA is externally connected to System Bus , System I/O's , ZBT Sram Bus and its io's 
-- and to ADC data bus and its io's . The AE will read , translate and latch values written by System into Registers. 
--
-- Functional Description:
-- See 535XT Acquisition Engine Interface Control.doc
--
--
-------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-------------------------------------
-- 
-- Most of the ports will be brought out to pins, with the exception of the
-- system interface signals (sys_xxxx). These are internal and connect to the
-- FPGA implementation of the interface to the DSP. 

-- Generic "revision" defines the current rev number of the design.
-- The revision number of the AE itself is defined in "AEtop" (AErev)
-- It can be read from register DR when AE in Idle state.
-- High Word when LH=0, Low Word when LH=1

entity AEtop is 
generic (
	revision : std_logic_vector(15 downto 0):= x"CCCC"
);
port (
	clk	: in std_logic;	-- 100MHz System Clock
	clk2	: in std_logic;	-- 100MHz System Clock

-- System Interface signals and buses -- tot signals : 22 
	sys_rst		: in std_logic;
	sys_ADDR	   : in std_logic_vector ( 3 downto 0 );
	sys_DATA	   : inout std_logic_vector ( 15 downto 0 );
 	sys_INT		: out std_logic;
	sys_Rn		:  in std_logic;
 	sys_Wn		:  in std_logic;
	sys_CEn		:  in std_logic;
-----------------------------------------------------   

-- From Dual-Ended Distance Receiver 	
	 DDin : in std_logic; -- tot no : 1 signals 
-----------------------------------------------------   

-- ADC signals and buses -- tot no : 18 signals 
	adc_data	   :  in std_logic_vector( 13 downto 0 ); 
	adc_clk		: out std_logic;
	adc_shdn	   : out std_logic;
	adc_OEn		: out std_logic;
	adc_OF		:  in std_logic;
-----------------------------------------------------

-- Programmable Delay Signals -- tot no : 11 signals
	dly_out		: out std_logic_vector ( 7 downto 0 );
	dly_trg		: out std_logic;
	dly_ae		: out std_logic;
-----------------------------------------------------

-- Laser Drive Signals -- tot no : 11 signals
	lsr_delayed		:  in std_logic;					-- Output from the prog. delay line
	lsr_drive		: out std_logic_vector( 2 downto 0 );			-- Trigger outputs for up to 3 lasers
-----------------------------------------------------

-- ZBT SRAM signals -- tot signals : 70 signals 
	ZBT_A		   : out std_logic_vector( 19 downto 0 ); 
	ZBT_ADV		: out std_logic;
	ZBT_WEn		: out std_logic; 
	ZBT_CENn	   : out std_logic ;
  	ZBT_BW4n	   : out std_logic ;
  	ZBT_BW3n	   : out std_logic ;
  	ZBT_BW2n	   : out std_logic ;
  	ZBT_BW1n	   : out std_logic ;
	ZBT_CE1n	   : out std_logic ;
  	ZBT_CE2		: out std_logic ;
  	ZBT_CE2n	   : out std_logic ; 
	ZBT_CLK 	   : out std_logic ; -- pin_C1
  	ZBT_IO		: inout std_logic_vector ( 31 downto 0 );
	ZBT_IOP		: inout std_logic_vector (  4 downto 1 );
	ZBT_MODE	   : out std_logic ;
	ZBT_OEn		: out std_logic ;
  	ZBT_ZZ		: out std_logic 
	---------------------------------------------------------
);
end AEtop ;

--

architecture acquisition of AEtop is
-----------
constant maj_rev	: std_logic_vector(3 downto 0):= x"1"; -- major operational changes.
constant min_rev	: std_logic_vector(3 downto 0):= x"1"; -- may affect external operation.
constant patch		: std_logic_vector(3 downto 0):= x"0"; -- affects internal operation only.
constant ae_type	: std_logic_vector(2 downto 0):= "000"; -- 0--OTDR, 1--TDR
constant xp			: std_logic_vector(0 downto 0):= "1"; -- 1- experimental, 0--RELEASED
--combine above into a 16 bit vector
constant AErev		: std_logic_vector(15 downto 0):= maj_rev & min_rev & patch & ae_type & xp;
-----------
attribute chip_pin : string; -- Example: attribute chip_pin of adc_clk : signal is "K22";
attribute keep : boolean;
--------------------------------------------------------

component pulse_gen is
	generic ( pw_bits : natural);
	port (
		areset	: in std_logic;
		clk		: in std_logic;				-- 100MHz -- this drives the PLL as well
		pwidth	: in std_logic_vector(pw_bits-1 downto 0);-- 10 bits -- 0 ns to 10.23us wide
		trig	: in std_logic;				-- trigger, already synchronous to the clock (clk)
		pulseout: out std_logic				-- delayed varible witdh pulse
	);
end component;
--------------------------------------------------------

component pulse_delay is
port (
	areset	: in std_logic;
	clk_400_000	:in std_logic;						-- 400MHz phase 0
	clk_400_180	:in std_logic;						-- 400MHz phase 180
	pdelay	:    in std_logic_vector(3 downto 0);	-- 16 steps
	outsel	:    in std_logic_vector(2 downto 0);	-- select laser to drive
	pulsein	:    in std_logic;						-- trigger, already synchronous to the clock (clk)
	pulseout:   out std_logic_vector(2 downto 0) 	-- delayed varible witdh pulse
	);
end component;
--------------------------------------------------------

component PLL is
port (
	areset	:    in STD_LOGIC := '0';
	inclk0	:    in STD_LOGIC := '0';
	c0		:   out STD_LOGIC ;
	c1		:   out STD_LOGIC ;
	c2		:   out STD_LOGIC ;
	locked	:   out STD_LOGIC 
	);
end component;
--------------------------------------------------------

component fsm_pll is
port ( 
	areset	:    in STD_LOGIC := '0';
	inclk0	:    in STD_LOGIC := '0';
	c0		:   out STD_LOGIC ;
	c2		:   out STD_LOGIC ;
	locked	:   out STD_LOGIC 
	);
end component;
--------------------------------------------------------

component AEregisters is
port(
	clk	:    in std_logic;	-- 100MHz System Clock
-- System Interface signals and buses
	rst	:    in std_logic;
	ADDR	:    in std_logic_vector ( 3 downto 0 );
	DATA	: inout std_logic_vector ( 15 downto 0 );
	Rn		:    in std_logic;
	Wn		:    in std_logic;
	CEn	:    in std_logic;
	Drr 	:   out std_logic;
	Trrd	: 	out std_logic;
	DR		:    in std_logic_vector( 15 downto 0 );	--	2.1	R0 - Data Read Register (DR)
	ER		:   out std_logic_vector( 15 downto 0 );	--	2.2	R1 - End Register (ER)
	ILVS	:   out std_logic_vector( 4 downto 0 );		--	2.3	R2 - Resolution Register (RR) translates to this
	HALFSPD	:   out std_logic;							--		this is 1 when we go at half speed (ILVS= 1)
	DLY0	   :   out std_logic_vector( 4 downto 0 );		--		delay for interleave 0
	DLYSTEP	:   out std_logic_vector( 4 downto 0 ); 	--		delay step (decerement by)
	PR		   :   out std_logic_vector( 9 downto 0 );		--	2.4	R3 -  Pulsewidth Register (PR)
	AVTRACES :   out std_logic_vector( 20 downto 0 );	--	2.5	R4 -  Averages Register (AR) translates to this:
														-- 		=2^AR up to 1048576
	TLR		:    in std_logic_vector ( 15 downto 0 );	--	2.6	R5 -  Traces Accumulated Register - low word (TLR)
	THR		:    in std_logic_vector ( 15 downto 0 );	--	2.7	R6 -  Traces Accumulated Register - high word (THR
	CR		: inout std_logic_vector ( 15 downto 0 )	--	2.8	R7 -  Control/Status Register (CR)
);
end component;
--------------------------------------------------------

--Defintion of Register CR bits ------------------------------------------------------------------------
	constant cr_STRT	: natural:=0;
	constant cr_RST	: natural:=1;
	constant cr_MODE	: natural:=2; -- 0 - start acquistion cycle, 1- read cycle
	constant cr_INTP	: natural:=3;
	constant cr_LSR0	: natural:=4;
	constant cr_LSR1	: natural:=5;
	constant cr_LSR2	: natural:=6;
	constant cr_LH		: natural:=7;
	constant cr_ADCPWR	: natural:=8;
	constant cr_RESUME	: natural:=9;
	constant cr_ACCUM	   : natural:=10;
	constant cr_ADCOVF	: natural:=13;
	constant cr_IDLEn	   : natural:=14;
	constant cr_INT 	   : natural:=15;

-- Configuration Register related signals:
-- These signal will be used in the top level to connect to entities that will use them or drive them ----
	signal sig_DR		   : std_logic_vector( 15 downto 0 );	--	2.1	R0 - Data Read Register (DR)
	signal sig_ER		   : std_logic_vector( 15 downto 0 );	--	2.2	R1 - End Register (ER)
	signal sig_ILVS		: std_logic_vector( 4 downto 0 );	--	2.3	R2 - Resolution Register (RR) translates to this
	signal sig_HALFSPD	: std_logic;						--		this is 1 when we go at half speed (ILVS= 1)
	signal sig_DLY0		: std_logic_vector( 4 downto 0 );	--delay for interleave 0
	signal sig_DLYSTEP	: std_logic_vector( 4 downto 0 ); 	--delay step (decerement by)
	signal sig_PR		   : std_logic_vector( 9 downto 0 );	--	2.4	R3 -  Pulsewidth Register (PR)
															--	2.5	R4 -  Averages Register (AR) translates to this:
	signal sig_AVTRACES	: std_logic_vector( 20 downto 0 );	-- 		=2^AR up to 1048576
	signal sig_TLR		   :  std_logic_vector ( 15 downto 0 );--	2.6	R5 -  Traces Accumulated Register - low word (TLR)
	signal sig_THR		   :  std_logic_vector ( 15 downto 0 );--	2.7	R6 -  Traces Accumulated Register - high word (THR
	signal sig_CR		   :  std_logic_vector ( 15 downto 0 );	--	2.8	R7 -  Control/Status Register (CR)

-- Operational signals----------------------------------------------------------------------------------
	signal Drr_sig 		: std_logic; 
	signal Drr_sig1		: std_logic; 
	signal Drr_sig2		: std_logic; 
	signal Drrend		   : std_logic; 
	--------------------------------	
	signal Trr_sig 		: std_logic; 
	signal Trr_sig1		: std_logic; 
	signal Trr_sig2		: std_logic; 
	signal Trrend		: std_logic; 
	attribute keep of Trr_sig : signal is true;
	
	signal HalfSpeedFF	: std_logic;
	attribute keep of HalfSpeedFF: signal is true;
	signal AddRAM		: std_logic;
	signal AddRAM0		: std_logic;
	signal AddRAM1		: std_logic;
	signal AddRAM2		: std_logic;

	signal sig_adc_clk	: std_logic;
	signal adc_data_reg	: std_logic_vector( 13 downto 0 );
   
-- Reed ZBT  Signals ----------------------------------------------
	signal Reedadrs_lmt	: std_logic_vector( 20 downto 0 );
    signal Last16b 		: std_logic;
    signal Rdrg			: std_logic_vector( 31 downto 0 );
	---------------------------------------------------------------
	signal sig_RESET0	: std_logic;
	signal sig_RESET	: std_logic;

	signal sig_INT		: std_logic;
    signal contgap 		: std_logic_vector( 16 downto 0 );   
    signal lmtgap 		: std_logic_vector( 16 downto 0 );		
	signal sig_ZBT_CLK	: std_logic;
	signal sig_DLY		: std_logic_vector( 4 downto 0 );   
	signal sig_start_pulse	: std_logic;
	signal sig_STRT		: std_logic;
	signal sig_STOP 	: std_logic;
	signal zio_en  		: std_logic;
	signal dla1 		: std_logic;
	signal dla2 		: std_logic;
	signal Latch_data 	: std_logic;
	signal ZBTrw_sig1 	: std_logic;
	signal ZBTrw_sig 	: std_logic;
	signal ILcnt		: std_logic_vector( 4 downto 0 ); -- Interleave Counter .
	signal ADRscntrg	: std_logic_vector( 19 downto 0 ); -- Addres_interleave_accumulator Counter .
	signal TRAcntr		: std_logic_vector( 20 downto 0 ); -- Trace Counter .
	signal LMTcntr		: std_logic_vector( 15 downto 0 ); -- Limit Counter .
	signal ZBTrg		: std_logic_vector( 31 downto 0 );	--	ZBT Data register.
	signal ADDrg		: std_logic_vector( ZBT_IO'high downto 0 );	--	Addition Data register.
    signal PLL_out 		: std_logic;
    signal c1 			: std_logic;
    signal c2 			: std_logic;
	signal Locked_fsm_pll 	: std_logic;
	signal Locked_delay_pll : std_logic;
	signal s_pulse		: std_logic;
	signal sig_dly_trg  : std_logic;
	signal sig_lsr_drive	: std_logic_vector( 2 downto 0 );

-- FSM---------------------------------------------------------------------------------------------------------
	type AEstate_type is (   --THESE ARE THE STATES
			IDLE,
			START_ACQ,		-- will sart acquisition on the next clock cycle
			NEWTRACE,		-- start the next full trace (starting with interleave 0)
			NEXTILV,		-- start a new interleave
			AcqreData,		-- in this state ADC and ZBT are enabled .
			AcqreData0,		-- in this state ADC and ZBT are enabled .
			AddDATA,		-- in this state ADC samples added to memeory, location stepped etc.
			MESRTdn,		-- Measurement Complete.
			IL_Cnt,			-- See if any more interleaves are needed.
			Gap_cnt,		-- Count out minimum time between laser firings.
			ReedZBT,
			ReedZBT1,
			ReedZBT2,
			ReedZBT3,
			ReedZBT4,
			ReedZBT5,
			ReedZBT6,
			ReedZBT7
			 ); --we'll add more as needed	
	signal curr_state	: AEstate_type ;

-- Signals to keep for observation	
	attribute keep of sig_DR : signal is true;
	attribute keep of Drr_sig : signal is true;
	attribute keep of PLL_out : signal is true;

    --------------------------------------------------------------------------------------------------------	

begin
	--Pins that are static, and likely to remain so.
	ZBT_ADV <= '0';		ZBT_IOP <= (others=>'Z');
	ZBT_MODE <= '0';	ZBT_CE2 <= '1';			ZBT_CE2n <= '0'; 		ZBT_OEn <= '0'; 		
	ZBT_BW4n <= '0';	ZBT_BW3n <= '0';		ZBT_BW2n <= '0'; 		ZBT_BW1n <= '0';
	dly_ae	<= '1' ;
	
    --------------------------------------------------------------------------------------------------------
 	--Pin driving signals...
	adc_clk <= sig_adc_clk;
	lsr_drive <= sig_lsr_drive;
	dly_trg <= sig_dly_trg;	-- Need to drive pin only if ext. delay chip is used.
	sys_INT <= sig_INT xnor sig_CR(cr_INTP);
	
	ZBT_WEn <= ZBTrw_sig; 
	ZBT_A   <= ADRscntrg ;
	ZBT_CLK <= sig_ZBT_CLK;
	
	------------------------------ 
	
	sig_CR(cr_IDLEn	) <= '0' when curr_state = IDLE else '1';
	sig_CR(cr_INT)<= sig_INT;
	sig_CR(cr_ADCOVF) <= '0' when (sig_RESET ='1' or curr_state = START_ACQ) else
					  '1' when	adc_OF='1' and curr_state /= IDLE; 

	adc_shdn <= not sig_CR(cr_ADCPWR);	
	adc_OEn  <= not sig_CR(cr_ADCPWR);	-- when shutting down also pull up OEn to put it in sleep mode; we'll also stop the clock
	
 	--Sum of RAM+ADC or just ADC   
	ADDrg <= ZBTrg + ("000000000000000000"&adc_data_reg) when Addram = '1' else ("000000000000000000"&adc_data_reg);  
	lmtgap <= sig_PR*"1100100" ; --Pulsewidth*100 is the minimum pulse period
	
	-- Read ZBT address limit -----------------------------------------------
	Reedadrs_lmt <= (sig_ER*sig_ILVS) + 1 ;  
	-------------------------------------------------------------------------
		
Registers: AEregisters
	port map(
		clk		=> clk,			rst		=> sig_RESET,	ADDR	=> sys_ADDR,		DATA	=> sys_DATA,
		Rn		=> sys_Rn,		Wn		=> sys_Wn,		CEn		=> sys_CEn,			Drr		=> Drr_sig,
		Trrd	=> Trr_sig, 	DR		=> sig_DR,		ER		=> sig_ER,			ILVS	=> sig_ILVS,
		HALFSPD	=> sig_HALFSPD,	DLY0	=> sig_DLY0,	DLYSTEP	=> sig_DLYSTEP,		PR		=> sig_PR,
		AVTRACES=> sig_AVTRACES,TLR		=> sig_TLR,		THR		=> sig_THR,			CR		=> sig_CR
	);
------------------------------------------------------------------------------------
PLL_ports : fsm_pll
	port map (
		areset	=> sys_rst,
		inclk0	=> clk,	
		c0		=> PLL_out,
		c2		=> sig_ZBT_CLK		
--		locked	=> Locked_fsm_pll	
	); 
------------------------------------------------------------------------------------
PLL_dly: PLL
	port map (
		areset	=> sys_rst,
		inclk0	=> clk2,
		c1		=> c1,
		c2		=> c2
--		locked	=> Locked_delay_pll
	); 
------------------------------------------------------------------------------------
Pulser: pulse_gen
	generic map(pw_bits => 10)
	port map(
		areset	=> sig_RESET,
		clk		=> clk,
		pwidth	=> sig_PR,
		trig	=> sig_start_pulse,
		pulseout=> sig_dly_trg
	);
------------------------------------------------------------------------------------
Delay: pulse_delay
	port map(
		areset		=> sig_RESET,
		clk_400_000 => c1,
		clk_400_180 => c2,
		pulsein		=> sig_dly_trg,
		pdelay		=> sig_DLY(3 downto 0),
		outsel		=> 	sig_CR(cr_LSR2 downto cr_LSR0),
		pulseout	=> sig_lsr_drive
	);
------------------------------------------------------------------------------------

-- free running ADC clock.
Clock_ADC: process (clk, sig_RESET, sig_CR(cr_ADCPWR), sig_adc_clk)
begin
	if sig_RESET = '1'or sig_CR(cr_ADCPWR) = '0' then
		sig_adc_clk <= '0';
	elsif rising_edge(clk) then
		sig_adc_clk <= not sig_adc_clk;
	end if;
end process;

-- FSM--------------------------------------------------------------------------
process(curr_state,clk, sig_RESET, sig_CR) -- Single Process for Fsm operation --
    
	procedure reset_all is  -- Procedure to initialize all
		begin
		sig_start_pulse <= '0'; 
		LMTcntr <=(others =>'0');	
		ILcnt <=(others =>'0');	
	 	ADRscntrg <=(others =>'0');	
		ZBT_CENn <= '0';
	    return ;
	end procedure;    
--------------------------------------------------------------------------------    
begin
	if sig_RESET='1' then
		reset_all;	
		sig_INT <= '0';
		ZBTrw_sig <= '1';
		ZBT_CE1n <= '1';	
 		curr_state <= IDLE;
	elsif rising_edge(clk) then
		case curr_state is
      --------------------------------------------------------------------------------------------------
		when IDLE =>
			Last16b <= '0'; -- setup so we can read the design revision
			Rdrg <= revision & AErev; -- by reading DR
			
			ZBTrw_sig <= '1';
			ZBT_CENn <= '0';
			ZBT_ZZ	<= '1';
			---------------
			
			if sig_STRT ='1' then
				sig_INT <= '0';
				ZBT_ZZ	<= '0';
				
				if sig_CR(cr_MODE)='0' then   -- start Acquisition cycle , MODE = 02 .
					reset_all;				-- Reset all counters and setting some default
					if (sig_CR(cr_RESUME)='0') then -- don't clear he trace counter if we're RESUMEing.
						TRAcntr <=(others =>'0');
					end if;
					curr_state <= START_ACQ; 
				else
				    curr_state <= ReedZBT; 
				end if;
			else
				curr_state <= IDLE;
			end if;
      --------------------------------------------------------------------------------------------------
		when START_ACQ =>
		
			ZBT_CE1n <= '0'; 	
			-- do not remove EVEN IF EMPTY.
			-- "curr_state" is checked by processes outside the FSM.
			curr_state <= NEWTRACE ; 
		-----------------------------------------------------------	
		when NEWTRACE =>
			if TRAcntr = sig_AVTRACES or sig_STOP='1' then 
				--sig_INT <= '1';--moved to MESRTdn
				curr_state <= MESRTdn;
		 	else  
				ZBT_CENn <= '0';
				ILcnt <= (others =>'0');	
				sig_DLY <= sig_DLY0;
				curr_state <= NEXTILV;
			end if ; 
	----------------------------------------------------------------------------------------------------
		when NEXTILV =>
			HalfSpeedFF <='0';
			LMTcntr <=(others =>'0');	 
			ADRscntrg <= "000000000000000"&ILcnt;  
			sig_DLY <= sig_DLY - sig_DLYSTEP;   -- set pulse delay

			sig_start_pulse <= '0';
			ZBTrw_sig <= '1';

			curr_state <= AcqreData0;
		when AcqreData0 => 	
			
			if sig_adc_clk	= '1' then 			-- get synchronized to the AD clock.
				sig_start_pulse <= '1';				-- start pulse on next clock, Laser Fire
				curr_state <= AcqreData;
			else
				curr_state <= AcqreData0;
			end if ; 
	
	----------------------------------------------------------------------------------------------------
		when AcqreData =>
			ZBT_CENn <= '0';
			if LMTcntr = sig_ER then
    		   curr_state <= Gap_Cnt;
			else   
				if sig_HALFSPD = '1' then
					HalfSpeedFF <= not HalfSpeedFF;
				end if;

				sig_start_pulse <= '0';		  -- turn-off flag , Laser Off.
				ZBTrw_sig <= '0';
				curr_state <= AddDATA ;  
			end if ;
	-----------------------------------------------------------------------------------------------
		when AddDATA =>
			ZBT_CENn <= '0';
			if HalfSpeedFF = '0' then
				LMTcntr <= LMTcntr + '1'; 
				ADRscntrg <= ADRscntrg + sig_ILVS ;
			end if;
			ZBTrw_sig <= '1';
			curr_state <= AcqreData ;  
	------------------------------------------------------------------------------------------------
		when Gap_cnt =>
			if contgap = lmtgap then  
				curr_state <= IL_Cnt;
			else 
				curr_state <= Gap_cnt;
			end if ; 
----------------------------------------------------------------------------------------------------
		when IL_Cnt =>
			if ILcnt = ( sig_ILVS - 1 )	 then 
				TRAcntr <= TRAcntr + "000000000000000000001"; 
				curr_state <= NEWTRACE ; 
			else 
				LMTcntr <=( others=>'0');
				ILcnt <= ILcnt + "00001";
				ADRscntrg <=(others =>'0');	-- is this needed?
				curr_state <= NEXTILV ;   
			end if ; 
-----------------------------------------------------------------------------------------------------	
-- ZBT Sram Procedures starts from here --
       when ReedZBT =>	ADRscntrg <=(others=>'0');
						Last16b <= '0';
						ZBTrw_sig <= '1'; 
						ZBT_CENn <= '0';
						ZBT_CE1n <= '0'; 		
						curr_state <= ReedZBT2 ; 

		when ReedZBT2 =>ZBT_CENn <= '0'; 
						curr_state <= ReedZBT3 ; 

		when ReedZBT3 =>ZBT_CENn <= '0'; 
				 		ADRscntrg <= ADRscntrg + 1;	
						curr_state <= ReedZBT4 ; 

		when ReedZBT4 => ZBT_CENn <= '1'; 
						 curr_state <= ReedZBT5 ; 

		when ReedZBT5 => ZBT_CENn <= '1'; 
				 		 curr_state <= ReedZBT6 ; 

		when ReedZBT6 => ZBT_CENn <= '1'; 
				 		 Rdrg <= ZBTrg;		
						 curr_state <= ReedZBT7 ; 

        when ReedZBT7 =>ZBT_CENn <= '1'; 
						if sig_STOP='1' then -- we're done early
					   		--sig_INT <= '1'; --moved to MESRTdn
			         		curr_state <= MESRTdn;
						else
							curr_state <= ReedZBT7 ; -- we're staying here unless we're done
							if ADRscntrg = Reedadrs_lmt then -- wait for one more read by the system.
								if Drrend = '1' then
					         		curr_state <= MESRTdn;	-- ok, now we're done
								end if;
							else
								if Drrend = '1' then
									if Last16b = '0' then
							    		ZBT_CENn <= '0';
								 		ADRscntrg <= ADRscntrg + 1;
									else
								 		Rdrg <= ZBTrg;	
									end if;
									Last16b <= not Last16b;
								end if;
							end if ;
						end if ;
						
		when MESRTdn => 
			ZBT_CE1n <= '1'; -- Disabling CS for Ram
			sig_INT <= '1';
			curr_state <= IDLE;
----------------------------------------------------------------------------------------------------------		
		when others  => curr_state <= IDLE;
----------------------------------------------------------------------------------------------------------		
		end case;
	end if;
end process;
----------------------------MAIN FSM Process End----------------------------------------------------------

-- Dead Time control process -------------------------------  
laser_firing_gap : process ( sig_RESET, clk , sig_start_pulse )
begin 
	if sig_start_pulse = '1' or sig_RESET = '1' then 
 		contgap <= (others=>'0') ;
	elsif sig_start_pulse = '0' then 
		if rising_edge(clk) then
			if contgap /= lmtgap then
				contgap <= contgap + "00000000000001";
			end if ;
	    end if ;
	  end if ;
end process ;

   
--Register RAM & ADC data
reg_data: process (PLL_out)
begin
	if rising_edge(PLL_out) then 
		if ZBTrw_sig = '1' then
			ZBTrg <= ZBT_IO;
			  adc_data_reg <= adc_data;
		end if;
	end if;
end process ;
--
-- Create signal to enable accumulating of the data
--
Add_ena: process ( PLL_out)
begin
	if rising_edge(PLL_out) then 
		if (TRAcntr = "000000000000000000000" and sig_CR(cr_ACCUM) = '0') then
			AddRAM0 <= HalfSpeedFF and sig_HALFSPD; -- add even when TRAcntr, if this is the second of the Half-Speed samples
		else
			AddRAM0	<= '1';
		end if;
		AddRAM1 <= AddRAM0;
		AddRAM2 <= AddRAM1;
		AddRAM <= AddRAM2;
	end if ;
end process ;

-- 
--Create delayed versions of the "START" bit  of the CR register
-- so that we can generate start and stop pulses from them.
--
Start_stop : process ( clk , sig_CR(cr_STRT) ) 
begin 
	if rising_edge(clk) then 
		dla1 <= sig_CR(cr_STRT) ;
		dla2 <= dla1 ;
	end if ;
end process ;

	sig_STOP <= not dla2; 
	sig_STRT <= (dla1) and (not dla2); 


-- ZBT SRAM Bus impedence control
Z_CTL:	process (sig_RESET, clk)
	variable zio : std_logic_vector( ZBT_IO'range); 
 begin
	if  sig_RESET='1' then
		zio_en <= '0';
	elsif rising_edge(clk) then
		if curr_state = AcqreData then 
			zio_en <= '1';
		else
			zio_en <= '0';
		end if;
	end if;
end process;
	
ZBT_IO <= ADDrg when zio_en = '1' else (others=>'Z') ;

-------------------------------------------------------
--
-- Generate delayed versions of Drr_sig (synchronized to the clock
-- to pemit detection of the trailing edge of the DR read strobe
--
drr_delay : process ( clk , sig_RESET	 )
begin 
	if sig_RESET = '1' then 
		Drr_sig1 <= '0' ;
 		Drr_sig2 <= '0' ;
		Trr_sig1 <= '0' ;
		Trr_sig2 <= '0' ;		

	elsif rising_edge(clk) then 
		Drr_sig1 <= Drr_sig ;
		Drr_sig2 <= Drr_sig1 ;
		Trr_sig1 <= Trr_sig ;
		Trr_sig2 <= Trr_sig1 ;
 	end if ;
end process ;

	Drrend <= Drr_sig2 and (not(Drr_sig1));
	Trrend <= Trr_sig2 and (not(Trr_sig1));

-----------------------------------------------------
-- Select high or low 16 bits for output.
sig_Dr <= Rdrg ( 31 downto 16 ) when (Last16b xnor sig_CR(cr_LH)) = '0' else Rdrg ( 15 downto 0 );   


-- Traces update process ------------------------------------------
Traces_read : process ( clk, Trrend, sig_RESET  )
begin 
	if sig_RESET = '1' then 
		sig_THR <= (others=>'0');
		sig_TLR <=  (others=>'0');
	elsif rising_edge(clk) then
		if Trrend = '1' then
			sig_TLR <= TRAcntr( 15 downto 0 );
			sig_THR <= ("00000000000" & TRAcntr( 20 downto 16 ));	
		end if ;
	end if ;
end process ;
--------------------------------------------------------------------		

--
-- generate a proper master reset
--
reset_gen: process(clk, sys_rst, sig_CR)
begin
	if rising_edge(clk) then
		sig_RESET0 <= sys_rst or sig_CR(cr_RST);
		sig_RESET <= sig_RESET0;
	end if;
end process ;

end acquisition;      
