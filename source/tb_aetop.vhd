-- $Id: tb_aetop.vhd 1.35 2007/06/28 21:34:21Z Aghouri Exp $
-- Test bench to test the functionality of AEtop --
-- Dated : 06 Feb 2007 .
-- Ahmed Asim Ghouri .
-- Only testing System i/o's --
-----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
--------------------------------
--library Packages;
--use Packages.AE_top_lib.all;
--use package_utility.all;
-----------------------------------------------------
-----------------------------------------------------

entity tb_aetop is 
end tb_aetop ;
--------------
architecture testing of tb_aetop is

-- Functions declared here --------
FUNCTION convert_string( S: in STRING) RETURN STD_LOGIC_VECTOR;
--------------------------------------------------------------
--Converts string into std_logic_vector 
----------------------------------------------------------------

FUNCTION convert_string(S: in STRING) RETURN STD_LOGIC_VECTOR IS
	VARIABLE result : STD_LOGIC_VECTOR(S'RANGE);
		BEGIN
			FOR i	IN S'RANGE LOOP
				IF 	S(i) = '0' THEN
					result(i) := '0'; 
				ELSIF S(i) = '1' THEN 
					result(i) := '1';
				ELSIF S(i) = 'X' THEN
					result(i) := 'X';
				ELSE
					result(i) := 'Z';
				END IF;
			END LOOP;
		RETURN result;
END convert_string;
------------------------------------------------------------
FUNCTION CONV_INTEGER1(S : STD_LOGIC_VECTOR) RETURN INTEGER;

------------------------------------------------------------
--Converts std_logic_vector into integer
------------------------------------------------------------

FUNCTION CONV_INTEGER1(S : STD_LOGIC_VECTOR) RETURN INTEGER IS
		VARIABLE result : INTEGER := 0;
		BEGIN
			FOR i IN S'RANGE LOOP
				IF S(i) = '1' THEN
					result := result + (2**i);
				ELSIF S(i) = '0' THEN
					result := result;
				ELSE
					result := 0;
				END IF;
			END LOOP;
			RETURN result;
		END CONV_INTEGER1;
-------------------------------------------------------------------
FUNCTION CONV_LV (S : INTEGER; N:NATURAL ) return std_logic_vector ;
-------------------------------------------------------------------
FUNCTION CONV_LV (S : INTEGER; N:NATURAL ) return std_logic_vector is
     variable result : std_logic_vector(N-1 downto 0) := (others=>'0');
     variable m : INTEGER:=S;

 begin
     for i in 0 to N-1 loop
         IF m rem 2 = 1 then
             result(i) := '1';
         else
             result(i) := '0';
         end if;
         m := m/2;
     end loop;
     return result;
 end CONV_LV;	
-------------------------------------------
-- ADC VHDL model ------------------------- 
--component LTC2228 is
--generic(
--   ADC_bits : natural:=12;
--   pipe_len	: natural:=5;
--   tCO      : time:=2.7 ns;
--   tOEn   : time:= 4.3 ns
--
--   );
--port(
--
--    VDD    : in  real:=3.300;
--    VCM    : out real:=1.500;-- (1.475, 1525)
--    
--    AinP   : in  real:=1.55;
--    AinN   : in  real:=1.55;
--    RefH   : out real:= 0.55;
--    RefL   : out real:=-0.55;
--    
--    Sense  : in  real:=3.3; -- Sense=VCM        ->(Ref=Internal, Range=+/-0.5V)
--                      -- Sense=VDD        ->(Ref=Internal, Range=+/-1.0V)
--                      -- Sense=(0.5, 1.0) ->(Ref=Sense, Range=+/-Sense)
--    
--    MODE   : in  real:=0.0; -- (0/3)VDD --> Offset binary, clock duty-cycle stabilizer=OFF
--                      -- (1/3)VDD --> Offset binary, clock duty-cycle stabilizer=ON
--                      -- (2/3)VDD --> 2's complement, clock duty-cycle stabilizer=ON
--                      -- (3/3)VDD --> 2's complement, clock duty-cycle stabilizer=OFF
--    
--    Clk    : in  std_logic;
--    SHDN   : in  std_logic;
--    OEn    : in  std_logic;
--    OVF     : out std_logic;
--    D      : out std_logic_vector(11 downto 0) -- need a generic here
--    );
--    
--end component;
--------------------------------------------------------------------------------------------------------

---- ZBT Sram Model --------------------------------------------------------------------------------------
--component cy7c1350 is 
--    GENERIC (
--        -- Constant parameters
--	addr_bits : INTEGER;
--	data_bits : INTEGER;
--        -- Timing parameters:
--        tCYC	: TIME; tCH	: TIME; tCL	: TIME; tCO	: TIME; tAS	: TIME; tCENS	: TIME; tWES	: TIME; tDS	: TIME; tAH	: TIME; tCENH	: TIME; tWEH	: TIME; tDH	: TIME
--	);
--
-- PORT (
--	Dq	: INOUT STD_LOGIC_VECTOR ((data_bits - 1) DOWNTO 0);   	-- Data I/O
--	Addr	: IN	STD_LOGIC_VECTOR ((addr_bits - 1) DOWNTO 0);   	-- Address
--	Mode	: IN	STD_LOGIC; 						-- Burst Mode
--	Clk	: IN	STD_LOGIC;                                   -- Clk
--	CEN_n	: IN	STD_LOGIC;                                   -- CEN#
--	AdvLd_n	: IN	STD_LOGIC;                                   -- Adv/Ld#
--	Bwa_n	: IN	STD_LOGIC;                                   -- Bwa#
--	Bwb_n	: IN	STD_LOGIC;                                   -- BWb#
--	Bwc_n	: IN	STD_LOGIC;                                   -- Bwc#
--	Bwd_n	: IN	STD_LOGIC;                                   -- BWd#
--	Rw_n	: IN	STD_LOGIC;                                   -- RW#
--	Oe_n	: IN	STD_LOGIC;                                   -- OE#
--	Ce1_n	: IN	STD_LOGIC;                                   -- CE1#
--	Ce2	: IN	STD_LOGIC;                                   -- CE2
--	Ce3_n	: IN	STD_LOGIC;                                   -- CE3#
--	Zz	: IN	STD_LOGIC                                  -- Snooze Mode
--	    );
--   END component ; 
------------------------------------------------------------------------------------------------------------------
 
-- Acquisition Engine -------------------------------
component AEtop is 
port (
     	clk		: in std_logic;	-- 100MHz System Clock
    	clk2		: in std_logic;	-- 100MHz System Clock
		
-- System Interface signals and buses -- tot signals : 22 
	sys_rst		:  in std_logic;
	sys_ADDR		:  in std_logic_vector ( 3 downto 0 );
  	sys_DATA		: inout std_logic_vector ( 15 downto 0 );
   sys_INT		: out std_logic:='0';
   sys_Rn		:  in std_logic;
	sys_Wn		:  in std_logic;
	sys_CEn		:  in std_logic;
-- From Dual-Ended Distance Receiver 	
	 DDin		: in std_logic; -- tot no : 1 signals 
-- ADC signals and buses -- tot no : 18 signals 
	adc_data		:  in std_logic_vector( 13 downto 0 ); 
	adc_clk		: out std_logic;
	adc_shdn		: out std_logic;
	adc_OEn		: out std_logic;
	adc_OF		:  in std_logic;
-- Programmable Delay Signals -- tot no : 11 signals
	dly_out		: out std_logic_vector ( 7 downto 0 );
--	dly_mode		: out std_logic;
	dly_trg		: out std_logic;
	dly_ae		: out std_logic;
-- Laser Drive Signals -- tot no : 11 signals
	lsr_delayed		:  in std_logic;							-- Output from the prog. delay line
	lsr_drive		: out std_logic_vector( 2 downto 0 );	-- Trigger outputs for up to 3 lasers
--ZBT SRAM signals -- tot signals : 70 signals 
	ZBT_A		: out std_logic_vector( 19 downto 0 ); 
	ZBT_ADV		: out std_logic;
	ZBT_WEn		: out std_logic; 
	ZBT_CENn		: out std_logic ;
   ZBT_BW4n		: out std_logic ;
   ZBT_BW3n		: out std_logic ;
   ZBT_BW2n		: out std_logic ;
   ZBT_BW1n		: out std_logic ;
	ZBT_CE1n		: out std_logic ;
   ZBT_CE2		: out std_logic ;
   ZBT_CE2n		: out std_logic ; 
	ZBT_CLK 		: out std_logic ;
   ZBT_IO		: inout std_logic_vector ( 31 downto 0 );
	ZBT_IOP		: inout std_logic_vector (  4 downto 1 );
	ZBT_MODE		: out std_logic ;
	ZBT_OEn		: out std_logic ;
   ZBT_ZZ		: out std_logic 
);    
end component ;
------------------------------------------------------------------------------------------------------------ 
 
 	constant DReg			: std_logic_vector ( 3 downto 0 ):="0000"; -- Data Read
	constant EReg			: std_logic_vector ( 3 downto 0 ):="0001"; -- End
	constant RReg			: std_logic_vector ( 3 downto 0 ):="0010"; -- Resolution
	constant PReg			: std_logic_vector ( 3 downto 0 ):="0011"; -- Pulse Width
	constant AReg			: std_logic_vector ( 3 downto 0 ):="0100"; -- Averages
	constant TLReg	   	: std_logic_vector ( 3 downto 0 ):="0101"; -- Traces Accumulated ? low word (TLR):
	constant THReg		   : std_logic_vector ( 3 downto 0 ):="0110"; -- Traces Accumulated ? high word (THR):
	constant CReg			: std_logic_vector ( 3 downto 0 ):="0111"; -- Control/Status
   ---------------------------------------------------------------------------------------------------



--
--FOR ALL: AEtop USE ENTITY work.AEtop(acquisition);
--FOR ALL: AEtop USE ENTITY work.AEtop(structure);
--
	signal tbVCM    : real;
	signal tbAinP   : real;
	signal tbAinN   : real;
	signal tbLT2228_d	 : std_logic_vector ( 11 downto 0 );
	signal tsysR		: std_logic:='1';
	signal tsysW		: std_logic:='1';
	signal tsysCE		: std_logic:='1';
	signal tb_rst		: std_logic:='0';
	
	signal	tb_clk , tbsysclk,	tsysint,		tbddin ,		tbadc_clk,
		tbadc_shdn,		tbadcoe,		tbadcof,		tbdlymd,
		tbdlytrg,		tbdlyae,		tblsrdlyd		 		: std_logic;

	signal	tbzbt_adv,		tbzbt_rw,		tbzbt_ce1n,		tbzbt_bw4, 
		tbzbt_bw3,		tbzbt_bw2,		tbzbt_bw1,		tbzbt_ce1,
		tbzbt_ce2,		tbzbt_ce3n,		tbzbt_clk,		tbzbt_lbon,
		tbzbt_ce,		tbzbt_oe,		tbzz, tpll_out				: std_logic;

	signal tbsys_addr			: std_logic_vector ( 3 downto 0 ):=(others=>'1');
	signal tbsys_data			: std_logic_vector ( 15 downto 0 );
	signal addrscnt			: std_logic_vector ( 3 downto 0 ); 
	signal tbadc_data			: std_logic_vector ( 13 downto 0 );
	signal tbadc_dat			: std_logic_vector ( 13 downto 0 );
	signal tbdlyout			: std_logic_vector ( 7 downto 0 );
	signal tblsrdrve			: std_logic_vector ( 2 downto 0 );
	signal tbzbtadrs			: std_logic_vector ( 19 downto 0 );
	signal tbzbtio			   : std_logic_vector ( 31 downto 0 ):=(others=>'W');  
	signal tbzbtiop			: std_logic_vector ( 4 downto 1 ):=(others=>'L');  
	signal ZBT_data			: std_logic_vector ( 31 downto 0 );
-------------------------------------------------------
   signal Tclk      : time:= 10 ns;
-------------------------------------------------------

begin 
tbzbtiop <= (others=>'L');  
    
--ADC: LTC2228 port map (
--	VCM	=> tbVCM,
--	AinP	=> tbAinP,
--	AinN	=> tbAinN,
--	Clk	=> tbadc_clk,
--	SHDN	=> tbadc_shdn,
--	OEn	=> tbadcoe,
--	OVF	=> tbadcof,
--	D	=> tbLT2228_d
--);

uut : AEtop port map ( 
	clk 	   => tb_clk ,
	clk2 	   => tb_clk ,
	sys_rst  => tb_rst,  
	sys_ADDR => tbsys_addr, 
	sys_DATA => tbsys_data , 
	sys_INT	=> tsysint,  
	sys_Rn   => tsysR, 
	sys_Wn   => tsysW, 
	sys_CEn  => tsysCE,
	DDin	   => tbddin,
	adc_data => tbadc_data ,
	adc_clk	=> tbadc_clk,
	adc_shdn	=> tbadc_shdn,
	adc_OEn  => tbadcoe,
	adc_OF   => tbadcof,
-- Programmable Delay Signals -- tot no : 11 signals
	dly_out 	=> tbdlyout ,
	--dly_mode	=> tbdlymd,
	dly_trg 	=> tbdlytrg,
	dly_ae 	=> tbdlyae,
-- Laser Drive Signals -- tot no : 11 signals
	lsr_delayed	=> tblsrdlyd,
	lsr_drive   => tblsrdrve,
-- ZBT SRAM signals
	ZBT_A		=> tbzbtadrs,
	ZBT_ADV 	=> tbzbt_adv,   
	ZBT_WEn 	=> tbzbt_rw, 
	ZBT_CENn	=> tbzbt_ce,
	ZBT_BW4n	=> tbzbt_bw4,
   ZBT_BW3n	=> tbzbt_bw3,
   ZBT_BW2n	=> tbzbt_bw2,
   ZBT_BW1n	=> tbzbt_bw1,
	ZBT_CE1n	=> tbzbt_ce1n,
   ZBT_CE2 	=> tbzbt_ce2,
   ZBT_CE2n	=> tbzbt_ce3n,
	ZBT_CLK 	=> tbzbt_clk,
   ZBT_IO	=>	tbzbtio,
	ZBT_IOP	=> tbzbtiop ,
   ZBT_MODE	=> tbzbt_lbon ,
   ZBT_OEn 	=> tbzbt_oe,
   ZBT_ZZ	=> tbzz
     	);
------------------------------------------Cypress RAM----------------------------------------
--Cypress_ram : cy7c1350 
-- 	GENERIC MAP (
-- 	addr_bits => 6, --17
-- 	data_bits => 36,
--        -- Timing parameters:
--        --166MHz
----        tCYC	=> 6.0 ns, tCH		=> 2.5 ns, tCL		=> 2.5 ns, tCO		=> 3.5 ns, tAS		=> 1.5 ns, tCENS	=> 1.5 ns, tWES	=> 1.5 ns, tDS	=> 1.5 ns, tAH	=> 0.5 ns, tCENH	=> 0.5 ns, tWEH	=> 0.5 ns, tDH	=> 0.5 ns
--        --133MHz
--        tCYC	=> 7.5 ns,	tCH  => 3.0 ns, tCL  => 3.0 ns, tCO  => 4.0 ns, tAS	 => 1.5 ns, tCENS	=> 1.5 ns, tWES	=> 1.5 ns, tDS => 1.5 ns, tAH => 0.5 ns, tCENH	=> 0.5 ns, tWEH	=> 0.5 ns, tDH	=> 0.5 ns
--
--	)
--	port map 
-- 	(
--	Dq(31 downto 0)	=>	tbzbtio,
--	Dq(35 downto 32) => tbzbtiop,
--	Addr => tbzbtadrs(16 downto 0),	
--	Mode => tbzbt_lbon,	
--	Clk 	=> tbzbt_clk,
--	CEN_n =>  tbzbt_ce,	
--	AdvLd_n => tbzbt_adv,   	
--	Bwa_n => tbzbt_bw1,
--	Bwb_n => tbzbt_bw2,
--	Bwc_n => tbzbt_bw3,
--	Bwd_n => tbzbt_bw4,	
--	Rw_n  => tbzbt_rw, 	
--	Oe_n =>  tbzbt_oe,	
--	Ce1_n => tbzbt_ce1n,	
--	Ce2	=> tbzbt_ce2,
--	Ce3_n	=> tbzbt_ce3n,
--	Zz  	=>  tbzz 
--	);
-------------------------------Cypress RAM------------------------------
   
   tbddin <='0';	--DDIn;
--   tbadcof <= '0';-- adc overflow
   tblsrdlyd <='0';
--------------------------------------------------
	tb_rst <= '0', '1' after 20 ns , '0' after 50 ns ;
	tbsys_data <= ( others=>'Z');
	
---------------------------------------------------
writing_to_registers : process 

variable pr_data : std_logic_vector ( 15 downto 0 );
variable datacnt : std_logic_vector ( 15 downto 0 );
variable tbsys_rdata : std_logic_vector ( 15 downto 0 );

--------------------------------------------------------------------    
-- Read Procedure --
procedure sys_read ( addrs : in std_logic_vector ( 3 downto 0 );
                     data : out std_logic_vector ( 15 downto 0 )) is  
begin
	tbsys_addr <= addrs ; 
	tsysCE <= '1'; 
	tsysR <='1'; 
	tsysW <='1';
	wait for 5 ns ;
	tsysCE <= '0';	
	wait for 10 ns ;
	tsysR <='0'; 	
	wait for 50 ns ;
	tsysR <='1'; 	
	data := tbsys_data ;		
	wait for 10 ns ;
	tsysCE <= '1';	
	wait for 10 ns ;
	tbsys_data <= ( others=>'Z');
	wait for 10 ns ;
	return ;
end procedure;
-------------------------------------------------------------------- 

-- Read DR_reg  Procedure --
procedure sys_dr_read ( addrs : in std_logic_vector ( 3 downto 0 );
                     data : out std_logic_vector ( 15 downto 0 )) is  
begin
	tbsys_addr <= addrs ;
	tsysCE <= '1';
	tsysR <='1';
	tsysW <='1';
	wait until tbsysclk ='1';
	-- wait for 2.5 ns ;
	tsysCE <= '0';
	wait for 10 ns ;
	tsysR <='0'; 					
	wait for 13 ns ;
	tsysR <='1';
	data := tbsys_data ;				
	wait for 10 ns ;
	tsysCE <= '1';					
	wait for 10 ns ;
	tbsys_data <= ( others=>'Z');					
	wait for 10 ns ;
	return ;
end procedure;
--------------------------------------------------------------------    

   
-- Write Procedure -- 
procedure sys_write( addrs : in std_logic_vector ( 3 downto 0 );
                    data : in std_logic_vector ( 15 downto 0 )) is  
	begin
	tbsys_addr <= addrs ; 
	tbsys_data <= data ; 
	tsysCE <= '1';	
	tsysR <='1'; 
	tsysW <='1'; 
	wait for 5 ns ;
	tsysCE <= '0';		
	wait for 10 ns ;
	tsysW <='0';  		
	wait for 50 ns ;
	tsysW <='1'; 		
	wait for 10 ns ;
	tsysCE <= '1';		
	tbsys_data <= ( others=>'Z');	
	wait for 10 ns ;
	return ;
end procedure;

--------SYSTEM READ/WRITE PROCESS-------------------------------------
begin 
	wait for 80 ns ;
--Setup the registers without starting the Acquisition cycle
	sys_write ( EReg, CONV_LV(20, 16));  -- ER
	sys_write ( RReg, CONV_LV( 5, 16) ); -- put in half-speed mode -- RR (2^x) 000->1 ILV, 1->2 ILV 2->4, 3->8, 4->16
	sys_write ( PReg, CONV_LV( 1, 16) ); -- PR
	sys_write ( AReg, CONV_LV( 2, 16) ); -- AR (2^x)
	sys_write ( CReg, CONV_LV( 0, 16) ); -- set ADC OFF
	wait for 100 ns;
--Read back Register contents
   sys_read ( DReg,tbsys_rdata );
	sys_read ( EReg,tbsys_rdata );
	sys_read ( RReg,tbsys_rdata );
	sys_read ( PReg,tbsys_rdata );
	sys_read ( AReg,tbsys_rdata );
	sys_read ( CReg,tbsys_rdata );
-- Turn on ADC and wait for it to "stabilize".
	sys_write ( CReg, CONV_LV( 256, 16) ); -- Enable the ADC
	wait for 100 ns;				-- warmup time will be in the milliseconds in real life.
--Start Acquisition cycle
	wait until tb_clk='1';
	sys_write ( CReg,"0000000100010001" ); -- Control Register : Start AE cycle , Laser 00 .
	wait for 6170 ns ;--------------------------------------------------------------
	sys_write ( CReg, CONV_LV( 0, 16) ); -- set ADC OFF
	wait for 100 ns;
	sys_write ( PReg, CONV_LV( 4, 16) ); -- Set pulse widthof 20ns
	sys_write ( CReg,"0000000100010001" ); -- Control Register : Start AE cycle , Laser 00 .
	wait for 7170 ns ;
	-------------------------
	sys_write ( CReg, CONV_LV( 0, 16) ); -- set ADC OFF
	wait for 100 ns;
	sys_write ( PReg, CONV_LV( 6, 16) ); -- -- Set pulse widthof 40ns
	sys_write ( CReg,"0000000100010001" ); -- Control Register : Start AE cycle , Laser 00 .
	wait for 8170 ns ;
	-- Reading No of traces accumulated --
	sys_read ( THReg,tbsys_rdata ); -- Higher 16-bit of Trace accumulated reg.
	wait for 20 ns ;
	sys_read ( TLReg,tbsys_rdata ); -- Lower 16-bit of Trace accumulated reg.
	wait for 25 ns ;
	sys_read ( THReg,tbsys_rdata ); -- Higher 16-bit of Trace accumulated reg.
	-------------------------------------------------------------------------------
	
	
	wait for 3160 ns ;---------------------------------------------------------------
	-- Reading No of traces accumulated --
	sys_read ( THReg,tbsys_rdata ); -- Higher 16-bit of Trace accumulated reg.
	wait for 20 ns ;
	sys_read ( TLReg,tbsys_rdata ); -- Lower 16-bit of Trace accumulated reg.
	wait for 25 ns ;
	sys_read ( THReg,tbsys_rdata ); -- Higher 16-bit of Trace accumulated reg.
	-------------------------------------------------------------------------------
	
	
	wait for 834 ns ;---------------------------------------------------------------
	-- Reading No of traces accumulated --
	sys_read ( THReg,tbsys_rdata ); -- Higher 16-bit of Trace accumulated reg.
	wait for 20 ns ;
	sys_read ( TLReg,tbsys_rdata ); -- Lower 16-bit of Trace accumulated reg.
	wait for 25 ns ;
	sys_read ( THReg,tbsys_rdata ); -- Higher 16-bit of Trace accumulated reg.
	-------------------------------------------------------------------------------
	
	
	
	
--Read the status while it is running
	wait for 300 ns;				
	sys_read ( CReg,tbsys_rdata ); -- just to see of the IDLEn and INT bits are ok

--Wait until it is finished
	wait until tsysint'event; -- end of measurement
	sys_read ( CReg,tbsys_rdata ); -- just to see of the IDLEn and INT bits are ok

	wait until tb_clk='1';
	sys_write ( RReg, CONV_LV( 0, 16) ); -- Normal one interleave
	sys_write ( CReg, CONV_LV( 0, 16) ); -- Clear the start bit
	sys_write ( CReg,"0000000001110001" ); -- Run
	


	-- wait until tsysint'event; -- end o measurement

-- Put the ADC to sleep
	sys_write ( CReg, CONV_LV( 256, 16) ); -- ADC OFF
	
	wait for 100 ns;
	
	sys_write ( CReg, CONV_LV( 133, 16) ); --<< Starting Read ZBT and write to Fifo .
	
	
   wait for 150 ns ;
   
	-- Reading Fifo Contends via DR register ------
	
	FOR i in 0 to 110 loop
	wait for 127 ns;
   sys_dr_read ( DReg,tbsys_rdata ); 
   end loop;
	
	
	
	
	-----------------------------------------------
		
	wait for 50000 ns ;
--DONE!
	assert false
	report "End of Simulation"  severity failure;
end process ;
-----------------------------------------------------------------------------------------------------------------
-- Select source of data:
--tbadc_data <= "00" & tbLT2228_d; -- from LT2228 driven by process "adc_drive"
tbadc_data <= tbadc_dat; -- from process "adc_data"
-- tbadc_data <= CONV_LV( 5,14);
-----------------------------------------------------------------------------------------------------------------

-- Produce "ADC" data...
adc_data : process ( tbdlytrg, tbadc_clk, tb_rst)
	variable adcdata : std_logic_vector(tbadc_data'range):=(others=>'0');
begin
	if tb_rst = '1' then
		adcdata := (others=>'0');
	elsif rising_edge(tbadc_clk) then 
   	if tbdlytrg='1' then 
       		adcdata := CONV_LV(32451, 14); -- this may never happen if the output pulse is less than 20 ns (2 Tclk)
   	else
       		adcdata := adcdata + "00000000000001";
  	end if ;
  end if;
  
  tbadc_dat <= transport adcdata after 2.7 ns;   
end process ; 

-- Drive the LT2228...
adc_drive: process ( tblsrdrve, tbadc_clk, tb_rst )
	variable adc_in : real;
	variable t : time;
begin
	if tb_rst = '1' then
		adc_in := 0.0;
	elsif tblsrdrve /= "000" then
		adc_in := 1.05; -- saturation level = 1.0
	elsif rising_edge(tbadc_clk) then 
		adc_in := adc_in * 0.895; -- exponential decay
	end if;

	-- if the end of the fiber has been reached(not exactly), we get no more signal.
	if tblsrdrve'last_event > 0.92 us then -- fiber length
		adc_in := 0.0;
	end if;
	
	tbAinP <= tbVCM + (2.0*adc_in -1.0);
	tbAinN <= tbVCM - (2.0*adc_in -1.0);
end process;

 ---------------------------
clocking_at_10ns : process 
begin 
 wait for Tclk/2;
	tb_clk <= '1';
	tbsysclk <=  '1';
		wait for Tclk/2;
	tb_clk <= '0';
	tbsysclk <=  '0';
end process ;
-------------------------------
--clocking_to_read_fifo : process 
--begin 
--wait for Tclk; 
--tbsysclk <=  '1';
--wait for Tclk; 
--tbsysclk <=  '0';
--end process;
--------------------------------
end testing;  
    
         


