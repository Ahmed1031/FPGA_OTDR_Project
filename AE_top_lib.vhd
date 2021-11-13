
   Library ieee,work; 
   Use ieee.std_logic_1164.all;
   Use IEEE.Std_Logic_Arith.all;
   Use IEEE.std_logic_TextIO.all;


Package AE_top_lib is 
    
    
    -- Register addresses:
	constant DReg			: std_logic_vector ( 3 downto 0 ):="0000"; -- Data Read
	constant EReg			: std_logic_vector ( 3 downto 0 ):="0001"; -- End
	constant RReg			: std_logic_vector ( 3 downto 0 ):="0010"; -- Resolution
	constant PReg			: std_logic_vector ( 3 downto 0 ):="0011"; -- Pulse Width
	constant AReg			: std_logic_vector ( 3 downto 0 ):="0100"; -- Averages
	constant TLReg			: std_logic_vector ( 3 downto 0 ):="0101"; -- Traces Accumulated ? low word (TLR):
	constant THReg			: std_logic_vector ( 3 downto 0 ):="0110"; -- Traces Accumulated ? high word (THR):
	constant CReg			: std_logic_vector ( 3 downto 0 ):="0111"; -- Control/Status
-------------------------------------------------------------------------------------------------------

	
	signal FLASH_data : std_logic_vector(15 DOWNTO 0);
	signal FLASH_addr : std_logic_vector(25 DOWNTO 1);
	signal FPGA_CLKIN0_35p528MHz : std_logic;
	signal FPGA_CLKIN5_35p528MHzz :std_logic;
	signal FLASH_CLK		: std_logic;
	signal FLASH_nCS1A	: std_logic;
	signal FLASH_nCS1b : std_logic;
	signal FLASH_nCS2A : std_logic;
	signal FLASH_nADV : std_logic;
	signal FLASH_nOE : std_logic;
	signal FLASH_nWE : std_logic;
	signal UART1_RTS : std_logic;
	signal UART1_TX : std_logic;
	signal MCBSP1_DX : std_logic;
	signal MCBSP1_FSX : std_logic;
	signal MCBSP1_CLKX : std_logic;
	signal MCBSP3_DX : std_logic;
	signal MCBSP3_FSX : std_logic;
	signal OMAP_FPGA_INT : std_logic;
	--
	signal io_data : std_logic_vector(15 downto 0);

	--
	signal clk: std_logic;
	signal DSP_INT	: std_logic;
	signal sys_rst: std_logic;
	signal DDin	: std_logic;

	signal adc_data : std_logic_vector(13 downto 0);
	signal adc_clk	: std_logic;
	signal adc_shdn	: std_logic;
	signal adc_OEn	: std_logic;
	signal adc_OF	: std_logic;

	signal dly_out	: std_logic_vector(7 downto 0);
	signal lsr_drive: std_logic_vector(2 downto 0);
	signal dly_trg	: std_logic;
	signal dly_ae	: std_logic;
	signal lsr_delayed	: std_logic;

	signal ZBT_A	: std_logic_vector(19 downto 0);
	signal ZBT_IOP	: std_logic_vector(4 downto 1);
	signal ZBT_IO	: std_logic_vector(31 downto 0);

	signal	ZBT_WEn,	 ZBT_CENn,	ZBT_ADV,
		ZBT_BW4n,	ZBT_BW3n,	ZBT_BW2n,
		ZBT_BW1n,	ZBT_CE1n,	ZBT_CE2,
		ZBT_CE2n,	ZBT_CLK,	 ZBT_MODE,
		ZBT_OEn,	 ZBT_ZZ			: std_logic;

	signal tbadc_dat	   : std_logic_vector ( 13 downto 0 );
	signal LTC2228_data	: std_logic_vector ( 11 downto 0 );

	signal TC_CLK	: std_logic;	
	signal REF_CLK	: std_logic ;
	signal wr_data	: std_logic_vector (15 downto 0) ;
	signal rd_data	: std_logic_vector (31 downto 0):=(others=>'L') ;
	signal addr_in	: std_logic_vector (25 downto 1) :=(others=>'0') ;
	signal op		:integer:=0; -- '0'--write, '1' --read16, 2--read32
	signal busy		: std_logic:='0';
	signal start	: std_logic:='0';
	signal DSP_CSn	: std_logic:='1';
	signal DSP_OEn	: std_logic:='1';
	signal DSP_WEn	: std_logic:='1';
	signal DSP_addr:std_logic_vector (25 downto 1) :=(others=>'0') ;
	signal DSP_data:std_logic_vector (15 downto 0):=(others=>'Z');

-------------------------------------------------------


         
     component LTC2228 is
generic(
   ADC_bits : natural:=12;
   pipe_len	: natural:=5;
   tCO      : time:=2.7 ns;
   tOEn   : time:= 4.3 ns

   );
port(

    VDD    : in  real:=3.300;
    VCM    : out real:=1.500;-- (1.475, 1525)
    
    AinP   : in  real:=1.55;
    AinN   : in  real:=1.55;
    RefH   : out real:= 0.55;
    RefL   : out real:=-0.55;
    
    Sense  : in  real:=3.3; -- Sense=VCM        ->(Ref=Internal, Range=+/-0.5V)
                      -- Sense=VDD        ->(Ref=Internal, Range=+/-1.0V)
                      -- Sense=(0.5, 1.0) ->(Ref=Sense, Range=+/-Sense)
    
    MODE   : in  real:=0.0; -- (0/3)VDD --> Offset binary, clock duty-cycle stabilizer=OFF
                      -- (1/3)VDD --> Offset binary, clock duty-cycle stabilizer=ON
                      -- (2/3)VDD --> 2's complement, clock duty-cycle stabilizer=ON
                      -- (3/3)VDD --> 2's complement, clock duty-cycle stabilizer=OFF
    
    Clk    : in  std_logic;
    SHDN   : in  std_logic;
    OEn    : in  std_logic;
    OVF     : out std_logic;
    D      : out std_logic_vector(11 downto 0) -- need a generic here
    );
    
end component;


-- ZBT Sram Model --------------------------------------------------------------------------------------
component cy7c1350 is 
    GENERIC (
        -- Constant parameters
	addr_bits : INTEGER;
	data_bits : INTEGER;
        -- Timing parameters:
        tCYC	: TIME; tCH	: TIME; tCL	: TIME; tCO	: TIME; tAS	: TIME; tCENS	: TIME; tWES	: TIME; tDS	: TIME; tAH	: TIME; tCENH	: TIME; tWEH	: TIME; tDH	: TIME
	);

 PORT (
	Dq	: INOUT STD_LOGIC_VECTOR ((data_bits - 1) DOWNTO 0);   	-- Data I/O
	Addr	: IN	STD_LOGIC_VECTOR ((addr_bits - 1) DOWNTO 0);   	-- Address
	Mode	: IN	STD_LOGIC; 						-- Burst Mode
	Clk	: IN	STD_LOGIC;                                   -- Clk
	CEN_n	: IN	STD_LOGIC;                                   -- CEN#
	AdvLd_n	: IN	STD_LOGIC;                                   -- Adv/Ld#
	Bwa_n	: IN	STD_LOGIC;                                   -- Bwa#
	Bwb_n	: IN	STD_LOGIC;                                   -- BWb#
	Bwc_n	: IN	STD_LOGIC;                                   -- Bwc#
	Bwd_n	: IN	STD_LOGIC;                                   -- BWd#
	Rw_n	: IN	STD_LOGIC;                                   -- RW#
	Oe_n	: IN	STD_LOGIC;                                   -- OE#
	Ce1_n	: IN	STD_LOGIC;                                   -- CE1#
	Ce2	: IN	STD_LOGIC;                                   -- CE2
	Ce3_n	: IN	STD_LOGIC;                                   -- CE3#
	Zz	: IN	STD_LOGIC                                  -- Snooze Mode
	    );
   END component ; 
----------------------------------------------------------------------------------------------------------------

   
component	CORE_FPGA_535XT IS
    PORT (
	Bank5_J18 : INOUT std_logic;
	Bank5_L17 : INOUT std_logic;
	Bank8_Y10 : INOUT std_logic;
	FPGA_CLKIN0_35p528MHz : IN std_logic;
	FPGA_CLKIN4_100MHz : IN std_logic;
	FPGA_CLKIN5_35p528MHzz : IN std_logic;
	FPGA_CLKIN8_100MHz : IN std_logic;
	PLL1_V4 : OUT std_logic;
	PLL1_U4 : OUT std_logic;
	PLL2_OUTn : OUT std_logic;
	PLL2_OUTp : OUT std_logic;
	PLL3_OUTn : OUT std_logic;
	PLL3_OUTp : OUT std_logic;
	PLL4_U18 : OUT std_logic;
	PLL4_T18 : OUT std_logic;
	ram_dataP4 : INOUT std_logic;
	ram_dataP3 : INOUT std_logic;
	ram_dataP2 : INOUT std_logic;
	ram_dataP1 : INOUT std_logic;
	ram_data : INOUT std_logic_vector(31 DOWNTO 0);
	ram_addr : OUT std_logic_vector(19 DOWNTO 0);
	ram_nLBO : OUT std_logic;
	ram_nOE : OUT std_logic;
	ram_RW : OUT std_logic;
	ram_ZZ : OUT std_logic;
	ram_ADV : OUT std_logic;
	ram_CLK : OUT std_logic;
	ram_nBW4 : OUT std_logic;
	ram_nBW3 : OUT std_logic;
	ram_nBW2 : OUT std_logic;
	ram_CE2 : OUT std_logic;
	ram_nCE2 : OUT std_logic;
	ram_nCE1 : OUT std_logic;
	ram_nCEN : OUT std_logic;
	ram_nBW1 : OUT std_logic;
	dac_CLK : OUT std_logic;
	dac_SLEEP : OUT std_logic;
	dac_data : OUT std_logic_vector(11 DOWNTO 0);
	adc_OF : IN std_logic;
	adc_SHDN : OUT std_logic;
	adc_nOE : OUT std_logic;
	adc_CLK : OUT std_logic;
	adc_data : IN std_logic_vector(13 DOWNTO 0);
	FLASH_RDY : OUT std_logic;
	FLASH_CLK : IN std_logic;
	FLASH_nCS1A : IN std_logic;
	FLASH_nCS1b : IN std_logic;
	FLASH_nCS2A : IN std_logic;
	FLASH_nOE : IN std_logic;
	FLASH_nWE : IN std_logic;
	FLASH_nADV : IN std_logic;
	FLASH_addr : IN std_logic_vector(25 DOWNTO 1);
	FLASH_data : INOUT std_logic_vector(15 DOWNTO 0);
	UART1_RTS : INOUT std_logic;
	UART1_CTS : INOUT std_logic;
	UART1_RX : OUT std_logic;
	UART1_TX : IN std_logic;
	MCBSP1_DR : INOUT std_logic;
	MCBSP1_DX : INOUT std_logic;
	MCBSP1_FSX : INOUT std_logic;
	MCBSP1_CLKS : INOUT std_logic;
	MCBSP1_CLKX : INOUT std_logic;
	MCBSP3_DR : INOUT std_logic;
	MCBSP3_DX : INOUT std_logic;
	MCBSP3_FSX : INOUT std_logic;
	MCBSP3_CLKR : OUT std_logic;
	MCBSP2_CLKR : OUT std_logic;
	OMAP_FPGA_INT : INOUT std_logic;
	io_data : INOUT std_logic_vector(15 DOWNTO 0);
	Bank1_P3 : INOUT std_logic;
	Bank1_U3 : INOUT std_logic;
	Bank2_F4 : INOUT std_logic;
	Bank2_H3 : INOUT std_logic;
	Bank3_A3 : INOUT std_logic;
	Bank3_A4 : INOUT std_logic;
	Bank3_A5 : INOUT std_logic;
	Bank3_A6 : INOUT std_logic;
	Bank3_A7 : INOUT std_logic;
	Bank3_A8 : INOUT std_logic;
	Bank3_A9 : INOUT std_logic;
	Bank3_A10 : INOUT std_logic;
	Bank3_A11 : INOUT std_logic;
	Bank3_B4 : INOUT std_logic;
	Bank3_B5 : INOUT std_logic;
	Bank3_B6 : INOUT std_logic;
	Bank3_B7 : INOUT std_logic;
	Bank3_B8 : INOUT std_logic;
	Bank3_B9 : INOUT std_logic;
	Bank3_B10 : INOUT std_logic;
	Bank3_B11 : INOUT std_logic;
	Bank3_C9 : INOUT std_logic;
	Bank3_D7 : INOUT std_logic;
	Bank3_D8 : INOUT std_logic;
	Bank3_D9 : INOUT std_logic;
	Bank3_D11 : INOUT std_logic;
	Bank3_E7 : INOUT std_logic;
	Bank3_E8 : INOUT std_logic;
	Bank3_E9 : INOUT std_logic;
	Bank3_E11 : INOUT std_logic;
	Bank3_F8 : INOUT std_logic;
	Bank3_F9 : INOUT std_logic;
	Bank3_F10 : INOUT std_logic;
	Bank3_F11 : INOUT std_logic;
	Bank3_G7 : INOUT std_logic;
	Bank3_G11 : INOUT std_logic;
	Bank3_H7 : INOUT std_logic;
	Bank3_H11 : INOUT std_logic;
	Bank3_C10 : INOUT std_logic;
	Bank3_C7 : INOUT std_logic;
	Bank4_B19 : INOUT std_logic;
	Bank4_B20 : INOUT std_logic;
	BrdRESET : OUT std_logic;
	Bank4_C17 : INOUT std_logic;
	Bank4_C18 : INOUT std_logic;
	Bank4_D14 : INOUT std_logic;
	Bank4_D15 : INOUT std_logic;
	Bank4_D16 : INOUT std_logic;
	Bank4_E14 : INOUT std_logic;
	Bank4_E15 : INOUT std_logic;
	Bank4_C16 : INOUT std_logic;
	Bank4_C13 : INOUT std_logic;
	Bank5_C19 : INOUT std_logic;
	Bank5_D19 : INOUT std_logic;
	Bank5_D20 : INOUT std_logic;
	Bank5_E20 : INOUT std_logic;
	Bank5_H16 : INOUT std_logic;
	Bank5_J15 : INOUT std_logic;
	Bank5_J17 : INOUT std_logic;
	Bank5_K18 : INOUT std_logic;
	Bank5_L18 : INOUT std_logic;
	Bank5_L19 : INOUT std_logic;
	Bank5_G20 : INOUT std_logic;
	Bank5_K20 : INOUT std_logic;
	Bank6_M15 : INOUT std_logic;
	Bank6_M16 : INOUT std_logic;
	Bank6_M18 : INOUT std_logic;
	Bank6_M19 : INOUT std_logic;
	Bank6_N15 : INOUT std_logic;
	Bank6_N21 : INOUT std_logic;
	Bank6_N22 : INOUT std_logic;
	Bank6_P15 : INOUT std_logic;
	Bank6_P17 : INOUT std_logic;
	Bank6_P18 : INOUT std_logic;
	Bank6_P19 : INOUT std_logic;
	Bank6_P20 : INOUT std_logic;
	Bank6_P21 : INOUT std_logic;
	Bank6_P22 : INOUT std_logic;
	Bank6_R18 : INOUT std_logic;
	Bank6_R19 : INOUT std_logic;
	Bank6_R21 : INOUT std_logic;
	Bank6_R22 : INOUT std_logic;
	Bank6_T21 : INOUT std_logic;
	Bank6_T22 : INOUT std_logic;
	Bank6_R17 : INOUT std_logic;
	Bank6_U19 : INOUT std_logic;
	Bank6_U21 : INOUT std_logic;
	Bank6_U22 : INOUT std_logic;
	Bank6_V20 : INOUT std_logic;
	Bank6_V21 : INOUT std_logic;
	Bank6_V22 : INOUT std_logic;
	Bank6_W18 : INOUT std_logic;
	Bank6_W21 : INOUT std_logic;
	Bank6_W22 : INOUT std_logic;
	Bank6_Y18 : INOUT std_logic;
	Bank6_Y19 : INOUT std_logic;
	Bank6_Y20 : INOUT std_logic;
	Bank6_Y21 : INOUT std_logic;
	Bank6_Y22 : INOUT std_logic;
	Bank6_R20 : INOUT std_logic;
	Bank6_U20 : INOUT std_logic;
	Bank7_Y16 : INOUT std_logic;
	Bank7_Y13 : INOUT std_logic;
	GPIO1 : INOUT std_logic;
	Bank8_Y9 : INOUT std_logic;
	Bank8_Y7 : INOUT std_logic
	);
END component;
component dsp_emifs is
generic (
	FCLKDIV	: integer range 0 to 3  :=0;
	RDWST	: integer range 0 to 15 :=0;			--Read Wait States (CSn width).
	BTWST	: integer range 0 to 15 :=0;			--Read Wait States, CSn = high, between cycles
	OESETUP	: integer range 0 to 15 :=0;			-- OE activation delay time from CS and address valid.
	OEHOLD	: integer range 0 to 15 :=0;			-- OE deactivation advance time to CS and address invalid.
	WRWST	: integer range 0 to 15 :=0;			-- Setup time CSn and address to WE going low.
	WELEN	: integer range 0 to 15 :=0			-- Write enable length in REF_CLK cycles.
	) ;
port (
	-- control
	TC_CLK	:   in std_logic :='0';
	REF_CLK	:  out std_logic :='0';
	wr_data	:   in std_logic_vector (15 downto 0)  :=(others=>'0') ;
	rd_data	:  out std_logic_vector (31 downto 0) ;
	addr_in	:   in std_logic_vector (25 downto 1) :=(others=>'0') ;
	op	:   in integer:=0; -- '0'--write, '1' --read16, 2--read32
	start	:   in std_logic:='0';
	busy	:  out std_logic:='0';
	-- bus ports
	CSn	:  out std_logic:='1';
	OEn	:  out std_logic:='1';
	WEn	:  out std_logic:='1';
	ADDR	:  out std_logic_vector (25 downto 1) :=(others=>'0') ;
	data	:inout std_logic_vector (15 downto 0):=(others=>'Z') 
	);
end component; --fs   
   
   
 function CONV_LV (S : INTEGER; N:NATURAL ) return std_logic_vector ;
 
 function conv_int (X:std_logic_vector) return integer ;
 
 -- EMIFS Write Procedure ---------------------------------------------------
 procedure EMIFS_write( addri : in std_logic_vector ( 3 downto 0 ); 
                       wrdata : in std_logic_vector ( 15 downto 0 ); 
                       signal op : out integer ;
                       signal wr_data : out std_logic_vector ( 15 downto 0 );
                       signal addr_in	: out std_logic_vector (25 downto 1);
                       signal busy		: in std_logic ; 
                       signal start		: out std_logic ) ;
                       
                       
-- EMIFS Read Procedure -------------------------------------------- 
procedure EMIFS_reed( addri : in std_logic_vector ( 3 downto 0 ); 
                       signal op : out integer ;
                       signal addr_in	: out std_logic_vector (25 downto 1);
                       signal busy		: in std_logic ; 
                       signal start		: out std_logic );

-------------------------------------------------------------------
                       
                       
                        
 end package ;
 ----------------------------------------------------------------------------
 Package body  AE_top_lib is  
     
         
     function CONV_LV (S : INTEGER; N:NATURAL ) return std_logic_vector is
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
 
 -- Function to Convert Std_vector - to o integer 
 
 Function conv_int (X:std_logic_vector) return integer is 
variable result :integer; 
begin 
result:= 0; 
For i in X' range loop 
result:=result*2; 
       case x(i) is 
                 when '0'|'L' => null; 
                 when '1'|'H' => result:= result+1; 
                 when others =>null; 
         end case; 
      end loop; 
   return result; 
end conv_int; 


---- EMIFS Write Procedure ------------------------------------------- 
procedure EMIFS_write( addri : in std_logic_vector ( 3 downto 0 ); 
                       wrdata : in std_logic_vector ( 15 downto 0 ); 
                       signal op : out integer ;
                       signal wr_data : out std_logic_vector ( 15 downto 0 );
                       signal addr_in	: out std_logic_vector (25 downto 1);
                       signal busy		: in std_logic ; 
                       signal start		: out std_logic ) is  
	begin
	op <= 0;  
	wr_data	<= wrdata;
	addr_in(4 downto 1) <= addri ;
	start <= '1' ,  '0' after 5 ns;	
	wait on busy	until busy= '0'; 
	  
return ;
end procedure;
--------------------------------------------------------------------


-- EMIFS Read Procedure -------------------------------------------- 
procedure EMIFS_reed( addri : in std_logic_vector ( 3 downto 0 ); 
                       signal op : out integer ;
                       signal addr_in	: out std_logic_vector (25 downto 1);
                       signal busy		: in std_logic ; 
                       signal start		: out std_logic ) is   
	begin
	op <= 1;  
	addr_in(4 downto 1) <= addri ;
	start <= '1' ,  '0' after 5 ns;	
	wait on busy	until busy= '0'; 
	  
return ;
end procedure;
-------------------------------------------------------------------
 
 end AE_top_lib;