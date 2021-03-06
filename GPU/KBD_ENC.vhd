--------------------------------------------------------------------------------
-- KBD ENC
-- Anders Nilsson
-- 16-feb-2016
-- Version 1.1


-- library declaration
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;            -- basic IEEE library
use IEEE.NUMERIC_STD.ALL;               -- IEEE library for the unsigned type
                                        -- and various arithmetic operations

-- entity
entity KBD_ENC is
  port ( clk	                : in std_logic;			-- system clock (100 MHz)
	 rst		        : in std_logic;			-- reset signal
         PS2KeyboardCLK	        : in std_logic; 		-- USB keyboard PS2 clock
         PS2KeyboardData	: in std_logic;			-- USB keyboard PS2 data
         data			: out std_logic_vector(7 downto 0);		-- tile data
         addr			: out unsigned(10 downto 0);	-- tile address
         we			: out std_logic);		-- write enable
end KBD_ENC;

-- architecture
architecture behavioral of KBD_ENC is
  signal PS2Clk			: std_logic;			-- Synchronized PS2 clock
  signal PS2Data		: std_logic;			-- Synchronized PS2 data
  signal PS2Clk_Q1, PS2Clk_Q2 	: std_logic;			-- PS2 clock one pulse flip flop
  signal PS2Clk_op 		: std_logic;			-- PS2 clock one pulse 
	
  signal PS2Data_sr 		: std_logic_vector(10 downto 0);-- PS2 data shift register
	
  signal PS2BitCounter	        : unsigned(3 downto 0);		-- PS2 bit counter
--My own shieet
  signal BC11			: std_logic;
--Not my own shieet
  signal make_Q			: std_logic;			-- make one pulselse flip flop
  signal make_op		: std_logic;			-- make one pulse

  type state_type is (IDLE, MAKE, BREAK);			-- declare state types for PS2
  signal PS2state : state_type;					-- PS2 state

  signal ScanCode		: std_logic_vector(7 downto 0);	-- scan code
  signal TileIndex		: std_logic_vector(7 downto 0);	-- tile index
  
  type curmov_type is (FORWARD, BACKWARD, NEWLINE);		-- declare cursor movement types
  signal curMovement : curmov_type;				-- cursor movement
	
  signal curposX		: unsigned(5 downto 0);		-- cursor X position
  signal curposY		: unsigned(4 downto 0);		-- cursor Y position
	
  type wr_type is (STANDBY, WRCHAR, WRCUR);			-- declare state types for write cycle
  signal WRstate : wr_type;					-- write cycle state

begin

  -- Synchronize PS2-KBD signals
  process(clk)
  begin
    if rising_edge(clk) then
      PS2Clk <= PS2KeyboardCLK;
      PS2Data <= PS2KeyboardData;
    end if;
  end process;

	
  -- Generate one cycle pulse from PS2 clock, negative edge

  process(clk)
  begin
    if rising_edge(clk) then
      if rst='1' then
        PS2Clk_Q1 <= '1';
        PS2Clk_Q2 <= '0';
      else
        PS2Clk_Q1 <= PS2Clk;
        PS2Clk_Q2 <= not PS2Clk_Q1;
      end if;
    end if;
  end process;
	
  PS2Clk_op <= (not PS2Clk_Q1) and (not PS2Clk_Q2);
	

  
  -- PS2 data shift register

  -- ***********************************
  -- *                                 *
  -- *  VHDL for :                     *
  -- *  PS2_data_shift_reg             *
  -- *                                 *
  -- ***********************************
	process(clk)
	begin
		if rising_edge(clk) then
			if PS2Clk_op = '1' then
				PS2Data_sr <= PS2Data & PS2Data_sr(10 downto 1);
			end if;
		end if;
	end process;
				



  ScanCode <= PS2Data_sr(8 downto 1);
	
  -- PS2 bit counter
  -- The purpose of the PS2 bit counter is to tell the PS2 state machine when to change state

  -- ***********************************
  -- *                                 *
  -- *  VHDL for :                     *
  -- *  PS2_bit_Counter                *
  -- *                                 *
  -- ***********************************
	process(clk)
	begin
		if rising_edge(clk) then
			if BC11 = '1' then
				PS2BitCounter <= to_unsigned(0, 4);
			elsif PS2Clk_op = '1' then 
				PS2BitCounter <= PS2BitCounter + 1;
			end if;
			
		end if;
	end process;
	
	--process(clk)
	--begin
	--	if PS2BitCounter = 11 then
	--		BC11 <= '1';
	--	end if;
	--end process;	

	
	with PS2BitCounter select BC11 <= 
		'1' when "1011",
		'0' when others;

  -- PS2 state
  -- Either MAKE or BREAK state is identified from the scancode
  -- Only single character scan codes are identified
  -- The behavior of multiple character scan codes is undefined

  -- ***********************************
  -- *                                 *
  -- *  VHDL for :                     *
  -- *  PS2_State                      *
  -- *                                 *
  -- ***********************************
	process(clk)
	begin
		if rising_edge(clk) then
			if PS2state = BREAK then
				if BC11 = '1' then
					PS2state <= IDLE;
				end if;
			elsif PS2state = MAKE then
				PS2state <= IDLE;

			elsif PS2state = IDLE then
				if BC11 = '1' and ScanCode = x"F0" then --BREAK
					--ScanCode <= x"FF";
					PS2state <= BREAK;
				elsif BC11 = '1' and not (ScanCode = x"F0") then --MAKE
					PS2state <= MAKE;
				end if;
			end if;
		end if;
	end process;
	

  -- Scan Code -> Tile Index mapping
  with ScanCode select
    TileIndex <= x"00" when x"29",	-- space
                 x"01" when x"1C",	-- A
                 x"02" when x"32",	-- B
								 x"03" when x"21",	-- C
								 x"04" when x"23",	-- D
								 x"05" when x"24",	-- E
								 x"06" when x"2B",	-- F
								 x"07" when x"34",	-- G
								 x"08" when x"33",	-- H
								 x"09" when x"43",	-- I
								 x"0A" when x"3B",	-- J
								 x"0B" when x"42",	-- K
								 x"0C" when x"4B",	-- L
								 x"0D" when x"3A",	-- M
								 x"0E" when x"31",	-- N
								 x"0F" when x"44",	-- O
								 x"10" when x"4D",	-- P
								 x"11" when x"15",	-- Q
								 x"12" when x"2D",	-- R
								 x"13" when x"1B",	-- S
								 x"14" when x"2C",	-- T
								 x"15" when x"3C",	-- U
								 x"16" when x"2A",	-- V
								 x"17" when x"1D",	-- W
								 x"18" when x"22",	-- X
								 x"19" when x"35",	-- Y
								 x"1A" when x"1A",	-- Z
								 x"1B" when x"54",  -- �
								 x"1C" when x"52",  -- �
								 x"1D" when x"4C",  -- �
								 x"FF" when others;
						 
						 
  -- set cursor movement based on scan code
  with ScanCode select
    curMovement <= NEWLINE when x"5A",	        -- enter scancode (5A), so move cursor to next line
                   BACKWARD when x"66",	        -- backspace scancode (66), so move cursor backward
                   FORWARD when others;	        -- for all other scancodes, move cursor forward


  -- curposX
  -- update cursor X position based on current cursor position (curposX and curposY) and cursor
  -- movement (curMovement)

	

  -- curposY
  -- update cursor Y position based on current cursor position (curposX and curposY) and cursor
  -- movement (curMovement)



  -- write state
  -- every write cycle begins with writing the character tile index at the current
  -- cursor position, then moving to the next cursor position and there write the
  -- cursor tile index
  process(clk)
  begin
    if rising_edge(clk) then
      if rst='1' then
        WRstate <= STANDBY;
      else
        case WRstate is
          when STANDBY =>
            if (PS2state = MAKE) then
              WRstate <= WRCHAR;
            else
              WRstate <= STANDBY;
            end if;
          when WRCHAR =>
            WRstate <= WRCUR;
          when WRCUR =>
            WRstate <= STANDBY;
          when others =>
            WRstate <= STANDBY;
        end case;
      end if;
    end if;
  end process;
	

  -- we will be enabled ('1') for two consecutive clock cycles during WRCHAR and WRCUR states
  -- and disabled ('0') otherwise at STANDBY state
  we <= '0' when (WRstate = STANDBY) else '1';


  -- memory address is a composite of curposY and curposX
  -- the "to_unsigned(20, 6)" is needed to generate a correct size of the resulting unsigned vector
  --addr <= to_unsigned(20, 6)*curposY + curposX;
  --addr <= "00000000001" when (PS2state = BREAK);
  
  -- data output is set to be x"1F" (cursor tile index) during WRCUR state, otherwise set as scan code tile index
  --data <= x"FF" when (PS2state = IDLE) else TileIndex;
	data <= TileIndex when (PS2state = MAKE) else x"FF";

  
end behavioral;
