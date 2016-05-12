library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
	
package types_package is

		type pos_t is record		
			x std_logic_vector(9 downto 0);
			y std_logic_vector(8 downto 0);
		end record;
	
		type sprite_t is record
			pos : pos_t;
			spriteType : std_logic := '0';
		end record;
		
		type boardSprites_t is array (0 to 10) of sprite_t;
	
end package	
	

entity CPU is
    Port ( 
        CLK : in  STD_LOGIC;
	keyBoard : in std_logic_vector(15 downto 0);	
	--NEW_FRAME : in std_logic;
	--RST : in  STD_LOGIC;
	--btnu : in std_logic;
	--mem : out std_logic_vector(15 downto 0);
	playerPos : out integer range 121 to 339 := "101010011";
	--boardSprites : out is array (0 to 10) of std_logic_vector(19 downto 0);
    );
	
end entity;
architecture rtl of CPU is

	component lab
		port(
		clk, rst, alive : in std_logic;
		boardSprites : in boardSprites_t;
		);
	end component;




    -- Register
    signal ASR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal PC_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal AR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal HR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal IR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal GR0_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; 
    signal GR1_REG : STD_LOGIC_VECTOR(15 downto 0) := X"00F0"; 
    signal GR2_REG : STD_LOGIC_VECTOR(15 downto 0) := X"00F0";  
    signal GR3_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    
    -- PM/RAM och MyM
    type ram_type is array (0 to 15) of std_logic_vector(15 downto 0); --inte 15, beror på hur stort programmet blir
    type mram_type is array (0 to 36) of std_logic_vector(24 downto 0);

    signal ram : ram_type := (


    constant mram : mram_type := (
--    ALU     TB      FB      S     P     LC     SEQ       myADR      
    "0000" & "011" & "111" & "0" & "0" & "00" & "0000" & "0000000", --0x00 PC => ASR, mpc++,  			(HÄMTFAS)
    "0000" & "010" & "001" & "0" & "0" & "00" & "0000" & "0000000", --0x01 PM => IR, mpc++ 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0010" & "0000000", --0x02 K1 => mpc
    "0000" & "001" & "111" & "0" & "0" & "00" & "0001" & "0000000", --0x03 IR => ASR, K2 => mpc
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x04
    "0000" & "010" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x05 PM => GRx, mpc = 0,  	        (LOAD)
    "0000" & "110" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x06 GRx => PM, mpc = 0,                  (STORE)
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x07 Grx => AR, mpc++,                    (ADD)
    "1000" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x08 (AR + PM) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x09 AR => GRx, PC++, mpc = 0
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0A Grx => AR, mpc++,                    (SUB)
    "0101" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0B (AR - PM) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x0C AR => GRx, PC++, mpc = 0
    "0001" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0D PM => AR, mpc++,                     (LSR)
    "1110" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0E (AR LSR 1) => AR , mpc++
    "0000" & "100" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x0F AR => PM, PC++, mpc = 0
--    ALU     TB      FB      S     P     LC     SEQ       myADR      
    "0000" & "010" & "011" & "0" & "0" & "00" & "0011" & "0000000", --0x10 PM => PC, mpc = 0                    (JMP)
    "0000" & "001" & "111" & "0" & "0" & "00" & "1011" & "0010000", --0x11 IF flagga = 1 then mpc++ else mpc =>myADR(RESET)
    "0100" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x12 ALU RESET MAGIC, PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x13 
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x14 GRx => AR, mpc++                     (AND)
    "0010" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x15 ALU magic, mp++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x16 AR => GRx, PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0011" & "0000000", --0x17 mpc = 0                              (NOP)
    "0000" & "001" & "111" & "0" & "0" & "00" & "1000" & "0010000", --0x18 IF flagga = 1 then mpc++ else mpc =>myADR (BNE)
    "0000" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x19 PC++, mpc = 0

--    ALU     TB      FB      S     P    LC      SEQ       myADR       
    "1111" & "010" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1A BTST PM, PC++, mpc = 0               (BTST)
    "0000" & "001" & "111" & "0" & "0" & "00" & "1001" & "0010000", --0x1B IF z = 1 then mpc++ else mpc=>myADR  (BNE)
    "0000" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1C PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x1D 
    "1101" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x1E Alu magic => AR, mpc ++
    "0000" & "100" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x1F AR => PM, PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x20 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x21 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x22 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x23
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x24
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x25 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000"  --0x26
    );
    
    -- K1 och K2
    type k1_type is array(0 to 10) of std_logic_vector(7 downto 0);
    type k2_type is array(0 to 3) of std_logic_vector(7 downto 0);

    constant k1 : k1_type := (
    X"00", -- NOP    0x00
    X"05", -- LOAD   0x01
    X"06", -- STORE  0x02   
    X"07", -- ADD    0x03
    X"0A", -- SUB    0x04
    X"0D", -- LSR    0x05
    X"10", -- JMP    0x06
    X"11", -- RST    0x07
    X"14", -- AND    0x08
    X"1A", -- BTST   0x09
    X"1B", -- BNE    0x0A
    );
    constant k2 : k2_type := (
    X"03",
    X"00",
    X"00",
    X"00"
    );

----------banor----------------------------c-p-d-w-----------------
	
	--bana1
	type bana1_type is array(0 to 1) of std_logic_vector(17 downto 0);
	
	constant bana1 : bana1_type := (
--	ypos	     tid       spritetyp		
	"1010 1001 1 0000 0000 0",
	"1001 0101 1 1111 0000 0",		
	);

-- ypos        	 tid	   spritetyp
-- 0000 0000 0 - 0000 0000 0

--------------------------------------------------------------------

-----------board-sprites-----------------c-p-d-w--------------------
--ar det okej att ha en tom array??--

	process(CLK) begin
		if rising_edge(CLK) then			
			if bana1(i)(8 downto 1) = CLK
				for i in boardSprites(9 downto 0) loop
					boardSprites(i) <= boardSprites(i+1)
				end loop;	
--							    xpos(10bitar)  ypos(9bitar)		spritetyp
				boardSprites(10) = "1001 1111 11" + bana1(i)(17 downto 9) + bana1(i)(0 downto 0);		
			end if;	 	
		end if;
	end process;

	process(CLK) begin
		if rising_edge(CLK) then
			for i in boardSprites loop
				if boardSprites(19 downto 10) = (-)"0000 0101 00" then -- (-20) 
					boardSprites(i) = "0000 0000 0000 0000 0000";
				end if;
			end loop;
		end if;
	end process;

---------------------------------------------------------------------


------------------------------SPRITES--------------------------------
	signal color_type is array (0 to 7) of std_logic_vector(7 downto 0);
	signal colors : color_type := (
	"000 000 00", --black	000
	"111 111 11", --white	001
	"110 000 00", --red	010
	"000 111 00", --green	011
	"000 000 11", --blue	100
	);



    -- Interna signaler
    signal buss : std_logic_vector(15 downto 0) := X"0000";
    signal mux : std_logic_vector(1 downto 0) := "00"; --Mux för de 4 GR register
    signal current_GR : std_logic_vector(15 downto 0);
    
    signal myPC : STD_LOGIC_VECTOR(7 downto 0) := X"00";
    signal myM : std_logic_vector(24 downto 0);
    -- Mym operatorer
    signal ALU_OP : std_logic_vector(3 downto 0) := "0000"; --Bestammer operator i ALU
    signal TB : std_logic_vector(2 downto 0) := "000";
    signal FB : std_logic_vector(2 downto 0) := "000";
    signal S : std_logic;
    signal P : std_logic_vector(1 downto 0);
    signal LC : std_logic;
    signal SEQ : std_logic_vector(3 downto 0);
    signal myADR : std_logic_vector(6 downto 0) := "0000000";

  

    
begin

    -- ----------------------------------------
    -- # ASR Register
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
	    if (FB = "111") then
                ASR_REG(15 downto 0) <= buss(15 downto 0);
            else
                ASR_REG(15 downto 0) <= ASR_REG(15 downto 0);
            end if;
        end if;
    end process;
    -- ----------------------------------------
    -- # PM
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
            if FB="010" then
                ram(conv_integer(ASR_REG(7 downto 0))) <= buss(15 downto 0);
            else 
                ram <= ram;
            end if;
        end if;
    end process;
    -- ----------------------------------------
    -- # IR Register
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
	    if FB="001" then
                IR_REG(15 downto 0) <= buss(15 downto 0);
            else
                IR_REG(15 downto 0) <= IR_REG(15 downto 0);
            end if;
        end if;
    end process;
    -- ----------------------------------------
    -- # PC Register
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
	       if FB="011" then
                PC_REG(15 downto 0) <= buss(15 downto 0);
            elsif (P = "01") then
                PC_REG <= PC_REG + 1;
            elsif (P="11") then
                PC_REG <= ASR_REG;
            else
                PC_REG(15 downto 0) <= PC_REG(15 downto 0);
            end if;
        end if;
    end process;    
    -- ----------------------------------------
    -- # HR Register
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
	    if FB="101" then
                HR_REG(15 downto 0) <= buss(15 downto 0);
            else
                HR_REG(15 downto 0) <= HR_REG(15 downto 0);
            end if;
        end if;
    end process;
    -- ----------------------------------------
    -- # MUX1 Register
    -- ----------------------------------------
    mux1 <= IR_REG(11 downto 10);
    process(CLK) begin
        if rising_edge(CLK) then
	    if FB="110" then
                case mux1 is
                    when "00" => GR0_REG(15 downto 0) <= buss(15 downto 0);
                    when "01" => GR1_REG(15 downto 0) <= buss(15 downto 0);
                    when "10" => GR2_REG(15 downto 0) <= buss(15 downto 0);
                    when "11" => GR3_REG(15 downto 0) <= buss(15 downto 0);
                    when others => null;
                end case;
            end if;
        end if;
    end process;
    
    -- ----------------------------------------
    -- # BUSSEN MUX
    -- ----------------------------------------
    with TB select
    buss <= 
        ASR_REG when "111",
        IR_REG when "001",
        ram(conv_integer(ASR_REG(7 downto 0))) when "010",
        PC_REG when "011",
        AR_REG when "100",
        HR_REG when "101",
        current_GR when "110",
        "000" & mram(conv_integer(MPC))(12 downto 0) when "000",
        (others => '0') when others;
    
    with mux1 select
        current_GR <=
            GR0_REG when "00",
            GR1_REG when "01",
            GR2_REG when "10",
            GR3_REG when "11",
        (others => '0') when others;    

    -- ----------------------------------------
    -- # Mikro
    -- ----------------------------------------
    myM <= mram(conv_integer(MPC(5 downto 0)));
    myADR <= myM(6 downto 0);
    SEQ <= myM(10 downto 7);
    LC <= myM(11);
    P <= myM(13 downto 12);
    S <= myM(14);
    FB <= myM(17 downto 15);
    TB <= myM(20 downto 18);
    ALU_OP <= myM(24 downto 21);
    process(CLK) begin
        if rising_edge(CLK) then
            case SEQ is
                when "0000" => MPC <= MPC + 1;
                when "0001" => MPC <= k1(conv_integer(IR_REG(15 downto 12)));
                when "0010" => MPC <= k2(conv_integer(IR_REG(9 downto 8)));
                when "0011" => MPC <= '0' & myADR;
                when others => MPC <= MPC;
            end case;
        end if;
    end process;
    -- ----------------------------------------
    -- # ALU
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
            case ALU_OP is
                when "0000" => null;
                when "0100" => AR_REG(15 downto 0) <= AR_REG(15 downto 0) + buss(15 downto 0); -- ADD
                when "0101" => AR_REG(15 downto 0) <= AR_REG(15 downto 0) - buss(15 downto 0); -- SUB
                when "0001" => AR_REG(15 downto 0) <= buss(15 downto 0); -- LOAD
		when "0011" => AR_REG(15 downto 0) <= X"0000"; -- RESET
		when "1110" => -- något
		when "1111" => -- något
		when "0110" => --outPos1 <= GR1_REG(9 downto 0) & GR2_REG (9 downto 0);
                when others => null;
            end case;
        end if;
    end process;

end rtl;
