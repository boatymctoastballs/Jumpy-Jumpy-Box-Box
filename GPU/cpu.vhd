library IEEE;
library ieee_proposed;
  use ieee_proposed.fixed_pkg.all;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity CPU is
    Port ( 
    CLK : in  STD_LOGIC;
    RST : in  STD_LOGIC;
    playerPosY : out std_logic_vector(15 downto 0)
    );
end entity;
architecture rtl of CPU is
    -- Register
    signal ASR_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal PC_REG  : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal AR_REG  : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal HR_REG  : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal IR_REG  : STD_LOGIC_VECTOR(15 downto 0) := X"0000";
    signal GR0_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; -- Player1 HÄLSA
    signal GR1_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; -- PLayer2 HÄLSA 
    signal GR2_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; -- MAP COUNTER
    signal GR3_REG : STD_LOGIC_VECTOR(15 downto 0) := X"0000"; -- MAP
    signal flag_newframe : STD_LOGIC := '0';
    signal z_flag : STD_LOGIC := '0';
    
    -- PM/RAM och MyM
    type ram_type is array (0 to 67) of std_logic_vector(15 downto 0);
    type mram_type is array (0 to 45) of std_logic_vector(24 downto 0);
    


    signal ram : ram_type := (
    -- Programkod
    -- 0001 0000 0001 1010
    X"101C", --00 
    X"0000", --01 
    X"0000", --02 
    X"0000", --03 
    X"0000", --04 
    X"0000", --05 
    X"0000", --06 
    X"0000", --07 
    X"0000", --08 
    X"0000", --09 
    X"0000", --0A 
    X"0000", --0B 
    X"0000", --0C 
    X"0000", --0D 
    X"0000", --0E 
    X"0000", --0F 
    X"0000", --10 
    X"0000", --11 
    X"0000", --12 
    X"0000", --13 
    X"0000", --14 
    X"0000", --15 
    X"0000", --16 
    X"0000", --17 
    X"0000", --18 
    X"0000", --19 
    X"0000", --1A 
    X"0000", --1B 
    X"ABBA", --1C 
    X"0000", --1D 
    X"0000", --1E 
    X"0000", --1F 
    X"0000", --20 
    X"0000", --21 
    X"0000", --22 
    X"0000", --23 
    X"0000", --24 
    X"0000", --25 
    X"0000", --26 
    X"0000", --27 
    X"0000", --28 
    X"0000", --29 
    X"0000", --2A 
    X"0000", --2B 
    X"0000", --2C 
    X"0000", --2D 
    X"0000", --2E 
    X"0000", --2F 
    X"0000", --30 
    X"0000", --31 
    X"0000", --32 
    X"0000", --33
    X"0000", --34
    X"0000", --35
    X"0000", --36
    X"0000", --37
    X"0000", --38
    X"0000", --39
    X"0000", --3A
    X"0000", --3B
    X"0000", --3C
    X"0000", --3D
    X"0000", --3E
    X"0000", --3F
    X"0000", --40
    X"0000", --41
    X"0000", --42
    X"0000"  --43 NOLLA
    );


    constant mram : mram_type := (
--    ALU     TB      FB      S     P     LC     SEQ       myADR      
    "0000" & "011" & "111" & "0" & "0" & "00" & "0000" & "0000000", --0x00 PC => ASR, mpc++,                        (HÄMTFAS)
    "0000" & "010" & "001" & "0" & "0" & "00" & "0000" & "0000000", --0x01 PM => IR, mpc++ 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0010" & "0000000", --0x02 K1 => mpc
    "0000" & "001" & "111" & "0" & "0" & "00" & "0001" & "0000000", --0x03 IR => ASR, K2 => mpc
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x04
    "0000" & "010" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x05 PM => GRx, mpc = 0,                      (LOAD)
    "0000" & "110" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x06 GRx => PM, mpc = 0,                      (STORE)
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x07 Grx => AR, mpc++,                        (ADD)
    "1000" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x08 (AR + PM) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x09 AR => GRx, PC++, mpc = 0
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0A Grx => AR, mpc++,                        (SUB)
    "0101" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0B (AR - PM) => AR, mpc++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x0C AR => GRx, PC++, mpc = 0
    "0001" & "010" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0D PM => AR, mpc++,                        (LSR)
    "1110" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x0E (AR LSR 1) => AR , mpc++
    "0000" & "100" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x0F AR => PM, PC++, mpc = 0
--    ALU     TB      FB      S     P     LC     SEQ       myADR      
    "0000" & "010" & "011" & "0" & "0" & "00" & "0011" & "0000000", --0x10 PM => PC, mpc = 0                        (JMP)
    "0000" & "001" & "111" & "0" & "0" & "00" & "1011" & "0010000", --0x11 IF flagga = 1 then mpc++ else mpc =>myADR(RESET)
    "0100" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x12 ALU RESET MAGIC, PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x13 
    "0001" & "110" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x14 GRx => AR, mpc++                         (AND)
    "0010" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x15 ALU magic, mp++
    "0000" & "100" & "110" & "0" & "1" & "00" & "0011" & "0000000", --0x16 AR => GRx, PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0011" & "0000000", --0x17 mpc = 0                                  (NOP)
    "0000" & "001" & "111" & "0" & "0" & "00" & "1000" & "0010000", --0x18 IF flagga = 1 then mpc++ else mpc =>myADR (BNE)
    "0000" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x19 PC++, mpc = 0
    "1001" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1A ALU MAGIC, PC++, mpc = 0                (SUPER MOVE)
    "1011" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1B ALU MAGIC, PC++, mpc = 0                (SUPER PEWPEW)
    "1100" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1C ALU MAGIC, PC++, mpc = 0                (SUPER HIT)
    "1101" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x1D 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x1E
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000",  --0x1F
--    ALU     TB      FB      S     P    LC      SEQ       myADR      
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x20 AR => PM, PC++, mpc = 0                  (Flag on burning horse) 
    "1111" & "010" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x21 BTST PM, PC++, mpc = 0                   (BTST)
    "0000" & "001" & "111" & "0" & "0" & "00" & "1001" & "0010000", --0x22 IF z = 1 then mpc++ else mpc=>myADR      (BNE)
    "0000" & "000" & "000" & "0" & "1" & "00" & "0011" & "0000000", --0x23 PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x24 
    "1101" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x25 Alu magic => AR, mpc ++
    "0000" & "100" & "010" & "0" & "1" & "00" & "0011" & "0000000", --0x26 AR => PM, PC++, mpc = 0
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x27 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x28 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x29 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x2A
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x2B
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000", --0x2E 
    "0000" & "000" & "000" & "0" & "0" & "00" & "0000" & "0000000"  --0x2F
    );
    
    -- K1 och K2
    type k1_type is array(0 to 15) of std_logic_vector(7 downto 0);
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
    X"18", -- BNW    0x09  (BRANCH NOT NEWFRAME)
    X"1A", -- MOVE   0x0A
    X"1B", -- PEW    0x0B
    X"1C", -- HIT    0x0C
    X"25", -- GBH    0x0D  (GET BURNING HORSE) 
    X"21", -- BTST   0x0E
    X"22"  -- BNE    0x0F
    );
    constant k2 : k2_type := (
    X"03",
    X"00",
    X"00",
    X"00"
    );
    -- Interna signaler
    signal buss : std_logic_vector(15 downto 0) := X"0000";
    signal mux1 : std_logic_vector(1 downto 0) := "00"; --Mux för de 4 GR register
    signal current_GR : std_logic_vector(15 downto 0);
    
    signal MPC : STD_LOGIC_VECTOR(7 downto 0) := X"00";
    signal myM : std_logic_vector(24 downto 0);
    -- Mym operatorer
    signal ALU_OP : std_logic_vector(3 downto 0) := "0000"; --Bestämmer operator i ALU
    signal TB : std_logic_vector(2 downto 0) := "000"; -- To BUSS
    signal FB : std_logic_vector(2 downto 0) := "000"; -- From BUSS
    signal S : std_logic;
    signal P : std_logic; -- Flagga för att PC ska plussas på
    signal LC : std_logic_vector(1 downto 0); -- Används ej
    signal SEQ : std_logic_vector(3 downto 0); -- SEQ
    signal myADR : std_logic_vector(6 downto 0) := "0000000"; --Mikroaddress



    -- PLAYER MOVE SIGNALS

    -- PLAYER 1
    --x
    signal vel_x1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Velocity
    signal delta_x1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Acceleration
    signal xpos_real1 : sfixed(9 downto -4) := to_sfixed(240, 9, -4); -- precise x value
    signal jstk_x1 : std_logic_vector(9 downto 0); -- Joystick x value
    --y
    signal vel_y1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Velocity
    signal delta_y1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Acceleration
    signal ypos_real1 : sfixed(9 downto -4) := to_sfixed(240, 9, -4); -- Precise y value
    signal jstk_y1 : std_logic_vector(9 downto 0); -- Joystick y value
    --PLAYER 2
    --x
    signal vel_x2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Velocity
    signal delta_x2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Acceleration
    signal xpos_real2 : sfixed(9 downto -4) := to_sfixed(240, 9, -4); -- Precise x value
    signal jstk_x2 : std_logic_vector(9 downto 0); -- Joystick x value
    --y
    signal vel_y2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Velocity
    signal delta_y2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Acceleration
    signal ypos_real2 : sfixed(9 downto -4) := to_sfixed(240, 9, -4); -- Precise y value
    signal jstk_y2 : std_logic_vector(9 downto 0); -- Joystick y value


    -- PROJECTILE SIGNALS

    --PROJ 1
    signal proj_active1 : std_logic := '0';
    --x
    signal proj_dirx1 : std_logic_vector(9 downto 0); -- Joystick x value
    signal proj_deltax1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Projectile x speed
    signal proj_real_xpos1 : sfixed(9 downto -4) := to_sfixed(120, 9, -4); -- precise x position
    signal projectile_xpos1 : integer range 0 to 1023 := 32; -- rounded x position
    --y
    signal proj_diry1 : std_logic_vector(9 downto 0); -- joystick y value
    signal proj_deltay1 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- y speed
    signal proj_real_ypos1 : sfixed(9 downto -4) := to_sfixed(120, 9, -4); -- precise y position
    signal projectile_ypos1 : integer range 0 to 1023 := 32; -- rounded y position
    
    --PROJ 2
    signal proj_active2 : std_logic := '0';
    --x
    signal proj_dirx2 : std_logic_vector(9 downto 0); -- Joystick x value
    signal proj_deltax2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- Projectile x speed
    signal proj_real_xpos2 : sfixed(9 downto -4) := to_sfixed(240, 9, -4); -- precise x position
    signal projectile_xpos2 : integer range 0 to 1023 := 32; -- rounded x position
    --y
    signal proj_diry2 : std_logic_vector(9 downto 0); -- joystick y value
    signal proj_deltay2 : sfixed(2 downto -9) := to_sfixed(0, 2, -9); -- y speed
    signal proj_real_ypos2 : sfixed(9 downto -4) := to_sfixed(240, 9, -4); -- precise y position
    signal projectile_ypos2 : integer range 0 to 1023 := 32; -- rounded y position



    signal hit_counter1 : integer range 0 to 15 := 0; -- Knockback duration player 1
    signal hit_counter2 : integer range 0 to 15 := 0; -- Knockback duration player 2

    signal hp1 : integer range 0 to 63 := 63; -- Healthpoints player 1
    signal hp2 : integer range 0 to 63 := 63; -- Healthpoints player 2

    signal map_counter : std_logic_vector(7 downto 0) := X"00"; --Timer for map cycle
    signal cur_map : std_logic_vector(1 downto 0) := "00"; -- Current map

begin

    playerPosY <= GR0_REG;
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
                ram(conv_integer(ASR_REG(8 downto 0))) <= buss(15 downto 0);
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
            elsif (P = '1') then
                PC_REG <= PC_reg + 1;
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
    mux1 <= IR_REG(11 downto 10); -- GRx
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
        ram(conv_integer(ASR_REG(8 downto 0))) when "010",
        PC_REG when "011",
        AR_REG when "100",
        HR_REG when "101",
        current_GR when "110",
        "000" & mram(conv_integer(MPC))(12 downto 0) when "000",
        (others => '0') when others;
    
    -- GR MUX
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
    LC <= myM(12 downto 11);
    P <= myM(13);
    S <= myM(14);
    FB <= myM(17 downto 15);
    TB <= myM(20 downto 18);
    ALU_OP <= myM(24 downto 21);
    process(CLK) begin
        if rising_edge(CLK) then
            case SEQ is
                when "0000" => MPC <= MPC + 1;
                when "0001" => MPC <= k1(conv_integer(IR_REG(15 downto 12)));
                when "0010" => MPC <= k2(conv_integer('0'&IR_REG(9)));
                when "0011" => MPC <= '0' & myADR;
                when others => MPC <= MPC;
            end case;
            if SEQ = "1000" then
                if flag_newframe = '1' then
                    MPC <= MPC + 1;
                else
                    MPC <= '0' & myADR;
                end if;
            end if;
            if SEQ = "1001" then
                if z_flag = '1' then
                    MPC <= MPC + 1;
                else
                    MPC <= '0' & myADR;
                end if;
            end if;
            if SEQ = "1011" then
                if RST = '1' then
                    MPC <= MPC + 1;
                else
                    MPC <= '0' & myADR;
                end if;
            end if;
            
        end if;
    end process;
    -- ----------------------------------------
    -- # ALU
    -- ----------------------------------------
    process(CLK) begin
        if rising_edge(CLK) then
            case ALU_OP is
                 when "0000" => null;
                 when "0010" => AR_REG(15 downto 0) <= AR_REG(15 downto 0) and buss(15 downto 0); -- AND
                 when "1000" => AR_REG(15 downto 0) <= AR_REG(15 downto 0) + buss(15 downto 0); -- ADD
                 when "0001" => AR_REG(15 downto 0) <= buss(15 downto 0);
                 when "0011" => AR_REG(15 downto 0) <= X"0000";
                 when "1110" => AR_REG(15 downto 0) <= "0" & AR_REG(15 downto 1); -- LSR
                 when "1111" => if buss(0) = '1' then z_flag <= '1'; else z_flag <= '0'; end if; -- BTST
                 when "0101" =>
                            AR_REG(15 downto 0) <= AR_REG(15 downto 0) - buss(15 downto 0); -- SUB, om skillnad är större än 0 höjs z-flaggan
                            if AR_REG = 0 then
                                z_flag <= '0';
                            else
                                z_flag <= '1';
                            end if;
                 when others => null;
            end case;

            if ALU_OP = "0100" then -- Reset player position

            end if;

            -- ----------------------------------------
            -- # PLAYER MOVER
            -- ----------------------------------------
            if ALU_OP = "1001" then
                if knapp = '1' then
                    if jumpctr = 0 and turnaround = '0' and ypos<= 295  then
                        ypos <= ypos-1;
                    elsif jumpctr = 0 and turnaround = '1' and ypos <= 295  then
                        ypos <= ypos+1;

                    elsif (jumpctr = 0 or jumpctr = x"7FFF") and turnaround = '0' and ypos >= 295 then
                       ypos <= ypos-1;
                    elsif (jumpctr = 0 or jumpctr = x"7FFF") and turnaround = '1' and ypos >= 295 then
                      ypos <= ypos+1;
                      if ypos = 339 then
                         knapp <= '0';
                            turnaround <= '0';
                    end if;
            end if;
            if ALU_OP = "1011" then
            end if;  
            if ALU_OP = "1100" then
            end if;  
        end if;
    end process;

end rtl;