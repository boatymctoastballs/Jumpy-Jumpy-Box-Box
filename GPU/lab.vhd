library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;   
-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;


entity lab is
    Port ( clk,rst : in  STD_LOGIC;
           vgaRed, vgaGreen : out  STD_LOGIC_VECTOR (2 downto 0);
           vgaBlue : out  STD_LOGIC_VECTOR (2 downto 1);
           ca,cb,cc,cd,ce,cf,cg,dp, Hsync,Vsync : out  STD_LOGIC;
           an : out  STD_LOGIC_VECTOR (3 downto 0);
	   PS2KeyboardCLK	  : in STD_LOGIC;
	   PS2KeyboardData        : in STD_LOGIC);
end lab;

architecture Behavioral of lab is
  component leddriver
    Port ( clk,rst : in  STD_LOGIC;
           ca,cb,cc,cd,ce,cf,cg,dp : out  STD_LOGIC;
           an : out  STD_LOGIC_VECTOR (3 downto 0);
           ledvalue : in  STD_LOGIC_VECTOR (15 downto 0));
  end component;

    --PS2 keyboard encoder component
  component KBD_ENC
    port ( clk		        : in std_logic;				-- system clock
	   rst		        : in std_logic;				-- reset signal
	   PS2KeyboardCLK       : in std_logic;				-- PS2 clock
	   PS2KeyboardData      : in std_logic;				-- PS2 data
	   data		        : out std_logic_vector(7 downto 0);	-- tile data
	   addr			: out unsigned(10 downto 0);	        -- tile address
	   we			: out std_logic);	                -- write enable
  end component;

  component PICT_MEM
    port ( clk			: in std_logic;                         -- system clock
	 -- port 1
           we1		        : in std_logic;                         -- write enable
           data_in1	        : in std_logic_vector(7 downto 0);      -- data in
           data_out1	        : out std_logic_vector(7 downto 0);     -- data out
           addr1	        : in unsigned(10 downto 0);             -- address
	 -- port 2
           we2			: in std_logic;                         -- write enable
           data_in2	        : in std_logic_vector(7 downto 0);      -- data in
           data_out2	        : out std_logic_vector(7 downto 0);     -- data out
           addr2		: in unsigned(10 downto 0));            -- address
  end component;

  -- intermediate signals between KBD_ENC and PICT_MEM
  signal        data_s	        : std_logic_vector(7 downto 0);         -- data
  signal	addr_s	        : unsigned(10 downto 0);                -- address
  signal	we_s		: std_logic;                            -- write enable
	
  -- intermediate signals between PICT_MEM and VGA_MOTOR
  signal	data_out2_s     : std_logic_vector(7 downto 0);         -- data
  signal	addr2_s		: unsigned(10 downto 0);                -- address
	

  signal xctr,yctr : std_logic_vector(9 downto 0) := "0000000000";
  alias rad : std_logic_vector(6 downto 0) is yctr(9 downto 3); -- i bildminnet
  alias kol : std_logic_vector(6 downto 0) is xctr(9 downto 3);  -- i bildminnet
  alias ypix : std_logic_vector(2 downto 0) is yctr(2 downto 0); -- i pixeln
  alias xpix : std_logic_vector(2 downto 0) is xctr(2 downto 0);  -- i pixeln
  signal pixel : std_logic_vector(1 downto 0) := "00";
  signal a,b,c,d : std_logic_vector(0 to 79) := X"00000000000000000000";
  signal a0,a1,a2,b0,b1,b2,c0,c1,c2 : std_logic := '0';
  signal nr : std_logic_vector(3 downto 0) := "0000";
  signal ctr : std_logic_vector(15 downto 0) := X"0000";
  signal hs : std_logic := '1';
  signal vs : std_logic := '1';
  --type ram_t is array (0 to 59) of std_logic_vector(0 to 79);
  signal hej : std_logic_vector(7 downto 0) := "00000000";





  signal video : std_logic;
begin
  process(clk) begin
     if rising_edge(clk) then
       if rst='1' then
         pixel <= "00";
       else
         pixel <= pixel + 1;
       end if;
     end if;
  end process;


  process(clk) begin
    if rising_edge(clk) then
      if rst='1' then
         xctr <= "0000000000";
      elsif pixel=3 then
       if xctr=799 then
         xctr <= "0000000000";
       else
         xctr <= xctr + 1;
       end if;
      end if;
      -- 
      if xctr=656 then
        hs <= '0';
      elsif xctr=752 then
        hs <= '1';
      end if;
    end if;
  end process;

  process(clk) begin
    if rising_edge(clk) then
      if rst='1' then
        yctr <= "0000000000";
      elsif xctr=799 and pixel=0 then
       if yctr=520 then
         yctr <= "0000000000";
       else
         yctr <= yctr + 1;
       end if;
       --
       if yctr=490 then
         vs <= '0';
       elsif  yctr=492 then
         vs <= '1';
       end if;
      end if;
    end if;
  end process;
  Hsync <= hs;
  Vsync <= vs;

 
------------------ RITA UPP SPELPLAN ---------------------------
  process(clk) begin
    if rising_edge(clk) then
	if xctr <= 639 and yctr <= 479 then
		--------------RAM---------------------
		if xctr = 0 or yctr = 479 or yctr = 0 or xctr = 639 then
			hej <= "11111111";
		else
			if yctr < 359 and yctr > 120 then
				if data_s = x"01" then
					hej <= "11100011";
				else
					hej <= "00011100";
				end if;
			
			else	
				hej <= "00000000";
			end if;
		--------------------------------------------
		-------------------SPELARE-----------------------
			if xctr>20 and xctr<40 and yctr<359 and yctr>339 then
				hej <= x"0F";
			end if;
		---------------------------------------------------
		end if;
	else
		hej <= "00000000";
  	end if;
     end if;
  end process;
  vgaRed(2 downto 0) <= hej(7 downto 5);
  vgaGreen(2 downto 0) <= hej(4 downto 2);
  vgaBlue(2 downto 1) <= hej(1 downto 0);
  
  
  -- ************************************
  
  process(clk) begin
     if rising_edge(clk) then
       if rst='1' then
         ctr <= X"0000";
       elsif yctr=0 and xctr=0 and pixel=0 then
         ctr <= ctr+1;
       end if;
     end if;
  end process;
  

  U0 : KBD_ENC port map(clk=>clk, rst=>rst, PS2KeyboardCLK=>PS2KeyboardCLK, PS2KeyboardData=>PS2KeyboardData, data=>data_s, addr=>addr_s, we=>we_s);
  U1 : PICT_MEM port map(clk=>clk, we1=>we_s, data_in1=>data_s, addr1=>addr_s, we2=>'0', data_in2=>"00000000", data_out2=>data_out2_s, addr2=>addr2_s);     
  led: leddriver port map (clk,rst,ca,cb,cc,cd,ce,cf,cg,dp,an, ctr);
end Behavioral;
