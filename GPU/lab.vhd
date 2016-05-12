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
	  -- boardSprites : in boardSprites_t);
end lab;

architecture Behavioral of lab is
  component leddriver
    Port ( clk,rst : in  STD_LOGIC;
           ca,cb,cc,cd,ce,cf,cg,dp : out  STD_LOGIC;
           an : out  STD_LOGIC_VECTOR (3 downto 0);
           ledvalue : in  STD_LOGIC_VECTOR (15 downto 0));
  end component;

	--CPU component insignaler
 -- component CPU
	--port ( clk,rst	: in std_logic; 
		--playerPos	: in integer range 121 to 339;		-- Spelarens position
		--boardSprites : in is array (0 to 10) of STD_LOGIC_VECTOR(19 downto 0); --sprites på skärmen

--);
	--end component;

--bana1: sprite1 - posY = 1, tid= 1, sprite 2 ; sprite2 - posY = 3, tid=1 ... sprite(n) - posy = tid;
 --boardSprites(ele1,ele2,ele3,ele4..ele10) ; ele1= x=40, y=50, sprite=fårnudda 


--						 x				y			  sprite
--binärt tal av 4 bitar. 0000 0000 00 - 0000 0000 0 - 0

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
  signal    data_s	        : std_logic_vector(7 downto 0);         -- data
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
  signal jumpctr : std_logic_vector(19 downto 0) := X"00000";
  signal PlayerPosX : std_logic_vector(15 downto 0) := std_logic_vector(to_unsigned(60, 16));
  signal ypos : std_logic_vector(15 downto 0) := std_logic_vector(to_unsigned(339, 16));
  signal boxpos : std_logic_vector(15 downto 0) := std_logic_vector(to_unsigned(619, 16));
  signal knapp : std_logic := '0';
  signal turnaround : std_logic := '0';
  signal video : std_logic;
  signal boxctr : std_logic_vector(19 downto 0) := X"00000";
  signal ducka : std_logic := '0';
  signal collision : std_logic := '0';
--------------------------signaler----------------cpdw----------
  signal spriteXPos : std_logic_vector(9 downto 0);
  signal spriteYPos : std_logic_vector(8 downto 0);
  signal spriteType : std_logic := '0';
----------------------------------------------------
  signal pix_counter : std_logic_vector(4 downto 0) := "00000"; -- count from 0 to 19 - 00000 to 10011

	--colors for sprites
	type colors_type is array (0 to 4) of std_logic_vector(7 downto 0);	
	signal colors : colors_type := (
		"00000000", -- BLACK 000
		"11111111", -- WHITE 001		
		"11000000", -- RED   010
		"00011100", -- GREEN 011
		"00000011"  -- BLUE  100	
	);
	
	
	--player sprite 
  	type playerSprite_t is array (0 to 399) of std_logic_vector(2 downto 0);
	constant playerSprite : playerSprite_t :=(
		"100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100",
		"100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","100","100",
		"100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100",
		"100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100"	
	);
	
	--sprite that is ok to jump on
	type okSprite_t is array (0 to 399) of std_logic_vector(2 downto 0);
	constant okSprite : okSprite_t :=(
		"011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011",
		"011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","011","011",
		"011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011",
		"011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011","011"
	);
	
	--sprite that you cannot jump on
	type nokSprite_t is array (0 to 399) of std_logic_vector(2 downto 0);
	constant nokSprite : nokSprite_t :=(
		"110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110",
		"110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","000","110","110",
		"110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110",
		"110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110","110"
	);


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
				hej <= "11100011";

			else	
			    hej <= "00000000";
			end if;
		--------------------------------------------
		-------------------SPELARE-----------------------
			if xctr>PlayerPosX and xctr<PlayerPosX+20 and yctr<ypos+20 and yctr>ypos+10 and ducka = '1' then
				hej <= x"0F";
			elsif xctr>PlayerPosX and xctr<PlayerPosX+20 and yctr<ypos+20 and yctr>ypos and ducka = '0' then
				hej <= x"0F";
			end if;
		---------------------------------------------------
		-------------------LÅDA---------------------------
			if boxpos+20>=PlayerPosX and boxpos<=PlayerPosX+20 and ypos > 319 then	
				collision <= '1';
			else
				collision <= '0';
			end if;
			--if xctr<=PlayerPosX and xctr>=PlayerPosX+20 then
				--collision <= '0';
			--end if;

			if xctr>boxpos and xctr<boxpos+20 and yctr>339 and yctr<359 then
				if collision = '1' then
					hej <= "11100000";
				else
					hej <= "11111111";
				end if;
			--elsif xctr>boxpos and xctr<boxpos+20 and yctr>339 and yctr<359 and collision = '0' then
				--hej <= "11111111";
			end if;
			--elsif xctr>boxpos and xctr<boxpos+20 and yctr>339 and yctr<359 then
				--hej <= "11111111";
		end if;
	 else
		hej <= "00000000";
  	 end if; 
    end if;
  end process;





-------------------RITA-UPP-SPRITES--------------------c-p-d-w--------
	--process(clk) begin
		--if rising_edge(clk) then
			--for i in boardSprites' range loop
				--spriteXPos <= boardSprites(i)(19 downto 10);  -- xpos for sprite
				--spriteYPos <= boardSprites(i)(9 downto 1);   -- ypos for sprite
				--spriteType <= boardSprites(i)(0 downto 0);   -- garattnuddasprite = 1 = gron, garinteattnuddasprite = 0 = rod
				--if xctr >=spriteXPos and xctr< spriteXPos+20 and yctr >= spriteYPos and yctr < spriteYPos+20 then    --rita upp sprite
				--	if spriteType = "1" then
					--	hej <= "00011100";      -- gron
					--elsif spriteType = "0" then
					--	hej <= "11100000";     -- rod
					--end if;
				--end if;
		--end if;	
----------------------------------------------------------------------

-----------------------------RITA-UPP-PLAYER-----------c-p-d-w-------
	--process(clk) begin
		--if rising_edge(clk) then
			--if xctr>=60 and xctr<80 and yctr>=playerPos and yctr< playerPos+20 -- rita upp player
				--hej <= x"00000011";    --blå
			--end if;
		--end if;	
---------------------------------------------------------------------


  vgaRed(2 downto 0) <= hej(7 downto 5);
  vgaGreen(2 downto 0) <= hej(4 downto 2);
  vgaBlue(2 downto 1) <= hej(1 downto 0);

 ----************** KOD FÖR ATT FLYTTA PÅ SPELAREN VARIABLAR OCH GREJER *******************--------
  process(clk) begin
		if rising_edge(clk) then
			jumpctr <= jumpctr+1;
			--if ypos = 339 then
				-- knapp <= '0';
			--end if;
			if data_s = x"01" then
				 ducka <= '1';
			end if;
			if knapp = '1' then
				 ducka <= '0';
			end if;
			if data_s = x"00" then
				 knapp <= '1';
			end if;
			if ypos = 290 then
				turnaround <= '1';
			end if;
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


					if ypos < 120 then
						ypos <= std_logic_vector(to_unsigned(339, 16));
					end if;
				end if;
			end if;
		 end if;
 end process;
  --(jumpctr = 0 or jumpctr = x"7FFF")


-----*********************LÅDA SOM KOMMER FRÅN HÖGER SOM VI SKA DODGEA***********-----------
  process(clk) begin
		if rising_edge(clk) then

			if jumpctr = 0 or jumpctr = x"7FFF" then
				boxpos <= boxpos-1; 
			end if;

			if boxpos <= 0 then
				boxpos <= std_logic_vector(to_unsigned(640, 16));
			end if;

		end if;

  end process;
  -- ************************************
  
  process(clk) begin
     if rising_edge(clk) then
       if rst='1' then
         ctr <= X"0000";
       elsif data_s /= x"FF" then
         ctr(7 downto 0) <= data_s;
			 end if;
     end if;
  end process;
  

  U0 : KBD_ENC port map(clk=>clk, rst=>rst, PS2KeyboardCLK=>PS2KeyboardCLK, PS2KeyboardData=>PS2KeyboardData, data=>data_s, addr=>addr_s, we=>we_s);
 -- U1 : PICT_MEM port map(clk=>clk, we1=>we_s, data_in1=>data_s, addr1=>addr_s, we2=>'0', data_in2=>"00000000", data_out2=>data_out2_s, addr2=>addr2_s);
--CPU : CPU(playerPos=>playerPos, boardSprites<=boardSprites);     
  led: leddriver port map (clk,rst,ca,cb,cc,cd,ce,cf,cg,dp,an, ctr);
end Behavioral;

