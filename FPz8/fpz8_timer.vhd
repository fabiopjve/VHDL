-- FPz8 simple timer
-- By Fabio Pereira

library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all ;

entity fpz8_timer IS
	port
	(
		RAB			: in std_logic_vector(11 downto 0);
		RIDB		: in std_logic_vector(7 downto 0);
		RODB		: out std_logic_vector(7 downto 0);
		SIDB		: in std_logic_vector(7 downto 0);
		REG_SEL		: in std_logic;
		WR			: in std_logic;
		CLK_IN		: in std_logic;
		STOP		: in std_logic;
		INT			: out std_logic;
		TMR_OUT		: out std_logic;
		TMR_IN		: in std_logic;
		TMR_ID		: in std_logic_vector(1 downto 0);
		RESET		: in std_logic
	);
end fpz8_timer;

architecture timer of fpz8_timer is
shared variable TMR_EN		: std_logic;
shared variable TMR_CMP		: std_logic_vector(15 downto 0);
shared variable TMR_TEMP	: std_logic_vector(7 downto 0);
shared variable TMR_PRESEL	: std_logic_vector(2 downto 0);
shared variable TMR_CNT		: std_logic_vector(15 downto 0);
shared variable TMR_PRE		: std_logic_vector(7 downto 0);
shared variable INT_OUT		: std_logic;
shared variable BASE_ADDR	: std_logic_vector(11 downto 0);
begin
	control: process(CLK_IN,REG_SEL,RESET)
	begin
		INT <= INT_OUT;
		case TMR_ID is
			when "00" =>	BASE_ADDR := x"F00";
			when "01" =>	BASE_ADDR := x"F08";
			when "10" =>	BASE_ADDR := x"F10";
			when "11" =>	BASE_ADDR := x"F18";
		end case;		
		if (RESET='1') then
			TMR_EN := '0';
			TMR_CMP := x"0000";			
			TMR_TEMP := x"00";
		elsif (rising_edge(CLK_IN) and REG_SEL='1') then
			if (WR='0') then	-- it is a reading operation
				if (RAB=(BASE_ADDR+7)) then ------ register TMR_CTL
					RODB<=TMR_EN&'0'&TMR_PRESEL&"000";
				elsif (RAB=BASE_ADDR+2) then	-- register TMR_CMPH
					RODB<=TMR_CMP(15 downto 8);
				elsif (RAB=BASE_ADDR+3) then	-- register TMR_CMPL
					RODB<=TMR_CMP(7 downto 0);
				else RODB<=SIDB;
				end if;
			else				-- it is a writing operation
				if (RAB=BASE_ADDR+7) then ----- register TMR_CTL
					TMR_EN:=RIDB(7);
					TMR_PRESEL:=RIDB(5 downto 3);
				elsif (RAB=BASE_ADDR+2) then	-- register TMR_RLH
					TMR_TEMP := RIDB;
				elsif (RAB=BASE_ADDR+3) then	-- register TMR_RLL
					TMR_CMP(7 downto 0) := RIDB;
					TMR_CMP(15 downto 8) := TMR_TEMP;
				end if;			
			end if;
		end if;
	end process control;	-- control process
	counter: process(CLK_IN,RESET,STOP)
	variable TMR_PRECP	: std_logic_vector(7 downto 0);	
	begin
		if (RESET='1') then
			TMR_CNT := x"0000";
			TMR_PRE := x"00";
			TMR_PRECP := x"00";
			INT_OUT := '0';
		elsif (rising_edge(CLK_IN)) then
			if (STOP='0') then
				if (TMR_EN='1') then
					case TMR_PRESEL is
						when "000"	=> TMR_PRECP:=x"01";	-- prescaler divide by 1
						when "001"	=> TMR_PRECP:=x"02";	-- prescaler divide by 2
						when "010"	=> TMR_PRECP:=x"04";	-- prescaler divide by 4
						when "011"	=> TMR_PRECP:=x"08";	-- prescaler divide by 8
						when "100"	=> TMR_PRECP:=x"10";	-- prescaler divide by 16
						when "101"	=> TMR_PRECP:=x"20";	-- prescaler divide by 32
						when "110"	=> TMR_PRECP:=x"40";	-- prescaler divide by 64
						when others	=> TMR_PRECP:=x"80";	-- prescaler divide by 128
					end case;
					TMR_PRE := TMR_PRE + 1;
					if (TMR_PRE=TMR_PRECP) then
						TMR_PRE:=x"00";
						TMR_CNT:=TMR_CNT+1;
						if (TMR_CNT=TMR_CMP) then
							TMR_CNT:=x"0000";
							INT_OUT := not INT_OUT;
						end if;
					end if;
				else
					TMR_CNT:=x"0000";
					TMR_PRE:=x"00";
				end if;	-- if TMR_EN=1
			end if;	-- if STOP=0
		end if;	-- rising edge of CLK_IN
	end process;	-- counter process
end timer;