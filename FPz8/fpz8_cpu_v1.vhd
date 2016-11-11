-- FPz8 mk1
-- Zilog Z8 Encore! 100% compatible softcore
-- Author: 	Fábio Pereira (fabio.jve@gmail.com)
-- Version:	0.9		Nov, 11th, 2016

-- FPz8 is a softcore 100% object code compatible with the Z8 encore microcontroller line. Current implementation includes 
-- 2kb of file registers (RAM), 16kb of program memory (using FPGA RAM), 8 vectored interrupts with programmable priority, 
-- full-featured onchip debugger 100% compatible with Zilog's OCD and ZDS-II IDE.
-- It was designed to work as a SoC and everything (except the USB chip) fits inside a single FPGA (I have used an Altera 
-- Cyclone IV EP4CE6 device). The debugger connection makes use of a serial-to-USB chip (it is part of the low-cost FPGA 
-- board used on the project).
-- In a near future I plan to add some more features to the device (such as a timer and maybe other peripherals).
-- The idea behind the FPz8 was to learn more on VHDL and FPGAs (this is my second design using those technologies). I also 
-- believe the FPz8 can be a very interesting tool for learning/teaching about VHDL, computing and microprocessors/microcontrollers 
-- programming.

-- You are free to use and to modify the FPz8 to fit your needs, except for comercial use (I don't expect anyone would do that anyway).
-- If you want to contribute to the project, contact me and share your thoughts.
-- Don't forget to credit the author!
-- Note: currently there are only a few SFRs physically implemented, they are:
-- 0xFC0 - IRQ0
-- 0xFC1 - IRQ0ENH
-- 0xFC2 - IRQ0ENL
-- 0xFCF - IRQCTL
-- 0xFD2 - PAIN
-- 0xFD3 - PAOUT
-- 0xFF8 - FCTL
-- 0xFFC - FLAGS
-- 0xFFD - RP
-- 0xFFE - SPH
-- 0xFFF - SPL

-- This work is licensed under the Creative Commons Attribution 4.0 International License.
-- To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/.

library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all ;

entity fpz8_cpu_v1 IS
	port
	(
		IAB			: buffer std_logic_vector(15 downto 0);	-- instruction address bus (16 bits)
		IDB			: in std_logic_vector(7 downto 0);		-- instruction data bus (8 bits)
		PWDB		: out std_logic_vector(7 downto 0);		-- program write data bus (8 bits)
		MAB			: buffer std_logic_vector(11 downto 0);	-- memory address bus (12 bits)
		MIDB		: in std_logic_vector(7 downto 0);		-- memory input data bus (8 bits)
		MODB		: out std_logic_vector(7 downto 0);		-- memory output data bus (8 bits)
		RIDB		: in std_logic_vector(7 downto 0);		-- register input data bus (8 bits)
		RODB		: out std_logic_vector(7 downto 0);		-- register output data bus (8 bits)		
		PGM_WR		: out std_logic;						-- program memory write enable
		WR			: buffer std_logic;						-- write enable
		REG_SEL		: buffer std_logic;						-- SFR select (addresses F00 to FFF, except internal registers)
		MEM_SEL		: buffer std_logic;						-- memory select
		INT0		: in std_logic;							-- interrupt 0 input (vector 0x0016)
		INT1		: in std_logic;							-- interrupt 1 input (vector 0x0014)
		INT2		: in std_logic;							-- interrupt 2 input (vector 0x0012)
		INT3		: in std_logic;							-- interrupt 3 input (vector 0x0010)
		INT4		: in std_logic;							-- interrupt 4 input (vector 0x000E)
		INT5		: in std_logic;							-- interrupt 5 input (vector 0x000C)
		INT6		: in std_logic;							-- interrupt 6 input (vector 0x000A)
		DBG_RX		: in std_logic;							-- debugger receive input
		DBG_TX		: buffer std_logic;						-- debugger transmit output
		PAOUT		: buffer std_logic_vector(7 downto 0);	-- port A output data
		PAIN		: in std_logic_vector(7 downto 0);		-- port A input data
		CLK			: in std_logic;							-- main clock
		CLK_OUT		: out std_logic;						-- main gated-clock output
		CLK_OUTN	: out std_logic;						-- main inverted-gated-clock output
		STOP		: buffer std_logic;						-- stop output	 
		RESET		: in std_logic							-- CPU reset			
	);
end fpz8_cpu_v1;

architecture cpu of fpz8_cpu_v1 is

type Tinstqueue is array (0 TO 7) of std_logic_vector(7 downto 0);
type Tflags is record
	C,Z,S,V,D,H,F2,F1			: std_logic;
end record;
shared variable CPU_FLAGS, ALU_FLAGS		: Tflags;
shared variable ALU_NOUPDATE				: std_logic;
shared variable INT7						: std_logic;
shared variable IRQE						: std_logic;
shared variable HALT						: std_logic;
shared variable IRQ0						: std_logic_vector(7 downto 0);		-- interrupts 0-7 flags
shared variable IRQ0ENH,IRQ0ENL				: std_logic_vector(7 downto 0);		-- interrupts 0-7 enable high and low
shared variable SP 							: std_logic_vector(11 downto 0);	-- stack pointer
shared variable RP							: std_logic_vector(7 downto 0);		-- register pointer
shared variable FCTL						: std_logic_vector(7 downto 0);		-- flash control
signal 			RXSYNC1, RXSYNC2			: std_logic;
ATTRIBUTE preserve							: boolean;
ATTRIBUTE preserve OF RXSYNC1				: signal IS true;
ATTRIBUTE preserve OF RXSYNC2				: signal IS true;

constant ALU_ADD 	: std_logic_vector(3 downto 0):=x"0";	-- CZSVH D=0
constant ALU_ADC 	: std_logic_vector(3 downto 0):=x"1";	-- CZSVH D=0
constant ALU_SUB 	: std_logic_vector(3 downto 0):=x"2";	-- CZSVH D=1
constant ALU_SBC 	: std_logic_vector(3 downto 0):=x"3";	-- CZSVH D=1
constant ALU_OR	 	: std_logic_vector(3 downto 0):=x"4";	-- ZS V=0
constant ALU_AND 	: std_logic_vector(3 downto 0):=x"5";	-- ZS V=0
constant ALU_TCM 	: std_logic_vector(3 downto 0):=x"6";	-- ZS V=0
constant ALU_TM  	: std_logic_vector(3 downto 0):=x"7";	-- ZS V=0
constant ALU_CPC  	: std_logic_vector(3 downto 0):=x"9";	-- CZSV
constant ALU_CP  	: std_logic_vector(3 downto 0):=x"A";	-- CZSV
constant ALU_XOR 	: std_logic_vector(3 downto 0):=x"B";	-- ZS V=0
constant ALU_BSWAP  : std_logic_vector(3 downto 0):=x"D";	-- ZS V=0
constant ALU_LD  	: std_logic_vector(3 downto 0):=x"E";	-- Load does not change any flag

constant LU2_RLC	: std_logic_vector(3 downto 0):=x"1";	-- CZSV
constant LU2_INC	: std_logic_vector(3 downto 0):=x"2";	-- ZSV
constant LU2_DEC	: std_logic_vector(3 downto 0):=x"3";	-- ZSV
constant LU2_DA		: std_logic_vector(3 downto 0):=x"4";	-- CZS
constant LU2_COM	: std_logic_vector(3 downto 0):=x"6";	-- ZS V=0
constant LU2_LD		: std_logic_vector(3 downto 0):=x"7";	-- Load does not change any flag
constant LU2_RL		: std_logic_vector(3 downto 0):=x"9";	-- CZSV
constant LU2_SRL	: std_logic_vector(3 downto 0):=x"A";	-- CZSV
constant LU2_CLR	: std_logic_vector(3 downto 0):=x"B";	-- Clear does not change any flag
constant LU2_RRC	: std_logic_vector(3 downto 0):=x"C";	-- CZSV
constant LU2_SRA	: std_logic_vector(3 downto 0):=x"D";	-- CZSV
constant LU2_RR		: std_logic_vector(3 downto 0):=x"E";	-- CZSV
constant LU2_SWAP	: std_logic_vector(3 downto 0):=x"F";	-- ZS

-- Debug commands
constant DBGCMD_READ_REV		: std_logic_vector(7 downto 0):=x"00";
constant DBGCMD_READ_STATUS		: std_logic_vector(7 downto 0):=x"02";
constant DBGCMD_READ_RUNCOUNTER	: std_logic_vector(7 downto 0):=x"03";
constant DBGCMD_WRITE_CTRL		: std_logic_vector(7 downto 0):=x"04";
constant DBGCMD_READ_CTRL		: std_logic_vector(7 downto 0):=x"05";
constant DBGCMD_WRITE_PC		: std_logic_vector(7 downto 0):=x"06";
constant DBGCMD_READ_PC			: std_logic_vector(7 downto 0):=x"07";
constant DBGCMD_WRITE_REG		: std_logic_vector(7 downto 0):=x"08";
constant DBGCMD_READ_REG		: std_logic_vector(7 downto 0):=x"09";
constant DBGCMD_WRITE_PROGRAM	: std_logic_vector(7 downto 0):=x"0A";
constant DBGCMD_READ_PROGRAM	: std_logic_vector(7 downto 0):=x"0B";
constant DBGCMD_READ_CRC		: std_logic_vector(7 downto 0):=x"0E";
constant DBGCMD_STEP			: std_logic_vector(7 downto 0):=x"10";
constant DBGCMD_STUFF			: std_logic_vector(7 downto 0):=x"11";
constant DBGCMD_EXEC			: std_logic_vector(7 downto 0):=x"12";

-- DATAWRITE controls where data to be written actually goes (an internal register, an external register (through register data bus) or RAM)
procedure DATAWRITE
	(	ADDRESS	: in std_logic_vector(11 downto 0);
		DATA	: in std_logic_vector(7 downto 0)) is
begin
	if (ADDRESS>=x"F00") then		----------------------------------------------- it is a SFR address
		if (ADDRESS=x"FFC") then	---------------------------------------------------- FLAGS register
			CPU_FLAGS.C := DATA(7);
			CPU_FLAGS.Z := DATA(6);
			CPU_FLAGS.S := DATA(5);
			CPU_FLAGS.V := DATA(4);
			CPU_FLAGS.D := DATA(3);
			CPU_FLAGS.H := DATA(2);
			CPU_FLAGS.F2 := DATA(1);
			CPU_FLAGS.F1 := DATA(0);
		elsif (ADDRESS=x"FFD") then RP := DATA;	------------------------------------------- RP register
		elsif (ADDRESS=x"FFE") then SP(11 downto 8) := DATA(3 downto 0);	-------------- SPH register
		elsif (ADDRESS=x"FFF") then SP(7 downto 0) := DATA;	------------------------------ SPL register
		elsif (ADDRESS=x"FF8") then	----------------------------------------------------- FCTL register
			if (DATA=x"73") then FCTL:=x"01";
			elsif (DATA=x"8C" and FCTL=x"01") then FCTL:=x"03";
			elsif (DATA=x"95") then FCTL:=x"04";
			else FCTL:=x"00";
			end if;
		elsif (ADDRESS=x"FC0") then	IRQ0 := DATA; --------------------------------------- IRQ0 register
		elsif (ADDRESS=x"FC1") then IRQ0ENH := DATA;	------------------------------ IRQ0ENH register
		elsif (ADDRESS=x"FC2") then IRQ0ENL := DATA;	------------------------------ IRQ0ENL register
		elsif (ADDRESS=x"FCF") then	IRQE := DATA(7);	------------------------------- IRQCTL register
		elsif (ADDRESS=x"FD3") then PAOUT <= DATA;	------------------------------------ PAOUT register
		else 
			REG_SEL <= '1';
			RODB <= DATA;
		end if;
	else
		MEM_SEL <= '1';
		MODB <= DATA;
	end if;		
end datawrite;

-- DATAREAD controls where the data to be read actually comes from (an internal register, an external register (through register data bus) or RAM)
impure function DATAREAD
	(ADDRESS	: in std_logic_vector(11 downto 0))
	return std_logic_vector is
begin
	if (ADDRESS>=x"F00") then	-------------------------------- it is a SFR address
		if (ADDRESS=x"FFC") then	--------------------------------- FLAGS register
			return (CPU_FLAGS.C,CPU_FLAGS.Z,CPU_FLAGS.S,CPU_FLAGS.V,CPU_FLAGS.D,CPU_FLAGS.H,CPU_FLAGS.F2,CPU_FLAGS.F1);
		elsif (ADDRESS=x"FFD") then return RP;	------------------------ RP register
		elsif (ADDRESS=x"FFE") then	----------------------------------- SPH register
			return "0000" & SP(11 downto 8);
		elsif (ADDRESS=x"FFF") then return SP(7 downto 0);	----------- SPL register
		elsif (ADDRESS=x"FF8") then return FCTL;	------------------ FCTL register
		elsif (ADDRESS=x"FC0") then	return IRQ0;	------------------ IRQ0 register
		elsif (ADDRESS=x"FC1") then return IRQ0ENH;	--------------- IRQ0ENH register
		elsif (ADDRESS=x"FC2") then return IRQ0ENL;	--------------- IRQ0ENL register
		elsif (ADDRESS=x"FCF") then return IRQE&"0000000";	-------- IRQCTL register
		elsif (ADDRESS=x"FD2") then return PAIN;	------------------ PAIN register
		elsif (ADDRESS=x"FD3") then return PAOUT;	----------------- PAOUT register
		else 
			REG_SEL <= '1';
			return RIDB;
		end if;
	else
		MEM_SEL <= '1';
		return MIDB;
	end if;		
end DATAREAD;

-- CONDITIONCODE returns the result of a logical condition (for conditional jumps)
function CONDITIONCODE
	(	CONDITION	: in std_logic_vector(3 downto 0)) return STD_LOGIC is
begin
	case CONDITION is
		when x"0" =>
			return '0';
		when x"1" =>
			return ALU_FLAGS.S xor ALU_FLAGS.V;
		when x"2" =>
			return ALU_FLAGS.Z or (ALU_FLAGS.S xor ALU_FLAGS.V);
		when x"3" =>
			return ALU_FLAGS.C or ALU_FLAGS.Z;
		when x"4" =>
			return ALU_FLAGS.V;
		when x"5" =>
			return ALU_FLAGS.S;
		when x"6" =>
			return ALU_FLAGS.Z;
		when x"7" =>
			return ALU_FLAGS.C;
		when x"8" =>
			return '1';
		when x"9" =>
			return NOT (ALU_FLAGS.S xor ALU_FLAGS.V);
		when x"A" =>
			return NOT (ALU_FLAGS.Z or (ALU_FLAGS.S xor ALU_FLAGS.V));
		when x"B" =>
			return (NOT ALU_FLAGS.C) AND (NOT ALU_FLAGS.Z);
		when x"C" =>
			return NOT ALU_FLAGS.V;
		when x"D" =>
			return NOT ALU_FLAGS.S;
		when x"E" =>
			return NOT ALU_FLAGS.Z;
		when others =>
			return NOT ALU_FLAGS.C;
	end case;
end CONDITIONCODE;
		
-- ADDRESSER12 generates a 12-bit address (it decides when to use escaped addressing mode)
function ADDRESSER12
	(	ADDR	: in std_logic_vector(11 downto 0)) return std_logic_vector is
begin
	if (ADDR(11 downto 4)=x"EE") then		-- escaped addressing mode (work register)
		return RP(3 downto 0) & RP(7 downto 4) & ADDR(3 downto 0);
	elsif (ADDR(11 downto 8)=x"E") then		-- escaped addressing mode (register)
		return RP(3 downto 0) & ADDR(7 downto 0);
	else return ADDR;						-- full address
	end if;
end ADDRESSER12;

-- ADDRESSER8 generates a 12-bit address from an 8-bit address (it decides when to use escaped addressing mode)
function ADDRESSER8
	(	ADDR	: in std_logic_vector(7 downto 0)) return std_logic_vector is
begin
	if (ADDR(7 downto 4)=x"E") then		-- escaped addressing mode (register)
		return RP(3 downto 0) & RP(7 downto 4) & ADDR(3 downto 0);
	else return RP(3 downto 0) & ADDR(7 downto 0);	-- full address
	end if;
end ADDRESSER8;

-- ADDRESSER12 generates a 12-bit address from a 4-bit address (using RP register)
function ADDRESSER4
	(	ADDR	: in std_logic_vector(3 downto 0)) return std_logic_vector is
begin
	return RP(3 downto 0) & RP(7 downto 4) & ADDR;
end ADDRESSER4;

-- ALU is the arithmetic and logic unit, it receives two 8-bit operands along with a 4-bit operation code and a carry input, returning an 8-bit result
function ALU
	(	ALU_OP	: in std_logic_vector(3 downto 0);
		OPER1	: in std_logic_vector(7 downto 0);
		OPER2	: in std_logic_vector(7 downto 0);
		CIN		: in STD_LOGIC) return std_logic_vector is
variable RESULT : std_logic_vector(7 downto 0);
variable HALF1,HALF2	: std_logic_vector(4 downto 0);
begin
	ALU_NOUPDATE := '0';
	case ALU_OP is
		when ALU_ADD =>		-- ADD operation **************************************************
			HALF1 := ('0'&OPER1(3 downto 0))+('0'&OPER2(3 downto 0));
			ALU_FLAGS.H := HALF1(4);
			HALF2 := ('0'&OPER1(7 downto 4))+('0'&OPER2(7 downto 4))+HALF1(4);
			RESULT := HALF2(3 downto 0) & HALF1(3 downto 0);
			ALU_FLAGS.C := HALF2(4);
			if (OPER1(7)=OPER2(7)) then
				if (OPER1(7)/=RESULT(7)) then ALU_FLAGS.V :='1'; else ALU_FLAGS.V :='0';
				end if;
			else ALU_FLAGS.V:='0';
			end if;
		when ALU_ADC =>		-- ADC operation **************************************************
			HALF1 := ('0'&OPER1(3 downto 0))+('0'&OPER2(3 downto 0)+(CIN));
			ALU_FLAGS.H := HALF1(4);
			HALF2 := ('0'&OPER1(7 downto 4))+('0'&OPER2(7 downto 4))+HALF1(4);
			RESULT := HALF2(3 downto 0) & HALF1(3 downto 0);
			ALU_FLAGS.C := HALF2(4);
			if (OPER1(7)=OPER2(7)) then
				if (OPER1(7)/=RESULT(7)) then ALU_FLAGS.V :='1'; else ALU_FLAGS.V :='0';
				end if;
			else ALU_FLAGS.V:='0';
			end if;			
		when ALU_SUB =>		-- SUB operation **************************************************
			HALF1 := ('0'&OPER1(3 downto 0))-('0'&(OPER2(3 downto 0)));
			ALU_FLAGS.H := (HALF1(4));
			HALF2 := ('0'&OPER1(7 downto 4))-('0'&(OPER2(7 downto 4)))-HALF1(4);
			RESULT := HALF2(3 downto 0) & HALF1(3 downto 0);
			ALU_FLAGS.C := (HALF2(4));
			if (OPER1(7)/=OPER2(7)) then
				if (OPER1(7)=RESULT(7)) then ALU_FLAGS.V :='1'; else ALU_FLAGS.V :='0';
				end if;
			else ALU_FLAGS.V:='0';
			end if;
		when ALU_SBC =>		-- SBC operation **************************************************
			HALF1 := ('0'&OPER1(3 downto 0))-('0'&(OPER2(3 downto 0)))-CIN;
			ALU_FLAGS.H := (HALF1(4));
			HALF2 := ('0'&OPER1(7 downto 4))-('0'&(OPER2(7 downto 4)))-HALF1(4);
			RESULT := HALF2(3 downto 0) & HALF1(3 downto 0);
			ALU_FLAGS.C := (HALF2(4));
			if (OPER1(7)/=OPER2(7)) then
				if (OPER1(7)=RESULT(7)) then ALU_FLAGS.V :='1'; else ALU_FLAGS.V :='0';
				end if;
			else ALU_FLAGS.V:='0';
			end if;	
		when ALU_OR =>	-- Logical or operation ***********************************************
			RESULT := OPER1 or OPER2;
		when ALU_AND =>	-- Logical AND operation **********************************************
			RESULT := OPER1 AND OPER2;
		when ALU_TCM => -- Test Complement Mask operation *************************************
			RESULT := (NOT OPER1) AND OPER2;
			ALU_NOUPDATE := '1';
		when ALU_TM =>	-- Test Mask operation ************************************************
			RESULT := OPER1 AND OPER2;
			ALU_NOUPDATE := '1';
		when ALU_CPC =>		-- CPC operation **************************************************
			HALF1 := ('0'&OPER1(3 downto 0))-('0'&(OPER2(3 downto 0)))-CIN;
			ALU_FLAGS.H := (HALF1(4));
			HALF2 := ('0'&OPER1(7 downto 4))-('0'&(OPER2(7 downto 4)))-HALF1(4);
			RESULT := HALF2(3 downto 0) & HALF1(3 downto 0);
			ALU_FLAGS.C := (HALF2(4));
			if (OPER1(7)/=OPER2(7)) then
				if (OPER1(7)=RESULT(7)) then ALU_FLAGS.V :='1'; else ALU_FLAGS.V :='0';
				end if;
			else ALU_FLAGS.V:='0';
			end if;
			ALU_NOUPDATE := '1';			
		when ALU_CP =>	-- Compare operation **************************************************
			HALF1 := ('0'&OPER1(3 downto 0))-('0'&(OPER2(3 downto 0)));
			ALU_FLAGS.H := (HALF1(4));
			HALF2 := ('0'&OPER1(7 downto 4))-('0'&(OPER2(7 downto 4)))-HALF1(4);
			RESULT := HALF2(3 downto 0) & HALF1(3 downto 0);
			ALU_FLAGS.C := (HALF2(4));
			if (OPER1(7)/=OPER2(7)) then
				if (OPER1(7)=RESULT(7)) then ALU_FLAGS.V :='1'; else ALU_FLAGS.V :='0';
				end if;
			else ALU_FLAGS.V:='0';
			end if;
			ALU_NOUPDATE := '1';
		when ALU_XOR =>	-- Logical xor operation **********************************************
			RESULT := OPER1 xor OPER2;
		when ALU_BSWAP =>	-- Bit Swap operation *********************************************
			RESULT := OPER2(0)&OPER2(1)&OPER2(2)&OPER2(3)&OPER2(4)&OPER2(5)&OPER2(6)&OPER2(7);			
		when others =>	-- Load operation *****************************************************
			RESULT := OPER2;
		
	end case;
	if (RESULT(7 downto 0)=x"00") then ALU_FLAGS.Z := '1'; else ALU_FLAGS.Z := '0';
	end if;
	ALU_FLAGS.S := RESULT(7);
	return RESULT(7 downto 0);
end ALU;	

-- LU2 is the second logic unit, it performs mostly logical operations not covered by the ALU
function LU2
	(	LU2_OP	: in std_logic_vector(3 downto 0);
		OPER	: in std_logic_vector(7 downto 0);
		DIN		: in std_logic;
		HIN		: in std_logic;
		CIN		: in std_logic) return std_logic_vector is
variable RESULT : std_logic_vector(7 downto 0);
begin
	case LU2_OP is
		when LU2_RLC =>		-- RLC operation **************************************************
			ALU_FLAGS.C := OPER(7);
			RESULT := OPER(6)&OPER(5)&OPER(4)&OPER(3)&OPER(2)&OPER(1)&OPER(0)&CIN;
		when LU2_INC =>		-- INC operation **************************************************
			RESULT := OPER+1;	
			if (RESULT=x"00") then ALU_FLAGS.C:='1'; else ALU_FLAGS.C:='0';
			end if;
		when LU2_DEC =>		-- DEC operation **************************************************
			RESULT := OPER-1;
			if (RESULT=x"FF") then ALU_FLAGS.C:='1'; else ALU_FLAGS.C:='0';
			end if;
		when LU2_DA =>		-- DA operation ***************************************************
			if (DIN='0') then	-- decimal adjust following an add operation
				if (OPER(3 downto 0)>x"9" or HIN='1') then
					RESULT := ALU(ALU_ADD,OPER,x"06",'0');
				else RESULT := OPER;
				end if;
				if (RESULT(7 downto 4)>x"9" or ALU_FLAGS.C='1') then
					RESULT := ALU(ALU_ADD,RESULT,x"60",'0');
				end if;
			else	-------------- decimal adjust following a sub operation
			end if;
		when LU2_COM =>		-- COM operation **************************************************
			RESULT := NOT OPER;
		when LU2_RL =>		-- RL operation ***************************************************
			ALU_FLAGS.C := OPER(7);
			RESULT := OPER(6)&OPER(5)&OPER(4)&OPER(3)&OPER(2)&OPER(1)&OPER(0)&ALU_FLAGS.C;
		when LU2_SRL =>		-- SRL operation **************************************************
			ALU_FLAGS.C := OPER(0);
			RESULT := '0'&OPER(7)&OPER(6)&OPER(5)&OPER(4)&OPER(3)&OPER(2)&OPER(1);				
		when LU2_RRC =>		-- RRC operation **************************************************
			ALU_FLAGS.C := OPER(0);
			RESULT := CIN&OPER(7)&OPER(6)&OPER(5)&OPER(4)&OPER(3)&OPER(2)&OPER(1);	
		when LU2_RR =>		-- RR operation ***************************************************
			ALU_FLAGS.C := OPER(0);
			RESULT := ALU_FLAGS.C&OPER(7)&OPER(6)&OPER(5)&OPER(4)&OPER(3)&OPER(2)&OPER(1);
		when LU2_SRA =>		-- SRA operation **************************************************
			ALU_FLAGS.C := OPER(0);
			RESULT := OPER(7)&OPER(7)&OPER(6)&OPER(5)&OPER(4)&OPER(3)&OPER(2)&OPER(1);					
		when LU2_SWAP =>	-- SWAP operation *************************************************
			RESULT := OPER(3)&OPER(2)&OPER(1)&OPER(0)&OPER(7)&OPER(6)&OPER(5)&OPER(4);
		when LU2_LD =>		-- LOAD operation *************************************************
			RESULT := OPER;
		when others =>		-- CLR operation **************************************************
			RESULT := x"00";
	end case;
	if (OPER(7)/=RESULT(7)) then ALU_FLAGS.V :='1'; else ALU_FLAGS.V :='0';
	end if;	
	if (RESULT=x"00") then ALU_FLAGS.Z := '1'; else ALU_FLAGS.Z := '0';
	end if;
	ALU_FLAGS.S := RESULT(7);
	return RESULT;
end LU2;

-- ADDER16 adds a signed 8-bit offset to a 16-bit address
function ADDER16
	(	ADDR16	: in std_logic_vector(15 downto 0);
		OFFSET	: in std_logic_vector(7 downto 0)) return std_logic_vector is
begin
	if (OFFSET(7)='0') then return ADDR16 + (x"00" & OFFSET);
	else return ADDR16 + (x"FF" & OFFSET);
	end if;
end ADDER16;

begin
	clock_out: process(CLK)
	begin
		CLK_OUTN <= not CLK;
		CLK_OUT <= CLK;
	end process;
	-- main process controls debugging and instruction fetching and decoding along
	main: process (CLK,RESET,DBG_RX)
	-- CPU state machine
	type Tcpu_state is (
		CPU_DECOD,
		CPU_INDRR,
		CPU_MUL, CPU_MUL1, CPU_MUL2,
		CPU_XADTOM,
		CPU_MTOXAD, CPU_MTOXAD2,
		CPU_XRTOM,
		CPU_XRRTORR, CPU_XRRTORR2,
		CPU_XRRTORR3, CPU_XRRTORR4,
		CPU_IMTOIRR, CPU_MTOIRR,		-- indirect and direct to indirect register pair addressing mode
		CPU_IRRS, CPU_IRRS2,
		CPU_XRRD, CPU_XRRD2, CPU_XRRD3,	-- indexed rr pair as destination
		CPU_XRRS, CPU_XRRS2, CPU_XRRS3,	-- indexed rr pair as source
		CPU_IND1, CPU_IND2,				-- indirect memory access
		CPU_ISMD1,						-- indirect source to memory destination
		CPU_TMA,						-- Two memory access instructions (register to/with register)
		CPU_OMA,						-- One memory access instructions (immediate to/with register)
		CPU_OMA2,						-- One memory access instructions (immediate to/with register) logic unit related
		CPU_DMAB,						-- Decrement address bus (for word access)
		CPU_LDW, CPU_LDW2,
		CPU_LDPTOIM, CPU_LDPTOIM2,
		CPU_LDPTOM, CPU_LDPTOM2,
		CPU_LDPTOM3, CPU_LDPTOM4,
		CPU_LDMTOP, CPU_LDMTOP2,
		CPU_BIT,
		CPU_IBTJ, CPU_BTJ,
		CPU_DJNZ,
		CPU_INDJUMP, CPU_INDJUMP2,
		CPU_TRAP, CPU_TRAP2,
		CPU_INDSTACK, CPU_INDSTACK2,
		CPU_STACK, CPU_STACK1,
		CPU_STACK2, CPU_STACK3,
		CPU_UNSTACK, CPU_UNSTACK2,
		CPU_UNSTACK3,
		CPU_STORE,						-- store results, no change to the flags
		CPU_VECTOR, CPU_VECTOR2,
		CPU_HALTED,
		CPU_RESET,
		CPU_ILLEGAL
	);
	type Tfetch_state is (
		F_ADDR,		-- instruction queue is initializing, reset pointers and empty queue
		F_READ		-- instruction queue is fetching opcodes
	);
	type Tdbg_uartrxstate is (
		DBGST_NOSYNC,			-- debug UART receiver is not synchronized to the host
		DBGST_WAITSTART,		-- debug UART receiver is waiting for a 0x80 char
		DBGST_MEASURING,		-- debug UART receiver is measuring a possible sync char
		DBGST_IDLE,				-- debug UART receiver is synchronized and awaiting commands
		DBGST_START,			-- debug UART received a start bit
		DBGST_RECEIVING,		-- debug UART is receiving new command/data
		DBGST_ERROR				-- debug UART receiver is in error state
	);
	type Tdbg_uarttxstate is (
		DBGTX_INIT,				-- debug UART transmitter is initializing
		DBGTX_IDLE,				-- debug UART transmitter is waiting new data to transmit
		DBGTX_START,			-- debug UART transmitter is sending a start bit
		DBGTX_TRASMITTING,		-- debug UART transmitter is sending data
		DBGTX_BREAK, 			-- debug UART transmitter is preparing to send a break
		DBGTX_BREAK2			-- debug UART is waiting for the break complete
	);
	type Tdbg_command is (
		DBG_WAIT_CMD,							-- debugger is waiting for commands
		DBG_SEND_REV, DBG_SEND_REV2,			-- debugger is processing a read revision command
		DBG_SEND_STATUS,						-- debugger is processing a read OCDST command
		DBG_WRITE_CTRL,							-- debugger is processing a write OCDCTRL command
		DBG_SEND_CTRL,							-- debugger is processing a read OCDCTRL command
		DBG_WRITE_PC, DBG_WRITE_PC2,			-- debugger is processing a PC write command
		DBG_SEND_PC, DBG_SEND_PC2,				-- debugger is processing a PC read command
		DBG_WRITE_REG, DBG_READ_REG,			-- debugger is processing a read/write to registers
		DBG_REG, DBG_REG2, DBG_REG3, 			-- debugger is processing a read/write to registers
		DBG_REG4, DBG_REG5,						-- debugger is processing a read/write to registers
		DBG_WRITE_PROGMEM, DBG_READ_PROGMEM,	-- debugger is processing a read/write to program memory
		DBG_PROGMEM, DBG_PROGMEM2, 				-- debugger is processing a read/write to program memory
		DBG_PROGMEM3, DBG_PROGMEM4, 			-- debugger is processing a read/write to program memory
		DBG_PROGMEM5, DBG_PROGMEM6,				-- debugger is processing a read/write to program memory
		DBG_STEP, 								-- debugger is processing a step command
		DBG_STUFF, 								-- debugger is processing a stuff command
		DBG_EXEC, DBG_EXEC2, DBG_EXEC3			-- debugger is processing a execute command
	);
	type Tdbg_uart is record
		RX_STATE	: Tdbg_uartrxstate;
		TX_STATE	: Tdbg_uarttxstate;
		RX_DONE		: std_logic;						-- new data is available
		TX_EMPTY	: std_logic;						-- tx buffer is empty
		DBG_SYNC	: std_logic;						-- debugger is synchronized to host
		WRT			: std_logic;						-- write/read command flag
		LAST_SMP	: std_logic;						-- last sample read from DBG_RX pin
		SIZE		: std_logic_vector(15 downto 0);	-- 16-bit size of command
		TXSHIFTREG	: std_logic_vector(8 downto 0);		-- transmitter shift register
		RXSHIFTREG	: std_logic_vector(8 downto 0);		-- receiver shift register
		TX_DATA		: std_logic_vector(7 downto 0);		-- TX buffer
		RX_DATA		: std_logic_vector(7 downto 0);		-- RX buffer
		RXCNT		: integer range 0 to 15;			-- received bit counter
		TXCNT		: integer range 0 to 15;			-- transmitted bit counter
		BAUDPRE		: integer range 0 to 2;				-- baud prescaler
		BAUDCNTRX	: std_logic_vector(11 downto 0);	-- RX baud divider
		BAUDCNTTX	: std_logic_vector(11 downto 0);	-- TX baud divider
		BITTIMERX	: std_logic_vector(11 downto 0);	-- RX bit-time register (1/2 bit-time)
		BITTIMETX	: std_logic_vector(11 downto 0);	-- TX bit-time register
	end record;
	variable CPU_STATE 		: TCPU_STATE;  						-- current CPU state 
	variable DBG_UART		: Tdbg_uart;
	variable DBG_CMD		: Tdbg_command;
	variable CAN_FETCH		: std_logic;						-- controls whether the instruction queue can actually fetch opcodes
	variable LU_INSTRUCTION	: std_logic;						-- indicates a LU2-related instruction
	variable WORD_DATA		: std_logic;						-- indicates a 16-bit data instruction
	variable PC 			: std_logic_vector(15 downto 0);	-- program counter
	variable FETCH_ADDR		: std_logic_vector(15 downto 0);	-- next address to be fetched
	variable DEST_ADDR16	: std_logic_vector(15 downto 0);	-- temporary 16-bit destination address
	variable DEST_ADDR		: std_logic_vector(11 downto 0);	-- temporary 12-bit destination address
	variable TEMP_DATA		: std_logic_vector(7 downto 0);		-- temporary 8-bit data
	variable OLD_IRQ0		: std_logic_vector(7 downto 0);		-- previous state of IRQs
	variable INTVECT		: std_logic_vector(7 downto 0);		-- current interrupt vector (lower 8-bits)
	variable RESULT			: std_logic_vector(7 downto 0);		-- temporary 8-bit result
	variable TEMP_OP		: std_logic_vector(3 downto 0);		-- ALU/LU2 operation code
	variable ATM_COUNTER	: integer range 0 to 3;				-- temporary interrupt disable counter (ATM instruction)
	variable NUM_BYTES		: integer range 0 to 5;				-- number of bytes decoded
	variable CKDIVIDER		: integer range 0 to 2;
	type Tinstructionqueue is record
		WRPOS				: integer range 0 to 7;				-- instruction queue write pointer
		RDPOS				: integer range 0 to 7;				-- instruction queue read pointer
		CNT					: integer range 0 to 7;				-- instruction queue available bytes
		FETCH_STATE			: tfetch_state;		
		QUEUE				: Tinstqueue;
		FULL				: std_logic;						-- indicates whether the queue is full or not
	end record;
	variable IQUEUE			: Tinstructionqueue;
	type Tocdcr is record
		DBGMODE				: std_logic;
		BRKEN				: std_logic;
		DBGACK				: std_logic;
		BRKLOOP				: std_logic;
		RST					: std_logic;
	end record;
	variable OCDCR			: Tocdcr;
	type Tocdflags is record
		SINGLESTEP			: std_logic;
	end record;
	variable OCD			: Tocdflags;
		
	begin
		if (reset='1') then	-- reset operations		
			IAB <= x"0002";
			MAB <= x"000";
			PWDB <= x"00";
			SP := x"000";
			RP := x"00";
			WR <= '0';
			PGM_WR <= '0';
			STOP <= '0';
			CAN_FETCH := '1';
			FETCH_ADDR := x"0000";
			DBG_UART.RX_STATE := DBGST_NOSYNC;
			DBG_UART.TX_STATE := DBGTX_INIT;
			OCDCR.DBGMODE := '0';
			OCDCR.BRKEN := '0';
			OCDCR.DBGACK := '0';
			OCDCR.BRKLOOP := '0';				
			OCD.SINGLESTEP := '0';
			OCDCR.RST := '0';
			RXSYNC1 <= '1';
			RXSYNC2 <= '1';
			DBG_UART.LAST_SMP := '1';
			IQUEUE.FETCH_STATE := F_ADDR;
			IRQE := '0';
			IRQ0 := x"00";
			OLD_IRQ0 := x"00";
			IRQ0ENH := x"00";
			IRQ0ENL := x"00";
			ATM_COUNTER := 0;
			CKDIVIDER := 0;
			CPU_STATE := CPU_VECTOR;			
		elsif (rising_edge(clk)) then
			if (OLD_IRQ0(0)='0' and INT0='1') then IRQ0(0) := '1'; end if;
			if (OLD_IRQ0(1)='0' and INT1='1') then IRQ0(1) := '1'; end if;
			if (OLD_IRQ0(2)='0' and INT2='1') then IRQ0(2) := '1'; end if;
			if (OLD_IRQ0(3)='0' and INT3='1') then IRQ0(3) := '1'; end if;
			if (OLD_IRQ0(4)='0' and INT4='1') then IRQ0(4) := '1'; end if;
			if (OLD_IRQ0(5)='0' and INT5='1') then IRQ0(5) := '1'; end if;
			if (OLD_IRQ0(6)='0' and INT6='1') then IRQ0(6) := '1'; end if;
			OLD_IRQ0 := INT7&INT6&INT5&INT4&INT3&INT2&INT1&INT0;
			CKDIVIDER := CKDIVIDER + 1;
			if (CKDIVIDER=0) then
				WR <= '0';
				PGM_WR <= '0';
				
				-- This is the instruction queue FSM
				if (CAN_FETCH='1') then
					if (IQUEUE.FETCH_STATE=F_ADDR) then
						FETCH_ADDR := PC;
						IAB <= PC;
						IQUEUE.WRPOS := 0;
						IQUEUE.RDPOS := 0;
						IQUEUE.CNT := 0;						
						IQUEUE.FETCH_STATE := F_READ;
					else
						if (IQUEUE.FULL='0') then
							IQUEUE.QUEUE(IQUEUE.WRPOS) := IDB;
							FETCH_ADDR := FETCH_ADDR + 1;
							IAB <= FETCH_ADDR;
							IQUEUE.WRPOS := IQUEUE.WRPOS + 1;
							IQUEUE.CNT := IQUEUE.CNT + 1;
						end if;
					end if;
				end if;
				if (IQUEUE.CNT=7) then IQUEUE.FULL:='1'; else IQUEUE.FULL:='0'; 
				end if;
				-- This is the end of instruction queue FSM	
								
				-- These are the Debugger FSMs
				DBG_UART.BAUDPRE := DBG_UART.BAUDPRE+1;
				if (DBG_UART.BAUDPRE=0) then 
					DBG_UART.BAUDCNTRX := DBG_UART.BAUDCNTRX+1;
					DBG_UART.BAUDCNTTX := DBG_UART.BAUDCNTTX+1;
				end if;
				RXSYNC2 <= DBG_RX;
				RXSYNC1 <= RXSYNC2;		
				case DBG_UART.RX_STATE is
					when DBGST_NOSYNC =>
						DBG_UART.DBG_SYNC := '0';
						DBG_UART.RX_DONE := '0';
						DBG_CMD := DBG_WAIT_CMD;
						DBG_UART.RX_STATE := DBGST_WAITSTART;
					when DBGST_WAITSTART =>
						if (RXSYNC1='0' and DBG_UART.LAST_SMP='1') then
							DBG_UART.RX_STATE := DBGST_MEASURING;
							DBG_UART.BAUDCNTRX := x"000";
						end if;
					when DBGST_MEASURING =>
						if (DBG_UART.BAUDCNTRX/=x"FFF") then 
							if (RXSYNC1='1') then
								DBG_UART.DBG_SYNC := '1';
								DBG_UART.RX_STATE := DBGST_IDLE;
								DBG_UART.BITTIMERX := "0000"&DBG_UART.BAUDCNTRX(11 downto 4);
								DBG_UART.BITTIMETX := "000"&DBG_UART.BAUDCNTRX(11 downto 3);
							end if;
						else
							DBG_UART.RX_STATE := DBGST_NOSYNC;
						end if;
					when DBGST_IDLE =>
						DBG_UART.BAUDCNTRX:=x"000";
						DBG_UART.RXCNT:=0;
						if (RXSYNC1='0' and DBG_UART.LAST_SMP='1') then	-- it's a start bit
							DBG_UART.RX_STATE := DBGST_START;				
						end if;
					when DBGST_START =>
						if (DBG_UART.BAUDCNTRX=DBG_UART.BITTIMERX) then
							DBG_UART.BAUDCNTRX:=x"000";
							if (RXSYNC1='0') then
								DBG_UART.RX_STATE := DBGST_RECEIVING;
							else
								DBG_UART.RX_STATE := DBGST_ERROR;
								DBG_UART.TX_STATE := DBGTX_BREAK;
							end if;
						end if;
					when DBGST_RECEIVING =>
						if (DBG_UART.BAUDCNTRX=DBG_UART.BITTIMETX) then
							DBG_UART.BAUDCNTRX:=x"000";
							-- one bit time elapsed, sample RX input
							DBG_UART.RXSHIFTREG := RXSYNC1 & DBG_UART.RXSHIFTREG(8 downto 1);
							DBG_UART.RXCNT := DBG_UART.RXCNT + 1;
							if (DBG_UART.RXCNT=9) then
								if (RXSYNC1='1') then
									-- if the stop bit is 1, rx is completed ok
									DBG_UART.RX_DATA := DBG_UART.RXSHIFTREG(7 downto 0);
									DBG_UART.RX_DONE := '1';
									DBG_UART.RX_STATE := DBGST_IDLE;
								else
									-- if the stop bit is 0, it is a break char, reset receiver
									DBG_UART.RX_STATE := DBGST_ERROR;
									DBG_UART.TX_STATE := DBGTX_BREAK;
								end if;
							end if;
						end if;
					when others =>
				end case;
				DBG_UART.LAST_SMP := RXSYNC1;
				case DBG_UART.TX_STATE is
					when DBGTX_INIT =>
						DBG_UART.TX_EMPTY := '1';
						DBG_UART.TX_STATE:=DBGTX_IDLE;
					when DBGTX_IDLE =>	-- UART is idle and not transmitting
						DBG_TX <= '1';
						if (DBG_UART.TX_EMPTY='0' and DBG_UART.DBG_SYNC='1') then	-- there is new data in TX_DATA register
							DBG_UART.BAUDCNTTX:=x"000";
							DBG_UART.TX_STATE := DBGTX_START;
						end if;
					when DBGTX_START =>
						if (DBG_UART.BAUDCNTTX=DBG_UART.BITTIMETX) then
							DBG_UART.BAUDCNTTX:=x"000";
							DBG_UART.TXSHIFTREG := '1'&DBG_UART.TX_DATA;
							DBG_UART.TXCNT := 10;
							DBG_UART.TX_STATE := DBGTX_TRASMITTING;
							DBG_TX <= '0';
						end if;
					when DBGTX_TRASMITTING =>	-- UART is shifting data
						if (DBG_UART.BAUDCNTTX=DBG_UART.BITTIMETX) then
							DBG_UART.BAUDCNTTX:=x"000";
							DBG_TX <= DBG_UART.TXSHIFTREG(0);
							DBG_UART.TXSHIFTREG := '1'&DBG_UART.TXSHIFTREG(8 downto 1);
							DBG_UART.TXCNT :=DBG_UART.TXCNT - 1;
							if (DBG_UART.TXCNT=0) then 
								DBG_UART.TX_STATE:=DBGTX_IDLE;
								DBG_UART.TX_EMPTY := '1';
							end if;
						end if;
					when DBGTX_BREAK =>					
						DBG_UART.BAUDCNTTX:=x"000";
						DBG_UART.TX_STATE:=DBGTX_BREAK2;
					when DBGTX_BREAK2 =>
						DBG_TX <= '0';
						DBG_UART.RX_STATE := DBGST_NOSYNC;
						if (DBG_UART.BAUDCNTTX=x"FFF") then	
							DBG_UART.TX_STATE:=DBGTX_INIT;
						end if;
				end case;		
				if (RXSYNC1='0') then DBG_TX <='0';
				end if;
				case DBG_CMD is
					when DBG_WAIT_CMD =>
						if (DBG_UART.RX_DONE='1') then
							case DBG_UART.RX_DATA is
								when DBGCMD_READ_REV =>		DBG_CMD := DBG_SEND_REV;
								when DBGCMD_READ_STATUS =>	DBG_CMD := DBG_SEND_STATUS;
								when DBGCMD_WRITE_CTRL =>	DBG_CMD := DBG_WRITE_CTRL;
								when DBGCMD_READ_CTRL =>	DBG_CMD := DBG_SEND_CTRL;
								when DBGCMD_WRITE_PC =>		DBG_CMD := DBG_WRITE_PC;
								when DBGCMD_READ_PC =>		DBG_CMD := DBG_SEND_PC;
								when DBGCMD_WRITE_REG =>	DBG_CMD := DBG_WRITE_REG;
								when DBGCMD_READ_REG =>		DBG_CMD := DBG_READ_REG;
								when DBGCMD_WRITE_PROGRAM=>	DBG_CMD := DBG_WRITE_PROGMEM;
								when DBGCMD_READ_PROGRAM=>	DBG_CMD := DBG_READ_PROGMEM;
								when DBGCMD_STEP =>			DBG_CMD := DBG_STEP;
								when DBGCMD_STUFF =>		DBG_CMD := DBG_STUFF;
								when DBGCMD_EXEC =>			DBG_CMD := DBG_EXEC;
								when others =>				
							end case;
							DBG_UART.RX_DONE:='0';
						end if;
					when DBG_SEND_REV =>
						if (DBG_UART.TX_EMPTY='1') then
							DBG_UART.TX_DATA:=x"01";
							DBG_UART.TX_EMPTY:='0';
							DBG_CMD := DBG_SEND_REV2;
						end if;
					when DBG_SEND_REV2 =>
						if (DBG_UART.TX_EMPTY='1') then
							DBG_UART.TX_DATA:=x"00";
							DBG_UART.TX_EMPTY:='0';
							DBG_CMD := DBG_WAIT_CMD;
						end if;
					when DBG_SEND_STATUS =>
						if (DBG_UART.TX_EMPTY='1') then
							DBG_UART.TX_DATA:=OCDCR.DBGMODE&HALT&"000000";
							DBG_UART.TX_EMPTY:='0';
							DBG_CMD := DBG_WAIT_CMD;
						end if;
					when DBG_WRITE_CTRL =>
						if (DBG_UART.RX_DONE='1') then
							DBG_UART.RX_DONE:='0';
							OCDCR.DBGMODE := DBG_UART.RX_DATA(7);
							OCDCR.BRKEN := DBG_UART.RX_DATA(6);
							OCDCR.DBGACK := DBG_UART.RX_DATA(5);
							OCDCR.BRKLOOP := DBG_UART.RX_DATA(4);
							OCDCR.RST := DBG_UART.RX_DATA(0);
							if (OCDCR.RST='1') then CPU_STATE:=CPU_RESET;
							end if;
							DBG_CMD := DBG_WAIT_CMD;
						end if;
					when DBG_SEND_CTRL =>
						if (DBG_UART.TX_EMPTY='1') then
							DBG_UART.TX_DATA:=OCDCR.DBGMODE&OCDCR.BRKEN&OCDCR.DBGACK&OCDCR.BRKLOOP&"000"&OCDCR.RST;
							DBG_UART.TX_EMPTY:='0';
							DBG_CMD := DBG_WAIT_CMD;
						end if;			
					when DBG_WRITE_PC =>
						if (DBG_UART.RX_DONE='1' and OCDCR.DBGMODE='1') then
							DBG_UART.RX_DONE:='0';
							CAN_FETCH := '0';
							PC(15 downto 8) := DBG_UART.RX_DATA;
							DBG_CMD := DBG_WRITE_PC2;
						end if;
					when DBG_WRITE_PC2 =>
						if (DBG_UART.RX_DONE='1') then
							DBG_UART.RX_DONE:='0';
							PC(7 downto 0) := DBG_UART.RX_DATA;
							IQUEUE.FETCH_STATE := F_ADDR;
							IQUEUE.CNT := 0;
							CAN_FETCH := '1';
							DBG_CMD := DBG_WAIT_CMD;
						end if;
					when DBG_SEND_PC =>
						if (DBG_UART.TX_EMPTY='1') then
							DBG_UART.TX_DATA:=PC(15 downto 8);
							DBG_UART.TX_EMPTY:='0';
							DBG_CMD := DBG_SEND_PC2;
						end if;
					when DBG_SEND_PC2 =>
						if (DBG_UART.TX_EMPTY='1') then
							DBG_UART.TX_DATA:=PC(7 downto 0);
							DBG_UART.TX_EMPTY:='0';
							DBG_CMD := DBG_WAIT_CMD;
						end if;					
					when DBG_WRITE_REG =>
						DBG_UART.WRT := '1';
						DBG_CMD := DBG_REG;
					when DBG_READ_REG =>
						DBG_UART.WRT := '0';
						DBG_CMD := DBG_REG;					
					when DBG_REG =>
						if (DBG_UART.RX_DONE='1' and OCDCR.DBGMODE='1') then
							CAN_FETCH := '0';
							MAB(11 downto 8) <= DBG_UART.RX_DATA(3 downto 0);
							DBG_UART.RX_DONE:='0';
							DBG_CMD := DBG_REG2;
						end if;
					when DBG_REG2 =>
						if (DBG_UART.RX_DONE='1') then
							MAB(7 downto 0) <= DBG_UART.RX_DATA;
							DBG_UART.RX_DONE:='0';
							DBG_CMD := DBG_REG3;
						end if;
					when DBG_REG3 =>
						if (DBG_UART.RX_DONE='1') then
							DBG_UART.SIZE := x"00"&DBG_UART.RX_DATA;
							DBG_UART.RX_DONE:='0';
							DBG_CMD := DBG_REG4;
						end if;
					when DBG_REG4 =>
						if (OCDCR.DBGMODE='1') then
							if (DBG_UART.WRT='1') then
								if (DBG_UART.RX_DONE='1') then
									CPU_STATE := CPU_OMA;
									TEMP_DATA := DBG_UART.RX_DATA;
									DBG_UART.RX_DONE:='0';
									DBG_CMD := DBG_REG5;
								end if;
							else
								if (DBG_UART.TX_EMPTY='1') then
									DBG_UART.TX_DATA:=DATAREAD(MAB);
									DBG_UART.TX_EMPTY:='0';
									DBG_CMD := DBG_REG5;
								end if;
							end if;
						end if;
					when DBG_REG5 =>
						if (CPU_STATE=CPU_DECOD) then
							MAB <= MAB + 1;
							DBG_UART.SIZE := DBG_UART.SIZE - 1;
							if (DBG_UART.SIZE=x"0000") then
								DBG_CMD := DBG_WAIT_CMD; 
								CAN_FETCH := '1';
							else DBG_CMD := DBG_REG4;
							end if;
						end if;
					when DBG_WRITE_PROGMEM =>
						DBG_UART.WRT := '1';
						DBG_CMD := DBG_PROGMEM;
					when DBG_READ_PROGMEM =>
						DBG_UART.WRT := '0';
						DBG_CMD := DBG_PROGMEM;					
					when DBG_PROGMEM =>
						if (DBG_UART.RX_DONE='1') then
							CAN_FETCH := '0';
							IAB(15 downto 8) <= DBG_UART.RX_DATA;
							DBG_UART.RX_DONE:='0';
							DBG_CMD := DBG_PROGMEM2;
						end if;
					when DBG_PROGMEM2 =>
						if (DBG_UART.RX_DONE='1') then
							IAB(7 downto 0) <= DBG_UART.RX_DATA;
							DBG_UART.RX_DONE:='0';
							DBG_CMD := DBG_PROGMEM3;
						end if;					
					when DBG_PROGMEM3 =>
						if (DBG_UART.RX_DONE='1') then
							DBG_UART.SIZE(15 downto 8) := DBG_UART.RX_DATA;
							DBG_UART.RX_DONE:='0';
							DBG_CMD := DBG_PROGMEM4;
						end if;					
					when DBG_PROGMEM4 =>
						if (DBG_UART.RX_DONE='1') then
							DBG_UART.SIZE(7 downto 0) := DBG_UART.RX_DATA;
							DBG_UART.RX_DONE:='0';
							DBG_CMD := DBG_PROGMEM5;
						end if;
					when DBG_PROGMEM5 =>
						if (DBG_UART.WRT='1') then
							if (DBG_UART.RX_DONE='1') then
								PWDB <= DBG_UART.RX_DATA;
								DBG_UART.RX_DONE:='0';
								PGM_WR <= '1';
								DBG_CMD := DBG_PROGMEM6;
							end if;
						else
							if (DBG_UART.TX_EMPTY='1') then
								DBG_UART.TX_DATA:=IDB;
								DBG_UART.TX_EMPTY:='0';
								DBG_CMD := DBG_PROGMEM6;
							end if;
						end if;
					when DBG_PROGMEM6 =>
						IAB <= IAB + 1;
						DBG_UART.SIZE := DBG_UART.SIZE - 1;
						if (DBG_UART.SIZE=x"0000") then 
							DBG_CMD := DBG_WAIT_CMD; 
							CAN_FETCH := '1';
							IQUEUE.CNT := 0;
							IQUEUE.FETCH_STATE := F_ADDR;						
						else DBG_CMD := DBG_PROGMEM5;
						end if;
					when DBG_STEP =>
						OCD.SINGLESTEP:='1';
						IQUEUE.FETCH_STATE := F_ADDR;
						DBG_CMD := DBG_WAIT_CMD;
					when DBG_STUFF =>
						if (DBG_UART.RX_DONE='1' and OCDCR.DBGMODE='1') then
							IQUEUE.QUEUE(IQUEUE.RDPOS) := DBG_UART.RX_DATA;
							DBG_UART.RX_DONE:='0';
							DBG_CMD := DBG_STEP;
						end if;
					when DBG_EXEC =>
						if (OCDCR.DBGMODE='1') then
							OCD.SINGLESTEP:='1';
							CAN_FETCH:='0';
							IQUEUE.CNT := 0;
							IQUEUE.FETCH_STATE := F_ADDR;
						end if;
						DBG_CMD := DBG_EXEC2;
					when DBG_EXEC2 =>
						if (DBG_UART.RX_DONE='1') then
							if (OCDCR.DBGMODE='0') then DBG_CMD := DBG_WAIT_CMD;
							else
								IQUEUE.QUEUE(IQUEUE.WRPOS) := DBG_UART.RX_DATA;
								DBG_UART.RX_DONE:='0';
								IQUEUE.WRPOS := IQUEUE.WRPOS + 1;
								IQUEUE.CNT := IQUEUE.CNT + 1;
								DBG_CMD := DBG_EXEC3;
							end if;
						end if;
					when DBG_EXEC3 =>
						if (OCD.SINGLESTEP='1') then DBG_CMD := DBG_EXEC2; else 
							DBG_CMD := DBG_WAIT_CMD;
							CAN_FETCH:='0';
							IQUEUE.FETCH_STATE := F_ADDR;
						end if;
					when others =>
				end case;
				-- This is the end of the debugger code		
				
				-- This is the main instruction decoder
				case CPU_STATE IS	
				when CPU_DECOD =>
					TEMP_OP := ALU_LD;					-- default ALU operation is load
					LU_INSTRUCTION := '0';				-- default is ALU operation (instead of LU2)
					WORD_DATA := '0';					-- default is 8-bit operation
					INTVECT := x"00";					-- default vector is 0x00
					NUM_BYTES := 0;						-- default instruction length is 0 bytes
					if (ATM_COUNTER/=3) then ATM_COUNTER := ATM_COUNTER+1;
					else	-- interrupt processing *****************************************************************************
						if (IRQE='1') then	-- if interrupts are enabled
							-- first the highest priority interrupts
							if ((IRQ0(7)='1') and (IRQ0ENH(7)='1') and IRQ0ENL(7)='1') then
								INTVECT:=x"08";
								IRQ0(7):='0';
							elsif ((IRQ0(6)='1') and (IRQ0ENH(6)='1') and IRQ0ENL(6)='1') then 
								INTVECT:=x"0A";
								IRQ0(6):='0';
							elsif ((IRQ0(5)='1') and (IRQ0ENH(5)='1') and IRQ0ENL(5)='1') then 
								INTVECT:=x"0C";
								IRQ0(5):='0';
							elsif ((IRQ0(4)='1') and (IRQ0ENH(4)='1') and IRQ0ENL(4)='1') then 
								INTVECT:=x"0E";
								IRQ0(4):='0';
							elsif ((IRQ0(3)='1') and (IRQ0ENH(3)='1') and IRQ0ENL(3)='1') then 
								INTVECT:=x"10";
								IRQ0(3):='0';
							elsif ((IRQ0(2)='1') and (IRQ0ENH(2)='1') and IRQ0ENL(2)='1') then 
								INTVECT:=x"12";
								IRQ0(2):='0';
							elsif ((IRQ0(1)='1') and (IRQ0ENH(1)='1') and IRQ0ENL(1)='1') then 
								INTVECT:=x"14";
								IRQ0(1):='0';
							elsif ((IRQ0(0)='1') and (IRQ0ENH(0)='1') and IRQ0ENL(0)='1') then 
								INTVECT:=x"16";
								IRQ0(0):='0';
							-- now priority level 2 interrupts
							elsif ((IRQ0(7)='1') and (IRQ0ENH(7)='1') and IRQ0ENL(7)='0') then
								INTVECT:=x"08";
								IRQ0(7):='0';
							elsif ((IRQ0(6)='1') and (IRQ0ENH(6)='1') and IRQ0ENL(6)='0') then 
								INTVECT:=x"0A";
								IRQ0(6):='0';
							elsif ((IRQ0(5)='1') and (IRQ0ENH(5)='1') and IRQ0ENL(5)='0') then 
								INTVECT:=x"0C";
								IRQ0(5):='0';
							elsif ((IRQ0(4)='1') and (IRQ0ENH(4)='1') and IRQ0ENL(4)='0') then 
								INTVECT:=x"0E";
								IRQ0(4):='0';
							elsif ((IRQ0(3)='1') and (IRQ0ENH(3)='1') and IRQ0ENL(3)='0') then 
								INTVECT:=x"10";
								IRQ0(3):='0';
							elsif ((IRQ0(2)='1') and (IRQ0ENH(2)='1') and IRQ0ENL(2)='0') then 
								INTVECT:=x"12";
								IRQ0(2):='0';
							elsif ((IRQ0(1)='1') and (IRQ0ENH(1)='1') and IRQ0ENL(1)='0') then 
								INTVECT:=x"14";
								IRQ0(1):='0';
							elsif ((IRQ0(0)='1') and (IRQ0ENH(0)='1') and IRQ0ENL(0)='0') then 
								INTVECT:=x"16";
								IRQ0(0):='0';
							-- now priority level 1 interrupts
							elsif ((IRQ0(7)='1') and (IRQ0ENH(7)='0') and IRQ0ENL(7)='1') then
								INTVECT:=x"08";
								IRQ0(7):='0';
							elsif ((IRQ0(6)='1') and (IRQ0ENH(6)='0') and IRQ0ENL(6)='1') then 
								INTVECT:=x"0A";
								IRQ0(6):='0';
							elsif ((IRQ0(5)='1') and (IRQ0ENH(5)='0') and IRQ0ENL(5)='1') then 
								INTVECT:=x"0C";
								IRQ0(5):='0';
							elsif ((IRQ0(4)='1') and (IRQ0ENH(4)='0') and IRQ0ENL(4)='1') then 
								INTVECT:=x"0E";
								IRQ0(4):='0';
							elsif ((IRQ0(3)='1') and (IRQ0ENH(3)='0') and IRQ0ENL(3)='1') then 
								INTVECT:=x"10";
								IRQ0(3):='0';
							elsif ((IRQ0(2)='1') and (IRQ0ENH(2)='0') and IRQ0ENL(2)='1') then 
								INTVECT:=x"12";
								IRQ0(2):='0';
							elsif ((IRQ0(1)='1') and (IRQ0ENH(1)='0') and IRQ0ENL(1)='1') then 
								INTVECT:=x"14";
								IRQ0(1):='0';
							elsif ((IRQ0(0)='1') and (IRQ0ENH(0)='0') and IRQ0ENL(0)='1') then 
								INTVECT:=x"16";
								IRQ0(0):='0';						
							end if;
							if (INTVECT/=x"00") then
								DEST_ADDR16 := PC;
								IAB <= x"00"&INTVECT;	-- build the address of the interrupt vector
								SP := SP - 1;			-- prepare stack pointer by decrementing it
								MAB <= SP;				-- put SP on MAB
								CAN_FETCH := '0';		-- disable instruction fetching
								IQUEUE.CNT := 0;		-- empty instruction queue
								STOP <= '0';			-- disable stop bit
								LU_INSTRUCTION := '1';	-- the stacking uses this bit to flag it is an interrupt stacking operation
								CPU_STATE := CPU_STACK;
							end if;
						end if;
					end if;
					
					if (OCDCR.DBGMODE='0' or (OCDCR.DBGMODE='1' and OCD.SINGLESTEP='1')) then
						------------------------------------------------------------------------------------------------------------------------------
						--**************************************************************************************************************************--
						--                                                     5-byte instructions                                                  --
						--**************************************************************************************************************************--
						------------------------------------------------------------------------------------------------------------------------------
						if (IQUEUE.CNT>=5) then
							---------------------------------------------------------------------------------------------------- 2nd page instructions
							if (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"1F") then
								---------------------------------------------------------------------------------------------- CPC ER2,ER1 instruction
								if (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"A8") then
									MAB <= ADDRESSER12((IQUEUE.QUEUE(IQUEUE.RDPOS+3)(3 downto 0)) & IQUEUE.QUEUE(IQUEUE.RDPOS+4));
									DEST_ADDR := ADDRESSER12((IQUEUE.QUEUE(IQUEUE.RDPOS+2)) & IQUEUE.QUEUE(IQUEUE.RDPOS+3)(7 downto 4));
									TEMP_OP := ALU_CPC;
									NUM_BYTES := 5;
									CPU_STATE := CPU_TMA;
								---------------------------------------------------------------------------------------------- CPC IMM,ER1 instruction
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"A9") then
									MAB <= ADDRESSER12((IQUEUE.QUEUE(IQUEUE.RDPOS+3)(3 downto 0)) & IQUEUE.QUEUE(IQUEUE.RDPOS+4));
									TEMP_DATA := IQUEUE.QUEUE(IQUEUE.RDPOS+2);
									TEMP_OP := ALU_CPC;
									NUM_BYTES := 5;
									CPU_STATE := CPU_OMA;
								--------------------------------------------------------------------------------------------- LDWX ER1,ER2 instruction
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"E8") then
									MAB <= ADDRESSER12((IQUEUE.QUEUE(IQUEUE.RDPOS+3)(3 downto 0)) & IQUEUE.QUEUE(IQUEUE.RDPOS+4));
									DEST_ADDR := ADDRESSER12((IQUEUE.QUEUE(IQUEUE.RDPOS+2)) & IQUEUE.QUEUE(IQUEUE.RDPOS+3)(7 downto 4));
									NUM_BYTES := 5;
									CPU_STATE := CPU_LDW;						
								end if;
							end if;
						end if;
						
						------------------------------------------------------------------------------------------------------------------------------
						--**************************************************************************************************************************--
						--                                                     4-byte instructions                                                  --
						--**************************************************************************************************************************--
						------------------------------------------------------------------------------------------------------------------------------
						if (IQUEUE.CNT>=4) then
							if (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"9") then	------------------------------------------------ column 9 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"8" =>	------------------------------------------------------------------------- LDX rr1,r2,X instruction
									when x"9" =>	------------------------------------------------------------------------ LEA rr1,rr2,X instruction
									when x"C" =>
										CPU_STATE := CPU_ILLEGAL;
									when x"D" =>
										CPU_STATE := CPU_ILLEGAL;
									when x"F" =>
										CPU_STATE := CPU_ILLEGAL;
									when others =>	-------------------------------------------------------------- IM,ER1 addressing mode instructions
										MAB <= ADDRESSER12((IQUEUE.QUEUE(IQUEUE.RDPOS+2)(3 downto 0)) & IQUEUE.QUEUE(IQUEUE.RDPOS+3));
										TEMP_DATA := IQUEUE.QUEUE(IQUEUE.RDPOS+1);
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 4;
										CPU_STATE := CPU_OMA;																		
								end case;						
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"8") then	-------------------------------------------- column 8 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"8" =>	------------------------------------------------------------------------- LDX r1,rr2,X instruction
									when x"9" =>	-------------------------------------------------------------------------- LEA r1,r2,X instruction
									when x"C" =>	-------------------------------------------------------------------------------- PUSHX instruction
									when x"D" =>	--------------------------------------------------------------------------------- POPX instruction
									when x"F" =>
										CPU_STATE := CPU_ILLEGAL;
									when others =>	------------------------------------------------------------- ER2,ER1 addressing mode instructions
										MAB <= ADDRESSER12((IQUEUE.QUEUE(IQUEUE.RDPOS+1)) & IQUEUE.QUEUE(IQUEUE.RDPOS+2)(7 downto 4));
										DEST_ADDR := ADDRESSER12((IQUEUE.QUEUE(IQUEUE.RDPOS+2)(3 downto 0)) & IQUEUE.QUEUE(IQUEUE.RDPOS+3));
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 4;
										CPU_STATE := CPU_TMA;																			
								end case;
							---------------------------------------------------------------------------------------------------- 2nd page instructions
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"1F") then
								------------------------------------------------------------------------------------------------ CPC R2,R1 instruction
								TEMP_OP := ALU_CPC;
								if (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"A4") then
									MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+3));
									DEST_ADDR := ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+2));
									NUM_BYTES := 4;
									CPU_STATE := CPU_TMA;
								----------------------------------------------------------------------------------------------- CPC IR2,R1 instruction
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"A5") then
									DEST_ADDR := ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+3));
									MAB <= RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
									NUM_BYTES := 4;
									CPU_STATE := CPU_ISMD1;
								----------------------------------------------------------------------------------------------- CPC R1,IMM instruction
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"A6") then
									MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+2));
									TEMP_DATA := IQUEUE.QUEUE(IQUEUE.RDPOS+3);
									NUM_BYTES := 4;
									CPU_STATE := CPU_OMA;
								---------------------------------------------------------------------------------------------- CPC IR1,IMM instruction
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"A7") then
									MAB <= RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
									TEMP_DATA := IQUEUE.QUEUE(IQUEUE.RDPOS+3);
									NUM_BYTES := 4;
									CPU_STATE := CPU_IND1;						
								end if;
							end if;
						end if;
						
						------------------------------------------------------------------------------------------------------------------------------
						--**************************************************************************************************************************--
						--                                                     3-byte instructions                                                  --
						--**************************************************************************************************************************--
						------------------------------------------------------------------------------------------------------------------------------
						if (IQUEUE.CNT>=3) then			
							if (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"D") then	-------------------------------------------------- JP cc,DirectAddress
								if (CONDITIONCODE(IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4))='1') then 
									PC := IQUEUE.QUEUE(IQUEUE.RDPOS+1) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
									IQUEUE.FETCH_STATE := F_ADDR;
								else
									NUM_BYTES := 3;
								end if;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"9") then	-------------------------------------------- column 9 instructions
								if (IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4)=x"8") then	----------------------------------------- LDX rr1,r2,X instruction
									MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));							-- source address				
									DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));						-- dest address
									RESULT := IQUEUE.QUEUE(IQUEUE.RDPOS+2);													-- RESULT = offset (X)
									NUM_BYTES := 3;
									CPU_STATE := CPU_XRRD;
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4)=x"9") then	------------------------------------ LEA rr1,rr2,X instruction
									MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));							-- source address				
									DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));						-- dest address
									RESULT := IQUEUE.QUEUE(IQUEUE.RDPOS+2);
									NUM_BYTES := 3;
									CPU_STATE := CPU_XRRTORR;
								end if;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"8") then	-------------------------------------------- column 8 instructions
								if (IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4)=x"8") then	----------------------------------------- LDX r1,rr2,X instruction
									MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));							-- source address				
									DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));						-- dest address
									RESULT := IQUEUE.QUEUE(IQUEUE.RDPOS+2);													-- RESULT = offset (X)
									NUM_BYTES := 3;
									CPU_STATE := CPU_XRRS;
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4)=x"9") then	-------------------------------------- LEA r1,r2,X instruction
									MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));							-- source address				
									DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));						-- dest address
									RESULT := IQUEUE.QUEUE(IQUEUE.RDPOS+2);
									NUM_BYTES := 3;
									CPU_STATE := CPU_XRTOM;						
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4)=x"C") then	---------------------------------------- PUSHX ER2 instruction
									SP := SP - 1;
									MAB <= ADDRESSER12(IQUEUE.QUEUE(IQUEUE.RDPOS+1)&IQUEUE.QUEUE(IQUEUE.RDPOS+2)(7 downto 4));
									DEST_ADDR := SP;
									NUM_BYTES := 3;
									CPU_STATE := CPU_TMA;
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4)=x"D") then	----------------------------------------- POPX ER2 instruction
									MAB <= SP;
									DEST_ADDR := ADDRESSER12(IQUEUE.QUEUE(IQUEUE.RDPOS+1)&IQUEUE.QUEUE(IQUEUE.RDPOS+2)(7 downto 4));
									SP := SP + 1;
									NUM_BYTES := 3;
									CPU_STATE := CPU_TMA;
								end if;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"7") then	-------------------------------------------- column 7 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"8" =>	------------------------------------------------------------------------- LDX IRR2,IR1 instruction
										MAB <= RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+1);
										DEST_ADDR := RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
										NUM_BYTES := 3;
										CPU_STATE := CPU_IRRS;
									when x"9" =>	------------------------------------------------------------------------- LDX IR2,IRR1 instruction
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										DEST_ADDR := RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
										NUM_BYTES := 3;
										CPU_STATE := CPU_IMTOIRR;
									when x"C" =>	--------------------------------------------------------------------------- LD r1,r2,X instruction
										MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));							-- source address				
										DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));						-- dest address
										RESULT := IQUEUE.QUEUE(IQUEUE.RDPOS+2);													-- RESULT = offset (X)
										NUM_BYTES := 3;
										CPU_STATE := CPU_XADTOM;
									when x"D" =>	--------------------------------------------------------------------------- LD r2,r1,X instruction
										MAB <= RP(3 downto 0) & RP(7 downto 4) & IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4);
										DEST_ADDR := RP(3 downto 0) & RP(7 downto 4) & IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0);
										RESULT := IQUEUE.QUEUE(IQUEUE.RDPOS+2);
										NUM_BYTES := 3;
										CPU_STATE := CPU_MTOXAD;
									when x"F" =>	------------------------------------------------------------------------ BTJ p,b,Ir1,X instruction
									when others =>	----------------------------------------------------------------------------- IR1,imm instructions
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										TEMP_DATA := IQUEUE.QUEUE(IQUEUE.RDPOS+2);
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 3;
										CPU_STATE := CPU_IND1;																				
								end case;						
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"6") then	-------------------------------------------- column 6 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"8" =>	-------------------------------------------------------------------------- LDX IRR2,R1 instruction
										MAB <= RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+1);
										DEST_ADDR := ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+2));
										NUM_BYTES := 3;
										LU_INSTRUCTION := '1';	-- in this mode this flag is used to signal the direct register addressing mode
										CPU_STATE := CPU_IRRS;								
									when x"9" =>	-------------------------------------------------------------------------- LDX R2,IRR1 instruction
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										DEST_ADDR := RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
										NUM_BYTES := 3;
										CPU_STATE := CPU_MTOIRR;
									when x"C" =>	-- illegal, decoded at 1-byte decoder
										--CPU_STATE := CPU_ILLEGAL;	-- uncommenting this adds +400 LEs to the design!!!
									when x"D" =>	------------------------------------------------------------------------------ CALL DA instruction
									when x"F" =>	------------------------------------------------------------------------- BTJ p,b,r1,X instruction
									when others =>	------------------------------------------------------------------------------ R1,imm instructions
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										TEMP_DATA := IQUEUE.QUEUE(IQUEUE.RDPOS+2);
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 3;
										CPU_STATE := CPU_OMA;																				
								end case;						
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"5") then	-------------------------------------------- column 5 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"8" =>	-------------------------------------------------------------------------- LDX Ir1,ER2 instruction
										MAB <= IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);				
										DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));					-- dest address
										NUM_BYTES := 3;
										CPU_STATE := CPU_IND2;
									when x"9" =>	-------------------------------------------------------------------------- LDX Ir2,ER1 instruction
										MAB <= RP(3 downto 0) & RP(7 downto 4) & IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4);
										DEST_ADDR := IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
										NUM_BYTES := 3;
										CPU_STATE := CPU_ISMD1;								
									when x"C" =>	------------------------------------------------------------------------- LDC Ir1,Irr2 instruction
									when x"D" =>	----------------------------------------------------------------------------- BSWAP R1 instruction
									when x"F" =>	---------------------------------------------------------------------------- LD R2,IR1 instruction
									when others =>	------------------------------------------------------------------------------ IR2,R1 instructions
										DEST_ADDR := ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+2));
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 3;
										CPU_STATE := CPU_ISMD1;																				
								end case;						
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"4") then	-------------------------------------------- column 4 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"8" =>	--------------------------------------------------------------------------- LDX r1,ER2 instruction
										MAB <= IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);			
										DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));					-- dest address
										NUM_BYTES := 3;
										CPU_STATE := CPU_TMA;							
									when x"9" =>	--------------------------------------------------------------------------- LDX r2,ER1 instruction
										MAB <= RP(3 downto 0) & RP(7 downto 4) & IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4);
										DEST_ADDR := IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
										NUM_BYTES := 3;
										CPU_STATE := CPU_TMA;							
									when x"C" =>	------------------------------------------------------------------------------ JP Irr1 instruction
									when x"D" =>	---------------------------------------------------------------------------- CALL Irr1 instruction
									when x"F" =>	----------------------------------------------------------------------------- MULT RR1 instruction
									when others =>	------------------------------------------------------------------------------- R2,R1 instructions
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										DEST_ADDR := ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+2));
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 3;
										CPU_STATE := CPU_TMA;																				
								end case;					
							end if;
							------------------------------------------------------------------------------------------------- BTJ p,b,r1,X instruction
							------------------------------------------------------------------------------------------------ BTJ p,b,Ir1,X instruction
							if (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"F6" or IQUEUE.QUEUE(IQUEUE.RDPOS)=x"F7") then
								MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));							-- source address				
								TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4);	-- TEMP_OP has the polarity (bit 3) and bit number (bits 2:0)
								RESULT := IQUEUE.QUEUE(IQUEUE.RDPOS+2);					-- RESULT has the offset X
								NUM_BYTES := 3;
								if (IQUEUE.QUEUE(IQUEUE.RDPOS)(0)='0') then CPU_STATE := CPU_BTJ;	else CPU_STATE := CPU_IBTJ;
								end if;
							---------------------------------------------------------------------------------------------------- LD R2,IR1 instruction
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"F5") then
								MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
								DEST_ADDR := RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
								NUM_BYTES := 3;
								CPU_STATE := CPU_IND2;						
							------------------------------------------------------------------------------------------------------ CALL DA instruction
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"D6") then
								DEST_ADDR16 := PC + 3;
								PC := IQUEUE.QUEUE(IQUEUE.RDPOS+1) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
								SP := SP - 1;
								MAB <= SP;				
								LU_INSTRUCTION := '0';	-- this is used to indicate wether the stacking is due to a CALL or INT, 0 for a CALL
								IQUEUE.FETCH_STATE := F_ADDR;
								CPU_STATE := CPU_STACK;
							---------------------------------------------------------------------------------------------------- 2nd page instructions
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"1F") then
								-------------------------------------------------------------------------------------------------- PUSH IM instruction
								if (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"70") then
									SP := SP - 1;
									MAB <= SP;
									TEMP_DATA := IQUEUE.QUEUE(IQUEUE.RDPOS+2);
									NUM_BYTES := 3;
									CPU_STATE := CPU_OMA;
								------------------------------------------------------------------------------------------------ CPC r1,r2 instruction
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"A2") then
									MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+2)(3 downto 0));							-- source address				
									DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+2)(7 downto 4));						-- dest address
									TEMP_OP := ALU_CPC;
									NUM_BYTES := 3;
									CPU_STATE := CPU_TMA;
								----------------------------------------------------------------------------------------------- CPC r1,Ir2 instruction
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"A3") then
									MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+2)(3 downto 0));							-- source address				
									DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+2)(7 downto 4));						-- dest address
									TEMP_OP := ALU_CPC;
									NUM_BYTES := 3;
									CPU_STATE := CPU_ISMD1;
								--------------------------------------------------------------------------------------------------- SRL R1 instruction
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"C0") then
									MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+2));
									TEMP_OP := LU2_SRL;
									NUM_BYTES := 3;
									CPU_STATE := CPU_OMA2;
								-------------------------------------------------------------------------------------------------- SRL IR1 instruction
								elsif (IQUEUE.QUEUE(IQUEUE.RDPOS+1)=x"C1") then
									MAB <= RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+2);
									TEMP_OP := LU2_SRL;
									NUM_BYTES := 3;
									CPU_STATE := CPU_IND1;
									LU_INSTRUCTION := '1';						
								end if;
							end if;
						end if;
						
						------------------------------------------------------------------------------------------------------------------------------
						--**************************************************************************************************************************--
						--                                                     2-byte instructions                                                  --
						--**************************************************************************************************************************--
						------------------------------------------------------------------------------------------------------------------------------
						if (IQUEUE.CNT>=2) then
							if (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"C") then	------------------------------------------------- LD r,IMM instruction
								MAB <= RP(3 downto 0) & RP(7 downto 4) & IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
								DATAWRITE(RP(3 downto 0) & RP(7 downto 4) & IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4),IQUEUE.QUEUE(IQUEUE.RDPOS+1));
								NUM_BYTES := 2;
								CPU_STATE := CPU_STORE;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"B") then	-------------------------------------------- JR cc,RelativeAddress
								PC := PC + 2;
								if (CONDITIONCODE(IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4))='1') then
									PC := ADDER16(PC,IQUEUE.QUEUE(IQUEUE.RDPOS+1));
									IQUEUE.FETCH_STATE := F_ADDR;
								else
									IQUEUE.RDPOS := IQUEUE.RDPOS + 2;
									IQUEUE.CNT := IQUEUE.CNT - 2;							
								end if;
								CPU_STATE := CPU_DECOD;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"A") then	------------------------------------------- DJNZ r,RelativeAddress
								MAB <= RP(3 downto 0) & RP(7 downto 4) & IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
								PC := PC + 2;
								DEST_ADDR16 := ADDER16(PC,IQUEUE.QUEUE(IQUEUE.RDPOS+1)); 					
								IQUEUE.RDPOS := IQUEUE.RDPOS + 2;
								IQUEUE.CNT := IQUEUE.CNT - 2;
								IQUEUE.FETCH_STATE := F_ADDR;
								CPU_STATE := CPU_DJNZ;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"3") then	-------------------------------------------- column 3 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"8" =>	------------------------------------------------------------------------ LDEI Ir1,Irr2 instruction
									when x"9" =>	------------------------------------------------------------------------ LDEI Ir2,Irr1 instruction
									when x"C" =>	------------------------------------------------------------------------ LDCI Ir1,Irr2 instruction
										RESULT(3 downto 0) := IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0);
										MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));
										NUM_BYTES := 2;
										CAN_FETCH := '0';
										LU_INSTRUCTION := '0';	-- indicates it is a read from program memory
										WORD_DATA := '1';		-- indicates it is a LDCI instruction
										CPU_STATE := CPU_LDPTOIM;
									when x"D" =>	------------------------------------------------------------------------ LDCI Ir2,Irr1 instruction
										RESULT(3 downto 0) := IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0);
										MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));
										NUM_BYTES := 2;
										CAN_FETCH := '0';
										LU_INSTRUCTION := '1';	-- indicates it is a write onto program memory
										WORD_DATA := '1';		-- indicates it is a LDCI instruction
										CPU_STATE := CPU_LDPTOIM;
									when x"F" =>	---------------------------------------------------------------------------- LD Ir1,r2 instruction
										MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));
										DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));
										NUM_BYTES := 2;
										CPU_STATE := CPU_IND2;
									when others =>	--------------------------------------------------------------------------- Ir2 to r1 instructions
										MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));							-- source address				
										DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));						-- dest address
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 2;
										CPU_STATE := CPU_ISMD1;						
								end case;						
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"2") then	-------------------------------------------- column 2 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"8" =>	-------------------------------------------------------------------------- LDE r1,Irr2 instruction
									when x"9" =>	-------------------------------------------------------------------------- LDE r2,Irr1 instruction
									when x"C" =>	-------------------------------------------------------------------------- LDC r1,Irr2 instruction
										MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));
										DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));
										NUM_BYTES := 2;
										CAN_FETCH := '0';
										LU_INSTRUCTION := '0';
										CPU_STATE := CPU_LDPTOM;
									when x"D" =>	-------------------------------------------------------------------------- LDC r2,Irr1 instruction
										MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));
										DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));
										NUM_BYTES := 2;
										CAN_FETCH := '0';
										LU_INSTRUCTION := '1';
										CPU_STATE := CPU_LDPTOM;
									when x"E" =>	--------------------------------------------------------------------------- BIT p,b,r1 instruction
										MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4);	-- TEMP_OP has the polarity (bit 3) and bit number (bits 2:0)
										RESULT := IQUEUE.QUEUE(IQUEUE.RDPOS+2);					-- RESULT has the offset X
										NUM_BYTES := 2;
										CPU_STATE := CPU_BIT;
									when x"F" =>	----------------------------------------------------------------------------- TRAP imm instruction
										MAB <= "000" & IQUEUE.QUEUE(IQUEUE.RDPOS+1) & '0';
										DEST_ADDR16 := PC + 2;
										NUM_BYTES := 2;
										CPU_STATE := CPU_TRAP;
									when others =>	---------------------------------------------------------------------------- r2 to r1 instructions
										MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0));							-- source address				
										DEST_ADDR := ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));						-- dest address
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 2;
										CPU_STATE := CPU_TMA;					
								end case;						
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"1") then	-------------------------------------------- column 1 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"0" =>	------------------------------------------------------------------------------ SRP IMM instruction	
										RP := IQUEUE.QUEUE(IQUEUE.RDPOS+1);
										NUM_BYTES := 2;
										CPU_STATE := CPU_DECOD;
									when x"5" =>	------------------------------------------------------------------------------ POP IR1 instruction
										MAB <= SP;
										DEST_ADDR := ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										SP := SP + 1;
										NUM_BYTES := 2;
										CPU_STATE := CPU_IND2;								
									when x"7" =>	----------------------------------------------------------------------------- PUSH IR1 instruction
										SP := SP - 1;
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										DEST_ADDR := SP;
										NUM_BYTES := 2;
										CPU_STATE := CPU_ISMD1;								
									when x"8" =>	--------------------------------------------------------------------------------- DECW instruction
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										TEMP_OP := LU2_DEC;
										WORD_DATA := '1';
										NUM_BYTES := 2;
										CPU_STATE := CPU_INDRR;							
									when x"A" =>	--------------------------------------------------------------------------------- INCW instruction
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										TEMP_OP := LU2_INC;
										WORD_DATA := '1';
										NUM_BYTES := 2;
										CPU_STATE := CPU_INDRR;
									when others =>	--------------------------------------------------------------------------------- IR1 instructions
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 2;
										CPU_STATE := CPU_IND1;
										LU_INSTRUCTION := '1';
								end case;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"0") then	-------------------------------------------- column 0 instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"0" =>	---------------------------------------------------------------------------------- BRK instruction	
										-- do nothing, BRK decoding is done in 1-byte instruction section
									when x"5" =>	---------------------------------------------------------------------------------- POP instruction
										MAB <= SP;
										DEST_ADDR := ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										SP := SP + 1;
										NUM_BYTES := 2;
										CPU_STATE := CPU_TMA;
									when x"7" =>	--------------------------------------------------------------------------------- PUSH instruction
										SP := SP - 1;
										MAB <= RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+1);
										DEST_ADDR := SP;
										NUM_BYTES := 2;
										CPU_STATE := CPU_TMA;							
									when x"8" =>	--------------------------------------------------------------------------------- DECW instruction
										DEST_ADDR := ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										MAB <= DEST_ADDR+1;
										TEMP_OP := LU2_DEC;
										WORD_DATA := '1';
										NUM_BYTES := 2;
										CPU_STATE := CPU_OMA2;							
									when x"A" =>	--------------------------------------------------------------------------------- INCW instruction
										DEST_ADDR := ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										MAB <= DEST_ADDR+1;
										TEMP_OP := LU2_INC;
										WORD_DATA := '1';
										NUM_BYTES := 2;
										CPU_STATE := CPU_OMA2;
									when others =>	---------------------------------------------------------------------------------- R1 instructions
										MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
										TEMP_OP := IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
										NUM_BYTES := 2;
										CPU_STATE := CPU_OMA2;
								end case;
							end if;	
							---------------------------------------------------------------------------------------------------------- MUL instruction 
							if (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"F4") then
								MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
								NUM_BYTES := 2;
								CPU_STATE := CPU_MUL;
							---------------------------------------------------------------------------------------------------- CALL IRR1 instruction
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"D4") then
								DEST_ADDR16 := PC + 2;
								MAB <= RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+1);
								IQUEUE.FETCH_STATE := F_ADDR;
								CPU_STATE := CPU_INDSTACK;
							------------------------------------------------------------------------------------------------------ JP IRR1 instruction
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"C4") then
								MAB <= RP(3 downto 0) & IQUEUE.QUEUE(IQUEUE.RDPOS+1);
								IQUEUE.FETCH_STATE := F_ADDR;
								CPU_STATE := CPU_INDJUMP;
							----------------------------------------------------------------------------------------------------- BSWAP R1 instruction
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"D5") then
								MAB <= ADDRESSER8(IQUEUE.QUEUE(IQUEUE.RDPOS+1));
								TEMP_OP := ALU_BSWAP;
								NUM_BYTES := 2;
								CPU_STATE := CPU_OMA;
							------------------------------------------------------------------------------------------------- LDC Ir1,Irr2 instruction
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"C5") then
								RESULT(3 downto 0) := IQUEUE.QUEUE(IQUEUE.RDPOS+1)(3 downto 0);
								MAB <= ADDRESSER4(IQUEUE.QUEUE(IQUEUE.RDPOS+1)(7 downto 4));
								NUM_BYTES := 2;
								CAN_FETCH := '0';
								LU_INSTRUCTION := '0';
								CPU_STATE := CPU_LDPTOIM;						
							end if;
						end if;
						
						------------------------------------------------------------------------------------------------------------------------------
						--**************************************************************************************************************************--
						--                                                     1-byte instructions                                                  --
						--**************************************************************************************************************************--
						------------------------------------------------------------------------------------------------------------------------------
						if (IQUEUE.CNT>=1) then
							if (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"F") then	------------------------------------------------ column F instructions
								case IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4) is
									when x"0" =>	---------------------------------------------------------------------------------- NOP instruction	
										NUM_BYTES := 1;
									when x"1" =>	------------------------------------------------------------------------------ page 2 instructions
									when x"2" =>
										ATM_COUNTER := 0;
										NUM_BYTES := 1;
									when x"3" =>
										CPU_STATE := CPU_ILLEGAL;
									when x"4" =>
										CPU_STATE := CPU_ILLEGAL;
									when x"5" =>	---------------------------------------------------------------------------------- WDT instruction
										NUM_BYTES := 1;
									when x"6" =>	--------------------------------------------------------------------------------- STOP instruction
										NUM_BYTES := 1;
										CPU_STATE := CPU_HALTED;
										STOP <= '1';
									when x"7" =>	--------------------------------------------------------------------------------- HALT instruction
										NUM_BYTES := 1;
										CPU_STATE := CPU_HALTED;
									when x"8" =>	----------------------------------------------------------------------------------- DI instruction
										IRQE := '0';
										NUM_BYTES := 1;
									when x"9" =>	----------------------------------------------------------------------------------- EI instruction
										IRQE := '1';
										NUM_BYTES := 1;							
									when x"A" =>	---------------------------------------------------------------------------------- RET instruction
										NUM_BYTES := 1;
										MAB <= SP;
										CPU_STATE := CPU_UNSTACK;	
									when x"B" =>	--------------------------------------------------------------------------------- IRET instruction
										NUM_BYTES := 1;
										IRQE := '1';
										MAB <= SP;
										CPU_STATE := CPU_UNSTACK3;
									when x"C" =>	---------------------------------------------------------------------------------- RCF instruction
										CPU_FLAGS.C := '0';
										NUM_BYTES := 1;
									when x"D" =>	---------------------------------------------------------------------------------- SCF instruction
										CPU_FLAGS.C := '1';
										NUM_BYTES := 1;
									when x"E" =>	---------------------------------------------------------------------------------- CCF instruction
										CPU_FLAGS.C := not CPU_FLAGS.C;
										NUM_BYTES := 1;	
									when others =>	---------------------------------------------------------------------------------- R1 instructions
										CPU_STATE := CPU_ILLEGAL;
								end case;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)(3 downto 0)=x"E") then	------------------------------------------------ INC r instruction
								MAB <= RP(3 downto 0) & RP(7 downto 4) & IQUEUE.QUEUE(IQUEUE.RDPOS)(7 downto 4);
								TEMP_OP := LU2_INC;
								NUM_BYTES := 1;
								CPU_STATE := CPU_OMA2;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"00") then	-------------------------------------------------------------- BRK instruction
								if (OCDCR.BRKEN='1') then		-- the BRK instruction is enabled
									if (OCDCR.DBGACK='1') then
										if (DBG_UART.TX_EMPTY='1') then
											DBG_UART.TX_DATA:=x"FF";
											DBG_UART.TX_EMPTY:='0';
										end if;
									end if;
									if (OCDCR.BRKLOOP='0') then	-- if loop on BRK is disabled
										OCDCR.DBGMODE := '1';	-- set DBGMODE halting CPU
									end if;
								else
									NUM_BYTES := 1;			-- remove the instruction from queue (execute as a NOP)
								end if;
							elsif (IQUEUE.QUEUE(IQUEUE.RDPOS)=x"C9" or IQUEUE.QUEUE(IQUEUE.RDPOS)=x"D9" or IQUEUE.QUEUE(IQUEUE.RDPOS)=x"F9" or 
									IQUEUE.QUEUE(IQUEUE.RDPOS)=x"F8" or IQUEUE.QUEUE(IQUEUE.RDPOS)=x"C6") then	--------------------------- illegal opcode
								CPU_STATE:= CPU_ILLEGAL;
								NUM_BYTES := 1;
							end if;
						end if;
					end if;
					PC := PC + NUM_BYTES;
					IQUEUE.RDPOS := IQUEUE.RDPOS + NUM_BYTES;
					IQUEUE.CNT := IQUEUE.CNT - NUM_BYTES;
					if (OCD.SINGLESTEP='1') then
						if (NUM_BYTES/=0 or IQUEUE.FETCH_STATE=F_ADDR) then OCD.SINGLESTEP:='0';
						end if;
					end if;
				when CPU_MUL =>	-- MUL ***********************************************************************************************************
					TEMP_DATA := DATAREAD(MAB);		-- read first operand
					MAB <= MAB + 1;					-- go to the next operand 
					CPU_STATE := CPU_MUL1;
				when CPU_MUL1 =>
					DEST_ADDR16 := TEMP_DATA * DATAREAD(MAB);	-- multiply previous operand by the second operand and store temporarily
					DATAWRITE(MAB,DEST_ADDR16(7 downto 0));		-- write the lower byte in current memory address 
					WR <= '1';
					CPU_STATE := CPU_MUL2;
				when CPU_MUL2 =>
					MAB <= MAB - 1;								-- decrement memory address (point to the first operand)
					DATAWRITE(MAB,DEST_ADDR16(15 downto 8));	-- write the higher byte
					CPU_STATE := CPU_STORE;						-- complete store operation
				when CPU_XRRTORR =>	-- LEA *******************************************************************************************************
					TEMP_DATA := DATAREAD(MAB);					-- read the operand and store it
					MAB <= MAB + 1;								-- go to the next memory address
					CPU_STATE := CPU_XRRTORR2;
				when CPU_XRRTORR2 =>
					-- read the next operand and perform a 16 bit add with the offset previously in result
					DEST_ADDR16 := ADDER16(TEMP_DATA & DATAREAD(MAB),RESULT);	
					MAB <= DEST_ADDR;							-- point to the destination address
					DATAWRITE(MAB,DEST_ADDR16(15 downto 8));	-- store the higher byte of the 16-bit result
					CPU_STATE := CPU_XRRTORR3;
				when CPU_XRRTORR3 =>
					WR <= '1';
					CPU_STATE := CPU_XRRTORR4;
				when CPU_XRRTORR4 =>
					MAB <= MAB + 1;								-- go to the next memory address
					DATAWRITE(MAB,DEST_ADDR16(7 downto 0));		-- store the lower byte of the 16-bit result
					CPU_STATE := CPU_STORE;						-- complete store operation
				when CPU_MTOXAD =>	-- MEMORY TO INDEXED 8-BIT ADDRESS ***************************************************************************
					TEMP_DATA := DATAREAD(MAB);		
					MAB <= DEST_ADDR;
					CPU_STATE := CPU_MTOXAD2;
				when CPU_MTOXAD2 =>
					MAB <= RP(3 downto 0)&(DATAREAD(MAB) + RESULT);
					DATAWRITE(MAB,TEMP_DATA);
					CPU_STATE := CPU_STORE;
				when CPU_XADTOM => 	-- INDEXED 8-BIT ADDRESS TO MEMORY ***************************************************************************
					MAB <= RP(3 downto 0)&(DATAREAD(MAB) + RESULT);
					CPU_STATE := CPU_TMA;
				when CPU_XRTOM =>	-- LEA *******************************************************************************************************
					TEMP_DATA := DATAREAD(MAB)+RESULT;
					MAB <= DEST_ADDR;
					DATAWRITE(MAB,TEMP_DATA);
					CPU_STATE := CPU_STORE;
				when CPU_IMTOIRR =>	-- INDIRECT MEMORY TO INDIRECT ADDRESS READ FROM REGISTER PAIR ***********************************************
					MAB <= RP(3 downto 0) & DATAREAD(MAB);	-- source address is read from indirect register
					CPU_STATE := CPU_MTOIRR;
				when CPU_MTOIRR =>	-- MEMORY TO INDIRECT ADDRESS READ FROM REGISTER PAIR ********************************************************
					TEMP_DATA := DATAREAD(MAB);				-- reads data from the source (MAB) address and store it into TEMP_DATA
					MAB <= DEST_ADDR;						-- MAB points to the indirect destination register pair
					RESULT := x"00";
					CPU_STATE := CPU_XRRD2;					-- proceed as X indexed register pair destination
				when CPU_IRRS =>	-- RR PAIR AS INDIRECT SOURCE ADDRESS ************************************************************************
					DEST_ADDR16(15 downto 8) := DATAREAD(MAB);
					MAB <= MAB + 1;
					CPU_STATE := CPU_IRRS2;
				when CPU_IRRS2 =>
					DEST_ADDR16(7 downto 0) := DATAREAD(MAB);
					MAB <= DEST_ADDR16(11 downto 0);
					if (LU_INSTRUCTION='1') then 
						CPU_STATE:= CPU_TMA;				-- if it is direct addressing mode, go to TMA
					else
						CPU_STATE := CPU_IND2;				-- if it is indirect addressing mode, go to IND2
					end if;
				when CPU_XRRD =>
					TEMP_DATA := DATAREAD(MAB);				-- reads data from the source (MAB) address and store it into TEMP_DATA
					MAB <= DEST_ADDR;
					CPU_STATE := CPU_XRRD2;
				when CPU_XRRD2 =>
					DEST_ADDR16(15 downto 8) := DATAREAD(MAB);
					MAB <= MAB + 1;
					CPU_STATE := CPU_XRRD3;
				when CPU_XRRD3 =>
					DEST_ADDR16(7 downto 0) := DATAREAD(MAB);
					MAB <= ADDER16(DEST_ADDR16,RESULT)(11 downto 0);
					DATAWRITE(MAB,TEMP_DATA);
					CPU_STATE := CPU_STORE;
				when CPU_XRRS =>
					DEST_ADDR16(15 downto 8) := DATAREAD(MAB);
					MAB <= MAB + 1;
					CPU_STATE := CPU_XRRS2;
				when CPU_XRRS2 =>
					DEST_ADDR16(7 downto 0) := DATAREAD(MAB);
					MAB <= ADDER16(DEST_ADDR16,RESULT)(11 downto 0);
					CPU_STATE := CPU_XRRS3;
				when CPU_XRRS3 =>
					TEMP_DATA := DATAREAD(MAB);
					MAB <= DEST_ADDR;
					DATAWRITE(MAB,TEMP_DATA);
					CPU_STATE := CPU_STORE;
				when CPU_INDRR =>	-- INDIRECT DESTINATION ADDRESS FOR WORD INSTRUCTIONS (DECW AND INCW) ****************************************
					MAB <= (RP(3 downto 0) & DATAREAD(MAB))+1;	-- the destination address is given by indirect address
					CPU_STATE := CPU_OMA2;
				when CPU_ISMD1 =>	-- INDIRECT SOURCE ADDRESS ***********************************************************************************
					MAB <= RP(3 downto 0) & DATAREAD(MAB);	-- source address is read from indirect register
					CPU_STATE := CPU_TMA;
				when CPU_IND2 =>	-- READS REGISTER AND PERFORM OPERATION ON AN INDIRECT DESTINATION *******************************************
					TEMP_DATA := DATAREAD(MAB);				-- reads data from the source (MAB) address and store it into TEMP_DATA
					MAB <= DEST_ADDR;						-- place the address of the indirect register on MAB
					CPU_STATE := CPU_IND1;					-- proceed to the indirect
				when CPU_IND1 =>	-- INDIRECT DESTINATION ADDRESS ******************************************************************************
					MAB <= RP(3 downto 0) & DATAREAD(MAB);	-- the destination address is given by indirect address
					if (LU_INSTRUCTION='0') then CPU_STATE := CPU_OMA;					-- proceed with one memory access
					else CPU_STATE := CPU_OMA2;											-- proceed with one memory access (logic unit related)
					end if;
				when CPU_TMA =>		-- TWO MEMORY ACCESS, READS SOURCE OPERAND FROM MEMORY *******************************************************
					TEMP_DATA := DATAREAD(MAB);				-- reads data from the source (MAB) address and store it into TEMP_DATA
					MAB <= DEST_ADDR;						-- place the destination address (DEST_ADDR) on the memory address bus (MAB)
					CPU_STATE := CPU_OMA;					-- proceed to the last stage
				when CPU_OMA =>		-- ONE MEMORY ACCESS stage ***********************************************************************************
					-- this stage performs the TEMP_OP operation between TEMP_DATA and data read from current (MAB) address (destination)
					RESULT := ALU(TEMP_OP,DATAREAD(MAB),TEMP_DATA,CPU_FLAGS.C);
					if (TEMP_OP<ALU_OR) then
						CPU_FLAGS.C := ALU_FLAGS.C;
						CPU_FLAGS.V := ALU_FLAGS.V;
						CPU_FLAGS.Z := ALU_FLAGS.Z;
						CPU_FLAGS.S := ALU_FLAGS.S;
						CPU_FLAGS.H := ALU_FLAGS.H;
						CPU_FLAGS.D := TEMP_OP(1);
					elsif (TEMP_OP=ALU_CP or TEMP_OP=ALU_CPC) then
						CPU_FLAGS.C := ALU_FLAGS.C;
						CPU_FLAGS.V := ALU_FLAGS.V;
						CPU_FLAGS.Z := ALU_FLAGS.Z;
						CPU_FLAGS.S := ALU_FLAGS.S;
					elsif (TEMP_OP/=ALU_LD) then
						CPU_FLAGS.Z := ALU_FLAGS.Z;
						CPU_FLAGS.S := ALU_FLAGS.S;
						CPU_FLAGS.V := '0';				
					end if;
					if (ALU_NOUPDATE='0') then
						DATAWRITE(MAB,RESULT);
						WR <= '1';
					end if;
					CPU_STATE := CPU_DECOD;
					if (WORD_DATA='1') then
						CPU_STATE := CPU_LDW;
					end if;
				when CPU_OMA2 =>	-- ONE MEMORY ACCESS stage logic unit related ****************************************************************
					-- this stage performs the TEMP_OP LU2 operation on data read from current (MAB) address
					RESULT := LU2(TEMP_OP,DATAREAD(MAB),CPU_FLAGS.D,CPU_FLAGS.H,CPU_FLAGS.C);
					if (TEMP_OP=LU2_DEC or TEMP_OP=LU2_INC) then
						CPU_FLAGS.V := ALU_FLAGS.V;
						CPU_FLAGS.Z := ALU_FLAGS.Z;
						CPU_FLAGS.S := ALU_FLAGS.S;
					elsif (TEMP_OP=LU2_COM) then
						CPU_FLAGS.V := '0';
						CPU_FLAGS.Z := ALU_FLAGS.Z;
						CPU_FLAGS.S := ALU_FLAGS.S;
					elsif (TEMP_OP=LU2_SWAP) then
						CPU_FLAGS.Z := ALU_FLAGS.Z;
						CPU_FLAGS.S := ALU_FLAGS.S;
					elsif (TEMP_OP=LU2_DA) then
						CPU_FLAGS.C := ALU_FLAGS.C;
						CPU_FLAGS.Z := ALU_FLAGS.Z;
						CPU_FLAGS.S := ALU_FLAGS.S;					
					elsif (TEMP_OP=LU2_LD) then
						CPU_FLAGS.V := ALU_FLAGS.V;
						CPU_FLAGS.S := ALU_FLAGS.S;
						CPU_FLAGS.Z := CPU_FLAGS.Z and ALU_FLAGS.Z;
					elsif (TEMP_OP/=LU2_CLR) then
						CPU_FLAGS.C := ALU_FLAGS.C;
						CPU_FLAGS.V := ALU_FLAGS.V;
						CPU_FLAGS.Z := ALU_FLAGS.Z;
						CPU_FLAGS.S := ALU_FLAGS.S;				
					end if;
					DATAWRITE(MAB,RESULT);
					WR <= '1';
					if (WORD_DATA='1') then
						WORD_DATA := '0';
						if (ALU_FLAGS.C='0') then TEMP_OP := LU2_LD;
						end if;
						CPU_STATE := CPU_DMAB;
					else CPU_STATE := CPU_DECOD;				
					end if;
				when CPU_DMAB =>	-- DECREMENT MEMORY ADDRESS BUS *****************************************************************************
					MAB <= MAB - 1;
					CPU_STATE := CPU_OMA2;
				when CPU_LDW =>
					if (WORD_DATA='0') then
						TEMP_DATA := DATAREAD(MAB);
						MAB <= MAB + 1;
					else
						DATAWRITE(MAB,TEMP_DATA);
						WR <= '1';
					end if;	
					CPU_STATE := CPU_LDW2;
				when CPU_LDW2 =>
					if (WORD_DATA='0') then
						RESULT := DATAREAD(MAB);
						MAB <= DEST_ADDR;
						WORD_DATA := '1';
						CPU_STATE := CPU_LDW;
					else
						MAB <= MAB + 1;
						DATAWRITE(MAB,RESULT);
						CPU_STATE := CPU_STORE;
					end if;
				when CPU_LDPTOIM =>	-- LOAD PROGRAM TO INDIRECT MEMORY **************************************************************************
					TEMP_DATA := DATAREAD(MAB);
					DEST_ADDR := RP(3 downto 0) & TEMP_DATA;	-- the destination address is read from the indirect address
					if (WORD_DATA='1') then
						-- it is a LDCI instruction, so we have to increment the indirect address after the operation
						DATAWRITE(MAB,TEMP_DATA+1);
						WR <= '1';
						CPU_STATE := CPU_LDPTOIM2;
					else
						-- it is a LDC instruction, proceed the load instruction
						MAB <= ADDRESSER4(RESULT(3 downto 0));		-- the source address (program memory) is read from source register pair
						CPU_STATE := CPU_LDPTOM;
					end if;
				when CPU_LDPTOIM2 =>
					MAB <= ADDRESSER4(RESULT(3 downto 0));		-- the source address (program memory) is read from source register pair
					CPU_STATE := CPU_LDPTOM;				
				when CPU_LDPTOM =>	-- LOAD PROGRAM TO MEMORY ***********************************************************************************
					IAB(15 downto 8) <= DATAREAD(MAB);	-- read the high address from the first register
					MAB <= MAB + 1;
					CPU_STATE := CPU_LDPTOM2;
				when CPU_LDPTOM2 =>	
					IAB(7 downto 0) <= DATAREAD(MAB);	-- read the low address from the second register
					MAB <= DEST_ADDR;
					if (LU_INSTRUCTION='0') then 
						CPU_STATE := CPU_LDPTOM3;		-- if it is a read from program memory
					else
						CPU_STATE := CPU_LDMTOP;		-- if it is a write onto program memory
					end if;
				when CPU_LDPTOM3 =>	-- READ PROGRAM MEMORY AND STORE INTO RAM *******************************************************************
					DATAWRITE(MAB,IDB);
					WR <= '1';
					CAN_FETCH := '1';	-- re-enable fetching
					FETCH_ADDR := PC;
					IAB <= PC;
					CPU_STATE := CPU_DECOD;
					if (WORD_DATA='1') then
						CPU_STATE := CPU_LDPTOM4;
					end if;
				when CPU_LDPTOM4 =>
					DEST_ADDR := ADDRESSER4(RESULT(3 downto 0));
					MAB <= DEST_ADDR+1;
					TEMP_OP := LU2_INC;
					CPU_STATE := CPU_OMA2;
				when CPU_LDMTOP =>	-- READ RAM AND STORE ONTO PROGRAM MEMORY *******************************************************************
					PWDB <= DATAREAD(MAB);	-- PWDB receive the content of RAM
					PGM_WR <= '1';			-- enable program memory write signal
					CPU_STATE := CPU_LDMTOP2;
				when CPU_LDMTOP2 =>				
					CAN_FETCH := '1';		-- re-enable instruction fetching
					FETCH_ADDR := PC;
					IAB <= PC;
					CPU_STATE := CPU_DECOD;
					if (WORD_DATA='1') then
						CPU_STATE := CPU_LDPTOM4;
					end if;
				when CPU_BIT =>
					TEMP_DATA := DATAREAD(MAB);
					TEMP_DATA(to_integer(unsigned(TEMP_OP(2 downto 0)))):=TEMP_OP(3);
					DATAWRITE(MAB,TEMP_DATA);
					WR <= '1';
					CPU_STATE := CPU_DECOD;
				when CPU_IBTJ =>
					MAB <= RP(3 downto 0) & DATAREAD(MAB);
					CPU_STATE := CPU_BTJ;
				when CPU_BTJ =>
					TEMP_DATA := DATAREAD(MAB);
					if (TEMP_DATA(to_integer(unsigned(TEMP_OP(2 downto 0))))=TEMP_OP(3)) then
						PC := ADDER16(PC,RESULT);
						IQUEUE.FETCH_STATE := F_ADDR;
					end if; 
					CPU_STATE := CPU_DECOD;
				when CPU_DJNZ =>
					RESULT := LU2(LU2_DEC,DATAREAD(MAB),'0','0','0');
					if (ALU_FLAGS.Z='0') then	-- result is not zero, then jump relative
						PC := DEST_ADDR16;
						IQUEUE.FETCH_STATE := F_ADDR;
					end if;
					DATAWRITE(MAB,RESULT);
					WR <= '1';
					CPU_STATE := CPU_DECOD;
				when CPU_INDJUMP =>
					PC(15 downto 8) := DATAREAD(MAB);
					MAB <= MAB + 1;
					CPU_STATE:= CPU_INDJUMP2;
				when CPU_INDJUMP2 =>
					PC(7 downto 0) := DATAREAD(MAB);
					IQUEUE.FETCH_STATE := F_ADDR;
					CPU_STATE := CPU_DECOD;
				when CPU_TRAP =>
					PC(15 downto 8) := DATAREAD(MAB);
					MAB <= MAB + 1;
					CPU_STATE:= CPU_TRAP2;
				when CPU_TRAP2 =>
					PC(7 downto 0) := DATAREAD(MAB);
					SP := SP - 1;
					MAB <= SP;
					IQUEUE.FETCH_STATE := F_ADDR;
					LU_INSTRUCTION := '1';
					CPU_STATE := CPU_STACK;
				when CPU_INDSTACK =>
					PC(15 downto 8) := DATAREAD(MAB);
					MAB <= MAB + 1;
					CPU_STATE:= CPU_INDSTACK2;
				when CPU_INDSTACK2 =>
					PC(7 downto 0) := DATAREAD(MAB);
					SP := SP - 1;
					MAB <= SP;
					IQUEUE.FETCH_STATE := F_ADDR;
					CPU_STATE := CPU_STACK;
				when CPU_VECTOR =>
					PC(15 downto 8) := IDB;
					IAB <= IAB + 1;
					CPU_STATE := CPU_VECTOR2;
				when CPU_VECTOR2 =>
					PC(7 downto 0) := IDB;
					IQUEUE.FETCH_STATE := F_ADDR;
					CAN_FETCH := '1';
					if (LU_INSTRUCTION='1') then CPU_STATE := CPU_STACK;
					else CPU_STATE := CPU_DECOD;
					end if;
				when CPU_STACK =>	-- PUSH PC 7:0 INTO THE STACK *******************************************************************************
					DATAWRITE(MAB,DEST_ADDR16(7 downto 0));
					WR <= '1';
					CPU_STATE := CPU_STACK1;
				when CPU_STACK1 =>
					SP := SP - 1;
					MAB <= SP;				
					CPU_STATE := CPU_STACK2;
				when CPU_STACK2 =>
					DATAWRITE(MAB,DEST_ADDR16(15 downto 8));
					WR <= '1';
					if (LU_INSTRUCTION='1') then
						CPU_STATE := CPU_STACK3;
					else					
						CPU_STATE := CPU_DECOD;
					end if;
				when CPU_STACK3 =>	-- PUSH FLAGS INTO THE STACK *******************************************************************************
					SP := SP - 1;
					MAB <= SP;
					DATAWRITE(MAB,CPU_FLAGS.C&CPU_FLAGS.Z&CPU_FLAGS.S&CPU_FLAGS.V&CPU_FLAGS.D&CPU_FLAGS.H&CPU_FLAGS.F2&CPU_FLAGS.F1);
					WR <= '1';
					IRQE := '0';
					CPU_STATE := CPU_DECOD;			
				when CPU_UNSTACK3 =>
					TEMP_DATA := DATAREAD(MAB);
					CPU_FLAGS.C := TEMP_DATA(7);
					CPU_FLAGS.Z := TEMP_DATA(6);
					CPU_FLAGS.S := TEMP_DATA(5);
					CPU_FLAGS.V := TEMP_DATA(4);
					CPU_FLAGS.D := TEMP_DATA(3);
					CPU_FLAGS.H := TEMP_DATA(2);
					CPU_FLAGS.F2 := TEMP_DATA(1);
					CPU_FLAGS.F1 := TEMP_DATA(0);
					SP := SP + 1;
					MAB <= SP;
					CPU_STATE := CPU_UNSTACK;				
				when CPU_UNSTACK =>
					DEST_ADDR16(15 downto 8) := DATAREAD(MAB);
					SP := SP + 1;
					MAB <= SP;
					CPU_STATE := CPU_UNSTACK2;
				when CPU_UNSTACK2 =>
					DEST_ADDR16(7 downto 0) := DATAREAD(MAB);
					SP := SP + 1;
					PC := DEST_ADDR16;
					IQUEUE.FETCH_STATE := F_ADDR;			
					CPU_STATE := CPU_DECOD;
				when CPU_STORE =>
					WR <= '1';
					CPU_STATE := CPU_DECOD;
				when CPU_HALTED =>	-- HALT mode, wait for an interrupt or reset
				when CPU_ILLEGAL =>	-- An illegal opcode was fetched 
				when CPU_RESET =>
					IAB <= x"0002";
					MAB <= x"000";
					PWDB <= x"00";
					SP := x"000";
					RP := x"00";
					WR <= '0';
					PGM_WR <= '0';
					STOP <= '0';
					CAN_FETCH := '1';
					FETCH_ADDR := x"0000";
					RXSYNC1 <= '1';
					RXSYNC2 <= '1';
					DBG_UART.LAST_SMP := '1';
					IQUEUE.FETCH_STATE := F_ADDR;
					IRQE := '0';
					IRQ0 := x"00";
					OLD_IRQ0 := x"00";
					IRQ0ENH := x"00";
					IRQ0ENL := x"00";
					OCDCR.RST := '0';
					ATM_COUNTER := 0;
					CPU_STATE := CPU_VECTOR;			
				when others =>
					CPU_STATE := CPU_DECOD;
				end case;	
				-- end of the main decoder
			end if;	-- CKDIVIDER
		end if;
	end process;
end CPU;