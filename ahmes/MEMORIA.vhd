LIBRARY ieee ;
USE ieee.std_logic_1164.all ;
USE ieee.std_logic_unsigned.all ;

ENTITY memoria IS
	PORT
	(
		address_bus	: IN INTEGER RANGE 0 TO 255;
		data_in		: IN INTEGER RANGE 0 TO 255;
		data_out	: OUT INTEGER RANGE 0 TO 255;
		mem_write	: IN std_logic;
		rst			: IN std_logic
	);
END memoria;

ARCHITECTURE MEMO OF MEMORIA IS
constant NOP : INTEGER := 0;
constant STA : INTEGER := 16;
constant LDA : INTEGER := 32;
constant ADD : INTEGER := 48;
constant IOR : INTEGER := 64;
constant IAND: INTEGER := 80;
constant INOT: INTEGER := 96;
constant SUB : INTEGER := 112;
constant JMP : INTEGER := 128;
constant JN  : INTEGER := 144;
constant JP  : INTEGER := 148;
constant JV  : INTEGER := 152;
constant JNV : INTEGER := 156;
constant JZ  : INTEGER := 160;
constant JNZ : INTEGER := 164;
constant JC  : INTEGER := 176;
constant JNC : INTEGER := 180;
constant JB  : INTEGER := 184;
constant JNB : INTEGER := 188;
constant SHR : INTEGER := 224;
constant SHL : INTEGER := 225;
constant IROR: INTEGER := 226;
constant IROL: INTEGER := 227;
constant HLT : INTEGER := 240;
TYPE DATA IS ARRAY (0 TO 255) OF INTEGER;
BEGIN
	process (mem_write,rst)
		VARIABLE DATA_ARRAY: DATA;
	BEGIN
		IF (RST='1') THEN
			-- Contador decrescente de 10 (conteúdo do endereço 130) até 0 
			DATA_ARRAY(0) := LDA;	-- Carrega A com (130) (A=10)
			DATA_ARRAY(1) := 130;	
			DATA_ARRAY(2) := SUB;	-- Subtrai (132) de A (A=A-1)
			DATA_ARRAY(3) := 132;
			DATA_ARRAY(4) := JZ;	-- Salta para 8 se A=0
			DATA_ARRAY(5) := 8;
			DATA_ARRAY(6) := JMP;	-- Salta para o endereço 2 (loop)
			DATA_ARRAY(7) := 2;
			-- Terminou a contagem, agora faz a soma de (130) com (131) e salva em (128)
			DATA_ARRAY(8) := LDA;	-- Carrega A com (130) (A=10)
			DATA_ARRAY(9) := 130;
			DATA_ARRAY(10) := ADD;	-- Soma A com (131) (A=10+18)
			DATA_ARRAY(11) := 131;
			DATA_ARRAY(12) := STA;	-- Guarda A em (128)
			DATA_ARRAY(13) := 128;
			-- Agora faz um OR de (128) com (129) rotacionado 4 bits à esquerda, salva o resultado em (133)
			DATA_ARRAY(14) := LDA;	-- Carrega A com (129)
			DATA_ARRAY(15) := 129;
			DATA_ARRAY(16) := SHL;	-- Desloca A 1 bit à esquerda (o LSB é zero)
			DATA_ARRAY(17) := SHL;	-- Desloca A 1 bit à esquerda (o LSB é zero)
			DATA_ARRAY(18) := SHL;	-- Desloca A 1 bit à esquerda (o LSB é zero)
			DATA_ARRAY(19) := SHL;	-- Desloca A 1 bit à esquerda (o LSB é zero)
			DATA_ARRAY(20) := IOR;	-- OU lógico de A com (128)
			DATA_ARRAY(21) := 128;
			DATA_ARRAY(22) := STA;	-- Guarda o resultado em (133)
			DATA_ARRAY(23) := 133;
			DATA_ARRAY(24) := HLT;	-- Pára o processamento
			-- Variáveis e constantes utilizadas no programa
			DATA_ARRAY(128) := 0;
			DATA_ARRAY(129) := 5;
			DATA_ARRAY(130) := 10;
			DATA_ARRAY(131) := 18;
			DATA_ARRAY(132) := 1;
		ELSIF (RISING_EDGE(MEM_WRITE)) THEN
			DATA_ARRAY(ADDRESS_BUS) := DATA_IN;			
		END IF;
		DATA_OUT <= DATA_ARRAY(ADDRESS_BUS);
	end process;
END MEMO;
