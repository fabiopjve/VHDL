LIBRARY ieee ;
USE ieee.std_logic_1164.all ;
USE ieee.std_logic_unsigned.all ;

ENTITY memoria IS
	PORT
	(
		address_bus	: IN INTEGER RANGE 0 TO 255;
		data_in		: IN INTEGER RANGE 0 TO 255;
		out_port	: OUT INTEGER RANGE 0 TO 255;
		mem_write	: IN std_logic
	);
END memoria;

ARCHITECTURE MEMO OF MEMORIA IS
BEGIN
	process (mem_write)
	BEGIN
		IF (RISING_EDGE(MEM_WRITE)) THEN
			IF (ADDRESS_BUS=255) THEN
				out_port <= DATA_IN;
			END IF;
		END IF;
	end process;
END MEMO;
