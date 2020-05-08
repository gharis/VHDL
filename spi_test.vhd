LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY spi_test IS
GENERIC(
	slaves  : INTEGER := 1;  --number of spi slaves
	d_width : INTEGER := 12); --data bus width
PORT(
	--avlon slave inputsa and outputs
	clk       : IN  STD_LOGIC;                                    --system clock
	reset   : IN  STD_LOGIC;                                    --asynchronous reset
	avs_s0_address	: 	IN STD_LOGIC;
	avs_s0_read	: 	IN STD_LOGIC;
	avs_s0_write	: 	IN STD_LOGIC;
	avs_s0_readdata	: 	OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	avs_s0_writedata	: 	IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	clk_50   : IN     STD_LOGIC;                             --system clock
	miso    : IN     STD_LOGIC;                            --master in, slave out
	sclk    : BUFFER STD_LOGIC;                             --spi clock
	ss_n    : BUFFER STD_LOGIC_VECTOR(slaves-1 DOWNTO 0);   --slave select
	mosi    : OUT    STD_LOGIC;                             --master out, slave in
	rs_spi  : IN     STD_LOGIC);                            --asynchronous reset
END spi_test;

ARCHITECTURE logic OF spi_test IS
	TYPE machine IS(ready, execute);                           --state machine data type
	SIGNAL state       : machine;                              --current state
	SIGNAL slave       : INTEGER;                              --slave selected for current transaction
	SIGNAL clk_ratio   : INTEGER;                              --current clk_div
	SIGNAL count       : INTEGER;                              --counter to trigger sclk from system clock
	SIGNAL clk_toggles : INTEGER RANGE 0 TO d_width*2 + 1;     --count spi clock toggles
	SIGNAL assert_data : STD_LOGIC;                            --'1' is tx sclk toggle, '0' is rx sclk toggle
	SIGNAL continue    : STD_LOGIC;                            --flag to continue transaction
	SIGNAL rx_buffer   : STD_LOGIC_VECTOR(d_width-1 DOWNTO 0); --receive data buffer
	SIGNAL tx_buffer   : STD_LOGIC_VECTOR(d_width-1 DOWNTO 0); --transmit data buffer
	SIGNAL last_bit_rx : INTEGER RANGE 0 TO d_width*2;         --last rx data bit location
	SIGNAL busy    : STD_LOGIC;  									 --busy / data ready signal
	SIGNAL enable  : STD_LOGIC;                             --initiate transaction
	SIGNAL cpol    : STD_LOGIC;                             --spi clock polarity
	SIGNAL cpha    : STD_LOGIC;                             --spi clock phase
	SIGNAL cont    : STD_LOGIC;                             --continuous mode command
	SIGNAL clk_div : INTEGER;                               --system clock cycles per 1/2 period of sclk
  	SIGNAL addr    : INTEGER;                               --address of slave
	SIGNAL tx_data     : STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --data to transmit
	SIGNAL rx_data     : STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);	 --data received
	SIGNAL mydata_write : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL mydata_read : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL led :STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000"; --temp variable
  
BEGIN
  ----- avlon read
	PROCESS(avs_s0_read)
	BEGIN
		IF (avs_s0_read = '1')THEN
			CASE(avs_s0_address) IS
				WHEN '0' => avs_s0_readdata <= rx_data AND x"FFFFFFFF";
				WHEN others => avs_s0_readdata <= x"00000000";
			END CASE	;
		ELSE
			avs_s0_readdata <= x"00000000";
		END IF;
	END PROCESS;
-----

-----avlon write
	PROCESS(clk)
	BEGIN
		IF (reset = '1') THEN
			led<=x"00";
		ELSIF (avs_s0_write = '1') THEN
			CASE (avs_s0_address) IS
				WHEN '0' => mydata_write <=avs_s0_writedata(31 downto 0);
				WHEN others => led<=led;
			END CASE	;
			busy <= mydata_write(0);
			enable <= mydata_write(1);
			cpol <= mydata_write(2);
			cpha <= mydata_write(3);
			cont <= mydata_write(4);
			-- <= conv_integer(unsigned(input_4));
			--to_integer(unsigned(a));
			clk_div <= conv_integer(unsigned(mydata_write(6 DOWNTO 5)));
			addr <= conv_integer(unsigned(mydata_write(7 DOWNTO 6)));
			tx_data <= mydata_write(19 DOWNTO 8);
			--ena <= mydata_write(7 DOWNTO 0);
			--duty <= mydata_write(15 DOWNTO 8);
			
		END IF;
	END PROCESS;
-----

	PROCESS(clk_50, rs_spi)
	BEGIN

	IF(rs_spi = '0') THEN        --reset system
		busy <= '1';                --set busy signal
		ss_n <= (OTHERS => '1');    --deassert all slave select lines
		mosi <= 'Z';                --set master out to high impedance
		rx_data <= (OTHERS => '0'); --clear receive data port
		state <= ready;             --go to ready state when reset is exited

	ELSIF(clk_50'EVENT AND clk_50 = '1') THEN
	 
      CASE state IS               --state machine

			WHEN ready =>
				busy <= '0';             --clock out not busy signal
				ss_n <= (OTHERS => '1'); --set all slave select outputs high
				mosi <= 'Z';             --set mosi output high impedance
				continue <= '0';         --clear continue flag

				 --user input to initiate transaction
            IF(enable = '1') THEN       
					busy <= '1';             --set busy signal
					IF(addr < slaves) THEN   --check for valid slave address
						slave <= addr;         --clock in current slave selection if valid
					ELSE
						slave <= 0;            --set to first slave if not valid
					END IF;
					IF(clk_div = 0) THEN     --check for valid spi speed
						clk_ratio <= 1;        --set to maximum speed if zero
						count <= 1;            --initiate system-to-spi clock counter
					ELSE
						clk_ratio <= clk_div;  --set to input selection if valid
						count <= clk_div;      --initiate system-to-spi clock counter
					END IF;
					sclk <= cpol;            --set spi clock polarity
					assert_data <= NOT cpha; --set spi clock phase
					tx_buffer <= tx_data;    --clock in data for transmit into buffer
					clk_toggles <= 0;        --initiate clock toggle counter
					last_bit_rx <= d_width*2 + conv_integer(cpha) - 1; --set last rx data bit
					state <= execute;        --proceed to execute state
				ELSE
					state <= ready;          --remain in ready state
				END IF;

		WHEN execute =>
			busy <= '1';        --set busy signal
			ss_n(slave) <= '0'; --set proper slave select output
	 
			--system clock to sclk ratio is met
			IF(count = clk_ratio) THEN        
				count <= 1;                     --reset system-to-spi clock counter
				assert_data <= NOT assert_data; --switch transmit/receive indicator
				IF(clk_toggles = d_width*2 + 1) THEN
					clk_toggles <= 0;               --reset spi clock toggles counter
				ELSE
					clk_toggles <= clk_toggles + 1; --increment spi clock toggles counter
            END IF;
            
            --spi clock toggle needed
            IF(clk_toggles <= d_width*2 AND ss_n(slave) = '0') THEN 
					sclk <= NOT sclk; --toggle spi clock
            END IF;
            
            --receive spi clock toggle
            IF(assert_data = '0' AND clk_toggles < last_bit_rx + 1 AND ss_n(slave) = '0') THEN 
					rx_buffer <= rx_buffer(d_width-2 DOWNTO 0) & miso; --shift in received bit
            END IF;
            
            --transmit spi clock toggle
            IF(assert_data = '1' AND clk_toggles < last_bit_rx) THEN 
					mosi <= tx_buffer(d_width-1);                     --clock out data bit
					tx_buffer <= tx_buffer(d_width-2 DOWNTO 0) & '0'; --shift data transmit buffer
            END IF;
            
            --last data receive, but continue
            IF(clk_toggles = last_bit_rx AND cont = '1') THEN 
					tx_buffer <= tx_data;                       --reload transmit buffer
					clk_toggles <= last_bit_rx - d_width*2 + 1; --reset spi clock toggle counter
					continue <= '1';                            --set continue flag
            END IF;
            
            --normal end of transaction, but continue
            IF(continue = '1') THEN  
					continue <= '0';      --clear continue flag
					busy <= '0';          --clock out signal that first receive data is ready
					rx_data <= rx_buffer; --clock out received data to output port    
            END IF;
            
            --end of transaction
            IF((clk_toggles = d_width*2 + 1) AND cont = '0') THEN   
					busy <= '0';             --clock out not busy signal
					ss_n <= (OTHERS => '1'); --set all slave selects high
					mosi <= 'Z';             --set mosi output high impedance
					rx_data <= rx_buffer;    --clock out received data to output port
					state <= ready;          --return to ready state
            ELSE                       --not end of transaction
					state <=	 execute;        --remain in execute state
            END IF;
          
			ELSE        --system clock to sclk ratio not met
					count <= count + 1; --increment counter
					state <= execute;   --remain in execute state
			END IF;
		END CASE;
	END IF;
	END PROCESS; 
END logic;
