--------------------------------------------------------------------------------
--
--   FileName:         pwm.vhd
--   Dependencies:     none
--   Design Software:  Quartus II 64-bit Version 12.1 Build 177 SJ Full Version
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 8/1/2013 Scott Larson
--     Initial Public Release
--   Version 2.0 1/9/2015 Scott Larson
--     Transistion between duty cycles always starts at center of pulse to avoid
--     anomalies in pulse shapes
--    
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

ENTITY pwm IS
  GENERIC(
      sys_clk         : INTEGER := 50000000; --system clock frequency in Hz
      pwm_freq        : INTEGER := 100000;    --PWM switching frequency in Hz
      bits_resolution : INTEGER := 8;          --bits of resolution setting the duty cycle
      phases          : INTEGER := 1);         --number of output pwms and phases
  PORT(
      
		--avlon slave inputsa and outputs
		clk       : IN  STD_LOGIC;                                    --system clock
      reset   : IN  STD_LOGIC;                                    --asynchronous reset
      --avs_s0_address	: 	IN STD_LOGIC;
		avs_s0_address	: 	IN  STD_LOGIC_VECTOR(7 DOWNTO 0);

		avs_s0_read	: 	IN STD_LOGIC;
		avs_s0_write	: 	IN STD_LOGIC;
		avs_s0_readdata	: 	OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		avs_s0_writedata	: 	IN STD_LOGIC_VECTOR(31 DOWNTO 0);

		rs : in std_logic;
		clk_50 : in std_logic;		
		m_dir : OUT STD_LOGIC_VECTOR(1 DOWNTO 0); 
		--ena       : IN  STD_LOGIC;                                    --latches in new duty cycle
      --duty      : IN  STD_LOGIC_VECTOR(bits_resolution-1 DOWNTO 0); --duty cycle
      pwm_out   : OUT STD_LOGIC_VECTOR(phases-1 DOWNTO 0);          --pwm outputs
      pwm_n_out : OUT STD_LOGIC_VECTOR(phases-1 DOWNTO 0)           --pwm inverse outputs
		);         
END pwm;

ARCHITECTURE logic OF pwm IS
  CONSTANT  period     :  INTEGER := sys_clk/pwm_freq;                      --number of clocks in one pwm period
  TYPE counters IS ARRAY (0 TO phases-1) OF INTEGER RANGE 0 TO period - 1;  --data type for array of period counters
  SIGNAL  count        :  counters := (OTHERS => 0);                        --array of period counters
  SIGNAL   half_duty_new  :  INTEGER RANGE 0 TO period/2 := 0;              --number of clocks in 1/2 duty cycle
  TYPE half_duties IS ARRAY (0 TO phases-1) OF INTEGER RANGE 0 TO period/2; --data type for array of half duty values
  SIGNAL  half_duty    :  half_duties := (OTHERS => 0);                     --array of half duty values (for each phase)
 

  SIGNAL ena : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL duty : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL mydata_write : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL mydata_read : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL led :STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000"; --temp variable
  SIGNAL m_dir_32 : STD_LOGIC_VECTOR(31 DOWNTO 0);

  BEGIN


----- avlon read
--	PROCESS(avs_s0_read)
--	BEGIN
--		IF avs_s0_read = '1' THEN
--			CASE(avs_s0_address) IS
--				WHEN '0' => avs_s0_readdata <= mydata_read AND x"FFFFFFFF";
--				WHEN others => avs_s0_readdata <= x"00000000";
--			END CASE	;
--		ELSE
--			avs_s0_readdata <= x"00000000";
--		END IF;
--	END PROCESS;
-----
	  PROCESS(clk)

	  BEGIN
        IF rising_edge(clk) THEN
            IF (reset='1') THEN
                avs_s0_readdata <= x"00000000";
            ELSIF (avs_s0_read='1') THEN
                
					 IF (avs_s0_address = "00000000" )THEN
                    avs_s0_readdata <= x"00000001"; --1st 32 bits worked fine
                
					 ELSIF (avs_s0_address = x"01") THEN
                    avs_s0_readdata <= x"00000002"; --2nd 32 bits tried access from ex = *(Count_ptr+1);
					  
					 ELSE avs_s0_readdata <= x"0ff0ffff";
					 END IF;		
            END IF;
        END IF;
	  END PROCESS;
-----avlon write


	  PROCESS(clk)

	  BEGIN
        IF rising_edge(clk) THEN
            IF (reset='1') THEN
                led <= "00001111";
            ELSIF (avs_s0_write='1') THEN
                
					 IF (avs_s0_address = x"00" )THEN
                    ena <= avs_s0_writedata; --enable
                
					 ELSIF (avs_s0_address = x"01") THEN
                    duty <= avs_s0_writedata; --duty
					 ELSIF (avs_s0_address = x"02") THEN
                    m_dir_32 <= avs_s0_writedata; --direction 
					  
					 ELSE led <= "11110000";
					 END IF;
					 m_dir(0)<= m_dir_32(0);
					 m_dir(1)<= m_dir_32(1);
            END IF;
        END IF;
	  END PROCESS;


--	PROCESS(clk)
--	BEGIN
--		IF reset = '1' THEN
--			led<=x"00";
--		ELSIF avs_s0_write = '1' THEN
--			CASE (avs_s0_address) IS
--				WHEN '0' => mydata_write <=avs_s0_writedata(31 downto 0);
--				WHEN others => led<=led;
--			END CASE	;
--			
--			ena <= mydata_write(7 DOWNTO 0);
--			duty <= mydata_write(15 DOWNTO 8);
--			
--		END IF;
--	END PROCESS;
-----






  PROCESS(clk_50 , rs)
  
  BEGIN
    IF rs = '0' THEN                                                 --asynchronous reset
      count <= (OTHERS => 0);                                                --clear counter
      pwm_out <= (OTHERS => '0');                                            --clear pwm outputs
      pwm_n_out <= (OTHERS => '0');                                          --clear pwm inverse outputs
    ELSIF clk_50 'EVENT AND clk_50  = '1' THEN                                      --rising system clock edge
      --IF ena = '1' THEN                                                   --latch in new duty cycle
        IF ena(0) = '1' THEN 
		  
		  half_duty_new <= conv_integer(duty)*period/(2**bits_resolution)/2;   --determine clocks in 1/2 duty cycle
      END IF;
      FOR i IN 0 to phases-1 LOOP                                            --create a counter for each phase
        IF count(0) = period - 1 - i*period/phases THEN                       --end of period reached
          count(i) <= 0;                                                         --reset counter
          half_duty(i) <= half_duty_new;                                         --set most recent duty cycle value
        ELSE                                                                   --end of period not reached
          count(i) <= count(i) + 1;                                              --increment counter
        END IF;
      END LOOP;
      FOR i IN 0 to phases-1 LOOP                                            --control outputs for each phase
        IF count(i) = half_duty(i) THEN                                       --phase's falling edge reached
          pwm_out(i) <= '0';                                                     --deassert the pwm output
          pwm_n_out(i) <= '1';                                                   --assert the pwm inverse output
        ELSIF count(i) = period - half_duty(i) THEN                           --phase's rising edge reached
          pwm_out(i) <= '1';                                                     --assert the pwm output
          pwm_n_out(i) <= '0';                                                   --deassert the pwm inverse output
        END IF;
      END LOOP;
    END IF;
  END PROCESS;
END logic;
