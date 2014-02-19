library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

--
entity sectorbuffer is
  port (Clk : in std_logic;
        address : in std_logic_vector(8 downto 0);
        we : in std_logic;
        cs : in std_logic;
        oe : in std_logic;
        data_i : in std_logic_vector(7 downto 0);
        data_o : out std_logic_vector(7 downto 0);
        data_o_always : out std_logic_vector(7 downto 0)
        );
end sectorbuffer;

architecture Behavioral of sectorbuffer is

-- 512 x 8bit pre-initialised RAM
  type ram_t is array (0 to 511) of std_logic_vector(7 downto 0);
  signal ram : ram_t := (
    x"54",x"68",x"69",x"73",x"20",x"72",x"75",x"66",
    x"66",x"65",x"72",x"20",x"69",x"6e",x"74",x"65",
    x"6e",x"6e",x"74",x"69",x"6f",x"6e",x"6c",x"79",
    x"6c",x"65",x"66",x"74",x"20",x"62",x"6c",x"61",
    x"6e",x"6b",x"2e",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",

    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",

    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",

    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",

    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",

    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",

    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",

    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00");

begin

--process for read and write operation.
  PROCESS(Clk,cs,ram,address)
  BEGIN
    if(rising_edge(Clk)) then 
      if cs='1' then
        if(we='1') then
          ram(to_integer(unsigned(address))) <= data_i;
        end if;
        data_o <= ram(to_integer(unsigned(address)));
      end if;
    end if;
    if oe='1' then
      data_o <= ram(to_integer(unsigned(address)));
    else
      data_o <= "ZZZZZZZZ";
    end if;
    data_o_always <= ram(to_integer(unsigned(address)));
  END PROCESS;

end Behavioral;
