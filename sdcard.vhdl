use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;

entity sdcard is
  port (
    cpuclock : in std_logic;
    reset : in std_logic;
    
    ---------------------------------------------------------------------------
    -- fast IO port (clocked at core clock). 1MB address space
    ---------------------------------------------------------------------------
    cs : in std_logic;
    fastio_addr : in unsigned(3 downto 0);
    fastio_write : in std_logic;
    fastio_wdata : in unsigned(7 downto 0);
    fastio_rdata : out unsigned(7 downto 0);

    -------------------------------------------------------------------------
    -- Lines for the SDcard interface itself
    -------------------------------------------------------------------------
    cs_bo : out std_logic;
    sclk_o : out std_logic;
    mosi_o : out std_logic;
    miso_i : in  std_logic

    );
end sdcard;

architecture behavioural of sdcard is

  signal reg_sector_number : unsigned(31 downto 0) := x"00000000";
  signal reg_write_count : unsigned(7 downto 0) := x"00";
  
begin  -- behavioural
  
  process(cpuclock,fastio_addr,fastio_write,reset,cs) is
    variable register_number : unsigned(3 downto 0);
  begin
    -- fix SD interface lines for now.
    cs_bo <= '1';
    sclk_o <= '1';
    mosi_o <= '1';
    
    register_number := fastio_addr(3 downto 0);
    if cs='0' then
      -- Tri-state read lines if not selected
      fastio_rdata <= (others => 'Z');
    else
      if rising_edge(cpuclock) then
        -- Reading of registers
        if fastio_write='1' then            
          -- Tri-state read lines if writing
          fastio_rdata <= (others => 'Z');
          -- count number of times we have written to a register so that we can
          -- debug problems with writing registers
          reg_write_count <= reg_write_count + 1;
          case register_number is
            when x"0" =>
              -- control register
            when x"1" => reg_sector_number(7 downto 0) <= unsigned(fastio_wdata);
            when x"2" => reg_sector_number(15 downto 8) <= unsigned(fastio_wdata);
            when x"3" => reg_sector_number(23 downto 16) <= unsigned(fastio_wdata);
            when x"4" => reg_sector_number(31 downto 24) <= unsigned(fastio_wdata);
            when others => null;
          end case;
        else
          case register_number is
            when x"1" => fastio_rdata <= reg_sector_number(7 downto 0);
            when x"2" => fastio_rdata <= reg_sector_number(15 downto 8);
            when x"3" => fastio_rdata <= reg_sector_number(23 downto 16);
            when x"4" => fastio_rdata <= reg_sector_number(31 downto 24);
            when x"f" => fastio_rdata <= reg_write_count;
            when others => fastio_rdata <= (others => 'Z');
          end case;
        end if;
      end if;
    end if;
  end process;

end behavioural;
