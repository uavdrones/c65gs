library ieee;
USE ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;
use work.all;
use work.debugtools.all;

entity tb_iomapper is
  
end tb_iomapper;

architecture behavior of tb_iomapper is

  component iomapper is
    port (Clk : in std_logic;
          pixelclk : in std_logic;
          phi0 : in std_logic;
          reset : in std_logic;
          irq : out std_logic;
          nmi : out std_logic;
          address : in std_logic_vector(19 downto 0);
          r : in std_logic;
          w : in std_logic;
          data_i : in std_logic_vector(7 downto 0);
          data_o : out std_logic_vector(7 downto 0);
          kickstart_o : out std_logic_vector(7 downto 0);

          ps2data : in std_logic;
          ps2clock : in std_logic;

          -------------------------------------------------------------------------
          -- Lines for the SDcard interface itself
          -------------------------------------------------------------------------
          cs_bo : out std_logic;
          sclk_o : out std_logic;
          mosi_o : out std_logic;
          miso_i : in  std_logic;
          
          seg_led : out unsigned(31 downto 0);
          
          colourram_at_dc00 : in std_logic
          );
  end component;

  signal cpuclock : std_logic := '1';
  signal pixelclock : std_logic := '1';
  signal phi0 : std_logic := '1';
  signal reset : std_logic := '0';
  signal irq : std_logic;
  signal nmi : std_logic;
  signal address : std_logic_vector(19 downto 0) := (others => '0');
  signal r : std_logic := '0';
  signal w : std_logic := '0';
  signal data_i : std_logic_vector(7 downto 0) := x"00";
  signal data_o : std_logic_vector(7 downto 0);
  signal kickstart_o : std_logic_vector(7 downto 0);
  
  signal ps2data : std_logic := '1';
  signal ps2clock : std_logic := '1';
  
  -------------------------------------------------------------------------
  -- Lines for the SDcard interface itself
  -------------------------------------------------------------------------
  signal cs_bo : std_logic;
  signal sclk_o : std_logic;
  signal mosi_o : std_logic;
  signal miso_i : std_logic := '1';
  
  signal seg_led : unsigned(31 downto 0);
  
  signal colourram_at_dc00 : std_logic := '0';
  
begin  
  iomapper0: iomapper port map (
    clk => cpuclock,
    pixelclk => pixelclock,
    phi0 => phi0,
    reset => reset,
    irq => irq, -- (but we might like to AND this with the hardware IRQ button)
    nmi => nmi, -- (but we might like to AND this with the hardware IRQ button)
    address => address,
    r => r, w => w,
    data_i => data_i, data_o => data_o,
    kickstart_o => kickstart_o,
    colourram_at_dc00 => colourram_at_dc00,
    seg_led => seg_led,

    cs_bo => cs_bo,
    sclk_o => sclk_o,
    mosi_o => mosi_o,
    miso_i => miso_i,
    
    ps2data => ps2data,
    ps2clock => ps2clock
    );

  process
    -- purpose: tick clocks
    procedure tick is
    begin  -- tick
      wait for 2.5 ns;
      pixelclock <= '1';
      cpuclock <= '1';
      wait for 2.5 ns;     
      pixelclock <= '0';
      
      wait for 2.5 ns;      
      pixelclock <= '1';
      cpuclock <= '0';
      wait for 2.5 ns;     
      pixelclock <= '0';
    end tick;
  begin  -- process tb
    
    report "beginning simulation" severity note;

    reset <= '0';
    for i in 1 to 10 loop
      wait for 2.5 ns;      
      pixelclock <= '1';
      wait for 2.5 ns;
      pixelclock <= '0';
    end loop;  -- i
    reset <= '1';
    report "reset released" severity note;
    
    r <= '1'; w <= '0'; address <= x"D3C1F";
    tick;
    report "write_count=$" & to_hstring(data_o) severity note;
    tick;
    r <= '0'; w <= '1'; address <= x"D3C01"; data_i <= x"55";
    tick;
    r <= '1'; w <= '0'; address <= x"D3C01";
    tick;
    report "$DC00=$" & to_hstring(data_o) severity note;
    r <= '1'; w <= '0'; address <= x"D3C1F";
    tick;
    report "write_count=$" & to_hstring(data_o) severity note;
    r <= '1'; w <= '0'; address <= x"FE000";
    tick;
    report "kickstart_read=$" & to_hstring(kickstart_o) severity note;

    -- Map sector buffer
    r <= '0'; w <= '1'; address <= x"D3680"; data_i <= x"81";
    tick;
    r <= '1'; w <= '0'; address <= x"D3680";
    tick;
    report "SD controller status = $" & to_hstring(data_o) severity note;

    r <= '1'; w <= '0'; address <= x"D3E00";
    tick;
    report "Frist byte of sector buffer = $" & to_hstring(data_o) severity note;
    r <= '1'; w <= '0'; address <= x"D3E01";
    tick;
    report "Second byte of sector buffer = $" & to_hstring(data_o) severity note;
    r <= '0'; w <= '1'; address <= x"D3E00"; data_i <= x"AA";
    tick;
    r <= '1'; w <= '0'; address <= x"D3E00";
    tick;
    report "Frist byte of sector buffer after write = $" & to_hstring(data_o) severity note;
    r <= '1'; w <= '0'; address <= x"D3E01";
    tick;
    report "Second byte of sector buffer = $" & to_hstring(data_o) severity note;
    
    assert false report "End of simulation" severity failure;
  end process;
end behavior;

