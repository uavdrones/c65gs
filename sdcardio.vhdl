entity sdcardio is
  port (
    clock : in std_logic;
    reset : in std_logic;
    irq : out std_logic := 'Z';

    ---------------------------------------------------------------------------
    -- fast IO port (clocked at core clock). 1MB address space
    ---------------------------------------------------------------------------
    fastio_addr : in unsigned(7 downto 0);
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
end sdcardio;

architecture behavioural of sdcardio is

  component SdCardCtrl is
    generic (
      FREQ_G          : real       := 100.0;  -- Master clock frequency (MHz).
      INIT_SPI_FREQ_G : real       := 0.4;  -- Slow SPI clock freq. during initialization (MHz).
      SPI_FREQ_G      : real       := 25.0;  -- Operational SPI freq. to the SD card (MHz).
      BLOCK_SIZE_G    : natural    := 512;  -- Number of bytes in an SD card block or sector.
      CARD_TYPE_G     : CardType_t := SD_CARD_E  -- Type of SD card connected to this controller.
      );
    port (
      -- Host-side interface signals.
      clk_i      : in  std_logic;       -- Master clock.
      reset_i    : in  std_logic := NO;  -- active-high, synchronous  reset.
      rd_i       : in  std_logic := NO;  -- active-high read block request.
      wr_i       : in  std_logic := NO;  -- active-high write block request.
      continue_i : in  std_logic := NO;  -- If true, inc address and continue R/W.
      addr_i     : in  std_logic_vector(31 downto 0) := x"00000000";  -- Block address.
      data_i     : in  std_logic_vector(7 downto 0)  := x"00";  -- Data to write to block.
      data_o     : out std_logic_vector(7 downto 0)  := x"00";  -- Data read from block.
      busy_o     : out std_logic;  -- High when controller is busy performing some operation.
      hndShk_i   : in  std_logic;  -- High when host has data to give or has taken data.
      hndShk_o   : out std_logic;  -- High when controller has taken data or has data to give.
      error_o    : out std_logic_vector(15 downto 0) := (others => NO);
      -- I/O signals to the external SD card.
      cs_bo      : out std_logic := HI;  -- Active-low chip-select.
      sclk_o     : out std_logic := LO;  -- Serial clock to SD card.
      mosi_o     : out std_logic := HI;  -- Serial data output to SD card.
      miso_i     : in  std_logic := ZERO  -- Serial data input from SD card.
      );
  end component;

  signal rd_is           : std_logic;
  signal wr_is           : std_logic;
  signal rdFromPc_s      : std_logic;
  signal wrFromPc_s      : std_logic;
  signal continue_is     : std_logic;
  signal sdsector        : std_logic_vector(31 downto 0);
  signal sd_rdata        : std_logic_vector(7 downto 0);
  signal sd_wdata        : std_logic_vector(7 downto 0);
  signal sd_busy         : std_logic;
  signal sd_error        : std_logic_vector(15 downto 0);

begin  -- behavioural

    --**********************************************************************
  -- SD card controller module.
  --**********************************************************************
  u3 : SdCardCtrl
    generic map (
      -- XXX Fix FREQ_G to be based off pixelclock, not variable cpuclock
      FREQ_G => 48.0;                   -- CPU at 48MHz
      )
    port map (
      clk_i      => cpuclock,
      -- Internal control signals
      reset_i    => reset_is,
      rd_i       => rd_is,
      wr_i       => wr_is,
      continue_i => sd_continue,
      addr_i     => sd_sector,
      data_i     => sd_wdata,
      data_o     => sd_rdata,
      busy_o     => sd_busy,
      hndShk_i   => hndShk_is,
      hndShk_o   => hndShk_os,
      error_o    => sd_error,
      -- External signals to SD card slot
      cs_bo      => cs_bo,
      sclk_o     => sclk_o,
      mosi_o     => mosi_o,
      miso_i     => miso_i
      );

  process (cpuclock,fastio_addr,fastio_wdata) is
    if fastio_read='1' then
      if (fastio_addr(19 downto 4) = x"D168"
          or fastio_addr(19 downto 4) = x"D368") then
        -- microSD controller registers
        case fastio_addr(3 downto 0) use
          when x"0" =>
            -- status / command register
            fastio_rdata <= "0000000" & busy_os;
          when x"1" => fastio_rdata <= sd_sector(7 downto 0);
          when x"2" => fastio_rdata <= sd_sector(15 downto 8);
          when x"3" => fastio_rdata <= sd_sector(23 downto 16);
          when x"4" => fastio_rdata <= sd_sector(31 downto 24);        
          when others => fastio_rdata <= (others => 'Z');
        end case;
      elsif sector_buffer_mapped='1' and
        (fastio_addr(19 downto 9)&'0' = x"D1E"
          or fastio_addr(19 downto 9)&'0' = x"D3E") then
        -- Map sector buffer at $DE00-$DFFF when required
        fastio_rdata <= sector_buffer(to_integer(fastio_addr(8 downto 0)))
      else
        -- Otherwise tristate output
        fastio_rdata <= (others => 'Z');
      end if;
    else
      fastio_rdata <= (others => 'Z');
    end if;
  end process;
  
end behavioural;
