use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;
use work.CommonPckg.all;

entity sdcardio is
  port (
    clock : in std_logic;
    reset : in std_logic;

    ---------------------------------------------------------------------------
    -- fast IO port (clocked at core clock). 1MB address space
    ---------------------------------------------------------------------------
    fastio_addr : in unsigned(19 downto 0);
    fastio_write : in std_logic;
    fastio_read : in std_logic;
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
      BLOCK_SIZE_G    : natural    := 512  -- Number of bytes in an SD card block or sector.
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

  signal rd_is           : std_logic := '0';
  signal wr_is           : std_logic := '0';
  signal hndShk_is       : std_logic := '0';
  signal hndShk_os       : std_logic;
  
  signal sd_continue     : std_logic := '0';
  signal sd_sector       : std_logic_vector(31 downto 0) := (others => '0');
  signal sd_rdata        : std_logic_vector(7 downto 0);
  signal sd_wdata        : std_logic_vector(7 downto 0) := (others => '0');
  signal sd_busy         : std_logic;   -- busy line from SD card itself
  signal sd_error        : std_logic;
  signal sd_errorcode    : std_logic_vector(15 downto 0);
  signal sd_reset        : std_logic := '1';
  
  -- IO mapped register to indicate if SD card interface is busy
  signal sdio_busy : std_logic := '0';
  signal sdio_error : std_logic := '0';

  -- 512 byte sector buffer
  type sector_buffer_t is array (0 to 511) of unsigned(7 downto 0);
  signal sector_buffer : sector_buffer_t;
  signal sector_buffer_mapped : std_logic := '0';

  -- Counter for reading/writing sector
  signal sector_offset : unsigned(8 downto 0);

  type sd_state_t is (Idle,
                      ReadSector,ReadingSector,ReadingSectorAckByte,DoneReadingSector,
                      WriteSector,WritingSector,WritingSectorAckByte,
                      DoneWritingSector);
  signal sd_state : sd_state_t := Idle;

  signal fsm_running : std_logic := '0';
  
begin  -- behavioural

  --**********************************************************************
  -- SD card controller module.
  --**********************************************************************
  u3 : SdCardCtrl
    generic map (
      -- XXX Fix FREQ_G to be based off pixelclock, not variable cpuclock
      FREQ_G => 48.0                   -- CPU at 48MHz
      )
    port map (
      clk_i      => clock,
      -- Internal control signals
      reset_i    => sd_reset,
      rd_i       => rd_is,
      wr_i       => wr_is,
      continue_i => sd_continue,
      addr_i     => sd_sector,
      data_i     => sd_wdata,
      data_o     => sd_rdata,
      busy_o     => sd_busy,
      hndShk_i   => hndShk_is,
      hndShk_o   => hndShk_os,
      error_o    => sd_errorcode,
      -- External signals to SD card slot
      cs_bo      => cs_bo,
      sclk_o     => sclk_o,
      mosi_o     => mosi_o,
      miso_i     => miso_i
      );

  -- XXX also implement F1011 floppy controller emulation.
  process (clock,fastio_addr,fastio_wdata,sector_buffer_mapped,sdio_busy,
           sd_reset,fastio_read,sd_sector,fastio_write) is
  begin
    if fastio_read='1' then
      if (fastio_addr(19 downto 4) = x"D168"
          or fastio_addr(19 downto 4) = x"D368") then
        -- microSD controller registers
        case fastio_addr(3 downto 0) is
          when x"0" =>
            -- status / command register
            -- error status in bit 6 so that V flag can be used for check      
            fastio_rdata(7) <= '0';
            fastio_rdata(6) <= sdio_error;
            fastio_rdata(5 downto 3) <= "000";
            fastio_rdata(2) <= sd_reset;
            fastio_rdata(1) <= sector_buffer_mapped;
            fastio_rdata(0) <= sdio_busy;
          when x"1" => fastio_rdata <= unsigned(sd_sector(7 downto 0));
          when x"2" => fastio_rdata <= unsigned(sd_sector(15 downto 8));
          when x"3" => fastio_rdata <= unsigned(sd_sector(23 downto 16));
          when x"4" => fastio_rdata <= unsigned(sd_sector(31 downto 24));        
          when x"5" => fastio_rdata <= unsigned(sd_errorcode(7 downto 0));        
          when x"6" => fastio_rdata <= unsigned(sd_errorcode(15 downto 8));
          when x"7" => fastio_rdata <= to_unsigned(sd_state_t'pos(sd_state),8);
          when others => fastio_rdata <= (others => 'Z');
        end case;
      elsif (sector_buffer_mapped='1') and (sdio_busy='0') and
        (fastio_addr(19 downto 9)&'0' = x"D1E"
         or fastio_addr(19 downto 9)&'0' = x"D3E") then
        -- Map sector buffer at $DE00-$DFFF when required
        fastio_rdata <= sector_buffer(to_integer(fastio_addr(8 downto 0)));
      else
        -- Otherwise tristate output
        fastio_rdata <= (others => 'Z');
      end if;
    else
      fastio_rdata <= (others => 'Z');
    end if;

    if rising_edge(clock) and fastio_write='1' then
      if (fastio_addr(19 downto 4) = x"D168"
          or fastio_addr(19 downto 4) = x"D368") then
        -- microSD controller registers
        case fastio_addr(3 downto 0) is
          when x"0" =>
            -- status / command register
            case fastio_wdata is
              when x"00" =>
                -- Reset SD card
                sd_reset <= '1';
                if fsm_running='0' then
                  sd_state <= Idle;
                else
                  fsm_running <= '0';
                end if;
              when x"01" =>
                -- End reset
                sd_reset <= '0';
                if fsm_running='0' then
                  sd_state <= Idle;
                else
                  fsm_running <= '0';
                end if;
              when x"02" =>
                -- Read sector
                if sdio_busy='1' or fsm_running='1' then
                  sdio_error <= '1';
                else
                  sd_state <= ReadSector;
                  fsm_running <= '1';
                end if;
              when x"03" =>
                -- Write sector
                if sdio_busy='1' or fsm_running='1' then
                  sdio_error <= '1';
                else                  
                  sd_state <= WriteSector;
                  fsm_running <= '1';
                end if;
              when x"81" => sector_buffer_mapped<='1';
              when x"82" => sector_buffer_mapped<='0';
              when others =>
                sdio_error <= '1';
            end case;
          when x"1" => sd_sector(7 downto 0) <= std_logic_vector(fastio_wdata);
          when x"2" => sd_sector(15 downto 8) <= std_logic_vector(fastio_wdata);
          when x"3" => sd_sector(23 downto 16) <= std_logic_vector(fastio_wdata);
          when x"4" => sd_sector(31 downto 24) <= std_logic_vector(fastio_wdata);
          when others => null;
        end case;
      elsif (sector_buffer_mapped='1') and (sdio_busy='0') and
        (fastio_addr(19 downto 9)&'0' = x"D1E"
         or fastio_addr(19 downto 9)&'0' = x"D3E") then
        -- Map sector buffer at $DE00-$DFFF when required
        sector_buffer(to_integer(fastio_addr(8 downto 0))) <= fastio_wdata;
      end if;
    end if;    
    
  end process;

  process (clock) is
  begin
    if rising_edge(clock) then
      if fsm_running='1' then
        case sd_state is
          when Idle => fsm_running<='0';
          when ReadSector =>
            -- Begin reading a sector into the buffer
            if sd_busy='1' then
              rd_is <= '0';
              sd_state <= ReadingSector;
              sdio_busy <= '1';
              sector_offset <= (others => '0');
            else
              rd_is <= '1';
            end if;
          when ReadingSector =>
            if hndShk_os='1' then
              -- A byte is ready to read, so store it
              sector_buffer(to_integer(sector_offset)) <= unsigned(sd_rdata);
              -- Tell controller that we have latched it
              hndShk_is <= '1';
              sd_state <= ReadingSectorAckByte;
              sector_offset <= sector_offset + 1;
            end if;
          when ReadingSectorAckByte =>
            -- Wait until controller acknowledges that we have acked it
            if hndShk_os='0' then
              hndShk_is <= '0';
              if sector_offset = "000000000" then
                -- sector offset has wrapped back to zero, so we must have
                -- read the whole sector.
                sd_state <= DoneReadingSector;
              else
                -- Still more bytes to read.
                sd_state <= ReadingSector;
              end if;
            end if;
          when WriteSector =>
            -- Begin writing a sector into the buffer
            if sd_busy='1' then
              wr_is <= '0';
              sdio_busy <= '1';
              sd_state <= WritingSector;
              sector_offset <= (others => '0');
            else
              wr_is <= '1';
            end if;
          when WritingSector =>
            if hndShk_os='1' then
              -- A byte is ready to read, so store it
              sd_wdata <= std_logic_vector(sector_buffer(to_integer(sector_offset)));
              -- Tell controller that we have latched it
              hndShk_is <= '1';
              sd_state <= WritingSectorAckByte;
              sector_offset <= sector_offset + 1;
            end if;
          when WritingSectorAckByte =>
            -- Wait until controller acknowledges that we have acked it
            if hndShk_os='0' then
              hndShk_is <= '0';
              if sector_offset = "000000000" then
                -- sector offset has wrapped back to zero, so we must have
                -- read the whole sector.
                sd_state <= DoneWritingSector;
              else
                -- Still more bytes to read.
                sd_state <= WritingSector;
              end if;
            end if;
          when DoneReadingSector =>
            sdio_busy <= '0';
            sd_state <= Idle;
          when DoneWritingSector =>
            sdio_busy <= '0';
            sd_state <= Idle;
          when others =>
            sd_state <= Idle;
            sdio_busy <= '0';
            sdio_error <= '1';
        end case;    
      else
        sd_state <= Idle;
      end if;
    end if;
  end process;
  
end behavioural;
