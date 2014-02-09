entity sdcardio is
  port (
    cpuclock : in std_logic;
    reset : in std_logic;
    irq : out std_logic := '1';

    ---------------------------------------------------------------------------
    -- fast IO port (clocked at core clock). 1MB address space
    ---------------------------------------------------------------------------
    cs : in std_logic;
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

