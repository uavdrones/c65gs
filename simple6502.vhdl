use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;

entity simple6502 is
  port (
    Clock : in std_logic;
    reset : in std_logic;
    irq : in std_logic;
    nmi : in std_logic;
    monitor_pc : out std_logic_vector(15 downto 0);
    monitor_opcode : out std_logic_vector(7 downto 0);
    monitor_a : out std_logic_vector(7 downto 0);
    monitor_x : out std_logic_vector(7 downto 0);
    monitor_y : out std_logic_vector(7 downto 0);
    monitor_sp : out std_logic_vector(7 downto 0);
    monitor_p : out std_logic_vector(7 downto 0);
    
    ---------------------------------------------------------------------------
    -- fast IO port (clocked at core clock). 1MB address space
    ---------------------------------------------------------------------------
    fastio_addr : out std_logic_vector(19 downto 0);
    fastio_read : out std_logic;
    fastio_write : out std_logic;
    fastio_wdata : out std_logic_vector(7 downto 0);
    fastio_rdata : in std_logic_vector(7 downto 0)
    );
end entity simple6502;

architecture Behavioural of simple6502 is
-- CPU RAM bank selection registers.
-- Each 4KB (12 address bits) can be made to point to a section of memory.
-- Sections have 16bit addresses, for a total of 28bits (256MB) of address
-- space.  It also makes it possible to multiple 4KB banks point to the same
-- block of RAM.
  type bank_register_set is array (0 to 15) of std_logic_vector(15 downto 0);
  signal ram_bank_registers_read : bank_register_set;
  signal ram_bank_registers_write : bank_register_set;
  signal ram_bank_registers_instructions : bank_register_set;

-- CPU internal state
  signal flag_c : std_logic;        -- carry flag
  signal flag_z : std_logic;        -- zero flag
  signal flag_d : std_logic;        -- decimal mode flag
  signal flag_n : std_logic;        -- negative flag
  signal flag_v : std_logic;        -- positive flag
  signal flag_i : std_logic;        -- interrupt disable flag

  signal reg_a : unsigned(7 downto 0);
  signal reg_x : unsigned(7 downto 0);
  signal reg_y : unsigned(7 downto 0);
  signal reg_sp : unsigned(7 downto 0);
  signal reg_pc : unsigned(15 downto 0);

  -- Flags to detect interrupts
  signal nmi_pending : std_logic := '0';
  signal irq_pending : std_logic := '0';
  signal nmi_state : std_logic := '1';
  signal irq_state : std_logic := '1';
  -- Interrupt/reset vector being used
  signal vector : unsigned(15 downto 0);
  
-- Indicate source of operand for instructions
-- Note that ROM is actually implemented using
-- power-on initialised RAM in the FPGA mapped via our io interface.
  signal accessing_fastio : std_logic;
  signal accessing_ram : std_logic;
  signal accessing_slowram : std_logic;

  type processor_state is (
    -- When CPU first powers up, or reset is bought low
    ResetLow,
    -- States for handling interrupts and reset
    Interrupt,VectorRead,VectorRead1,VectorRead2,VectorRead3,
    InstructionFetch
    );
  signal state : processor_state := ResetLow;  -- start processor in reset state
  -- For memory access we push the processor state to follow once the memory
  -- access is complete.
  signal pending_state : processor_state;
  
  type instruction is (
    -- 6502/6510 legal and illegal ops
    I_ADC,I_AHX,I_ALR,I_ANC,I_AND,I_ARR,I_ASL,I_AXS,
    I_BCC,I_BCS,I_BEQ,I_BIT,I_BMI,I_BNE,I_BPL,I_BRK,
    I_BVC,I_BVS,I_CLC,I_CLD,I_CLI,I_CLV,I_CMP,I_CPX,
    I_CPY,I_DCP,I_DEC,I_DEX,I_DEY,I_EOR,I_INC,I_INX,
    I_INY,I_ISC,I_JMP,I_JSR,I_KIL,I_LAS,I_LAX,I_LDA,
    I_LDX,I_LDY,I_LSR,I_NOP,I_ORA,I_PHA,I_PHP,I_PLA,
    I_PLP,I_RLA,I_ROL,I_ROR,I_RRA,I_RTI,I_RTS,I_SAX,
    I_SBC,I_SEC,I_SED,I_SEI,I_SHX,I_SHY,I_SLO,I_SRE,
    I_STA,I_STX,I_STY,I_TAS,I_TAX,I_TAY,I_TSX,I_TXA,
    I_TXS,I_TYA,I_XAA,
    -- 65GS02 special ops
    I_SETMAP
    );

  type ilut8bit is array(0 to 255) of instruction;
  constant instruction_lut : ilut8bit := (
    I_BRK,  I_ORA,  I_SETMAP,  I_SLO,  I_NOP,  I_ORA,  I_ASL,  I_SLO,  I_PHP,  I_ORA,  I_ASL,  I_ANC,  I_NOP,  I_ORA,  I_ASL,  I_SLO, 
    I_BPL,  I_ORA,  I_KIL,  I_SLO,  I_NOP,  I_ORA,  I_ASL,  I_SLO,  I_CLC,  I_ORA,  I_NOP,  I_SLO,  I_NOP,  I_ORA,  I_ASL,  I_SLO, 
    I_JSR,  I_AND,  I_KIL,  I_RLA,  I_BIT,  I_AND,  I_ROL,  I_RLA,  I_PLP,  I_AND,  I_ROL,  I_ANC,  I_BIT,  I_AND,  I_ROL,  I_RLA, 
    I_BMI,  I_AND,  I_KIL,  I_RLA,  I_NOP,  I_AND,  I_ROL,  I_RLA,  I_SEC,  I_AND,  I_NOP,  I_RLA,  I_NOP,  I_AND,  I_ROL,  I_RLA, 
    I_RTI,  I_EOR,  I_KIL,  I_SRE,  I_NOP,  I_EOR,  I_LSR,  I_SRE,  I_PHA,  I_EOR,  I_LSR,  I_ALR,  I_JMP,  I_EOR,  I_LSR,  I_SRE, 
    I_BVC,  I_EOR,  I_KIL,  I_SRE,  I_NOP,  I_EOR,  I_LSR,  I_SRE,  I_CLI,  I_EOR,  I_NOP,  I_SRE,  I_NOP,  I_EOR,  I_LSR,  I_SRE, 
    I_RTS,  I_ADC,  I_KIL,  I_RRA,  I_NOP,  I_ADC,  I_ROR,  I_RRA,  I_PLA,  I_ADC,  I_ROR,  I_ARR,  I_JMP,  I_ADC,  I_ROR,  I_RRA, 
    I_BVS,  I_ADC,  I_KIL,  I_RRA,  I_NOP,  I_ADC,  I_ROR,  I_RRA,  I_SEI,  I_ADC,  I_NOP,  I_RRA,  I_NOP,  I_ADC,  I_ROR,  I_RRA, 
    I_NOP,  I_STA,  I_KIL,  I_SAX,  I_STY,  I_STA,  I_STX,  I_SAX,  I_DEY,  I_NOP,  I_TXA,  I_XAA,  I_STY,  I_STA,  I_STX,  I_SAX, 
    I_BCC,  I_STA,  I_NOP,  I_AHX,  I_STY,  I_STA,  I_STX,  I_SAX,  I_TYA,  I_STA,  I_TXS,  I_TAS,  I_SHY,  I_STA,  I_SHX,  I_AHX, 
    I_LDY,  I_LDA,  I_LDX,  I_LAX,  I_LDY,  I_LDA,  I_LDX,  I_LAX,  I_TAY,  I_LDA,  I_TAX,  I_LAX,  I_LDY,  I_LDA,  I_LDX,  I_LAX, 
    I_BCS,  I_LDA,  I_NOP,  I_LAX,  I_LDY,  I_LDA,  I_LDX,  I_LAX,  I_CLV,  I_LDA,  I_TSX,  I_LAS,  I_LDY,  I_LDA,  I_LDX,  I_LAX, 
    I_CPY,  I_CMP,  I_KIL,  I_DCP,  I_CPY,  I_CMP,  I_DEC,  I_DCP,  I_INY,  I_CMP,  I_DEX,  I_AXS,  I_CPY,  I_CMP,  I_DEC,  I_DCP, 
    I_BNE,  I_CMP,  I_NOP,  I_DCP,  I_NOP,  I_CMP,  I_DEC,  I_DCP,  I_CLD,  I_CMP,  I_NOP,  I_DCP,  I_NOP,  I_CMP,  I_DEC,  I_DCP, 
    I_CPX,  I_SBC,  I_KIL,  I_ISC,  I_CPX,  I_SBC,  I_INC,  I_ISC,  I_INX,  I_SBC,  I_NOP,  I_SBC,  I_CPX,  I_SBC,  I_INC,  I_ISC, 
    I_BEQ,  I_SBC,  I_NOP,  I_ISC,  I_NOP,  I_SBC,  I_INC,  I_ISC,  I_SED,  I_SBC,  I_NOP,  I_ISC,  I_NOP,  I_SBC,  I_INC,  I_ISC);

  type addressingmode is (
    M_implied,M_immediate,M_accumulator,
    M_zeropage,M_zeropageX,M_zeropageY,
    M_absolute,M_absoluteY,M_absoluteX,
    M_relative,M_indirect,M_indirectX,M_indirectY);

  type mlut8bit is array(0 to 255) of addressingmode;
  constant mode_lut : mlut8bit := (
    -- 00
    M_implied,  M_indirectX,  M_implied,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage,
    M_implied,  M_immediate,  M_accumulator,  M_immediate,  M_absolute,  M_absolute,  M_absolute,  M_absolute, 
    -- 10
    M_relative,  M_indirectY,  M_immediate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
    M_implied,  M_absoluteY,  M_accumulator,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX,
    -- 20
    M_absolute,  M_indirectX,  M_immediate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
    M_implied,  M_immediate,  M_accumulator,  M_immediate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
    -- 30
    M_relative,  M_indirectY,  M_immediate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
    M_implied,  M_absoluteY,  M_accumulator,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX, 
    -- 40
    M_implied,  M_indirectX,  M_immediate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
    M_implied,  M_immediate,  M_accumulator,  M_immediate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
    -- 50
    M_relative,  M_indirectY,  M_immediate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
    M_implied,  M_absoluteY,  M_accumulator,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX, 
    -- 60
    M_implied,  M_indirectX,  M_immediate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
    M_implied,  M_immediate,  M_accumulator,  M_immediate,  M_indirect,  M_absolute,  M_absolute,  M_absolute,
    -- 70
    M_relative,  M_indirectY,  M_immediate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
    M_implied,  M_absoluteY,  M_accumulator,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX,
    -- 80
    M_immediate,  M_indirectX,  M_immediate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
    M_implied,  M_immediate,  M_implied,  M_immediate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
    -- 90
    M_relative,  M_indirectY,  M_immediate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageY,  M_zeropageY, 
    M_implied,  M_absoluteY,  M_implied,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_implied,  M_absoluteY,
    -- A0
    M_immediate,  M_indirectX,  M_immediate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
    M_implied,  M_immediate,  M_implied,  M_immediate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
    -- B0
    M_relative,  M_indirectY,  M_immediate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageY,  M_zeropageY, 
    M_implied,  M_absoluteY,  M_implied,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteY,  M_absoluteY,
    -- C0
    M_immediate,  M_indirectX,  M_immediate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
    M_implied,  M_immediate,  M_implied,  M_immediate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
    -- D0
    M_relative,  M_indirectY,  M_immediate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
    M_implied,  M_absoluteY,  M_implied,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX,
    -- E0
    M_immediate,  M_indirectX,  M_immediate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
    M_implied,  M_immediate,  M_implied,  M_immediate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
    -- F0
    M_relative,  M_indirectY,  M_immediate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
    M_implied,  M_absoluteY,  M_implied,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX);
  
begin
  process(clock)

    procedure reset_cpu_state is
  begin
    -- Default register values
    reg_a <= x"AA";
    reg_x <= x"11";
    reg_y <= x"22";
    reg_sp <= x"ff";

    -- Default CPU flags
    flag_c <= '0';
    flag_d <= '0';
    flag_i <= '1';                -- start with IRQ disabled
    flag_z <= '0';
    flag_n <= '0';
    flag_v <= '0';

    -- Default memory map (C64 like, but with enhanced IO and kernel)
    -- Map first bank of fast RAM at $0000 - $CFFF
    for i in 0 to 12 loop
      ram_bank_registers_read(i)<=std_logic_vector(to_unsigned(i,16));
      ram_bank_registers_write(i)<=std_logic_vector(to_unsigned(i,16));
      ram_bank_registers_instructions(i)<=std_logic_vector(to_unsigned(i,16));
    end loop;  -- i
    -- enhanced IO at $D000-$DFFF
    ram_bank_registers_read(13) <= x"FFD3";  
    ram_bank_registers_write(13) <= x"FFD3";
    ram_bank_registers_instructions(13) <= x"FFD3";
    -- Kernel65 ROM at $E000-$FFFF (writes redirect to "underlying" fast RAM)
    ram_bank_registers_read(14) <= x"FFFE";
    ram_bank_registers_write(14) <= x"000E";
    ram_bank_registers_instructions(14) <= x"FFFE";
    ram_bank_registers_read(15) <= x"FFFF";
    ram_bank_registers_write(15) <= x"000F";
    ram_bank_registers_instructions(15) <= x"FFFF";       
  end procedure reset_cpu_state;

  procedure check_for_interrupts is
  begin
    if nmi = '0' and nmi_state = '1' then
      nmi_pending <= '1';        
    end if;
    nmi_state <= nmi;
    if irq = '0' and irq_state = '1' then
      irq_pending <= '1';        
    end if;
    irq_state <= irq;
  end procedure check_for_interrupts;

  -- purpose: read from a 16-bit CPU address
  procedure read_address (
    address    : in unsigned(15 downto 0);
    next_state : in processor_state) is
  begin  -- read_address
    -- Schedule the memory read from the appropriate source.
    -- Once read, we then resume processing from the specified state.
    pending_state <= next_state;
  end read_address;

  -- purpose: obtain the byte of memory that has been read
  impure function read_data
    return unsigned is
  begin  -- read_data
    if accessing_fastio='1' then
      return unsigned(fastio_rdata);
    else
      return x"FF";
    end if;
  end read_data;
  
  variable virtual_reg_p : std_logic_vector(7 downto 0);
  begin
    if rising_edge(clock) then
      -- Clear memory access interfaces
      fastio_addr <= (others => '1');
      fastio_read <= '0';
      fastio_write <= '0';
      
      -- Generate virtual processor status register for convenience
      virtual_reg_p(7) := flag_n;
      virtual_reg_p(6) := flag_v;
      virtual_reg_p(5) := '1';
      virtual_reg_p(4) := '0';
      virtual_reg_p(3) := flag_d;
      virtual_reg_p(2) := flag_i;
      virtual_reg_p(1) := flag_z;
      virtual_reg_p(0) := flag_c;

      if reset = '0' or state = ResetLow then
        -- reset cpu
        state <= VectorRead;
        vector <= x"FFFC";
        reset_cpu_state;
      else
        -- CPU running, so do CPU state machine
        check_for_interrupts;
        case state is
          when VectorRead => read_address(vector,VectorRead2);
          when VectorRead2 => reg_pc(7 downto 0) <= read_data; read_address(vector+1,VectorRead3);
          when VectorRead3 => reg_pc(15 downto 8) <= read_data; state <= InstructionFetch;
          when others => null;
        end case;
      end if;
    end if;
  end process;
end Behavioural;