-- Accelerated 6502-like CPU for the C65GS
--
-- Written by
--    Paul Gardner-Stephen <hld@c64.org>
--
-- * ADC/SBC algorithm derived from  6510core.c - WICE MOS6510 emulation core.
-- *   Written by
-- *    Ettore Perazzoli <ettore@comm2000.it>
-- *    Andreas Boose <viceteam@t-online.de>
-- *
-- *  This program is free software; you can redistribute it and/or modify
-- *  it under the terms of the GNU Lesser General Public License as
-- *  published by the Free Software Foundation; either version 2 of the
-- *  License, or (at your option) any later version.
-- *
-- *  This program is distributed in the hope that it will be useful,
-- *  but WITHOUT ANY WARRANTY; without even the implied warranty of
-- *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- *  GNU General Public License for more details.
-- *
-- *  You should have received a copy of the GNU Lesser General Public License
-- *  along with this program; if not, write to the Free Software
-- *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
-- *  02111-1307  USA.

use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;

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
    monitor_z : out std_logic_vector(7 downto 0);
    monitor_sp : out std_logic_vector(15 downto 0);
    monitor_p : out std_logic_vector(7 downto 0);
    monitor_state : out std_logic_vector(7 downto 0);

    ---------------------------------------------------------------------------
    -- Memory access interface used by monitor
    ---------------------------------------------------------------------------
    monitor_mem_address : in std_logic_vector(27 downto 0);
    monitor_mem_rdata : out unsigned(7 downto 0);
    monitor_mem_wdata : in unsigned(7 downto 0);
    monitor_mem_read : in std_logic;
    monitor_mem_write : in std_logic;
    monitor_mem_setpc : in std_logic;
    monitor_mem_register : out unsigned(15 downto 0);
    monitor_mem_attention_request : in std_logic;
    monitor_mem_attention_granted : out std_logic := '0';
    monitor_mem_trace_mode : in std_logic;
    monitor_mem_stage_trace_mode : in std_logic;
    monitor_mem_trace_toggle : in std_logic;
    
    ---------------------------------------------------------------------------
    -- Interface to FastRAM in video controller (just 128KB for now)
    ---------------------------------------------------------------------------
    fastram_we : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    fastram_read : OUT STD_LOGIC;
    fastram_write : OUT STD_LOGIC;
    fastram_address : OUT STD_LOGIC_VECTOR(13 DOWNTO 0);
    fastram_datain : OUT STD_LOGIC_VECTOR(63 DOWNTO 0);
    fastram_dataout : IN STD_LOGIC_VECTOR(63 DOWNTO 0);

    ---------------------------------------------------------------------------
    -- Interface to Slow RAM (16MB cellular RAM chip)
    ---------------------------------------------------------------------------
    slowram_addr : out std_logic_vector(22 downto 0);
    slowram_we : out std_logic;
    slowram_ce : out std_logic;
    slowram_oe : out std_logic;
    slowram_lb : out std_logic;
    slowram_ub : out std_logic;
    slowram_data : inout std_logic_vector(15 downto 0);
    
    ---------------------------------------------------------------------------
    -- fast IO port (clocked at core clock). 1MB address space
    ---------------------------------------------------------------------------
    fastio_addr : inout std_logic_vector(19 downto 0);
    fastio_read : inout std_logic;
    fastio_write : out std_logic;
    fastio_wdata : out std_logic_vector(7 downto 0);
    fastio_rdata : inout std_logic_vector(7 downto 0)
    );
end entity simple6502;

architecture Behavioural of simple6502 is

  signal slowram_lohi : std_logic;
  signal slowram_counter : unsigned(7 downto 0);
  -- SlowRAM has 70ns access time, so need some wait states.
  -- The wait states
  signal slowram_waitstates : unsigned(7 downto 0) := x"05";
  
-- CPU RAM bank selection registers.
-- Each 4KB (12 address bits) can be made to point to a section of memory.
-- Sections have 16bit addresses, for a total of 28bits (256MB) of address
-- space.  It also makes it possible to multiple 4KB banks point to the same
-- block of RAM.
  type bank_register_set is array (0 to 15) of unsigned(15 downto 0);
  signal ram_bank_registers_read : bank_register_set;
  signal ram_bank_registers_write : bank_register_set;
  signal ram_bank_registers_instructions : bank_register_set;

  -- For debugging, keep a log of the processor states for the last instruction.
  -- It's a bit of a kluge to use this type of array, but it is convenient.
  signal recent_states : bank_register_set;
  
  signal fastram_byte_number : unsigned(2 DOWNTO 0);
  
-- CPU internal state
  signal flag_c : std_logic;        -- carry flag
  signal flag_z : std_logic;        -- zero flag
  signal flag_d : std_logic;        -- decimal mode flag
  signal flag_n : std_logic;        -- negative flag
  signal flag_v : std_logic;        -- positive flag
  signal flag_i : std_logic;        -- interrupt disable flag
  signal flag_e : std_logic;        -- 8-bit stack flag

  signal reg_a : unsigned(7 downto 0);
  signal reg_b : unsigned(7 downto 0);
  signal reg_x : unsigned(7 downto 0);
  signal reg_y : unsigned(7 downto 0);
  signal reg_z : unsigned(7 downto 0);
  signal reg_sp : unsigned(7 downto 0);
  signal reg_sph : unsigned(7 downto 0);
  signal reg_pc : unsigned(15 downto 0);

  -- Flags to detect interrupts
  signal map_interrupt_inhibit : std_logic := '0';
  signal nmi_pending : std_logic := '0';
  signal irq_pending : std_logic := '0';
  signal nmi_state : std_logic := '1';
  signal irq_state : std_logic := '1';
  -- Interrupt/reset vector being used
  signal vector : unsigned(15 downto 0);
  
  type processor_state is (
    InstructionFetch,
    WaitOneCycle,ResetLow,
    SlowRamRead1,SlowRamRead2,SlowRamWrite1,SlowRamWrite2,
    MonitorAccessDone,MonitorReadDone,
    FastIOWait
    );
  signal state : processor_state := ResetLow;  -- start processor in reset state
  -- For memory access we push the processor state to follow once the memory
  -- access is complete.
  signal pending_state : processor_state;
  -- Information about instruction currently being executed
  signal opcode : unsigned(7 downto 0);
  signal arg1 : unsigned(7 downto 0);

  type addressingmode is (
    M_impl,M_InnX,M_nn,M_immnn,M_A,M_nnnn,M_nnrr,
    M_rr,M_InnY,M_InnZ,M_rrrr,M_nnX,M_nnnnY,M_nnnnX,M_Innnn,
    M_InnnnX,M_InnSPY,M_nnY,M_immnnnn);

  type mode_list is array(addressingmode'low to addressingmode'high) of integer;
  constant mode_bytes_lut : mode_list := (
    M_impl => 0,
    M_InnX => 1,
    M_nn => 1,
    M_immnn => 1,
    M_A => 0,
    M_nnnn => 2,
    M_nnrr => 2,
    M_rr => 1,
    M_InnY => 1,
    M_InnZ => 1,
    M_rrrr => 2,
    M_nnX => 1,
    M_nnnnY => 2,
    M_nnnnX => 2,
    M_Innnn => 2,
    M_InnnnX => 2,
    M_InnSPY => 1,
    M_nnY => 1,
    M_immnnnn => 2);
  
  type instruction is (
    -- 4510 opcodes
    I_BRK,I_ORA,I_CLE,I_SEE,I_TSB,I_ASL,I_RMB,
    I_PHP,I_TSY,I_BBR,I_BPL,I_TRB,I_CLC,I_INC,I_INZ,
    I_JSR,I_AND,I_BIT,I_ROL,I_PLP,I_TYS,I_BMI,I_SEC,
    I_DEC,I_DEZ,I_RTI,I_EOR,I_NEG,I_ASR,I_LSR,I_PHA,
    I_TAZ,I_JMP,I_BVC,I_CLI,I_PHY,I_TAB,I_MAP,I_RTS,
    I_ADC,I_BSR,I_STZ,I_ROR,I_PLA,I_TZA,I_BVS,I_SEI,
    I_PLY,I_TBA,I_BRA,I_STA,I_STY,I_STX,I_SMB,I_DEY,
    I_TXA,I_BBS,I_BCC,I_TYA,I_TXS,I_LDY,I_LDA,I_LDX,
    I_LDZ,I_TAY,I_TAX,I_BCS,I_CLV,I_TSX,I_CPY,I_CMP,
    I_CPZ,I_DEW,I_INY,I_DEX,I_ASW,I_BNE,I_CLD,I_PHX,
    I_PHZ,I_CPX,I_SBC,I_INW,I_INX,I_EOM,I_ROW,I_BEQ,
    I_PHW,I_SED,I_PLX,I_PLZ);

  type ilut8bit is array(0 to 255) of instruction;
  constant instruction_lut : ilut8bit := (
    I_BRK,I_ORA,I_CLE,I_SEE,I_TSB,I_ORA,I_ASL,I_RMB,I_PHP,I_ORA,I_ASL,I_TSY,I_TSB,I_ORA,I_ASL,I_BBR,
    I_BPL,I_ORA,I_ORA,I_BPL,I_TRB,I_ORA,I_ASL,I_RMB,I_CLC,I_ORA,I_INC,I_INZ,I_TRB,I_ORA,I_ASL,I_BBR,
    I_JSR,I_AND,I_JSR,I_JSR,I_BIT,I_AND,I_ROL,I_RMB,I_PLP,I_AND,I_ROL,I_TYS,I_BIT,I_AND,I_ROL,I_BBR,
    I_BMI,I_AND,I_AND,I_BMI,I_BIT,I_AND,I_ROL,I_RMB,I_SEC,I_AND,I_DEC,I_DEZ,I_BIT,I_AND,I_ROL,I_BBR,
    I_RTI,I_EOR,I_NEG,I_ASR,I_ASR,I_EOR,I_LSR,I_RMB,I_PHA,I_EOR,I_LSR,I_TAZ,I_JMP,I_EOR,I_LSR,I_BBR,
    I_BVC,I_EOR,I_EOR,I_BVC,I_ASR,I_EOR,I_LSR,I_RMB,I_CLI,I_EOR,I_PHY,I_TAB,I_MAP,I_EOR,I_LSR,I_BBR,
    I_RTS,I_ADC,I_RTS,I_BSR,I_STZ,I_ADC,I_ROR,I_RMB,I_PLA,I_ADC,I_ROR,I_TZA,I_JMP,I_ADC,I_ROR,I_BBR,
    I_BVS,I_ADC,I_ADC,I_BVS,I_STZ,I_ADC,I_ROR,I_RMB,I_SEI,I_ADC,I_PLY,I_TBA,I_JMP,I_ADC,I_ROR,I_BBR,
    I_BRA,I_STA,I_STA,I_BRA,I_STY,I_STA,I_STX,I_SMB,I_DEY,I_BIT,I_TXA,I_STY,I_STY,I_STA,I_STX,I_BBS,
    I_BCC,I_STA,I_STA,I_BCC,I_STY,I_STA,I_STX,I_SMB,I_TYA,I_STA,I_TXS,I_STX,I_STZ,I_STA,I_STZ,I_BBS,
    I_LDY,I_LDA,I_LDX,I_LDZ,I_LDY,I_LDA,I_LDX,I_SMB,I_TAY,I_LDA,I_TAX,I_LDZ,I_LDY,I_LDA,I_LDX,I_BBS,
    I_BCS,I_LDA,I_LDA,I_BCS,I_LDY,I_LDA,I_LDX,I_SMB,I_CLV,I_LDA,I_TSX,I_LDZ,I_LDY,I_LDA,I_LDX,I_BBS,
    I_CPY,I_CMP,I_CPZ,I_DEW,I_CPY,I_CMP,I_DEC,I_SMB,I_INY,I_CMP,I_DEX,I_ASW,I_CPY,I_CMP,I_DEC,I_BBS,
    I_BNE,I_CMP,I_CMP,I_BNE,I_CPZ,I_CMP,I_DEC,I_SMB,I_CLD,I_CMP,I_PHX,I_PHZ,I_CPZ,I_CMP,I_DEC,I_BBS,
    I_CPX,I_SBC,I_LDA,I_INW,I_CPX,I_SBC,I_INC,I_SMB,I_INX,I_SBC,I_EOM,I_ROW,I_CPX,I_SBC,I_INC,I_BBS,
    I_BEQ,I_SBC,I_SBC,I_BEQ,I_PHW,I_SBC,I_INC,I_SMB,I_SED,I_SBC,I_PLX,I_PLZ,I_PHW,I_SBC,I_INC,I_BBS);

  
  type mlut8bit is array(0 to 255) of addressingmode;
  constant mode_lut : mlut8bit := (
    M_impl,  M_InnX,  M_impl,  M_impl,  M_nn,    M_nn,    M_nn,    M_nn,    
    M_impl,  M_immnn, M_A,     M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
    M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nn,    M_nnX,   M_nnX,   M_nn,    
    M_impl,  M_nnnnY, M_impl,  M_impl,  M_nnnn,  M_nnnnX, M_nnnnX, M_nnrr,  
    M_nnnn,  M_InnX,  M_Innnn, M_InnnnX,M_nn,    M_nn,    M_nn,    M_nn,    
    M_impl,  M_immnn, M_A,     M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
    M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nnX,   M_nnX,   M_nnX,   M_nn,    
    M_impl,  M_nnnnY, M_impl,  M_impl,  M_nnnnX, M_nnnnX, M_nnnnX, M_nnrr,  
    M_impl,  M_InnX,  M_impl,  M_impl,  M_nn,    M_nn,    M_nn,    M_nn,    
    M_impl,  M_immnn, M_A,     M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
    M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nnX,   M_nnX,   M_nnX,   M_nn,    
    M_impl,  M_nnnnY, M_impl,  M_impl,  M_impl,  M_nnnnX, M_nnnnX, M_nnrr,  
    M_impl,  M_InnX,  M_immnn, M_rrrr,  M_nn,    M_nn,    M_nn,    M_nn,    
    M_impl,  M_immnn, M_A,     M_impl,  M_Innnn, M_nnnn,  M_nnnn,  M_nnrr,  
    M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nnX,   M_nnX,   M_nnX,   M_nn,    
    M_impl,  M_nnnnY, M_impl,  M_impl,  M_InnnnX,M_nnnnX, M_nnnnX, M_nnrr,  
    M_rr,    M_InnX,  M_InnSPY,M_rrrr,  M_nn,    M_nn,    M_nn,    M_nn,    
    M_impl,  M_immnn, M_impl,  M_nnnnX, M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
    M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nnX,   M_nnX,   M_nnY,   M_nn,    
    M_impl,  M_nnnnY, M_impl,  M_nnnnY, M_nnnn,  M_nnnnX, M_nnnnX, M_nnrr,  
    M_immnn, M_InnX,  M_immnn, M_immnn, M_nn,    M_nn,    M_nn,    M_nn,    
    M_impl,  M_immnn, M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
    M_rr,    M_InnY,  M_InnY,  M_rrrr,  M_nnX,   M_nnX,   M_nnY,   M_nn,    
    M_impl,  M_nnnnY, M_impl,  M_nnnnX, M_nnnnX, M_nnnnX, M_nnnnY, M_nnrr,  
    M_immnn, M_InnX,  M_immnn, M_nnnn,  M_nn,    M_nn,    M_nn,    M_nn,    
    M_impl,  M_immnn, M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
    M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_nn,    M_nnX,   M_nnX,   M_nn,    
    M_impl,  M_nnnnY, M_impl,  M_impl,  M_nnnn,  M_nnnnX, M_nnnnX, M_nnrr,  
    M_immnn, M_InnX,  M_InnSPY,M_nnnn,  M_nn,    M_nn,    M_nn,    M_nn,    
    M_impl,  M_immnn, M_impl,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnnn,  M_nnrr,  
    M_rr,    M_InnY,  M_InnZ,  M_rrrr,  M_immnnnn,M_nnX,   M_nnX,   M_nn,    
    M_impl,  M_nnnnY, M_impl,  M_impl,  M_nnnn,  M_nnnnX, M_nnnnX, M_nnrr);
  
  -- PC used for JSR is the value of reg_pc after reading only one of
  -- of the argument bytes.  We could subtract one, but it is less logic to
  -- just remember PC after reading one argument byte.
  signal reg_pc_jsr : unsigned(15 downto 0);
  -- Temporary address register (used for indirect modes)
  signal reg_addr : unsigned(15 downto 0);
  -- Temporary instruction register (used for many modes)
  signal reg_instruction : instruction;
  -- Temporary value holder (used for RMW instructions)
  signal reg_value : unsigned(7 downto 0);
  
-- Indicate source of operand for instructions
-- Note that ROM is actually implemented using
-- power-on initialised RAM in the FPGA mapped via our io interface.
  signal accessing_fastio : std_logic;
  signal accessing_ram : std_logic;
  signal accessing_slowram : std_logic;
  signal accessing_cpuport : std_logic;
  signal cpuport_num : std_logic;
  signal cpuport_ddr : unsigned(7 downto 0) := x"FF";
  signal cpuport_value : unsigned(7 downto 0) := x"3F";

  signal monitor_mem_trace_toggle_last : std_logic := '0';

begin
  process(clock)

    procedure reset_cpu_state is
  begin
    -- Default register values
    reg_b <= x"01";
    reg_a <= x"11";    
    reg_x <= x"22";
    reg_y <= x"33";
    reg_z <= x"00";
    reg_sp <= x"ff";
    reg_sph <= x"01";

    -- Default CPU flags
    flag_c <= '0';
    flag_d <= '0';
    flag_i <= '1';                -- start with IRQ disabled
    flag_z <= '0';
    flag_n <= '0';
    flag_v <= '0';
    flag_e <= '1';

    cpuport_ddr <= x"FF";
    cpuport_value <= x"3F";
    
    -- Default memory map (C64 like, but with enhanced IO and kernel)
    -- Map first bank of fast RAM at $0000 - $CFFF
    for i in 0 to 12 loop
      ram_bank_registers_read(i)<=to_unsigned(i,16);
      ram_bank_registers_write(i)<=to_unsigned(i,16);
      ram_bank_registers_instructions(i)<=to_unsigned(i,16);
    end loop;  -- i
    -- enhanced IO at $D000-$DFFF
    ram_bank_registers_read(13) <= x"FFD3";  
    ram_bank_registers_write(13) <= x"FFD3";
    ram_bank_registers_instructions(13) <= x"FFD3";
    -- Kernel64 ROM at $E000-$FFFF (writes redirect to "underlying" fast RAM)
    ram_bank_registers_read(14) <= x"FFEE";
    ram_bank_registers_write(14) <= x"000E";
    ram_bank_registers_instructions(14) <= x"FFEE";
    ram_bank_registers_read(15) <= x"FFEF";
    ram_bank_registers_write(15) <= x"000F";
    ram_bank_registers_instructions(15) <= x"FFEF";       
    -- BASIC64 ROM at $A000-$BFFF (writes redirect to "underlying" fast RAM)
    ram_bank_registers_read(10) <= x"FFEA";
    ram_bank_registers_write(10) <= x"000A";
    ram_bank_registers_instructions(10) <= x"FFEA";
    ram_bank_registers_read(11) <= x"FFEB";
    ram_bank_registers_write(11) <= x"000B";
    ram_bank_registers_instructions(11) <= x"FFEB";

    -- For debugging, HESMON at $C000-$CFFF
    ram_bank_registers_read(12) <= x"FFEC";
    ram_bank_registers_write(12) <= x"FFEC";
    ram_bank_registers_instructions(12) <= x"FFEC";
    
    -- For simulation: our own rom at $E000 - $FFFF, also mapping to $0000-$0FFF
    -- since fastram doesn't work in simulation with GHDL.
    --ram_bank_registers_read(14) <= x"FFFE";
    --ram_bank_registers_write(14) <= x"000E";
    --ram_bank_registers_instructions(14) <= x"FFFE";
    --ram_bank_registers_read(15) <= x"FFFF";
    --ram_bank_registers_write(15) <= x"000F";
    --ram_bank_registers_instructions(15) <= x"FFFF";       

    --ram_bank_registers_read(0) <= x"FFFE";
    --ram_bank_registers_write(0) <= x"FFFE";
    --ram_bank_registers_instructions(0) <= x"FFFE";
    
  end procedure reset_cpu_state;

  procedure read_long_address(
    long_address : in unsigned(27 downto 0);
    next_state : in processor_state) is
  begin
    -- Schedule the memory read from the appropriate source.
    accessing_ram <= '0'; accessing_slowram <= '0';
    accessing_fastio <= '0'; accessing_cpuport <= '0';
    if long_address(27 downto 17)="00000000000" then
      report "Reading from fastram address $" & to_hstring(long_address(19 downto 0))
        & ", word $" & to_hstring(long_address(18 downto 3)) severity note;
      accessing_ram <= '1';
      fastram_address <= std_logic_vector(long_address(16 downto 3));
      fastram_byte_number <= long_address(2 downto 0);
      fastram_read <= '1';
      pending_state <= next_state;
      state <= WaitOneCycle;
      -- No wait states in fastram system, so proceed directly to next state
    elsif long_address(27 downto 24) = x"8" then
      accessing_slowram <= '1';
      slowram_addr <= std_logic_vector(long_address(23 downto 1));
      slowram_data <= (others => 'Z');  -- tristate data lines
      slowram_we <= '1';
      slowram_ce <= '1';
      slowram_oe <= '1';
      slowram_lb <= '1';
      slowram_ub <= '1';
      slowram_lohi <= long_address(0);
      pending_state <= next_state;
      state <= SlowRamRead1;
    elsif long_address(27 downto 24) = x"F" then
      accessing_fastio <= '1';
      fastio_addr <= std_logic_vector(long_address(19 downto 0));
      fastio_read <= '1';
      -- XXX Some fastio (that referencing dual-port block rams) does require
      -- a wait state.  For now, just apply the wait state to all fastio
      -- addresses.
      -- Eventually can narrow down to colour ram, palette and some of the other
      -- IO features that use dual-port rams to provide access.
      -- Probably easier just to make the single-port ROM portion of fastio fast,
      -- and assume all else is slow, as there are many pieces of fastio that need
      -- a wait state.
      -- So let's just make the top 128KB of fastio fast, and assume the rest needs
      -- the wait state.  Also the CIAs as interrupts are acknowledged and cleared
      -- by reading registers, so reading twice would lose the ability to see
      -- the interrupt source.
      if long_address(19 downto 17)="111"
        --or long_address(19 downto 8)=x"D0C" or long_address(19 downto 8)=x"D0D"
        --or long_address(19 downto 8)=x"D1C" or long_address(19 downto 8)=x"D1D"
        --or long_address(19 downto 8)=x"D2C" or long_address(19 downto 8)=x"D2D"
        --or long_address(19 downto 8)=x"D3C" or long_address(19 downto 8)=x"D3D"
        or long_address(19 downto 8)=x"D00" or long_address(19 downto 8)=x"D10"
        or long_address(19 downto 8)=x"D20" or long_address(19 downto 8)=x"D30"
      then 
        state <= next_state;
      else
        pending_state <= next_state;
        state <= FastIOWait;
      end if;
    else
      -- Don't let unmapped memory jam things up
      state <= next_state;
    end if;
    -- Once read, we then resume processing from the specified state.
    pending_state <= next_state;
  end read_long_address;
  
  procedure write_long_byte(
    long_address       : in unsigned(27 downto 0);
    value              : in unsigned(7 downto 0);
    next_state         : in processor_state) is
  begin
    -- Schedule the memory write to the appropriate destination.
    accessing_ram <= '0'; accessing_slowram <= '0';
    accessing_fastio <= '0'; accessing_cpuport <= '0';
    if long_address(27 downto 17)="00000000000" then
      accessing_ram <= '1';
      fastram_address <= std_logic_vector(long_address(16 downto 3));
      fastram_write <= '1';
      fastram_read <= '0';
      fastram_we <= (others => '0');
      fastram_datain <= (others => '1');
      case long_address(2 downto 0) is
        when "000" => fastram_we <= "00000001"; fastram_datain(7 downto 0) <= std_logic_vector(value);
        when "001" => fastram_we <= "00000010"; fastram_datain(15 downto 8) <= std_logic_vector(value);
        when "010" => fastram_we <= "00000100"; fastram_datain(23 downto 16) <= std_logic_vector(value);
        when "011" => fastram_we <= "00001000"; fastram_datain(31 downto 24) <= std_logic_vector(value);
        when "100" => fastram_we <= "00010000"; fastram_datain(39 downto 32) <= std_logic_vector(value);
        when "101" => fastram_we <= "00100000"; fastram_datain(47 downto 40) <= std_logic_vector(value);
        when "110" => fastram_we <= "01000000"; fastram_datain(55 downto 48) <= std_logic_vector(value);
        when "111" => fastram_we <= "10000000"; fastram_datain(63 downto 56) <= std_logic_vector(value);
        when others =>
          report "dud write to fastram" severity note;
      end case;
      -- report "writing to fastram..." severity note;
      state <= next_state;
    elsif long_address(27 downto 24) = x"8" then
      accessing_slowram <= '1';
      slowram_addr <= std_logic_vector(long_address(23 downto 1));
      slowram_we <= '1';
      slowram_ce <= '1';
      slowram_oe <= '1';
      slowram_lohi <= long_address(0);
      slowram_lb <= std_logic(long_address(0));
      slowram_ub <= std_logic(not long_address(0));
      slowram_data <= std_logic_vector(value) & std_logic_vector(value);
      pending_state <= next_state;
      state <= SlowRamWrite1;
    elsif long_address(27 downto 24) = x"F" then
      accessing_fastio <= '1';
      fastio_addr <= std_logic_vector(long_address(19 downto 0));
      fastio_write <= '1';
      fastio_wdata <= std_logic_vector(value);
      if long_address = x"FFC00A0" then
        slowram_waitstates <= value;
      end if;
      -- No wait states on I/O write, so proceed directly to the next state
      state <= next_state;
    else
      -- Don't let unmapped memory jam things up
      state <= next_state;
    end if;
    -- Once read, we then resume processing from the specified state.
    pending_state <= next_state;
  end write_long_byte;

  -- purpose: obtain the byte of memory that has been read
  impure function read_data
    return unsigned is
  begin  -- read_data
    if accessing_cpuport='1' then
      if cpuport_num='0' then
        -- DDR
        return cpuport_ddr;
      else
        -- CPU port
        return cpuport_value;
      end if;
    elsif accessing_fastio='1' then 
      return unsigned(fastio_rdata);
    elsif accessing_ram='1' then
      report "Extracting fastram value from 64-bit read $" & to_hstring(fastram_dataout) severity note;
      case fastram_byte_number is
        when "000" => return unsigned(fastram_dataout( 7 downto 0));
        when "001" => return unsigned(fastram_dataout(15 downto 8));
        when "010" => return unsigned(fastram_dataout(23 downto 16));
        when "011" => return unsigned(fastram_dataout(31 downto 24));
        when "100" => return unsigned(fastram_dataout(39 downto 32));
        when "101" => return unsigned(fastram_dataout(47 downto 40));
        when "110" => return unsigned(fastram_dataout(55 downto 48));
        when "111" => return unsigned(fastram_dataout(63 downto 56));
        when others => return x"FF";
      end case;
    elsif accessing_slowram='1' then           
      slowram_ce <= '1'; -- Release after reading so that refresh can occur
      slowram_data <= (others => 'Z');  -- tristate data lines as well
      case slowram_lohi is
        when '0' => return unsigned(slowram_data(7 downto 0));
        when '1' => return unsigned(slowram_data(15 downto 8));
        when others => return x"FF";
      end case;
    else
      return x"FF";
    end if;
  end read_data; 

  -- purpose: set processor flags from a byte (eg for PLP or RTI)
  procedure load_processor_flags (
    value : in unsigned(7 downto 0)) is
  begin  -- load_processor_flags
    flag_n <= value(7);
    flag_v <= value(6);
    -- C65/4502 specifications says that E is not set by PLP, only by SEE/CLE
    flag_d <= value(3);
    flag_i <= value(2);
    flag_z <= value(1);
    flag_c <= value(0);
  end procedure load_processor_flags;

  impure function with_nz (
    value : unsigned(7 downto 0)) return unsigned is
  begin
    -- report "calculating N & Z flags on result $" & to_hstring(value) severity note;
    flag_n <= value(7);
    if value(7 downto 0) = x"00" then
      flag_z <= '1';
    else
      flag_z <= '0';
    end if;
    return value;
  end with_nz;        

  function flag_status (
    yes : in string;
    no : in string;
    condition : in std_logic) return string is
  begin
    if condition='1' then
      return yes;
    else
      return no;
    end if;
  end function flag_status;

  variable virtual_reg_p : std_logic_vector(7 downto 0);
  variable temp_pc : unsigned(15 downto 0);
  begin
    if rising_edge(clock) then
      monitor_state <= std_logic_vector(to_unsigned(processor_state'pos(state),8));
      monitor_pc <= std_logic_vector(reg_pc);
      monitor_a <= std_logic_vector(reg_a);
      monitor_x <= std_logic_vector(reg_x);
      monitor_y <= std_logic_vector(reg_y);
      monitor_z <= std_logic_vector(reg_z);
      monitor_sp <= std_logic_vector(reg_sph) & std_logic_vector(reg_sp);
      
      -- Clear memory access interfaces
      -- Allow fastio to continue reading to support 1 cycle wait state
      -- for those fastio addresses that require it.
      -- XXX This can have problems for reading from registers that have
      -- special side effects.
      accessing_fastio <= '0';
      if accessing_fastio='0' then
        fastio_addr <= (others => '1');
        fastio_read <= '0';        
      end if;
      fastio_write <= '0';
      fastram_read <= '0';
      fastram_write <= '0';
      fastram_we <= (others => '0');
      fastram_address <= "11111111111111";
      fastram_datain <= x"d0d1d2d3d4d5d6d7";
      
      -- Generate virtual processor status register for convenience
      virtual_reg_p(7) := flag_n;
      virtual_reg_p(6) := flag_v;
      virtual_reg_p(5) := '1';
      virtual_reg_p(4) := '0';
      virtual_reg_p(3) := flag_d;
      virtual_reg_p(2) := flag_i;
      virtual_reg_p(1) := flag_z;
      virtual_reg_p(0) := flag_c;

      monitor_p <= std_logic_vector(virtual_reg_p);

      if reset = '0' or state = ResetLow then
        -- reset cpu
        state <= InstructionFetch;
        vector <= x"FFFC";
        reset_cpu_state;
      elsif monitor_mem_attention_request='1' and state = InstructionFetch then
        -- Memory access by serial monitor.
        if monitor_mem_write='1' then
          -- Write to specified long address
          write_long_byte(unsigned(monitor_mem_address),monitor_mem_wdata,
                          MonitorAccessDone);
        else
          -- Read from specified long address
          read_long_address(unsigned(monitor_mem_address),MonitorReadDone);
          -- and optionally set PC
          if monitor_mem_setpc='1' then
            reg_pc <= unsigned(monitor_mem_address(15 downto 0));
          end if;
        end if;   
      else
        -- CPU running, so do CPU state machine

        recent_states(1 to 15) <= recent_states(0 to 14);
        recent_states(0) <= to_unsigned(processor_state'pos(state),16);

        if monitor_mem_stage_trace_mode='0' or
          monitor_mem_trace_toggle /= monitor_mem_trace_toggle_last then
          monitor_mem_trace_toggle_last <= monitor_mem_trace_toggle;
          case state is
            when MonitorReadDone =>            
              monitor_mem_rdata <= read_data;
              state <= MonitorAccessDone;

              -- Don't overwrite recent states record
              recent_states <= recent_states;
            when MonitorAccessDone =>
              fastram_we <= (others => '0');
              monitor_mem_attention_granted <= '1';
              if monitor_mem_attention_request='0' then
                monitor_mem_attention_granted <= '0';
                state <= InstructionFetch;
              end if;
              -- Don't overwrite recent states record
              recent_states <= recent_states;
            when InstructionFetch =>

              -- Show CPU state for debugging
              -- report "state = " & processor_state'image(state) severity note;
              -- Use a format very like that of VICE so that we can compare our boot
              -- sequence to theirs, to find errors in our CPU etc.
              report ""
                & ".C:" & to_hstring(std_logic_vector(reg_pc))
                & " - A:" & to_hstring(std_logic_vector(reg_a))
                & " X:" & to_hstring(std_logic_vector(reg_x))
                & " Y:" & to_hstring(std_logic_vector(reg_y))
                & " SP:" & to_hstring(std_logic_vector(reg_sp))
                & " "
                & flag_status("N",".",flag_n)
                & flag_status("V",".",flag_v)
                & "-"
                & "."
                & flag_status("D",".",flag_d)
                & flag_status("I",".",flag_i)
                & flag_status("Z",".",flag_z)
                & flag_status("C",".",flag_c)        
                severity note;        
              
              monitor_mem_attention_granted <= '0';
              if monitor_mem_trace_mode='0' or
                monitor_mem_trace_toggle /= monitor_mem_trace_toggle_last then
                monitor_mem_trace_toggle_last <= monitor_mem_trace_toggle;
              else
                -- Hold recent states
                recent_states <= recent_states;
              end if;
            when WaitOneCycle => state <= pending_state;
            when FastIOWait => state <= pending_state; accessing_fastio <= '1';
            when SlowRamRead1 =>
              slowram_ce <= '0';
              slowram_oe <= '0';
              slowram_lb <= '0';
              slowram_ub <= '0';
              slowram_counter <= x"00";
              state <= SlowRamRead2;
            when SlowRamRead2 =>
              if slowram_counter=slowram_waitstates then
                state <= pending_state;
              else
                slowram_counter <= slowram_counter + 1;
              end if;
            when SlowRamWrite1 =>
              slowram_ce <= '0';
              slowram_oe <= '0';
              slowram_we <= '0';
              slowram_lb <= slowram_lohi;
              slowram_ub <= not slowram_lohi;
              slowram_counter <= x"00";
              state <= SlowRamWrite2;
            when SlowRamWrite2 =>
              if slowram_counter=slowram_waitstates then
                state <= pending_state;
                slowram_ce <= '1';
                slowram_oe <= '1';
                slowram_we <= '1';
                slowram_data <= (others => 'Z');  -- tristate data lines
              else
                slowram_counter <= slowram_counter + 1;
              end if;
            when others =>
              -- Don't allow CPU to stay stuck
              state<= InstructionFetch;
          end case;
        end if;
      end if;
    end if;
  end process;

  -- purpose: present MMU registers and other stuff to fastio interface
  -- type   : combinational
  -- inputs : ram_bank_registers_read
  -- outputs: fastio_*
  fastio: process (ram_bank_registers_read,fastio_addr,fastio_read,
                   irq,irq_pending,recent_states,clock)
    variable address : unsigned(19 downto 0);
    variable rwx : integer;
    variable lohi : std_logic;
    variable value : unsigned(15 downto 0);
    variable reg_num : integer;
  begin  -- process fastio
    if fastio_read='1' and address(19 downto 8) = x"FC0" then
      address := unsigned(fastio_addr);
      rwx := to_integer(address(7 downto 5));
      lohi := fastio_addr(0);
      reg_num := to_integer(address(4 downto 1));
      
      case rwx is
        when 0 => value := ram_bank_registers_read(reg_num);
        when 1 => value := ram_bank_registers_read(reg_num);
        when 2 => value := ram_bank_registers_read(reg_num);
        when 5 => value := x"FF" & slowram_waitstates;
        when 6 => value := x"EEE" & irq & irq_pending & nmi & nmi_pending;
        when 7 => value := recent_states(reg_num);
        when others => value := x"F00D";
      end case;
      if lohi='0' then
        fastio_rdata <= std_logic_vector(value(7 downto 0));
      else
        fastio_rdata <= std_logic_vector(value(15 downto 8));
      end if;
    else
      fastio_rdata <= (others => 'Z');
    end if;
  end process fastio;
end Behavioural;
