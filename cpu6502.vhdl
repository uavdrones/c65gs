use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity cpu6502 is
 port (
   Clock : in std_logic;
   reset : in std_logic;
   irq : in std_logic;
   nmi : in std_logic;
	monitor_pc : out std_logic_vector(15 downto 0));
  
end cpu6502;

architecture Behavioral of cpu6502 is

-- 512KB RAM as 64K x 64bit
-- The wide databus allows us to read entire instructions in one go,
-- and potentially to write operands to memory while fetching the next
-- instruction if the lower 3 bits don't conflict
type unsigned_array_8 is array(0 to 7) of std_logic_vector(15 downto 0);
type stdlogic_array_8 is array (natural range <>) of std_logic_vector(7 downto 0);
signal ram_address : unsigned_array_8;
signal ram_we : std_logic_vector(0 to 7);
signal ram_data_i : stdlogic_array_8(0 to 7);
signal ram_data_o : stdlogic_array_8(0 to 7);

-- CPU RAM bank selection registers.
-- Each 4KB (12 address bits) can be made to point to a section of memory.
-- Sections have 16bit addresses, for a total of 28bits (256MB) of address
-- space.  It also makes it possible to multiple 4KB banks point to the same
-- block of RAM.
type bank_register_set is array (0 to 15) of std_logic_vector(15 downto 0);
signal ram_bank_registers : bank_register_set;

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

-- Keep incremented versions of PC around for fast hopping over instructions
-- (note these are not used for instruction fetching, as that code operates
--  differently).
signal reg_pcplus1 : unsigned(15 downto 0);
signal reg_pcplus2 : unsigned(15 downto 0);

-- When pulling a value from the stack, remember which of the 8 RAM banks it will
-- come from.
signal pull_bank : unsigned(2 downto 0);

-- Temporary address used in various states
signal temp_opcode : std_logic_vector(7 downto 0);
signal temp_value : std_logic_vector(7 downto 0);
-- Other temporary variables
signal op_mem_slot : unsigned(2 downto 0);
signal operand1_mem_slot : unsigned(2 downto 0);
signal operand2_mem_slot : unsigned(2 downto 0);
signal vector : std_logic_vector(15 downto 0);

-- Indicate source of operand for instructions
-- Note that ROM is actually implemented using
-- power-on initialised RAM in the FPGA.  This
-- allows fast parallel fetching of instructions
-- and their arguments.
-- For FPGAs that don't support pre-initialisation
-- of RAM, we can easily have a reset routine that
-- copies a real ROM into RAM.
signal operand_from_io : std_logic;
signal operand_from_ram : std_logic;
signal operand_from_slowram : std_logic;

type processor_state is (
  -- When CPU first powers up, or reset is bought low
  ResetLow,
  -- States for handling interrupts and reset
  VectorPushPC,VectorPushP,VectorRead,VectorLoadPC,
  -- Normal instruction states.  Many states will be skipped
  -- by any given instruction.
  -- When an instruction completes, we move back to InstructionFetch
  InstructionFetch,OperandResolve,MemoryRead,
  Calculate,MemoryWrite,
  -- Special states used for special instructions
  PushP,                                -- PHP
  PushA,                                -- PHA
  PullA,                                -- PLA
  PullP,                                -- PLP
  BRKPushPCH,BRKPushPCL,PRKPushP,       -- BRK
  RTIPull,                              -- RTI
  RTSPull,                              -- RTS
  JMPIndirectFetch,                     -- JMP absolute indirect
  Halt                                  -- KIL
  );
signal state : processor_state := ResetLow;  -- start processor in reset state

type instruction is (
I_ADC,I_AHX,I_ALR,I_ANC,I_AND,I_ARR,I_ASL,I_AXS,
I_BCC,I_BCS,I_BEQ,I_BIT,I_BMI,I_BNE,I_BPL,I_BRK,
I_BVC,I_BVS,I_CLC,I_CLD,I_CLI,I_CLV,I_CMP,I_CPX,
I_CPY,I_DCP,I_DEC,I_DEX,I_DEY,I_EOR,I_INC,I_INX,
I_INY,I_ISC,I_JMP,I_JSR,I_KIL,I_LAS,I_LAX,I_LDA,
I_LDX,I_LDY,I_LSR,I_NOP,I_ORA,I_PHA,I_PHP,I_PLA,
I_PLP,I_RLA,I_ROL,I_ROR,I_RRA,I_RTI,I_RTS,I_SAX,
I_SBC,I_SEC,I_SED,I_SEI,I_SHX,I_SHY,I_SLO,I_SRE,
I_STA,I_STX,I_STY,I_TAS,I_TAX,I_TAY,I_TSX,I_TXA,
I_TXS,I_TYA,I_XAA);
signal op_instruction : instruction;

type ilut8bit is array(0 to 255) of instruction;
constant instruction_lut : ilut8bit := (
I_BRK,  I_ORA,  I_KIL,  I_SLO,  I_NOP,  I_ORA,  I_ASL,  I_SLO,  I_PHP,  I_ORA,  I_ASL,  I_ANC,  I_NOP,  I_ORA,  I_ASL,  I_SLO, 
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
M_implied,M_immidiate,M_accumulator,
M_zeropage,M_zeropageX,M_zeropageY,
M_absolute,M_absoluteY,M_absoluteX,
M_relative,M_indirect,M_indirectX,M_indirectY);
signal op_mode : addressingmode;

type mlut8bit is array(0 to 255) of addressingmode;
constant mode_lut : mlut8bit := (
  -- 00
  M_implied,  M_indirectX,  M_immidiate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage,
  M_implied,  M_immidiate,  M_accumulator,  M_immidiate,  M_absolute,  M_absolute,  M_absolute,  M_absolute, 
  -- 10
  M_relative,  M_indirectY,  M_immidiate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
  M_implied,  M_absoluteY,  M_accumulator,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX,
  -- 20
  M_absolute,  M_indirectX,  M_immidiate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
  M_implied,  M_immidiate,  M_accumulator,  M_immidiate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
  -- 30
  M_relative,  M_indirectY,  M_immidiate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
  M_implied,  M_absoluteY,  M_accumulator,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX, 
  -- 40
  M_implied,  M_indirectX,  M_immidiate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
  M_implied,  M_immidiate,  M_accumulator,  M_immidiate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
  -- 50
  M_relative,  M_indirectY,  M_immidiate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
  M_implied,  M_absoluteY,  M_accumulator,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX, 
  -- 60
  M_implied,  M_indirectX,  M_immidiate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
  M_implied,  M_immidiate,  M_accumulator,  M_immidiate,  M_indirect,  M_absolute,  M_absolute,  M_absolute,
  -- 70
  M_relative,  M_indirectY,  M_immidiate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
  M_implied,  M_absoluteY,  M_accumulator,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX,
  -- 80
  M_immidiate,  M_indirectX,  M_immidiate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
  M_implied,  M_immidiate,  M_implied,  M_immidiate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
  -- 90
  M_relative,  M_indirectY,  M_immidiate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageY,  M_zeropageY, 
  M_implied,  M_absoluteY,  M_implied,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_implied,  M_absoluteY,
  -- A0
  M_immidiate,  M_indirectX,  M_immidiate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
  M_implied,  M_immidiate,  M_implied,  M_immidiate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
  -- B0
  M_relative,  M_indirectY,  M_immidiate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageY,  M_zeropageY, 
  M_implied,  M_absoluteY,  M_implied,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteY,  M_absoluteY,
  -- C0
  M_immidiate,  M_indirectX,  M_immidiate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
  M_implied,  M_immidiate,  M_implied,  M_immidiate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
  -- D0
  M_relative,  M_indirectY,  M_immidiate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
  M_implied,  M_absoluteY,  M_implied,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX,
  -- E0
  M_immidiate,  M_indirectX,  M_immidiate,  M_indirectX,  M_zeropage,  M_zeropage,  M_zeropage,  M_zeropage, 
  M_implied,  M_immidiate,  M_implied,  M_immidiate,  M_absolute,  M_absolute,  M_absolute,  M_absolute,
  -- F0
  M_relative,  M_indirectY,  M_immidiate,  M_indirectY,  M_zeropageX,  M_zeropageX,  M_zeropageX,  M_zeropageX, 
  M_implied,  M_absoluteY,  M_implied,  M_absoluteY,  M_absoluteX,  M_absoluteX,  M_absoluteX,  M_absoluteX);


begin
  -- Each block portram is 64KBx8bits, so we need 8 of them
  -- to make 512KB, approximately the total available on this FPGA.
  gen_ram: for i in 0 to 7 generate
    ramx: entity spartan6blockram
    port map (
      Clk   => clock,
      address => ram_address(i),
      we      => ram_we(i),
      data_i  => ram_data_i(i),
      data_o  => ram_data_o(i));
  end generate;
  
  process(clock)
      variable normal_instruction : boolean;

      variable temp_address : std_logic_vector(15 downto 0);
      variable temp_bank_block : std_logic_vector(15 downto 0);
      variable temp_operand : std_logic_vector(7 downto 0);
      variable temp_operand_address : std_logic_vector(15 downto 0);

      -- Memory read and write routines assume that they are contention free.
      -- This way, multiple refernces to write_to_long_address() can be made in a single
      -- cycle, provided that they map to different RAM units.

      -- purpose: Convert a 16-bit C64 address to native RAM (or I/O or ROM) address
      function resolve_address_to_long(short_address : std_logic_vector(15 downto 0); ram_bank_registers : bank_register_set)
        return std_logic_vector is 
       variable temp_address : std_logic_vector(27 downto 0);
       variable temp_bank_block : std_logic_vector(15 downto 0);
      begin  -- resolve_long_address
        temp_bank_block :=ram_bank_registers(to_integer(unsigned(short_address(15 downto 12))));
        temp_address(27 downto 12):= temp_bank_block;
        temp_address(11 downto 0):=std_logic_vector(short_address(11 downto 0));

        return temp_address;
      end resolve_address_to_long;
      
      procedure request_read_long_address(long_address : std_logic_vector(27 downto 0)) is
        variable ram_bank : std_logic_vector(2 downto 0);
        variable bank_address : std_logic_vector(15 downto 0);
      begin
          ram_bank := long_address(2 downto 0);
          bank_address := long_address(18 downto 3);
          ram_address(to_integer(unsigned(ram_bank))) <= bank_address;
          ram_we(to_integer(unsigned(ram_bank))) <= '0';
      end request_read_long_address;
      procedure write_to_long_address(long_address : std_logic_vector(27 downto 0);
                                  value : in std_logic_vector(7 downto 0)) is
        variable ram_bank : std_logic_vector(2 downto 0);
        variable bank_address : std_logic_vector(15 downto 0);
      begin
        if long_address(27 downto 19)="000000000" then
          -- we have RAM to read from
          ram_bank := long_address(2 downto 0);
          bank_address := long_address(18 downto 3);
          ram_address(to_integer(unsigned(ram_bank))) <= bank_address;
          ram_we(to_integer(unsigned(ram_bank))) <= '1';
        end if;
      end procedure write_to_long_address;

      procedure write_to_short_address(short_address : std_logic_vector(15 downto 0);
                                       value : in std_logic_vector(7 downto 0);
                                       ram_bank_registers : bank_register_set) is
        variable long_address : std_logic_vector(27 downto 0);
      begin
        long_address := resolve_address_to_long(short_address,ram_bank_registers);
        write_to_long_address(long_address,value);
      end procedure;
      
      procedure push_byte(value : in std_logic_vector(7 downto 0)) is
        variable push_long_address : std_logic_vector(27 downto 0);
      begin
        -- Stack is page 1, which is in the first 4KB bank of RAM
        push_long_address(27 downto 12) := ram_bank_registers(0);
        push_long_address(11 downto 8) := "0001";
        -- Now append stack pointer
        push_long_address(7 downto 0) := std_logic_vector(reg_sp);
        write_to_long_address(push_long_address,value);
        -- decrement stack pointer
        reg_sp <= reg_sp - 1;
      end procedure push_byte;        
      
      procedure pull_byte is
        variable long_address : std_logic_vector(27 downto 0);
      begin
        -- pre-increment SP before using
        -- Stack is page 1, which is in the first 4KB bank of RAM
        long_address(27 downto 12) := ram_bank_registers(0);
        long_address(11 downto 8) := "0001";
        -- Now append stack pointer
        long_address(7 downto 0) := std_logic_vector(reg_sp+1);

        request_read_long_address(long_address);
        -- increment SP
        reg_sp <= reg_sp + 1;
        pull_bank <= unsigned(long_address(2 downto 0));
      end procedure pull_byte;
      
      procedure set_cpu_flags_inc (value : in unsigned(7 downto 0)) is
      begin
        if value=x"00" then
          flag_z <='1';
        else
          flag_z <='0';
        end if;
        flag_n <= value(7);
      end procedure set_cpu_flags_inc;

      procedure advance_pc(value : integer) is
      begin
        reg_pc <= reg_pc + value;
        reg_pcplus1 <= reg_pc + value + 1;
        reg_pcplus2 <= reg_pc + value + 2;
      end procedure advance_pc;

      procedure set_pc(addr : unsigned(15 downto 0)) is
      begin
        reg_pc <= addr;
        reg_pcplus1 <= addr + 1;
        reg_pcplus2 <= addr + 2;
      end procedure set_pc;

      procedure read_indirect_operand (
        address           : std_logic_vector(7 downto 0);
        ram_bank_registers : bank_register_set) is
        variable long_addr : std_logic_vector(27 downto 0);
        variable addr_lo : std_logic_vector(15 downto 0);
        variable addr_hi : std_logic_vector(15 downto 0);
      begin
        -- Read indirect zero page operand and leave state so that
        -- instruction executation can continue as though the operand
        -- were direct.  The caller will have already change op_mode
        -- to the appropriate absolute or absolute indexed mode, and
        -- adjusted the address passed in here for any pre-indexing.

        addr_lo(15 downto 8) := x"00";
        addr_lo(7 downto 0) := address;
        long_addr := resolve_address_to_long(addr_lo,ram_bank_registers);
        operand1_mem_slot <= unsigned(long_addr(2 downto 0));
        request_read_long_address(long_addr);
        
        addr_hi(15 downto 8) := x"00";
        addr_hi(7 downto 0) := std_logic_vector(unsigned(address) + 1);
        long_addr := resolve_address_to_long(addr_hi,ram_bank_registers);
        operand2_mem_slot <= unsigned(long_addr(2 downto 0));
        request_read_long_address(long_addr);        

      end procedure read_indirect_operand;
      
      procedure do_direct_operand (
        address           : std_logic_vector(15 downto 0);
        ram_bank_registers : bank_register_set;
        instruction : instruction) is
        variable long_addr : std_logic_vector(27 downto 0);
        variable writeP : boolean;
        variable write_value : std_logic_vector(7 downto 0);
      begin
        -- Fetch the byte at the specified address, and remember the slot it
        -- will be fetched into.
        -- Or, for write instructions, write to the address.

        -- In any case, first work out the actual address in question.
        long_addr := resolve_address_to_long(address,ram_bank_registers);

        case instruction is
          when I_STA => writeP:=true; write_value:=std_logic_vector(reg_a);
          when I_STX => writeP:=true; write_value:=std_logic_vector(reg_x);
          when I_STY => writeP:=true; write_value:=std_logic_vector(reg_y);                   
          when others => writeP:=false;
        end case;
        
        if long_addr(27 downto 12) = x"FFFD" then
          -- Read from I/O block
          operand_from_io <= '1';
          operand_from_ram <= '0';
          operand_from_slowram <= '0';        
        elsif long_addr(27 downto 12) > x"8000"
              and long_addr(27 downto 12) < x"9000" then
          -- Read from slow 16MB cellular RAM
          operand_from_io <= '0';
          operand_from_ram <= '0';
          operand_from_slowram <= '1';
        elsif long_addr(27 downto 12) < x"0080" then
          -- Read from fast RAM
          -- (Note that reading
          operand_from_io <= '0';
          operand_from_ram <= '1';
          operand_from_slowram <= '0';
          operand1_mem_slot <= unsigned(address(2 downto 0));
          if writeP then
            write_to_long_address(long_addr,write_value);
            -- XXX If the memory slot doesn't conflict with the
            -- slots for instruction fetch, then we can fetch the
            -- next instruction, saving a cycle.
            -- but care is required because PC isn't updated until
            -- the end of this cycle, which makes life interesting.
            state <= InstructionFetch;
          else
            request_read_long_address(long_addr);
          end if;
        else
          -- Reading from unmapped address space
          -- XXX Trigger a page-fault?
          operand_from_io <= '0';
          operand_from_ram <= '0';
          operand_from_slowram <= '0';
        end if;
        state <= MemoryRead;
      end procedure do_direct_operand;
      
      procedure fetch_next_instruction(pc : unsigned(15 downto 0)) is 
       variable long_pc : std_logic_vector(27 downto 0);
       variable long_pc1 : std_logic_vector(27 downto 0);
       variable long_pc2 : std_logic_vector(27 downto 0);
       variable temp_pc : std_logic_vector(15 downto 0);
      begin
        long_pc := resolve_address_to_long(std_logic_vector(pc),ram_bank_registers);
        case pc(2 downto 0) is
          when "000" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "001";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "010";
          when "001" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "010";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "011";
          when "010" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "011";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "100";
          when "011" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "100";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "101";
          when "100" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "101";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "110";
          when "101" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "110";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "111";
          when "110" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "111";
            temp_pc:=std_logic_vector(unsigned(pc)+2);
            long_pc2 := resolve_address_to_long(temp_pc,ram_bank_registers);
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "000";
          when "111" =>
            temp_pc:=std_logic_vector(unsigned(pc)+1);
            long_pc1 := resolve_address_to_long(temp_pc,ram_bank_registers);
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "000";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "001";
          -- If missing, generates an error, if present, generates a warning :/
          when others => null;
        end case;
        op_mem_slot <= unsigned(long_pc(2 downto 0));
        operand1_mem_slot <= unsigned(long_pc1(2 downto 0));
        operand2_mem_slot <= unsigned(long_pc2(2 downto 0));
        request_read_long_address(long_pc);
        request_read_long_address(long_pc1);
        request_read_long_address(long_pc2);
      end procedure fetch_next_instruction;

      procedure fetch_stack_bytes is 
       variable long_pc : std_logic_vector(27 downto 0);
       variable long_pc1 : std_logic_vector(27 downto 0);
       variable long_pc2 : std_logic_vector(27 downto 0);
       variable temp_sp : std_logic_vector(15 downto 0);
       variable stack_pointer : std_logic_vector(15 downto 0);
      begin
        -- Work out stack pointer address, bug compatible with 6502
        -- that always keeps stack within memory page $01
        stack_pointer(15 downto 8) := "00000001";
        stack_pointer(7 downto 0) := std_logic_vector(reg_sp +1);
        long_pc := resolve_address_to_long(std_logic_vector(stack_pointer),ram_bank_registers);
        case stack_pointer(2 downto 0) is
          when "000" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "001";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "010";
          when "001" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "010";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "011";
          when "010" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "011";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "100";
          when "011" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "100";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "101";
          when "100" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "101";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "110";
          when "101" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "110";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "111";
          when "110" =>
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "111";
            temp_sp(15 downto 8) := stack_pointer(15 downto 8);
            temp_sp(7 downto 0):=std_logic_vector(unsigned(stack_pointer(7 downto 0))+2);
            long_pc2 := resolve_address_to_long(temp_sp,ram_bank_registers);
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "000";
          when "111" =>
            temp_sp(15 downto 8) := stack_pointer(15 downto 8);
            temp_sp(7 downto 0):=std_logic_vector(unsigned(stack_pointer(7 downto 0))+1);
            long_pc1 := resolve_address_to_long(temp_sp,ram_bank_registers);
            long_pc1(27 downto 2) := long_pc(27 downto 2);
            long_pc1(2 downto 0) := "000";
            long_pc2(27 downto 2) := long_pc(27 downto 2);
            long_pc2(2 downto 0) := "001";
          -- If missing, generates an error, if present, generates a warning :/
          when others => null;
        end case;
        op_mem_slot <= unsigned(long_pc(2 downto 0));
        operand1_mem_slot <= unsigned(long_pc1(2 downto 0));
        operand2_mem_slot <= unsigned(long_pc2(2 downto 0));
        request_read_long_address(long_pc);
        request_read_long_address(long_pc1);
        request_read_long_address(long_pc2);
      end procedure fetch_stack_bytes;

      variable temp_sp_addr : std_logic_vector(15 downto 0);
    begin
      if rising_edge(clock) then
		  monitor_pc <= std_logic_vector(reg_pc);
        report "tick" severity note;
        if reset = '0' or state = ResetLow then
          state <= VectorRead;
          vector <= x"FFFC";
          -- reset cpu
          reg_a <= x"00";
          reg_x <= x"00";
          reg_y <= x"00";
          reg_sp <= x"ff";
          flag_c <= '0';
          flag_d <= '0';
          flag_i <= '1';                -- start with IRQ disabled
          flag_z <= '0';
          flag_n <= '0';
          flag_v <= '0';
          -- Read nothingness from RAM
          for i  in 0 to 7 loop
            ram_address(i)<="0000000000000000";
            ram_we(i) <= '0';
          end loop;  -- i
          -- Reset memory bank to first 64KB
          -- bank at 0x0000-0x0FFF points to 0x0000000-0x0000FFF = block 0x0000
          -- bank at 0x1000-0x1FFF points to 0x0001000-0x0001FFF = block 0x0001
          -- and so on.          
          for i in 0 to 15 loop
            ram_bank_registers(i)<=std_logic_vector(to_unsigned(i,16));
          end loop;  -- i
        else
          -- act based on state
          case state is
            when VectorRead =>
              report "state=VectorRead" severity note;
              -- Read PC from vector,vector+1
              -- Reading memory is a bit interesting because we have
              -- to work out which of the 32 memories to read from
              -- However, since we only support the CPU vectors from FFFA-FFFF
              -- we can do this fairly easily by setting all six relevant
              -- memories to read their last cell, and then pick the right one
              -- out in processor_state'VectorLoadPC
              -- The vectors always live in the natural memory locations
              -- 0x000FFFA - 0x000FFF, and ignore bank switching
              for i in 0 to 7 loop
                ram_we(i) <= '0';
              end loop;  -- i
              -- Memory banks are 8x64KB.
              -- So we want addresses shifted down log2(8)=3 bits
              -- which for all of them corresponds to addres 0x1FFF
              for i in 2 to 7 loop
                ram_address(i)<=x"1FFF";
              end loop;  -- i              
              state<=VectorLoadPC;
            when VectorLoadPC =>
                case vector is
                  when x"FFFA" =>
                    temp_address(7 downto 0) :=  ram_data_o(2);
                    temp_address(15 downto 8) := ram_data_o(3);
                  when x"FFFC" =>
                    temp_address(7 downto 0) :=  ram_data_o(4);
                    temp_address(15 downto 8) := ram_data_o(5);
                  when x"FFFE" =>
                    temp_address(7 downto 0) :=  ram_data_o(6);
                    temp_address(15 downto 8) := ram_data_o(7);
                  when others =>
                    -- unknown vector, so use reset vector
                    temp_address(7 downto 0) :=  ram_data_o(6);
                    temp_address(15 downto 8) := ram_data_o(7);
                end case;
                reg_pc<=unsigned(temp_address);
                reg_pcplus1<=unsigned(temp_address)+1;
                reg_pcplus2<=unsigned(temp_address)+2;
                -- We have loaded the program counter (and operand addresses
                -- derived from that), so now we can proceed to instruction fetch.
                -- (XXX We could save one cycle for every interrupt here by doing
                -- the instruction fetch here, and then passing into OperandResolve
                -- next cycle.
                state<=InstructionFetch;
            when InstructionFetch =>
                -- Work out which three bytes to fetch.
                -- Probably easiest to do a parallel calculation based on lower
                -- bits of reg_pc, reg_pcplus1, reg_pcplus2
              fetch_next_instruction(reg_pc);
              state<=OperandResolve;
            when OperandResolve =>
              -- Get opcode and operands
              temp_opcode <= ram_data_o(to_integer(op_mem_slot));
              temp_operand_address(15 downto 8) := ram_data_o(to_integer(operand2_mem_slot));
              temp_operand_address(7 downto 0) := ram_data_o(to_integer(operand1_mem_slot));

              -- Lookup instruction and addressing mode
              op_instruction <= instruction_lut(to_integer(unsigned(temp_opcode)));
              op_mode <= mode_lut(to_integer(unsigned(temp_opcode)));

              if op_mode=M_implied then
                -- implied mode, handle instruction now, add one to PC, and
                -- go to fetch next instruction
                normal_instruction := true;
                case op_instruction is
                  when I_BRK =>
                    -- break instruction. Push state and jump to the appropriate
                    -- vector.
                    -- push high(PC+2) first
                    
                    push_byte(std_logic_vector(reg_pcplus2(15 downto 8)));
                    
                    state <= BRKPushPCL;              
                    normal_instruction := false;
                    null;
                  when I_CLC => flag_c <= '0';
                  when I_CLD => flag_d <= '0';
                  when I_CLI => flag_i <= '0';
                  when I_CLV => flag_v <= '0';
                  when I_DEX => reg_x <= reg_x - 1; set_cpu_flags_inc(reg_x);
                  when I_DEY => reg_y <= reg_y - 1; set_cpu_flags_inc(reg_y);
                  when I_INX => reg_x <= reg_x + 1; set_cpu_flags_inc(reg_x);
                  when I_INY => reg_y <= reg_y + 1; set_cpu_flags_inc(reg_y);
                  when I_KIL => state <= Halt; normal_instruction:= false;
                  when I_NOP => null;
                  when I_PHA =>
                    push_byte(std_logic_vector(reg_a));
                    advance_pc(1);
                    normal_instruction := false;
                    state <= InstructionFetch;
                  when I_PHP =>
                    temp_operand(7) := flag_n;
                    temp_operand(6) := flag_v;
                    temp_operand(5) := '1';  -- unused bit
                    temp_operand(4) := '0';  -- BRK flag
                    temp_operand(3) := flag_d;
                    temp_operand(2) := flag_i;
                    temp_operand(1) := flag_z;
                    temp_operand(0) := flag_c;
                    push_byte(temp_operand);
                    advance_pc(1);
                    normal_instruction := false;
                    state <= InstructionFetch;
                  when I_PLA =>
                    pull_byte;
                    state <= PullA;
                    advance_pc(1);
                    normal_instruction := false;
                  when I_PLP =>
                    pull_byte;
                    state <= PullP;
                    advance_pc(1);
                    normal_instruction := false;
                  when I_RTI =>
                    fetch_stack_bytes;
                    reg_sp <= reg_sp + 3;
                    state <= RTIPull;
                    normal_instruction := false;
                  when I_RTS =>
                    fetch_stack_bytes;
                    reg_sp <= reg_sp + 2;
                    state <= RTSPull;
                    normal_instruction := false;
                  when I_SEC => flag_c <= '1';
                  when I_SED => flag_d <= '1';
                  when I_SEI => flag_i <= '1';
                  when I_TAX => reg_x <= reg_a;
                  when I_TAY => reg_y <= reg_a;
                  when I_TSX => reg_x <= reg_sp;
                  when I_TXA => reg_a <= reg_x;
                  when I_TXS => reg_sp <= reg_x;
                  when I_TYA => reg_a <= reg_a;
                  when others => null;
                    -- unsupported instruction, just advance PC
                end case;
                if normal_instruction=true then
                  -- advance PC to next instruction, and fetch it.
                  -- XXX We can actually pre-fetch the instruction and
                  -- bypass going through InstructionFetch.
                  -- This will result in implied mode instructions that don't
                  -- touch the stack taking only one cycle.
                  fetch_next_instruction(reg_pcplus1);
                  state <= OperandResolve;
                end if;
              elsif op_mode=M_relative then
                -- a relative branch, work out whether to take the branch
                -- and act accordingly. We don't need to do anything further
                if (op_instruction=I_BCC and flag_c='0')
                   or (op_instruction=I_BCS and flag_c='1')
                   or (op_instruction=I_BVC and flag_v='0')
                   or (op_instruction=I_BVS and flag_v='1')
                   or (op_instruction=I_BEQ and flag_z='0')
                   or (op_instruction=I_BNE and flag_z='1') then
                  -- take branch
                  if temp_operand_address(7)='0' then
                    -- branch forwards. Add two to address because this is a two
                    -- byte instruction
                    reg_pc <= reg_pcplus2 + unsigned(temp_operand_address(6 downto 0));
                  else
                    -- branch backwards.
                    reg_pc <= reg_pcplus2 - 128 + unsigned(not temp_operand_address(6 downto 0));
                  end if;
                  reg_pcplus1 <= reg_pc + 1;
                  reg_pcplus2 <= reg_pc + 2;
                else
                  -- don't take branch, just advance program counter
                  reg_pc <= reg_pc + 2;
                  reg_pcplus1 <= reg_pc + 3;
                  reg_pcplus2 <= reg_pc + 4;
                end if;
                state <= InstructionFetch;
              -- JMP and JSR require special treatment
              elsif op_instruction=I_JSR then
                -- Push both operands, bump SP and set PC all in one
                -- cycle.
                -- We push (pc+2) since we have not yet incremented it, but
                -- not (pc+3) because RTS adds one to the popped value.
                temp_sp_addr(15 downto 8) := x"01";
                temp_sp_addr(7 downto 0) := std_logic_vector(reg_sp);
                write_to_short_address(temp_sp_addr,
                                       std_logic_vector(reg_pcplus2(15 downto 8)),
                                       ram_bank_registers);
                temp_sp_addr(7 downto 0) := std_logic_vector(reg_sp -1);
                write_to_short_address(temp_sp_addr,
                                       std_logic_vector(reg_pcplus2(7 downto 0)),
                                       ram_bank_registers);
                reg_sp <= reg_sp - 2;
                set_pc(unsigned(temp_operand_address));
                fetch_next_instruction(unsigned(temp_operand_address));
                state <= OperandResolve;
              elsif op_instruction=I_JMP and op_mode=M_absolute then
                -- JMP absolute mode: very simple, just overwrite the
                -- programme counter.
                set_pc(unsigned(temp_operand_address));
                fetch_next_instruction(unsigned(temp_operand_address));
                state <= OperandResolve;
              elsif op_instruction=I_JMP and op_mode=M_indirect then
                -- JMP indirect absolute.
                -- Fetch the two bytes, and then do the jump next cycle
                -- We can use fetch_next_instruction to do this for us
                -- (although it will read the byte before as a pretend
                --  opcode).
                fetch_next_instruction(unsigned(temp_operand_address)-1);
                state <= JMPIndirectFetch;
              elsif op_mode=M_accumulator then
                -- accumulator mode, so no need to read from memory
                -- There are only four accumulator mode instructions that do anything.
                -- They are all bit shifting operations.
                -- There are also four NOPs
                -- We handle the accumulator mode instructions as a special case
                -- basically because we can execute them faster (in 1 cycle) that
                -- way.  The memory based versions will take 3 cycles since they
                -- need to read and write a memory location.
                if op_instruction = I_ASL or op_instruction = I_ROL then
                  -- shift left
                  temp_operand(7 downto 1) := std_logic_vector(reg_a(6 downto 0));
                  if op_instruction = I_ROL then
                    temp_operand(0) := flag_c;
                  else
                    temp_operand(0) := '0';
                  end if;
                  flag_c <= reg_a(7);
                end if;
                if op_instruction = I_LSR or op_instruction = I_ROR then
                  -- shift left
                  temp_operand(6 downto 0) := std_logic_vector(reg_a(7 downto 1));
                  if op_instruction = I_ROR then
                    temp_operand(7) := flag_c;
                  else
                    temp_operand(7) := '0';
                  end if;
                  flag_c <= reg_a(0);
                end if;
                if op_instruction = I_NOP then
                  null;
                else
                  flag_n <= temp_operand(7);
                  if temp_operand = x"00" then
                    flag_z <= '1';
                  else
                    flag_z <= '0';
                  end if;
                  reg_a <= unsigned(temp_operand);
                end if;
                fetch_next_instruction(reg_pcplus1);
                state <= OperandResolve;
              elsif op_mode=M_zeropage then
                temp_address(7 downto 0) := temp_operand_address(7 downto 0);
                temp_address(15 downto 8) := "00000000";
                advance_pc(2);
                do_direct_operand(temp_address,ram_bank_registers,op_instruction);
              elsif op_mode=M_zeropageX then
                temp_address(7 downto 0) := std_logic_vector(unsigned(temp_operand_address(7 downto 0)) + unsigned(reg_x));
                temp_address(15 downto 8) := "00000000";
                advance_pc(2);
                do_direct_operand(temp_address,ram_bank_registers,op_instruction);
              elsif op_mode=M_zeropageY then
                temp_address(7 downto 0) := std_logic_vector(unsigned(temp_operand_address(7 downto 0)) + unsigned(reg_y));
                temp_address(15 downto 8) := "00000000";
                advance_pc(2);
                do_direct_operand(temp_address,ram_bank_registers,op_instruction);
              elsif op_mode=M_absolute then
                temp_address(15 downto 0) := temp_operand_address;
                advance_pc(3);
                do_direct_operand(temp_address,ram_bank_registers,op_instruction);
              elsif op_mode=M_absoluteX then
                temp_address(15 downto 0) := std_logic_vector(unsigned(temp_operand_address) + unsigned(reg_x));
                advance_pc(3);
                do_direct_operand(temp_address,ram_bank_registers,op_instruction);
              elsif op_mode=M_absoluteY then
                temp_address(15 downto 0) := std_logic_vector(unsigned(temp_operand_address) + unsigned(reg_y));
                advance_pc(3);
                do_direct_operand(temp_address,ram_bank_registers,op_instruction);
              elsif op_mode=M_indirectX then
                -- Pre-indexed ZP indirect, wrapping from $FF to $00, not $100
                -- if operand+X=$FF
                temp_address(7 downto 0) := std_logic_vector(unsigned(temp_operand_address(7 downto 0)) + reg_x);
                advance_pc(2);
                read_indirect_operand(temp_address(7 downto 0),ram_bank_registers);
                op_mode<=M_absolute;
                state <= OperandResolve;
              elsif op_mode=M_indirectY then
                -- Post-indexed ZP indirect, wrapping from $FF to $00, not $100
                -- if operand=$FF
                temp_address(7 downto 0) := temp_operand_address(7 downto 0);
                advance_pc(2);
                read_indirect_operand(temp_address(7 downto 0),ram_bank_registers);
                op_mode<=M_absoluteY;
                state <= OperandResolve;
              end if;
            when JMPIndirectFetch =>
              -- Fetched indirect address, so copy it into the programme counter
              set_pc(unsigned(temp_operand_address));
              fetch_next_instruction(unsigned(temp_operand_address));
              state <= OperandResolve;
            when PullA =>
              -- PLA - Pull Accumulator from the stack
              -- In the previous cycle we asked for the byte to be read from
              -- the stack.
              reg_a <= unsigned(ram_data_o(to_integer(pull_bank)));
              fetch_next_instruction(reg_pcplus1);
              state <= OperandResolve;
            when PullP =>
              -- PLA - Pull Accumulator from the stack
              -- In the previous cycle we asked for the byte to be read from
              -- the stack.
              temp_operand := ram_data_o(to_integer(unsigned(pull_bank)));
              flag_n <= temp_operand(7);
              flag_v <= temp_operand(6);
              flag_d <= temp_operand(3);
              flag_i <= temp_operand(2);
              flag_z <= temp_operand(1);
              flag_c <= temp_operand(0);
              fetch_next_instruction(reg_pcplus1);
              state <= OperandResolve;
            when RTIPull =>
              -- All values needed have been read in one go
              temp_operand := ram_data_o(to_integer(unsigned(op_mem_slot)));
              flag_n <= temp_operand(7);
              flag_v <= temp_operand(6);
              flag_d <= temp_operand(3);
              flag_i <= temp_operand(2);
              flag_z <= temp_operand(1);
              flag_c <= temp_operand(0);
              reg_pc(7 downto 0) <= unsigned(ram_data_o(to_integer(unsigned(operand1_mem_slot))));
              reg_pc(15 downto 8) <= unsigned(ram_data_o(to_integer(unsigned(operand2_mem_slot))));
              state <= InstructionFetch;
            when RTSPull =>
              -- All values needed have been read in one go
              temp_address(7 downto 0) := ram_data_o(to_integer(unsigned(op_mem_slot)));
              temp_address(15 downto 8) := ram_data_o(to_integer(unsigned(operand1_mem_slot)));
              reg_pc <= unsigned(temp_address) + 1;
              state <= InstructionFetch;            when others => null;
          end case;
        end if;
      end if;
    end process;
end Behavioral;