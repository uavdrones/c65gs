use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
use work.debugtools.all;

entity tb_adder is
end tb_adder;

architecture behaviour of tb_adder is
  signal flag_n : std_logic := '0';
  signal flag_z : std_logic := '0';
  signal flag_v : std_logic := '0';
  signal flag_c : std_logic := '0';
  signal flag_d : std_logic := '0';
begin  -- behaviour

-- purpose: test adder
  -- type   : combinational
  -- inputs : 
  -- outputs: 
  test: process
    
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
    
    impure function alu_op_add (
      i1 : in unsigned(7 downto 0);
      i2 : in unsigned(7 downto 0)) return unsigned is
    variable tmp : unsigned(8 downto 0);
  begin
    if flag_d='1' then
      tmp(8) := '0';
      tmp(7 downto 0) := (i1 and x"0f") + (i2 and x"0f") + ("0000000" & flag_c);

      report "flag_c=" & std_logic'image(flag_c) severity note;
      report "low nybl sum before fixing = $" & to_hstring(tmp(7 downto 0)) severity note;
      report "i1=$" & to_hstring(i1) severity note;
      report "i2=$" & to_hstring(i2) severity note;
      if tmp > x"09" then
        tmp := tmp + x"06";                                                                         
      end if;
      if tmp < x"10" then
        tmp := ("0"&(tmp(7 downto 0) and x"0f")) + ("0"&(i1 and x"f0")) + ("0"&(i2 and x"f0"));
      else
        tmp := ("0"&(tmp(7 downto 0) and x"0f")) + ("0"&(i1 and x"f0")) + ("0"&(i2 and x"f0")) + "0"&x"10";
      end if;
      if (i1 + i2 + ( "0000000" & flag_c )) = x"00" then
        flag_z <= '1';
      else
        flag_z <= '0';
      end if;
      flag_n <= tmp(7);
      flag_v <= (i1(7) xor tmp(7)) and (not (i1(7) and i2(7)));      
      if tmp(8 downto 4) > "01001" then
        tmp := tmp + x"60";
      end if;
      flag_c <= tmp(8);
    else
      tmp := ("0"&i2)
             + ("0"&i1)
             + ("00000000"&flag_c);
      tmp(7 downto 0) := with_nz(tmp(7 downto 0));
      flag_v <= (not (i1(7) xor i2(7))) and (i1(7) xor tmp(7));
      flag_c <= tmp(8);
    end if;
    -- Return final value
    report "add result of "
      & "$" & to_hstring(std_logic_vector(i1)) 
      & " + "
      & "$" & to_hstring(std_logic_vector(i2)) 
      & " + "
      & "$" & std_logic'image(flag_c)
      & " = " & to_hstring(std_logic_vector(tmp(7 downto 0))) severity note;
    return tmp(7 downto 0);
  end function alu_op_add;

    variable result : unsigned(7 downto 0);
  begin  -- process test
    flag_c <= '0';
    wait for 1 ns;
    result := alu_op_add(x"00",x"88");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert flag_v='0' report "v should be 0" severity failure;

    flag_c <= '0';
    wait for 1 ns;
    result := alu_op_add(x"11",x"77");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert flag_v='1' report "v should be 1" severity failure;

    flag_c <= '0';
    wait for 1 ns;
    result := alu_op_add(x"11",x"88");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert flag_v='0' report "v should be 0" severity failure;

    flag_c <= '0';
    wait for 1 ns;
    result := alu_op_add(x"11",x"99");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert flag_v='0' report "v should be 0" severity failure;

    flag_c <= '0';
    wait for 1 ns;
    result := alu_op_add(x"22",x"66");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert flag_v='1' report "v should be 1" severity failure;

    flag_c <= '0';
    wait for 1 ns;
    result := alu_op_add(x"33",x"55");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert flag_v='1' report "v should be 1" severity failure;

    flag_c <= '0';
    wait for 1 ns;
    result := alu_op_add(x"44",x"44");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert flag_v='1' report "v should be 1" severity failure;

    flag_c <= '0';
    wait for 1 ns;
    result := alu_op_add(x"44",x"88");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert flag_v='0' report "v should be 0" severity failure;

    flag_c <= '1';
    wait for 1 ns;
    result := alu_op_add(x"11",x"77");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert flag_v='1' report "v should be 1" severity failure;

    report "DECIMAL MODE TESTS" severity note;
    
    flag_c <= '0';
    flag_d <= '1';
    wait for 1 ns;
    result := alu_op_add(x"00",x"11");
    report "result is $" & to_hstring(result) severity note;
    wait for 1 ns;
    report "v=" &std_logic'image(flag_v) & ", z=" &std_logic'image(flag_z) & ", c=" &std_logic'image(flag_c) & ", n=" &std_logic'image(flag_n) severity note;
    assert result=x"11" report "result should be $11" severity failure;

    
    report "Simulation ended" severity failure;
  end process test;
end behaviour;