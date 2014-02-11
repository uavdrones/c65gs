ROUTEEFFORT=	std
#ROUTEEFFORT=	med
#ROUTEEFFORT=	high

all:	makerom kernel65.vhdl \
	container.prj \
	cpu6502.vhdl alu6502.vhdl bcdadder.vhdl spartan6blockram.vhdl vga.vhd
#	scp *.xise *.prj *vhd *vhdl 192.168.56.102:c64accel/
#	ssh 192.168.56.102 "( cd c64accel ; /opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/xst -intstyle ise -ifn \"/home/gardners/c64accel/container.xst\" -ofn \"/home/gardners/c64accel/container.syr\" )"
#	ssh 192.168.56.102 "( cd c64accel ; /opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/map -intstyle ise -p xc7a100t-csg324-1 -w -logic_opt off -ol "$(ROUTEEFFORT)" -xe n -t 1 -xt 0 -register_duplication off -r 4 -mt off -ir off -pr off -lc off -power off -o container_map.ncd container.ngd container.pcf )"
#	ssh 192.168.56.102 "( cd c64accel ; /opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/par -w -intstyle ise -ol "$(ROUTEEFFORT)" -xe n -mt off container_map.ncd container.ngd container.pcf )"
	/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/xst -intstyle ise -ifn \"/home/gardners/c64accel/container.xst\" -ofn \"/home/gardners/c64accel/container.syr\"
	/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/map -intstyle ise -p xc7a100t-csg324-1 -w -logic_opt off -ol "$(ROUTEEFFORT)" -xe n -t 1 -xt 0 -register_duplication off -r 4 -mt off -ir off -pr off -lc off -power off -o container_map.ncd container.ngd container.pcf
	/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/par -w -intstyle ise -ol "$(ROUTEEFFORT)" -xe n -mt off container_map.ncd container.ngd container.pcf

cputest.prg:	cputest.a65
	Ophis-2.0-standalone/ophis -o cputest.prg cputest.a65


kernel65.bin:	kernel65.a65
	Ophis-2.0-standalone/ophis kernel65.a65

kernel65.vhdl:	rom_template.vhdl kernel65.bin makerom
	./makerom rom_template.vhdl kernel65.bin kernel65

kernel64.vhdl:	rom_template.vhdl kernel64.bin makerom
	./makerom rom_template.vhdl kernel64.bin kernel64

basic64.vhdl:	rom_template.vhdl basic64.bin makerom
	./makerom rom_template.vhdl basic64.bin basic64

hesmonc000.vhdl:	rom_template.vhdl hesmonc000.bin makerom
	./makerom rom_template.vhdl hesmonc000.bin hesmonc000

transfer:
	scp -p Makefile makerom kernel65.a65 *.ucf *.xise *.prj *vhd *vhdl 192.168.56.101:c64accel/


simulate:	bcdadder.vhdl alu6502.vhdl cpu6502.vhdl kernel65.vhdl kernel64.vhdl basic64.vhdl iomapper.vhdl container.vhd cpu_test.vhdl vga.vhd simple6502.vhdl debugtools.vhdl UART_TX_CTRL.vhd uart_rx.vhdl uart_monitor.vhdl machine.vhdl cia6526.vhdl keymapper.vhdl ghdl_ram8x64k.vhdl charrom.vhdl hesmonc000.vhdl ghdl_ram64x16k.vhdl
	ghdl -c kernel65.vhdl kernel64.vhdl basic64.vhdl iomapper.vhdl container.vhd cpu_test.vhdl vga.vhd simple6502.vhdl debugtools.vhdl UART_TX_CTRL.vhd uart_rx.vhdl uart_monitor.vhdl machine.vhdl cia6526.vhdl keymapper.vhdl ghdl_ram8x64k.vhdl charrom.vhdl hesmonc000.vhdl ghdl_ram64x16k.vhdl -r cpu_test

testcia:	tb_cia.vhdl cia6526.vhdl debugtools.vhdl
	ghdl -c tb_cia.vhdl cia6526.vhdl debugtools.vhdl -r tb_cia

testadder:	tb_adder.vhdl debugtools.vhdl
	ghdl -c tb_adder.vhdl debugtools.vhdl -r tb_adder

adctest:	adctest.a65
	Ophis-2.0-standalone/ophis -o adctest adctest.a65

monitor_drive:	monitor_drive.c Makefile
	gcc -g -Wall -o monitor_drive monitor_drive.c

chargen_debug:	chargen_debug.c
	gcc -Wall -o chargen_debug chargen_debug.c

dis4510:	dis4510.c
	gcc -g -Wall -o dis4510 dis4510.c

4510tables:	4510tables.c
	gcc -g -Wall -o 4510tables 4510tables.c

sdcard.vhdl:
	wget -O sdcard.vhdl https://raw2.github.com/xesscorp/XuLA2/master/FPGA/XuLA_lib/SDCard.vhd

common.vhdl:
	wget -O common.vhdl https://raw2.github.com/xesscorp/XuLA2/master/FPGA/XuLA_lib/Common.vhd

clkgen.vhdl:
	wget -O clkgen.vhdl https://raw2.github.com/xesscorp/XuLA2/master/FPGA/XuLA_lib/ClkGen.vhd

hostio.vhdl:
	wget -O hostio.vhdl https://raw2.github.com/xesscorp/XuLA2/master/FPGA/XuLA_lib/HostIo.vhd

synctoclk.vhdl:
	wget -O synctoclk.vhdl https://raw2.github.com/xesscorp/XuLA2/master/FPGA/XuLA_lib/SyncToClk.vhd
