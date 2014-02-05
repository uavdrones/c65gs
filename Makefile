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

transfer:
	scp -p Makefile makerom kernel65.a65 *.ucf *.xise *.prj *vhd *vhdl 192.168.56.101:c64accel/


simulate:	bcdadder.vhdl alu6502.vhdl cpu6502.vhdl kernel65.vhdl kernel64.vhdl basic64.vhdl iomapper.vhdl container.vhd cpu_test.vhdl vga.vhd simple6502.vhdl debugtools.vhdl UART_TX_CTRL.vhd uart_rx.vhdl uart_monitor.vhdl machine.vhdl cia6526.vhdl keymapper.vhdl ghdl_ram8x64k.vhdl charrom.vhdl interfacec000.vhdl ghdl_ram64x16k.vhdl
	ghdl -c kernel65.vhdl kernel64.vhdl basic64.vhdl iomapper.vhdl container.vhd cpu_test.vhdl vga.vhd simple6502.vhdl debugtools.vhdl UART_TX_CTRL.vhd uart_rx.vhdl uart_monitor.vhdl machine.vhdl cia6526.vhdl keymapper.vhdl ghdl_ram8x64k.vhdl charrom.vhdl interfacec000.vhdl ghdl_ram64x16k.vhdl -r cpu_test

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

#
#  Extract ROMs if necessary from a C65 ROM dump
#

interface.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=interface.bin bs=4096 count=1 skip=12

kernel64.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=kernel64.bin bs=4096 count=2 skip=14

char64.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=char64.bin bs=4096 count=1 skip=13

basic64.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=basic64.bin bs=4096 count=2 skip=10

char65.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=char65.bin bs=4096 count=2 skip=8

c65doslow.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=c65doslow.bin bs=4096 count=2 skip=0

c65doshigh.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=c65doshigh.bin bs=4096 count=2 skip=2

c65monitor.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=c65monitor.bin bs=4096 count=2 skip=16

c65basic2000.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=c65basic2000.bin bs=4096 count=2 skip=18

c65basic4000.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=c65basic4000.bin bs=4096 count=2 skip=20

c65basic6000.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=c65basic6000.bin bs=4096 count=2 skip=22

c65graphics.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=c65graphics.bin bs=4096 count=2 skip=24

kernel65.bin:	c65-rom-910111.bin
	dd if=c65-rom-910111.bin of=kernel65.bin bs=4096 count=2 skip=30

kernel65.vhdl:	rom_template.vhdl kernel65.bin makerom
	./makerom rom_template.vhdl kernel65.bin kernel65

kernel64.vhdl:	rom_template.vhdl kernel64.bin makerom
	./makerom rom_template.vhdl kernel64.bin kernel64

basic64.vhdl:	rom_template.vhdl basic64.bin makerom
	./makerom rom_template.vhdl basic64.bin basic64

interfacec000.vhdl:	rom_4k_template.vhdl interface.bin makerom
	./makerom rom_4k_template.vhdl interface.bin interfacec000

char64.vhdl:	rom_4k_template.vhdl char64.bin makerom
	./makerom rom_4k_template.vhdl char64.bin char64

%.vhdl:	%.bin makerom rom_template.vhdl Makefile
	./makerom rom_template.vhdl $< $@
