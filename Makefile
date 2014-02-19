ROUTEEFFORT=	std
#ROUTEEFFORT=	med
#ROUTEEFFORT=	high

all:	makerom \
	container.prj \
	gs4510.vhdl viciv.vhdl
#	scp *.xise *.prj *vhd *vhdl 192.168.56.102:c64accel/
#	ssh 192.168.56.102 "( cd c64accel ; /opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/xst -intstyle ise -ifn \"/home/gardners/c64accel/container.xst\" -ofn \"/home/gardners/c64accel/container.syr\" )"
#	ssh 192.168.56.102 "( cd c64accel ; /opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/map -intstyle ise -p xc7a100t-csg324-1 -w -logic_opt off -ol "$(ROUTEEFFORT)" -xe n -t 1 -xt 0 -register_duplication off -r 4 -mt off -ir off -pr off -lc off -power off -o container_map.ncd container.ngd container.pcf )"
#	ssh 192.168.56.102 "( cd c64accel ; /opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/par -w -intstyle ise -ol "$(ROUTEEFFORT)" -xe n -mt off container_map.ncd container.ngd container.pcf )"
	/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/xst -intstyle ise -ifn \"/home/gardners/c64accel/container.xst\" -ofn \"/home/gardners/c64accel/container.syr\"
	/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/map -intstyle ise -p xc7a100t-csg324-1 -w -logic_opt off -ol "$(ROUTEEFFORT)" -xe n -t 1 -xt 0 -register_duplication off -r 4 -mt off -ir off -pr off -lc off -power off -o container_map.ncd container.ngd container.pcf
	/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin/par -w -intstyle ise -ol "$(ROUTEEFFORT)" -xe n -mt off container_map.ncd container.ngd container.pcf

kickstart65gs.bin:	kickstart.a65 Makefile
	../Ophis/bin/ophis -4 kickstart.a65 -l kickstart.list

kickstart.vhdl:	kickstart65gs.bin rom_template.vhdl makerom
	./makerom rom_template.vhdl kickstart65gs.bin kickstart

transfer:	kickstart.vhdl
	scp -p Makefile makerom *.ucf *.xise *.prj *vhd *vhdl 192.168.56.101:c64accel/


simulate:	iomapper.vhdl container.vhd cpu_test.vhdl viciv.vhdl gs4510.vhdl debugtools.vhdl UART_TX_CTRL.vhd uart_rx.vhdl uart_monitor.vhdl machine.vhdl cia6526.vhdl keymapper.vhdl ghdl_ram8x64k.vhdl charrom.vhdl ghdl_ram64x16k.vhdl kickstart.vhdl sdcardio.vhdl sd.vhdl sectorbuffer.vhdl
	ghdl -c iomapper.vhdl container.vhd cpu_test.vhdl viciv.vhdl gs4510.vhdl debugtools.vhdl UART_TX_CTRL.vhd uart_rx.vhdl uart_monitor.vhdl machine.vhdl cia6526.vhdl keymapper.vhdl ghdl_ram8x64k.vhdl charrom.vhdl ghdl_ram64x16k.vhdl kickstart.vhdl sdcardio.vhdl sd.vhdl sectorbuffer.vhdl -r cpu_test

testcia:	tb_cia.vhdl cia6526.vhdl debugtools.vhdl
	ghdl -c tb_cia.vhdl cia6526.vhdl debugtools.vhdl -r tb_cia

testiomapper:	tb_iomapper.vhdl debugtools.vhdl iomapper.vhdl kickstart.vhdl cia6526.vhdl sdcardio.vhdl sd.vhdl sectorbuffer.vhdl
	ghdl -c tb_iomapper.vhdl debugtools.vhdl iomapper.vhdl kickstart.vhdl cia6526.vhdl sdcardio.vhdl sd.vhdl sectorbuffer.vhdl -r tb_iomapper

adctest:	adctest.a65
	Ophis-2.0-standalone/ophis -o adctest adctest.a65

monitor_drive:	monitor_drive.c Makefile
	gcc -g -Wall -o monitor_drive monitor_drive.c

kickload:	kickload.c Makefile
	gcc -g -Wall -o kickload kickload.c

chargen_debug:	chargen_debug.c
	gcc -Wall -o chargen_debug chargen_debug.c

dis4510:	dis4510.c
	gcc -g -Wall -o dis4510 dis4510.c

4510tables:	4510tables.c
	gcc -g -Wall -o 4510tables 4510tables.c
