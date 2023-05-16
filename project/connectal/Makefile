
CONNECTALDIR ?=/home/hengyu/zspace/mit/connectal
S2H_INTERFACES = \
	ConnectalProcRequest:ConnectalWrapper.connectProc\
	ConnectalMemoryInitialization:ConnectalWrapper.initProc
H2S_INTERFACES = \
	ConnectalWrapper:ConnectalProcIndication

USE_BUILDCACHE=1

BSVFILES += \
	./*.bsv
BSVPATH += / \
	../src/ \
	../src/includes \
	../src/ref \
	$(CONNECTALDIR)/bsv
CPPFILES += \
	main.cpp \
	Platform.cpp \

C_LIB_DIR = $(realpath ../src/ref)
C_LIB_DIR2 = $(realpath bluesim/obj)


#CONNECTALFLAGS += --mainclockperiod=20
#CONNECTALFLAGS += --verilog=ddr3_v2_0/

SOFTWARE_SOCKET_NAME=/tmp/connectal$(USER)
export SOFTWARE_SOCKET_NAME



CONNECTALFLAGS += -D CORE_NUM=$(CORENUM)
CONNECTALFLAGS += -D $(VPROC)
CONNECTALFLAGS += -D $(CACHE)

CONNECTALFLAGS += --bscflags " -I $(C_LIB_DIR)"
CONNECTALFLAGS += --bscflags " -L $(C_LIB_DIR)"
CONNECTALFLAGS += --bscflags " -I $(C_LIB_DIR2)"
CONNECTALFLAGS += --bscflags " -L $(C_LIB_DIR2)"

CONNECTALFLAGS += --bscflags " -D CORE_NUM=$(CORENUM) -D $(VPROC) -D $(CACHE) -steps-max-intervals 50 +RTS -K256M -RTS"
CONNECTALFLAGS += --bscflags " -show-schedule"
CONNECTALFLAGS += --cxxflags="-g -std=gnu++11"

include $(CONNECTALDIR)/Makefile.connectal

build:
	# make -j32 build.bluesim CORENUM=2 VPROC=SIXSTAGE CACHE=SLHUSM
	make -j32 build.bluesim CORENUM=2 VPROC=SIXSTAGE CACHE=SC
	# can not link the bluesim executable with the above command, so
	# run the following command to generate the bluesim executable
	cd bluesim && \
	g++ -O -g -o bin/bsim -Iobj -I/opt/bsc/lib/Bluesim obj/*.o  ../../src/ref/*.o /home/hengyu/zspace/mit/connectal/cpp/bluesim_main.cxx -L/opt/bsc/lib/Bluesim -lbskernel -lbsprim -lpthread -L bin -lconnectal-sim -L obj
clean:
	rm -rf bluesim
