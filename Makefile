# -----------------------------------------------------------------------------

megabuild		= 1
finalbuild		= 1
attachdebugger	= 0

# -----------------------------------------------------------------------------

MAKE			= make
CP				= cp
MV				= mv
RM				= rm -f

SRC_DIR			= ./src
EXE_DIR			= ./exe
BIN_DIR			= ./bin

# mega65 fork of ca65: https://github.com/dillof/cc65
AS				= ca65mega
ASFLAGS			= -g -D megabuild=$(megabuild) --cpu 45GS02 -U --feature force_range -I ./exe
LD				= ld65
C1541			= c1541
CC1541			= cc1541
SED				= sed
PU				= pucrunch
BBMEGA			= b2mega
LC				= crush 6
GCC				= gcc
MC				= MegaConvert
MEGAADDRESS		= megatool -a
MEGACRUNCH		= megatool -c
MEGAIFFL		= megatool -i
MEGAMOD			= MegaMod
EL				= etherload -i 192.168.1.255
XMEGA65			= H:\xemu\xmega65.exe
MEGAFTP			= mega65_ftp -i 192.168.1.255

CONVERTBREAK	= 's/al [0-9A-F]* \.br_\([a-z]*\)/\0\nbreak \.br_\1/'
CONVERTWATCH	= 's/al [0-9A-F]* \.wh_\([a-z]*\)/\0\nwatch store \.wh_\1/'

CONVERTVICEMAP	= 's/al //'

.SUFFIXES: .o .s .out .bin .pu .b2 .a

default: all

OBJS = $(EXE_DIR)/boot.o $(EXE_DIR)/main.o

# -----------------------------------------------------------------------------

$(BIN_DIR)/bitmap_pal0.bin: $(BIN_DIR)/bitmap.bin
	$(MC) $< cm1:1 d1:3 cl1:10000 rc1:0

$(BIN_DIR)/m65sprites_sprites0.bin: $(BIN_DIR)/m65sprites.bin
	$(MC) $< cm1:1 d1:3 cl1:10000 rc1:0 sm1:1

$(EXE_DIR)/boot.o:	$(SRC_DIR)/boot.s \
					$(SRC_DIR)/main.s \
					$(SRC_DIR)/sdc.s \
					$(SRC_DIR)/macros.s \
					$(SRC_DIR)/mathmacros.s \
					$(SRC_DIR)/ringbuffer.s \
					$(SRC_DIR)/sampleplay.s \
					$(SRC_DIR)/keyboard.s \
					$(BIN_DIR)/bitmap_pal0.bin \
					$(BIN_DIR)/m65sprites_sprites0.bin \
					Makefile Linkfile
	$(AS) $(ASFLAGS) -o $@ $<

$(EXE_DIR)/boot.prg.addr.mc: $(BINFILES) $(EXE_DIR)/boot.o Linkfile
	$(LD) -Ln $(EXE_DIR)/boot.maptemp --dbgfile $(EXE_DIR)/boot.dbg -C Linkfile -o $(EXE_DIR)/boot.prg $(EXE_DIR)/boot.o
	$(MEGAADDRESS) $(EXE_DIR)/boot.prg 00001000
	$(MEGACRUNCH) -e 00001000 $(EXE_DIR)/boot.prg.addr

$(EXE_DIR)/megapple.d81: $(EXE_DIR)/boot.prg.addr.mc
	$(RM) $@
	$(CC1541) -n "megapple" -i " 2023" -d 19 -v\
	 \
	 -f "megapple" -w $(EXE_DIR)/boot.prg.addr.mc \
	$@

# -----------------------------------------------------------------------------

run: $(EXE_DIR)/megapple.d81

ifeq ($(megabuild), 1)
	$(MEGAFTP) -c "put D:\Mega\MEGApple\exe\megapple.d81 megapple.d81" -c "quit"
	$(EL) -m MEGAPPLE.D81 -r $(EXE_DIR)/boot.prg.addr.mc
ifeq ($(attachdebugger), 1)
	m65dbg --device /dev/ttyS2
endif
else
	cmd.exe /c $(XMEGA65) -autoload -8 $(EXE_DIR)/megapple.d81
endif

clean:
	$(RM) $(EXE_DIR)/*.*
	$(RM) $(EXE_DIR)/*
