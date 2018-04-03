# Copyright 2017 jem@seethis.link
# Licensed under the MIT license (http://opensource.org/licenses/MIT)

TARGET_BASE_NAME = boot_jack

ARCH = AVR_MEGA
F_CPU = 16000000
# F_USB = 48000000

# Output format. (can be srec, ihex, binary)
FORMAT = ihex

MCU = atmega32u4

#######################################################################
#                        board config options                         #
#######################################################################

BOARD_DIR = boards
BUILD_DIR = build

# Note: Specific board configs are stored in the `boards` directory.

ifndef BOARD
  BOARD = default
endif

# Look for the board appropriate board .mk file to include
include avr-makefile/boards.mk

#######################################################################
#                         programmer options                          #
#######################################################################

# AVRDUDE_CMD=avrdude -p $(AVRDUDE_PART) -c avrispmkII
AVRDUDE_CMD=avrdude -p $(MCU) -c usbasp

#######################################################################
#                           compiler setup                            #
#######################################################################

CFLAGS +=

# List C source files here.
C_SRC += \
	main.c
	# bootjack.c
	 # src/bootjack.c

# List Assembler source files here.
# NOTE: Use *.S for user written asm files. *.s is used for compiler generated
ASM_SRC = \
	# main.S \

# Optimization level, can be [0, 1, 2, 3, s].
OPT = s

# List any extra directories to look for include files here.
EXTRAINCDIRS =

# Compiler flag to set the C Standard level.
CSTANDARD = -std=gnu99

CDEFS +=
ADEFS +=

LD_SCRIPT_DIR = /usr/lib/ldscripts
# LD_SCRIPT_DIR = ./ld-scripts

all: hex fuse

program-dfu: $(TARGET_HEX)
	dfu-programmer $(MCU) erase --force
	dfu-programmer $(MCU) flash $(TARGET_HEX)
	dfu-programmer $(MCU) start
include avr-makefile/avr.mk
include avr-makefile/avr-program.mk

# Listing of phony targets.
.PHONY : all begin finish end sizebefore sizeafter gccversion \
build elf hex eep lss sym coff extcoff doxygen clean program-fuses \
