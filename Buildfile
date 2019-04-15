#Arduino home
ARDUINO = /usr/share/arduino

#Compilers, etc.
CC    = $(ARDUINO)/hardware/tools/avr/bin/avr-gcc
CPP   = $(ARDUINO)/hardware/tools/avr/bin/avr-g++
AR    = $(ARDUINO)/hardware/tools/avr/bin/avr-ar
OBJCP = $(ARDUINO)/hardware/tools/avr/bin/avr-objcopy
DUDE  = $(ARDUINO)/hardware/tools/avr/bin/avrdude

#Board parameters
ifeq ($(BOARD), uno)
	FCPU = 16000000
	LMCU = atmega328p
	SMCU = m328p
	VAR  = standard
endif

#Flags
CCFLAGS  = -c -g -Os -Wall -ffunction-sections -fdata-sections -mmcu=$(LMCU) -DF_CPU=$(FCPU)L -MMD -DUSB_VID=null -DUSB_PID=null -DARDUINO=106
CPPFLAGS = $(CCFLAGS) -fno-exceptions

#Include files
INCLUDES = "-I$(ARDUINO)/hardware/arduino/cores/arduino" "-I$(ARDUINO)/hardware/arduino/variants/$(VAR)"

#Libraries
LIBS = "$(ARDUINO)/hardware/arduino/cores/arduino"

build:
	$(CPP) $(CPPFLAGS) $(INCLUDES) -x c++ -include Arduino.h $(SKETCH) -o $(SKETCH).o
	for file in $(LIBS)/avr-libc/*.c; do \
		$(CC) $(CCFLAGS) $(INCLUDES) $$file -o $$(basename $$file).o; \
	done
	for file in $(LIBS)/*.c; do \
		$(CC) $(CCFLAGS) $(INCLUDES) $$file -o $$(basename $$file).o; \
	done
	for file in $(LIBS)/*.cpp; do \
		$(CPP) $(CPPFLAGS) $(INCLUDES) $$file -o $$(basename $$file).o; \
	done
	for file in ./*.o; do \
		$(AR) rcs core.a $$file; \
	done
	$(CC) -Os -Wl,--gc-sections -mmcu=$(LMCU) -o $(SKETCH).elf $(SKETCH).o core.a -lm
	$(OBJCP) -O ihex -j .eeprom --set-section-flags=.eeprom=alloc,load --no-change-warnings --change-section-lma .eeprom=0 $(SKETCH).elf $(SKETCH).eep
	$(OBJCP) -O ihex -R .eeprom $(SKETCH).elf $(SKETCH).hex

upload:
	$(DUDE) -c arduino -p $(SMCU) -P $(PORT) -U flash:w:$(HEX)

clean:
	rm ./*.o ./*.d ./*.a ./*.eep ./*.elf ./*.hex ./*.ino
