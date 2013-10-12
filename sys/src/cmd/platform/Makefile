# platform - simple os-like version of electron 
# Original Code (C)opyright 2013 Joshua Scoggins

include config.mk

SRC = platform.c reroute.c userfunctions.c
OBJ = ${SRC:.c=.o}


all: options platform 

options:
	@echo platform build options:
	@echo "CFLAGS = ${CFLAGS}"
	@echo "LDFLAGS = ${LDFLAGS}"
	@echo "CC = ${CC}"

.c.o:
	@echo CC $<
	@${CC} -c ${CFLAGS} $<

${OBJ}: config.mk

platform: ${OBJ}
	@echo CC -o platform
	@${CC} -o $@ ${OBJ} ${LDFLAGS}

clean: 
	@echo cleaning
	@rm -f platform ${OBJ}

install: all
	@echo installing binary to ${PREFIX}/bin
	@mkdir -p ${PREFIX}/bin
	@cp platform ${PREFIX}/bin

uninstall:
	@echo removing binary from ${PREFIX}/bin
	@rm -f ${PREFIX}/bin/platform

.PHONY: all options clean install uninstall
