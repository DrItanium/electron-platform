# raw - simple os-like version of electron 
# Original Code (C)opyright 2013 Joshua Scoggins

include config.mk

SRC = raw.c reroute.c userfunctions.c
OBJ = ${SRC:.c=.o}


all: options raw 

options:
	@echo raw build options:
	@echo "CFLAGS = ${CFLAGS}"
	@echo "LDFLAGS = ${LDFLAGS}"
	@echo "CC = ${CC}"

.c.o:
	@echo CC $<
	@${CC} -c ${CFLAGS} $<

${OBJ}: config.mk

raw: ${OBJ}
	@echo CC -o raw
	@${CC} -o $@ ${OBJ} ${LDFLAGS}

clean: 
	@echo cleaning
	@rm -f raw ${OBJ}

install: all
	@echo installing binary to ${PREFIX}/bin
	@mkdir -p ${PREFIX}/bin
	@cp raw ${PREFIX}/bin

uninstall:
	@echo removing binary from ${PREFIX}/bin
	@rm -f ${PREFIX}/bin/raw

.PHONY: all options clean install uninstall
