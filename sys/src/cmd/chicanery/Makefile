# chicanery - simple os-like version of electron 
# Original Code (C)opyright 2013 Joshua Scoggins

include config.mk

SRC = chicanery.c reroute.c userfunctions.c
OBJ = ${SRC:.c=.o}


all: options chicanery 

options:
	@echo chicanery build options:
	@echo "CFLAGS = ${CFLAGS}"
	@echo "LDFLAGS = ${LDFLAGS}"
	@echo "CC = ${CC}"

.c.o:
	@echo CC $<
	@${CC} -c ${CFLAGS} $<

${OBJ}: config.mk

chicanery: ${OBJ}
	@echo CC -o chicanery
	@${CC} -o $@ ${OBJ} ${LDFLAGS}

clean: 
	@echo cleaning
	@rm -f chicanery ${OBJ}

install: all
	@echo installing binary to ${PREFIX}/bin
	@mkdir -p ${PREFIX}/bin
	@cp chicanery ${PREFIX}/bin

uninstall:
	@echo removing binary from ${PREFIX}/bin
	@rm -f ${PREFIX}/bin/chicanery

.PHONY: all options clean install uninstall
