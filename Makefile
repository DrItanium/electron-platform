# efssrv - electron file system server
# Original Code (C)opyright 2013 Joshua Scoggins

include config.mk

SRC = fsoverride.c
OBJ = ${SRC:.c=.o}


all: options efssrv

options:
	@echo efssrv build options:
	@echo "CFLAGS = ${CFLAGS}"
	@echo "LDFLAGS = ${LDFLAGS}"
	@echo "CC = ${CC}"

.c.o:
	@echo CC $<
	@${CC} -c ${CFLAGS} $<

${OBJ}: efssrv.h config.h config.mk

config.h:
	@echo creating $@ from config.default.h
	@cp config.default.h $@

efssrv: ${OBJ}
	@echo building static library
	@${AR} rcs libefssrv.a ${OBJ}
	@echo building dynamic library
	@${LD} ${LDFLAGS} -shared -o libefssrv.so ${OBJS} 

install: all
	@echo installing static library to ${PREFIX}/lib
	@mkdir -p ${PREFIX}/lib
	@cp -f libefssrv.a ${PREFIX}/lib
	@echo installing dynamic library to ${PREFIX}/lib
	@cp -f libefssrv.so ${PREFIX}/lib
	@echo installing headers to ${PREFIX}/include
	@cp -f efssrv.h ${PREFIX}/include

uninstall:
	@echo removing static library from ${PREFIX}/lib
	@rm -f ${PREFIX}/lib/libefssrv.a
	@echo removing dynamic library from ${PREFIX}/lib
	@rm -f ${PREFIX}/lib/libefssrv.so
	@echo removing headers from ${PREFIX}/include
	@rm -f ${PREFIX}/include/efssrv.h

clean: 
	@echo cleaning
	@rm -f libefssrv.a libefssrv.so ${OBJ}

.PHONY: all options clean install uninstall
	
