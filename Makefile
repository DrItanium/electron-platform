# efssrv - electron file system server
# Original Code (C)opyright 2013 Joshua Scoggins

include config.mk

all: options

options:
	@echo efssrv build options:
	@echo "CFLAGS = ${CFLAGS}"
	@echo "LDFLAGS = ${LDFLAGS}"
	@echo "CC = ${CC}"

