# Config.mk for electron

PREFIX = ${ELECTRON_PLATFORM_ROOT}/sys

INCS = -I. -I${PREFIX}/include -I/usr/include
LIBS = -L/usr/lib -lc -lm -lrt

CFLAGS = -Os ${INCS} 
LDFLAGS = ${LIBS}

CC ?= cc
LD ?= cc
