# Config.mk for electron

PREFIX = ${ELECTRON_PLATFORM_ROOT}/sys

include ${PREFIX}/src/common/config.mk

INCS = -I. -I${PREFIX}/include -I/usr/include
LIBS = -L/usr/lib -lc -lm -lrt

CFLAGS = -Os ${INCS} ${DEFS}
LDFLAGS = ${LIBS}

CC ?= cc
LD ?= cc
