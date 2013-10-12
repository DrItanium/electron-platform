# chicanery version
VERSION = 9


# paths
PREFIX = ${ELECTRON_PLATFORM_ROOT}/sys

include ${PREFIX}/src/common/config.mk

DEFS += -DVERSION=\"${VERSION}\"

# includes and libs
INCS = -I. -I${PREFIX}/include -I/usr/include 
LIBS = -L/usr/lib -lc -lm -lrt ${PREFIX}/lib/libefssrv.a ${PREFIX}/lib/libelectron.a

CFLAGS = -Os ${INCS} ${DEFS}
LDFLAGS = ${LIBS}

# compiler and linker
CC ?= cc
LD ?= ld
