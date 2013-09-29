# efssrv version
VERSION = 9

# Customize below to fit your needs

# paths
PREFIX = ${ELECTRON_PLATFORM_ROOT}/sys

include ${PREFIX}/src/common/config.mk

# includes and libs
INCS = -I. -I${PREFIX}/include -I/usr/include
LIBS = -L${PREFIX}/lib -lelectron -L/usr/lib -lc -lm -lrt

DEFS += -DVERSION=\"${VERSION}\"
CFLAGS = -Os ${INCS} ${DEFS}
LDFLAGS = ${LIBS}

# compiler and linker
CC ?= cc
LD ?= ld
