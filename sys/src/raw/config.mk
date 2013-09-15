# raw version
VERSION = 9

# Customize below to fit your needs

# paths
PREFIX = ${ELECTRON_PLATFORM_ROOT}/sys

# includes and libs
INCS = -I. -I${PREFIX}/include -I/usr/include 
LIBS = -L/usr/lib -lc -lm -lrt ${PREFIX}/lib/libelectron.a

CFLAGS = -Os ${INCS} -DVERSION=\"${VERSION}\" 
LDFLAGS = ${LIBS}

# compiler and linker
CC ?= cc
LD ?= ld
