# efssrv version
VERSION = 9

# Customize below to fit your needs

# paths
PREFIX = ${ElectronFSRoot}/sys

# includes and libs
INCS = -I. -I${PREFIX}/include -I/usr/include
LIBS = -L${PREFIX}/lib -lelectron -L/usr/lib -lc -lm -lrt

CFLAGS = -O2 ${INCS} -DVERSION=\"${VERSION}\"
LDFLAGS = ${LIBS}

# compiler and linker
CC ?= cc
LD ?= ld
