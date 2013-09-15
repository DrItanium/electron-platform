# Electron - tool for writing expert systems (a fork of CLIPS)
# original code created by NASA's AI Lab 
# changes copyright 2012-2013 Joshua Scoggins

include config.mk
SRC = agenda.c analysis.c argacces.c bload.c bmathfun.c bsave.c \
 	classcom.c classexm.c classfun.c classinf.c classini.c \
	classpsr.c clsltpsr.c commline.c conscomp.c constrct.c \
 	constrnt.c crstrtgy.c cstrcbin.c cstrccom.c cstrcpsr.c \
 	cstrnbin.c cstrnchk.c cstrncmp.c cstrnops.c cstrnpsr.c \
 	cstrnutl.c default.c defins.c developr.c dffctbin.c dffctbsc.c \
 	dffctcmp.c dffctdef.c dffctpsr.c dffnxbin.c dffnxcmp.c \
	dffnxexe.c dffnxfun.c dffnxpsr.c dfinsbin.c dfinscmp.c drive.c \
	emathfun.c engine.c envrnmnt.c evaluatn.c expressn.c exprnbin.c exprnops.c \
 	exprnpsr.c extnfunc.c factbin.c factbld.c factcmp.c factcom.c \
 	factfun.c factgen.c facthsh.c factlhs.c factmch.c factmngr.c \
 	factprt.c factqpsr.c factqury.c factrete.c factrhs.c filecom.c \
 	filertr.c generate.c genrcbin.c genrccmp.c genrccom.c genrcexe.c \
 	genrcfun.c genrcpsr.c globlbin.c globlbsc.c globlcmp.c globlcom.c \
 	globldef.c globlpsr.c immthpsr.c incrrset.c inherpsr.c \
 	inscom.c insfile.c insfun.c insmngr.c insmoddp.c insmult.c \
 	inspsr.c insquery.c insqypsr.c iofun.c lgcldpnd.c \
 	memalloc.c miscfun.c modulbin.c modulbsc.c modulcmp.c moduldef.c \
 	modulpsr.c modulutl.c msgcom.c msgfun.c msgpass.c msgpsr.c \
 	multifld.c multifun.c objbin.c objcmp.c objrtbin.c objrtbld.c \
 	objrtcmp.c objrtfnx.c objrtgen.c objrtmch.c parsefun.c pattern.c \
 	pprint.c prccode.c prcdrfun.c prcdrpsr.c prdctfun.c prntutil.c \
 	proflfun.c reorder.c reteutil.c retract.c router.c rulebin.c \
 	rulebld.c rulebsc.c rulecmp.c rulecom.c rulecstr.c ruledef.c \
 	ruledlt.c rulelhs.c rulepsr.c scanner.c sortfun.c strngfun.c \
 	strngrtr.c symblbin.c symblcmp.c symbol.c sysdep.c textpro.c \
 	tmpltbin.c tmpltbsc.c tmpltcmp.c tmpltdef.c tmpltfun.c tmpltlhs.c \
 	tmpltpsr.c tmpltrhs.c tmpltutl.c userdata.c userfunctions.c \
 	utility.c watch.c binops.c arch.c shellvar.c 
OBJ = ${SRC:.c=.o}

.PHONY: clean all

all: libraries

libraries: ${OBJ}
	@echo Building libelectron.a
	@${AR} rcs libelectron.a ${OBJ}
	@echo Building libelectron.so
	@${LD} ${LDFLAGS} -shared -o libelectron.so ${OBJ} -lm -lrt

install:
	@echo Installing libraries to ${PREFIX}/lib
	@mkdir -p ${PREFIX}/lib
	@cp libelectron.so ${PREFIX}/lib
	@cp libelectron.a ${PREFIX}/lib

deinstall uninstall:
	@echo Uninstalling...
	@rm -f ${PREFIX}/lib/libelectron.so
	@rm -f ${PREFIX}/lib/libelectron.a


clean: 
	@echo Cleaning
	@rm -f *.o
	@rm -f libelectron.so
	@rm -f libelectron.a


.c.o :
	@echo CC $<
	@$(CC) -c $(CFLAGS) -D_POSIX_C_SOURCE=200112L \
		-std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Wmissing-prototypes -Wnested-externs -Wstrict-prototypes \
	    -Waggregate-return -Wno-implicit -fPIC ${INCS} $<


