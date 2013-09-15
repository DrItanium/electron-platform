########### MAKEFILE FOR ELECTRON ###########
CC := cc
LD := cc
OUTPUT = electron
DESTDIR ?= $(ELECTRON_PLATFORM_ROOT)/sys
BINDIR ?= bin
INCS ?= -I${ELECTRON_PLATFORM_ROOT}/sys/include/
LIBDIR ?= lib

OBJS = agenda.o analysis.o argacces.o bload.o bmathfun.o bsave.o \
 	classcom.o classexm.o classfun.o classinf.o classini.o \
	classpsr.o clsltpsr.o commline.o conscomp.o constrct.o \
 	constrnt.o crstrtgy.o cstrcbin.o cstrccom.o cstrcpsr.o \
 	cstrnbin.o cstrnchk.o cstrncmp.o cstrnops.o cstrnpsr.o \
 	cstrnutl.o default.o defins.o developr.o dffctbin.o dffctbsc.o \
 	dffctcmp.o dffctdef.o dffctpsr.o dffnxbin.o dffnxcmp.o \
	dffnxexe.o dffnxfun.o dffnxpsr.o dfinsbin.o dfinscmp.o drive.o \
	emathfun.o engine.o envrnmnt.o evaluatn.o expressn.o exprnbin.o exprnops.o \
 	exprnpsr.o extnfunc.o factbin.o factbld.o factcmp.o factcom.o \
 	factfun.o factgen.o facthsh.o factlhs.o factmch.o factmngr.o \
 	factprt.o factqpsr.o factqury.o factrete.o factrhs.o filecom.o \
 	filertr.o generate.o genrcbin.o genrccmp.o genrccom.o genrcexe.o \
 	genrcfun.o genrcpsr.o globlbin.o globlbsc.o globlcmp.o globlcom.o \
 	globldef.o globlpsr.o immthpsr.o incrrset.o inherpsr.o \
 	inscom.o insfile.o insfun.o insmngr.o insmoddp.o insmult.o \
 	inspsr.o insquery.o insqypsr.o iofun.o lgcldpnd.o \
 	memalloc.o miscfun.o modulbin.o modulbsc.o modulcmp.o moduldef.o \
 	modulpsr.o modulutl.o msgcom.o msgfun.o msgpass.o msgpsr.o \
 	multifld.o multifun.o objbin.o objcmp.o objrtbin.o objrtbld.o \
 	objrtcmp.o objrtfnx.o objrtgen.o objrtmch.o parsefun.o pattern.o \
 	pprint.o prccode.o prcdrfun.o prcdrpsr.o prdctfun.o prntutil.o \
 	proflfun.o reorder.o reteutil.o retract.o router.o rulebin.o \
 	rulebld.o rulebsc.o rulecmp.o rulecom.o rulecstr.o ruledef.o \
 	ruledlt.o rulelhs.o rulepsr.o scanner.o sortfun.o strngfun.o \
 	strngrtr.o symblbin.o symblcmp.o symbol.o sysdep.o textpro.o \
 	tmpltbin.o tmpltbsc.o tmpltcmp.o tmpltdef.o tmpltfun.o tmpltlhs.o \
 	tmpltpsr.o tmpltrhs.o tmpltutl.o userdata.o userfunctions.o \
 	utility.o watch.o binops.o arch.o shellvar.o 

.PHONY: clean all

all: libraries

libraries: $(OBJS)
	@echo Building lib$(OUTPUT).a
	@$(AR) rcs lib$(OUTPUT).a $(OBJS)
	@echo Building lib$(OUTPUT).so
	@$(LD) $(LDFLAGS) -shared -o lib$(OUTPUT).so $(OBJS) -lm -lrt

install:
	@echo Installing libraries to $(DESTDIR)/$(LIBDIR)
	@mkdir -p $(DESTDIR)/$(LIBDIR)
	@cp lib$(OUTPUT).so $(DESTDIR)/$(LIBDIR)
	@cp lib$(OUTPUT).a $(DESTDIR)/$(LIBDIR)

deinstall uninstall:
	@echo Uninstalling...
	@rm -f $(DESTDIR)/$(LIBDIR)/lib$(OUTPUT).so
	@rm -f $(DESTDIR)/$(LIBDIR)/lib$(OUTPUT).a


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


