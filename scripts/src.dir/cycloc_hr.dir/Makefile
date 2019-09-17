# MAKEFILE FOR PROGRAMMES FOR PS CONVERSION & TRACKING CYCLONES
 
# psfmx     writes data from an unformatted PS file to ascii form
# cyclocx   converts to PS and/or finds cyclones

# At present, disable array bounds checking since there is
# a glitch with g77

FC = gfortran
FFLAGSOPT = -O3
FFLAGSDEBUG = -g -fbacktrace -fbounds-check
FFLAGS = $(FFLAGSDEBUG)


#

SRCS.GP = intgpr.f llmaprd.f globtest.f llexpand.f splseg.f bisplsv.f \
           isplcf.f intpgr.f lint.f
SRCS.CF10= cycloc_3.F cycp_2.F fmin.F frmin.F cycio_1.F \
          sint.F sdiv.F cvave_2.F steer1_2.F steer2.F pssmth_2.F supvar.F
SRCS.CS = psio.f rsplcf.f brsplsv.f checkdate.f timedif.f psconv.f ngtopt.f \
           srchls.f getwds.f lnblnk.f
SRCS.EX = matrix.f psfm.f

OBJS.CL10 = ${SRCS.CF10:.F=.o} ${SRCS.CS:.f=.o} ${SRCS.GP:.f=.o}

OBJS.FM = ${SRCS.EX:.F=.o} psio.o ngtopt.o

# Debugging only
#cycloc10: depend.cl10 $(OBJS.CL10) 
#	g77 -g -fugly -funroll-loops -fsource-case-lower $(OBJS.CL10) trapfpe.o  -o ../cycbin/cycloc10x

cycloc: depend.cl10 $(OBJS.CL10) 
	$(FC) -O $(OBJS.CL10)  -o ../../cycbin/cycloc3x.hr

psfm:   $(OBJS.FM) 
	$(FC) $? -o ../../cycbin/psfmx

depend.cl10:
	makedepend  ${SRCS.CF10} -f Makefile

clean:
	-rm *.o $(objects)

# DO NOT DELETE THIS LINE -- make depend depends on it.

cycloc_3.o: cyc1.h cyc2.h cyc3.h cyc4.h cyc5.h
cycp_2.o: cyc1.h cyc2.h cyc3.h cyc4.h
fmin.o: cyc6.h
frmin.o: cyc6.h
cycio_1.o: cyc1.h cyc2.h
sint.o: cyc6.h
sdiv.o: cyc3.h cyc5.h cyc6.h
cvave_2.o: cyc3.h
steer1_2.o: cyc1.h cyc3.h
steer2.o: cyc1.h cyc2.h cyc4.h cyc5.h
pssmth_2.o: cyc3.h
supvar.o: cyc1.h cyc2.h cyc4.h cyc5.h
