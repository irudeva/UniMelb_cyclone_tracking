# Notes
# (1) 13/11/2002: The following change was made to allow for skipped points:
# parameter (nisglt=120,njsglt=621)  ! Was 600 - allow for 5 skipped points
# (2) 11/10/2004: Turn off array bounds checking - problem with g77.
# (3) 13/5/2005: Use f77 (Sun)
# (4) 8/9/2006: Now nisglt=200,njsglt=1021 

#FC = g77
FC = gfortran
FFLAGSOPT = -O3
FFLAGSDEBUG = -g -fbacktrace -fbounds-check
FFLAGS = $(FFLAGSDEBUG) -c

SRCS.TSTATA_5 = tstat_5.F tstatwrite_5.F 

SRCS.TSTATB_1 = tstatzone.f llexpand.f bisplcf.f isplcf.f meanij.f cspline.f trackioa_2.f timedif.o ngtopt.o globtest.o psconv.o

OBJS.TSTAT_5 = ${SRCS.TSTATA_5:.F=.o} ${SRCS.TSTATB_1:.f=.o}

tstat: cpinclude5 depend.tstat5 $(OBJS.TSTAT_5)
	$(FC) $(OBJS.TSTAT_5) -o ../../cycbin/tstatx.hr

.F.o:
	$(FC) ${FFLAGS} $<
.f.o:
	$(FC) ${FFLAGS} $<

cpinclude5:
	cp -p tstat1_5.h tstat1.h

depend.tstat5:
	makedepend  ${SRCS.TSTATA_5} -f Makefile

clean:
	-rm *.o $(objects)

# DO NOT DELETE THIS LINE -- make depend depends on it.

tstat_5.o: tstat1.h
tstatwrite_5.o: tstat1.h
