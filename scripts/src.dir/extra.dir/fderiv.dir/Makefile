#
# The g77 (== f77 here) flag: -fugly-logint allows logical and integer variables to 
# be interchanged - common in Sun Fortran but messy
# Kevin Keay 11/10/2003
# 
# Notes
#  (1) wrapll.f: Changed amax0 to max
#  (2) fderiv.f: Removed int from line 121 nsels = ...
#      Changed / to // on line 135
#
 
FC = g77
FFLAGS = -c -fugly-logint

BINDIR = ../../cycbin

OBJS.FD = fderiv.o llmaprd.o llexpand2.o wrapll.o splseg.o bisplsv.o isplcf.o ngtopt.o
fderiv:	$(OBJS.FD)
	$(FC) $(OBJS.FD) -o $(BINDIR)/fderivx

