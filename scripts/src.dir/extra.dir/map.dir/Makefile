#
# The g77 (== f77 here) flag: -fugly-logint allows logical and integer variables to 
# be interchanged - common in Sun Fortran but messy
# Kevin Keay 11/10/2003
 
FC = g77
FFLAGS = -c -fugly-logint

BINDIR = ../../cycbin

OBJS1 = qmapread.o qmapwrite.o getnos.o 
OBJS2 = globtest.o lintxy1.o findi1.o matrix.o checknos.o ngtopt.o map.o getwds1.o

map: $(OBJS1) $(OBJS2)
	$(FC) $(OBJS1) $(OBJS2) -o $(BINDIR)/mapx
