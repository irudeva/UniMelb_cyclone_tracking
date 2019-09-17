      integer	UT_EOF, UT_ENOFILE, UT_ESYNTAX, UT_EUNKNOWN
      integer	UT_EIO, UT_EINVALID, UT_ENOINIT, UT_ECONVERT
      integer	UT_EALLOC, UT_ENOROOM, UT_ENOTTIME

      parameter (UT_EOF		= 1)
      parameter	(UT_ENOFILE	= -1)
      parameter (UT_ESYNTAX	= -2)
      parameter (UT_EUNKNOWN	= -3)
      parameter (UT_EIO		= -4)
      parameter (UT_EINVALID	= -5)
      parameter (UT_ENOINIT	= -6)
      parameter (UT_ECONVERT	= -7)
      parameter (UT_EALLOC	= -8)
      parameter (UT_ENOROOM	= -9)
      parameter (UT_ENOTTIME	= -10)

      integer	UT_MAXNUM_BASE_QUANTITIES
      parameter	(UT_MAXNUM_BASE_QUANTITIES = 10)

C The FORTRAN API:
C
C NB: `PTR' in the following stands for whatever FORTRAN type is
C appropriate for storing a pointer to a structure.

C #define integer*8	integer*8

C
C   Initialize the units package:
      integer utopen
C	(character*(*) fpath)

C   Create a new unit:
      integer*8 utmake
C	()

C   Is a unit a temporal one?
      integer uttime
C	(integer*8 unit)

C   Indicate whether or not a unit has a non-zero origin (0=>no, 1=>yes).
      integer utorigin
C	  (integer*8 unit)

C   Clear a unit:
C     utclr
C	(integer*8 unit)

C   Copy a unit:
C     utcpy
C	(integer*8 source,
C	integer*8 dest)

C   Shift the origin of a unit:
C     utorig
C	(integer*8 source, 
C	doubleprecision amount, 
C	integer*8 result)

C   Scale a unit:
C     utscal
C	(integer*8 source, 
C	doubleprecision factor, 
C	integer*8 result)

C   Multiply two units:
C     utmult
C	(integer*8 term1,
C	integer*8 term2, 
C	integer*8 result)

C   Invert a unit:
C     utinv
C	(integer*8 source,
C	integer*8 result)

C   Divide one unit by another:
C     utdiv
C	(integer*8 numer,
C	integer*8 denom, 
C	integer*8 result)

C   Raise a unit to a power:
C     utexp
C	(integer*8 source,
C	integer*8 power, 
C	integer*8 result)

C   Decode a formatted specification into a unit:
      integer utdec
C	(character*(*) fspec,
C	integer*8 unit)

C   Convert a temporal value into a UTC Gregorian date and time:
      integer utcaltime
C	(doubleprecision    value,
C	integer*8 unit,
C	integer year
C	integer month,
C	integer day,
C	integer hour,
C	integer minute,
C	real    second)

C   Convert a Gregorian/Julian date and time into a temporal value:
      integer uticaltime
C       (integer year,
C       integer  month,
C       integer  day,
C       integer  hour,
C       integer  minute,
C       real     second,
C       integer*8      unit,
C       doubleprecision value)

C   Encode a unit into a formatted specification:
      integer utenc
C	(integer*8 unit,
C	char fspec)

C   Convert from one unit to another:
      integer utcvt
C	(integer*8 from,
C	integer*8 to, 
C	doubleprecision slope, 
C	doubleprecision intercept)

C   Free a unit thingy created by utmake():
C     utfree
C	(integer*8 unit)

C   Close the units package:
C     utcls
C	()
