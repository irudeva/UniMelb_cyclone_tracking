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
C NB: `PTR in the following stands for whatever FORTRAN type is
C appropriate for storing a pointer to a structure.  Because this is
C necessarily platform-dependent, IT IS UP TO THE USER TO DECLARE AND USE
C THE CORRECT TYPE.


C
C   Initialize the units package:
      integer utopen
C	(character*(*) fpath)

C   Create a new unit:
ckk      integer utmake
C	()

C   Is a unit a temporal one?
ckk      integer uttime
C	(integer unit)

C   Indicate whether or not a unit has a non-zero origin (0=>no, 1=>yes).
      integer utorigin
C	  (integer unit)

C   Clear a unit:
C     utclr
C	(integer unit)

C   Copy a unit:
C     utcpy
C	(integer source,
C	integer dest)

C   Shift the origin of a unit:
C     utorig
C	(integer source, 
C	doubleprecision amount, 
C	integer result)

C   Scale a unit:
C     utscal
C	(integer source, 
C	doubleprecision factor, 
C	integer result)

C   Multiply two units:
C     utmult
C	(integer term1,
C	integer term2, 
C	integer result)

C   Invert a unit:
C     utinv
C	(integer source,
C	integer result)

C   Divide one unit by another:
C     utdiv
C	(integer numer,
C	integer denom, 
C	integer result)

C   Raise a unit to a power:
C     utexp
C	(integer source,
C	integer power, 
C	integer result)

C   Decode a formatted specification into a unit:
ckk      integer utdec
C	(character*(*) fspec,
C	integer unit)

C   Convert a temporal value into a UTC Gregorian date and time:
ckk      integer utcaltime
C	(real    value,
C	integer unit,
C	integer year
C	integer month,
C	integer day,
C	integer hour,
C	integer minute,
C	real    second)

C   Convert a Gregorian/Julian date and time into a temporal value:
ckk      integer uticaltime
C       (integer year,
C       integer  month,
C       integer  day,
C       integer  hour,
C       integer  minute,
C       real     second,
C       integer      unit,
C       doubleprecision value)

C   Encode a unit into a formatted specification:
      integer utenc
C	(integer unit,
C	char fspec)

C   Convert from one unit to another:
      integer utcvt
C	(integer from,
C	integer to, 
C	doubleprecision slope, 
C	doubleprecision intercept)

C   Free a unit thingy created by utmake():
C     utfree
C	(integer unit)

C   Close the units package:
C     utcls
C	()
