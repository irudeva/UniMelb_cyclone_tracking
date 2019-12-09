      function ngtopt (list, i, arg)
! version 1.3 90/11/16 12:56:40
c Written by Harvey Davies, CSIRO Division of Atmospheric Research,
c   Aspendale, Victoria
! Get next command-line option. Similar to sh command getopts

! If there are no more options then ngtopt is set to -1. 
! At this stage the value of
! i can be used to access any further arguments. Note that any further calls to
! clopt result in another pass, restarting again at the 1st argument.

! arguments 

! Arguments:
      character*(*) list ! list of valid option letters. Colon (:) indicates
!			   that preceding option takes an argument (input)
      character*1 letter ! letter corresponding to next option (output)
      character*(*) arg ! option-argument corresponding to next option
!			  error message if ngtopt > 1 (output)
      integer i ! subscript of next command-line argument (output)
      integer ngtopt ! exit status (>0 = ichar(letter),
!                                   -1 = end, 0 = errror) (output)

! Other variables:
      character*255 cla ! current command-line argument
      integer       ia ! subscript of current command-line argument
      integer       ind ! character position within list
      integer       k ! character position within current command-line argument
      integer       lcla ! length of cla excluding trailing spaces
      logical       optarg ! true iff option takes an argument
      save

! Functions:
      integer iargc ! function giving arg count

      data ia / 0 /

      letter = '?'
      arg = ' '
      ngtopt = ichar(letter)
      i = ia + 1
      if ( ia .eq. 0 ) then
	k = 0
	lcla = 0
      end if
      k = k + 1
      if ( k .gt. lcla ) then
	ia = ia + 1
	if ( ia .gt. iargc() ) then
	  ngtopt = -1
	else
	  call getarg( ia, cla )
	  if ( cla(1:1) .ne. '-' ) then
	    ngtopt = -1
	  else
	    i = ia + 1
	    if ( cla(2:2) .eq. '-' ) ngtopt = -1
	  end if
	end if
	lcla = len( cla )
	do while ( lcla .gt. 1  .and.  cla(lcla:lcla) .eq. ' ' )
	  lcla = lcla - 1
	end do
	k = 2
      end if
      if (ngtopt .ge. 1) then
	ind = index( list, cla(k:k) )
	if ( ind .eq. 0 ) then
	  ngtopt = 0
	  arg = 'illegal option -' // cla(k:k)
	  return
	else if ( ind .lt. len(list) ) then
	  optarg = list(ind+1:ind+1) .eq. ':'
	else
	  optarg = .false.
	end if
	if ( optarg ) then
	  if ( k .lt. lcla ) then
	    arg = cla(k+1:lcla)  
	  else
	    ia = ia + 1
	    if ( ia .gt. iargc() ) then
	      ngtopt = 0
	      arg = 'missing argument -' // list(ind:ind)
	      return
	    end if
	    i = ia + 1
	    call getarg( ia, arg )
	  end if
	  k = 0
	  lcla = 0
	end if
	letter = list(ind:ind)
        ngtopt = ichar(letter)
      else
	ia = 0
      end if

      end
