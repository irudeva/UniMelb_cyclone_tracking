      Subroutine splithist(junit)

c       This routine writes separate component history file
c     from a single concatenated track history files.

c     Written by Ross Murray.
c     Last revised 10th Aug., 1995.

c-----------------------------------------------------------------------
c     Declarations
c-----------------------------------------------------------------------

#include "cyc1.h"
#include "trk1.h"
#include "trk2.h"
#include "trk3.h"

c     Local declarations
c     ------------------

      integer trk,trkexp
      integer statf,statl
      character trform*11

c-----------------------------------------------------------------------
c     Read and rewrite files
c-----------------------------------------------------------------------

      trform = 'unformatted'
      if (trfmin) trform = 'formatted'

c     Write content file
c     ------------------

      write (6,*) ' Copying to thist1.1'
      open (unit=21,file='thist1.1',status='new',form=trform,err=960)

      call tr1rd(junit,trfmin,ierr)
      if (ierr.eq.1) go to 970
      if (ierr.eq.2) go to 980
      if (ierr.eq.3) stop ' splithist: Not a thist1 type file.'

      call tr1wr(21,trfmin)
      call tr1wr(6,.true.)
      close (21)

c     Write track files
c     -----------------

      write (6,*) ' Opening thist2.1'
      if (lstrk1.gt.0) open (22,file='thist2.1',status='unknown',
     * form=trform,err=960)
      write (6,*) ' Opening thist3.1'
      if (lstrk2.gt.lstrk1) open (23,file='thist3.1',status='unknown',
     * form=trform,err=960)

      write (6,*) ' Copying to thist2.1'
      ihunit = 22
      do 780 trkexp =  1,99999
        call tr2rd(21,trfmin,trk,statf,statl,ifst,ilst,nit,
     *   idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z,ierr)
        if (trkexp.eq.1) call trtabhd(itabt3z,itabt4z)
        if (ierr.eq.1) go to 970
        if (ierr.eq.2) go to 980
        if (ierr.eq.3) stop ' splithist: Not a thist2/3 type file.'
        if (trk.ne.trkexp) then
          write (6,'('' Track read is not '',i5,
     *     '' but '',i5,''.''/'' file.'')') trk,trkexp
          stop ' splithist'
        endif
        if (trk.gt.lstrk2) then
          write (6,'(/'' Last track in file ('',i5,
     *     '') > expected (''i5,'').''/'' file.'')') trk,lstrk2
          stop ' splithist'
        endif
        if (trk.gt.lstrk1) then
          close (22)
          ihunit = 23
          write (6,*) ' Copying to thist3.1'
        endif

        call tr3rd(junit,trfmin,.false.,itabt3z,itabt4z,nvarlt,nitlt,
     * nit,vart,ierr)
        if (ierr.eq.1) go to 970
        if (ierr.eq.2) go to 980
        call tr2wr(ihunit,trfmin,trk,statf,statl,ifst,ilst,nit,
     *   idafst,ihrfst,idalst,ihrlst,itabt3z,itabt4z)
        call tr3wr(ihunit,trfmin,itabt3z,itabt4z,nit)
 780  continue
      close (23)

      return

c-----------------------------------------------------------------------
c     Errors
c-----------------------------------------------------------------------

 960  stop 
     * ' splithist: Error in opening concatenated track history file.'
 970  stop 
     * ' splithist: End of concatenated track history file, track data.'
 980  stop 
     * ' splithist: Error in concatenated reading track history file.'
 990  stop 
     * ' splithist: Not a track history file.'
      end
