      Subroutine matrix (nunit,array,irdim,istrt,im,jstrt,jm,scale,
     * spval)

c       This subroutine is a general two-dimensional array printing 
c     routine only slightly modified from the version used by the
c     GFDL ocean model.

c     array = the array to be printed
c     irdim = the 1st dimension of array
c     istrt = the 1st element of the 1st dimension to be printed
c     im    = the last element of the 1st dimension to be printed
c     jstrt = the 1st element of the 2nd dimension to be printed
c     jm    = the last element of the 2nd dimension to be printed
c             the 2nd dimension is printed in reverse order if both
c             jstrt & jm are negative
c     scale = a scaling factor by which array is divided before
c             printing.  (if this is zero, no scaling is done.)
c             if scale=0, 10 columns are printed across in e format
c             if scale>0, 20 columns are printed across in f format

c-----------------------------------------------------------------------

      dimension array(irdim,1000),aline(20)

      if (jstrt*jm .lt. 0) then
        write (0,999)  jstrt, jm
        stop '=>matrix'
      endif

c     allow for inversion of 2nd dimension

      if (jm .lt. 0) then
        js   = -jm
        je   = -jstrt
        jinc = -1
      else
        js   = jstrt
        je   = jm
        jinc = 1
      endif

      if (scale .eq. 0.) then
        do 100 is=istrt,im,10
          ie = min(is + 9,im)
          write (nunit,9001) (i, i=is,ie)
          do 90 l=js,je,jinc
            write (nunit,9002) l, (array(i,l),i=is,ie)
90        continue
100     continue
      else
        scaler = 1.0/scale
        do 200 is=istrt,im,15
          ie = min(is + 14,im)
          write (nunit,9003) (i, i=is,ie)
          iirange = ie - is + 1
          do 190 l=js,je,jinc
            ii = 0
            do 180 i=is,ie
              ii = ii + 1
              if (array(i,l).eq.spval) then
                aline(ii) = spval
              else
                aline(ii) = array(i,l)*scaler
              endif
180         continue
            write (nunit,9004) l, (aline(ii),ii=1,iirange)
190       continue
200     continue
      endif

      return

999   format (1x,'jstrt=',i5,' jm=',i5,' in matrix')
9001  format(/10i12/)
9002  format(1x,i2,10(1pe12.4))
9003  format(/3x,20i8/)
9004  format(1x,i3,1x,20f8.2)

      end
