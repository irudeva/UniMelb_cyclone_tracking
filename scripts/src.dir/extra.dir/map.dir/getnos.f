      Subroutine getnos(line,nomin,nomax,nno,no,n,ie)

      dimension no(nno)
      character line*80,word*80

          ie = 0
c     if (nno.gt.40) then
c         write (6,*) 'Too many numbers in array.'
c         ie = 3
c         return
c     endif

      do 10 i = 1,nno
          no(i) = 0
 10   continue

          ifirst = 1
          n = 0
      do 20 i = 1,81
          if ((line(i:i).eq.' ').or.(i.eq.81)) then
              ilast = i-1
          if (ilast.ge.ifirst) then
              word = line(ifirst:ilast)
              read (word,*,err=30) number
          if ((number.ge.nomin).and.(number.le.nomax)) then
              if (n.eq.nno) then
                  write (6,*) ' Only first ',n,' nos. returned.'
                  ie = 1
                  return
              endif
              n = n+1
              no(n) = number
          else
              write (6,*) number,' not allowable: skipping.'
          endif
          endif
              ifirst = i + 1
          endif
 20   continue
      return
 30   continue
      write (6,*) 'Non integer characters recorded;'
      write (6,*) line
      ie = 2
      return

      end
