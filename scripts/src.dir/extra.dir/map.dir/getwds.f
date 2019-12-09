      Subroutine getwds(line,nwdlt,wd,nwd,ie)

      character line*80
      character*30 wd(nwdlt)

      if (nwdlt.gt.40) then
          write (6,*) 'Too many words in array.'
          ie = 3
          return
      endif

      do 10 nwd = 1,nwdlt
          wd(nwd) = ' '
 10   continue

          ifirst = 1
          nwd = 0
      do 20 i = 1,81
          if ((line(i:i).eq.' ').or.(i.eq.81)) then
              ilast = i-1
          if (ilast.ge.ifirst) then
              nwd = nwd+1
              wd(nwd) = line(ifirst:ilast)
c             write (0,*) 'nwd=',nwd,' ',wd(nwd)
              if (nwd.eq.nwdlt) return
          endif
              ifirst = i + 1
          endif
 20   continue

      return
      end
