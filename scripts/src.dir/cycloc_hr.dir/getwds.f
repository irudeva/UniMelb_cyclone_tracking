      Subroutine getwds(line,len,nwdlt,wd,nwd,ie)

      character line*160,wd(nwdlt)*(*)

      if (nwdlt.gt.40) then
          write (6,*) 'Too many words in array.'
          ie = 3
          return
      endif
      if (len.gt.160) then
          write (6,*) 'Line too long.'
          ie = 3
          return
      endif

      do 10 nwd = 1,nwdlt
          wd(nwd) = ' '
 10   continue

          ifirst = 1
          nwd = 0
      do 20 i = 1,len
          if ((line(i:i).eq.' ').or.(ichar(line(i:i)).eq.9)
     *     .or.(i.eq.161)) then
              ilast = i-1
          if (ilast.ge.ifirst) then
              nwd = nwd+1
              wd(nwd) = line(ifirst:ilast)
              if (nwd.eq.nwdlt) return
          endif
              ifirst = i + 1
          endif
 20   continue

      return
      end
