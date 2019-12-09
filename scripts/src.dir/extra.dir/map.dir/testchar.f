      character a*1
      call getarg(1,a)
      i = ichar(a)
      ii = 0
      if (i.ge.65 .and. i.le.90) ii = i - 38 ! = 64 + 26
      if (i.ge.97 .and. i.le.122) ii = i - 96
      if (i.ge.65 .and. i.le.90) iii = i + 32 
      if (i.ge.97 .and. i.le.122) iii = i - 32
      write (6,*) i
      write (6,*) ii
      write (6,*) iii
      write (6,*) char(iii)
