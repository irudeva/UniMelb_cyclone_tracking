      parameter (nisglt=200,njsglt=1021)  ! 1021= 200*5 + 4*5 +1 Allow for 5 skipped points during interpolation KK 8/9/2006
ckk      parameter (nisglt=120,njsglt=621)  ! Was 600 - allow for 5 skipped points
      parameter (nlonlt=1441,nlatlt=721,nbuflt=nlonlt*nlatlt) !KK 30/10/2005
      parameter (lsmax=45)
      parameter (nfun=40)
      parameter (rad=57.295779,rrad=1./rad,r2rad=1./(2.*rad))
      parameter (pi=3.1415927)
      parameter (nilt=721,njlt=721)  !KK 30/10/2005
      parameter (ni=721,nj=721) !KK 30/10/2005 
      parameter (icen=(ni+1)/2,jcen=(nj+1)/2)
      parameter (xcen=icen,ycen=jcen)
      parameter (rproj=120.) !Was 120 but orginally 60
      parameter (niwt=121,njwt=121) !Was 61 KK 4/1/2006
      parameter (spval=99999.9)

      character shead*960,llhead*80
      character fname(nfun)*22,funit(nfun)*15,fn(nfun)*6
      character funitx*15,fnx*6
      dimension finc(nfun)
      logical llwrit

      common /blhead/shead,llhead
      common /blfn/fname,funit,fn
      common /blilts/llwrit,fhem,nlons,nlats,idiagt
      common /blfinc/finc

