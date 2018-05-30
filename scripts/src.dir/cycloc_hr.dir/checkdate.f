      Subroutine checkdate(istrt2,da,hr,dalast,hrlast,dmode,
     * ddhmmn,ddhmmx,dastrt,hrstrt,dastop,hrstop,ie)

c         This subroutine checks whether the date and time of a data file
c     (a)  fall within a specified period,
c     (b)  follow the previous date and time sequentially, and to within 
c            specified limits.

c     Last revised 18 Feb 1991,  R.J. Murray.

c-------------------------------------------------------------------------------
c       (1)  Explanation.
c-------------------------------------------------------------------------------

c     dmode            Mode for interpretation of dates and times, e.g.,
c                        ymdhm  year,month,day,hour,minute (yymmddhhmm)
c                        ydd    year,day(1-365) (yydddd)
c                        ddddd  day(d),decimals of day(D) (ddddddDDDD)
c     da,hr            day/date and time of day (hours,mins/decimal day)
c     dalast,hrlast    day/date and time of day of last data record used
c     dastrt,hrstrt    Starting date and time
c     dastop,hrstop    Finishing date and time
c     ddhmmn           Minimum time interval between successive data fields,
c                        in days,hours,minutes (ddhhmm)
c     ddhmmx           Maximum time interval between successive data fields

c         If a record have already been used (when istrt is changed from 0 to 1)
c     the routine, timedif, calculates the interval between present and 
c     previous date/time.

c     idmin            Interval in mins.
c     ddhm             Interval in (whole) days, hours, and minutes (ddhhmm)
c     dday             Interval in days (decimal)

c     This interval must not be less than ddhmmn and not greater than ddhmmx
c     (unless ddhmmx = 0).

c-------------------------------------------------------------------------------
c       (2)  Declarations.
c-------------------------------------------------------------------------------

      integer da,hr,dalast,hrlast,dastrt,hrstrt,dastop,hrstop
      integer ddhm,ddhmmn,ddhmmx
      character dmode*6

      ie = 0

c-------------------------------------------------------------------------------
c       (3)  Checking date within required period.
c-------------------------------------------------------------------------------


      if ((da.lt.dastrt).or.((da.eq.dastrt).and.(hr.lt.hrstrt))) then
          ie = 1
          return
      endif

      if (((dastop.gt.0).or.(hrstop.gt.0)).and.((da.gt.dastop).or.
     * ((da.eq.dastop).and.(hr.gt.hrstop)))) stop

      if (istrt2.eq.0) then
          istrt2 = 1
          return
      endif

c-------------------------------------------------------------------------------
c       (4)  Checking date/time within a specified interval of previous
c             date/time.
c-------------------------------------------------------------------------------

      call timedif(dalast,hrlast,da,hr,dmode,idmin,ddhm,dday,itderr)

      if (ddhm.lt.0) then
          write (6,*) ' Data periods out of order (',dalast,hrlast,
     *     ' followed by ',da,hr,').'
          stop
      else if (ddhm.eq.0) then
          write (6,*) ' Two copies of data for same day/time.',
     *     da,hr,'. Skipping to next data period.'
          ie = 1
      else if (ddhm.lt.ddhmmn) then
          ie = 1
      else if ((ddhmmx.ne.0).and.(ddhm.gt.ddhmmx)) then
          write (6,*) ' Spacing of data periods too great: ',
     *     dalast,hrlast,' to ',da,hr,'.'
          stop
      endif

      return
      end
