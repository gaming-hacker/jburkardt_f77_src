      program main

c***********************************************************************
c
cc MAIN is the main program for TOMS648_NSD_PRB.
c
c  Discussion:
c
c    TOMS648_NSD_PRB tests the TOMS648_NSD library.
c
C    This is a sample driver for NSDTST, a test set of nonstiff ODE's.
c
c    Two groups of problems, classes A and B, are solved in unscaled 
c    form.
c
c    Four tolerances are used.
c
c    The first test uses:
c
c      IOPT = 1
c      NORMEF = 0
c
c    The second test uses:
c
c      IOPT = 3
c      NORMEF = 0
c
c    The third test uses:
c
c      IOPT = 2
c      NORMEF = 2
c
c    Single precision arithmetic is used.
c
      implicit none

      double precision flag
      integer idlist(60)
      external nsdtst
      integer option(10)
      character * 80 title
      double precision tol(11)

      data option / 2, 0, 1, 1, 6*0 /
      data tol /1.0D-02, 1.0D-04, 1.0D-06, 1.0D-08, 7 * 0.0D+00 /
      data idlist /-11, -12, -13, -14, -15, 0,
     &                -21, -22, -23, -24, -25, 49*0 /

      title = 'DVERK,  HULL-ENRIGHT-JACKSON CODE'//
     &         ' BASED ON VERNER RK FORMULAS'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS648_NSD_PRB:'
      write ( *, '(a)' ) '  Example run of'
      write ( *, '(a)' ) '  ACM TOMS Algorithm 648'
      write ( *, '(a)' ) '  non-stiff sample ODE''s.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Double precision arithmetic.'

      write ( *, '(a)' )
      write ( *, '(a)' ) 'RUN NUMBER ONE'
      write ( *, '(a)' )

      call nsdtst ( title, option, tol, idlist, flag )

      write ( *, '(a)' )
      write ( *, '(a)' ) 'RUN NUMBER TWO'
      write ( *, '(a)' )

      option(1) = 3
      call nsdtst ( title, option, tol, idlist, flag )

      write ( *, '(a)' )
      write ( *, '(a)' ) 'RUN NUMBER THREE'
      write ( *, '(a)' )

      option(1) = 2
      option(2) = 2
      call nsdtst ( title, option, tol, idlist, flag )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS648_NSD_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'

      stop
      end
      subroutine method ( n, x, y, xend, tol, hmax, hstart )

c***********************************************************************
c
cc METHOD is a driver for the DVERK code.
c
c  Discussion:
c
C    DRIVER FOR THE DVERK CODE WHICH IS PART OF THE PACKAGE.
C    IT IS SOMEWHAT LENGTHY BECAUSE ITS INTERRUPT MECHANISM DOES
C    NOT ALLOW INTERRUPT IMMEDIATELY AFTER ACCEPTING A STEP.
c
c  Modified:
c
c    16 March 2006
c
c  Parameters:
c
c    n, x, y, xend, tol, hmax, hstart
c
      implicit none

      integer n

      double precision c(24)
      external fcn1
      double precision hmax
      double precision hstart
      integer i
      integer ind
      integer nw
      external stats
      double precision temp
      double precision tol
      external true
      double precision w(51,9)
      double precision x
      double precision xend
      double precision y(n)

      save nw

      data nw / 51 /

      ind = 2

      do i = 1, 9
        c(i) = 0.0D+00
      end do
C
C  SET:
c    c(1) = ABS ERROR CONTROL; 
c    c(4) = STARTING STEPSIZE;
C    c(6) = MAX STEPSIZE; 
c    c(9) = INTERRUPT NO. 2;
c
      c(1) = 1.0D+00
      c(4) = hstart
      c(6) = hmax
      c(9) = 1.0D+00

40    continue

      call true ( n, fcn1, x, y, xend, tol, ind, c, nw, w )

      if ( ind .eq. 6 ) then
        go to 40
      end if

      if ( ind .ne. 5 ) then
        go to 60
      end if

      temp = c(17)
C
C  THE DOCUMENTATION AND COMMENTS IN DVERK (SEE END OF 'STAGE 3')
C  DESCRIBE IT AS AIMING AT AN ERROR-PER-UNIT-STEP CONTROL OF THE
C  LOCALLY EXTRAPOLATED SOLUTION, USING THE VARIABLE 'SCALE'=C(15)
C  AS A RATHER ARBITRARY SCALEFACTOR.  HOWEVER, THE STATS CALL BELOW
C  ANALYSES W(*,2) AS AN ESTIMATE OF THE ERROR-PER-STEP IN THE
C  UNEXTRAPOLATED SOLUTION, ACCORDINGLY MUST BE USED WITH OPTION(4)
C  SET TO 1 TO INDICATE THAT THIS IS A METHOD THAT DOES LOC. EXTRAP.
c
      call stats ( c(17), w(1,9), tol, w(1,2) )

      if ( c(17) .ne. temp ) then
        go to 80
      end if

      go to 40

   60 continue

      if ( ind .ne. 3 ) then
        go to 80
      end if

      x = xend
      go to 100
c
c  Failure exit of some kind.
c
   80 continue

      x = c(17)

  100 continue

      return
      end
      subroutine fcn1 ( n, x, y, yp )

c***********************************************************************
c
cc FCN1 transfers function values from FCN to METHOD.
c
c  Discussion:
c
c    The METHOD package integrates an ODE whose right hand side is
c    evaluated by a routine whose name is an arbitrary external,
c    but which must have the form
c
c      subroutine FCN1 ( N, X, Y, YP )
c
c    while the ODE test package has a generic interface for the
c    function whose name and form are
c
c      subroutine FCN ( X, Y, YP )
c
c    This routine serves as an interface.  METHOD is told to call
c    FCN1 (the name is specified as an external function argument to 
c    METHOD) and FCN1, in turn, calls FCN.
c
c  Modified:
c
c    15 March 2006
c
c  Parameters:
c
c    Input, integer N, the number of dependent variables.
c
c    Input, double precision X, the value of the independent variable.
c
c    Input, double precision Y(N), the value of the dependent variables.
c
c    Output, double precision YP(N), the value of the right hand side of
c    the differential equations associated with the dependent
c    variables.
c
      implicit none

      integer n

      external fcn
      double precision x
      double precision y(n)
      double precision yp(n)

      call fcn ( x, y, yp )

      return
      end
