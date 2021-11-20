!  toms614prb.f  16 April 1999
!
      program driver
!
!***********************************************************************
!
!! DRIVER is a test program for INTHP.
!
!     test of 8 integrands representing classes of problems
!     that inthp solves. each integrand function includes
!     comments on the class of problems it belongs to.
!
      real a
      real b
      real d
      real eps
      real f1
      real f2
      real f3
      real f4
      real f5
      real f6
      real f7
      real f8
      integer i
      integer inf
      integer j
      integer m
      real p
      real quadr
!
      external f1
      external f2
      external f3
      external f4
      external f5
      external f6
      external f7
      external f8
!
      do j = 1, 2

        do i = 1, 8

          write ( *, * ) ' '
          write ( *, * ) 'Test of integrand number ', i
          write ( *, * ) ' '
          write ( *, * ) 'Input parameters:'
          write ( *, * ) ' '

          if ( i .eq. 1 ) then

            a = 0.0
            b = 0.0
            d = 0.7854
            m = 10000
            p = 1.0
            eps = 1.0e-10
            inf = 1

          else if ( i .eq. 2 ) then

            a = 0.0
            b = 1.0
            d = 1.5706
            m = 10000
            p = 2.0
            eps = 1.0e-10
            inf = 4

          else if ( i .eq. 3 ) then

            a = 0.0
            b = 0.0
            d = 1.5706
            m = 10000
            p = 2.0
            eps = 1.0e-10
            inf = 3

          else if ( i .eq. 4 ) then

            a = 0.0
            b = 0.0
            d = 1.5706
            m = 10000
            p = 2.0
            eps = 1.0e-10
            inf = 2

          else if ( i .eq. 5 ) then

            a = 0.0
            b = 0.0
            d = 1.5706
            m = 10000
            p = 100.0
            eps = 1.0e-10
            inf = 3

          else if ( i .eq. 6 ) then

            a = -1.0
            b = 1.0
            d = 1.5706
            m = 10000
            p = 2.0
            eps = 5.0e-3
            inf = 4

          else if ( i .eq. 7 ) then

            a = 0.0
            b = 2.0
            d = 1.5706
            m = 10000
            p = 2.0
            eps = 1.0e-10
            inf = 4

          else if ( i .eq. 8 ) then

            a = 0.0
            b = 0.0
            d = 1.5706
            m = 10000
            p = 2.0
            eps = 1.0e-10
            inf = 1

          end if
!
!  On second pass, set P = 0 to use heuristic termination.
!
          if ( j .eq. 2 ) then
            p = 0.0
          end if

          write ( *, * ) 'A =     ', a
          write ( *, * ) 'B =     ', b
          write ( *, * ) 'D =     ', d
          write ( *, * ) 'M =     ', m
          write ( *, * ) 'P =     ', p
          write ( *, * ) 'EPS =   ', eps
          write ( *, * ) 'INF =   ', inf

          if ( i .eq. 1 ) then

            call inthp ( a, b, d, f1, m, p, eps, inf, quadr )

          else if ( i .eq. 2 ) then

            call inthp ( a, b, d, f2, m, p, eps, inf, quadr )

          else if ( i .eq. 3 ) then

            call inthp ( a, b, d, f3, m, p, eps, inf, quadr )

          else if ( i .eq. 4 ) then

            call inthp ( a, b, d, f4, m, p, eps, inf, quadr )

          else if ( i .eq. 5 ) then

            call inthp ( a, b, d, f5, m, p, eps, inf, quadr )

          else if ( i .eq. 6 ) then

            call inthp ( a, b, d, f6, m, p, eps, inf, quadr )

          else if ( i .eq. 7 ) then

            call inthp ( a, b, d, f7, m, p, eps, inf, quadr )

          else if ( i .eq. 8 ) then

            call inthp ( a, b, d, f8, m, p, eps, inf, quadr )

          end if

          write ( *, * ) ' '
          write ( *, * ) 'Output parameters:'
          write ( *, * ) ' '
          write ( *, * ) 'INF =   ', inf
          write ( *, * ) 'EPS =   ', eps
          write ( *, * ) 'QUADR = ', quadr
          write ( *, * ) 'M =     ', m

        end do

      end do

      stop
      end
      function f1 ( x )
!
!***********************************************************************
!
!! F1(X) = EXP ( - X**2 ).
!
!
!     f1 belongs to the space h(p,dd) with p=+infinity
!     and dd=strip(z:abs(im(z)).lt.pi/4). 
!
!  f1 decreases exponentially as abs(x) goes to infinity.
!
      real f1
      real x
!
      f1 = exp ( - x**2 )

      return
      end
      function f2 ( x )
!
!***********************************************************************
!
!! F2(X) = 2 / ( SQRT(X) * (X+1) ).
!
!
!     f2 belongs to the space h(p,dd) with p=2 and
!     dd=lens region(z:abs(arg(z/(1-z)).lt.pi/2).
!
!  f2 has an algebraic type singularity at x=0.
!
      real f2
      real x
!
      f2 = 2.0 / sqrt ( x ) / ( x + 1.0 )

      return
      end
      function f3 ( x )
!
!***********************************************************************
!
!! F3(X) = 1 / ( SQRT(X) * EXP(X) ).
!
!
!     f3 belongs to the space h(p,dd) with p=2 and
!     dd=region(z:abs(arg(sinh(z))).lt.pi/2). 
!
!  f3 has an algebraic type singularity at x=0.
!
!  f3 decreases exponentially as x goes to +infinity.
!
      real f3
      real x
!
      f3 = 1.0 / sqrt ( x ) / exp ( x )

      return
      end
      function f4 ( x )
!
!***********************************************************************
!
!! F4(X) = 1 / ( SQRT(X) * (X+1) ).
!
!
!     f4 belongs to the space h(p,dd) with p=2 and
!     dd=sector(z:abs(arg(z)).lt.pi/2). 
!
!  f4 has an algebraic type singularity at x=0.
!
!  f4 decreases with algebraic rate as x goes to +infinity.
!
      real f4
      real x
!
      f4 = 1.0 / sqrt ( x ) / ( x + 1.0 )

      return
      end
      function f5 ( x )
!
!***********************************************************************
!
!! F5(X) = LOG ( 1 - SIN(X)/X ) / EXP(X).
!
!
!     f5 belongs to the space h(p,dd) with p=100 and
!     dd=region(z:abs(arg(sinh(z))).lt.pi/2). 
!
!  f5 is an oscillatory integrand.
!
!  f5 has a logarithmic type singularity at x=0.
!
!  f5 decreases exponentially as x goes to +infinity.
!
      real f
      real f5
      real s, t, x, z, s1, z1, sig
!
      if (x.le.0.1) go to 10
      s = 1. - sin(x)/x
      go to 30
   10 sig = 1.
      z1 = x*x
      z = z1
      s = 0.
      s1 = 0.
      t = 3.
      f = 1.
   20 f = f*t*(t-1.)
      s = s + sig*z/f
      if (s.eq.s1) go to 30
      s1 = s
      z = z*z1
      sig = -sig
      t = t + 2.
      go to 20

   30 continue

      f5 = log ( s ) / exp ( x )

      return
      end
      function f6 ( x )
!
!***********************************************************************
!
!! F6(X) = 2 / SQRT ( 3 - 2*X - X**2 ).
!
!
!     f6 belongs to the space h(p,dd) with p=2 and
!     dd=lens region(z:abs(arg((z+1)/(1-z))).lt.pi/2). 
!
!  f6 has an algebraic type singularity at x=1.
!
!       this is an example for loss of significant figures due
!     to inaccurate evaluation of an integrand near the singu-
!     larity-see [2] and integrand f7.
!
      real f6
      real x
!
      f6 = 2.0 / sqrt ( 3.0 - 2.0 * x - x**2 )

      return
      end
      function f7 ( x )
!
!***********************************************************************
!
!! F7(X) = 2 / SQRT ( 4*X - X**2 ).
!
!
!     f7 belongs to the space h(p,dd) with p=2 and
!     dd=lens region(z:abs(arg(z/(2-z)).lt.pi/2). 
!
!  f7 has algebraic type singularity at x=0.
!
!       to overcome numerical problems in computing f6 near
!     the singularity we replace x in f6 by 1-t and get f7.
!     now we can achieve all significant figures-see [2].
!
      real f7
      real x
!
      f7 = 2.0 / sqrt ( x * ( 4.0  - x ) )

      return
      end
      function f8 ( x )
! 
!***********************************************************************
!
!! F8(X) = 1 / ( EXP(X/2) + EXP(-X/2) ).
!
!
!     f8 belongs to the space h(p,dd) with p=2 and
!     dd=strip(z:abs(im(z)).lt.pi/2). 
!
!  f8 decreases expotentially as abs(x) goes to infinity. 
!
!  the rate of decrease is slower than that of f1.
!
      real f8
      real x
!
      f8 = 1.0 / ( exp ( 0.5 * x ) + exp ( - 0.5 * x ) )

      return
      end
