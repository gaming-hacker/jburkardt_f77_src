      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS655_PRB.
c
c  Discussion:
c
c    TOMS655_PRB tests the TOMS655 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      double precision a
      double precision alpha
      double precision b
      double precision beta
      integer kind
      integer nt

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS655_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS655 library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )
c
c  Compute 15 points of an example of each rule, with default A, B.
c
      do kind = 1, 9
        nt = 15
        if ( kind .eq. 8 ) then
          alpha = 1.0D+00
          beta = - alpha - 2 * nt - 2
        else
          alpha = 0.0D+00
          beta = 0.0D+00
        end if
        call test10 ( nt, kind, alpha, beta )
      end do
c
c  Compute 15 points of an example of each rule using nondefault A, B.
c
      do kind = 1, 9

        nt = 15

        if ( kind .eq. 1 ) then
          alpha = 0.0D+00
          beta = 0.0D+00
          a = 0.0D+00
          b = 1.0D+00
        else if ( kind .eq. 2 ) then
          alpha = 0.0D+00
          beta = 0.0D+00
          a = 0.0D+00
          b = 1.0D+00
        else if ( kind .eq. 3 ) then
          alpha = 1.0D+00
          beta = 0.0D+00
          a = 0.0D+00
          b = 1.0D+00
        else if ( kind .eq. 4 ) then
          alpha = 1.5D+00
          beta = 0.5D+00
          a = 0.0D+00
          b = 1.0D+00
        else if ( kind .eq. 5 ) then
          alpha = 1.0D+00
          beta = 0.0D+00
          a = 1.0D+00
          b = 1.0D+00
        else if ( kind .eq. 6 ) then
          alpha = 1.0D+00
          beta = 0.0D+00
          a = 0.0D+00
          b = 0.5D+00
        else if ( kind .eq. 7 ) then
          alpha = 1.0D+00
          beta = 0.0D+00
          a = 0.0D+00
          b = 1.0D+00
        else if ( kind .eq. 8 ) then
          alpha = 1.0D+00
          beta = - alpha - 2 * nt - 2
          a = 0.0D+00
          b = 1.0D+00
        else if ( kind .eq. 9 ) then
          alpha = 0.0D+00
          beta = 0.0D+00
          a = 0.0D+00
          b = 1.0D+00
        end if

        call test11 ( nt, kind, alpha, beta, a, b )

      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS655_PRB'
      write ( *, '(a)' ) '  Normal end of TOMS655 tests.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests CIQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer nt
      parameter ( nt = 5 )

      double precision alpha
      double precision beta
      integer i
      integer i4vec_sum
      integer key
      integer kind
      integer lu
      integer mlt(nt)
      integer ndx(nt)
      integer nwts
      double precision pi 
      parameter ( pi = 3.141592653589793D+00 )
      double precision t(nt)
      double precision wts(2*nt)

      write ( *, '(a)' ) '  ----------------------------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Test CIQFS.'
c
c  Set the knots in the default interval [-1,+1].
c
      do i = 1, nt
        t(i) = cos ( dble ( 2 * i - 1 ) * pi / dble ( 2 * nt ) )
      end do
c
c  Set the knot multiplicities.
c
      do i = 1, nt
        mlt(i) = 2
      end do
c
c  Set the size of the weights array.
c
      nwts = i4vec_sum ( nt, mlt )
c
c  Because KEY = 1, NDX will be set up for us.
c
c  KEY = 1 indicates that the WTS array should hold the weights
c  in the usual order.
c
      key = 1
c
c  Request Legendre weight function.
c
      kind = 1
c
c  ALPHA, BETA not used in Legendre weight function but set anyway.
c
      alpha = 0.0D+00
      beta  = 0.0D+00
c
c  LU controls printing.
c  A positive value requests that we compute and print weights, and
c  conduct a moments check.
c
      lu = 6
c
c  This call returns the WTS array.
c
      call ciqfs ( nt, t, mlt, nwts, ndx, key, kind, alpha, beta, lu, 
     &  wts )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests CIQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer nt
      parameter ( nt = 5 )

      double precision a
      double precision alpha
      double precision b
      double precision beta
      integer i
      integer i4vec_sum
      integer key
      integer kind
      integer lu
      integer mlt(nt)
      integer ndx(nt)
      integer nwts
      double precision t(nt)
      double precision wts(2*nt)

      write ( *, '(a)' ) '  ----------------------------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Test CIQF, CIQFS, CGQF and CGQFS'
      write ( *, '(a)' ) '  with all classical weight functions.'
c
c  Try all weight functions.
c
      do kind = 1, 9
c
c  Set parameters ALPHA and BETA.
c
        alpha = 0.5D+00
        if ( kind .ne. 8 ) then
          beta  = 2.0D+00
        else
          beta = - 16.0D+00
        end if
c
c  Set A and B.
c
        lu = 6
        a = - 0.5D+00
        b = 2.0D+00
c
c  Have CGQF compute the knots and weights.
c
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Knots and weights of Gauss quadrature formula'
        write ( *, '(a)' ) '  computed by CGQF.'
        call cgqf ( nt, kind, alpha, beta, a, b, lu, t, wts )
c
c  Now compute the weights for the same knots by CIQF.
c
c  Set the knot multiplicities.
c
        do i = 1, nt
          mlt(i) = 2
        end do
c
c  Set the size of the weights array.
c
        nwts = i4vec_sum ( nt, mlt )
c
c  Because KEY = 1, NDX will be set up for us.
c
c  KEY = 1 indicates that the WTS array should hold the weights
c  in the usual order.
c
        key = 1
c
c  LU controls printing.
c  A positive value requests that we compute and print weights, and
c  conduct a moments check.
c
        lu = 6

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Weights of Gauss quadrature formula computed from the'
        write ( *, '(a)' ) '  knots by CIQF.'
        call ciqf ( nt, t, mlt, nwts, ndx, key, kind, alpha, beta, 
     &    a, b, lu, wts )

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests CEIQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer nt
      parameter ( nt = 5 )

      double precision alpha
      double precision beta
      double precision f
      external f
      integer i
      integer kind
      integer lu
      integer mlt(nt)
      integer nwts
      double precision pi 
      parameter ( pi = 3.141592653589793D+00 )
      double precision qfsum
      double precision qfsx
      double precision t(nt)

      write ( *, '(a)' ) '  ----------------------------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Test CEIQFS.'
c
c  Set the knots in the default interval [-1,+1].
c
      do i = 1, nt
        t(i) = cos ( dble ( 2 * i - 1 ) * pi / dble ( 2 * nt ) )
      end do
c
c  Set the knot multiplicities.
c
      do i = 1, nt
        mlt(i) = 2
      end do
c
c  Set KIND to the Legendre weight function.
c
      kind = 1
c
c  ALPHA, BETA not used in Legendre weight function but set anyway.
c
      alpha = 0.0D+00
      beta  = 0.0D+00
c
c  Call CEIQFS to set up the quadrature formula and evaluate it on F.
c
      call ceiqfs ( nt, t, mlt, kind, alpha, beta, f, qfsum )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Integral of sin(x) on -1, 1 by Fejer type rule'
      write ( *, '(a,i4,a,i4)' ) 
     &  '  with ', nt, ' points of multiplicity 2.'
      write ( *, '(a,g24.16)' ) '  Quadrature formula:', qfsum

      qfsx = cos ( - 1.0D+00 ) - cos ( 1.0D+00 )
      write ( *, '(a,g24.16)' ) '  Exact value       :', qfsx
      write ( *, '(a,g24.16)' ) '  Error             :', 
     &  abs ( qfsum - qfsx )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests CEIQF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer nt
      parameter ( nt = 5 )

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision f
      external f
      integer i
      integer kind
      integer lu
      integer mlt(2*nt)
      integer nwts
      double precision pi 
      parameter ( pi = 3.141592653589793D+00 )
      double precision qfsum
      double precision qfsx
      double precision t(nt)

      write ( *, '(a)' ) '  ----------------------------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  Test CEIQF.'
c
c  Set the knots in the default interval [-1,+1].
c
      do i = 1, nt
        t(i) = cos ( dble ( 2 * i - 1 ) * pi / dble ( 2 * nt ) )
      end do
c
c  Set the knot multiplicities.
c
      do i = 1, nt
        mlt(i) = 2
      end do
c
c  Set KIND to the Legendre weight function.
c
      kind = 1
c
c  ALPHA, BETA not used in Legendre weight function but set anyway.
c
      alpha = 0.0D+00
      beta  = 0.0D+00
c
c  Set nonstandard interval A, B.
c
      a = -0.5D+00
      b = 2.0D+00
c
c  Shift knots from [-1,1] to [A,B].
c
      do i = 1, nt
        t(i) = ( ( b - a ) * t(i) + ( a + b ) ) / 2.0D+00
      end do
c
c  Call CEIQF to set up the quadrature formula and evaluate it on F.
c
      call ceiqf ( nt, t, mlt, kind, alpha, beta, a, b, f, qfsum )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6,a,g14.6)' ) 
     &  '  Integral of sin(x) from ', a, ' to ', b
      write ( *, '(a,i4,a)' ) 
     &  '  by Fejer type rule with ', nt, ' points'
      write ( *, '(a)' ) '  of multiplicity 2.'
      write ( *, '(a,g24.16)' ) '  Quadrature formula:', qfsum

      qfsx = cos ( a ) - cos ( b )
      write ( *, '(a,g24.16)' ) '  Exact value       :', qfsx
      write ( *, '(a,g24.16)' ) '  Error             :', 
     &  abs ( qfsum - qfsx )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests CLIQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer nt
      parameter ( nt = 5 )

      double precision alpha
      double precision beta
      integer i
      integer kind
      integer lu
      double precision pi 
      parameter ( pi = 3.141592653589793D+00 )
      double precision t(nt)
      double precision wts(nt)

      write ( *, '(a)' ) '  ----------------------------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  Test CLIQFS.'
c
c  Number of knots.
c
c  Set the knots in the default interval [-1,+1].
c
      do i = 1, nt
        t(i) = cos ( dble ( 2 * i - 1 ) * pi / dble ( 2 * nt ) )
      end do
c
c  Request Legendre weight function.
c
      kind = 1
c
c  ALPHA, BETA not used in Legendre weight function but set anyway.
c
      alpha = 0.0D+00
      beta  = 0.0D+00
c
c  LU controls printing.
c  A positive value requests that we compute and print weights, and
c  conduct a moments check.
c
      lu = 6
c
c  This call returns the WTS array.
c
      call cliqfs ( nt, t, kind, alpha, beta, lu, wts )

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests CLIQF and EIQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer nt
      parameter ( nt = 5 )

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision f
      external f
      integer i
      integer kind
      integer lu
      double precision pi 
      parameter ( pi = 3.141592653589793D+00 )
      double precision qfsum
      double precision qfsx
      double precision t(nt)
      double precision wts(nt)

      write ( *, '(a)' ) '  ----------------------------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  Test CLIQF and EIQFS.'
c
c  Number of knots.
c
c  Set the knots in the default interval [-1,+1].
c
      do i = 1, nt
        t(i) = cos ( dble ( 2 * i - 1 ) * pi / dble ( 2 * nt ) )
      end do
c
c  Set KIND to the Legendre weight function.
c
      kind = 1
c
c  ALPHA, BETA not used in Legendre weight function but set anyway.
c
      alpha = 0.0D+00
      beta  = 0.0D+00
c
c  Set nonstandard interval A, B.
c
      a = -0.5D+00
      b = 2.0D+00
c
c  Shift knots from [-1,1] to [A,B].
c
      do i = 1, nt
        t(i) = ( ( b - a ) * t(i) + ( a + b ) ) / 2.0D+00
      end do
c
c  LU controls printout.
c
      lu = 6
c
c  Call CLIQF to set up the quadrature formula.
c
      call cliqf ( nt, t, kind, alpha, beta, a, b, lu, wts )
c
c  Call EIQFS to evaluate the quadrature formula.
c
      call eiqfs ( nt, t, wts, f, qfsum )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6,a,g14.6)' ) 
     &  '  Integral of sin(x) from ', a, ' to ', b
      write ( *, '(a,i4,a)' ) 
     &  '  by Fejer type rule with ', nt, ' points'
      write ( *, '(a)' ) '  of multiplicity 1.'
      write ( *, '(a,g24.16)' ) '  Quadrature formula:', qfsum

      qfsx = cos ( a ) - cos ( b )
      write ( *, '(a,g24.16)' ) '  Exact value       :', qfsx
      write ( *, '(a,g24.16)' ) 
     &  '  Error             :', abs ( qfsum - qfsx )

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 tests CEGQF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer nt
      parameter ( nt = 12 )

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision f
      external f
      integer kind
      double precision qfsum
      double precision qfsx

      write ( *, '(a)' ) '  ----------------------------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  Test CEGQF.'
c
c  Number of knots.
c
c  Request exponential weight function.
c
      kind = 7
c
c  Set ALPHA and BETA.
c
      alpha = 1.0D+00
      beta  = 0.0D+00
c
c  Set interval [A,B].
c
      a = -0.5D+00
      b = 2.0D+00
c
c  Call CEGQF to compute and evaluate the Gauss quadrature formula.
c
      call cegqf ( nt, kind, alpha, beta, a, b, f, qfsum )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6,a,g14.6)' ) 
     &  '  Integral of x*sin(x) from ', a, ' to ', b
      write ( *, '(a,i4,a)' ) 
     &  '  by Gauss-exponential rule with ', nt, ' points'
      write ( *, '(a,g24.16)' ) '  Quadrature formula:', qfsum

      qfsx = ( b - a ) * 0.5D+00 * ( cos ( a ) - cos ( b ) )
     &  + sin ( b ) + sin ( a ) - 2.0D+00 * sin ( ( a + b ) / 2.0D+00 )

      write ( *, '(a,g24.16)' ) '  Exact value       :', qfsx
      write ( *, '(a,g24.16)' ) 
     &  '  Error             :', abs ( qfsum - qfsx )

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 tests CEGQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer nt
      parameter ( nt = 12 )

      double precision alpha
      double precision beta
      double precision f
      external f
      integer kind
      double precision qfsum
      double precision qfsx

      write ( *, '(a)' ) '  ----------------------------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  Test CEGQFS.'
c
c  Number of knots.
c
c  Request exponential weight function.
c
      kind = 7
c
c  Set ALPHA and BETA.
c
      alpha = 1.0D+00
      beta  = 0.0D+00
c
c  Call CEGQFS to compute and evaluate the Gauss quadrature formula.
c
      call cegqfs ( nt, kind, alpha, beta, f, qfsum )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integral of x*sin(x) from -1 to +1'
      write ( *, '(a,i4,a)' ) 
     &  '  by Gauss-exponential rule with ', nt, ' points'
      write ( *, '(a,g24.16)' ) '  Quadrature formula:', qfsum

      qfsx = cos ( -1.0D+00 ) - cos ( +1.0D+00 )

      write ( *, '(a,g24.16)' ) '  Exact value       :', qfsx
      write ( *, '(a,g24.16)' ) 
     &  '  Error             :', abs ( qfsum - qfsx )

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 calls CGQFS to compute and print generalized Gauss-Hermite rules.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nt
      parameter ( nt = 15 )

      double precision alpha
      double precision beta
      integer io
      integer kind
      double precision t(nt)
      double precision wts(nt)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09'
      write ( *, '(a)' ) 
     &  '  Call CGQFS to compute generalized Hermite rules.'

      kind = 6
      alpha = 1.0D+00
      beta = 0.0D+00
      io = - 6

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  NT = ', nt
      write ( *, '(a,g14.6)' ) '  ALPHA = ', alpha

      call cgqfs ( nt, kind, alpha, beta, io, t, wts )

      return
      end
      subroutine test10 ( nt, kind, alpha, beta )

c*********************************************************************72
c
cc TEST10 calls CDGQF to compute a quadrature formula.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nt

      double precision alpha
      double precision beta
      integer i
      integer kind
      double precision t(nt)
      double precision wts(nt)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST10'
      write ( *, '(a)' ) '  Call CDGQF to compute a quadrature formula.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  KIND = ', kind
      write ( *, '(a,g14.6)' ) '  ALPHA = ', alpha
      write ( *, '(a,g14.6)' ) '  BETA  = ', beta

      call cdgqf ( nt, kind, alpha, beta, t, wts )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' Index     Abscissas                 Weights'
      write ( *, '(a)' ) ' '
      do i = 1, nt
        write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) i, t(i), wts(i)
      end do

      return
      end
      subroutine test11 ( nt, kind, alpha, beta, a, b )

c*********************************************************************72
c
cc TEST11 calls CGQF to compute a quadrature formula.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nt

      double precision a
      double precision alpha
      double precision b
      double precision beta
      integer i
      integer kind
      integer lu
      double precision t(nt)
      double precision wts(nt)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST11'
      write ( *, '(a)' ) '  Call CGQF to compute a quadrature formula'
      write ( *, '(a)' ) '  with nondefault values of parameters A, B.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  KIND = ', kind
      write ( *, '(a,g14.6)' ) '  ALPHA = ', alpha
      write ( *, '(a,g14.6)' ) '  BETA  = ', beta
      write ( *, '(a,g14.6)' ) '  A =     ', a
      write ( *, '(a,g14.6)' ) '  B =     ', b

      lu = 0
      call cgqf ( nt, kind, alpha, beta, a, b, lu, t, wts )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' Index     Abscissas                 Weights'
      write ( *, '(a)' ) ' '
      do i = 1, nt
        write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) i, t(i), wts(i)
      end do

      return
      end
      function f ( x, i )

c*********************************************************************72
c
cc F returns values of the integrand or its derivatives.
c
c  Discussion:
c
c    This function is an example of an integrand function.
c
c    The package can generate quadrature formulas that use derivative
c    information as well as function values.  Therefore, this routine is
c    set up to provide derivatives of any order as well as the function
c    value.  In an actual application, the highest derivative needed
c    is of order one less than the highest knot multiplicity.
c
c    In other words, in the usual case where knots are not repeated,
c    this routine only needs to return function values, not any derivatives.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Input, integer I, the order of the derivative of F to
c    be evaluated.
c
c    Output, double precision F, the value of the I-th derivative of F at X.
c
      implicit none

      double precision f
      integer i
      integer l
      double precision x

      l = mod ( i, 4 )

      if ( l .eq. 0 ) then
        f = sin ( x )
      else if ( l .eq. 1 ) then
        f = cos ( x )
      else if ( l .eq. 2 ) then
        f = - sin ( x )
      else if ( l .eq. 3 ) then
        f = - cos ( x )
      end if

      return
      end
