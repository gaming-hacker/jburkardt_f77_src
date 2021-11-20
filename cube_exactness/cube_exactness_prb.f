      program main

c*********************************************************************72
c
cc MAIN is the main program for CUBE_EXACTNESS_PRB.
c
c  Discussion:
c
c    CUBE_EXACTNESS_PRB tests the CUBE_EXACTNESS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CUBE_EXACTNESS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CUBE_EXACTNESS library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CUBE_EXACTNESS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests product Gauss-Legendre rules for the Legendre 3D integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer l_max
      parameter ( l_max = 5 )
      integer n_max
      parameter ( n_max = 
     &  ( l_max + 1 ) * ( l_max + 1 ) * ( l_max + 1 ) )

      double precision a(3)
      double precision b(3)
      integer l
      integer n
      integer nx
      integer ny
      integer nz
      integer p_max
      integer t
      double precision w(n_max)
      double precision x(n_max)
      double precision y(n_max)
      double precision z(n_max)

      a(1) = -1.0D+00
      a(2) = -1.0D+00
      a(3) = -1.0D+00
      b(1) = +1.0D+00
      b(2) = +1.0D+00
      b(3) = +1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Product Gauss-Legendre rules for the 3D Legendre integral.'
      write ( *, '(a)' ) '  Density function rho(x) = 1.'
      write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
      write ( *, '(a)' ) '          -1 <= y <= +1.'
      write ( *, '(a)' ) '          -1 <= z <= +1.'
      write ( *, '(a)' ) '  Level: L'
      write ( *, '(a)' ) '  Exactness: 2*L+1'
      write ( *, '(a)' ) '  Order: N = (L+1)*(L+1)*(L+1)'

      do l = 0, l_max

        nx = l + 1
        ny = l + 1
        nz = l + 1
        n = nx * ny * nz
        t = 2 * l + 1

        call legendre_3d_set ( a, b, nx, ny, nz, x, y, z, w )

        p_max = t + 1
        call legendre_3d_exactness ( a, b, n, x, y, z, w, p_max )

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests product Gauss-Legendre rules for the Legendre 3D integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nx
      parameter ( nx = 2 )
      integer ny
      parameter ( ny = 3 )
      integer nz
      parameter ( nz = 4 )
      integer n
      parameter ( n = nx * ny * nz )

      double precision a(3)
      double precision b(3)
      integer i
      integer l
      integer p_max
      integer t
      double precision w(n)
      double precision x(n)
      double precision y(n)
      double precision z(n)

      a(1) = -1.0D+00
      a(2) = -1.0D+00
      a(3) = -1.0D+00
      b(1) = +1.0D+00
      b(2) = +1.0D+00
      b(3) = +1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Product Gauss-Legendre rules for the 3D Legendre integral.'
      write ( *, '(a)' ) '  Density function rho(x) = 1.'
      write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
      write ( *, '(a)' ) '          -1 <= y <= +1.'
      write ( *, '(a)' ) '          -1 <= z <= +1.'
      write ( *, '(a)' ) '  Exactness: 3 = 2 * min ( 2, 3, 4 ) - 1'
      write ( *, '(a)' ) '  Order: N = 2 * 3 * 4'

      call legendre_3d_set ( a, b, nx, ny, nz, x, y, z, w )

      p_max = 4

      call legendre_3d_exactness ( a, b, n, x, y, z, w, p_max )

      return
      end
      subroutine legendre_3d_set ( a, b, nx, ny, nz, x, y, z, w )

c*********************************************************************72
c
cc LEGENDRE_3D_SET: set a 3D Gauss-Legendre quadrature rule.
c
c  Discussion:
c
c    The integral:
c
c      integral ( z1 <= z <= z2 )
c               ( y1 <= y <= y2 )
c               ( x1 <= x <= x2 ) f(x,y,z) dx dy dz
c
c    The quadrature rule:
c
c      sum ( 1 <= i <= n ) w(i) * f ( x(i),y(i),z(i) )
c
c    where n = nx * ny * nz.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(3), B(3), the lower and upper integration
c    limits.
c
c    Input, integer NX, NY, NZ, the orders in the X and Y directions.
c    These orders must be between 1 and 10.
c
c    Output, double precision X(N), Y(N), Z(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer nx
      integer ny
      integer nz

      double precision a(3)
      double precision b(3)
      integer i
      integer j
      integer k
      integer l
      double precision w(nx*ny*nz)
      double precision wx(nx)
      double precision wy(ny)
      double precision wz(nz)
      double precision x(nx*ny*nz)
      double precision xx(nx)
      double precision y(nx*ny*nz)
      double precision yy(ny)
      double precision z(nx*ny*nz)
      double precision zz(nz)
c
c  Get the rules for [-1,+1].
c
      call legendre_set ( nx, xx, wx )
      call legendre_set ( ny, yy, wy )
      call legendre_set ( nz, zz, wz )
c
c  Adjust from [-1,+1] to [A,B].
c
      do i = 1, nx
        xx(i) = ( ( 1.0D+00 - xx(i) ) * a(1)
     &          + ( 1.0D+00 + xx(i) ) * b(1) )
     &          /   2.0D+00
        wx(i) = wx(i) * ( b(1) - a(1) ) / 2.0D+00
      end do

      do j = 1, ny
        yy(j) = ( ( 1.0D+00 - yy(j) ) * a(2)
     &          + ( 1.0D+00 + yy(j) ) * b(2) )
     &          /   2.0D+00
        wy(j) = wy(j) * ( b(2) - a(2) ) / 2.0D+00
      end do

      do k = 1, nz
        zz(k) = ( ( 1.0D+00 - zz(k) ) * a(3)
     &          + ( 1.0D+00 + zz(k) ) * b(3) )
     &          /   2.0D+00
        wz(k) = wz(k) * ( b(3) - a(3) ) / 2.0D+00
      end do
c
c  Compute the product rule.
c
      l = 0
      do k = 1, nz
        do j = 1, ny
          do i = 1, nx
            l = l + 1
            x(l) = xx(i)
            y(l) = yy(j)
            z(l) = zz(k)
            w(l) = wx(i) * wy(j) * wz(k)
          end do
        end do
      end do

      return
      end
      subroutine legendre_set ( n, x, w )

c*********************************************************************72
c
cc LEGENDRE_SET sets abscissas and weights for Gauss-Legendre quadrature.
c
c  Discussion:
c
c    The integral:
c
c      integral ( -1 <= x <= 1 ) f(x) dx
c
c    The quadrature rule:
c
c      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
c
c    The quadrature rule is exact for polynomials through degree 2*N-1.
c
c    The abscissas are the zeroes of the Legendre polynomial P(N)(X).
c
c    Mathematica can compute the abscissas and weights of a Gauss-Legendre
c    rule of order N for the interval [A,B] with P digits of precision
c    by the commands:
c
c    Needs["NumericalDifferentialEquationAnalysis`"]
c    GaussianQuadratureWeights [ n, a, b, p ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2010
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Vladimir Krylov,
c    Approximate Calculation of Integrals,
c    Dover, 2006,
c    ISBN: 0486445798,
c    LC: QA311.K713.
c
c    Arthur Stroud, Don Secrest,
c    Gaussian Quadrature Formulas,
c    Prentice Hall, 1966,
c    LC: QA299.4G3S7.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996,
c    ISBN: 0-8493-2479-3,
c    LC: QA47.M315.
c
c  Parameters:
c
c    Input, integer N, the order.
c    N must be between 1 and 10.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision w(n)
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = 0.000000000000000000000000000000D+00

        w(1) = 2.000000000000000000000000000000D+00

      else if ( n .eq. 2 ) then

        x(1) = -0.577350269189625764509148780502D+00
        x(2) = 0.577350269189625764509148780502D+00

        w(1) = 1.000000000000000000000000000000D+00
        w(2) = 1.000000000000000000000000000000D+00

      else if ( n .eq. 3 ) then

        x(1) = -0.774596669241483377035853079956D+00
        x(2) = 0.000000000000000000000000000000D+00
        x(3) = 0.774596669241483377035853079956D+00

        w(1) = 0.555555555555555555555555555556D+00
        w(2) = 0.888888888888888888888888888889D+00
        w(3) = 0.555555555555555555555555555556D+00

      else if ( n .eq. 4 ) then

        x(1) = -0.861136311594052575223946488893D+00
        x(2) = -0.339981043584856264802665759103D+00
        x(3) = 0.339981043584856264802665759103D+00
        x(4) = 0.861136311594052575223946488893D+00

        w(1) = 0.347854845137453857373063949222D+00
        w(2) = 0.652145154862546142626936050778D+00
        w(3) = 0.652145154862546142626936050778D+00
        w(4) = 0.347854845137453857373063949222D+00

      else if ( n .eq. 5 ) then

        x(1) = -0.906179845938663992797626878299D+00
        x(2) = -0.538469310105683091036314420700D+00
        x(3) = 0.000000000000000000000000000000D+00
        x(4) = 0.538469310105683091036314420700D+00
        x(5) = 0.906179845938663992797626878299D+00

        w(1) = 0.236926885056189087514264040720D+00
        w(2) = 0.478628670499366468041291514836D+00
        w(3) = 0.568888888888888888888888888889D+00
        w(4) = 0.478628670499366468041291514836D+00
        w(5) = 0.236926885056189087514264040720D+00

      else if ( n .eq. 6 ) then

        x(1) = -0.932469514203152027812301554494D+00
        x(2) = -0.661209386466264513661399595020D+00
        x(3) = -0.238619186083196908630501721681D+00
        x(4) = 0.238619186083196908630501721681D+00
        x(5) = 0.661209386466264513661399595020D+00
        x(6) = 0.932469514203152027812301554494D+00

        w(1) = 0.171324492379170345040296142173D+00
        w(2) = 0.360761573048138607569833513838D+00
        w(3) = 0.467913934572691047389870343990D+00
        w(4) = 0.467913934572691047389870343990D+00
        w(5) = 0.360761573048138607569833513838D+00
        w(6) = 0.171324492379170345040296142173D+00

      else if ( n .eq. 7 ) then

        x(1) = -0.949107912342758524526189684048D+00
        x(2) = -0.741531185599394439863864773281D+00
        x(3) = -0.405845151377397166906606412077D+00
        x(4) = 0.000000000000000000000000000000D+00
        x(5) = 0.405845151377397166906606412077D+00
        x(6) = 0.741531185599394439863864773281D+00
        x(7) = 0.949107912342758524526189684048D+00

        w(1) = 0.129484966168869693270611432679D+00
        w(2) = 0.279705391489276667901467771424D+00
        w(3) = 0.381830050505118944950369775489D+00
        w(4) = 0.417959183673469387755102040816D+00
        w(5) = 0.381830050505118944950369775489D+00
        w(6) = 0.279705391489276667901467771424D+00
        w(7) = 0.129484966168869693270611432679D+00

      else if ( n .eq. 8 ) then

        x(1) = -0.960289856497536231683560868569D+00
        x(2) = -0.796666477413626739591553936476D+00
        x(3) = -0.525532409916328985817739049189D+00
        x(4) = -0.183434642495649804939476142360D+00
        x(5) = 0.183434642495649804939476142360D+00
        x(6) = 0.525532409916328985817739049189D+00
        x(7) = 0.796666477413626739591553936476D+00
        x(8) = 0.960289856497536231683560868569D+00

        w(1) = 0.101228536290376259152531354310D+00
        w(2) = 0.222381034453374470544355994426D+00
        w(3) = 0.313706645877887287337962201987D+00
        w(4) = 0.362683783378361982965150449277D+00
        w(5) = 0.362683783378361982965150449277D+00
        w(6) = 0.313706645877887287337962201987D+00
        w(7) = 0.222381034453374470544355994426D+00
        w(8) = 0.101228536290376259152531354310D+00

      else if ( n .eq. 9 ) then

        x(1) = -0.968160239507626089835576203D+00
        x(2) = -0.836031107326635794299429788D+00
        x(3) = -0.613371432700590397308702039D+00
        x(4) = -0.324253423403808929038538015D+00
        x(5) = 0.000000000000000000000000000D+00
        x(6) = 0.324253423403808929038538015D+00
        x(7) = 0.613371432700590397308702039D+00
        x(8) = 0.836031107326635794299429788D+00
        x(9) = 0.968160239507626089835576203D+00

        w(1) = 0.081274388361574411971892158111D+00
        w(2) = 0.18064816069485740405847203124D+00
        w(3) = 0.26061069640293546231874286942D+00
        w(4) = 0.31234707704000284006863040658D+00
        w(5) = 0.33023935500125976316452506929D+00
        w(6) = 0.31234707704000284006863040658D+00
        w(7) = 0.26061069640293546231874286942D+00
        w(8) = 0.18064816069485740405847203124D+00
        w(9) = 0.081274388361574411971892158111D+00

      else if ( n .eq. 10 ) then

        x(1) = -0.973906528517171720077964012D+00
        x(2) = -0.865063366688984510732096688D+00
        x(3) = -0.679409568299024406234327365D+00
        x(4) = -0.433395394129247190799265943D+00
        x(5) = -0.148874338981631210884826001D+00
        x(6) = 0.148874338981631210884826001D+00
        x(7) = 0.433395394129247190799265943D+00
        x(8) = 0.679409568299024406234327365D+00
        x(9) = 0.865063366688984510732096688D+00
        x(10) = 0.973906528517171720077964012D+00

        w(1) = 0.066671344308688137593568809893D+00
        w(2) = 0.14945134915058059314577633966D+00
        w(3) = 0.21908636251598204399553493423D+00
        w(4) = 0.26926671930999635509122692157D+00
        w(5) = 0.29552422471475287017389299465D+00
        w(6) = 0.29552422471475287017389299465D+00
        w(7) = 0.26926671930999635509122692157D+00
        w(8) = 0.21908636251598204399553493423D+00
        w(9) = 0.14945134915058059314577633966D+00
        w(10) = 0.066671344308688137593568809893D+00

      else

        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'LEGENDRE_SET - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of N.'
        stop 1

      end if

      return
      end

