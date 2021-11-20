      program main

c*********************************************************************72
c
cc MAIN is the main program for FEM1D_LAGRANGE_PRB.
c
c  Discussion:
c
c    FEM1D_LAGRANGE_PRB tests FEM1D_LAGRANGE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer q_num
      integer x_num

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FEM1D_LAGRANGE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the FEM1D_LAGRANGE library.'

      call legendre_set_test ( )
      call lagrange_value_test ( )
      call lagrange_derivative_test ( )

      x_num = 11
      q_num = 5
      call fem1d_lagrange_stiffness_test ( x_num, q_num )

      x_num = 11
      q_num = 10
      call fem1d_lagrange_stiffness_test ( x_num, q_num )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FEM1D_LAGRANGE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine legendre_set_test ( )

c*********************************************************************72
c
cc LEGENDRE_SET_TEST tests LEGENDRE_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision e1
      double precision e2
      double precision e3
      integer i
      integer n
      double precision r8vec_sum
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEGENDRE_SET_TEST'
      write ( *, '(a)' ) '  LEGENDRE_SET returns points and weights of'
      write ( *, '(a)' ) '  Gauss-Legendre quadrature rules.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '   N               1             X^4           Runge'
      write ( *, '(a)' ) ''

      do n = 1, 10


        call legendre_set ( n, x, w )
        e1 = r8vec_sum ( n, w )
        e2 = 0.0D+00
        do i = 1, n
          e2 = e2 + w(i) * x(i) ** 4
        end do
        e3 = 0.0D+00
        do i = 1, n
          e3 = e3 + w(i) / ( 1.0D+00 + 25.0D+00 * x(i) ** 2 )
        end do
        write ( *, '(2x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) n, e1, e2, e3

      end do

      return
      end
      subroutine lagrange_value_test ( )

c*********************************************************************72
c
cc LAGRANGE_VALUE_TEST tests LAGRANGE_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nd
      parameter ( nd = 5 )
      integer ni
      parameter ( ni = 2 * nd - 1 )

      integer i
      integer j
      double precision li(ni,nd)
      double precision xd(nd)
      double precision xhi
      double precision xi(ni)
      double precision xlo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LAGRANGE_VALUE_TEST'
      write ( *, '(a)' ) 
     &  '  LAGRANGE_VALUE evaluates the Lagrange basis polynomials.'

      xlo = 0.0D+00
      xhi = dble ( nd - 1 )
      call r8vec_linspace ( nd, xlo, xhi, xd )

      call r8vec_print ( nd, xd, '  Lagrange basis points:' )
c
c  Evaluate the polynomials.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' )     
     &  '   I      X          L1(X)       L2(X)       L3(X)' //
     &  '       L4(X)       L5(X)'
      write ( *, '(a)' ) ''

      call r8vec_linspace ( ni, xlo, xhi, xi )

      call lagrange_value ( nd, xd, ni, xi, li )

      do i = 1, ni

        write ( *, '(2x,i2,f10.4)', advance = 'no' ) i, xi(i)
        do j = 1, nd
          write ( *, '(2x,f10.4)', advance = 'no' ) li(i,j)
        end do
        write ( *, '(a)' ) ''

      end do

      return
      end
      subroutine lagrange_derivative_test ( )

c*********************************************************************72
c
cc LAGRANGE_DERIVATIVE_TEST tests LAGRANGE_DERIVATIVE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nd
      parameter ( nd = 5 )
      integer ni
      parameter ( ni = 2 * nd - 1 )

      integer i
      integer j
      double precision lpi(ni,nd)
      double precision xd(nd)
      double precision xhi
      double precision xi(ni)
      double precision xlo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LAGRANGE_DERIVATIVE_TEST'
      write ( *, '(a)' ) 
     &  '  LAGRANGE_DERIVATIVE evaluates the Lagrange basis derivative.'

      xlo = 0.0D+00
      xhi = dble ( nd - 1 )
      call r8vec_linspace ( nd, xlo, xhi, xd )

      call r8vec_print ( nd, xd, '  Lagrange basis points:' )
c
c  Evaluate the polynomials.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '   I      X         L1''(X)      L2''(X)      L3''(X)' //
     &  '      L4''(X)      L5''(X)'
      write ( *, '(a)' ) ''

      call r8vec_linspace ( ni, xlo, xhi, xi )

      call lagrange_derivative ( nd, xd, ni, xi, lpi )

      do i = 1, ni

        write ( *, '(2x,i2,f10.4)', advance = 'no' ) i, xi(i)
        do j = 1, nd
          write ( *, '(2x,f10.4)', advance = 'no' ) lpi(i,j)
        end do
        write ( *, '(a)' ) ''

      end do

      return
      end
      subroutine fem1d_lagrange_stiffness_test ( x_num, q_num )

c*********************************************************************72
c
cc FEM1D_LAGRANGE_STIFFNESS_TEST tests FEM1D_LAGRANGE_STIFFNESS.
c
c  Discussion:
c
c    The results are very sensitive to the quadrature rule.
c
c    In particular, if X_NUM points are used, the mass matrix will
c    involve integrals of polynomials of degree 2*(X_NUM-1), so the
c    quadrature rule should use at least Q_NUM = X_NUM - 1 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X_NUM, the number of nodes.
c
c    Input, integer Q_NUM, the number of quadrature points.
c
      implicit none

      integer x_num
      integer q_num

      double precision a(x_num,x_num)
      double precision b(x_num)
      double precision f
      external f
      integer i
      integer info
      integer j
      double precision k(x_num,x_num)
      double precision m(x_num,x_num)
      double precision u(x_num)
      double precision u_e(x_num)
      double precision x(x_num)
      double precision x_hi
      double precision x_lo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FEM1D_LAGRANGE_STIFFNESS_TEST'
      write ( *, '(a)' ) 
     &  '  FEM1D_LAGRANGE_STIFFNESS computes the stiffness matrix,'
      write ( *, '(a)' ) 
     &  '  the mass matrix, and right hand side vector for a'
      write ( *, '(a)' ) 
     &  '  finite element problem using Lagrange interpolation'
      write ( *, '(a)' ) '  basis polynomials.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Solving:'
      write ( *, '(a)' ) '    -u"+u=x on 0 < x < 1'
      write ( *, '(a)' ) '    u(0) = u(1) = 0'
      write ( *, '(a)' ) '  Exact solution:'
      write ( *, '(a)' ) '    u(x) = x - sinh(x)/sinh(1)'
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of mesh points = ', x_num
      write ( *, '(a,i4)' ) '  Number of quadrature points = ', q_num

      x_lo = 0.0D+00
      x_hi = 1.0D+00
      call r8vec_linspace ( x_num, x_lo, x_hi, x )

      call fem1d_lagrange_stiffness ( x_num, x, q_num, f, a, m, b )

      k = a + m

      do j = 1, x_num
        k(1,j) = 0.0D+00
      end do
      k(1,1) = 1.0D+00
      b(1) = 0.0D+00

      do j = 1, x_num
        k(x_num,j) = 0.0D+00
      end do
      k(x_num,x_num) = 1.0D+00
      b(x_num) = 0.0D+00

      do i = 1, x_num
        u(i) = b(i)
      end do

      call r8mat_fs ( x_num, k, u, info )

      call exact ( x_num, x, u_e )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '   I      X             U              U(exact)         Error'
      write ( *, '(a)' ) ''

      do i = 1, x_num
        write ( *, '(2x,i2,2x,f8.4,2x,g14.6,2x,g14.6,2x,g14.6)' )       
     &    i, x(i), u(i), u_e(i), abs ( u(i) - u_e(i) )
      end do

      return
      end
      function f ( x )

c*********************************************************************72
c
cc F evaluates the right hand side function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F, the value of the right hand side at X.
c
      implicit none

      double precision f
      double precision x

      f = x

      return
      end
      subroutine exact ( x_num, x, ue )

c*********************************************************************72
c
cc EXACT returns the exact solution.
c
c  Discussion:
c
c    The results are very sensitive to the quadrature rule.
c
c    In particular, if X_NUM points are used, the mass matrix will
c    involve integrals of polynomials of degree 2*(X_NUM-1), so the
c    quadrature rule should use at least Q_NUM = X_NUM - 1 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X_NUM, the number of nodes.
c
c    Input, double precision X(X_NUM), the nodes.
c
c    Output, double precision UE(X_NUM), the exact solution at the nodes.
c
      implicit none

      integer x_num

      integer i
      double precision ue(x_num)
      double precision x(x_num)

      do i = 1, x_num
        ue(i) = x(i) - sinh ( x(i) ) / sinh ( 1.0D+00 )
      end do

      return
      end

