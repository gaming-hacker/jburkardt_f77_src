      program main

c*********************************************************************72
c
cc MAIN is the main program for DIFFER_PRB.
c
c  Discussion:
c
c    DIFFER_PRB tests the DIFFER library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIFFER_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the DIFFER library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIFFER_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests DIFFER_MATRIX.
c
c  Discussion:
c
c    DIFFER_MATRIX computes a modified Vandermonde matrix A1.
c
c    The solution of a system A1 * X1 = B is related to the solution
c    of the system A2 * X2 = B, where A2 is the standard Vandermonde
c    matrix, simply by X2(I) = X1(I) * A(I,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    28 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision b(n)
      integer i
      integer info
      integer job
      double precision stencil(n)
      double precision x1(n)
      double precision x2(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Demonstrate that the DIFFER matrix is "really"'
      write ( *, '(a)' ) '  a Vandermonde matrix.'

      stencil(1) = 2.5
      stencil(2) = 3.3
      stencil(3) = -1.3
      stencil(4) = 0.5

      x1(1) = 1.0
      x1(2) = 2.0
      x1(3) = 3.0
      x1(4) = 4.0

      call differ_matrix ( n, stencil, a )
      call r8mat_print ( n, n, a, '  Stencil matrix:' )
      call r8mat_mv ( n, n, a, x1, b )
c
c  Set up and solve the DIFFER system.
c
      do i = 1, n
        x1(i) = b(i)
      end do

      call r8mat_fs ( n, a, x1, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST01 - Warning!'
        write ( *, '(a)' ) '  DIFFER system is singular.'
        return
      end if

      call r8vec_print ( n, x1, '  Solution of DIFFER system:' )
c
c  R8VM_SL solves the related Vandermonde system.
c  A simple transformation gives us the solution to the DIFFER system.
c
      job = 0
      call r8vm_sl ( n, stencil, b, x2, job, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST01 - Warningc'
        write ( *, '(a)' ) '  VANDERMONDE system is singular.'
        return
      end if

      call r8vec_print ( n, x2, '  Solution of VANDERMONDE system:' )

      do i = 1, n
        x2(i) = x2(i) / stencil(i)
      end do

      call r8vec_print ( n, x2, 
     &  '  Transformed solution of VANDERMONDE system:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests DIFFER_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 8 )

      double precision a(n_max,n_max)
      double precision b(n_max,n_max)
      double precision err
      integer n
      integer seed
      integer test
      double precision x(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  DIFFER_INVERSE returns the inverse of a DIFFER matrix;'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   N    Inverse error'

      seed = 123456789;

      do n = 2, n_max

        write ( *, '(a)' ) ''

        do test = 1, 5
          call r8vec_uniform_01 ( n, seed, x )
          call differ_matrix ( n, x, a )
          call differ_inverse ( n, x, b )
          call inverse_error ( n, a, b, err )
          write ( *, '(2x,i2,2x,g14.6)' ) n, err
        end do

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests DIFFER_MATRIX.
c
c  Discussion:
c
c    Reproduce a specific example.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    28 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision b(n)
      double precision c(n)
      double precision df
      double precision dfdx
      double precision dx
      integer i
      integer info
      integer order
      double precision stencil(n)
      double precision x(n)
      double precision x0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Reproduce a specific example.'
c
c  Compute the coefficients for a specific stencil.
c
      stencil(1) = -3.0
      stencil(2) = -2.0
      stencil(3) = -1.0
      stencil(4) = 1.0

      order = 1
      do i = 1, n
        b(i) = 0.0
      end do
      b(order) = 1.0

      call differ_matrix ( n, stencil, a )

      do i = 1, n
        c(i) = b(i)
      end do

      call r8mat_fs ( n, a, c, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST03 - Warningc'
        write ( *, '(a)' ) '  DIFFER system is singular.'
        return
      end if

      call r8vec_print ( n, c, '  Solution of DIFFER system:' )
c
c  Use the coefficients C to estimate the first derivative of EXP(X)
c  at X0, using a spacing of DX = 0.1.
c
      x0 = 1.3D+00
      dx = 0.1D+00
      df = 0.0D+00
      do i = 1, n
        df = df + c(i) * ( exp ( x0 + stencil(i) * dx ) - exp ( x0 ) )
      end do
      dfdx = df / dx

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  DFDX =         ', dfdx
      write ( *, '(a,g14.6)' ) '  d exp(x) /dx = ', exp ( x0 )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests DIFFER_FORWARD, DIFFER_BACKWARD, DIFFER_CENTRAL.
c
c  Discussion:
c
c    Evaluate the coefficients for uniformly spaced finite difference
c    approximations of derivatives.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 7 )

      double precision c(n_max)
      double precision h
      character * ( 80 ) label
      integer n
      integer o
      integer p
      double precision x(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  DIFFER_FORWARD,'
      write ( *, '(a)' ) '  DIFFER_BACKWARD, and'
      write ( *, '(a)' ) 
     &  '  DIFFER_CENTRAL produce coefficients for difference'
      write ( *, '(a)' ) '  approximations of the O-th derivative,'
      write ( *, '(a)' ) 
     &  '  with error of order H^P, for a uniform spacing of H.'

      h = 1.0D+00
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6,a)' ) 
     &  '  Use a spacing of H = ', h, ' for all examples.'
c
c  Forward difference approximation to the third derivative with error of O(h).
c
      o = 3
      p = 1
      n = o + p
      call differ_forward ( h, o, p, c, x )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Forward difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Backward difference approximation to the third derivative with error of O(h).
c
      o = 3
      p = 1
      n = o + p
      call differ_backward ( h, o, p, c, x )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Backward difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Central difference approximation to the third derivative with error of O(h^2).
c
      o = 3
      p = 2
      n = o + p
      call differ_central ( h, o, p, c, x )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Central difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Central difference approximation to the third derivative with error of O(h^4).
c
      o = 3
      p = 4
      n = o + p
      call differ_central ( h, o, p, c, x )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Central difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Forward difference approximation to the fourth derivative with error of O(h).
c
      o = 4
      p = 1
      n = o + p
      call differ_forward ( h, o, p, c, x )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Forward difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Backward difference approximation to the fourth derivative with error of O(h).
c
      o = 4
      p = 1
      n = o + p
      call differ_backward ( h, o, p, c, x )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Backward difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Central difference approximation to the fourth derivative with error of O(h^3).
c
      o = 4
      p = 3
      n = o + p
      call differ_central ( h, o, p, c, x )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Central difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests DIFFER_STENCIL.
c
c  Discussion:
c
c    Use DIFFER_STENCIL to reproduce forward, backward and central differences.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 7 )

      double precision c(n_max)
      double precision h
      integer i
      character * ( 80 ) label
      integer n
      integer o
      integer p
      double precision x(n_max)
      double precision x0

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) 
     &  '  DIFFER_STENCIL produces coefficients for difference'
      write ( *, '(a)' ) '  approximations of the O-th derivative,'
      write ( *, '(a)' ) 
     &  '  using arbitrarily spaced data, with maximum spacing H'
      write ( *, '(a)' ) '  with error of order H^P.'
c
c  Let X0 = 0.
c
      x0 = 0.0D+00
      h = 1.0D+00
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  For all tests, let X0 = ', x0
      write ( *, '(a,g14.6,a)' ) '  and use a uniformly spacing of ', h
      write ( *, '(a)' ) '  so we can compare with previous results.'
c
c  Forward difference approximation to the third derivative with error of O(h).
c
      o = 3
      p = 1
      n = o + p
      do i = 1, n
        x(i) = dble ( i - 1 ) * h
      end do
      call differ_stencil ( x0, o, p, x, c )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Forward difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Backward difference approximation to the third derivative with error of O(h).
c
      o = 3
      p = 1
      n = o + p
      do i = 1, n
        x(i) = dble ( i - n ) * h
      end do
      call differ_stencil ( x0, o, p, x, c )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Backward difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Central difference approximation to the third derivative with error of O(h^2).
c
      o = 3
      p = 2
      n = o + p
      do i = 1, n
        x(i) = dble ( - n - 1 + 2 * i ) * h / 2.0D+00
      end do
      call differ_stencil ( x0, o, p, x, c )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Central difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Central difference approximation to the third derivative with error of O(h^4).
c
      o = 3
      p = 4
      n = o + p
      do i = 1, n
        x(i) = dble ( - n - 1 + 2 * i ) * h / 2.0D+00
      end do
      call differ_stencil ( x0, o, p, x, c )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Central difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Forward difference approximation to the fourth derivative with error of O(h).
c
      o = 4
      p = 1
      n = o + p
      do i = 1, n
        x(i) = dble ( i - 1 ) * h
      end do
      call differ_stencil ( x0, o, p, x, c )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Forward difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Backward difference approximation to the fourth derivative with error of O(h).
c
      o = 4
      p = 1
      n = o + p
      do i = 1, n
        x(i) = dble ( i - n ) * h
      end do
      call differ_stencil ( x0, o, p, x, c )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Backward difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )
c
c  Central difference approximation to the fourth derivative with error of O(h^3).
c
      o = 4
      p = 3
      n = o + p
      do i = 1, n
        x(i) = dble ( - n - 1 + 2 * i ) * h / 2.0D+00
      end do
      call differ_stencil ( x0, o, p, x, c )
      write ( label, '(a,i2,a,i2)' ) 
     &  '  Central difference coefficients, O = ', o, ', P = ', p
      call r8vec2_print ( n, x, c, label )

      return
      end
