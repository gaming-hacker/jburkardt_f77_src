      program main

c*********************************************************************72
c
cc LOBATTO_POLYNOMIAL_TEST tests the LOBATTO_POLYNOMIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_TEST:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the LOBATTO_POLYNOMIAL library.'

      call lobatto_polynomial_value_test ( )
      call lobatto_polynomial_derivative_test ( )
      call lobatto_polynomial_plot_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( );

      stop
      end
      subroutine lobatto_polynomial_value_test ( )

c*********************************************************************72
c
cc LOBATTO_POLYNOMIAL_VALUE_TEST tests LOBATTO_POLYNOMIAL_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      double precision e
      double precision fx1
      double precision fx2
      double precision l(1,n_max)
      integer m
      integer n
      integer n_data
      double precision x

      m = 1

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_VALUE_TEST:'
      write ( *, '(a)' ) '  LOBATTO_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the completed Lobatto polynomial L(n,x).'
      write ( *, '(a)' ) 
     &  '  LOBATTO_POLYNOMIAL_VALUE evaluates the Lobatto polynomial.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '                                       Tabulated' //
     &  '                 Computed'
      write ( *, '(a)' ) 
     &  '     N        X                        L(N,X)' //
     &  '                    L(N,X)        Error'
      write ( *, '(a)' ) ''

      n_data = 0

10    continue

        call lobatto_polynomial_values ( n_data, n, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call lobatto_polynomial_value ( m, n, x, l )

        fx2 = l(1,n)

        e = fx1 - fx2

        write ( *, '(2x,i4,2x,f12.4,2x,g24.16,2x,g24.16,2x,g8.1)' )     
     &    n, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine lobatto_polynomial_derivative_test ( )

c*********************************************************************72
c
cc LOBATTO_POLYNOMIAL_DERIVATIVE_TEST tests LOBATTO_POLYNOMIAL_DERIVATIVE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      double precision e
      double precision fx1
      double precision fx2
      double precision lp(1,n_max)
      integer m
      integer n
      integer n_data
      double precision x

      m = 1


      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_DERIVATIVE_TEST:'
      write ( *, '(a)' ) 
     &  '  LOBATTO_POLYNOMIAL_DERIVATIVES stores derivatives of'
      write ( *, '(a)' ) '  the completed Lobatto polynomial L(n,x).'
      write ( *, '(a)' ) 
     &  '  LOBATTO_POLYNOMIAL_DERIVATIVE evaluates the polynomial.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '                                       Tabulated' //
     &  '                 Computed'
      write ( *, '(a)' ) 
     &  '     N        X                        L''(N,X)' //
     &  '                   L''(N,X)       Error'
      write ( *, '(a)' ) ''

      n_data = 0

10    continue

        call lobatto_polynomial_derivatives ( n_data, n, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call lobatto_polynomial_derivative ( m, n, x, lp )
        fx2 = lp(1,n)

        e = fx1 - fx2

        write ( *, '(2x,i4,2x,f12.4,2x,g24.16,2x,g24.16,2x,g8.1)' )     
     &    n, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine lobatto_polynomial_plot_test ( )

c*********************************************************************72
c
cc LOBATTO_POLYNOMIAL_PLOT_TEST tests LOBATTO_POLYNOMIAL_PLOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ndx_num
      parameter ( ndx_num = 7 )

      integer ndx(ndx_num)
      character * ( 255 ) prefix

      save ndx

      data ndx / 1, 2, 3, 4, 5, 6, 7 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_PLOT_TEST:'
      write ( *, '(a)' ) 
     &  '  LOBATTO_POLYNOMIAL_PLOT plots Lobatto polynomials.'

      prefix = 'test'

      call lobatto_polynomial_plot ( ndx_num, ndx, prefix )

      return
      end
