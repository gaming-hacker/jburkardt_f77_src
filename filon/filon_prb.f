      program main

c*********************************************************************72
c
cc MAIN is the main program for FILON_PRB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILON_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FILON library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILON_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests FILON_TAB_COS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a
      double precision b
      double precision error
      double precision exact
      double precision ftab(n)
      integer i
      integer j
      integer k
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision result
      double precision t
      double precision x(n)

      a = 0.0D+00
      b = 2.0D+00 * r8_pi
c
c  Set the X values.
c
      do i = 1, n
        x(i) = ( dble ( n - i     ) * a   
     &         + dble (     i - 1 ) * b ) 
     &         / dble ( n     - 1 )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  FILON_TAB_COS estimates the integral of.'
      write ( *, '(a)' ) '  F(X) * COS ( T * X )'
      write ( *, '(a)' ) '  Use integrands F(X)=1, X, X^2.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g24.16)' ) '  A = ', a
      write ( *, '(a,g24.16)' ) '  B = ', b
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       T                      Approximate             Exact'

      do k = 1, 3

        if ( k .eq. 1 ) then
          t = 1.0D+00
        else if ( k .eq. 2 ) then
          t = 2.0D+00
        else if ( k .eq. 3 ) then
          t = 10.0D+00
        end if

        write ( *, '(a)' ) ' '

        do i = 1, 3

          if ( i .eq. 1 ) then
            call zero_integrand ( n, x, ftab )
          else if ( i .eq. 2 ) then
            call one_integrand ( n, x, ftab )
          else if ( i .eq. 3 ) then
            call two_integrand ( n, x, ftab )
          end if

          call filon_tab_cos ( n, ftab, a, b, t, result )

          if ( i .eq. 1 ) then
            exact = ( sin ( t * b ) - sin ( t * a ) ) / t
          else if ( i .eq. 2 ) then
            exact = ( ( cos ( t * b ) + t * b * sin ( t * b ) ) 
     &              - ( cos ( t * a ) + t * a * sin ( t * a ) ) ) 
     &              / t**2
          else if ( i .eq. 3 ) then
            exact = ( ( 2.0D+00 * t * b * cos ( t * b ) 
     &            + ( t * t * b**2 - 2.0D+00 ) * sin ( t * b ) ) 
     &              - ( 2.0D+00 * t * a * cos ( t * a ) 
     &            + ( t * t * a**2 - 2.0D+00 ) * sin ( t * a ) ) ) 
     &            / t**3
          end if

          write ( *, '(2x,3g24.16)' ) t, result, exact

        end do

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests FILON_TAB_COS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 641 )

      double precision a
      double precision b
      double precision error
      double precision exact
      double precision ftab(n_max)
      integer i
      integer j
      integer k
      integer n
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision result
      double precision t
      double precision x(n_max)
c
c  Example suggested by James Roedder.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Integrate F(X) = log(1+X)*cos(T*X):'
      write ( *, '(a)' ) '  Supply integrand as a table.'
      write ( *, '(a)' ) '  T = 10, and N increases'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    Approximate             ' //
     &  'Exact                   Error'
      write ( *, '(a)' ) ' '

      a = 0.0D+00
      b = 2.0D+00 * r8_pi

      do j = 1, 6

        n = 2 ** j * 10 + 1
c
c  Set the X values.
c
        do i = 1, n
          x(i) = ( dble ( n - i     ) * a   
     &           + dble (     i - 1 ) * b ) 
     &           / dble ( n     - 1 )
        end do

        call log_integrand ( n, x, ftab )
     
        t = 10.0D+00

        call filon_tab_cos ( n, ftab, a, b, t, result )

        exact = -0.008446594405D+00
        error = result - exact

        write ( *, '(2x,i6,g24.16,g24.16,g16.8)' ) 
     &    n, result, exact, error

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests FILON_FUN_COS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision error
      double precision exact
      external log_integrand
      integer i
      integer j
      integer k
      integer n
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision result
      double precision t
c
c  Example suggested by James Roedder.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Integrate F(X)=log(1+X)*cos(T*X):'
      write ( *, '(a)' ) '  Supply integrand as a function.'
      write ( *, '(a)' ) '  T = 10, and N increases'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    Approximate             ' //
     &  'Exact                   Error'
      write ( *, '(a)' ) ' '

      a = 0.0D+00
      b = 2.0D+00 * r8_pi

      do j = 1, 6

        n = 2 ** j * 10 + 1
     
        t = 10.0D+00

        call filon_fun_cos ( n, log_integrand, a, b, t, result )

        exact = -0.008446594405D+00
        error = result - exact

        write ( *, '(2x,i6,g24.16,g24.16,g16.8)' ) 
     &    n, result, exact, error

      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests FILON_TAB_SIN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a
      double precision b
      double precision error
      double precision exact
      double precision ftab(n)
      integer i
      integer j
      integer k
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision result
      double precision t
      double precision x(n)

      a = 0.0D+00
      b = 2.0D+00 * r8_pi
c
c  Set the X values.
c
      do i = 1, n
        x(i) = ( dble ( n - i     ) * a   
     &         + dble (     i - 1 ) * b ) 
     &         / dble ( n     - 1 )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  FILON_TAB_SIN estimates the integral of.'
      write ( *, '(a)' ) '  F(X) * SIN ( T * X )'
      write ( *, '(a)' ) '  Use integrands 1, X, X^2.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g24.16)' ) '  A = ', a
      write ( *, '(a,g24.16)' ) '  B = ', b
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       T                      Approximate             Exact'
      write ( *, '(a)' ) ' '

      do k = 1, 3

        if ( k .eq. 1 ) then
          t = 1.0D+00
        else if ( k .eq. 2 ) then
          t = 2.0D+00
        else if ( k .eq. 3 ) then
          t = 10.0D+00
        end if

        write ( *, '(a)' ) ' '

        do i = 1, 3

          if ( i .eq. 1 ) then
            call zero_integrand ( n, x, ftab )
          else if ( i .eq. 2 ) then
            call one_integrand ( n, x, ftab )
          else if ( i .eq. 3 ) then
            call two_integrand ( n, x, ftab )
          end if

          call filon_tab_sin ( n, ftab, a, b, t, result )

          if ( i .eq. 1 ) then
            exact = ( - cos ( t * b ) + cos ( t * a ) ) / t
          else if ( i .eq. 2 ) then
            exact = ( ( sin ( t * b ) - t * b * cos ( t * b ) ) 
     &              - ( sin ( t * a ) - t * a * cos ( t * a ) ) ) 
     &              / t**2
          else if ( i .eq. 3 ) then
            exact = ( ( 2.0D+00 * t * b * sin ( t * b ) 
     &              + ( 2.0D+00 - t**2 * b**2 ) * cos ( t * b ) ) 
     &              - ( 2.0D+00 * t * a * sin ( t * a ) 
     &              + ( 2.0D+00 - t**2 * a**2 ) * cos ( t * a ) ) ) 
     &              / t**3
          end if

          write ( *, '(2x,3g24.16)' ) t, result, exact

        end do

      end do

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests FILON_TAB_COS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 641 )

      double precision a
      double precision b
      double precision error
      double precision exact
      double precision ftab(n_max)
      integer i
      integer j
      integer k
      integer n
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision result
      double precision t
      double precision x(n_max)
c
c  Example suggested by James Roedder.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  Integrate F(X)=log(1+X)*sin(T*X):'
      write ( *, '(a)' ) '  Supply integrand as a table.'
      write ( *, '(a)' ) '  T = 10, and N increases'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    Approximate             ' //
     &  'Exact                   Error'
      write ( *, '(a)' ) ' '

      a = 0.0D+00
      b = 2.0D+00 * r8_pi

      do j = 1, 6

        n = 2 ** j * 10 + 1
c
c  Set the X values.
c
        do i = 1, n
          x(i) = ( dble ( n - i     ) * a   
     &           + dble (     i - 1 ) * b ) 
     &           / dble ( n     - 1 )
        end do

        call log_integrand ( n, x, ftab )

        t = 10.0D+00

        call filon_tab_sin ( n, ftab, a, b, t, result )

        exact = -0.19762680771872D+00
        error = result - exact

        write ( *, '(2x,i6,g24.16,g24.16,g16.8)' ) 
     &     n, result, exact, error

      end do

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests FILON_FUN_COS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision error
      double precision exact
      external log_integrand
      integer i
      integer j
      integer k
      integer n
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision result
      double precision t
c
c  Example suggested by James Roedder.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  Integrate F(X)=log(1+X)*sin(T*X):'
      write ( *, '(a)' ) '  Supply integrand as a function.'
      write ( *, '(a)' ) '  T = 10, and N increases'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    Approximate             Exact' // 
     &  '                   Error'
      write ( *, '(a)' ) ' '

      a = 0.0D+00
      b = 2.0D+00 * r8_pi

      do j = 1, 6

        n = 2 ** j * 10 + 1

        t = 10.0D+00

        call filon_fun_sin ( n, log_integrand, a, b, t, result )

        exact = -0.19762680771872D+00
        error = result - exact

        write ( *, '(2x,i6,g24.16,g24.16,g16.8)' ) 
     &    n, result, exact, error

      end do

      return
      end
      subroutine zero_integrand ( n, x, fx )

c*********************************************************************72
c
cc ZERO_INTEGRAND evaluates the integrand x^0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the evaluation points.
c
c    Output, double precision FX(N), the function values.
c
      implicit none

      integer n

      double precision fx(n)
      integer i
      double precision x(n)

      do i = 1, n
        fx(i) = 1.0D+00
      end do

      return
      end
      subroutine one_integrand ( n, x, fx )

c*********************************************************************72
c
cc ONE_INTEGRAND evaluates the integrand X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the evaluation points.
c
c    Output, double precision FX(N), the function values.
c
      implicit none

      integer n

      double precision fx(n)
      integer i
      double precision x(n)

      do i = 1, n
        fx(i) = x(i)
      end do

      return
      end
      subroutine two_integrand ( n, x, fx )

c*********************************************************************72
c
cc TWO_INTEGRAND evaluates the integrand X^2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the evaluation points.
c
c    Output, double precision FX(N), the function values.
c
      implicit none

      integer n

      double precision fx(n)
      integer i
      double precision x(n)

      do i = 1, n
        fx(i) = x(i) ** 2
      end do

      return
      end
      subroutine log_integrand ( n, x, fx )

c*********************************************************************72
c
cc LOG_INTEGRAND evaluates the logarithmic integrand.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the evaluation points.
c
c    Output, double precision FX(N), the function values.
c
      implicit none

      integer n

      double precision fx(n)
      integer i
      double precision x(n)

      do i = 1, n
        fx(i) = log ( 1.0D+00 + x(i) )
      end do

      return
      end
