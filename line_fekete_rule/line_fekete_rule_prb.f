      program main

c*********************************************************************72
c
cc MAIN is the main program for LINE_FEKETE_RULE_PRB.
c
c  Discussion:
c
c    LINE_FEKETE_RULE_PRB tests the LINE_FEKETE_RULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 3 )

      integer m
      integer m_test(test_num)
      integer test

      save m_test

      data m_test /
     &  5, 11, 21 /

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_FEKETE_RULE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the LINE_FEKETE_RULE library.'

      do test = 1, test_num
        m = m_test(test)
        call test01 ( m )
      end do

      do test = 1, test_num
        m = m_test(test)
        call test02 ( m )
      end do

      do test = 1, test_num
        m = m_test(test)
        call test03 ( m )
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_FEKETE_RULE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( m )

c*********************************************************************72
c
cc TEST01 seeks Fekete points in [-1,+1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Alvise Sommariva, Marco Vianello,
c    Computing approximate Fekete points by QR factorizations of Vandermonde 
c    matrices,
c    Computers and Mathematics with Applications,
c    Volume 57, 2009, pages 1324-1336.
c
c  Parameters:
c
c    Input, integer M, the dimension of the polynomial space.
c
      implicit none

      integer m
      integer n
      parameter ( n = 5001 )

      double precision a
      double precision b
      integer nf
      double precision r8vec_sum
      double precision wf(m)
      double precision wf_sum
      double precision x(n)
      double precision xf(m)

      a = -1.0D+00
      b = +1.0D+00
      call r8vec_linspace ( n, a, b, x )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a,g14.6,a,g14.6,a)' ) 
     &  '  Seek Fekete points in [', a, ',', b, ']'
      write ( *, '(a,i6,a)' ) 
     &  '  using ', n, ' equally spaced sample points'
      write ( *, '(a,i6)' ) '  for polynomials of degree M = ', m
      write ( *, '(a)' ) 
     &  '  using the monomial basis and uniform weight.'

      call line_fekete_monomial ( m, a, b, n, x, nf, xf, wf )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  NF = ', nf
      call r8vec_print ( nf, xf, '  Estimated Fekete points XF:' )
      wf_sum = r8vec_sum ( nf, wf )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Sum(WF) = ', wf_sum

      return
      end
      subroutine test02 ( m )

c*********************************************************************72
c
cc TEST02 seeks Fekete points in [-1,+1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    L Bos, N Levenberg,
c    On the calculation of approximate Fekete points: the univariate case,
c    Electronic Transactions on Numerical Analysis,
c    Volume 30, pages 377-397, 2008.
c
c  Parameters:
c
c    Input, integer M, the dimension of the polynomial space.
c
      implicit none

      integer m
      integer n
      parameter ( n = 5001 )

      double precision a
      double precision b
      integer nf
      double precision r8vec_sum
      double precision wf(m)
      double precision wf_sum
      double precision x(n)
      double precision xf(m)

      a = -1.0D+00
      b = +1.0D+00
      call r8vec_linspace ( n, a, b, x )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a,g14.6,a,g14.6,a)' ) 
     &  '  Seek Fekete points in [', a, ',', b, ']'
      write ( *, '(a,i6,a)' ) 
     &  '  using ', n, ' equally spaced sample points'
      write ( *, '(a,i6)' ) '  for polynomials of degree M = ', m
      write ( *, '(a)' ) '  with the Chebyshev basis.'

      call line_fekete_chebyshev ( m, a, b, n, x, nf, xf, wf )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  NF = ', nf
      call r8vec_print ( nf, xf, '  Estimated Fekete points XF:' )
      wf_sum = r8vec_sum ( nf, wf )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Sum(WF) = ', wf_sum

      return
      end
      subroutine test03 ( m )

c*********************************************************************72
c
cc TEST03 seeks Fekete points in [-1,+1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the dimension of the polynomial space.
c
      implicit none

      integer m
      integer n
      parameter ( n = 5001 )

      double precision a
      double precision b
      integer nf
      double precision r8vec_sum
      double precision wf(m)
      double precision wf_sum
      double precision x(n)
      double precision xf(m)

      a = -1.0D+00
      b = +1.0D+00
      call r8vec_linspace ( n, a, b, x )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a,g14.6,a,g14.6,a)' ) 
     &  '  Seek Fekete points in [', a, ',', b, ']'
      write ( *, '(a,i6,a)' ) 
     &  '  using ', n, ' equally spaced sample points'
      write ( *, '(a,i6)' ) '  for polynomials of degree M = ', m
      write ( *, '(a)' ) 
     &  '  with the Legendre basis and uniform weight.'

      call line_fekete_legendre ( m, a, b, n, x, nf, xf, wf )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  NF = ', nf
      call r8vec_print ( nf, xf, '  Estimated Fekete points XF:' )
      wf_sum = r8vec_sum ( nf, wf )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Sum(WF) = ', wf_sum

      return
      end

