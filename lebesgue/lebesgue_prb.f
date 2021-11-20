      program main

c*********************************************************************72
c
cc MAIN is the main program for LEBESGUE_PRB.
c
c  Discussion:
c
c    LEBESGUE_PRB tests the LEBESGUE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2014
c
c  Author:
c
c    John Burkardt
c
      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the LEBESGUE library.'

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
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc LEBESGUE_TEST01 looks at Chebyshev1 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )
      integer nfun
      parameter ( nfun = 501 )

      character * ( 255 ) filename
      double precision l(n_max)
      character * ( 255 ) label
      integer n
      double precision x(n_max)
      double precision xfun(nfun)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_TEST01:'
      write ( *, '(a)' ) '  Analyze Chebyshev1 points.'

      call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

      do n = 1, n_max

        call chebyshev1 ( n, x )
        call lebesgue_constant ( n, x, nfun, xfun, l(n) )
      end do

      call r8vec_print ( n_max, l, 
     &  '  Chebyshev1 Lebesgue constants for N = 1 to 11:' )
c
c  Examine one case more closely.
c
      n = 11
      call chebyshev1 ( n, x )
      call r8vec_print ( n, x, '  Chebyshev1 points for N = 11' )

      label = 'Chebyshev1 points for N = 11'
      filename = 'chebyshev1'
      call lebesgue_plot ( n, x, nfun, xfun, label, filename )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc LEBESGUE_TEST02 looks at Chebyshev2 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )
      integer nfun
      parameter ( nfun = 501 )

      character * ( 255 ) filename
      double precision l(n_max)
      character * ( 255 ) label
      integer n
      double precision x(n_max)
      double precision xfun(nfun)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_TEST02:'
      write ( *, '(a)' ) '  Analyze Chebyshev2 points.'

      call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

      do n = 1, n_max
        call chebyshev2 ( n, x )
        call lebesgue_constant ( n, x, nfun, xfun, l(n) )
      end do

      call r8vec_print ( n_max, l, 
     &  '  Chebyshev2 Lebesgue constants for N = 1 to 11:' )
c
c  Examine one case more closely.
c
      n = 11
      call chebyshev2 ( n, x )
      call r8vec_print ( n, x, '  Chebyshev2 points for N = 11' )

      label = 'Chebyshev2 points for N = 11'
      filename = 'chebyshev2'
      call lebesgue_plot ( n, x, nfun, xfun, label, filename )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc LEBESGUE_TEST03 looks at Chebyshev3 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )
      integer nfun
      parameter ( nfun = 501 )

      character * ( 255 ) filename
      double precision l(n_max)
      character * ( 255 ) label
      integer n
      double precision x(n_max)
      double precision xfun(nfun)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_TEST03:'
      write ( *, '(a)' ) '  Analyze Chebyshev3 points.'

      call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

      do n = 1, n_max
        call chebyshev3 ( n, x )
        call lebesgue_constant ( n, x, nfun, xfun, l(n) )
      end do

      call r8vec_print ( n_max, l, 
     &  '  Chebyshev3 Lebesgue constants for N = 1 to 11:' )
c
c  Examine one case more closely.
c
      n = 11
      call chebyshev3 ( n, x )
      call r8vec_print ( n, x, '  Chebyshev3 points for N = 11' )

      label = 'Chebyshev3 points for N = 11'
      filename = 'chebyshev3'
      call lebesgue_plot ( n, x, nfun, xfun, label, filename )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc LEBESGUE_TEST04 looks at Chebyshev4 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )
      integer nfun
      parameter ( nfun = 501 )

      character * ( 255 ) filename
      double precision l(n_max)
      character * ( 255 ) label
      integer n
      double precision x(n_max)
      double precision xfun(nfun)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_TEST04:'
      write ( *, '(a)' ) '  Analyze Chebyshev4 points.'

      call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

      do n = 1, n_max
        call chebyshev4 ( n, x )
        call lebesgue_constant ( n, x, nfun, xfun, l(n) )
      end do

      call r8vec_print ( n_max, l, 
     &  '  Chebyshev4 Lebesgue constants for N = 1 to 11:' )
c
c  Examine one case more closely.
c
      n = 11
      call chebyshev4 ( n, x )
      call r8vec_print ( n, x, '  Chebyshev4 points for N = 11' )

      label = 'Chebyshev4 points for N = 11'
      filename = 'chebyshev4'
      call lebesgue_plot ( n, x, nfun, xfun, label, filename )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc LEBESGUE_TEST05 looks at Equidistant1 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )
      integer nfun
      parameter ( nfun = 501 )

      character * ( 255 ) filename
      double precision l(n_max)
      character * ( 255 ) label
      integer n
      double precision x(n_max)
      double precision xfun(nfun)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_TEST05:'
      write ( *, '(a)' ) '  Analyze Equidistant1 points.'

      call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

      do n = 1, n_max
        call equidistant1 ( n, x )
        call lebesgue_constant ( n, x, nfun, xfun, l(n) )
      end do

      call r8vec_print ( n_max, l, 
     &  '  Equidistant1 Lebesgue constants for N = 1 to 11:' )
c
c  Examine one case more closely.
c
      n = 11
      call equidistant1 ( n, x )
      call r8vec_print ( n, x, '  Equidistant1 points for N = 11' )

      label = 'Equidistant1 points for N = 11'
      filename = 'equidistant1'
      call lebesgue_plot ( n, x, nfun, xfun, label, filename )

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc LEBESGUE_TEST06 looks at Equidistant2 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )
      integer nfun
      parameter ( nfun = 501 )

      character * ( 255 ) filename
      double precision l(n_max)
      character * ( 255 ) label
      integer n
      double precision x(n_max)
      double precision xfun(nfun)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_TEST06:'
      write ( *, '(a)' ) '  Analyze Equidistant2 points.'

      call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

      do n = 1, n_max
        call equidistant2 ( n, x )
        call lebesgue_constant ( n, x, nfun, xfun, l(n) )
      end do

      call r8vec_print ( n_max, l, 
     &  '  Equidistant2 Lebesgue constants for N = 1 to 11:' )
c
c  Examine one case more closely.
c
      n = 11
      call equidistant2 ( n, x )
      call r8vec_print ( n, x, '  Equidistant2 points for N = 11' )

      label = 'Equidistant2 points for N = 11'
      filename = 'equidistant2'
      call lebesgue_plot ( n, x, nfun, xfun, label, filename )

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc LEBESGUE_TEST07 looks at Equidistant3 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )
      integer nfun
      parameter ( nfun = 501 )

      character * ( 255 ) filename
      double precision l(n_max)
      character * ( 255 ) label
      integer n
      double precision x(n_max)
      double precision xfun(nfun)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_TEST07:'
      write ( *, '(a)' ) '  Analyze Equidistant3 points.'

      call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

      do n = 1, n_max
        call equidistant3 ( n, x )
        call lebesgue_constant ( n, x, nfun, xfun, l(n) )
      end do

      call r8vec_print ( n_max, l, 
     &  '  Equidistant3 Lebesgue constants for N = 1 to 11:' )
c
c  Examine one case more closely.
c
      n = 11
      call equidistant3 ( n, x )
      call r8vec_print ( n, x, '  Equidistant3 points for N = 11' )

      label = 'Equidistant3 points for N = 11'
      filename = 'equidistant3'
      call lebesgue_plot ( n, x, nfun, xfun, label, filename )

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc LEBESGUE_TEST08 looks at Fejer 1 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )
      integer nfun
      parameter ( nfun = 501 )

      character * ( 255 ) filename
      double precision l(n_max)
      character * ( 255 ) label
      integer n
      double precision x(n_max)
      double precision xfun(nfun)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_TEST08:'
      write ( *, '(a)' ) '  Analyze Fejer1 points.'

      call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

      do n = 1, n_max
        call fejer1 ( n, x )
        call lebesgue_constant ( n, x, nfun, xfun, l(n) )
      end do

      call r8vec_print ( n_max, l, 
     &  '  Fejer1 Lebesgue constants for N = 1 to 11:' )
c
c  Examine one case more closely.
c
      n = 11
      call fejer1 ( n, x )
      call r8vec_print ( n, x, '  Fejer1 points for N = 11' )

      label = 'Fejer1 points for N = 11'
      filename = 'fejer1'
      call lebesgue_plot ( n, x, nfun, xfun, label, filename )

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc LEBESGUE_TEST09 looks at Fejer2 points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )
      integer nfun
      parameter ( nfun = 501 )

      character * ( 255 ) filename
      double precision l(n_max)
      character * ( 255 ) label
      integer n
      double precision x(n_max)
      double precision xfun(nfun)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEBESGUE_TEST09:'
      write ( *, '(a)' ) '  Analyze Fejer2 points.'

      call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

      do n = 1, n_max
        call fejer2 ( n, x )
        call lebesgue_constant ( n, x, nfun, xfun, l(n) )
      end do

      call r8vec_print ( n_max, l, 
     &  '  Fejer2 Lebesgue constants for N = 1 to 11:' )
c
c  Examine one case more closely.
c
      n = 11
      call fejer2 ( n, x )
      call r8vec_print ( n, x, '  Fejer2 points for N = 11' )

      label = 'Fejer2 points for N = 11'
      filename = 'fejer2'
      call lebesgue_plot ( n, x, nfun, xfun, label, filename )

      return
      end

