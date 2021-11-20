      program main

c*********************************************************************72
c
cc MAIN is the main program for PADUA_PRB.
c
c  Discussion:
c
c    PADUA_PRB tests the PADUA library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PADUA_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the PADUA library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PADUA_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests PADUA_ORDER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer l
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  PADUA_ORDER converts the level L into the'
      write ( *, '(a)' ) '  order N of any Padua rule.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     L         N'
      write ( *, '(a)' ) ' '

      do l = 0, 10
        call padua_order ( l, n )
        write ( *, '(2x,i4,2x,i8)' ) l, n
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests PADUA_POINTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer l
      character * ( 80 ) label
      integer n
      double precision xy(2,66)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  PADUA_POINTS returns the points of a Padua rule.'

      do l = 0, 10
        call padua_order ( l, n )
        call padua_points ( l, xy )
        write ( label, '(a,i2,a)' ) '  Level ', l, ' Padua points:'
        call r8mat_transpose_print ( 2, n, xy, label )
      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests PADUA_PLOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character * ( 255 ) filename
      integer l
     
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  PADUA_PLOT plots the Padua points.'

      filename = 'padua_00'

      do l = 0, 10
        call padua_plot ( l, filename )
        call filename_inc ( filename )
      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests PADUA_POINTS and PADUA_POINTS_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 15 )

      integer j
      integer l
      integer n
      double precision xy1(2,n_max)
      double precision x2(n_max)
      double precision y2(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) 
     &  '  PADUA_POINTS computes the points of a Padua rule.'
      write ( *, '(a)' ) '  PADUA_POINTS_SET looks them up in a table.'
 
      do l = 3, 4
        call padua_order ( l, n )
        call padua_points ( l, xy1 )
        call padua_points_set ( l, x2, y2 )
        write ( *, '(a)' ) ' '
        write ( *, '(a,i1,a)' ) '  Level ', l, '  Padua points'
        write ( *, '(a)' ) ' '
        do j = 1, n
          write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' )
     &      j, xy1(1,j), xy1(2,j)
          write ( *, '(2x,4x,2x,g14.6,2x,g14.6)' )
     &         x2(j), y2(j)
        end do
      end do

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests PADUA_WEIGHTS and PADUA_WEIGHTS_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 15 )

      double precision diff
      integer j
      integer l
      integer n
      double precision w1(n_max)
      double precision w2(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) 
     &  '  PADUA_WEIGHTS computes Padua quadrature weights.'
      write ( *, '(a)' ) '  PADUA_WEIGHTS_SET looks them up in a table.'
 
      do l = 3, 4
        call padua_order ( l, n )
        call padua_weights ( l, w1 )
        call padua_weights_set ( l, w2 )
        write ( *, '(a)' ) ' '
        write ( *, '(a,i1,a)' ) '  Level ', l, '  Padua points'
        write ( *, '(a)' ) ' '
        diff = 0.0D+00
        do j = 1, n
          write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) j, w1(j), w2(j)
          diff = max ( diff, abs ( w1(j) - w2(j) ) )
        end do
        write ( *, '(a)' ) ''
        write ( *, '(a,e10.2)' ) '  Maximum difference = ', diff
      end do

      return
      end
