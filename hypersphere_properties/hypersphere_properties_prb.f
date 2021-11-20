      program main

c*********************************************************************72
c
cc MAIN is the main program for HYPERSPHERE_PROPERTIES_PRB.
c
c  Discussion:
c
c    HYPERSPHERE_PROPERTIES_PRB tests the HYPERSPHERE_PROPERTIES library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HYPERSPHERE_PROPERTIES_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the HYPERSPHERE_PROPERTIES library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HYPERSPHERE_PROPERTIES_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests the coordinate conversion routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m_max
      parameter ( m_max = 5 )
      integer n
      parameter ( n = 1 )

      double precision c(m_max)
      double precision err
      integer m
      double precision r(n)
      double precision r8mat_norm_fro_affine
      integer seed
      integer test
      double precision theta(m_max-1,n)
      double precision x(m_max,n)
      double precision x2(m_max,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Test the coordinate conversion routines:'
      write ( *, '(a)' ) 
     &  '  CARTESIAN_TO_HYPERSPHERE: X       -> R,Theta'
      write ( *, '(a)' ) '  HYPERSPHERE_TO_CARTESIAN: R,Theta -> X.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Pick a random X, and compute X2 by converting X'
      write ( *, '(a)' ) 
     &  '  to hypersphere and back.  Consider norm of difference.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  M    || X - X2 ||'

      seed = 123456789

      do m = 1, 5

        write ( *, '(a)' ) ''

        do test = 1, 5
          call r8mat_uniform_01 ( m, n, seed, x )
          call r8vec_uniform_01 ( m, seed, c )
          call cartesian_to_hypersphere ( m, n, c, x, r, theta )
          call hypersphere_to_cartesian ( m, n, c, r, theta, x2 )
          err = r8mat_norm_fro_affine ( m, n, x, x2 )
          write ( *, '(2x,i2,2x,g14.6)' ) m, err
        end do

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests HYPERSPHERE_01_SURFACE_UNIFORM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m_max
      parameter ( m_max = 5 )
      integer n
      parameter ( n = 1 )

      integer m
      integer seed
      integer test
      double precision x(m_max,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  HYPERSPHERE_01_SURFACE_UNIFORM samples uniformly from the'
      write ( *, '(a)' ) '  surface of the unit hypersphere'

      seed = 123456789

      do m = 1, 5
        do test = 1, 3
          call hypersphere_01_surface_uniform ( m, n, seed, x )
          call r8vec_transpose_print ( m, x, 
     &      '  Random hypersphere point:' )
        end do
      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests HYPERSPHERE_01_AREA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision area
      double precision area2
      double precision hypersphere_01_area
      integer m
      integer n_data

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) 
     &  '  HYPERSPHERE_01_AREA evaluates the area of the unit'
      write ( *, '(a)' ) '  hypersphere in M dimensions.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '       M      Exact       Computed'
      write ( *, '(a)' ) '              Area        Area'
      write ( *, '(a)' ) ''

      n_data = 0

10    continue

        call hypersphere_01_area_values ( n_data, m, area )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        area2 = hypersphere_01_area ( m )

        write ( *, '(2x,i6,2x,f10.4,2x,f10.4)' ) m, area, area2

      go to 10

20    continue

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests HYPERSPHERE_01_VOLUME.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision hypersphere_01_volume
      integer m
      integer n_data
      double precision volume
      double precision volume2

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) 
     &  '  HYPERSPHERE_01_VOLUME evaluates the area of the unit'
      write ( *, '(a)' ) '  hypersphere in M dimensions.'
      write ( *, '(a)' ) 
     &  '  HYPERSPHERE_01_VOLUME_VALUES returns some test values.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '       M      Exact       Computed'
      write ( *, '(a)' ) '              Volume      Volume'
      write ( *, '(a)' ) ''

      n_data = 0

10    continue

        call hypersphere_01_volume_values ( n_data, m, volume )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        volume2 = hypersphere_01_volume ( m )

        write ( *, '(2x,i6,2x,f10.4,2x,f10.4)' ) m, volume, volume2

      go to 10

20    continue

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests HYPERSPHERE_AREA, HYPERSPHERE_VOLUME.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision area
      double precision hypersphere_area
      double precision hypersphere_volume
      integer m
      double precision r
      double precision volume

      r = 1.5D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  For a hypersphere in M dimensions:'
      write ( *, '(a)' ) '  HYPERSPHERE_AREA computes the area'
      write ( *, '(a)' ) '  HYPERSPHERE_VOLUME computes the volume.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Notice that both quantities eventually decrease.'
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  We use a radius of R = ', r
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '    M        Area          Volume    Area / Volume '
      write ( *, '(a)' ) ''

      do m = 1, 20
        area = hypersphere_area ( m, r )
        volume = hypersphere_volume ( m, r )
        write ( *, '(2x,i3,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    m, area, volume, area / volume
      end do

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests the stereographic mapping.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m_max
      parameter ( m_max = 5 )
      integer n
      parameter ( n = 1 ) 

      double precision err
      integer m
      double precision r8mat_norm_fro_affine
      integer seed
      integer test
      double precision x1(m_max,n)
      double precision x2(m_max-1,n)
      double precision x3(m_max,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  Test the stereographic mapping:'
      write ( *, '(a)' ) 
     &  '  HYPERSPHERE_STEREOGRAPH maps hypersphere points' //
     &  ' to the plane.'
      write ( *, '(a)' ) 
     &  '  HYPERSPHERE_STEREOGRAPH_INVERSE inverts the mapping.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Pick a random X1 on the hypersphere.'
      write ( *, '(a)' ) '  Map it to a point X2 on the plane.'
      write ( *, '(a)' ) 
     &  '  Map it back to a point X3 on the hypersphere.'
      write ( *, '(a)' ) '  Consider norm of difference.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  M    || X1 - X3 ||'

      seed = 123456789

      do m = 2, 5 
        write ( *, '(a)' ) ''
        do test = 1, 5
          call hypersphere_01_surface_uniform ( m, n, seed, x1 )
          call hypersphere_stereograph ( m, n, x1, x2 )
          call hypersphere_stereograph_inverse ( m, n, x2, x3 )
          err = r8mat_norm_fro_affine ( m, n, x1, x3 )
          write ( *, '(2x,i2,2x,g14.6)' ) m, err
        end do
      end do

      return
      end

