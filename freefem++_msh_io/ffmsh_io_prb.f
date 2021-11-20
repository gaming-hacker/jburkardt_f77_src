      program main

c*********************************************************************72
c
cc MAIN is the main program for FFMSH_IO_PRB.
c
c  Discussion:
c
c    FFMSH_IO_PRB tests the FFMSH_IO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FFMSH_IO_PRB'
      write ( *, '(a)' ) '  FORTRAN90 version'
      write ( *, '(a)' ) '  Test the FFMSH_IO library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FFMSH_IO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 gets the example data and prints it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer e_num
      integer t_num
      integer v_num
 
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) '  Get the example 2D data and print it.'
c
c  Get example sizes.
c
      call ffmsh_2d_size_example ( v_num, e_num, t_num )
c
c  Print example sizes.
c
      call ffmsh_2d_size_print ( '  Example Sizes:', v_num, e_num, 
     &  t_num )
c
c  "Allocate" memory.
c
      call test01_sub ( v_num, e_num, t_num )

      return
      end
      subroutine test01_sub ( v_num, e_num, t_num )

c*********************************************************************72
c
cc TEST01_SUB allocates memory for TEST01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer e_num
      integer t_num
      integer v_num

      integer e_l(e_num)
      integer e_v(2,e_num)
      integer t_l(t_num)
      integer t_v(3,t_num)
      integer v_l(v_num)
      double precision v_xy(2,v_num)
c
c  Get example data.
c
      call ffmsh_2d_data_example ( v_num, e_num, t_num, v_xy, v_l, e_v, 
     &  e_l, t_v, t_l )
c
c  Print example data.
c
      call ffmsh_2d_data_print ( '  Example data:', v_num, e_num, t_num,
     &  v_xy, v_l, e_v, e_l, t_v, t_l )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 writes the example data to a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer e_num
      integer t_num
      integer v_num

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) '  Write the example 2D data to a file.'
c
c  Get example sizes.
c
      call ffmsh_2d_size_example ( v_num, e_num, t_num )
c
c  "Allocate" memory.
c
      call test02_sub ( v_num, e_num, t_num )

      return
      end
      subroutine test02_sub ( v_num, e_num, t_num )

c*********************************************************************72
c
cc TEST02_SUB allocates memory for TEST02.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer e_num
      integer t_num
      integer v_num

      integer e_l(e_num)
      integer e_v(2,e_num)
      character * ( 255 ) filename
      integer t_l(t_num)
      integer t_v(3,t_num)
      integer v_l(v_num)
      double precision v_xy(2,v_num)
c
c  Get example data.
c
      call ffmsh_2d_data_example ( v_num, e_num, t_num, v_xy, v_l, e_v, 
     &  e_l, t_v, t_l )
c
c  Write the data to a file.
c
      filename = 'output.msh'

      call ffmsh_2d_write ( filename, v_num, e_num, t_num, v_xy, v_l,
     &  e_v, e_l, t_v, t_l )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  The data was written to "' 
     &  // trim ( filename ) // '"'

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 gets the example data from a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer e_num
      character * ( 255 ) ffmsh_filename
      integer t_num
      integer v_num

      ffmsh_filename = 'input.msh'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) '  Read the example 2D data from a file.'
c
c  Get example sizes.
c
      call ffmsh_2d_size_read ( ffmsh_filename, v_num, e_num, t_num )
c
c  Print example sizes.
c
      call ffmsh_2d_size_print ( ffmsh_filename, v_num, e_num, t_num )
c
c  "Allocate" memory.
c
      call test03_sub ( ffmsh_filename, v_num, e_num, t_num )

      return
      end
      subroutine test03_sub ( ffmsh_filename, v_num, e_num, t_num )

c*********************************************************************72
c
cc TEST03_SUB allocates memory for TEST03.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer e_num
      integer t_num
      integer v_num

      integer e_l(e_num)
      integer e_v(2,e_num)
      character * ( 255 ) ffmsh_filename
      integer t_l(t_num)
      integer t_v(3,t_num)
      integer v_l(v_num)
      double precision v_xy(2,v_num)
c
c  Get example data.
c
      call ffmsh_2d_data_read ( ffmsh_filename, v_num, e_num, t_num, 
     &  v_xy, v_l, e_v, e_l, t_v, t_l )
c
c  Print example data.
c
      call ffmsh_2d_data_print ( '  Example data:', v_num, e_num, t_num,
     &  v_xy, v_l, e_v, e_l, t_v, t_l )

      return
      end
