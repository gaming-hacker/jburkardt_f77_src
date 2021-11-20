      program main

c*********************************************************************72
c
cc MAIN is the main program for BVEC_PRB.
c
c  Discussion:
c
c    BVEC_PRB tests the BVEC library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BVEC library.'

      call bvec_add_test ( )
      call bvec_complement2_test ( )
      call bvec_mul_test ( )
      call bvec_next_test ( )
      call bvec_next_grlex_test ( )
      call bvec_print_test ( )
      call bvec_sub_test ( )
      call bvec_to_i4_test ( )
      call bvec_uniform_test ( )
      call i4_bclr_test ( )
      call i4_bset_test ( )
      call i4_btest_test ( )
      call i4_to_bvec_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine bvec_add_test ( )

c*********************************************************************72
c
cc BVEC_ADD_TEST tests BVEC_ADD;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n)
      integer bvec4(n)
      integer i
      integer i4_uniform_ab
      integer j
      integer k
      integer l
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 10 )

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_ADD_TEST'
      write ( *, '(a)' ) 
     &  '  BVEC_ADD adds binary vectors representing integers;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '        I        J        I + J      BVEC_ADD'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        
        i = i4_uniform_ab ( -100, 100, seed )
        j = i4_uniform_ab ( -100, 100, seed )

        k = i + j

        call i4_to_bvec ( i, n, bvec1 )
        call i4_to_bvec ( j, n, bvec2 )
        call bvec_add ( n, bvec1, bvec2, bvec3 )
        call bvec_to_i4 ( n, bvec3, l )

        write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l

      end do

      return
      end
      subroutine bvec_complement2_test ( )

c*********************************************************************72
c
cc BVEC_COMPLEMENT2_TEST tests BVEC_COMPLEMENT2;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer bvec1(n)
      integer bvec2(n)
      integer i
      integer j
      integer i4_uniform_ab
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 5 )

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_COMPLEMENT2_TEST'
      write ( *, '(a)' ) 
     &  '  BVEC_COMPLEMENT2 returns the two''s complement'
      write ( *, '(a)' ) '  of a (signed) binary vector;'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
    
        i = i4_uniform_ab ( -100, 100, seed )

        call i4_to_bvec ( i, n, bvec1 )

        call bvec_complement2 ( n, bvec1, bvec2 )

        call bvec_to_i4 ( n, bvec2, j )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i8)' ) '  I = ', i
        write ( *, '(a,2x,i8)' ) '  J = ', j
        call bvec_print ( n, bvec1, ' ' )
        call bvec_print ( n, bvec2, ' ' )

      end do

      return
      end
      subroutine bvec_mul_test ( )

c*********************************************************************72
c
cc BVEC_MUL_TEST tests BVEC_MUL;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 15 )

      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n)
      integer i
      integer i4_uniform_ab
      integer j
      integer k
      integer l
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 10 )

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_MUL_TEST'
      write ( *, '(a)' ) '  BVEC_MUL multiplies binary vectors '
      write ( *, '(a)' ) '  representing integers;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        I        J        I * J  BVEC_MUL'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
    
        i = i4_uniform_ab ( -100, 100, seed )
        j = i4_uniform_ab ( -100, 100, seed )

        k = i * j

        call i4_to_bvec ( i, n, bvec1 )
        call i4_to_bvec ( j, n, bvec2 )
        call bvec_mul ( n, bvec1, bvec2, bvec3 )
        call bvec_to_i4 ( n, bvec3, l )

        write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l

      end do
    
      return
      end
      subroutine bvec_next_test ( )

c*********************************************************************72
c
cc BVEC_NEXT_TEST tests BVEC_NEXT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n 
      parameter ( n = 4 )

      integer b(n)
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_NEXT_TEST'
      write ( *, '(a)' ) '  BVEC_NEXT computes the "next" BVEC.'
      write ( *, '(a)' ) ''

      do i = 1, n
        b(i) = 0
      end do

      do i = 0, 16
        call bvec_print ( n, b, '' )
        call bvec_next ( n, b )
      end do

      return
      end
      subroutine bvec_next_grlex_test ( )

c*********************************************************************72
c
cc BVEC_NEXT_GRLEX_TEST tests BVEC_NEXT_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
 
      integer b(n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_NEXT_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  BVEC_NEXT_GRLEX computes binary vectors in GRLEX order.'
      write ( *, '(a)' ) ''

      b(1:n) = 0

      do i = 0, 16
        write ( *, '(2x,i2,a)', advance = 'no' ) i, ':  '
        do j = 1, n
          write ( *, '(i1)', advance = 'no' ) b(j)
        end do
        write ( *, '(a)' ) ''
        call bvec_next_grlex ( n, b )
      end do

      return
      end
      subroutine bvec_print_test ( )

c*********************************************************************72
c
cc BVEC_PRINT_TEST tests BVEC_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer bvec(n)

      save bvec

      data bvec /
     &  1, 0, 0, 1, 0, 1, 1, 1, 0, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_PRINT_TEST'
      write ( *, '(a)' ) '  BVEC_PRINT prints a binary vector.'

      call bvec_print ( n, bvec, '  BVEC:' )

      return
      end
      subroutine bvec_sub_test ( )

c*********************************************************************72
c
cc BVEC_SUB_TEST tests BVEC_SUB;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    25 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n)
      integer bvec4(n)
      integer i
      integer i4_uniform_ab
      integer j
      integer k
      integer l
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 10 )

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_SUB_TEST'
      write ( *, '(a)' ) 
     &  '  BVEC_SUB subtracts binary vectors representing integers;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '        I        J        I - J  BVEC_SUB'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        
        i = i4_uniform_ab ( -100, 100, seed )
        j = i4_uniform_ab ( -100, 100, seed )

        k = i - j

        call i4_to_bvec ( i, n, bvec1 )
        call i4_to_bvec ( j, n, bvec2 )
        call bvec_sub ( n, bvec1, bvec2, bvec4 )
        call bvec_to_i4 ( n, bvec4, l )

        write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l

      end do

      return
      end
      subroutine bvec_to_i4_test ( )

c*********************************************************************72
c
cc BVEC_TO_I4_TEST tests BVEC_TO_I4;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer bvec(n)
      integer i
      integer i2
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_TO_I4_TEST'
      write ( *, '(a)' ) '  BVEC_TO_I4 converts a signed binary vector'
      write ( *, '(a)' ) '  to an integer;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I --> BVEC  -->  I'
      write ( *, '(a)' ) ' '
      do i = -3, 10
        call i4_to_bvec ( i, n, bvec )
        call bvec_to_i4 ( n, bvec, i2 )
        write ( *, '(2x,i3,2x,10i1,2x,i3)' ) 
     &    i, ( bvec(j), j = 1, n ), i2
      end do

      return
      end
      subroutine bvec_uniform_test ( )

c*********************************************************************72
c
cc BVEC_UNIFORM_TEST tests BVEC_UNIFORM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer b(n)
      integer i
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_UNIFORM_TEST'
      write ( *, '(a)' ) '  BVEC_UNIFORM computes a binary vector.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  The initial seed is ', seed

      write ( *, '(a)' ) ''
      do i = 1, 10
        call bvec_uniform ( n, seed, b )
        call bvec_print ( n, b, '' )
      end do

      return
      end
      subroutine i4_bclr_test ( )

c*********************************************************************72
c
cc I4_BCLR_TEST tests I4_BCLR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 2 )

      integer i4
      integer i4_test(test_num)
      integer i4_bclr
      integer ivec(0:31)
      integer j1
      integer pos
      integer test

      save i4_test

      data i4_test / 101, -31 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_BCLR_TEST'
      write ( *, '(a)' ) '  I4_BCLR sets a given bit to 0.'

      do test = 1, test_num

        i4 = i4_test(test)

        call i4_to_bvec ( i4, 32, ivec )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Working on I4 = ', i4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '       Pos     Digit       I4_BCLR'
        write ( *, '(a)' ) ' '

        do pos = 0, 31
  
          j1 = i4_bclr ( i4, pos )

          write ( *, '(2x,i8,2x,i8,2x,i12)' ) pos, ivec(pos), j1

        end do

      end do

      return
      end
      subroutine i4_bset_test ( )

c*********************************************************************72
c
cc I4_BSET_TEST tests I4_BSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 2 )

      integer i4
      integer i4_test(test_num)
      integer i4_bset
      integer ivec(0:31)
      integer j1
      integer pos
      integer test

      save i4_test

      data i4_test / 101, -31 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_BSET_TEST'
      write ( *, '(a)' ) '  I4_BSET sets a given bit to 0.'

      do test = 1, test_num

        i4 = i4_test(test)

        call i4_to_bvec ( i4, 32, ivec )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Working on I4 = ', i4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '       Pos     Digit       I4_BSET'
        write ( *, '(a)' ) ' '

        do pos = 0, 31
  
          j1 = i4_bset ( i4, pos )

          write ( *, '(2x,i8,2x,i8,2x,i12)' ) pos, ivec(pos), j1

        end do

      end do

      return
      end
      subroutine i4_btest_test ( )

c*********************************************************************72
c
cc I4_BTEST_TEST tests I4_BTEST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 2 )

      integer i4
      integer i4_test(test_num)
      logical i4_btest
      integer ivec(0:31)
      logical j1
      integer pos
      integer test

      save i4_test

      data i4_test / 101, -31 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_BTEST_TEST'
      write ( *, '(a)' ) '  I4_BTEST reports if a given bit is 0 or 1.'

      do test = 1, test_num

        i4 = i4_test(test)

        call i4_to_bvec ( i4, 32, ivec )
    
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Analyze the integer I4 = ', i4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '       Pos     Digit  I4_BTEST'
        write ( *, '(a)' ) ' '

        do pos = 0, 31
  
          j1 = i4_btest ( i4, pos )

          write ( *, '(2x,i8,2x,i8,2x,7x,l1,2x)' ) pos, ivec(pos), j1

        end do

      end do

      return
      end
      subroutine i4_to_bvec_test ( )

c*********************************************************************72
c
cc I4_TO_BVEC_TEST tests I4_TO_BVEC;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer bvec(n)
      integer i
      integer i2
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_TO_BVEC_TEST'
      write ( *, '(a)' ) '  I4_TO_BVEC converts an integer to a '
      write ( *, '(a)' ) '  signed binary vector;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I --> BVEC  -->  I'
      write ( *, '(a)' ) ' '
      do i = -3, 10
        call i4_to_bvec ( i, n, bvec )
        call bvec_to_i4 ( n, bvec, i2 )
        write ( *, '(2x,i3,2x,10i1,2x,i3)' ) 
     &    i, ( bvec(j), j = 1, n ), i2
      end do

      return
      end
