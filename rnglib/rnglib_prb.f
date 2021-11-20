      program main

c*********************************************************************72
c
cc MAIN is the main program for RNGLIB_PRB.
c
c  Discussion:
c
c    RNGLIB_PRB tests the RNGLIB library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RNGLIB_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the RNGLIB library.'
c
c  Call tests.
c
      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RNGLIB_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 calls I4_UNI 10 times, just to show how it is done.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer g
      integer i
      integer i4_uni
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  I4_UNI ( ) returns a random positive integer'
      write ( *, '(a)' ) '  using the current generator.'
c
c  Initialize the package.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  INITIALIZE initializes the random number generator.'
      write ( *, '(a)' ) 
     &  '  It only needs to be called once before using the package.'

      call initialize ( )
c
c  Set the current generator index to #1.
c
      g = 1
      call cgn_set ( g )
      write ( *, '(a)' ) ' '
      write ( *, '(a,i2)' ) '  Current generator index = ', g
c
c  Now call I4_UNI().
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I     I4_UNI ( )'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        j = i4_uni ( )
        write ( *, '(2x,i2,2x,i12)' ) i, j
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 calls R4_UNI_01 10 times, just to show how it is done.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer g
      integer i
      real r4_uni_01
      real u

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  R4_UNI_01 ( ) returns a random real number'
      write ( *, '(a)' ) '  in [0,1] using the current generator.'
c
c  Initialize the package.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  INITIALIZE initializes the random number generator.'
      write ( *, '(a)' ) 
     &  '  It only needs to be called once before using the package.'

      call initialize ( )
c
c  Set the current generator index to #2.
c
      g = 2
      call cgn_set ( g )
      write ( *, '(a)' ) ' '
      write ( *, '(a,i2)' ) '  Current generator index = ', g
c
c  Repeatedly call R4_UNI_01().
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I     R4_UNI_01 ( )'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        u = r4_uni_01 ( )
        write ( *, '(2x,i2,2x,g14.6)' ) i, u
      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 demonstrates how the seed can be reset to its initial or last value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer g
      integer i
      real r4_uni_01
      real u

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  R4_UNI_01 ( ) returns a random real number'
      write ( *, '(a)' ) '  in [0,1] using the current generator.'
c
c  Initialize the package.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  INITIALIZE initializes the random number generator.'
      write ( *, '(a)' ) 
     &  '  It only needs to be called once before using the package.'

      call initialize ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  INIT_GENERATOR can reset the seed to the initial value,'
      write ( *, '(a)' ) '  the last (previous) value, or a new seed.'
c
c  Set the current generator index to #17.
c
      g = 17
      call cgn_set ( g )
      write ( *, '(a)' ) ' '
      write ( *, '(a,i2)' ) '  Current generator index = ', g
c
c  Force the current generator to begin at its initial seed.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  INIT_GENERATOR ( 0 ) starts at the initial seed.'

      call init_generator ( 0 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I    R4_UNI_01  )'
      write ( *, '(a)' ) ' '
      do i = 1, 10
        u = r4_uni_01 ( )
        write ( *, '(2x,i2,2x,g14.6)' ) i, u
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Calling INIT_GENERATOR ( 0 ) again restarts'
      write ( *, '(a)' ) '  at the initial seed.'

      call init_generator ( 0 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I    R4_UNI_01 ( )'
      write ( *, '(a)' ) ' '
      do i = 1, 10
        u = r4_uni_01 ( )
        write ( *, '(2x,i2,2x,g14.6)' ) i, u
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Calling INIT_GENERATOR ( 2 ) restarts'
      write ( *, '(a)' ) '  at a new "far ahead" seed.'

      call init_generator ( 2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I    R4_UNI_01 ( )'
      write ( *, '(a)' ) ' '
      do i = 1, 10
        u = r4_uni_01 ( )
        write ( *, '(2x,i2,2x,g14.6)' ) i, u
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Calling INIT_GENERATOR ( 1 ) restarts'
      write ( *, '(a)' )
     &  '  at the last seed (in this case, the "far ahead"'
      write ( *, '(a)' ) '  seed specified on the previous call.)'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I    R4_UNI_01 ( )'
      write ( *, '(a)' ) ' '
      do i = 1, 10
        u = r4_uni_01 ( )
        write ( *, '(2x,i2,2x,g14.6)' ) i, u
        if ( mod ( i, 3 ) .eq. 0 ) then
          call init_generator ( 1 )
          write ( *, '(a)' ) '  (Reset to last seed)'
        end if
      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 demonstrates the use of multiple streams.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer g(3)
      integer i
      integer j
      real r4_uni_01
      real u(3)

      g(1) = 3
      g(2) = 6
      g(3) = 9

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) 
     &  '  R4_UNI_01 ( ) returns a random real number'
      write ( *, '(a)' ) '  in [0,1] using the current generator.'
c
c  Initialize the package.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  INITIALIZE initializes the random number generator.'
      write ( *, '(a)' ) 
     &  '  It only needs to be called once before using the package.'

      call initialize ( )
c
c  Use three separate generators, 3, 6 and 9.
c  Force them to start at their initial seeds.
c
      write ( *, '(a)' ) ' '
      do i = 1, 3
        write ( *, '(a,i1)' ) '  Initialize generator ', g(i)
        call cgn_set ( g(i) )
        call init_generator ( 0 )
      end do
c
c  Call the generators in the order 3, 6, 9.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   I    R4_UNI_01 ( 3 )  R4_UNI_01 ( 6 )  ' // 
     &  'R4_UNI_01 ( 9 )'
      write ( *, '(a)' ) ' '
      do i = 1, 10
        do j = 1, 3
          call cgn_set ( g(j) )
          u(j) = r4_uni_01 ( )
        end do
        write ( *, '(2x,i2,2x,g14.6,4x,g14.6,4x,g14.6)' ) 
     &    i, ( u(j), j = 1, 3 )
      end do
c
c  Restart the generators at their initial seeds.
c
      g(1) = 6
      g(2) = 9
      g(3) = 3

      write ( *, '(a)' ) ' '
      do i = 1, 3
        write ( *, '(a,i1)' ) '  Reinitialize generator ', g(i)
        call cgn_set ( g(i) )
        call init_generator ( 0 )
      end do
c
c  Call them in a different order, same result.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Restart the generators at their initial seeds.'
      write ( *, '(a)' ) 
     &  '  Now call generators in order 6, 9, 3, but we'
      write ( *, '(a)' ) '  will get the same results, because the'
      write ( *, '(a)' ) '  generators are independent.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   I    R4_UNI_01 ( 6 )  R4_UNI_01 ( 9 )  ' //
     &  'R4_UNI_01 ( 3 )'
      write ( *, '(a)' ) ' '
      do i = 1, 10
        do j = 1, 3
          call cgn_set ( g(j) )
          u(j) = r4_uni_01 ( )
        end do
        write ( *, '(2x,i2,2x,g14.6,4x,g14.6,4x,g14.6)' ) 
     &    i, ( u(j), j = 1, 3 )
      end do

      return
      end
