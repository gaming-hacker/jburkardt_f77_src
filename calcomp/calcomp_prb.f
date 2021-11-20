      program main

c*********************************************************************72
c
cc MAIN is the main program for CALCOMP_PRB.
c
c  Discussion:
c
c    CALCOMP_PRB is a sample calling program for the CALCOMP utilities.
c
c  Modified:
c
c    10 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CALCOMP_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Demonstrate the CALCOMP emulator.'

      call test01

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CALCOMP_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( ) 

      stop
      end
      subroutine test01

c*********************************************************************72
c
cc TEST01 demonstrates how to plot a simple line graph.
c
c  Modified:
c
c    01 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real height
      integer i
      integer indx
      integer n
      parameter ( n = 101 )
      integer nchar
      real pi
      parameter ( pi = 3.141592653589793E+00 )
      character*(80) string
      real x
      real xmax
      real xmin
      real xpage
      real y
      real ymax
      real ymin
      real ypage

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Demonstrate a simple line graph.'
c
c  Open the file.
c
      call plots ( 0, 0, 0 )
c
c  Draw a plot box.
c
      call plot ( 1.0, 1.0, 3 )
      call plot ( 7.5, 1.0, 2 )
      call plot ( 7.5, 7.5, 2 )
      call plot ( 1.0, 7.5, 2 )
      call plot ( 1.0, 1.0, 2 )
c
c  Define the size of the data.
c
      xmin = 0.0E+00
      xmax = 2.0E+00 * pi

      ymin = -1.0E+00
      ymax =  1.0E+00
c
c  Compute the data.
c
      do i = 1, n

        x = ( real ( n - i     ) * xmin 
     &      + real (     i - 1 ) * xmax )
     &      / real ( n     - 1 )

        y = sin ( x )

        xpage = ( ( xmax - x        ) * 7.5
     &          + (        x - xmin ) * 1.0 )
     &          / ( xmax     - xmin )

        ypage = ( ( ymax - y        ) * 7.5
     &          + (        y - ymin ) * 1.0 )
     &          / ( ymax     - ymin )

        if ( i .eq. 1 ) then
          call plot ( xpage, ypage, 3 )
        else
          call plot ( xpage, ypage, 2 )
        end if

        if ( mod ( i - 1, 10 ) .eq. 0 ) then
          height = 0.25 / 72.0
          indx = 1
          angle = 0.0
          nchar = -1
          call symbol ( xpage, ypage, height, indx, angle, nchar )
        end if
  
      end do
c
c  Label the plot.
c
      xpage = 3.0
      ypage = 8.5
      height = 0.50
      string = 'Sine Curve'
      angle = 0.0
      nchar = 0

      call text ( xpage, ypage, height, string, angle, nchar )

      xpage = 3.0
      ypage = 8.0
      height = 0.20
      string = 'An example of plotting.'
      angle = 0.0
      nchar = 0

      call text ( xpage, ypage, height, string, angle, nchar )
c
c  Close the plot
c
      call plot ( xpage, ypage, 999 )

      return
      end
