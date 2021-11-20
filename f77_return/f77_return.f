      program main

c*********************************************************************72
c
cc MAIN is the main program for F77_RETURN.
c
c  Discussion:
c
c    C and C++ programs can return an integer value signalling the status
c    of the program's execution.  By convention, a return value of 0 indicates
c    that the program executed successfully, while a nonzero value indicates
c    some kind of error.
c
c    The return value of such a program can be retrieved and used to make
c    decisions.  In particular, if the execution of the program is part of
c    a script, then the failure of the program can be detected, and the
c    script can terminate gracefully.
c
c    For an example in the BASH shell, we might have a script that reads:
c
c      ./prog
c      if [ $? -ne 0 ]; then
c        echo "Errors while running prog."
c        exit
c      fi
c
c    Here, $? is a BASH symbol that returns the most recent program status
c    value, and the script exits if that value is not zero.
c
c    FORTRAN has always had the option to include a constant integer value 
c    as part of the STOP statement.  At least some FORTRAN compilers can
c    treat this value as a  program status value that is returned to the
c    calling environment, and hence can signal whether certain errors
c    have occurred.
c
c    Here, we show a simple example in which the program is guaranteed
c    to fail, and in that case will return the arbitrary but nonzero value 13.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 August 2013
c
c  Author:
c
c    John Burkardt
c
      integer i
      integer t

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'F77_RETURN'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Demonstrate how a FORTRAN77 program can return an'
      write ( *, '(a)' ) 
     &  '  integer program status value to the calling environment.'
c
c  Add up the squares, but stop if we exceed 250.
c
      t = 0
      do i = 1, 100
        t = t + i * i
        if ( 250 .lt. t ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'F77_RETURN - Fatal error!'
          write ( *, '(a)' ) '  Sum exceeds 250.'
          stop 13
        end if
      end do
c
c  If the final sum is less than 1000, then that's an error too.
c
      if ( t .lt. 1000 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F77_RETURN - Fatal error!'
        write ( *, '(a)' ) '  Sum is less than 1000.'
        stop 99
      end if
c
c  If no error occurred, we have normal execution.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'F77_RETURN:'
      write ( *, '(a)' ) '  Normal end of execution.'

      stop 0
      end