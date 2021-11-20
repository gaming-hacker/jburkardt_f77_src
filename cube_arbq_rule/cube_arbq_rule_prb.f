      program main

c*********************************************************************72
c
cc MAIN is the main program for CUBE_ARBQ_RULE_PRB.
c
c  Discussion:
c
c    CUBE_ARBQ_RULE_PRB tests the CUBE_ARBQ_RULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    03 July 2014
c
c  Author:
c
c    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Hong Xiao, Zydrunas Gimbutas, 
c    A numerical algorithm for the construction of efficient quadrature 
c    rules in two and higher dimensions,
c    Computers and Mathematics with Applications,
c    Volume 59, 2010, pages 663-676.
c
      implicit none

      integer cube_arbq_size
      integer degree
      character * ( 255 ) header
      integer n

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CUBE_ARBQ_RULE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CUBE_ARBQ_RULE library.'

      degree = 8
      n = cube_arbq_size ( degree )
      header = 'cube08'

      call test01 ( degree, n )

      call test02 ( degree, n, header )

      call test03 ( degree, n, header )

      call test04 ( degree, n )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CUBE_ARBQ_RULE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( degree, n )

c*********************************************************************72
c
cc TEST01 calls CUBE_ARBQ for a quadrature rule of given order.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    03 July 2014
c
c  Author:
c
c    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Hong Xiao, Zydrunas Gimbutas, 
c    A numerical algorithm for the construction of efficient quadrature 
c    rules in two and higher dimensions,
c    Computers and Mathematics with Applications,
c    Volume 59, 2010, pages 663-676.
c
c  Parameters:
c
c    Input, integer DEGREE, the desired total polynomial degree exactness
c    of the quadrature rule.
c
c    Input, integer N, the number of nodes.
c
      implicit none

      integer n

      double precision d
      integer degree
      integer j
      double precision r8vec_sum
      double precision volume
      double precision w(n)
      double precision x(3,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Quadrature rule for the symmetric cube.'
      write ( *, '(a,i4)' ) 
     &  '  Polynomial exactness degree DEGREE = ', degree

      volume = 8.0D+00
c
c  Retrieve and print a quadrature rule.
c
      call cube_arbq ( degree, n, x, w )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of nodes N = ', n

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '     J      W               X               Y          Z'
      write ( *, '(a)' ) ''
      do j = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    j, w(j), x(1,j), x(2,j), x(3,j)
      end do

      d = r8vec_sum ( n, w )

      write ( *, '(a,2x,g14.6)' ) '   Sum  ', d
      write ( *, '(a,2x,g14.6)' ) '  Volume', volume

      return
      end
      subroutine test02 ( degree, n, header )

c*********************************************************************72
c
cc TEST02 gets a rule and writes it to a file.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    03 July 2014
c
c  Author:
c
c    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Hong Xiao, Zydrunas Gimbutas, 
c    A numerical algorithm for the construction of efficient quadrature 
c    rules in two and higher dimensions,
c    Computers and Mathematics with Applications,
c    Volume 59, 2010, pages 663-676.
c
c  Parameters:
c
c    Input, integer DEGREE, the desired total polynomial degree exactness
c    of the quadrature rule.  0 <= DEGREE <= 15.
c
c    Input, integer N, the number of nodes to be used by the rule.
c
c    Input, character * ( * ) HEADER, an identifier for the filenames.
c
      implicit none

      integer n

      integer degree
      character * ( * ) header
      integer i
      integer rule_unit
      character * ( 255 ) rule_filename
      double precision w(n)
      double precision x(3,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Get a quadrature rule for the symmetric cube.'
      write ( *, '(a)' ) '  Then write it to a file.'
      write ( *, '(a,i4)' ) 
     &  '  Polynomial exactness degree DEGREE = ', degree
c
c  Retrieve a quadrature rule.
c
      call cube_arbq ( degree, n, x, w )
c
c  Write the points and weights to a file.
c
      call get_unit ( rule_unit )

      rule_filename = trim ( header ) // '.txt'

      open ( unit = rule_unit, file = rule_filename, 
     &  status = 'replace' )
      do i = 1, n
        write ( rule_unit, '(4(e21.15,2x))' ) 
     &    x(1,i), x(2,i), x(3,i), w(i)
      end do
      close ( unit = rule_unit )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Quadrature rule written to file "' 
     &  // trim ( rule_filename ) // '".'

      return
      end
      subroutine test03 ( degree, n, header )

c*********************************************************************72
c
cc TEST03 gets a rule and creates GNUPLOT input files.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    03 July 2014
c
c  Author:
c
c    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Hong Xiao, Zydrunas Gimbutas, 
c    A numerical algorithm for the construction of efficient quadrature 
c    rules in two and higher dimensions,
c    Computers and Mathematics with Applications,
c    Volume 59, 2010, pages 663-676.
c
c  Parameters:
c
c    Input, integer DEGREE, the desired total polynomial degree exactness
c    of the quadrature rule.  0 <= DEGREE <= 15.
c
c    Input, integer N, the number of nodes to be used by the rule.
c
c    Input, character * ( * ) HEADER, an identifier for the filenames.
c
      implicit none

      integer n

      integer degree
      character * ( * ) header
      double precision w(n)
      double precision x(3,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  Get a quadrature rule for the symmetric cube.'
      write ( *, '(a)' ) '  Set up GNUPLOT graphics input.'
      write ( *, '(a,i4)' ) 
     &  '  Polynomial exactness degree DEGREE = ', degree
c
c  Retrieve a quadrature rule.
c
      call cube_arbq ( degree, n, x, w )
c
c  Create files for input to GNUPLOT.
c
      call cube_arbq_gnuplot ( n, x, header )

      return
      end
      subroutine test04 ( degree, n )

c*********************************************************************72
c
cc TEST04 gets a rule and tests its accuracy.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    03 July 2014
c
c  Author:
c
c    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Hong Xiao, Zydrunas Gimbutas, 
c    A numerical algorithm for the construction of efficient quadrature 
c    rules in two and higher dimensions,
c    Computers and Mathematics with Applications,
c    Volume 59, 2010, pages 663-676.
c
c  Parameters:
c
c    Input, integer DEGREE, the desired total polynomial degree exactness
c    of the quadrature rule.  0 <= DEGREE <= 15.
c
c    Input, integer N, the number of nodes to be used by the rule.
c
      implicit none

      integer n

      double precision d
      integer degree
      integer i
      integer j
      integer npols
      double precision pols(((degree+1)*(degree+2)*(degree+3))/6)
      double precision rints(((degree+1)*(degree+2)*(degree+3))/6)
      double precision volume
      double precision w(n)
      double precision work(2*(degree+1))
      double precision x(3,n)
      double precision z(3)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) 
     &  '  Get a quadrature rule for the symmetric cube.'
      write ( *, '(a)' ) '  Test its accuracy.'
      write ( *, '(a,i4)' ) 
     &  '  Polynomial exactness degree DEGREE = ', degree
c
c  Retrieve a quadrature rule.
c
      call cube_arbq ( degree, n, x, w )

      npols = ( ( degree + 1 ) * ( degree + 2 ) * ( degree + 3 ) ) / 6

      do j = 1, npols
        rints(j) = 0.0D+00
      end do

      do i = 1, n

        z(1) = x(1,i)
        z(2) = x(2,i)
        z(3) = x(3,i)

        call lege3eva ( degree, z, pols, work )

        do j = 1, npols
          rints(j) = rints(j) + w(i) * pols(j)
        end do

      end do

      volume = 8.0D+00
      d = 0.0D+00
      d = ( rints(1) - sqrt ( volume ) )**2
      do i = 2, npols
        d = d + rints(i)**2
      end do
      d = sqrt ( d ) / dble ( npols )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  RMS error = ', d

      return
      end

