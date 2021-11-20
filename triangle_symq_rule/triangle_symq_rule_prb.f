      program main

c*********************************************************************72
c
cc MAIN is the main program for TRIANGLE_SYMQ_RULE_PRB.
c
c  Discussion:
c
c    TRIANGLE_SYMQ_RULE_PRB tests the TRIANGLE_SYMQ_RULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    26 June 2014
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

      integer degree
      character * ( 255 ) header
      integer itype
      integer numnodes
      double precision vert1(2)
      double precision vert2(2)
      double precision vert3(2)

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRIANGLE_SYMQ_RULE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TRIANGLE_SYMQ_RULE library.'

      call test01 ( )

      do itype = 0, 2

        if ( itype == 0 ) then

          write ( *, '(a)' ) ''
          write ( *, '(a)' ) '  Region is user-defined triangle.'
          vert1(1) = 1.0D+00
          vert1(2) = 0.0D+00
          vert2(1) = 4.0D+00
          vert2(2) = 4.0D+00
          vert3(1) = 0.0D+00
          vert3(2) = 3.0D+00
          header = 'user08'
          degree = 8

        else if ( itype == 1 ) then

          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 
     &      '  Region is standard equilateral triangle.'
          vert1(1) = -1.0D+00
          vert1(2) = -1.0D+00 / sqrt ( 3.0D+00 )
          vert2(1) = +1.0D+00
          vert2(2) = -1.0D+00 / sqrt ( 3.0D+00 )
          vert3(1) =  0.0D+00
          vert3(2) =  2.0D+00 / sqrt ( 3.0D+00 )
          header = 'equi08'
          degree = 8

        else if ( itype == 2 ) then

          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 
     &      '  Region is the simplex (0,0),(1,0),(0,1).'
          vert1(1) = 0.0D+00
          vert1(2) = 0.0D+00
          vert2(1) = 1.0D+00
          vert2(2) = 0.0D+00
          vert3(1) = 0.0D+00
          vert3(2) = 1.0D+00
          header = 'simp08'
          degree = 8

        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Triangle:'
        write ( *, '(a)' ) ' '
        write ( *, '(2x,g14.6,2x,g14.6)' ) vert1(1:2)
        write ( *, '(2x,g14.6,2x,g14.6)' ) vert2(1:2)
        write ( *, '(2x,g14.6,2x,g14.6)' ) vert3(1:2)
c
c  Determine the size of the rule.
c
        call rule_full_size ( degree, numnodes )
c
c  Retrieve a rule and print it.
c
        call test02 ( degree, numnodes, vert1, vert2, vert3 )
c
c  Get a rule, and write data files that gnuplot can use to plot the points.
c
        call test03 ( degree, numnodes, vert1, vert2, vert3, header )

        call test04 ( degree, numnodes, vert1, vert2, vert3, header )

        call test05 ( degree, numnodes, vert1, vert2, vert3 )

      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRIANGLE_SYMQ_RULE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests TRIANGLE_TO_SIMPLEX, TRIANGLE_TO_REF, REF_TO_TRIANGLE, SIMPLEX_TO_TRIANGLE.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    21 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision rp1(2)
      double precision rp2(2)
      double precision rv1(2)
      double precision rv2(2)
      double precision rv3(2)
      integer seed
      double precision sp1(2)
      double precision sp2(2)
      double precision sv1(2)
      double precision sv2(2)
      double precision sv3(2)
      double precision tp1(2)
      double precision tp2(2)
      double precision tv1(2)
      double precision tv2(2)
      double precision tv3(2)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Map points from one triangle to another.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  R = reference triangle'
      write ( *, '(a)' ) '  S = simplex'
      write ( *, '(a)' ) '  T = user-defined triangle.'
      write ( *, '(a)' ) '  REF_TO_TRIANGLE:     R => T' 
      write ( *, '(a)' ) '  SIMPLEX_TO_TRIANGLE: S => T' 
      write ( *, '(a)' ) '  TRIANGLE_TO_REF:     T => R' 
      write ( *, '(a)' ) '  TRIANGLE_TO_SIMPLEX: T => S' 
c
c  Reference triangle
c
      rv1(1) = -1.0D+00
      rv1(2) = -1.0D+00 / sqrt ( 3.0D+00 )
      rv2(1) = +1.0D+00
      rv2(2) = -1.0D+00 / sqrt ( 3.0D+00 )
      rv3(1) =  0.0D+00
      rv3(2) =  2.0D+00 / sqrt ( 3.0D+00 )
c
c  Simplex
c
      sv1(1) = 0.0D+00
      sv1(2) = 0.0D+00
      sv2(1) = 1.0D+00
      sv2(2) = 0.0D+00
      sv3(1) = 0.0D+00
      sv3(2) = 1.0D+00
c
c  User triangle.
c
      tv1(1) = 1.0D+00
      tv1(2) = 0.0D+00
      tv2(1) = 4.0D+00
      tv2(2) = 4.0D+00
      tv3(1) = 0.0D+00
      tv3(2) = 3.0D+00

      seed = 123456789

      do i = 1, 5

        call r8vec_uniform_01 ( 2, seed, sp1 )

        if ( 1.0D+00 .lt. sp1(1) + sp1(2) ) then
          sp1(1) = 1.0D+00 - sp1(1)
          sp1(2) = 1.0D+00 - sp1(2)
        end if

        call simplex_to_triangle ( tv1, tv2, tv3, sp1, tp1 )
        call triangle_to_ref ( tv1, tv2, tv3, tp1, rp1 )
        call ref_to_triangle ( tv1, tv2, tv3, rp1, tp2 )
        call triangle_to_simplex ( tv1, tv2, tv3, tp2, sp2 )

        write ( *, '(a)' ) ''
        write ( *, '(a,2g14.6)' ) '  SP1: ', sp1(1:2)
        write ( *, '(a,2g14.6)' ) '  TP1: ', tp1(1:2)
        write ( *, '(a,2g14.6)' ) '  RP1: ', rp1(1:2)
        write ( *, '(a,2g14.6)' ) '  TP2: ', tp2(1:2)
        write ( *, '(a,2g14.6)' ) '  SP2: ', sp2(1:2)

      end do

      return
      end
      subroutine test02 ( degree, numnodes, vert1, vert2, vert3 )

c*********************************************************************72
c
cc TEST02 calls TRIASYMQ for a quadrature rule of given order and region.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    21 June 2014
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
c    of the quadrature rule.  0 <= DEGREE <= 50.
c
c    Input, integer NUMNODES, the number of nodes to be used by the rule.
c
c    Input, double precision VERT1(2), VERT2(2), VERT3(2), the
c    vertices of the triangle.
c
      implicit none

      integer numnodes

      double precision area
      double precision d
      integer degree
      integer j
      double precision rnodes(2,numnodes)
      double precision triangle_area
      double precision vert1(2)
      double precision vert2(2)
      double precision vert3(2)
      double precision weights(numnodes)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Symmetric quadrature rule for a triangle.'
      write ( *, '(a,i4)' ) 
     &  '  Polynomial exactness degree DEGREE = ', degree

      area = triangle_area ( vert1, vert2, vert3 )
c
c  Retrieve and print a symmetric quadrature rule.
c
      call triasymq ( degree, vert1, vert2, vert3, rnodes, weights,
     &  numnodes )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  NUMNODES = ', numnodes

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '     J      W               X               Y'
      write ( *, '(a)' ) ''
      do j = 1, numnodes
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    j, weights(j), rnodes(1,j), rnodes(2,j)
      end do

      d = 0.0D+00
      do j = 1, numnodes
        d = d + weights(j)
      end do

      write ( *, '(a,2x,g14.6)' ) '   Sum', d
      write ( *, '(a,2x,g14.6)' ) '  Area', area

      return
      end
      subroutine test03 ( degree, numnodes, vert1, vert2, vert3, 
     &  header )

c*********************************************************************72
c
cc TEST03 calls TRIASYMQ_GNUPLOT to generate graphics files.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    21 June 2014
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
c    of the quadrature rule.  0 <= DEGREE <= 50.
c
c    Input, integer NUMNODES, the number of nodes to be used by the rule.
c
c    Input, double precision VERT1(2), VERT2(2), VERT3(2), the
c    vertices of the triangle.
c
c    Input, character * ( * ) HEADER, an identifier for the graphics filenames.
c
      implicit none

      integer numnodes

      integer degree
      character * ( * ) header
      double precision rnodes(2,numnodes)
      double precision vert1(2)
      double precision vert2(2)
      double precision vert3(2)
      double precision weights(numnodes)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  TRIASYMQ_GNUPLOT creates gnuplot graphics files.'
      write ( *, '(a,i4)' ) 
     &  '  Polynomial exactness degree DEGREE = ', degree

      call triasymq ( degree, vert1, vert2, vert3, rnodes, weights,
     &  numnodes )

      write ( *, '(a,i4)' ) '  Number of nodes = ', numnodes

      call triasymq_gnuplot ( vert1, vert2, vert3, numnodes, rnodes, 
     &  header )

      return
      end
      subroutine test04 ( degree, numnodes, vert1, vert2, vert3, 
     &  header )

c*********************************************************************72
c
cc TEST04 gets a rule and writes it to a file.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    20 June 2014
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
c    of the quadrature rule.  0 <= DEGREE <= 50.
c
c    Input, integer NUMNODES, the number of nodes to be used by the rule.
c
c    Input, double precision VERT1(2), VERT2(2), VERT3(2), the
c    vertices of the triangle.
c
c    Input, character * ( * ) HEADER, an identifier for the filenames.
c
      implicit none

      integer numnodes

      integer degree
      character * ( * ) header
      integer i
      double precision rnodes(2,numnodes)
      integer rule_unit
      character * ( 255 ) rule_filename
      double precision vert1(2)
      double precision vert2(2)
      double precision vert3(2)
      double precision weights(numnodes)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  Get a quadrature rule for a triangle.'
      write ( *, '(a)' ) '  Then write it to a file.'
      write ( *, '(a,i4)' ) 
     &  '  Polynomial exactness degree DEGREE = ', degree
c
c  Retrieve a symmetric quadrature rule.
c
      call triasymq ( degree, vert1, vert2, vert3, rnodes, weights, 
     &  numnodes )
c
c  Write the points and weights to a file.
c
      call get_unit ( rule_unit )

      rule_filename = trim ( header ) // '.txt'

      open ( unit = rule_unit, file = rule_filename, 
     &  status = 'replace' )
      do i = 1, numnodes
        write ( rule_unit, '(3(e21.15,2x))' ) 
     &    rnodes(1,i), rnodes(2,i), weights(i)
      end do
      close ( unit = rule_unit )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Quadrature rule written to file "' 
     &  // trim ( rule_filename ) // '".'

      return
      end
      subroutine test05 ( degree, numnodes, vert1, vert2, vert3 )

c*********************************************************************72
c
cc TEST05 calls TRIASYMQ for a quadrature rule of given order and region.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    28 June 2014
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
c    of the quadrature rule.  0 <= DEGREE <= 50.
c
c    Input, integer NUMNODES, the number of nodes to be used by the rule.
c
c    Input, double precision VERT1(2), VERT2(2), VERT3(2), the
c    vertices of the triangle.
c
      implicit none

      integer degree
      integer numnodes

      double precision area
      double precision d
      integer i
      integer j
      integer npols
      double precision pols(((degree+1)*(degree+2))/2)
      double precision r(2)
      double precision rints(((degree+1)*(degree+2))/2)
      double precision rnodes(2,numnodes)
      double precision triangle_area
      double precision scale
      double precision vert1(2)
      double precision vert2(2)
      double precision vert3(2)
      double precision weights(numnodes)
      double precision work((degree+1)*(degree+2))
      double precision z(2)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  Compute a quadrature rule for a triangle.'
      write ( *, '(a)' ) 
     &  '  Check it by integrating orthonormal polynomials.'
      write ( *, '(a,i4)' ) 
     &  '  Polynomial exactness degree DEGREE = ', degree

      area = triangle_area ( vert1, vert2, vert3 )
c
c  Retrieve a symmetric quadrature rule.
c
      call triasymq ( degree, vert1, vert2, vert3, rnodes, weights, 
     &  numnodes )
c
c  Construct the matrix of values of the orthogonal polynomials
c  at the user-provided nodes        
c
      npols = ( degree + 1 ) * ( degree + 2 ) / 2

      do j = 1, npols
        rints(j) = 0.0D+00
      end do

      do i = 1, numnodes
        z(1) = rnodes(1,i)
        z(2) = rnodes(2,i)
        call triangle_to_ref ( vert1, vert2, vert3, z, r )
        call ortho2eva ( degree, r, pols, work )
        do j = 1, npols
          rints(j) = rints(j) + weights(i) * pols(j)
        end do
      end do

      scale = sqrt ( sqrt ( 3.0D+00 ) ) / sqrt ( area )
      do j = 1, npols
        rints(j) = rints(j) * scale
      end do

      d = ( rints(1) - sqrt ( area ) )**2
      do i = 2, npols
        d = d + rints(i)**2
      enddo
      d = sqrt ( d ) / dble ( npols )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  RMS integration error = ', d

      return
      end
