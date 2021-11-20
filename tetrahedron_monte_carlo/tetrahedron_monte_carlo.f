       function r8mat_det_4d ( a )

c*********************************************************************72
c
cc R8MAT_DET_4D computes the determinant of a 4 by 4 R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(4,4), the matrix whose determinant is desired.
c
c    Output, double precision R8MAT_DET_4D, the determinant of the matrix.
c
      implicit none

      double precision a(4,4)
      double precision r8mat_det_4d

      r8mat_det_4d =
     &       a(1,1) * (
     &           a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) )
     &         - a(2,3) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) )
     &         + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) )
     &     - a(1,2) * (
     &           a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) )
     &         - a(2,3) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) )
     &         + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) )
     &     + a(1,3) * (
     &           a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) )
     &         - a(2,2) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) )
     &         + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )
     &     - a(1,4) * (
     &           a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) )
     &         - a(2,2) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) )
     &         + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )

      return
      end
      subroutine r8mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character*(*) title

      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)') i
        end do

        write ( *, '(''       Row'',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '       Col'

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do

          write ( *, '(2x,i8,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      function r8vec_sum ( n, v1 )

c*********************************************************************72
c
cc R8VEC_SUM sums the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, the system routine SUM should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), the vector.
c
c    Output, double precision R8VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_sum
      double precision v1(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i)
      end do

      r8vec_sum = value

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine reference_to_physical_tet4 ( t, n, ref, phy )

c*********************************************************************72
c
cc REFERENCE_TO_PHYSICAL_TET4 maps TET4 reference points to physical points.
c
c  Discussion:
c
c    Given the vertices of an order 4 physical tetrahedron and a point
c    (R,S,T) in the reference tetrahedron, the routine computes the value
c    of the corresponding point (X,Y,Z) in the physical tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(3,4), the coordinates of the vertices.
c    The vertices are assumed to be the images of (1,0,0), (0,1,0),
c    (0,0,1) and (0,0,0) respectively.
c
c    Input, integer N, the number of points to transform.
c
c    Input, double precision REF(3,N), points in the reference tetrahedron.
c
c    Output, double precision PHY(3,N), corresponding points in the
c    physical tetrahedron.
c
      implicit none

      integer n

      integer i
      integer j
      double precision phy(3,n)
      double precision ref(3,n)
      double precision t(3,4)

      do j = 1, n
        do i = 1, 3
          phy(i,j) =                                                    
     &        t(i,1) *             ref(1,j)                           
     &      + t(i,2) *                        ref(2,j)            
     &      + t(i,3) *                                   ref(3,j)
     &      + t(i,4) * ( 1.0D+00 - ref(1,j) - ref(2,j) - ref(3,j) )
        end do
      end do

      return
      end
      subroutine tetrahedron_integrand_01 ( p_num, p, f_num, fp )

c*********************************************************************72
c
cc TETRAHEDRON_INTEGRAND_01 evaluates 1 integrand function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input, double precision P(3,P_NUM), the evaluation points.
c
c    Input, integer F_NUM, the number of integrands.
c
c    Output, double precision FP(F_NUM,P_NUM), the integrand values.
c
      implicit none

      integer f_num
      integer p_num

      double precision fp(f_num,p_num)
      integer j
      double precision p(3,p_num)

      do j = 1, p_num
        fp(1,j) = 1.0D+00
      end do

      return
      end
      subroutine tetrahedron_integrand_02 ( p_num, p, f_num, fp )

c*********************************************************************72
c
cc TETRAHEDRON_INTEGRAND_02 evaluates 3 integrand functions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input, double precision P(3,P_NUM), the evaluation points.
c
c    Input, integer F_NUM, the number of integrands.
c
c    Output, double precision FP(F_NUM,P_NUM), the integrand values.
c
      implicit none

      integer f_num
      integer p_num

      double precision fp(f_num,p_num)
      integer j
      double precision p(3,p_num)

      do j = 1, p_num
        fp(1,j) = p(1,j)
        fp(2,j) = p(2,j)
        fp(3,j) = p(3,j)
      end do

      return
      end
      subroutine tetrahedron_integrand_03 ( p_num, p, f_num, fp )

c*********************************************************************72
c
cc TETRAHEDRON_INTEGRAND_03 evaluates 6 integrand functions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input, double precision P(3,P_NUM), the evaluation points.
c
c    Input, integer F_NUM, the number of integrands.
c
c    Output, double precision FP(F_NUM,P_NUM), the integrand values.
c
      implicit none

      integer f_num
      integer p_num

      double precision fp(f_num,p_num)
      integer j
      double precision p(3,p_num)

      do j = 1, p_num
        fp(1,j) = p(1,j) * p(1,j)
        fp(2,j) = p(1,j) * p(2,j)
        fp(3,j) = p(1,j) * p(3,j)
        fp(4,j) = p(2,j) * p(2,j)
        fp(5,j) = p(2,j) * p(3,j)
        fp(6,j) = p(3,j) * p(3,j)
      end do

      return
      end
      subroutine tetrahedron_integrand_04 ( p_num, p, f_num, fp )

c*********************************************************************72
c
cc TETRAHEDRON_INTEGRAND_04 evaluates 10 integrand functions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input, double precision P(3,P_NUM), the evaluation points.
c
c    Input, integer F_NUM, the number of integrands.
c
c    Output, double precision FP(F_NUM,P_NUM), the integrand values.
c
      implicit none

      integer f_num
      integer p_num

      double precision fp(f_num,p_num)
      integer j
      double precision p(3,p_num)

      do j = 1, p_num
        fp( 1,j) = p(1,j)**3
        fp( 2,j) = p(1,j)**2 * p(2,j)
        fp( 3,j) = p(1,j)**2             * p(3,j)
        fp( 4,j) = p(1,j)    * p(2,j)**2
        fp( 5,j) = p(1,j)    * p(2,j)    * p(3,j)
        fp( 6,j) = p(1,j)                * p(3,j)**2
        fp( 7,j) =             p(2,j)**3
        fp( 8,j) =             p(2,j)**2 * p(3,j)
        fp( 9,j) =             p(2,j)    * p(3,j)**2
        fp(10,j) =                         p(3,j)**3
      end do

      return
      end
      subroutine tetrahedron_integrand_05 ( p_num, p, f_num, fp )

c*********************************************************************72
c
cc TETRAHEDRON_INTEGRAND_05 evaluates 15 integrand functions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input, double precision P(3,P_NUM), the evaluation points.
c
c    Input, integer F_NUM, the number of integrands.
c
c    Output, double precision FP(F_NUM,P_NUM), the integrand values.
c
      implicit none

      integer f_num
      integer p_num

      double precision fp(f_num,p_num)
      integer j
      double precision p(3,p_num)

      do j = 1, p_num
        fp( 1,j) = p(1,j)**4
        fp( 2,j) = p(1,j)**3 * p(2,j)
        fp( 3,j) = p(1,j)**3             * p(3,j)
        fp( 4,j) = p(1,j)**2 * p(2,j)**2
        fp( 5,j) = p(1,j)**2 * p(2,j)    * p(3,j)
        fp( 6,j) = p(1,j)**2             * p(3,j)**2
        fp( 7,j) = p(1,j)    * p(2,j)**3
        fp( 8,j) = p(1,j)    * p(2,j)**2 * p(3,j)
        fp( 9,j) = p(1,j)    * p(2,j)    * p(3,j)**2
        fp(10,j) = p(1,j)                * p(3,j)**3
        fp(11,j) =             p(2,j)**4
        fp(12,j) =             p(2,j)**3 * p(3,j)
        fp(13,j) =             p(2,j)**2 * p(3,j)**2
        fp(14,j) =             p(2,j)    * p(3,j)**3
        fp(15,j) =                         p(3,j)**4
      end do

      return
      end
      subroutine tetrahedron_monte_carlo ( t, p_num, f_num,
     &  tetrahedron_unit_sample, tetrahedron_integrand, seed, result )

c*********************************************************************72
c
cc TETRAHEDRON_MONTE_CARLO applies the Monte Carlo rule to integrate a function.
c
c  Discussion:
c
c    The function f(x,y,z) is to be integrated over a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(3,4), the vertices.
c
c    Input, integer P_NUM, the number of sample points.
c
c    Input, integer F_NUM, the number of functions to integrate.
c
c    Input, external TETRAHEDRON_UNIT_SAMPLE, the sampling routine.
c
c    Input, external TETRAHEDRON_INTEGRAND, the integrand routine.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision RESULT(F_NUM), the approximate integrals.
c
      implicit none

      integer f_num
      integer p_num

      double precision fp(f_num,p_num)
      integer i
      double precision p(3,p_num)
      double precision p2(3,p_num)
      double precision r8vec_sum
      double precision result(f_num)
      integer seed
      double precision t(3,4)
      external tetrahedron_sample
      external tetrahedron_integrand
      double precision volume

      call tetrahedron_volume ( t, volume )

      call tetrahedron_unit_sample ( p_num, seed, p )

      call reference_to_physical_tet4 ( t, p_num, p, p2 )

      call tetrahedron_integrand ( p_num, p2, f_num, fp )

      do i = 1, f_num
        result(i) = volume * r8vec_sum ( p_num, fp(i,1:p_num) ) 
     &    / dble ( p_num )
      end do

      return
      end
      subroutine tetrahedron_unit_sample_01 ( p_num, seed, p )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_SAMPLE_01 selects points from the unit tetrahedron.
c
c  Discussion:
c
c    The unit tetrahedron has vertices (1,0,0), (0,1,0), (0,0,1), (0,0,0).
c
c    Any point in the unit tetrahedron CAN be chosen by this algorithm.
c
c    However, the points that are chosen tend to be clustered near
c    the centroid.
c
c    This routine is supplied as an example of "bad" sampling.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision P(3,P_NUM), the points.
c
      implicit none

      integer p_num

      double precision e(4)
      double precision e_sum
      integer i
      double precision r8vec_sum
      integer j
      double precision p(3,p_num)
      integer seed

      do j = 1, p_num

        call r8vec_uniform_01 ( 4, seed, e )

        e_sum = r8vec_sum ( 4, e )

        do i = 1, 4
          e(i) = e(i) / e_sum
        end do
c
c  We may take the values E(1:3) as being the barycentric
c  coordinates of the point.
c
        do i = 1, 3
          p(i,j) = e(i)
        end do

      end do

      return
      end
      subroutine tetrahedron_unit_sample_02 ( p_num, seed, p )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_SAMPLE_02 selects points from the unit tetrahedron.
c
c  Discussion:
c
c    The unit tetrahedron has vertices (1,0,0), (0,1,0), (0,0,1), (0,0,0).
c
c    The sampling is uniform.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Claudio Rocchini, Paolo Cignoni,
c    Generating Random Points in a Tetrahedron,
c    Journal of Graphics Tools,
c    Volume 5, Number 5, 2000, pages 9-12.
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision P(3,P_NUM), the points.
c
      implicit none

      integer p_num

      double precision c(3)
      integer i
      integer j
      integer seed
      double precision t
      double precision p(3,p_num)

      do j = 1, p_num

        call r8vec_uniform_01 ( 3, seed, c )

        if ( 1.0D+00 .lt. c(1) + c(2) ) then
          c(1) = 1.0D+00 - c(1)
          c(2) = 1.0D+00 - c(2)
        end if

        if ( 1.0D+00 .lt. c(2) + c(3) ) then
          t = c(3)
          c(3) = 1.0D+00 - c(1) - c(2)
          c(2) = 1.0D+00 - t
        else if ( 1.0D+00 .lt. c(1) + c(2) + c(3) ) then
          t = c(3)
          c(3) = c(1) + c(2) + c(3) - 1.0D+00
          c(1) = 1.0D+00 - c(2) - t
        end if

        do i = 1, 3
          p(i,j) = c(i)
        end do

      end do

      return
      end
      subroutine tetrahedron_unit_sample_03 ( p_num, seed, p )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_SAMPLE_03 selects points from the unit tetrahedron.
c
c  Discussion:
c
c    The unit tetrahedron has vertices (1,0,0), (0,1,0), (0,0,1), (0,0,0).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Greg Turk,
c    Generating Random Points in a Triangle,
c    in Graphics Gems,
c    edited by Andrew Glassner,
c    AP Professional, 1990, pages 24-28.
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision P(3,P_NUM), the points.
c
      implicit none

      integer p_num

      double precision a
      double precision b
      double precision c
      double precision d
      double precision e
      double precision f
      double precision g
      integer j
      double precision p(3,p_num)
      double precision r(3)
      integer seed

      do j = 1, p_num

        call r8vec_uniform_01 ( 3, seed, r )

        e = r(1) ** ( 1.0D+00 / 3.0D+00 )
        f = sqrt ( r(2) )
        g = r(3)

        a =   1.0D+00 - e
        b = ( 1.0D+00 - f )       * e
        c = ( 1.0D+00 - g ) * f   * e
        d =             g   * f   * e

        p(1,j) = a
        p(2,j) = b
        p(3,j) = c

      end do

      return
      end
      subroutine tetrahedron_unit_sample_04 ( p_num, seed, p )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_SAMPLE_04 selects points from the unit tetrahedron.
c
c  Discussion:
c
c    The unit tetrahedron has vertices (1,0,0), (0,1,0), (0,0,1), (0,0,0).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation, and Sensitivity
c    of Queueing Networks,
c    Krieger, 1992,
c    ISBN: 0894647644,
c    LC: QA298.R79.
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision P(3,P_NUM), the points.
c
      implicit none

      integer p_num

      double precision e(4)
      double precision e_sum
      integer i
      integer j
      double precision p(3,p_num)
      double precision r8vec_sum
      integer seed
c
c  The construction begins by sampling DIM_NUM+1 points from the
c  exponential distribution with parameter 1.
c
      do j = 1, p_num

        call r8vec_uniform_01 ( 4, seed, e )

        do i = 1, 4
          e(i) = - log ( e(i) )
        end do

        e_sum = r8vec_sum ( 4, e )

        do i = 1, 3
          p(i,j) = e(i) / e_sum
        end do

      end do

      return
      end
      subroutine tetrahedron_volume ( tet_xyz, volume )

c*********************************************************************72
c
cc TETRAHEDRON_VOLUME computes the volume of a tetrahedron in 3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TET_XYZ(3,4), the coordinates of the vertices.
c
c    Output, double precision VOLUME, the volume of the tetrahedron.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )

      double precision a(4,4)
      integer i
      integer j
      double precision r8mat_det_4d
      double precision tet_xyz(dim_num,4)
      double precision volume

      do j = 1, 4
        do i = 1, 3
          a(i,j) = tet_xyz(i,j)
        end do
        a(4,j) = 1.0D+00
      end do

      volume = abs ( r8mat_det_4d ( a ) ) / 6.0D+00

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ',
     &  'May      ', 'June     ', 'July     ', 'August   ',
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *,
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' )
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
