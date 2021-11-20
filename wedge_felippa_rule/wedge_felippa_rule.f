      subroutine comp_next ( n, k, a, more, h, t )

c*********************************************************************72
c
cc COMP_NEXT computes the compositions of the integer N into K parts.
c
c  Discussion:
c
c    A composition of the integer N into K parts is an ordered sequence
c    of K nonnegative integers which sum to N.  The compositions (1,2,1)
c    and (1,1,2) are considered to be distinct.
c
c    The routine computes one composition on each call until there are no more.
c    For instance, one composition of 6 into 3 parts is
c    3+2+1, another would be 6+0+0.
c
c    On the first call to this routine, set MORE = FALSE.  The routine
c    will compute the first element in the sequence of compositions, and
c    return it, as well as setting MORE = TRUE.  If more compositions
c    are desired, call again, and again.  Each time, the routine will
c    return with a new composition.
c
c    However, when the LAST composition in the sequence is computed 
c    and returned, the routine will reset MORE to FALSE, signaling that
c    the end of the sequence has been reached.
c
c    This routine originally used a SAVE statement to maintain the
c    variables H and T.  I have decided (based on an wasting an
c    entire morning trying to track down a problem) that it is safer
c    to pass these variables as arguments, even though the user should
c    never alter them.  This allows this routine to safely shuffle
c    between several ongoing calculations.
c
c
c    There are 28 compositions of 6 into three parts.  This routine will
c    produce those compositions in the following order:
c
c     I         A
c     -     ---------
c     1     6   0   0
c     2     5   1   0
c     3     4   2   0
c     4     3   3   0
c     5     2   4   0
c     6     1   5   0
c     7     0   6   0
c     8     5   0   1
c     9     4   1   1
c    10     3   2   1
c    11     2   3   1
c    12     1   4   1
c    13     0   5   1
c    14     4   0   2
c    15     3   1   2
c    16     2   2   2
c    17     1   3   2
c    18     0   4   2
c    19     3   0   3
c    20     2   1   3
c    21     1   2   3
c    22     0   3   3
c    23     2   0   4
c    24     1   1   4
c    25     0   2   4
c    26     1   0   5
c    27     0   1   5
c    28     0   0   6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 July 2008
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Second Edition,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the integer whose compositions are desired.
c
c    Input, integer K, the number of parts in the composition.
c
c    Input/output, integer A(K), the parts of the composition.
c
c    Input/output, logical MORE, set by the user to start the computation,
c    and by the routine to terminate it.
c
c    Input/output, integer H, T, two internal parameters needed for the
c    computation.  The user should allocate space for these in the calling
c    program, include them in the calling sequence, but never alter them!
c
      implicit none

      integer k

      integer a(k)
      integer h
      integer i
      logical more
      integer n
      integer t
c
c  The first computation.
c
      if ( .not. more ) then

        t = n
        h = 0
        a(1) = n
        do i = 2, k
          a(i) = 0
        end do
c
c  The next computation.
c
      else

        if ( 1 .lt. t ) then
          h = 0
        end if

        h = h + 1
        t = a(h)
        a(h) = 0
        a(1) = t - 1
        a(h+1) = a(h+1) + 1

      end if
c
c  This is the last element of the sequence if all the
c  items are in the last slot.
c
      more = ( a(k) .ne. n )

      return
      end
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine line_o01 ( w, x )

c*********************************************************************72
c
cc LINE_O01 returns a 1 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(1), the weights.
c
c    Output, double precision X(1), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      double precision w(order)
      double precision w_save(1)
      double precision x(order)
      double precision x_save(1)

      save w_save
      save x_save

      data w_save /
     &  1.0D+00 /
      data x_save /
     & 0.0D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8vec_copy ( order, x_save, x )

      return
      end
      subroutine line_o02 ( w, x )

c*********************************************************************72
c
cc LINE_O02 returns a 2 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(2), the weights.
c
c    Output, double precision X(2), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 2 )

      double precision w(order)
      double precision w_save(2)
      double precision x(order)
      double precision x_save(2)

      save w_save
      save x_save

      data w_save /
     &  0.5D+00,
     &  0.5D+00 /
      data x_save /
     &  -0.57735026918962576451D+00,
     &   0.57735026918962576451D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8vec_copy ( order, x_save, x )

      return
      end
      subroutine line_o03 ( w, x )

c*********************************************************************72
c
cc LINE_O03 returns a 3 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(3), the weights.
c
c    Output, double precision X(3), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 3 )

      double precision w(order)
      double precision w_save(3)
      double precision x(order)
      double precision x_save(3)

      save w_save
      save x_save

      data w_save /
     &  0.27777777777777777777D+00,
     &  0.44444444444444444444D+00,
     &  0.27777777777777777777D+00 /
      data x_save /
     &  -0.77459666924148337704D+00,
     &   0.00000000000000000000D+00,
     &   0.77459666924148337704D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8vec_copy ( order, x_save, x )

      return
      end
      subroutine line_o04 ( w, x )

c*********************************************************************72
c
cc LINE_O04 returns a 4 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(4), the weights.
c
c    Output, double precision X(4), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 4 )

      double precision line_volume
      double precision w(order)
      double precision w_save(4)
      double precision x(order)
      double precision x_save(4)

      save w_save
      save x_save

      data w_save /
     &  0.173927422568727D+00,    
     &  0.326072577431273D+00,
     &  0.326072577431273D+00,
     &  0.173927422568727D+00 /
      data x_save /
     &  -0.86113631159405257522D+00,
     &  -0.33998104358485626480D+00,
     &   0.33998104358485626480D+00,
     &   0.86113631159405257522D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8vec_copy ( order, x_save, x )

      return
      end
      subroutine line_o05 ( w, x )

c*********************************************************************72
c
cc LINE_O05 returns a 5 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(5), the weights.
c
c    Output, double precision X(5), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 5 )

      double precision w(order)
      double precision w_save(5)
      double precision x(order)
      double precision x_save(5)

      save w_save
      save x_save

      data w_save /
     &  0.118463442528095D+00,    
     &  0.239314335249683D+00,
     &  0.284444444444444D+00,
     &  0.239314335249683D+00,
     &  0.118463442528095D+00 /
      data x_save /
     &  -0.90617984593866399280D+00,
     &  -0.53846931010568309104D+00,
     &   0.00000000000000000000D+00,
     &   0.53846931010568309104D+00,
     &   0.90617984593866399280D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8vec_copy ( order, x_save, x )

      return
      end
      subroutine monomial_value ( m, n, e, x, v )

c*****************************************************************************80
c
cc MONOMIAL_VALUE evaluates a monomial.
c
c  Discussion:
c
c    This routine evaluates a monomial of the form
c
c      product ( 1 <= i <= m ) x(i)^e(i)
c
c    The combination 0.0^0 is encountered is treated as 1.0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, integer E(M), the exponents.
c
c    Input, double precision X(M,N), the point coordinates.
c
c    Output, double precision V(N), the monomial values.
c
      implicit none

      integer m
      integer n

      integer e(m)
      integer i
      integer j
      double precision v(n)
      double precision x(m,n)

      do j = 1, n
        v(j) = 1.0D+00
      end do

      do i = 1, m
        if ( 0 .ne. e(i) ) then
          do j = 1, n
            v(j) = v(j) * x(i,j) ** e(i)
          end do
        end if
      end do

      return
      end
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(M,N), a copy of the matrix.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      subroutine r8mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_WRITE writes a R8MAT file.
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
c    22 October 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      double precision table(m,n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a1,i8,a1,i8,a1)' )
     &    '(', m, 'g', 24, '.', 16, ')'
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, string ) table(1:m,j)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

      return
      end
      subroutine r8vec_copy ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_COPY copies an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, double precision A1(N), the vector to be copied.
c
c    Output, double precision A2(N), a copy of A1.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i

      do i = 1, n
        a2(i) = a1(i)
      end do

      return
      end
      function r8vec_dot_product ( n, v1, v2 )

c*********************************************************************72
c
cc R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine DOT_PRODUCT should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), V2(N), the vectors.
c
c    Output, double precision R8VEC_DOT_PRODUCT, the dot product.
c
      implicit none

      integer n

      integer i
      double precision r8vec_dot_product
      double precision v1(n)
      double precision v2(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i) * v2(i)
      end do

      r8vec_dot_product = value

      return
      end
      subroutine subcomp_next ( n, k, a, more, h, t )

c*********************************************************************72
c
cc SUBCOMP_NEXT computes the next subcomposition of N into K parts.
c
c  Discussion:
c
c    A composition of the integer N into K parts is an ordered sequence
c    of K nonnegative integers which sum to a value of N.
c
c    A subcomposition of the integer N into K parts is a composition
c    of M into K parts, where 0 .le. M .le. N.
c
c    A subcomposition of the integer N into K parts is also a lattice
c    point in the simplex whose vertices are the origin, and the K direction
c    vectors N*E(I) for I = 1 to K.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the integer whose subcompositions are desired.
c
c    Input, integer K, the number of parts in the subcomposition.
c
c    Input/output, integer A(K), the parts of the subcomposition.
c
c    Input/output, logical MORE, set by the user to start the computation,
c    and by the routine to terminate it.
c
c    Input/output, integer H, T, two internal parameters needed for the
c    computation.  The user should allocate space for these in the calling
c    program, include them in the calling sequence, but never alter them!
c
      implicit none

      integer k

      integer a(k)
      integer h
      integer i
      logical more
      logical more2
      integer n
      integer n2
      integer t

      save more2
      save n2

      data more2 / .false. /
      data n2 / 0 /
c
c  The first computation.
c
      if ( .not. more ) then

        more = .true.

        do i = 1, k
          a(i) = 0
        end do

        n2 = 0
        more2 = .false.
c
c  Do the next element at the current value of N.
c    
      else if ( more2 ) then

        call comp_next ( n2, k, a, more2, h, t )

      else

        more2 = .false.
        n2 = n2 + 1

        call comp_next ( n2, k, a, more2, h, t )
        
      end if
c
c  Termination occurs if MORE2 = FALSE and N2 = N.
c
      if ( .not. more2 .and. n2 .eq. n ) then
        more = .false.
      end if

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
      subroutine triangle_o01 ( w, xy )

c*********************************************************************72
c
cc TRIANGLE_O01 returns a 1 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 1.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(1), the weights.
c
c    Output, double precision XY(2,1), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      double precision w(order)
      double precision w_save(1)
      double precision xy(2,order)
      double precision xy_save(2,1)

      save w_save
      save xy_save

      data w_save /
     &  1.0D+00 /
      data xy_save /
     &  0.33333333333333333333D+00,     0.33333333333333333333D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine triangle_o03 ( w, xy )

c*********************************************************************72
c
cc TRIANGLE_O03 returns a 3 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 2.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(3), the weights.
c
c    Output, double precision XY(2,3), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 3 )

      double precision w(order)
      double precision w_save(3)
      double precision xy(2,order)
      double precision xy_save(2,3)

      save w_save
      save xy_save

      data w_save /
     &  0.33333333333333333333D+00,
     &  0.33333333333333333333D+00,
     &  0.33333333333333333333D+00 /
      data xy_save /
     &  0.66666666666666666667D+00,     0.16666666666666666667D+00,
     &  0.16666666666666666667D+00,     0.66666666666666666667D+00,
     &  0.16666666666666666667D+00,     0.16666666666666666667D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine triangle_o03b ( w, xy )

c*********************************************************************72
c
cc TRIANGLE_O03B returns a 3 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 2.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(3), the weights.
c
c    Output, double precision XY(2,3), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 3 )

      double precision w(order)
      double precision w_save(3)
      double precision xy(2,order)
      double precision xy_save(2,3)

      save w_save
      save xy_save

      data w_save /
     &  0.33333333333333333333D+00,
     &  0.33333333333333333333D+00,
     &  0.33333333333333333333D+00 /
      data xy_save /
     &  0.0D+00,     0.5D+00,
     &  0.5D+00,     0.0D+00,
     &  0.5D+00,     0.5D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine triangle_o06 ( w, xy )

c*********************************************************************72
c
cc TRIANGLE_O06 returns a 6 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 4.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(6), the weights.
c
c    Output, double precision XY(2,6), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 6 )

      double precision w(order)
      double precision w_save(6)
      double precision xy(2,order)
      double precision xy_save(2,6)

      save w_save
      save xy_save

      data w_save /
     &  0.22338158967801146570D+00,
     &  0.22338158967801146570D+00,
     &  0.22338158967801146570D+00, 
     &  0.10995174365532186764D+00,
     &  0.10995174365532186764D+00,   
     &  0.10995174365532186764D+00 /
      data xy_save /
     &  0.10810301816807022736D+00,     0.44594849091596488632D+00,
     &  0.44594849091596488632D+00,     0.10810301816807022736D+00,
     &  0.44594849091596488632D+00,     0.44594849091596488632D+00,
     &  0.81684757298045851308D+00,     0.091576213509770743460D+00,
     &  0.091576213509770743460D+00,    0.81684757298045851308D+00,
     &  0.091576213509770743460D+00,    0.091576213509770743460D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine triangle_o06b ( w, xy )

c*********************************************************************72
c
cc TRIANGLE_O06B returns a 6 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 3.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(6), the weights.
c
c    Output, double precision XY(2,6), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 6 )

      double precision w(order)
      double precision w_save(6)
      double precision xy(2,order)
      double precision xy_save(2,6)

      save w_save
      save xy_save

      data w_save /
     &  0.30000000000000000000D+00,
     &  0.30000000000000000000D+00,
     &  0.30000000000000000000D+00, 
     &  0.033333333333333333333D+00,
     &  0.033333333333333333333D+00, 
     &  0.033333333333333333333D+00 /
      data xy_save /
     &  0.66666666666666666667D+00,     0.16666666666666666667D+00,
     &  0.16666666666666666667D+00,     0.66666666666666666667D+00,
     &  0.16666666666666666667D+00,     0.16666666666666666667D+00,
     &  0.0D+00,     0.5D+00,
     &  0.5D+00,     0.0D+00,
     &  0.5D+00,     0.5D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine triangle_o07 ( w, xy )

c*********************************************************************72
c
cc TRIANGLE_O07 returns a 7 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 5.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(7), the weights.
c
c    Output, double precision XY(2,7), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 7 )

      double precision w(order)
      double precision w_save(7)
      double precision xy(2,order)
      double precision xy_save(2,7)

      save w_save
      save xy_save

      data w_save /
     &  0.12593918054482715260D+00,
     &  0.12593918054482715260D+00,
     &  0.12593918054482715260D+00, 
     &  0.13239415278850618074D+00,
     &  0.13239415278850618074D+00,   
     &  0.13239415278850618074D+00,
     &  0.22500000000000000000D+00 /
      data xy_save /
     &  0.79742698535308732240D+00,     0.10128650732345633880D+00,
     &  0.10128650732345633880D+00,     0.79742698535308732240D+00,
     &  0.10128650732345633880D+00,     0.10128650732345633880D+00,
     &  0.059715871789769820459D+00,    0.47014206410511508977D+00,
     &  0.47014206410511508977D+00,     0.059715871789769820459D+00,
     &  0.47014206410511508977D+00,     0.47014206410511508977D+00,
     &  0.33333333333333333333D+00,     0.33333333333333333333D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine triangle_o12 ( w, xy )

c*********************************************************************72
c
cc TRIANGLE_O12 returns a 12 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 6.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(12), the weights.
c
c    Output, double precision XY(2,12), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 12 )

      double precision w(order)
      double precision w_save(12)
      double precision xy(2,order)
      double precision xy_save(2,12)

      save w_save
      save xy_save

      data w_save /
     & 0.050844906370206816921D+00,
     & 0.050844906370206816921D+00,
     & 0.050844906370206816921D+00,
     & 0.11678627572637936603D+00,
     & 0.11678627572637936603D+00,
     & 0.11678627572637936603D+00,
     & 0.082851075618373575194D+00,
     & 0.082851075618373575194D+00,
     & 0.082851075618373575194D+00,
     & 0.082851075618373575194D+00,
     & 0.082851075618373575194D+00,
     & 0.082851075618373575194D+00 /
      data xy_save /
     & 0.87382197101699554332D+00,     0.063089014491502228340D+00,
     & 0.063089014491502228340D+00,    0.87382197101699554332D+00,
     & 0.063089014491502228340D+00,    0.063089014491502228340D+00,
     & 0.5014265096587915742D+00,      0.24928674517091042129D+00,
     & 0.24928674517091042129D+00,     0.50142650965817915742D+00,
     & 0.24928674517091042129D+00,     0.24928674517091042129D+00,
     & 0.053145049844816947353D+00,    0.31035245103378440542D+00,
     & 0.31035245103378440542D+00,     0.053145049844816947353D+00,
     & 0.053145049844816947353D+00,    0.63650249912139864723D+00,
     & 0.31035245103378440542D+00,     0.63650249912139864723D+00,
     & 0.63650249912139864723D+00,     0.053145049844816947353D+00,
     & 0.63650249912139864723D+00,     0.31035245103378440542D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine wedge_integral ( expon, value )

c*********************************************************************72
c
cc WEDGE_INTEGRAL: monomial integral in a unit wedge.
c
c  Discussion:
c
c    This routine returns the integral of
c
c      product ( 1 <= I <= 3 ) X(I)^EXPON(I)
c
c    over the unit wedge.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1
c      -1 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Arthur Stroud,
c    Approximate Calculation of Multiple Integrals,
c    Prentice Hall, 1971,
c    ISBN: 0130438936,
c    LC: QA311.S85.
c
c  Parameters:
c
c    Input, integer EXPON(3), the exponents.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      integer expon(3)
      integer i
      integer k
      double precision value
c
c  The first computation ends with VALUE = 1.0;
c
      value = 1.0D+00

      k = expon(1)

      do i = 1, expon(2)
        k = k + 1
        value = value * dble ( i ) / dble ( k )
      end do

      k = k + 1
      value = value / dble ( k )

      k = k + 1
      value = value / dble ( k )
c
c  Now account for integration in Z.
c
      if ( expon(3) .eq. - 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEDGE_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  EXPON(3) = -1 is not a legal input.'
        stop 1
      else if ( mod ( expon(3), 2 ) .eq. 1 ) then
        value = 0.0D+00
      else
        value = value * 2.0D+00 / dble ( expon(3) + 1 )
      end if

      return
      end
      subroutine wedge_rule ( line_order, triangle_order, w, xyz )

c*********************************************************************72
c
cc WEDGE_RULE returns a quadrature rule for the unit wedge.
c
c  Discussion:
c
c    It is usually sensible to take LINE_ORDER and TRIG_ORDER so that
c    the line and triangle rules are roughly the same precision.  For that
c    criterion, we recommend the following combinations:
c
c      TRIANGLE_ORDER  LINE_ORDER  Precision
c      --------------  ----------  ---------
c          1               1       1 x 1
c          3               2       2 x 3
c         -3               2       2 x 3
c          6               3       4 x 5
c         -6               2       3 x 3
c          7               3       5 x 5
c         12               4       6 x 7
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1
c      -1 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Input, integer LINE_ORDER, the index of the line rule.
c    The index of the rule is equal to the order of the rule.
c    1 <= LINE_ORDER <= 5.
c
c    Input, integer TRIANGLE_ORDER, the indes of the triangle rule.
c    The index of the rule is 1, 3, -3, 6, -6, 7 or 12.
c
c    Output, double precision W(LINE_ORDER*abs(TRIANGLE_ORDER)), the weights.
c
c    Output, double precision XYZ(3,LINE_ORDER*abs(TRIANGLE_ORDER)),
c    the abscissas.
c
      implicit none

      integer line_order
      integer triangle_order

      integer i
      integer j
      integer k
      double precision line_w(line_order)
      double precision line_x(line_order)
      double precision triangle_w(abs(triangle_order))
      double precision triangle_xy(2,abs(triangle_order))
      double precision w(line_order*abs(triangle_order))
      double precision xyz(3,line_order*abs(triangle_order))

      if ( line_order .eq. 1 ) then
        call line_o01 ( line_w, line_x )
      else if ( line_order .eq. 2 ) then
        call line_o02 ( line_w, line_x )
      else if ( line_order .eq. 3 ) then
        call line_o03 ( line_w, line_x )
      else if ( line_order .eq. 4 ) then
        call line_o04 ( line_w, line_x )
      else if ( line_order .eq. 5 ) then
        call line_o05 ( line_w, line_x )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEDGE_RULE - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of LINE_ORDER.'
        stop 1
      end if

      if ( triangle_order .eq. 1 ) then
        call triangle_o01 ( triangle_w, triangle_xy )
      else if ( triangle_order .eq. 3 ) then
        call triangle_o03 ( triangle_w, triangle_xy )
      else if ( triangle_order .eq. - 3 ) then
        call triangle_o03b ( triangle_w, triangle_xy )
      else if ( triangle_order .eq. 6 ) then
        call triangle_o06 ( triangle_w, triangle_xy )
      else if ( triangle_order .eq. - 6 ) then
        call triangle_o06b ( triangle_w, triangle_xy )
      else if ( triangle_order .eq. 7 ) then
        call triangle_o07 ( triangle_w, triangle_xy )
      else if ( triangle_order .eq. 12 ) then
        call triangle_o12 ( triangle_w, triangle_xy )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEDGE_RULE - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of TRIANGLE_ORDER.'
        stop 1
      end if

      k = 0
      do i = 1, line_order
        do j = 1, abs ( triangle_order )
          k = k + 1
          w(k) = line_w(i) * triangle_w(j)
          xyz(1,k) = triangle_xy(1,j)
          xyz(2,k) = triangle_xy(2,j)
          xyz(3,k) = line_x(i)
        end do
      end do

      return
      end
      function wedge_volume ( )

c*********************************************************************72
c
cc WEDGE_VOLUME: volume of a unit wedge.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1
c      -1 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision WEDGE_VOLUME, the volume.
c
      implicit none

      double precision wedge_volume

      wedge_volume = 1.0D+00

      return
      end
