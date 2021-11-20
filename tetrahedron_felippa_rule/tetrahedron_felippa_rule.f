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
      subroutine monomial_value ( m, n, e, x, v )

c*********************************************************************72
c
cc MONOMIAL_VALUE evaluates a monomial.
c
c  Discussion:
c
c    F(X) = product ( 1 <= DIM <= M ) X(I)^EXPON(I)
c
c    with the convention that 0^0 = 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, integer E(M), the exponents.
c
c    Input, double precision X(M,N), the evaluation points.
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
        if ( e(i) .ne. 0.0D+00 ) then
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
      subroutine tetrahedron_unit_monomial ( expon, value )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_MONOMIAL integrates a monomial over the unit tetrahedron.
c
c  Discussion:
c
c    This routine integrates a monomial of the form
c
c      product ( 1 <= dim <= 3 ) x(dim)^expon(dim)
c
c    where the exponents are nonnegative integers.  Note that
c    if the combination 0^0 is encountered, it should be treated
c    as 1.
c
c    Integral ( over unit tetrahedron ) x^l y^m z^n dx dy =
c    l! * m! * n! / ( m + n + 3 )!
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      0 <= X + Y + Z <= 1.
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
c
c  The first loop simply calculates 1, so we short circuit it.
c
      k = expon(1)
      do i = 1, expon(2)
        k = k + 1
        value = value * dble ( i ) / real ( k )
      end do

      do i = 1, expon(3)
        k = k + 1
        value = value * dble ( i ) / real ( k )
      end do

      k = k + 1
      value = value / dble ( k )

      k = k + 1
      value = value / dble ( k )

      k = k + 1
      value = value / dble ( k )

      return
      end
      subroutine tetrahedron_unit_o01 ( w, xyz )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_O01 returns a 1 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
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
c    Output, double precision XYZ(3,1), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      double precision w(order)
      double precision w_save(1)
      double precision xyz(3,order)
      double precision xyz_save(3,1)

      data w_save /
     &  1.0000000000000000000D+00 /
      data xyz_save /
     &  0.25000000000000000000D+00,  
     &  0.25000000000000000000D+00,  
     &  0.25000000000000000000D+00 /

      save w_save
      save xyz_save

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetrahedron_unit_o04 ( w, xyz )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_O04 returns a 4 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
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
c    Output, double precision XYZ(3,4), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 4 )

      double precision w(order)
      double precision w_save(4)
      double precision xyz(3,order)
      double precision xyz_save(3,4)

      save w_save
      save xyz_save

      data w_save /
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00 /
      data xyz_save /
     & 0.58541019662496845446D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.58541019662496845446D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.58541019662496845446D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetrahedron_unit_o08 ( w, xyz )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_O08 returns an 8 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
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
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(8), the weights.
c
c    Output, double precision XYZ(3,8), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 8 )

      double precision w(order)
      double precision w_save(8)
      double precision xyz(3,order)
      double precision xyz_save(3,8)

      save w_save
      save xyz_save

      data w_save /
     & 0.13852796651186214232D+00, 
     & 0.13852796651186214232D+00, 
     & 0.13852796651186214232D+00, 
     & 0.13852796651186214232D+00, 
     & 0.11147203348813785768D+00, 
     & 0.11147203348813785768D+00, 
     & 0.11147203348813785768D+00, 
     & 0.11147203348813785768D+00 /
      data xyz_save /
     & 0.015835909865720057993D+00, 
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.015835909865720057993D+00, 
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.015835909865720057993D+00, 
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.67914317820120795168D+00,
     & 0.10695227393293068277D+00, 
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00,
     & 0.67914317820120795168D+00,
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00,
     & 0.67914317820120795168D+00,
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetrahedron_unit_o08b ( w, xyz )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_O08B returns an 8 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
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
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(8), the weights.
c
c    Output, double precision XYZ(3,8), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 8 )

      double precision w(order)
      double precision w_save(8)
      double precision xyz(3,order)
      double precision xyz_save(3,8)

      save w_save
      save xyz_save

      data w_save /
     & 0.025000000000000000000D+00, 
     & 0.025000000000000000000D+00, 
     & 0.025000000000000000000D+00, 
     & 0.025000000000000000000D+00, 
     & 0.22500000000000000000D+00, 
     & 0.22500000000000000000D+00, 
     & 0.22500000000000000000D+00, 
     & 0.22500000000000000000D+00 /
      data xyz_save /
     & 1.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 1.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 1.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetrahedron_unit_o14 ( w, xyz )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_O14 returns a 14 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
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
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(ORDER), the weights.
c
c    Output, double precision XYZ(3,ORDER), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 14 )

      double precision w(order)
      double precision w_save(14)
      double precision xyz(3,order)
      double precision xyz_save(3,14)

      save w_save
      save xyz_save

      data w_save /
     & 0.073493043116361949544D+00, 
     & 0.073493043116361949544D+00, 
     & 0.073493043116361949544D+00, 
     & 0.073493043116361949544D+00, 
     & 0.11268792571801585080D+00, 
     & 0.11268792571801585080D+00, 
     & 0.11268792571801585080D+00, 
     & 0.11268792571801585080D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00 /
      data xyz_save /
     & 0.72179424906732632079D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.72179424906732632079D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.72179424906732632079D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.067342242210098170608D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.067342242210098170608D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.067342242210098170608D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.045503704125649649492D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.45449629587435035051D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.45449629587435035051D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetrahedron_unit_o14b ( w, xyz )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_O14B returns a 14 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2009
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
c    Output, double precision W(14), the weights.
c
c    Output, double precision XYZ(3,14), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 14 )

      double precision w(order)
      double precision w_save(14)
      double precision xyz(3,order)
      double precision xyz_save(3,14)

      save w_save
      save xyz_save

      data w_save /
     & 0.13283874668559071814D+00, 
     & 0.13283874668559071814D+00, 
     & 0.13283874668559071814D+00, 
     & 0.13283874668559071814D+00, 
     & 0.088589824742980710434D+00, 
     & 0.088589824742980710434D+00, 
     & 0.088589824742980710434D+00, 
     & 0.088589824742980710434D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00 /
      data xyz_save /
     & 0.056881379520423421748D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.056881379520423421748D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.056881379520423421748D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.69841970432438656092D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.69841970432438656092D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.69841970432438656092D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.50000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetrahedron_unit_o15 ( w, xyz )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_O15 returns a 15 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2009
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
c    Output, double precision W(15), the weights.
c
c    Output, double precision XYZ(3,15), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 15 )

      double precision w(order)
      double precision w_save(15)
      double precision xyz(3,order)
      double precision xyz_save(3,15)

      save w_save
      save xyz_save

      data w_save /
     & 0.071937083779018620010D+00, 
     & 0.071937083779018620010D+00, 
     & 0.071937083779018620010D+00, 
     & 0.071937083779018620010D+00, 
     & 0.069068207226272385281D+00, 
     & 0.069068207226272385281D+00, 
     & 0.069068207226272385281D+00, 
     & 0.069068207226272385281D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.11851851851851851852D+00 /
      data xyz_save /
     & 0.72408676584183090163D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.72408676584183090163D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.72408676584183090163D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.040619116511110274837D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.040619116511110274837D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.040619116511110274837D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.44364916731037084426D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.056350832689629155741D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.056350832689629155741D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetrahedron_unit_o15b ( w, xyz )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_O15B returns a 15 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
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
c    Output, double precision W(15), the weights.
c
c    Output, double precision XYZ(3,15), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 15 )

      double precision w(order)
      double precision w_save(15)
      double precision xyz(3,order)
      double precision xyz_save(3,15)

      save w_save
      save xyz_save

      data w_save /
     & 0.036160714285714285714D+00, 
     & 0.036160714285714285714D+00, 
     & 0.036160714285714285714D+00, 
     & 0.036160714285714285714D+00, 
     & 0.069871494516173816465D+00, 
     & 0.069871494516173816465D+00, 
     & 0.069871494516173816465D+00, 
     & 0.069871494516173816465D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.18170206858253505484D+00 /
      data xyz_save /
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.72727272727272727273D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.72727272727272727273D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.72727272727272727273D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.43344984642633570176D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.066550153573664298240D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.066550153573664298240D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00, 
     & 0.250000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetrahedron_unit_o24 ( w, xyz )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_O24 returns a 24 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2009
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
c    Output, double precision W(24), the weights.
c
c    Output, double precision XYZ(3,24), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 24 )

      double precision w(order)
      double precision w_save(24)
      double precision xyz(3,order)
      double precision xyz_save(3,24)

      save w_save
      save xyz_save

      data w_save /
     & 0.039922750257869636194D+00, 
     & 0.039922750257869636194D+00, 
     & 0.039922750257869636194D+00, 
     & 0.039922750257869636194D+00, 
     & 0.010077211055345822612D+00, 
     & 0.010077211055345822612D+00, 
     & 0.010077211055345822612D+00, 
     & 0.010077211055345822612D+00, 
     & 0.055357181543927398338D+00, 
     & 0.055357181543927398338D+00, 
     & 0.055357181543927398338D+00, 
     & 0.055357181543927398338D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00 /
      data xyz_save /
     & 0.35619138622025439121D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.35619138622025439121D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.35619138622025439121D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.87797812439616594065D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.87797812439616594065D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.87797812439616594065D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.032986329573173468968D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.032986329573173468968D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.032986329573173468968D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.60300566479164914137D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.26967233145831580803D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      function tetrahedron_unit_volume ( )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_VOLUME returns the volume of the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X,
c      0 <= Y,
c      0 <= Z,
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision TETRAHEDRON_UNIT_VOLUME, the volume.
c
      implicit none

      double precision tetrahedron_unit_volume

      tetrahedron_unit_volume = 1.0D+00 / 6.0D+00

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

