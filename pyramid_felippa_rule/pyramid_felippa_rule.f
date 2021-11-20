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
      subroutine pyramid_unit_monomial ( expon, value )

c*********************************************************************72
c
cc PYRAMID_UNIT_MONOMIAL: monomial integral in a unit pyramid.
c
c  Discussion:
c
c    This routine returns the integral of
c
c      product ( 1 <= I <= 3 ) X(I)^EXPON(I)
c
c    over the unit pyramid.
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
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
      integer i_hi
      double precision r8_choose
      double precision r8_mop
      double precision value

      value = 0.0D+00

      if ( mod ( expon(1), 2 ) .eq. 0 .and. 
     &     mod ( expon(2), 2 ) .eq. 0 ) then

        i_hi = 2 + expon(1) + expon(2)

        do i = 0, i_hi
          value = value + r8_mop ( i ) * r8_choose ( i_hi, i )
     &      / dble ( i + expon(3) + 1 )
        end do

        value = value
     &    * 2.0D+00 / dble ( expon(1) + 1 )
     &    * 2.0D+00 / dble ( expon(2) + 1 )

      end if

      return
      end
      subroutine pyramid_unit_o01 ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O01 returns a 1 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Output, double precision XYZ(3,1), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      double precision w(order)
      double precision w_save(1)
      double precision xyz(3,order)
      double precision xyz_save(3,1)

      save w_save
      save xyz_save

      data w_save /
     &  1.0D+00 /
      data xyz_save /
     &  0.0D+00, 0.0D+00, 0.25D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyramid_unit_o05 ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O05 returns a 5 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Output, double precision W(5), the weights.
c
c    Output, double precision XYZ(3,5), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 5 )

      double precision w(order)
      double precision w_save(5)
      double precision xyz(3,order)
      double precision xyz_save(3,5)

      save w_save
      save xyz_save

      data w_save /
     & 0.21093750000000000000D+00, 
     & 0.21093750000000000000D+00, 
     & 0.21093750000000000000D+00, 
     & 0.21093750000000000000D+00, 
     & 0.15625000000000000000D+00 /
      data xyz_save /
     &  -0.48686449556014765641D+00, 
     &  -0.48686449556014765641D+00, 
     &   0.16666666666666666667D+00, 
     &   0.48686449556014765641D+00, 
     &  -0.48686449556014765641D+00, 
     &   0.16666666666666666667D+00, 
     &   0.48686449556014765641D+00, 
     &   0.48686449556014765641D+00, 
     &   0.16666666666666666667D+00, 
     &  -0.48686449556014765641D+00, 
     &   0.48686449556014765641D+00, 
     &   0.16666666666666666667D+00, 
     &   0.00000000000000000000D+00, 
     &   0.00000000000000000000D+00, 
     &   0.70000000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyramid_unit_o06 ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O06 returns a 6 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Output, double precision XYZ(3,6), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 6 )

      double precision w(order)
      double precision w_save(6)
      double precision xyz(3,order)
      double precision xyz_save(3,6)

      save w_save
      save xyz_save

      data w_save /
     & 0.21000000000000000000D+00, 
     & 0.21000000000000000000D+00, 
     & 0.21000000000000000000D+00, 
     & 0.21000000000000000000D+00, 
     & 0.06000000000000000000D+00, 
     & 0.10000000000000000000D+00 /
      data xyz_save /
     &-0.48795003647426658968D+00, 
     &-0.48795003647426658968D+00, 
     & 0.16666666666666666667D+00, 
     & 0.48795003647426658968D+00, 
     &-0.48795003647426658968D+00, 
     & 0.16666666666666666667D+00, 
     & 0.48795003647426658968D+00, 
     & 0.48795003647426658968D+00, 
     & 0.16666666666666666667D+00, 
     &-0.48795003647426658968D+00, 
     & 0.48795003647426658968D+00, 
     & 0.16666666666666666667D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.58333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.75000000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyramid_unit_o08 ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O08 returns an 8 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
     & 0.075589411559869072938D+00, 
     & 0.075589411559869072938D+00, 
     & 0.075589411559869072938D+00, 
     & 0.075589411559869072938D+00, 
     & 0.17441058844013092706D+00, 
     & 0.17441058844013092706D+00, 
     & 0.17441058844013092706D+00, 
     & 0.17441058844013092706D+00 /
      data xyz_save /
     &-0.26318405556971359557D+00, 
     &-0.26318405556971359557D+00, 
     & 0.54415184401122528880D+00, 
     & 0.26318405556971359557D+00, 
     &-0.26318405556971359557D+00, 
     & 0.54415184401122528880D+00, 
     & 0.26318405556971359557D+00, 
     & 0.26318405556971359557D+00, 
     & 0.54415184401122528880D+00, 
     &-0.26318405556971359557D+00, 
     & 0.26318405556971359557D+00, 
     & 0.54415184401122528880D+00, 
     &-0.50661630334978742377D+00, 
     &-0.50661630334978742377D+00, 
     & 0.12251482265544137787D+00, 
     & 0.50661630334978742377D+00, 
     &-0.50661630334978742377D+00, 
     & 0.12251482265544137787D+00, 
     & 0.50661630334978742377D+00, 
     & 0.50661630334978742377D+00, 
     & 0.12251482265544137787D+00, 
     &-0.50661630334978742377D+00, 
     & 0.50661630334978742377D+00, 
     & 0.12251482265544137787D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyramid_unit_o08b ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O08B returns an 8 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Output, double precision W(8), the weights.
c
c    Output, double precision XYZ(3,8), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      double precision w(order)
      double precision w_save(8)
      double precision xyz(3,order)
      double precision xyz_save(3,8)

      save w_save
      save xyz_save

      data w_save /
     & 0.16438287736328777572D+00, 
     & 0.16438287736328777572D+00, 
     & 0.16438287736328777572D+00, 
     & 0.16438287736328777572D+00, 
     & 0.085617122636712224276D+00, 
     & 0.085617122636712224276D+00, 
     & 0.085617122636712224276D+00, 
     & 0.085617122636712224276D+00 /
      data xyz_save /
     &-0.51197009372656270107D+00, 
     &-0.51197009372656270107D+00, 
     & 0.11024490204163285720D+00, 
     & 0.51197009372656270107D+00, 
     &-0.51197009372656270107D+00, 
     & 0.11024490204163285720D+00, 
     & 0.51197009372656270107D+00, 
     & 0.51197009372656270107D+00, 
     & 0.11024490204163285720D+00, 
     &-0.51197009372656270107D+00, 
     & 0.51197009372656270107D+00, 
     & 0.11024490204163285720D+00, 
     &-0.28415447557052037456D+00, 
     &-0.28415447557052037456D+00, 
     & 0.518326526529795714229D+00, 
     & 0.28415447557052037456D+00, 
     &-0.28415447557052037456D+00, 
     & 0.518326526529795714229D+00, 
     & 0.28415447557052037456D+00, 
     & 0.28415447557052037456D+00, 
     & 0.518326526529795714229D+00, 
     &-0.28415447557052037456D+00, 
     & 0.28415447557052037456D+00, 
     & 0.518326526529795714229D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyramid_unit_o09 ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O09 returns a 9 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Output, double precision W(9), the weights.
c
c    Output, double precision XYZ(3,9), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 9 )

      double precision w(order)
      double precision w_save(9)
      double precision xyz(3,order)
      double precision xyz_save(3,9)

      save w_save
      save xyz_save

      data w_save /
     & 0.13073389672275944791D+00, 
     & 0.13073389672275944791D+00, 
     & 0.13073389672275944791D+00, 
     & 0.13073389672275944791D+00, 
     & 0.10989110327724055209D+00, 
     & 0.10989110327724055209D+00, 
     & 0.10989110327724055209D+00, 
     & 0.10989110327724055209D+00, 
     & 0.03750000000000000000D+00 /
      data xyz_save /
     &-0.52966422253852215131D+00, 
     &-0.52966422253852215131D+00, 
     & 0.08176876558246862335D+00, 
     & 0.52966422253852215131D+00, 
     &-0.52966422253852215131D+00, 
     & 0.08176876558246862335D+00, 
     & 0.52966422253852215131D+00, 
     & 0.52966422253852215131D+00, 
     & 0.08176876558246862335D+00, 
     &-0.52966422253852215131D+00, 
     & 0.52966422253852215131D+00, 
     & 0.08176876558246862335D+00, 
     &-0.34819753825720418039D+00, 
     &-0.34819753825720418039D+00, 
     & 0.400374091560388519511D+00, 
     & 0.34819753825720418039D+00, 
     &-0.34819753825720418039D+00, 
     & 0.400374091560388519511D+00, 
     & 0.34819753825720418039D+00, 
     & 0.34819753825720418039D+00, 
     & 0.400374091560388519511D+00, 
     &-0.34819753825720418039D+00, 
     & 0.34819753825720418039D+00, 
     & 0.400374091560388519511D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.83333333333333333333D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyramid_unit_o13 ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O13 returns a 13 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Output, double precision W(13), the weights.
c
c    Output, double precision XYZ(3,13), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 13 )

      double precision w(order)
      double precision w_save(13)
      double precision xyz(3,order)
      double precision xyz_save(3,13)

      save w_save
      save xyz_save

      data w_save /
     & 0.063061594202898550725D+00, 
     & 0.063061594202898550725D+00, 
     & 0.063061594202898550725D+00, 
     & 0.063061594202898550725D+00, 
     & 0.042101946815575556199D+00, 
     & 0.042101946815575556199D+00, 
     & 0.042101946815575556199D+00, 
     & 0.042101946815575556199D+00, 
     & 0.13172030707666776585D+00, 
     & 0.13172030707666776585D+00, 
     & 0.13172030707666776585D+00, 
     & 0.13172030707666776585D+00, 
     & 0.05246460761943250889D+00 /
      data xyz_save /
     &-0.38510399211870384331D+00, 
     &-0.38510399211870384331D+00, 
     &0.428571428571428571429D+00, 
     & 0.38510399211870384331D+00, 
     &-0.38510399211870384331D+00, 
     &0.428571428571428571429D+00, 
     & 0.38510399211870384331D+00, 
     & 0.38510399211870384331D+00, 
     &0.428571428571428571429D+00, 
     &-0.38510399211870384331D+00, 
     & 0.38510399211870384331D+00, 
     &0.428571428571428571429D+00, 
     &-0.40345831960728204766D+00, 
     & 0.00000000000000000000D+00, 
     &0.33928571428571428571D+00,
     & 0.40345831960728204766D+00, 
     & 0.00000000000000000000D+00, 
     &0.33928571428571428571D+00,
     & 0.00000000000000000000D+00, 
     &-0.40345831960728204766D+00, 
     &0.33928571428571428571D+00,
     & 0.00000000000000000000D+00, 
     & 0.40345831960728204766D+00, 
     &0.33928571428571428571D+00,
     &-0.53157877436961973359D+00, 
     &-0.53157877436961973359D+00, 
     &0.08496732026143790850D+00,
     & 0.53157877436961973359D+00, 
     &-0.53157877436961973359D+00, 
     &0.08496732026143790850D+00,
     & 0.53157877436961973359D+00, 
     & 0.53157877436961973359D+00, 
     &0.08496732026143790850D+00,
     &-0.53157877436961973359D+00, 
     & 0.53157877436961973359D+00, 
     &0.08496732026143790850D+00,
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.76219701803768503595D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyramid_unit_o18 ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O18 returns an 18 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Output, double precision W(18), the weights.
c
c    Output, double precision XYZ(3,18), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 18 )

      double precision w(order)
      double precision w_save(18)
      double precision xyz(3,order)
      double precision xyz_save(3,18)

      save w_save
      save xyz_save

      data w_save /
     & 0.023330065296255886709D+00, 
     & 0.037328104474009418735D+00, 
     & 0.023330065296255886709D+00, 
     & 0.037328104474009418735D+00, 
     & 0.059724967158415069975D+00, 
     & 0.037328104474009418735D+00, 
     & 0.023330065296255886709D+00, 
     & 0.037328104474009418735D+00, 
     & 0.023330065296255886709D+00, 
     & 0.05383042853090460712D+00, 
     & 0.08612868564944737139D+00, 
     & 0.05383042853090460712D+00, 
     & 0.08612868564944737139D+00, 
     & 0.13780589703911579422D+00, 
     & 0.08612868564944737139D+00, 
     & 0.05383042853090460712D+00, 
     & 0.08612868564944737139D+00, 
     & 0.05383042853090460712D+00 /
      data xyz_save /
     &-0.35309846330877704481D+00, 
     &-0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     & 0.00000000000000000000D+00, 
     &-0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     & 0.35309846330877704481D+00, 
     &-0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     &-0.35309846330877704481D+00, 
     & 0.00000000000000000000D+00, 
     &0.544151844011225288800D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.544151844011225288800D+00, 
     & 0.35309846330877704481D+00, 
     & 0.00000000000000000000D+00, 
     &0.544151844011225288800D+00, 
     &-0.35309846330877704481D+00, 
     & 0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     & 0.00000000000000000000D+00, 
     & 0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     & 0.35309846330877704481D+00, 
     & 0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     &-0.67969709567986745790D+00, 
     &-0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     & 0.00000000000000000000D+00, 
     &-0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     & 0.67969709567986745790D+00, 
     &-0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     &-0.67969709567986745790D+00, 
     & 0.00000000000000000000D+00, 
     &0.12251482265544137787D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.12251482265544137787D+00, 
     & 0.67969709567986745790D+00, 
     & 0.00000000000000000000D+00, 
     &0.12251482265544137787D+00, 
     &-0.67969709567986745790D+00, 
     & 0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     & 0.00000000000000000000D+00, 
     & 0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     & 0.67969709567986745790D+00, 
     & 0.67969709567986745790D+00, 
     &0.12251482265544137787D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyramid_unit_o27 ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O27 returns a 27 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Output, double precision W(27), the weights.
c
c    Output, double precision XYZ(3,27), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 27 )

      double precision w(order)
      double precision w_save(27)
      double precision xyz(3,order)
      double precision xyz_save(3,27)

      save w_save
      save xyz_save

      data w_save /
     & 0.036374157653908938268D+00, 
     & 0.05819865224625430123D+00, 
     & 0.036374157653908938268D+00, 
     & 0.05819865224625430123D+00, 
     & 0.09311784359400688197D+00, 
     & 0.05819865224625430123D+00, 
     & 0.036374157653908938268D+00, 
     & 0.05819865224625430123D+00, 
     & 0.036374157653908938268D+00, 
     & 0.033853303069413431019D+00, 
     & 0.054165284911061489631D+00, 
     & 0.033853303069413431019D+00, 
     & 0.054165284911061489631D+00, 
     & 0.08666445585769838341D+00, 
     & 0.054165284911061489631D+00, 
     & 0.033853303069413431019D+00, 
     & 0.054165284911061489631D+00, 
     & 0.033853303069413431019D+00, 
     & 0.006933033103838124540D+00, 
     & 0.011092852966140999264D+00, 
     & 0.006933033103838124540D+00, 
     & 0.011092852966140999264D+00, 
     & 0.017748564745825598822D+00, 
     & 0.011092852966140999264D+00, 
     & 0.006933033103838124540D+00, 
     & 0.011092852966140999264D+00, 
     & 0.006933033103838124540D+00 /
      data xyz_save /
     &-0.7180557413198889387D+00, 
     & -0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     & 0.00000000000000000000D+00, 
     &-0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     & 0.7180557413198889387D+00, 
     & -0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     &-0.7180557413198889387D+00, 
     & 0.00000000000000000000D+00, 
     &0.07299402407314973216D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.07299402407314973216D+00, 
     & 0.7180557413198889387D+00, 
     & 0.00000000000000000000D+00, 
     &0.07299402407314973216D+00, 
     &-0.7180557413198889387D+00, 
     & 0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     & 0.00000000000000000000D+00, 
     & 0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     & 0.7180557413198889387D+00, 
     & 0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     &-0.50580870785392503961D+00, 
     &-0.50580870785392503961D+00, 
     & 0.34700376603835188472D+00, 
     & 0.00000000000000000000D+00, 
     &-0.50580870785392503961D+00, 
     & 0.34700376603835188472D+00, 
     & 0.50580870785392503961D+00, 
     &-0.50580870785392503961D+00, 
     &0.34700376603835188472D+00, 
     &-0.50580870785392503961D+00, 
     & 0.00000000000000000000D+00, 
     &0.34700376603835188472D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.34700376603835188472D+00, 
     & 0.50580870785392503961D+00, 
     & 0.00000000000000000000D+00, 
     &0.34700376603835188472D+00, 
     &-0.50580870785392503961D+00, 
     & 0.50580870785392503961D+00, 
     &0.34700376603835188472D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50580870785392503961D+00, 
     &0.34700376603835188472D+00, 
     & 0.50580870785392503961D+00, 
     & 0.50580870785392503961D+00, 
     &0.34700376603835188472D+00, 
     &-0.22850430565396735360D+00, 
     &-0.22850430565396735360D+00, 
     &0.70500220988849838312D+00, 
     & 0.00000000000000000000D+00, 
     &-0.22850430565396735360D+00, 
     &0.70500220988849838312D+00, 
     & 0.22850430565396735360D+00, 
     &-0.22850430565396735360D+00, 
     & 0.70500220988849838312D+00, 
     &-0.22850430565396735360D+00, 
     & 0.00000000000000000000D+00, 
     & 0.70500220988849838312D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.70500220988849838312D+00, 
     & 0.22850430565396735360D+00, 
     & 0.00000000000000000000D+00, 
     & 0.70500220988849838312D+00, 
     &-0.22850430565396735360D+00, 
     & 0.22850430565396735360D+00, 
     & 0.70500220988849838312D+00, 
     & 0.00000000000000000000D+00, 
     & 0.22850430565396735360D+00, 
     & 0.70500220988849838312D+00, 
     & 0.22850430565396735360D+00, 
     & 0.22850430565396735360D+00, 
     & 0.70500220988849838312D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyramid_unit_o48 ( w, xyz )

c*********************************************************************72
c
cc PYRAMID_UNIT_O48 returns a 48 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Arthur Stroud,
c    Approximate Calculation of Multiple Integrals,
c    Prentice Hall, 1971,
c    ISBN: 0130438936,
c    LC: QA311.S85.
c
c  Parameters:
c
c    Output, double precision W(48), the weights.
c
c    Output, double precision XYZ(3,48), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 48 )

      double precision w(order)
      double precision w_save(48)
      double precision xyz(3,order)
      double precision xyz_save(3,48)

      save w_save
      save xyz_save

      data w_save /
     &2.01241939442682455D-002, 
     &2.01241939442682455D-002, 
     &2.01241939442682455D-002, 
     &2.01241939442682455D-002, 
     &2.60351137043010779D-002, 
     &2.60351137043010779D-002, 
     &2.60351137043010779D-002, 
     &2.60351137043010779D-002, 
     &1.24557795239745531D-002, 
     &1.24557795239745531D-002, 
     &1.24557795239745531D-002, 
     &1.24557795239745531D-002, 
     &1.87873998794808156D-003, 
     &1.87873998794808156D-003, 
     &1.87873998794808156D-003, 
     &1.87873998794808156D-003, 
     &4.32957927807745280D-002, 
     &4.32957927807745280D-002, 
     &4.32957927807745280D-002, 
     &4.32957927807745280D-002, 
     &1.97463249834127288D-002, 
     &1.97463249834127288D-002, 
     &1.97463249834127288D-002, 
     &1.97463249834127288D-002, 
     &5.60127223523590526D-002, 
     &5.60127223523590526D-002, 
     &5.60127223523590526D-002, 
     &5.60127223523590526D-002, 
     &2.55462562927473852D-002, 
     &2.55462562927473852D-002, 
     &2.55462562927473852D-002, 
     &2.55462562927473852D-002, 
     &2.67977366291788643D-002, 
     &2.67977366291788643D-002, 
     &2.67977366291788643D-002, 
     &2.67977366291788643D-002, 
     &1.22218992265373354D-002, 
     &1.22218992265373354D-002, 
     &1.22218992265373354D-002, 
     &1.22218992265373354D-002, 
     &4.04197740453215038D-003, 
     &4.04197740453215038D-003, 
     &4.04197740453215038D-003, 
     &4.04197740453215038D-003, 
     &1.84346316995826843D-003, 
     &1.84346316995826843D-003, 
     &1.84346316995826843D-003, 
     &1.84346316995826843D-003 /
      data xyz_save /
     &0.88091731624450909D+00, 
     & 0.0000000000000000D+00, 
     & 4.85005494469969989D-02, 
     & -0.88091731624450909D+00, 
     & 0.0000000000000000D+00, 
     & 4.85005494469969989D-02, 
     & 0.0000000000000000D+00, 
     & 0.88091731624450909D+00, 
     &4.85005494469969989D-02, 
     & 0.0000000000000000D+00, 
     &-0.88091731624450909D+00, 
     &4.85005494469969989D-02, 
     &0.70491874112648223D+00, 
     & 0.0000000000000000D+00, 
     & 0.23860073755186201D+00, 
     & -0.70491874112648223D+00, 
     & 0.0000000000000000D+00, 
     & 0.23860073755186201D+00, 
     & 0.0000000000000000D+00, 
     & 0.70491874112648223D+00, 
     &0.23860073755186201D+00, 
     & 0.0000000000000000D+00, 
     &-0.70491874112648223D+00, 
     &0.23860073755186201D+00, 
     &0.44712732143189760D+00, 
     & 0.0000000000000000D+00, 
     & 0.51704729510436798D+00, 
     & -0.44712732143189760D+00, 
     & 0.0000000000000000D+00, 
     & 0.51704729510436798D+00, 
     & 0.0000000000000000D+00, 
     & 0.44712732143189760D+00, 
     &0.51704729510436798D+00, 
     & 0.0000000000000000D+00, 
     &-0.44712732143189760D+00, 
     &0.51704729510436798D+00, 
     &0.18900486065123448D+00, 
     & 0.0000000000000000D+00, 
     & 0.79585141789677305D+00, 
     & -0.18900486065123448D+00, 
     & 0.0000000000000000D+00, 
     & 0.79585141789677305D+00, 
     & 0.0000000000000000D+00, 
     & 0.18900486065123448D+00, 
     &0.79585141789677305D+00, 
     & 0.0000000000000000D+00, 
     &-0.18900486065123448D+00, 
     &0.79585141789677305D+00, 
     &0.36209733410322176D+00, 
     & 0.36209733410322176D+00, 
     &4.85005494469969989D-02, 
     & -0.36209733410322176D+00, 
     & 0.36209733410322176D+00, 
     &4.85005494469969989D-02, 
     & -0.36209733410322176D+00, 
     &-0.36209733410322176D+00, 
     &4.85005494469969989D-02, 
     &0.36209733410322176D+00, 
     &-0.36209733410322176D+00, 
     &4.85005494469969989D-02, 
     &0.76688932060387538D+00, 
     & 0.76688932060387538D+00, 
     &4.85005494469969989D-02, 
     & -0.76688932060387538D+00, 
     & 0.76688932060387538D+00, 
     &4.85005494469969989D-02, 
     & -0.76688932060387538D+00, 
     &-0.76688932060387538D+00, 
     &4.85005494469969989D-02, 
     &0.76688932060387538D+00, 
     &-0.76688932060387538D+00, 
     &4.85005494469969989D-02, 
     &0.28975386476618070D+00, 
     & 0.28975386476618070D+00, 
     &0.23860073755186201D+00, 
     & -0.28975386476618070D+00, 
     & 0.28975386476618070D+00, 
     &0.23860073755186201D+00, 
     & -0.28975386476618070D+00, 
     &-0.28975386476618070D+00, 
     &0.23860073755186201D+00, 
     &0.28975386476618070D+00, 
     &-0.28975386476618070D+00, 
     &0.23860073755186201D+00, 
     &0.61367241226233160D+00, 
     & 0.61367241226233160D+00, 
     &0.23860073755186201D+00, 
     & -0.61367241226233160D+00, 
     & 0.61367241226233160D+00, 
     &0.23860073755186201D+00, 
     & -0.61367241226233160D+00, 
     &-0.61367241226233160D+00, 
     &0.23860073755186201D+00, 
     &0.61367241226233160D+00, 
     &-0.61367241226233160D+00, 
     &0.23860073755186201D+00, 
     &0.18378979287798017D+00, 
     & 0.18378979287798017D+00, 
     &0.51704729510436798D+00, 
     & -0.18378979287798017D+00, 
     & 0.18378979287798017D+00, 
     &0.51704729510436798D+00, 
     & -0.18378979287798017D+00, 
     &-0.18378979287798017D+00, 
     &0.51704729510436798D+00, 
     &0.18378979287798017D+00, 
     &-0.18378979287798017D+00, 
     &0.51704729510436798D+00, 
     &0.38925011625173161D+00, 
     & 0.38925011625173161D+00, 
     &0.51704729510436798D+00, 
     & -0.38925011625173161D+00, 
     & 0.38925011625173161D+00, 
     &0.51704729510436798D+00, 
     & -0.38925011625173161D+00, 
     &-0.38925011625173161D+00, 
     &0.51704729510436798D+00, 
     &0.38925011625173161D+00, 
     &-0.38925011625173161D+00, 
     &0.51704729510436798D+00, 
     &7.76896479525748113D-02, 
     & 7.76896479525748113D-02, 
     &0.79585141789677305D+00, 
     & -7.76896479525748113D-02, 
     & 7.76896479525748113D-02, 
     &0.79585141789677305D+00, 
     & -7.76896479525748113D-02, 
     &-7.76896479525748113D-02, 
     &0.79585141789677305D+00, 
     &7.76896479525748113D-02, 
     &-7.76896479525748113D-02, 
     &0.79585141789677305D+00, 
     &0.16453962988669860D+00, 
     & 0.16453962988669860D+00, 
     &0.79585141789677305D+00, 
     & -0.16453962988669860D+00, 
     & 0.16453962988669860D+00, 
     &0.79585141789677305D+00, 
     & -0.16453962988669860D+00, 
     &-0.16453962988669860D+00, 
     &0.79585141789677305D+00, 
     &0.16453962988669860D+00, 
     &-0.16453962988669860D+00, 
     &0.79585141789677305D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      function pyramid_unit_volume ( )

c*********************************************************************72
c
cc PYRAMID_UNIT_VOLUME: volume of a unit pyramid with square base.
c
c  Discussion:
c
c    The volume of this unit pyramid is 4/3.
c
c    The integration region is:
c
c      - ( 1 - Z ) <= X <= 1 - Z
c      - ( 1 - Z ) <= Y <= 1 - Z
c                0 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision PYRAMID_UNIT_VOLUME, the volume.
c
      implicit none

      double precision pyramid_unit_volume

      pyramid_unit_volume = 4.0D+00 / 3.0D+00

      return
      end
      function r8_choose ( n, k )

c*********************************************************************72
c
cc R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in R8 arithmetic.
c
c    The formula used is:
c
c      C(N,K) = N! / ( K! * (N-K)! )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 June 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    ML Wolfson, HV Wright,
c    Algorithm 160:
c    Combinatorial of M Things Taken N at a Time,
c    Communications of the ACM,
c    Volume 6, Number 4, April 1963, page 161.
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, double precision R8_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer k
      integer mn
      integer mx
      integer n
      double precision r8_choose
      double precision value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0.0D+00

      else if ( mn .eq. 0 ) then

        value = 1.0D+00

      else

        mx = max ( k, n - k )
        value = dble ( mx + 1 )

        do i = 2, mn
          value = ( value * dble ( mx + i ) ) / dble ( i )
        end do

      end if

      r8_choose = value

      return
      end
      function r8_mop ( i )

c*********************************************************************72
c
cc R8_MOP returns the I-th power of -1 as an R8.
c
c  Discussion:
c
c    An R8 is a double precision real value.
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
c    Input, integer I, the power of -1.
c
c    Output, double precision R8_MOP, the I-th power of -1.
c
      implicit none

      integer i
      double precision r8_mop
      double precision value

      if ( mod ( i, 2 ) .eq. 0 ) then
        value = + 1.0D+00
      else
        value = - 1.0D+00
      end if

      r8_mop = value

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

