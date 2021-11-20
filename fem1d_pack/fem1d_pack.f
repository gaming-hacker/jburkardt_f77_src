      subroutine bandwidth_mesh ( element_order, element_num, 
     &  element_node, ml, mu, m )

c*********************************************************************72
c
cc BANDWIDTH_MESH: bandwidth of finite element mesh.
c
c  Discussion:
c
c    The quantity computed here is the "geometric" bandwidth determined
c    by the finite element mesh alone.
c
c    If a single finite element variable is associated with each node
c    of the mesh, and if the nodes and variables are numbered in the
c    same way, then the geometric bandwidth is the same as the bandwidth
c    of a typical finite element matrix.
c
c    The bandwidth M is defined in terms of the lower and upper bandwidths:
c
c      M = ML + 1 + MU
c
c    where
c
c      ML = maximum distance from any diagonal entry to a nonzero
c      entry in the same row, but earlier column,
c
c      MU = maximum distance from any diagonal entry to a nonzero
c      entry in the same row, but later column.
c
c    Because the finite element node adjacency relationship is symmetric,
c    we are guaranteed that ML = MU.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM);
c    ELEMENT_NODE(I,J) is the global index of local node I in element J.
c
c    Output, integer ML, MU, the lower and upper bandwidths of
c    the matrix.
c
c    Output, integer M, the bandwidth of the matrix.
c
      implicit none

      integer element_num
      integer element_order

      integer element
      integer element_node(element_order,element_num)
      integer global_i
      integer global_j
      integer local_i
      integer local_j
      integer m
      integer ml
      integer mu

      ml = 0
      mu = 0

      do element = 1, element_num

        do local_i = 1, element_order
          global_i = element_node(local_i,element)

          do local_j = 1, element_order
            global_j = element_node(local_j,element)

            mu = max ( mu, global_j - global_i )
            ml = max ( ml, global_i - global_j )

          end do
        end do
      end do

      m = ml + 1 + mu

      return
      end
      subroutine legendre_com ( norder, xtab, weight )

c*********************************************************************72
c
cc LEGENDRE_COM computes abscissas and weights for Gauss-Legendre quadrature.
c
c  Integration interval:
c
c    [ -1, 1 ]
c
c  Weight function:
c
c    1.
c
c  Integral to approximate:
c
c    Integral ( -1 <= X <= 1 ) F(X) dX.
c
c  Approximate integral:
c
c    sum ( 1 <= I <= NORDER ) WEIGHT(I) * F ( XTAB(I) ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2013
c
c  Author:
c
c    Original FORTRAN77 version by Philip Davis, Philip Rabinowitz
c    This FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, integer NORDER, the order of the rule.
c    NORDER must be greater than 0.
c
c    Output, double precision XTAB(NORDER), the abscissas of the rule.
c
c    Output, double precision WEIGHT(NORDER), the weights of the rule.
c    The weights are positive, symmetric, and should sum to 2.
c
      implicit none

      integer norder

      double precision d1
      double precision d2pn
      double precision d3pn
      double precision d4pn
      double precision dp
      double precision dpn
      double precision e1
      double precision fx
      double precision h
      integer i
      integer iback
      integer k
      integer m
      integer mp1mi
      integer ncopy
      integer nmove
      double precision p
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision pk
      double precision pkm1
      double precision pkp1
      double precision t
      double precision u
      double precision v
      double precision x0
      double precision xtab(norder)
      double precision xtemp
      double precision weight(norder)

      if ( norder .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LEGENDRE_COM - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal value of NORDER = ', norder
        stop
      end if

      e1 = dble ( norder * ( norder + 1 ) )

      m = ( norder + 1 ) / 2

      do i = 1, ( norder + 1 ) / 2

        mp1mi = m + 1 - i
        t = pi * dble ( 4 * i - 1 ) / dble ( 4 * norder + 2 )
        x0 = cos(t) * ( 1.0D+00 - ( 1.0D+00 - 1.0D+00 
     &    / dble ( norder ) ) / dble ( 8 * norder * norder ) )

        pkm1 = 1.0D+00
        pk = x0

        do k = 2, norder
          pkp1 = 2.0D+00 * x0 * pk - pkm1 - ( x0 * pk - pkm1 ) 
     &      / dble ( k )
          pkm1 = pk
          pk = pkp1
        end do

        d1 = real ( norder, kind = 8 ) * ( pkm1 - x0 * pk )

        dpn = d1 / ( 1.0D+00 - x0 * x0 )

        d2pn = ( 2.0D+00 * x0 * dpn - e1 * pk ) / ( 1.0D+00 - x0 * x0 )

        d3pn = ( 4.0D+00 * x0 * d2pn + ( 2.0D+00 - e1 ) * dpn ) /       
     &    ( 1.0D+00 - x0 * x0 )

        d4pn = ( 6.0D+00 * x0 * d3pn + ( 6.0D+00 - e1 ) * d2pn ) /
     &    ( 1.0D+00 - x0 * x0 )

        u = pk / dpn
        v = d2pn / dpn
c
c  Initial approximation H:
c
        h = - u * ( 1.0D+00 + 0.5D+00 * u * ( v + u * ( v * v - d3pn    
     &   / ( 3.0D+00 * dpn ) ) ) )
c
c  Refine H using one step of Newton's method:
c
        p = pk + h * ( dpn + 0.5D+00 * h * ( d2pn + h / 3.0D+00 * 
     &    ( d3pn + 0.25D+00 * h * d4pn ) ) )

        dp = dpn + h * ( d2pn + 0.5D+00 * h * ( d3pn + h * d4pn 
     &    / 3.0D+00 ) )

        h = h - p / dp

        xtemp = x0 + h

        xtab(mp1mi) = xtemp

        fx = d1 - h * e1 * ( pk + 0.5D+00 * h * ( dpn + h / 3.0D+00     
     &    * ( d2pn + 0.25D+00 * h * ( d3pn + 0.2D+00 * h * d4pn ) ) ) )

        weight(mp1mi) = 2.0D+00 * ( 1.0D+00 - xtemp * xtemp ) 
     &    / ( fx * fx )

      end do

      if ( mod ( norder, 2 ) .eq. 1 ) then
        xtab(1) = 0.0D+00
      end if
c
c  Shift the data up.
c
      nmove = int ( ( norder + 1 ) / 2 )
      ncopy = norder - nmove

      do i = 1, nmove
        iback = norder + 1 - i
        xtab(iback) = xtab(iback-ncopy)
        weight(iback) = weight(iback-ncopy)
      end do
c
c  Reflect values for the negative abscissas.
c
      do i = 1, norder - nmove
        xtab(i) = - xtab(norder+1-i)
        weight(i) = weight(norder+1-i)
      end do

      return
      end
      subroutine local_basis_1d ( order, node_x, x, phi )

c*********************************************************************72
c
cc LOCAL_BASIS_1D evaluates the basis functions in an element.
c
c  Discussion:
c
c    PHI(I)(X) = product ( J ~= I ) ( X         - NODE_X(I) )
c                                 / ( NODE_X(J) - NODE_X(I) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ORDER, the order of the element.
c    0 <= ORDER.  ORDER = 1 means piecewise linear.
c
c    Input, double precision NODE_X(ORDER), the element nodes.
c    These must be distinct.  Basis function I is 1 when X = NODE_X(I)
c    and 0 when X is equal to any other node.
c
c    Input, double precision X, the point at which the basis functions are to
c    be evaluated.
c
c    Output, double precision PHI(ORDER), the basis functions.
c
      implicit none

      integer order

      integer i
      integer j
      double precision node_x(order)
      double precision phi(order)
      double precision x

      do i = 1, order
        phi(i) = 1.0D+00
      end do

      do i = 1, order
        do j = 1, order
          if ( j .ne. i ) then
            phi(j) = ( phi(j) * ( x - node_x(i) ) ) 
     &        / ( node_x(j) - node_x(i) )
          end if
        end do
      end do

      return
      end
      subroutine local_basis_prime_1d ( order, node_x, x, dphidx )

c*********************************************************************72
c
cc LOCAL_BASIS_PRIME_1D evaluates the basis function derivatives in an element.
c
c  Discussion:
c
c    PHI(I)(X) = product ( J ~= I ) ( X - NODE_X(I) )
c                                 / ( NODE_X(J) - NODE_X(I) )
c
c    dPHIdx(I)(X) = sum ( J ~= I ) ( 1 / ( NODE_X(J) - NODE_X(I) ) *
c      product ( K ~= ( J, I ) ) ( X - NODE_X(I) ) / ( NODE_X(J) - NODE_X(I) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ORDER, the order of the element.
c    0 <= ORDER.  ORDER = 1 means piecewise linear.
c
c    Input, double precision NODE_X(ORDER), the element nodes.
c    These must be distinct.  Basis function I is 1 when X = NODE_X(I)
c    and 0 when X is equal to any other node.
c
c    Input, double precision X, the point at which the basis functions are to
c    be evaluated.
c
c    Output, double precision PHI(ORDER), the basis functions.
c
      implicit none

      integer order

      double precision dphidx(order)
      integer i
      integer j
      integer k
      double precision node_x(order)
      double precision term
      double precision x

      do i = 1, order
        dphidx(i) = 0.0D+00
      end do

      do i = 1, order
        do j = 1, order
          if ( j .ne. i ) then
            term = 1.0D+00 / ( node_x(j) - node_x(i) )
            do k = 1, order
              if ( k .ne. i .and. k .ne. j ) then
                term = term * ( x - node_x(i) ) 
     &            / ( node_x(k) - node_x(i) )
              end if
            end do
            dphidx(i) = dphidx(i) + term
          end if
        end do
      end do

      return
      end
      subroutine local_fem_1d ( order, node_x, node_v, sample_num, 
     &  sample_x, sample_v )

c*********************************************************************72
c
cc LOCAL_FEM_1D evaluates a local finite element function.
c
c  Discussion:
c
c    A local finite element function is a finite element function
c    defined over a single element.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ORDER, the order of the element.
c    0 <= ORDER.  ORDER = 1 means piecewise linear.
c
c    Input, double precision NODE_X(ORDER), the element nodes.
c    These must be distinct.  Basis function I is 1 when X = NODE_X(I) and 0
c    when X is equal to any other node.
c
c    Input, double precision NODE_V(ORDER), the value of the finite element
c    function at each node.
c
c    Input, integer SAMPLE_NUM, the number of sample points.
c
c    Input, double precision SAMPLE_X(SAMPLE_NUM), the sample points at which
c    the local finite element function is to be evaluated.
c
c    Output, double precision SAMPLE_V(SAMPLE_NUM), the values of the local
c    finite element basis functions.
c
      implicit none

      integer order
      integer sample_num

      integer i
      double precision node_v(order)
      double precision node_x(order)
      double precision phi(order)
      double precision r8vec_dot_product
      integer sample
      double precision sample_v(sample_num)
      double precision sample_x(sample_num)
      double precision x

      do i = 1, sample_num
        sample_v(i) = 0.0D+00
      end do

      do sample = 1, sample_num

        x = sample_x(sample)
        call local_basis_1d ( order, node_x, x, phi )
        sample_v(sample) = r8vec_dot_product ( order, node_v, phi )

      end do

      return
      end
      function r8_uniform_ab ( a, b, seed )

c*********************************************************************72
c
cc R8_UNIFORM_AB returns a pseudorandom R8 scaled to [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_AB, a number strictly between A and B.
c
      implicit none

      double precision a
      double precision b
      integer k
      double precision r8_uniform_ab
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_ab = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

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
