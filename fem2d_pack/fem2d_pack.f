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
c    02 June 2009
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
      subroutine bandwidth_var ( element_order, element_num, 
     &  element_node, node_num, var_node, var_num, var, ml, mu, m )

c*********************************************************************72
c
cc BANDWIDTH_VAR determines the bandwidth for finite element variables.
c
c  Discussion:
c
c    We assume that, attached to each node in the finite element mesh
c    there are a (possibly zero) number of finite element variables.
c    We wish to determine the bandwidth necessary to store the stiffness
c    matrix associated with these variables.
c
c    An entry K(I,J) of the stiffness matrix must be zero unless the
c    variables I and J correspond to nodes N(I) and N(J) which are
c    common to some element.
c
c    In order to determine the bandwidth of the stiffness matrix, we
c    essentially seek a nonzero entry K(I,J) for which abs ( I - J )
c    is maximized.
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
c    We assume the finite element variable adjacency relationship is 
c    symmetric, so we are guaranteed that ML = MU.
c
c    Note that the user is free to number the variables in any way
c    whatsoever, and to associate variables to nodes in any way,
c    so that some nodes have no variables, some have one, and some
c    have several.  
c
c    The storage of the indices of the variables is fairly simple.
c    In VAR, simply list all the variables associated with node 1, 
c    then all those associated with node 2, and so on.  Then set up
c    the pointer array VAR_NODE so that we can jump to the section of
c    VAR where the list begins for any particular node.
c
c    The routine does not check that each variable is only associated
c    with a single node.  This would normally be the case in a finite
c    element setting.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 June 2009
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
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer VAR_NODE(NODE_NUM+1), used to find the 
c    variables associated with a given node, which are in VAR in locations 
c    VAR_NODE(NODE) to VAR_NODE(NODE+1)-1.  Note that the last entry of
c    this array points to the location just after the last location in VAR.
c
c    Input, integer VAR_NUM, the number of variables.
c
c    Input, integer VAR(VAR_NUM), the indexes of the variables, 
c    which are presumably (but not necessarily) 1, 2, 3, ..., VAR_NUM.
c
c    Output, integer ML, MU, the lower and upper bandwidths of the 
c    matrix.
c
c    Output, integer M, the bandwidth of the matrix.
c
      implicit none

      integer element_num
      integer element_order
      integer node_num
      integer var_num

      integer element
      integer element_node(element_order,element_num)
      integer m
      integer ml
      integer mu
      integer node_global_i
      integer node_global_j
      integer node_local_i
      integer node_local_j
      integer var(var_num)
      integer var_global_i
      integer var_global_j
      integer var_local_i
      integer var_local_j
      integer var_node(node_num+1)

      ml = 0
      mu = 0

      do element = 1, element_num

        do node_local_i = 1, element_order
          node_global_i = element_node(node_local_i,element)

          do var_local_i = var_node(node_global_i), 
     &      var_node(node_global_i+1)-1
            var_global_i = var(var_local_i)

            do node_local_j = 1, element_order
              node_global_j = element_node(node_local_j,element)

              do var_local_j = var_node(node_global_j), 
     &          var_node(node_global_j+1)-1
                var_global_j = var(var_local_j)

                mu = max ( mu, var_global_j - var_global_i )
                ml = max ( ml, var_global_i - var_global_j )

              end do
            end do
          end do
        end do
      end do

      m = ml + 1 + mu

      return
      end
      subroutine basis_11_q4 ( q, i, p, phi, dphidx, dphidy )

c*********************************************************************72
c
cc BASIS_11_Q4: one basis at one point for a Q4 element.
c
c  Discussion:
c
c    Note that this formulation of the Q4 element assumes that the element 
c    sides are parallel to coordinate directions.
c
c    The routine is given the coordinates of the vertices of a quadrilateral.
c    It works directly with these coordinates, and does not refer to a 
c    reference element.
c
c    The routine evaluates the basis functions, and their X and Y derivatives.
c
c  Physical Element Q4:
c
c    |
c    |  4-----3
c    |  |     |
c    |  |     |
c    Y  |     |
c    |  |     |
c    |  |     |
c    |  1-----2
c    |
c    +-----X------>
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision Q(2,4), the coordinates of the vertices.
c    It is common to list these points in counter clockwise order.
c
c    Input, integer I, the index of the basis function.
c
c    Input, double precision P(2), the evaluation point.
c
c    Output, double precision PHI, the basis function 
c    at the evaluation points.
c
c    Output, double precision DPHIDX, DPHIDY, the basis
c    derivatives at the evaluation points.
c
c  Local Parameter:
c
c    Local, double precision AREA, the area of the rectangle.
c
      implicit none

      double precision area
      double precision dphidx
      double precision dphidy
      integer i
      double precision p(2)
      double precision phi
      double precision q(2,4)

      area = ( q(1,3) - q(1,1) ) * ( q(2,3) - q(2,1) )

      if ( i .eq. 1 ) then
        phi    =   ( q(1,3) - p(1) ) * ( q(2,3) - p(2) ) / area
        dphidx = -                     ( q(2,3) - p(2) ) / area
        dphidy = - ( q(1,3) - p(1) )                     / area
      else if ( i .eq. 2 ) then
        phi    =   ( p(1) - q(1,1) ) * ( q(2,3) - p(2) ) / area
        dphidx =                       ( q(2,3) - p(2) ) / area
        dphidy = - ( p(1) - q(1,1) )                     / area
      else if ( i .eq. 3 ) then
        phi    =   ( p(1) - q(1,1) ) * ( p(2) - q(2,1) ) / area
        dphidx =                       ( p(2) - q(2,1) ) / area
        dphidy =   ( p(1) - q(1,1) )                     / area
      else if ( i .eq. 4 ) then
        phi    =   ( q(1,3) - p(1) ) * ( p(2) - q(2,1) ) / area
        dphidx = -                     ( p(2) - q(2,1) ) / area
        dphidy =   ( q(1,3) - p(1) )                     / area
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BASIS_11_Q4 - Fatal error!'
        write ( *, '(a)' ) '  Illegal basis function index.'
        stop
      end if

      return
      end
      subroutine basis_11_q4_test ( )

c*********************************************************************72
c
cc BASIS_11_Q4_TEST verifies BASIS_11_Q4.
c
c  Discussion:
c
c    Note that this formulation of the Q4 element assumes that the element 
c    sides are parallel to coordinate directions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
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

      integer node_num
      parameter ( node_num = 4 )

      double precision dphidx(node_num,node_num)
      double precision dphidy(node_num,node_num)
      integer i
      integer j
      double precision phi(node_num,node_num)
      double precision q(2,node_num)
      double precision sum_x
      double precision sum_y

      save q

      data q /
     &  2.0D+00, 1.0D+00, 
     &  3.0D+00, 1.0D+00, 
     &  3.0D+00, 4.0D+00, 
     &  2.0D+00, 4.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BASIS_11_Q4_TEST:'
      write ( *, '(a)' ) '  Verify basis functions for element Q4.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Physical Nodes:'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, q(1:2,j)
      end do
     
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The basis function values at basis nodes'
      write ( *, '(a)' ) '  should form the identity matrix.'
      write ( *, '(a)' ) ' '

      do i = 1, node_num
        do j = 1, node_num
          call basis_11_q4 ( q, i, q(1:2,j), phi(i,j), dphidx(i,j), 
     &      dphidy(i,j) )
        end do
      end do

      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) phi(i,1:node_num)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The X and Y derivatives should sum to 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      dPhidX sum      dPhidY sum'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        sum_x = 0.0D+00
        sum_y = 0.0D+00
        do i = 1, node_num
          sum_x = sum_x + dphidx(i,j)
          sum_y = sum_y + dphidy(i,j)
        end do
        write ( *, '(2x,f14.8,2x,f14.8)' ) sum_x, sum_y
      end do

      return
      end
      subroutine basis_11_t3 ( t, i, p, qi, dqidx, dqidy )

c*********************************************************************72
c
cc BASIS_11_T3: one basis at one point for the T3 element.
c
c  Discussion:
c
c    The routine is given the coordinates of the nodes of a triangle. 
c        
c           3
c          / \
c         /   \
c        /     \
c       1-------2
c
c    It evaluates the linear basis function Q(I)(X,Y) associated with
c    node I, which has the property that it is a linear function
c    which is 1 at node I and zero at the other two nodes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the coordinates of the nodes.
c
c    Input, integer I, the index of the desired basis function.
c    I should be between 1 and 3.
c
c    Input, double precision P(2), the coordinates of a point at which the
c    basis function is to be evaluated.
c
c    Output, double precision QI, DQIDX, DQIDY, the values of the basis
c    function and its X and Y derivatives.
c
      implicit none

      double precision area
      double precision dqidx
      double precision dqidy
      integer i
      integer i4_wrap
      integer ip1
      integer ip2
      double precision p(2)
      double precision qi
      double precision t(2,3)

      area = abs ( t(1,1) * ( t(2,2) - t(2,3) ) 
     &           + t(1,2) * ( t(2,3) - t(2,1) ) 
     &           + t(1,3) * ( t(2,1) - t(2,2) ) )

      if ( area .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BASIS_11_T3 - Fatal error!'
        write ( *, '(a)' ) '  Element has zero area.'
        stop
      end if

      if ( i .lt. 1 .or. 3 .lt. i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BASIS_11_T3 - Fatal error!'
        write ( *, '(a)' ) '  Basis index I is not between 1 and 3.'
        write ( *, '(a,i8)' ) '  I = ', i
        stop
      end if

      ip1 = i4_wrap ( i + 1, 1, 3 )
      ip2 = i4_wrap ( i + 2, 1, 3 )

      qi = ( ( t(1,ip2) - t(1,ip1) ) * ( p(2) - t(2,ip1) ) 
     &     - ( t(2,ip2) - t(2,ip1) ) * ( p(1) - t(1,ip1) ) ) / area

      dqidx = - ( t(2,ip2) - t(2,ip1) ) / area
      dqidy =   ( t(1,ip2) - t(1,ip1) ) / area

      return
      end
      subroutine basis_11_t3_test ( )

c*********************************************************************72
c
cc BASIS_11_T3_TEST verifies BASIS_11_T3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 June 2009
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

      integer node_num
      parameter ( node_num = 3 )

      double precision dphidx(node_num,node_num)
      double precision dphidy(node_num,node_num)
      integer i
      integer j
      double precision phi(node_num,node_num)
      double precision sum_x
      double precision sum_y
      double precision t(2,node_num)

      save t

      data t /
     &  2.0D+00, 0.0D+00, 
     &  4.0D+00, 3.0D+00, 
     &  0.0D+00, 4.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BASIS_11_T3_TEST:'
      write ( *, '(a)' ) '  Verify basis functions for element T3.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Physical Nodes:'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, t(1,j), t(2,j)
      end do
     
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The basis function values at basis nodes'
      write ( *, '(a)' ) '  should form the identity matrix.'
      write ( *, '(a)' ) ' '

      do i = 1, node_num
        do j = 1, node_num
          call basis_11_t3 ( t, i, t(1,j), phi(i,j), dphidx(i,j), 
     &      dphidy(i,j) )
        end do
      end do

      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) ( phi(i,j), j = 1, node_num )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The X and Y derivatives should sum to 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      dPhidX sum    dPhidY sum'
      write ( *, '(a)' ) ' '

      do j = 1, node_num

        sum_x = 0.0D+00
        sum_y = 0.0D+00
        do i = 1, node_num
          sum_x = sum_x + dphidx(i,j)
          sum_y = sum_y + dphidy(i,j)
        end do
        write ( *, '(2x,f14.8,f14.8)' ) sum_x, sum_y

      end do

      return
      end
      subroutine basis_11_t4 ( t, i, p, phi, dphidx, dphidy )

c*********************************************************************72
c
cc BASIS_11_T4: one basis at one point for a T4 element.
c
c  Discussion:
c
c    The T4 element is the cubic bubble triangle.
c
c    The routine is given the coordinates of the vertices of a triangle.
c    It works directly with these coordinates, and does not refer to a 
c    reference element.
c
c    The sides of the triangle DO NOT have to lie along a coordinate
c    axis.
c
c    The routine evaluates the basis functions associated with each vertex,
c    and their derivatives with respect to X and Y.
c
c  Physical Element T4: 
c        
c            3
c           / \
c          /   \
c         /  4  \
c        /       \
c       1---------2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,4), the coordinates of the vertices
c    of the triangle, and the coordinates of the centroid.  
c    It is common to list the first three points in counter clockwise
c    order.
c
c    Input, integer I, the index of the basis function.
c
c    Input, double precision P(2), the points where the basis function
c    is to be evaluated.
c
c    Output, double precision PHI, the value of the basis function
c    at the evaluation point.
c
c    Output, double precision DPHIDX, DPHIDY, the value of the 
c    derivatives at the evaluation point.
c
c  Local parameters:
c
c    Local, double precision AREA, is (twice) the area of the triangle.
c
      implicit none

      double precision area
      double precision dphidx
      double precision dphidy
      double precision dpsidx(4)
      double precision dpsidy(4)
      integer i
      integer j
      double precision p(2)
      double precision phi
      double precision psi(4)
      double precision t(2,4)

      area = t(1,1) * ( t(2,2) - t(2,3) ) 
     &     + t(1,2) * ( t(2,3) - t(2,1) ) 
     &     + t(1,3) * ( t(2,1) - t(2,2) )

      psi(1) =     (   ( t(1,3) - t(1,2) ) * ( p(2) - t(2,2) )     
     &               - ( t(2,3) - t(2,2) ) * ( p(1) - t(1,2) ) ) 
      dpsidx(1) =    - ( t(2,3) - t(2,2) )
      dpsidy(1) =      ( t(1,3) - t(1,2) )

      psi(2) =     (   ( t(1,1) - t(1,3) ) * ( p(2) - t(2,3) )     
     &               - ( t(2,1) - t(2,3) ) * ( p(1) - t(1,3) ) )
      dpsidx(2) =    - ( t(2,1) - t(2,3) )
      dpsidy(2) =      ( t(1,1) - t(1,3) )

      psi(3) =     (   ( t(1,2) - t(1,1) ) * ( p(2) - t(2,1) )     
     &               - ( t(2,2) - t(2,1) ) * ( p(1) - t(1,1) ) )
      dpsidx(3) =    - ( t(2,2) - t(2,1) )
      dpsidy(3) =      ( t(1,2) - t(1,1) )
c
c  Normalize the first three functions.
c
      psi(1:3)    =    psi(1:3) / area
      dpsidx(1:3) = dpsidx(1:3) / area
      dpsidy(1:3) = dpsidy(1:3) / area
c
c  Compute the cubic bubble function.
c
      psi(4) = 27.0D+00 * psi(1) * psi(2) * psi(3)

      dpsidx(4) = 27.0D+00 * ( 
     &              dpsidx(1) *    psi(2) *    psi(3) 
     &              +  psi(1) * dpsidx(2) *    psi(3) 
     &              +  psi(1) *    psi(2) * dpsidx(3) )

      dpsidy(4) = 27.0D+00 * ( 
     &              dpsidy(1) *    psi(2) *    psi(3) 
     &              +  psi(1) * dpsidy(2) *    psi(3) 
     &              +  psi(1) *    psi(2) * dpsidy(3) )
c
c  Subtract 1/3 of the cubic bubble function from each of the three linears.
c
      do j = 1, 3
        psi(j)    =    psi(j) -    psi(4) / 3.0D+00
        dpsidx(j) = dpsidx(j) - dpsidx(4) / 3.0D+00
        dpsidy(j) = dpsidy(j) - dpsidy(4) / 3.0D+00
      end do

      phi    = psi(i)
      dphidx = dpsidx(i)
      dphidy = dpsidy(i)

      return
      end
      subroutine basis_11_t4_test ( )

c*********************************************************************72
c
cc BASIS_11_T4_TEST verifies BASIS_11_T4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 June 2009
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

      integer node_num
      parameter ( node_num = 4 )

      double precision dphidx(node_num,node_num)
      double precision dphidy(node_num,node_num)
      integer i
      integer j
      double precision phi(node_num,node_num)
      double precision sum_x
      double precision sum_y
      double precision t(2,node_num)

      save t

      data t /
     &  2.0D+00, 0.0D+00, 
     &  4.0D+00, 3.0D+00, 
     &  0.0D+00, 4.0D+00, 
     &  0.0D+00, 0.0D+00 /
c
c  The node associated with the fourth basis function is the centroid.
c
      t(1,4) = ( t(1,1) + t(1,2) + t(1,3) ) / 3.0D+00
      t(2,4) = ( t(2,1) + t(2,2) + t(2,3) ) / 3.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BASIS_11_T4_TEST:'
      write ( *, '(a)' ) '  Verify basis functions for element T4.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Physical Nodes:'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, t(1,j), t(2,j)
      end do
     
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The basis function values at basis nodes'
      write ( *, '(a)' ) '  should form the identity matrix.'
      write ( *, '(a)' ) ' '

      do i = 1, node_num
        do j = 1, node_num
          call basis_11_t4 ( t, i, t(1,j), phi(i,j), dphidx(i,j), 
     &      dphidy(i,j) )
        end do
      end do

      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) ( phi(i,j), j = 1, node_num )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The X and Y derivatives should sum to 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      dPhidX sum    dPhidY sum'
      write ( *, '(a)' ) ' '

      do j = 1, node_num

        sum_x = 0.0D+00
        sum_y = 0.0D+00
        do i = 1, node_num
          sum_x = sum_x + dphidx(i,j)
          sum_y = sum_y + dphidy(i,j)
        end do
        write ( *, '(2x,f14.8,f14.8)' ) sum_x, sum_y

      end do

      return
      end
      subroutine basis_11_t6 ( t, i, p, bi, dbidx, dbidy )

c*********************************************************************72
c
cc BASIS_11_T6: one basis at one point for the T6 element.
c
c  Discussion:
c
c    The routine is given the coordinates of the nodes of a triangle. 
c        
c           3
c          / \
c         6   5
c        /     \
c       1---4---2
c
c    It evaluates the quadratic basis function B(I)(X,Y) associated with
c    node I, which has the property that it is a quadratic function
c    which is 1 at node I and zero at the other five nodes.
c
c    This routine assumes that the sides of the triangle are straight,
c    so that the midside nodes fall on the line between two vertices.
c
c    This routine relies on the fact that each basis function can be
c    written as the product of two linear factors, which are easily
c    computed and normalized.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,6), the coordinates of the nodes.
c
c    Input, integer I, the index of the desired basis function.
c    I should be between 1 and 6.
c
c    Input, double precision P(2), the coordinates of a point at which the
c    basis function is to be evaluated.
c
c    Output, double precision BI, DBIDX, DBIDY, the values of the basis
c    function and its X and Y derivatives.
c
      implicit none

      double precision bi
      double precision dbidx
      double precision dbidy
      double precision gf
      double precision gn
      double precision hf
      double precision hn
      integer i
      integer i4_wrap
      integer j1
      integer j2
      integer k1
      integer k2
      double precision p(2)
      double precision t(2,6)

      if ( i .lt. 1 .or. 6 .lt. i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BASIS_11_T6 - Fatal error!'
        write ( *, '(a)' ) '  Basis index I is not between 1 and 6.'
        write ( *, '(a,i8)' ) '  I = ', i
        stop
      end if
c
c  Determine the pairs of nodes.
c
      if ( i .le. 3 ) then
        j1 = i4_wrap ( i + 1, 1, 3 )
        j2 = i4_wrap ( i + 2, 1, 3 )
        k1 = i + 3
        k2 = i4_wrap ( i + 5, 4, 6 )
      else
        j1 = i - 3
        j2 = i4_wrap ( i - 3 + 2, 1, 3 )
        k1 = i4_wrap ( i - 3 + 1, 1, 3 )
        k2 = i4_wrap ( i - 3 + 2, 1, 3 )
      end if
c
c  Evaluate the two linear factors GF and HF, 
c  and their normalizers GN and HN.
c
      gf = ( p(1)    - t(1,j1) ) * ( t(2,j2) - t(2,j1) ) 
     &   - ( t(1,j2) - t(1,j1) ) * ( p(2)    - t(2,j1) ) 

      gn = ( t(1,i)  - t(1,j1) ) * ( t(2,j2) - t(2,j1) ) 
     &   - ( t(1,j2) - t(1,j1) ) * ( t(2,i)  - t(2,j1) )   

      hf = ( p(1)    - t(1,k1) ) * ( t(2,k2) - t(2,k1) ) 
     &   - ( t(1,k2) - t(1,k1) ) * ( p(2)    - t(2,k1) ) 

      hn = ( t(1,i)  - t(1,k1) ) * ( t(2,k2) - t(2,k1) ) 
     &   - ( t(1,k2) - t(1,k1) ) * ( t(2,i)  - t(2,k1) ) 
c
c  Construct the basis function and its derivatives.
c
      bi =        ( gf                  / gn ) 
     &          * ( hf                  / hn )

      dbidx =   ( ( t(2,j2) - t(2,j1) ) / gn ) 
     &          * ( hf                  / hn ) 
     &          + ( gf                  / gn ) 
     &        * ( ( t(2,k2) - t(2,k1) ) / hn )

      dbidy = - ( ( t(1,j2) - t(1,j1) ) / gn ) 
     &        * (   hf                  / hn ) 
     &        - (   gf                  / gn ) 
     &        * ( ( t(1,k2) - t(1,k1) ) / hn )

      return
      end
      subroutine basis_11_t6_test ( )

c*********************************************************************72
c
cc BASIS_11_T6_TEST verifies BASIS_11_T6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2009
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

      integer node_num
      parameter ( node_num = 6 )

      double precision dphidx(node_num,node_num)
      double precision dphidy(node_num,node_num)
      integer i
      integer j
      double precision phi(node_num,node_num)
      double precision sum_x
      double precision sum_y
      double precision t(2,node_num)

      save t

      data t /
     &  2.0D+00, 0.0D+00, 
     &  4.0D+00, 3.0D+00, 
     &  0.0D+00, 4.0D+00, 
     &  3.0D+00, 1.5D+00, 
     &  2.0D+00, 3.5D+00, 
     &  1.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BASIS_11_T6_TEST:'
      write ( *, '(a)' ) '  Verify basis functions for element T6.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Physical Nodes:'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, t(1:2,j)
      end do
     
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The basis function values at basis nodes'
      write ( *, '(a)' ) '  should form the identity matrix.'
      write ( *, '(a)' ) ' '

      do i = 1, node_num
        do j = 1, node_num
          call basis_11_t6 ( t, i, t(1,j), phi(i,j), dphidx(i,j), 
     &      dphidy(i,j) )
        end do
      end do

      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) ( phi(i,j), j = 1, node_num)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The X and Y derivatives should sum to 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      dPhidX sum      dPhidY sum'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        sum_x = 0.0D+00
        sum_y = 0.0D+00
        do i = 1, node_num
          sum_x = sum_x + dphidx(i,j)
          sum_y = sum_y + dphidy(i,j)
        end do
        write ( *, '(2x,f14.8,2x,f14.8)' ) sum_x, sum_y
      end do

      return
      end
      subroutine basis_mn_q4 ( q, n, p, phi, dphidx, dphidy )

c*********************************************************************72
c
cc BASIS_MN_Q4: all bases at N points for a Q4 element.
c
c  Discussion:
c
c    The routine is given the coordinates of the vertices of a quadrilateral.
c    It works directly with these coordinates, and does not refer to a 
c    reference element.
c
c    The sides of the element are presumed to lie along coordinate axes.
c
c    The routine evaluates the basis functions, and their X and Y derivatives.
c
c  Physical Element Q4:
c
c    |
c    |  4-----3
c    |  |     |
c    |  |     |
c    Y  |     |
c    |  |     |
c    |  |     |
c    |  1-----2
c    |
c    +-----X------>
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision Q(2,4), the coordinates of the vertices.
c    It is common to list these points in counter clockwise order.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision P(2,N), the evaluation points.
c
c    Output, double precision PHI(4,N), the basis functions 
c    at the evaluation points.
c
c    Output, double precision DPHIDX(4,N), DPHIDY(4,N), the basis
c    derivatives at the evaluation points.
c
c  Local Parameter:
c
c    Local, double precision AREA, the area of the rectangle.
c
      implicit none

      integer n

      double precision area
      double precision dphidx(4,n)
      double precision dphidy(4,n)
      integer i
      integer j
      double precision p(2,n)
      double precision phi(4,n)
      double precision q(2,4)

      area =            ( q(1,3)             - q(1,1) ) 
     &                * ( q(2,3)             - q(2,1) )

      do j = 1, n
        phi(1,j) =      ( q(1,3) - p(1,j)           ) 
     &                * ( q(2,3) - p(2,j)           )
        phi(2,j) =      (          p(1,j)  - q(1,1) ) 
     &                * ( q(2,3) - p(2,j)           )
        phi(3,j) =      (          p(1,j)  - q(1,1) ) 
     &                * (          p(2,j)  - q(2,1) )
        phi(4,j) =      ( q(1,3) - p(1,j)           ) 
     &                * (          p(2,j)  - q(2,1) )
    
        dphidx(1,j) = - ( q(2,3) - p(2,j)           )
        dphidx(2,j) =   ( q(2,3) - p(2,j)           )
        dphidx(3,j) =   (          p(2,j)  - q(2,1) )
        dphidx(4,j) = - (          p(2,j)  - q(2,1) )
 
        dphidy(1,j) = - ( q(1,3) - p(1,j)           )
        dphidy(2,j) = - (          p(1,j)  - q(1,1) )
        dphidy(3,j) =   (          p(1,j)  - q(1,1) )
        dphidy(4,j) =   ( q(1,3) - p(1,j)           )
      end do
c
c  Normalize.
c
      do j = 1, n
        do i = 1, 4
          phi(i,j)    = phi(i,j)    / area
          dphidx(i,j) = dphidx(i,j) / area
          dphidy(i,j) = dphidy(i,j) / area
        end do
      end do

      return
      end
      subroutine basis_mn_q4_test ( )

c*********************************************************************72
c
cc BASIS_MN_Q4_TEST verifies BASIS_MN_Q4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
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

      integer node_num
      parameter ( node_num = 4 )

      double precision dphidx(node_num,node_num)
      double precision dphidy(node_num,node_num)
      integer i
      integer j
      double precision phi(node_num,node_num)
      double precision q(2,node_num)
      double precision sum_x
      double precision sum_y

      save q

      data q /
     &  3.0D+00, 1.0D+00, 
     &  5.0D+00, 1.0D+00, 
     &  5.0D+00, 4.0D+00, 
     &  3.0D+00, 4.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  BASIS_MN_Q4_TEST: '
      write ( *, '(a)' ) '    Verify basis functions for element Q4.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Physical Nodes:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       I        X        Y'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, q(1:2,j)
      end do
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The basis function values at basis nodes'
      write ( *, '(a)' ) '  should form the identity matrix.'
      write ( *, '(a)' ) ' '

      call basis_mn_q4 ( q, node_num, q, phi, dphidx, dphidy )

      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) phi(i,1:node_num)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The X and Y derivatives should sum to 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      dPhidX sum      dPhidY sum'
      write ( *, '(a)' ) ' '

      do j = 1, node_num

        sum_x = 0.0D+00
        sum_y = 0.0D+00
        do i = 1, node_num
          sum_x = sum_x + dphidx(i,j)
          sum_y = sum_y + dphidy(i,j)
        end do
        write ( *, '(2x,f14.8,2x,f14.8)' ) sum_x, sum_y

      end do

      return
      end
      subroutine basis_mn_t3 ( t, n, p, phi, dphidx, dphidy )

c*********************************************************************72
c
cc BASIS_MN_T3: all bases at N points for a T3 element.
c
c  Discussion:
c
c    The routine is given the coordinates of the vertices of a triangle.
c    It works directly with these coordinates, and does not refer to a
c    reference element.
c
c    The sides of the triangle DO NOT have to lie along a coordinate
c    axis.
c
c    The routine evaluates the basis functions associated with each vertex,
c    and their derivatives with respect to X and Y.
c
c  Physical Element T3:
c
c            3
c           / \
c          /   \
c         /     \
c        /       \
c       1---------2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the coordinates of the vertices
c    of the triangle.  It is common to list these points in counter clockwise
c    order.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision P(2,N), the points where the basis functions
c    are to be evaluated.
c
c    Output, double precision PHI(3,N), the value of the basis functions
c    at the evaluation points.
c
c    Output, double precision DPHIDX(3,N), DPHIDY(3,N), the value of the
c    derivatives at the evaluation points.
c
c  Local parameters:
c
c    Local, double precision AREA, is (twice) the area of the triangle.
c
      implicit none

      integer n

      double precision area
      double precision dphidx(3,n)
      double precision dphidy(3,n)
      integer i
      integer j
      double precision p(2,n)
      double precision phi(3,n)
      double precision t(2,3)

      area = t(1,1) * ( t(2,2) - t(2,3) ) 
     &     + t(1,2) * ( t(2,3) - t(2,1) ) 
     &     + t(1,3) * ( t(2,1) - t(2,2) )

      if ( area .eq. 0.0D+00 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BASIS_MN_T3 - Fatal error!'
        write ( *, '(a)' ) '  Element has zero area.'
        stop

      end if

      do j = 1, n

        phi(1,j) =     ( ( t(1,3) - t(1,2) ) * ( p(2,j) - t(2,2) )     
     &                 - ( t(2,3) - t(2,2) ) * ( p(1,j) - t(1,2) ) )
        dphidx(1,j) =  - ( t(2,3) - t(2,2) )
        dphidy(1,j) =    ( t(1,3) - t(1,2) )

        phi(2,j) =     ( ( t(1,1) - t(1,3) ) * ( p(2,j) - t(2,3) )     
     &                 - ( t(2,1) - t(2,3) ) * ( p(1,j) - t(1,3) ) )
        dphidx(2,j) =  - ( t(2,1) - t(2,3) )
        dphidy(2,j) =    ( t(1,1) - t(1,3) )

        phi(3,j) =     ( ( t(1,2) - t(1,1) ) * ( p(2,j) - t(2,1) )     
     &                 - ( t(2,2) - t(2,1) ) * ( p(1,j) - t(1,1) ) )
        dphidx(3,j) =  - ( t(2,2) - t(2,1) )
        dphidy(3,j) =    ( t(1,2) - t(1,1) )

      end do
c
c  Normalize.
c
      do j = 1, n
        do i = 1, 3
          phi(i,j) = phi(i,j) / area
          dphidx(i,j) = dphidx(i,j) / area
          dphidy(i,j) = dphidy(i,j) / area
        end do
      end do

      return
      end
      subroutine basis_mn_t3_test ( )

c*********************************************************************72
c
cc BASIS_MN_T3_TEST verifies BASIS_MN_T3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
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

      integer node_num
      parameter ( node_num = 3 )

      double precision dphidx(node_num,node_num)
      double precision dphidy(node_num,node_num)
      integer i
      integer j
      double precision phi(node_num,node_num)
      double precision sum_x
      double precision sum_y
      double precision t(2,node_num)

      save t

      data t / 
     &  2.0D+00, 0.0D+00, 
     &  4.0D+00, 3.0D+00, 
     &  0.0D+00, 4.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BASIS_MN_T3_TEST:'
      write ( *, '(a)' ) '  Verify basis functions for element T3.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Physical Nodes:'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, t(1:2,j)
      end do
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The basis function values at basis nodes'
      write ( *, '(a)' ) '  should form the identity matrix.'
      write ( *, '(a)' ) ' '
      call basis_mn_t3 ( t, node_num, t, phi, dphidx, dphidy )

      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) phi(i,1:node_num)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The X and Y derivatives should sum to 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      dPhidX sum    dPhidY sum'
      write ( *, '(a)' ) ' '

      do j = 1, node_num

        sum_x = 0.0D+00
        sum_y = 0.0D+00
        do i = 1, node_num
          sum_x = sum_x + dphidx(i,j)
          sum_y = sum_y + dphidy(i,j)
        end do
        write ( *, '(2x,f14.8,2x,f14.8)' ) sum_x, sum_y

      end do

      return
      end
      subroutine basis_mn_t4 ( t, n, p, phi, dphidx, dphidy )

c*********************************************************************72
c
cc BASIS_MN_T4: all bases at N points for a T4 element.
c
c  Discussion:
c
c    The T4 element is the cubic bubble triangle.
c
c    The routine is given the coordinates of the vertices of a triangle.
c    It works directly with these coordinates, and does not refer to a 
c    reference element.
c
c    The sides of the triangle DO NOT have to lie along a coordinate
c    axis.
c
c    The routine evaluates the basis functions associated with each vertex,
c    and their derivatives with respect to X and Y.
c
c  Physical Element T4: 
c        
c            3
c           / \
c          /   \
c         /  4  \
c        /       \
c       1---------2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,4), the coordinates of the vertices
c    of the triangle, and the coordinates of the centroid.  
c    It is common to list the first three points in counter clockwise
c    order.
c
c    Input, integer ( kind = 4 ) N, the number of evaluation points.
c
c    Input, double precision P(2,N), the points where the basis functions 
c    are to be evaluated.
c
c    Output, double precision PHI(4,N), the value of the basis functions 
c    at the evaluation points.
c
c    Output, double precision DPHIDX(4,N), DPHIDY(4,N), the value of the 
c    derivatives at the evaluation points.
c
c  Local parameters:
c
c    Local, double precision AREA, is (twice) the area of the triangle.
c
      implicit none

      integer n

      double precision area
      double precision dphidx(4,n)
      double precision dphidy(4,n)
      integer i
      integer j
      double precision p(2,n)
      double precision phi(4,n)
      double precision t(2,4)

      area = t(1,1) * ( t(2,2) - t(2,3) ) 
     &     + t(1,2) * ( t(2,3) - t(2,1) ) 
     &     + t(1,3) * ( t(2,1) - t(2,2) )

      do j = 1, n

        phi(1,j) =     (   ( t(1,3) - t(1,2) ) * ( p(2,j) - t(2,2) )     
     &                   - ( t(2,3) - t(2,2) ) * ( p(1,j) - t(1,2) ) )
        dphidx(1,j) =    - ( t(2,3) - t(2,2) )
        dphidy(1,j) =      ( t(1,3) - t(1,2) )

        phi(2,j) =     (   ( t(1,1) - t(1,3) ) * ( p(2,j) - t(2,3) )     
     &                   - ( t(2,1) - t(2,3) ) * ( p(1,j) - t(1,3) ) )
        dphidx(2,j) =    - ( t(2,1) - t(2,3) )
        dphidy(2,j) =      ( t(1,1) - t(1,3) )

        phi(3,j) =     (   ( t(1,2) - t(1,1) ) * ( p(2,j) - t(2,1) )     
     &                   - ( t(2,2) - t(2,1) ) * ( p(1,j) - t(1,1) ) )
        dphidx(3,j) =    - ( t(2,2) - t(2,1) )
        dphidy(3,j) =      ( t(1,2) - t(1,1) )

      end do
c
c  Normalize the first three functions.
c
      do j = 1, n
        do i = 1, 3
          phi(i,j)    =    phi(i,j) / area
          dphidx(i,j) = dphidx(i,j) / area
          dphidy(i,j) = dphidy(i,j) / area
        end do
      end do
c
c  Compute the cubic bubble function.
c
      do j = 1, n

        phi(4,j) = 27.0D+00 * phi(1,j) * phi(2,j) * phi(3,j)

        dphidx(4,j) = 27.0D+00 * ( 
     &                dphidx(1,j) *    phi(2,j) *    phi(3,j) 
     &                +  phi(1,j) * dphidx(2,j) *    phi(3,j) 
     &                +  phi(1,j) *    phi(2,j) * dphidx(3,j) )

        dphidy(4,j) = 27.0D+00 * ( 
     &                dphidy(1,j) *    phi(2,j) *    phi(3,j) 
     &                +  phi(1,j) * dphidy(2,j) *    phi(3,j) 
     &                +  phi(1,j) *    phi(2,j) * dphidy(3,j) )

      end do
c
c  Subtract 1/3 of the cubic bubble function from each of the three linears.
c
      do j = 1, n
        do i = 1, 3
          phi(i,j)    =    phi(i,j) -    phi(4,j) / 3.0D+00
          dphidx(i,j) = dphidx(i,j) - dphidx(4,j) / 3.0D+00
          dphidy(i,j) = dphidy(i,j) - dphidy(4,j) / 3.0D+00
        end do
      end do

      return
      end
      subroutine basis_mn_t4_test ( )

c*********************************************************************72
c
cc BASIS_MN_T4_TEST verifies BASIS_MN_T4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
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

      integer node_num
      parameter ( node_num = 4 )

      double precision dphidx(node_num,node_num)
      double precision dphidy(node_num,node_num)
      integer i
      integer j
      double precision phi(node_num,node_num)
      double precision sum_x
      double precision sum_y
      double precision t(2,node_num)

      save t

      data t / 
     &  2.0D+00, 0.0D+00, 
     &  4.0D+00, 2.0D+00, 
     &  0.0D+00, 4.0D+00,
     &  2.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BASIS_MN_T4_TEST:'
      write ( *, '(a)' ) '  Verify basis functions for element T4.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Physical Nodes:'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, t(1:2,j)
      end do
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The basis function values at basis nodes'
      write ( *, '(a)' ) '  should form the identity matrix.'
      write ( *, '(a)' ) ' '
      call basis_mn_t4 ( t, node_num, t, phi, dphidx, dphidy )

      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) phi(i,1:node_num)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The X and Y derivatives should sum to 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      dPhidX sum    dPhidY sum'
      write ( *, '(a)' ) ' '

      do j = 1, node_num

        sum_x = 0.0D+00
        sum_y = 0.0D+00
        do i = 1, node_num
          sum_x = sum_x + dphidx(i,j)
          sum_y = sum_y + dphidy(i,j)
        end do
        write ( *, '(2x,f14.8,2x,f14.8)' ) sum_x, sum_y

      end do

      return
      end
      subroutine basis_mn_t6 ( t, n, p, phi, dphidx, dphidy )

c*********************************************************************72
c
cc BASIS_MN_T6: all bases at N points for a T6 element.
c
c  Discussion:
c
c    The routine is given the coordinates of the vertices and midside
c    nodes of a triangle.  It works directly with these coordinates, and does
c    not refer to a reference element.
c
c    This routine requires that the midside nodes be "in line"
c    with the vertices, that is, that the sides of the triangle be
c    straight.  However, the midside nodes do not actually have to
c    be halfway along the side of the triangle.
c
c  Physical element T6:
c
c    This picture indicates the assumed ordering of the six nodes
c    of the triangle.
c
c             3
c            / \
c           /   \
c          6     5
c         /       \
c        /         \
c       1-----4-----2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,6), the nodal oordinates of the element.
c    It is common to list these points in counter clockwise order.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision P(2,N), the coordinates of the point where
c    the basis functions are to be evaluated.
c
c    Output, double precision PHI(6,N), the basis functions at the
c    evaluation points.
c
c    Output, double precision DPHIDX(6,N), DPHIDY(6,N), the derivatives
c    of the basis functions at the evaluation points.
c
c  Local Parameters:
c
c    Local, double precision AREA, is (twice) the area of the triangle.
c
      implicit none

      integer n

      double precision dphidx(6,n)
      double precision dphidy(6,n)
      double precision gn(n)
      double precision gx(n)
      double precision hn(n)
      double precision hx(n)
      integer j
      double precision p(2,n)
      double precision phi(6,n)
      double precision t(2,6)
c
c  Basis function 1: PHI(X,Y) = G(3,2) * H(6,4) / normalization.
c
      do j = 1, n

        gx(j) = ( p(1,j) - t(1,2) ) * ( t(2,3) - t(2,2) ) 
     &        - ( t(1,3) - t(1,2) ) * ( p(2,j) - t(2,2) )

        gn(j) = ( t(1,1) - t(1,2) ) * ( t(2,3) - t(2,2) ) 
     &        - ( t(1,3) - t(1,2) ) * ( t(2,1) - t(2,2) )

        hx(j) = ( p(1,j) - t(1,4) ) * ( t(2,6) - t(2,4) ) 
     &        - ( t(1,6) - t(1,4) ) * ( p(2,j) - t(2,4) )

        hn(j) = ( t(1,1) - t(1,4) ) * ( t(2,6) - t(2,4) ) 
     &        - ( t(1,6) - t(1,4) ) * ( t(2,1) - t(2,4) )

        phi(1,j) =     ( gx(j) * hx(j) ) / ( gn(j) * hn(j) )
        dphidx(1,j) =  (      ( t(2,3) - t(2,2) ) * hx(j) 
     &              + gx(j) * ( t(2,6) - t(2,4) ) ) / ( gn(j) * hn(j) )
        dphidy(1,j) = -(      ( t(1,3) - t(1,2) ) * hx(j) 
     &              + gx(j) * ( t(1,6) - t(1,4) ) ) / ( gn(j) * hn(j) )
c
c  Basis function 2: PHI(X,Y) = G(3,1) * H(4,5) / normalization.
c
        gx(j) = ( p(1,j) - t(1,1) ) * ( t(2,3) - t(2,1) ) 
     &        - ( t(1,3) - t(1,1) ) * ( p(2,j) - t(2,1) )

        gn(j) = ( t(1,2) - t(1,1) ) * ( t(2,3) - t(2,1) ) 
     &        - ( t(1,3) - t(1,1) ) * ( t(2,2) - t(2,1) )

        hx(j) = ( p(1,j) - t(1,5) ) * ( t(2,4) - t(2,5) ) 
     &        - ( t(1,4) - t(1,5) ) * ( p(2,j) - t(2,5) )

        hn(j) = ( t(1,2) - t(1,5) ) * ( t(2,4) - t(2,5) ) 
     &        - ( t(1,4) - t(1,5) ) * ( t(2,2) - t(2,5) )

        phi(2,j) = ( gx(j) * hx(j) ) / ( gn(j) * hn(j) )
        dphidx(2,j) =  (      ( t(2,3) - t(2,1) ) * hx(j) 
     &              + gx(j) * ( t(2,4) - t(2,5) ) ) / ( gn(j) * hn(j) )
        dphidy(2,j) = -(      ( t(1,3) - t(1,1) ) * hx(j) 
     &              + gx(j) * ( t(1,4) - t(1,5) ) ) / ( gn(j) * hn(j) )
c
c  Basis function 3: PHI(X,Y) = G(1,2) * H(5,6) / normalization.
c
        gx(j) = ( p(1,j) - t(1,2) ) * ( t(2,1) - t(2,2) ) 
     &        - ( t(1,1) - t(1,2) ) * ( p(2,j) - t(2,2) )

        gn(j) = ( t(1,3) - t(1,2) ) * ( t(2,1) - t(2,2) ) 
     &        - ( t(1,1) - t(1,2) ) * ( t(2,3) - t(2,2) )

        hx(j) = ( p(1,j) - t(1,6) ) * ( t(2,5) - t(2,6) ) 
     &        - ( t(1,5) - t(1,6) ) * ( p(2,j) - t(2,6) )

        hn(j) = ( t(1,3) - t(1,6) ) * ( t(2,5) - t(2,6) ) 
     &        - ( t(1,5) - t(1,6) ) * ( t(2,3) - t(2,6) )

        phi(3,j) = ( gx(j) * hx(j) ) / ( gn(j) * hn(j) )
        dphidx(3,j) =  (      ( t(2,1) - t(2,2) ) * hx(j)
     &              + gx(j) * ( t(2,5) - t(2,6) ) ) / ( gn(j) * hn(j) )
        dphidy(3,j) = -(      ( t(1,1) - t(1,2) ) * hx(j)
     &              + gx(j) * ( t(1,5) - t(1,6) ) ) / ( gn(j) * hn(j) )
c
c  Basis function 4: PHI(X,Y) = G(1,3) * H(2,3) / normalization.
c
        gx(j) = ( p(1,j) - t(1,3) ) * ( t(2,1) - t(2,3) )
     &        - ( t(1,1) - t(1,3) ) * ( p(2,j) - t(2,3) )

        gn(j) = ( t(1,4) - t(1,3) ) * ( t(2,1) - t(2,3) )
     &        - ( t(1,1) - t(1,3) ) * ( t(2,4) - t(2,3) )

        hx(j) = ( p(1,j) - t(1,3) ) * ( t(2,2) - t(2,3) )
     &        - ( t(1,2) - t(1,3) ) * ( p(2,j) - t(2,3) )

        hn(j) = ( t(1,4) - t(1,3) ) * ( t(2,2) - t(2,3) )
     &        - ( t(1,2) - t(1,3) ) * ( t(2,4) - t(2,3) )

        phi(4,j) = ( gx(j) * hx(j) ) / ( gn(j) * hn(j) )
        dphidx(4,j) =  (      ( t(2,1) - t(2,3) ) * hx(j)
     &             + gx(j) * ( t(2,2) - t(2,3) ) ) / ( gn(j) * hn(j) )
        dphidy(4,j) = -(      ( t(1,1) - t(1,3) ) * hx(j)
     &             + gx(j) * ( t(1,2) - t(1,3) ) ) / ( gn(j) * hn(j) )
c
c  Basis function 5: PHI(X,Y) = G(2,1) * H(3,1) / normalization.
c
        gx(j) = ( p(1,j) - t(1,1) ) * ( t(2,2) - t(2,1) )
     &        - ( t(1,2) - t(1,1) ) * ( p(2,j) - t(2,1) )

        gn(j) = ( t(1,5) - t(1,1) ) * ( t(2,2) - t(2,1) )
     &        - ( t(1,2) - t(1,1) ) * ( t(2,5) - t(2,1) )

        hx(j) = ( p(1,j) - t(1,1) ) * ( t(2,3) - t(2,1) )
     &        - ( t(1,3) - t(1,1) ) * ( p(2,j) - t(2,1) )

        hn(j) = ( t(1,5) - t(1,1) ) * ( t(2,3) - t(2,1) )
     &        - ( t(1,3) - t(1,1) ) * ( t(2,5) - t(2,1) )

        phi(5,j) = ( gx(j) * hx(j) ) / ( gn(j) * hn(j) )
        dphidx(5,j) =  (      ( t(2,2) - t(2,1) ) * hx(j)
     &             + gx(j) * ( t(2,3) - t(2,1) ) ) / ( gn(j) * hn(j) )
        dphidy(5,j) = -(      ( t(1,2) - t(1,1) ) * hx(j)
     &             + gx(j) * ( t(1,3) - t(1,1) ) ) / ( gn(j) * hn(j) )
c
c  Basis function 6: PHI(X,Y) = G(1,2) * H(3,2) / normalization.
c
        gx(j) = ( p(1,j) - t(1,2) ) * ( t(2,1) - t(2,2) )
     &        - ( t(1,1) - t(1,2) ) * ( p(2,j) - t(2,2) )

        gn(j) = ( t(1,6) - t(1,2) ) * ( t(2,1) - t(2,2) )
     &        - ( t(1,1) - t(1,2) ) * ( t(2,6) - t(2,2) )

        hx(j) = ( p(1,j) - t(1,2) ) * ( t(2,3) - t(2,2) )
     &        - ( t(1,3) - t(1,2) ) * ( p(2,j) - t(2,2) )

        hn(j) = ( t(1,6) - t(1,2) ) * ( t(2,3) - t(2,2) )
     &        - ( t(1,3) - t(1,2) ) * ( t(2,6) - t(2,2) )

        phi(6,j) = ( gx(j) * hx(j) ) / ( gn(j) * hn(j) )
        dphidx(6,j) =  (      ( t(2,1) - t(2,2) ) * hx(j)
     &             + gx(j) * ( t(2,3) - t(2,2) ) ) / ( gn(j) * hn(j) )
        dphidy(6,j) = -(      ( t(1,1) - t(1,2) ) * hx(j)
     &             + gx(j) * ( t(1,3) - t(1,2) ) ) / ( gn(j) * hn(j) )

      end do

      return
      end
      subroutine basis_mn_t6_test ( )

c*********************************************************************72
c
cc BASIS_MN_T6_TEST verifies BASIS_MN_T6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
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

      integer node_num
      parameter ( node_num = 6 )

      double precision dphidx(node_num,node_num)
      double precision dphidy(node_num,node_num)
      integer i
      integer j
      double precision phi(node_num,node_num)
      double precision sum_x
      double precision sum_y
      double precision t(2,node_num)

      save t

      data t / 
     &  2.0D+00, 0.0D+00, 
     &  4.0D+00, 3.0D+00, 
     &  0.0D+00, 4.0D+00,
     &  3.0D+00, 1.5D+00,
     &  2.0D+00, 3.5D+00,
     &  1.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BASIS_MN_T6_TEST:'
      write ( *, '(a)' ) '  Verify basis functions for element T6.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Physical Nodes:'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, t(1:2,j)
      end do
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The basis function values at basis nodes'
      write ( *, '(a)' ) '  should form the identity matrix.'
      write ( *, '(a)' ) ' '
      call basis_mn_t6 ( t, node_num, t, phi, dphidx, dphidy )

      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) phi(i,1:node_num)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The X and Y derivatives should sum to 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      dPhidX sum    dPhidY sum'
      write ( *, '(a)' ) ' '

      do j = 1, node_num

        sum_x = 0.0D+00
        sum_y = 0.0D+00
        do i = 1, node_num
          sum_x = sum_x + dphidx(i,j)
          sum_y = sum_y + dphidy(i,j)
        end do
        write ( *, '(2x,f14.8,2x,f14.8)' ) sum_x, sum_y

      end do

      return
      end
      subroutine ch_cap ( ch )

c*********************************************************************72
c
cc CH_CAP capitalizes a single character.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character CH, the character to capitalize.
c
      implicit none

      character ch
      integer itemp

      itemp = ichar ( ch )
 
      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        ch = char ( itemp - 32 )
      end if
 
      return
      end
      function degrees_to_radians ( degrees )

c*********************************************************************72
c
cc DEGREES_TO_RADIANS converts an angle measure from degrees to radians.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    01 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision DEGREES, the angle measure in degrees.
c
c    Output, double precision DEGREES_TO_RADIANS, the angle measure in radians.
c
      implicit none

      double precision degrees
      double precision degrees_to_radians
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      degrees_to_radians = ( degrees / 180.0D+00 ) * pi

      return
      end
      subroutine derivative_average_t3 ( node_num, node_xy, element_num,
     &  element_node, c, dcdx, dcdy )

c*********************************************************************72
c
cc DERIVATIVE_AVERAGE_T3 averages derivatives at the nodes of a T3 mesh.
c
c  Discussion:
c
c    This routine can be used to compute an averaged nodal value of any
c    quantity associated with the finite element function.  At a node 
c    that is shared by several elements, the fundamental function
c    U will be continuous, but its spatial derivatives, for instance,
c    will generally be discontinuous.  This routine computes the
c    value of the spatial derivatives in each element, and averages
c    them, to make a reasonable assignment of a nodal value.
c
c    In this version of the routine, the average is not weighted.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the coordinates of 
c    the nodes.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(3,ELEMENT_NUM), 
c    the element->node data.
c
c    Input, double precision C(NODE_NUM), the finite element coefficient
c    vector.
c
c    Output, double precision DCDX(NODE_NUM), DCDY(NODE_NUM), the averaged
c    values of dCdX and dCdY at the nodes.
c
      implicit none

      integer element_num
      integer element_order
      parameter ( element_order = 3 )
      integer node_num

      double precision c(node_num)
      double precision dcdx(node_num)
      double precision dcdy(node_num)
      double precision dphidx(element_order,element_order)
      double precision dphidy(element_order,element_order)
      integer element
      integer element_node(element_order,element_num)
      integer i
      integer j
      integer node_count(node_num)
      integer node_global1
      integer node_global2
      integer node_local1
      integer node_local2
      double precision node_xy(2,node_num)
      double precision phi(element_order,element_order)
      double precision t(2,element_order)

      do i = 1, node_num
        node_count(i) = 0
        dcdx(i) = 0.0D+00
        dcdy(i) = 0.0D+00
      end do
c
c  Consider every element.
c
      do element = 1, element_num
c
c  Get the coordinates of the nodes of the element.
c
        do j = 1, element_order
          do i = 1, 2
            t(i,j) = node_xy(i,element_node(j,element))
          end do
        end do
c
c  Evaluate the X and Y derivatives of the basis functions at the nodes.
c
        call basis_mn_t3 ( t, element_order, t, phi, dphidx, dphidy )
c
c  Evaluate dCdX and dCdY at each node in the element, and add
c  them to the running totals.
c
        do node_local1 = 1, element_order

          node_global1 = element_node(node_local1,element)

          do node_local2 = 1, element_order

            node_global2 = element_node(node_local2,element)

            dcdx(node_global1) = dcdx(node_global1) 
     &        + c(node_global2) * dphidx(node_local2,node_local1)

            dcdy(node_global1) = dcdy(node_global1) 
     &        + c(node_global2) * dphidy(node_local2,node_local1)

          end do

          node_count(node_global1) = node_count(node_global1) + 1

        end do

      end do
c
c  Average the running totals.
c
      do i = 1, node_num
        dcdx(i) = dcdx(i) / dble ( node_count(i) )
        dcdy(i) = dcdy(i) / dble ( node_count(i) )
      end do

      return
      end
      subroutine derivative_average_t6 ( node_num, node_xy, element_num, 
     &  element_node, c, dcdx, dcdy )

c*********************************************************************72
c
cc DERIVATIVE_AVERAGE_T6 averages derivatives at the nodes of a T6 mesh.
c
c  Discussion:
c
c    This routine can be used to compute an averaged nodal value of any
c    quantity associated with the finite element function.  At a node 
c    that is shared by several elements, the fundamental function
c    U will be continuous, but its spatial derivatives, for instance,
c    will generally be discontinuous.  This routine computes the
c    value of the spatial derivatives in each element, and averages
c    them, to make a reasonable assignment of a nodal value.
c
c    In this version of the routine, the average is not weighted.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the coordinates of 
c    the nodes.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(6,ELEMENT_NUM), 
c    the element->node data.
c
c    Input, double precision C(NODE_NUM), the finite element coefficient
c    vector.
c
c    Output, double precision DCDX(NODE_NUM), DCDY(NODE_NUM), the averaged
c    values of dCdX and dCdY at the nodes.
c
      implicit none

      integer element_num
      integer element_order
      parameter ( element_order = 6 )
      integer node_num

      double precision c(node_num)
      double precision dcdx(node_num)
      double precision dcdy(node_num)
      double precision dphidx(element_order,element_order)
      double precision dphidy(element_order,element_order)
      integer element
      integer element_node(element_order,element_num)
      integer i
      integer j
      integer node_count(node_num)
      integer node_global1
      integer node_global2
      integer node_local1
      integer node_local2
      double precision node_xy(2,node_num)
      double precision phi(element_order,element_order)
      double precision t(2,element_order)

      do j = 1, node_num
        node_count(j) = 0
        dcdx(j) = 0.0D+00
        dcdy(j) = 0.0D+00
      end do
c
c  Consider every element.
c
      do element = 1, element_num
c
c  Get the coordinates of the nodes of the element.
c
        do j = 1, element_order
          do i = 1, 2
            t(i,j) = node_xy(i,element_node(j,element))
          end do
        end do
c
c  Evaluate the X and Y derivatives of the basis functions at the nodes.
c
        call basis_mn_t6 ( t, element_order, t, phi, dphidx, dphidy )
c
c  Evaluate dCdX and dCdY at each node in the element, and add
c  them to the running totals.
c
        do node_local1 = 1, element_order

          node_global1 = element_node(node_local1,element)

          do node_local2 = 1, element_order

            node_global2 = element_node(node_local2,element)

            dcdx(node_global1) = dcdx(node_global1) 
     &        + c(node_global2) * dphidx(node_local2,node_local1)

            dcdy(node_global1) = dcdy(node_global1) 
     &        + c(node_global2) * dphidy(node_local2,node_local1)

          end do

          node_count(node_global1) = node_count(node_global1) + 1

        end do

      end do
c
c  Average the running totals.
c
      do j = 1, node_num
        dcdx(j) = dcdx(j) / dble ( node_count(j) )
        dcdy(j) = dcdy(j) / dble ( node_count(j) )
      end do

      return
      end
      subroutine div_q4 ( m, n, u, v, q, div, vort )

c*********************************************************************72
c
cc DIV_Q4 estimates the divergence and vorticity of a discrete field.
c
c  Discussion:
c
c    The routine is given the values of a vector field ( U(X,Y), V(X,Y) ) at
c    an array of points ( X(1:M), Y(1:N) ).
c
c    The routine models the vector field over the interior of this region using
c    a bilinear interpolant.  It then uses the interpolant to estimate the
c    value of the divergence:
c
c      DIV(X,Y) = dU/dX + dV/dY
c
c    and the vorticity:
c
c      VORT(X,Y) = dV/dX - dU/dY
c
c    at the center point of each of the bilinear elements.
c
c        |       |       |
c      (3,1)---(3,2)---(3,3)---
c        |       |       |
c        | [2,1] | [2,2] |
c        |       |       |
c      (2,1)---(2,2)---(2,3)---
c        |       |       |
c        | [1,1] | [1,2] |
c        |       |       |
c      (1,1)---(1,2)---(1,3)---
c
c    Here, the nodes labeled with parentheses represent the points at
c    which the original (U,V) data is given, while the nodes labeled
c    with square brackets represent the centers of the bilinear
c    elements, where the approximations to the divergence and vorticity
c    are made.
c
c    The reason for evaluating the divergence and vorticity in this way
c    is that the bilinear interpolant to the (U,V) data is not
c    differentiable at the boundaries of the elements, nor especially at
c    the nodes, but is an (infinitely differentiable) bilinear function
c    in the interior of each element.  If a value at the original nodes
c    is strongly desired, then the average at the four surrounding
c    central nodes may be taken.
c
c  Reference Element Q4:
c
c    |
c    1  4-----3
c    |  |     |
c    |  |     |
c    S  |     |
c    |  |     |
c    |  |     |
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of data rows.  
c    M must be at least 2.
c
c    Input, integer N, the number of data columns.  
c    N must be at least 2.
c
c    Input, double precision U(M,N), V(M,N), the value of the components 
c    of a vector quantity whose divergence and vorticity are desired. 
c    A common example would be that U and V are the horizontal and 
c    vertical velocity component of a flow field.
c
c    Input, double precision Q(2,4), the coordinates of the nodes of
c    the quadrilateral, in counterclockwise order.
c
c    Output, double precision DIV(M-1,N-1), an estimate for
c    the divergence in the bilinear element that lies between
c    data rows I and I+1, and data columns J and J+1.
c
c    Output, double precision VORT(M-1,N-1), an estimate for
c    the vorticity in the bilinear element that lies between
c    data rows I and I+1, and data columns J and J+1.
c
      implicit none

      integer m
      integer n

      double precision div(m-1,n-1)
      double precision dphidx(4)
      double precision dphidy(4)
      integer i
      integer j
      integer node_num
      parameter ( node_num = 1 )
      double precision p(2)
      double precision phi(4)
      double precision q(2,4)
      double precision q2(2,4)
      double precision u(m,n)
      double precision v(m,n)
      double precision vort(m-1,n-1)

      if ( m .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIV_Q4 - Fatal error!'
        write ( *, '(a)' ) '  M must be at least 2,'
        write ( *, '(a,i8)' ) '  but the input value of M is ', m
        stop
      end if

      if ( n .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIV_Q4 - Fatal error!'
        write ( *, '(a)' ) '  N must be at least 2,'
        write ( *, '(a,i8)' ) '  but the input value of N is ', n
        stop
      end if

      do i = 1, m - 1

        q2(2,1) =   ( dble ( 2 * m - 2 * i     ) * q(2,1)   
     &              + dble (         2 * i - 2 ) * q(2,3) ) 
     &              / dble ( 2 * m         - 2 )
        p(2) =      ( dble ( 2 * m - 2 * i - 1 ) * q(2,1)   
     &              + dble (         2 * i - 1 ) * q(2,3) ) 
     &              / dble ( 2 * m         - 2 )
        q2(2,3) =   ( dble ( 2 * m - 2 * i - 2 ) * q(2,1)   
     &              + dble (         2 * i     ) * q(2,3) ) 
     &              / dble ( 2 * m         - 2 )

        q2(2,2) = q2(2,1)
        q2(2,4) = q2(2,3)

        do j = 1, n - 1

          q2(1,1) =   ( dble ( 2 * n - 2 * j     ) * q(1,1)   
     &                + dble (         2 * j - 2 ) * q(1,3) ) 
     &                / dble ( 2 * n         - 2 )
          p(1) =      ( dble ( 2 * n - 2 * j - 1 ) * q(1,1)   
     &                + dble (         2 * j - 1 ) * q(1,3) ) 
     &                 / dble ( 2 * n         - 2 )
          q2(1,3) =   ( dble ( 2 * n - 2 * j - 2 ) * q(1,1)   
     &                + dble (         2 * j     ) * q(1,3) ) 
     &                / dble ( 2 * n         - 2 )

          q2(1,2) = q2(1,3)
          q2(1,4) = q2(1,1)

          call basis_mn_q4 ( q2, node_num, p, phi, dphidx, dphidy )
c
c  Note the following formula for the value of U and V at the same
c  point that the divergence and vorticity are being evaluated.
c
c         umid =  u(i  ,j  ) * phi(1) 
c               + u(i  ,j+1) * phi(2) 
c               + u(i+1,j+1) * phi(3) 
c               + u(i+1,j  ) * phi(4) 
c
c         vmid =  v(i  ,j  ) * phi(1) 
c               + v(i  ,j+1) * phi(2) 
c               + v(i+1,j+1) * phi(3) 
c               + v(i+1,j  ) * phi(4) 
c
          div(i,j) =  u(i  ,j  ) * dphidx(1) + v(i  ,j  ) * dphidy(1) 
     &              + u(i  ,j+1) * dphidx(2) + v(i  ,j+1) * dphidy(2) 
     &              + u(i+1,j+1) * dphidx(3) + v(i+1,j+1) * dphidy(3) 
     &              + u(i+1,j  ) * dphidx(4) + v(i+1,j  ) * dphidy(4) 

          vort(i,j) =  v(i  ,j  ) * dphidx(1) - u(i  ,j  ) * dphidy(1) 
     &               + v(i  ,j+1) * dphidx(2) - u(i  ,j+1) * dphidy(2) 
     &               + v(i+1,j+1) * dphidx(3) - u(i+1,j+1) * dphidy(3) 
     &               + v(i+1,j  ) * dphidx(4) - u(i+1,j  ) * dphidy(4) 
                      
        end do
      end do

      return
      end
      subroutine div_t3 ( m, n, u, v, q, div, vor )

c*********************************************************************72
c
cc DIV_T3 estimates the divergence and vorticity of a discrete field.
c
c  Discussion:
c
c    The routine is given the values of a vector field ( U(X,Y), V(X,Y) ) at
c    a regularly spaced grid of points ( X(1:M), Y(1:N) ).  This grid is 
c    described implicitly by giving the values M, N, and the coordinates
c    Q(2,4) of the bounding quadrilateral.  (Note that Q need not be a 
c    rectangle.)
c
c    The quadrilateral is suggested by the following diagram:
c
c     ^  Q(1:2,4)-----Q(1:2,3)
c     |      |            |
c     N      |            |
c     |      |            |
c     V  Q(1:2,1)-----Q(1:2,2)
c
c              <--(M)--->
c
c    The routine models the vector field over the interior of this region using
c    a linear interpolant over 2*(M-1)*(N-1) triangles.  It then uses the 
c    interpolant to estimate the value of the divergence:
c
c      DIV(X,Y) = dU/dX + dV/dY
c
c    and the vorticity:
c
c      VOR(X,Y) = dV/dX - dU/dY
c
c    at the centroid of each of the triangular elements.
c
c    The grid is (somewhat arbitrarily) subdivided into triangular elements
c    as suggested here:
c
c      (3,1)---(3,2)---(3,3)
c        | \     |  \    |
c        |  \    |   \   |
c        |   \   |    \  |
c        |    \  |     \ |
c      (2,1)---(2,2)---(2,3)
c        | \     |  \    |
c        |  \    |   \   |
c        |   \   |    \  |
c        |    \  |     \ |
c      (1,1)---(1,2)---(1,3)
c
c    In each triangular element, linear functions are used to interpolate
c    the U and V data.  The divergence and vorticity functions are then
c    evaluated at the centroid of each element.
c
c    This means that, given a grid of M X coordinates and N Y coordinates,
c    we will construct 2 * ( M - 1 ) * ( N - 1 ) triangular elements.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of data rows.  M must be at least 2.
c
c    Input, integer N, the number of data columns.  N must be at least 2.
c
c    Input, double precision U(M,N), V(M,N), the value of the components 
c    of a vector quantity whose divergence and vorticity are desired. 
c    A common example would be that U and V are the horizontal and 
c    vertical velocity component of a flow field.
c
c    Input, double precision Q(2,4), the coordinates of the nodes of
c    the quadrilateral, in counterclockwise order.
c
c    Output, double precision DIV(2,M-1,N-1), an estimate for
c    the divergence in the two linear elements that lie between
c    data rows I and I+1, and data columns J and J+1.
c
c    Output, double precision VOR(2,M-1,N-1), an estimate for
c    the vorticity in the two linear elements that lie between
c    data rows I and I+1, and data columns J and J+1.
c
      implicit none

      integer m
      integer n

      double precision div(2,m-1,n-1)
      double precision dphidx(3)
      double precision dphidy(3)
      double precision dudx
      double precision dudy
      double precision dvdx
      double precision dvdy
      integer i
      integer j
      double precision p(2)
      double precision phi(3)
      double precision q(2,4)
      double precision t(2,3)
      double precision u(m,n)
      double precision v(m,n)
      double precision vor(2,m-1,n-1)
      double precision xlb
      double precision xlt
      double precision xrb
      double precision xrt
      double precision xxlb
      double precision xxlt
      double precision xxrb
      double precision xxrt
      double precision ylb
      double precision ylt
      double precision yrb
      double precision yrt
      double precision yylb
      double precision yylt
      double precision yyrb
      double precision yyrt

      if ( m .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIV_T3 - Fatal error!'
        write ( *, '(a)' ) '  M must be at least 2,'
        write ( *, '(a,i8)' ) '  but the input value of M is ', m
        stop
      end if

      if ( n .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIV_T3 - Fatal error!'
        write ( *, '(a)' ) '  N must be at least 2,'
        write ( *, '(a,i8)' ) '  but the input value of N is ', n
        stop
      end if
c
c  Consider the data between logical rows I and I + 1.
c
      do i = 1, m - 1
c
c  Consider the data between logical columns J and J + 1.
c
        do j = 1, n - 1

          xlb = q(1,1)
          ylb = q(2,1)
          xrb = q(1,2)
          yrb = q(2,2)
          xrt = q(1,3)
          yrt = q(2,3)
          xlt = q(1,4)
          ylt = q(2,4)
          
          yylb = 
     &      ( 
     &          dble ( n - j     )  * ( dble ( m - i     ) * ylb   
     &                                + dble (     i - 1 ) * yrb ) 
     &                                / dble ( m     - 1 )         
     &        + dble (     j - 1 )  * ( dble ( m - i     ) * ylt   
     &                                + dble (     i - 1 ) * yrt ) 
     &                                / dble ( m     - 1 )         
     &      ) / dble ( n     - 1 )

          yyrb = ( 
     &             dble ( n - j     )  * ( dble ( m - i - 1 ) * ylb   
     &                                   + dble (     i     ) * yrb ) 
     &                                   / dble ( m     - 1 )         
     &           + dble (     j - 1 )  * ( dble ( m - i - 1 ) * ylt   
     &                                   + dble (     i     ) * yrt ) 
     &                                   / dble ( m     - 1 )         
     &         ) / dble ( n     - 1 )

          yylt = ( 
     &             dble ( n - j - 1 )  * ( dble ( m - i     ) * ylb   
     &                                   + dble (     i - 1 ) * yrb ) 
     &                                   / dble ( m     - 1 )         
     &           + dble (     j     )  * ( dble ( m - i     ) * ylt   
     &                                   + dble (     i - 1 ) * yrt ) 
     &                                   / dble ( m     - 1 )         
     &         ) / dble ( n     - 1 )       

          yyrt = ( 
     &             dble ( n - j - 1 )  * ( dble ( m - i - 1 ) * ylb   
     &                                   + dble (     i     ) * yrb ) 
     &                                   / dble ( m     - 1 )         
     &           + dble (     j     )  * ( dble ( m - i - 1 ) * ylt   
     &                                   + dble (     i     ) * yrt ) 
     &                                   / dble ( m     - 1 )         
     &         ) / dble ( n     - 1 )       

          xxlb = ( 
     &             dble ( n - j     )  * ( dble ( m - i     ) * xlb   
     &                                   + dble (     i - 1 ) * xrb ) 
     &                                   / dble ( m     - 1 )         
     &           + dble (     j - 1 )  * ( dble ( m - i     ) * xlt   
     &                                   + dble (     i - 1 ) * xrt ) 
     &                                   / dble ( m     - 1 )         
     &         ) / dble ( n     - 1 )

          xxlt = ( 
     &             dble ( n - j - 1 )  * ( dble ( m - i     ) * xlb   
     &                                   + dble (     i - 1 ) * xrb ) 
     &                                   / dble ( m     - 1 )         
     &           + dble (     j     )  * ( dble ( m - i     ) * xlt   
     &                                   + dble (     i - 1 ) * xrt ) 
     &                                   / dble ( m     - 1 )         
     &         ) / dble ( n     - 1 )

          xxrb = ( 
     &             dble ( n - j     )  * ( dble ( m - i - 1 ) * xlb   
     &                                   + dble (     i     ) * xrb ) 
     &                                   / dble ( m     - 1 )         
     &           + dble (     j - 1 )  * ( dble ( m - i - 1 ) * xlt   
     &                                   + dble (     i     ) * xrt ) 
     &                                   / dble ( m     - 1 )         
     &         ) / dble ( n     - 1 )

          xxrt = ( 
     &             dble ( n - j - 1 )  * ( dble ( m - i - 1 ) * xlb   
     &                                   + dble (     i     ) * xrb ) 
     &                                   / dble ( m     - 1 )         
     &           + dble (     j     )  * ( dble ( m - i - 1 ) * xlt   
     &                                   + dble (     i     ) * xrt ) 
     &                                   / dble ( m     - 1 )         
     &         ) / dble ( n     - 1 )

c         write(*,'(i4,i4,8f8.3)')i,j,xxlb, yylb, xxrb, yyrb, xxrt,yyrt, xxlt,yylt
c
c  (I,J+1) = LT-----RT = (I+1,J+1)
c            |\      |
c            | \  T2 |
c            |  \    |
c            |   \   |
c            | T1 \  |
c            |     \ |
c  (I,J)   = LB-----RB = (I+1,J)
c
          t(1,1) = xxlb
          t(2,1) = yylb
          t(1,2) = xxrb
          t(2,2) = yyrb
          t(1,3) = xxrt
          t(2,3) = yyrt

          p(1) = ( xxlb + xxrb + xxrt ) / 3.0D+00
          p(2) = ( yylb + yyrb + yyrt ) / 3.0D+00
          call basis_mn_t3 ( t, 1, p, phi, dphidx, dphidy )
          dudx = u(i,j) * dphidx(1) + u(i+1,j) * dphidx(2) 
     &      + u(i+1,j+1) * dphidx(3)
          dudy = u(i,j) * dphidy(1) + u(i+1,j) * dphidy(2) 
     &      + u(i+1,j+1) * dphidy(3)
          dvdx = v(i,j) * dphidx(1) + v(i+1,j) * dphidx(2) 
     &      + v(i+1,j+1) * dphidx(3)
          dvdy = v(i,j) * dphidy(1) + v(i+1,j) * dphidy(2) 
     &      + v(i+1,j+1) * dphidy(3)

          div(1,i,j) = dudx + dvdy
          vor(1,i,j) = dvdx - dudy

c         write ( *, '(4g14.6)' ) p(1), p(2), div(1,i,j), vor(1,i,j)

          t(1,1) = xxrt
          t(2,1) = yyrt
          t(1,2) = xxlt
          t(2,2) = yylt
          t(1,3) = xxlb
          t(2,3) = yylb

          p(1) = ( xxrt + xxlt + xxlb ) / 3.0D+00
          p(2) = ( yyrt + yylt + yyrb ) / 3.0D+00

          call basis_mn_t3 ( t, 1, p, phi, dphidx, dphidy )
          dudx = u(i+1,j+1) * dphidx(1) + u(i,j+1) * dphidx(2) 
     &      + u(i,j) * dphidx(3)
          dudy = u(i+1,j+1) * dphidy(1) + u(i,j+1) * dphidy(2) 
     &      + u(i,j) * dphidy(3)
          dvdx = v(i+1,j+1) * dphidx(1) + v(i,j+1) * dphidx(2) 
     &      + v(i,j) * dphidx(3)
          dvdy = v(i+1,j+1) * dphidy(1) + v(i,j+1) * dphidy(2) 
     &      + v(i,j) * dphidy(3)

          div(2,i,j) = dudx + dvdy
          vor(2,i,j) = dvdx - dudy

c         write ( *, '(4g14.6)' ) p(1), p(2), div(2,i,j), vor(2,i,j)

        end do
      end do

      return
      end
      function element_code ( i )

c*********************************************************************72
c
cc ELEMENT_CODE returns the code for each element.
c
c  Discussion:
c
c     I  ELEMENT_CODE   Definition
c     -  ------------   ----------
c     1  Q4             4 node linear Lagrange/serendipity quadrilateral;
c     2  Q8             8 node quadratic serendipity quadrilateral;
c     3  Q9             9 node quadratic Lagrange quadrilateral;
c     4  Q12            12 node cubic serendipity quadrilateral;
c     5  Q16            16 node cubic Lagrange quadrilateral;
c     6  QL             6 node linear/quadratic quadrilateral;
c     7  T3             3 node linear triangle;
c     8  T4             4 node cubic bubble triangle
c     9  T6             6 node quadratic triangle;
c    10  T10            10 node cubic triangle.
c 
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the index of the element type.  
c
c    Output, character * ( 3 ) ELEMENT_CODE, the code for the element type.
c
      implicit none

      character * ( 3 ) element_code
      integer i

      if ( i .eq. 1 ) then
        element_code = 'Q4'
      else if ( i .eq. 2 ) then
        element_code = 'Q8'
      else if ( i .eq. 3 ) then
        element_code = 'Q9'
      else if ( i .eq. 4 ) then
        element_code = 'Q12'
      else if ( i .eq. 5 ) then
        element_code = 'Q16'
      else if ( i .eq. 6 ) then
        element_code = 'QL'
      else if ( i .eq. 7 ) then
        element_code = 'T3'
      else if ( i .eq. 8 ) then
        element_code = 'T4'
      else if ( i .eq. 9 ) then
        element_code = 'T6'
      else if ( i .eq. 10 ) then
        element_code = 'T10'
      else
        element_code = '???'
      end if

      return
      end
      subroutine elements_eps ( file_name, node_num, node_xy, code, 
     &  element_order, element_num, element_mask, element_node, title )

c*********************************************************************72
c
cc ELEMENTS_EPS creates an EPS file image of the elements of a grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILE_NAME, the name of the file to create.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the
c    coordinates of the nodes.
c
c    Input, character ( len = * ) CODE, the code for the element.
c
c    Input, integer ELEMENT_ORDER, the element order.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, logical ELEMENT_MASK(ELEMENT_NUM), a mask for the elements.
c
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), the 
c    element->node data.
c
c    Input, character * ( * ) TITLE, a title for the plot.
c
      implicit none

      integer element_num
      integer element_order
      integer node_num

      double precision ave_x
      double precision ave_y
      integer circle_size
      parameter ( circle_size = 3 )
      character * ( * ) code
      double precision dif
      integer element
      logical element_mask(element_num)
      integer element_node(element_order,element_num)
      integer eps_unit
      integer eps_x
      integer eps_y
      character * ( * )  file_name
      integer i
      integer ios
      integer j
      integer local
      integer next_boundary_node
      integer node
      logical node_mask(node_num)
      double precision node_x_max
      double precision node_x_min
      double precision node_xy(2,node_num)
      double precision node_y_max
      double precision node_y_min
      double precision r8_huge
      double precision scale
      character * ( 40 ) string
      character * ( * ) title
c
c  Determine the range of the unmasked elements.
c
      node_x_min =  r8_huge ( )
      node_x_max = -r8_huge ( )
      node_y_min =  r8_huge ( )
      node_y_max = -r8_huge ( )

      do i = 1, node_num
        node_mask(i) = .false.
      end do

      do element = 1, element_num
        if ( element_mask(element) ) then
          do j = 1, element_order
            node = element_node(j,element)
            node_mask(node) = .true.
            node_x_min = min ( node_x_min, node_xy(1,node) )
            node_x_max = max ( node_x_max, node_xy(1,node) )
            node_y_min = min ( node_y_min, node_xy(2,node) )
            node_y_max = max ( node_y_max, node_xy(2,node) )
          end do
        end if
      end do

      if ( node_y_max - node_y_min < node_x_max - node_x_min ) then
        scale = node_x_max - node_x_min
        dif = ( node_x_max - node_x_min ) - ( node_y_max - node_y_min )
        node_y_max = node_y_max + 0.5D+00 * dif
        node_y_min = node_y_min - 0.5D+00 * dif
      else
        scale = node_y_max - node_y_min
        dif = ( node_y_max - node_y_min ) - ( node_x_max - node_x_min )
        node_x_max = node_x_max + 0.5D+00 * dif
        node_x_min = node_x_min - 0.5D+00 * dif
      end if

      call get_unit ( eps_unit )

      open ( unit = eps_unit, file = file_name, status = 'replace' )

      write ( eps_unit, '(a)' ) '%cPS-Adobe-3.0 EPSF-3.0'
      write ( eps_unit, '(a)' ) '%%Creator: elements_eps(fempack.f)'
      write ( eps_unit, '(a)' ) '%%Title: ' // trim ( file_name )
      write ( eps_unit, '(a)' ) '%%Pages: 1'
      write ( eps_unit, '(a)' ) '%%BoundingBox:    36    36   576   756'
      write ( eps_unit, '(a)' ) '%%Document-Fonts: Times-Roman'
      write ( eps_unit, '(a)' ) '%%LanguageLevel: 1'
      write ( eps_unit, '(a)' ) '%%EndComments'
      write ( eps_unit, '(a)' ) '%%BeginProlog'
      write ( eps_unit, '(a)' ) '/inch {72 mul} def'
      write ( eps_unit, '(a)' ) '%%EndProlog'
      write ( eps_unit, '(a)' ) '%%Page:      1     1'
      write ( eps_unit, '(a)' ) 'save'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '% Set RGB line color.'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) ' 0.9000 0.9000 0.9000 setrgbcolor'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '% Draw a gray border around the page.'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) 'newpath'
      write ( eps_unit, '(a)' ) '    36   126 moveto'
      write ( eps_unit, '(a)' ) '   576   126 lineto'
      write ( eps_unit, '(a)' ) '   576   666 lineto'
      write ( eps_unit, '(a)' ) '    36   666 lineto'
      write ( eps_unit, '(a)' ) '    36   126 lineto'
      write ( eps_unit, '(a)' ) 'stroke'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '% Set RGB line color.'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) ' 0.0000 0.0000 0.0000 setrgbcolor'

      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '%  Label the plot:'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) ' 0.0000 0.0000 0.0000 setrgbcolor'
      write ( eps_unit, '(a)' ) 
     &  '/Times-Roman findfont 0.50 inch scalefont setfont'
      write ( eps_unit, '(a)' ) '    36   666 moveto'
      write ( eps_unit, '(a)' ) '(' // trim ( title ) // ') show'

      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '% Define a clipping polygon'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '    36   126 moveto'
      write ( eps_unit, '(a)' ) '   576   126 lineto'
      write ( eps_unit, '(a)' ) '   576   666 lineto'
      write ( eps_unit, '(a)' ) '    36   666 lineto'
      write ( eps_unit, '(a)' ) '    36   126 lineto'
      write ( eps_unit, '(a)' ) 'clip newpath'

      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '%  Draw filled dots at each node:'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) ' 0.0000 0.0000 0.9000 setrgbcolor'

      do node = 1, node_num

        if ( node_mask(node) ) then

          eps_x = int 
     &      ( ( node_x_max - node_xy(1,node)              ) *  61.0D+00   
     &      + (            + node_xy(1,node) - node_x_min ) 
     &      * 551.0D+00 )
     &      / scale

          eps_y = int 
     &      ( ( node_y_max - node_xy(2,node)              ) * 151.0D+00   
     &      + (              node_xy(2,node) - node_y_min ) 
     &      * 641.0D+00 )
     &      / scale

          write ( eps_unit, '(a,i4,2x,i4,2x,i4,a)' ) 
     &      'newpath  ', eps_x, eps_y, circle_size, 
     &      ' 0 360 arc closepath fill'

        end if

      end do

      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '%  Label the nodes:'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) ' 0.0000 0.0000 1.0000 setrgbcolor'
      write ( eps_unit, '(a)' ) 
     &  '/Times-Roman findfont 0.20 inch scalefont setfont'

      do node = 1, node_num

        if ( node_mask(node) ) then

          eps_x = int 
     &      ( ( node_x_max - node_xy(1,node)              ) *  61.0D+00   
     &      + (            + node_xy(1,node) - node_x_min ) 
     &      * 551.0D+00 ) 
     &      / scale

          eps_y = int 
     &      ( ( node_y_max - node_xy(2,node)              ) * 151.0D+00   
     &      + (              node_xy(2,node) - node_y_min ) 
     &      * 641.0D+00 ) 
     &      / scale

          write ( string, '(i4)' ) node

          write ( eps_unit, '(i4,2x,i4,a)' ) eps_x, eps_y+5, 
     &      ' moveto (' // trim ( string ) // ') show'

        end if

      end do

      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '%  Draw the element sides:'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) ' 0.9000 0.0000 0.0000 setrgbcolor'

      do element = 1, element_num

        if ( .not. element_mask(element) ) then
          go to 30
        end if

        local = 1
        node = element_node(local,element)

        eps_x = int 
     &    ( ( node_x_max - node_xy(1,node)              ) *  61.0D+00   
     &    + (            + node_xy(1,node) - node_x_min ) * 551.0D+00 ) 
     &    / scale

        eps_y = int 
     &    ( ( node_y_max - node_xy(2,node)              ) * 151.0D+00   
     &    + (              node_xy(2,node) - node_y_min ) * 641.0D+00 ) 
     &    / scale

        write ( eps_unit, '(a,i4,2x,i4,a)' ) 
     &    'newpath ', eps_x, eps_y, ' moveto'

10      continue

          local = next_boundary_node ( local, code )
          node = element_node(local,element)

          eps_x = int 
     &      ( ( node_x_max - node_xy(1,node)              ) *  61.0D+00   
     &      + (            + node_xy(1,node) - node_x_min ) 
     &      * 551.0D+00 ) 
     &      / scale

          eps_y = int 
     &      ( ( node_y_max - node_xy(2,node)              ) * 151.0D+00 
     &      + (              node_xy(2,node) - node_y_min ) 
     &      * 641.0D+00 ) 
     &      / scale

          write ( eps_unit, '(i4,2x,i4,a)' ) eps_x, eps_y, ' lineto'

          if ( local .eq. 1 ) then
            go to 20
          end if

        go to 10

20      continue

        write ( eps_unit, '(a)' ) 'stroke'

30      continue

      end do

      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '%  Label the elements:'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) ' 1.0000 0.0000 0.0000 setrgbcolor'
      write ( eps_unit, '(a)' ) 
     &  '/Times-Roman findfont 0.30 inch scalefont setfont'

      do element = 1, element_num

        if ( .not. element_mask(element) ) then
          go to 40
        end if

        ave_x = 0.0D+00
        ave_y = 0.0D+00

        do i = 1, element_order

          node = element_node(i,element)

          ave_x = ave_x + node_xy(1,node)
          ave_y = ave_y + node_xy(2,node)

        end do

        ave_x = ave_x / dble ( element_order )
        ave_y = ave_y / dble ( element_order )

        eps_x = int 
     &    ( ( node_x_max - ave_x              ) *  61.0D+00   
     &    + (            + ave_x - node_x_min ) * 551.0D+00 ) 
     &    / scale

        eps_y = int 
     &    ( ( node_y_max - ave_y              ) * 151.0D+00   
     &    + (              ave_y - node_y_min ) * 641.0D+00 ) 
     &    / scale

        write ( string, '(i4)' ) element

        write ( eps_unit, '(i4,2x,i4,a)' ) eps_x, eps_y, ' moveto (' 
     &    // trim ( string ) // ') show'

40      continue

      end do

      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) 'restore showpage'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '% End of page'
      write ( eps_unit, '(a)' ) '%'
      write ( eps_unit, '(a)' ) '%%Trailer'
      write ( eps_unit, '(a)' ) '%%EOF'

      close ( unit = eps_unit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ELEMENTS_EPS: An encapsulated PostScript'
      write ( *, '(a)' ) '  file was created containing an image of '
      write ( *, '(a)' ) '  the nodes and elements.  '
      write ( *, '(a)' ) '  The file is "' // trim ( file_name ) // '".'

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
      subroutine grid_element ( code, element_order, nelemx, nelemy, 
     &  element_node )

c*********************************************************************72
c
cc GRID_ELEMENT returns the element grid associated with any available element.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element desired.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3', 
c    'T4', 'T6' and 'T10'.
c
c    Input, integer ELEMENT_ORDER, the number of nodes
c    per element.
c
c    Input, integer NELEMX, NELEMY, the number of quadrilaterals 
c    along the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY for quadrilaterals, or 2 * NELEMX * NELEMY for
c    triangles.
c
c    Output, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
c    the nodes that form each element.  
c
      implicit none

      integer element_order

      character ( len = * ) code
      integer element_node(element_order,*)
      integer nelemx
      integer nelemy
      logical s_eqi

      if ( s_eqi ( code, 'Q4' ) ) then
        call grid_q4_element ( nelemx, nelemy, element_node )
      else if ( s_eqi ( code, 'Q8' ) ) then
        call grid_q8_element ( nelemx, nelemy, element_node )
      else if ( s_eqi ( code, 'Q9' ) ) then
        call grid_q9_element ( nelemx, nelemy, element_node )
      else if ( s_eqi ( code, 'Q12' ) ) then
        call grid_q12_element ( nelemx, nelemy, element_node )
      else if ( s_eqi ( code, 'Q16' ) ) then
        call grid_q16_element ( nelemx, nelemy, element_node )
      else if ( s_eqi ( code, 'QL' ) ) then
        call grid_ql_element ( nelemx, nelemy, element_node )
      else if ( s_eqi ( code, 'T3' ) ) then
        call grid_t3_element ( nelemx, nelemy, element_node )
      else if ( s_eqi ( code, 'T4' ) ) then
        call grid_t4_element ( nelemx, nelemy, element_node )
      else if ( s_eqi ( code, 'T6' ) ) then
        call grid_t6_element ( nelemx, nelemy, element_node )
      else if ( s_eqi ( code, 'T10' ) ) then
        call grid_t10_element ( nelemx, nelemy, element_node )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GRID_ELEMENT - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Illegal value of CODE = "' // trim ( code ) // '".'
        stop

      end if

      return
      end
      subroutine grid_element_num ( code, nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_ELEMENT_NUM returns the number of elements in a grid.
c
c  Discussion:
c
c    The number of elements generated will be NELEMX * NELEMY for
c    quadrilaterals, or 2 * NELEMX * NELEMY for triangles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element desired.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3', 
c    'T4', 'T6' and 'T10'.
c
c    Input, integer NELEMX, NELEMY, the number of quadrilaterals 
c    along the X and Y directions.  
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      character * ( * ) code
      integer element_num
      integer nelemx
      integer nelemy
      logical s_eqi

      if ( s_eqi ( code, 'Q4' ) ) then
        call grid_q4_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'Q8' ) ) then
        call grid_q8_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'Q9' ) ) then
        call grid_q9_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'Q12' ) ) then
        call grid_q12_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'Q16' ) ) then
        call grid_q16_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'QL' ) ) then
        call grid_ql_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'T3' ) ) then
        call grid_t3_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'T4' ) ) then
        call grid_t4_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'T6' ) ) then
        call grid_t6_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'T10' ) ) then
        call grid_t10_element_num ( nelemx, nelemy, element_num )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GRID_ELEMENT_NUM - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Illegal value of CODE = "' // trim ( code ) // '".'
        element_num = -1
        stop

      end if

      return
      end
      subroutine grid_node_num ( code, nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_NODE_NUM returns the number of nodes in a grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element desired.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3', 
c    'T4', 'T6' and 'T10'.
c
c    Input, integer NELEMX, NELEMY, the number of quadrilaterals 
c    along the X and Y directions.  
c
c    Output, integer NODE_NUM, the number of elements in the grid.
c
      implicit none

      character * ( * ) code
      integer node_num
      integer nelemx
      integer nelemy
      logical s_eqi

      if ( s_eqi ( code, 'Q4' ) ) then
        call grid_q4_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'Q8' ) ) then
        call grid_q8_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'Q9' ) ) then
        call grid_q9_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'Q12' ) ) then
        call grid_q12_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'Q16' ) ) then
        call grid_q16_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'QL' ) ) then
        call grid_ql_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'T3' ) ) then
        call grid_t3_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'T4' ) ) then
        call grid_t4_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'T6' ) ) then
        call grid_t6_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'T10' ) ) then
        call grid_t10_node_num ( nelemx, nelemy, node_num )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GRID_NODE_NUM - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Illegal value of CODE = "' // trim ( code ) // '".'
        node_num = -1
        stop

      end if

      return
      end
      subroutine grid_nodes_01 ( x_num, y_num, node_xy )

c*********************************************************************72
c
cc GRID_NODES_01 returns an equally spaced rectangular grid in the unit square.
c
c  Example:
c
c    X_NUM = 5
c    Y_NUM = 3
c
c    NODE_XY = 
c    ( 0, 0.25, 0.5, 0.75, 1, 0,   0.25, 0.5, 0.75, 1,   0, 0.25, 0.5, 0.75, 1;
c      0, 0,    0,   0,    0, 0.5, 0.5,  0.5, 0.5,  0.5, 1, 1.0,  1.0, 1.0,  1 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X_NUM, Y_NUM, the number of nodes in the 
c    X and Y directions.
c
c    Output, double precision NODE_XY(2,X_NUM*Y_NUM), the coordinates of
c    the nodes.
c
      implicit none

      integer x_num
      integer y_num

      integer i
      integer j
      integer node_num
      double precision node_xy(2,x_num*y_num)

      node_num = x_num * y_num

      do j = 1, node_num
        do i = 1, 2
          node_xy(i,j) = 0.0D+00
        end do
      end do

      if ( x_num .eq. 1 ) then
        do j = 1, node_num
          node_xy(1,j) = 0.5D+00
        end do
      else
        do i = 1, x_num
          do j = i, node_num, x_num
            node_xy(1,j) = dble ( i - 1 ) / dble ( x_num - 1 )
          end do
        end do
      end if

      if ( y_num .eq. 1 ) then
        do j = 1, node_num
          node_xy(2,1:node_num) = 0.5D+00
        end do
      else
        do i = 1, y_num
          do j = 1 + ( i - 1 ) * x_num, i * x_num
            node_xy(2,j) = dble ( i - 1 ) / dble ( y_num - 1 )
          end do
        end do
      end if

      return
      end
      subroutine grid_print ( element_order, element_num, element_node )

c*********************************************************************72
c
cc GRID_PRINT prints the elements that form a grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ELEMENT_ORDER, the number of nodes 
c    per element.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c 
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
c    the nodes that form each element.
c
      implicit none

      integer element_order
      integer element_num

      integer element
      integer element_node(element_order,element_num)
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  GRID_PRINT: Element -> Node table.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of elements = ', element_num
      write ( *, '(a,i8)' ) '  Element order      = ', element_order
      write ( *, '(a)' ) ' '
      write ( *, '(2x,a,3x,20i3)' ) '  #', ( i, i = 1, element_order )
      write ( *, '(a)' ) ' '

      do element = 1, element_num
        write ( *, '(2x,i3,3x,20i3)' ) 
     &    element, element_node(1:element_order,element)
      end do

      return
      end
      subroutine grid_q4_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_Q4_ELEMENT produces an element grid of 4 node quadrilaterals.
c
c  Discussion:
c
c    For each element, the nodes are listed in counter-clockwise order.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE = 
c         1, 2,  6,  5;
c         2, 3,  7,  6;
c         3, 4,  8,  7;
c         5, 6, 10,  9;
c         6, 7, 11, 10;
c         7, 8, 12, 11.
c
c  Grid:
c
c    9---10---11---12
c    |    |    |    |
c    |    |    |    |
c    |  4 |  5 |  6 |
c    |    |    |    |
c    5----6----7----8
c    |    |    |    |
c    |    |    |    |
c    |  1 |  2 |  3 |
c    |    |    |    |
c    1----2----3----4
c
c  Reference Element Q4:
c
c    |
c    1  4-----3
c    |  |     |
c    |  |     |
c    S  |     |
c    |  |     |
c    |  |     |
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(4,NELEMX*NELEMY), the nodes 
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 4 )

      integer element
      integer element_node(element_order,nelemx*nelemy)
      integer i
      integer j
      integer ne
      integer nw
      integer se
      integer sw
c
c  Node labeling:
c
c    NW---NE
c     |    |
c    SW---SE
c
      element = 0
     
      do j = 1, nelemy
        do i = 1, nelemx

          sw = i     + ( j - 1 ) * ( nelemx + 1 )
          se = i + 1 + ( j - 1 ) * ( nelemx + 1 )
          nw = i     +   j       * ( nelemx + 1 )
          ne = i + 1 +   j       * ( nelemx + 1 )
      
          element = element + 1

          element_node(1,element) = sw
          element_node(2,element) = se
          element_node(3,element) = ne
          element_node(4,element) = nw

        end do
      end do

      return
      end
      subroutine grid_q4_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_Q4_ELEMENT_NUM counts the elements in a grid of 4 node quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = NELEMX * NELEMY = 6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions. 
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = nelemx * nelemy

      return
      end
      subroutine grid_q4_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_Q4_NODE_NUM counts the nodes in a grid of 4 node quadrilaterals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = ( nelemx + 1 ) * ( nelemy + 1 )

      return
      end
      subroutine grid_q8_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_Q8_ELEMENT produces an element grid of 8 node quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE =
c         1,  3, 14, 12,  2,  9, 13,  8;
c         3,  5, 16, 14,  4, 10, 15,  9;
c         5,  7, 18, 16,  6, 11, 17, 10;
c        12, 14, 25, 23, 13, 20, 24, 19;
c        14, 16, 27, 25, 15, 21, 26, 20;
c        16, 18, 29, 27, 17, 22, 28, 21.
c
c  Diagram:
c
c   23---24---25---26---27---28---29
c    |         |         |         |
c    |         |         |         |
c   19        20        21        22
c    |         |         |         |
c    | 4       | 5       | 6       |
c   12---13---14---15---16---17---18
c    |         |         |         |
c    |         |         |         |
c    8         9        10        11
c    |         |         |         |
c    | 1       | 2       | 3       |
c    1----2----3----4----5----6----7
c
c  Reference Element Q8:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8     6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(8,NELEMX*NELEMY), the nodes that form
c    each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 8 )

      integer e
      integer element
      integer element_node(element_order,nelemx*nelemy)
      integer i
      integer j
      integer n
      integer ne
      integer nw
      integer s
      integer se
      integer sw
      integer w
c
c  Node labeling:
c
c    NW----N----NE
c     |          |
c     W   (C)    E
c     |          |
c    SW----S----SE
c

      element = 0

      do j = 1, nelemy
        do i = 1, nelemx

          sw = ( j - 1 )  * ( 3 * nelemx + 2 ) + 2 * i - 1
          w  = sw + 2 * nelemx + 2 - i
          nw = sw + 3 * nelemx + 2

          s =  sw + 1
          n =  sw + ( 3 * nelemx + 2 ) + 1

          se = sw + 2
          e  = sw + 2 * nelemx + 2 - i + 1
          ne = sw + ( 3 * nelemx + 2 ) + 2

          element = element + 1

          element_node(1,element) = sw
          element_node(2,element) = se
          element_node(3,element) = ne
          element_node(4,element) = nw
          element_node(5,element) = s
          element_node(6,element) = e
          element_node(7,element) = n
          element_node(8,element) = w

        end do
      end do

      return
      end
      subroutine grid_q8_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_Q8_ELEMENT_NUM counts the elements in a grid of 8 node quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = NELEMX * NELEMY = 6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NUM, the number of elements in the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = nelemx * nelemy

      return
      end
      subroutine grid_q8_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_Q8_NODE_NUM counts the nodes in a grid of 8 node quadrilaterals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.
c
c    Output, integer NODE_NUM, the number of node in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = 3 * nelemx * nelemy + 2 * nelemx + 2 * nelemy + 1

      return
      end
      subroutine grid_q9_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_Q9_ELEMENT produces an element grid of 9 node quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE = 
c         1,  3, 17, 15,  2, 10, 16,  8,  9;
c         3,  5, 19, 17,  4, 12, 18, 10, 11;
c         5,  7, 21, 19,  6, 14, 20, 12, 13;
c        15, 17, 31, 29, 16, 24, 30, 22, 23;
c        17, 19, 33, 31, 18, 26, 32, 24, 25;
c        19, 21, 35, 33, 20, 28, 34, 26, 27.
c
c  Grid:
c
c   29---30---31---32---33---34---35
c    |    .    |    .    |    .    |
c    |    .    |    .    |    .    |
c   22 . 23 . 24 . 25 . 26 . 27 . 28
c    |    .    |    .    |    .    |
c    | 4  .    | 5  .    | 6  .    |
c   15---16---17---18---19---20---21
c    |    .    |    .    |    .    |
c    |    .    |    .    |    .    |
c    8 .  9 . 10 . 11 . 12 . 13 . 14
c    |    .    |    .    |    .    |
c    | 1  .    | 2  .    | 3  .    |
c    1----2----3----4----5----6----7
c
c  Reference Element Q9:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8  9  6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(9,NELEMX*NELEMY), the nodes that 
c    form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 9 )

      integer c
      integer e
      integer element
      integer element_node(element_order,nelemx*nelemy)
      integer i
      integer j
      integer n
      integer ne
      integer nw
      integer s
      integer se
      integer sw
      integer w
c
c  Node labeling:
c
c    NW----N----NE
c     |          |
c     W    C     E
c     |          |
c    SW----S----SE
c
      element = 0

      do j = 1, nelemy
        do i = 1, nelemx

          sw = 2 * ( j - 1 )  * ( 2 * nelemx + 1 ) + 2 * ( i - 1 ) + 1
          w  = sw +               2 * nelemx + 1
          nw = sw +         2 * ( 2 * nelemx + 1 )

          s  = sw + 1
          c  = sw + 1 +               2 * nelemx + 1
          n  = sw + 1 +         2 * ( 2 * nelemx + 1 )

          se = sw + 2
          e  = sw + 2 +               2 * nelemx + 1
          ne = sw + 2 +         2 * ( 2 * nelemx + 1 )

          element = element + 1

          element_node(1,element) = sw
          element_node(2,element) = se
          element_node(3,element) = ne
          element_node(4,element) = nw
          element_node(5,element) = s
          element_node(6,element) = e
          element_node(7,element) = n
          element_node(8,element) = w
          element_node(9,element) = c

        end do
      end do

      return
      end
      subroutine grid_q9_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_Q9_ELEMENT_NUM counts the elements in a grid of 9 node quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = NELEMX * NELEMY = 6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = nelemx * nelemy

      return
      end
      subroutine grid_q9_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_Q9_NODE_NUM counts the nodes in a grid of 9 node quadrilaterals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions. 
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = ( 2 * nelemx + 1 ) * ( 2 * nelemy + 1 )

      return
      end
      subroutine grid_q12_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_Q12_ELEMENT produces an element grid of 12 node quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE =
c         1,  2,  3,  4, 11, 12, 15, 16, 19, 20, 21, 22;
c         4,  5,  6,  7, 12, 13, 16, 17, 22, 23, 24, 25;
c         7,  8,  9, 10, 13, 14, 17, 18, 25, 26, 27, 28;
c        19, 20, 21, 22, 29, 30, 33, 34, 37, 38, 39, 40;
c        22, 23, 24, 25, 30, 31, 34, 35, 40, 41, 42, 43;
c        25, 26, 27, 28, 31, 32, 35, 36, 43, 44, 45, 46.
c
c  Grid:
c
c   37-38-39-40-41-42-43-44-45-46
c    |        |        |        |
c   33       34       35       36
c    |        |        |        |
c   29       30       31       32
c    | 4      | 5      | 6      |
c   19-20-21-22-23-24-25-26-27-28
c    |        |        |        |
c   15       16       17       18
c    |        |        |        |
c   11       12       13       14
c    | 1      | 2      | 3      |
c    1--2--3--4--5--6--7--8--9-10
c
c  Reference Element Q12:
c
c    |
c    1  9-10-11-12
c    |  |        |
c    |  7        8
c    S  |        |
c    |  5        6
c    |  |        |
c    0  1--2--3--4
c    |
c    +--0---R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(12,NELEMX*NELEMY), the nodes 
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 12 )

      integer base
      integer element
      integer element_node(element_order,nelemx*nelemy)
      integer i
      integer j

      element = 0

      do j = 1, nelemy
        do i = 1, nelemx

          base = ( j - 1 )  * ( 5 * nelemx + 3 ) + 1

          element = element + 1

          element_node( 1,element) = base + ( i - 1 ) * 3
          element_node( 2,element) = base + ( i - 1 ) * 3 + 1
          element_node( 3,element) = base + ( i - 1 ) * 3 + 2
          element_node( 4,element) = base + ( i - 1 ) * 3 + 3

          element_node( 5,element) = base + 3 * nelemx + i
          element_node( 6,element) = base + 3 * nelemx + i + 1

          element_node( 7,element) = base + 4 * nelemx + i + 1
          element_node( 8,element) = base + 4 * nelemx + i + 2

          element_node( 9,element) = base + 5 * nelemx + 3 * i
          element_node(10,element) = base + 5 * nelemx + 3 * i + 1
          element_node(11,element) = base + 5 * nelemx + 3 * i + 2
          element_node(12,element) = base + 5 * nelemx + 3 * i + 3

        end do
      end do

      return
      end
      subroutine grid_q12_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_Q12_ELEMENT_NUM counts the elements in a grid of 12 node quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = NELEMX * NELEMY = 6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = nelemx * nelemy

      return
      end
      subroutine grid_q12_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_Q12_NODE_NUM counts the nodes in a grid of 12 node quadrilaterals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements 
c    along the X and Y directions.
c
c    Output, integer NODE_NUM, the number of node in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = 5 * nelemx * nelemy + 3 * nelemx + 3 * nelemy + 1

      return
      end
      subroutine grid_q16_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_Q16_ELEMENT produces an element grid of 16 node quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 2, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE = 
c         1,  2,  3,  4,  8,  9, 10, 11, 15, 16, 17, 18, 22, 23, 24, 25;
c         4,  5,  6,  7, 11, 12, 13, 14, 18, 19, 20, 21, 25, 26, 27, 28;
c        22, 23, 24, 25, 29, 30, 31, 32, 36, 37, 38, 39, 43, 44, 45, 46;
c        25, 26, 27, 28, 32, 33, 34, 35, 39, 40, 41, 42, 46, 47, 48, 49. 
c        
c  Grid:
c
c   43-44-45-46-47-48-49
c    |        |        |
c    |        |        |
c   36 37 38 39 40 41 42
c    |        |        |
c    |        |        |
c   29 30 31 32 33 34 35
c    |        |        |
c    | 3      | 4      |
c   22-23-24-25-26-27-28
c    |        |        |
c    |        |        |
c   15 16 17 18 19 20 21
c    |        |        |
c    |        |        |
c    8  9 10 11 12 13 14
c    |        |        |
c    | 1      | 2      |
c    1--2--3--4--5--6--7
c
c  Reference Element Q16:
c
c    |
c    1 13--14--15--16
c    |  |   :   :   |
c    |  |   :   :   |
c    |  9..10..11..12
c    S  |   :   :   |
c    |  |   :   :   |
c    |  5...6...7...8
c    |  |   :   :   |
c    |  |   :   :   |  
c    0  1---2---3---4
c    |
c    +--0-----R-----1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(16,NELEMX*NELEMY), the nodes 
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 16 )

      integer base
      integer element
      integer element_node(element_order,nelemx*nelemy)
      integer i
      integer j

      element = 0
     
      do j = 1, nelemy
        do i = 1, nelemx

          base = ( j - 1 ) * 3 * ( 3 * nelemx + 1 ) + 3 * i - 2

          element = element + 1

          element_node( 1,element) = base
          element_node( 2,element) = base                          + 1
          element_node( 3,element) = base                          + 2
          element_node( 4,element) = base                          + 3
          element_node( 5,element) = base +     ( 3 * nelemx + 1 )
          element_node( 6,element) = base +     ( 3 * nelemx + 1 ) + 1
          element_node( 7,element) = base +     ( 3 * nelemx + 1 ) + 2
          element_node( 8,element) = base +     ( 3 * nelemx + 1 ) + 3
          element_node( 9,element) = base + 2 * ( 3 * nelemx + 1 )
          element_node(10,element) = base + 2 * ( 3 * nelemx + 1 ) + 1
          element_node(11,element) = base + 2 * ( 3 * nelemx + 1 ) + 2
          element_node(12,element) = base + 2 * ( 3 * nelemx + 1 ) + 3
          element_node(13,element) = base + 3 * ( 3 * nelemx + 1 )
          element_node(14,element) = base + 3 * ( 3 * nelemx + 1 ) + 1
          element_node(15,element) = base + 3 * ( 3 * nelemx + 1 ) + 2
          element_node(16,element) = base + 3 * ( 3 * nelemx + 1 ) + 3

        end do
      end do

      return
      end
      subroutine grid_q16_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_Q16_ELEMENT_NUM counts the elements in a grid of 16 node quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = NELEMX * NELEMY = 6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NUM, the number of elements in the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = nelemx * nelemy

      return
      end
      subroutine grid_q16_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_Q16_NODE_NUM counts the nodes in a grid of 16 node quadrilaterals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = ( 3 * nelemx + 1 ) * ( 3 * nelemy + 1 )

      return
      end
      subroutine grid_ql_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_QL_ELEMENT produces an element grid of 6 node quadratics/linears.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE = 
c         1,  2,  3,  8,  9, 10;
c         3,  4,  5, 10, 11, 12;
c         5,  6,  7, 12, 13, 14;
c         8,  9, 10, 15, 16, 17;
c        10, 11, 12, 17, 18, 19;
c        12, 13, 14, 19, 20, 21.
c
c  Grid:
c
c   15---16---17---18---19---20---21
c    |         |         |         |
c    |         |         |         |
c    |    4    |    5    |    6    |
c    |         |         |         |
c    |         |         |         |
c    8----9---10---11---12---13---14
c    |         |         |         |
c    |         |         |         |
c    |    1    |    2    |    3    |
c    |         |         |         |
c    |         |         |         |
c    1----2----3----4----5----6----7
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.  X will the the "quadratic direction", and
c    Y will be the "linear direction".
c
c    Output, integer ELEMENT_NODE(6,NELEMX*NELEMY), the nodes 
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 6 )

      integer base
      integer element
      integer element_node(element_order,nelemx*nelemy)
      integer i
      integer j

      element = 0
     
      do j = 1, nelemy
        do i = 1, nelemx

          base = ( j - 1 )  * ( 2 * nelemx + 1 ) + 2 * i - 1

          element = element + 1

          element_node(1,element) = base
          element_node(2,element) = base + 1
          element_node(3,element) = base + 2
          element_node(4,element) = base + ( 2 * nelemx + 1 )
          element_node(5,element) = base + ( 2 * nelemx + 1 ) + 1
          element_node(6,element) = base + ( 2 * nelemx + 1 ) + 2

        end do
      end do

      return
      end
      subroutine grid_ql_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_QL_ELEMENT_NUM counts the elements in a grid of QL quadrilaterals.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = NELEMX * NELEMY = 6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements 
c    along the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NUM, the number of elements in the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = nelemx * nelemy

      return
      end
      subroutine grid_ql_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_QL_NODE_NUM counts the nodes in a grid of QL quadrilaterals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements 
c    along the X and Y directions.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = 2 * nelemx * nelemy + 2 * nelemx + nelemy + 1

      return
      end
      subroutine grid_shape_2d ( n, a, n1, n2 )

c*********************************************************************72
c
cc GRID_SHAPE_2D guesses the shape N1 by N2 of a vector of data.
c
c  Discussion:
c
c    The data vector A is assumed to contain N1 * N2 values, with
c    where each of N2 values is repeated N1 times.
c
c  Example:
c
c    Input:
c
c      A = ( 2, 2, 2, 7, 7, 7 )
c
c    Output:
c
c      N1 = 3, N2 = 2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of data values.
c
c    Input, double precision A(N), the data, which should have the properties
c    described above.
c
c    Output, integer N1, N2, the "shape" of the data in the array.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      integer n1
      integer n2
c
c  Make a guess for N1.
c
      i = 1
      n1 = 1

      do i = 2, n
        if ( a(i) .ne. a(1) ) then
          go to 10
        end if
        n1 = n1 + 1
      end do

10    continue
c
c  Guess that N2 = N / N1.
c
      n2 = n / n1

      return
      end
      subroutine grid_t3_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_T3_ELEMENT produces an element grid of pairs of 3 node triangles.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE = 
c         1,  2,  5;
c         6,  5,  2;
c         2,  3,  6;
c         7,  6,  3;
c         3,  4,  7;
c         8,  7,  4;
c         5,  6,  9;
c        10,  9,  6;
c         6,  7, 10;
c        11, 10,  7;
c         7,  8, 11;
c        12, 11,  8.
c
c  Grid:
c
c    9---10---11---12
c    |\ 8 |\10 |\12 |
c    | \  | \  | \  |
c    |  \ |  \ |  \ |
c    |  7\|  9\| 11\|
c    5----6----7----8
c    |\ 2 |\ 4 |\ 6 |
c    | \  | \  | \  |
c    |  \ |  \ |  \ |
c    |  1\|  3\|  5\|
c    1----2----3----4
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    2 * NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(3,2*NELEMX*NELEMY), the nodes 
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 3 )

      integer element
      integer element_node(element_order,2*nelemx*nelemy)
      integer i
      integer j
      integer ne
      integer nw
      integer se
      integer sw
c
c  Node labeling:
c
c    NW--NE
c     |\ |
c     | \|
c    SW--SE
c
      element = 0
     
      do j = 1, nelemy
        do i = 1, nelemx

          sw = i     + ( j - 1 ) * ( nelemx + 1 )
          se = i + 1 + ( j - 1 ) * ( nelemx + 1 )
          nw = i     +   j       * ( nelemx + 1 )
          ne = i + 1 +   j       * ( nelemx + 1 )

          element = element + 1

          element_node(1,element) = sw
          element_node(2,element) = se
          element_node(3,element) = nw

          element = element + 1

          element_node(1,element) = ne
          element_node(2,element) = nw
          element_node(3,element) = se

        end do
      end do

      return
      end
      subroutine grid_t3_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_T3_ELEMENT_NUM counts the elements in a grid of 3 node triangles.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = 2 * NELEMX * NELEMY = 12
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NUM, the number of elements in the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = 2 * nelemx * nelemy

      return
      end
      subroutine grid_t3_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_T3_NODE_NUM counts the nodes in a grid of 3 node triangles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = ( nelemx + 1 ) * ( nelemy + 1 )

      return
      end
      subroutine grid_t4_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_T4_ELEMENT produces an element grid of pairs of 4 node triangles.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE = 
c         1,  2,  11,  5;
c        12, 11,   2,  8;
c         2,  3,  12,  6;
c        13, 12,   3,  9;
c         3   4   13,  7;
c        14, 13,   4,  10;
c        11, 12,  21,  15;
c        22, 21,  12,  18;
c        12, 13,  22,  16;
c        23, 22,  13,  19;
c        13  14   23,  17;
c        24, 23,  14,  20;
c
c  Grid:
c
c   21---22---23---24
c    |\18 |\19 |\20 |
c    | \  | \  | \  |
c    |  \ |  \ |  \ |
c    | 15\| 16\| 17\|
c   11---12---13---14
c    |\ 8 |\ 9 |\10 |
c    | \  | \  | \  |
c    |  \ |  \ |  \ |
c    | 5 \|  6\|  7\|
c    1----2----3----4
c
c  Reference Element T4:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  | 4 \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    2 * NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(4,2*NELEMX*NELEMY), the nodes 
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 4 )

      integer element
      integer element_node(element_order,2*nelemx*nelemy)
      integer i
      integer j
      integer nc
      integer ne
      integer nw
      integer sc
      integer se
      integer sw
c
c  Node labeling:
c
c    NW----NE
c     |\   |
c     | \NC|
c     |SC\ |
c     |   \|
c    SW---SE
c
      element = 0
     
      do j = 1, nelemy
        do i = 1, nelemx

          sw = i     + ( j - 1 ) * ( 3 * nelemx + 1 )
          se = sw + 1
          sc = sw +     nelemx + 1
          nc = sw + 2 * nelemx + 1
          nw = sw + 3 * nelemx + 1
          ne = sw + 3 * nelemx + 2

          element = element + 1
          element_node(1,element) = sw
          element_node(2,element) = se
          element_node(3,element) = nw
          element_node(4,element) = sc

          element = element + 1
          element_node(1,element) = ne
          element_node(2,element) = nw
          element_node(3,element) = se
          element_node(4,element) = nc

        end do
      end do

      return
      end
      subroutine grid_t4_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_T4_ELEMENT_NUM counts the elements in a grid of 4 node triangles.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = 2 * NELEMX * NELEMY = 12
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = 2 * nelemx * nelemy

      return
      end
      subroutine grid_t4_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_T4_NODE_NUM counts the nodes in a grid of 4 node triangles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = ( nelemx + 1 ) * ( nelemy + 1 ) + 2 * nelemx * nelemy

      return
      end
      subroutine grid_t6_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_T6_ELEMENT produces an element grid of pairs of 6 node triangles.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE = 
c         1,  3, 15,  2,  9,  8;
c        17, 15,  3, 16,  9, 10;
c         3,  5, 17,  4, 11, 10;
c        19, 17,  5, 18, 11, 12;
c         5,  7, 19,  6, 13, 12;
c        21, 19,  7, 20, 13, 14;
c        15, 17, 29, 16, 23, 22;
c        31, 29, 17, 30, 23, 24;
c        17, 19, 31, 18, 25, 24;
c        33, 31, 19, 32, 25, 26;
c        19, 21, 33, 20, 27, 26;
c        35, 33, 21, 34, 27, 28.
c
c  Grid:
c
c   29-30-31-32-33-34-35
c    |\ 8  |\10  |\12  |
c    | \   | \   | \   |
c   22 23 24 25 26 27 28
c    |   \ |   \ |   \ |
c    |  7 \|  9 \| 11 \|
c   15-16-17-18-19-20-21
c    |\ 2  |\ 4  |\ 6  |
c    | \   | \   | \   |
c    8  9 10 11 12 13 14
c    |   \ |   \ |   \ |
c    |  1 \|  3 \|  5 \|
c    1--2--3--4--5--6--7
c
c  Reference Element T6:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  6  5
c    |  |   \
c    |  |    \
c    0  1--4--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    2 * NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(6,2*NELEMX*NELEMY), the nodes 
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 6 )

      integer c
      integer e
      integer element
      integer element_node(element_order,2*nelemx*nelemy)
      integer i
      integer j
      integer n
      integer ne
      integer nw
      integer s
      integer se
      integer sw
      integer w
c
c  Node labeling:
c
c    NW---N--NE
c     | \     |
c     W   C   E
c     |    \  |
c    SW---S--SE
c
      element = 0
     
      do j = 1, nelemy
        do i = 1, nelemx

          sw = 2 * ( j - 1 )  * ( 2 * nelemx + 1 ) + 2 * ( i - 1 ) + 1
          w  = sw +               2 * nelemx + 1
          nw = sw +         2 * ( 2 * nelemx + 1 )

          s  = sw + 1
          c  = sw + 1 +               2 * nelemx + 1
          n  = sw + 1 +         2 * ( 2 * nelemx + 1 )

          se = sw + 2
          e  = sw + 2 +               2 * nelemx + 1
          ne = sw + 2 +         2 * ( 2 * nelemx + 1 )

          element = element + 1

          element_node(1,element) = sw
          element_node(2,element) = se
          element_node(3,element) = nw
          element_node(4,element) = s
          element_node(5,element) = c
          element_node(6,element) = w

          element = element + 1

          element_node(1,element) = ne
          element_node(2,element) = nw
          element_node(3,element) = se
          element_node(4,element) = n
          element_node(5,element) = c
          element_node(6,element) = e

        end do
      end do

      return
      end
      subroutine grid_t6_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_T6_ELEMENT_NUM counts the elements in a grid of 6 node triangles.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = 2 * NELEMX * NELEMY = 12
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NUM, the number of elements in the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = 2 * nelemx * nelemy

      return
      end
      subroutine grid_t6_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_T6_NODE_NUM counts the nodes in a grid of 6 node triangles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = ( 2 * nelemx + 1 ) * ( 2 * nelemy + 1 )

      return
      end
      subroutine grid_t10_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc GRID_T10_ELEMENT produces an element grid of pairs of 10 node triangles.
c
c  Example:
c
c    Input:
c
c      NELEMX = 2, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE = 
c         1,  2,  3,  4, 10, 16, 22, 15,  8,  9;
c        25, 24, 23, 22, 16, 10,  4, 11, 18, 17;
c         4,  5,  6,  7, 13, 19, 25, 18, 11, 12;
c        28, 27, 26, 25, 19, 13,  7, 14, 21, 20;
c        22, 23, 24, 25, 31, 37, 43, 36, 29, 30;
c        46, 45, 44, 43, 37, 31, 25, 32, 39, 38;
c        25, 26, 27, 28, 34, 40, 46, 39, 31, 33;
c        49, 48, 47, 46, 40, 34, 28, 35, 42, 41.
c        
c  Grid:
c
c   43-44-45-46-47-48-49
c    |\     6 |\     8 |
c    | \      | \      |
c   36 37 38 39 40 41 42
c    |   \    |   \    |
c    |    \   |    \   |
c   29 30 31 32 33 34 35
c    |      \ |      \ |
c    | 5     \| 7     \|
c   22-23-24-25-26-27-28
c    |\     2 |\     4 |
c    | \      | \      |
c   15 16 17 18 19 20 21
c    |   \    |   \    |
c    |    \   |    \   |
c    8  9 10 11 12 13 14
c    |      \ |      \ |
c    | 1     \| 3     \|
c    1--2--3--4--5--6--7
c
c  Reference Element T10:
c
c    |
c    1  10
c    |  |\
c    |  | \
c    |  8  9
c    |  |   \
c    S  |    \
c    |  5  6  7
c    |  |      \
c    |  |       \
c    0  1--2--3--4
c    |
c    +--0----R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    2 * NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(10,2*NELEMX*NELEMY), the nodes 
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 10 )

      integer base
      integer element
      integer element_node(element_order,2*nelemx*nelemy)
      integer i
      integer j

      element = 0
     
      do j = 1, nelemy
        do i = 1, nelemx

          base = ( j - 1 ) * 3 * ( 3 * nelemx + 1 ) + 3 * i - 2

          element = element + 1

          element_node( 1,element) = base
          element_node( 2,element) = base                          + 1
          element_node( 3,element) = base                          + 2
          element_node( 4,element) = base                          + 3
          element_node( 5,element) = base +     ( 3 * nelemx + 1 ) + 2
          element_node( 6,element) = base + 2 * ( 3 * nelemx + 1 ) + 1
          element_node( 7,element) = base + 3 * ( 3 * nelemx + 1 )
          element_node( 8,element) = base + 2 * ( 3 * nelemx + 1 )
          element_node( 9,element) = base +     ( 2 * nelemx + 1 ) + 2
          element_node(10,element) = base +     ( 2 * nelemx + 1 ) + 3

          element = element + 1

          element_node( 1,element) = base + 3 * ( 3 * nelemx + 1 ) + 3
          element_node( 2,element) = base + 3 * ( 3 * nelemx + 1 ) + 2
          element_node( 3,element) = base + 3 * ( 3 * nelemx + 1 ) + 1
          element_node( 4,element) = base + 3 * ( 3 * nelemx + 1 )
          element_node( 5,element) = base + 2 * ( 3 * nelemx + 1 ) + 1
          element_node( 6,element) = base +     ( 3 * nelemx + 1 ) + 2
          element_node( 7,element) = base                          + 3
          element_node( 8,element) = base +     ( 3 * nelemx + 1 ) + 3
          element_node( 9,element) = base + 2 * ( 3 * nelemx + 1 ) + 3
          element_node(10,element) = base + 2 * ( 3 * nelemx + 1 ) + 2

        end do
      end do

      return
      end
      subroutine grid_t10_element_num ( nelemx, nelemy, element_num )

c*********************************************************************72
c
cc GRID_T10_ELEMENT_NUM counts the elements in a grid of 10 node triangles.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = 2 * NELEMX * NELEMY = 12
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NUM, the number of elements in the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = 2 * nelemx * nelemy

      return
      end
      subroutine grid_t10_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc GRID_T10_NODE_NUM counts the nodes in a grid of 10 node triangles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = ( 3 * nelemx + 1 ) * ( 3 * nelemy + 1 )

      return
      end
      subroutine grid_test ( code )

c*********************************************************************72
c
cc GRID_TEST tests the grid element routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, the code for the element.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
c    'T3', 'T4', 'T6' and 'T10'.
c
      implicit none
     
      character * ( * ) code
      integer element_node(16,12)
      integer element_num
      integer element_order
      integer nelemx
      integer nelemy
      integer node_num
      integer order_code
      integer width
c
c  NODE is defined as a vector rather than a two dimensional array,
c  so that we can handle the various cases using a single array.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  GRID_TEST for element "' // trim ( code ) // '".'

      nelemx = 3
      nelemy = 2

      write ( *, '(a,i8)' ) 
     &  '  Number of elements in X direction = ', nelemx
      write ( *, '(a,i8)' ) 
     &  '  Number of elements in Y direction = ', nelemy

      element_order = order_code ( code )

      call grid_node_num ( code, nelemx, nelemy, node_num )

      write ( *, '(a,i8)' ) 
     &  '  Nodes per element =       ', element_order
      write ( *, '(a,i8)' ) '  Nodes in grid =           ', node_num

      call grid_element_num ( code, nelemx, nelemy, element_num )

      call grid_element ( code, element_order, nelemx, nelemy, 
     &  element_node )

      call grid_print ( element_order, element_num, element_node )

      call grid_width ( element_order, element_num, element_node, 
     &  width )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Grid width is ', width

      return
      end
      subroutine grid_width ( element_order, element_num, element_node, 
     &  width )

c*********************************************************************72
c
cc GRID_WIDTH computes the width of a given grid.
c
c  Discussion:
c
c    The grid width is defined to the maximum absolute
c    difference of global indices of nodes in the same element.
c
c  Example:
c
c    For the following grid, the grid width is 13.
c
c   23---24---25---26---27---28---29
c    |         |         |         |
c    |         |         |         |
c   19        20        21        22
c    |         |         |         |
c    | 4       | 5       | 6       |
c   12---13---14---15---16---17---18
c    |         |         |         |
c    |         |         |         |
c    8         9        10        11
c    |         |         |         |
c    | 1       | 2       | 3       |
c    1----2----3----4----5----6----7
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ELEMENT_ORDER, the number of nodes per element.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c 
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
c    the nodes that form each element.
c
c    Output, integer WIDTH, the grid width.
c
      implicit none

      integer element_num
      integer element_order

      integer element
      integer element_node(element_order,element_num)
      integer ip1
      integer ip2
      integer node1
      integer node2
      integer width

      width = 0
 
      do element = 1, element_num
        do node1 = 1, element_order
          ip1 = element_node(node1,element)
          do node2 = 1, element_order
            ip2 = element_node(node2,element)
            width = max ( width, abs ( ip1 - ip2 ) )
          end do
        end do
      end do
 
      return
      end
      function i4_modp ( i, j )

c*********************************************************************72
c
cc I4_MODP returns the nonnegative remainder of integer division.
c
c  Discussion:
c
c    If
c      NREM = I4_MODP ( I, J )
c      NMULT = ( I - NREM ) / J
c    then
c      I = J * NMULT + NREM
c    where NREM is always nonnegative.
c
c    The MOD function computes a result with the same sign as the
c    quantity being divided.  Thus, suppose you had an angle A,
c    and you wanted to ensure that it was between 0 and 360.
c    Then mod(A,360) would do, if A was positive, but if A
c    was negative, your result would be between -360 and 0.
c
c    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
c
c  Example:
c
c        I     J     MOD I4_MODP    Factorization
c
c      107    50       7       7    107 =  2 *  50 + 7
c      107   -50       7       7    107 = -2 * -50 + 7
c     -107    50      -7      43   -107 = -3 *  50 + 43
c     -107   -50      -7      43   -107 =  3 * -50 + 43
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the number to be divided.
c
c    Input, integer J, the number that divides I.
c
c    Output, integer I4_MODP, the nonnegative remainder when I is
c    divided by J.
c
      implicit none

      integer i
      integer i4_modp
      integer j
      integer value

      if ( j .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_MODP - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
        stop
      end if

      value = mod ( i, j )

      if ( value .lt. 0 ) then
        value = value + abs ( j )
      end if

      i4_modp = value

      return
      end
      function i4_wrap ( ival, ilo, ihi )

c*********************************************************************72
c
cc I4_WRAP forces an I4 to lie between given limits by wrapping.
c
c  Example:
c
c    ILO = 4, IHI = 8
c
c    I  Value
c
c    -2     8
c    -1     4
c     0     5
c     1     6
c     2     7
c     3     8
c     4     4
c     5     5
c     6     6
c     7     7
c     8     8
c     9     4
c    10     5
c    11     6
c    12     7
c    13     8
c    14     4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer IVAL, an integer value.
c
c    Input, integer ILO, IHI, the desired bounds for the integer value.
c
c    Output, integer I4_WRAP, a "wrapped" version of IVAL.
c
      implicit none

      integer i4_modp
      integer i4_wrap
      integer ihi
      integer ilo
      integer ival
      integer jhi
      integer jlo
      integer value
      integer wide

      jlo = min ( ilo, ihi )
      jhi = max ( ilo, ihi )

      wide = jhi - jlo + 1

      if ( wide .eq. 1 ) then
        value = jlo
      else
        value = jlo + i4_modp ( ival - jlo, wide )
      end if

      i4_wrap = value

      return
      end
      subroutine i4mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc I4MAT_WRITE writes an I4MAT file.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2009
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
c    Input, integer TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      integer table(m,n)
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

        write ( string, '(a1,i8,a4)' ) '(', m, 'i10)'
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
      subroutine i4vec_print ( n, a, title )

c*********************************************************************72
c
cc I4VEC_PRINT prints an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of integer values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, integer A(N), the vector to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer n

      integer a(n)
      integer i
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,i12)' ) i, ':', a(i)
      end do

      return
      end
      subroutine interp ( code, element_order, r, s, node_u, u, dudr, 
     &  duds )

c*********************************************************************72
c
cc INTERP interpolates a quantity in an element from basis node values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
c    'T3', 'T4', 'T6' and 'T10'.
c
c    Input, integer ELEMENT_ORDER, the number of nodes per element.
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Input, double precision NODE_U(ELEMENT_ORDER), the value of the quantity 
c    at the basis nodes.
c
c    Output, double precision U, DUDR, DUDS, the interpolated value of the
c    quantity and its derivatives at the point (R,S).
c 
      implicit none

      integer element_order

      character * ( * ) code
      double precision dtdr(element_order)
      double precision dtds(element_order)
      double precision dudr
      double precision duds
      double precision node_u(element_order)
      double precision r
      double precision r8vec_dot_product
      double precision s
      double precision t(element_order)
      double precision u

      call shape ( code, r, s, t, dtdr, dtds )
 
      u    = r8vec_dot_product ( element_order, node_u, t )
      dudr = r8vec_dot_product ( element_order, node_u, dtdr )
      duds = r8vec_dot_product ( element_order, node_u, dtds )
 
      return
      end
      subroutine interp_test ( code )

c*********************************************************************72
c
cc INTERP_TEST tests the interpolation property of an element.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
c    'T3', 'T4', 'T6' and 'T10'.
c
      implicit none

      integer element_order_max
      parameter ( element_order_max = 16 )

      double precision area
      character * ( * ) code
      double precision dudr
      double precision dudr_exact
      double precision duds
      double precision duds_exact
      integer element_order
      integer i
      integer node
      double precision node_r(element_order_max)
      double precision node_s(element_order_max)
      double precision node_u(element_order_max)
      integer order_code
      double precision r
      double precision r_factor
      double precision r8_power
      integer rexp(element_order_max)
      double precision s
      double precision s_factor
      integer sexp(element_order_max)
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 5 )
      double precision u
      double precision u_exact

      if ( code .eq. 't4' .or. code .eq. 'T4' ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'INTERP_TEST - Warning!'
        write ( *, '(a)' ) '  Skipping test for element "T4".'
        return
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  'INTERP_TEST for element "' // trim ( code ) // '".'

      element_order = order_code ( code )

      write ( *, '(a,i8)' ) '  Element order = ', element_order
c
c  Get the coordinates of the reference nodes.
c
      call node_reference ( code, node_r, node_s, area )
c
c  Get the monomial exponents for which the element is exact.
c
      call poly ( code, rexp, sexp )

      seed = 123456789

      do i = 1, element_order

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8,a,i8)' ) 
     &    '  Interpolate R      ', rexp(i), ' * S      ', sexp(i)
        write ( *, '(a)' ) ' '
c
c  Evaluate R^REXP(I) * S^SEXP(I) at the nodes.  This is our data.
c
        do node = 1, element_order
          r = node_r(node)
          s = node_s(node)
          if ( rexp(i) == 0 ) then
            r_factor = 1.0D+00
          else
            r_factor = r**rexp(i)
          end if
          if ( sexp(i) == 0 ) then
            s_factor = 1.0D+00
          else
            s_factor = s**sexp(i)
          end if
          node_u(node) = r_factor * s_factor
          write ( *, '(a,3g14.6)' ) 
     &      '  (R,S,U):        ', r, s, node_u(node)
        end do
c
c  Now pick random points in the element, and compute the interpolated
c  value of R^REXP(*) * S^SEXP(I) there.  Mathematically, these
c  values should be exact.
c
        do test = 1, test_num

          call reference_sample ( code, seed, r, s )

          write ( *, '(a)' ) ' '
          write ( *, '(a,2g14.6)' ) '  (R,S):          ', r, s

          u_exact = r8_power ( r, rexp(i) ) * r8_power ( s, sexp(i) )

          dudr_exact = dble ( rexp(i) ) 
     &      * r8_power ( r, rexp(i) - 1 ) * r8_power ( s, sexp(i) )

          duds_exact = r8_power ( r, rexp(i) ) * dble ( sexp(i) ) 
     &      * r8_power ( s, sexp(i) - 1 )

          call interp ( code, element_order, r, s, node_u, u, dudr, 
     &      duds )

          write ( *, '(a,3g14.6)' ) 
     &      '  (U,U*,Error):   ', u_exact, u, 
     &      abs ( u_exact - u )
          write ( *, '(a,3g14.6)' ) 
     &      '  (Ur,Ur*,Error): ', dudr_exact, dudr, 
     &      abs ( dudr_exact - dudr )
          write ( *, '(a,3g14.6)' ) 
     &      '  (Us,Us*,Error): ', duds_exact, duds, 
     &      abs ( duds_exact - duds )

        end do

      end do

      return
      end
      subroutine legendre_dr_compute ( n, x, w )

c*********************************************************************72
c
cc LEGENDRE_DR_COMPUTE: Gauss-Legendre quadrature by Davis-Rabinowitz method.
c
c  Discussion:
c
c    The integral:
c
c      integral ( -1 <= x <= 1 ) f(x) dx
c
c    The quadrature rule:
c
c      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 June 2011
c
c  Author:
c
c    Original FORTRAN77 version by Philip Davis, Philip Rabinowitz.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Dover, 2007,
c    ISBN: 0486453391,
c    LC: QA299.3.D28.
c
c  Parameters:
c
c    Input, integer N, the order.
c    0 .lt. N.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

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
      double precision w(n)
      double precision x(n)
      double precision x0
      double precision xtemp

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LEGENDRE_DR_COMPUTE - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal value of N = ', n
        stop
      end if

      e1 = dble ( n * ( n + 1 ) )

      m = ( n + 1 ) / 2

      do i = 1, m

        mp1mi = m + 1 - i

        t = dble ( 4 * i - 1 ) * pi 
     &    / dble ( 4 * n + 2 )

        x0 = cos ( t ) * ( 1.0D+00 - ( 1.0D+00 - 1.0D+00 
     &    / dble ( n ) ) 
     &    / dble ( 8 * n * n ) )

        pkm1 = 1.0D+00
        pk = x0

        do k = 2, n
          pkp1 = 2.0D+00 * x0 * pk - pkm1 - ( x0 * pk - pkm1 ) 
     &      / dble ( k )
          pkm1 = pk
          pk = pkp1
        end do

        d1 = dble ( n ) * ( pkm1 - x0 * pk )

        dpn = d1 / ( 1.0D+00 - x0 ) / ( 1.0D+00 + x0 )

        d2pn = ( 2.0D+00 * x0 * dpn - e1 * pk ) / ( 1.0D+00 - x0 ) 
     &    / ( 1.0D+00 + x0 )

        d3pn = ( 4.0D+00 * x0 * d2pn + ( 2.0D+00 - e1 ) * dpn ) 
     &    / ( 1.0D+00 - x0 ) / ( 1.0D+00 + x0 )

        d4pn = ( 6.0D+00 * x0 * d3pn + ( 6.0D+00 - e1 ) * d2pn ) 
     &    / ( 1.0D+00 - x0 ) / ( 1.0D+00 + x0 )

        u = pk / dpn
        v = d2pn / dpn
c
c  Initial approximation H:
c
        h = - u * ( 1.0D+00 + 0.5D+00 * u * ( v + u * ( v * v - d3pn / 
     &    ( 3.0D+00 * dpn ) ) ) )
c
c  Refine H using one step of Newton's method:
c
        p = pk + h * ( dpn + 0.5D+00 * h * ( d2pn + h / 3.0D+00 
     &    * ( d3pn + 0.25D+00 * h * d4pn ) ) )

        dp = dpn + h * ( d2pn + 0.5D+00 * h * 
     &    ( d3pn + h * d4pn / 3.0D+00 ) )

        h = h - p / dp

        xtemp = x0 + h

        x(mp1mi) = xtemp

        fx = d1 - h * e1 * ( pk + 0.5D+00 * h * ( dpn + h / 3.0D+00 
     &    * ( d2pn + 0.25D+00 * h 
     &    * ( d3pn + 0.2D+00 * h * d4pn ) ) ) )

        w(mp1mi) = 2.0D+00 * ( 1.0D+00 - xtemp ) 
     &    * ( 1.0D+00 + xtemp ) / ( fx * fx )

      end do

      if ( mod ( n, 2 ) .eq. 1 ) then
        x(1) = 0.0D+00
      end if
c
c  Shift the data up.
c
      nmove = ( n + 1 ) / 2
      ncopy = n - nmove

      do i = 1, nmove
        iback = n + 1 - i
        x(iback) = x(iback-ncopy)
        w(iback) = w(iback-ncopy)
      end do
c
c  Reflect values for the negative abscissas.
c
      do i = 1, n - nmove
        x(i) = - x(n+1-i)
        w(i) = w(n+1-i)
      end do

      return
      end
      subroutine legendre_set ( n, x, w )

c*********************************************************************72
c
cc LEGENDRE_SET sets abscissas and weights for Gauss-Legendre quadrature.
c
c  Discussion:
c
c    The integral:
c
c      integral ( -1 <= x <= 1 ) f(x) dx
c
c    The quadrature rule:
c
c      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
c
c    The quadrature rule is exact for polynomials through degree 2*N-1.
c
c    The abscissas are the zeroes of the Legendre polynomial P(N)(X).
c
c    Mathematica can compute the abscissas and weights of a Gauss-Legendre
c    rule of order N for the interval [A,B] with P digits of precision
c    by the commands:
c
c    Needs["NumericalDifferentialEquationAnalysis`"]
c    GaussianQuadratureWeights [ n, a, b, p ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Vladimir Krylov,
c    Approximate Calculation of Integrals,
c    Dover, 2006,
c    ISBN: 0486445798,
c    LC: QA311.K713.
c
c    Arthur Stroud, Don Secrest,
c    Gaussian Quadrature Formulas,
c    Prentice Hall, 1966,
c    LC: QA299.4G3S7.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996,
c    ISBN: 0-8493-2479-3,
c    LC: QA47.M315.
c
c  Parameters:
c
c    Input, integer N, the order.
c    N must be between 1 and 33 or 63/64/65, 127/128/129,
c    255/256/257.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision w(n)
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = 0.000000000000000000000000000000D+00

        w(1) = 2.000000000000000000000000000000D+00

      else if ( n .eq. 2 ) then

        x(1) = -0.577350269189625764509148780502D+00
        x(2) = 0.577350269189625764509148780502D+00

        w(1) = 1.000000000000000000000000000000D+00
        w(2) = 1.000000000000000000000000000000D+00

      else if ( n .eq. 3 ) then

        x(1) = -0.774596669241483377035853079956D+00
        x(2) = 0.000000000000000000000000000000D+00
        x(3) = 0.774596669241483377035853079956D+00

        w(1) = 0.555555555555555555555555555556D+00
        w(2) = 0.888888888888888888888888888889D+00
        w(3) = 0.555555555555555555555555555556D+00

      else if ( n .eq. 4 ) then

        x(1) = -0.861136311594052575223946488893D+00
        x(2) = -0.339981043584856264802665759103D+00
        x(3) = 0.339981043584856264802665759103D+00
        x(4) = 0.861136311594052575223946488893D+00

        w(1) = 0.347854845137453857373063949222D+00
        w(2) = 0.652145154862546142626936050778D+00
        w(3) = 0.652145154862546142626936050778D+00
        w(4) = 0.347854845137453857373063949222D+00

      else if ( n .eq. 5 ) then

        x(1) = -0.906179845938663992797626878299D+00
        x(2) = -0.538469310105683091036314420700D+00
        x(3) = 0.000000000000000000000000000000D+00
        x(4) = 0.538469310105683091036314420700D+00
        x(5) = 0.906179845938663992797626878299D+00

        w(1) = 0.236926885056189087514264040720D+00
        w(2) = 0.478628670499366468041291514836D+00
        w(3) = 0.568888888888888888888888888889D+00
        w(4) = 0.478628670499366468041291514836D+00
        w(5) = 0.236926885056189087514264040720D+00

      else if ( n .eq. 6 ) then

        x(1) = -0.932469514203152027812301554494D+00
        x(2) = -0.661209386466264513661399595020D+00
        x(3) = -0.238619186083196908630501721681D+00
        x(4) = 0.238619186083196908630501721681D+00
        x(5) = 0.661209386466264513661399595020D+00
        x(6) = 0.932469514203152027812301554494D+00

        w(1) = 0.171324492379170345040296142173D+00
        w(2) = 0.360761573048138607569833513838D+00
        w(3) = 0.467913934572691047389870343990D+00
        w(4) = 0.467913934572691047389870343990D+00
        w(5) = 0.360761573048138607569833513838D+00
        w(6) = 0.171324492379170345040296142173D+00

      else if ( n .eq. 7 ) then

        x(1) = -0.949107912342758524526189684048D+00
        x(2) = -0.741531185599394439863864773281D+00
        x(3) = -0.405845151377397166906606412077D+00
        x(4) = 0.000000000000000000000000000000D+00
        x(5) = 0.405845151377397166906606412077D+00
        x(6) = 0.741531185599394439863864773281D+00
        x(7) = 0.949107912342758524526189684048D+00

        w(1) = 0.129484966168869693270611432679D+00
        w(2) = 0.279705391489276667901467771424D+00
        w(3) = 0.381830050505118944950369775489D+00
        w(4) = 0.417959183673469387755102040816D+00
        w(5) = 0.381830050505118944950369775489D+00
        w(6) = 0.279705391489276667901467771424D+00
        w(7) = 0.129484966168869693270611432679D+00

      else if ( n .eq. 8 ) then

        x(1) = -0.960289856497536231683560868569D+00
        x(2) = -0.796666477413626739591553936476D+00
        x(3) = -0.525532409916328985817739049189D+00
        x(4) = -0.183434642495649804939476142360D+00
        x(5) = 0.183434642495649804939476142360D+00
        x(6) = 0.525532409916328985817739049189D+00
        x(7) = 0.796666477413626739591553936476D+00
        x(8) = 0.960289856497536231683560868569D+00

        w(1) = 0.101228536290376259152531354310D+00
        w(2) = 0.222381034453374470544355994426D+00
        w(3) = 0.313706645877887287337962201987D+00
        w(4) = 0.362683783378361982965150449277D+00
        w(5) = 0.362683783378361982965150449277D+00
        w(6) = 0.313706645877887287337962201987D+00
        w(7) = 0.222381034453374470544355994426D+00
        w(8) = 0.101228536290376259152531354310D+00

      else if ( n .eq. 9 ) then

        x(1) = -0.968160239507626089835576203D+00
        x(2) = -0.836031107326635794299429788D+00
        x(3) = -0.613371432700590397308702039D+00
        x(4) = -0.324253423403808929038538015D+00
        x(5) = 0.000000000000000000000000000D+00
        x(6) = 0.324253423403808929038538015D+00
        x(7) = 0.613371432700590397308702039D+00
        x(8) = 0.836031107326635794299429788D+00
        x(9) = 0.968160239507626089835576203D+00

        w(1) = 0.081274388361574411971892158111D+00
        w(2) = 0.18064816069485740405847203124D+00
        w(3) = 0.26061069640293546231874286942D+00
        w(4) = 0.31234707704000284006863040658D+00
        w(5) = 0.33023935500125976316452506929D+00
        w(6) = 0.31234707704000284006863040658D+00
        w(7) = 0.26061069640293546231874286942D+00
        w(8) = 0.18064816069485740405847203124D+00
        w(9) = 0.081274388361574411971892158111D+00

      else if ( n .eq. 10 ) then

        x(1) = -0.973906528517171720077964012D+00
        x(2) = -0.865063366688984510732096688D+00
        x(3) = -0.679409568299024406234327365D+00
        x(4) = -0.433395394129247190799265943D+00
        x(5) = -0.148874338981631210884826001D+00
        x(6) = 0.148874338981631210884826001D+00
        x(7) = 0.433395394129247190799265943D+00
        x(8) = 0.679409568299024406234327365D+00
        x(9) = 0.865063366688984510732096688D+00
        x(10) = 0.973906528517171720077964012D+00

        w(1) = 0.066671344308688137593568809893D+00
        w(2) = 0.14945134915058059314577633966D+00
        w(3) = 0.21908636251598204399553493423D+00
        w(4) = 0.26926671930999635509122692157D+00
        w(5) = 0.29552422471475287017389299465D+00
        w(6) = 0.29552422471475287017389299465D+00
        w(7) = 0.26926671930999635509122692157D+00
        w(8) = 0.21908636251598204399553493423D+00
        w(9) = 0.14945134915058059314577633966D+00
        w(10) = 0.066671344308688137593568809893D+00

      else if ( n .eq. 11 ) then

        x(1) = -0.978228658146056992803938001D+00
        x(2) = -0.887062599768095299075157769D+00
        x(3) = -0.730152005574049324093416252D+00
        x(4) = -0.519096129206811815925725669D+00
        x(5) = -0.269543155952344972331531985D+00
        x(6) = 0.000000000000000000000000000D+00
        x(7) = 0.269543155952344972331531985D+00
        x(8) = 0.519096129206811815925725669D+00
        x(9) = 0.730152005574049324093416252D+00
        x(10) = 0.887062599768095299075157769D+00
        x(11) = 0.978228658146056992803938001D+00

        w(1) = 0.055668567116173666482753720443D+00
        w(2) = 0.12558036946490462463469429922D+00
        w(3) = 0.18629021092773425142609764143D+00
        w(4) = 0.23319376459199047991852370484D+00
        w(5) = 0.26280454451024666218068886989D+00
        w(6) = 0.27292508677790063071448352834D+00
        w(7) = 0.26280454451024666218068886989D+00
        w(8) = 0.23319376459199047991852370484D+00
        w(9) = 0.18629021092773425142609764143D+00
        w(10) = 0.12558036946490462463469429922D+00
        w(11) = 0.055668567116173666482753720443D+00

      else if ( n .eq. 12 ) then

        x(1) = -0.981560634246719250690549090D+00
        x(2) = -0.904117256370474856678465866D+00
        x(3) = -0.769902674194304687036893833D+00
        x(4) = -0.587317954286617447296702419D+00
        x(5) = -0.367831498998180193752691537D+00
        x(6) = -0.125233408511468915472441369D+00
        x(7) = 0.125233408511468915472441369D+00
        x(8) = 0.367831498998180193752691537D+00
        x(9) = 0.587317954286617447296702419D+00
        x(10) = 0.769902674194304687036893833D+00
        x(11) = 0.904117256370474856678465866D+00
        x(12) = 0.981560634246719250690549090D+00

        w(1) = 0.047175336386511827194615961485D+00
        w(2) = 0.10693932599531843096025471819D+00
        w(3) = 0.16007832854334622633465252954D+00
        w(4) = 0.20316742672306592174906445581D+00
        w(5) = 0.23349253653835480876084989892D+00
        w(6) = 0.24914704581340278500056243604D+00
        w(7) = 0.24914704581340278500056243604D+00
        w(8) = 0.23349253653835480876084989892D+00
        w(9) = 0.20316742672306592174906445581D+00
        w(10) = 0.16007832854334622633465252954D+00
        w(11) = 0.10693932599531843096025471819D+00
        w(12) = 0.047175336386511827194615961485D+00

      else if ( n .eq. 13 ) then

        x(1) = -0.984183054718588149472829449D+00
        x(2) = -0.917598399222977965206547837D+00
        x(3) = -0.801578090733309912794206490D+00
        x(4) = -0.642349339440340220643984607D+00
        x(5) = -0.448492751036446852877912852D+00
        x(6) = -0.230458315955134794065528121D+00
        x(7) = 0.000000000000000000000000000D+00
        x(8) = 0.230458315955134794065528121D+00
        x(9) = 0.448492751036446852877912852D+00
        x(10) = 0.642349339440340220643984607D+00
        x(11) = 0.80157809073330991279420649D+00
        x(12) = 0.91759839922297796520654784D+00
        x(13) = 0.98418305471858814947282945D+00

        w(1) = 0.040484004765315879520021592201D+00
        w(2) = 0.092121499837728447914421775954D+00
        w(3) = 0.13887351021978723846360177687D+00
        w(4) = 0.17814598076194573828004669200D+00
        w(5) = 0.20781604753688850231252321931D+00
        w(6) = 0.22628318026289723841209018604D+00
        w(7) = 0.23255155323087391019458951527D+00
        w(8) = 0.22628318026289723841209018604D+00
        w(9) = 0.20781604753688850231252321931D+00
        w(10) = 0.17814598076194573828004669200D+00
        w(11) = 0.13887351021978723846360177687D+00
        w(12) = 0.092121499837728447914421775954D+00
        w(13) = 0.040484004765315879520021592201D+00

      else if ( n .eq. 14 ) then

        x(1) = -0.986283808696812338841597267D+00
        x(2) = -0.928434883663573517336391139D+00
        x(3) = -0.827201315069764993189794743D+00
        x(4) = -0.687292904811685470148019803D+00
        x(5) = -0.515248636358154091965290719D+00
        x(6) = -0.319112368927889760435671824D+00
        x(7) = -0.108054948707343662066244650D+00
        x(8) = 0.108054948707343662066244650D+00
        x(9) = 0.31911236892788976043567182D+00
        x(10) = 0.51524863635815409196529072D+00
        x(11) = 0.68729290481168547014801980D+00
        x(12) = 0.82720131506976499318979474D+00
        x(13) = 0.92843488366357351733639114D+00
        x(14) = 0.98628380869681233884159727D+00

        w(1) = 0.035119460331751863031832876138D+00
        w(2) = 0.08015808715976020980563327706D+00
        w(3) = 0.12151857068790318468941480907D+00
        w(4) = 0.15720316715819353456960193862D+00
        w(5) = 0.18553839747793781374171659013D+00
        w(6) = 0.20519846372129560396592406566D+00
        w(7) = 0.21526385346315779019587644332D+00
        w(8) = 0.21526385346315779019587644332D+00
        w(9) = 0.20519846372129560396592406566D+00
        w(10) = 0.18553839747793781374171659013D+00
        w(11) = 0.15720316715819353456960193862D+00
        w(12) = 0.12151857068790318468941480907D+00
        w(13) = 0.08015808715976020980563327706D+00
        w(14) = 0.035119460331751863031832876138D+00

      else if ( n .eq. 15 ) then

        x(1) = -0.987992518020485428489565719D+00
        x(2) = -0.937273392400705904307758948D+00
        x(3) = -0.848206583410427216200648321D+00
        x(4) = -0.724417731360170047416186055D+00
        x(5) = -0.570972172608538847537226737D+00
        x(6) = -0.394151347077563369897207371D+00
        x(7) = -0.201194093997434522300628303D+00
        x(8) = 0.00000000000000000000000000D+00
        x(9) = 0.20119409399743452230062830D+00
        x(10) = 0.39415134707756336989720737D+00
        x(11) = 0.57097217260853884753722674D+00
        x(12) = 0.72441773136017004741618605D+00
        x(13) = 0.84820658341042721620064832D+00
        x(14) = 0.93727339240070590430775895D+00
        x(15) = 0.98799251802048542848956572D+00

        w(1) = 0.030753241996117268354628393577D+00
        w(2) = 0.070366047488108124709267416451D+00
        w(3) = 0.107159220467171935011869546686D+00
        w(4) = 0.13957067792615431444780479451D+00
        w(5) = 0.16626920581699393355320086048D+00
        w(6) = 0.18616100001556221102680056187D+00
        w(7) = 0.19843148532711157645611832644D+00
        w(8) = 0.20257824192556127288062019997D+00
        w(9) = 0.19843148532711157645611832644D+00
        w(10) = 0.18616100001556221102680056187D+00
        w(11) = 0.16626920581699393355320086048D+00
        w(12) = 0.13957067792615431444780479451D+00
        w(13) = 0.107159220467171935011869546686D+00
        w(14) = 0.070366047488108124709267416451D+00
        w(15) = 0.030753241996117268354628393577D+00

      else if ( n .eq. 16 ) then

        x(1) = -0.989400934991649932596154173D+00
        x(2) = -0.944575023073232576077988416D+00
        x(3) = -0.865631202387831743880467898D+00
        x(4) = -0.755404408355003033895101195D+00
        x(5) = -0.617876244402643748446671764D+00
        x(6) = -0.458016777657227386342419443D+00
        x(7) = -0.281603550779258913230460501D+00
        x(8) = -0.09501250983763744018531934D+00
        x(9) = 0.09501250983763744018531934D+00
        x(10) = 0.28160355077925891323046050D+00
        x(11) = 0.45801677765722738634241944D+00
        x(12) = 0.61787624440264374844667176D+00
        x(13) = 0.75540440835500303389510119D+00
        x(14) = 0.86563120238783174388046790D+00
        x(15) = 0.94457502307323257607798842D+00
        x(16) = 0.98940093499164993259615417D+00

        w(1) = 0.027152459411754094851780572456D+00
        w(2) = 0.062253523938647892862843836994D+00
        w(3) = 0.09515851168249278480992510760D+00
        w(4) = 0.12462897125553387205247628219D+00
        w(5) = 0.14959598881657673208150173055D+00
        w(6) = 0.16915651939500253818931207903D+00
        w(7) = 0.18260341504492358886676366797D+00
        w(8) = 0.18945061045506849628539672321D+00
        w(9) = 0.18945061045506849628539672321D+00
        w(10) = 0.18260341504492358886676366797D+00
        w(11) = 0.16915651939500253818931207903D+00
        w(12) = 0.14959598881657673208150173055D+00
        w(13) = 0.12462897125553387205247628219D+00
        w(14) = 0.09515851168249278480992510760D+00
        w(15) = 0.062253523938647892862843836994D+00
        w(16) = 0.027152459411754094851780572456D+00

      else if ( n .eq. 17 ) then

        x(1) = -0.990575475314417335675434020D+00
        x(2) = -0.950675521768767761222716958D+00
        x(3) = -0.880239153726985902122955694D+00
        x(4) = -0.781514003896801406925230056D+00
        x(5) = -0.657671159216690765850302217D+00
        x(6) = -0.512690537086476967886246569D+00
        x(7) = -0.35123176345387631529718552D+00
        x(8) = -0.17848418149584785585067749D+00
        x(9) = 0.00000000000000000000000000D+00
        x(10) = 0.17848418149584785585067749D+00
        x(11) = 0.35123176345387631529718552D+00
        x(12) = 0.51269053708647696788624657D+00
        x(13) = 0.65767115921669076585030222D+00
        x(14) = 0.78151400389680140692523006D+00
        x(15) = 0.88023915372698590212295569D+00
        x(16) = 0.95067552176876776122271696D+00
        x(17) = 0.99057547531441733567543402D+00

        w(1) = 0.024148302868547931960110026288D+00
        w(2) = 0.055459529373987201129440165359D+00
        w(3) = 0.085036148317179180883535370191D+00
        w(4) = 0.111883847193403971094788385626D+00
        w(5) = 0.13513636846852547328631998170D+00
        w(6) = 0.15404576107681028808143159480D+00
        w(7) = 0.16800410215645004450997066379D+00
        w(8) = 0.17656270536699264632527099011D+00
        w(9) = 0.17944647035620652545826564426D+00
        w(10) = 0.17656270536699264632527099011D+00
        w(11) = 0.16800410215645004450997066379D+00
        w(12) = 0.15404576107681028808143159480D+00
        w(13) = 0.13513636846852547328631998170D+00
        w(14) = 0.111883847193403971094788385626D+00
        w(15) = 0.085036148317179180883535370191D+00
        w(16) = 0.055459529373987201129440165359D+00
        w(17) = 0.024148302868547931960110026288D+00

      else if ( n .eq. 18 ) then

        x(1) = -0.991565168420930946730016005D+00
        x(2) = -0.955823949571397755181195893D+00
        x(3) = -0.892602466497555739206060591D+00
        x(4) = -0.803704958972523115682417455D+00
        x(5) = -0.691687043060353207874891081D+00
        x(6) = -0.55977083107394753460787155D+00
        x(7) = -0.41175116146284264603593179D+00
        x(8) = -0.25188622569150550958897285D+00
        x(9) = -0.08477501304173530124226185D+00
        x(10) = 0.08477501304173530124226185D+00
        x(11) = 0.25188622569150550958897285D+00
        x(12) = 0.41175116146284264603593179D+00
        x(13) = 0.55977083107394753460787155D+00
        x(14) = 0.69168704306035320787489108D+00
        x(15) = 0.80370495897252311568241746D+00
        x(16) = 0.89260246649755573920606059D+00
        x(17) = 0.95582394957139775518119589D+00
        x(18) = 0.99156516842093094673001600D+00

        w(1) = 0.021616013526483310313342710266D+00
        w(2) = 0.049714548894969796453334946203D+00
        w(3) = 0.07642573025488905652912967762D+00
        w(4) = 0.10094204410628716556281398492D+00
        w(5) = 0.12255520671147846018451912680D+00
        w(6) = 0.14064291467065065120473130375D+00
        w(7) = 0.15468467512626524492541800384D+00
        w(8) = 0.16427648374583272298605377647D+00
        w(9) = 0.16914238296314359184065647013D+00
        w(10) = 0.16914238296314359184065647013D+00
        w(11) = 0.16427648374583272298605377647D+00
        w(12) = 0.15468467512626524492541800384D+00
        w(13) = 0.14064291467065065120473130375D+00
        w(14) = 0.12255520671147846018451912680D+00
        w(15) = 0.10094204410628716556281398492D+00
        w(16) = 0.07642573025488905652912967762D+00
        w(17) = 0.049714548894969796453334946203D+00
        w(18) = 0.021616013526483310313342710266D+00

      else if ( n .eq. 19 ) then

        x(1) = -0.992406843843584403189017670D+00
        x(2) = -0.960208152134830030852778841D+00
        x(3) = -0.903155903614817901642660929D+00
        x(4) = -0.822714656537142824978922487D+00
        x(5) = -0.72096617733522937861709586D+00
        x(6) = -0.60054530466168102346963816D+00
        x(7) = -0.46457074137596094571726715D+00
        x(8) = -0.31656409996362983199011733D+00
        x(9) = -0.16035864564022537586809612D+00
        x(10) = 0.00000000000000000000000000D+00
        x(11) = 0.16035864564022537586809612D+00
        x(12) = 0.31656409996362983199011733D+00
        x(13) = 0.46457074137596094571726715D+00
        x(14) = 0.60054530466168102346963816D+00
        x(15) = 0.72096617733522937861709586D+00
        x(16) = 0.82271465653714282497892249D+00
        x(17) = 0.90315590361481790164266093D+00
        x(18) = 0.96020815213483003085277884D+00
        x(19) = 0.99240684384358440318901767D+00

        w(1) = 0.019461788229726477036312041464D+00
        w(2) = 0.044814226765699600332838157402D+00
        w(3) = 0.069044542737641226580708258006D+00
        w(4) = 0.091490021622449999464462094124D+00
        w(5) = 0.111566645547333994716023901682D+00
        w(6) = 0.12875396253933622767551578486D+00
        w(7) = 0.14260670217360661177574610944D+00
        w(8) = 0.15276604206585966677885540090D+00
        w(9) = 0.15896884339395434764995643946D+00
        w(10) = 0.16105444984878369597916362532D+00
        w(11) = 0.15896884339395434764995643946D+00
        w(12) = 0.15276604206585966677885540090D+00
        w(13) = 0.14260670217360661177574610944D+00
        w(14) = 0.12875396253933622767551578486D+00
        w(15) = 0.111566645547333994716023901682D+00
        w(16) = 0.091490021622449999464462094124D+00
        w(17) = 0.069044542737641226580708258006D+00
        w(18) = 0.044814226765699600332838157402D+00
        w(19) = 0.019461788229726477036312041464D+00

      else if ( n .eq. 20 ) then

        x(1) = -0.993128599185094924786122388D+00
        x(2) = -0.963971927277913791267666131D+00
        x(3) = -0.912234428251325905867752441D+00
        x(4) = -0.83911697182221882339452906D+00
        x(5) = -0.74633190646015079261430507D+00
        x(6) = -0.63605368072651502545283670D+00
        x(7) = -0.51086700195082709800436405D+00
        x(8) = -0.37370608871541956067254818D+00
        x(9) = -0.22778585114164507808049620D+00
        x(10) = -0.07652652113349733375464041D+00
        x(11) = 0.07652652113349733375464041D+00
        x(12) = 0.22778585114164507808049620D+00
        x(13) = 0.37370608871541956067254818D+00
        x(14) = 0.51086700195082709800436405D+00
        x(15) = 0.63605368072651502545283670D+00
        x(16) = 0.74633190646015079261430507D+00
        x(17) = 0.83911697182221882339452906D+00
        x(18) = 0.91223442825132590586775244D+00
        x(19) = 0.96397192727791379126766613D+00
        x(20) = 0.99312859918509492478612239D+00

        w(1) = 0.017614007139152118311861962352D+00
        w(2) = 0.040601429800386941331039952275D+00
        w(3) = 0.062672048334109063569506535187D+00
        w(4) = 0.08327674157670474872475814322D+00
        w(5) = 0.10193011981724043503675013548D+00
        w(6) = 0.11819453196151841731237737771D+00
        w(7) = 0.13168863844917662689849449975D+00
        w(8) = 0.14209610931838205132929832507D+00
        w(9) = 0.14917298647260374678782873700D+00
        w(10) = 0.15275338713072585069808433195D+00
        w(11) = 0.15275338713072585069808433195D+00
        w(12) = 0.14917298647260374678782873700D+00
        w(13) = 0.14209610931838205132929832507D+00
        w(14) = 0.13168863844917662689849449975D+00
        w(15) = 0.11819453196151841731237737771D+00
        w(16) = 0.10193011981724043503675013548D+00
        w(17) = 0.08327674157670474872475814322D+00
        w(18) = 0.062672048334109063569506535187D+00
        w(19) = 0.040601429800386941331039952275D+00
        w(20) = 0.017614007139152118311861962352D+00

      else if ( n .eq. 21 ) then

        x( 1) =  -0.99375217062038950026024204D+00
        x( 2) =  -0.96722683856630629431662221D+00
        x( 3) =  -0.92009933415040082879018713D+00
        x( 4) =  -0.85336336458331728364725064D+00
        x( 5) =  -0.76843996347567790861587785D+00
        x( 6) =  -0.66713880419741231930596667D+00
        x( 7) =  -0.55161883588721980705901880D+00
        x( 8) =  -0.42434212020743878357366889D+00
        x( 9) =  -0.28802131680240109660079252D+00
        x(10) =  -0.14556185416089509093703098D+00
        x(11) =   0.00000000000000000000000000D+00
        x(12) =  +0.14556185416089509093703098D+00
        x(13) =  +0.28802131680240109660079252D+00
        x(14) =  +0.42434212020743878357366889D+00
        x(15) =  +0.55161883588721980705901880D+00
        x(16) =  +0.66713880419741231930596667D+00
        x(17) =  +0.76843996347567790861587785D+00
        x(18) =  +0.85336336458331728364725064D+00
        x(19) =  +0.92009933415040082879018713D+00
        x(20) =  +0.96722683856630629431662221D+00
        x(21) =  +0.99375217062038950026024204D+00

        w( 1) =   0.016017228257774333324224616858D+00
        w( 2) =   0.036953789770852493799950668299D+00
        w( 3) =   0.057134425426857208283635826472D+00
        w( 4) =   0.076100113628379302017051653300D+00
        w( 5) =   0.093444423456033861553289741114D+00
        w( 6) =   0.108797299167148377663474578070D+00
        w( 7) =   0.12183141605372853419536717713D+00
        w( 8) =   0.13226893863333746178105257450D+00
        w( 9) =   0.13988739479107315472213342387D+00
        w(10) =   0.14452440398997005906382716655D+00
        w(11) =   0.14608113364969042719198514768D+00
        w(12) =   0.14452440398997005906382716655D+00
        w(13) =   0.13988739479107315472213342387D+00
        w(14) =   0.13226893863333746178105257450D+00
        w(15) =   0.12183141605372853419536717713D+00
        w(16) =   0.108797299167148377663474578070D+00
        w(17) =   0.093444423456033861553289741114D+00
        w(18) =   0.076100113628379302017051653300D+00
        w(19) =   0.057134425426857208283635826472D+00
        w(20) =   0.036953789770852493799950668299D+00
        w(21) =   0.016017228257774333324224616858D+00

      else if ( n .eq. 22 ) then

        x(1) = -0.99429458548239929207303142D+00
        x(2) = -0.97006049783542872712395099D+00
        x(3) = -0.92695677218717400052069294D+00
        x(4) = -0.86581257772030013653642564D+00
        x(5) = -0.78781680597920816200427796D+00
        x(6) = -0.69448726318668278005068984D+00
        x(7) = -0.58764040350691159295887693D+00
        x(8) = -0.46935583798675702640633071D+00
        x(9) = -0.34193582089208422515814742D+00
        x(10) = -0.20786042668822128547884653D+00
        x(11) = -0.06973927331972222121384180D+00
        x(12) = 0.06973927331972222121384180D+00
        x(13) = 0.20786042668822128547884653D+00
        x(14) = 0.34193582089208422515814742D+00
        x(15) = 0.46935583798675702640633071D+00
        x(16) = 0.58764040350691159295887693D+00
        x(17) = 0.69448726318668278005068984D+00
        x(18) = 0.78781680597920816200427796D+00
        x(19) = 0.86581257772030013653642564D+00
        x(20) = 0.92695677218717400052069294D+00
        x(21) = 0.97006049783542872712395099D+00
        x(22) = 0.99429458548239929207303142D+00

        w(1) = 0.014627995298272200684991098047D+00
        w(2) = 0.033774901584814154793302246866D+00
        w(3) = 0.052293335152683285940312051273D+00
        w(4) = 0.06979646842452048809496141893D+00
        w(5) = 0.08594160621706772741444368137D+00
        w(6) = 0.10041414444288096493207883783D+00
        w(7) = 0.11293229608053921839340060742D+00
        w(8) = 0.12325237681051242428556098615D+00
        w(9) = 0.13117350478706237073296499253D+00
        w(10) = 0.13654149834601517135257383123D+00
        w(11) = 0.13925187285563199337541024834D+00
        w(12) = 0.13925187285563199337541024834D+00
        w(13) = 0.13654149834601517135257383123D+00
        w(14) = 0.13117350478706237073296499253D+00
        w(15) = 0.12325237681051242428556098615D+00
        w(16) = 0.11293229608053921839340060742D+00
        w(17) = 0.10041414444288096493207883783D+00
        w(18) = 0.08594160621706772741444368137D+00
        w(19) = 0.06979646842452048809496141893D+00
        w(20) = 0.052293335152683285940312051273D+00
        w(21) = 0.033774901584814154793302246866D+00
        w(22) = 0.014627995298272200684991098047D+00

      else if ( n .eq. 23 ) then

        x(1) = -0.99476933499755212352392572D+00
        x(2) = -0.97254247121811523195602408D+00
        x(3) = -0.93297108682601610234919699D+00
        x(4) = -0.87675235827044166737815689D+00
        x(5) = -0.80488840161883989215111841D+00
        x(6) = -0.71866136313195019446162448D+00
        x(7) = -0.61960987576364615638509731D+00
        x(8) = -0.50950147784600754968979305D+00
        x(9) = -0.39030103803029083142148887D+00
        x(10) = -0.26413568097034493053386954D+00
        x(11) = -0.13325682429846611093174268D+00
        x(12) = 0.00000000000000000000000000D+00
        x(13) = 0.13325682429846611093174268D+00
        x(14) = 0.26413568097034493053386954D+00
        x(15) = 0.39030103803029083142148887D+00
        x(16) = 0.50950147784600754968979305D+00
        x(17) = 0.61960987576364615638509731D+00
        x(18) = 0.71866136313195019446162448D+00
        x(19) = 0.80488840161883989215111841D+00
        x(20) = 0.87675235827044166737815689D+00
        x(21) = 0.93297108682601610234919699D+00
        x(22) = 0.97254247121811523195602408D+00
        x(23) = 0.99476933499755212352392572D+00

        w(1) = 0.013411859487141772081309493459D+00
        w(2) = 0.030988005856979444310694219642D+00
        w(3) = 0.048037671731084668571641071632D+00
        w(4) = 0.064232421408525852127169615159D+00
        w(5) = 0.079281411776718954922892524742D+00
        w(6) = 0.092915766060035147477018617370D+00
        w(7) = 0.104892091464541410074086185015D+00
        w(8) = 0.11499664022241136494164351293D+00
        w(9) = 0.12304908430672953046757840067D+00
        w(10) = 0.12890572218808214997859533940D+00
        w(11) = 0.13246203940469661737164246470D+00
        w(12) = 0.13365457218610617535145711055D+00
        w(13) = 0.13246203940469661737164246470D+00
        w(14) = 0.12890572218808214997859533940D+00
        w(15) = 0.12304908430672953046757840067D+00
        w(16) = 0.11499664022241136494164351293D+00
        w(17) = 0.104892091464541410074086185015D+00
        w(18) = 0.092915766060035147477018617370D+00
        w(19) = 0.079281411776718954922892524742D+00
        w(20) = 0.064232421408525852127169615159D+00
        w(21) = 0.048037671731084668571641071632D+00
        w(22) = 0.030988005856979444310694219642D+00
        w(23) = 0.013411859487141772081309493459D+00

      else if ( n .eq. 24 ) then

        x(1) = -0.99518721999702136017999741D+00
        x(2) = -0.97472855597130949819839199D+00
        x(3) = -0.93827455200273275852364900D+00
        x(4) = -0.88641552700440103421315434D+00
        x(5) = -0.82000198597390292195394987D+00
        x(6) = -0.74012419157855436424382810D+00
        x(7) = -0.64809365193697556925249579D+00
        x(8) = -0.54542147138883953565837562D+00
        x(9) = -0.43379350762604513848708423D+00
        x(10) = -0.31504267969616337438679329D+00
        x(11) = -0.19111886747361630915863982D+00
        x(12) = -0.06405689286260562608504308D+00
        x(13) = 0.06405689286260562608504308D+00
        x(14) = 0.19111886747361630915863982D+00
        x(15) = 0.31504267969616337438679329D+00
        x(16) = 0.43379350762604513848708423D+00
        x(17) = 0.54542147138883953565837562D+00
        x(18) = 0.64809365193697556925249579D+00
        x(19) = 0.74012419157855436424382810D+00
        x(20) = 0.82000198597390292195394987D+00
        x(21) = 0.88641552700440103421315434D+00
        x(22) = 0.93827455200273275852364900D+00
        x(23) = 0.97472855597130949819839199D+00
        x(24) = 0.99518721999702136017999741D+00

        w(1) = 0.012341229799987199546805667070D+00
        w(2) = 0.028531388628933663181307815952D+00
        w(3) = 0.044277438817419806168602748211D+00
        w(4) = 0.059298584915436780746367758500D+00
        w(5) = 0.07334648141108030573403361525D+00
        w(6) = 0.08619016153195327591718520298D+00
        w(7) = 0.09761865210411388826988066446D+00
        w(8) = 0.10744427011596563478257734245D+00
        w(9) = 0.11550566805372560135334448391D+00
        w(10) = 0.12167047292780339120446315348D+00
        w(11) = 0.12583745634682829612137538251D+00
        w(12) = 0.12793819534675215697405616522D+00
        w(13) = 0.12793819534675215697405616522D+00
        w(14) = 0.12583745634682829612137538251D+00
        w(15) = 0.12167047292780339120446315348D+00
        w(16) = 0.11550566805372560135334448391D+00
        w(17) = 0.10744427011596563478257734245D+00
        w(18) = 0.09761865210411388826988066446D+00
        w(19) = 0.08619016153195327591718520298D+00
        w(20) = 0.07334648141108030573403361525D+00
        w(21) = 0.059298584915436780746367758500D+00
        w(22) = 0.044277438817419806168602748211D+00
        w(23) = 0.028531388628933663181307815952D+00
        w(24) = 0.012341229799987199546805667070D+00

      else if ( n .eq. 25 ) then

        x(1) = -0.99555696979049809790878495D+00
        x(2) = -0.97666392145951751149831539D+00
        x(3) = -0.94297457122897433941401117D+00
        x(4) = -0.89499199787827536885104201D+00
        x(5) = -0.83344262876083400142102111D+00
        x(6) = -0.75925926303735763057728287D+00
        x(7) = -0.67356636847346836448512063D+00
        x(8) = -0.57766293024122296772368984D+00
        x(9) = -0.47300273144571496052218212D+00
        x(10) = -0.36117230580938783773582173D+00
        x(11) = -0.24386688372098843204519036D+00
        x(12) = -0.12286469261071039638735982D+00
        x(13) = 0.00000000000000000000000000D+00
        x(14) = 0.12286469261071039638735982D+00
        x(15) = 0.24386688372098843204519036D+00
        x(16) = 0.36117230580938783773582173D+00
        x(17) = 0.47300273144571496052218212D+00
        x(18) = 0.57766293024122296772368984D+00
        x(19) = 0.67356636847346836448512063D+00
        x(20) = 0.75925926303735763057728287D+00
        x(21) = 0.83344262876083400142102111D+00
        x(22) = 0.89499199787827536885104201D+00
        x(23) = 0.94297457122897433941401117D+00
        x(24) = 0.97666392145951751149831539D+00
        x(25) = 0.99555696979049809790878495D+00

        w(1) = 0.0113937985010262879479029641132D+00
        w(2) = 0.026354986615032137261901815295D+00
        w(3) = 0.040939156701306312655623487712D+00
        w(4) = 0.054904695975835191925936891541D+00
        w(5) = 0.068038333812356917207187185657D+00
        w(6) = 0.080140700335001018013234959669D+00
        w(7) = 0.091028261982963649811497220703D+00
        w(8) = 0.100535949067050644202206890393D+00
        w(9) = 0.108519624474263653116093957050D+00
        w(10) = 0.11485825914571164833932554587D+00
        w(11) = 0.11945576353578477222817812651D+00
        w(12) = 0.12224244299031004168895951895D+00
        w(13) = 0.12317605372671545120390287308D+00
        w(14) = 0.12224244299031004168895951895D+00
        w(15) = 0.11945576353578477222817812651D+00
        w(16) = 0.11485825914571164833932554587D+00
        w(17) = 0.108519624474263653116093957050D+00
        w(18) = 0.100535949067050644202206890393D+00
        w(19) = 0.091028261982963649811497220703D+00
        w(20) = 0.080140700335001018013234959669D+00
        w(21) = 0.068038333812356917207187185657D+00
        w(22) = 0.054904695975835191925936891541D+00
        w(23) = 0.040939156701306312655623487712D+00
        w(24) = 0.026354986615032137261901815295D+00
        w(25) = 0.0113937985010262879479029641132D+00

      else if ( n .eq. 26 ) then

        x(1) = -0.99588570114561692900321696D+00
        x(2) = -0.97838544595647099110058035D+00
        x(3) = -0.94715906666171425013591528D+00
        x(4) = -0.90263786198430707421766560D+00
        x(5) = -0.84544594278849801879750706D+00
        x(6) = -0.77638594882067885619296725D+00
        x(7) = -0.69642726041995726486381391D+00
        x(8) = -0.60669229301761806323197875D+00
        x(9) = -0.50844071482450571769570306D+00
        x(10) = -0.40305175512348630648107738D+00
        x(11) = -0.29200483948595689514283538D+00
        x(12) = -0.17685882035689018396905775D+00
        x(13) = -0.05923009342931320709371858D+00
        x(14) = 0.05923009342931320709371858D+00
        x(15) = 0.17685882035689018396905775D+00
        x(16) = 0.29200483948595689514283538D+00
        x(17) = 0.40305175512348630648107738D+00
        x(18) = 0.50844071482450571769570306D+00
        x(19) = 0.60669229301761806323197875D+00
        x(20) = 0.69642726041995726486381391D+00
        x(21) = 0.77638594882067885619296725D+00
        x(22) = 0.84544594278849801879750706D+00
        x(23) = 0.90263786198430707421766560D+00
        x(24) = 0.94715906666171425013591528D+00
        x(25) = 0.97838544595647099110058035D+00
        x(26) = 0.99588570114561692900321696D+00

        w(1) = 0.010551372617343007155651187685D+00
        w(2) = 0.024417851092631908789615827520D+00
        w(3) = 0.037962383294362763950303141249D+00
        w(4) = 0.050975825297147811998319900724D+00
        w(5) = 0.063274046329574835539453689907D+00
        w(6) = 0.07468414976565974588707579610D+00
        w(7) = 0.08504589431348523921044776508D+00
        w(8) = 0.09421380035591414846366488307D+00
        w(9) = 0.10205916109442542323841407025D+00
        w(10) = 0.10847184052857659065657942673D+00
        w(11) = 0.11336181654631966654944071844D+00
        w(12) = 0.11666044348529658204466250754D+00
        w(13) = 0.11832141527926227651637108570D+00
        w(14) = 0.11832141527926227651637108570D+00
        w(15) = 0.11666044348529658204466250754D+00
        w(16) = 0.11336181654631966654944071844D+00
        w(17) = 0.10847184052857659065657942673D+00
        w(18) = 0.10205916109442542323841407025D+00
        w(19) = 0.09421380035591414846366488307D+00
        w(20) = 0.08504589431348523921044776508D+00
        w(21) = 0.07468414976565974588707579610D+00
        w(22) = 0.063274046329574835539453689907D+00
        w(23) = 0.050975825297147811998319900724D+00
        w(24) = 0.037962383294362763950303141249D+00
        w(25) = 0.024417851092631908789615827520D+00
        w(26) = 0.010551372617343007155651187685D+00

      else if ( n .eq. 27 ) then

        x(1) = -0.99617926288898856693888721D+00
        x(2) = -0.97992347596150122285587336D+00
        x(3) = -0.95090055781470500685190803D+00
        x(4) = -0.90948232067749110430064502D+00
        x(5) = -0.85620790801829449030273722D+00
        x(6) = -0.79177163907050822714439734D+00
        x(7) = -0.71701347373942369929481621D+00
        x(8) = -0.63290797194649514092773464D+00
        x(9) = -0.54055156457945689490030094D+00
        x(10) = -0.44114825175002688058597416D+00
        x(11) = -0.33599390363850889973031903D+00
        x(12) = -0.22645936543953685885723911D+00
        x(13) = -0.11397258560952996693289498D+00
        x(14) = 0.00000000000000000000000000D+00
        x(15) = 0.11397258560952996693289498D+00
        x(16) = 0.22645936543953685885723911D+00
        x(17) = 0.33599390363850889973031903D+00
        x(18) = 0.44114825175002688058597416D+00
        x(19) = 0.54055156457945689490030094D+00
        x(20) = 0.63290797194649514092773464D+00
        x(21) = 0.71701347373942369929481621D+00
        x(22) = 0.79177163907050822714439734D+00
        x(23) = 0.85620790801829449030273722D+00
        x(24) = 0.90948232067749110430064502D+00
        x(25) = 0.95090055781470500685190803D+00
        x(26) = 0.97992347596150122285587336D+00
        x(27) = 0.99617926288898856693888721D+00

        w(1) = 0.0097989960512943602611500550912D+00
        w(2) = 0.022686231596180623196034206447D+00
        w(3) = 0.035297053757419711022578289305D+00
        w(4) = 0.047449412520615062704096710114D+00
        w(5) = 0.058983536859833599110300833720D+00
        w(6) = 0.069748823766245592984322888357D+00
        w(7) = 0.079604867773057771263074959010D+00
        w(8) = 0.088423158543756950194322802854D+00
        w(9) = 0.096088727370028507565652646558D+00
        w(10) = 0.102501637817745798671247711533D+00
        w(11) = 0.107578285788533187212162984427D+00
        w(12) = 0.111252488356845192672163096043D+00
        w(13) = 0.113476346108965148620369948092D+00
        w(14) = 0.11422086737895698904504573690D+00
        w(15) = 0.113476346108965148620369948092D+00
        w(16) = 0.111252488356845192672163096043D+00
        w(17) = 0.107578285788533187212162984427D+00
        w(18) = 0.102501637817745798671247711533D+00
        w(19) = 0.096088727370028507565652646558D+00
        w(20) = 0.088423158543756950194322802854D+00
        w(21) = 0.079604867773057771263074959010D+00
        w(22) = 0.069748823766245592984322888357D+00
        w(23) = 0.058983536859833599110300833720D+00
        w(24) = 0.047449412520615062704096710114D+00
        w(25) = 0.035297053757419711022578289305D+00
        w(26) = 0.022686231596180623196034206447D+00
        w(27) = 0.0097989960512943602611500550912D+00

      else if ( n .eq. 28 ) then

        x(1) = -0.99644249757395444995043639D+00
        x(2) = -0.98130316537087275369455995D+00
        x(3) = -0.95425928062893819725410184D+00
        x(4) = -0.91563302639213207386968942D+00
        x(5) = -0.86589252257439504894225457D+00
        x(6) = -0.80564137091717917144788596D+00
        x(7) = -0.73561087801363177202814451D+00
        x(8) = -0.65665109403886496121989818D+00
        x(9) = -0.56972047181140171930800328D+00
        x(10) = -0.47587422495511826103441185D+00
        x(11) = -0.37625151608907871022135721D+00
        x(12) = -0.27206162763517807767682636D+00
        x(13) = -0.16456928213338077128147178D+00
        x(14) = -0.05507928988403427042651653D+00
        x(15) = 0.05507928988403427042651653D+00
        x(16) = 0.16456928213338077128147178D+00
        x(17) = 0.27206162763517807767682636D+00
        x(18) = 0.37625151608907871022135721D+00
        x(19) = 0.47587422495511826103441185D+00
        x(20) = 0.56972047181140171930800328D+00
        x(21) = 0.65665109403886496121989818D+00
        x(22) = 0.73561087801363177202814451D+00
        x(23) = 0.80564137091717917144788596D+00
        x(24) = 0.86589252257439504894225457D+00
        x(25) = 0.91563302639213207386968942D+00
        x(26) = 0.95425928062893819725410184D+00
        x(27) = 0.98130316537087275369455995D+00
        x(28) = 0.99644249757395444995043639D+00

        w(1) = 0.009124282593094517738816153923D+00
        w(2) = 0.021132112592771259751500380993D+00
        w(3) = 0.032901427782304379977630819171D+00
        w(4) = 0.044272934759004227839587877653D+00
        w(5) = 0.055107345675716745431482918227D+00
        w(6) = 0.06527292396699959579339756678D+00
        w(7) = 0.07464621423456877902393188717D+00
        w(8) = 0.08311341722890121839039649824D+00
        w(9) = 0.09057174439303284094218603134D+00
        w(10) = 0.09693065799792991585048900610D+00
        w(11) = 0.10211296757806076981421663851D+00
        w(12) = 0.10605576592284641791041643700D+00
        w(13) = 0.10871119225829413525357151930D+00
        w(14) = 0.11004701301647519628237626560D+00
        w(15) = 0.11004701301647519628237626560D+00
        w(16) = 0.10871119225829413525357151930D+00
        w(17) = 0.10605576592284641791041643700D+00
        w(18) = 0.10211296757806076981421663851D+00
        w(19) = 0.09693065799792991585048900610D+00
        w(20) = 0.09057174439303284094218603134D+00
        w(21) = 0.08311341722890121839039649824D+00
        w(22) = 0.07464621423456877902393188717D+00
        w(23) = 0.06527292396699959579339756678D+00
        w(24) = 0.055107345675716745431482918227D+00
        w(25) = 0.044272934759004227839587877653D+00
        w(26) = 0.032901427782304379977630819171D+00
        w(27) = 0.021132112592771259751500380993D+00
        w(28) = 0.009124282593094517738816153923D+00

      else if ( n .eq. 29 ) then

        x(1) = -0.99667944226059658616319153D+00
        x(2) = -0.98254550526141317487092602D+00
        x(3) = -0.95728559577808772579820804D+00
        x(4) = -0.92118023295305878509375344D+00
        x(5) = -0.87463780492010279041779342D+00
        x(6) = -0.81818548761525244498957221D+00
        x(7) = -0.75246285173447713391261008D+00
        x(8) = -0.67821453760268651515618501D+00
        x(9) = -0.59628179713822782037958621D+00
        x(10) = -0.50759295512422764210262792D+00
        x(11) = -0.41315288817400866389070659D+00
        x(12) = -0.31403163786763993494819592D+00
        x(13) = -0.21135228616600107450637573D+00
        x(14) = -0.10627823013267923017098239D+00
        x(15) = 0.00000000000000000000000000D+00
        x(16) = 0.10627823013267923017098239D+00
        x(17) = 0.21135228616600107450637573D+00
        x(18) = 0.31403163786763993494819592D+00
        x(19) = 0.41315288817400866389070659D+00
        x(20) = 0.50759295512422764210262792D+00
        x(21) = 0.59628179713822782037958621D+00
        x(22) = 0.67821453760268651515618501D+00
        x(23) = 0.75246285173447713391261008D+00
        x(24) = 0.81818548761525244498957221D+00
        x(25) = 0.87463780492010279041779342D+00
        x(26) = 0.92118023295305878509375344D+00
        x(27) = 0.95728559577808772579820804D+00
        x(28) = 0.98254550526141317487092602D+00
        x(29) = 0.99667944226059658616319153D+00

        w(1) = 0.0085169038787464096542638133022D+00
        w(2) = 0.019732085056122705983859801640D+00
        w(3) = 0.030740492202093622644408525375D+00
        w(4) = 0.041402062518682836104830010114D+00
        w(5) = 0.051594826902497923912594381180D+00
        w(6) = 0.061203090657079138542109848024D+00
        w(7) = 0.070117933255051278569581486949D+00
        w(8) = 0.078238327135763783828144888660D+00
        w(9) = 0.085472257366172527545344849297D+00
        w(10) = 0.091737757139258763347966411077D+00
        w(11) = 0.096963834094408606301900074883D+00
        w(12) = 0.101091273759914966121820546907D+00
        w(13) = 0.104073310077729373913328471285D+00
        w(14) = 0.105876155097320941406591327852D+00
        w(15) = 0.10647938171831424424651112691D+00
        w(16) = 0.105876155097320941406591327852D+00
        w(17) = 0.104073310077729373913328471285D+00
        w(18) = 0.101091273759914966121820546907D+00
        w(19) = 0.096963834094408606301900074883D+00
        w(20) = 0.091737757139258763347966411077D+00
        w(21) = 0.085472257366172527545344849297D+00
        w(22) = 0.078238327135763783828144888660D+00
        w(23) = 0.070117933255051278569581486949D+00
        w(24) = 0.061203090657079138542109848024D+00
        w(25) = 0.051594826902497923912594381180D+00
        w(26) = 0.041402062518682836104830010114D+00
        w(27) = 0.030740492202093622644408525375D+00
        w(28) = 0.019732085056122705983859801640D+00
        w(29) = 0.0085169038787464096542638133022D+00

      else if ( n .eq. 30 ) then

        x(1) = -0.99689348407464954027163005D+00
        x(2) = -0.98366812327974720997003258D+00
        x(3) = -0.96002186496830751221687103D+00
        x(4) = -0.92620004742927432587932428D+00
        x(5) = -0.88256053579205268154311646D+00
        x(6) = -0.82956576238276839744289812D+00
        x(7) = -0.76777743210482619491797734D+00
        x(8) = -0.69785049479331579693229239D+00
        x(9) = -0.62052618298924286114047756D+00
        x(10) = -0.53662414814201989926416979D+00
        x(11) = -0.44703376953808917678060990D+00
        x(12) = -0.35270472553087811347103721D+00
        x(13) = -0.25463692616788984643980513D+00
        x(14) = -0.15386991360858354696379467D+00
        x(15) = -0.05147184255531769583302521D+00
        x(16) = 0.05147184255531769583302521D+00
        x(17) = 0.15386991360858354696379467D+00
        x(18) = 0.25463692616788984643980513D+00
        x(19) = 0.35270472553087811347103721D+00
        x(20) = 0.44703376953808917678060990D+00
        x(21) = 0.53662414814201989926416979D+00
        x(22) = 0.62052618298924286114047756D+00
        x(23) = 0.69785049479331579693229239D+00
        x(24) = 0.76777743210482619491797734D+00
        x(25) = 0.82956576238276839744289812D+00
        x(26) = 0.88256053579205268154311646D+00
        x(27) = 0.92620004742927432587932428D+00
        x(28) = 0.96002186496830751221687103D+00
        x(29) = 0.98366812327974720997003258D+00
        x(30) = 0.99689348407464954027163005D+00

        w(1) = 0.007968192496166605615465883475D+00
        w(2) = 0.018466468311090959142302131912D+00
        w(3) = 0.028784707883323369349719179611D+00
        w(4) = 0.038799192569627049596801936446D+00
        w(5) = 0.048402672830594052902938140423D+00
        w(6) = 0.057493156217619066481721689402D+00
        w(7) = 0.06597422988218049512812851512D+00
        w(8) = 0.07375597473770520626824385002D+00
        w(9) = 0.08075589522942021535469493846D+00
        w(10) = 0.08689978720108297980238753072D+00
        w(11) = 0.09212252223778612871763270709D+00
        w(12) = 0.09636873717464425963946862635D+00
        w(13) = 0.09959342058679526706278028210D+00
        w(14) = 0.10176238974840550459642895217D+00
        w(15) = 0.10285265289355884034128563671D+00
        w(16) = 0.10285265289355884034128563671D+00
        w(17) = 0.10176238974840550459642895217D+00
        w(18) = 0.09959342058679526706278028210D+00
        w(19) = 0.09636873717464425963946862635D+00
        w(20) = 0.09212252223778612871763270709D+00
        w(21) = 0.08689978720108297980238753072D+00
        w(22) = 0.08075589522942021535469493846D+00
        w(23) = 0.07375597473770520626824385002D+00
        w(24) = 0.06597422988218049512812851512D+00
        w(25) = 0.057493156217619066481721689402D+00
        w(26) = 0.048402672830594052902938140423D+00
        w(27) = 0.038799192569627049596801936446D+00
        w(28) = 0.028784707883323369349719179611D+00
        w(29) = 0.018466468311090959142302131912D+00
        w(30) = 0.007968192496166605615465883475D+00

      else if ( n .eq. 31 ) then

        x(1) = -0.99708748181947707405562655D+00
        x(2) = -0.98468590966515248400246517D+00
        x(3) = -0.96250392509294966178905240D+00
        x(4) = -0.93075699789664816495694576D+00
        x(5) = -0.88976002994827104337419201D+00
        x(6) = -0.83992032014626734008690454D+00
        x(7) = -0.78173314841662494040636002D+00
        x(8) = -0.71577678458685328390597087D+00
        x(9) = -0.64270672292426034618441820D+00
        x(10) = -0.56324916140714926272094492D+00
        x(11) = -0.47819378204490248044059404D+00
        x(12) = -0.38838590160823294306135146D+00
        x(13) = -0.29471806998170161661790390D+00
        x(14) = -0.19812119933557062877241300D+00
        x(15) = -0.09955531215234152032517479D+00
        x(16) = 0.00000000000000000000000000D+00
        x(17) = 0.09955531215234152032517479D+00
        x(18) = 0.19812119933557062877241300D+00
        x(19) = 0.29471806998170161661790390D+00
        x(20) = 0.38838590160823294306135146D+00
        x(21) = 0.47819378204490248044059404D+00
        x(22) = 0.56324916140714926272094492D+00
        x(23) = 0.64270672292426034618441820D+00
        x(24) = 0.71577678458685328390597087D+00
        x(25) = 0.78173314841662494040636002D+00
        x(26) = 0.83992032014626734008690454D+00
        x(27) = 0.88976002994827104337419201D+00
        x(28) = 0.93075699789664816495694576D+00
        x(29) = 0.96250392509294966178905240D+00
        x(30) = 0.98468590966515248400246517D+00
        x(31) = 0.99708748181947707405562655D+00

        w(1) = 0.0074708315792487758586968750322D+00
        w(2) = 0.017318620790310582463157996087D+00
        w(3) = 0.027009019184979421800608708092D+00
        w(4) = 0.036432273912385464024392010468D+00
        w(5) = 0.045493707527201102902315857895D+00
        w(6) = 0.054103082424916853711666259087D+00
        w(7) = 0.062174786561028426910343543687D+00
        w(8) = 0.069628583235410366167756126255D+00
        w(9) = 0.076390386598776616426357674901D+00
        w(10) = 0.082392991761589263903823367432D+00
        w(11) = 0.087576740608477876126198069695D+00
        w(12) = 0.091890113893641478215362871607D+00
        w(13) = 0.095290242912319512807204197488D+00
        w(14) = 0.097743335386328725093474010979D+00
        w(15) = 0.099225011226672307874875514429D+00
        w(16) = 0.09972054479342645142753383373D+00
        w(17) = 0.099225011226672307874875514429D+00
        w(18) = 0.097743335386328725093474010979D+00
        w(19) = 0.095290242912319512807204197488D+00
        w(20) = 0.091890113893641478215362871607D+00
        w(21) = 0.087576740608477876126198069695D+00
        w(22) = 0.082392991761589263903823367432D+00
        w(23) = 0.076390386598776616426357674901D+00
        w(24) = 0.069628583235410366167756126255D+00
        w(25) = 0.062174786561028426910343543687D+00
        w(26) = 0.054103082424916853711666259087D+00
        w(27) = 0.045493707527201102902315857895D+00
        w(28) = 0.036432273912385464024392010468D+00
        w(29) = 0.027009019184979421800608708092D+00
        w(30) = 0.017318620790310582463157996087D+00
        w(31) = 0.0074708315792487758586968750322D+00

      else if ( n .eq. 32 ) then

        x(1) = -0.99726386184948156354498113D+00
        x(2) = -0.98561151154526833540017504D+00
        x(3) = -0.96476225558750643077381193D+00
        x(4) = -0.93490607593773968917091913D+00
        x(5) = -0.89632115576605212396530724D+00
        x(6) = -0.84936761373256997013369300D+00
        x(7) = -0.79448379596794240696309730D+00
        x(8) = -0.73218211874028968038742667D+00
        x(9) = -0.66304426693021520097511517D+00
        x(10) = -0.58771575724076232904074548D+00
        x(11) = -0.50689990893222939002374747D+00
        x(12) = -0.42135127613063534536411944D+00
        x(13) = -0.33186860228212764977991681D+00
        x(14) = -0.23928736225213707454460321D+00
        x(15) = -0.14447196158279649348518637D+00
        x(16) = -0.04830766568773831623481257D+00
        x(17) = 0.04830766568773831623481257D+00
        x(18) = 0.14447196158279649348518637D+00
        x(19) = 0.23928736225213707454460321D+00
        x(20) = 0.33186860228212764977991681D+00
        x(21) = 0.42135127613063534536411944D+00
        x(22) = 0.50689990893222939002374747D+00
        x(23) = 0.58771575724076232904074548D+00
        x(24) = 0.66304426693021520097511517D+00
        x(25) = 0.73218211874028968038742667D+00
        x(26) = 0.79448379596794240696309730D+00
        x(27) = 0.84936761373256997013369300D+00
        x(28) = 0.89632115576605212396530724D+00
        x(29) = 0.93490607593773968917091913D+00
        x(30) = 0.96476225558750643077381193D+00
        x(31) = 0.98561151154526833540017504D+00
        x(32) = 0.99726386184948156354498113D+00

        w(1) = 0.007018610009470096600407063739D+00
        w(2) = 0.016274394730905670605170562206D+00
        w(3) = 0.025392065309262059455752589789D+00
        w(4) = 0.034273862913021433102687732252D+00
        w(5) = 0.042835898022226680656878646606D+00
        w(6) = 0.050998059262376176196163244690D+00
        w(7) = 0.058684093478535547145283637300D+00
        w(8) = 0.06582222277636184683765006371D+00
        w(9) = 0.07234579410884850622539935648D+00
        w(10) = 0.07819389578707030647174091883D+00
        w(11) = 0.08331192422694675522219907460D+00
        w(12) = 0.08765209300440381114277146275D+00
        w(13) = 0.09117387869576388471286857711D+00
        w(14) = 0.09384439908080456563918023767D+00
        w(15) = 0.09563872007927485941908200220D+00
        w(16) = 0.09654008851472780056676483006D+00
        w(17) = 0.09654008851472780056676483006D+00
        w(18) = 0.09563872007927485941908200220D+00
        w(19) = 0.09384439908080456563918023767D+00
        w(20) = 0.09117387869576388471286857711D+00
        w(21) = 0.08765209300440381114277146275D+00
        w(22) = 0.08331192422694675522219907460D+00
        w(23) = 0.07819389578707030647174091883D+00
        w(24) = 0.07234579410884850622539935648D+00
        w(25) = 0.06582222277636184683765006371D+00
        w(26) = 0.058684093478535547145283637300D+00
        w(27) = 0.050998059262376176196163244690D+00
        w(28) = 0.042835898022226680656878646606D+00
        w(29) = 0.034273862913021433102687732252D+00
        w(30) = 0.025392065309262059455752589789D+00
        w(31) = 0.016274394730905670605170562206D+00
        w(32) = 0.007018610009470096600407063739D+00

      else if ( n .eq. 33 ) then

        x(1) = -0.99742469424645521726616802D+00
        x(2) = -0.98645572623064248811037570D+00
        x(3) = -0.96682290968999276892837771D+00
        x(4) = -0.93869437261116835035583512D+00
        x(5) = -0.90231676774343358304053133D+00
        x(6) = -0.85800965267650406464306148D+00
        x(7) = -0.80616235627416658979620087D+00
        x(8) = -0.74723049644956215785905512D+00
        x(9) = -0.68173195996974278626821595D+00
        x(10) = -0.61024234583637902730728751D+00
        x(11) = -0.53338990478634764354889426D+00
        x(12) = -0.45185001727245069572599328D+00
        x(13) = -0.36633925774807334107022062D+00
        x(14) = -0.27760909715249702940324807D+00
        x(15) = -0.18643929882799157233579876D+00
        x(16) = -0.09363106585473338567074292D+00
        x(17) = 0.00000000000000000000000000D+00
        x(18) = 0.09363106585473338567074292D+00
        x(19) = 0.18643929882799157233579876D+00
        x(20) = 0.27760909715249702940324807D+00
        x(21) = 0.36633925774807334107022062D+00
        x(22) = 0.45185001727245069572599328D+00
        x(23) = 0.53338990478634764354889426D+00
        x(24) = 0.61024234583637902730728751D+00
        x(25) = 0.68173195996974278626821595D+00
        x(26) = 0.74723049644956215785905512D+00
        x(27) = 0.80616235627416658979620087D+00
        x(28) = 0.85800965267650406464306148D+00
        x(29) = 0.90231676774343358304053133D+00
        x(30) = 0.93869437261116835035583512D+00
        x(31) = 0.96682290968999276892837771D+00
        x(32) = 0.98645572623064248811037570D+00
        x(33) = 0.99742469424645521726616802D+00

        w(1) = 0.0066062278475873780586492352085D+00
        w(2) = 0.015321701512934676127945768534D+00
        w(3) = 0.023915548101749480350533257529D+00
        w(4) = 0.032300358632328953281561447250D+00
        w(5) = 0.040401541331669591563409790527D+00
        w(6) = 0.048147742818711695670146880138D+00
        w(7) = 0.055470846631663561284944495439D+00
        w(8) = 0.062306482530317480031627725771D+00
        w(9) = 0.068594572818656712805955073015D+00
        w(10) = 0.074279854843954149342472175919D+00
        w(11) = 0.079312364794886738363908384942D+00
        w(12) = 0.083647876067038707613928014518D+00
        w(13) = 0.087248287618844337607281670945D+00
        w(14) = 0.090081958660638577239743705500D+00
        w(15) = 0.092123986643316846213240977717D+00
        w(16) = 0.093356426065596116160999126274D+00
        w(17) = 0.09376844616020999656730454155D+00
        w(18) = 0.093356426065596116160999126274D+00
        w(19) = 0.092123986643316846213240977717D+00
        w(20) = 0.090081958660638577239743705500D+00
        w(21) = 0.087248287618844337607281670945D+00
        w(22) = 0.083647876067038707613928014518D+00
        w(23) = 0.079312364794886738363908384942D+00
        w(24) = 0.074279854843954149342472175919D+00
        w(25) = 0.068594572818656712805955073015D+00
        w(26) = 0.062306482530317480031627725771D+00
        w(27) = 0.055470846631663561284944495439D+00
        w(28) = 0.048147742818711695670146880138D+00
        w(29) = 0.040401541331669591563409790527D+00
        w(30) = 0.032300358632328953281561447250D+00
        w(31) = 0.023915548101749480350533257529D+00
        w(32) = 0.015321701512934676127945768534D+00
        w(33) = 0.0066062278475873780586492352085D+00

      else if ( n .eq. 63 ) then

        x(1) = -0.99928298402912378037893614D+00
        x(2) = -0.99622401277797010860219336D+00
        x(3) = -0.99072854689218946681089467D+00
        x(4) = -0.98280881059372723486251141D+00
        x(5) = -0.97248403469757002280196068D+00
        x(6) = -0.95977944975894192707035417D+00
        x(7) = -0.94472613404100980296637532D+00
        x(8) = -0.92736092062184320544703138D+00
        x(9) = -0.90772630277853155803695313D+00
        x(10) = -0.88587032850785342629029846D+00
        x(11) = -0.86184648236412371953961184D+00
        x(12) = -0.83571355431950284347180777D+00
        x(13) = -0.80753549577345676005146599D+00
        x(14) = -0.7773812629903723355633302D+00
        x(15) = -0.7453246483178474178293217D+00
        x(16) = -0.7114440995848458078514315D+00
        x(17) = -0.6758225281149860901311033D+00
        x(18) = -0.6385471058213653850003070D+00
        x(19) = -0.5997090518776252357390089D+00
        x(20) = -0.5594034094862850132676978D+00
        x(21) = -0.5177288132900332481244776D+00
        x(22) = -0.4747872479948043999222123D+00
        x(23) = -0.4306837987951116006620889D+00
        x(24) = -0.3855263942122478924776150D+00
        x(25) = -0.3394255419745844024688344D+00
        x(26) = -0.2924940585862514400361572D+00
        x(27) = -0.2448467932459533627484046D+00
        x(28) = -0.1966003467915066845576275D+00
        x(29) = -0.1478727863578719685698391D+00
        x(30) = -0.0987833564469452795297037D+00
        x(31) = -0.0494521871161596272342338D+00
        x(32) = 0.0000000000000000000000000D+00
        x(33) = 0.0494521871161596272342338D+00
        x(34) = 0.0987833564469452795297037D+00
        x(35) = 0.1478727863578719685698391D+00
        x(36) = 0.1966003467915066845576275D+00
        x(37) = 0.2448467932459533627484046D+00
        x(38) = 0.2924940585862514400361572D+00
        x(39) = 0.3394255419745844024688344D+00
        x(40) = 0.3855263942122478924776150D+00
        x(41) = 0.4306837987951116006620889D+00
        x(42) = 0.4747872479948043999222123D+00
        x(43) = 0.5177288132900332481244776D+00
        x(44) = 0.5594034094862850132676978D+00
        x(45) = 0.5997090518776252357390089D+00
        x(46) = 0.6385471058213653850003070D+00
        x(47) = 0.6758225281149860901311033D+00
        x(48) = 0.7114440995848458078514315D+00
        x(49) = 0.7453246483178474178293217D+00
        x(50) = 0.7773812629903723355633302D+00
        x(51) = 0.8075354957734567600514660D+00
        x(52) = 0.8357135543195028434718078D+00
        x(53) = 0.8618464823641237195396118D+00
        x(54) = 0.8858703285078534262902985D+00
        x(55) = 0.9077263027785315580369531D+00
        x(56) = 0.9273609206218432054470314D+00
        x(57) = 0.9447261340410098029663753D+00
        x(58) = 0.9597794497589419270703542D+00
        x(59) = 0.9724840346975700228019607D+00
        x(60) = 0.9828088105937272348625114D+00
        x(61) = 0.9907285468921894668108947D+00
        x(62) = 0.9962240127779701086021934D+00
        x(63) = 0.9992829840291237803789361D+00

        w(1) = 0.0018398745955770841170924455540D+00
        w(2) = 0.0042785083468637618660784110826D+00
        w(3) = 0.0067102917659601362519069307298D+00
        w(4) = 0.0091259686763266563540586454218D+00
        w(5) = 0.011519376076880041750750606149D+00
        w(6) = 0.013884612616115610824866086368D+00
        w(7) = 0.016215878410338338882283672975D+00
        w(8) = 0.018507464460161270409260545805D+00
        w(9) = 0.020753761258039090775341953421D+00
        w(10) = 0.022949271004889933148942319562D+00
        w(11) = 0.025088620553344986618630138068D+00
        w(12) = 0.027166574359097933225189839439D+00
        w(13) = 0.029178047208280526945551502154D+00
        w(14) = 0.031118116622219817508215988557D+00
        w(15) = 0.032982034883779341765683179672D+00
        w(16) = 0.034765240645355877697180504643D+00
        w(17) = 0.036463370085457289630452409788D+00
        w(18) = 0.038072267584349556763638324928D+00
        w(19) = 0.039587995891544093984807928149D+00
        w(20) = 0.041006845759666398635110037009D+00
        w(21) = 0.042325345020815822982505485403D+00
        w(22) = 0.043540267083027590798964315704D+00
        w(23) = 0.044648638825941395370332669517D+00
        w(24) = 0.045647747876292608685885992609D+00
        w(25) = 0.046535149245383696510395418747D+00
        w(26) = 0.047308671312268919080604988339D+00
        w(27) = 0.047966421137995131411052756195D+00
        w(28) = 0.048506789097883847864090099146D+00
        w(29) = 0.048928452820511989944709361549D+00
        w(30) = 0.049230380423747560785043116988D+00
        w(31) = 0.049411833039918178967039646117D+00
        w(32) = 0.04947236662393102088866936042D+00
        w(33) = 0.049411833039918178967039646117D+00
        w(34) = 0.049230380423747560785043116988D+00
        w(35) = 0.048928452820511989944709361549D+00
        w(36) = 0.048506789097883847864090099146D+00
        w(37) = 0.047966421137995131411052756195D+00
        w(38) = 0.047308671312268919080604988339D+00
        w(39) = 0.046535149245383696510395418747D+00
        w(40) = 0.045647747876292608685885992609D+00
        w(41) = 0.044648638825941395370332669517D+00
        w(42) = 0.043540267083027590798964315704D+00
        w(43) = 0.042325345020815822982505485403D+00
        w(44) = 0.041006845759666398635110037009D+00
        w(45) = 0.039587995891544093984807928149D+00
        w(46) = 0.038072267584349556763638324928D+00
        w(47) = 0.036463370085457289630452409788D+00
        w(48) = 0.034765240645355877697180504643D+00
        w(49) = 0.032982034883779341765683179672D+00
        w(50) = 0.031118116622219817508215988557D+00
        w(51) = 0.029178047208280526945551502154D+00
        w(52) = 0.027166574359097933225189839439D+00
        w(53) = 0.025088620553344986618630138068D+00
        w(54) = 0.022949271004889933148942319562D+00
        w(55) = 0.020753761258039090775341953421D+00
        w(56) = 0.018507464460161270409260545805D+00
        w(57) = 0.016215878410338338882283672975D+00
        w(58) = 0.013884612616115610824866086368D+00
        w(59) = 0.011519376076880041750750606149D+00
        w(60) = 0.0091259686763266563540586454218D+00
        w(61) = 0.0067102917659601362519069307298D+00
        w(62) = 0.0042785083468637618660784110826D+00
        w(63) = 0.0018398745955770841170924455540D+00

      else if ( n .eq. 64 ) then

        x(1) = -0.99930504173577213945690562D+00
        x(2) = -0.99634011677195527934692450D+00
        x(3) = -0.99101337147674432073938238D+00
        x(4) = -0.98333625388462595693129930D+00
        x(5) = -0.97332682778991096374185351D+00
        x(6) = -0.96100879965205371891861412D+00
        x(7) = -0.94641137485840281606248149D+00
        x(8) = -0.92956917213193957582149015D+00
        x(9) = -0.91052213707850280575638067D+00
        x(10) = -0.88931544599511410585340404D+00
        x(11) = -0.86599939815409281976078339D+00
        x(12) = -0.8406292962525803627516915D+00
        x(13) = -0.8132653151227975597419233D+00
        x(14) = -0.7839723589433414076102205D+00
        x(15) = -0.7528199072605318966118638D+00
        x(16) = -0.7198818501716108268489402D+00
        x(17) = -0.6852363130542332425635584D+00
        x(18) = -0.6489654712546573398577612D+00
        x(19) = -0.6111553551723932502488530D+00
        x(20) = -0.5718956462026340342838781D+00
        x(21) = -0.5312794640198945456580139D+00
        x(22) = -0.4894031457070529574785263D+00
        x(23) = -0.4463660172534640879849477D+00
        x(24) = -0.4022701579639916036957668D+00
        x(25) = -0.3572201583376681159504426D+00
        x(26) = -0.3113228719902109561575127D+00
        x(27) = -0.2646871622087674163739642D+00
        x(28) = -0.2174236437400070841496487D+00
        x(29) = -0.1696444204239928180373136D+00
        x(30) = -0.1214628192961205544703765D+00
        x(31) = -0.0729931217877990394495429D+00
        x(32) = -0.0243502926634244325089558D+00
        x(33) = 0.0243502926634244325089558D+00
        x(34) = 0.0729931217877990394495429D+00
        x(35) = 0.1214628192961205544703765D+00
        x(36) = 0.1696444204239928180373136D+00
        x(37) = 0.2174236437400070841496487D+00
        x(38) = 0.2646871622087674163739642D+00
        x(39) = 0.3113228719902109561575127D+00
        x(40) = 0.3572201583376681159504426D+00
        x(41) = 0.4022701579639916036957668D+00
        x(42) = 0.4463660172534640879849477D+00
        x(43) = 0.4894031457070529574785263D+00
        x(44) = 0.5312794640198945456580139D+00
        x(45) = 0.5718956462026340342838781D+00
        x(46) = 0.6111553551723932502488530D+00
        x(47) = 0.6489654712546573398577612D+00
        x(48) = 0.6852363130542332425635584D+00
        x(49) = 0.7198818501716108268489402D+00
        x(50) = 0.7528199072605318966118638D+00
        x(51) = 0.7839723589433414076102205D+00
        x(52) = 0.8132653151227975597419233D+00
        x(53) = 0.8406292962525803627516915D+00
        x(54) = 0.8659993981540928197607834D+00
        x(55) = 0.8893154459951141058534040D+00
        x(56) = 0.9105221370785028057563807D+00
        x(57) = 0.9295691721319395758214902D+00
        x(58) = 0.9464113748584028160624815D+00
        x(59) = 0.9610087996520537189186141D+00
        x(60) = 0.9733268277899109637418535D+00
        x(61) = 0.9833362538846259569312993D+00
        x(62) = 0.9910133714767443207393824D+00
        x(63) = 0.9963401167719552793469245D+00
        x(64) = 0.9993050417357721394569056D+00

        w(1) = 0.0017832807216964329472960791450D+00
        w(2) = 0.0041470332605624676352875357286D+00
        w(3) = 0.006504457968978362856117360400D+00
        w(4) = 0.008846759826363947723030914660D+00
        w(5) = 0.011168139460131128818590493019D+00
        w(6) = 0.013463047896718642598060766686D+00
        w(7) = 0.015726030476024719321965995298D+00
        w(8) = 0.017951715775697343085045302001D+00
        w(9) = 0.020134823153530209372340316729D+00
        w(10) = 0.022270173808383254159298330384D+00
        w(11) = 0.024352702568710873338177550409D+00
        w(12) = 0.026377469715054658671691792625D+00
        w(13) = 0.028339672614259483227511305200D+00
        w(14) = 0.030234657072402478867974059820D+00
        w(15) = 0.032057928354851553585467504348D+00
        w(16) = 0.033805161837141609391565482111D+00
        w(17) = 0.035472213256882383810693146715D+00
        w(18) = 0.037055128540240046040415101810D+00
        w(19) = 0.038550153178615629128962496947D+00
        w(20) = 0.039953741132720341386656926128D+00
        w(21) = 0.041262563242623528610156297474D+00
        w(22) = 0.042473515123653589007339767909D+00
        w(23) = 0.043583724529323453376827860974D+00
        w(24) = 0.044590558163756563060134710031D+00
        w(25) = 0.045491627927418144479770996971D+00
        w(26) = 0.046284796581314417295953249232D+00
        w(27) = 0.046968182816210017325326285755D+00
        w(28) = 0.047540165714830308662282206944D+00
        w(29) = 0.04799938859645830772812617987D+00
        w(30) = 0.04834476223480295716976952716D+00
        w(31) = 0.04857546744150342693479906678D+00
        w(32) = 0.04869095700913972038336539073D+00
        w(33) = 0.04869095700913972038336539073D+00
        w(34) = 0.04857546744150342693479906678D+00
        w(35) = 0.04834476223480295716976952716D+00
        w(36) = 0.04799938859645830772812617987D+00
        w(37) = 0.047540165714830308662282206944D+00
        w(38) = 0.046968182816210017325326285755D+00
        w(39) = 0.046284796581314417295953249232D+00
        w(40) = 0.045491627927418144479770996971D+00
        w(41) = 0.044590558163756563060134710031D+00
        w(42) = 0.043583724529323453376827860974D+00
        w(43) = 0.042473515123653589007339767909D+00
        w(44) = 0.041262563242623528610156297474D+00
        w(45) = 0.039953741132720341386656926128D+00
        w(46) = 0.038550153178615629128962496947D+00
        w(47) = 0.037055128540240046040415101810D+00
        w(48) = 0.035472213256882383810693146715D+00
        w(49) = 0.033805161837141609391565482111D+00
        w(50) = 0.032057928354851553585467504348D+00
        w(51) = 0.030234657072402478867974059820D+00
        w(52) = 0.028339672614259483227511305200D+00
        w(53) = 0.026377469715054658671691792625D+00
        w(54) = 0.024352702568710873338177550409D+00
        w(55) = 0.022270173808383254159298330384D+00
        w(56) = 0.020134823153530209372340316729D+00
        w(57) = 0.017951715775697343085045302001D+00
        w(58) = 0.015726030476024719321965995298D+00
        w(59) = 0.013463047896718642598060766686D+00
        w(60) = 0.011168139460131128818590493019D+00
        w(61) = 0.008846759826363947723030914660D+00
        w(62) = 0.006504457968978362856117360400D+00
        w(63) = 0.0041470332605624676352875357286D+00
        w(64) = 0.0017832807216964329472960791450D+00

      else if ( n .eq. 65 ) then

        x(1) = -0.99932609707541287726569361D+00
        x(2) = -0.99645094806184916305579494D+00
        x(3) = -0.99128527617680166872182118D+00
        x(4) = -0.98383981218703494137763778D+00
        x(5) = -0.97413153983355116907496789D+00
        x(6) = -0.96218275471805523771198375D+00
        x(7) = -0.94802092816840750637376974D+00
        x(8) = -0.93167862822874933796567699D+00
        x(9) = -0.91319344054284626173654692D+00
        x(10) = -0.89260788050473893142328554D+00
        x(11) = -0.8699692949264070361941320D+00
        x(12) = -0.8453297528999302839424500D+00
        x(13) = -0.8187459259226514534339191D+00
        x(14) = -0.7902789574921218430473804D+00
        x(15) = -0.7599943224419997868739828D+00
        x(16) = -0.7279616763294246790119737D+00
        x(17) = -0.6942546952139916335526225D+00
        x(18) = -0.6589509061936251330409408D+00
        x(19) = -0.6221315090854002415825996D+00
        x(20) = -0.5838811896604873133271545D+00
        x(21) = -0.5442879248622271385455725D+00
        x(22) = -0.5034427804550068823410431D+00
        x(23) = -0.4614397015691450576978341D+00
        x(24) = -0.4183752966234090092641990D+00
        x(25) = -0.3743486151220660120087939D+00
        x(26) = -0.3294609198374864076452867D+00
        x(27) = -0.2838154539022487306176554D+00
        x(28) = -0.2375172033464168065707124D+00
        x(29) = -0.1906726556261427697749124D+00
        x(30) = -0.1433895546989751711312496D+00
        x(31) = -0.0957766532091975056522186D+00
        x(32) = -0.0479434623531718575225298D+00
        x(33) = 0.0000000000000000000000000D+00
        x(34) = 0.0479434623531718575225298D+00
        x(35) = 0.0957766532091975056522186D+00
        x(36) = 0.1433895546989751711312496D+00
        x(37) = 0.1906726556261427697749124D+00
        x(38) = 0.2375172033464168065707124D+00
        x(39) = 0.2838154539022487306176554D+00
        x(40) = 0.3294609198374864076452867D+00
        x(41) = 0.3743486151220660120087939D+00
        x(42) = 0.4183752966234090092641990D+00
        x(43) = 0.4614397015691450576978341D+00
        x(44) = 0.5034427804550068823410431D+00
        x(45) = 0.5442879248622271385455725D+00
        x(46) = 0.5838811896604873133271545D+00
        x(47) = 0.6221315090854002415825996D+00
        x(48) = 0.6589509061936251330409408D+00
        x(49) = 0.6942546952139916335526225D+00
        x(50) = 0.7279616763294246790119737D+00
        x(51) = 0.7599943224419997868739828D+00
        x(52) = 0.7902789574921218430473804D+00
        x(53) = 0.8187459259226514534339191D+00
        x(54) = 0.8453297528999302839424500D+00
        x(55) = 0.8699692949264070361941320D+00
        x(56) = 0.8926078805047389314232855D+00
        x(57) = 0.9131934405428462617365469D+00
        x(58) = 0.9316786282287493379656770D+00
        x(59) = 0.9480209281684075063737697D+00
        x(60) = 0.9621827547180552377119837D+00
        x(61) = 0.9741315398335511690749679D+00
        x(62) = 0.9838398121870349413776378D+00
        x(63) = 0.9912852761768016687218212D+00
        x(64) = 0.9964509480618491630557949D+00
        x(65) = 0.9993260970754128772656936D+00

        w(1) = 0.0017292582513002508983395851463D+00
        w(2) = 0.0040215241720037363470786599528D+00
        w(3) = 0.0063079425789717545501888719039D+00
        w(4) = 0.0085801482668814598936358121592D+00
        w(5) = 0.0108326787895979686215140551272D+00
        w(6) = 0.013060311639994846336168342922D+00
        w(7) = 0.015257912146448310349265388145D+00
        w(8) = 0.017420421997670248495365759969D+00
        w(9) = 0.019542865836750062826837429313D+00
        w(10) = 0.021620361284934062841654274667D+00
        w(11) = 0.023648129691287236698780978994D+00
        w(12) = 0.025621506938037758214084978694D+00
        w(13) = 0.027535954088450343942499722327D+00
        w(14) = 0.029387067789310668062644859210D+00
        w(15) = 0.031170590380189142464431845777D+00
        w(16) = 0.032882419676368574984049638008D+00
        w(17) = 0.034518618398549058625221276859D+00
        w(18) = 0.036075423225565273932166270524D+00
        w(19) = 0.037549253448257709809772223198D+00
        w(20) = 0.038936719204051197616673806364D+00
        w(21) = 0.040234629273005533815446337743D+00
        w(22) = 0.041439998417240293022686299233D+00
        w(23) = 0.042550054246755802719217150803D+00
        w(24) = 0.043562243595800486532284821661D+00
        w(25) = 0.044474238395082974427323504000D+00
        w(26) = 0.045283941026300230657128240574D+00
        w(27) = 0.045989489146651696963893390818D+00
        w(28) = 0.046589259972233498302255136790D+00
        w(29) = 0.047081874010454522246006808290D+00
        w(30) = 0.047466198232885503152644458740D+00
        w(31) = 0.047741348681240621559038972227D+00
        w(32) = 0.047906692500495862031347289176D+00
        w(33) = 0.04796184939446661812070762137D+00
        w(34) = 0.047906692500495862031347289176D+00
        w(35) = 0.047741348681240621559038972227D+00
        w(36) = 0.047466198232885503152644458740D+00
        w(37) = 0.047081874010454522246006808290D+00
        w(38) = 0.046589259972233498302255136790D+00
        w(39) = 0.045989489146651696963893390818D+00
        w(40) = 0.045283941026300230657128240574D+00
        w(41) = 0.044474238395082974427323504000D+00
        w(42) = 0.043562243595800486532284821661D+00
        w(43) = 0.042550054246755802719217150803D+00
        w(44) = 0.041439998417240293022686299233D+00
        w(45) = 0.040234629273005533815446337743D+00
        w(46) = 0.038936719204051197616673806364D+00
        w(47) = 0.037549253448257709809772223198D+00
        w(48) = 0.036075423225565273932166270524D+00
        w(49) = 0.034518618398549058625221276859D+00
        w(50) = 0.032882419676368574984049638008D+00
        w(51) = 0.031170590380189142464431845777D+00
        w(52) = 0.029387067789310668062644859210D+00
        w(53) = 0.027535954088450343942499722327D+00
        w(54) = 0.025621506938037758214084978694D+00
        w(55) = 0.023648129691287236698780978994D+00
        w(56) = 0.021620361284934062841654274667D+00
        w(57) = 0.019542865836750062826837429313D+00
        w(58) = 0.017420421997670248495365759969D+00
        w(59) = 0.015257912146448310349265388145D+00
        w(60) = 0.013060311639994846336168342922D+00
        w(61) = 0.0108326787895979686215140551272D+00
        w(62) = 0.0085801482668814598936358121592D+00
        w(63) = 0.0063079425789717545501888719039D+00
        w(64) = 0.0040215241720037363470786599528D+00
        w(65) = 0.0017292582513002508983395851463D+00

      else if ( n .eq. 127 ) then

        x(1) = -0.9998221304153061462673512D+00
        x(2) = -0.9990629343553118951383159D+00
        x(3) = -0.9976975661898046210744170D+00
        x(4) = -0.9957265513520272266354334D+00
        x(5) = -0.9931510492545171473611308D+00
        x(6) = -0.9899726145914841576077867D+00
        x(7) = -0.9861931740169316667104383D+00
        x(8) = -0.9818150208038141100334631D+00
        x(9) = -0.9768408123430703268174439D+00
        x(10) = -0.9712735681615291922889469D+00
        x(11) = -0.9651166679452921210908251D+00
        x(12) = -0.9583738494252387711491029D+00
        x(13) = -0.9510492060778803105479076D+00
        x(14) = -0.9431471846248148273454496D+00
        x(15) = -0.9346725823247379685736349D+00
        x(16) = -0.9256305440562338491274647D+00
        x(17) = -0.9160265591914658093130886D+00
        x(18) = -0.9058664582618213828024613D+00
        x(19) = -0.8951564094170837089690438D+00
        x(20) = -0.8839029146800265699452579D+00
        x(21) = -0.8721128059985607114196375D+00
        x(22) = -0.8597932410977408098120313D+00
        x(23) = -0.8469516991340975984533393D+00
        x(24) = -0.8335959761548995143795572D+00
        x(25) = -0.8197341803650786741551191D+00
        x(26) = -0.8053747272046802146665608D+00
        x(27) = -0.7905263342398137999454500D+00
        x(28) = -0.7751980158702023824449628D+00
        x(29) = -0.7593990778565366715566637D+00
        x(30) = -0.7431391116709545129205669D+00
        x(31) = -0.7264279886740726855356929D+00
        x(32) = -0.7092758541221045609994446D+00
        x(33) = -0.6916931210077006701564414D+00
        x(34) = -0.6736904637382504853466825D+00
        x(35) = -0.6552788116554826302767651D+00
        x(36) = -0.6364693424002972413476082D+00
        x(37) = -0.6172734751268582838576392D+00
        x(38) = -0.5977028635700652293844120D+00
        x(39) = -0.5777693889706125800032517D+00
        x(40) = -0.5574851528619322329218619D+00
        x(41) = -0.5368624697233975674581664D+00
        x(42) = -0.5159138595042493572772773D+00
        x(43) = -0.4946520400227821173949402D+00
        x(44) = -0.4730899192454052416450999D+00
        x(45) = -0.4512405874502662273318986D+00
        x(46) = -0.4291173092801933762625441D+00
        x(47) = -0.4067335156897825634086729D+00
        x(48) = -0.3841027957915169357790778D+00
        x(49) = -0.3612388886058697060709248D+00
        x(50) = -0.3381556747203985013760003D+00
        x(51) = -0.3148671678628949814860148D+00
        x(52) = -0.2913875063937056207945188D+00
        x(53) = -0.2677309447223886208883435D+00
        x(54) = -0.2439118446539178579707132D+00
        x(55) = -0.2199446666696875424545234D+00
        x(56) = -0.1958439611486108515042816D+00
        x(57) = -0.1716243595336421650083449D+00
        x(58) = -0.1473005654490856693893293D+00
        x(59) = -0.1228873457740829717260337D+00
        x(60) = -0.0983995216776989707510918D+00
        x(61) = -0.0738519596210485452734404D+00
        x(62) = -0.0492595623319266303153793D+00
        x(63) = -0.0246372597574209446148971D+00
        x(64) = 0.0000000000000000000000000D+00
        x(65) = 0.0246372597574209446148971D+00
        x(66) = 0.0492595623319266303153793D+00
        x(67) = 0.0738519596210485452734404D+00
        x(68) = 0.0983995216776989707510918D+00
        x(69) = 0.1228873457740829717260337D+00
        x(70) = 0.1473005654490856693893293D+00
        x(71) = 0.1716243595336421650083449D+00
        x(72) = 0.1958439611486108515042816D+00
        x(73) = 0.2199446666696875424545234D+00
        x(74) = 0.2439118446539178579707132D+00
        x(75) = 0.2677309447223886208883435D+00
        x(76) = 0.2913875063937056207945188D+00
        x(77) = 0.3148671678628949814860148D+00
        x(78) = 0.3381556747203985013760003D+00
        x(79) = 0.3612388886058697060709248D+00
        x(80) = 0.3841027957915169357790778D+00
        x(81) = 0.4067335156897825634086729D+00
        x(82) = 0.4291173092801933762625441D+00
        x(83) = 0.4512405874502662273318986D+00
        x(84) = 0.4730899192454052416450999D+00
        x(85) = 0.4946520400227821173949402D+00
        x(86) = 0.5159138595042493572772773D+00
        x(87) = 0.5368624697233975674581664D+00
        x(88) = 0.5574851528619322329218619D+00
        x(89) = 0.5777693889706125800032517D+00
        x(90) = 0.5977028635700652293844120D+00
        x(91) = 0.6172734751268582838576392D+00
        x(92) = 0.6364693424002972413476082D+00
        x(93) = 0.6552788116554826302767651D+00
        x(94) = 0.6736904637382504853466825D+00
        x(95) = 0.6916931210077006701564414D+00
        x(96) = 0.7092758541221045609994446D+00
        x(97) = 0.7264279886740726855356929D+00
        x(98) = 0.7431391116709545129205669D+00
        x(99) = 0.7593990778565366715566637D+00
        x(100) = 0.7751980158702023824449628D+00
        x(101) = 0.7905263342398137999454500D+00
        x(102) = 0.8053747272046802146665608D+00
        x(103) = 0.8197341803650786741551191D+00
        x(104) = 0.8335959761548995143795572D+00
        x(105) = 0.8469516991340975984533393D+00
        x(106) = 0.8597932410977408098120313D+00
        x(107) = 0.8721128059985607114196375D+00
        x(108) = 0.8839029146800265699452579D+00
        x(109) = 0.8951564094170837089690438D+00
        x(110) = 0.9058664582618213828024613D+00
        x(111) = 0.9160265591914658093130886D+00
        x(112) = 0.9256305440562338491274647D+00
        x(113) = 0.9346725823247379685736349D+00
        x(114) = 0.9431471846248148273454496D+00
        x(115) = 0.9510492060778803105479076D+00
        x(116) = 0.9583738494252387711491029D+00
        x(117) = 0.965116667945292121090825D+00
        x(118) = 0.971273568161529192288947D+00
        x(119) = 0.976840812343070326817444D+00
        x(120) = 0.981815020803814110033463D+00
        x(121) = 0.986193174016931666710438D+00
        x(122) = 0.989972614591484157607787D+00
        x(123) = 0.993151049254517147361131D+00
        x(124) = 0.995726551352027226635433D+00
        x(125) = 0.997697566189804621074417D+00
        x(126) = 0.999062934355311895138316D+00
        x(127) = 0.999822130415306146267351D+00

        w(1) = 0.00045645726109586662791936519265D+00
        w(2) = 0.00106227668695384869596523598532D+00
        w(3) = 0.0016683488125171936761028862915D+00
        w(4) = 0.0022734860707492547802810840776D+00
        w(5) = 0.0028772587656289004082883197514D+00
        w(6) = 0.0034792893810051465908910894100D+00
        w(7) = 0.0040792095178254605327114733457D+00
        w(8) = 0.0046766539777779034772638165663D+00
        w(9) = 0.0052712596565634400891303815906D+00
        w(10) = 0.0058626653903523901033648343751D+00
        w(11) = 0.0064505120486899171845442463869D+00
        w(12) = 0.0070344427036681608755685893033D+00
        w(13) = 0.0076141028256526859356393930849D+00
        w(14) = 0.0081891404887415730817235884719D+00
        w(15) = 0.0087592065795403145773316804234D+00
        w(16) = 0.0093239550065309714787536985834D+00
        w(17) = 0.0098830429087554914716648010900D+00
        w(18) = 0.0104361308631410052256731719977D+00
        w(19) = 0.0109828830900689757887996573761D+00
        w(20) = 0.011522967656921087154811609735D+00
        w(21) = 0.012056056679400848183529562145D+00
        w(22) = 0.012581826520465013101514365424D+00
        w(23) = 0.013099957986718627426172681913D+00
        w(24) = 0.013610136522139249906034237534D+00
        w(25) = 0.014112052399003395774044161634D+00
        w(26) = 0.014605400905893418351737288079D+00
        w(27) = 0.015089882532666922992635733981D+00
        w(28) = 0.015565203152273955098532590263D+00
        w(29) = 0.016031074199309941802254151843D+00
        w(30) = 0.016487212845194879399346060358D+00
        w(31) = 0.016933342169871654545878815295D+00
        w(32) = 0.017369191329918731922164721250D+00
        w(33) = 0.017794495722974774231027912900D+00
        w(34) = 0.018208997148375106468721469154D+00
        w(35) = 0.018612443963902310429440419899D+00
        w(36) = 0.019004591238555646611148901045D+00
        w(37) = 0.019385200901246454628112623489D+00
        w(38) = 0.019754041885329183081815217323D+00
        w(39) = 0.020110890268880247225644623956D+00
        w(40) = 0.020455529410639508279497065713D+00
        w(41) = 0.020787750081531811812652137291D+00
        w(42) = 0.021107350591688713643523847922D+00
        w(43) = 0.021414136912893259295449693234D+00
        w(44) = 0.021707922796373466052301324695D+00
        w(45) = 0.021988529885872983756478409759D+00
        w(46) = 0.022255787825930280235631416460D+00
        w(47) = 0.022509534365300608085694429903D+00
        w(48) = 0.022749615455457959852242553241D+00
        w(49) = 0.022975885344117206754377437839D+00
        w(50) = 0.023188206663719640249922582982D+00
        w(51) = 0.023386450514828194170722043497D+00
        w(52) = 0.023570496544381716050033676844D+00
        w(53) = 0.023740233018760777777714726703D+00
        w(54) = 0.023895556891620665983864481754D+00
        w(55) = 0.024036373866450369675132086026D+00
        w(56) = 0.024162598453819584716522917711D+00
        w(57) = 0.024274154023278979833195063937D+00
        w(58) = 0.024370972849882214952813561907D+00
        w(59) = 0.024452996155301467956140198472D+00
        w(60) = 0.024520174143511508275183033290D+00
        w(61) = 0.024572466031020653286354137335D+00
        w(62) = 0.024609840071630254092545634003D+00
        w(63) = 0.024632273575707679066033370218D+00
        w(64) = 0.02463975292396109441957941748D+00
        w(65) = 0.024632273575707679066033370218D+00
        w(66) = 0.024609840071630254092545634003D+00
        w(67) = 0.024572466031020653286354137335D+00
        w(68) = 0.024520174143511508275183033290D+00
        w(69) = 0.024452996155301467956140198472D+00
        w(70) = 0.024370972849882214952813561907D+00
        w(71) = 0.024274154023278979833195063937D+00
        w(72) = 0.024162598453819584716522917711D+00
        w(73) = 0.024036373866450369675132086026D+00
        w(74) = 0.023895556891620665983864481754D+00
        w(75) = 0.023740233018760777777714726703D+00
        w(76) = 0.023570496544381716050033676844D+00
        w(77) = 0.023386450514828194170722043497D+00
        w(78) = 0.023188206663719640249922582982D+00
        w(79) = 0.022975885344117206754377437839D+00
        w(80) = 0.022749615455457959852242553241D+00
        w(81) = 0.022509534365300608085694429903D+00
        w(82) = 0.022255787825930280235631416460D+00
        w(83) = 0.021988529885872983756478409759D+00
        w(84) = 0.021707922796373466052301324695D+00
        w(85) = 0.021414136912893259295449693234D+00
        w(86) = 0.021107350591688713643523847922D+00
        w(87) = 0.020787750081531811812652137291D+00
        w(88) = 0.020455529410639508279497065713D+00
        w(89) = 0.020110890268880247225644623956D+00
        w(90) = 0.019754041885329183081815217323D+00
        w(91) = 0.019385200901246454628112623489D+00
        w(92) = 0.019004591238555646611148901045D+00
        w(93) = 0.018612443963902310429440419899D+00
        w(94) = 0.018208997148375106468721469154D+00
        w(95) = 0.017794495722974774231027912900D+00
        w(96) = 0.017369191329918731922164721250D+00
        w(97) = 0.016933342169871654545878815295D+00
        w(98) = 0.016487212845194879399346060358D+00
        w(99) = 0.016031074199309941802254151843D+00
        w(100) = 0.015565203152273955098532590263D+00
        w(101) = 0.015089882532666922992635733981D+00
        w(102) = 0.014605400905893418351737288079D+00
        w(103) = 0.014112052399003395774044161634D+00
        w(104) = 0.013610136522139249906034237534D+00
        w(105) = 0.013099957986718627426172681913D+00
        w(106) = 0.012581826520465013101514365424D+00
        w(107) = 0.012056056679400848183529562145D+00
        w(108) = 0.011522967656921087154811609735D+00
        w(109) = 0.0109828830900689757887996573761D+00
        w(110) = 0.0104361308631410052256731719977D+00
        w(111) = 0.0098830429087554914716648010900D+00
        w(112) = 0.0093239550065309714787536985834D+00
        w(113) = 0.0087592065795403145773316804234D+00
        w(114) = 0.0081891404887415730817235884719D+00
        w(115) = 0.0076141028256526859356393930849D+00
        w(116) = 0.0070344427036681608755685893033D+00
        w(117) = 0.0064505120486899171845442463869D+00
        w(118) = 0.0058626653903523901033648343751D+00
        w(119) = 0.0052712596565634400891303815906D+00
        w(120) = 0.0046766539777779034772638165663D+00
        w(121) = 0.0040792095178254605327114733457D+00
        w(122) = 0.0034792893810051465908910894100D+00
        w(123) = 0.0028772587656289004082883197514D+00
        w(124) = 0.0022734860707492547802810840776D+00
        w(125) = 0.0016683488125171936761028862915D+00
        w(126) = 0.00106227668695384869596523598532D+00
        w(127) = 0.00045645726109586662791936519265D+00

      else if ( n .eq. 128 ) then

        x(1) = -0.9998248879471319144736081D+00
        x(2) = -0.9990774599773758950119878D+00
        x(3) = -0.9977332486255140198821574D+00
        x(4) = -0.9957927585349811868641612D+00
        x(5) = -0.9932571129002129353034372D+00
        x(6) = -0.9901278184917343833379303D+00
        x(7) = -0.9864067427245862088712355D+00
        x(8) = -0.9820961084357185360247656D+00
        x(9) = -0.9771984914639073871653744D+00
        x(10) = -0.9717168187471365809043384D+00
        x(11) = -0.9656543664319652686458290D+00
        x(12) = -0.9590147578536999280989185D+00
        x(13) = -0.9518019613412643862177963D+00
        x(14) = -0.9440202878302201821211114D+00
        x(15) = -0.9356743882779163757831268D+00
        x(16) = -0.9267692508789478433346245D+00
        x(17) = -0.9173101980809605370364836D+00
        x(18) = -0.9073028834017568139214859D+00
        x(19) = -0.8967532880491581843864474D+00
        x(20) = -0.8856677173453972174082924D+00
        x(21) = -0.8740527969580317986954180D+00
        x(22) = -0.8619154689395484605906323D+00
        x(23) = -0.8492629875779689691636001D+00
        x(24) = -0.8361029150609068471168753D+00
        x(25) = -0.8224431169556438424645942D+00
        x(26) = -0.8082917575079136601196422D+00
        x(27) = -0.7936572947621932902433329D+00
        x(28) = -0.7785484755064119668504941D+00
        x(29) = -0.7629743300440947227797691D+00
        x(30) = -0.7469441667970619811698824D+00
        x(31) = -0.7304675667419088064717369D+00
        x(32) = -0.7135543776835874133438599D+00
        x(33) = -0.6962147083695143323850866D+00
        x(34) = -0.6784589224477192593677557D+00
        x(35) = -0.6602976322726460521059468D+00
        x(36) = -0.6417416925623075571535249D+00
        x(37) = -0.6228021939105849107615396D+00
        x(38) = -0.6034904561585486242035732D+00
        x(39) = -0.5838180216287630895500389D+00
        x(40) = -0.5637966482266180839144308D+00
        x(41) = -0.5434383024128103634441936D+00
        x(42) = -0.5227551520511754784539479D+00
        x(43) = -0.5017595591361444642896063D+00
        x(44) = -0.4804640724041720258582757D+00
        x(45) = -0.4588814198335521954490891D+00
        x(46) = -0.4370245010371041629370429D+00
        x(47) = -0.4149063795522750154922739D+00
        x(48) = -0.3925402750332674427356482D+00
        x(49) = -0.3699395553498590266165917D+00
        x(50) = -0.3471177285976355084261628D+00
        x(51) = -0.3240884350244133751832523D+00
        x(52) = -0.3008654388776772026671541D+00
        x(53) = -0.2774626201779044028062316D+00
        x(54) = -0.2538939664226943208556180D+00
        x(55) = -0.2301735642266599864109866D+00
        x(56) = -0.2063155909020792171540580D+00
        x(57) = -0.1823343059853371824103826D+00
        x(58) = -0.1582440427142249339974755D+00
        x(59) = -0.1340591994611877851175753D+00
        x(60) = -0.1097942311276437466729747D+00
        x(61) = -0.0854636405045154986364980D+00
        x(62) = -0.0610819696041395681037870D+00
        x(63) = -0.0366637909687334933302153D+00
        x(64) = -0.0122236989606157641980521D+00
        x(65) = 0.0122236989606157641980521D+00
        x(66) = 0.0366637909687334933302153D+00
        x(67) = 0.0610819696041395681037870D+00
        x(68) = 0.0854636405045154986364980D+00
        x(69) = 0.1097942311276437466729747D+00
        x(70) = 0.1340591994611877851175753D+00
        x(71) = 0.1582440427142249339974755D+00
        x(72) = 0.1823343059853371824103826D+00
        x(73) = 0.2063155909020792171540580D+00
        x(74) = 0.2301735642266599864109866D+00
        x(75) = 0.2538939664226943208556180D+00
        x(76) = 0.2774626201779044028062316D+00
        x(77) = 0.3008654388776772026671541D+00
        x(78) = 0.3240884350244133751832523D+00
        x(79) = 0.3471177285976355084261628D+00
        x(80) = 0.3699395553498590266165917D+00
        x(81) = 0.3925402750332674427356482D+00
        x(82) = 0.4149063795522750154922739D+00
        x(83) = 0.4370245010371041629370429D+00
        x(84) = 0.4588814198335521954490891D+00
        x(85) = 0.4804640724041720258582757D+00
        x(86) = 0.5017595591361444642896063D+00
        x(87) = 0.5227551520511754784539479D+00
        x(88) = 0.5434383024128103634441936D+00
        x(89) = 0.5637966482266180839144308D+00
        x(90) = 0.5838180216287630895500389D+00
        x(91) = 0.6034904561585486242035732D+00
        x(92) = 0.6228021939105849107615396D+00
        x(93) = 0.6417416925623075571535249D+00
        x(94) = 0.6602976322726460521059468D+00
        x(95) = 0.6784589224477192593677557D+00
        x(96) = 0.6962147083695143323850866D+00
        x(97) = 0.7135543776835874133438599D+00
        x(98) = 0.7304675667419088064717369D+00
        x(99) = 0.7469441667970619811698824D+00
        x(100) = 0.7629743300440947227797691D+00
        x(101) = 0.7785484755064119668504941D+00
        x(102) = 0.7936572947621932902433329D+00
        x(103) = 0.8082917575079136601196422D+00
        x(104) = 0.8224431169556438424645942D+00
        x(105) = 0.8361029150609068471168753D+00
        x(106) = 0.8492629875779689691636001D+00
        x(107) = 0.8619154689395484605906323D+00
        x(108) = 0.8740527969580317986954180D+00
        x(109) = 0.8856677173453972174082924D+00
        x(110) = 0.8967532880491581843864474D+00
        x(111) = 0.9073028834017568139214859D+00
        x(112) = 0.9173101980809605370364836D+00
        x(113) = 0.926769250878947843334625D+00
        x(114) = 0.935674388277916375783127D+00
        x(115) = 0.944020287830220182121111D+00
        x(116) = 0.951801961341264386217796D+00
        x(117) = 0.959014757853699928098919D+00
        x(118) = 0.965654366431965268645829D+00
        x(119) = 0.971716818747136580904338D+00
        x(120) = 0.977198491463907387165374D+00
        x(121) = 0.982096108435718536024766D+00
        x(122) = 0.986406742724586208871236D+00
        x(123) = 0.990127818491734383337930D+00
        x(124) = 0.993257112900212935303437D+00
        x(125) = 0.995792758534981186864161D+00
        x(126) = 0.997733248625514019882157D+00
        x(127) = 0.999077459977375895011988D+00
        x(128) = 0.999824887947131914473608D+00

        w(1) = 0.00044938096029209037639429223999D+00
        w(2) = 0.0010458126793403487793128516001D+00
        w(3) = 0.0016425030186690295387908755948D+00
        w(4) = 0.0022382884309626187436220542727D+00
        w(5) = 0.0028327514714579910952857346468D+00
        w(6) = 0.0034255260409102157743377846601D+00
        w(7) = 0.0040162549837386423131943434863D+00
        w(8) = 0.0046045842567029551182905419803D+00
        w(9) = 0.0051901618326763302050707671348D+00
        w(10) = 0.0057726375428656985893346176261D+00
        w(11) = 0.006351663161707188787214327826D+00
        w(12) = 0.006926892566898813563426670360D+00
        w(13) = 0.007497981925634728687671962688D+00
        w(14) = 0.008064589890486057972928598698D+00
        w(15) = 0.008626377798616749704978843782D+00
        w(16) = 0.009183009871660874334478743688D+00
        w(17) = 0.009734153415006805863548266094D+00
        w(18) = 0.010279479015832157133215340326D+00
        w(19) = 0.010818660739503076247659646277D+00
        w(20) = 0.011351376324080416693281668453D+00
        w(21) = 0.011877307372740279575891106926D+00
        w(22) = 0.012396139543950922968821728197D+00
        w(23) = 0.012907562739267347220442834004D+00
        w(24) = 0.013411271288616332314488951616D+00
        w(25) = 0.013906964132951985244288007396D+00
        w(26) = 0.014394345004166846176823892009D+00
        w(27) = 0.014873122602147314252385498520D+00
        w(28) = 0.015343010768865144085990853741D+00
        w(29) = 0.015803728659399346858965631687D+00
        w(30) = 0.016255000909785187051657456477D+00
        w(31) = 0.016696557801589204589091507954D+00
        w(32) = 0.017128135423111376830680987619D+00
        w(33) = 0.017549475827117704648706925634D+00
        w(34) = 0.017960327185008685940196927525D+00
        w(35) = 0.018360443937331343221289290991D+00
        w(36) = 0.018749586940544708650919548474D+00
        w(37) = 0.019127523609950945486518531668D+00
        w(38) = 0.019494028058706602823021918681D+00
        w(39) = 0.019848881232830862219944413265D+00
        w(40) = 0.020191871042130041180673158406D+00
        w(41) = 0.020522792486960069432284967788D+00
        w(42) = 0.020841447780751149113583948423D+00
        w(43) = 0.021147646468221348537019535180D+00
        w(44) = 0.021441205539208460137111853878D+00
        w(45) = 0.021721949538052075375260957768D+00
        w(46) = 0.021989710668460491434122106599D+00
        w(47) = 0.022244328893799765104629133607D+00
        w(48) = 0.022485652032744966871824603941D+00
        w(49) = 0.022713535850236461309712635923D+00
        w(50) = 0.022927844143686846920410987209D+00
        w(51) = 0.023128448824387027879297902403D+00
        w(52) = 0.023315229994062760122415671273D+00
        w(53) = 0.023488076016535913153025273282D+00
        w(54) = 0.023646883584447615143651392303D+00
        w(55) = 0.023791557781003400638780709885D+00
        w(56) = 0.023922012136703455672450408817D+00
        w(57) = 0.024038168681024052637587316820D+00
        w(58) = 0.024139957989019284997716653890D+00
        w(59) = 0.024227319222815248120093308442D+00
        w(60) = 0.024300200167971865323442606364D+00
        w(61) = 0.024358557264690625853268520246D+00
        w(62) = 0.024402355633849582093297989694D+00
        w(63) = 0.02443156909785004505484856143D+00
        w(64) = 0.02444618019626251821132585261D+00
        w(65) = 0.02444618019626251821132585261D+00
        w(66) = 0.02443156909785004505484856143D+00
        w(67) = 0.024402355633849582093297989694D+00
        w(68) = 0.024358557264690625853268520246D+00
        w(69) = 0.024300200167971865323442606364D+00
        w(70) = 0.024227319222815248120093308442D+00
        w(71) = 0.024139957989019284997716653890D+00
        w(72) = 0.024038168681024052637587316820D+00
        w(73) = 0.023922012136703455672450408817D+00
        w(74) = 0.023791557781003400638780709885D+00
        w(75) = 0.023646883584447615143651392303D+00
        w(76) = 0.023488076016535913153025273282D+00
        w(77) = 0.023315229994062760122415671273D+00
        w(78) = 0.023128448824387027879297902403D+00
        w(79) = 0.022927844143686846920410987209D+00
        w(80) = 0.022713535850236461309712635923D+00
        w(81) = 0.022485652032744966871824603941D+00
        w(82) = 0.022244328893799765104629133607D+00
        w(83) = 0.021989710668460491434122106599D+00
        w(84) = 0.021721949538052075375260957768D+00
        w(85) = 0.021441205539208460137111853878D+00
        w(86) = 0.021147646468221348537019535180D+00
        w(87) = 0.020841447780751149113583948423D+00
        w(88) = 0.020522792486960069432284967788D+00
        w(89) = 0.020191871042130041180673158406D+00
        w(90) = 0.019848881232830862219944413265D+00
        w(91) = 0.019494028058706602823021918681D+00
        w(92) = 0.019127523609950945486518531668D+00
        w(93) = 0.018749586940544708650919548474D+00
        w(94) = 0.018360443937331343221289290991D+00
        w(95) = 0.017960327185008685940196927525D+00
        w(96) = 0.017549475827117704648706925634D+00
        w(97) = 0.017128135423111376830680987619D+00
        w(98) = 0.016696557801589204589091507954D+00
        w(99) = 0.016255000909785187051657456477D+00
        w(100) = 0.015803728659399346858965631687D+00
        w(101) = 0.015343010768865144085990853741D+00
        w(102) = 0.014873122602147314252385498520D+00
        w(103) = 0.014394345004166846176823892009D+00
        w(104) = 0.013906964132951985244288007396D+00
        w(105) = 0.013411271288616332314488951616D+00
        w(106) = 0.012907562739267347220442834004D+00
        w(107) = 0.012396139543950922968821728197D+00
        w(108) = 0.011877307372740279575891106926D+00
        w(109) = 0.011351376324080416693281668453D+00
        w(110) = 0.010818660739503076247659646277D+00
        w(111) = 0.010279479015832157133215340326D+00
        w(112) = 0.009734153415006805863548266094D+00
        w(113) = 0.009183009871660874334478743688D+00
        w(114) = 0.008626377798616749704978843782D+00
        w(115) = 0.008064589890486057972928598698D+00
        w(116) = 0.007497981925634728687671962688D+00
        w(117) = 0.006926892566898813563426670360D+00
        w(118) = 0.006351663161707188787214327826D+00
        w(119) = 0.0057726375428656985893346176261D+00
        w(120) = 0.0051901618326763302050707671348D+00
        w(121) = 0.0046045842567029551182905419803D+00
        w(122) = 0.0040162549837386423131943434863D+00
        w(123) = 0.0034255260409102157743377846601D+00
        w(124) = 0.0028327514714579910952857346468D+00
        w(125) = 0.0022382884309626187436220542727D+00
        w(126) = 0.0016425030186690295387908755948D+00
        w(127) = 0.0010458126793403487793128516001D+00
        w(128) = 0.00044938096029209037639429223999D+00

      else if ( n .eq. 129 ) then

        x(1) = -0.9998275818477487191077441D+00
        x(2) = -0.9990916504696409986514389D+00
        x(3) = -0.9977681080525852721429460D+00
        x(4) = -0.9958574393142831982149111D+00
        x(5) = -0.9933607326210712814854011D+00
        x(6) = -0.9902794486488178389207689D+00
        x(7) = -0.9866153978313475022005761D+00
        x(8) = -0.9823707352517413115507418D+00
        x(9) = -0.9775479582993672474447814D+00
        x(10) = -0.9721499048427034297274163D+00
        x(11) = -0.9661797514202097197778763D+00
        x(12) = -0.9596410113101918904168119D+00
        x(13) = -0.9525375324342090471027732D+00
        x(14) = -0.9448734950776734726784764D+00
        x(15) = -0.9366534094216514605284616D+00
        x(16) = -0.9278821128840036204317296D+00
        x(17) = -0.9185647672698286252225115D+00
        x(18) = -0.9087068557320696331245539D+00
        x(19) = -0.8983141795436338850435985D+00
        x(20) = -0.8873928546826803665034968D+00
        x(21) = -0.8759493082329433892035217D+00
        x(22) = -0.8639902746011257878940216D+00
        x(23) = -0.8515227915535356930243826D+00
        x(24) = -0.8385541960742664442975407D+00
        x(25) = -0.8250921200473358809210133D+00
        x(26) = -0.8111444857653120742087717D+00
        x(27) = -0.7967195012670592680339606D+00
        x(28) = -0.7818256555073413245387500D+00
        x(29) = -0.7664717133611208816717785D+00
        x(30) = -0.7506667104654910227632368D+00
        x(31) = -0.7344199479022727047791516D+00
        x(32) = -0.7177409867244055767721220D+00
        x(33) = -0.7006396423293521790044710D+00
        x(34) = -0.6831259786828258512462248D+00
        x(35) = -0.6652103023962409818802202D+00
        x(36) = -0.6469031566613704719753373D+00
        x(37) = -0.6282153150457794374886895D+00
        x(38) = -0.6091577751526861909563306D+00
        x(39) = -0.5897417521489813916767844D+00
        x(40) = -0.5699786721652138894754096D+00
        x(41) = -0.5498801655714271702189358D+00
        x(42) = -0.5294580601328034000099406D+00
        x(43) = -0.5087243740491428186199463D+00
        x(44) = -0.4876913088822746111853066D+00
        x(45) = -0.4663712423755613514331869D+00
        x(46) = -0.4447767211697226217818454D+00
        x(47) = -0.4229204534192644388475065D+00
        x(48) = -0.4008153013138596117693121D+00
        x(49) = -0.3784742735090801012801265D+00
        x(50) = -0.3559105174709357969672656D+00
        x(51) = -0.3331373117387248575049982D+00
        x(52) = -0.3101680581107488341147318D+00
        x(53) = -0.2870162737574911929568755D+00
        x(54) = -0.2636955832669005409666949D+00
        x(55) = -0.2402197106264598167721148D+00
        x(56) = -0.2166024711467599103221439D+00
        x(57) = -0.1928577633313305998663880D+00
        x(58) = -0.1689995606975133227390302D+00
        x(59) = -0.1450419035531891084328306D+00
        x(60) = -0.1209988907342009817690539D+00
        x(61) = -0.0968846713073332753086909D+00
        x(62) = -0.0727134362437305599118207D+00
        x(63) = -0.0484994100676562986191764D+00
        x(64) = -0.0242568424855058415749954D+00
        x(65) = 0.0000000000000000000000000D+00
        x(66) = 0.0242568424855058415749954D+00
        x(67) = 0.0484994100676562986191764D+00
        x(68) = 0.0727134362437305599118207D+00
        x(69) = 0.0968846713073332753086909D+00
        x(70) = 0.1209988907342009817690539D+00
        x(71) = 0.1450419035531891084328306D+00
        x(72) = 0.1689995606975133227390302D+00
        x(73) = 0.1928577633313305998663880D+00
        x(74) = 0.2166024711467599103221439D+00
        x(75) = 0.2402197106264598167721148D+00
        x(76) = 0.2636955832669005409666949D+00
        x(77) = 0.2870162737574911929568755D+00
        x(78) = 0.3101680581107488341147318D+00
        x(79) = 0.3331373117387248575049982D+00
        x(80) = 0.3559105174709357969672656D+00
        x(81) = 0.3784742735090801012801265D+00
        x(82) = 0.4008153013138596117693121D+00
        x(83) = 0.4229204534192644388475065D+00
        x(84) = 0.4447767211697226217818454D+00
        x(85) = 0.4663712423755613514331869D+00
        x(86) = 0.4876913088822746111853066D+00
        x(87) = 0.5087243740491428186199463D+00
        x(88) = 0.5294580601328034000099406D+00
        x(89) = 0.5498801655714271702189358D+00
        x(90) = 0.5699786721652138894754096D+00
        x(91) = 0.5897417521489813916767844D+00
        x(92) = 0.6091577751526861909563306D+00
        x(93) = 0.6282153150457794374886895D+00
        x(94) = 0.6469031566613704719753373D+00
        x(95) = 0.6652103023962409818802202D+00
        x(96) = 0.6831259786828258512462248D+00
        x(97) = 0.7006396423293521790044710D+00
        x(98) = 0.7177409867244055767721220D+00
        x(99) = 0.7344199479022727047791516D+00
        x(100) = 0.7506667104654910227632368D+00
        x(101) = 0.7664717133611208816717785D+00
        x(102) = 0.7818256555073413245387500D+00
        x(103) = 0.7967195012670592680339606D+00
        x(104) = 0.8111444857653120742087717D+00
        x(105) = 0.8250921200473358809210133D+00
        x(106) = 0.8385541960742664442975407D+00
        x(107) = 0.8515227915535356930243826D+00
        x(108) = 0.8639902746011257878940216D+00
        x(109) = 0.875949308232943389203522D+00
        x(110) = 0.887392854682680366503497D+00
        x(111) = 0.898314179543633885043599D+00
        x(112) = 0.908706855732069633124554D+00
        x(113) = 0.918564767269828625222511D+00
        x(114) = 0.927882112884003620431730D+00
        x(115) = 0.936653409421651460528462D+00
        x(116) = 0.944873495077673472678476D+00
        x(117) = 0.952537532434209047102773D+00
        x(118) = 0.959641011310191890416812D+00
        x(119) = 0.966179751420209719777876D+00
        x(120) = 0.972149904842703429727416D+00
        x(121) = 0.977547958299367247444781D+00
        x(122) = 0.982370735251741311550742D+00
        x(123) = 0.986615397831347502200576D+00
        x(124) = 0.990279448648817838920769D+00
        x(125) = 0.993360732621071281485401D+00
        x(126) = 0.995857439314283198214911D+00
        x(127) = 0.997768108052585272142946D+00
        x(128) = 0.999091650469640998651439D+00
        x(129) = 0.999827581847748719107744D+00

        w(1) = 0.00044246794182939296923668005717D+00
        w(2) = 0.00102972844619622394463273519315D+00
        w(3) = 0.0016172530556785534682413679271D+00
        w(4) = 0.0022039015180966937075786419741D+00
        w(5) = 0.0027892681877797554940944677057D+00
        w(6) = 0.0033729979506246246117755709288D+00
        w(7) = 0.0039547444682113562172392974765D+00
        w(8) = 0.0045341644298525434513226874954D+00
        w(9) = 0.0051109164669246267289761565766D+00
        w(10) = 0.0056846609912469045788016012203D+00
        w(11) = 0.0062550602724461408889348709586D+00
        w(12) = 0.0068217785893519121070498527769D+00
        w(13) = 0.0073844824072454014447165055698D+00
        w(14) = 0.0079428405646668029041114107832D+00
        w(15) = 0.0084965244635723279730542832506D+00
        w(16) = 0.0090452082602137316404219313819D+00
        w(17) = 0.0095885690555104190787301294510D+00
        w(18) = 0.0101262870842733548093160774580D+00
        w(19) = 0.0106580459029055185304204093001D+00
        w(20) = 0.0111835325753305049735380697538D+00
        w(21) = 0.011702437856964778185746436834D+00
        w(22) = 0.012214456376582979416221105914D+00
        w(23) = 0.012719286815944623465099036330D+00
        w(24) = 0.013216632087061724231482387345D+00
        w(25) = 0.013706199506993971244060563234D+00
        w(26) = 0.014187700970062900419317230938D+00
        w(27) = 0.014660853117380060971041027493D+00
        w(28) = 0.015125377503587024690403432771D+00
        w(29) = 0.015581000760707523415881287558D+00
        w(30) = 0.016027454759014214436403950465D+00
        w(31) = 0.016464476764814667467169189640D+00
        w(32) = 0.016891809595063204177526208819D+00
        w(33) = 0.017309201768707240731293596444D+00
        w(34) = 0.017716407654678809269702031810D+00
        w(35) = 0.018113187616443980503999783812D+00
        w(36) = 0.018499308153024985727791918518D+00
        w(37) = 0.018874542036411948181617592169D+00
        w(38) = 0.019238668445283284085199492202D+00
        w(39) = 0.019591473094956024580283987216D+00
        w(40) = 0.019932748363489542089706675388D+00
        w(41) = 0.020262293413868438317104423081D+00
        w(42) = 0.020579914312192665948185517085D+00
        w(43) = 0.020885424141805311409990024684D+00
        w(44) = 0.021178643113290860912881038703D+00
        w(45) = 0.021459398670279205389981598196D+00
        w(46) = 0.021727525590993110687305178710D+00
        w(47) = 0.021982866085479386179554968899D+00
        w(48) = 0.022225269888466526554736910919D+00
        w(49) = 0.022454594347794176432066564511D+00
        w(50) = 0.022670704508362374313093970958D+00
        w(51) = 0.022873473191551169638592083492D+00
        w(52) = 0.023062781070063872924670495006D+00
        w(53) = 0.023238516738149892544490435771D+00
        w(54) = 0.023400576777165831146714346635D+00
        w(55) = 0.023548865816436258377269094263D+00
        w(56) = 0.023683296589378342897341543485D+00
        w(57) = 0.023803789984857314051325299744D+00
        w(58) = 0.023910275093742530302367230296D+00
        w(59) = 0.024002689250636756075547029720D+00
        w(60) = 0.024080978070754089272959634041D+00
        w(61) = 0.024145095481924836783843156014D+00
        w(62) = 0.024195003751708503129818111597D+00
        w(63) = 0.024230673509598936275508460625D+00
        w(64) = 0.024252083764308562906498864071D+00
        w(65) = 0.02425922191612154143202867472D+00
        w(66) = 0.024252083764308562906498864071D+00
        w(67) = 0.024230673509598936275508460625D+00
        w(68) = 0.024195003751708503129818111597D+00
        w(69) = 0.024145095481924836783843156014D+00
        w(70) = 0.024080978070754089272959634041D+00
        w(71) = 0.024002689250636756075547029720D+00
        w(72) = 0.023910275093742530302367230296D+00
        w(73) = 0.023803789984857314051325299744D+00
        w(74) = 0.023683296589378342897341543485D+00
        w(75) = 0.023548865816436258377269094263D+00
        w(76) = 0.023400576777165831146714346635D+00
        w(77) = 0.023238516738149892544490435771D+00
        w(78) = 0.023062781070063872924670495006D+00
        w(79) = 0.022873473191551169638592083492D+00
        w(80) = 0.022670704508362374313093970958D+00
        w(81) = 0.022454594347794176432066564511D+00
        w(82) = 0.022225269888466526554736910919D+00
        w(83) = 0.021982866085479386179554968899D+00
        w(84) = 0.021727525590993110687305178710D+00
        w(85) = 0.021459398670279205389981598196D+00
        w(86) = 0.021178643113290860912881038703D+00
        w(87) = 0.020885424141805311409990024684D+00
        w(88) = 0.020579914312192665948185517085D+00
        w(89) = 0.020262293413868438317104423081D+00
        w(90) = 0.019932748363489542089706675388D+00
        w(91) = 0.019591473094956024580283987216D+00
        w(92) = 0.019238668445283284085199492202D+00
        w(93) = 0.018874542036411948181617592169D+00
        w(94) = 0.018499308153024985727791918518D+00
        w(95) = 0.018113187616443980503999783812D+00
        w(96) = 0.017716407654678809269702031810D+00
        w(97) = 0.017309201768707240731293596444D+00
        w(98) = 0.016891809595063204177526208819D+00
        w(99) = 0.016464476764814667467169189640D+00
        w(100) = 0.016027454759014214436403950465D+00
        w(101) = 0.015581000760707523415881287558D+00
        w(102) = 0.015125377503587024690403432771D+00
        w(103) = 0.014660853117380060971041027493D+00
        w(104) = 0.014187700970062900419317230938D+00
        w(105) = 0.013706199506993971244060563234D+00
        w(106) = 0.013216632087061724231482387345D+00
        w(107) = 0.012719286815944623465099036330D+00
        w(108) = 0.012214456376582979416221105914D+00
        w(109) = 0.011702437856964778185746436834D+00
        w(110) = 0.0111835325753305049735380697538D+00
        w(111) = 0.0106580459029055185304204093001D+00
        w(112) = 0.0101262870842733548093160774580D+00
        w(113) = 0.0095885690555104190787301294510D+00
        w(114) = 0.0090452082602137316404219313819D+00
        w(115) = 0.0084965244635723279730542832506D+00
        w(116) = 0.0079428405646668029041114107832D+00
        w(117) = 0.0073844824072454014447165055698D+00
        w(118) = 0.0068217785893519121070498527769D+00
        w(119) = 0.0062550602724461408889348709586D+00
        w(120) = 0.0056846609912469045788016012203D+00
        w(121) = 0.0051109164669246267289761565766D+00
        w(122) = 0.0045341644298525434513226874954D+00
        w(123) = 0.0039547444682113562172392974765D+00
        w(124) = 0.0033729979506246246117755709288D+00
        w(125) = 0.0027892681877797554940944677057D+00
        w(126) = 0.0022039015180966937075786419741D+00
        w(127) = 0.0016172530556785534682413679271D+00
        w(128) = 0.00102972844619622394463273519315D+00
        w(129) = 0.00044246794182939296923668005717D+00

      else if ( n .eq. 255 ) then

        x(1) = -0.999955705317563751730191D+00
        x(2) = -0.999766621312000569367063D+00
        x(3) = -0.999426474680169959344386D+00
        x(4) = -0.998935241284654635142155D+00
        x(5) = -0.998292986136967889228248D+00
        x(6) = -0.997499804126615814044844D+00
        x(7) = -0.996555814435198617028738D+00
        x(8) = -0.995461159480026294089975D+00
        x(9) = -0.994216004616630164799381D+00
        x(10) = -0.992820538021989138984811D+00
        x(11) = -0.991274970630385567164523D+00
        x(12) = -0.989579536085920123498574D+00
        x(13) = -0.987734490699732356281248D+00
        x(14) = -0.985740113407419277752900D+00
        x(15) = -0.983596705724776358640192D+00
        x(16) = -0.981304591701017185126565D+00
        x(17) = -0.978864117869068155239121D+00
        x(18) = -0.976275653192735980815246D+00
        x(19) = -0.973539589010643617645393D+00
        x(20) = -0.970656338976880365477697D+00
        x(21) = -0.967626338998338798105523D+00
        x(22) = -0.964450047168726298761719D+00
        x(23) = -0.961127943699247839572910D+00
        x(24) = -0.957660530845962076295490D+00
        x(25) = -0.954048332833816317950921D+00
        x(26) = -0.950291895777368285733522D+00
        x(27) = -0.946391787598204251752103D+00
        x(28) = -0.942348597939064408301480D+00
        x(29) = -0.938162938074687317626793D+00
        x(30) = -0.933835440819386124349338D+00
        x(31) = -0.929366760431369935739045D+00
        x(32) = -0.924757572513824425220425D+00
        x(33) = -0.920008573912766315142721D+00
        x(34) = -0.915120482611686961035103D+00
        x(35) = -0.910094037623000801254172D+00
        x(36) = -0.904929998876314959753358D+00
        x(37) = -0.899629147103536800144342D+00
        x(38) = -0.894192283720836729335637D+00
        x(39) = -0.888620230707484040924981D+00
        x(40) = -0.882913830481574073645470D+00
        x(41) = -0.877073945772665439532627D+00
        x(42) = -0.871101459491346550796200D+00
        x(43) = -0.864997274595751144137121D+00
        x(44) = -0.858762313955042966785823D+00
        x(45) = -0.852397520209890250084237D+00
        x(46) = -0.845903855629951054143931D+00
        x(47) = -0.839282301968391021084600D+00
        x(48) = -0.832533860313455524647230D+00
        x(49) = -0.825659550937118650611534D+00
        x(50) = -0.818660413140831885432406D+00
        x(51) = -0.811537505098395829833580D+00
        x(52) = -0.804291903695978689734633D+00
        x(53) = -0.796924704369305728807154D+00
        x(54) = -0.789437020938044295117764D+00
        x(55) = -0.781829985437409458675147D+00
        x(56) = -0.774104747947015717207115D+00
        x(57) = -0.766262476417000644100858D+00
        x(58) = -0.758304356491446765092016D+00
        x(59) = -0.750231591329128358931528D+00
        x(60) = -0.742045401421610281838045D+00
        x(61) = -0.733747024408726316001889D+00
        x(62) = -0.725337714891464938687812D+00
        x(63) = -0.716818744242290800531501D+00
        x(64) = -0.708191400412930589382399D+00
        x(65) = -0.699456987739652339456557D+00
        x(66) = -0.690616826746067624571761D+00
        x(67) = -0.681672253943486448787259D+00
        x(68) = -0.672624621628855017806731D+00
        x(69) = -0.663475297680306939970658D+00
        x(70) = -0.654225665350358766508700D+00
        x(71) = -0.644877123056781136890077D+00
        x(72) = -0.635431084171177146547142D+00
        x(73) = -0.625888976805299900901619D+00
        x(74) = -0.616252243595141561442344D+00
        x(75) = -0.606522341482826526536576D+00
        x(76) = -0.596700741496341721653202D+00
        x(77) = -0.586788928527137300685706D+00
        x(78) = -0.576788401105631382036211D+00
        x(79) = -0.566700671174652760010815D+00
        x(80) = -0.556527263860855843833077D+00
        x(81) = -0.546269717244142383159817D+00
        x(82) = -0.535929582125124840335150D+00
        x(83) = -0.525508421790666565699453D+00
        x(84) = -0.515007811777534223035005D+00
        x(85) = -0.504429339634198197635551D+00
        x(86) = -0.493774604680816999489812D+00
        x(87) = -0.483045217767441948626854D+00
        x(88) = -0.472242801030478698742627D+00
        x(89) = -0.461368987647442418771401D+00
        x(90) = -0.450425421590043710043279D+00
        x(91) = -0.439413757375642589040685D+00
        x(92) = -0.428335659817108112494341D+00
        x(93) = -0.417192803771121462605751D+00
        x(94) = -0.405986873884960545511889D+00
        x(95) = -0.394719564341804385683361D+00
        x(96) = -0.383392578604595822734854D+00
        x(97) = -0.372007629158501235092510D+00
        x(98) = -0.360566437252006227074021D+00
        x(99) = -0.349070732636686422161576D+00
        x(100) = -0.337522253305692705554261D+00
        x(101) = -0.325922745230990453444769D+00
        x(102) = -0.314273962099392474845918D+00
        x(103) = -0.302577665047425574167140D+00
        x(104) = -0.290835622395070819082047D+00
        x(105) = -0.279049609378417768508970D+00
        x(106) = -0.267221407881273079721012D+00
        x(107) = -0.255352806165764071686080D+00
        x(108) = -0.243445598601977973686482D+00
        x(109) = -0.231501585396677734059116D+00
        x(110) = -0.219522572321135403508985D+00
        x(111) = -0.207510370438124240859625D+00
        x(112) = -0.195466795828110816293869D+00
        x(113) = -0.183393669314688508087976D+00
        x(114) = -0.171292816189293903533225D+00
        x(115) = -0.159166065935247723154292D+00
        x(116) = -0.147015251951161989456661D+00
        x(117) = -0.134842211273755257250625D+00
        x(118) = -0.122648784300117812092492D+00
        x(119) = -0.110436814509468826540991D+00
        x(120) = -0.098208148184447540736015D+00
        x(121) = -0.085964634131980604256000D+00
        x(122) = -0.073708123403767780288977D+00
        x(123) = -0.061440469016428270850728D+00
        x(124) = -0.049163525671349973093019D+00
        x(125) = -0.036879149474284021657652D+00
        x(126) = -0.024589197654727010541405D+00
        x(127) = -0.012295528285133320036860D+00
        x(128) = 0.000000000000000000000000D+00
        x(129) = 0.012295528285133320036860D+00
        x(130) = 0.024589197654727010541405D+00
        x(131) = 0.036879149474284021657652D+00
        x(132) = 0.049163525671349973093019D+00
        x(133) = 0.061440469016428270850728D+00
        x(134) = 0.073708123403767780288977D+00
        x(135) = 0.085964634131980604256000D+00
        x(136) = 0.098208148184447540736015D+00
        x(137) = 0.110436814509468826540991D+00
        x(138) = 0.122648784300117812092492D+00
        x(139) = 0.134842211273755257250625D+00
        x(140) = 0.147015251951161989456661D+00
        x(141) = 0.159166065935247723154292D+00
        x(142) = 0.171292816189293903533225D+00
        x(143) = 0.183393669314688508087976D+00
        x(144) = 0.195466795828110816293869D+00
        x(145) = 0.207510370438124240859625D+00
        x(146) = 0.219522572321135403508985D+00
        x(147) = 0.231501585396677734059116D+00
        x(148) = 0.243445598601977973686482D+00
        x(149) = 0.255352806165764071686080D+00
        x(150) = 0.267221407881273079721012D+00
        x(151) = 0.279049609378417768508970D+00
        x(152) = 0.290835622395070819082047D+00
        x(153) = 0.302577665047425574167140D+00
        x(154) = 0.314273962099392474845918D+00
        x(155) = 0.325922745230990453444769D+00
        x(156) = 0.337522253305692705554261D+00
        x(157) = 0.349070732636686422161576D+00
        x(158) = 0.360566437252006227074021D+00
        x(159) = 0.372007629158501235092510D+00
        x(160) = 0.383392578604595822734854D+00
        x(161) = 0.394719564341804385683361D+00
        x(162) = 0.405986873884960545511889D+00
        x(163) = 0.417192803771121462605751D+00
        x(164) = 0.428335659817108112494341D+00
        x(165) = 0.439413757375642589040685D+00
        x(166) = 0.450425421590043710043279D+00
        x(167) = 0.461368987647442418771401D+00
        x(168) = 0.472242801030478698742627D+00
        x(169) = 0.483045217767441948626854D+00
        x(170) = 0.493774604680816999489812D+00
        x(171) = 0.504429339634198197635551D+00
        x(172) = 0.515007811777534223035005D+00
        x(173) = 0.525508421790666565699453D+00
        x(174) = 0.535929582125124840335150D+00
        x(175) = 0.546269717244142383159817D+00
        x(176) = 0.556527263860855843833077D+00
        x(177) = 0.566700671174652760010815D+00
        x(178) = 0.576788401105631382036211D+00
        x(179) = 0.586788928527137300685706D+00
        x(180) = 0.596700741496341721653202D+00
        x(181) = 0.606522341482826526536576D+00
        x(182) = 0.616252243595141561442344D+00
        x(183) = 0.625888976805299900901619D+00
        x(184) = 0.635431084171177146547142D+00
        x(185) = 0.644877123056781136890077D+00
        x(186) = 0.654225665350358766508700D+00
        x(187) = 0.663475297680306939970658D+00
        x(188) = 0.672624621628855017806731D+00
        x(189) = 0.681672253943486448787259D+00
        x(190) = 0.690616826746067624571761D+00
        x(191) = 0.699456987739652339456557D+00
        x(192) = 0.708191400412930589382399D+00
        x(193) = 0.716818744242290800531501D+00
        x(194) = 0.725337714891464938687812D+00
        x(195) = 0.733747024408726316001889D+00
        x(196) = 0.742045401421610281838045D+00
        x(197) = 0.750231591329128358931528D+00
        x(198) = 0.758304356491446765092016D+00
        x(199) = 0.766262476417000644100858D+00
        x(200) = 0.774104747947015717207115D+00
        x(201) = 0.781829985437409458675147D+00
        x(202) = 0.789437020938044295117764D+00
        x(203) = 0.796924704369305728807154D+00
        x(204) = 0.804291903695978689734633D+00
        x(205) = 0.811537505098395829833580D+00
        x(206) = 0.818660413140831885432406D+00
        x(207) = 0.825659550937118650611534D+00
        x(208) = 0.832533860313455524647230D+00
        x(209) = 0.839282301968391021084600D+00
        x(210) = 0.845903855629951054143931D+00
        x(211) = 0.852397520209890250084237D+00
        x(212) = 0.858762313955042966785823D+00
        x(213) = 0.864997274595751144137121D+00
        x(214) = 0.871101459491346550796200D+00
        x(215) = 0.877073945772665439532627D+00
        x(216) = 0.882913830481574073645470D+00
        x(217) = 0.888620230707484040924981D+00
        x(218) = 0.894192283720836729335637D+00
        x(219) = 0.899629147103536800144342D+00
        x(220) = 0.904929998876314959753358D+00
        x(221) = 0.910094037623000801254172D+00
        x(222) = 0.915120482611686961035103D+00
        x(223) = 0.920008573912766315142721D+00
        x(224) = 0.924757572513824425220425D+00
        x(225) = 0.929366760431369935739045D+00
        x(226) = 0.933835440819386124349338D+00
        x(227) = 0.938162938074687317626793D+00
        x(228) = 0.942348597939064408301480D+00
        x(229) = 0.946391787598204251752103D+00
        x(230) = 0.950291895777368285733522D+00
        x(231) = 0.954048332833816317950921D+00
        x(232) = 0.957660530845962076295490D+00
        x(233) = 0.961127943699247839572910D+00
        x(234) = 0.964450047168726298761719D+00
        x(235) = 0.967626338998338798105523D+00
        x(236) = 0.970656338976880365477697D+00
        x(237) = 0.973539589010643617645393D+00
        x(238) = 0.976275653192735980815246D+00
        x(239) = 0.978864117869068155239121D+00
        x(240) = 0.981304591701017185126565D+00
        x(241) = 0.983596705724776358640192D+00
        x(242) = 0.985740113407419277752900D+00
        x(243) = 0.987734490699732356281248D+00
        x(244) = 0.989579536085920123498574D+00
        x(245) = 0.991274970630385567164523D+00
        x(246) = 0.992820538021989138984811D+00
        x(247) = 0.994216004616630164799381D+00
        x(248) = 0.995461159480026294089975D+00
        x(249) = 0.996555814435198617028738D+00
        x(250) = 0.997499804126615814044844D+00
        x(251) = 0.998292986136967889228248D+00
        x(252) = 0.998935241284654635142155D+00
        x(253) = 0.999426474680169959344386D+00
        x(254) = 0.999766621312000569367063D+00
        x(255) = 0.999955705317563751730191D+00

        w(1) = 0.00011367361999142272115645954414D+00
        w(2) = 0.00026459387119083065532790838855D+00
        w(3) = 0.00041569762526823913616284210066D+00
        w(4) = 0.00056675794564824918946626058353D+00
        w(5) = 0.00071773647800611087798371518325D+00
        w(6) = 0.00086860766611945667949717690640D+00
        w(7) = 0.00101934797642732530281229369360D+00
        w(8) = 0.0011699343729388079886897709773D+00
        w(9) = 0.0013203439900221692090523602144D+00
        w(10) = 0.0014705540427783843160097204304D+00
        w(11) = 0.0016205417990415653896921100325D+00
        w(12) = 0.0017702845706603213070421243905D+00
        w(13) = 0.0019197597117132050055085980675D+00
        w(14) = 0.0020689446195015801533643667413D+00
        w(15) = 0.0022178167367540171700373764020D+00
        w(16) = 0.0023663535543962867157201855305D+00
        w(17) = 0.0025145326145997073931298921370D+00
        w(18) = 0.0026623315139717112732749157331D+00
        w(19) = 0.0028097279068204407457332299361D+00
        w(20) = 0.0029566995084575002760043344138D+00
        w(21) = 0.0031032240985191112621977893133D+00
        w(22) = 0.0032492795242943133198690930777D+00
        w(23) = 0.0033948437040533928255056951665D+00
        w(24) = 0.0035398946303722552150296713510D+00
        w(25) = 0.0036844103734499176530742235517D+00
        w(26) = 0.0038283690844171626400743524999D+00
        w(27) = 0.0039717489986349171988699773906D+00
        w(28) = 0.0041145284389812475901826468094D+00
        w(29) = 0.0042566858191260658425395494472D+00
        w(30) = 0.0043981996467927779838546384780D+00
        w(31) = 0.0045390485270061921259394035112D+00
        w(32) = 0.0046792111653260640506279893190D+00
        w(33) = 0.0048186663710656988918572043815D+00
        w(34) = 0.0049573930604950563104281084148D+00
        w(35) = 0.0050953702600278273039420404117D+00
        w(36) = 0.0052325771093919661294970523234D+00
        w(37) = 0.0053689928647831724787741258653D+00
        w(38) = 0.0055045969020008281904902120813D+00
        w(39) = 0.0056393687195659001929970994675D+00
        w(40) = 0.0057732879418203275712033691864D+00
        w(41) = 0.0059063343220074160130475409466D+00
        w(42) = 0.0060384877453327676663371666884D+00
        w(43) = 0.0061697282320052788060812561217D+00
        w(44) = 0.0063000359402577418025981070425D+00
        w(45) = 0.0064293911693465917826140832500D+00
        w(46) = 0.0065577743625303421548456356354D+00
        w(47) = 0.0066851661100262568757892743568D+00
        w(48) = 0.0068115471519448109954345674817D+00
        w(49) = 0.0069368983812014946719507501243D+00
        w(50) = 0.0070612008464055194979848418291D+00
        w(51) = 0.0071844357547249896530757997058D+00
        w(52) = 0.0073065844747281040972736443146D+00
        w(53) = 0.0074276285391999597581348419714D+00
        w(54) = 0.0075475496479345294426435656724D+00
        w(55) = 0.0076663296705013920315933272426D+00
        w(56) = 0.0077839506489867963897419914623D+00
        w(57) = 0.0079003948007086443529587296692D+00
        w(58) = 0.0080156445209049821352946484008D+00
        w(59) = 0.0081296823853955935356080649925D+00
        w(60) = 0.0082424911532162924158504385939D+00
        w(61) = 0.0083540537692255160718568405530D+00
        w(62) = 0.0084643533666828253227353760036D+00
        w(63) = 0.0085733732697989214067758505840D+00
        w(64) = 0.0086810969962567940901133439612D+00
        w(65) = 0.0087875082597036197689825483144D+00
        w(66) = 0.0088925909722130327769834298578D+00
        w(67) = 0.0089963292467173975949700110383D+00
        w(68) = 0.0090987073994097142025303711406D+00
        w(69) = 0.0091997099521147934060534414075D+00
        w(70) = 0.0092993216346293436285393234867D+00
        w(71) = 0.0093975273870306153500305317074D+00
        w(72) = 0.0094943123619532541442165010292D+00
        w(73) = 0.0095896619268340180657610209655D+00
        w(74) = 0.0096835616661240200035669970076D+00
        w(75) = 0.0097759973834681605268499842249D+00
        w(76) = 0.0098669551038514217128483481814D+00
        w(77) = 0.0099564210757116974565448593910D+00
        w(78) = 0.0100443817730188408231888789497D+00
        w(79) = 0.0101308238973196141129538950955D+00
        w(80) = 0.0102157343797482324629939488415D+00
        w(81) = 0.0102991003830021970147153502911D+00
        w(82) = 0.0103809093032831189224876935085D+00
        w(83) = 0.0104611487722022407735015844669D+00
        w(84) = 0.0105398066586503673262517188088D+00
        w(85) = 0.0106168710706319228563864391054D+00
        w(86) = 0.0106923303570628578226139809571D+00
        w(87) = 0.0107661731095321330311788312990D+00
        w(88) = 0.0108383881640265149842990798832D+00
        w(89) = 0.0109089646026184216450603134401D+00
        w(90) = 0.0109778917551165634377595759712D+00
        w(91) = 0.0110451592006791299277436662993D+00
        w(92) = 0.0111107567693892782875426356195D+00
        w(93) = 0.0111746745437926853557086684962D+00
        w(94) = 0.0112369028603969308303734810332D+00
        w(95) = 0.0112974323111324849102690558722D+00
        w(96) = 0.0113562537447750795009464486204D+00
        w(97) = 0.011413358268329247942299599697D+00
        w(98) = 0.011468737248372824084374355981D+00
        w(99) = 0.011522382312362197440930930031D+00
        w(100) = 0.011574285349898127083439539046D+00
        w(101) = 0.011624438513951922901227922331D+00
        w(102) = 0.011672834222051808845465154244D+00
        w(103) = 0.011719465157429288794653489478D+00
        w(104) = 0.011764324270125341726399410909D+00
        w(105) = 0.011807404778056278953532930501D+00
        w(106) = 0.011848700168039102281222824051D+00
        w(107) = 0.011888204196776208064673282076D+00
        w(108) = 0.011925910891799288293359117699D+00
        w(109) = 0.011961814552372285996633285380D+00
        w(110) = 0.011995909750353268455989686823D+00
        w(111) = 0.012028191331015087920350431142D+00
        w(112) = 0.012058654413824705751531083631D+00
        w(113) = 0.012087294393181062176578184854D+00
        w(114) = 0.012114106939111380091025793650D+00
        w(115) = 0.012139087997925797641334635250D+00
        w(116) = 0.012162233792830230614908682534D+00
        w(117) = 0.012183540824497371981177306326D+00
        w(118) = 0.012203005871595742256331865516D+00
        w(119) = 0.012220625991276710706457005806D+00
        w(120) = 0.012236398519619413758040249691D+00
        w(121) = 0.012250321072033503350218104906D+00
        w(122) = 0.012262391543619664338660618398D+00
        w(123) = 0.012272608109487846445745237751D+00
        w(124) = 0.012280969225033162644659793962D+00
        w(125) = 0.012287473626169412265336919908D+00
        w(126) = 0.012292120329520193516690694701D+00
        w(127) = 0.012294908632567576531532225710D+00
        w(128) = 0.01229583811375831445681490730D+00
        w(129) = 0.012294908632567576531532225710D+00
        w(130) = 0.012292120329520193516690694701D+00
        w(131) = 0.012287473626169412265336919908D+00
        w(132) = 0.012280969225033162644659793962D+00
        w(133) = 0.012272608109487846445745237751D+00
        w(134) = 0.012262391543619664338660618398D+00
        w(135) = 0.012250321072033503350218104906D+00
        w(136) = 0.012236398519619413758040249691D+00
        w(137) = 0.012220625991276710706457005806D+00
        w(138) = 0.012203005871595742256331865516D+00
        w(139) = 0.012183540824497371981177306326D+00
        w(140) = 0.012162233792830230614908682534D+00
        w(141) = 0.012139087997925797641334635250D+00
        w(142) = 0.012114106939111380091025793650D+00
        w(143) = 0.012087294393181062176578184854D+00
        w(144) = 0.012058654413824705751531083631D+00
        w(145) = 0.012028191331015087920350431142D+00
        w(146) = 0.011995909750353268455989686823D+00
        w(147) = 0.011961814552372285996633285380D+00
        w(148) = 0.011925910891799288293359117699D+00
        w(149) = 0.011888204196776208064673282076D+00
        w(150) = 0.011848700168039102281222824051D+00
        w(151) = 0.011807404778056278953532930501D+00
        w(152) = 0.011764324270125341726399410909D+00
        w(153) = 0.011719465157429288794653489478D+00
        w(154) = 0.011672834222051808845465154244D+00
        w(155) = 0.011624438513951922901227922331D+00
        w(156) = 0.011574285349898127083439539046D+00
        w(157) = 0.011522382312362197440930930031D+00
        w(158) = 0.011468737248372824084374355981D+00
        w(159) = 0.011413358268329247942299599697D+00
        w(160) = 0.0113562537447750795009464486204D+00
        w(161) = 0.0112974323111324849102690558722D+00
        w(162) = 0.0112369028603969308303734810332D+00
        w(163) = 0.0111746745437926853557086684962D+00
        w(164) = 0.0111107567693892782875426356195D+00
        w(165) = 0.0110451592006791299277436662993D+00
        w(166) = 0.0109778917551165634377595759712D+00
        w(167) = 0.0109089646026184216450603134401D+00
        w(168) = 0.0108383881640265149842990798832D+00
        w(169) = 0.0107661731095321330311788312990D+00
        w(170) = 0.0106923303570628578226139809571D+00
        w(171) = 0.0106168710706319228563864391054D+00
        w(172) = 0.0105398066586503673262517188088D+00
        w(173) = 0.0104611487722022407735015844669D+00
        w(174) = 0.0103809093032831189224876935085D+00
        w(175) = 0.0102991003830021970147153502911D+00
        w(176) = 0.0102157343797482324629939488415D+00
        w(177) = 0.0101308238973196141129538950955D+00
        w(178) = 0.0100443817730188408231888789497D+00
        w(179) = 0.0099564210757116974565448593910D+00
        w(180) = 0.0098669551038514217128483481814D+00
        w(181) = 0.0097759973834681605268499842249D+00
        w(182) = 0.0096835616661240200035669970076D+00
        w(183) = 0.0095896619268340180657610209655D+00
        w(184) = 0.0094943123619532541442165010292D+00
        w(185) = 0.0093975273870306153500305317074D+00
        w(186) = 0.0092993216346293436285393234867D+00
        w(187) = 0.0091997099521147934060534414075D+00
        w(188) = 0.0090987073994097142025303711406D+00
        w(189) = 0.0089963292467173975949700110383D+00
        w(190) = 0.0088925909722130327769834298578D+00
        w(191) = 0.0087875082597036197689825483144D+00
        w(192) = 0.0086810969962567940901133439612D+00
        w(193) = 0.0085733732697989214067758505840D+00
        w(194) = 0.0084643533666828253227353760036D+00
        w(195) = 0.0083540537692255160718568405530D+00
        w(196) = 0.0082424911532162924158504385939D+00
        w(197) = 0.0081296823853955935356080649925D+00
        w(198) = 0.0080156445209049821352946484008D+00
        w(199) = 0.0079003948007086443529587296692D+00
        w(200) = 0.0077839506489867963897419914623D+00
        w(201) = 0.0076663296705013920315933272426D+00
        w(202) = 0.0075475496479345294426435656724D+00
        w(203) = 0.0074276285391999597581348419714D+00
        w(204) = 0.0073065844747281040972736443146D+00
        w(205) = 0.0071844357547249896530757997058D+00
        w(206) = 0.0070612008464055194979848418291D+00
        w(207) = 0.0069368983812014946719507501243D+00
        w(208) = 0.0068115471519448109954345674817D+00
        w(209) = 0.0066851661100262568757892743568D+00
        w(210) = 0.0065577743625303421548456356354D+00
        w(211) = 0.0064293911693465917826140832500D+00
        w(212) = 0.0063000359402577418025981070425D+00
        w(213) = 0.0061697282320052788060812561217D+00
        w(214) = 0.0060384877453327676663371666884D+00
        w(215) = 0.0059063343220074160130475409466D+00
        w(216) = 0.0057732879418203275712033691864D+00
        w(217) = 0.0056393687195659001929970994675D+00
        w(218) = 0.0055045969020008281904902120813D+00
        w(219) = 0.0053689928647831724787741258653D+00
        w(220) = 0.0052325771093919661294970523234D+00
        w(221) = 0.0050953702600278273039420404117D+00
        w(222) = 0.0049573930604950563104281084148D+00
        w(223) = 0.0048186663710656988918572043815D+00
        w(224) = 0.0046792111653260640506279893190D+00
        w(225) = 0.0045390485270061921259394035112D+00
        w(226) = 0.0043981996467927779838546384780D+00
        w(227) = 0.0042566858191260658425395494472D+00
        w(228) = 0.0041145284389812475901826468094D+00
        w(229) = 0.0039717489986349171988699773906D+00
        w(230) = 0.0038283690844171626400743524999D+00
        w(231) = 0.0036844103734499176530742235517D+00
        w(232) = 0.0035398946303722552150296713510D+00
        w(233) = 0.0033948437040533928255056951665D+00
        w(234) = 0.0032492795242943133198690930777D+00
        w(235) = 0.0031032240985191112621977893133D+00
        w(236) = 0.0029566995084575002760043344138D+00
        w(237) = 0.0028097279068204407457332299361D+00
        w(238) = 0.0026623315139717112732749157331D+00
        w(239) = 0.0025145326145997073931298921370D+00
        w(240) = 0.0023663535543962867157201855305D+00
        w(241) = 0.0022178167367540171700373764020D+00
        w(242) = 0.0020689446195015801533643667413D+00
        w(243) = 0.0019197597117132050055085980675D+00
        w(244) = 0.0017702845706603213070421243905D+00
        w(245) = 0.0016205417990415653896921100325D+00
        w(246) = 0.0014705540427783843160097204304D+00
        w(247) = 0.0013203439900221692090523602144D+00
        w(248) = 0.0011699343729388079886897709773D+00
        w(249) = 0.00101934797642732530281229369360D+00
        w(250) = 0.00086860766611945667949717690640D+00
        w(251) = 0.00071773647800611087798371518325D+00
        w(252) = 0.00056675794564824918946626058353D+00
        w(253) = 0.00041569762526823913616284210066D+00
        w(254) = 0.00026459387119083065532790838855D+00
        w(255) = 0.00011367361999142272115645954414D+00

      else if ( n .eq. 256 ) then

        x(1) = -0.999956050018992230734801D+00
        x(2) = -0.999768437409263186104879D+00
        x(3) = -0.999430937466261408240854D+00
        x(4) = -0.998943525843408856555026D+00
        x(5) = -0.998306266473006444055500D+00
        x(6) = -0.997519252756720827563409D+00
        x(7) = -0.996582602023381540430504D+00
        x(8) = -0.995496454481096356592647D+00
        x(9) = -0.994260972922409664962878D+00
        x(10) = -0.992876342608822117143534D+00
        x(11) = -0.991342771207583086922189D+00
        x(12) = -0.989660488745065218319244D+00
        x(13) = -0.987829747564860608916488D+00
        x(14) = -0.985850822286125956479245D+00
        x(15) = -0.983724009760315496166686D+00
        x(16) = -0.981449629025464405769303D+00
        x(17) = -0.979028021257622038824238D+00
        x(18) = -0.976459549719234155621011D+00
        x(19) = -0.973744599704370405266079D+00
        x(20) = -0.970883578480743029320923D+00
        x(21) = -0.967876915228489454909004D+00
        x(22) = -0.964725060975706430932612D+00
        x(23) = -0.961428488530732144006407D+00
        x(24) = -0.957987692411178129365790D+00
        x(25) = -0.954403188769716241764448D+00
        x(26) = -0.950675515316628276363852D+00
        x(27) = -0.946805231239127481372052D+00
        x(28) = -0.942792917117462443183076D+00
        x(29) = -0.938639174837814804981926D+00
        x(30) = -0.934344627502003094292477D+00
        x(31) = -0.929909919334005641180246D+00
        x(32) = -0.925335715583316202872730D+00
        x(33) = -0.920622702425146495505047D+00
        x(34) = -0.915771586857490384526670D+00
        x(35) = -0.910783096595065011890907D+00
        x(36) = -0.905657979960144647082682D+00
        x(37) = -0.900397005770303544771620D+00
        x(38) = -0.895000963223084577441223D+00
        x(39) = -0.889470661777610888828677D+00
        x(40) = -0.883806931033158284859826D+00
        x(41) = -0.878010620604706543986435D+00
        x(42) = -0.872082599995488289130046D+00
        x(43) = -0.866023758466554519297515D+00
        x(44) = -0.859835004903376350696173D+00
        x(45) = -0.853517267679502965073036D+00
        x(46) = -0.847071494517296207187072D+00
        x(47) = -0.840498652345762713895068D+00
        x(48) = -0.833799727155504894348444D+00
        x(49) = -0.826975723850812514289093D+00
        x(50) = -0.820027666098917067403478D+00
        x(51) = -0.812956596176431543136410D+00
        x(52) = -0.805763574812998623257389D+00
        x(53) = -0.798449681032170758782543D+00
        x(54) = -0.791016011989545994546707D+00
        x(55) = -0.783463682808183820750670D+00
        x(56) = -0.775793826411325739132053D+00
        x(57) = -0.768007593352445635975891D+00
        x(58) = -0.760106151642655454941907D+00
        x(59) = -0.752090686575492059587530D+00
        x(60) = -0.743962400549111568455683D+00
        x(61) = -0.735722512885917834620373D+00
        x(62) = -0.727372259649652126586894D+00
        x(63) = -0.718912893459971448372640D+00
        x(64) = -0.710345683304543313394566D+00
        x(65) = -0.701671914348685159406084D+00
        x(66) = -0.692892887742576960105342D+00
        x(67) = -0.684009920426075953124877D+00
        x(68) = -0.675024344931162763855919D+00
        x(69) = -0.665937509182048559906408D+00
        x(70) = -0.656750776292973221887500D+00
        x(71) = -0.647465524363724862617016D+00
        x(72) = -0.638083146272911368668689D+00
        x(73) = -0.628605049469014975432210D+00
        x(74) = -0.619032655759261219430968D+00
        x(75) = -0.609367401096333939522311D+00
        x(76) = -0.599610735362968321730388D+00
        x(77) = -0.589764122154454300785786D+00
        x(78) = -0.579829038559082944921832D+00
        x(79) = -0.569806974936568759057668D+00
        x(80) = -0.559699434694481145136907D+00
        x(81) = -0.549507934062718557042427D+00
        x(82) = -0.539234001866059181127936D+00
        x(83) = -0.528879179294822261951476D+00
        x(84) = -0.518445019673674476221662D+00
        x(85) = -0.507933088228616036231925D+00
        x(86) = -0.497344961852181477119512D+00
        x(87) = -0.486682228866890350103621D+00
        x(88) = -0.475946488786983306390738D+00
        x(89) = -0.465139352078479313645570D+00
        x(90) = -0.454262439917589998774455D+00
        x(91) = -0.443317383947527357216926D+00
        x(92) = -0.432305826033741309953441D+00
        x(93) = -0.421229418017623824976812D+00
        x(94) = -0.410089821468716550006434D+00
        x(95) = -0.398888707435459127713463D+00
        x(96) = -0.387627756194515583637985D+00
        x(97) = -0.376308656998716390283056D+00
        x(98) = -0.364933107823654018533465D+00
        x(99) = -0.353502815112969989537790D+00
        x(100) = -0.342019493522371636480730D+00
        x(101) = -0.330484865662416976229187D+00
        x(102) = -0.318900661840106275631683D+00
        x(103) = -0.307268619799319076258610D+00
        x(104) = -0.295590484460135614563787D+00
        x(105) = -0.283868007657081741799766D+00
        x(106) = -0.272102947876336609505245D+00
        x(107) = -0.260297069991942541978561D+00
        x(108) = -0.248452145001056666833243D+00
        x(109) = -0.236569949758284018477508D+00
        x(110) = -0.224652266709131967147878D+00
        x(111) = -0.212700883622625957937040D+00
        x(112) = -0.200717593323126670068001D+00
        x(113) = -0.188704193421388826461504D+00
        x(114) = -0.176662486044901997403722D+00
        x(115) = -0.164594277567553849829285D+00
        x(116) = -0.152501378338656395374607D+00
        x(117) = -0.140385602411375885913025D+00
        x(118) = -0.128248767270607094742050D+00
        x(119) = -0.116092693560332804940735D+00
        x(120) = -0.103919204810509403639197D+00
        x(121) = -0.091730127163519552031146D+00
        x(122) = -0.079527289100232965903227D+00
        x(123) = -0.067312521165716400242290D+00
        x(124) = -0.055087655694633984104561D+00
        x(125) = -0.042854526536379098381242D+00
        x(126) = -0.030614968779979029366279D+00
        x(127) = -0.018370818478813665117926D+00
        x(128) = -0.006123912375189529501170D+00
        x(129) = 0.006123912375189529501170D+00
        x(130) = 0.018370818478813665117926D+00
        x(131) = 0.030614968779979029366279D+00
        x(132) = 0.042854526536379098381242D+00
        x(133) = 0.055087655694633984104561D+00
        x(134) = 0.067312521165716400242290D+00
        x(135) = 0.079527289100232965903227D+00
        x(136) = 0.091730127163519552031146D+00
        x(137) = 0.103919204810509403639197D+00
        x(138) = 0.116092693560332804940735D+00
        x(139) = 0.128248767270607094742050D+00
        x(140) = 0.140385602411375885913025D+00
        x(141) = 0.152501378338656395374607D+00
        x(142) = 0.164594277567553849829285D+00
        x(143) = 0.176662486044901997403722D+00
        x(144) = 0.188704193421388826461504D+00
        x(145) = 0.200717593323126670068001D+00
        x(146) = 0.212700883622625957937040D+00
        x(147) = 0.224652266709131967147878D+00
        x(148) = 0.236569949758284018477508D+00
        x(149) = 0.248452145001056666833243D+00
        x(150) = 0.260297069991942541978561D+00
        x(151) = 0.272102947876336609505245D+00
        x(152) = 0.283868007657081741799766D+00
        x(153) = 0.295590484460135614563787D+00
        x(154) = 0.307268619799319076258610D+00
        x(155) = 0.318900661840106275631683D+00
        x(156) = 0.330484865662416976229187D+00
        x(157) = 0.342019493522371636480730D+00
        x(158) = 0.353502815112969989537790D+00
        x(159) = 0.364933107823654018533465D+00
        x(160) = 0.376308656998716390283056D+00
        x(161) = 0.387627756194515583637985D+00
        x(162) = 0.398888707435459127713463D+00
        x(163) = 0.410089821468716550006434D+00
        x(164) = 0.421229418017623824976812D+00
        x(165) = 0.432305826033741309953441D+00
        x(166) = 0.443317383947527357216926D+00
        x(167) = 0.454262439917589998774455D+00
        x(168) = 0.465139352078479313645570D+00
        x(169) = 0.475946488786983306390738D+00
        x(170) = 0.486682228866890350103621D+00
        x(171) = 0.497344961852181477119512D+00
        x(172) = 0.507933088228616036231925D+00
        x(173) = 0.518445019673674476221662D+00
        x(174) = 0.528879179294822261951476D+00
        x(175) = 0.539234001866059181127936D+00
        x(176) = 0.549507934062718557042427D+00
        x(177) = 0.559699434694481145136907D+00
        x(178) = 0.569806974936568759057668D+00
        x(179) = 0.579829038559082944921832D+00
        x(180) = 0.589764122154454300785786D+00
        x(181) = 0.599610735362968321730388D+00
        x(182) = 0.609367401096333939522311D+00
        x(183) = 0.619032655759261219430968D+00
        x(184) = 0.628605049469014975432210D+00
        x(185) = 0.638083146272911368668689D+00
        x(186) = 0.647465524363724862617016D+00
        x(187) = 0.656750776292973221887500D+00
        x(188) = 0.665937509182048559906408D+00
        x(189) = 0.675024344931162763855919D+00
        x(190) = 0.684009920426075953124877D+00
        x(191) = 0.692892887742576960105342D+00
        x(192) = 0.701671914348685159406084D+00
        x(193) = 0.710345683304543313394566D+00
        x(194) = 0.718912893459971448372640D+00
        x(195) = 0.727372259649652126586894D+00
        x(196) = 0.735722512885917834620373D+00
        x(197) = 0.743962400549111568455683D+00
        x(198) = 0.752090686575492059587530D+00
        x(199) = 0.760106151642655454941907D+00
        x(200) = 0.768007593352445635975891D+00
        x(201) = 0.775793826411325739132053D+00
        x(202) = 0.783463682808183820750670D+00
        x(203) = 0.791016011989545994546707D+00
        x(204) = 0.798449681032170758782543D+00
        x(205) = 0.805763574812998623257389D+00
        x(206) = 0.812956596176431543136410D+00
        x(207) = 0.820027666098917067403478D+00
        x(208) = 0.826975723850812514289093D+00
        x(209) = 0.833799727155504894348444D+00
        x(210) = 0.840498652345762713895068D+00
        x(211) = 0.847071494517296207187072D+00
        x(212) = 0.853517267679502965073036D+00
        x(213) = 0.859835004903376350696173D+00
        x(214) = 0.866023758466554519297515D+00
        x(215) = 0.872082599995488289130046D+00
        x(216) = 0.878010620604706543986435D+00
        x(217) = 0.883806931033158284859826D+00
        x(218) = 0.889470661777610888828677D+00
        x(219) = 0.895000963223084577441223D+00
        x(220) = 0.900397005770303544771620D+00
        x(221) = 0.905657979960144647082682D+00
        x(222) = 0.910783096595065011890907D+00
        x(223) = 0.915771586857490384526670D+00
        x(224) = 0.920622702425146495505047D+00
        x(225) = 0.925335715583316202872730D+00
        x(226) = 0.929909919334005641180246D+00
        x(227) = 0.934344627502003094292477D+00
        x(228) = 0.938639174837814804981926D+00
        x(229) = 0.942792917117462443183076D+00
        x(230) = 0.946805231239127481372052D+00
        x(231) = 0.950675515316628276363852D+00
        x(232) = 0.954403188769716241764448D+00
        x(233) = 0.957987692411178129365790D+00
        x(234) = 0.961428488530732144006407D+00
        x(235) = 0.964725060975706430932612D+00
        x(236) = 0.967876915228489454909004D+00
        x(237) = 0.970883578480743029320923D+00
        x(238) = 0.973744599704370405266079D+00
        x(239) = 0.976459549719234155621011D+00
        x(240) = 0.979028021257622038824238D+00
        x(241) = 0.981449629025464405769303D+00
        x(242) = 0.983724009760315496166686D+00
        x(243) = 0.985850822286125956479245D+00
        x(244) = 0.987829747564860608916488D+00
        x(245) = 0.989660488745065218319244D+00
        x(246) = 0.991342771207583086922189D+00
        x(247) = 0.992876342608822117143534D+00
        x(248) = 0.994260972922409664962878D+00
        x(249) = 0.995496454481096356592647D+00
        x(250) = 0.996582602023381540430504D+00
        x(251) = 0.997519252756720827563409D+00
        x(252) = 0.998306266473006444055500D+00
        x(253) = 0.998943525843408856555026D+00
        x(254) = 0.999430937466261408240854D+00
        x(255) = 0.999768437409263186104879D+00
        x(256) = 0.999956050018992230734801D+00

        w(1) = 0.00011278901782227217551253887725D+00
        w(2) = 0.00026253494429644590628745756250D+00
        w(3) = 0.00041246325442617632843218583774D+00
        w(4) = 0.00056234895403140980281523674759D+00
        w(5) = 0.0007121541634733206669089891511D+00
        w(6) = 0.0008618537014200890378140934163D+00
        w(7) = 0.0010114243932084404526058128414D+00
        w(8) = 0.0011608435575677247239705981135D+00
        w(9) = 0.0013100886819025044578316804271D+00
        w(10) = 0.0014591373333107332010883864996D+00
        w(11) = 0.0016079671307493272424499395690D+00
        w(12) = 0.0017565557363307299936069145295D+00
        w(13) = 0.0019048808534997184044191411746D+00
        w(14) = 0.0020529202279661431745487818492D+00
        w(15) = 0.0022006516498399104996848834189D+00
        w(16) = 0.0023480529563273120170064609087D+00
        w(17) = 0.0024951020347037068508395354372D+00
        w(18) = 0.0026417768254274905641208292516D+00
        w(19) = 0.0027880553253277068805747610763D+00
        w(20) = 0.0029339155908297166460123254142D+00
        w(21) = 0.0030793357411993375832053528316D+00
        w(22) = 0.0032242939617941981570107134269D+00
        w(23) = 0.0033687685073155510120191062489D+00
        w(24) = 0.0035127377050563073309710549844D+00
        w(25) = 0.0036561799581425021693892413052D+00
        w(26) = 0.0037990737487662579981170192082D+00
        w(27) = 0.0039413976414088336277290349840D+00
        w(28) = 0.0040831302860526684085997759212D+00
        w(29) = 0.0042242504213815362723565049060D+00
        w(30) = 0.0043647368779680566815684200621D+00
        w(31) = 0.0045045685814478970686417923159D+00
        w(32) = 0.0046437245556800603139790923525D+00
        w(33) = 0.0047821839258926913729317340448D+00
        w(34) = 0.0049199259218138656695587765655D+00
        w(35) = 0.0050569298807868423875578160762D+00
        w(36) = 0.0051931752508692809303287536296D+00
        w(37) = 0.0053286415939159303170811114788D+00
        w(38) = 0.0054633085886443102775705318566D+00
        w(39) = 0.0055971560336829100775514452572D+00
        w(40) = 0.005730163850601437177384417555D+00
        w(41) = 0.005862312086922653060661598801D+00
        w(42) = 0.005993580919115338221127696870D+00
        w(43) = 0.006123950655567932542389081187D+00
        w(44) = 0.006253401739542401272063645975D+00
        w(45) = 0.006381914752107880570375164275D+00
        w(46) = 0.006509470415053660267809899951D+00
        w(47) = 0.006636049593781065044590038355D+00
        w(48) = 0.006761633300173798780927861108D+00
        w(49) = 0.006886202695446320346713323775D+00
        w(50) = 0.007009739092969822621234436194D+00
        w(51) = 0.007132223961075390071672422986D+00
        w(52) = 0.007253638925833913783829137214D+00
        w(53) = 0.007373965773812346437572440695D+00
        w(54) = 0.007493186454805883358599761133D+00
        w(55) = 0.007611283084545659461618719618D+00
        w(56) = 0.007728237947381555631110194958D+00
        w(57) = 0.007844033498939711866810316151D+00
        w(58) = 0.007958652368754348353613161227D+00
        w(59) = 0.008072077362873499500946974804D+00
        w(60) = 0.008184291466438269935619761004D+00
        w(61) = 0.008295277846235225425171412553D+00
        w(62) = 0.008405019853221535756180301698D+00
        w(63) = 0.008513501025022490693838354790D+00
        w(64) = 0.008620705088401014305368838410D+00
        w(65) = 0.008726615961698807140336632217D+00
        w(66) = 0.008831217757248750025318272685D+00
        w(67) = 0.008934494783758207548408417085D+00
        w(68) = 0.009036431548662873680227775572D+00
        w(69) = 0.009137012760450806402000472219D+00
        w(70) = 0.009236223330956302687378716714D+00
        w(71) = 0.009334048377623269712466014486D+00
        w(72) = 0.009430473225737752747352764482D+00
        w(73) = 0.009525483410629284811829685754D+00
        w(74) = 0.009619064679840727857162164401D+00
        w(75) = 0.009711202995266279964249670496D+00
        w(76) = 0.009801884535257327825498800250D+00
        w(77) = 0.009891095696695828602630683809D+00
        w(78) = 0.009978823097034910124733949495D+00
        w(79) = 0.010065053576306383309460978930D+00
        w(80) = 0.010149774199094865654634066042D+00
        w(81) = 0.010232972256478219656954857160D+00
        w(82) = 0.010314635267934015068260713997D+00
        w(83) = 0.010394750983211728997101725205D+00
        w(84) = 0.010473307384170403003569566927D+00
        w(85) = 0.010550292686581481517533575536D+00
        w(86) = 0.010625695341896561133961681801D+00
        w(87) = 0.010699504038979785603048200583D+00
        w(88) = 0.010771707705804626636653631927D+00
        w(89) = 0.010842295511114795995293477058D+00
        w(90) = 0.010911256866049039700796847788D+00
        w(91) = 0.010978581425729570637988203448D+00
        w(92) = 0.011044259090813901263517571044D+00
        w(93) = 0.011108280009009843630460815451D+00
        w(94) = 0.011170634576553449462710881938D+00
        w(95) = 0.011231313439649668572656802083D+00
        w(96) = 0.011290307495875509508367594121D+00
        w(97) = 0.011347607895545491941625714297D+00
        w(98) = 0.011403206043039185964847059552D+00
        w(99) = 0.011457093598090639152334392298D+00
        w(100) = 0.011509262477039497958586392439D+00
        w(101) = 0.011559704854043635772668656950D+00
        w(102) = 0.011608413162253105722084706677D+00
        w(103) = 0.011655380094945242121298939730D+00
        w(104) = 0.011700598606620740288189823359D+00
        w(105) = 0.011744061914060550305376732759D+00
        w(106) = 0.011785763497343426181690117627D+00
        w(107) = 0.011825697100823977771160737958D+00
        w(108) = 0.011863856734071078731904572908D+00
        w(109) = 0.011900236672766489754287204237D+00
        w(110) = 0.011934831459563562255873201696D+00
        w(111) = 0.011967635904905893729007282670D+00
        w(112) = 0.011998645087805811934536710071D+00
        w(113) = 0.012027854356582571161267533498D+00
        w(114) = 0.012055259329560149814347085327D+00
        w(115) = 0.012080855895724544655975183976D+00
        w(116) = 0.012104640215340463097757829736D+00
        w(117) = 0.012126608720527321034718492205D+00
        w(118) = 0.012146758115794459815559837664D+00
        w(119) = 0.012165085378535502061307291839D+00
        w(120) = 0.012181587759481772174047585032D+00
        w(121) = 0.012196262783114713518180974196D+00
        w(122) = 0.012209108248037240407514094371D+00
        w(123) = 0.012220122227303969191708737227D+00
        w(124) = 0.012229303068710278904146266083D+00
        w(125) = 0.012236649395040158109242574767D+00
        w(126) = 0.012242160104272800769728083260D+00
        w(127) = 0.012245834369747920142463857550D+00
        w(128) = 0.01224767164028975590407032649D+00
        w(129) = 0.01224767164028975590407032649D+00
        w(130) = 0.012245834369747920142463857550D+00
        w(131) = 0.012242160104272800769728083260D+00
        w(132) = 0.012236649395040158109242574767D+00
        w(133) = 0.012229303068710278904146266083D+00
        w(134) = 0.012220122227303969191708737227D+00
        w(135) = 0.012209108248037240407514094371D+00
        w(136) = 0.012196262783114713518180974196D+00
        w(137) = 0.012181587759481772174047585032D+00
        w(138) = 0.012165085378535502061307291839D+00
        w(139) = 0.012146758115794459815559837664D+00
        w(140) = 0.012126608720527321034718492205D+00
        w(141) = 0.012104640215340463097757829736D+00
        w(142) = 0.012080855895724544655975183976D+00
        w(143) = 0.012055259329560149814347085327D+00
        w(144) = 0.012027854356582571161267533498D+00
        w(145) = 0.011998645087805811934536710071D+00
        w(146) = 0.011967635904905893729007282670D+00
        w(147) = 0.011934831459563562255873201696D+00
        w(148) = 0.011900236672766489754287204237D+00
        w(149) = 0.011863856734071078731904572908D+00
        w(150) = 0.011825697100823977771160737958D+00
        w(151) = 0.011785763497343426181690117627D+00
        w(152) = 0.011744061914060550305376732759D+00
        w(153) = 0.011700598606620740288189823359D+00
        w(154) = 0.011655380094945242121298939730D+00
        w(155) = 0.011608413162253105722084706677D+00
        w(156) = 0.011559704854043635772668656950D+00
        w(157) = 0.011509262477039497958586392439D+00
        w(158) = 0.011457093598090639152334392298D+00
        w(159) = 0.011403206043039185964847059552D+00
        w(160) = 0.011347607895545491941625714297D+00
        w(161) = 0.011290307495875509508367594121D+00
        w(162) = 0.011231313439649668572656802083D+00
        w(163) = 0.011170634576553449462710881938D+00
        w(164) = 0.011108280009009843630460815451D+00
        w(165) = 0.011044259090813901263517571044D+00
        w(166) = 0.010978581425729570637988203448D+00
        w(167) = 0.010911256866049039700796847788D+00
        w(168) = 0.010842295511114795995293477058D+00
        w(169) = 0.010771707705804626636653631927D+00
        w(170) = 0.010699504038979785603048200583D+00
        w(171) = 0.010625695341896561133961681801D+00
        w(172) = 0.010550292686581481517533575536D+00
        w(173) = 0.010473307384170403003569566927D+00
        w(174) = 0.010394750983211728997101725205D+00
        w(175) = 0.010314635267934015068260713997D+00
        w(176) = 0.010232972256478219656954857160D+00
        w(177) = 0.010149774199094865654634066042D+00
        w(178) = 0.010065053576306383309460978930D+00
        w(179) = 0.009978823097034910124733949495D+00
        w(180) = 0.009891095696695828602630683809D+00
        w(181) = 0.009801884535257327825498800250D+00
        w(182) = 0.009711202995266279964249670496D+00
        w(183) = 0.009619064679840727857162164401D+00
        w(184) = 0.009525483410629284811829685754D+00
        w(185) = 0.009430473225737752747352764482D+00
        w(186) = 0.009334048377623269712466014486D+00
        w(187) = 0.009236223330956302687378716714D+00
        w(188) = 0.009137012760450806402000472219D+00
        w(189) = 0.009036431548662873680227775572D+00
        w(190) = 0.008934494783758207548408417085D+00
        w(191) = 0.008831217757248750025318272685D+00
        w(192) = 0.008726615961698807140336632217D+00
        w(193) = 0.008620705088401014305368838410D+00
        w(194) = 0.008513501025022490693838354790D+00
        w(195) = 0.008405019853221535756180301698D+00
        w(196) = 0.008295277846235225425171412553D+00
        w(197) = 0.008184291466438269935619761004D+00
        w(198) = 0.008072077362873499500946974804D+00
        w(199) = 0.007958652368754348353613161227D+00
        w(200) = 0.007844033498939711866810316151D+00
        w(201) = 0.007728237947381555631110194958D+00
        w(202) = 0.007611283084545659461618719618D+00
        w(203) = 0.007493186454805883358599761133D+00
        w(204) = 0.007373965773812346437572440695D+00
        w(205) = 0.007253638925833913783829137214D+00
        w(206) = 0.007132223961075390071672422986D+00
        w(207) = 0.007009739092969822621234436194D+00
        w(208) = 0.006886202695446320346713323775D+00
        w(209) = 0.006761633300173798780927861108D+00
        w(210) = 0.006636049593781065044590038355D+00
        w(211) = 0.006509470415053660267809899951D+00
        w(212) = 0.006381914752107880570375164275D+00
        w(213) = 0.006253401739542401272063645975D+00
        w(214) = 0.006123950655567932542389081187D+00
        w(215) = 0.005993580919115338221127696870D+00
        w(216) = 0.005862312086922653060661598801D+00
        w(217) = 0.005730163850601437177384417555D+00
        w(218) = 0.0055971560336829100775514452572D+00
        w(219) = 0.0054633085886443102775705318566D+00
        w(220) = 0.0053286415939159303170811114788D+00
        w(221) = 0.0051931752508692809303287536296D+00
        w(222) = 0.0050569298807868423875578160762D+00
        w(223) = 0.0049199259218138656695587765655D+00
        w(224) = 0.0047821839258926913729317340448D+00
        w(225) = 0.0046437245556800603139790923525D+00
        w(226) = 0.0045045685814478970686417923159D+00
        w(227) = 0.0043647368779680566815684200621D+00
        w(228) = 0.0042242504213815362723565049060D+00
        w(229) = 0.0040831302860526684085997759212D+00
        w(230) = 0.0039413976414088336277290349840D+00
        w(231) = 0.0037990737487662579981170192082D+00
        w(232) = 0.0036561799581425021693892413052D+00
        w(233) = 0.0035127377050563073309710549844D+00
        w(234) = 0.0033687685073155510120191062489D+00
        w(235) = 0.0032242939617941981570107134269D+00
        w(236) = 0.0030793357411993375832053528316D+00
        w(237) = 0.0029339155908297166460123254142D+00
        w(238) = 0.0027880553253277068805747610763D+00
        w(239) = 0.0026417768254274905641208292516D+00
        w(240) = 0.0024951020347037068508395354372D+00
        w(241) = 0.0023480529563273120170064609087D+00
        w(242) = 0.0022006516498399104996848834189D+00
        w(243) = 0.0020529202279661431745487818492D+00
        w(244) = 0.0019048808534997184044191411746D+00
        w(245) = 0.0017565557363307299936069145295D+00
        w(246) = 0.0016079671307493272424499395690D+00
        w(247) = 0.0014591373333107332010883864996D+00
        w(248) = 0.0013100886819025044578316804271D+00
        w(249) = 0.0011608435575677247239705981135D+00
        w(250) = 0.0010114243932084404526058128414D+00
        w(251) = 0.0008618537014200890378140934163D+00
        w(252) = 0.0007121541634733206669089891511D+00
        w(253) = 0.00056234895403140980281523674759D+00
        w(254) = 0.00041246325442617632843218583774D+00
        w(255) = 0.00026253494429644590628745756250D+00
        w(256) = 0.00011278901782227217551253887725D+00

      else if ( n .eq. 257 ) then

        x(1) = -0.999956390712330402472857D+00
        x(2) = -0.999770232390338019056053D+00
        x(3) = -0.999435348366365078441838D+00
        x(4) = -0.998951714093223210129834D+00
        x(5) = -0.998319392445383847808766D+00
        x(6) = -0.997538475365520218731818D+00
        x(7) = -0.996609078365487004512326D+00
        x(8) = -0.995531339486830143483750D+00
        x(9) = -0.994305419008553630362377D+00
        x(10) = -0.992931499332908653172844D+00
        x(11) = -0.991409784923101705201254D+00
        x(12) = -0.989740502257507526030375D+00
        x(13) = -0.987923899788618253106809D+00
        x(14) = -0.985960247902290665366669D+00
        x(15) = -0.983849838875444644048531D+00
        x(16) = -0.981592986831381877693095D+00
        x(17) = -0.979190027692327124191591D+00
        x(18) = -0.976641319128992592610888D+00
        x(19) = -0.973947240507062326750976D+00
        x(20) = -0.971108192830542793021113D+00
        x(21) = -0.968124598681952354372943D+00
        x(22) = -0.964996902159337170373447D+00
        x(23) = -0.961725568810109767190665D+00
        x(24) = -0.958311085561711847074814D+00
        x(25) = -0.954753960649106318830855D+00
        x(26) = -0.951054723539105826691801D+00
        x(27) = -0.947213924851546682950881D+00
        x(28) = -0.943232136277318328151464D+00
        x(29) = -0.939109950493259404355123D+00
        x(30) = -0.934847981073932324370129D+00
        x(31) = -0.930446862400288909805510D+00
        x(32) = -0.925907249565240289235888D+00
        x(33) = -0.921229818276144817520964D+00
        x(34) = -0.916415264754228313295468D+00
        x(35) = -0.911464305630951423630955D+00
        x(36) = -0.906377677841339419411308D+00
        x(37) = -0.901156138514290206476301D+00
        x(38) = -0.895800464859876809085345D+00
        x(39) = -0.890311454053661045810287D+00
        x(40) = -0.884689923118035575018750D+00
        x(41) = -0.878936708800611938658765D+00
        x(42) = -0.873052667449672679799858D+00
        x(43) = -0.867038674886706051812473D+00
        x(44) = -0.860895626276042275514686D+00
        x(45) = -0.854624435991610735314055D+00
        x(46) = -0.848226037480837936478636D+00
        x(47) = -0.841701383125706473284556D+00
        x(48) = -0.835051444100995681967937D+00
        x(49) = -0.828277210229725073186687D+00
        x(50) = -0.821379689835822056081139D+00
        x(51) = -0.814359909594035880004229D+00
        x(52) = -0.807218914377120130552073D+00
        x(53) = -0.799957767100306523636066D+00
        x(54) = -0.792577548563093144962574D+00
        x(55) = -0.785079357288370682385816D+00
        x(56) = -0.777464309358910595129671D+00
        x(57) = -0.769733538251239556788216D+00
        x(58) = -0.761888194666924898264210D+00
        x(59) = -0.753929446361296162339238D+00
        x(60) = -0.745858477969628263337895D+00
        x(61) = -0.737676490830812123299244D+00
        x(62) = -0.729384702808539030149808D+00
        x(63) = -0.720984348110025333531072D+00
        x(64) = -0.712476677102304460118510D+00
        x(65) = -0.703862956126113592426171D+00
        x(66) = -0.695144467307402713168813D+00
        x(67) = -0.686322508366494071200553D+00
        x(68) = -0.677398392424920474813593D+00
        x(69) = -0.668373447809971163711735D+00
        x(70) = -0.659249017856974352220492D+00
        x(71) = -0.650026460709345873208532D+00
        x(72) = -0.640707149116433684724434D+00
        x(73) = -0.631292470229188329449219D+00
        x(74) = -0.621783825393689760680446D+00
        x(75) = -0.612182629942561267650033D+00
        x(76) = -0.602490312984301547488097D+00
        x(77) = -0.592708317190566281032495D+00
        x(78) = -0.582838098581430874902446D+00
        x(79) = -0.572881126308666332759406D+00
        x(80) = -0.562838882437060514424546D+00
        x(81) = -0.552712861723817332466074D+00
        x(82) = -0.542504571396066721967792D+00
        x(83) = -0.532215530926518500400434D+00
        x(84) = -0.521847271807293510797499D+00
        x(85) = -0.511401337321965712746629D+00
        x(86) = -0.500879282315849152005553D+00
        x(87) = -0.490282672964564000798817D+00
        x(88) = -0.479613086540916117008992D+00
        x(89) = -0.468872111180124821505728D+00
        x(90) = -0.458061345643433838720630D+00
        x(91) = -0.447182399080140586238810D+00
        x(92) = -0.436236890788079234603398D+00
        x(93) = -0.425226449972593188682213D+00
        x(94) = -0.414152715504032866791986D+00
        x(95) = -0.403017335673814873281489D+00
        x(96) = -0.391821967949078874408131D+00
        x(97) = -0.380568278725978696070941D+00
        x(98) = -0.369257943081644365255611D+00
        x(99) = -0.357892644524852014873858D+00
        x(100) = -0.346474074745438764010632D+00
        x(101) = -0.335003933362499872399782D+00
        x(102) = -0.323483927671405649204085D+00
        x(103) = -0.311915772389675771851948D+00
        x(104) = -0.300301189401748840754520D+00
        x(105) = -0.288641907502685160168097D+00
        x(106) = -0.276939662140840894253032D+00
        x(107) = -0.265196195159551900488370D+00
        x(108) = -0.253413254537865690008131D+00
        x(109) = -0.241592594130360106108882D+00
        x(110) = -0.229735973406087448117604D+00
        x(111) = -0.217845157186682897983880D+00
        x(112) = -0.205921915383676231351599D+00
        x(113) = -0.193968022735045913454182D+00
        x(114) = -0.181985258541054792946197D+00
        x(115) = -0.169975406399406713716337D+00
        x(116) = -0.157940253939763465806087D+00
        x(117) = -0.145881592557661591770148D+00
        x(118) = -0.133801217147868654144405D+00
        x(119) = -0.121700925837218653121859D+00
        x(120) = -0.109582519716966361063898D+00
        x(121) = -0.097447802574700412082119D+00
        x(122) = -0.085298580625855050603929D+00
        x(123) = -0.073136662244860502573600D+00
        x(124) = -0.060963857695971986730406D+00
        x(125) = -0.048781978863817431238958D+00
        x(126) = -0.036592838983704002816750D+00
        x(127) = -0.024398252371723591403953D+00
        x(128) = -0.012200034154697423345412D+00
        x(129) = 0.000000000000000000000000D+00
        x(130) = 0.012200034154697423345412D+00
        x(131) = 0.024398252371723591403953D+00
        x(132) = 0.036592838983704002816750D+00
        x(133) = 0.048781978863817431238958D+00
        x(134) = 0.060963857695971986730406D+00
        x(135) = 0.073136662244860502573600D+00
        x(136) = 0.085298580625855050603929D+00
        x(137) = 0.097447802574700412082119D+00
        x(138) = 0.109582519716966361063898D+00
        x(139) = 0.121700925837218653121859D+00
        x(140) = 0.133801217147868654144405D+00
        x(141) = 0.145881592557661591770148D+00
        x(142) = 0.157940253939763465806087D+00
        x(143) = 0.169975406399406713716337D+00
        x(144) = 0.181985258541054792946197D+00
        x(145) = 0.193968022735045913454182D+00
        x(146) = 0.205921915383676231351599D+00
        x(147) = 0.217845157186682897983880D+00
        x(148) = 0.229735973406087448117604D+00
        x(149) = 0.241592594130360106108882D+00
        x(150) = 0.253413254537865690008131D+00
        x(151) = 0.265196195159551900488370D+00
        x(152) = 0.276939662140840894253032D+00
        x(153) = 0.288641907502685160168097D+00
        x(154) = 0.300301189401748840754520D+00
        x(155) = 0.311915772389675771851948D+00
        x(156) = 0.323483927671405649204085D+00
        x(157) = 0.335003933362499872399782D+00
        x(158) = 0.346474074745438764010632D+00
        x(159) = 0.357892644524852014873858D+00
        x(160) = 0.369257943081644365255611D+00
        x(161) = 0.380568278725978696070941D+00
        x(162) = 0.391821967949078874408131D+00
        x(163) = 0.403017335673814873281489D+00
        x(164) = 0.414152715504032866791986D+00
        x(165) = 0.425226449972593188682213D+00
        x(166) = 0.436236890788079234603398D+00
        x(167) = 0.447182399080140586238810D+00
        x(168) = 0.458061345643433838720630D+00
        x(169) = 0.468872111180124821505728D+00
        x(170) = 0.479613086540916117008992D+00
        x(171) = 0.490282672964564000798817D+00
        x(172) = 0.500879282315849152005553D+00
        x(173) = 0.511401337321965712746629D+00
        x(174) = 0.521847271807293510797499D+00
        x(175) = 0.532215530926518500400434D+00
        x(176) = 0.542504571396066721967792D+00
        x(177) = 0.552712861723817332466074D+00
        x(178) = 0.562838882437060514424546D+00
        x(179) = 0.572881126308666332759406D+00
        x(180) = 0.582838098581430874902446D+00
        x(181) = 0.592708317190566281032495D+00
        x(182) = 0.602490312984301547488097D+00
        x(183) = 0.612182629942561267650033D+00
        x(184) = 0.621783825393689760680446D+00
        x(185) = 0.631292470229188329449219D+00
        x(186) = 0.640707149116433684724434D+00
        x(187) = 0.650026460709345873208532D+00
        x(188) = 0.659249017856974352220492D+00
        x(189) = 0.668373447809971163711735D+00
        x(190) = 0.677398392424920474813593D+00
        x(191) = 0.686322508366494071200553D+00
        x(192) = 0.695144467307402713168813D+00
        x(193) = 0.703862956126113592426171D+00
        x(194) = 0.712476677102304460118510D+00
        x(195) = 0.720984348110025333531072D+00
        x(196) = 0.729384702808539030149808D+00
        x(197) = 0.737676490830812123299244D+00
        x(198) = 0.745858477969628263337895D+00
        x(199) = 0.753929446361296162339238D+00
        x(200) = 0.761888194666924898264210D+00
        x(201) = 0.769733538251239556788216D+00
        x(202) = 0.777464309358910595129671D+00
        x(203) = 0.785079357288370682385816D+00
        x(204) = 0.792577548563093144962574D+00
        x(205) = 0.799957767100306523636066D+00
        x(206) = 0.807218914377120130552073D+00
        x(207) = 0.814359909594035880004229D+00
        x(208) = 0.821379689835822056081139D+00
        x(209) = 0.828277210229725073186687D+00
        x(210) = 0.835051444100995681967937D+00
        x(211) = 0.841701383125706473284556D+00
        x(212) = 0.848226037480837936478636D+00
        x(213) = 0.854624435991610735314055D+00
        x(214) = 0.860895626276042275514686D+00
        x(215) = 0.867038674886706051812473D+00
        x(216) = 0.873052667449672679799858D+00
        x(217) = 0.878936708800611938658765D+00
        x(218) = 0.884689923118035575018750D+00
        x(219) = 0.890311454053661045810287D+00
        x(220) = 0.895800464859876809085345D+00
        x(221) = 0.901156138514290206476301D+00
        x(222) = 0.906377677841339419411308D+00
        x(223) = 0.911464305630951423630955D+00
        x(224) = 0.916415264754228313295468D+00
        x(225) = 0.921229818276144817520964D+00
        x(226) = 0.925907249565240289235888D+00
        x(227) = 0.930446862400288909805510D+00
        x(228) = 0.934847981073932324370129D+00
        x(229) = 0.939109950493259404355123D+00
        x(230) = 0.943232136277318328151464D+00
        x(231) = 0.947213924851546682950881D+00
        x(232) = 0.951054723539105826691801D+00
        x(233) = 0.954753960649106318830855D+00
        x(234) = 0.958311085561711847074814D+00
        x(235) = 0.961725568810109767190665D+00
        x(236) = 0.964996902159337170373447D+00
        x(237) = 0.968124598681952354372943D+00
        x(238) = 0.971108192830542793021113D+00
        x(239) = 0.973947240507062326750976D+00
        x(240) = 0.976641319128992592610888D+00
        x(241) = 0.979190027692327124191591D+00
        x(242) = 0.981592986831381877693095D+00
        x(243) = 0.983849838875444644048531D+00
        x(244) = 0.985960247902290665366669D+00
        x(245) = 0.987923899788618253106809D+00
        x(246) = 0.989740502257507526030375D+00
        x(247) = 0.991409784923101705201254D+00
        x(248) = 0.992931499332908653172844D+00
        x(249) = 0.994305419008553630362377D+00
        x(250) = 0.995531339486830143483750D+00
        x(251) = 0.996609078365487004512326D+00
        x(252) = 0.997538475365520218731818D+00
        x(253) = 0.998319392445383847808766D+00
        x(254) = 0.998951714093223210129834D+00
        x(255) = 0.999435348366365078441838D+00
        x(256) = 0.999770232390338019056053D+00
        x(257) = 0.999956390712330402472857D+00

        w(1) = 0.00011191470145601756450862287886D+00
        w(2) = 0.00026049995580176964436806680831D+00
        w(3) = 0.00040926648283531339591138751432D+00
        w(4) = 0.00055799120546880640169677292533D+00
        w(5) = 0.00070663671051592291949335494247D+00
        w(6) = 0.00085517818446696565626595950963D+00
        w(7) = 0.00100359280467969441299468763292D+00
        w(8) = 0.0011518582377826677880963146741D+00
        w(9) = 0.0012999523174235227389668643832D+00
        w(10) = 0.0014478529559255120065233994722D+00
        w(11) = 0.0015955381166175133369701690235D+00
        w(12) = 0.0017429858051468299509941139300D+00
        w(13) = 0.0018901740676190104269878470891D+00
        w(14) = 0.0020370809914723626741694800322D+00
        w(15) = 0.0021836847075455253317921866057D+00
        w(16) = 0.0023299633927021828561308282641D+00
        w(17) = 0.0024758952727301488651840215879D+00
        w(18) = 0.0026214586253808109266552781372D+00
        w(19) = 0.0027666317834818283552560256501D+00
        w(20) = 0.0029113931380877846359302447381D+00
        w(21) = 0.0030557211416493711130936102459D+00
        w(22) = 0.0031995943111899437356540290142D+00
        w(23) = 0.0033429912314827618499065991316D+00
        w(24) = 0.0034858905582247143702551557840D+00
        w(25) = 0.0036282710212037760873102463983D+00
        w(26) = 0.0037701114274582873548537007645D+00
        w(27) = 0.0039113906644266662571543468015D+00
        w(28) = 0.0040520877030864825223229951262D+00
        w(29) = 0.0041921816010820254766367595011D+00
        w(30) = 0.0043316515058396297504806208252D+00
        w(31) = 0.0044704766576701092218388764046D+00
        w(32) = 0.0046086363928577081326523656522D+00
        w(33) = 0.0047461101467350184936945641585D+00
        w(34) = 0.0048828774567433411142588306018D+00
        w(35) = 0.0050189179654779878773297516544D+00
        w(36) = 0.0051542114237180378340642003713D+00
        w(37) = 0.0052887376934400710240953933529D+00
        w(38) = 0.0054224767508154127788846727083D+00
        w(39) = 0.0055554086891904284012033890901D+00
        w(40) = 0.0056875137220494140577838938236D+00
        w(41) = 0.0058187721859596348346566361185D+00
        w(42) = 0.0059491645434980654366600347567D+00
        w(43) = 0.0060786713861593931405204596709D+00
        w(44) = 0.0062072734372448464599330978665D+00
        w(45) = 0.0063349515547314166407936938524D+00
        w(46) = 0.0064616867341210426397202932350D+00
        w(47) = 0.0065874601112693336961737372300D+00
        w(48) = 0.0067122529651934070221351960200D+00
        w(49) = 0.0068360467208584215286561508406D+00
        w(50) = 0.0069588229519423919043121805236D+00
        w(51) = 0.0070805633835788707705149901066D+00
        w(52) = 0.0072012498950770900730828552207D+00
        w(53) = 0.0073208645226191563361371026044D+00
        w(54) = 0.0074393894619338979090297315972D+00
        w(55) = 0.0075568070709469658838993300454D+00
        w(56) = 0.0076730998724067939537782250476D+00
        w(57) = 0.0077882505564860261212726654404D+00
        w(58) = 0.0079022419833580248574070864277D+00
        w(59) = 0.0080150571857480760504667455353D+00
        w(60) = 0.0081266793714589108764118189068D+00
        w(61) = 0.0082370919258701685661946145361D+00
        w(62) = 0.0083462784144114279413811886655D+00
        w(63) = 0.0084542225850084395379670551258D+00
        w(64) = 0.0085609083705021941391459209280D+00
        w(65) = 0.0086663198910404675908861979240D+00
        w(66) = 0.0087704414564414858792445834744D+00
        w(67) = 0.0088732575685293586050755892934D+00
        w(68) = 0.0089747529234409331997949023068D+00
        w(69) = 0.0090749124139037264846862498962D+00
        w(70) = 0.0091737211314845944854270065178D+00
        w(71) = 0.0092711643688088057725325917169D+00
        w(72) = 0.0093672276217491880067391857021D+00
        w(73) = 0.0094618965915850218253881576301D+00
        w(74) = 0.0095551571871303607110514249099D+00
        w(75) = 0.0096469955268314600363329731559D+00
        w(76) = 0.0097373979408330030783691793250D+00
        w(77) = 0.0098263509730128164423854701706D+00
        w(78) = 0.0099138413829847720250916955489D+00
        w(79) = 0.0099998561480695773850435626986D+00
        w(80) = 0.0100843824652331611676814627839D+00
        w(81) = 0.0101674077529923650568895461852D+00
        w(82) = 0.0102489196532876585918958554047D+00
        w(83) = 0.0103289060333225980974485876288D+00
        w(84) = 0.0104073549873697559257355517893D+00
        w(85) = 0.0104842548385428511997370260353D+00
        w(86) = 0.0105595941405348182788823332058D+00
        w(87) = 0.0106333616793215542382761147904D+00
        w(88) = 0.0107055464748310917616231511294D+00
        w(89) = 0.0107761377825779489945556541150D+00
        w(90) = 0.0108451250952624130885928632830D+00
        w(91) = 0.0109124981443345193856719616965D+00
        w(92) = 0.0109782469015224934483083029166D+00
        w(93) = 0.0110423615803254284301924654946D+00
        w(94) = 0.0111048326374699756056269264803D+00
        w(95) = 0.0111656507743308312328559850485D+00
        w(96) = 0.0112248069383148083152535688671D+00
        w(97) = 0.0112822923242082872447042603128D+00
        w(98) = 0.0113380983754878447625379269120D+00
        w(99) = 0.011392216785593866154247619654D+00
        w(100) = 0.011444639499166951104119199270D+00
        w(101) = 0.011495358713246929174010288914D+00
        w(102) = 0.011544366878434306436012137033D+00
        w(103) = 0.011591656700013970380783131035D+00
        w(104) = 0.011637221139040985841125311445D+00
        w(105) = 0.011681053413388320313049670635D+00
        w(106) = 0.011723146998756342723302879656D+00
        w(107) = 0.011763495629643945382264331878D+00
        w(108) = 0.011802093300281144573421477037D+00
        w(109) = 0.011838934265523020964443424791D+00
        w(110) = 0.011874013041704866779344562066D+00
        w(111) = 0.011907324407458412445505183140D+00
        w(112) = 0.011938863404489011222535627643D+00
        w(113) = 0.011968625338313666131272065445D+00
        w(114) = 0.011996605778959789329711050159D+00
        w(115) = 0.012022800561624589927558893338D+00
        w(116) = 0.012047205787294992091420946532D+00
        w(117) = 0.012069817823327991167612855626D+00
        w(118) = 0.012090633303991361438266420912D+00
        w(119) = 0.012109649130964635027950450318D+00
        w(120) = 0.012126862473800277391553601370D+00
        w(121) = 0.012142270770344990738801546574D+00
        w(122) = 0.012155871727121082685623083829D+00
        w(123) = 0.012167663319667843366755737416D+00
        w(124) = 0.012177643792842880196606249581D+00
        w(125) = 0.012185811661083365425569178819D+00
        w(126) = 0.012192165708627157605870499188D+00
        w(127) = 0.012196704989693764053654538465D+00
        w(128) = 0.012199428828625117371582840212D+00
        w(129) = 0.01220033681998614507777289232D+00
        w(130) = 0.012199428828625117371582840212D+00
        w(131) = 0.012196704989693764053654538465D+00
        w(132) = 0.012192165708627157605870499188D+00
        w(133) = 0.012185811661083365425569178819D+00
        w(134) = 0.012177643792842880196606249581D+00
        w(135) = 0.012167663319667843366755737416D+00
        w(136) = 0.012155871727121082685623083829D+00
        w(137) = 0.012142270770344990738801546574D+00
        w(138) = 0.012126862473800277391553601370D+00
        w(139) = 0.012109649130964635027950450318D+00
        w(140) = 0.012090633303991361438266420912D+00
        w(141) = 0.012069817823327991167612855626D+00
        w(142) = 0.012047205787294992091420946532D+00
        w(143) = 0.012022800561624589927558893338D+00
        w(144) = 0.011996605778959789329711050159D+00
        w(145) = 0.011968625338313666131272065445D+00
        w(146) = 0.011938863404489011222535627643D+00
        w(147) = 0.011907324407458412445505183140D+00
        w(148) = 0.011874013041704866779344562066D+00
        w(149) = 0.011838934265523020964443424791D+00
        w(150) = 0.011802093300281144573421477037D+00
        w(151) = 0.011763495629643945382264331878D+00
        w(152) = 0.011723146998756342723302879656D+00
        w(153) = 0.011681053413388320313049670635D+00
        w(154) = 0.011637221139040985841125311445D+00
        w(155) = 0.011591656700013970380783131035D+00
        w(156) = 0.011544366878434306436012137033D+00
        w(157) = 0.011495358713246929174010288914D+00
        w(158) = 0.011444639499166951104119199270D+00
        w(159) = 0.011392216785593866154247619654D+00
        w(160) = 0.0113380983754878447625379269120D+00
        w(161) = 0.0112822923242082872447042603128D+00
        w(162) = 0.0112248069383148083152535688671D+00
        w(163) = 0.0111656507743308312328559850485D+00
        w(164) = 0.0111048326374699756056269264803D+00
        w(165) = 0.0110423615803254284301924654946D+00
        w(166) = 0.0109782469015224934483083029166D+00
        w(167) = 0.0109124981443345193856719616965D+00
        w(168) = 0.0108451250952624130885928632830D+00
        w(169) = 0.0107761377825779489945556541150D+00
        w(170) = 0.0107055464748310917616231511294D+00
        w(171) = 0.0106333616793215542382761147904D+00
        w(172) = 0.0105595941405348182788823332058D+00
        w(173) = 0.0104842548385428511997370260353D+00
        w(174) = 0.0104073549873697559257355517893D+00
        w(175) = 0.0103289060333225980974485876288D+00
        w(176) = 0.0102489196532876585918958554047D+00
        w(177) = 0.0101674077529923650568895461852D+00
        w(178) = 0.0100843824652331611676814627839D+00
        w(179) = 0.0099998561480695773850435626986D+00
        w(180) = 0.0099138413829847720250916955489D+00
        w(181) = 0.0098263509730128164423854701706D+00
        w(182) = 0.0097373979408330030783691793250D+00
        w(183) = 0.0096469955268314600363329731559D+00
        w(184) = 0.0095551571871303607110514249099D+00
        w(185) = 0.0094618965915850218253881576301D+00
        w(186) = 0.0093672276217491880067391857021D+00
        w(187) = 0.0092711643688088057725325917169D+00
        w(188) = 0.0091737211314845944854270065178D+00
        w(189) = 0.0090749124139037264846862498962D+00
        w(190) = 0.0089747529234409331997949023068D+00
        w(191) = 0.0088732575685293586050755892934D+00
        w(192) = 0.0087704414564414858792445834744D+00
        w(193) = 0.0086663198910404675908861979240D+00
        w(194) = 0.0085609083705021941391459209280D+00
        w(195) = 0.0084542225850084395379670551258D+00
        w(196) = 0.0083462784144114279413811886655D+00
        w(197) = 0.0082370919258701685661946145361D+00
        w(198) = 0.0081266793714589108764118189068D+00
        w(199) = 0.0080150571857480760504667455353D+00
        w(200) = 0.0079022419833580248574070864277D+00
        w(201) = 0.0077882505564860261212726654404D+00
        w(202) = 0.0076730998724067939537782250476D+00
        w(203) = 0.0075568070709469658838993300454D+00
        w(204) = 0.0074393894619338979090297315972D+00
        w(205) = 0.0073208645226191563361371026044D+00
        w(206) = 0.0072012498950770900730828552207D+00
        w(207) = 0.0070805633835788707705149901066D+00
        w(208) = 0.0069588229519423919043121805236D+00
        w(209) = 0.0068360467208584215286561508406D+00
        w(210) = 0.0067122529651934070221351960200D+00
        w(211) = 0.0065874601112693336961737372300D+00
        w(212) = 0.0064616867341210426397202932350D+00
        w(213) = 0.0063349515547314166407936938524D+00
        w(214) = 0.0062072734372448464599330978665D+00
        w(215) = 0.0060786713861593931405204596709D+00
        w(216) = 0.0059491645434980654366600347567D+00
        w(217) = 0.0058187721859596348346566361185D+00
        w(218) = 0.0056875137220494140577838938236D+00
        w(219) = 0.0055554086891904284012033890901D+00
        w(220) = 0.0054224767508154127788846727083D+00
        w(221) = 0.0052887376934400710240953933529D+00
        w(222) = 0.0051542114237180378340642003713D+00
        w(223) = 0.0050189179654779878773297516544D+00
        w(224) = 0.0048828774567433411142588306018D+00
        w(225) = 0.0047461101467350184936945641585D+00
        w(226) = 0.0046086363928577081326523656522D+00
        w(227) = 0.0044704766576701092218388764046D+00
        w(228) = 0.0043316515058396297504806208252D+00
        w(229) = 0.0041921816010820254766367595011D+00
        w(230) = 0.0040520877030864825223229951262D+00
        w(231) = 0.0039113906644266662571543468015D+00
        w(232) = 0.0037701114274582873548537007645D+00
        w(233) = 0.0036282710212037760873102463983D+00
        w(234) = 0.0034858905582247143702551557840D+00
        w(235) = 0.0033429912314827618499065991316D+00
        w(236) = 0.0031995943111899437356540290142D+00
        w(237) = 0.0030557211416493711130936102459D+00
        w(238) = 0.0029113931380877846359302447381D+00
        w(239) = 0.0027666317834818283552560256501D+00
        w(240) = 0.0026214586253808109266552781372D+00
        w(241) = 0.0024758952727301488651840215879D+00
        w(242) = 0.0023299633927021828561308282641D+00
        w(243) = 0.0021836847075455253317921866057D+00
        w(244) = 0.0020370809914723626741694800322D+00
        w(245) = 0.0018901740676190104269878470891D+00
        w(246) = 0.0017429858051468299509941139300D+00
        w(247) = 0.0015955381166175133369701690235D+00
        w(248) = 0.0014478529559255120065233994722D+00
        w(249) = 0.0012999523174235227389668643832D+00
        w(250) = 0.0011518582377826677880963146741D+00
        w(251) = 0.00100359280467969441299468763292D+00
        w(252) = 0.00085517818446696565626595950963D+00
        w(253) = 0.00070663671051592291949335494247D+00
        w(254) = 0.00055799120546880640169677292533D+00
        w(255) = 0.00040926648283531339591138751432D+00
        w(256) = 0.00026049995580176964436806680831D+00
        w(257) = 0.00011191470145601756450862287886D+00

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LEGENDRE_SET - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal value of N = ', n
        write ( *, '(a)' ) '  Legal values are 1 through 33, ' //
     &    '63/64/65, 127/128/129 and 255/256/257.'
        stop

      end if

      return
      end
      subroutine map ( code, element_order, w )

c*********************************************************************72
c
cc MAP returns the interpolation matrix for any available element.
c
c  Formula:
c
c    For an element of order ELEMENT_ORDER, we suppose we are given 
c    ELEMENT_ORDER items of data Q associated with the nodes.
c
c   Let PHI(J)(R,S) be the Lagrange basis polynomial associated with 
c   node J.  PHI(J)(R,S) is 1 at node J, and 0 at each of the other nodes.
c
c   Let P(R,S) be the polynomial of ELEMENT_ORDER terms which interpolates the
c   data Q, that is,
c
c      P(R(J),S(J)) = Q(J)
c
c   where the coordinates of node J are (R(J),S(J)).  Then we know
c   that we can write
c
c     P(R,S) = sum ( 1 <= J <= ELEMENT_ORDER ) Q(J) * PHI(J)(R,S)
c
c   But P(R,S) also has a standard representation as
c
c     P(R,S) = sum ( 1 <= I <= ELEMENT_ORDER ) A(I) * R**REXP(I) * S**SEXP(I)
c
c   where REXP(I) and SEXP(I) are the exponents of R and S and
c   the A(I) are the appropriate coefficients.
c
c   The interpolation matrix W allows us to immediately compute
c   the standard basis coefficients A from the data Q to be interpolated
c   using the formula:
c
c      A(I) = sum ( 1 <= J <= ELEMENT_ORDER ) W(I,J) * Q(J)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
c    'T3', 'T4', 'T6' and 'T10'.
c
c    Input, integer ELEMENT_ORDER, the number of nodes per element.
c
c    Output, double precision W(ELEMENT_ORDER,ELEMENT_ORDER),
c     the interpolation matrix.
c
      implicit none

      integer element_order

      double precision area
      character * ( * ) code
      integer i
      integer info
      integer j
      integer pivot(element_order)
      double precision r(element_order)
      integer rexp(element_order)
      double precision rfact
      double precision s(element_order)
      integer sexp(element_order)
      double precision sfact
      double precision w(element_order,element_order)
c
c  Get the (R,S) location of the nodes.
c
      call node_reference ( code, r, s, area )
c
c  Get the associated monomials.
c
      call poly ( code, rexp, sexp )
c
c  Set up the Vandermonde matrix.
c  Factors of the form 0**0 are to be understood as 1.
c
      do i = 1, element_order
        do j = 1, element_order

          if ( rexp(j) .eq. 0 ) then
            rfact = 1.0D+00
          else
            rfact = r(i)**rexp(j)
          end if

          if ( sexp(j) .eq. 0 ) then
            sfact = 1.0D+00
          else
            sfact = s(i)**sexp(j)
          end if

          w(i,j) = rfact * sfact

        end do
      end do
c
c  Factor the Vandermonde matrix.
c
      call r8ge_fa ( element_order, w, pivot, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MAP - Fatal error!'
        write ( *, '(a)' ) '  The Vandermonde matrix is singular.'
        stop
      end if
c
c  Invert the Vandermonde matrix.
c
      call r8ge_inverse ( element_order, w, pivot )

      return
      end
      subroutine map_test ( code )

c*********************************************************************72
c
cc MAP_TEST tests the map routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, the code for the element.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
c    'T3', 'T4', 'T6' and 'T10'.
c
      implicit none

      integer element_order_max
      parameter ( element_order_max = 16 )

      character * ( * ) code
      integer element_order
      integer order_code
      logical s_eqi
      double precision w(element_order_max,element_order_max)

      if ( s_eqi ( code, 'T4' ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MAP_TEST - Warning!'
        write ( *, '(a)' ) '  Skipping test for element "' 
     &    // trim ( code ) // '".'
        return
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  MAP_TEST: interpolation matrix for element "' 
     &  // trim ( code ) // '".'
      write ( *, '(a)' ) ' '

      element_order = order_code ( code )

      call map ( code, element_order, w )

      call r8mat_print ( element_order, element_order, w, 
     &  '  The interpolation matrix:' );

      return
      end
      subroutine mass_matrix_t3 ( node_num, element_num, element_node, 
     &  node_xy, a )

c*********************************************************************72
c
cc MASS_MATRIX_T3 computes the mass matrix, using 3-node triangles.
c
c  Discussion:
c
c    The mass matrix to be estimated has the form:
c
c      A(I,J) = integral ( PHI(I)(X,Y) * PHI(J)(X,Y) ) d Region
c
c    where PHI(I) and PHI(J) are the shape functions associated with
c    the I-th and J-th variables.
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes per element.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(3,ELEMENT_NUM), the nodes that 
c    make up each element.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the coordinates 
c    of the nodes.
c
c    Output, double precision A(NODE_NUM,NODE_NUM), the mass matrix.
c
      implicit none

      integer element_num
      integer element_order
      parameter ( element_order = 3 )
      integer node_num
      integer quad_num
      parameter ( quad_num = 4 )

      double precision a(node_num,node_num)
      double precision area
      double precision dwdr(element_order)
      double precision dwds(element_order)
      integer element
      integer element_node(element_order,element_num)
      integer i
      integer ip
      integer iq
      integer j
      integer jp
      integer jq
      double precision node_xy(2,node_num)
      integer p1
      integer p2
      integer p3
      integer quad
      double precision r
      double precision rtab(quad_num)
      integer rule
      double precision s
      double precision stab(quad_num)
      integer triangle_unit_size
      double precision w(element_order)
      double precision weight(quad_num)
c
c  Zero out the matrix.
c
      do j = 1, node_num
        do i = 1, node_num
          a(1:node_num,1:node_num) = 0.0D+00
        end do
      end do
c
c  Get the weights and abscissas for a unit triangle.
c
      rule = 4
      call triangle_unit_set ( rule, rtab, stab, weight )
c
c  For each element.
c
      do element = 1, element_num

        p1 = element_node(1,element)
        p2 = element_node(2,element)
        p3 = element_node(3,element)

        area = 0.5D+00 * abs ( 
     &      node_xy(1,p1) * ( node_xy(2,p2) - node_xy(2,p3) ) 
     &    + node_xy(1,p2) * ( node_xy(2,p3) - node_xy(2,p1) ) 
     &    + node_xy(1,p3) * ( node_xy(2,p1) - node_xy(2,p2) ) )
    
        if ( area == 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MASS_MATRIX_T3 - Fatal error!'
          write ( *, '(a,i8)' ) '  Zero area for element ', element
          stop
        end if
c
c  For each quadrature point in the element...
c
        do quad = 1, quad_num

          r = rtab(quad)
          s = stab(quad)

          call shape_t3 ( r, s, w, dwdr, dwds )
c
c  For each basis function PHI(I) associated with a node in the element,
c
          do iq = 1, element_order

            ip = element_node(iq,element)
c
c  For each "neighbor" basis function PHI(J) associated with a node in
c  the element.
c
            do jq = 1, element_order

              jp = element_node(jq,element)

              a(ip,jp) = a(ip,jp) + area * weight(quad) * w(iq) * w(jq)

            end do
          end do
        end do
      end do

      return
      end
      subroutine mass_matrix_t6 ( node_num, element_num, element_node, 
     &  node_xy, a )

c*********************************************************************72
c
cc MASS_MATRIX_T6 computes the mass matrix, using 6-node triangles.
c
c  Discussion:
c
c    The mass matrix to be estimated has the form:
c
c      A(I,J) = integral ( PHI(I)(X,Y) * PHI(J)(X,Y) ) d Region
c
c    where PHI(I) and PHI(J) are the shape functions associated with
c    the I-th and J-th variables.
c
c  Reference Element T6:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  6  5
c    |  |   \
c    |  |    \
c    0  1--4--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes per element.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(6,ELEMENT_NUM), the nodes that 
c    make up each element.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the coordinates 
c    of the nodes.
c
c    Output, double precision A(NODE_NUM,NODE_NUM), the mass matrix.
c
      implicit none

      integer element_num
      integer element_order
      parameter ( element_order = 6 )
      integer node_num
      integer quad_num
      parameter ( quad_num = 12 )

      double precision a(node_num,node_num)
      double precision area
      double precision dwdr(element_order)
      double precision dwds(element_order)
      integer element
      integer element_node(element_order,element_num)
      integer i
      integer ip
      integer iq
      integer j
      integer jp
      integer jq
      double precision node_xy(2,node_num)
      integer p1
      integer p2
      integer p3
      integer quad
      double precision r
      double precision rtab(quad_num)
      integer rule
      double precision s
      double precision stab(quad_num)
      integer triangle_unit_size
      double precision w(element_order)
      double precision weight(quad_num)
c
c  Zero out the matrix.
c
      do j = 1, node_num
        do i = 1, node_num
          a(1:node_num,1:node_num) = 0.0D+00
        end do
      end do
c
c  Get the weights and abscissas for a unit triangle.
c
      rule = 12
      call triangle_unit_set ( rule, rtab, stab, weight )
c
c  For each element.
c
      do element = 1, element_num

        p1 = element_node(1,element)
        p2 = element_node(2,element)
        p3 = element_node(3,element)

        area = 0.5D+00 * abs ( 
     &      node_xy(1,p1) * ( node_xy(2,p2) - node_xy(2,p3) ) 
     &    + node_xy(1,p2) * ( node_xy(2,p3) - node_xy(2,p1) ) 
     &    + node_xy(1,p3) * ( node_xy(2,p1) - node_xy(2,p2) ) )

        if ( area .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MASS_MATRIX_T6 - Fatal error!'
          write ( *, '(a,i8)' ) '  Zero area for element ', element
          stop
        end if
c
c  For each quadrature point in the element...
c
        do quad = 1, quad_num

          r = rtab(quad)
          s = stab(quad)

          call shape_t6 ( r, s, w, dwdr, dwds )
c
c  For each basis function PHI(I) associated with a node in the element,
c
          do iq = 1, element_order

            ip = element_node(iq,element)
c
c  For each "neighbor" basis function PHI(J) associated with a node in
c  the element.
c
            do jq = 1, element_order

              jp = element_node(jq,element)

              a(ip,jp) = a(ip,jp) + area * weight(quad) * w(iq) * w(jq)

            end do
          end do
        end do
      end do

      return
      end
      function next_boundary_node ( node, code )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE returns the next boundary node in any element.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 October 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Input, character * ( * ) CODE, identifies the element desired.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
c    'T3', 'T4', 'T6' and 'T10'.
c
c    Output, integer NEXT_BOUNDARY_NODE, the index of the next 
c    edge node.
c
      implicit none

      character * ( * ) code
      integer next_boundary_node
      integer next_boundary_node_q4
      integer next_boundary_node_q8
      integer next_boundary_node_q9
      integer next_boundary_node_q12
      integer next_boundary_node_q16
      integer next_boundary_node_ql
      integer next_boundary_node_t3
      integer next_boundary_node_t4
      integer next_boundary_node_t6
      integer next_boundary_node_t10
      integer node
      logical s_eqi

      if ( s_eqi ( code, 'Q4' ) ) then
        next_boundary_node = next_boundary_node_q4 ( node )
      else if ( s_eqi ( code, 'Q8' ) ) then
        next_boundary_node = next_boundary_node_q8 ( node )
      else if ( s_eqi ( code, 'Q9' ) ) then
        next_boundary_node = next_boundary_node_q9 ( node )
      else if ( s_eqi ( code, 'Q12' ) ) then
        next_boundary_node = next_boundary_node_q12 ( node )
      else if ( s_eqi ( code, 'Q16' ) ) then
        next_boundary_node = next_boundary_node_q16 ( node )
      else if ( s_eqi ( code, 'QL' ) ) then
        next_boundary_node = next_boundary_node_ql ( node )
      else if ( s_eqi ( code, 'T3' ) ) then
        next_boundary_node = next_boundary_node_t3 ( node )
      else if ( s_eqi ( code, 'T4' ) ) then
        next_boundary_node = next_boundary_node_t4 ( node )
      else if ( s_eqi ( code, 'T6' ) ) then
        next_boundary_node = next_boundary_node_t6 ( node )
      else if ( s_eqi ( code, 'T10' ) ) then
        next_boundary_node = next_boundary_node_t10 ( node )
      else
        next_boundary_node = -1
      end if

      return
      end
      function next_boundary_node_q4 ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_Q4 returns the next boundary node in a Q4 element.
c
c  Reference Element Q4:
c
c    |
c    1  4-----3
c    |  |     |
c    |  |     |
c    S  |     |
c    |  |     |
c    |  |     |
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_Q4, the index of the next 
c    edge node.
c
      implicit none

      integer next_boundary_node_q4
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_q4 = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_q4 = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_q4 = 4
      else if ( node .eq. 4 ) then
        next_boundary_node_q4 = 1
      else
        next_boundary_node_q4 = -1
      end if

      return
      end
      function next_boundary_node_q8 ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_Q8 returns the next boundary node in a Q8 element.
c
c  Reference Element Q8:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8     6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_Q8, the index of the next 
c    edge node.
c
      implicit none

      integer next_boundary_node_q8
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_q8 = 5
      else if ( node .eq. 5 ) then
        next_boundary_node_q8 = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_q8 = 6
      else if ( node .eq. 6 ) then
        next_boundary_node_q8 = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_q8 = 7
      else if ( node .eq. 7 ) then
        next_boundary_node_q8 = 4
      else if ( node .eq. 4 ) then
        next_boundary_node_q8 = 8
      else if ( node .eq. 8 ) then
        next_boundary_node_q8 = 1
      else
        next_boundary_node_q8 = -1
      end if

      return
      end
      function next_boundary_node_q9 ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_Q9 returns the next boundary node in a Q9 element.
c
c  Reference Element Q9:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8  9  6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_Q9, the index of the next 
c    edge node.
c
      implicit none

      integer next_boundary_node_q9
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_q9 = 5
      else if ( node .eq. 5 ) then
        next_boundary_node_q9 = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_q9 = 6
      else if ( node .eq. 6 ) then
        next_boundary_node_q9 = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_q9 = 7
      else if ( node .eq. 7 ) then
        next_boundary_node_q9 = 4
      else if ( node .eq. 4 ) then
        next_boundary_node_q9 = 8
      else if ( node .eq. 8 ) then
        next_boundary_node_q9 = 1
      else 
        next_boundary_node_q9 = -1
      end if

      return
      end
      function next_boundary_node_q12 ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_Q12 returns the next boundary node in a Q12 element.
c
c  Reference Element Q12:
c
c    |
c    1  9-10-11-12
c    |  |        |
c    |  7        8
c    S  |        |
c    |  5        6
c    |  |        |
c    0  1--2--3--4
c    |
c    +--0---R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_Q12, the index of the 
c    next edge node.
c
      implicit none

      integer next_boundary_node_q12
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_q12 = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_q12 = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_q12 = 4
      else if ( node .eq. 4 ) then
        next_boundary_node_q12 = 6
      else if ( node .eq. 6 ) then
        next_boundary_node_q12 = 8
      else if ( node .eq. 8 ) then
        next_boundary_node_q12 = 12
      else if ( node .eq. 12 ) then
        next_boundary_node_q12 = 11
      else if ( node .eq. 11 ) then
        next_boundary_node_q12 = 10
      else if ( node .eq. 10 ) then
        next_boundary_node_q12 = 9
      else if ( node .eq. 9 ) then
        next_boundary_node_q12 = 7
      else if ( node .eq. 7 ) then
        next_boundary_node_q12 = 5
      else if ( node .eq. 5 ) then
        next_boundary_node_q12 = 1
      else
        next_boundary_node_q12 = -1
      end if

      return
      end
      function next_boundary_node_q16 ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_Q16 returns the next boundary node in a Q16 element.
c
c  Reference Element Q16:
c
c    |
c    1 13--14--15--16
c    |  |   :   :   |
c    |  |   :   :   |
c    |  9..10..11..12
c    S  |   :   :   |
c    |  |   :   :   |
c    |  5...6...7...8
c    |  |   :   :   |
c    |  |   :   :   |  
c    0  1---2---3---4
c    |
c    +--0-----R-----1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_Q16, the index of the next 
c    edge node.
c
      implicit none

      integer next_boundary_node_q16
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_q16 = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_q16 = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_q16 = 4
      else if ( node .eq. 4 ) then
        next_boundary_node_q16 = 8
      else if ( node .eq. 8 ) then
        next_boundary_node_q16 = 12
      else if ( node .eq. 12 ) then
        next_boundary_node_q16 = 16
      else if ( node .eq. 16 ) then
        next_boundary_node_q16 = 15
      else if ( node .eq. 15 ) then
        next_boundary_node_q16 = 14
      else if ( node .eq. 14 ) then
        next_boundary_node_q16 = 13
      else if ( node .eq. 13 ) then
        next_boundary_node_q16 = 9
      else if ( node .eq. 9 ) then
        next_boundary_node_q16 = 5
      else if ( node .eq. 5 ) then
        next_boundary_node_q16 = 1
      else
        next_boundary_node_q16 = -1
      end if

      return
      end
      function next_boundary_node_ql ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_QL returns the next boundary node in a QL element.
c
c  Reference Element QL:
c
c    |
c    1  4---5---6
c    |  |       |
c    |  |       |
c    S  |       |
c    |  |       |
c    |  |       |
c    0  1---2---3
c    |
c    +--0---R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_QL, the index of the next 
c    edge node.
c
      implicit none

      integer next_boundary_node_ql
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_ql = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_ql = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_ql = 6
      else if ( node .eq. 6 ) then
        next_boundary_node_ql = 5
      else if ( node .eq. 5 ) then
        next_boundary_node_ql = 4
      else if ( node .eq. 4 ) then
        next_boundary_node_ql = 1
      else
        next_boundary_node_ql = -1
      end if

      return
      end
      function next_boundary_node_t3 ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_T3 returns the next boundary node in a T3 element.
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_T3, the index of the next 
c    edge node.
c
      implicit none

      integer next_boundary_node_t3
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_t3 = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_t3 = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_t3 = 1
      else
        next_boundary_node_t3 = -1
      end if

      return
      end
      function next_boundary_node_t4 ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_T4 returns the next boundary node in a T4 element.
c
c  Reference Element T4:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  | 4 \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 October 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_T4, the index of the next 
c    edge node.
c
      implicit none

      integer next_boundary_node_t4
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_t4 = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_t4 = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_t4 = 1
      else
        next_boundary_node_t4 = -1
      end if

      return
      end
      function next_boundary_node_t6 ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_T6 returns the next boundary node in a T6 element.
c
c  Reference Element T6:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  6  5
c    |  |   \
c    |  |    \
c    0  1--4--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_T6, the index of the next 
c    edge node.
c
      implicit none

      integer next_boundary_node_t6
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_t6 = 4
      else if ( node .eq. 4 ) then
        next_boundary_node_t6 = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_t6 = 5
      else if ( node .eq. 5 ) then
        next_boundary_node_t6 = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_t6 = 6
      else if ( node .eq. 6 ) then
        next_boundary_node_t6 = 1
      else
        next_boundary_node_t6 = -1
      end if

      return
      end
      function next_boundary_node_t10 ( node )

c*********************************************************************72
c
cc NEXT_BOUNDARY_NODE_T10 returns the next boundary node in a T10 element.
c
c  Reference Element T10:
c
c    |
c    1  10
c    |  |\
c    |  | \
c    |  8  9
c    |  |   \
c    S  |    \
c    |  5  6  7
c    |  |      \
c    |  |       \
c    0  1--2--3--4
c    |
c    +--0----R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE, the index of the current node.  An input
c    value of 0 (or any "unusual" value") indicates that the
c    first edge node is desired.
c
c    Output, integer NEXT_BOUNDARY_NODE_T10, the index of the next 
c    edge node.
c
      implicit none

      integer next_boundary_node_t10
      integer node

      if ( node .eq. 1 ) then
        next_boundary_node_t10 = 2
      else if ( node .eq. 2 ) then
        next_boundary_node_t10 = 3
      else if ( node .eq. 3 ) then
        next_boundary_node_t10 = 4
      else if ( node .eq. 4 ) then
        next_boundary_node_t10 = 7
      else if ( node .eq. 7 ) then
        next_boundary_node_t10 = 9
      else if ( node .eq. 9 ) then
        next_boundary_node_t10 = 10
      else if ( node .eq. 10 ) then
        next_boundary_node_t10 = 8
      else if ( node .eq. 8 ) then
        next_boundary_node_t10 = 5
      else if ( node .eq. 5 ) then
        next_boundary_node_t10 = 1
      else
        next_boundary_node_t10 = -1
      end if

      return
      end
      subroutine node_reference ( code, r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE returns the basis nodes for any available element.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element desired.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
c    'T3', 'T4', 'T6' and 'T10'.
c
c    Output, double precision R(NODE_NUM), S(NODE_NUM), the coordinates 
c    of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      character * ( * ) code
      double precision r(*)
      double precision s(*)
      logical s_eqi

      if ( s_eqi ( code, 'Q4' ) ) then
        call node_reference_q4 ( r, s, area )
      else if ( s_eqi ( code, 'Q8' ) ) then
        call node_reference_q8 ( r, s, area )
      else if ( s_eqi ( code, 'Q9' ) ) then
        call node_reference_q9 ( r, s, area )
      else if ( s_eqi ( code, 'Q12' ) ) then
        call node_reference_q12 ( r, s, area )
      else if ( s_eqi ( code, 'Q16' ) ) then
        call node_reference_q16 ( r, s, area )
      else if ( s_eqi ( code, 'QL' ) ) then
        call node_reference_ql ( r, s, area )
      else if ( s_eqi ( code, 'T3' ) ) then
        call node_reference_t3 ( r, s, area )
      else if ( s_eqi ( code, 'T4' ) ) then
        call node_reference_t4 ( r, s, area )
      else if ( s_eqi ( code, 'T6' ) ) then
        call node_reference_t6 ( r, s, area )
      else if ( s_eqi ( code, 'T10' ) ) then
        call node_reference_t10 ( r, s, area )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NODE_REFERENCE - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of CODE = ' // trim ( code )
        stop
      end if

      return
      end
      subroutine node_reference_q4 ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_Q4 returns the basis nodes for a 4 node quadrilateral.
c
c  Reference Element Q4:
c
c    |
c    1  4-------3
c    |  |       |
c    |  |       |
c    S  |       |
c    |  |       |
c    |  |       |
c    0  1-------2
c    |
c    +--0---R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R(4), S(4), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      double precision r(4)
      double precision s(4)

      r(1) = 0.0D+00
      r(2) = 1.0D+00
      r(3) = 1.0D+00
      r(4) = 0.0D+00

      s(1) = 0.0D+00
      s(2) = 0.0D+00
      s(3) = 1.0D+00
      s(4) = 1.0D+00

      area = 1.0D+00

      return
      end
      subroutine node_reference_q8 ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_Q8 returns the basis nodes for an 8 node quadrilateral.
c
c  Discussion:
c
c    This element is known as the quadratic "serendipity" element.
c
c  Reference Element Q8:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8     6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R(8), S(8), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      double precision r(8)
      double precision s(8)

      r(1) = 0.0D+00
      r(2) = 1.0D+00
      r(3) = 1.0D+00
      r(4) = 0.0D+00
      r(5) = 0.5D+00
      r(6) = 1.0D+00
      r(7) = 0.5D+00
      r(8) = 0.0D+00

      s(1) = 0.0D+00
      s(2) = 0.0D+00
      s(3) = 1.0D+00
      s(4) = 1.0D+00
      s(5) = 0.0D+00
      s(6) = 0.5D+00
      s(7) = 1.0D+00
      s(8) = 0.5D+00

      area = 1.0D+00

      return
      end
      subroutine node_reference_q9 ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_Q9 returns the basis nodes for a 9 node quadrilateral.
c
c  Reference Element Q9:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8  9  6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R(9), S(9), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      double precision r(9)
      double precision s(9)

      r(1) = 0.0D+00
      r(2) = 1.0D+00
      r(3) = 1.0D+00
      r(4) = 0.0D+00
      r(5) = 0.5D+00
      r(6) = 1.0D+00
      r(7) = 0.5D+00
      r(8) = 0.0D+00
      r(9) = 0.5D+00

      s(1) = 0.0D+00
      s(2) = 0.0D+00
      s(3) = 1.0D+00
      s(4) = 1.0D+00
      s(5) = 0.0D+00
      s(6) = 0.5D+00
      s(7) = 1.0D+00
      s(8) = 0.5D+00
      s(9) = 0.5D+00

      area = 1.0D+00

      return
      end
      subroutine node_reference_q12 ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_Q12 returns the basis nodes for a 12 node quadrilateral.
c
c  Discussion:
c
c    This element is known as the cubic "serendipity" element.
c
c  Reference Element Q12:
c
c    |
c    1  9-10-11-12
c    |  |        |
c    |  7        8
c    S  |        |
c    |  5        6
c    |  |        |
c    0  1--2--3--4
c    |
c    +--0---R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R(12), S(12), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision a
      parameter ( a = 0.0D+00 )
      double precision area
      double precision b
      parameter ( b = 1.0D+00 / 3.0D+00 )
      double precision c
      parameter ( c = 2.0D+00 / 3.0D+00 )
      double precision d
      parameter ( d = 1.0D+00 )
      double precision r(12)
      double precision s(12)

      r(1) = a
      r(2) = b
      r(3) = c
      r(4) = d
      r(5) = a
      r(6) = d
      r(7) = a
      r(8) = d
      r(9) = a
      r(10) = b
      r(11) = c
      r(12) = d

      s(1) = a
      s(2) = a
      s(3) = a
      s(4) = a
      s(5) = b
      s(6) = b
      s(7) = c
      s(8) = c
      s(9) = d
      s(10) = d
      s(11) = d
      s(12) = d

      area = 1.0D+00

      return
      end
      subroutine node_reference_q16 ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_Q16 returns the basis nodes for a 16 node quadrilateral.
c
c  Reference Element Q16:
c
c    |
c    1 13--14--15--16
c    |  |   :   :   |
c    |  |   :   :   |
c    |  9..10..11..12
c    S  |   :   :   |
c    |  |   :   :   |
c    |  5...6...7...8
c    |  |   :   :   |
c    |  |   :   :   |  
c    0  1---2---3---4
c    |
c    +--0-----R-----1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R(16), S(16), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      integer i
      integer j
      integer k
      double precision r(16)
      double precision s(16)

      k = 0
      do i = 0, 3
        do j = 0, 3
          k = k + 1
          r(k) = dble ( j ) / 3.0D+00
          s(k) = dble ( i ) / 3.0D+00
        end do
      end do

      area = 1.0D+00

      return
      end
      subroutine node_reference_ql ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_QL returns the basis nodes for a quadratic/linear.
c
c  Reference Element QL:
c
c    |
c    1  4---5---6
c    |  |       |
c    |  |       |
c    S  |       |
c    |  |       |
c    |  |       |
c    0  1---2---3
c    |
c    +--0---R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R(6), S(6), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      double precision r(6)
      double precision s(6)

      r(1) = 0.0D+00
      r(2) = 0.5D+00
      r(3) = 1.0D+00
      r(4) = 0.0D+00
      r(5) = 0.5D+00
      r(6) = 1.0D+00

      s(1) = 0.0D+00
      s(2) = 0.0D+00
      s(3) = 0.0D+00
      s(4) = 1.0D+00
      s(5) = 1.0D+00
      s(6) = 1.0D+00

      area = 1.0D+00

      return
      end
      subroutine node_reference_t3 ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_T3 returns the basis nodes for the 3 node triangle.
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R(3), S(3), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      double precision r(3)
      double precision s(3)

      r(1) = 0.0D+00
      r(2) = 1.0D+00
      r(3) = 0.0D+00

      s(1) = 0.0D+00
      s(2) = 0.0D+00
      s(3) = 1.0D+00

      area = 0.5D+00

      return
      end
      subroutine node_reference_t4 ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_T4 returns the basis nodes for the 4 node triangle.
c
c  Reference Element T4:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  | 4 \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R(4), S(4), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      double precision r(4)
      double precision s(4)

      r(1) = 0.0D+00
      r(2) = 1.0D+00
      r(3) = 0.0D+00
      r(4) = 1.0D+00 / 3.0D+00

      s(1) = 0.0D+00
      s(2) = 0.0D+00
      s(3) = 1.0D+00
      s(4) = 1.0D+00 / 3.0D+00

      area = 0.5D+00

      return
      end
      subroutine node_reference_t6 ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_T6 returns the basis nodes for a 6 node triangle.
c
c  Reference Element T6:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  6  5
c    |  |   \
c    |  |    \
c    0  1--4--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R(6), S(6), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      double precision r(6)
      double precision s(6)

      r(1) = 0.0D+00
      r(2) = 1.0D+00
      r(3) = 0.0D+00
      r(4) = 0.5D+00
      r(5) = 0.5D+00
      r(6) = 0.0D+00

      s(1) = 0.0D+00
      s(2) = 0.0D+00
      s(3) = 1.0D+00
      s(4) = 0.0D+00
      s(5) = 0.5D+00
      s(6) = 0.5D+00

      area = 0.5D+00

      return
      end
      subroutine node_reference_t10 ( r, s, area )

c*********************************************************************72
c
cc NODE_REFERENCE_T10 returns the basis nodes for a 10 node triangle.
c
c  Reference Element T10:
c
c    |
c    1  10
c    |  |\
c    |  | \
c    |  8  9
c    |  |   \
c    S  |    \
c    |  5  6  7
c    |  |      \
c    |  |       \
c    0  1--2--3--4
c    |
c    +--0----R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R(10), S(10), the coordinates of the basis nodes.
c
c    Output, double precision AREA, the area of the element.
c
      implicit none

      double precision area
      double precision r(10)
      double precision s(10)

      r(1) = 0.0D+00
      s(1) = 0.0D+00

      r(2) = 1.0D+00 / 3.0D+00
      s(2) = 0.0D+00

      r(3) = 2.0D+00 / 3.0D+00
      s(3) = 0.0D+00

      r(4) = 1.0D+00
      s(4) = 0.0D+00

      r(5) = 0.0D+00
      s(5) = 1.0D+00 / 3.0D+00

      r(6) = 1.0D+00 / 3.0D+00
      s(6) = 1.0D+00 / 3.0D+00

      r(7) = 2.0D+00 / 3.0D+00
      s(7) = 1.0D+00 / 3.0D+00

      r(8) = 0.0D+00
      s(8) = 2.0D+00 / 3.0D+00

      r(9) = 1.0D+00 / 3.0D+00
      s(9) = 2.0D+00 / 3.0D+00

      r(10) = 0.0D+00
      s(10) = 1.0D+00

      area = 0.5D+00

      return
      end
      subroutine ns_t6_var_count ( element_num, element_node, node_num, 
     &  var_node, var_num )

c*********************************************************************72
c
cc NS_T6_VAR_COUNT counts the Navier Stokes variables on a T6 grid.
c
c  Discussion:
c
c    We are given a mesh of T6 elements, and asked to count, in advance,
c    the number of Navier-Stokes variables associated with the grid.
c    In particular, every node has two velocity variables associated with
c    it, but only a node that is a vertex of the element will also have
c    an associated pressure variable.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM); 
c    ELEMENT_NODE(I,J) is the global index of local node I in element J.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Output, integer VAR_NODE(NODE_NUM+1), used to find the variables 
c    associated with a given node, which are in VAR in locations 
c    VAR_NODE(NODE) to VAR_NODE(NODE+1)-1.  Note that the last entry of
c    this array points to the location just after the last location in VAR.
c
c    Output, integer VAR_NUM, the number of variables.
c
      implicit none

      integer element_num
      integer element_order
      parameter ( element_order = 6 )
      integer node_num

      integer count
      integer element
      integer element_node(element_order,element_num)
      integer i
      integer node
      integer num
      integer order
      integer var_node(node_num+1)
      integer var_num
c
c  Our job is easy once we determine which nodes are vertices.
c  So to begin with, let VAR_NODE count the number of variables
c  associated with each node.
c
      do i = 1, node_num
        var_node(i) = 2
      end do

      do element = 1, element_num
        do order = 1, 3
          node = element_node(order,element)
          var_node(node) = 3
        end do
      end do
c
c  Count them.
c
      var_num = 0
      do i = 1, node_num
        var_num = var_num + var_node(i)
      end do
c
c  Make pointers.
c
      count = 1

      do node = 1, node_num
        num = var_node(node)
        var_node(node) = count
        count = count + num
      end do
      var_node(node_num+1) = count

      return
      end
      subroutine ns_t6_var_set ( element_num, element_node, node_num, 
     &  var_node, var_num, var )

c*********************************************************************72
c
cc NS_T6_VAR_SET sets the Navier Stokes variables on a T6 grid.
c
c  Discussion:
c
c    We are given a mesh of T6 elements, and asked to create the natural
c    list of indices for Navier-Stokes variables associated with each node.
c    In particular, every node has two velocity variables associated with
c    it, but only a node that is a vertex of the element will also have
c    an associated pressure variable.
c
c    The hard work has been done for us alread, because the variables
c    have been counted, and the pointers to the occurrence of the
c    first variable associated with each node have been created.
c
c    The indexing of the nodes can be arbitrary, although a bad
c    indexing will result in a miserably large bandwidth (if band
c    storage is being tried for the stiffness matrix).  Here, we
c    simply try to natural ordering, that is, the variables are
c    numbered in order of the node with which they are associated.
c
c    For the Navier Stokes problem on a T6 grid, we take it as
c    understood that each node has either 2 or 3 variables associated
c    with it, that the first two are always the horizontal and
c    then vertical velocity coefficients, and that the third, if
c    present, is a pressure coefficient.
c
c    In other settings, it might be necessary not merely to assign
c    the variables an index, but also to identify them as to type.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM); 
c    ELEMENT_NODE(I,J) is the global index of local node I in element J.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer VAR_NODE(NODE_NUM+1), used to find the variables 
c    associated with a given node, which are in VAR in locations 
c    VAR_NODE(NODE) to VAR_NODE(NODE+1)-1.  Note that the last entry of
c    this array points to the location just after the last location in VAR.
c
c    Input, integer VAR_NUM, the number of variables.
c
c    Output, integer VAR(VAR_NUM), the indexes of the variables, which
c    are simply 1, 2, 3, ..., VAR_NUM.
c
      implicit none

      integer element_num
      integer element_order
      parameter ( element_order = 6 )
      integer node_num
      integer var_num

      integer element_node(element_order,element_num)
      integer i
      integer var(var_num)
      integer var_node(node_num+1)

      do i = 1, var_num
        var(i) = i
      end do 
      
      return
      end
      function order_code ( code )

c*********************************************************************72
c
cc ORDER_CODE returns the order for each element.
c
c  Discussion:
c
c    CODE  Order  Definition
c    ----  -----  ----------
c    Q4     4     4 node linear Lagrange/serendipity quadrilateral;
c    Q8     8     8 node quadratic serendipity quadrilateral;
c    Q9     9     9 node quadratic Lagrange quadrilateral;
c    Q12   12     12 node cubic serendipity quadrilateral;
c    Q16   16     16 node cubic Lagrange quadrilateral;
c    QL     6     6 node linear/quadratic quadrilateral;
c    T3     3     3 node linear triangle;
c    T4     4     4 node cubic bubble triangle
c    T6     6     6 node quadratic triangle;
c    T10   10     10 node cubic triangle.
c 
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, the code for the element.
c
c    Output, integer ORDER_CODE, the order of the element.
c
      implicit none

      character ( len = * ) code
      integer order_code
      logical s_eqi

      if ( s_eqi ( code, 'Q4' ) ) then
        order_code = 4
      else if ( s_eqi ( code, 'Q8' ) ) then
        order_code = 8
      else if ( s_eqi ( code, 'Q9' ) ) then
        order_code = 9
      else if ( s_eqi ( code, 'Q12' ) ) then
        order_code = 12
      else if ( s_eqi ( code, 'Q16' ) ) then
        order_code = 16
      else if ( s_eqi ( code, 'QL' ) ) then
        order_code = 6
      else if ( s_eqi ( code, 'T3' ) ) then
        order_code = 3
      else if ( s_eqi ( code, 'T4' ) ) then
        order_code = 4
      else if ( s_eqi ( code, 'T6' ) ) then
        order_code = 6
      else if ( s_eqi ( code, 'T10' ) ) then
        order_code = 10
      else
        order_code = -1
      end if

      return
      end
      subroutine physical_to_reference_t3 ( t, n, phy, ref )

c*********************************************************************72
c
cc PHYSICAL_TO_REFERENCE_T3 maps physical points to reference points.
c
c  Discussion:
c
c    Given the vertices of an order 3 physical triangle and a point 
c    (X,Y) in the physical triangle, the routine computes the value 
c    of the corresponding image point (XSI,ETA) in reference space.
c
c    This routine is also appropriate for an order 4 triangle, assuming
c    that the fourth node is always the centroid of the triangle.
c
c    This routine may be appropriate for an order 6
c    triangle, if the mapping between reference and physical space
c    is linear.  This implies, in particular, that the sides of the
c    image triangle are straight and that the "midside" nodes in the
c    physical triangle are halfway along the sides of
c    the physical triangle.
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the X and Y coordinates
c    of the vertices.  The vertices are assumed to be the images of
c    (0,0), (1,0) and (0,1) respectively.
c
c    Input, integer N, the number of points to transform.
c
c    Input, double precision PHY(2,N), the coordinates of physical points
c    to be transformed.
c
c    Output, double precision REF(2,N), the coordinates of the corresponding
c    points in the reference space.
c
      implicit none

      integer n

      integer j
      double precision phy(2,n)
      double precision ref(2,n)
      double precision t(2,3)

      do j = 1, n

        ref(1,j) = ( ( t(2,3) - t(2,1) ) * ( phy(1,j) - t(1,1) )   
     &             - ( t(1,3) - t(1,1) ) * ( phy(2,j) - t(2,1) ) ) 
     &           / ( ( t(2,3) - t(2,1) ) * ( t(1,2)   - t(1,1) )   
     &             - ( t(1,3) - t(1,1) ) * ( t(2,2)   - t(2,1) ) )

        ref(2,j) = ( ( t(1,2) - t(1,1) ) * ( phy(2,j) - t(2,1) )   
     &             - ( t(2,2) - t(2,1) ) * ( phy(1,j) - t(1,1) ) ) 
     &           / ( ( t(2,3) - t(2,1) ) * ( t(1,2)   - t(1,1) )   
     &             - ( t(1,3) - t(1,1) ) * ( t(2,2)   - t(2,1) ) )

      end do

      return
      end
      subroutine points_plot ( file_name, node_num, node_xy, 
     &  node_label )

c*********************************************************************72
c
cc POINTS_PLOT plots a pointset.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILE_NAME, the name of the output file.
c
c    Input, integer NODE_NUM, the number of points.
c
c    Input, double precision NODE_XY(2,NODE_NUM), the nodes.
c
c    Input, logical NODE_LABEL, is TRUE if the nodes should be labeled.
c
c  Local parameters:
c
c    Local, integer CIRCLE_SIZE, controls the size of the circles depicting
c    the nodes.  Currently set to 5.  3 is pretty small, and 1 is
c    barely visible.
c
      implicit none

      integer node_num

      integer circle_size
      parameter ( circle_size = 5 )
      integer delta
      character * ( * ) file_name
      integer file_unit
      integer ios
      integer j
      integer node
      logical node_label
      double precision node_xy(2,node_num)
      character * ( 40 ) string
      double precision x_max
      double precision x_min
      integer x_ps
      integer x_ps_max
      integer x_ps_max_clip
      integer x_ps_min
      integer x_ps_min_clip
      double precision x_scale
      double precision y_max
      double precision y_min
      integer y_ps
      integer y_ps_max
      integer y_ps_max_clip
      integer y_ps_min
      integer y_ps_min_clip
      double precision y_scale

      x_ps_max = 576
      x_ps_max_clip = 594
      x_ps_min = 36
      x_ps_min_clip = 18
      y_ps_max = 666
      y_ps_max_clip = 684
      y_ps_min = 126
      y_ps_min_clip = 108
c
c  We need to do some figuring here, so that we can determine
c  the range of the data, and hence the height and width
c  of the piece of paper.
c
      x_max = node_xy(1,1)
      x_min = node_xy(1,1)
      do j = 1, node_num
        x_max = max ( x_max, node_xy(1,j) )
        x_min = min ( x_min, node_xy(1,j) )
      end do
      x_scale = x_max - x_min

      x_max = x_max + 0.05D+00 * x_scale
      x_min = x_min - 0.05D+00 * x_scale
      x_scale = x_max - x_min

      y_max = node_xy(2,1)
      y_min = node_xy(2,1)
      do j = 1, node_num
        y_max = max ( y_max, node_xy(2,j) )
        y_min = min ( y_min, node_xy(2,j) )
      end do
      y_scale = y_max - y_min

      y_max = y_max + 0.05D+00 * y_scale
      y_min = y_min - 0.05D+00 * y_scale
      y_scale = y_max - y_min

      if ( x_scale .lt. y_scale ) then

        delta = nint ( dble ( x_ps_max - x_ps_min ) 
     &    * ( y_scale - x_scale ) / ( 2.0D+00 * y_scale ) )

        x_ps_max = x_ps_max - delta
        x_ps_min = x_ps_min + delta

        x_ps_max_clip = x_ps_max_clip - delta
        x_ps_min_clip = x_ps_min_clip + delta

        x_scale = y_scale

      else if ( y_scale .lt. x_scale ) then

        delta = nint ( dble ( y_ps_max - y_ps_min ) 
     &    * ( x_scale - y_scale ) / ( 2.0D+00 * x_scale ) )

        y_ps_max = y_ps_max - delta
        y_ps_min = y_ps_min + delta

        y_ps_max_clip = y_ps_max_clip - delta
        y_ps_min_clip = y_ps_min_clip + delta

        y_scale = x_scale

      end if

      call get_unit ( file_unit )

      open ( unit = file_unit, file = file_name, status = 'replace', 
     &  iostat = ios )

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POINTS_PLOT - Fatal error!'
        write ( *, '(a)' ) '  Can not open output file.'
        return
      end if

      write ( file_unit, '(a)' ) '%cPS-Adobe-3.0 EPSF-3.0'
      write ( file_unit, '(a)' ) '%%Creator: points_plot.f90'
      write ( file_unit, '(a)' ) '%%Title: ' // trim ( file_name )
      write ( file_unit, '(a)' ) '%%Pages: 1'
      write ( file_unit, '(a,i3,2x,i3,2x,i3,2x,i3)' ) '%%BoundingBox: ', 
     &  x_ps_min, y_ps_min, x_ps_max, y_ps_max
      write ( file_unit, '(a)' ) '%%Document-Fonts: Times-Roman'
      write ( file_unit, '(a)' ) '%%LanguageLevel: 1'
      write ( file_unit, '(a)' ) '%%EndComments'
      write ( file_unit, '(a)' ) '%%BeginProlog'
      write ( file_unit, '(a)' ) '/inch {72 mul} def'
      write ( file_unit, '(a)' ) '%%EndProlog'
      write ( file_unit, '(a)' ) '%%Page: 1 1'
      write ( file_unit, '(a)' ) 'save'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 
     &  '%  Set the RGB line color to very light gray.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '0.900  0.900  0.900 setrgbcolor'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 
     &  '%  Draw a gray border around the page.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 'newpath'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_min, y_ps_min, ' moveto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_max, y_ps_min, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_max, y_ps_max, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_min, y_ps_max, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) 
     &  '  ', x_ps_min, y_ps_min, ' lineto'
      write ( file_unit, '(a)' ) 'stroke'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Set the RGB line color to black.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '0.000  0.000  0.000 setrgbcolor'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Set the font and its size.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '/Times-Roman findfont'
      write ( file_unit, '(a)' ) '0.50 inch scalefont'
      write ( file_unit, '(a)' ) 'setfont'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Print a title.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  210  702  moveto'
      write ( file_unit, '(a)' ) '%  (Pointset)  show'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Define a clipping polygon.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 'newpath'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_min_clip, y_ps_min_clip, ' moveto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_max_clip, y_ps_min_clip, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_max_clip, y_ps_max_clip, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_min_clip, y_ps_max_clip, ' lineto'
      write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', 
     &  x_ps_min_clip, y_ps_min_clip, ' lineto'
      write ( file_unit, '(a)' ) 'clip newpath'
c
c  Draw the nodes.
c
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Draw filled dots at each node.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Set the RGB color to blue.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '0.000  0.150  0.750 setrgbcolor'
      write ( file_unit, '(a)' ) '%'

      do node = 1, node_num

        x_ps = int ( 
     &    ( ( x_max - node_xy(1,node)         ) * dble ( x_ps_min )   
     &    + (         node_xy(1,node) - x_min ) * dble ( x_ps_max ) ) 
     &    / ( x_max                   - x_min ) )

        y_ps = int ( 
     &    ( ( y_max - node_xy(2,node)         ) * dble ( y_ps_min )   
     &    + (         node_xy(2,node) - y_min ) * dble ( y_ps_max ) ) 
     &    / ( y_max                   - y_min ) )

        write ( file_unit, '(a,i4,2x,i4,2x,i4,2x,a)' ) 
     &    'newpath ', x_ps, y_ps, 
     &    circle_size, '0 360 arc closepath fill'

      end do
c
c  Label the nodes.
c
      if ( node_label ) then

        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '%  Label the nodes:'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) 
     &    '%  Set the RGB color to darker blue.'
        write ( file_unit, '(a)' ) '%'
        write ( file_unit, '(a)' ) '0.000  0.250  0.850 setrgbcolor'
        write ( file_unit, '(a)' ) '/Times-Roman findfont'
        write ( file_unit, '(a)' ) '0.20 inch scalefont'
        write ( file_unit, '(a)' ) 'setfont'

        do node = 1, node_num

          x_ps = int ( 
     &      ( ( x_max - node_xy(1,node)         ) * dble ( x_ps_min )   
     &      + (       + node_xy(1,node) - x_min ) * dble ( x_ps_max ) ) 
     &      / ( x_max                   - x_min ) )

          y_ps = int ( 
     &      ( ( y_max - node_xy(2,node)         ) * dble ( y_ps_min )   
     &      + (         node_xy(2,node) - y_min ) * dble ( y_ps_max ) ) 
     &      / ( y_max                   - y_min ) )

          write ( string, '(i4)' ) node
          string = adjustl ( string )

          write ( file_unit, '(i4,2x,i4,a)' ) x_ps, y_ps+5, 
     &      ' moveto (' // trim ( string ) // ') show'

        end do

      end if

      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) 'restore  showpage'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  End of page.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%%Trailer'
      write ( file_unit, '(a)' ) '%%EOF'
      close ( unit = file_unit )

      return
      end
      subroutine poly ( code, rexp, sexp )

c*********************************************************************72
c
cc POLY returns the polynomial terms associated with any available element.
c
c  Discussion:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element desired.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3', 
c    'T4', 'T6' and 'T10'.
c
c    Output, integer REXP(N), SEXP(N), the powers of R and S 
c    associated with each polynomial.  The value of N, the dimension of these
c    arrays, can be determined by a call to ORDER_CODE.
c
      implicit none

      character * ( * ) code
      integer rexp(*)
      integer sexp(*)
      logical s_eqi

      if ( s_eqi ( code, 'Q4' ) ) then
        call poly_q4 ( rexp, sexp )
      else if ( s_eqi ( code, 'Q8' ) ) then
        call poly_q8 ( rexp, sexp )
      else if ( s_eqi ( code, 'Q9' ) ) then
        call poly_q9 ( rexp, sexp )
      else if ( s_eqi ( code, 'Q12' ) ) then
        call poly_q12 ( rexp, sexp )
      else if ( s_eqi ( code, 'Q16' ) ) then
        call poly_q16 ( rexp, sexp )
      else if ( s_eqi ( code, 'QL' ) ) then
        call poly_ql ( rexp, sexp )
      else if ( s_eqi ( code, 'T3' ) ) then
        call poly_t3 ( rexp, sexp )
      else if ( s_eqi ( code, 'T4' ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLY - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The T4 element does not follow the pattern.' 
        stop
      else if ( s_eqi ( code, 'T6' ) ) then
        call poly_t6 ( rexp, sexp )
      else if ( s_eqi ( code, 'T10' ) ) then
        call poly_t10 ( rexp, sexp )
      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLY - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Illegal value of CODE = "' // trim ( code ) // '".'
        stop

      end if

      return
      end
      subroutine poly_q4 ( rexp, sexp )

c*********************************************************************72
c
cc POLY_Q4 returns the monomials associated with a 4 node quadrilateral.
c
c  Reference Element Q4:
c
c    |
c    1  4-----3
c    |  |     |
c    |  |     |
c    S  |     |
c    |  |     |
c    |  |     |
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Formula:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer REXP(4), SEXP(4), the powers of R and S 
c    associated with each polynomial.
c
      implicit none

      integer rexp(4)
      integer sexp(4)

      rexp(1) = 0
      rexp(2) = 0
      rexp(3) = 1
      rexp(4) = 1

      sexp(1) = 0
      sexp(2) = 1
      sexp(3) = 0
      sexp(4) = 1

      return
      end
      subroutine poly_q8 ( rexp, sexp )

c*********************************************************************72
c
cc POLY_Q8 returns the monomials associated with an 8 node quadrilateral.
c
c  Reference Element Q8:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8     6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Formula:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer REXP(8), SEXP(8), the powers of R and S 
c    associated with each monomial.
c
      implicit none

      integer rexp(8)
      integer sexp(8)

      rexp(1) = 0
      rexp(2) = 0
      rexp(3) = 1
      rexp(4) = 0
      rexp(5) = 1
      rexp(6) = 2
      rexp(7) = 1
      rexp(8) = 2

      sexp(1) = 0
      sexp(2) = 1
      sexp(3) = 0
      sexp(4) = 2
      sexp(5) = 1
      sexp(6) = 0
      sexp(7) = 2
      sexp(8) = 1

      return
      end
      subroutine poly_q9 ( rexp, sexp )

c*********************************************************************72
c
cc POLY_Q9 returns the monomials associated with a 9 node quadrilateral.
c
c  Reference Element Q9:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8  9  6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Formula:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer REXP(9), SEXP(9), the powers of R and S 
c    associated with each monomial.
c
      implicit none

      integer rexp(9)
      integer sexp(9)

      rexp(1) = 0
      rexp(2) = 0
      rexp(3) = 1
      rexp(4) = 0
      rexp(5) = 1
      rexp(6) = 2
      rexp(7) = 1
      rexp(8) = 2
      rexp(9) = 2

      sexp(1) = 0
      sexp(2) = 1
      sexp(3) = 0
      sexp(4) = 2
      sexp(5) = 1
      sexp(6) = 0
      sexp(7) = 2
      sexp(8) = 1
      sexp(9) = 2

      return
      end
      subroutine poly_q12 ( rexp, sexp )

c*********************************************************************72
c
cc POLY_Q12 returns the monomials associated with a 12 node quadrilateral.
c
c  Reference Element Q12:
c
c    |
c    1  9-10-11-12
c    |  |        |
c    |  7        8
c    S  |        |
c    |  5        6
c    |  |        |
c    0  1--2--3--4
c    |
c    +--0---R---1-->
c
c  Formula:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer REXP(12), SEXP(12), the powers of R and S 
c    associated with each monomial.
c
      implicit none

      integer rexp(12)
      integer sexp(12)

      rexp(1) = 0
      rexp(2) = 0
      rexp(3) = 1
      rexp(4) = 0
      rexp(5) = 1
      rexp(6) = 2
      rexp(7) = 0
      rexp(8) = 1
      rexp(9) = 2
      rexp(10) = 3
      rexp(11) = 1
      rexp(12) = 3

      sexp(1) = 0
      sexp(2) = 1
      sexp(3) = 0
      sexp(4) = 2
      sexp(5) = 1
      sexp(6) = 0
      sexp(7) = 3
      sexp(8) = 2
      sexp(9) = 1
      sexp(10) = 0
      sexp(11) = 3
      sexp(12) = 1

      return
      end
      subroutine poly_q16 ( rexp, sexp )

c*********************************************************************72
c
cc POLY_Q16 returns the monomials associated with a 16 node quadrilateral.
c
c  Reference Element Q16:
c
c    |
c    1 13--14--15--16
c    |  |   :   :   |
c    |  |   :   :   |
c    |  9..10..11..12
c    S  |   :   :   |
c    |  |   :   :   |
c    |  5...6...7...8
c    |  |   :   :   |
c    |  |   :   :   |  
c    0  1---2---3---4
c    |
c    +--0-----R-----1-->
c
c  Formula:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer REXP(16), SEXP(16), the powers of R and S 
c    associated with each monomial.
c
      implicit none

      integer rexp(16)
      integer sexp(16)

      rexp(1) = 0
      rexp(2) = 0
      rexp(3) = 1
      rexp(4) = 0
      rexp(5) = 1
      rexp(6) = 2
      rexp(7) = 0
      rexp(8) = 1
      rexp(9) = 2
      rexp(10) = 3
      rexp(11) = 1
      rexp(12) = 2
      rexp(13) = 3
      rexp(14) = 2
      rexp(15) = 3
      rexp(16) = 3

      sexp(1) = 0
      sexp(2) = 1
      sexp(3) = 0
      sexp(4) = 2
      sexp(5) = 1
      sexp(6) = 0
      sexp(7) = 3
      sexp(8) = 2
      sexp(9) = 1
      sexp(10) = 0
      sexp(11) = 3
      sexp(12) = 2
      sexp(13) = 1
      sexp(14) = 3
      sexp(15) = 2
      sexp(16) = 3

      return
      end
      subroutine poly_ql ( rexp, sexp )

c*********************************************************************72
c
cc POLY_QL returns the monomials for a quadratic/linear quadrilateral.
c
c  Reference Element QL:
c
c    |
c    1  4---5---6
c    |  |       |
c    |  |       |
c    S  |       |
c    |  |       |
c    |  |       |
c    0  1---2---3
c    |
c    +--0---R---1-->
c
c  Formula:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer REXP(N), SEXP(N), the powers of R and S
c    associated with each monomial.
c
      implicit none

      integer rexp(6)
      integer sexp(6)

      rexp(1) = 0
      rexp(2) = 0
      rexp(3) = 1
      rexp(4) = 1
      rexp(5) = 2
      rexp(6) = 2

      sexp(1) = 0
      sexp(2) = 1
      sexp(3) = 0
      sexp(4) = 1
      sexp(5) = 0
      sexp(6) = 1

      return
      end
      subroutine poly_t3 ( rexp, sexp )

c*********************************************************************72
c
cc POLY_T3 returns the monomials associated with a 3 node triangle.
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Formula:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer REXP(N), SEXP(N), the powers of R and S 
c    associated with each monomial.
c
      implicit none

      integer rexp(3)
      integer sexp(3)

      rexp(1) = 0
      rexp(2) = 0
      rexp(3) = 1

      sexp(1) = 0
      sexp(2) = 1
      sexp(3) = 0

      return
      end
      subroutine poly_t6 ( rexp, sexp )

c*********************************************************************72
c
cc POLY_T6 returns the monomials associated with a 6 node triangle.
c
c  Reference Element T6:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  6  5
c    |  |   \
c    |  |    \
c    0  1--4--2
c    |
c    +--0--R--1-->
c
c  Formula:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer REXP(6), SEXP(6), the powers of R and S 
c    associated with each monomial.
c
      implicit none

      integer rexp(6)
      integer sexp(6)

      rexp(1) = 0
      rexp(2) = 0
      rexp(3) = 1
      rexp(4) = 0
      rexp(5) = 1
      rexp(6) = 2

      sexp(1) = 0
      sexp(2) = 1
      sexp(3) = 0
      sexp(4) = 2
      sexp(5) = 1
      sexp(6) = 0

      return
      end
      subroutine poly_t10 ( rexp, sexp )

c*********************************************************************72
c
cc POLY_T10 returns the monomials associated with a 10 node triangle.
c
c  Reference Element T10:
c
c    |
c    1  10
c    |  |\
c    |  | \
c    |  8  9
c    |  |   \
c    S  |    \
c    |  5  6  7
c    |  |      \
c    |  |       \
c    0  1--2--3--4
c    |
c    +--0----R---1-->
c
c  Formula:
c
c    Given coefficients A(I), the polynomial interpolant at (R,S) is
c
c      P(R,S) = sum ( 1 <= I <= N ) A(I) * R^REXP(I) * S^SEXP(I) 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer REXP(10), SEXP(10), the powers of R and S 
c    associated with each monomial.
c
      implicit none

      integer rexp(10)
      integer sexp(10)

      rexp(1) = 0
      rexp(2) = 0
      rexp(3) = 1
      rexp(4) = 0
      rexp(5) = 1
      rexp(6) = 2
      rexp(7) = 0
      rexp(8) = 1
      rexp(9) = 2
      rexp(10) = 3

      sexp(1) = 0
      sexp(2) = 1
      sexp(3) = 0
      sexp(4) = 2
      sexp(5) = 1
      sexp(6) = 0
      sexp(7) = 3
      sexp(8) = 2
      sexp(9) = 1
      sexp(10) = 0

      return
      end
      function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a "huge" R8.
c
c  Discussion:
c
c    The value returned by this function is NOT required to be the
c    maximum representable R8.  This value varies from machine to machine,
c    from compiler to compiler, and may cause problems when being printed.
c    We simply want a "very large" but non-infinite number.
c
c    FORTRAN90 provides a built-in routine HUGE ( X ) that
c    can return the maximum representable number of the same datatype
c    as X, if that is what is really desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a huge number.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

      return
      end
      function r8_power ( r, p )

c*********************************************************************72
c
cc R8_POWER computes the P-th power of an R8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the base.
c
c    Input, integer P, the power, which may be negative.
c
c    Output, double precision R8_POWER, the value of the P-th power of R.
c
      implicit none

      integer p
      double precision r
      double precision r8_power
      double precision value
c
c  Special case.  R^0 = 1.
c
      if ( p .eq. 0 ) then

        value = 1.0D+00
c
c  Special case.  Positive powers of 0 are 0.
c  For negative powers of 0, we go ahead and compute R**P,
c  relying on the software to complain.
c
      else if ( r .eq. 0.0D+00 ) then

        if ( 0 .lt. p ) then
          value = 0.0D+00
        else
          value = r**p
        end if

      else if ( 1 .le. p ) then
        value = r**p
      else
        value = 1.0D+00 / r**(-p)
      end if

      r8_power = value

      return
      end
      subroutine r8_swap ( x, y )

c*********************************************************************72
c
cc R8_SWAP switches two R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, double precision X, Y.  On output, the values of X and
c    Y have been interchanged.
c
      implicit none

      double precision x
      double precision y
      double precision z

      z = x
      x = y
      y = z

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a unit pseudorandom R8.
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r8_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R8_UNIFORM_01
c      SEED      SEED
c
c         12345   207482415  0.096616
c     207482415  1790989824  0.833995
c    1790989824  2035175616  0.947702
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
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
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley Interscience, page 95, 1998.
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
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if
c
c  Although SEED can be represented exactly as a 32 bit integer,
c  it generally cannot be represented exactly as a 32 bit real number!
c
      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8ge_fa ( n, a, pivot, info )

c*********************************************************************72
c
cc R8GE_FA performs a LINPACK style PLU factorization of an R8GE matrix.
c
c  Discussion:
c
c    The R8GE storage format is used for a general M by N matrix.  A storage 
c    space is made for each entry.  The two dimensional logical
c    array can be thought of as a vector of M*N entries, starting with
c    the M entries in the column 1, then the M entries in column 2
c    and so on.  Considered as a vector, the entry A(I,J) is then stored
c    in vector location I+(J-1)*M.
c
c    R8GE storage is used by LINPACK and LAPACK.
c
c    R8GE_FA is a simplified version of the LINPACK routine SGEFA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 March 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input/output, double precision A(N,N), the matrix to be factored.
c    On output, A contains an upper triangular matrix and the multipliers
c    which were used to obtain it.  The factorization can be written
c    A = L * U, where L is a product of permutation and unit lower
c    triangular matrices and U is upper triangular.
c
c    Output, integer PIVOT(N), a vector of pivot indices.
c
c    Output, integer INFO, singularity flag.
c    0, no singularity detected.
c    nonzero, the factorization failed on the INFO-th step.
c 
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer info
      integer pivot(n)
      integer j
      integer k
      integer l
      double precision t

      info = 0

      do k = 1, n - 1
c
c  Find L, the index of the pivot row.
c
        l = k
        do i = k + 1, n
          if ( abs ( a(l,k) ) .lt. abs ( a(i,k) ) ) then
            l = i
          end if
        end do

        pivot(k) = l
c
c  If the pivot index is zero, the algorithm has failed.
c
        if ( a(l,k) .eq. 0.0D+00 ) then
          info = k
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8GE_FA - Fatal error!'
          write ( *, '(a,i8)' ) '  Zero pivot on step ', info
          return
        end if
c
c  Interchange rows L and K if necessary.
c
        if ( l .ne. k ) then
          t      = a(l,k)
          a(l,k) = a(k,k)
          a(k,k) = t
        end if
c
c  Normalize the values that lie below the pivot entry A(K,K).
c
        do i = k + 1, n
          a(i,k) = - a(i,k) / a(k,k)
        end do
c
c  Row elimination with column indexing.
c
        do j = k + 1, n

          if ( l .ne. k ) then
            t      = a(l,j)
            a(l,j) = a(k,j)
            a(k,j) = t
          end if

          do i = k + 1, n
            a(i,j) = a(i,j) + a(i,k) * a(k,j)
          end do

        end do

      end do

      pivot(n) = n

      if ( a(n,n) .eq. 0.0D+00 ) then
        info = n
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8GE_FA - Fatal error!'
        write ( *, '(a,i8)' ) '  Zero pivot on step ', info
      end if

      return
      end
      subroutine r8ge_inverse ( n, a, pivot )

c*********************************************************************72
c
cc R8GE_INVERSE computes the inverse of a matrix factored by R8GE_FA.
c
c  Discussion:
c
c    The R8GE storage format is used for a general M by N matrix.  A storage
c    space is made for each logical entry.  The two dimensional logical
c    array is mapped to a vector, in which storage is by columns.
c
c    R8GE_INVERSE is a simplified standalone version of the LINPACK routine
c    DGEDI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix A.
c
c    Input/output, double precision A(N,N).
c    On input, the factor information computed by R8GE_FA.
c    On output, the inverse matrix.
c
c    Input, integer PIVOT(N), the pivot vector from R8GE_FA.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer pivot(n)
      integer j
      integer k
      double precision temp
      double precision work(n)
c
c  Compute Inverse(U).
c
      do k = 1, n

        a(k,k) = 1.0D+00 / a(k,k)
        do i = 1, k - 1
          a(i,k) = -a(i,k) * a(k,k)
        end do

        do j = k + 1, n

          temp = a(k,j)
          a(k,j) = 0.0D+00
          do i = 1, k
            a(i,j) = a(i,j) + a(i,k) * temp
          end do

        end do

      end do
c
c  Form Inverse(U) * Inverse(L).
c
      do k = n - 1, 1, -1

        do i = k + 1, n
          work(i) = a(i,k)
          a(i,k) = 0.0D+00
        end do

        do j = k + 1, n
          do i = 1, n
            a(i,k) = a(i,k) + a(i,j) * work(j)
          end do
        end do

        if ( pivot(k) .ne. k ) then

          do i = 1, n
            temp = a(i,k)
            a(i,k) = a(i,pivot(k))
            a(i,pivot(k)) = temp
          end do

        end if

      end do

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character ( len = * ) TITLE, a title to be printed.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character ( len = * ) title

      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2007
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
c    Input, character ( len = * ) TITLE, an optional title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title
      integer title_length

      title_length = len_trim ( title )

      if ( 0 .lt. title_length ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) title(1:title_length)
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j), j = 1, inc )

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
      subroutine reference_sample ( code, seed, r, s )

c*********************************************************************72
c
cc REFERENCE_SAMPLE samples a reference element.
c
c  Discussion:
c
c    The routine either samples the unit triangle or the unit square.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element desired.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3', 
c    'T4', 'T6' and 'T10'.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, double precision R, S, a random point in the reference element.
c
      implicit none

      character * ( * ) code
      double precision r
      double precision r8_uniform_01 
      double precision s
      integer seed

      if ( code(1:1) .eq. 'Q' .or. code(1:1) .eq. 'q' ) then

        r = r8_uniform_01 ( seed )
        s = r8_uniform_01 ( seed )

      else if ( code(1:1) .eq. 'T' .or. code(1:1) .eq. 't' ) then

        r = r8_uniform_01 ( seed )
        s = r8_uniform_01 ( seed )

        if ( 1.0D+00 .lt. r + s ) then
          r = 1.0D+00 - r
          s = 1.0D+00 - s
        end if

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'REFERENCE_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  Illegal code = "' // trim ( code ) // '".'
        stop

      end if

      return
      end
      subroutine reference_to_physical_q4 ( q4, n, rs, xy )

c*********************************************************************72
c
cc REFERENCE_TO_PHYSICAL_Q4 maps Q4 reference points to physical points.
c
c  Discussion:
c
c    XY(R,S) = XY(0,0) * (1-R) * (1-S)
c            + XY(1,0) *    R  * (1-S)
c            + XY(1,1) *    R  *    S
c            + XY(0,1) * (1-R) *    S
c
c  Reference Element Q4:
c
c    |
c    1  4-----3
c    |  |     |
c    |  |     |
c    S  |     |
c    |  |     |
c    |  |     |
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision Q4(2,4), the coordinates of the vertices.
c    The vertices are assumed to be the images of the reference vertices
c    (0,0), (1,0), (1,1) and (0,1) respectively.
c
c    Input, integer N, the number of points to transform.
c
c    Input, double precision RS(2,N), (R,S) points in the reference element.
c
c    Output, double precision XY(2,N), (X,Y) points in the physical element.
c
      implicit none

      integer n

      integer j
      double precision psi(4,n)
      double precision q4(2,4)
      double precision rs(2,n)
      double precision xy(2,n)

      do j = 1, n
        psi(1,j) = ( 1.0D+00 - rs(1,j) ) * ( 1.0D+00 - rs(2,j) )
        psi(2,j) =             rs(1,j)   * ( 1.0D+00 - rs(2,j) )
        psi(3,j) =             rs(1,j)   *             rs(2,j)
        psi(4,j) = ( 1.0D+00 - rs(1,j) ) *             rs(2,j)

        xy(1,j) = q4(1,1) * psi(1,j)
     &          + q4(1,2) * psi(2,j)
     &          + q4(1,3) * psi(3,j)
     &          + q4(1,4) * psi(4,j)

        xy(2,j) = q4(2,1) * psi(1,j)
     &          + q4(2,2) * psi(2,j)
     &          + q4(2,3) * psi(3,j)
     &          + q4(2,4) * psi(4,j)

      end do

      return
      end
      subroutine reference_to_physical_t3 ( t, n, ref, phy )

c*********************************************************************72
c
cc REFERENCE_TO_PHYSICAL_T3 maps T3 reference points to physical points.
c
c  Discussion:
c
c    Given the vertices of an order 3 physical triangle and a point
c    (XSI,ETA) in the reference triangle, the routine computes the value
c    of the corresponding image point (X,Y) in physical space.
c
c    This routine is also appropriate for an order 4 triangle,
c    as long as the fourth node is the centroid of the triangle.
c
c    This routine may also be appropriate for an order 6
c    triangle, if the mapping between reference and physical space
c    is linear.  This implies, in particular, that the sides of the
c    image triangle are straight and that the "midside" nodes in the
c    physical triangle are halfway along the sides of
c    the physical triangle.
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the coordinates of the vertices.
c    The vertices are assumed to be the images of (0,0), (1,0) and
c    (0,1) respectively.
c
c    Input, integer N, the number of points to transform.
c
c    Input, double precision REF(2,N), points in the reference triangle.
c
c    Output, double precision PHY(2,N), corresponding points in the
c    physical triangle.
c
      implicit none

      integer n

      integer i
      integer j
      double precision phy(2,n)
      double precision ref(2,n)
      double precision t(2,3)

      do i = 1, 2
        do j = 1, n
          phy(i,j) = t(i,1) * ( 1.0D+00 - ref(1,j) - ref(2,j) )
     &             + t(i,2) *             ref(1,j)
     &             + t(i,3) *                        ref(2,j)
        end do
      end do

      return
      end
      subroutine reference_to_physical_t6 ( t, n, ref, phy )

c*********************************************************************72
c
cc REFERENCE_TO_PHYSICAL_T6 maps T6 reference points to physical points.
c
c  Discussion:
c
c    Given the vertices of an order 6 physical triangle and a point
c    (XSI,ETA) in the reference triangle, the routine computes the value
c    of the corresponding image point (X,Y) in physical space.
c
c    The mapping from (XSI,ETA) to (X,Y) has the form:
c
c      X(ETA,XSI) = A1 * XSI**2 + B1 * XSI*ETA + C1 * ETA**2
c                 + D1 * XSI    + E1 * ETA     + F1
c
c      Y(ETA,XSI) = A2 * XSI**2 + B2 * XSI*ETA + C2 * ETA**2
c                 + D2 * XSI    + E2 * ETA     + F2
c
c  Reference Element T6:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  6  5
c    |  |   \
c    |  |    \
c    0  1--4--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,6), the coordinates of the vertices.
c    The vertices are assumed to be the images of (0,0), (1,0),
c    (0,1),(1/2,0), (1/2,1/2) and (0,1/2) respectively.
c
c    Input, integer N, the number of points to transform.
c
c    Input, double precision REF(2,N), points in the reference triangle.
c
c    Output, double precision PHY(2,N), corresponding points in the
c    physical triangle.
c
      implicit none

      integer n

      double precision a(2)
      double precision b(2)
      double precision c(2)
      double precision d(2)
      double precision e(2)
      double precision f(2)
      integer i
      integer j
      double precision phy(2,n)
      double precision ref(2,n)
      double precision t(2,6)

      do i = 1, 2

        a(i) =   2.0D+00 * t(i,1) + 2.0D+00 * t(i,2)                    
     &         - 4.0D+00 * t(i,4)

        b(i) =   4.0D+00 * t(i,1)                                       
     &         - 4.0D+00 * t(i,4) + 4.0D+00 * t(i,5) - 4.0D+00 * t(i,6)

        c(i) =   2.0D+00 * t(i,1)                    + 2.0D+00 * t(i,3) 
     &                                               - 4.0D+00 * t(i,6)

        d(i) = - 3.0D+00 * t(i,1) -           t(i,2)                    
     &         + 4.0D+00 * t(i,4)

        e(i) = - 3.0D+00 * t(i,1)                    -           t(i,3) 
     &                                               + 4.0D+00 * t(i,6)
        f(i) =             t(i,1)

      end do

      do i = 1, 2
        do j = 1, n
          phy(i,j) = a(i) * ref(1,j) * ref(1,j) 
     &             + b(i) * ref(1,j) * ref(2,j) 
     &             + c(i) * ref(2,j) * ref(2,j) 
     &             + d(i) * ref(1,j)
     &             + e(i) * ref(2,j)
     &             + f(i)
        end do
      end do

      return
      end
      function s_eqi ( s1, s2 )

c*********************************************************************72
c
cc S_EQI is a case insensitive comparison of two strings for equality.
c
c  Example:
c
c    S_EQI ( 'Anjana', 'ANJANA' ) is TRUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S1, S2, the strings to compare.
c
c    Output, logical S_EQI, the result of the comparison.
c
      implicit none

      character c1
      character c2
      integer i
      integer lenc
      logical s_eqi
      character*(*) s1
      integer s1_length
      character*(*) s2
      integer s2_length

      s1_length = len ( s1 )
      s2_length = len ( s2 )
      lenc = min ( s1_length, s2_length )

      s_eqi = .false.

      do i = 1, lenc

        c1 = s1(i:i)
        c2 = s2(i:i)
        call ch_cap ( c1 )
        call ch_cap ( c2 )

        if ( c1 .ne. c2 ) then
          return
        end if

      end do

      do i = lenc + 1, s1_length
        if ( s1(i:i) .ne. ' ' ) then
          return
        end if
      end do

      do i = lenc + 1, s2_length
        if ( s2(i:i) .ne. ' ' ) then
          return
        end if
      end do

      s_eqi = .true.

      return
      end
      subroutine s_l2norm ( psi_num, element_num, quad_num, 
     &  element_area, quad_weight, psi_quad, s_coef, l2norm )

c*********************************************************************72
c
cc S_L2NORM computes the "big" L2 norm of a scalar function over a region.
c
c  Discussion:
c
c    It is assumed that a set of finite element basis functions PSI
c    have been defined over a collection of elements that compose
c    the region.  Moreover, integrals over the region are assumed to
c    be approximated by applying a fixed quadrature rule to all the
c    elements.
c
c    Finally, we assume that we have a scalar function S(X), which
c    is represented as a linear combination of basis functions, and
c    it is desired to determine the L2 norm of S.
c
c    This routine estimates the integral
c
c      Sqrt ( Integral ( X in Omega ) S(X) * S(X) dOmega )
c
c    using the finite element representation of S, and applying the
c    given quadrature rule.
c
c    It assumes that a (probably very large) array of data is available,
c    recording the value of each basis function PSI in every element
c    at every quadrature point.  If this is true, then the computation
c    becomes very simple.
c
c    If your problem is small or sufficient memory is available, this
c    may be an efficient computation.  It requires that the value of
c    all the basis functions be stored for all the elements and all
c    the quadrature points.  That particular information need only
c    be computed once.
c
c    Actually, no assumptions are made here about the dimension of the
c    space, so this same code can handle problems in 1D, 2D, 3D and
c    so on.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PSI_NUM, the number of global element functions.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer QUAD_NUM, the number of points in the 
c    quadrature rule.
c
c    Input, double precision ELEMENT_AREA(ELEMENT_NUM), the area of 
c    each element.
c
c    Input, double precision QUAD_WEIGHT(QUAD_NUM), the quadrature
c    weights associated with the quadrature points.
c
c    Input, double precision PSI_QUAD(PSI_NUM,ELEMENT_NUM,QUAD_NUM), the 
c    value of the I-th PSI function in the J-th element at the K-th 
c    quadrature point.
c
c    Input, double precision S_COEF(PSI_NUM), the coefficients of the 
c    PSI functions associated with the scalar function S.
c
c    Output, L2NORM, the L2 norm of the scalar function S over the region.
c
      implicit none

      integer element_num
      integer psi_num
      integer quad_num

      double precision element_area(element_num)
      integer i
      integer j
      integer k
      double precision l2norm
      double precision psi_quad(psi_num,element_num,quad_num)
      double precision quad_weight(quad_num)
      double precision r8vec_dot_product
      double precision s(element_num,quad_num)
      double precision s_coef(psi_num)
      double precision t(quad_num)
      double precision temp
      double precision u
c
c  #1: Sum over all basis functions to get the value of S in each element
c  at each quadrature point.
c
c  The MATMUL function requires that one of its arguments be shaped
c  like a vector, and one like a 2 dimensional matrix, so we have
c  to insert a loop on the quadrature points.
c
      do k = 1, quad_num
        do j = 1, element_num
          s(j,k) = 0.0D+00
          do i = 1, psi_num
            s(j,k) = s(j,k) + s_coef(i) * psi_quad(i,j,k)
          end do
        end do
      end do
c
c  #2: Sum over all elements to get the value of S * S weighted by its element
c  area.  SUM expects to see vector quantities, so we have a loop on
c  quadrature points.
c
      do k = 1, quad_num
        temp = 0.0D+00
        do i = 1, element_num
          temp = temp + s(i,k)**2 * element_area(i)
        end do
        t(k) = temp
      end do
c
c  #3: Sum over all quadrature points weighted by the quadrature weight.
c
      u = r8vec_dot_product ( quad_num, t, quad_weight )

      l2norm = sqrt ( u )

      return
      end
      subroutine serene ( type, ve, vn, vne, vnw, vs, vse, vsw, vw, 
     &  vterp )

c*********************************************************************72
c
cc SERENE interpolates data using a Q8 element.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( 2 ) TYPE, tells SERENE the geometry of the
c    finite element that surrounds the point of interest.  The options
c    are displayed in the following table, which suggests the meaning
c    of each option by its position:
c
c        |   |
c     NW * N * NE
c        |   |
c     -*-*-*-*-*-
c        |   |
c      W * C * E
c        |   |
c     -*-*-*-*-*-
c        |   |
c     SW * S * SE
c        |   |
c
c    Input, double precision VE, VN, VNE, VNW, VS, VSE, VSW, VW,
c    are the values of the function at the nodes to the east,
c    north, northeast, northwest, south, southeast, southwest and
c    west of the point of interest.  If the finite element is of
c    type 'C', then all 8 values are needed.  However, if the
c    finite element is of type 'SE', for instance, then only three
c    values are needed, namely VE, VN, and VNW, since these are
c    the only node positions defined in such a finite element.
c
c    Output, double precision VTERP, the interpolated value of the
c    function at the point of interest.
c
      implicit none

      double precision eta
      double precision pe
      double precision pn
      double precision pne
      double precision pnw
      double precision ps
      double precision pse
      double precision psw
      double precision pw
      double precision r8_huge
      character * ( 2 ) type
      double precision ve
      double precision vn
      double precision vne
      double precision vnw
      double precision vs
      double precision vse
      double precision vsw
      double precision vw
      double precision vterp
      double precision xsi
c
c  To make this routine more general, simply pass in the values of XSI
c  and ETA at which the interpolated value is desired.
c
c  By setting XSI = ETA = 0, we are asking for the interpolated value
c  at the center of the finite element.
c
      xsi = 0.0D+00
      eta = 0.0D+00
c
c  8 node center
c
c  Polynomial space is spanned by:
c
c         1
c       x    y
c    x^2  xy  y^2
c      x^2y xy^2
c
c
c    ^   1    4--7--3
c    |        c     c
c    E        c     c
c    T   0    8  X  6
c    A        c     c
c    |        c     c
c    V  -1    1--5--2
c
c            -1  0  1
c
c           <---XSI--->
c
      if ( type .eq. 'C' ) then

        psw = - 0.25D+00 * ( 1.0D+00 - xsi ) * ( 1.0D+00 - eta )
     &    * ( 1.0D+00 + xsi + eta )
        pse = - 0.25D+00 * ( 1.0D+00 + xsi ) * ( 1.0D+00 - eta )
     &    * ( 1.0D+00 - xsi + eta )
        pne = - 0.25D+00 * ( 1.0D+00 + xsi ) * ( 1.0D+00 + eta )
     &    * ( 1.0D+00 - xsi - eta )
        pnw = - 0.25D+00 * ( 1.0D+00 - xsi ) * ( 1.0D+00 + eta )
     &    * ( 1.0D+00 + xsi - eta )
        ps =    0.50D+00 * ( 1.0D+00 - xsi ) * ( 1.0D+00 + xsi )
     &    * ( 1.0D+00 - eta )
        pe =    0.50D+00 * ( 1.0D+00 + xsi ) * ( 1.0D+00 + eta ) 
     &    * ( 1.0D+00 - eta )
        pn =    0.50D+00 * ( 1.0D+00 - xsi ) * ( 1.0D+00 + xsi ) 
     &    * ( 1.0D+00 + eta )
        pw =    0.50D+00 * ( 1.0D+00 - xsi ) * ( 1.0D+00 + eta ) 
     &    * ( 1.0D+00 - eta )

        vterp = vsw * psw + vse * pse + vne * pne + vnw * pnw 
     &    + vs * ps + ve * pe + vn * pn + vw * pw
c
c  5 node side
c
c    ^   1
c    |
c    E
c    T   0    8  X  6
c    A        c     c
c    |        c     c
c    V  -1    1--5--2
c
c            -1  0  1
c
c           <---XSI--->
c
      else if ( type .eq. 'N' ) then

        psw =  0.5D+00 * ( xsi - 1.0D+00 ) * ( 1.0D+00 + xsi + eta )
        pse = -0.5D+00 * ( xsi + 1.0D+00 ) * ( 1.0D+00 - xsi + eta )
        ps =  -          ( xsi + 1.0D+00 ) * ( xsi - 1.0D+00 )
        pe =   0.5D+00 * ( xsi + 1.0D+00 ) * ( eta + 1.0D+00 )
        pw =  -0.5D+00 * ( xsi - 1.0D+00 ) * ( eta + 1.0D+00 )

        vterp = vsw * psw + vse * pse + vs * ps + ve * pe + vw * pw
c
c    ^   1    4--7
c    |        c
c    E        c
c    T   0    8  X
c    A        c
c    |        c
c    V  -1    1--5
c
c            -1  0  1
c
c           <---XSI--->
c
      else if ( type .eq. 'E' ) then

        pse =  0.5D+00 * ( eta - 1.0D+00 ) * ( 1.0D+00 + xsi + eta )
        pne = -0.5D+00 * ( eta + 1.0D+00 ) * ( 1.0D+00 + xsi - eta )
        ps =  -0.5D+00 * ( xsi + 1.0D+00 ) * ( eta - 1.0D+00 )
        pn =   0.5D+00 * ( xsi + 1.0D+00 ) * ( eta + 1.0D+00 )
        pw =  -          ( eta + 1.0D+00 ) * ( eta - 1.0D+00 )

        vterp = vse * pse + vne * pne + vs * ps + vn * pn + vw * pw
c
c  5 node side
c
c    ^   1       7--3
c    |              c
c    E              c
c    T   0       X  6
c    A              c
c    |              c
c    V  -1       5--2
c
c            -1  0  1
c
c           <---XSI--->
c
      else if ( type .eq. 'W' ) then

        pse =   0.5D+00 * ( eta - 1.0D+00 ) * ( 1.0D+00 - xsi + eta )
        pne = - 0.5D+00 * ( eta + 1.0D+00 ) * ( 1.0D+00 - xsi - eta )
        ps =    0.5D+00 * ( xsi - 1.0D+00 ) * ( eta - 1.0D+00 )
        pe =  -           ( eta - 1.0D+00 ) * ( eta + 1.0D+00 )
        pn =  - 0.5D+00 * ( xsi - 1.0D+00 ) * ( eta + 1.0D+00 )

        vterp = vse * pse + vne * pne + vs * ps + ve * pe + vn * pn
c
c  5 node side
c
c    ^   1    4--7--3
c    |        c     c
c    E        c     c
c    T   0    8  X  6
c    A
c    |
c    V  -1
c
c            -1  0  1
c
c           <---XSI--->
c
      else if ( type .eq. 'S' ) then

        pne = - 0.5D+00 * ( xsi + 1.0D+00 ) * ( 1.0D+00 - xsi - eta )
        pnw =   0.5D+00 * ( xsi - 1.0D+00 ) * ( 1.0D+00 + xsi - eta )
        pe =  - 0.5D+00 * ( eta - 1.0D+00 ) * ( xsi + 1.0D+00 )
        pn =  -           ( xsi + 1.0D+00 ) * ( xsi - 1.0D+00 )
        pw =    0.5D+00 * ( eta - 1.0D+00 ) * ( xsi - 1.0D+00 )

        vterp = vne * pne + vnw * pnw + ve * pe + vn * pn + vw * pw
c
c  3 node corner
c
c  Polynomial space is spanned by:
c
c         1
c       x    y
c
c
c    ^   1
c    |
c    E
c    T   0    8  X
c    A        c
c    |        c
c    V  -1    1--5
c
c            -1  0  1
c
c           <---XSI--->
c
      else if ( type .eq. 'NE' ) then

        psw = - 1.0D+00 - xsi - eta
        ps =    1.0D+00 + xsi
        pw =    1.0D+00       + eta

        vterp = vsw * psw + vs * ps + vw * pw
c
c  3 node corner
c
c  Polynomial space is spanned by:
c
c         1
c       x    y
c
c    ^   1
c    |
c    E
c    T   0       X  6
c    A              c
c    |              c
c    V  -1       5--2
c
c            -1  0  1
c
c           <---XSI--->
c
      else if ( type .eq. 'NW' ) then

        pse = 1.0D+00 + xsi - eta
        ps =  1.0D+00 - xsi
        pe =  1.0D+00       + eta

        vterp = + vse * pse + vs * ps + ve * pe
c
c  3 node corner
c
c  Polynomial space is spanned by:
c         1
c       x    y
c
c
c    ^   1    4--7
c    |        c
c    E        c
c    T   0    8  X
c    A
c    |
c    V  -1
c
c            -1  0  1
c
c           <---XSI--->
c
      else if ( type .eq. 'SE' ) then

        pnw = - 1.0D+00 - xsi + eta
        pn =    1.0D+00 + xsi
        pw =    1.0D+00       - eta

        vterp = vnw * pnw + vn * pn + vw * pw
c
c  3 node corner
c
c  Polynomial space is spanned by:
c
c         1
c       x    y
c
c    ^   1       7--3
c    |              c
c    E              c
c    T   0       X  6
c    A
c    |
c    V  -1
c
c            -1  0  1
c
c           <---XSI--->
c
      else if ( type .eq. 'SW' ) then

        pne = - 1.0D+00 + xsi + eta
        pe =    1.0D+00       - eta
        pn =    1.0D+00 - xsi

        vterp = vne * pne + ve * pe + vn * pn

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SERENE - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Illegal value of TYPE = "' // trim ( type ) // '".'
        vterp = - r8_huge ( )
        stop

      end if

      return
      end
      subroutine shape ( code, r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE evaluates shape functions for any available reference element.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
c    'T3', 'T4', 'T6' and 'T10'.
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(N), the basis functions at the point.
c
c    Output, double precision DTDR(N), the R basis derivatives at the point.
c
c    Output, double precision DTDS(N), the S basis derivatives at the point.
c
      implicit none

      character * ( * ) code
      double precision dtdr(*)
      double precision dtds(*)
      double precision r
      double precision s
      logical s_eqi
      double precision t(*)

      if ( s_eqi ( code, 'Q4' ) ) then
        call shape_q4 ( r, s, t, dtdr, dtds )
      else if( s_eqi ( code, 'Q8' ) ) then
        call shape_q8 ( r, s, t, dtdr, dtds )
      else if ( s_eqi ( code, 'Q9' ) ) then
        call shape_q9 ( r, s, t, dtdr, dtds )
      else if ( s_eqi ( code, 'Q12' ) ) then
        call shape_q12 ( r, s, t, dtdr, dtds )
      else if ( s_eqi ( code, 'Q16' ) ) then
        call shape_q16 ( r, s, t, dtdr, dtds )
      else if ( s_eqi ( code, 'QL' ) ) then
        call shape_ql ( r, s, t, dtdr, dtds )
      else if ( s_eqi ( code, 'T3' ) ) then
        call shape_t3 ( r, s, t, dtdr, dtds )
      else if ( s_eqi ( code, 'T4' ) ) then
        call shape_t4 ( r, s, t, dtdr, dtds )
      else if ( s_eqi ( code, 'T6' ) ) then
        call shape_t6 ( r, s, t, dtdr, dtds )
      else if ( s_eqi ( code, 'T10' ) ) then
        call shape_t10 ( r, s, t, dtdr, dtds )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SHAPE - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Unrecognized code = "' // trim ( code ) // '".'
        stop
      end if

      return
      end
      subroutine shape_q4 ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_Q4 evaluates shape functions for a 4 node reference quadrilateral.
c
c  Reference Element Q4:
c
c    |
c    1  4-----3
c    |  |     |
c    |  |     |
c    S  |     |
c    |  |     |
c    |  |     |
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(4), the basis functions at the point.
c
c    Output, double precision DTDR(4), the R basis derivatives at the point.
c
c    Output, double precision DTDS(4), the S basis derivatives at the point.
c
      implicit none

      double precision dtdr(4)
      double precision dtds(4)
      double precision r
      double precision s
      double precision t(4)

      t(1) = ( 1.0D+00 - r ) * ( 1.0D+00 - s )
      t(2) =             r   * ( 1.0D+00 - s )
      t(3) =             r   *             s
      t(4) = ( 1.0D+00 - r ) *             s

      dtdr(1) = - 1.0D+00 + s
      dtdr(2) =   1.0D+00 - s     
      dtdr(3) =             s
      dtdr(4) =           - s

      dtds(1) = - 1.0D+00 + r
      dtds(2) =           - r
      dtds(3) =             r
      dtds(4) =   1.0D+00 - r

      return
      end
      subroutine shape_q8 ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_Q8 evaluates shape functions for an 8 node reference quadrilateral.
c
c  Discussion:
c
c    This element is known as the "serendipity" element.
c
c  Reference Element Q8:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8     6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(8), the basis functions at the point.
c
c    Output, double precision DTDR(8), the R basis derivatives at the point.
c
c    Output, double precision DTDS(8), the S basis derivatives at the point.
c
      implicit none

      double precision dtdr(8)
      double precision dtds(8)
      double precision r
      double precision s
      double precision t(8)

      t(1) =                 ( r - 1.0D+00 )     * ( s - 1.0D+00 ) 
     &  * ( 1.0D+00 - 2.0D+00 * r - 2.0D+00 * s )
      t(2) =             r                       * ( s - 1.0D+00 ) 
     &  * ( 1.0D+00 - 2.0D+00 * r + 2.0D+00 * s )
      t(3) =             r                   * s                   
     &  * ( 2.0D+00 * r + 2.0D+00 * s - 3.0D+00 )
      t(4) =                 ( r - 1.0D+00 ) * s                   
     &  * ( 2.0D+00 * r - 2.0D+00 * s + 1.0D+00 )
      t(5) =   4.0D+00 * r * ( r - 1.0D+00 )     * ( s - 1.0D+00 )
      t(6) = - 4.0D+00 * r                   * s * ( s - 1.0D+00 )
      t(7) = - 4.0D+00 * r * ( r - 1.0D+00 ) * s    
      t(8) =   4.0D+00 *     ( r - 1.0D+00 ) * s * ( s - 1.0D+00 )

      dtdr(1) = ( s - 1.0D+00 ) 
     &  * ( - 4.0D+00 * r - 2.0D+00 * s + 3.0D+00 )
      dtdr(2) = ( s - 1.0D+00 ) 
     &  * ( - 4.0D+00 * r + 2.0D+00 * s + 1.0D+00 )
      dtdr(3) =   s         * (   4.0D+00 * r + 2.0D+00 * s - 3.0D+00 )
      dtdr(4) =   s         * (   4.0D+00 * r - 2.0D+00 * s - 1.0D+00 )
      dtdr(5) =   4.0D+00 * ( 2.0D+00 * r - 1.0D+00 ) * ( s - 1.0D+00 )
      dtdr(6) = - 4.0D+00 *                     s * ( s - 1.0D+00 )
      dtdr(7) = - 4.0D+00 * ( 2.0D+00 * r - 1.0D+00 ) * s
      dtdr(8) =   4.0D+00 *                     s * ( s - 1.0D+00 )

      dtds(1) = ( r - 1.0D+00 ) 
     &  * ( - 4.0D+00 * s - 2.0D+00 * r + 3.0D+00 )
      dtds(2) =   r *       (   4.0D+00 * s - 2.0D+00 * r - 1.0D+00 )
      dtds(3) =   r *       (   4.0D+00 * s + 2.0D+00 * r - 3.0D+00 )
      dtds(4) = ( r - 1.0D+00 ) 
     &  * ( - 4.0D+00 * s + 2.0D+00 * r + 1.0D+00 )
      dtds(5) =   4.0D+00 * r * ( r - 1.0D+00 )
      dtds(6) = - 4.0D+00 * r               * ( 2.0D+00 * s - 1.0D+00 )
      dtds(7) = - 4.0D+00 * r * ( r - 1.0D+00 )
      dtds(8) =   4.0D+00 * ( r - 1.0D+00 ) * ( 2.0D+00 * s - 1.0D+00 )

      return
      end
      subroutine shape_q9 ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_Q9 evaluates shape functions for a 9 node reference quadrilateral.
c
c  Reference Element Q9:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8  9  6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(9), the basis functions at the point.
c
c    Output, double precision DTDR(9), the R basis derivatives at the point.
c
c    Output, double precision DTDS(9), the S basis derivatives at the point.
c
      implicit none

      double precision dtdr(9)
      double precision dtds(9)
      double precision r
      double precision s
      double precision t(9)

      t(1) =    4.0D+00 * ( r - 1.0D+00 ) * ( r - 0.5D+00 ) 
     &  * ( s - 1.0D+00 )  * ( s - 0.5D+00 )
      t(2) =    4.0D+00 * r * ( r - 0.5D+00 ) * ( s - 1.0D+00 ) 
     &  * ( s - 0.5D+00 )
      t(3) =    4.0D+00 * r * ( r - 0.5D+00 ) * s * ( s - 0.5D+00 )
      t(4) =    4.0D+00 * ( r - 1.0D+00 ) * ( r - 0.5D+00 ) * s 
     &  * ( s - 0.5D+00 )
      t(5) = -  8.0D+00 * r * ( r - 1.0D+00 ) * ( s - 1.0D+00 ) 
     &  * ( s - 0.5D+00 )
      t(6) = -  8.0D+00 * r * ( r - 0.5D+00 ) * s * ( s - 1.0D+00 )
      t(7) = -  8.0D+00 * r * ( r - 1.0D+00 ) * s * ( s - 0.5D+00 )
      t(8) = -  8.0D+00 * ( r - 1.0D+00 ) * ( r - 0.5D+00 ) * s 
     &  * ( s - 1.0D+00 )
      t(9) =   16.0D+00 * r * ( r - 1.0D+00 ) * s * ( s - 1.0D+00 )

      dtdr(1) =   4.0D+00 * ( 2.0D+00 * r - 1.5D+00 ) * ( s - 1.0D+00 ) 
     &  * ( s - 0.5D+00 )
      dtdr(2) =   4.0D+00 * ( 2.0D+00 * r - 0.5D+00 ) * ( s - 1.0D+00 ) 
     &  * ( s - 0.5D+00 )
      dtdr(3) =   4.0D+00 * ( 2.0D+00 * r - 0.5D+00 ) * s 
     &  * ( s - 0.5D+00 )
      dtdr(4) =   4.0D+00 * ( 2.0D+00 * r - 1.5D+00 ) * s 
     &  * ( s - 0.5D+00 )

      dtdr(5) = - 8.0D+00 * ( 2.0D+00 * r - 1.0D+00 ) * ( s - 1.0D+00 ) 
     &  * ( s - 0.5D+00 )
      dtdr(6) = - 8.0D+00 * ( 2.0D+00 * r - 0.5D+00 ) * s 
     &  * ( s - 1.0D+00 )
      dtdr(7) = - 8.0D+00 * ( 2.0D+00 * r - 1.0D+00 ) * s 
     &  * ( s - 0.5D+00 )
      dtdr(8) = - 8.0D+00 * ( 2.0D+00 * r - 1.5D+00 ) * s 
     &  * ( s - 1.0D+00 )
      dtdr(9) =  16.0D+00 * ( 2.0D+00 * r - 1.0D+00 ) * s 
     &  * ( s - 1.0D+00 )

      dtds(1) =   4.0D+00 * ( r - 1.0D+00 ) * ( r - 0.5D+00 ) 
     &  * ( 2.0D+00 * s - 1.5D+00 )
      dtds(2) =   4.0D+00 * r * ( r - 0.5D+00 ) 
     &  * ( 2.0D+00 * s - 1.5D+00 )
      dtds(3) =   4.0D+00 * r * ( r - 0.5D+00 ) 
     &  * ( 2.0D+00 * s - 0.5D+00 )
      dtds(4) =   4.0D+00 * ( r - 1.0D+00 ) * ( r - 0.5D+00 ) 
     &  * ( 2.0D+00 * s - 0.5D+00 )
      dtds(5) = - 8.0D+00 * r * ( r - 1.0D+00 ) 
     &  * ( 2.0D+00 * s - 1.5D+00 )
      dtds(6) = - 8.0D+00 * r * ( r - 0.5D+00 ) 
     &  * ( 2.0D+00 * s - 1.0D+00 )
      dtds(7) = - 8.0D+00 * r * ( r - 1.0D+00 ) 
     &  * ( 2.0D+00 * s - 0.5D+00 ) 
      dtds(8) = - 8.0D+00 * ( r - 1.0D+00 ) * ( r - 0.5D+00 ) 
     &  * ( 2.0D+00 * s - 1.0D+00 )
      dtds(9) =  16.0D+00 * r * ( r - 1.0D+00 ) 
     &  * ( 2.0D+00 * s - 1.0D+00 )

      return
      end
      subroutine shape_q12 ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_Q12 evaluates shape functions for a 12 node reference quadrilateral.
c
c  Reference Element Q12:
c
c    |
c    1  9-10-11-12
c    |  |        |
c    |  7        8
c    S  |        |
c    |  5        6
c    |  |        |
c    0  1--2--3--4
c    |
c    +--0---R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(12), the basis functions at the point.
c
c    Output, double precision DTDR(12), the R basis derivatives at the point.
c
c    Output, double precision DTDS(12), the S basis derivatives at the point.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision corner
      double precision d
      double precision dcdr
      double precision dcds
      double precision dtdr(12)
      double precision dtds(12)
      double precision r
      double precision s
      double precision t(12)

      a = 0.0D+00
      b = 1.0D+00 / 3.0D+00
      c = 2.0D+00 / 3.0D+00
      d = 1.0D+00

      corner = 9.0D+00 * ( 
     &    ( 2.0D+00 * r - 1.0D+00 ) * ( 2.0D+00 * r - 1.0D+00 ) 
     &  + ( 2.0D+00 * s - 1.0D+00 ) * ( 2.0D+00 * s - 1.0D+00 ) ) 
     &  - 10.0D+00

      t(1) =     0.125D+00  * ( r - d ) * ( s - d ) * corner
      t(2) =  - 13.5D+00    * ( r - a ) * ( r - c ) * ( r - d ) 
     &  * ( s - d )
      t(3) =    13.5D+00    * ( r - a ) * ( r - b ) * ( r - d ) 
     &  * ( s - d )
      t(4) =   - 0.125D+00  * ( r - a ) * ( s - d ) * corner
      t(5) =  - 13.5D+00    * ( r - d ) * ( s - a ) * ( s - c ) 
     &  * ( s - d ) 
      t(6) =    13.5D+00    * ( r - a ) * ( s - a ) * ( s - c ) 
     &  * ( s - d )
      t(7) =    13.5D+00    * ( r - d ) * ( s - a ) * ( s - b ) 
     &  * ( s - d )
      t(8) =  - 13.5D+00    * ( r - a ) * ( s - a ) * ( s - b ) 
     &  * ( s - d )
      t(9) =   - 0.125D+00  * ( r - d ) * ( s - a ) * corner
      t(10) =   13.5D+00    * ( r - a ) * ( r - c ) * ( r - d ) 
     &  * ( s - a )
      t(11) = - 13.5D+00    * ( r - a ) * ( r - b ) * ( r - d ) 
     &  * ( s - a )
      t(12) =    0.125D+00  * ( r - a ) * ( s - a ) * corner
     
      dcdr = 36.0D+00 * ( 2.0D+00 * r - 1.0D+00 )

      dtdr(1) =  0.125 * ( s - d ) * ( ( r - d ) * dcdr + corner )
      dtdr(2) =  - 13.5D+00 * ( s - d ) * ( 3.0D+00 * r * r 
     &  - 2.0D+00 * ( a + c + d ) * r + a * c + c * d + d * a ) 
      dtdr(3) =    13.5D+00 * ( s - d ) * ( 3.0D+00 * r * r 
     &  - 2.0D+00 * ( a + b + d ) * r + a * b + b * d + d * a )
      dtdr(4) = - 0.125D+00 * ( s - d ) * ( ( r - a ) * dcdr + corner )
      dtdr(5) = - 13.5D+00 * ( s - a ) * ( s - c ) * ( s - d ) 
      dtdr(6) =   13.5D+00 * ( s - a ) * ( s - c ) * ( s - d )
      dtdr(7) =   13.5D+00 * ( s - a ) * ( s - b ) * ( s - d )
      dtdr(8) = - 13.5D+00 * ( s - a ) * ( s - b ) * ( s - d )
      dtdr(9) = - 0.125D+00 * ( s - a ) * ( ( r - d ) * dcdr + corner )
      dtdr(10) =   13.5D+00 * ( s - a ) * ( 3.0D+00 * r * r 
     &  - 2.0D+00 * ( a + c + d ) * r + a * c + c * d + d * a ) 
      dtdr(11) = - 13.5D+00 * ( s - a ) * ( 3.0D+00 * r * r 
     &  - 2.0D+00 * ( a + b + d ) * r + a * b + b * d + d * a )
      dtdr(12) = 0.125D+00 * ( s - a ) * ( ( r - a ) * dcdr + corner )

      dcds = 36.0D+00 * ( 2.0D+00 * s - 1.0D+00 )

      dtds(1) =  0.125D+00 * ( r - d ) * ( corner + ( s - d ) * dcds )
      dtds(2) =  - 13.5D+00 * ( r - a ) * ( r - c ) * ( r - d ) 
      dtds(3) =  13.5D+00 * ( r - a ) * ( r - b ) * ( r - d )
      dtds(4) = - 0.125D+00  * ( r - a ) * ( corner + ( s - d ) * dcds )
      dtds(5) =  - 13.5D+00 * ( r - d ) * ( 3.0D+00 * s * s 
     &  - 2.0D+00 * ( a + c + d ) * s + a * c + c * d + d * a )
      dtds(6) =  13.5D+00 * ( r - a ) * ( 3.0D+00 * s * s 
     &  - 2.0D+00 * ( a + c + d ) * s + a * c + c * d + d * a )
      dtds(7) =  13.5D+00 * ( r - d ) * ( 3.0D+00 * s * s 
     &  - 2.0D+00 * ( a + b + d ) * s + a * b + b * d + d * a )
      dtds(8) =  - 13.5D+00 * ( r - a ) * ( 3.0D+00 * s * s 
     &  - 2.0D+00 * ( a + b + d ) * s + a * b + b * d + d * a )
      dtds(9) =  - 0.125D+00 * ( r - d ) * ( corner + ( s - a ) * dcds )
      dtds(10) = 13.5D+00 * ( r - a ) * ( r - c ) * ( r - d ) 
      dtds(11) = - 13.5D+00 * ( r - a ) * ( r - b ) * ( r - d ) 
      dtds(12) = 0.125D+00 * ( r - a ) * ( corner + ( s - a ) * dcds )

      return
      end
      subroutine shape_q16 ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_Q16 evaluates shape functions for a 16 node reference quadrilateral.
c
c  Reference Element Q16:
c
c    |
c    1 13--14--15--16
c    |  |   :   :   |
c    |  |   :   :   |
c    |  9..10..11..12
c    S  |   :   :   |
c    |  |   :   :   |
c    |  5...6...7...8
c    |  |   :   :   |
c    |  |   :   :   |  
c    0  1---2---3---4
c    |
c    +--0-----R-----1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(16), the basis functions at the point.
c
c    Output, double precision DTDR(16), the R basis derivatives at the point.
c
c    Output, double precision DTDS(16), the S basis derivatives at the point.
c
      implicit none

      double precision dabc
      double precision dabd
      double precision dacd
      double precision dbcd
      double precision dtdr(16)
      double precision dtds(16)
      double precision r
      double precision ra
      double precision rb
      double precision rc
      double precision rd
      double precision s
      double precision sa
      double precision sb
      double precision sc
      double precision sd
      double precision t(16)

      ra = r - 0.0D+00
      rb = r - 1.0D+00 / 3.0D+00
      rc = r - 2.0D+00 / 3.0D+00
      rd = r - 1.0D+00

      sa = s - 0.0D+00
      sb = s - 1.0D+00 / 3.0D+00
      sc = s - 2.0D+00 / 3.0D+00
      sd = s - 1.0D+00

      t(1)  =   (  81.0D+00 / 4.0D+00 ) * rb * rc * rd * sb * sc * sd
      t(2)  = - ( 243.0D+00 / 4.0D+00 ) * ra * rc * rd * sb * sc * sd
      t(3)  =   ( 243.0D+00 / 4.0D+00 ) * ra * rb * rd * sb * sc * sd
      t(4)  = - (  81.0D+00 / 4.0D+00 ) * ra * rb * rc * sb * sc * sd

      t(5)  = - ( 243.0D+00 / 4.0D+00 ) * rb * rc * rd * sa * sc * sd
      t(6)  =   ( 729.0D+00 / 4.0D+00 ) * ra * rc * rd * sa * sc * sd
      t(7)  = - ( 729.0D+00 / 4.0D+00 ) * ra * rb * rd * sa * sc * sd
      t(8)  =   ( 243.0D+00 / 4.0D+00 ) * ra * rb * rc * sa * sc * sd

      t(9)  =   ( 243.0D+00 / 4.0D+00 ) * rb * rc * rd * sa * sb * sd
      t(10) = - ( 729.0D+00 / 4.0D+00 ) * ra * rc * rd * sa * sb * sd
      t(11) =   ( 729.0D+00 / 4.0D+00 ) * ra * rb * rd * sa * sb * sd
      t(12) = - ( 243.0D+00 / 4.0D+00 ) * ra * rb * rc * sa * sb * sd

      t(13) = - (  81.0D+00 / 4.0D+00 ) * rb * rc * rd * sa * sb * sc
      t(14) =   ( 243.0D+00 / 4.0D+00 ) * ra * rc * rd * sa * sb * sc
      t(15) = - ( 243.0D+00 / 4.0D+00 ) * ra * rb * rd * sa * sb * sc
      t(16) =   (  81.0D+00 / 4.0D+00 ) * ra * rb * rc * sa * sb * sc

      dbcd = 3.0D+00 * r * r -  4.0D+00 * r       + 11.0D+00 / 9.0D+00
      dacd = 3.0D+00 * r * r - 10.0D+00 * r / 3.0D+00 
     &  +  2.0D+00 / 3.0D+00
      dabd = 3.0D+00 * r * r -  8.0D+00 * r / 3.0D+00 
     &  +  1.0D+00 / 3.0D+00
      dabc = 3.0D+00 * r * r -  2.0D+00 * r       +  2.0D+00 / 9.0D+00

      dtdr(1)  =   (  81.0D+00 / 4.0D+00 ) * dbcd * sb * sc * sd
      dtdr(2)  = - ( 243.0D+00 / 4.0D+00 ) * dacd * sb * sc * sd
      dtdr(3)  =   ( 243.0D+00 / 4.0D+00 ) * dabd * sb * sc * sd
      dtdr(4)  = - (  81.0D+00 / 4.0D+00 ) * dabc * sb * sc * sd
      dtdr(5)  = - ( 243.0D+00 / 4.0D+00 ) * dbcd * sa * sc * sd
      dtdr(6)  =   ( 729.0D+00 / 4.0D+00 ) * dacd * sa * sc * sd
      dtdr(7)  = - ( 729.0D+00 / 4.0D+00 ) * dabd * sa * sc * sd
      dtdr(8)  =   ( 243.0D+00 / 4.0D+00 ) * dabc * sa * sc * sd
      dtdr(9)  =   ( 243.0D+00 / 4.0D+00 ) * dbcd * sa * sb * sd
      dtdr(10) = - ( 729.0D+00 / 4.0D+00 ) * dacd * sa * sb * sd
      dtdr(11) =   ( 729.0D+00 / 4.0D+00 ) * dabd * sa * sb * sd
      dtdr(12) = - ( 243.0D+00 / 4.0D+00 ) * dabc * sa * sb * sd
      dtdr(13) = - (  81.0D+00 / 4.0D+00 ) * dbcd * sa * sb * sc
      dtdr(14) =   ( 243.0D+00 / 4.0D+00 ) * dacd * sa * sb * sc
      dtdr(15) = - ( 243.0D+00 / 4.0D+00 ) * dabd * sa * sb * sc
      dtdr(16) =   (  81.0D+00 / 4.0D+00 ) * dabc * sa * sb * sc

      dbcd = 3.0D+00 * s * s -  4.0D+00 * s       + 11.0D+00 / 9.0D+00
      dacd = 3.0D+00 * s * s - 10.0D+00 * s / 3.0D+00 
     &  +  2.0D+00 / 3.0D+00
      dabd = 3.0D+00 * s * s -  8.0D+00 * s / 3.0D+00 
     &  +  1.0D+00 / 3.0D+00
      dabc = 3.0D+00 * s * s -  2.0D+00 * s       +  2.0D+00 / 9.0D+00

      dtds(1)  =   (  81.0D+00 / 4.0D+00 ) * rb * rc * rd * dbcd
      dtds(2)  = - ( 243.0D+00 / 4.0D+00 ) * ra * rc * rd * dbcd
      dtds(3)  =   ( 243.0D+00 / 4.0D+00 ) * ra * rb * rd * dbcd
      dtds(4)  = - (  81.0D+00 / 4.0D+00 ) * ra * rb * rc * dbcd
      dtds(5)  = - ( 243.0D+00 / 4.0D+00 ) * rb * rc * rd * dacd
      dtds(6)  =   ( 729.0D+00 / 4.0D+00 ) * ra * rc * rd * dacd
      dtds(7)  = - ( 729.0D+00 / 4.0D+00 ) * ra * rb * rd * dacd
      dtds(8)  =   ( 243.0D+00 / 4.0D+00 ) * ra * rb * rc * dacd
      dtds(9)  =   ( 243.0D+00 / 4.0D+00 ) * rb * rc * rd * dabd
      dtds(10) = - ( 729.0D+00 / 4.0D+00 ) * ra * rc * rd * dabd
      dtds(11) =   ( 729.0D+00 / 4.0D+00 ) * ra * rb * rd * dabd
      dtds(12) = - ( 243.0D+00 / 4.0D+00 ) * ra * rb * rc * dabd
      dtds(13) = - (  81.0D+00 / 4.0D+00 ) * rb * rc * rd * dabc
      dtds(14) =   ( 243.0D+00 / 4.0D+00 ) * ra * rc * rd * dabc
      dtds(15) = - ( 243.0D+00 / 4.0D+00 ) * ra * rb * rd * dabc
      dtds(16) =   (  81.0D+00 / 4.0D+00 ) * ra * rb * rc * dabc
      
      return
      end
      subroutine shape_ql ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_QL evaluates shape functions for a 6 node quadratic/linear.
c
c  Reference Element QL:
c
c    |
c    1  4--5--6
c    |  |     |
c    |  |     |
c    S  |     |
c    |  |     |
c    |  |     |
c    0  1--2--3
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(6), the basis functions at the point.
c
c    Output, double precision DTDR(6), the R basis derivatives at the point.
c
c    Output, double precision DTDS(6), the S basis derivatives at the point.
c
      implicit none

      double precision dtdr(6)
      double precision dtds(6)
      double precision r
      double precision s
      double precision t(6)

      t(1) = - 2.0D+00 *     ( r - 0.5D+00 ) * ( r - 1.0D+00 )     
     &  * ( s - 1.0D+00 )
      t(2) =   4.0D+00 * r                   * ( r - 1.0D+00 )     
     &  * ( s - 1.0D+00 )
      t(3) = - 2.0D+00 * r * ( r - 0.5D+00 )                       
     &  * ( s - 1.0D+00 )
      t(4) =   2.0D+00 *     ( r - 0.5D+00 ) * ( r - 1.0D+00 ) * s
      t(5) = - 4.0D+00 * r                   * ( r - 1.0D+00 ) * s
      t(6) =   2.0D+00 * r * ( r - 0.5D+00 )                   * s

      dtdr(1) = 2.0D+00 * ( - 2.0D+00 * r + 1.5D+00 )     
     &  * ( s - 1.0D+00 )
      dtdr(2) = 4.0D+00 * (   2.0D+00 * r - 1.0D+00 )     
     &  * ( s - 1.0D+00 )
      dtdr(3) = 2.0D+00 * ( - 2.0D+00 * r + 0.5D+00 )     
     &  * ( s - 1.0D+00 ) 
      dtdr(4) = 2.0D+00 * (   2.0D+00 * r - 1.5D+00 ) * s
      dtdr(5) = 4.0D+00 * ( - 2.0D+00 * r + 1.0D+00 ) * s
      dtdr(6) = 2.0D+00 * (   2.0D+00 * r - 0.5D+00 ) * s

      dtds(1) = - 2.0D+00 *     ( r - 0.5D+00 ) * ( r - 1.0D+00 )
      dtds(2) =   4.0D+00 * r                   * ( r - 1.0D+00 )
      dtds(3) = - 2.0D+00 * r * ( r - 0.5D+00 )
      dtds(4) =   2.0D+00 *     ( r - 0.5D+00 ) * ( r - 1.0D+00 )
      dtds(5) = - 4.0D+00 * r                   * ( r - 1.0D+00 )
      dtds(6) =   2.0D+00 * r * ( r - 0.5D+00 )

      return
      end
      subroutine shape_t3 ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_T3 evaluates shape functions for a 3 node reference triangle.
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(3), the basis functions at the point.
c
c    Output, double precision DTDR(3), the R basis derivatives at the point.
c
c    Output, double precision DTDS(3), the S basis derivatives at the point.
c
      implicit none

      double precision dtdr(3)
      double precision dtds(3)
      double precision r
      double precision s
      double precision t(3)

      t(1) = 1.0D+00 - r - s
      t(2) =           r
      t(3) =               s

      dtdr(1) = -1.0D+00
      dtdr(2) =  1.0D+00
      dtdr(3) =  0.0D+00

      dtds(1) = -1.0D+00
      dtds(2) =  0.0D+00
      dtds(3) =  1.0D+00

      return
      end
      subroutine shape_t4 ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_T4 evaluates shape functions for a 4 node reference triangle.
c
c  Reference Element T4:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  | 4 \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 October 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(4), the basis functions at the point.
c
c    Output, double precision DTDR(4), the R basis derivatives at the point.
c
c    Output, double precision DTDS(4), the S basis derivatives at the point.
c
      implicit none

      double precision dtdr(4)
      double precision dtds(4)
      double precision r
      double precision s
      double precision t(4)

      t(1) = ( 1.0D+00 - 9.0D+00 * r * s ) * ( 1.0D+00 - r - s )
      t(2) = r * ( 1.0D+00 - 9.0D+00 * ( 1.0D+00 - r - s ) * s )
      t(3) = s * ( 1.0D+00 - 9.0D+00 * ( 1.0D+00 - r - s ) * r )
      t(4) = 27.0D+00 * ( 1.0D+00 - r - s ) * r * s

      dtdr(1) = -1.0D+00 +  9.0D+00 * ( - s + 2.0D+00 * r * s + s**2 )
      dtdr(2) =  1.0D+00 +  9.0D+00 * ( - s + 2.0D+00 * r * s + s**2 )
      dtdr(3) =             9.0D+00 * ( - s + 2.0D+00 * r * s + s**2 )
      dtdr(4) =          - 27.0D+00 * ( - s + 2.0D+00 * r * s + s**2 )

      dtds(1) = -1.0D+00 +  9.0D+00 * ( - r + r**2 + 2.0D+00 * r * s )
      dtds(2) =             9.0D+00 * ( - r + r**2 + 2.0D+00 * r * s )
      dtds(3) =  1.0D+00 +  9.0D+00 * ( - r + r**2 + 2.0D+00 * r * s )
      dtds(4) =          - 27.0D+00 * ( - r + r**2 + 2.0D+00 * r * s )

      return
      end
      subroutine shape_t6 ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_T6 evaluates shape functions for a 6 node reference triangle.
c
c  Reference Element T6:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  6  5
c    |  |   \
c    |  |    \
c    0  1--4--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(6), the basis functions at the point.
c
c    Output, double precision DTDR(6), the R basis derivatives at the point.
c
c    Output, double precision DTDS(6), the S basis derivatives at the point.
c
      implicit none

      double precision dtdr(6)
      double precision dtds(6)
      double precision r
      double precision s
      double precision t(6)

      t(1) = 2.0D+00 *     ( 1.0D+00 - r - s ) * ( 0.5D+00 - r - s )
      t(2) = 2.0D+00 * r * ( r - 0.5D+00 )
      t(3) = 2.0D+00 * s * ( s - 0.5D+00 )
      t(4) = 4.0D+00 * r * ( 1.0D+00 - r - s )
      t(5) = 4.0D+00 * r * s
      t(6) = 4.0D+00 * s * ( 1.0D+00 - r - s )

      dtdr(1) = - 3.0D+00 + 4.0D+00 * r + 4.0D+00 * s
      dtdr(2) = - 1.0D+00 + 4.0D+00 * r
      dtdr(3) =   0.0D+00
      dtdr(4) =   4.0D+00 - 8.0D+00 * r - 4.0D+00 * s
      dtdr(5) =                           4.0D+00 * s
      dtdr(6) =                         - 4.0D+00 * s

      dtds(1) = - 3.0D+00 + 4.0D+00 * r + 4.0D+00 * s
      dtds(2) =   0.0D+00
      dtds(3) = - 1.0D+00               + 4.0D+00 * s
      dtds(4) =           - 4.0D+00 * r
      dtds(5) =             4.0D+00 * r
      dtds(6) =   4.0D+00 - 4.0D+00 * r - 8.0D+00 * s

      return
      end
      subroutine shape_t10 ( r, s, t, dtdr, dtds )

c*********************************************************************72
c
cc SHAPE_T10 evaluates shape functions for a 10 node reference triangle.
c
c  Reference Element T10:
c
c    |
c    1  10
c    |  |\
c    |  | \
c    |  8  9
c    |  |   \
c    S  |    \
c    |  5  6  7
c    |  |      \
c    |  |       \
c    0  1--2--3--4
c    |
c    +--0----R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, S, the reference coordinates of a point.
c
c    Output, double precision T(10), the basis functions at the point.
c
c    Output, double precision DTDR(10), the R basis derivatives at the point.
c
c    Output, double precision DTDS(10), the S basis derivatives at the point.
c
      implicit none

      double precision a 
      double precision b
      double precision c
      double precision dtdr(10)
      double precision dtds(10)
      double precision r
      double precision s
      double precision t(10)

      a = 1.0D+00 / 3.0D+00
      b = 2.0D+00 / 3.0D+00
      c = 1.0D+00

      t(1)  = 4.5D+00 * ( a - r - s ) * ( b - r - s ) * ( c - r - s )
      t(2)  = 13.5D+00 * r * ( b - r - s ) * ( c - r - s ) 
      t(3)  = - 13.5D+00 * r * ( a - r ) * ( c - r - s )
      t(4)  = 4.5D+00 * r * ( a - r ) * ( b - r )
      t(5)  = 13.5D+00 * s * ( b - r - s ) * ( c - r - s )
      t(6)  = 27.0D+00 * r * s * ( c - r - s )
      t(7)  = 13.5D+00 * r * s * ( r - a )
      t(8)  = 13.5D+00 * s * ( s - a ) * ( c - r - s )
      t(9)  = 13.5D+00 * r * s * ( s - a )
      t(10) = 4.5D+00 * s * ( a - s ) * ( b - s )

      dtdr(1) = 4.5D+00 * ( ( a - s ) 
     &  * ( 2.0D+00 * r - c - b + 2.0D+00 * s ) 
     &  - ( s - b ) * ( s - c ) 
     &  - 2.0D+00 * ( 2.0D+00 * s - b - c ) * r 
     &  - 3.0D+00 * r * r )
      dtdr(2) = 13.5D+00 * ( 
     &  ( s - b ) * ( s - c ) + 2.0D+00 * ( 2.0D+00 * s - b - c ) * r 
     &  + 3.0D+00 * r * r )
      dtdr(3) = - 13.5D+00 * ( a * ( c - s ) 
     &  + 2.0D+00 * ( s - a - c ) * r 
     &  + 3.0D+00 * r * r )
      dtdr(4) = 4.5D+00 * ( a * b - 2.0D+00 * ( a + b ) * r 
     &  + 3.0D+00 * r * r )
      dtdr(5) = 13.5D+00 * s * ( 2.0D+00 * s - b - c + 2.0D+00 * r )
      dtdr(6) = 27.0D+00 * s * ( c - s - 2.0D+00 * r )
      dtdr(7) = 13.5D+00 * s * ( 2.0D+00 * r - a )
      dtdr(8) = - 13.5D+00 * s * ( s - a )
      dtdr(9) = 13.5D+00 * s * ( s - a)
      dtdr(10) = 0.0D+00

      dtds(1) = 4.5D+00 * ( ( a - r ) * ( 2.0D+00 * s - c - b 
     &  + 2.0D+00 * r ) 
     &  - ( r - b ) * ( r - c ) - 2.0D+00 * ( 2.0D+00 * r - b - c ) * s 
     &  - 3.0D+00 * s * s )
      dtds(2) = 13.5D+00 * r * ( 2.0D+00 * s + 2.0D+00 * r - b - c )
      dtds(3) = 13.5D+00 * r * ( a - r )
      dtds(4) = 0.0D+00
      dtds(5) = 13.5D+00 * ( ( r - b ) * ( r - c ) + 
     &  2.0D+00 * ( 2.0D+00 * r - b - c ) * s + 3.0D+00 * s * s )
      dtds(6) = 27.0D+00 * r * ( c - r - 2.0D+00 * s )
      dtds(7) = 13.5D+00 * r * ( r - a )
      dtds(8) = - 13.5D+00 * ( a * ( c - r ) 
     &  + 2.0D+00 * ( r - c - a ) * s 
     &  + 3.0D+00 * s * s )
      dtds(9) = 13.5D+00 * r * ( 2.0D+00 * s - a)
      dtds(10) = 4.5D+00 * ( a * b - 2.0D+00 * ( a + b ) * s 
     &  + 3.0D+00 * s * s )

      return
      end
      subroutine shape_test ( code )

c*********************************************************************72
c
cc SHAPE_TEST verifies the shape function values at the basis nodes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element to be used.
c    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 
c    'T3', 'T4', 'T6' and 'T10'.
c
      implicit none

      integer max_order
      parameter ( max_order = 16 )

      double precision area
      character * ( * ) code
      double precision dtdr(max_order)
      double precision dtds(max_order)
      integer element_order
      integer node
      integer order_code
      double precision r(max_order)
      double precision r8vec_sum
      double precision rsum
      double precision s(max_order)
      double precision ssum
      double precision t(max_order)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SHAPE_TEST for "' // trim ( code ) 
     &  // '" shape functions.'

      element_order = order_code ( code )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Element order = ', element_order

      call node_reference ( code, r, s, area )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Basis function values at basis nodes'
      write ( *, '(a)' ) '  should form the identity matrix.'
      write ( *, '(a)' ) ' '

      do node = 1, element_order
        call shape ( code, r(node), s(node), t, dtdr, dtds )
        write ( *, '(2x,10f7.3)' ) t(1:element_order)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The R and S derivatives should sum to 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        dTdR sum      dTdS sum'
      write ( *, '(a)' ) ' '
      do node = 1, element_order
        call shape ( code, r(node), s(node), t, dtdr, dtds )
        rsum = r8vec_sum ( element_order, dtdr )
        ssum = r8vec_sum ( element_order, dtds )
        write ( *, '(2x,f14.8,f14.8)' ) rsum, ssum
      end do

      return
      end
      subroutine sphere_grid_element_num ( code, nelemx, nelemy, 
     &  element_num )

c*********************************************************************72
c
cc SPHERE_GRID_ELEMENT_NUM returns the number of elements in a sphere grid.
c
c  Discussion:
c
c    The number of elements generated will be NELEMX * NELEMY for
c    quadrilaterals, or 2 * NELEMX * ( NELEMY - 1 ) for triangles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element desired.
c    Legal values include 'Q4', 'Q9', 'Q16', 'T3', 'T6'.
c
c    Input, integer NELEMX, NELEMY, the number of quadrilaterals 
c    along the X and Y directions.  
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      character * ( * ) code
      integer element_num
      integer nelemx
      integer nelemy
      logical s_eqi

      if ( s_eqi ( code, 'Q4' ) ) then
        call sphere_grid_q4_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'Q9' ) ) then
        call sphere_grid_q9_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'Q16' ) ) then
        call sphere_grid_q16_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'T3' ) ) then
        call sphere_grid_t3_element_num ( nelemx, nelemy, element_num )
      else if ( s_eqi ( code, 'T6' ) ) then
        call sphere_grid_t6_element_num ( nelemx, nelemy, element_num )
      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SPHERE_GRID_ELEMENT_NUM - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Illegal value of CODE = "' // trim ( code ) // '".'
        element_num = -1
        stop

      end if

      return
      end
      subroutine sphere_grid_node_num ( code, nelemx, nelemy, node_num )

c*********************************************************************72
c
cc SPHERE_GRID_NODE_NUM returns the number of nodes in a sphere grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) CODE, identifies the element desired.
c    Legal values include 'Q4', 'Q9', 'Q16', 'T3', 'T6'.
c
c    Input, integer NELEMX, NELEMY, the number of quadrilaterals 
c    along the X and Y directions.  
c
c    Output, integer NODE_NUM, the number of elements in the grid.
c
      implicit none

      character * ( * ) code
      integer node_num
      integer nelemx
      integer nelemy
      logical s_eqi

      if ( s_eqi ( code, 'Q4' ) ) then
        call sphere_grid_q4_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'Q9' ) ) then
        call sphere_grid_q9_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'Q16' ) ) then
        call sphere_grid_q16_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'T3' ) ) then
        call sphere_grid_t3_node_num ( nelemx, nelemy, node_num )
      else if ( s_eqi ( code, 'T6' ) ) then
        call sphere_grid_t6_node_num ( nelemx, nelemy, node_num )
      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SPHERE_GRID_NODE_NUM - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Illegal value of CODE = "' // trim ( code ) // '".'
        node_num = -1
        stop

      end if

      return
      end
      subroutine sphere_grid_q4_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc SPHERE_GRID_Q4_ELEMENT produces a Q4 sphere grid.
c
c  Discussion:
c
c    This would be the same as the grid in a plane, except that all the
c    nodes along the bottom edge are identified (replaced by a single node
c    that is the south pole) and similarly for the top edge, and the
c    nodes on the extreme right edge are identified pairwise with those 
c    on the extreme left edge.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 4
c
c    Output:
c
c      ELEMENT_NODE =
c         1,  1,  3,  2;
c         1,  1,  4,  3;
c         1,  1,  2,  4;
c         2,  3,  6,  5;
c         3,  4,  7,  6;
c         4,  2,  5,  7;
c         5,  6,  9,  8;
c         6,  7, 10,  9;
c         7,  5,  8, 10;
c         8,  9, 11, 11;
c         9, 10, 11, 11;
c        10,  8, 11, 11;
c
c  Grid:
c
c   11----11----11----11
c    |     |     |     |
c    | E10 | E11 | E12 |
c    |     |     |     |
c    8-----9----10-----8
c    |     |     |     |
c    | E7  | E8  | E9  |
c    |     |     |     |
c    5-----6-----7-----5
c    |     |     |     |
c    | E4  | E5  | E6  |
c    |     |     |     |
c    2-----3-----4-----2
c    |     |     |     |
c    | E1  | E2  | E3  |
c    |     |     |     |
c    1-----1-----1-----1
c
c  Reference Element Q4:
c
c    |
c    1  4------3
c    |  |      |
c    S  |      |
c    |  |      |
c    |  |      |
c    0  1------2
c    |
c    +--0--R---1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(4,NELEMX*NELEMY), the nodes that form
c    each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 4 )

      integer base1
      integer base2
      integer element
      integer element_node(element_order,nelemx*nelemy)
      integer i
      integer j

      element = 0

      do j = 1, nelemy

        base1 = ( j - 1 ) * nelemx + 2 - nelemx

        do i = 1, nelemx

          base2 = base1 + i - 1

          element = element + 1

          element_node(1,element) = base2
          if ( i .lt. nelemx ) then
            element_node(2,element) = base2 + 1
          else
            element_node(2,element) = base1
          end if
          element_node(3,element) = element_node(2,element) + nelemx
          element_node(4,element) = element_node(1,element) + nelemx

          if ( j .eq. 1 ) then

            element_node( 1,element) = 1
            element_node( 2,element) = 1

          else if ( j .eq. nelemy ) then

            element_node(3,element) = base1 + nelemx
            element_node(4,element) = base1 + nelemx

          end if

        end do
      end do

      return
      end
      subroutine sphere_grid_q4_element_num ( nelemx, nelemy, 
     &  element_num )

c*********************************************************************72
c
cc SPHERE_GRID_Q4_ELEMENT_NUM counts the elements in a Q4 sphere grid.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = NELEMX * NELEMY = 6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions. 
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = nelemx * nelemy

      return
      end
      subroutine sphere_grid_q4_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc SPHERE_GRID_Q4_NODE_NUM counts nodes in a Q4 sphere grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = nelemx * ( nelemy - 1 ) + 2

      return
      end
      subroutine sphere_grid_q4_node_xyz ( nelemx, nelemy, node_xyz )

c*********************************************************************72
c
cc SPHERE_GRID_Q4_NODE_XYZ produces node coordinates for a Q4 sphere grid.
c
c  Discussion:
c
c    The number of nodes to be generated is
c
c      NODE_NUM = NELEMX * ( NELEMY - 1 ) + 2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.
c
c    Output, double precision NODE_XYZ(3,NODE_NUM), 
c    the node coordinates.
c
      implicit none

      integer nelemx
      integer nelemy

      integer i
      integer j
      integer node
      double precision node_xyz(3,nelemx*(nelemy-1)+2)
      double precision phi
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta

      node = 0

      node = node + 1      
      node_xyz(1,node) =  0.0D+00
      node_xyz(2,node) =  0.0D+00
      node_xyz(3,node) = -1.0D+00

      do j = nelemy, 2, -1

        phi = dble ( j - 1 ) * pi / dble ( nelemy )

        do i = 1, nelemx

          theta = dble ( i - 1 ) * 2.0D+00 * pi / dble ( nelemx )

          node = node + 1      
          node_xyz(1,node) = cos ( theta ) * sin ( phi )
          node_xyz(2,node) = sin ( theta ) * sin ( phi )
          node_xyz(3,node) =                 cos ( phi )

        end do
      end do

      node = node + 1      
      node_xyz(1,node) =  0.0D+00
      node_xyz(2,node) =  0.0D+00
      node_xyz(3,node) =  1.0D+00

      return
      end
      subroutine sphere_grid_q9_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc SPHERE_GRID_Q9_ELEMENT produces a Q9 sphere grid.
c
c  Discussion:
c
c    This would be the same as the grid in a plane, except that all the
c    nodes along the bottom edge are identified (replaced by a single node
c    that is the south pole) and similarly for the top edge, and the
c    nodes on the extreme right edge are identified pairwise with those 
c    on the extreme left edge.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 4
c
c    Output:
c
c      ELEMENT_NODE =
c         1,  1, 10,  8,  1,  4,  9,  2,  3;
c         1,  1, 12, 10,  1,  6, 11,  4,  5;
c         1,  1,  8, 12,  1,  2, 13,  6,  7;
c         8, 10, 22, 20,  9, 16, 21, 14, 15;
c        10, 12, 24, 22, 11, 18, 23, 16, 17;
c        12,  8, 20, 24, 13, 14, 25, 18, 19;
c        20, 22, 34, 32, 21, 28, 33, 26, 27;
c        22, 24, 36, 34, 23, 30, 35, 28, 29;
c        24, 20, 32, 36, 25, 26, 37, 30, 31;
c        32, 34, 44, 44, 33, 40, 44, 38, 39;
c        34, 36, 44, 44, 35, 42, 44, 40, 41;
c        36, 32, 44, 44, 37, 38, 44, 42, 43;
c
c  Grid:
c
c   44-44-44-44-44-44-44
c    |     |     |     |
c   38 39 40 41 42 43 38
c    |     |     |     |
c   32-33-34-35-36-37-32
c    |     |     |     |
c   26 27 28 29 30 31 26
c    |     |     |     |
c   20-21-22-23-24-25-20
c    |     |     |     |
c   14 15 16 17 18 19 14
c    |     |     |     |
c    8--9-10-11-12-13--8
c    |     |     |     |
c    2  3  4  5  6  7  2
c    |     |     |     |
c    1--1--1--1--1--1--1
c
c  Reference Element Q9:
c
c    |
c    1  4--7--3
c    |  |     |
c    |  |     |
c    S  8  9  6
c    |  |     |
c    |  |     |
c    0  1--5--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.
c
c    Output, integer ELEMENT_NODE(9,NELEMX*NELEMY), 
c    the nodes that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 9 )

      integer base1
      integer base2
      integer element
      integer element_node(element_order,4*nelemx*nelemy-2*nelemx+2)
      integer i
      integer j

      element = 0

      do j = 1, nelemy

        base1 = ( j - 1 ) * 2 * ( 2 * nelemx ) + 2 - 2 * nelemx

        do i = 1, nelemx

          base2 = base1 + 2 * ( i - 1 )

          element = element + 1

          element_node(1,element) = base2
          element_node(5,element) = base2 + 1

          if ( i .lt. nelemx ) then
            element_node(2,element) = base2 + 2
          else
            element_node(2,element) = base1
          end if

          element_node(8,element) = element_node(1,element) + 2 * nelemx
          element_node(9,element) = element_node(5,element) + 2 * nelemx
          element_node(6,element) = element_node(2,element) + 2 * nelemx

          element_node(4,element) = element_node(8,element) + 2 * nelemx
          element_node(7,element) = element_node(9,element) + 2 * nelemx
          element_node(3,element) = element_node(6,element) + 2 * nelemx

          if ( j .eq. 1 ) then

            element_node(1,element) = 1
            element_node(5,element) = 1
            element_node(2,element) = 1

          else if ( j .eq. nelemy ) then

            element_node(4,element) = base1 + 4 * nelemx
            element_node(7,element) = base1 + 4 * nelemx
            element_node(3,element) = base1 + 4 * nelemx

          end if

        end do

      end do

      return
      end
      subroutine sphere_grid_q9_element_num ( nelemx, nelemy, 
     &  element_num )

c*********************************************************************72
c
cc SPHERE_GRID_Q9_ELEMENT_NUM counts the elements in a Q9 sphere grid.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = NELEMX * NELEMY = 6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions. 
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = nelemx * nelemy

      return
      end
      subroutine sphere_grid_q9_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc SPHERE_GRID_Q9_NODE_NUM counts nodes in a Q9 sphere grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = 4 * nelemx * nelemy - 2 * nelemx + 2

      return
      end
      subroutine sphere_grid_q9_node_xyz ( nelemx, nelemy, node_xyz )

c*********************************************************************72
c
cc SPHERE_GRID_Q9_NODE_XYZ produces node coordinates for a Q9 sphere grid.
c
c  Discussion:
c
c    The number of nodes to be generated is
c
c      NODE_NUM = 4 * NELEMX * NELEMY - 2 * NELEMX + 2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.  
c
c    Output, double precision NODE_XYZ(3,NODE_NUM), 
c    the node coordinates.
c
      implicit none

      integer nelemx
      integer nelemy

      integer i
      integer j
      integer node
      double precision node_xyz(3,4*nelemx*nelemy-2*nelemx+2)
      double precision phi
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta

      node = 0

      node = node + 1      
      node_xyz(1,node) =  0.0D+00
      node_xyz(2,node) =  0.0D+00
      node_xyz(3,node) = -1.0D+00

      do j = 2 * nelemy, 2, -1

        phi = dble ( j - 1 ) * pi / dble ( 2 * nelemy )

        do i = 1, 2 * nelemx

          theta = dble ( i - 1 ) * 2.0D+00 * pi / dble ( 2 * nelemx )

          node = node + 1      
          node_xyz(1,node) = cos ( theta ) * sin ( phi )
          node_xyz(2,node) = sin ( theta ) * sin ( phi )
          node_xyz(3,node) =                 cos ( phi )

        end do
      end do

      node = node + 1      
      node_xyz(1,node) =  0.0D+00
      node_xyz(2,node) =  0.0D+00
      node_xyz(3,node) =  1.0D+00

      return
      end
      subroutine sphere_grid_q16_element ( nelemx, nelemy, 
     &  element_node )

c*********************************************************************72
c
cc SPHERE_GRID_Q16_ELEMENT produces a Q16 sphere grid.
c
c  Discussion:
c
c    This would be the same as the grid in a plane, except that all the
c    nodes along the bottom edge are identified (replaced by a single node
c    that is the south pole) and similarly for the top edge, and the
c    nodes on the extreme right edge are identified pairwise with those 
c    on the extreme left edge.
c
c  Example:
c
c    Input:
c
c      NELEMX = 2, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NODE =
c         1,  1,  1,  1,  2,  3,  4,  5,  8,  9, 10, 11, 14, 15, 16, 17;
c         1,  1,  1,  1,  5,  6,  7,  2, 11, 12, 13,  8, 17, 18, 19, 14;
c        14, 15, 16, 17, 20, 21, 22, 23, 26, 27, 28, 29, 32, 32, 32, 32;
c        17, 18, 19, 14, 23, 24, 25, 20, 29, 30, 31, 26, 32, 32, 32, 32.
c
c  Grid:
c
c   32-32-32-32-32-32-32
c    |        |        |
c    |        |        |
c   26 27 28 29 30 31 26
c    |        |        |
c    |        |        |
c   20 21 22 23 24 25 20
c    |        |        |
c    | E3     | E4     |
c   14-15-16-17-18-19-14
c    |        |        |
c    |        |        |
c    8  9 10 11 12 13  8
c    |        |        |
c    |        |        |
c    2  3  4  5  6  7  2
c    |        |        |
c    | E1     | E2     |
c    1--1--1--1--1--1--1
c
c  Reference Element Q16:
c
c    |
c    1 13--14--15--16
c    |  |   :   :   |
c    |  |   :   :   |
c    |  9..10..11..12
c    S  |   :   :   |
c    |  |   :   :   |
c    |  5...6...7...8
c    |  |   :   :   |
c    |  |   :   :   |
c    0  1---2---3---4
c    |
c    +--0-----R-----1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.  The number of elements generated will be
c    NELEMX * NELEMY.
c
c    Output, integer ELEMENT_NODE(16,NELEMX*NELEMY), the nodes 
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 16 )

      integer base1
      integer base2
      integer element
      integer element_node(element_order,nelemx*nelemy)
      integer i
      integer j

      element = 0

      do j = 1, nelemy

        base1 = ( j - 1 ) * 3 * ( 3 * nelemx ) + 2 - 3 * nelemx

        do i = 1, nelemx

          base2 = base1 + 3 * ( i - 1 )

          element = element + 1

          element_node( 1,element) = base2
          element_node( 2,element) = base2 + 1
          element_node( 3,element) = base2 + 2

          if ( i .lt. nelemx ) then
            element_node( 4,element) = base2 + 3
          else
            element_node( 4,element) = base1
          end if

          element_node( 5,element) 
     &      = element_node( 1,element) + 3 * nelemx
          element_node( 6,element) 
     &      = element_node( 2,element) + 3 * nelemx
          element_node( 7,element) 
     &      = element_node( 3,element) + 3 * nelemx
          element_node( 8,element) 
     &      = element_node( 4,element) + 3 * nelemx

          element_node( 9,element) 
     &      = element_node( 5,element) + 3 * nelemx
          element_node(10,element) 
     &      = element_node( 6,element) + 3 * nelemx
          element_node(11,element) 
     &      = element_node( 7,element) + 3 * nelemx
          element_node(12,element) 
     &      = element_node( 8,element) + 3 * nelemx

          element_node(13,element) 
     &      = element_node( 9,element) + 3 * nelemx
          element_node(14,element) 
     &      = element_node(10,element) + 3 * nelemx
          element_node(15,element) 
     &      = element_node(11,element) + 3 * nelemx
          element_node(16,element) 
     &      = element_node(12,element) + 3 * nelemx

          if ( j .eq. 1 ) then

            element_node( 1,element) = 1
            element_node( 2,element) = 1
            element_node( 3,element) = 1
            element_node( 4,element) = 1

          else if ( j .eq. nelemy ) then

            element_node(13,element) = base1 + 9 * nelemx
            element_node(14,element) = base1 + 9 * nelemx
            element_node(15,element) = base1 + 9 * nelemx
            element_node(16,element) = base1 + 9 * nelemx

          end if

        end do
      end do

      return
      end
      subroutine sphere_grid_q16_element_num ( nelemx, nelemy, 
     &  element_num )

c*********************************************************************72
c
cc SPHERE_GRID_Q16_ELEMENT_NUM counts the elements in a Q16 sphere grid.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 2
c
c    Output:
c
c      ELEMENT_NUM = NELEMX * NELEMY = 6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions. 
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = nelemx * nelemy

      return
      end
      subroutine sphere_grid_q16_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc SPHERE_GRID_Q16_NODE_NUM counts nodes in a Q16 sphere grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = 9 * nelemx * nelemy - 3 * nelemx + 2

      return
      end
      subroutine sphere_grid_q16_node_xyz ( nelemx, nelemy, node_xyz )

c*********************************************************************72
c
cc SPHERE_GRID_Q16_NODE_XYZ produces node coordinates for a Q16 sphere grid.
c
c  Discussion:
c
c    The number of nodes to be generated is
c
c      NODE_NUM = 9 * NELEMX * NELEMY - 3 * NELEMX + 2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.
c
c    Output, double precision NODE_XYZ(3,NODE_NUM), the node coordinates.
c
      implicit none

      integer nelemx
      integer nelemy

      integer i
      integer j
      integer node
      double precision node_xyz(3,9*nelemx*nelemy-3*nelemx+2)
      double precision phi
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta

      node = 0

      do j = 3 * nelemy + 1, 1, -1

        phi = dble ( j - 1 ) * pi / dble ( 3 * nelemy )

        if ( j .eq. 1 ) then

          node = node + 1
          node_xyz(1,node) =  0.0D+00
          node_xyz(2,node) =  0.0D+00
          node_xyz(3,node) =  1.0D+00

        else if ( j .lt. 3 * nelemy + 1 ) then

          do i = 1, 3 * nelemx

            theta = dble ( i - 1 ) * 2.0D+00 * pi / dble ( 3 * nelemx )

            node = node + 1      
            node_xyz(1,node) = cos ( theta ) * sin ( phi )
            node_xyz(2,node) = sin ( theta ) * sin ( phi )
            node_xyz(3,node) =                 cos ( phi )

          end do

        else if ( j .eq. 3 * nelemy + 1 ) then

          node = node + 1
          node_xyz(1,node) =  0.0D+00
          node_xyz(2,node) =  0.0D+00
          node_xyz(3,node) = -1.0D+00

        end if

      end do

      return
      end
      subroutine sphere_grid_t3_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc SPHERE_GRID_T3_ELEMENT produces a T3 sphere grid.
c
c  Discussion:
c
c    This would be the same as the grid in a plane, except that all the
c    nodes along the bottom edge are identified (replaced by a single node
c    that is the south pole) and similarly for the top edge, and the
c    nodes on the extreme right edge are identified pairwise with those 
c    on the extreme left edge.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 4
c
c    Output:
c
c      ELEMENT_NODE =
c         1,  3,  2;
c         1,  4,  3;
c         1,  2,  4;
c         2,  3,  5
c         6,  5,  3
c         3,  4,  6
c         7,  6,  4;
c         4,  2,  7;
c         5,  7,  2;
c         5,  6,  8;
c         9,  8,  6;
c         6,  7,  9;
c        10,  9,  7;
c         7,  5, 10;
c         8, 10,  5;
c         8,  9, 11;
c         9, 10, 11;
c        10,  8, 11;
c
c  Grid:
c
c   11    11    11    11
c    | \   | \   | \   |
c    |  \  |  \  |  \  |
c    |E16\ |E17 \|E18\ |
c    8-----9----10-----8
c    | \E11| \E13| \E15|
c    |  \  |  \  |  \  |
c    |E10\ |E12\ |E14\ |
c    5-----6-----7-----5
c    | \E5 | \E7 | \E9 |
c    |  \  |  \  |  \  |
c    |E4 \ |E6 \ |E8 \ |
c    2-----3-----4-----2
c      \E1 | \E2 | \E3 |
c       \  |  \  |  \  |
c        \ |   \ |   \ |
c          1     1     1
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.
c
c    Output, integer ELEMENT_NODE(3,2*NELEMX*(NELEMY-1)), the nodes
c    that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 3 )

      integer base1
      integer base2
      integer element
      integer element_node(element_order,nelemx*nelemy)
      integer i
      integer j
      integer ne
      integer nw
      integer se
      integer sw

      element = 0

      do j = 1, nelemy

        base1 = ( j - 1 ) * nelemx + 2 - nelemx

        do i = 1, nelemx

          base2 = base1 + i - 1

          sw = base2
          if ( i .lt. nelemx ) then
            se = base2 + 1
          else
            se = base1
          end if
          nw = sw + nelemx
          ne = se + nelemx

          if ( j .eq. 1 ) then
            sw = 1
            se = 1
          else if ( j .eq. nelemx ) then
            nw = base1 + nelemx
            ne = base1 + nelemx
          end if

          if ( 1 .lt. j ) then
            element = element + 1
            element_node(1,element) = sw
            element_node(2,element) = se
            element_node(3,element) = nw
          end if

          if ( j .lt. nelemy ) then
            element = element + 1
            element_node(1,element) = ne
            element_node(2,element) = nw
            element_node(3,element) = se
          end if

        end do
      end do

      return
      end
      subroutine sphere_grid_t3_element_num ( nelemx, nelemy, 
     &  element_num )

c*********************************************************************72
c
cc SPHERE_GRID_T3_ELEMENT_NUM counts the elements in a T3 sphere grid.
c
c  Example:
c
c    Input:
c
c      NELEMX = 6, NELEMY = 4
c
c    Output:
c
c      ELEMENT_NUM = 2 * NELEMX * ( NELEMY - 1 ) = 36
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along
c    the X and Y directions. 
c
c    Output, integer ELEMENT_NUM, the number of elements in 
c    the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = 2 * nelemx * ( nelemy - 1 )

      return
      end
      subroutine sphere_grid_t3_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc SPHERE_GRID_T3_NODE_NUM counts nodes in a T3 sphere grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = nelemx * ( nelemy - 1 ) + 2

      return
      end
      subroutine sphere_grid_t3_node_xyz ( nelemx, nelemy, node_xyz )

c*********************************************************************72
c
cc SPHERE_GRID_T3_NODE_XYZ produces node coordinates for a T3 sphere grid.
c
c  Discussion:
c
c    The number of nodes to be generated is
c
c      NODE_NUM = NELEMX * ( NELEMY - 1 ) + 2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along 
c    the X and Y directions. 
c
c    Output, double precision NODE_XYZ(3,NODE_NUM), 
c    the node coordinates.
c
      implicit none

      integer nelemx
      integer nelemy

      integer i
      integer j
      integer node
      double precision node_xyz(3,nelemx*(nelemy-1)+2)
      double precision phi
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta

      node = 0

      node = node + 1      
      node_xyz(1,node) =  0.0D+00
      node_xyz(2,node) =  0.0D+00
      node_xyz(3,node) = -1.0D+00

      do j = nelemy, 2, -1

        phi = dble ( j - 1 ) * pi / dble ( nelemy )

        do i = 1, nelemx

          theta = dble ( i - 1 ) * 2.0D+00 * pi / dble ( nelemx )

          node = node + 1      
          node_xyz(1,node) = cos ( theta ) * sin ( phi )
          node_xyz(2,node) = sin ( theta ) * sin ( phi )
          node_xyz(3,node) =                 cos ( phi )

        end do
      end do

      node = node + 1      
      node_xyz(1,node) =  0.0D+00
      node_xyz(2,node) =  0.0D+00
      node_xyz(3,node) =  1.0D+00

      return
      end
      subroutine sphere_grid_t6_element ( nelemx, nelemy, element_node )

c*********************************************************************72
c
cc SPHERE_GRID_T6_ELEMENT produces a T6 sphere grid.
c
c  Discussion:
c
c    This would be the same as the grid in a plane, except that all the
c    nodes along the bottom edge are identified (replaced by a single node
c    that is the south pole) and similarly for the top edge, and the
c    nodes on the extreme right edge are identified pairwise with those 
c    on the extreme left edge.
c
c  Example:
c
c    Input:
c
c      NELEMX = 3, NELEMY = 4
c
c    Output:
c
c      ELEMENT_NODE =
c        10,  8,  1,  9,  3,  4;
c        12, 10,  1, 11,  5,  6;
c         8, 12,  1, 13,  7,  2;
c         8, 10, 20,  9, 15, 14;
c        22, 20, 10, 21, 15, 16;
c        10, 12, 22, 11, 17, 16;
c        24, 22, 12, 23, 17, 18;
c        12,  8, 24, 13, 19, 18;
c        20, 24,  8, 25, 19, 14;
c        20, 22, 32, 21, 27, 26;
c        34, 32, 22, 33, 27, 28;
c        22, 24, 34, 23, 29, 28;
c        36, 34, 24, 35, 29, 30;
c        24, 20, 36, 25, 31, 30;
c        32, 36, 20, 37, 31, 26;
c        32, 34, 44, 33, 39, 38;
c        34, 36, 44, 35, 41, 40;
c        36, 32, 44, 37, 43, 42;
c
c  Grid:
c
c   44    44    44
c    |\    |\    |\
c   38 39 40 41 42 43 
c    |    \|    \|    \
c   32-33-34-35-36-37-32
c    |\    |\    |\    |
c   26 27 28 29 30 31 26
c    |    \|    \|    \|
c   20-21-22-23-24-25-20
c    |\    |\    |\    |
c   14 15 16 17 18 19 14
c    |    \|    \|    \|
c    8--9-10-11-12-13--8
c     \    |\    |\    |
c       3  4  5  6  7  2
c         \|    \|    \|
c          1     1     1
c
c  Reference Element T6:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  6  5
c    |  |   \
c    |  |    \
c    0  1--4--2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.
c
c    Output, integer ELEMENT_NODE(6,2*NELEMX*(NELEMY-1)), 
c    the nodes that form each element.
c
      implicit none

      integer nelemx
      integer nelemy
      integer element_order
      parameter ( element_order = 6 )

      integer base1
      integer base2
      integer c
      integer e
      integer element
      integer element_node(element_order,4*nelemx*nelemy-2*nelemx+2)
      integer i
      integer j
      integer n
      integer ne
      integer nw
      integer s
      integer se
      integer sw
      integer w

      element = 0

      do j = 1, nelemy

        base1 = ( j - 1 ) * 2 * ( 2 * nelemx ) + 2 - 2 * nelemx

        do i = 1, nelemx

          base2 = base1 + 2 * ( i - 1 )

          sw = base2
          s = base2 + 1
          if ( i .lt. nelemx ) then
            se = base2 + 2
          else
            se = base1
          end if

          w = sw + 2 * nelemx
          c = s  + 2 * nelemx
          e = se + 2 * nelemx

          nw = w + 2 * nelemx
          n  = c + 2 * nelemx
          ne = e + 2 * nelemx

          if ( j .eq. 1 ) then
            sw = 1
            s  = 1
            se = 1
          else if ( j .eq. nelemy ) then
            nw = base1 + 4 * nelemx
            n  = base1 + 4 * nelemx
            ne = base1 + 4 * nelemx
          end if

          if ( 1 .lt. j ) then
            element = element + 1
            element_node(1,element) = sw
            element_node(2,element) = se
            element_node(3,element) = nw
            element_node(4,element) = s
            element_node(5,element) = c
            element_node(6,element) = w
          end if

          if ( j .lt. nelemy ) then
            element = element + 1
            element_node(1,element) = ne
            element_node(2,element) = nw
            element_node(3,element) = se
            element_node(4,element) = n
            element_node(5,element) = c
            element_node(6,element) = e
          end if


        end do

      end do

      return
      end
      subroutine sphere_grid_t6_element_num ( nelemx, nelemy, 
     &  element_num )

c*********************************************************************72
c
cc SPHERE_GRID_T6_ELEMENT_NUM counts the elements in a T6 sphere grid.
c
c  Example:
c
c    Input:
c
c      NELEMX = 6, NELEMY = 4
c
c    Output:
c
c      ELEMENT_NUM = 2 * NELEMX * ( NELEMY - 1 ) = 36
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions. 
c
c    Output, integer ELEMENT_NUM, the number of elements in the grid.
c
      implicit none

      integer element_num
      integer nelemx
      integer nelemy

      element_num = 2 * nelemx * ( nelemy - 1 )

      return
      end
      subroutine sphere_grid_t6_node_num ( nelemx, nelemy, node_num )

c*********************************************************************72
c
cc SPHERE_GRID_T6_NODE_NUM counts nodes in a T6 sphere grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.
c
c    Output, integer NODE_NUM, the number of nodes in the grid.
c
      implicit none

      integer nelemx
      integer nelemy
      integer node_num

      node_num = 4 * nelemx * nelemy - 2 * nelemx + 2

      return
      end
      subroutine sphere_grid_t6_node_xyz ( nelemx, nelemy, node_xyz )

c*********************************************************************72
c
cc SPHERE_GRID_T6_NODE_XYZ produces node coordinates for a T6 sphere grid.
c
c  Discussion:
c
c    The number of nodes to be generated is
c
c      NODE_NUM = 4 * NELEMX * NELEMY - 2 * NELEMX + 2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NELEMX, NELEMY, the number of elements along the
c    X and Y directions.  
c
c    Output, double precision NODE_XYZ(3,NODE_NUM), 
c    the node coordinates.
c
      implicit none

      integer nelemx
      integer nelemy

      integer i
      integer j
      integer node
      double precision node_xyz(3,4*nelemx*nelemy-2*nelemx+2)
      double precision phi
      double precision pi 
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta

      node = 0

      node = node + 1      
      node_xyz(1,node) =  0.0D+00
      node_xyz(2,node) =  0.0D+00
      node_xyz(3,node) = -1.0D+00

      do j = 2 * nelemy, 2, -1

        phi = dble ( j - 1 ) * pi / dble ( 2 * nelemy )

        do i = 1, 2 * nelemx

          theta = dble ( i - 1 ) * 2.0D+00 * pi / dble ( 2 * nelemx )

          node = node + 1      
          node_xyz(1,node) = cos ( theta ) * sin ( phi )
          node_xyz(2,node) = sin ( theta ) * sin ( phi )
          node_xyz(3,node) =                 cos ( phi )

        end do
      end do

      node = node + 1      
      node_xyz(1,node) =  0.0D+00
      node_xyz(2,node) =  0.0D+00
      node_xyz(3,node) =  1.0D+00

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
      subroutine triangle_unit_set ( rule, xtab, ytab, weight )

c*********************************************************************72
c
cc TRIANGLE_UNIT_SET sets a quadrature rule in the unit triangle.
c
c  Discussion:
c
c    The user is responsible for determining the value of ORDER,
c    and appropriately dimensioning the arrays XTAB, YTAB and
c    WEIGHT so that they can accommodate the data.
c
c    The value of ORDER for each rule can be found by invoking
c    the function TRIANGLE_RULE_SIZE.
c
c    The integration region is:
c
c      0 <= X and 0 <= Y and X + Y <= 1.
c
c  Graph:
c
c      ^
c    1 | *
c      | |\
c    Y | | \
c      | |  \
c    0 | *---*
c      +------->
c        0 X 1
c
c   The rules are accessed by an index number, RULE.  The indices,
c   and the descriptions of the corresponding rules, are:
c
c     1, ORDER =  1, precision 1, Zienkiewicz #1.
c     2, ORDER =  2, precision 1, (the "vertex rule").
c     3, ORDER =  3, precision 2, Strang and Fix formula #1.
c     4, ORDER =  3, precision 2, Strang and Fix formula #2,
c                                 Zienkiewicz #2.
c     5, ORDER =  4, precision 3, Strang and Fix formula #3,
c                                 Zienkiewicz #3.
c     6, ORDER =  6, precision 3, Strang and Fix formula #4.
c     7, ORDER =  6, precision 3, Stroud formula T2:3-1.
c     8, ORDER =  6, precision 4, Strang and Fix formula #5.
c     9, ORDER =  7, precision 4, Strang and Fix formula #6.
c    10, ORDER =  7, precision 5, Strang and Fix formula #7,
c                                 Stroud formula T2:5-1,
c                                 Zienkiewicz #4,
c                                 Schwarz Table 2.2.
c    11, ORDER =  9, precision 6, Strang and Fix formula #8.
c    12, ORDER = 12, precision 6, Strang and Fix formula #9.
c    13, ORDER = 13, precision 7, Strang and Fix formula #10.
c        Note that there is a typographical error in Strang and Fix
c        which lists the value of the XSI(3) component of the
c        last generator point as 0.4869... when it should be 0.04869...
c    14, ORDER =  7, precision ?.
c    15, ORDER = 16, precision 7, conical product Gauss, Stroud formula T2:7-1.
c    16, ORDER = 64, precision 15, triangular product Gauss rule.
c    17, ORDER = 19, precision 8, from CUBTRI, ACM TOMS #584.
c    18, ORDER = 19, precision 9, from TRIEX, ACM TOMS #612.
c    19, ORDER = 28, precision 11, from TRIEX, ACM TOMS #612.
c    20, ORDER = 37, precision 13, from ACM TOMS #706.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Jarle Berntsen, Terje Espelid,
c    Algorithm 706,
c    DCUTRI: an algorithm for adaptive cubature over a collection of triangles,
c    ACM Transactions on Mathematical Software,
c    Volume 18, Number 3, September 1992, pages 329-342.
c
c    Elise deDoncker, Ian Robinson,
c    Algorithm 612:
c    Integration over a Triangle Using Nonlinear Extrapolation,
c    ACM Transactions on Mathematical Software,
c    Volume 10, Number 1, March 1984, pages 17-22.
c
c    Dirk Laurie,
c    Algorithm 584,
c    CUBTRI, Automatic Cubature Over a Triangle,
c    ACM Transactions on Mathematical Software,
c    Volume 8, Number 2, 1982, pages 210-218.
c
c    James Lyness, Dennis Jespersen,
c    Moderate Degree Symmetric Quadrature Rules for the Triangle,
c    Journal of the Institute of Mathematics and its Applications,
c    Volume 15, Number 1, February 1975, pages 19-32.
c
c    Hans Rudolf Schwarz,
c    Finite Element Methods,
c    Academic Press, 1988,
c    ISBN: 0126330107,
c    LC: TA347.F5.S3313.
c
c    Gilbert Strang, George Fix,
c    An Analysis of the Finite Element Method,
c    Cambridge, 1973,
c    ISBN: 096140888X,
c    LC: TA335.S77.
c
c    Arthur Stroud,
c    Approximate Calculation of Multiple Integrals,
c    Prentice Hall, 1971,
c    ISBN: 0130438936,
c    LC: QA311.S85.
c
c    Olgierd Zienkiewicz,
c    The Finite Element Method,
c    Sixth Edition,
c    Butterworth-Heinemann, 2005,
c    ISBN: 0750663200,
c    LC: TA640.2.Z54
c
c  Parameters:
c
c    Input, integer RULE, the index of the rule.
c
c    Output, double precision XTAB(ORDER), YTAB(ORDER), the abscissas.
c
c    Output, double precision WEIGHT(ORDER), the weights of the rule.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision d
      double precision e
      double precision f
      double precision g
      double precision h
      integer i
      integer j
      integer k
      integer order2
      double precision p
      double precision q
      double precision r
      integer rule
      double precision s
      double precision t
      double precision u
      double precision v
      double precision w
      double precision w1
      double precision w2
      double precision w3
      double precision w4
      double precision w5
      double precision w6
      double precision w7
      double precision w8
      double precision w9
      double precision weight(*)
      double precision weight1(8)
      double precision weight2(8)
      double precision wx
      double precision x
      double precision xtab(*)
      double precision xtab1(8)
      double precision xtab2(8)
      double precision y
      double precision ytab(*)
      double precision z
c
c  1 point, precision 1.
c
      if ( rule .eq. 1 ) then

        xtab(1)   = 0.33333333333333333333D+00

        ytab(1)   = 0.33333333333333333333D+00

        weight(1) = 1.00000000000000000000D+00
c
c  3 points, precision 1, the "vertex rule".
c
      else if ( rule .eq. 2 ) then

        xtab(1) =   1.00000000000000000000D+00
        xtab(2) =   0.00000000000000000000D+00
        xtab(3) =   0.00000000000000000000D+00

        ytab(1) =   0.00000000000000000000D+00
        ytab(2) =   1.00000000000000000000D+00
        ytab(3) =   0.00000000000000000000D+00

        weight(1) = 0.33333333333333333333D+00
        weight(2) = 0.33333333333333333333D+00
        weight(3) = 0.33333333333333333333D+00
c
c  3 points, precision 2, Strang and Fix formula #1.
c
      else if ( rule .eq. 3 ) then

        xtab(1)   = 0.66666666666666666667D+00
        xtab(2)   = 0.16666666666666666667D+00
        xtab(3)   = 0.16666666666666666667D+00

        ytab(1)   = 0.16666666666666666667D+00
        ytab(2)   = 0.66666666666666666667D+00
        ytab(3)   = 0.16666666666666666667D+00

        weight(1) = 0.33333333333333333333D+00
        weight(2) = 0.33333333333333333333D+00
        weight(3) = 0.33333333333333333333D+00
c
c  3 points, precision 2, Strang and Fix formula #2.
c
      else if ( rule .eq. 4 ) then

        xtab(1)   = 0.50000000000000000000D+00
        xtab(2)   = 0.50000000000000000000D+00
        xtab(3)   = 0.00000000000000000000D+00

        ytab(1)   = 0.00000000000000000000D+00
        ytab(2)   = 0.50000000000000000000D+00
        ytab(3)   = 0.50000000000000000000D+00

        weight(1) = 0.33333333333333333333D+00
        weight(2) = 0.33333333333333333333D+00
        weight(3) = 0.33333333333333333333D+00
c
c  4 points, precision 3, Strang and Fix formula #3.
c
      else if ( rule .eq. 5 ) then

        a =   6.0D+00
        b =  10.0D+00
        c =  18.0D+00
        d =  25.0D+00
        e = -27.0D+00
        f =  30.0D+00
        g =  48.0D+00

        xtab(1) = b / f
        xtab(2) = c / f
        xtab(3) = a / f
        xtab(4) = a / f
        ytab(1) = b / f
        ytab(2) = a / f
        ytab(3) = c / f
        ytab(4) = a / f
        weight(1) = e / g
        weight(2) = d / g
        weight(3) = d / g
        weight(4) = d / g
c
c  6 points, precision 3, Strang and Fix formula #4.
c
      else if ( rule .eq. 6 ) then

        a = 0.659027622374092D+00
        b = 0.231933368553031D+00
        c = 0.109039009072877D+00

        xtab(1) = a
        xtab(2) = a
        xtab(3) = b
        xtab(4) = b
        xtab(5) = c
        xtab(6) = c

        ytab(1) = b
        ytab(2) = c
        ytab(3) = a
        ytab(4) = c
        ytab(5) = a
        ytab(6) = b

        weight(1) = 0.16666666666666666667D+00
        weight(2) = 0.16666666666666666667D+00
        weight(3) = 0.16666666666666666667D+00
        weight(4) = 0.16666666666666666667D+00
        weight(5) = 0.16666666666666666667D+00
        weight(6) = 0.16666666666666666667D+00
c
c  6 points, precision 3, Stroud T2:3-1.
c
      else if ( rule .eq. 7 ) then

        a = 0.0D+00
        b = 0.5D+00
        c = 2.0D+00 /  3.0D+00
        d = 1.0D+00 /  6.0D+00
        v = 1.0D+00 / 30.0D+00
        w = 3.0D+00 / 10.0D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = b
        xtab(4) = c
        xtab(5) = d
        xtab(6) = d

        ytab(1) = b
        ytab(2) = a
        ytab(3) = b
        ytab(4) = d
        ytab(5) = c
        ytab(6) = d

        weight(1) = v
        weight(2) = v
        weight(3) = v
        weight(4) = w
        weight(5) = w
        weight(6) = w
c
c  6 points, precision 4, Strang and Fix, formula #5.
c
      else if ( rule .eq. 8 ) then

        a = 0.816847572980459D+00
        b = 0.091576213509771D+00
        c = 0.108103018168070D+00
        d = 0.445948490915965D+00
        v = 0.109951743655322D+00
        w = 0.223381589678011D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = b
        xtab(4) = c
        xtab(5) = d
        xtab(6) = d

        ytab(1) = b
        ytab(2) = a
        ytab(3) = b
        ytab(4) = d
        ytab(5) = c
        ytab(6) = d

        weight(1) = v
        weight(2) = v
        weight(3) = v
        weight(4) = w
        weight(5) = w
        weight(6) = w
c
c  7 points, precision 4, Strang and Fix formula #6.
c
      else if ( rule .eq. 9 ) then

        a = 1.0D+00 / 3.0D+00
        c = 0.736712498968435D+00
        d = 0.237932366472434D+00
        e = 0.025355134551932D+00
        v = 0.375000000000000D+00
        w = 0.104166666666667D+00

        xtab(1) = a
        xtab(2) = c
        xtab(3) = c
        xtab(4) = d
        xtab(5) = d
        xtab(6) = e
        xtab(7) = e

        ytab(1) = a
        ytab(2) = d
        ytab(3) = e
        ytab(4) = c
        ytab(5) = e
        ytab(6) = c
        ytab(7) = d

        weight(1) = v
        weight(2) = w
        weight(3) = w
        weight(4) = w
        weight(5) = w
        weight(6) = w
        weight(7) = w
c
c  7 points, precision 5, Strang and Fix formula #7, Stroud T2:5-1
c
      else if ( rule .eq. 10 ) then

        a = 1.0D+00 / 3.0D+00
        b = ( 9.0D+00 + 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
        c = ( 6.0D+00 -           sqrt ( 15.0D+00 ) ) / 21.0D+00
        d = ( 9.0D+00 - 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
        e = ( 6.0D+00 +           sqrt ( 15.0D+00 ) ) / 21.0D+00
        u = 0.225D+00
        v = ( 155.0D+00 - sqrt ( 15.0D+00 ) ) / 1200.0D+00
        w = ( 155.0D+00 + sqrt ( 15.0D+00 ) ) / 1200.0D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = c
        xtab(4) = c
        xtab(5) = d
        xtab(6) = e
        xtab(7) = e

        ytab(1) = a
        ytab(2) = c
        ytab(3) = b
        ytab(4) = c
        ytab(5) = e
        ytab(6) = d
        ytab(7) = e

        weight(1) = u
        weight(2) = v
        weight(3) = v
        weight(4) = v
        weight(5) = w
        weight(6) = w
        weight(7) = w
c
c  9 points, precision 6, Strang and Fix formula #8.
c
      else if ( rule .eq. 11 ) then

        a = 0.124949503233232D+00
        b = 0.437525248383384D+00
        c = 0.797112651860071D+00
        d = 0.165409927389841D+00
        e = 0.037477420750088D+00

        u = 0.205950504760887D+00
        v = 0.063691414286223D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = b
        xtab(4) = c
        xtab(5) = c
        xtab(6) = d
        xtab(7) = d
        xtab(8) = e
        xtab(9) = e

        ytab(1) = b
        ytab(2) = a
        ytab(3) = b
        ytab(4) = d
        ytab(5) = e
        ytab(6) = c
        ytab(7) = e
        ytab(8) = c
        ytab(9) = d

        weight(1) = u
        weight(2) = u
        weight(3) = u
        weight(4) = v
        weight(5) = v
        weight(6) = v
        weight(7) = v
        weight(8) = v
        weight(9) = v
c
c  12 points, precision 6, Strang and Fix, formula #9.
c
      else if ( rule .eq. 12 ) then

        a = 0.873821971016996D+00
        b = 0.063089014491502D+00
        c = 0.501426509658179D+00
        d = 0.249286745170910D+00
        e = 0.636502499121399D+00
        f = 0.310352451033785D+00
        g = 0.053145049844816D+00

        u = 0.050844906370207D+00
        v = 0.116786275726379D+00
        w = 0.082851075618374D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = b
        xtab(4) = c
        xtab(5) = d
        xtab(6) = d
        xtab(7) = e
        xtab(8) = e
        xtab(9) = f
        xtab(10) = f
        xtab(11) = g
        xtab(12) = g

        ytab(1) = b
        ytab(2) = a
        ytab(3) = b
        ytab(4) = d
        ytab(5) = c
        ytab(6) = d
        ytab(7) = f
        ytab(8) = g
        ytab(9) = e
        ytab(10) = g
        ytab(11) = e
        ytab(12) = f

        weight(1) = u
        weight(2) = u
        weight(3) = u
        weight(4) = v
        weight(5) = v
        weight(6) = v
        weight(7) = w
        weight(8) = w
        weight(9) = w
        weight(10) = w
        weight(11) = w
        weight(12) = w
c
c  13 points, precision 7, Strang and Fix, formula #10.
c
c  Note that there is a typographical error in Strang and Fix
c  which lists the value of the XSI(3) component of the
c  last generator point as 0.4869... when it should be 0.04869...
c
      else if ( rule .eq. 13 ) then

        h = 1.0D+00 / 3.0D+00
        a = 0.479308067841923D+00
        b = 0.260345966079038D+00
        c = 0.869739794195568D+00
        d = 0.065130102902216D+00
        e = 0.638444188569809D+00
        f = 0.312865496004875D+00
        g = 0.048690315425316D+00

        w = -0.149570044467670D+00
        t =  0.175615257433204D+00
        u =  0.053347235608839D+00
        v =  0.077113760890257D+00

        xtab(1) = h
        xtab(2) = a
        xtab(3) = b
        xtab(4) = b
        xtab(5) = c
        xtab(6) = d
        xtab(7) = d
        xtab(8) = e
        xtab(9) = e
        xtab(10) = f
        xtab(11) = f
        xtab(12) = g
        xtab(13) = g

        ytab(1) = h
        ytab(2) = b
        ytab(3) = a
        ytab(4) = b
        ytab(5) = d
        ytab(6) = c
        ytab(7) = d
        ytab(8) = f
        ytab(9) = g
        ytab(10) = e
        ytab(11) = g
        ytab(12) = e
        ytab(13) = f

        weight(1) = w
        weight(2) = t
        weight(3) = t
        weight(4) = t
        weight(5) = u
        weight(6) = u
        weight(7) = u
        weight(8) = v
        weight(9) = v
        weight(10) = v
        weight(11) = v
        weight(12) = v
        weight(13) = v
c
c  7 points, precision ?.
c
      else if ( rule .eq. 14 ) then

        a = 1.0D+00 / 3.0D+00
        b = 1.0D+00
        c = 0.5D+00
        z = 0.0D+00

        u = 27.0D+00 / 60.0D+00
        v =  3.0D+00 / 60.0D+00
        w =  8.0D+00 / 60.0D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = z
        xtab(4) = z
        xtab(5) = z
        xtab(6) = c
        xtab(7) = c

        ytab(1) = a
        ytab(2) = z
        ytab(3) = b
        ytab(4) = z
        ytab(5) = c
        ytab(6) = z
        ytab(7) = c

        weight(1) = u
        weight(2) = v
        weight(3) = v
        weight(4) = v
        weight(5) = w
        weight(6) = w
        weight(7) = w
c
c  16 points, Stroud T2:7-1.
c
      else if ( rule .eq. 15 ) then
c
c  Legendre rule of order 4.
c
        order2 = 4

        xtab1(1) = -0.861136311594052575223946488893D+00
        xtab1(2) = -0.339981043584856264802665759103D+00
        xtab1(3) =  0.339981043584856264802665759103D+00
        xtab1(4) =  0.861136311594052575223946488893D+00

        weight1(1) = 0.347854845137453857373063949222D+00
        weight1(2) = 0.652145154862546142626936050778D+00
        weight1(3) = 0.652145154862546142626936050778D+00
        weight1(4) = 0.347854845137453857373063949222D+00

        do i = 1, order2
          xtab1(i) = 0.5D+00 * ( xtab1(i) + 1.0D+00 )
        end do

        weight2(1) = 0.1355069134D+00
        weight2(2) = 0.2034645680D+00
        weight2(3) = 0.1298475476D+00
        weight2(4) = 0.0311809709D+00

        xtab2(1) = 0.0571041961D+00
        xtab2(2) = 0.2768430136D+00
        xtab2(3) = 0.5835904324D+00
        xtab2(4) = 0.8602401357D+00

        k = 0
        do i = 1, order2
          do j = 1, order2
            k = k + 1
            xtab(k) = xtab2(j)
            ytab(k) = xtab1(i) * ( 1.0D+00 - xtab2(j) )
            weight(k) = weight1(i) * weight2(j)
          end do
        end do
c
c  64 points, precision 15.
c
      else if ( rule .eq. 16 ) then
c
c  Legendre rule of order 8.
c
        order2 = 8

        xtab1(1) = -0.960289856497536231683560868569D+00
        xtab1(2) = -0.796666477413626739591553936476D+00
        xtab1(3) = -0.525532409916328985817739049189D+00
        xtab1(4) = -0.183434642495649804939476142360D+00
        xtab1(5) =  0.183434642495649804939476142360D+00
        xtab1(6) =  0.525532409916328985817739049189D+00
        xtab1(7) =  0.796666477413626739591553936476D+00
        xtab1(8) =  0.960289856497536231683560868569D+00

        weight1(1) = 0.101228536290376259152531354310D+00
        weight1(2) = 0.222381034453374470544355994426D+00
        weight1(3) = 0.313706645877887287337962201987D+00
        weight1(4) = 0.362683783378361982965150449277D+00
        weight1(5) = 0.362683783378361982965150449277D+00
        weight1(6) = 0.313706645877887287337962201987D+00
        weight1(7) = 0.222381034453374470544355994426D+00
        weight1(8) = 0.101228536290376259152531354310D+00

        weight2(1) = 0.00329519144D+00
        weight2(2) = 0.01784290266D+00
        weight2(3) = 0.04543931950D+00
        weight2(4) = 0.07919959949D+00
        weight2(5) = 0.10604735944D+00
        weight2(6) = 0.11250579947D+00
        weight2(7) = 0.09111902364D+00
        weight2(8) = 0.04455080436D+00

        xtab2(1) = 0.04463395529D+00
        xtab2(2) = 0.14436625704D+00
        xtab2(3) = 0.28682475714D+00
        xtab2(4) = 0.45481331520D+00
        xtab2(5) = 0.62806783542D+00
        xtab2(6) = 0.78569152060D+00
        xtab2(7) = 0.90867639210D+00
        xtab2(8) = 0.98222008485D+00

        k = 0
        do j = 1, order2
          do i = 1, order2
            k = k + 1
            xtab(k) = 1.0D+00 - xtab2(j)
            ytab(k) = 0.5D+00 * ( 1.0D+00 + xtab1(i) ) * xtab2(j)
            weight(k) = weight1(i) * weight2(j)
          end do
        end do
c
c  19 points, precision 8, from CUBTRI.
c
      else if ( rule .eq. 17 ) then

        a = 1.0D+00 / 3.0D+00
        b = ( 9.0D+00 + 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
        c = ( 6.0D+00 -       sqrt ( 15.0D+00 ) ) / 21.0D+00
        d = ( 9.0D+00 - 2.0D+00 * sqrt ( 15.0D+00 ) ) / 21.0D+00
        e = ( 6.0D+00 +       sqrt ( 15.0D+00 ) ) / 21.0D+00
        f = ( 40.0D+00 - 10.0D+00 * sqrt ( 15.0D+00 ) 
     &    + 10.0D+00 * sqrt ( 7.0D+00 ) + 2.0D+00 * sqrt ( 105.0D+00 ) ) 
     &    / 90.0D+00
        g = ( 25.0D+00 +  5.0D+00 * sqrt ( 15.0D+00 ) 
     &    -  5.0D+00 * sqrt ( 7.0D+00 ) - sqrt ( 105.0D+00 ) ) 
     &    / 90.0D+00
        p = ( 40.0D+00 + 10.0D+00 * sqrt ( 15.0D+00 ) 
     &    + 10.0D+00 * sqrt ( 7.0D+00 ) - 2.0D+00 * sqrt ( 105.0D+00 ) ) 
     &    / 90.0D+00
        q = ( 25.0D+00 -  5.0D+00 * sqrt ( 15.0D+00 ) 
     &    -  5.0D+00 * sqrt ( 7.0D+00 ) + sqrt ( 105.0D+00 ) ) / 90.0D+00
        r = ( 40.0D+00 + 10.0D+00 * sqrt ( 7.0D+00 ) ) / 90.0D+00
        s = ( 25.0D+00 +  5.0D+00 * sqrt ( 15.0D+00 ) 
     &    - 5.0D+00 * sqrt ( 7.0D+00 ) 
     &    - sqrt ( 105.0D+00 ) ) / 90.0D+00
        t = ( 25.0D+00 -  5.0D+00 * sqrt ( 15.0D+00 ) 
     &    - 5.0D+00 * sqrt ( 7.0D+00 ) 
     &    + sqrt ( 105.0D+00 ) ) / 90.0D+00

        w1 = ( 7137.0D+00 - 1800.0D+00 * sqrt ( 7.0D+00 ) ) / 62720.0D+00
        w2 = -9301697.0D+00 / 4695040.0D+00 
     &    - 13517313.0D+00 * sqrt ( 15.0D+00 ) 
     &    / 23475200.0D+00 + 764885.0D+00 * sqrt ( 7.0D+00 ) 
     &    / 939008.0D+00 
     &    + 198763.0D+00 * sqrt ( 105.0D+00 ) / 939008.0D+00
        w2 = w2 / 3.0D+00
        w3 = -9301697.0D+00 / 4695040.0D+00 
     &    + 13517313.0D+00 * sqrt ( 15.0D+00 ) 
     &    / 23475200.0D+00 
     &    + 764885.0D+00 * sqrt ( 7.0D+00 ) / 939008.0D+00 
     &    - 198763.0D+00 * sqrt ( 105.0D+00 ) / 939008.0D+00
        w3 = w3 / 3.0D+00
        w4 = ( 102791225.0D+00 - 23876225.0D+00 * sqrt ( 15.0D+00 ) 
     &    - 34500875.0D+00 * sqrt ( 7.0D+00 ) 
     &    + 9914825.0D+00 * sqrt ( 105.0D+00 ) ) / 59157504.0D+00
        w4 = w4 / 3.0D+00
        w5 = ( 102791225.0D+00 + 23876225.0D+00 * sqrt ( 15.0D+00 ) 
     &    - 34500875.0D+00 * sqrt ( 7.0D+00 ) 
     &    - 9914825D+00 * sqrt ( 105.0D+00 ) ) / 59157504.0D+00
        w5 = w5 / 3.0D+00
        w6 = ( 11075.0D+00 - 3500.0D+00 * sqrt ( 7.0D+00 ) ) 
     &    / 8064.0D+00
        w6 = w6 / 6.0D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = c
        xtab(4) = c
        xtab(5) = d
        xtab(6) = e
        xtab(7) = e
        xtab(8) = f
        xtab(9) = g
        xtab(10) = g
        xtab(11) = p
        xtab(12) = q
        xtab(13) = q 
        xtab(14) = r
        xtab(15) = r
        xtab(16) = s
        xtab(17) = s
        xtab(18) = t
        xtab(19) = t

        ytab(1) = a
        ytab(2) = c
        ytab(3) = b
        ytab(4) = c
        ytab(5) = e
        ytab(6) = d
        ytab(7) = e
        ytab(8) = g
        ytab(9) = f
        ytab(10) = g
        ytab(11) = q
        ytab(12) = p
        ytab(13) = q 
        ytab(14) = s
        ytab(15) = t
        ytab(16) = r
        ytab(17) = t
        ytab(18) = r
        ytab(19) = s

        weight(1) = w1
        weight(2) = w2
        weight(3) = w2
        weight(4) = w2
        weight(5) = w3
        weight(6) = w3
        weight(7) = w3
        weight(8) = w4
        weight(9) = w4
        weight(10) = w4
        weight(11) = w5
        weight(12) = w5
        weight(13) = w5
        weight(14) = w6
        weight(15) = w6
        weight(16) = w6
        weight(17) = w6
        weight(18) = w6
        weight(19) = w6
c
c  19 points, precision 9.
c  Lyness and Jesperson.
c
      else if ( rule .eq. 18 ) then

        a = 1.0D+00 / 3.0D+00
        b = 0.02063496160252593D+00
        c = 0.4896825191987370D+00
        d = 0.1258208170141290D+00
        e = 0.4370895914929355D+00
        f = 0.6235929287619356D+00
        g = 0.1882035356190322D+00
        r = 0.9105409732110941D+00
        s = 0.04472951339445297D+00
        t = 0.7411985987844980D+00
        u = 0.03683841205473626D+00
        v = 0.22196288916076574D+00

        w1 = 0.09713579628279610D+00
        w2 = 0.03133470022713983D+00
        w3 = 0.07782754100477543D+00
        w4 = 0.07964773892720910D+00
        w5 = 0.02557767565869810D+00
        w6 = 0.04328353937728940D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = c
        xtab(4) = c
        xtab(5) = d
        xtab(6) = e
        xtab(7) = e
        xtab(8) = f
        xtab(9) = g
        xtab(10) = g
        xtab(11) = r
        xtab(12) = s
        xtab(13) = s 
        xtab(14) = t
        xtab(15) = t
        xtab(16) = u
        xtab(17) = u
        xtab(18) = v
        xtab(19) = v

        ytab(1) = a
        ytab(2) = c
        ytab(3) = b
        ytab(4) = c
        ytab(5) = e
        ytab(6) = d
        ytab(7) = e
        ytab(8) = g
        ytab(9) = f
        ytab(10) = g
        ytab(11) = s
        ytab(12) = r
        ytab(13) = s 
        ytab(14) = u
        ytab(15) = v
        ytab(16) = t
        ytab(17) = v
        ytab(18) = t
        ytab(19) = u

        weight(1) = w1
        weight(2) = w2
        weight(3) = w2
        weight(4) = w2
        weight(5) = w3
        weight(6) = w3
        weight(7) = w3
        weight(8) = w4
        weight(9) = w4
        weight(10) = w4
        weight(11) = w5
        weight(12) = w5
        weight(13) = w5
        weight(14) = w6
        weight(15) = w6
        weight(16) = w6
        weight(17) = w6
        weight(18) = w6
        weight(19) = w6
c
c  28 points, precision 11.
c  Lyness and Jesperson.
c
      else if ( rule .eq. 19 ) then

        a = 1.0D+00 / 3.0D+00
        b = 0.9480217181434233D+00
        c = 0.02598914092828833D+00
        d = 0.8114249947041546D+00
        e = 0.09428750264792270D+00
        f = 0.01072644996557060D+00
        g = 0.4946367750172147D+00
        p = 0.5853132347709715D+00
        q = 0.2073433826145142D+00
        r = 0.1221843885990187D+00
        s = 0.4389078057004907D+00
        t = 0.6779376548825902D+00
        u = 0.04484167758913055D+00
        v = 0.27722066752827925D+00
        w = 0.8588702812826364D+00
        x = 0.0D+00
        y = 0.1411297187173636D+00

        w1 = 0.08797730116222190D+00
        w2 = 0.008744311553736190D+00
        w3 = 0.03808157199393533D+00
        w4 = 0.01885544805613125D+00
        w5 = 0.07215969754474100D+00
        w6 = 0.06932913870553720D+00
        w7 = 0.04105631542928860D+00
        w8 = 0.007362383783300573D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = c
        xtab(4) = c
        xtab(5) = d
        xtab(6) = e
        xtab(7) = e
        xtab(8) = f
        xtab(9) = g
        xtab(10) = g
        xtab(11) = p
        xtab(12) = q
        xtab(13) = q
        xtab(14) = r
        xtab(15) = s
        xtab(16) = s
        xtab(17) = t
        xtab(18) = t
        xtab(19) = u
        xtab(20) = u
        xtab(21) = v
        xtab(22) = v
        xtab(23) = w
        xtab(24) = w
        xtab(25) = x
        xtab(26) = x
        xtab(27) = y
        xtab(28) = y

        ytab(1) = a
        ytab(2) = c
        ytab(3) = b
        ytab(4) = c
        ytab(5) = e
        ytab(6) = d
        ytab(7) = e
        ytab(8) = g
        ytab(9) = f
        ytab(10) = g
        ytab(11) = q
        ytab(12) = p
        ytab(13) = q
        ytab(14) = s
        ytab(15) = r
        ytab(16) = s
        ytab(17) = u
        ytab(18) = v
        ytab(19) = t
        ytab(20) = v
        ytab(21) = t
        ytab(22) = u
        ytab(23) = x
        ytab(24) = y
        ytab(25) = w
        ytab(26) = y
        ytab(27) = w 
        ytab(28) = x

        weight(1) = w1
        weight(2) = w2
        weight(3) = w2
        weight(4) = w2
        weight(5) = w3
        weight(6) = w3
        weight(7) = w3
        weight(8) = w4
        weight(9) = w4
        weight(10) = w4
        weight(11) = w5
        weight(12) = w5
        weight(13) = w5
        weight(14) = w6
        weight(15) = w6
        weight(16) = w6
        weight(17) = w7
        weight(18) = w7
        weight(19) = w7
        weight(20) = w7
        weight(21) = w7
        weight(22) = w7
        weight(23) = w8
        weight(24) = w8
        weight(25) = w8
        weight(26) = w8
        weight(27) = w8
        weight(28) = w8
c
c  37 points, precision 13.
c
      else if ( rule .eq. 20 ) then

        a = 1.0D+00 / 3.0D+00
        b = 0.950275662924105565450352089520D+00
        c = 0.024862168537947217274823955239D+00
        d = 0.171614914923835347556304795551D+00
        e = 0.414192542538082326221847602214D+00
        f = 0.539412243677190440263092985511D+00
        g = 0.230293878161404779868453507244D+00

        w1 = 0.051739766065744133555179145422D+00
        w2 = 0.008007799555564801597804123460D+00
        w3 = 0.046868898981821644823226732071D+00
        w4 = 0.046590940183976487960361770070D+00
        w5 = 0.031016943313796381407646220131D+00
        w6 = 0.010791612736631273623178240136D+00
        w7 = 0.032195534242431618819414482205D+00
        w8 = 0.015445834210701583817692900053D+00
        w9 = 0.017822989923178661888748319485D+00
        wx = 0.037038683681384627918546472190D+00

        xtab(1) = a
        xtab(2) = b
        xtab(3) = c
        xtab(4) = c
        xtab(5) = d
        xtab(6) = e
        xtab(7) = e
        xtab(8) = f
        xtab(9) = g
        xtab(10) = g

        ytab(1) = a
        ytab(2) = c
        ytab(3) = b
        ytab(4) = c
        ytab(5) = e
        ytab(6) = d
        ytab(7) = e
        ytab(8) = g
        ytab(9) = f
        ytab(10) = g

        weight(1) = w1
        weight(2) = w2
        weight(3) = w2
        weight(4) = w2
        weight(5) = w3
        weight(6) = w3
        weight(7) = w3
        weight(8) = w4
        weight(9) = w4
        weight(10) = w4
        weight(11) = w5
        weight(12) = w5
        weight(13) = w5
        weight(14) = w6
        weight(15) = w6
        weight(16) = w6
        weight(17) = w7
        weight(18) = w7
        weight(19) = w7
        weight(20) = w8
        weight(21) = w8
        weight(22) = w8
        weight(23) = w8
        weight(24) = w8
        weight(25) = w8
        weight(26) = w9
        weight(27) = w9
        weight(28) = w9
        weight(29) = w9
        weight(30) = w9
        weight(31) = w9
        weight(32) = wx
        weight(33) = wx
        weight(34) = wx
        weight(35) = wx
        weight(36) = wx
        weight(37) = wx

        a = 0.772160036676532561750285570113D+00
        b = 0.113919981661733719124857214943D+00

        xtab(11) = a
        ytab(11) = b

        xtab(12) = b
        ytab(12) = a

        xtab(13) = b
        ytab(13) = b

        a = 0.009085399949835353883572964740D+00
        b = 0.495457300025082323058213517632D+00

        xtab(14) = a
        ytab(14) = b

        xtab(15) = b
        ytab(15) = a

        xtab(16) = b
        ytab(16) = b

        a = 0.062277290305886993497083640527D+00
        b = 0.468861354847056503251458179727D+00

        xtab(17) = a
        ytab(17) = b

        xtab(18) = b
        ytab(18) = a

        xtab(19) = b
        ytab(19) = b

        a = 0.022076289653624405142446876931D+00
        b = 0.851306504174348550389457672223D+00
        c = 1.0D+00 - a - b

        xtab(20) = a
        ytab(20) = b

        xtab(21) = a
        ytab(21) = c

        xtab(22) = b
        ytab(22) = a

        xtab(23) = b
        ytab(23) = c

        xtab(24) = c
        ytab(24) = a

        xtab(25) = c
        ytab(25) = b

        a = 0.018620522802520968955913511549D+00
        b = 0.689441970728591295496647976487D+00
        c = 1.0D+00 - a - b

        xtab(26) = a
        ytab(26) = b

        xtab(27) = a
        ytab(27) = c

        xtab(28) = b
        ytab(28) = a

        xtab(29) = b
        ytab(29) = c

        xtab(30) = c
        ytab(30) = a

        xtab(31) = c
        ytab(31) = b

        a = 0.096506481292159228736516560903D+00
        b = 0.635867859433872768286976979827D+00
        c = 1.0D+00 - a - b

        xtab(32) = a
        ytab(32) = b

        xtab(33) = a
        ytab(33) = c

        xtab(34) = b
        ytab(34) = a

        xtab(35) = b
        ytab(35) = c

        xtab(36) = c
        ytab(36) = a

        xtab(37) = c
        ytab(37) = b

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_UNIT_SET - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal value of RULE = ', rule
        stop

      end if

      return
      end
      function triangle_unit_size ( rule )

c******************************************************************************
c
cc TRIANGLE_UNIT_SIZE returns the "size" of a unit triangle quadrature rule.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gilbert Strang, George Fix,
c    An Analysis of the Finite Element Method,
c    Prentice Hall, 1973,
c    TA335.S77.
c
c    Olgierd Zienkiewicz,
c    The Finite Element Method,
c    McGraw Hill, Third Edition, 1977, page 202.
c
c  Parameters:
c
c    Input, integer RULE, the index of the rule.
c     1, ORDER =  1, precision 1, Zienkiewicz #1.
c     2, ORDER =  2, precision 1, (the "vertex rule").
c     3, ORDER =  3, precision 2, Strang and Fix formula #1.
c     4, ORDER =  3, precision 2, Strang and Fix formula #2, Zienkiewicz #2.
c     5, ORDER =  4, precision 3, Strang and Fix formula #3, Zienkiewicz #3.
c     6, ORDER =  6, precision 3, Strang and Fix formula #4.
c     7, ORDER =  6, precision 3, Stroud formula T2:3-1.
c     8, ORDER =  6, precision 4, Strang and Fix formula #5.
c     9, ORDER =  7, precision 4, Strang and Fix formula #6.
c    10, ORDER =  7, precision 5, Strang and Fix formula #7,
c        Stroud formula T2:5-1, Zienkiewicz #4, Schwarz Table 2.2.
c    11, ORDER =  9, precision 6, Strang and Fix formula #8.
c    12, ORDER = 12, precision 6, Strang and Fix formula #9.
c    13, ORDER = 13, precision 7, Strang and Fix formula #10.
c    14, ORDER =  7, precision ?.
c    15, ORDER = 16, precision 7, conical product Gauss, Stroud formula T2:7-1.
c    16, ORDER = 64, precision 15, triangular product Gauss rule.
c    17, ORDER = 19, precision 8, from CUBTRI, ACM TOMS #584.
c    18, ORDER = 19, precision 9, from TRIEX, Lyness and Jespersen.
c    19, ORDER = 28, precision 11, from TRIEX, Lyness and Jespersen.
c    20, ORDER = 37, precision 13, from ACM TOMS #706.
c
c    Output, integer TRIANGLE_UNIT_SIZE, the order of the rule.
c
      implicit none

      integer rule
      integer triangle_unit_size

      if ( rule .eq. 1 ) then
        triangle_unit_size = 1
      else if ( rule .eq. 2 ) then
        triangle_unit_size = 3
      else if ( rule .eq. 3 ) then
        triangle_unit_size = 3
      else if ( rule .eq. 4 ) then
        triangle_unit_size = 3
      else if ( rule .eq. 5 ) then
        triangle_unit_size = 4
      else if ( rule .eq. 6 ) then
        triangle_unit_size = 6
      else if ( rule .eq. 7 ) then
        triangle_unit_size = 6
      else if ( rule .eq. 8 ) then
        triangle_unit_size = 6
      else if ( rule .eq. 9 ) then
        triangle_unit_size = 7
      else if ( rule .eq. 10 ) then
        triangle_unit_size = 7
      else if ( rule .eq. 11 ) then
        triangle_unit_size = 9
      else if ( rule .eq. 12 ) then
        triangle_unit_size = 12
      else if ( rule .eq. 13 ) then
        triangle_unit_size = 13
      else if ( rule .eq. 14 ) then
        triangle_unit_size = 7
      else if ( rule .eq. 15 ) then
        triangle_unit_size = 16
      else if ( rule .eq. 16 ) then
        triangle_unit_size = 64
      else if ( rule .eq. 17 ) then
        triangle_unit_size = 19
      else if ( rule .eq. 18 ) then
        triangle_unit_size = 19
      else if ( rule .eq. 19 ) then
        triangle_unit_size = 28
      else if ( rule .eq. 20 ) then
        triangle_unit_size = 37
      else
        triangle_unit_size = -1
      end if

      return
      end
