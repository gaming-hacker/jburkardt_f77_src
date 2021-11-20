      subroutine dhzero ( nn, kj, x, y, d, ws )

c*********************************************************************72
c
cc dhzero preconditions the CG iteration.
c
      integer nn,kj
      double precision    x(*),y(*),d(*),ws(*)
c
c     this routine preconditions the cg - iteration
c     by multipying with an approximation to the
c     inverse of the capacitance matrix.
c
c     x is input vector. x is not changed.
c     y is output vector. ie y=h0*x .
c     d is a diagonal matrix of dimension nn needed to define h0.
c     kj and ws are dummy parameters in this version of dhzero.
c
c     local.
c
      integer i
c
c  Stupid statements to shut up the "unused variable" compiler warning.
c
      kj = max ( kj, kj )
      ws(1) = max ( ws(1), ws(1) )

      do i=1,nn
        y(i)=d(i)*x(i)
      end do

      return
      end
