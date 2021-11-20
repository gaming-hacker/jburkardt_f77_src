      subroutine drffti (n,wsave)

c*********************************************************************72
c
cc drffti
c
      double precision wsave(1)
c
      if (n .eq. 1) return
c
      call drfti1 (n,wsave(n+1),wsave(2*n+1))
c
      return
      end
