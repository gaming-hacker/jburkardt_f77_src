      subroutine b4step1 ( maxmx, mbc, mx, meqn, q,
     &  xlower, dx, t, dt, maux, aux )

c*********************************************************************72
c
cc B4STEP1 carries out initialization before each step in 1D.
c
c  Discussion:
c
c    called from claw1 before each call to step1.
c    use to set time-dependent aux arrays or perform other tasks
c    which must be done every time step.
c    dummy routine 
c
c  Modified:
c
c    05 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
c  Parameters:
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)

      return
      end
      subroutine b4step2(maxmx,maxmy,mbc,mx,my,meqn,q,
     &            xlower,ylower,dx,dy,t,dt,maux,aux)

c*********************************************************************72
c
cc B4STEP2 carries out initialization before each step in 2D.
c
c  Discussion:
c
c    called from claw2 before each call to step2.
c    use to set time-dependent aux arrays or perform other tasks
c    which must be done every time step.
c
c    dummy routine 
c
c  Modified:
c
c    05 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c  
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, *)

      return
      end
      subroutine b4step3(maxmx,maxmy,maxmz,mbc,mx,my,mz,meqn,q,
     &            xlower,ylower,zlower,dx,dy,dz,t,dt,maux,aux)

c*********************************************************************72
c
cc B4STEP3 carries out initialization before each step in 3D.
c
c  Discussion:
c
c    called from claw3 before each call to step3.
c    use to set time-dependent aux arrays or perform other tasks
c    which must be done every time step.
c
c    dummy routine 
c
c  Modified:
c
c    05 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &               1-mbc:maxmz+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 
     &               1-mbc:maxmz+mbc, maux)

      return
      end
      subroutine bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt,mthbc)

c*********************************************************************72
c
cc BC1 implements standard boundary condition choices for 1D.
c
c  Discussion:
c
c    At each boundary  k = 1 (left),  2 (right):
c      mthbc(k) =  0  for user-supplied BC's (must be inserted!)
c               =  1  for zero-order extrapolation
c               =  2  for periodic boundary coniditions
c               =  3  for solid walls, assuming this can be implemented
c                     by reflecting the data about the boundary and then
c                     negating the 2'nd component of q.
c
c    Extend the data from the computational region
c         i = 1, 2, ..., mx2
c    to the virtual cells outside the region, with
c         i = 1-ibc  and   i = mx+ibc   for ibc=1,...,mbc
c
c  Modified:
c
c    05 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)

      dimension mthbc(2)
c
c  left boundary:
c
      go to (100,110,120,130) mthbc(1)+1

  100 continue
c
c  user-specified boundary conditions go here in place of error output
c
      write(6,*) '*** ERROR *** mthbc(1)=0 and no BCs specified in bc1'
      stop
      go to 199

  110 continue
c
c  zero-order extrapolation:
c
      do 115 m=1,meqn
         do 115 ibc=1,mbc
               q(1-ibc,m) = q(1,m)
  115       continue
      go to 199

  120 continue
c
c  periodic:  
c
      do m=1,meqn
        do ibc=1,mbc
          q(1-ibc,m) = q(mx+1-ibc,m)
        end do
      end do

      go to 199

  130 continue
c
c  solid wall (assumes 2'nd component is velocity or momentum in x):
c
      do m=1,meqn
         do ibc=1,mbc
               q(1-ibc,m) = q(ibc,m)
        end do
      end do
c
c  negate the normal velocity:
c
      do ibc=1,mbc
            q(1-ibc,2) = -q(ibc,2)
      end do
      go to 199

  199 continue
c
c  right boundary:
c
      go to (200,210,220,230) mthbc(2)+1

  200 continue
c
c  user-specified boundary conditions go here in place of error output
c
      write(6,*) '*** ERROR *** mthbc(2)=0 and no BCs specified in bc2'
      stop
      go to 299

  210 continue
c
c  zero-order extrapolation:
c
      do 215 m=1,meqn
         do 215 ibc=1,mbc
               q(mx+ibc,m) = q(mx,m)
  215       continue
      go to 299

  220 continue
c
c  periodic:  
c
      do 225 m=1,meqn
         do 225 ibc=1,mbc
               q(mx+ibc,m) = q(ibc,m)
  225       continue
      go to 299

  230 continue
c
c  solid wall (assumes 2'nd component is velocity or momentum in x):
c
      do m=1,meqn
         do ibc=1,mbc
               q(mx+ibc,m) = q(mx+1-ibc,m)
         end do
      end do

      do ibc=1,mbc
            q(mx+ibc,2) = -q(mx+1-ibc,2)
      end do

      go to 299

  299 continue

      return
      end
      subroutine bc2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &               dx,dy,q,maux,aux,t,dt,mthbc)

c*********************************************************************72
c
cc BC2 implements standard boundary condition choices for 2D.
c
c  Discussion:
c
c    At each boundary  k = 1 (left),  2 (right),  3 (top), 4 (bottom):
c      mthbc(k) =  0  for user-supplied BC's (must be inserted!)
c               =  1  for zero-order extrapolation
c               =  2  for periodic boundary coniditions
c               =  3  for solid walls, assuming this can be implemented
c                     by reflecting the data about the boundary and then
c                     negating the 2'nd (for k=1,2) or 3'rd (for k=3,4)
c                     component of q.
c
c    Extend the data from the interior cells (1:mx, 1:my)
c    to the ghost cells outside the region:
c      (i, 1-jbc)   for jbc = 1,mbc,  i = 1-mbc, mx+mbc
c      (i, my+jbc)  for jbc = 1,mbc,  i = 1-mbc, mx+mbc
c      (1-ibc, j)   for ibc = 1,mbc,  j = 1-mbc, my+mbc
c      (mx+ibc, j)  for ibc = 1,mbc,  j = 1-mbc, my+mbc
c
c  Modified:
c
c    05 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, *)
      dimension mthbc(4)
c
c     # left boundary:
c
      go to (100,110,120,130) mthbc(1)+1

  100 continue
c
c     # user-specified boundary conditions go here in place of error output
c
      write(6,*) '*** ERROR *** mthbc(1)=0 and no BCs specified in bc2'
      stop
      go to 199

  110 continue
c
c     # zero-order extrapolation:
c
      do 115 m=1,meqn
         do 115 ibc=1,mbc
            do 115 j = 1-mbc, my+mbc
               q(1-ibc,j,m) = q(1,j,m)
  115       continue
      go to 199

  120 continue
c
c     # periodic:  
c
      do 125 m=1,meqn
         do 125 ibc=1,mbc
            do 125 j = 1-mbc, my+mbc
               q(1-ibc,j,m) = q(mx+1-ibc,j,m)
  125       continue
      go to 199

  130 continue
c
c     # solid wall (assumes 2'nd component is velocity or momentum in x):
c
      do 135 m=1,meqn
         do 135 ibc=1,mbc
            do 135 j = 1-mbc, my+mbc
               q(1-ibc,j,m) = q(ibc,j,m)
  135       continue
c
c     # negate the normal velocity:
c
      do 136 ibc=1,mbc
         do 136 j = 1-mbc, my+mbc
            q(1-ibc,j,2) = -q(ibc,j,2)
  136    continue
      go to 199

  199 continue
c
c     # right boundary:
c
      go to (200,210,220,230) mthbc(2)+1
c
  200 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(2)=0 and no BCs specified in bc2'
      stop
      go to 299

  210 continue
c     # zero-order extrapolation:
      do 215 m=1,meqn
         do 215 ibc=1,mbc
            do 215 j = 1-mbc, my+mbc
               q(mx+ibc,j,m) = q(mx,j,m)
  215       continue
      go to 299

  220 continue
c     # periodic:  
      do 225 m=1,meqn
         do 225 ibc=1,mbc
            do 225 j = 1-mbc, my+mbc
               q(mx+ibc,j,m) = q(ibc,j,m)
  225       continue
      go to 299

  230 continue
c     # solid wall (assumes 2'nd component is velocity or momentum in x):
      do 235 m=1,meqn
         do 235 ibc=1,mbc
            do 235 j = 1-mbc, my+mbc
               q(mx+ibc,j,m) = q(mx+1-ibc,j,m)
  235       continue
c     # negate the normal velocity:
      do 236 ibc=1,mbc
         do 236 j = 1-mbc, my+mbc
            q(mx+ibc,j,2) = -q(mx+1-ibc,j,2)
  236    continue
      go to 299

  299 continue
c
c
c     # bottom boundary:
c
      go to (300,310,320,330) mthbc(3)+1
c
  300 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(3)=0 and no BCs specified in bc2'
      stop
      go to 399
c
  310 continue
c     # zero-order extrapolation:
      do 315 m=1,meqn
         do 315 jbc=1,mbc
            do 315 i = 1-mbc, mx+mbc
               q(i,1-jbc,m) = q(i,1,m)
  315       continue
      go to 399

  320 continue
c     # periodic:  
      do 325 m=1,meqn
         do 325 jbc=1,mbc
            do 325 i = 1-mbc, mx+mbc
               q(i,1-jbc,m) = q(i,my+1-jbc,m)
  325       continue
      go to 399

  330 continue
c     # solid wall (assumes 3'rd component is velocity or momentum in y):
      do 335 m=1,meqn
         do 335 jbc=1,mbc
            do 335 i = 1-mbc, mx+mbc
               q(i,1-jbc,m) = q(i,jbc,m)
  335       continue
c     # negate the normal velocity:
      do 336 jbc=1,mbc
         do 336 i = 1-mbc, mx+mbc
            q(i,1-jbc,3) = -q(i,jbc,3)
  336    continue
      go to 399

  399 continue
c
c
c     # top boundary:
c
      go to (400,410,420,430) mthbc(4)+1
c
  400 continue
c     # user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(4)=0 and no BCs specified in bc2'
      stop
      go to 499

  410 continue
c     # zero-order extrapolation:
      do 415 m=1,meqn
         do 415 jbc=1,mbc
            do 415 i = 1-mbc, mx+mbc
               q(i,my+jbc,m) = q(i,my,m)
  415       continue
      go to 499

  420 continue
c     # periodic:  
      do 425 m=1,meqn
         do 425 jbc=1,mbc
            do 425 i = 1-mbc, mx+mbc
               q(i,my+jbc,m) = q(i,jbc,m)
  425       continue
      go to 499

  430 continue
c     # solid wall (assumes 3'rd component is velocity or momentum in y):
      do 435 m=1,meqn
         do 435 jbc=1,mbc
            do 435 i = 1-mbc, mx+mbc
               q(i,my+jbc,m) = q(i,my+1-jbc,m)
  435       continue
c     # negate the normal velocity:
      do 436 jbc=1,mbc
         do 436 i = 1-mbc, mx+mbc
            q(i,my+jbc,3) = -q(i,my+1-jbc,3)
  436    continue
      go to 499

  499 continue

      return
      end
      subroutine bc3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,xlower,
     &               ylower,zlower,dx,dy,dz,q,maux,aux,t,dt,mthbc)

c*********************************************************************72
c
cc BC3 implements standard boundary condition choices for 3D.
c
c  Discussion:
c
c    At each boundary  k = 1 (xlower),  2 (xupper), 
c                          3 (ylower),  4 (yupper),
c                          5 (zlower),  6 (zupper):
c      mthbc(k) =  0  for user-supplied BC's (must be inserted!)
c               =  1  for zero-order extrapolation
c               =  2  for periodic boundary coniditions
c               =  3  for solid walls, assuming this can be implemented
c                     by reflecting the data about the boundary and then
c                     negating the 2'nd (for k=1,2) or 3'rd (for k=3,4)
c                     or 4'th (for k=5,6) component of q.
c
c
c    Extend the data from the interior cells (1:mx, 1:my, 1:mz)
c    to a layer of mbc ghost cells outside the region.
c
c  Modified:
c
c    05 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &               1-mbc:maxmz+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 
     &               1-mbc:maxmz+mbc, maux)
      dimension mthbc(6)
c
c    left boundary (xlower):
c
      go to (100,110,120,130) mthbc(1)+1
c
  100 continue
c    user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(1)=0 and no BCs specified in bc3'
      stop
      go to 199
c
  110 continue
c     # zero-order extrapolation:
      do 115 m=1,meqn
         do 115 ibc=1,mbc
            do 115 j = 1-mbc, my+mbc
               do 115 k = 1-mbc, mz+mbc
                  q(1-ibc,j,k,m) = q(1,j,k,m)
  115       continue
      go to 199

  120 continue
c    periodic:  
      do 125 m=1,meqn
         do 125 ibc=1,mbc
            do 125 j = 1-mbc, my+mbc
               do 125 k = 1-mbc, mz+mbc
                  q(1-ibc,j,k,m) = q(mx+1-ibc,j,k,m)
  125       continue
      go to 199

  130 continue
c    solid wall (assumes 2'nd component is velocity or momentum in x):
      do 135 m=1,meqn
         do 135 ibc=1,mbc
            do 135 j = 1-mbc, my+mbc
               do 135 k = 1-mbc, mz+mbc
                  q(1-ibc,j,k,m) = q(ibc,j,k,m)
  135       continue
c    negate the normal velocity:
      do 136 ibc=1,mbc
         do 136 j = 1-mbc, my+mbc
            do 136 k = 1-mbc, mz+mbc
               q(1-ibc,j,k,2) = -q(ibc,j,k,2)
  136    continue
      go to 199

  199 continue
c
c    right boundary (xupper):
c
      go to (200,210,220,230) mthbc(2)+1

  200 continue
c    user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(2)=0 and no BCs specified in bc3'
      stop
      go to 299

  210 continue
c    zero-order extrapolation:
      do 215 m=1,meqn
         do 215 ibc=1,mbc
            do 215 j = 1-mbc, my+mbc
               do 215 k = 1-mbc, mz+mbc
                  q(mx+ibc,j,k,m) = q(mx,j,k,m)
  215       continue
      go to 299

  220 continue
c    periodic:  
      do 225 m=1,meqn
         do 225 ibc=1,mbc
            do 225 j = 1-mbc, my+mbc
               do 225 k = 1-mbc, mz+mbc
                  q(mx+ibc,j,k,m) = q(ibc,j,k,m)
  225       continue
      go to 299

  230 continue
c    solid wall (assumes 2'nd component is velocity or momentum in x):
      do 235 m=1,meqn
         do 235 ibc=1,mbc
            do 235 j = 1-mbc, my+mbc
               do 235 k = 1-mbc, mz+mbc
                  q(mx+ibc,j,k,m) = q(mx+1-ibc,j,k,m)
  235       continue
c    negate the normal velocity:
      do 236 ibc=1,mbc
         do 236 j = 1-mbc, my+mbc
            do 236 k = 1-mbc, mz+mbc
               q(mx+ibc,j,k,2) = -q(mx+1-ibc,j,k,2)
  236    continue
      go to 299

  299 continue
c
c    bottom boundary (ylower):
c
      go to (300,310,320,330) mthbc(3)+1
c
  300 continue
c    user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(3)=0 and no BCs specified in bc3'
      stop
      go to 399
c
  310 continue
c    zero-order extrapolation:
      do 315 m=1,meqn
         do 315 jbc=1,mbc
            do 315 i = 1-mbc, mx+mbc
               do 315 k = 1-mbc, mz+mbc
                  q(i,1-jbc,k,m) = q(i,1,k,m)
  315       continue
      go to 399

  320 continue
c     # periodic:  
      do 325 m=1,meqn
         do 325 jbc=1,mbc
            do 325 i = 1-mbc, mx+mbc
               do 325 k = 1-mbc, mz+mbc
                  q(i,1-jbc,k,m) = q(i,my+1-jbc,k,m)
  325       continue
      go to 399

  330 continue
c    solid wall (assumes 3'rd component is velocity or momentum in y):
      do 335 m=1,meqn
         do 335 jbc=1,mbc
            do 335 i = 1-mbc, mx+mbc
               do 335 k = 1-mbc, mz+mbc
                  q(i,1-jbc,k,m) = q(i,jbc,k,m)
  335       continue
c    negate the normal velocity:
      do 336 jbc=1,mbc
         do 336 i = 1-mbc, mx+mbc
            do 336 k = 1-mbc, mz+mbc
               q(i,1-jbc,k,3) = -q(i,jbc,k,3)
  336    continue
      go to 399

  399 continue
c
c    top boundary (yupper):
c
      go to (400,410,420,430) mthbc(4)+1
c
  400 continue
c    user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(4)=0 and no BCs specified in bc3'
      stop
      go to 499

  410 continue
c    zero-order extrapolation:
      do 415 m=1,meqn
         do 415 jbc=1,mbc
            do 415 i = 1-mbc, mx+mbc
               do 415 k = 1-mbc, mz+mbc
                  q(i,my+jbc,k,m) = q(i,my,k,m)
  415       continue
      go to 499

  420 continue
c    periodic:  
      do 425 m=1,meqn
         do 425 jbc=1,mbc
            do 425 i = 1-mbc, mx+mbc
               do 425 k = 1-mbc, mz+mbc
                  q(i,my+jbc,k,m) = q(i,jbc,k,m)
  425       continue
      go to 499

  430 continue
c    solid wall (assumes 3'rd component is velocity or momentum in y):
      do 435 m=1,meqn
         do 435 jbc=1,mbc
            do 435 i = 1-mbc, mx+mbc
               do 435 k = 1-mbc, mz+mbc
                  q(i,my+jbc,k,m) = q(i,my+1-jbc,k,m)
  435       continue
c    negate the normal velocity:
      do 436 jbc=1,mbc
         do 436 i = 1-mbc, mx+mbc
            do 436 k = 1-mbc, mz+mbc
               q(i,my+jbc,k,3) = -q(i,my+1-jbc,k,3)
  436    continue
      go to 499

  499 continue
c
c    boundary (zlower):
c
      go to (500,510,520,530) mthbc(5)+1
c
  500 continue
c    user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(5)=0 and no BCs specified in bc3'
      stop
      go to 599
c
  510 continue
c    zero-order extrapolation:
      do 515 m=1,meqn
         do 515 kbc=1,mbc
            do 515 i = 1-mbc, mx+mbc
               do 515 j = 1-mbc, my+mbc
                  q(i,j,1-kbc,m) = q(i,j,1,m)
  515       continue
      go to 599

  520 continue
c    periodic:  
      do 525 m=1,meqn
         do 525 kbc=1,mbc
            do 525 i = 1-mbc, mx+mbc
               do 525 j = 1-mbc, my+mbc
                  q(i,j,1-kbc,m) = q(i,j,mz+1-kbc,m)
  525       continue
      go to 599

  530 continue
c    solid wall (assumes 4'rd component is velocity or momentum in y):
      do 535 m=1,meqn
         do 535 kbc=1,mbc
            do 535 i = 1-mbc, mx+mbc
               do 535 j = 1-mbc, my+mbc
                  q(i,j,1-kbc,m) = q(i,j,kbc,m)
  535       continue
c    negate the normal velocity:
      do 536 kbc=1,mbc
         do 536 i = 1-mbc, mx+mbc
            do 536 j = 1-mbc, my+mbc
               q(i,j,1-kbc,4) = -q(i,j,kbc,4)
  536    continue
      go to 599

  599 continue
c
c    boundary (zupper):
c
      go to (600,610,620,630) mthbc(6)+1
c
  600 continue
c    user-specified boundary conditions go here in place of error output
      write(6,*) '*** ERROR *** mthbc(6)=0 and no BCs specified in bc3'
      stop
      go to 699

  610 continue
c    zero-order extrapolation:
      do 615 m=1,meqn
         do 615 kbc=1,mbc
            do 615 i = 1-mbc, mx+mbc
               do 615 j = 1-mbc, my+mbc
                  q(i,j,mz+kbc,m) = q(i,j,mz,m)
  615       continue
      go to 699

  620 continue
c    periodic:  
      do 625 m=1,meqn
         do 625 kbc=1,mbc
            do 625 i = 1-mbc, mx+mbc
               do 625 j = 1-mbc, my+mbc
                  q(i,j,mz+kbc,m) = q(i,j,kbc,m)
  625       continue
      go to 699

  630 continue
c    solid wall (assumes 3'rd component is velocity or momentum in y):
      do 635 m=1,meqn
         do 635 kbc=1,mbc
            do 635 i = 1-mbc, mx+mbc
               do 635 j = 1-mbc, my+mbc
                  q(i,j,mz+kbc,m) = q(i,j,mz+1-kbc,m)
  635       continue
c    negate the normal velocity:
      do 636 kbc=1,mbc
         do 636 i = 1-mbc, mx+mbc
            do 636 j = 1-mbc, my+mbc
               q(i,j,mz+kbc,4) = -q(i,j,mz+1-kbc,4)
  636    continue
      go to 699

  699 continue

      return
      end
      subroutine cellave(xlow,ylow,dx,dy,wl)

c*********************************************************************72
c
cc CELLAVE computes the cell fraction lying in the left state.
c
c  Discussion:
c
c    compute wl, fraction of cell that lies in left state.
c    For initial data with two states ql and qr separated by a 
c    discontinuity. The curve along which the discontinuity lies is
c    specified by the function fdisc, which should return a value that
c    is negative on the side where ql lies and positive on the qr side.
c
c    xlow,ylow is the coordinate of the lower left corner of the cell.
c    dx, dy are grid spacing in x and y.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external fss
      logical fl(5),alll,allr
      dimension x(10),y(10),xx(5),yy(5)
      common/fsscorn/ xc0,yc0,xc1,yc1

      xx(1) = xlow
      xx(2) = xlow
      xx(3) = xlow+dx
      xx(4) = xlow+dx
      xx(5) = xx(1)
      yy(1) = ylow
      yy(2) = ylow+dy
      yy(3) = ylow+dy
      yy(4) = ylow
      yy(5) = yy(1)
      alll = .true.
      allr = .true.

      do 20 i=1,4
         fl(i) = fdisc(xx(i),yy(i)) .lt. 0.d0
         alll = alll .and. fl(i)
         allr = allr .and. (.not. fl(i))
   20    continue
      fl(5) = fl(1)

      if (alll) then
         wl = 1.d0
         return
         end if
      if (allr) then
         wl = 0.d0
         return
         end if

      iv = 0
      do 40 i=1,4
          if (fl(i)) then
               iv = iv+1
               x(iv) = xx(i)
               y(iv) = yy(i)
               end if
          if (fl(i).neqv.fl(i+1)) then
               iv = iv+1
               xc0 = xx(i)
               yc0 = yy(i)
               xc1 = xx(i+1)
               yc1 = yy(i+1)
               ss = zeroin(0.d0, 1.d0, fss, 1d-8)
c              write(27,*) 'xc,yc,ss:',xc0,yc0,xc1,yc1,ss
               x(iv) = xx(i) + ss*(xx(i+1)-xx(i))
               y(iv) = yy(i) + ss*(yy(i+1)-yy(i))
               end if
   40     continue
c
c    compute area:
c
      if (iv.eq.0) then
         wl = 0.d0
         return
         end if

      x(iv+1) = x(1)
      y(iv+1) = y(1)
      area = 0.d0
      do 50 i=1,iv
         area = area + .5d0*(y(i)+y(i+1))*(x(i+1)-x(i))
c        write(27,*) '  x,y:',x(i),y(i)
   50    continue

      wl = area / (dx*dy)
c     write(27,*) 'area,wl:',area,wl

      return
      end
      subroutine chkmth(method,info)

c*********************************************************************72
c
cc CHKMTH checks whether the method parameters are correct.
c
c  Discussion:
c
c     # Note that method(3) < 0 yields dimensional splitting.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension method(*)

      info = 0

      if(method(1) .ne. 0 .and. method(1) .ne. 1)then
         write(6,*) ' '
         write(6,*) 'CLAW3 error .... method(1) should be:'
         write(6,*) '   0 - fixed time steps'
         write(6,*) '   1 - variable time steps'
         write(6,*) ' '
         info = 6 
      end if

      if(method(2) .ne. 1 .and. method(2) .ne. 2)then
         write(6,*) ' '
         write(6,*) 'CLAW3 error .... method(2) should be:'
         write(6,*) '   1 - first order method'
         write(6,*) '   2 - second order method'
         write(6,*) ' '
         info = 6 
      end if

      if(method(2) .eq. 1 .and. method(3) .ge. 0)then
         if(method(3) .ne. 0 .and. method(3) .ne. 10 .and.
     &      method(3) .ne. 11)then
            write(6,*) ' '
            write(6,*) 'CLAW3 error .... when method(2) = 1,'
            write(6,*) 'method(3) should be:'
            write(6,*) '   0  - donor cell'
            write(6,*) '   10 - 2D wave propagation of increment waves'
            write(6,*) '   11 - corner transport upwind'
            write(6,*) ' '
            info = 6
         end if
      end if       

      if(method(2) .eq. 2 .and. method(3) .ge. 0)then
         if(method(3) .ne. 0  .and. method(3) .ne. 10 .and.
     &      method(3) .ne. 11 .and. method(3) .ne. 20 .and.
     &      method(3) .ne. 21 .and. method(3) .ne. 22)then
            write(6,*) ' '
            write(6,*) 'CLAW3 error .... when method(2) = 2,'
            write(6,*) 'method(3) should be:'
            write(6,*) '   0  - donor cell'
            write(6,*) '   10 - 2D wave propagation of increment waves'
            write(6,*) '        and 1D propagation of correction waves'
            write(6,*) '        UNCONDITIONALLY UNSTABLE'
            write(6,*) '   11 - corner transport upwind with 2D'
            write(6,*) '        propagation of the correction waves'
            write(6,*) '        UNCONDITIONALLY UNSTABLE'
            write(6,*) '   20 - 2D propagation of both increment and'
            write(6,*) '        correction waves.'
            write(6,*) '   21 - corner transport upwind with 2D '
            write(6,*) '        propagation of the correction waves.'
            write(6,*) '   22 - Full 3D wave propagation method.'
            write(6,*) ' '
            info = 6
         end if
      end if       

      return
      end
      subroutine claw1ez ( maxmx, meqn, mwaves, mbc, maux, mwork, 
     &  mthlim, q, work, aux )

c*********************************************************************72
c
cc CLAW1EZ is an easy-to-use clawpack driver routine for simple applications.
c
c  Discussion:
c
c    This routine makes it easier to use the CLAW1 routine, by making many
c    choices for the user, using default values and supplying simple versions
c    of certain routines that would otherwise be user-supplied.
c
c    In the argument list for CLAW1EZ, the user supplies 6 numbers that
c    specify the sizes of certain arrays, and 4 arrays, which do not need to
c    be initialized.
c
c    The user also supplies a data file, named "claw1ez.data",
c    containing the value of about 16 variables (some of which are short arrays).
c
c    The user also supplies the routines QINIT, RP1 and SETPROB.
c
c    The entire computation is controlled by the CLAW1 routine.
c
c    Documentation is available at
c      http://www.amath.washington.edu/~claw/doc.html
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
c  Parameters:
c
c    Input, integer MAXMX, the maximum number of interior grid points in X. 
c
c    Input, integer MEQN, the number of equations in the system of conservation laws.
c
c    Input, integer MWAVES, the number of waves that result from the solution 
c    of each Riemann problem.  Often, MWAVES = MEQN but for some problems 
c    these may be different.
c
c    Input, integer MBC, the number of "ghost cells" that must be added on to each
c    side of the domain to handle boundary conditions.  The cells actually in the 
c    physical domain are labelled from 1 to MX in X.  The arrays are dimensioned 
c    actually indexed from 1-MBC to MX+MBC.  For the methods currently implemented, 
c    MBC = 2 should be used.  If the user implements another method that has a 
c    larger stencil and hence requires more ghost cells, a larger value of MBC 
c    could be used.  Q is extended from the physical domain to the ghost cells by the
c    library-supplied routine BC1.
c
c    Input, integer MAUX, the number of auxilliary variables.
c
c    Input, integer MWORK, the size of the work array WORK.
c    MWORK must be at least
c    ( MAXMX + 2 * MBC ) * (2 + 4 * MEQN + MWAVES + MEQN * MWAVES )
c
c    Work array, integer MTHLIM(MWAVES), space in which a limiter for each wave.
c    can be stored.  The actual values are read from the user's input data file.
c
c    Work array, double precision Q(1-MBC:MAXMX+MBC,MEQN), space in which
c    the value of Q may be stored.  The actual values are determined by calling
c    QINIT, and then by computation.
c
c    Work array, double precision WORK(MWORK).
c
c    Work array, double precision AUX(1-MBC:MAXMX+MBC,MAUX), space in which
c    the value of the auxilliary variables can be store.
c
      implicit double precision (a-h,o-z)

      integer maux
      integer maxmx
      integer mbc
      integer meqn
      integer mwaves
      integer mwork

      double precision aux(1-mbc:maxmx+mbc, maux)
      external b4step1
      external bc1
      double precision cflv(4)
      double precision dtv(5)
      integer method(7)
      integer mthbc(2)
      integer mthlim(mwaves)
      integer nv(2)
      logical outt0
      double precision q(1-mbc:maxmx+mbc, meqn)
      external rp1
      external src1
      double precision tout(100)
      double precision work(mwork)

      open(55,file='claw1ez.data',status='old',form='formatted')
      open(10,file='fort.info',status='unknown',form='formatted')
      open(11,file='fort.nplot',status='unknown',form='formatted')
c
c  Read the input in standard form from claw1ez.data:
c  For a description of input parameters see the documentation at
c  http://www.amath.washington.edu/~claw
c
c  MX = number of grid cells:
c
      read(55,*) mx
c
c  I/O variables.
c
      read(55,*) nout
      read(55,*) outstyle

      if (outstyle.eq.1) then
        read(55,*) tfinal
        nstepout = 1
      elseif (outstyle.eq.2) then
        read(55,*) (tout(i), i=1,nout)
        nstepout = 1
      elseif (outstyle.eq.3) then
        read(55,*) nstepout, nstop
	nout = nstop
      end if
c
c     timestepping variables.
c
      read(55,*) dtv(1)
      read(55,*) dtv(2)
      read(55,*) cflv(1)
      read(55,*) cflv(2)
      read(55,*) nv(1)
c
c  Input parameters for clawpack routines.
c
      read(55,*) method(1)
      read(55,*) method(2)
      read(55,*) method(3)
      read(55,*) method(4)
      read(55,*) method(5)
      read(55,*) method(6)  
      read(55,*) method(7) 

      read(55,*) meqn1
      read(55,*) mwaves1
      read(55,*) (mthlim(mw), mw=1,mwaves)
c
c  Physical domain:
c
      read(55,*) t0
      read(55,*) xlower
      read(55,*) xupper
c
c  Boundary conditions.
c
      read(55,*) mbc1
      read(55,*) mthbc(1)
      read(55,*) mthbc(2)

      close ( unit = 55 )
c
c  Now check the input.
c
      if (method(7) .ne. maux) then
         write(6,*) '*** ERROR ***  maux set wrong in input or driver'
         stop
         end if

      if (meqn1 .ne. meqn) then
         write(6,*) '*** ERROR ***  meqn set wrong in input or driver'
         stop
         end if
      if (mwaves1 .ne. mwaves) then
         write(6,*) '*** ERROR ***  mwaves set wrong in input or driver'
         stop
         end if
      if (mbc1 .ne. mbc) then
         write(6,*) '*** ERROR ***  mbc set wrong in input or driver'
         stop
         end if

      if ((mthbc(1).eq.2 .and. mthbc(2).ne.2) .or.
     &    (mthbc(2).eq.2 .and. mthbc(1).ne.2)) then
         write(6,*) '*** ERROR ***  periodic boundary conditions'
         write(6,*) ' require mthbc(1) and mthbc(2) BOTH be set to 2'
         stop 
         end if
c
c  Check that enough storage has been allocated:
c
      mwork1 = (maxmx + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)

      if (mx.gt.maxmx .or. mwork.lt.mwork1) then
         maxmx1 = max0(mx,maxmx)

         mwork1 = (maxmx1 + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)

         write(6,*) ' '
         write(6,*) '*** ERROR *** Insufficient storage allocated'
         write(6,*) 'Recompile after increasing values in driver.f:'
         write(6,611) maxmx1
         write(6,613) mwork1
 611     format(/,'parameter (maxmx = ',i5,')')
 613     format('parameter (mwork = ',i7,')',/)
         stop
         end if

      write(6,*) 'running...'
      write(6,*) ' '
c
c  Grid spacing.
c
      dx = (xupper - xlower) / float(mx)
c
c  Time increments between outputing solution.
c
      if (outstyle .eq. 1) then
         dtout = (tfinal - t0)/float(nout)
         end if

      write(11,1101) nout
 1101 format(i5)
c        
c  Call user's routine setprob to set any specific parameters
c  or other initialization required.
c
      call setprob
c        
c  Set auxilliary arrays.
c
      if ( maux .gt. 0 )  then
         call setaux1 ( maxmx, mbc, mx, xlower, dx, maux, aux )
      end if
c
c  Set initial conditions:
c
      call qinit ( maxmx, meqn, mbc, mx, xlower, dx, q, maux, aux )

      outt0 = .true.
c
c  Output the initial data.
c
      if (outt0) then
         call out1 ( maxmx, meqn, mbc, mx, xlower, dx, q, t0, 0 )
         write(6,601) 0, t0
      end if
c
c     Main loop:
c
      tend = t0

      do 100 n=1,nout
         tstart = tend
         if (outstyle .eq. 1)  tend = tstart + dtout
         if (outstyle .eq. 2)  tend = tout(n)
         if (outstyle .eq. 3)  tend = tstart - 1.d0  !# single-step mode

         call claw1(maxmx,meqn,mwaves,mbc,mx,
     &           q,aux,xlower,dx,tstart,tend,dtv,cflv,nv,method,mthlim,
     &           mthbc,work,mwork,info,bc1,rp1,src1,b4step1)
c
c  check to see if an error occured:
c
         if (info .ne. 0) then
            write(6,*) '*** ERROR in claw1 ***  info =',info
            if (info.eq.1) then
               write(6,*) '***   either mx > maxmx or mbc < 2'
               end if
            if (info.eq.2) then
               write(6,*) '***   dt does not divide (tend - tstart)'
               write(6,*) '***   and dt is fixed since method(1)=0'
               end if
            if (info.eq.3) then
               write(6,*) '***   method(1)=1 and cflv(2) > cflv(1)'
               end if
            if (info.eq.4) then
               write(6,*) '***   mwork is too small'
               end if
            if (info.eq.11) then
               write(6,*) '***   Too many times steps, n > nv(1)'
               end if
            if (info.eq.12) then
               write(6,*) 
     &          '***   The Courant number is greater than cflv(1)'
               write(6,*) '***   and dt is fixed since method(1)=0'
               end if

            go to 999
            end if

         dtv(1) = dtv(5)  !# use final dt as starting value on next call
c
c  output solution at this time
c
c  if outstyle=1 or 2, then nstepout=1 and we output every time
c  we reach this point, since claw1 was called for the entire time
c  increment between outputs.
c
c  if outstyle=3 then we only output if we have taken nstepout
c  time steps since the last output.
c
c  iframe is the frame number used to form file names in out1
c
         iframe = n/nstepout
	 if (iframe*nstepout .eq. n) then
            call out1(maxmx,meqn,mbc,mx,xlower,dx,q,tend,iframe)
            write(6,601) iframe,tend
            write(10,1010) tend,info,dtv(3),dtv(4),dtv(5),
     &           cflv(3),cflv(4),nv(2)
	    end if
c
c  formats for writing out information about this call to claw:
c
  601    format('CLAW1EZ: Frame ',i4,
     &           ' matlab plot files done at time t =',
     &           d12.4,/)

 1010    format('tend =',d15.4,/,
     &       'info =',i5,/,'smallest dt =',d15.4,/,'largest dt =',
     &       d15.4,/,'last dt =',d15.4,/,'largest cfl =',
     &         d15.4,/,'last cfl =',d15.4,/,'steps taken =',i4,/)

  100    continue

  999 continue

      return 
      end
      subroutine claw1 ( maxmx, meqn, mwaves, mbc, mx, q, aux, xlower,
     &  dx, tstart, tend, dtv, cflv, nv, method, mthlim, mthbc, work,
     &  mwork, info, bc1, rp1, src1, b4step1 )

c*********************************************************************72
c
cc CLAW1 solves a hyperbolic system of conservation laws in one space dimension.
c
c  Discussion:
c
c    The equations have the general form 
c
c      capa * q_t + A q_x = psi
c
c    The "capacity function" capa(x) and source term psi are optional
c
c    For a more complete description see the documentation at
c      http://www.amath.washington.edu/~claw
c
c    Sample driver programs and user-supplied subroutines are available.
c    See the the directories claw/clawpack/1d/example* for some examples, and
c    codes in claw/applications for more extensive examples.
c
c    The user must supply the following subroutines:
c
c      bc1       subroutine specifying the boundary conditions.
c
c      rp1       subroutine specifying the Riemann solver.
c
c      b4step1   The routine b4step1 is called each time step and
c                can be supplied by the user in order to perform
c                other operations that are necessary every time
c                step.  For example, if the variables stored in
c                the aux arrays are time-dependent then these
c                values can be set.
c
c    In addition, if the equation contains source terms psi, then the user
c    must provide:
c
c      src1      subroutine that solves capa * q_t = psi over a single time step.
c
c    These routines must be declared EXTERNAL in the main program.
c    For description of the calling sequences, see below.
c
c    Dummy routines b4step1 and src1 are provided in this library.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
c  Parameters:
c
c    Input, integer MAXMX, the maximum number of interior grid points in X. 
c
c    Input, integer MEQN, the number of equations in the system of conservation laws.
c
c    Input, integer MWAVES, the number of waves that result from the solution 
c    of each Riemann problem.  Often, MWAVES = MEQN but for some problems 
c    these may be different.
c
c    Input, integer MBC, the number of "ghost cells" that must be added on to each
c    side of the domain to handle boundary conditions.  The cells actually in the 
c    physical domain are labelled from 1 to MX in X.  The arrays are dimensioned 
c    actually indexed from 1-MBC to MX+MBC.  For the methods currently implemented, 
c    MBC = 2 should be used.  If the user implements another method that has a 
c    larger stencil and hence requires more ghost cells, a larger value of MBC 
c    could be used.  Q is extended from the physical domain to the ghost cells by the
c    user-supplied routine BC1.
c
c    Input, integer MX, the number of grid cells in the X-direction, in the
c    physical domain.  In addition there are MBC grid cells along each edge of the 
c    grid that are used for boundary conditions.  MX <= MAXMX is required.
c 
c    q(1-mbc:maxmx+mbc, meqn) 
c        On input:  initial data at time tstart.
c        On output: final solution at time tend.
c        q(i,m) = value of mth component in the i'th cell.
c        Values within the physical domain are in q(i,m) 
c                for i = 1,2,...,mx
c        mbc extra cells on each end are needed for boundary conditions
c        as specified in the routine bc1.
c
c    aux(1-mbc:maxmx+mbc, maux)
c        Array of auxiliary variables that are used in specifying the problem.
c        If method(7) = 0 then there are no auxiliary variables and aux
c                         can be a dummy variable.
c        If method(7) = maux > 0 then there are maux auxiliary variables
c                         and aux must be dimensioned as above.
c
c        Capacity functions are one particular form of auxiliary variable.
c        These arise in some applications, e.g. variable coefficients in
c        advection or acoustics problems.
c        See Clawpack Note # 5 for examples.
c
c        If method(6) = 0 then there is no capacity function.
c        If method(6) = mcapa > 0  then there is a capacity function and
c            capa(i), the "capacity" of the i'th cell, is assumed to be
c            stored in aux(i,mcapa).
c            In this case we require method(7).ge.mcapa.
c
c    dx = grid spacing in x.  
c         (for a computation in ax <= x <= bx,  set dx = (bx-ax)/mx.)
c
c    tstart = initial time.
c
c    tend = Desired final time (on input).
c              If tend<tstart, then claw1 returns after a single successful
c                 time step has been taken (single-step mode).
c              Otherwise, as many steps are taken as needed to reach tend, 
c                 up to a maximum of nv(1).
c         = Actual time reached (on output).
c
c    dtv(1:5) = array of values related to the time step:
c               (Note: method(1)=1 indicates variable size time steps)
c         dtv(1) = value of dt to be used in all steps if method(1) = 0
c                = value of dt to use in first step if method(1) = 1
c         dtv(2) = unused if method(1) = 0.
c                = maximum dt allowed if method(1) = 1.
c         dtv(3) = smallest dt used (on output)
c         dtv(4) = largest dt used (on output)
c         dtv(5) = dt used in last step (on output)
c
c    cflv(1:4) = array of values related to Courant number:
c         cflv(1) = maximum Courant number to be allowed.  With variable
c                   time steps the step is repeated if the Courant
c                   number is larger than this value.  With fixed time
c                   steps the routine aborts.  Usually cflv(1)=1.0
c                   should work.
c         cflv(2) = unused if method(1) = 0.
c                 = desired Courant number if method(1) = 1.
c                   Should be somewhat less than cflv(1), e.g. 0.9
c         cflv(3) = largest Courant number observed (on output).
c         cflv(4) = Courant number in last step (on output).
c
c    nv(1:2) = array of values related to the number of time steps:
c         nv(1) = unused if method(1) = 0
c               = maximum number of time steps allowed if method(1) = 1
c         nv(2) = number of time steps taken (on output).
c
c    Input, integer METHOD(7), specifies the numerical method to use.
c    The recommended choice of methods for most problems is
c    METHOD(1) = 1,  METHOD(2) = 2.
c    METHOD(1)
c    = 0, if fixed size time steps are to be taken.  In this case, DT = dTV(1) 
c      in all steps.
c    = 1, if variable time steps are to be used.  In this case, DT = DTV(1) 
c      in the first step and thereafter the value cflv(2) is used to choose the
c      next time step based on the maximum wave speed seen in the previous step.  
c      Note that since this value comes from the previous step, the Courant 
c      number will not in general be exactly equal to the desired value
c      If the actual Courant number in the next step is greater than 1, then 
c      this step is redone with a smaller DT.
c    METHOD(2)
c    = 1 if Godunov's method is to be used, with no 2nd order corrections.
c    = 2 if second order correction terms are to be added, with
c      a flux limiter as specified by mthlim.  
c    METHOD(3)  is not used in one-dimension.
c    METHOD(4) 
c    = 0 to suppress printing
c    = 1 to print DT and Courant number every time step
c    METHOD(5) 
c    = 0 if there is no source term PSI.  In this case the subroutine SRC1 
c      is never called so a dummy parameter can be given.
c    = 1 if there is a source term.  In this case the subroutine SRC1 must 
c      be provided and a fractional step method is used.
c      In each time step the following sequence is followed:
c        call BC to extend data to ghost cells
c        call STEP1 to advance hyperbolic eqn by DT
c        call SRC1 to advance source terms by DT
c    = 2 if there is a source term and Strang splitting is to
c      be used instead of the Godunov splitting above.
c      In each time step the following sequence is followed:
c        call BC to extend data to ghost cells
c        call SRC1 to advance source terms by DT/2
c        call STEP1 to advance hyperbolic equation by DT
c        call SRC1 to advance source terms by DT/2
c      For most problems 1 is recommended rather than 2
c      since it is less expensive and works essentially as
c      well on most problems.
c    METHOD(6)
c    = 0 if there is no capacity function CAPA.
c    = MCAPA > 0 if there is a capacity function.  In this case
c      AUX(I,MCAPA) is the capacity of the I'th cell and you
c      must also specify METHOD(7) .ge. MCAPA and set AUX.
c    METHOD(7)
c      = 0 if there is no aux array used.
c      = MAUX > 0 if there are MAUX auxiliary variables.
c
c    mthlim(1:mwaves) = array of values specifying the flux limiter to be used
c                     in each wave family mw.  Often the same value will be used
c                     for each value of mw, but in some cases it may be
c                     desirable to use different limiters.  For example,
c                     for the Euler equations the superbee limiter might be
c                     used for the contact discontinuity (mw=2) while another
c                     limiter is used for the nonlinear waves.  Several limiters
c                     are built in and others can be added by modifying the
c                     subroutine philim.
c
c        mthlim(mw) = 0 for no limiter
c                   = 1 for minmod
c                   = 2 for superbee
c                   = 3 for van Leer
c                   = 4 for monotonized centered
c
c    work(mwork) = double precision work array of length at least mwork
c
c    mwork = length of work array.  Must be at least
c               (maxmx + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)
c            If mwork is too small then the program returns with info = 4
c            and prints the necessary value of mwork to unit 6.
c
c            
c    info = output value yielding error information:
c         = 0 if normal return.
c         = 1 if mx.gt.maxmx   or  mbc.lt.2
c         = 2 if method(1)=0 and dt doesn't divide (tend - tstart).
c         = 3 if method(1)=1 and cflv(2) > cflv(1).
c         = 4 if mwork is too small.
c         = 11 if the code attempted to take too many time steps, n > nv(1).
c              This could only happen if method(1) = 1 (variable time steps).
c         = 12 if the method(1)=0 and the Courant number is greater than 1
c              in some time step.
c
c           Note: if info.ne.0, then tend is reset to the value of t actually
c           reached and q contains the value of the solution at this time.
c
c    User-supplied subroutines
c
c
c    bc1 = subroutine that specifies the boundary conditions.  
c         This subroutine should extend the values of q from cells
c         1:mx to the mbc ghost cells along each edge of the domain.
c
c          The form of this subroutine is
c
c     subroutine bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,mthbc)
c     implicit double precision (a-h,o-z)
c     dimension   q(1-mbc:maxmx+mbc, meqn)
c     dimension aux(1-mbc:maxmx+mbc, *)
c     dimension mthbc(2)
c
c
c    The routine claw/clawpack/1d/lib/bc1.f can be used to specify
c    various standard boundary conditions.
c
c
c    rp1 = user-supplied subroutine that implements the Riemann solver
c
c          The form of this subroutine is
c
c     subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,wave,s,amdq,apdq)
c     implicit double precision (a-h,o-z)
c     dimension   ql(1-mbc:maxmx+mbc, meqn)
c     dimension   qr(1-mbc:maxmx+mbc, meqn)
c     dimension auxl(1-mbc:maxmx+mbc, *)
c     dimension auxr(1-mbc:maxmx+mbc, *)
c     dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
c     dimension    s(1-mbc:maxmx+mbc, mwaves)
c     dimension amdq(1-mbc:maxmx+mbc, meqn)
c     dimension apdq(1-mbc:maxmx+mbc, meqn)
c
c
c         On input, ql contains the state vector at the left edge of each cell
c                   qr contains the state vector at the right edge of each cell
c                 auxl contains auxiliary values at the left edge of each cell
c                 auxr contains auxiliary values at the right edge of each cell
c
c         Note that the i'th Riemann problem has left state qr(i-1,:)
c                                            and right state ql(i,:)
c         In the standard clawpack routines, this Riemann solver is
c         called with ql=qr=q along this slice.  More flexibility is allowed
c         in case the user wishes to implement another solution method
c         that requires left and rate states at each interface.

c         If method(7)=maux > 0 then the auxiliary variables along this slice
c         are passed in using auxl and auxr.  Again, in the standard routines
c         auxl=auxr=aux in the call to rp1.
c
c          On output, 
c              wave(i,m,mw) is the m'th component of the jump across
c                              wave number mw in the ith Riemann problem.
c              s(i,mw) is the wave speed of wave number mw in the
c                              ith Riemann problem.
c              amdq(i,m) = m'th component of A^- Delta q,
c              apdq(i,m) = m'th component of A^+ Delta q,
c                     the decomposition of the flux difference
c                         f(qr(i-1)) - f(ql(i))
c                     into leftgoing and rightgoing parts respectively.
c
c           It is assumed that each wave consists of a jump discontinuity
c           propagating at a single speed, as results, for example, from a
c           Roe approximate Riemann solver.  An entropy fix can be included
c           into the specification of amdq and apdq.
c
c    src1 = subroutine for the source terms that solves the equation
c               capa * q_t = psi 
c           over time dt.
c
c           If method(5)=0 then the equation does not contain a source
c           term and this routine is never called.  A dummy argument can
c           be used with many compilers, or provide a dummy subroutine that
c           does nothing (such a subroutine can be found in
c           claw/clawpack/1d/lib/src1.f)
c
c          The form of this subroutine is
c
c     subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
c     implicit double precision (a-h,o-z)
c     dimension   q(1-mbc:maxmx+mbc, meqn)
c     dimension aux(1-mbc:maxmx+mbc, *)
c
c      If method(7)=0  or the auxiliary variables are not needed in this solver,
c      then the latter dimension statement can be omitted, but aux should
c      still appear in the argument list.
c
c      On input, q(i,m) contains the data for solving the
c                source term equation.
c      On output, q(i,m) should have been replaced by the solution to
c                 the source term equation after a step of length dt.
c
c
c      b4step1 = subroutine that is called from claw1 before each call to
c                step1.  Use to set time-dependent aux arrays or perform
c                other tasks which must be done every time step.
c
c          The form of this subroutine is
c      
c
c      subroutine b4step1(maxmx,mbc,mx,meqn,q,xlower,dx,time,dt,maux,aux)
c      implicit double precision (a-h,o-z)
c      dimension   q(1-mbc:maxmx+mbc, meqn)
c      dimension aux(1-mbc:maxmx+mbc, *)
c
c
c  Copyright 1994 -- 2002 R. J. LeVeque
c
c  This software is made available for research and instructional use only. 
c  You may copy and use this software without charge for these non-commercial
c  purposes, provided that the copyright notice and associated text is
c  reproduced on all copies.  For all other uses (including distribution of
c  modified versions), please contact the author at the address given below. 
c  
c  This software is made available "as is" without any assurance that it
c  will work for your purposes.  The software may in fact have defects, so
c  use the software at your own risk.
c
c
c    CLAWPACK Version 4.1,  August, 2002
c    Webpage: http://www.amath.washington.edu/~claw
c
c    Author:  Randall J. LeVeque
c             Applied Mathematics
c             Box 352420
c             University of Washington, 
c             Seattle, WA 98195-2420
c             rjl@amath.washington.edu
c
c    Beginning of claw1 code
c
c 
      implicit double precision (a-h,o-z)

      external bc1,rp1,src1,b4step1
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
      dimension work(mwork)
      dimension mthlim(mwaves),method(7),dtv(5),cflv(4),nv(2)
      dimension mthbc(2)
      common /comxt/ dtcom,dxcom,tcom

      info = 0
      t = tstart
      maxn = nv(1)
      dt = dtv(1)   !# initial dt
      cflmax = 0.d0
      dtmin = dt
      dtmax = dt
      nv(2) = 0
      maux = method(7)
c
c  check for errors in data:
c
      if (mx .gt. maxmx) then
         info = 1
         go to 900
         end if

      if (method(1) .eq. 0) then
c
c        # fixed size time steps.  Compute the number of steps:
c
         if (tend .lt. tstart) then
c
c             # single step mode
c
	      maxn = 1
           else
              maxn = (tend - tstart + 1d-10) / dt
              if (dabs(maxn*dt - (tend-tstart)) .gt.
     &                          1d-5*(tend-tstart)) then
c
c                # dt doesn't divide time interval integer number of times
c
                 info = 2
                 go to 900
                 end if
	   end if
         end if

      if (method(1).eq.1 .and. cflv(2).gt.cflv(1)) then
         info = 3
         go to 900
         end if
c
c  partition work array into pieces for passing into step1:
c
      i0f = 1
      i0wave = i0f + (maxmx + 2*mbc) * meqn
      i0s = i0wave + (maxmx + 2*mbc) * meqn * mwaves
      i0dtdx = i0s + (maxmx + 2*mbc) * mwaves
      i0qwork = i0dtdx + (maxmx + 2*mbc) 
      i0amdq = i0qwork + (maxmx + 2*mbc) * meqn
      i0apdq = i0amdq + (maxmx + 2*mbc) * meqn
      i0dtdx = i0apdq + (maxmx + 2*mbc) * meqn
      i0end = i0dtdx + (maxmx + 2*mbc) - 1

      if (mwork .lt. i0end) then
         write(6,*) 'mwork must be increased to ',i0end
         info = 4
         go to 900
         end if
c
c  main loop
c
      if (maxn.eq.0) go to 900
      do 100 n=1,maxn
         told = t   !# time at beginning of time step.

c        # adjust dt to hit tend exactly if we're near end of computation
c        #  (unless tend < tstart, which is a flag to take only a single step)
         if (told+dt.gt.tend .and. tstart.lt.tend) dt = tend - told

         if (method(1).eq.1) then
c
c           # save old q in case we need to retake step with smaller dt:
c
            call copyq1(maxmx,meqn,mbc,mx,q,work(i0qwork))
            end if
         
   40    continue
         dt2 = dt / 2.d0
         thalf = t + dt2  !# midpoint in time for Strang splitting
         t = told + dt    !# time at end of step
c
c        # store dt and t in the common block comxt in case they are needed
c        # in the Riemann solvers (for variable coefficients)
c
         tcom = told
         dtcom = dt
         dxcom = dx
c
c  main steps in algorithm:
c
c  extend data from grid to bordering boundary cells:
c
         call bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,told,dt,mthbc)
c
c  call user-supplied routine which might set aux arrays
c  for this time step, for example.
c

         call b4step1(maxmx,mbc,mx,meqn,q,
     &                xlower,dx,told,dt,maux,aux)

         if (method(5).eq.2) then
c            # with Strang splitting for source term:
             call src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,told,dt2)
             end if
c
c        # take a step on the homogeneous conservation law:
         call step1(maxmx,meqn,mwaves,mbc,mx,q,aux,dx,dt,
     &             method,mthlim,cfl,work(i0f),work(i0wave),
     &             work(i0s),work(i0amdq),work(i0apdq),work(i0dtdx),
     &             rp1)

         if (method(5).eq.2) then
c
c            # source terms over a second half time step for Strang splitting:
c            # Note it is not so clear what time t should be used here if
c            # the source terms are time-dependent!
c
             call src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,thalf,dt2)
             end if

         if (method(5).eq.1) then
c
c            # source terms over a full time step:
c
             call src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
             end if

         if (method(4) .eq. 1) write(6,601) n,cfl,dt,t
  601    format('CLAW1... Step',i4,
     &                   '   Courant number =',f6.3,'  dt =',d12.4,
     &                   '  t =',d12.4)

         if (method(1) .eq. 1) then
c
c           # choose new time step if variable time step
c
            if (cfl .gt. 0.d0) then
                dt = dmin1(dtv(2), dt * cflv(2)/cfl)
                dtmin = dmin1(dt,dtmin)
                dtmax = dmax1(dt,dtmax)
              else
                dt = dtv(2)
              end if
            end if
c
c        # check to see if the Courant number was too large:
c
         if (cfl .le. cflv(1)) then
c
c               # accept this step
c
                cflmax = dmax1(cfl,cflmax)
              else
c
c               # reject this step
c
                t = told
                call copyq1(maxmx,meqn,mbc,mx,work(i0qwork),q)

                if (method(4) .eq. 1) then
                   write(6,602) 
  602              format('CLAW1 rejecting step... ',
     &                         'Courant number too large')
                   end if
                if (method(1).eq.1) then
c
c                   # if variable dt, go back and take a smaller step
c
                    go to 40
                  else
c
c                   # if fixed dt, give up and return
c
                    cflmax = dmax1(cfl,cflmax)
                    go to 900
                  end if
               end if
c
c        # see if we are done:
         nv(2) = nv(2) + 1
         if (t .ge. tend) go to 900

  100    continue

  900  continue
c 
c  return information
c
c  too many timesteps
c
       if (method(1).eq.1 .and. t.lt.tend .and. nv(2) .eq. maxn) then
          info = 11
       end if
c
c  Courant number too large with fixed dt
c
       if (method(1).eq.0 .and. cflmax .gt. cflv(1)) then
          info = 12
       end if

       tend = t
       cflv(3) = cflmax
       cflv(4) = cfl
       dtv(3) = dtmin
       dtv(4) = dtmax
       dtv(5) = dt
       return 
       end
      subroutine claw2ez(maxmx,maxmy,meqn,mwaves,mbc,maux,mwork,mthlim,
     &                   q,work,aux)

c*********************************************************************72
c
cc CLAW2EZ is an easy-to-use clawpack driver routine for simple applications.
c
c  Discussion:
c
c     Documentation is available at
c                 http://www.amath.washington.edu/~claw/doc.html
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external bc2,rpn2,rpt2,src2,b4step2

      dimension  aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, maux)
      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension work(mwork)
      dimension mthlim(mwaves)
c
      dimension method(7),dtv(5),cflv(4),nv(2),mthbc(4)
      dimension tout(100)
      logical outt0

      common /restrt_block/ tinitial, iframe, outt0

      open(55,file='claw2ez.data',status='old',form='formatted')
      open(10,file='fort.info',status='unknown',form='formatted')
      open(11,file='fort.nplot',status='unknown',form='formatted')
c
c
c     # Read the input in standard form from claw2ez.data:

c     domain variables
      read(55,*) mx
      read(55,*) my

c     i/o variables
      read(55,*) nout
      read(55,*) outstyle
      if (outstyle.eq.1) then
          read(55,*) tfinal
          nstepout = 1
        elseif (outstyle.eq.2) then
          read(55,*) (tout(i), i=1,nout)
          nstepout = 1
        elseif (outstyle.eq.3) then
          read(55,*) nstepout, nstop
          nout = nstop
        end if


c     timestepping variables
      read(55,*) dtv(1)
      read(55,*) dtv(2)
      read(55,*) cflv(1)
      read(55,*) cflv(2)
      read(55,*) nv(1)
c
c     # input parameters for clawpack routines
      read(55,*) method(1)
      read(55,*) method(2)
      read(55,*) method(3)
      read(55,*) method(4)
      read(55,*) method(5)
      read(55,*) method(6)
      read(55,*) method(7)

      read(55,*) meqn1
      read(55,*) mwaves1
      read(55,*) (mthlim(mw), mw=1,mwaves1)

      read(55,*) t0
      read(55,*) xlower
      read(55,*) xupper
      read(55,*) ylower
      read(55,*) yupper

      read(55,*) mbc1
      read(55,*) mthbc(1)
      read(55,*) mthbc(2)
      read(55,*) mthbc(3)
      read(55,*) mthbc(4)

      if ((mthbc(1).eq.2 .and. mthbc(2).ne.2) .or.
     &    (mthbc(2).eq.2 .and. mthbc(1).ne.2)) then
         write(6,*) '*** ERROR ***  periodic boundary conditions'
         write(6,*) 'require mthbc(1) and mthbc(2) BOTH be set to 2'
         stop
         end if

      if ((mthbc(3).eq.2 .and. mthbc(4).ne.2) .or.
     &    (mthbc(4).eq.2 .and. mthbc(3).ne.2)) then
         write(6,*) '*** ERROR ***  periodic boundary conditions'
         write(6,*) 'require mthbc(3) and mthbc(4) BOTH be set to 2'
         stop
         end if

c     # These values were passed in, but check for consistency:
c
      if (method(7) .ne. maux) then
         write(6,*) '*** ERROR ***  method(7) should equal maux'
         stop
         end if
      if (meqn1 .ne. meqn) then
         write(6,*) '*** ERROR ***  meqn set wrong in input or driver'
         stop
         end if
      if (mwaves1 .ne. mwaves) then
         write(6,*) '*** ERROR ***  mwaves set wrong in input or driver'
         stop
         end if
      if (mbc1 .ne. mbc) then
         write(6,*) '*** ERROR ***  mbc set wrong in input or driver'
         stop
         end if
c
c     # check that enough storage has been allocated:
c
      if (method(5).lt.2) then
          narray = 1   !# only need one qwork array
        else
          narray = 2   !# need two qwork arrays for Strang splitting
        end if

      maxm = max0(maxmx, maxmy)
      mwork1 = (maxm+2*mbc)*(10*meqn + mwaves + meqn*mwaves
     &                      + 3*maux + 2)
     &          + narray * (maxmx + 2*mbc) * (maxmy + 2*mbc) * meqn
c
      if (mx.gt.maxmx .or. my.gt.maxmy .or. mwork.lt.mwork1) then
c        # insufficient storage
         maxmx1 = max0(mx,maxmx)
         maxmy1 = max0(my,maxmy)
         maxm1 = max0(maxmx1,maxmy1)

         mwork1 = (maxm1+2*mbc)*(10*meqn + mwaves + meqn*mwaves
     &                      + 3*maux + 2)
     &          + narray * (maxmx1 + 2*mbc) * (maxmy1 + 2*mbc) * meqn

         write(6,*) ' '
         write(6,*) '*** ERROR *** Insufficient storage allocated'
         write(6,*) 'Recompile after increasing values in driver.f:'
         write(6,611) maxmx1
         write(6,612) maxmy1
         write(6,613) mwork1
 611     format(/,'parameter (maxmx = ',i5,')')
 612     format('parameter (maxmy = ',i5,')')
 613     format('parameter (mwork = ',i9,')',/)
         stop
         end if

      write(6,*) 'running...'
      write(6,*) ' '
c
c     # grid spacing
      dx = (xupper - xlower) / float(mx)
      dy = (yupper - ylower) / float(my)
c
c     # time increments between outputing solution:
      if (outstyle .eq. 1) then
         dtout = (tfinal - t0)/float(nout)
         end if
c
      write(11,1101) nout
      write(11,1101) 1
c
 1101 format(i5)
c
c     # call user's routine setprob to set any specific parameters
c     # or other initialization required.
c
      call setprob
c
c  Set auxilliary arrays.
c
      if (maux .gt. 0)  then
         call setaux2(maxmx,maxmy,mbc,mx,my,xlower,ylower,dx,dy,
     &               maux,aux)
         end if
c
c     # set initial conditions:
      iframe = 0
      outt0  = .true.
      tinitial = t0

      call qinit(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &          dx,dy,q,maux,aux)
c
c     # RESTART: reset initial time if changed by restart.
      t0 = tinitial
c
      if (outt0) then
c        # output initial data
         call out2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,dx,dy,
     &          q,t0,iframe)
         write(6,601) iframe, t0
         end if

c
c     Main loop:
c
      tend = t0
      n0   = iframe*nstepout + 1
      do 100 n=n0,nout
         tstart = tend
         if (outstyle .eq. 1)  tend = tstart + dtout
         if (outstyle .eq. 2)  tend = tout(n)
         if (outstyle .eq. 3)  tend = tstart - 1.d0  !# single-step mode
c
         call claw2(maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &           q,aux,xlower,ylower,dx,dy,tstart,tend,dtv,
     &           cflv,nv,method,mthlim,mthbc,
     &           work,mwork,info,bc2,rpn2,rpt2,src2,b4step2)
c
c        # check to see if an error occured:
         if (info .ne. 0) then
            write(6,*) 'claw2ez aborting: Error return from claw2',
     &                 ' with info =',info
            go to 999
            end if
c
         dtv(1) = dtv(5)  !# use final dt as starting value on next call
c
c        # output solution at this time
c
c        # if outstyle=1 or 2, then nstepout=1 and we output every time
c        # we reach this point, since claw1 was called for the entire time
c        # increment between outputs.
c
c        # if outstyle=3 then we only output if we have taken nstepout
c        # time steps since the last output.

c        # iframe is the frame number used to form file names in out1
         iframe = n/nstepout
         if (iframe*nstepout .eq. n) then
            call out2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,dx,dy,
     &             q,tend,iframe)
            write(6,601) iframe,tend
            write(10,1010) tend,info,dtv(3),dtv(4),dtv(5),
     &           cflv(3),cflv(4),nv(2)
            end if

c
c        # formats for writing out information about this call to claw:
c
  601    format('CLAW2EZ: Frame ',i4,
     &           ' matlab plot files done at time t =',
     &           d12.4,/)
c
 1010    format('tend =',d15.4,/,
     &       'info =',i5,/,'smallest dt =',d15.4,/,'largest dt =',
     &       d15.4,/,'last dt =',d15.4,/,'largest cfl =',
     &         d15.4,/,'last cfl =',d15.4,/,'steps taken =',i4,/)
c
  100    continue
c
  999 continue
c
      return
      end
      subroutine claw2(maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &           q,aux,xlower,ylower,dx,dy,tstart,tend,dtv,
     &           cflv,nv,method,mthlim,mthbc,
     &           work,mwork,info,bc2,rpn2,rpt2,src2,b4step2)

c*********************************************************************72
c
cc CLAW2 solves a hyperbolic system of conservation laws in two space dimensions.
c
c  Discussion:
c
c  The equations have the general form
c  
c     capa * q_t + A q_x + B q_y = psi
c
c  The "capacity function" capa(x,y) and source term psi are optional 
c  (see below).
c
c  For a more complete description see the documentation at
c      http://www.amath.washington.edu/~claw
c
c  Sample driver programs and user-supplied subroutines are available.
c  See the the directories claw/clawpack/2d/example* for some examples, and
c  codes in claw/applications for more extensive examples.
c
c
c
c  The user must supply the following subroutines:
c
c    bc2, rpn2, rpt2,        subroutines specifying the boundary conditions
c                            and Riemann solvers.
c
c    b4step2            The routine b4step2 is called each time step and
c                       can be supplied by the user in order to perform
c                       other operations that are necessary every time
c                       step.  For example, if the variables stored in
c                       the aux arrays are time-dependent then these
c                       values can be set.   
c
c  In addition, if the equation contains source terms psi, then the user
c  must provide:
c
c    src2               subroutine that solves capa * q_t = psi
c                       over a single time step.
c
c  These routines must be declared EXTERNAL in the main program.
c  For description of the calling sequences, see below.
c
c  Dummy routines b4step1.f and src1.f are available in
c       claw/clawpack/1d/lib
c
c  A subroutine implementing many standard boundary conditions is
c  available in claw/clawpack/2d/lib/bc2.f.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
c
c  Description of parameters...
c
c
c    maxmx is the maximum number of interior grid points in x, 
c          and is used in declaration of the array q
c
c    maxmy is the maximum number of interior grid points in y, 
c          and is used in declaration of the array q
c
c    meqn is the number of equations in the system of
c         conservation laws.
c
c    mwaves is the number of waves that result from the
c           solution of each Riemann problem.  Often mwaves = meqn but
c           for some problems these may be different, e.g. for the Euler
c           equations meqn = 4 but mwaves = 3 since there are only 3
c           distinct wave speeds.
c
c    mbc is the number of "ghost cells" that must be added on to each
c       side of the domain to handle boundary conditions.  The cells
c       actually in the physical domain are labelled from 1 to mx in x and
c       from 1 to my in y.  The arrays are dimensioned actually indexed
c       from 1-mbc to mx+mbc and from 1-mbc to my+mbc.
c       For the methods currently implemented, mbc = 2 should be used.
c       If the user implements another method that has a larger stencil and
c       hence requires more ghost cells, a larger value of mbc could be used.
c       q is extended from the physical domain to the ghost cells by the
c       user-supplied routine bc2.
c
c    mx is the number of grid cells in the x-direction, in the
c       physical domain.  In addition there are mbc grid cells
c       along each edge of the grid that are used for boundary
c       conditions.
c       Must have mx .le. maxmx
c 
c    my is the number of grid cells in the y-direction, in the
c       physical domain.  In addition there are mbc grid cells
c       along each edge of the grid that are used for boundary
c       conditions.
c       Must have my .le. maxmy
c 
c    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn) 
c        On input:  initial data at time tstart.
c        On output: final solution at time tend.
c        q(i,j,m) = value of mth component in the (i,j) cell.
c        Values within the physical domain are in q(i,j,m) 
c                for i = 1,2,...,mx   and j = 1,2,...,my.
c        mbc extra cells on each end are needed for boundary conditions
c        as specified in the routine bc2.
c
c    aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, maux)
c        Array of auxiliary variables that are used in specifying the problem.
c        If method(7) = 0 then there are no auxiliary variables and aux
c                         can be a dummy variable.
c        If method(7) = maux > 0 then there are maux auxiliary variables
c                         and aux must be dimensioned as above.
c
c        Capacity functions are one particular form of auxiliary variable.
c        These arise in some applications, e.g. the
c        determinant of the Jacobian if a mapped grid is used, or a density
c        or porosity function in some advection problems.  
c        See Clawpack Note # 5 for examples.
c
c        If method(6) = 0 then there is no capacity function.
c        If method(6) = mcapa > 0  then there is a capacity function and 
c            capa(i,j), the "capacity" of the (i,j) cell, is assumed to be 
c            stored in aux(i,j,mcapa).
c            In this case we require method(7).ge.mcapa.
c
c    dx = grid spacing in x.  
c         (for a computation in ax <= x <= bx,  set dx = (bx-ax)/mx.)
c
c    dy = grid spacing in y.  
c         (for a computation in ay <= y <= by,  set dy = (by-ay)/my.)
c
c    tstart = initial time.
c
c    tend = Desired final time (on input).
c              If tend<tstart, then claw2 returns after a single successful
c                 time step has been taken (single-step mode).  
c              Otherwise, as many steps are taken as needed to reach tend, 
c                 up to a maximum of nv(1).
c         = Actual time reached (on output).
c
c    dtv(1:5) = array of values related to the time step:
c               (Note: method(1)=1 indicates variable size time steps)
c         dtv(1) = value of dt to be used in all steps if method(1) = 0
c                = value of dt to use in first step if method(1) = 1
c         dtv(2) = unused if method(1) = 0.
c                = maximum dt allowed if method(1) = 1.
c         dtv(3) = smallest dt used (on output)
c         dtv(4) = largest dt used (on output)
c         dtv(5) = dt used in last step (on output)
c
c    cflv(1:4) = array of values related to Courant number:
c         cflv(1) = maximum Courant number to be allowed.  
c                   With variable time steps the step is retracted and a 
c                   smaller step taken if the Courant
c                   number is larger than this value.  
c                   With fixed time steps the routine aborts.
c                   Usually cflv(1)=1.0 should work 
c                   (or cflv(1)=0.5 if method(3)=0).
c         cflv(2) = unused if method(1) = 0.
c                 = desired Courant number if method(1) = 1.
c                   Should be somewhat less than cflv(1), e.g. 0.9
c         cflv(3) = largest Courant number observed (on output).
c         cflv(4) = Courant number in last step (on output).
c
c    nv(1:2) = array of values related to the number of time steps:
c         nv(1) = unused if method(1) = 0
c               = maximum number of time steps allowed if method(1) = 1
c         nv(2) = number of time steps taken (on output).
c
c    method(1:7) = array of values specifying the numerical method to use
c                  and also indicating whether source terms, capacity
c                  function, auxiliary variables are present in the equation.
c
c         method(1) = 0 if fixed size time steps are to be taken.
c                       In this case, dt = dtv(1) in all steps.
c                   = 1 if variable time steps are to be used.
c                       In this case, dt = dtv(1) in the first step and
c                       thereafter the value cflv(2) is used to choose the
c                       next time step based on the maximum wave speed seen
c                       in the previous step.  Note that since this value
c                       comes from the previous step, the Courant number will
c                       not in general be exactly equal to the desired value
c                       If the actual Courant number in the next step is
c                       greater than cflv(1), then this step is redone with a 
c                       smaller dt.
c
c         method(2) = 1 if only first order increment waves are to be used.
c                   = 2 if second order correction terms are to be added, with
c                       a flux limiter as specified by mthlim.  
c
c
c         method(3) = 0 if no transverse propagation is to be applied.
c                       Increment and perhaps correction waves are propagated
c                       normal to the interface.
c                   = 1 if transverse propagation of increment waves 
c                       (but not correction waves, if any) is to be applied.
c                   = 2 if transverse propagation of correction waves is also
c                       to be included.  
c
c                   = -1 if dimensional splitting is to be used instead
c                        of the multi-dimensional wave-propagation.  The
c                        Godunov splitting is used which consists of
c                        sweeping first in x and then in y, with a step of
c                        length dt in each.  The routine bc2 is called
c                        before either sweep to set boundary data, and in
c                        the x-sweep goes over the rows of ghost cells too
c                        so that proper boundary conditions should be set
c                        for the y-sweeps by this process.  Dimensional
c                        splitting is somewhat faster than the unsplit
c                        method and works as well for many (though not all)
c                        problems.
c
c                   = -2 if dimensional splitting is to be used with the
c                        Strang splitting, consisting of 
c                           sweep in x over time dt/2
c                           sweep in y over time dt
c                           sweep in x over time dt/2
c                        This is not recommended because it is slower than
c                        the Godunov splitting and does not appear to be
c                        appreciably better.  Moreover, the boundary
c                        conditions will not be properly set for the final
c                        x-sweep.  (The code could be modified to achieve
c                        this by sweeping over more ghost cells.)
c
c         method(4) = 0 to suppress printing
c                   = 1 to print dt and Courant number every time step
c
c         method(5) = 0 if there is no source term psi.  In this case
c                       the subroutine src2 is never called so a dummy
c                       parameter can be given.
c                   = 1 if there is a source term.  In this case 
c                       the subroutine src2 must be provided and a 
c                       fractional step method is used.
c                       In each time step the following sequence is followed:
c                            call bc to extend data to ghost cells
c                            call step2 to advance hyperbolic eqn by dt
c                            call src2 to advance source terms by dt
c                   = 2 if there is a source term and Strang splitting is to
c                       be used instead of the Godunov splitting above.
c                       In each time step the following sequence is followed:
c                            call bc to extend data to ghost cells
c                            call src2 to advance source terms by dt/2
c                            call step2 to advance hyperbolic equation by dt
c                            call src2 to advance source terms by dt/2
c                       For most problems 1 is recommended rather than 2
c                       since it is less expensive and works essentially as 
c                       well on most problems.  
c                           
c
c         method(6) = 0 if there is no capacity function capa.  
c                   = mcapa > 0 if there is a capacity function.  In this case 
c                       aux(i,j,mcapa) is the capacity of cell (i,j) and you
c                       must also specify method(7) .ge. mcapa and set aux.
c
c         method(7) = 0 if there is no aux array used.
c                   = maux > 0  if there are maux auxiliary variables.
c
c         The recommended choice of methods for most problems is 
c            method(1) = 1,  method(2) = 2,  method(3) = 2.
c
c
c    mthlim(1:mwaves) = array of values specifying the flux limiter to be used
c                     in each wave family mw.  Often the same value will be used
c                     for each value of mw, but in some cases it may be
c                     desirable to use different limiters.  For example,
c                     for the Euler equations the superbee limiter might be
c                     used for the contact discontinuity (mw=2) while another
c                     limiter is used for the nonlinear waves.  Several limiters
c                     are built in and others can be added by modifying the
c                     subroutine philim.
c
c        mthlim(mw) = 0 for no limiter
c                   = 1 for minmod
c                   = 2 for superbee
c                   = 3 for van Leer
c                   = 4 for monotonized centered
c
c    mthbc(1:4) = array of values specifying what boundary conditions should
c                 be used at each edge of the domain, if the standard
c                 bc2.f routine is used.  Passed to bc2.
c
c    work(mwork) = double precision work array of length at least mwork
c
c    mwork = length of work array.  Must be at least
c               N * (maxmx + 2*mbc) * (maxmy + 2*mbc) * meqn   
c               + (max(mx,my) + 2*mbc) * (10*meqn + mwaves + meqn*mwaves 
c                                          + 3*maux + 2) 
c            where N = 1 if method(5)<2  (no source term or Godunov splitting)
c                  N = 2 if method(5)=2  (source term with Strang splitting)
c            If mwork is too small then the program returns with info = 4
c            and also prints the required value of mwork to unit 6.
c
c            
c    info = output value yielding error information:
c         = 0 if normal return.
c         = 1 if mx.gt.maxmx  or   my.gt.maxmy  or  mbc.lt.2
c         = 2 if method(1)=0 and dt doesn't divide (tend - tstart).
c         = 3 if method(1)=1 and cflv(2) > cflv(1).
c         = 4 if mwork is too small.
c         = 5 if method(6) > method(7)
c         = 6 if method(3) > method(2)
c         = 11 if the code attempted to take too many time steps, n > nv(1).
c              This could only happen if method(1) = 1 (variable time steps).
c         = 12 if the method(1)=0 and the Courant number is greater than 1
c              in some time step.
c
c           Note: if info.ne.0, then tend is reset to the value of t actually
c           reached and q contains the value of the solution at this time.
c
c    User-supplied subroutines
c
c    bc2 = subroutine that specifies the boundary conditions.  
c         This subroutine should extend the values of q from cells
c         (1:mx, 1:my) to the mbc ghost cells along each edge of the domain.
c
c
c    rpn2 = user-supplied subroutine that implements the Riemann solver
c           along a one-dimensional slice of data.
c
c          The form of this subroutine is
c
c     subroutine rpn2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,
c    &                  auxl,auxr,wave,s,amdq,apdq)
c
c     implicit double precision (a-h,o-z)
c     dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
c     dimension    s(1-mbc:maxm+mbc, mwaves)
c     dimension   ql(1-mbc:maxm+mbc, meqn)
c     dimension   qr(1-mbc:maxm+mbc, meqn)
c     dimension auxl(1-mbc:maxm+mbc, *)
c     dimension auxr(1-mbc:maxm+mbc, *)
c     dimension amdq(1-mbc:maxm+mbc, meqn)
c     dimension apdq(1-mbc:maxm+mbc, meqn)
c
c         On input, ql contains the state vector at the left edge of each cell
c                   qr contains the state vector at the right edge of each cell
c                 auxl contains auxiliary values at the left edge of each cell
c                 auxr contains auxiliary values at the right edge of each cell
c
c         This data is along a slice in the x-direction if ixy=1
c                                    or the y-direction if ixy=2.
c
c         Note that the i'th Riemann problem has left state qr(i-1,:)
c                                            and right state ql(i,:)
c         In the standard clawpack routines, this Riemann solver is 
c         called with ql=qr=q along this slice.  More flexibility is allowed
c         in case the user wishes to implement another solution method
c         that requires left and rate states at each interface.

c         If method(7)=maux > 0 then the auxiliary variables along this slice
c         are passed in using auxl and auxr.  Again, in the standard routines
c         auxl=auxr is just the values of aux along this slice.

c          On output, 
c             wave(i,m,mw) is the mth component of the jump across
c                              wave number mw in the ith Riemann problem.
c             s(i,mw) is the wave speed of wave number mw in the
c                              ith Riemann problem.
c             amdq(i,m) is the m'th component of the left-going flux difference.
c             apdq(i,m) is the m'th component of the right-going flux difference.
c           It is assumed that each wave consists of a jump discontinuity
c           propagating at a single speed, as results, for example, from a
c           Roe approximate Riemann solver.  An entropy fix can be included
c           into the specification of amdq and apdq.
c
c
c    rpt2 = user-supplied subroutine that implements the splitting of
c           a flux difference asdq into waves in the transverse direction.
c           The form of this subroutine is
c
c     subroutine rpt2(ixy,maxm,meqn,mwaves,mbc,mx,ql,qr,aux1,aux2,aux3,
c                     imp,asdq,bmasdq,bpasdq)
c
c     implicit double precision (a-h,o-z)
c     dimension     ql(1-mbc:maxm+mbc, meqn)
c     dimension     qr(1-mbc:maxm+mbc, meqn)
c     dimension   aux1(1-mbc:maxm+mbc, maux)
c     dimension   aux2(1-mbc:maxm+mbc, maux)
c     dimension   aux3(1-mbc:maxm+mbc, maux)
c     dimension   asdq(1-mbc:maxm+mbc, meqn)
c     dimension bmasdq(1-mbc:maxm+mbc, meqn)
c     dimension bpasdq(1-mbc:maxm+mbc, meqn)
c
c          On input, 
c              ql,qr is the data along some one-dimensional slice, as in rpn2
c                   This slice is in the x-direction
c                   if ixy=1, or in the y-direction if ixy=2.  
c              aux2 is the auxiliary array (if method(6)=maux>0) along
c                   this slice, say at j=J if ixy=1.
c              aux1 is the auxiliary array along the adjacent slice J-1
c              aux3 is the auxiliary array along the adjacent slice J+1
c          
c              asdq is an array of flux differences (A^* \Delta q).  
c                   asdq(i,:) is the flux difference propagating away from
c                   the interface between cells i-1 and i.
c              imp = 1 if asdq = A^- \Delta q,  the left-going flux difference
c                    2 if asdq = A^+ \Delta q, the right-going flux difference
c          On output, 
c              bmasdq is the down-going portion of the flux difference
c                   determined by solving a Riemann problem in the transverse
c                   direction using asdq as data.  
c              bpasdq is the up-going portion of the flux difference.
c           For example, for a linear system q_t + Aq_x + Bq_y = 0,  
c                   asdq = A^+ dq  or  A^- dq
c                   and this is then split into
c                       bmasdq = B^- asdq   and   bpasdq = B^+ asdq
c
c
c    src2 = user-supplied subroutine that takes one time step on the 
c           source terms alone, solving
c               capa * q_t = psi
c           over time dt.
c
c           If method(5)=0 then the equation does not contain a source
c           term and this routine is never called.  A dummy argument can
c           be used with many compilers, or provide a dummy subroutine that
c           does nothing (such a subroutine can be found in 
c           clawpack/2d/lib/src2.f)
c
c           The form of this subroutine is
c
c      subroutine src2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
c    &                 dx,dy,q,maux,aux,told,dt2)
c      implicit double precision (a-h,o-z)
c      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
c      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, *)
c
c      If method(7)=0  or the auxiliary variables are not needed in this solver,
c      then the latter dimension statement can be omitted, but aux should
c      still appear in the argument list.
c
c      On input, q(i,j,m) contains the data for solving the 
c                source term equation.
c      On output, q(i,j,m) should have been replaced by the solution to
c                 the source term equation after a step of length dt.
c
c
c
c      b4step2 = subroutine that is called from claw2 before each call to
c                step2.  Use to set time-dependent aux arrays or perform
c                other tasks which must be done every time step.
c
c          The form of this subroutine is
c
c
c      subroutine b4step2(maxmx,maxmy,mbc,mx,my,meqn,q,
c    &            xlower,ylower,dx,dy,time,dt,maux,aux)
c      implicit double precision (a-h,o-z)
c      dimension   q(1-mbc:maxmx+mbc, meqn)
c      dimension aux(1-mbc:maxmx+mbc, *)
c
c
c  
c
c
c  Copyright 1994 -- 1999 R. J. LeVeque
c
c  This software is made available for research and instructional use only. 
c  You may copy and use this software without charge for these non-commercial
c  purposes, provided that the copyright notice and associated text is
c  reproduced on all copies.  For all other uses (including distribution of
c  modified versions), please contact the author at the address given below. 
c  
c  *** This software is made available "as is" without any assurance that it
c  *** will work for your purposes.  The software may in fact have defects, so
c  *** use the software at your own risk.
c
c
c    CLAWPACK Version 4.1,  August, 2002
c    Webpage: http://www.amath.washington.edu/~claw
c
c
c    Author:  Randall J. LeVeque
c             Applied Mathematics
c             Box 352420
c             University of Washington, 
c             Seattle, WA 98195-2420
c             rjl@amath.washington.edu
c
c    Beginning of claw2 code
c
      implicit double precision (a-h,o-z)

      external bc2,rpn2,rpt2,src2,b4step2
      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, *)
      dimension work(mwork)
      dimension mthlim(mwaves),method(7),dtv(5),cflv(4),nv(2),mthbc(4)
      common /comxyt/ dtcom,dxcom,dycom,tcom,icom,jcom
c
      maxm = max0(maxmx, maxmy)
      info = 0
      t = tstart
      maxn = nv(1)
      dt = dtv(1)   !# initial dt
      cflmax = 0.d0
      dtmin = dt
      dtmax = dt
      nv(2) = 0
      maux = method(7)
c
c     # check for errors in data:
c
c
      if (mx.gt.maxmx .or. my.gt.maxmy .or. mbc.lt.2) then
         info = 1
         write(6,*) 'CLAW2 ERROR...  check mx,maxmx,my,maxmy,mbc'
         go to 900
         end if
c
      if (method(1) .eq. 0) then
c        # fixed size time steps.  Compute the number of steps:
         if (tend .lt. tstart) then
c             # single step mode 
              maxn = 1
           else
              maxn = (tend - tstart + 1d-10) / dt
              if (dabs(maxn*dt - (tend-tstart)) .gt.
     &                          1d-5*(tend-tstart)) then
c                # dt doesn't divide time interval integer number of times
                 info = 2
                 write(6,*) 
     &               'CLAW2 ERROR... dt does not divide (tend-tstart)'
                 go to 900
                 end if
           end if
         end if

c
      if (method(1).eq.1 .and. cflv(2).gt.cflv(1)) then
         info = 3
         write(6,*) 'CLAW2 ERROR...  cflv(2) > cflv(1)'
         go to 900
         end if
c
      if (method(6).gt.method(7)) then
         info = 5
         write(6,*) 'CLAW2 ERROR...  method(6) > method(7)'
         go to 900
         end if
c
      if (method(2) .lt. method(3)) then
         info = 6
         write(6,*) 'CLAW2 ERROR...  method(3) > method(2)'
         go to 900
         end if
c
      if (method(5).lt.2) then
          narray = 1   !# only need one qwork array
        else
          narray = 2   !# need two qwork arrays for Strang splitting
        end if
c
      mwork0 = (maxm+2*mbc)*(10*meqn + mwaves + meqn*mwaves 
     &                      + 3*maux + 2) 
     &          + narray * (maxmx + 2*mbc) * (maxmy + 2*mbc) * meqn   
c
      if (mwork .lt. mwork0) then
         info = 4
         write(6,*) 'CLAW2 ERROR... mwork should be increased to ',
     &               mwork0
         go to 900
         end if
c
c     # partition work array into pieces needed for local storage in 
c     # step2 routine. Find starting index of each piece:
c
      i0qadd = 1
      i0fadd = i0qadd + (maxm+2*mbc)*meqn
      i0gadd = i0fadd + (maxm+2*mbc)*meqn
      i0q1d = i0gadd + 2*(maxm+2*mbc)*meqn 
      i0dtdx1 = i0q1d + (maxm+2*mbc)*meqn  
      i0dtdy1 = i0dtdx1 + (maxm+2*mbc)
      i0qwrk1 = i0dtdy1 + (maxm+2*mbc)
c
      nqwork = (maxmx + 2*mbc) * (maxmy + 2*mbc) * meqn  !# size of q array
      if (method(5).lt.2) then
          i0qwrk2 = i0qwrk1  !# qwrk2 points to same storage as qwrk1
        else
          i0qwrk2 = i0qwrk1 + nqwork  !# second qwork array is needed for
                                      !# Strang spliting
        end if
c
      i0aux1 = i0qwrk2 + nqwork
      i0aux2 = i0aux1 + (maxm+2*mbc)*maux
      i0aux3 = i0aux2 + (maxm+2*mbc)*maux
c
      i0next = i0aux3 + (maxm+2*mbc)*maux  !# next free space
      mused = i0next - 1                  !# space already used
      mwork1 = mwork - mused              !# remaining space (passed to step2)
c
c     # main loop
c
      if (maxn.eq.0) go to 900
      do 100 n=1,maxn
         told = t   !# time at beginning of time step.

c        # adjust dt to hit tend exactly if we're near end of computation
c        #  (unless tend < tstart, which is a flag to take only a single step)
         if (told+dt.gt.tend .and. tstart.lt.tend) dt = tend - told

c
   40    continue
c
c        # store dt and t in the common block comxyt in case they are needed
c        # in the Riemann solvers (for variable coefficients)
         tcom = told
         dtcom = dt
         dxcom = dx
         dycom = dy
c
c        # main steps in algorithm
c
c
c        # extend data from grid to bordering boundary cells:
         call bc2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &               dx,dy,q,maux,aux,told,dt,mthbc)
c
c
c
c        # call user-supplied routine which might set aux arrays
c        # for this time step, for example.

         call b4step2(maxmx,maxmy,mbc,mx,my,meqn,q,
     &                xlower,ylower,dx,dy,told,dt,maux,aux)
c
c
c
         if (method(5).eq.2) then
c            # with Strang splitting for source term:
c            # First need to store solution before taking
c            # step on source terms in case we need to redo everything with
c            # a smaller time step if the Courant number is too large in 
c            # subroutine step2.
             call copyq2(maxmx,maxmy,meqn,mbc,mx,my,q,work(i0qwrk2))
c
c            # source terms over a half time step:
             dt2 = dt / 2.d0
             call src2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                 dx,dy,q,maux,aux,told,dt2)
             end if
c
c        # copy q into qwork1.  q is updated in step2 and qwork1 is
c        # preserved to provide data for Riemann problems.
c        # qwork1 can also be used to restart if the Courant number is 
c        # too large, unless Strang splitting is used in which case we
c        # must used the values already stored above before
c        # taking the source term step.
         call copyq2(maxmx,maxmy,meqn,mbc,mx,my,q,work(i0qwrk1))
c
c        # take one step on the conservation law:
c
         if( method(3) .ge. 0 )then
c            # unsplit version
c             
             call step2(maxm,maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &                  work(i0qwrk1),q,aux,
     &                  dx,dy,dt,method,mthlim,cfl,
     &                  work(i0qadd),work(i0fadd),work(i0gadd),
     &                  work(i0q1d),work(i0dtdx1),work(i0dtdy1),
     &                  work(i0aux1),work(i0aux2),work(i0aux3),
     &                  work(i0next),mwork1,rpn2,rpt2)
c
         else
c           # dimensional splitting (fractional steps)
c
            call dimsp2(maxm,maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &                  work(i0qwrk1),q,aux,
     &                  dx,dy,dt,method,mthlim,cfl,cflv,
     &                  work(i0qadd),work(i0fadd),work(i0gadd),
     &                  work(i0q1d),work(i0dtdx1),work(i0dtdy1),
     &                  work(i0aux1),work(i0aux2),work(i0aux3),
     &                  work(i0next),mwork1,rpn2,rpt2)
c
         end if
c
         t = told + dt
c
         if (method(4).eq.1) then
c            # verbose mode
             write(6,601) n,cfl,dt,t
  601        format('CLAW2... Step',i6,
     &                   '   Courant number =',f6.3,'  dt =',d12.4,
     &                   '  t =',d12.4)
             end if
c
c
c        # check to see if the Courant number was too large:
         if (cfl .le. cflv(1)) then
c             # accept this step
              cflmax = dmax1(cfl,cflmax)
            else
c             # Reject this step.  Reset q to qwork from previous time:
c             # Note that i0qwrk2 points to work space where previous
c             # solution is stored in all cases method(5) = 0,1, or 2. 
              t = told
              call copyq2(maxmx,maxmy,meqn,mbc,mx,my,work(i0qwrk2),q)
c
              if (method(4).eq.1) then
c                # verbose mode
                 write(6,602)
                 end if
  602         format('CLAW2 rejecting step... Courant number too large')
c
              if (method(1).eq.1) then
c                 # if variable dt, go back and take a smaller step.
                  dt = dmin1(dtv(2), dt * cflv(2)/cfl)
                  go to 40
                else
c                 # if fixed dt, give up and return
                  cflmax = dmax1(cfl,cflmax)
                  go to 900
                end if
            end if
c
c
c        # claw2 step is accepted
c        # now apply source terms:
c
         if (method(5).eq.2) then
c            # source terms over a second half time step for Strang splitting:
c            # Note it is not so clear what time t should be used here if
c            # the source terms are time-dependent!
             call src2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                 dx,dy,q,maux,aux,t,dt2)
             end if
c
         if (method(5).eq.1) then
c            # source terms over a full time step:
             call src2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                 dx,dy,q,maux,aux,t,dt)
             end if
c
c
c
         if (method(1) .eq. 1) then
c           # choose new time step if variable time step
            if (cfl.eq.0.d0) then 
                dt = dtv(2)
              else
                dt = dmin1(dtv(2), dt * cflv(2)/cfl)
              end if
            dtmin = dmin1(dt,dtmin)
            dtmax = dmax1(dt,dtmax)
            end if
c
c
c        # see if we are done:
c
         nv(2) = nv(2) + 1
         if (t .ge. tend) go to 900
  100    continue
c
  900  continue
c 
c      # return information
c
       if (method(1).eq.1 .and. t.lt.tend .and. nv(2) .eq. maxn) then
c         # too many timesteps
          write(6,*) 'CLAW2 ERROR...  too many timesteps'
          info = 11
          end if
       if (method(1).eq.0 .and. cflmax .gt. cflv(1)) then
c         # Courant number too large with fixed dt
          write(6,*) 'CLAW2 ERROR...  Courant number too large'
          info = 12
          end if
       tend = t
       cflv(3) = cflmax
       cflv(4) = cfl
       dtv(3) = dtmin
       dtv(4) = dtmax
       dtv(5) = dt
c
       return 
       end
      subroutine claw3ez(maxmx,maxmy,maxmz,meqn,mwaves,mbc,maux,mwork,
     &                   mthlim,q,work,aux)

c*********************************************************************72
c
cc CLAW3EZ is an easy-to-use clawpack driver routine for simple applications.
c
c  Discussion:
c
c     Documentation is available at
c                 http://www.amath.washington.edu/~claw/doc.html
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external bc3,rpn3,rpt3,rptt3,src3,b4step3

      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &               1-mbc:maxmz+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &               1-mbc:maxmz+mbc, maux)
      dimension work(mwork)
      dimension mthlim(mwaves)

      dimension method(7),dtv(5),cflv(4),nv(2),mthbc(6)
      dimension tout(100)
      logical outt0

      common /restrt_block/ tinitial, iframe, outt0

      open(55,file='claw3ez.data',status='old',form='formatted')
      open(10,file='fort.info',status='unknown',form='formatted')
      open(11,file='fort.nplot',status='unknown',form='formatted')
c
c  Read the input in standard form from claw2ez.data:
c
c     domain variables
c
      read(55,*) mx
      read(55,*) my
      read(55,*) mz
c
c     i/o variables
c
      read(55,*) nout
      read(55,*) outstyle
      if (outstyle.eq.1) then
          read(55,*) tfinal
          nstepout = 1
        elseif (outstyle.eq.2) then
          read(55,*) (tout(i), i=1,nout)
          nstepout = 1
        elseif (outstyle.eq.3) then
          read(55,*) nstepout, nstop
          nout = nstop
        end if
c
c  timestepping variables
c
      read(55,*) dtv(1)
      read(55,*) dtv(2)
      read(55,*) cflv(1)
      read(55,*) cflv(2)
      read(55,*) nv(1)
c
c  input parameters for clawpack routines
c
      read(55,*) method(1)
      read(55,*) method(2)
      read(55,*) method(3)
      read(55,*) method(4)
      read(55,*) method(5)
      read(55,*) method(6)
      read(55,*) method(7)

      read(55,*) meqn1
      read(55,*) mwaves1
      read(55,*) (mthlim(mw), mw=1,mwaves1)

      read(55,*) t0
      read(55,*) xlower
      read(55,*) xupper
      read(55,*) ylower
      read(55,*) yupper
      read(55,*) zlower
      read(55,*) zupper
c
      read(55,*) mbc1
      read(55,*) mthbc(1)
      read(55,*) mthbc(2)
      read(55,*) mthbc(3)
      read(55,*) mthbc(4)
      read(55,*) mthbc(5)
      read(55,*) mthbc(6)

      if ((mthbc(1).eq.2 .and. mthbc(2).ne.2) .or.
     &    (mthbc(2).eq.2 .and. mthbc(1).ne.2)) then
         write(6,*) '*** ERROR ***  periodic boundary conditions'
         write(6,*) 'require mthbc(1) and mthbc(2) BOTH be set to 2'
         stop
         end if

      if ((mthbc(3).eq.2 .and. mthbc(4).ne.2) .or.
     &    (mthbc(4).eq.2 .and. mthbc(3).ne.2)) then
         write(6,*) '*** ERROR ***  periodic boundary conditions'
         write(6,*) 'require mthbc(3) and mthbc(4) BOTH be set to 2'
         stop
         end if

      if ((mthbc(5).eq.2 .and. mthbc(6).ne.2) .or.
     &    (mthbc(6).eq.2 .and. mthbc(5).ne.2)) then
         write(6,*) '*** ERROR ***  periodic boundary conditions'
         write(6,*) 'require mthbc(5) and mthbc(6) BOTH be set to 2'
         stop
         end if
c
c  These values were passed in, but check for consistency:
c
      if (method(7) .ne. maux) then
         write(6,*) '*** ERROR ***  method(7) should equal maux'
         stop
         end if
      if (meqn1 .ne. meqn) then
         write(6,*) '*** ERROR ***  meqn set wrong in input or driver'
         stop
         end if
      if (mwaves1 .ne. mwaves) then
         write(6,*) '*** ERROR ***  mwaves set wrong in input or driver'
         stop
         end if
      if (mbc1 .ne. mbc) then
         write(6,*) '*** ERROR ***  mbc set wrong in input or driver'
         stop
         end if
c
c  check that enough storage has been allocated:
c
      if (method(5).lt.2) then
          narray = 1   !# only need one qwork array
        else
          narray = 2   !# need two qwork arrays for Strang splitting
        end if

      maxm = max0(maxmx, maxmy, maxmz)
      mwork1 = (maxm+2*mbc)*(46*meqn + mwaves + meqn*mwaves
     &                      + 9*maux + 3)
     &          + narray * (maxmx + 2*mbc) * (maxmy + 2*mbc)
     &                   * (maxmz + 2*mbc) * meqn

      if (mx.gt.maxmx .or. my.gt.maxmy .or. mz.gt.maxmz .or.
     &    mwork.lt.mwork1) then
c
c  insufficient storage
c
         maxmx1 = max0(mx,maxmx)
         maxmy1 = max0(my,maxmy)
         maxmz1 = max0(mz,maxmz)
         maxm1 = max0(maxmx1,maxmy1,maxmz1)

         mwork1 = (maxm1+2*mbc)*(46*meqn + mwaves + meqn*mwaves
     &                      + 9*maux + 3)
     &          + narray * (maxmx + 2*mbc) * (maxmy + 2*mbc)
     &                   * (maxmz + 2*mbc) * meqn

         write(6,*) ' '
         write(6,*) '*** ERROR *** Insufficient storage allocated'
         write(6,*) 'Recompile after increasing values in driver.f:'
         write(6,611) maxmx1
         write(6,612) maxmy1
         write(6,613) maxmz1
         write(6,614) mwork1
 611     format(/,'parameter (maxmx = ',i5,')')
 612     format('parameter (maxmy = ',i5,')')
 613     format('parameter (maxmz = ',i5,')')
 614     format('parameter (mwork = ',i9,')',/)
         stop
         end if

      call chkmth(method,info)
      if( info .eq. 6) stop

      write(6,*) 'running...'
      write(6,*) ' '
c
c  grid spacing
c
      dx = (xupper - xlower) / float(mx)
      dy = (yupper - ylower) / float(my)
      dz = (zupper - zlower) / float(mz)
c
c  time increments between outputing solution:
c
      if (outstyle .eq. 1) then
         dtout = (tfinal - t0)/float(nout)
      end if

      write(11,1101) nout
      write(11,1101) 1

 1101 format(i5)
c
c  call user's routine setprob to set any specific parameters
c  or other initialization required.
c
      call setprob
c
c  Set auxilliary arrays.
c
      if (maux .gt. 0)  then
         call setaux3(maxmx,maxmy,maxmz,mbc,mx,my,mz,xlower,ylower,
     &               zlower,dx,dy,dz,maux,aux)
         end if
c
c  set initial conditions:
c
      iframe = 0
      outt0      = .true.
      tinitial = t0

      call qinit(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,xlower,ylower,
     &           zlower,dx,dy,dz,q,maux,aux)
c
c  RESTART: reset initial time if changed by restart.
c
      t0 = tinitial

      if (outt0) then
c
c  output initial data
c
         call out3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,xlower,ylower,
     &          zlower,dx,dy,dz,q,t0,iframe)
         write(6,601) iframe, t0
         end if
c
c     Main loop:
c
      tend = t0
      n0   = iframe*nstepout + 1
      do 100 n=n0,nout
         tstart = tend
         if (outstyle .eq. 1)  tend = tstart + dtout
         if (outstyle .eq. 2)  tend = tout(n)
         if (outstyle .eq. 3)  tend = tstart - 1.d0  !# single-step mode

         call claw3(maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,mz,
     &           q,aux,xlower,ylower,zlower,dx,dy,dz,tstart,tend,dtv,
     &           cflv,nv,method,mthlim,mthbc,
     &           work,mwork,info,bc3,rpn3,rpt3,rptt3,src3,b4step3)
c
c  check to see if an error occured:
c
         if (info .ne. 0) then
            write(6,*) 'claw3ez aborting: Error return from claw3',
     &                 ' with info =',info
            go to 999
            end if

         dtv(1) = dtv(5)  !# use final dt as starting value on next call
c
c  output solution at this time
c
c
c  if outstyle=1 or 2, then nstepout=1 and we output every time
c  we reach this point, since claw1 was called for the entire time
c  increment between outputs.
c
c  if outstyle=3 then we only output if we have taken nstepout
c  time steps since the last output.
c  iframe is the frame number used to form file names in out3
c
         if (mod(n,nstepout) .eq. 0) then
            iframe = iframe + 1
            call out3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,xlower,ylower,
     &            zlower,dx,dy,dz,q,tend,iframe)
            write(6,601) iframe,tend
            write(10,1010) tend,info,dtv(3),dtv(4),dtv(5),
     &           cflv(3),cflv(4),nv(2)
            end if
c
c  formats for writing out information about this call to claw:
c
  601    format('CLAW3EZ: Frame ',i4,
     &         ' matlab plot files done at time t =',
     &         d12.4,/)

 1010    format('tend =',d15.4,/,
     &       'info =',i5,/,'smallest dt =',d15.4,/,'largest dt =',
     &       d15.4,/,'last dt =',d15.4,/,'largest cfl =',
     &         d15.4,/,'last cfl =',d15.4,/,'steps taken =',i4,/)

  100    continue

  999 continue

      return
      end
      subroutine claw3(maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,mz,
     &           q,aux,xlower,ylower,zlower,dx,dy,dz,tstart,tend,dtv,
     &           cflv,nv,method,mthlim,mthbc,
     &           work,mwork,info,bc3,rpn3,rpt3,rptt3,src3,b4step3)

c*********************************************************************72
c
cc CLAW3 solves a hyperbolic system of conservation laws in three space dimensions.
c
c  Discussion:
c
c  The equations have the general form
c
c     capa * q_t + A q_x + B q_y + C q_z = psi
c
c  The "capacity function" capa(x,y,z) and source term psi are optional
c  (see below).
c
c  For a more complete description see the documentation at
c      http://www.amath.washington.edu/~claw
c
c
c  The user must supply the following subroutines:
c
c    bc3, rpn3, rpt3, rptt3  subroutines specifying the boundary conditions
c                            and Riemann solvers.
c
c    b4step3            The routine b4step3 is called each time step and
c                       can be supplied by the user in order to perform
c                       other operations that are necessary every time
c                       step.  For example, if the variables stored in
c                       the aux arrays are time-dependent then these
c                       values can be set.
c
c
c  In addition, if the equation contains source terms psi, then the user
c  must provide:
c
c    src3               subroutine that solves capa * q_t = psi
c                       over a single time step.
c
c  These routines must be declared EXTERNAL in the main program.
c  For description of the calling sequences, see below.
c
c  Dummy routines b4step3.f and src3.f are available in
c       claw/clawpack/3d/lib
c
c  A subroutine implementing many standard boundary conditions is
c  available in claw/clawpack/3d/lib/bc3.f.
c
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
c  Description of parameters...
c
c    maxmx is the maximum number of interior grid points in x,
c          and is used in declaration of the array q
c
c    maxmy is the maximum number of interior grid points in y,
c          and is used in declaration of the array q
c
c    maxmz is the maximum number of interior grid points in z,
c          and is used in declaration of the array q
c
c    meqn is the number of equations in the system of
c         conservation laws.
c
c    mwaves is the number of waves that result from the
c           solution of each Riemann problem.  Often mwaves = meqn but
c           for some problems these may be different, e.g. for the Euler
c           equations meqn = 5 but mwaves = 3 since there are only 3
c           distinct wave speeds.
c
c    mbc is the number of "ghost cells" that must be added on to each
c       side of the domain to handle boundary conditions.  The cells
c       actually in the physical domain are labelled from 1 to mx in x,
c       from 1 to my in y, and from 1 to mz in z.
c       The arrays are dimensioned actually indexed
c       from 1-mbc to mx+mbc, from 1-mbc to my+mbc and from 1-mbc
c       to mz+mbc.
c       For the methods currently implemented, mbc = 2 should be used.
c       If the user implements another method that has a larger stencil and
c       hence requires more ghost cells, a larger value of mbc could be used.
c       q is extended from the physical domain to the ghost cells by the
c       user-supplied routine bc2.
c
c    mx is the number of grid cells in the x-direction, in the
c       physical domain.  In addition there are mbc grid cells
c       along each edge of the grid that are used for boundary
c       conditions.
c       Must have mx .le. maxmx
c
c    my is the number of grid cells in the y-direction, in the
c       physical domain.  In addition there are mbc grid cells
c       along each edge of the grid that are used for boundary
c       conditions.
c       Must have my .le. maxmy
c
c    mz is the number of grid cells in the z-direction, in the
c       physical domain.  In addition there are mbc grid cells
c       along each edge of the grid that are used for boundary
c       conditions.
c       Must have my .le. maxmz
c
c    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 1-mbc:maxmz+mbc,meqn)
c        On input:  initial data at time tstart.
c        On output: final solution at time tend.
c        q(i,j,k,m) = value of mth component in the (i,j,k) cell.
c        Values within the physical domain are in q(i,j,k,m)
c                for i = 1,2,...,mx , j = 1,2,...,my, and
c                    k = 1,2,...,mz,
c        mbc extra cells on each end are needed for boundary conditions
c        as specified in the routine bc3.
c
c    aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 1-mbc:maxmy+mbc,maux)
c        Array of auxiliary variables that are used in specifying the problem.
c        If method(7) = 0 then there are no auxiliary variables and aux
c                         can be a dummy variable.
c        If method(7) = maux > 0 then there are maux auxiliary variables
c                         and aux must be dimensioned as above.
c
c        Capacity functions are one particular form of auxiliary variable.
c        These arise in some applications, e.g. the
c        determinant of the Jacobian if a mapped grid is used, or a density
c        or porosity function in some advection problems.
c        See Clawpack Note # 5 for examples.
c
c        If method(6) = 0 then there is no capacity function.
c        If method(6) = mcapa > 0  then there is a capacity function and
c            capa(i,j,k), the "capacity" of the (i,j,k) cell, is assumed to be
c            stored in aux(i,j,k,mcapa).
c            In this case we require method(7).ge.mcapa.
c
c    dx = grid spacing in x.
c         (for a computation in ax <= x <= bx,  set dx = (bx-ax)/mx.)
c
c    dy = grid spacing in y.
c         (for a computation in ay <= y <= by,  set dy = (by-ay)/my.)
c
c    dz = grid spacing in z.
c         (for a computation in az <= z <= bz,  set dz = (bz-az)/mz.)
c
c    tstart = initial time.
c
c    tend = Desired final time (on input).
c              If tend<tstart, then claw3 returns after a single successful
c                 time step has been taken (single-step mode).
c              Otherwise, as many steps are taken as needed to reach tend,
c                 up to a maximum of nv(1).
c         = Actual time reached (on output).
c
c    dtv(1:5) = array of values related to the time step:
c               (Note: method(1)=1 indicates variable size time steps)
c         dtv(1) = value of dt to be used in all steps if method(1) = 0
c                = value of dt to use in first step if method(1) = 1 
c         dtv(2) = unused if method(1) = 0.
c                = maximum dt allowed if method(1) = 1.
c         dtv(3) = smallest dt used (on output)
c         dtv(4) = largest dt used (on output)
c         dtv(5) = dt used in last step (on output)
c
c    cflv(1:4) = array of values related to Courant number:
c         cflv(1) = maximum Courant number to be allowed.
c                   With variable time steps the step is retracted and a
c                   smaller step taken if the Courant
c                   number is larger than this value.
c                   With fixed time steps the routine aborts.
c                   Usually cflv(1)=1.0 should work
c         cflv(2) = unused if method(1) = 0.
c                 = desired Courant number if method(1) = 1.
c                   Should be somewhat less than cflv(1), e.g. 0.9
c         cflv(3) = largest Courant number observed (on output).
c         cflv(4) = Courant number in last step (on output).
c
c    nv(1:2) = array of values related to the number of time steps:
c         nv(1) = unused if method(1) = 0
c               = maximum number of time steps allowed if method(1) = 1
c         nv(2) = number of time steps taken (on output).
c
c    method(1:7) = array of values specifying the numerical method to use
c                  and also indicating whether source terms, capacity
c                  function, auxiliary variables are present in the equation.
c
c         method(1) = 0 if fixed size time steps are to be taken.
c                       In this case, dt = dtv(1) in all steps.
c                   = 1 if variable time steps are to be used.
c                       In this case, dt = dtv(1) in the first step and
c                       thereafter the value cflv(2) is used to choose the
c                       next time step based on the maximum wave speed seen
c                       in the previous step.  Note that since this value
c                       comes from the previous step, the Courant number will
c                       not in general be exactly equal to the desired value
c                       If the actual Courant number in the next step is
c                       greater than cflv(1), then this step is redone with a
c                       smaller dt.
c
c         method(2) = 1 if only first order increment waves are to be used.
c                   = 2 if second order correction terms are to be added, with
c                       a flux limiter as specified by mthlim.
c
c         method(3) <  0 Gives dimensional splitting using Godunov
c                        splitting, i.e. formally first order
c                        accurate.
c                      0 Gives the Donor cell method. No transverse
c                        propagation of neither the increment wave
c                        nor the correction wave.
c                   = 10 Transverse propagation of the increment wave
c                        as in 2D. Note that method (2,10) is
c                        unconditionally unstable.
c                   = 11 Corner transport upwind of the increment
c                        wave. Note that method (2,11) also is
c                        unconditionally unstable.
c                   = 20 Both the increment wave and the correction
c                        wave propagate as in the 2D case. Only to
c                        be used with method(2) = 2.
c                   = 21 Corner transport upwind of the increment wave,
c                        and the correction wave propagates as in 2D.
c                        Only to be used with method(2) = 2.
c                   = 22 3D propagation of both the increment wave and
c                        the correction wave. Only to be used with
c                        method(2) = 2.
c
c         method(4) = 0 to suppress printing
c                   = 1 to print dt and Courant number every time step
c
c         method(5) = 0 if there is no source term psi.  In this case
c                       the subroutine src2 is never called so a dummy
c                       parameter can be given.
c                   = 1 if there is a source term.  In this case
c                       the subroutine src2 must be provided.
c
c         method(6) = 0 if there is no capacity function capa.
c                   = mcapa > 0 if there is a capacity function.  In this case
c                       aux(i,j,k,mcapa) is the capacity of cell (i,j,k)
c                       and you must also specify method(7) .ge. mcapa
c                       and set aux.
c
c         method(7) = 0 if there is no aux array used.
c                   = maux > 0  if there are maux auxiliary variables.
c
c         The recommended choice of methods for most problems is
c            method(1) = 1,  method(2) = 2,  method(3) = 22.
c
c    mthlim(1:mwaves) = array of values specifying the flux limiter to be used
c                     in each wave family mw.  Often the same value will be used
c                     for each value of mw, but in some cases it may be
c                     desirable to use different limiters.  For example,
c                     for the Euler equations the superbee limiter might be
c                     used for the contact discontinuity (mw=2) while another
c                     limiter is used for the nonlinear waves.  Several limiters
c                     are built in and others can be added by modifying the
c                     subroutine philim.
c
c        mthlim(mw) = 0 for no limiter
c                   = 1 for minmod
c                   = 2 for superbee
c                   = 3 for van Leer
c                   = 4 for monotonized centered
c
c    mthbc(1:6) = array of values specifying what boundary conditions should
c                 be used at each edge of the domain, if the standard
c                 bc3.f routine is used.  Passed to bc3.
c
c
c    work(mwork) = double precision work array of length at least mwork
c
c    mwork = length of work array.  Must be at least
c               N * (maxmx + 2*mbc)*(maxmy + 2*mbc)*(maxmz + 2*mbc)*meqn
c               + (max(mx,my) + 2*mbc) * (46*meqn + mwaves + meqn*mwaves
c                                          + 9*maux + 2)
c            where N = 1 if method(5)=0  (no source term)
c                  N = 2 if method(5)=1  (source term)
c            If mwork is too small then the program returns with info = 4
c            and also prints the required value of mwork to unit 6.
c
c
c    info = output value yielding error information:
c         = 0 if normal return.
c         = 1 if mx.gt.maxmx  or   my.gt.maxmy  or
c                mz.gt.maxmz  or   mbc.lt.2
c         = 2 if method(1)=0 and dt doesn't divide (tend - tstart).
c         = 3 if method(1)=1 and cflv(2) > cflv(1).
c         = 4 if mwork is too small.
c         = 5 if method(6) > method(7)
c         = 6 if illegal parameters are used in specifying the method.
c         = 11 if the code attempted to take too many time steps, n > nv(1).
c              This could only happen if method(1) = 1 (variable time steps).
c         = 12 if the method(1)=0 and the Courant number is greater than 1
c              in some time step.
c
c           Note: if info.ne.0, then tend is reset to the value of t actually
c           reached and q contains the value of the solution at this time.
c
c    User-supplied subroutines
c
c    bc3 = subroutine that specifies the boundary conditions.
c          This subroutine should extend the values of q from cells
c          (1:mx, 1:my, 1:mz) to the mbc ghost cells along each edge
c          of the domain.  The routine claw/clawpack/3d/lib/bc3.f
c          implements many standard choices.
c
c
c    rpn3 = user-supplied subroutine that implements the Riemann solver
c           along a one-dimensional slice of data.
c
c          The form of this subroutine is
c
c     subroutine rpn3(ixyz,maxm,meqn,mwaves,mbc,mx,ql,qr,
c    &                  auxl,auxr,maux,wave,s,amdq,apdq)
c    implicit double precision (a-h,o-z)
c     dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
c     dimension    s(1-mbc:maxm+mbc, mwaves)
c     dimension   ql(1-mbc:maxm+mbc, meqn)
c     dimension   qr(1-mbc:maxm+mbc, meqn)
c     dimension amdq(1-mbc:maxm+mbc, meqn)
c     dimension apdq(1-mbc:maxm+mbc, meqn)
c     dimension auxl(1-mbc:maxm+mbc, maux, 3)
c     dimension auxr(1-mbc:maxm+mbc, maux, 3)
c
c         solve Riemann problems along one slice of data.
c         This data is along a slice in the x-direction if ixyz=1
c                                       the y-direction if ixyz=2.
c                                       the z-direction if ixyz=3.
c
c         On input, ql contains the state vector at the left edge of each cell
c                   qr contains the state vector at the right edge of each cell
c
c         auxl(i,ma,2) contains auxiliary data for cells along this slice,
c            where ma=1,maux in the case where maux=method(7) > 0.
c         auxl(i,ma,1) and auxl(i,ma,3) contain auxiliary data along
c         neighboring slices that generally aren't needed in the rpn3 routine.
c
c
c         Note that the i'th Riemann problem has left state qr(i-1,:)
c                                            and right state ql(i,:)
c         From the basic clawpack routines, this routine is called with ql = qr
c
c         More flexibility is allowed
c         in case the user wishes to implement another solution method
c         that requires left and rate states at each interface.
c
c
c          On output,
c             wave(i,m,mw) is the mth component of the jump across
c                              wave number mw in the ith Riemann problem.
c             s(i,mw) is the wave speed of wave number mw in the
c                              ith Riemann problem.
c             amdq(i,m) is the m'th component of the left-going flux
c                       difference.
c             apdq(i,m) is the m'th component of the right-going flux
c                       difference.
c           It is assumed that each wave consists of a jump discontinuity
c           propagating at a single speed, as results, for example, from a
c           Roe approximate Riemann solver.  An entropy fix can be included
c           into the specification of amdq and apdq.
c
c
c    rpt3 = user-supplied subroutine that implements the splitting of
c           a flux difference asdq into waves in the transverse direction.
c           The form of this subroutine is
c
c     subroutine rpt3(ixyz,icoor,maxm,meqn,mwaves,mbc,mx,
c    &                  ql,qr,aux1,aux2,aux3,maux,imp,asdq,
c    &                  bmasdq,bpasdq)
c     implicit double precision (a-h,o-z)
c     dimension     ql(1-mbc:maxm+mbc, meqn)
c     dimension     qr(1-mbc:maxm+mbc, meqn)
c     dimension   asdq(1-mbc:maxm+mbc, meqn)
c     dimension bmasdq(1-mbc:maxm+mbc, meqn)
c     dimension bpasdq(1-mbc:maxm+mbc, meqn)
c     dimension   aux1(1-mbc:maxm+mbc, maux, 3)
c     dimension   aux2(1-mbc:maxm+mbc, maux, 3)
c     dimension   aux3(1-mbc:maxm+mbc, maux, 3)
c
c       On input,
c
c          ql,qr is the data along some one-dimensional slice, as in rpn3
c               This slice is
c                   in the x-direction if ixyz=1,
c                   in the y-direction if ixyz=2, or
c                   in the z-direction if ixyz=3.
c          asdq is an array of flux differences (A^* \Delta q).
c               asdq(i,:) is the flux difference propagating away from
c               the interface between cells i-1 and i.
c          imp = 1 if asdq = A^- \Delta q,  the left-going flux difference
c                2 if asdq = A^+ \Delta q, the right-going flux difference
c
c          aux2 is the auxiliary array (if method(7)=maux>0) along
c               the plane where this slice lies, say at j=J if ixyz=1.
c               aux2(:,:,1) contains data along j=J, k=k-1
c               aux2(:,:,2) contains data along j=J, k=k
c               aux2(:,:,3) contains data along j=J, k=k+1
c          aux1 is the auxiliary array along the plane with j=J-1
c          aux3 is the auxiliary array along the plane with j=J+1
c
c            if ixyz=2 then aux2 is in some plane k=K, and
c               aux2(:,:,1)  contains data along i=I-1, k=K, etc.
c
c            if ixyz=3 then aux2 is in some plane i=I, and
c               aux2(:,:,1)  contains data along j=j-1, i=I, etc.
c
c       On output,

c       If data is in x-direction (ixyz=1) then this routine does the
c       splitting of  asdq (= A^* \Delta q, where * = + or -) ...
c
c       into down-going flux difference bmasdq (= B^- A^* \Delta q)
c          and up-going flux difference bpasdq (= B^+ A^* \Delta q)
c          when icoor = 2,
c
c       or
c
c       into down-going flux difference bmasdq (= C^- A^* \Delta q)
c          and up-going flux difference bpasdq (= C^+ A^* \Delta q)
c          when icoor = 3.
c
c
c       More generally, ixyz specifies what direction the slice of data is
c       in, and icoor tells which transverse direction to do the splitting in:
c
c       If ixyz = 1,  data is in x-direction and then
c             icoor = 2  =>  split in the y-direction  (iuvw=2)
c             icoor = 3  =>  split in the z-direction  (iuvw=3)
c
c       If ixyz = 2,  data is in y-direction and then
c             icoor = 2  =>  split in the z-direction  (iuvw=3)
c             icoor = 3  =>  split in the x-direction  (iuvw=1)
c
c       If ixyz = 3,  data is in z-direction and then
c             icoor = 2  =>  split in the x-direction  (iuvw=1)
c             icoor = 3  =>  split in the y-direction  (iuvw=2)
c
c           For example, for a linear system q_t + Aq_x + Bq_y + Cq_z = 0,
c                   asdq = A^+ dq  or  A^- dq
c                   and this is then split into
c                       bmasdq = B^- asdq   and   bpasdq = B^+ asdq
c                   when ixyz = 1, and icoor = 2.
c
c   rptt3 = user-supplied subroutine that implements the splitting of
c           a flux difference bsasdq into waves in the double transverse
c           direction.
c           This subroutine has the form :
c
c     subroutine rptt3(ixyz,icoor,maxm,meqn,mwaves,mbc,mx,
c    &                  ql,qr,aux1,aux2,aux3,maux,imp,impt,bsasdq,
c    &                  cmbsasdq,cpbsasdq)
c     implicit double precision (a-h,o-z)
c     dimension      ql(1-mbc:maxm+mbc, meqn)
c     dimension      qr(1-mbc:maxm+mbc, meqn)
c     dimension   bsasdq(1-mbc:maxm+mbc, meqn)
c     dimension cmbsasdq(1-mbc:maxm+mbc, meqn)
c     dimension cpbsasdq(1-mbc:maxm+mbc, meqn)
c     dimension   aux1(1-mbc:maxm+mbc, maux, 3)
c     dimension   aux2(1-mbc:maxm+mbc, maux, 3)
c     dimension   aux3(1-mbc:maxm+mbc, maux, 3)
c
c       On input,
c
c          ql,qr is the data along some one-dimensional slice, as in rpn3
c               This slice is
c                   in the x-direction if ixyz=1,
c                   in the y-direction if ixyz=2, or
c                   in the z-direction if ixyz=3.
c          bsasdq is an array of flux differences (B^* A^* \Delta q).
c               bsasdq(i,:) is the flux difference propagating into cells
c               who share only a corner with interface between cells i and i-1.
c          imp = 1 if asdq = A^- \Delta q,  the left-going flux difference
c                2 if asdq = A^+ \Delta q, the right-going flux difference
c
c        impt = 1 if bsasdq = B^-A^* \Delta q,
c               2 if bsasdq = B^+A^* \Delta q
c
c          aux2 is the auxiliary array (if method(7)=maux>0) along
c               the plane where this slice lies, say at j=J if ixyz=1.
c               aux2(:,:,1) contains data along j=J, k=k-1
c               aux2(:,:,2) contains data along j=J, k=k
c               aux2(:,:,3) contains data along j=J, k=k+1
c          aux1 is the auxiliary array along the plane with j=J-1
c          aux3 is the auxiliary array along the plane with j=J+1
c
c            if ixyz=2 then aux2 is in some plane k=K, and
c               aux2(:,:,1)  contains data along i=I-1, k=K, etc.
c
c            if ixyz=3 then aux2 is in some plane i=I, and
c               aux2(:,:,1)  contains data along j=j-1, i=I, etc.
c
c       On output,

c       If data is in x-direction (ixyz=1) then this routine does the
c       splitting of  bsasdq (= B^*A^* \Delta q, where * = + or -) ...
c
c       into down-going flux difference cmbsasdq (= C^- B^* A^* \Delta q)
c          and up-going flux difference cpbsasdq (= c^+ B^* A^* \Delta q)
c          when icoor = 2,
c
c       or
c
c       into front-going (in negative y direction) flux difference
c          bmcsasdq (= B^- C^*A^* \Delta q)
c          and back-going (in positive y direction) flux difference
c          bpcsasdq (= B^+ C^* A^* \Delta q) when icoor = 3.
c
c
c       More generally, ixyz specifies what direction the slice of data is
c       in, and icoor tells which transverse direction to do the splitting in:
c
c       If ixyz = 1,  data is in x-direction and then
c             icoor = 2  =>  split in the y-direction  (iuvw=2)
c             icoor = 3  =>  split in the z-direction  (iuvw=3)
c
c       If ixyz = 2,  data is in y-direction and then
c             icoor = 2  =>  split in the z-direction  (iuvw=3)
c             icoor = 3  =>  split in the x-direction  (iuvw=1)
c
c       If ixyz = 3,  data is in z-direction and then
c             icoor = 2  =>  split in the x-direction  (iuvw=1)
c             icoor = 3  =>  split in the y-direction  (iuvw=2)
c
c           For example, for a linear system q_t + Aq_x + Bq_y + Cq_z = 0,
c                   bsasdq = B^+ A^* dq  or  B^- A^* dq
c                   and this is then split into
c                       cmbsasdq = C^- bsasdq   and   cpbsasdq = C^+ bsasdq
c                   when ixyz = 1, and icoor = 3.
c
c           Or, if ixyz = 1, and icoor = 2, then
c                   bsasdq = C^+ A^* dq or C^- A^* dq
c                   and this is then split into
c                       cmbsasdq = B^- bsasdq and cpbsasdq = B^+ bsasdq
c
c
c
c    src3 = user-supplied subroutine that takes one time step on the
c           source terms alone, solving
c               capa * q_t = psi
c           over time dt.
c
c           If method(5)=0 then the equation does not contain a source
c           term and this routine is never called.  A dummy argument can
c           be used with many compilers, or provide a dummy subroutine that
c           does nothing.
c
c           The form of this subroutine is
c
c      subroutine src3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,q,aux,t,dt)
c      implicit double precision (a-h,o-z)
c      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
c     &               1-mbc:maxmz+mbc, meqn)
c      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
c     &              1-mbc:maxxz+mbc, *)
c
c      If method(7)=0  or the auxiliary variables are not needed in
c      this solver,
c      then the latter dimension statement can be omitted, but aux should
c      still appear in the argument list.
c
c      On input, q(i,j,k,m) contains the data for solving the
c                source term equation.
c      On output, q(i,j,k,m) should have been replaced by the solution to
c                 the source term equation after a step of length dt.
c
c      b4step3 = subroutine that is called from claw3 before each call to
c                step3.  Use to set time-dependent aux arrays or perform
c                other tasks which must be done every time step.
c
c          The form of this subroutine is
c
c      subroutine b4step3(maxmx,maxmy,maxmz,mbc,mx,my,mz,meqn,q,
c    &            xlower,ylower,zlower,dx,dy,dz,time,dt,maux,aux)
c      implicit double precision (a-h,o-z)
c      dimension     q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
c     &                1-mbc:maxmz+mbc, meqn)
c      dimension   aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
c     &                1-mbc:maxmz+mbc, maux)
c
c
c  Copyright 1996 -- 2002 R. J. LeVeque and J. O. Langseth
c
c  This software is made available for research and instructional use only.
c  You may copy and use this software without charge for these non-commercial
c  purposes, provided that the copyright notice and associated text is
c  reproduced on all copies.  For all other uses (including distribution of
c  modified versions), please contact the author at the address given below.
c
c  *** This software is made available "as is" without any assurance that it
c  *** will work for your purposes.  The software may in fact have defects, so
c  *** use the software at your own risk.
c
c
c    CLAWPACK Version 4.1,  August, 2002
c    Webpage: http://www.amath.washington.edu/~claw
c
c
c    Authors: Randall J. LeVeque
c             Applied Mathematics
c             Box 352420
c             University of Washington,
c             Seattle, WA 98195-2420
c             rjl@amath.washington.edu
c
c      and
c             Jan Olav Langseth
c             FFI/VM
c             Box 25
c             N-2007 Kjeller
c             Norway
c             jol@ffi.no
c
c     Modified in 2002 by
c             Donna Calhoun
c             Applied Mathematics
c             Box 352420
c             University of Washington,
c             Seattle, WA 98195-2420
c             calhoun@amath.washington.edu
c
c
c    Beginning of claw3 code
c
      implicit double precision (a-h,o-z)

      external bc3,rpn3,rpt3, rptt3
      dimension     q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &                1-mbc:maxmz+mbc, meqn)
      dimension   aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &                1-mbc:maxmz+mbc, *)
      dimension work(mwork)
      dimension mthlim(mwaves),method(7),dtv(5),cflv(4),nv(2)

      common /comxyzt/ dtcom,dxcom,dycom,dzcom,tcom,icom,jcom,kcom

      maxm = max0(maxmx, maxmy, maxmz)
      info = 0
      t = tstart
      maxn = nv(1)
      dt = dtv(1)   !# initial dt
      cflmax = 0.d0
      dtmin = dt
      dtmax = dt
      nv(2) = 0
      maux = method(7)
c
c  check for errors in data:
c
      call chkmth(method,info)
      if( info .eq. 6) go to 900

      if ( (mx.gt.maxmx) .or. (my.gt.maxmy) .or. (mz.gt.maxmz)
     &    .or. (mbc.lt.2) ) then
         info = 1
         go to 900
         end if

      if (method(1) .eq. 0) then
c
c        # fixed size time steps.  Compute the number of steps:
c
         if (tend .lt. tstart) then
c
c             # single step mode
c
              maxn = 1
           else
              maxn = (tend - tstart + 1d-10) / dt
              if (dabs(maxn*dt - (tend-tstart)) .gt.
     &                          1d-5*(tend-tstart)) then
c
c                # dt doesn't divide time interval integer number of times
c
                 info = 2
                 write(6,*) 
     &               'CLAW3 ERROR... dt does not divide (tend-tstart)'
                 go to 900
                 end if
           end if
         end if

      if (method(1).eq.1 .and. cflv(2).gt.cflv(1)) then
         info = 3
         write(6,*) 'CLAW3 ERROR...  cflv(2) > cflv(1)'
         go to 900
         end if

      if (method(6).gt.method(7)) then
         info = 5
         write(6,*) 'CLAW3 ERROR...  method(6) > method(7)'
         go to 900
         end if

      if (method(5).lt.2) then
          narray = 1   !# no source terms -- only need one qwork array
        else
          narray = 2           !# need two qwork arrays
        end if

      mwork0 = (maxm+2*mbc)*(46*meqn + mwaves + meqn*mwaves
     &                      + 9*maux + 3)
     &          + narray * (maxmx + 2*mbc) * (maxmy + 2*mbc)
     &                   * (maxmz + 2*mbc) * meqn

      if (mwork .lt. mwork0) then
         info = 4
         write(6,*) 'CLAW3 ERROR... mwork should be increased to ',
     &               mwork0
         go to 900
         end if
c
c     # partition work array into pieces needed for local storage in
c     # step2 routine. Find starting index of each piece:
c
      i0qadd = 1
      i0fadd = i0qadd + (maxm+2*mbc)*meqn
      i0gadd = i0fadd + (maxm+2*mbc)*meqn
      i0hadd = i0gadd + 6*(maxm+2*mbc)*meqn
      i0q1d = i0hadd + 6*(maxm+2*mbc)*meqn
      i0dtdx1d = i0q1d + (maxm+2*mbc)*meqn
      i0dtdy1d = i0dtdx1d + (maxm+2*mbc)
      i0dtdz1d = i0dtdy1d + (maxm+2*mbc)
      i0qwrk1 = i0dtdz1d + (maxm+2*mbc)

      nqwork = (maxmx + 2*mbc) * (maxmy + 2*mbc)
     &       * (maxmz + 2*mbc) * meqn  !# size of q array
      if (method(5).lt.2) then
          i0qwrk2 = i0qwrk1  !# qwrk2 points to same storage as qwrk1
        else
          i0qwrk2 = i0qwrk1 + nqwork  !# second qwork array with source term
        end if

      i0aux1 = i0qwrk2 + nqwork
      i0aux2 = i0aux1 + 3*(maxm+2*mbc)*maux
      i0aux3 = i0aux2 + 3*(maxm+2*mbc)*maux

      i0next = i0aux3 + 3*(maxm+2*mbc)*maux  !# next free space
      mused = i0next - 1                     !# space already used
      mwork1 = mwork - mused              !# remaining space (passed to step2)
c
c     # main loop
c
      if (maxn.eq.0) go to 900
      do 100 n=1,maxn
         told = t   !# time at beginning of time step.

c        # adjust dt to hit tend exactly if we're near end of computation
c        #  (unless tend < tstart, which is a flag to take only a single step)
         if (told+dt.gt.tend .and. tstart.lt.tend) dt = tend - told


   40    continue
c
c        # store dt and t in the common block comxyt in case they are needed
c        # in the Riemann solvers (for variable coefficients)
c
         tcom = told
         dtcom = dt
         dxcom = dx
         dycom = dy
         dzcom = dz
c
c        # main steps in algorithm
c
c        # extend data from grid to bordering boundary cells:
c
         call bc3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,xlower,
     &               ylower,zlower,dx,dy,dz,q,maux,aux,told,dt,mthbc)
c
c        # call user-supplied routine which might set aux arrays
c        # for this time step, for example.
c
         call b4step3(maxmx,maxmy,maxmz,mbc,mx,my,mz,meqn,q,
     &            xlower,ylower,zlower,dx,dy,dz,told,dt,maux,aux)
c
c
         if (method(5).eq.2) then
c
c            # with source term:   use Strang splitting
c            # First need to store solution before taking
c            # step on source terms in case we need to redo everything with
c            # a smaller time step if the Courant number is too large in
c            # subroutine step3.
c
              call copyq3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,q,
     &                   work(i0qwrk2))
c
c            # source terms over a half time step:
c
             dt2 = dt / 2.d0
             thalf = told + dt2 !# midpoint in time.
             call src3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,
     &                 xlower,ylower,zlower,dx,dy,dz,
     &                 q,maux,aux,told,dt2)
             end if
c
c        # copy q into qwork1.  q is updated in step2 and qwork1 is
c        # preserved to provide data for Riemann problems.
c
         call copyq3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,q,
     &               work(i0qwrk1))
c
c        # take one step on the conservation law:
c
         if(method(3) .ge. 0)then
c
c           # unsplit version
c
            call step3(maxm,maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,
     &                 mz,work(i0qwrk1),q,aux,dx,dy,dz,dt,method,
     &                 mthlim,cfl,
     &                 work(i0qadd),work(i0fadd),work(i0gadd),
     &                 work(i0hadd),work(i0q1d),work(i0dtdx1d),
     &                 work(i0dtdy1d),work(i0dtdz1d),
     &                 work(i0aux1),work(i0aux2),work(i0aux3),maux,
     &                 work(i0next),mwork1,rpn3,rpt3, rptt3)

         else
c
c           # dimensional splitting (fractional steps)
c
            call dimsp3(maxm,maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,
     &                  mz,work(i0qwrk1),q,aux,dx,dy,dz,dt,method,
     &                  mthlim,cfl,cflv,
     &                  work(i0qadd),work(i0fadd),work(i0gadd),
     &                  work(i0hadd),work(i0q1d),work(i0dtdx1d),
     &                  work(i0dtdy1d),work(i0dtdz1d),
     &                  work(i0aux1),work(i0aux2),work(i0aux3),maux,
     &                  work(i0next),mwork1,rpn3,rpt3,rptt3)
c
         end if
c
         t = told + dt
c
         if (method(4).eq.1) then
c            # verbose mode
             write(6,601) n,cfl,dt,t
  601        format('CLAW3... Step',i4,
     &                   '   Courant number =',f6.3,'  dt =',d12.4,
     &                   '  t =',d12.4)
             end if
c
c
c        # check to see if the Courant number was too large:
c
         if (cfl .le. cflv(1)) then
c
c             # accept this step
c
              cflmax = dmax1(cfl,cflmax)
            else
c
c             # Reject this step.  Reset q to qwork from previous time:
c             # Note that i0qwrk2 points to work space where previous
c             # solution is stored in both cases method(5) = 0 or 1.
c
              t = told
              call copyq3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,
     &                    work(i0qwrk2),q)

              if (method(4).eq.1) then
c
c                # verbose mode
c
                 write(6,602)
  602            format('CLAW3 rejecting step... ',
     &                  'Courant number too large')
                 end if
              if (method(1).eq.1) then
c
c                 # if variable dt, go back and take a smaller step.
c
                  dt = dmin1(dtv(2), dt * cflv(2)/cfl)
                  go to 40
                else
c                 # if fixed dt, give up and return
                  cflmax = dmax1(cfl,cflmax)
                  go to 900
                end if
            end if
c
c        # claw3 step is accepted
c        # now apply source terms:
c
         if (method(5).eq.2) then
c
c            # source terms over a second half time step for Strang splitting:
c            # Note it is not so clear what time t should be used here if
c            # the source terms are time-dependent!
c
             call src3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,
     &                 xlower,ylower,zlower,dx,dy,dz,
     &                 q,maux,aux,t,dt2)
             end if

         if (method(5).eq.1) then
c
c            # source terms over a full time step:
c
             call src3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,
     &                 xlower,ylower,zlower,dx,dy,dz,
     &                 q,maux,aux,t,dt)
             end if

         if (method(1) .eq. 1) then
c
c           # choose new time step if variable time step
c
            if (cfl.eq.0.d0) then
                dt = dtv(2)
              else
                dt = dmin1(dtv(2), dt * cflv(2)/cfl)
              end if
            dtmin = dmin1(dt,dtmin)
            dtmax = dmax1(dt,dtmax)
            end if
c
c        # see if we are done:
c
         nv(2) = nv(2) + 1
         if (t .ge. tend) go to 900
  100    continue

  900  continue
c
c      # return information
c
       if (method(1).eq.1 .and. t.lt.tend .and. nv(2) .eq. maxn) then
c
c         # too many timesteps
c
          write(6,*) 'CLAW3 ERROR...  too many timesteps'
          info = 11
          end if
       if (method(1).eq.0 .and. cflmax .gt. cflv(1)) then
c
c         # Courant number too large with fixed dt
c
          write(6,*) 'CLAW3 ERROR...  Courant number too large'
          info = 12
          end if
       tend = t
       cflv(3) = cflmax
       cflv(4) = cfl
       dtv(3) = dtmin
       dtv(4) = dtmax
       dtv(5) = dt

       return
       end
      subroutine copyq1(maxmx,meqn,mbc,mx,q1,q2)

c*********************************************************************72
c
cc COPYQ1 copies the contents of q1 into q2
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q1(1-mbc:maxmx+mbc, meqn)
      dimension q2(1-mbc:maxmx+mbc, meqn)

      do i = 1-mbc, mx+mbc
        do m=1,meqn
          q2(i,m) = q1(i,m)
        end do
      end do

      return
      end
      subroutine copyq2(maxmx,maxmy,meqn,mbc,mx,my,q1,q2)

c*********************************************************************72
c
cc COPYQ2 copies the contents of q1 into q2
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q1(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension q2(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)

      do j = 1-mbc, my+mbc
        do i = 1-mbc, mx+mbc
          do m=1,meqn
             q2(i,j,m) = q1(i,j,m)
          end do
        end do
      end do

      return
      end
      subroutine copyq3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,q1,q2)

c*********************************************************************72
c
cc COPYQ3 copies the contents of q1 into q2
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q1(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 
     &             1-mbc:maxmz+mbc,meqn)
      dimension q2(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 
     &             1-mbc:maxmz+mbc,meqn)

      do 10 m=1,meqn
         do 10 k = 1-mbc, mz+mbc
            do 10 j = 1-mbc, my+mbc
               do 10 i = 1-mbc, mx+mbc
                  q2(i,j,k,m) = q1(i,j,k,m)
   10       continue

      return
      end
      subroutine dimsp2(maxm,maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &                  qold,qnew,aux,dx,dy,dt,method,mthlim,cfl,
     &                  cflv,qadd,fadd,gadd,q1d,dtdx1d,dtdy1d,
     &                  aux1,aux2,aux3,work,mwork,rpn2,rpt2)

c*********************************************************************72
c
cc DIMSP2 takes a time step, updating Q, using dimensional splitting.
c
c  Discussion:
c
c     # Take one time step, updating q, using dimensional
c     # splitting. Two choices are available:
c     #
c     # method(3) = -1   gives Godunov splitting:
c     #    time step dt in x-direction
c     #    time step dt in y-direction
c
c     # method(3) = -2   gives Strang splitting
c     #    time step dt/2 in x-direction
c     #    time step dt   in y-direction
c     #    time step dt/2 in x-direction
c
c     # Godunov splitting is recommended over Strang splitting normally
c     # since it typically works as well, is faster, and boundary
c     # conditions are handled properly.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn2,rpt2
      dimension qold(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension qnew(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension  q1d(1-mbc:maxm+mbc, meqn)
      dimension cflv(4)
      dimension qadd(1-mbc:maxm+mbc, meqn)
      dimension fadd(1-mbc:maxm+mbc, meqn)
      dimension gadd(1-mbc:maxm+mbc, meqn, 2)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, *)
      dimension aux1(1-mbc:maxm+mbc, *)
      dimension aux2(1-mbc:maxm+mbc, *)
      dimension aux3(1-mbc:maxm+mbc, *)

      dimension dtdx1d(1-mbc:maxmx+mbc)
      dimension dtdy1d(1-mbc:maxmx+mbc)
      dimension method(7),mthlim(mwaves)
      dimension work(mwork)

c
c     # If method(3) = -1, take a full time step in x.
c     # If method(3) = -2, take a half time step in x.
c
      dt2 = dt/2.d0
c
      if( method(3) .eq. -2 )then
          call step2ds(maxm,maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &                 qold,qnew,aux,dx,dy,dt2,method,mthlim,cflx,
     &                 qadd,fadd,gadd,q1d,dtdx1d,dtdy1d,
     &                 aux1,aux2,aux3,work,mwork,rpn2,rpt2,1)
      else
          call step2ds(maxm,maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &                 qold,qnew,aux,dx,dy,dt,method,mthlim,cflx,
     &                 qadd,fadd,gadd,q1d,dtdx1d,dtdy1d,
     &                 aux1,aux2,aux3,work,mwork,rpn2,rpt2,1)
      end if

      if (cflx .gt. cflv(1)) then
c        # Abort if the Courant number was too large in x-sweep
         cfl = cflx
         return
         end if
c
c     # Take full step in y-direction
c
      call step2ds(maxm,maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &             qnew,qnew,aux,dx,dy,dt,method,mthlim,cfly,
     &             qadd,fadd,gadd,q1d,dtdx1d,dtdy1d,
     &             aux1,aux2,aux3,work,mwork,rpn2,rpt2,2)
c
      cfl = dmax1(cflx,cfly)
c
c     # Finally, take a half time step in the x-direction
c     # if Strang splitting is used.  NOTE: boundary conditions may
c     # not be set properly for this sweep.
c
      if( method(3) .eq. -2 )then
         if (cfly .gt. cflv(1)) then
c           # Abort if the Courant number was too large in y-sweep
            cfl = cfly
            return
            end if
          call step2ds(maxm,maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &                 qnew,qnew,aux,dx,dy,dt2,method,mthlim,cflx,
     &                 qadd,fadd,gadd,q1d,dtdx1d,dtdy1d,
     &                 aux1,aux2,aux3,work,mwork,rpn2,rpt2,1)
          cfl = dmax1(cfl,cflx)
      end if
c
      return
      end
      subroutine dimsp3(maxm,maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,
     &                  mz,qold,qnew,aux,dx,dy,dz,dt,method,mthlim,cfl,
     &                  cflv,qadd,fadd,gadd,hadd,q1d,dtdx1d,dtdy1d,
     &                  dtdz1d,aux1,aux2,aux3,maux,work,mwork,
     &                  rpn3,rpt3,rptt3)

c*********************************************************************72
c
cc DISMP3 takes a time step, updating Q, using dimensional splitting.
c
c  Discussion:
c
c     # Take one time step, updating q, using dimensional
c     # splitting. Only first order Godunov splitting is
c     # included. The main reason why not also Strang splitting 
c     # is included, is that it is not clear how to include
c     # boundary conditions in this case.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn3,rpt3,rptt3
      dimension qold(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 
     &          1-mbc:maxmz+mbc, meqn)
      dimension qnew(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 
     &          1-mbc:maxmz+mbc, meqn)
      dimension  q1d(1-mbc:maxm+mbc, meqn)
      dimension cflv(4)
      dimension qadd(1-mbc:maxm+mbc, meqn)
      dimension fadd(1-mbc:maxm+mbc, meqn)
      dimension gadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
      dimension hadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 
     &              1-mbc:maxmz+mbc, *)
      dimension aux1(1-mbc:maxm+mbc, maux, 3)
      dimension aux2(1-mbc:maxm+mbc, maux, 3)
      dimension aux3(1-mbc:maxm+mbc, maux, 3)
      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension dtdy1d(1-mbc:maxm+mbc)
      dimension dtdz1d(1-mbc:maxm+mbc)
      dimension method(7),mthlim(mwaves)
      dimension work(mwork)
c
c     # Take one full time step in the x-direction
c
      idir = 1
      call step3ds(maxm,maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,
     &             mz,qold,qnew,aux,dx,dy,dz,dt,method,mthlim,cflx,
     &             qadd,fadd,gadd,hadd,q1d,dtdx1d,dtdy1d,dtdz1d,
     &             aux1,aux2,aux3,maux,work,mwork,
     &             rpn3,rpt3,rptt3,idir)
c
      if (cflx .gt. cflv(1)) then
c        # Abort if the Courant number was too large in x-sweep
         cfl = cflx
         return
         end if
c
c     # Take one full time step in the y-direction
c
      idir = 2
c
      call step3ds(maxm,maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,
     &             mz,qnew,qnew,aux,dx,dy,dz,dt,method,mthlim,cfly,
     &             qadd,fadd,gadd,hadd,q1d,dtdx1d,dtdy1d,dtdz1d,
     &             aux1,aux2,aux3,maux,work,mwork,
     &             rpn3,rpt3,rptt3,idir)
c
      if (cfly .gt. cflv(1)) then
c        # Abort if the Courant number was too large in y-sweep
         cfl = cfly
         return
         end if
c
c     # Take one full time step in the z-direction
c
      idir = 3
      call step3ds(maxm,maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,
     &             mz,qnew,qnew,aux,dx,dy,dz,dt,method,mthlim,cflz,
     &             qadd,fadd,gadd,hadd,q1d,dtdx1d,dtdy1d,dtdz1d,
     &             aux1,aux2,aux3,maux,work,mwork,
     &             rpn3,rpt3,rptt3,idir)
c
      cfl = dmax1(cflx,cfly,cflz)
c
      return
      end
      subroutine dump2(maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &           q,t,dx,dy,dtv,cflv,nv,method,mthlim,info)

c*********************************************************************72
c
cc DUMP2 dumps the data needed for a possible restart
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension dtv(5),cflv(4),nv(2),method(7),mthlim(mwaves)

      write(99,*) mx,my
      write(99,*) dx,dy
      write(99,*) t
      write(99,*) dtv
      write(99,*) cflv
      write(99,*) nv
      write(99,*) method
      write(99,*) mthlim
      write(99,*) info

      do 12 m=1,meqn
         do 11 j=1,my
            do 10 i=1,mx
               write(99,*) q(i,j,m)
   10       continue
   11    continue
   12 continue
      return
      end
      subroutine flux2(ixy,maxm,meqn,mwaves,mbc,mx,
     &                 q1d,dtdx1d,aux1,aux2,aux3,method,mthlim,
     &               qadd,fadd,gadd,cfl1d,wave,s,
     &                 amdq,apdq,cqxx,bmasdq,bpasdq,rpn2,rpt2)

c*********************************************************************72
c
cc FLUX2 computes modified fluxes along a 1D slice of the 2D grid.
c
c  Discussion:
c
c     # Compute the modification to fluxes f and g that are generated by
c     # all interfaces along a 1D slice of the 2D grid. 
c     #    ixy = 1  if it is a slice in x
c     #          2  if it is a slice in y
c     # This value is passed into the Riemann solvers. The flux modifications
c     # go into the arrays fadd and gadd.  The notation is written assuming
c     # we are solving along a 1D slice in the x-direction.
c
c     # fadd(i,.) modifies F to the left of cell i
c     # gadd(i,.,1) modifies G below cell i
c     # gadd(i,.,2) modifies G above cell i
c
c     # The method used is specified by method(2:3):
c
c         method(2) = 1 if only first order increment waves are to be used.
c                   = 2 if second order correction terms are to be added, with
c                       a flux limiter as specified by mthlim.  
c
c         method(3) = 0 if no transverse propagation is to be applied.
c                       Increment and perhaps correction waves are propagated
c                       normal to the interface.
c                   = 1 if transverse propagation of increment waves 
c                       (but not correction waves, if any) is to be applied.
c                   = 2 if transverse propagation of correction waves is also
c                       to be included.  
c
c     Note that if method(6)=1 then the capa array comes into the second 
c     order correction terms, and is already included in dtdx1d:
c     If ixy = 1 then
c        dtdx1d(i) = dt/dx                 if method(6) = 0
c                  = dt/(dx*capa(i,jcom))  if method(6) = 1
c     If ixy = 2 then
c        dtdx1d(j) = dt/dy                 if method(6) = 0
c                  = dt/(dy*capa(icom,j))  if method(6) = 1
c
c     Notation:
c        The jump in q (q1d(i,:)-q1d(i-1,:))  is split by rpn2 into
c            amdq =  the left-going flux difference  A^- Delta q  
c            apdq = the right-going flux difference  A^+ Delta q  
c        Each of these is split by rpt2 into 
c            bmasdq = the down-going transverse flux difference B^- A^* Delta q
c            bpasdq =   the up-going transverse flux difference B^+ A^* Delta q
c        where A^* represents either A^- or A^+.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn2,rpt2
      dimension    q1d(1-mbc:maxm+mbc, meqn)
      dimension   amdq(1-mbc:maxm+mbc, meqn)
      dimension   apdq(1-mbc:maxm+mbc, meqn)
      dimension bmasdq(1-mbc:maxm+mbc, meqn)
      dimension bpasdq(1-mbc:maxm+mbc, meqn)
      dimension   cqxx(1-mbc:maxm+mbc, meqn)
      dimension   qadd(1-mbc:maxm+mbc, meqn)
      dimension   fadd(1-mbc:maxm+mbc, meqn)
      dimension   gadd(1-mbc:maxm+mbc, meqn, 2)
c
      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension aux1(1-mbc:maxm+mbc, *)
      dimension aux2(1-mbc:maxm+mbc, *)
      dimension aux3(1-mbc:maxm+mbc, *)
c
      dimension     s(1-mbc:maxm+mbc, mwaves)
      dimension  wave(1-mbc:maxm+mbc, meqn, mwaves)
c
      dimension method(7),mthlim(mwaves)
      logical limit
      common /comxyt/ dtcom,dxcom,dycom,tcom,icom,jcom
c
      limit = .false.
      do 5 mw=1,mwaves
         if (mthlim(mw) .gt. 0) limit = .true.
   5  continue
c
c     # initialize flux increments:
c
         do 20 m=1,meqn
            do 10 i = 1-mbc, mx+mbc
               qadd(i,m) = 0.d0
               fadd(i,m) = 0.d0
               gadd(i,m,1) = 0.d0
               gadd(i,m,2) = 0.d0
   10       continue
   20    continue
c
c     # initialize apdq/amdq and transverse corrections
c
         do m=1,meqn
            do i = 1-mbc, mx+mbc
               amdq(i,m) = 0.d0
               apdq(i,m) = 0.d0

               bmasdq(i,m) = 0.d0
               bpasdq(i,m) = 0.d0               

               cqxx(i,m) = 0.d0
            end do
         end do
c
c
c     # solve Riemann problem at each interface and compute Godunov updates
c
c
      call rpn2(ixy,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux2,aux2,
     &        wave,s,amdq,apdq)
c
c     # Set qadd for the donor-cell upwind method (Godunov)
      do 41 m=1,meqn
         do 40 i=1,mx+1
            qadd(i,m) = qadd(i,m) - dtdx1d(i)*apdq(i,m)
            qadd(i-1,m) = qadd(i-1,m) - dtdx1d(i-1)*amdq(i,m)
   40    continue
   41 continue
c
c     # compute maximum wave speed for checking Courant number:
      cfl1d = 0.d0
      do 51 mw=1,mwaves
         do 50 i=1,mx+1
c          # if s>0 use dtdx1d(i) to compute CFL,
c          # if s<0 use dtdx1d(i-1) to compute CFL:
            cfl1d = dmax1(cfl1d, dtdx1d(i)*s(i,mw), 
     &                          -dtdx1d(i-1)*s(i,mw))
   50    continue
   51 continue
c
      if (method(2).eq.1) go to 130
c
c     # modify F fluxes for second order q_{xx} correction terms:
c
c
c     # apply limiter to waves:
      if (limit) call limiter(maxm,meqn,mwaves,mbc,mx,wave,s,mthlim)
c
      do 121 m=1,meqn
         do 120 i = 2-mbc, mx+mbc
c
c        # For correction terms below, need average of dtdx in cell
c        # i-1 and i.  Compute these and overwrite dtdx1d:
c
            dtdx1d(i-1) = 0.5d0 * (dtdx1d(i-1) + dtdx1d(i))
c
            cqxx(i,m) = 0.d0
            do 119 mw=1,mwaves
c
c              # second order corrections:
               cqxx(i,m) = cqxx(i,m) + dabs(s(i,mw))
     &            * (1.d0 - dabs(s(i,mw))*dtdx1d(i-1)) * wave(i,m,mw)
c
  119       continue
            fadd(i,m) = fadd(i,m) + 0.5d0 * cqxx(i,m)
  120    continue
  121 continue
c
c
  130  continue
c
      if (method(3).le.0) go to 999   !# no transverse propagation
c
      if (method(2).gt.1 .and. method(3).eq.2) then
c        # incorporate cqxx into amdq and apdq so that it is split also.
         do 151 m=1,meqn
            do 150 i = 1, mx+1
               amdq(i,m) = amdq(i,m) + cqxx(i,m)
               apdq(i,m) = apdq(i,m) - cqxx(i,m)
  150       continue
  151    continue
      end if
c
c
c      # modify G fluxes for transverse propagation
c
c     # split the left-going flux difference into down-going and up-going:
      call rpt2(ixy,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,aux3,
     &        1,amdq,bmasdq,bpasdq)
c
c     # modify flux below and above by B^- A^- Delta q and  B^+ A^- Delta q:
      do 161 m=1,meqn
         do 160 i = 1, mx+1
            gadd(i-1,m,1) = gadd(i-1,m,1) - 
     &                 0.5d0*dtdx1d(i-1) * bmasdq(i,m)
            gadd(i-1,m,2) = gadd(i-1,m,2) -
     &                 0.5d0*dtdx1d(i-1) * bpasdq(i,m)
  160    continue
  161 continue
c
c     # split the right-going flux difference into down-going and up-going:
      call rpt2(ixy,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,aux3,
     &        2,apdq,bmasdq,bpasdq)
c
c     # modify flux below and above by B^- A^+ Delta q and  B^+ A^+ Delta q:
      do 181 m=1,meqn
         do 180 i = 1, mx+1
            gadd(i,m,1) = gadd(i,m,1) - 
     &                0.5d0*dtdx1d(i-1) * bmasdq(i,m)
            gadd(i,m,2) = gadd(i,m,2) - 
     &                0.5d0*dtdx1d(i-1) * bpasdq(i,m)
  180    continue
  181 continue
c
  999 continue
      return
      end
      subroutine flux2fw(ixy,maxm,meqn,mwaves,mbc,mx,
     &                 q1d,dtdx1d,aux1,aux2,aux3,method,mthlim,
     &               qadd,fadd,gadd,cfl1d,fwave,s,
     &                 amdq,apdq,cqxx,bmasdq,bpasdq,rpn2,rpt2)

c*********************************************************************72
c
cc FLUX2FW is a modified version of FLUX2.
c
c  Discussion:
c
c     # flux2fw is a modified version of flux2 to use fwave instead of wave.
c     # A modified Riemann solver rp2n must be used in conjunction with this
c     # routine, which returns fwave's instead of wave's.
c     # See http://amath.washington.edu/~claw/fwave.html
c
c     # Limiters are applied to the fwave's, and the only significant
c     # modification of this code is in the "do 119" loop, for the
c     # second order corrections.
c
c     # Compute the modification to fluxes f and g that are generated by
c     # all interfaces along a 1D slice of the 2D grid. 
c     #    ixy = 1  if it is a slice in x
c     #          2  if it is a slice in y
c     # This value is passed into the Riemann solvers. The flux modifications
c     # go into the arrays fadd and gadd.  The notation is written assuming
c     # we are solving along a 1D slice in the x-direction.
c
c     # fadd(i,.) modifies F to the left of cell i
c     # gadd(i,.,1) modifies G below cell i
c     # gadd(i,.,2) modifies G above cell i
c
c     # The method used is specified by method(2:3):
c
c         method(2) = 1 if only first order increment waves are to be used.
c                   = 2 if second order correction terms are to be added, with
c                       a flux limiter as specified by mthlim.  
c
c         method(3) = 0 if no transverse propagation is to be applied.
c                       Increment and perhaps correction waves are propagated
c                       normal to the interface.
c                   = 1 if transverse propagation of increment waves 
c                       (but not correction waves, if any) is to be applied.
c                   = 2 if transverse propagation of correction waves is also
c                       to be included.  
c
c     Note that if method(6)=1 then the capa array comes into the second 
c     order correction terms, and is already included in dtdx1d:
c     If ixy = 1 then
c        dtdx1d(i) = dt/dx                 if method(6) = 0
c                  = dt/(dx*capa(i,jcom))  if method(6) = 1
c     If ixy = 2 then
c        dtdx1d(j) = dt/dy                 if method(6) = 0
c                  = dt/(dy*capa(icom,j))  if method(6) = 1
c
c     Notation:
c        The jump in q (q1d(i,:)-q1d(i-1,:))  is split by rpn2 into
c            amdq =  the left-going flux difference  A^- Delta q  
c            apdq = the right-going flux difference  A^+ Delta q  
c        Each of these is split by rpt2 into 
c            bmasdq = the down-going transverse flux difference B^- A^* Delta q
c            bpasdq =   the up-going transverse flux difference B^+ A^* Delta q
c        where A^* represents either A^- or A^+.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn2,rpt2
      dimension    q1d(1-mbc:maxm+mbc, meqn)
      dimension   amdq(1-mbc:maxm+mbc, meqn)
      dimension   apdq(1-mbc:maxm+mbc, meqn)
      dimension bmasdq(1-mbc:maxm+mbc, meqn)
      dimension bpasdq(1-mbc:maxm+mbc, meqn)
      dimension   cqxx(1-mbc:maxm+mbc, meqn)
      dimension   qadd(1-mbc:maxm+mbc, meqn)
      dimension   fadd(1-mbc:maxm+mbc, meqn)
      dimension   gadd(1-mbc:maxm+mbc, meqn, 2)
c
      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension aux1(1-mbc:maxm+mbc, *)
      dimension aux2(1-mbc:maxm+mbc, *)
      dimension aux3(1-mbc:maxm+mbc, *)
c
      dimension     s(1-mbc:maxm+mbc, mwaves)
      dimension  fwave(1-mbc:maxm+mbc, meqn, mwaves)
c
      dimension method(7),mthlim(mwaves)
      logical limit
      common /comxyt/ dtcom,dxcom,dycom,tcom,icom,jcom
c
      limit = .false.
      do 5 mw=1,mwaves
         if (mthlim(mw) .gt. 0) limit = .true.
   5  continue
c
c     # initialize flux increments:
c
c
         do 20 m=1,meqn
            do 10 i = 1-mbc, mx+mbc
               qadd(i,m) = 0.d0
               fadd(i,m) = 0.d0
               gadd(i,m,1) = 0.d0
               gadd(i,m,2) = 0.d0
   10       continue
   20    continue
c
c
c     # solve Riemann problem at each interface and compute Godunov updates
c
      call rpn2(ixy,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux2,aux2,
     &        fwave,s,amdq,apdq)
c
c     # Set qadd for the donor-cell upwind method (Godunov)
      do 41 m=1,meqn
         do 40 i=1,mx+1
            qadd(i,m) = qadd(i,m) - dtdx1d(i)*apdq(i,m)
            qadd(i-1,m) = qadd(i-1,m) - dtdx1d(i-1)*amdq(i,m)
   40    continue
   41 continue
c
c     # compute maximum wave speed for checking Courant number:
      cfl1d = 0.d0
      do 51 mw=1,mwaves
         do 50 i=1,mx+1
c          # if s>0 use dtdx1d(i) to compute CFL,
c          # if s<0 use dtdx1d(i-1) to compute CFL:
            cfl1d = dmax1(cfl1d, dtdx1d(i)*s(i,mw), 
     &                          -dtdx1d(i-1)*s(i,mw))
   50    continue
   51 continue
c
      if (method(2).eq.1) go to 130
c
c     # modify F fluxes for second order q_{xx} correction terms:
c
c
c     # apply limiter to waves:
      if (limit) call limiter(maxm,meqn,mwaves,mbc,mx,fwave,s,mthlim)
c
      do 121 m=1,meqn
         do 120 i = 1, mx+1
c
c        # For correction terms below, need average of dtdx in cell
c        # i-1 and i.  Compute these and overwrite dtdx1d:
c
            dtdx1d(i-1) = 0.5d0 * (dtdx1d(i-1) + dtdx1d(i))
c
            cqxx(i,m) = 0.d0
            do 119 mw=1,mwaves
c
c              # second order corrections:
               cqxx(i,m) = cqxx(i,m) + dsign(1.d0,s(i,mw))
     &            * (1.d0 - dabs(s(i,mw))*dtdx1d(i-1)) * fwave(i,m,mw)
c
  119       continue
            fadd(i,m) = fadd(i,m) + 0.5d0 * cqxx(i,m)
  120    continue
  121 continue
c
c
  130  continue
c
      if (method(3).le.0) go to 999   !# no transverse propagation
c
      if (method(2).gt.1 .and. method(3).eq.2) then
c        # incorporate cqxx into amdq and apdq so that it is split also.
         do 151 m=1,meqn
            do 150 i = 1, mx+1
               amdq(i,m) = amdq(i,m) + cqxx(i,m)
               apdq(i,m) = apdq(i,m) - cqxx(i,m)
  150       continue
  151    continue
      end if
c
c
c      # modify G fluxes for transverse propagation
c
c
c     # split the left-going flux difference into down-going and up-going:
      call rpt2(ixy,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,aux3,
     &        1,amdq,bmasdq,bpasdq)
c
c     # modify flux below and above by B^- A^- Delta q and  B^+ A^- Delta q:
      do 161 m=1,meqn
         do 160 i = 1, mx+1
            gadd(i-1,m,1) = gadd(i-1,m,1) - 
     &                 0.5d0*dtdx1d(i-1) * bmasdq(i,m)
            gadd(i-1,m,2) = gadd(i-1,m,2) -
     &                 0.5d0*dtdx1d(i-1) * bpasdq(i,m)
  160    continue
  161 continue
c
c     # split the right-going flux difference into down-going and up-going:
      call rpt2(ixy,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,aux3,
     &        2,apdq,bmasdq,bpasdq)
c
c     # modify flux below and above by B^- A^+ Delta q and  B^+ A^+ Delta q:
      do 181 m=1,meqn
         do 180 i = 1, mx+1
            gadd(i,m,1) = gadd(i,m,1) - 
     &                0.5d0*dtdx1d(i-1) * bmasdq(i,m)
            gadd(i,m,2) = gadd(i,m,2) - 
     &                0.5d0*dtdx1d(i-1) * bpasdq(i,m)
  180    continue
  181 continue
c
  999 continue
      return
      end
      subroutine flux3_extra (ixyz,maxm,meqn,mwaves,mbc,mx,
     &                 q1d,dtdx1d,dtdy,dtdz,aux1,aux2,aux3,maux,
     &                 method,mthlim,qadd,fadd,gadd,hadd,cfl1d,
     &                 wave,s,amdq,apdq,cqxx,
     &                 bmamdq,bmapdq,bpamdq,bpapdq,
     &                 cmamdq,cmapdq,cpamdq,cpapdq,
     &                 cmamdq2,cmapdq2,cpamdq2,cpapdq2,
     &                 bmcqxxp,bpcqxxp,bmcqxxm,bpcqxxm,
     &                 cmcqxxp,cpcqxxp,cmcqxxm,cpcqxxm,
     &                 bmcmamdq,bmcmapdq,bpcmamdq,bpcmapdq,
     &                 bmcpamdq,bmcpapdq,bpcpamdq,bpcpapdq,
     &                 rpn3,rpt3,rptt3)

c*********************************************************************72
c
cc FLUX3_EXTRA is another version of FLUX3.
c
c  Discussion:
c
c     # Compute the modification to fluxes f, g and h that are generated by
c     # all interfaces along a 1D slice of the 3D grid.
c     #    ixyz = 1  if it is a slice in x
c     #           2  if it is a slice in y
c     #           3  if it is a slice in z
c     # This value is passed into the Riemann solvers. The flux modifications
c     # go into the arrays fadd, gadd and hadd.  The notation is written
c     # assuming we are solving along a 1D slice in the x-direction.
c
c     # fadd(i,.) modifies F to the left of cell i
c     # gadd(i,.,1,slice) modifies G below cell i (in the z-direction)
c     # gadd(i,.,2,slice) modifies G above cell i
c     #                   The G flux in the surrounding slices may
c     #                   also be updated.
c     #                   slice  =  -1     The slice below in y-direction
c     #                   slice  =   0     The slice used in the 2D method
c     #                   slice  =   1     The slice above in y-direction
c     # hadd(i,.,1,slice) modifies H below cell i (in the y-direction)
c     # hadd(i,.,2,slice) modifies H above cell i
c     #                   The H flux in the surrounding slices may
c     #                   also be updated.
c     #                   slice  =  -1     The slice below in z-direction
c     #                   slice  =   0     The slice used in the 2D method
c     #                   slice  =   1     The slice above in z-direction
c     #
c     # The method used is specified by method(2) and method(3):
c
c        method(2) = 1 No correction waves
c                  = 2 if second order correction terms are to be added, with
c                      a flux limiter as specified by mthlim.  No transverse
c                      propagation of these waves.
c
c         method(3) specify how the transverse wave propagation
c         of the increment wave and the correction wave are performed.
c         Note that method(3) is given by a two digit number, in
c         contrast to what is the case for claw2. It is convenient
c         to define the scheme using the pair (method(2),method(3)).
c
c         method(3) <  0 Gives dimensional splitting using Godunov
c                        splitting, i.e. formally first order
c                        accurate.
c                      0 Gives the Donor cell method. No transverse
c                        propagation of neither the increment wave
c                        nor the correction wave.
c                   = 10 Transverse propagation of the increment wave
c                        as in 2D. Note that method (2,10) is
c                        unconditionally unstable.
c                   = 11 Corner transport upwind of the increment
c                        wave. Note that method (2,11) also is
c                        unconditionally unstable.
c                   = 20 Both the increment wave and the correction
c                        wave propagate as in the 2D case. Only to
c                        be used with method(2) = 2.
c                   = 21 Corner transport upwind of the increment wave,
c                        and the correction wave propagates as in 2D.
c                        Only to be used with method(2) = 2.
c                   = 22 3D propagation of both the increment wave and
c                        the correction wave. Only to be used with
c                        method(2) = 2.
c
c         Recommended settings:   First order schemes:
c                                       (1,10) Stable for CFL < 1/2
c                                       (1,11) Stable for CFL < 1
c                                 Second order schemes:
c                                        (2,20) Stable for CFL < 1/2
c                                        (2,22) Stable for CFL < 1
c
c         WARNING! The schemes (2,10), (2,11) are unconditionally
c                  unstable.
c
c
c     Note that if method(6)=1 then the capa array comes into the second
c     order correction terms, and is already included in dtdx1d:
c     If ixyz = 1 then
c        dtdx1d(i) = dt/dx                      if method(6) = 0
c                  = dt/(dx*capa(i,jcom,kcom))  if method(6) = 1
c     If ixyz = 2 then
c        dtdx1d(j) = dt/dy                      if method(6) = 0
c                  = dt/(dy*capa(icom,j,kcom))  if method(6) = 1
c     If ixyz = 3 then
c        dtdx1d(k) = dt/dz                      if method(6) = 0
c                  = dt/(dz*capa(icom,jcom,k))  if method(6) = 1
c
c     Notation:
c        The jump in q (q1d(i,:)-q1d(i-1,:))  is split by rpn3 into
c            amdq =  the left-going flux difference  A^- Delta q
c            apdq = the right-going flux difference  A^+ Delta q
c        Each of these is split by rpt3 into
c            bmasdq = the down-going transverse flux difference B^- A^* Delta q
c            bpasdq =   the up-going transverse flux difference B^+ A^* Delta q
c        where A^* represents either A^- or A^+.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn3,rpt3, rptt3
      dimension     q1d(1-mbc:maxm+mbc, meqn)
      dimension    amdq(1-mbc:maxm+mbc, meqn)
      dimension    apdq(1-mbc:maxm+mbc, meqn)
      dimension  bmamdq(1-mbc:maxm+mbc, meqn)
      dimension  bmapdq(1-mbc:maxm+mbc, meqn)
      dimension  bpamdq(1-mbc:maxm+mbc, meqn)
      dimension  bpapdq(1-mbc:maxm+mbc, meqn)
      dimension   cqxx(1-mbc:maxm+mbc, meqn)
      dimension   qadd(1-mbc:maxm+mbc, meqn)
      dimension   fadd(1-mbc:maxm+mbc, meqn)
      dimension   gadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
      dimension   hadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
c
      dimension  cmamdq(1-mbc:maxm+mbc, meqn)
      dimension  cmapdq(1-mbc:maxm+mbc, meqn)
      dimension  cpamdq(1-mbc:maxm+mbc, meqn)
      dimension  cpapdq(1-mbc:maxm+mbc, meqn)
c
      dimension  cmamdq2(1-mbc:maxm+mbc, meqn)
      dimension  cmapdq2(1-mbc:maxm+mbc, meqn)
      dimension  cpamdq2(1-mbc:maxm+mbc, meqn)
      dimension  cpapdq2(1-mbc:maxm+mbc, meqn)
c
      dimension  bmcqxxm(1-mbc:maxm+mbc, meqn)
      dimension  bpcqxxm(1-mbc:maxm+mbc, meqn)
      dimension  cmcqxxm(1-mbc:maxm+mbc, meqn)
      dimension  cpcqxxm(1-mbc:maxm+mbc, meqn)
c
      dimension  bmcqxxp(1-mbc:maxm+mbc, meqn)
      dimension  bpcqxxp(1-mbc:maxm+mbc, meqn)
      dimension  cmcqxxp(1-mbc:maxm+mbc, meqn)
      dimension  cpcqxxp(1-mbc:maxm+mbc, meqn)
c
      dimension  bpcmamdq(1-mbc:maxm+mbc, meqn)
      dimension  bpcmapdq(1-mbc:maxm+mbc, meqn)
      dimension  bpcpamdq(1-mbc:maxm+mbc, meqn)
      dimension  bpcpapdq(1-mbc:maxm+mbc, meqn)
      dimension  bmcmamdq(1-mbc:maxm+mbc, meqn)
      dimension  bmcmapdq(1-mbc:maxm+mbc, meqn)
      dimension  bmcpamdq(1-mbc:maxm+mbc, meqn)
      dimension  bmcpapdq(1-mbc:maxm+mbc, meqn)
c
      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension aux1(1-mbc:maxm+mbc, maux, 3)
      dimension aux2(1-mbc:maxm+mbc, maux, 3)
      dimension aux3(1-mbc:maxm+mbc, maux, 3)
c
      dimension    s(1-mbc:maxm+mbc, mwaves)
      dimension  wave(1-mbc:maxm+mbc, meqn, mwaves)
c
      dimension method(7),mthlim(mwaves)
      logical limit
      common/comxyt/dtcom,dxcom,dycom,dzcom,tcom,icom,jcom,kcom


      limit = .false.
      do 5 mw=1,mwaves
         if (mthlim(mw) .gt. 0) limit = .true.
   5     continue
c
c     # initialize flux increments:
c
      do 10 m=1,meqn
         do 10 i = 1-mbc, mx+mbc
            qadd(i,m) = 0.d0
            fadd(i,m) = 0.d0
            gadd(i,m,1,-1) = 0.d0
            gadd(i,m,1, 0) = 0.d0
            gadd(i,m,1, 1) = 0.d0
            hadd(i,m,1,-1) = 0.d0
            hadd(i,m,1, 0) = 0.d0
            hadd(i,m,1, 1) = 0.d0
            gadd(i,m,2,-1) = 0.d0
            gadd(i,m,2, 0) = 0.d0
            gadd(i,m,2, 1) = 0.d0
            hadd(i,m,2,-1) = 0.d0
            hadd(i,m,2, 0) = 0.d0
            hadd(i,m,2, 1) = 0.d0
   10    continue
c
c     # local method parameters
      if (method(3) .lt. 0) then
c        # dimensional splitting
         m3 = -1
         m4 = 0
      else
c        # unsplit method
         m3 = method(3)/10
         m4 = method(3) - 10*m3
      end if
c
c
c     # solve normal Riemann problem and compute Godunov updates
c
c
c     # aux2(1-mbc,1,2) is the start of a 1d array now used by rpn3
c
      call rpn3(ixyz,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &      aux2(1-mbc,1,2),aux2(1-mbc,1,2),
     &          maux,wave,s,amdq,apdq)

c
c     # Set qadd for the donor-cell upwind method (Godunov)
      do 40 i=1,mx+1
         do 40 m=1,meqn
            qadd(i,m) = qadd(i,m) - dtdx1d(i)*apdq(i,m)
            qadd(i-1,m) = qadd(i-1,m) - dtdx1d(i-1)*amdq(i,m)
   40    continue
c
c     # compute maximum wave speed for checking Courant number:
      cfl1d = 0.d0
      do 51 mw=1,mwaves
         do 50 i=1,mx+1
c          # if s>0 use dtdx1d(i) to compute CFL,
c          # if s<0 use dtdx1d(i-1) to compute CFL:
            cfl1d = dmax1(cfl1d, dtdx1d(i)*s(i,mw),
     &                          -dtdx1d(i-1)*s(i,mw))
   50       continue
   51    continue
c
      if (method(2).eq.1) go to 130
c
c
c     # modify F fluxes for second order q_{xx} correction terms:
c     #   F fluxes are in normal, or x-like, direction
c
c
c     # apply limiter to waves:
      if (limit) call limiter(maxm,meqn,mwaves,mbc,mx,wave,s,mthlim)
c
      do 120 i = 2-mbc,mx+mbc
c
c        # For correction terms below, need average of dtdx in cell
c        # i-1 and i.  Compute these and overwrite dtdx1d:
c
         dtdx1d(i-1) = 0.5d0 * (dtdx1d(i-1) + dtdx1d(i))
c
         do 120 m=1,meqn
            cqxx(i,m) = 0.d0
            do 119 mw=1,mwaves
               cqxx(i,m) = cqxx(i,m) + 0.5d0 * dabs(s(i,mw))
     &             * (1.d0 - dabs(s(i,mw))*dtdx1d(i-1)) * wave(i,m,mw)
  119          continue
            fadd(i,m) = fadd(i,m) + cqxx(i,m)
  120       continue
c
  130 continue
c
      if (m3 .le. 0) return !! no transverse propagation
c
c
c     # TRANSVERSE PROPAGATION
c
c
c     # split the left-going flux difference into down-going and up-going
c     # flux differences (in the y-direction).
c
      call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &          aux3,maux,1,amdq,bmamdq,bpamdq)
c
c     # split the right-going flux difference into down-going and up-going
c     # flux differences (in the y-direction).
c
      call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &          aux3,maux,2,apdq,bmapdq,bpapdq)
c
c     # split the left-going flux difference into down-going and up-going
c     # flux differences (in the z-direction).
c
      call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &          aux3,maux,1,amdq,cmamdq,cpamdq)
c
c     # split the right-going flux difference into down-going and up-going
c     # flux differences (in the y-direction).
c
      call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &          aux3,maux,2,apdq,cmapdq,cpapdq)
c
c     # Split the correction wave into transverse propagating waves
c     # in the y-direction and z-direction.
c
      if (m3.eq.2) then
         if (maux > 0) then
c            # The corrections cqxx affect both cell i-1 to left and cell i
c            # to right of interface.  Transverse splitting will affect
c            # fluxes on both sides.
c            # If there are aux arrays, then we must split cqxx twice in
c            # each transverse direction, once with imp=1 and once with imp=2:

c            # imp = 1 or 2 is used to indicate whether we are propagating
c            # amdq or apdq, i.e. cqxxm or cqxxp

c            # in the y-like direction with imp=1
              call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &            aux1,aux2,aux3,maux,1,cqxx,bmcqxxm,bpcqxxm)

c            # in the y-like direction with imp=2
              call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &            aux1,aux2,aux3,maux,2,cqxx,bmcqxxp,bpcqxxp)

c            # in the z-like direction with imp=1
              call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &            aux1,aux2, aux3,maux,1,cqxx,cmcqxxm,cpcqxxm)

c            # in the z-like direction with imp=2
             call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &            aux1,aux2,aux3,maux,2,cqxx,cmcqxxp,cpcqxxp)
           else
c            # aux arrays aren't being used, so we only need to split
c            # cqxx once in each transverse direction and the same result can
c            # presumably be used to left and right.  
c            # Set imp = 0 since this shouldn't be needed in rpt3 in this case.

c            # in the y-like direction 
              call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &              aux1,aux2,aux3,maux,0,cqxx,bmcqxxm,bpcqxxm)

c            # in the z-like direction 
              call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &              aux1,aux2,aux3,maux,0,cqxx,cmcqxxm,cpcqxxm)

c             # use the same splitting to left and right:
              do m = 1,meqn
                 do i = 0,mx+2
                    bmcqxxp(i,m) = bmcqxxm(i,m)
                    bpcqxxp(i,m) = bpcqxxm(i,m)
                    cmcqxxp(i,m) = cmcqxxm(i,m)
                    cpcqxxp(i,m) = cpcqxxm(i,m)
                 enddo
              enddo
           end if
        end if
c
c
c      # modify G fluxes in the y-like direction
c
c
c     # If the correction wave also propagates in a 3D sense, incorporate
c     # cpcqxx,... into cmamdq, cpamdq, ... so that it is split also.
c
      if(m4 .eq. 1)then
         do 145 m = 1, meqn
            do 145 i = 0, mx+2
               cpapdq2(i,m) = cpapdq(i,m)
               cpamdq2(i,m) = cpamdq(i,m)
               cmapdq2(i,m) = cmapdq(i,m)
               cmamdq2(i,m) = cmamdq(i,m)
 145         continue
      else if(m4 .eq. 2)then
         do 146 m = 1, meqn
            do 146 i = 0, mx+2
               cpapdq2(i,m) = cpapdq(i,m) - 3.d0*cpcqxxp(i,m)
               cpamdq2(i,m) = cpamdq(i,m) + 3.d0*cpcqxxm(i,m)
               cmapdq2(i,m) = cmapdq(i,m) - 3.d0*cmcqxxp(i,m)
               cmamdq2(i,m) = cmamdq(i,m) + 3.d0*cmcqxxm(i,m)
 146        continue
      end if
c
c     # The transverse flux differences in the z-direction are split
c     # into waves propagating in the y-direction. If m4 = 2,
c     # then the transverse propagating correction waves in the z-direction
c     # are also split. This yields terms of the form BCAu_{xzy} and
c     # BCAAu_{xxzy}.
c
      if( m4.gt.0 )then
          call rptt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,2,2,cpapdq2,bmcpapdq,bpcpapdq)
          call rptt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,1,2,cpamdq2,bmcpamdq,bpcpamdq)
          call rptt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,2,1,cmapdq2,bmcmapdq,bpcmapdq)
          call rptt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,1,1,cmamdq2,bmcmamdq,bpcmamdq)
      end if
c
c
c     # The updates for G fluxes :
c
c
      do 180 m=1,meqn
         do 180 i = 1, mx+1
c
c           # Transverse propagation of the increment waves
c           # between cells sharing interfaces, i.e. the 2D approach.
c           # Yields BAu_{xy}.
c
            gadd(i-1,m,1,0) = gadd(i-1,m,1,0)
     &                      - 0.5d0*dtdx1d(i-1)*bmamdq(i,m)
            gadd(i-1,m,2,0) = gadd(i-1,m,2,0)
     &                      - 0.5d0*dtdx1d(i-1)*bpamdq(i,m)
            gadd(i,m,1,0)   = gadd(i,m,1,0)
     &                      - 0.5d0*dtdx1d(i-1)*bmapdq(i,m)
            gadd(i,m,2,0)   = gadd(i,m,2,0)
     &                      - 0.5d0*dtdx1d(i-1)*bpapdq(i,m)
c
c           # Transverse propagation of the increment wave (and the
c           # correction wave if m4=2) between cells
c           # only having a corner or edge in common. Yields terms of the
c           # BCAu_{xzy} and BCAAu_{xxzy}.
c
            if( m4.gt.0 )then
c

                gadd(i,m,2,0) = gadd(i,m,2,0)
     &                  + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                  * (bpcpapdq(i,m) - bpcmapdq(i,m))
                gadd(i,m,1,0) = gadd(i,m,1,0)
     &                  + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                  * (bmcpapdq(i,m) - bmcmapdq(i,m))


                gadd(i,m,2,1) = gadd(i,m,2,1)
     &                          - (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bpcpapdq(i,m)
                gadd(i,m,1,1) = gadd(i,m,1,1)
     &                          - (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bmcpapdq(i,m)
                gadd(i,m,2,-1) = gadd(i,m,2,-1)
     &                          + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bpcmapdq(i,m)
                gadd(i,m,1,-1) = gadd(i,m,1,-1)
     &                          + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bmcmapdq(i,m)
c
                gadd(i-1,m,2,0) = gadd(i-1,m,2,0)
     &                   + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                   * (bpcpamdq(i,m) - bpcmamdq(i,m))
                gadd(i-1,m,1,0) = gadd(i-1,m,1,0)
     &                   + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                   * (bmcpamdq(i,m) - bmcmamdq(i,m))


                gadd(i-1,m,2,1) = gadd(i-1,m,2,1)
     &                          - (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bpcpamdq(i,m)
                gadd(i-1,m,1,1) = gadd(i-1,m,1,1)
     &                          - (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bmcpamdq(i,m)
                gadd(i-1,m,2,-1) = gadd(i-1,m,2,-1)
     &                          + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bpcmamdq(i,m)
                gadd(i-1,m,1,-1) = gadd(i-1,m,1,-1)
     &                          + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bmcmamdq(i,m)
c
            end if
c
c           # Transverse propagation of the correction wave between
c           # cells sharing faces. This gives BAAu_{xxy}.
c
            if(m3.lt.2) go to 180
               gadd(i,m,2,0)   = gadd(i,m,2,0)
     &                         + dtdx1d(i-1)*bpcqxxp(i,m)
               gadd(i,m,1,0)   = gadd(i,m,1,0)
     &                         + dtdx1d(i-1)*bmcqxxp(i,m)
               gadd(i-1,m,2,0) = gadd(i-1,m,2,0)
     &                         - dtdx1d(i-1)*bpcqxxm(i,m)
               gadd(i-1,m,1,0) = gadd(i-1,m,1,0)
     &                         - dtdx1d(i-1)*bmcqxxm(i,m)
c
  180       continue
c
c
c
c      # modify H fluxes in the z-like direction
c
c
c     # If the correction wave also propagates in a 3D sense, incorporate
c     # cqxx into bmamdq, bpamdq, ... so that is is split also.
c
      if(m4 .eq. 2)then
         do 155 m = 1, meqn
            do 155 i = 0, mx+2
               bpapdq(i,m) = bpapdq(i,m) - 3.d0*bpcqxxp(i,m)
               bpamdq(i,m) = bpamdq(i,m) + 3.d0*bpcqxxm(i,m)
               bmapdq(i,m) = bmapdq(i,m) - 3.d0*bmcqxxp(i,m)
               bmamdq(i,m) = bmamdq(i,m) + 3.d0*bmcqxxm(i,m)
155         continue
      end if
c
c     # The transverse flux differences in the y-direction are split
c     # into waves propagating in the z-direction. If m4 = 2,
c     # then the transverse propagating correction waves in the y-direction
c     # are also split. This yields terms of the form BCAu_{xzy} and
c     # BCAAu_{xxzy}.
c
c     # note that the output to rptt3 below should logically be named
c     # cmbsasdq and cpbsasdq rather than bmcsasdq and bpcsasdq, but
c     # we are re-using the previous storage rather than requiring new arrays.
c
      if( m4.gt.0 )then
          call rptt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,2,2,bpapdq,bmcpapdq,bpcpapdq)
          call rptt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,1,2,bpamdq,bmcpamdq,bpcpamdq)
          call rptt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,2,1,bmapdq,bmcmapdq,bpcmapdq)
          call rptt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,1,1,bmamdq,bmcmamdq,bpcmamdq)
      end if
c
c
c     # The updates for H fluxes :
c
c
      do 200 m=1,meqn
         do 200 i = 1, mx+1
c
c           # Transverse propagation of the increment waves
c           # between cells sharing interfaces, i.e. the 2D approach.
c           # Yields CAu_{xy}.
c
            hadd(i-1,m,1,0) = hadd(i-1,m,1,0)
     &                      - 0.5d0*dtdx1d(i-1)*cmamdq(i,m)
            hadd(i-1,m,2,0) = hadd(i-1,m,2,0)
     &                      - 0.5d0*dtdx1d(i-1)*cpamdq(i,m)
            hadd(i,m,1,0)   = hadd(i,m,1,0)
     &                      - 0.5d0*dtdx1d(i-1)*cmapdq(i,m)
            hadd(i,m,2,0)   = hadd(i,m,2,0)
     &                      - 0.5d0*dtdx1d(i-1)*cpapdq(i,m)
c
c           # Transverse propagation of the increment wave (and the
c           # correction wave if m4=2) between cells
c           # only having a corner or edge in common. Yields terms of the
c           # CBAu_{xzy} and CBAAu_{xxzy}.
c
            if( m4.gt.0 )then
c
                hadd(i,m,2,0)  = hadd(i,m,2,0)
     &                  + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                  * (bpcpapdq(i,m) - bpcmapdq(i,m))
                hadd(i,m,1,0)  = hadd(i,m,1,0)
     &                  + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                  * (bmcpapdq(i,m) - bmcmapdq(i,m))


                hadd(i,m,2,1)  = hadd(i,m,2,1)
     &                         - (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                         * bpcpapdq(i,m)
                hadd(i,m,1,1)  = hadd(i,m,1,1)
     &                         - (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                         * bmcpapdq(i,m)
                hadd(i,m,2,-1) = hadd(i,m,2,-1)
     &                         + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                         * bpcmapdq(i,m)
                hadd(i,m,1,-1) = hadd(i,m,1,-1)
     &                         + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                         * bmcmapdq(i,m)
c
                hadd(i-1,m,2,0)  = hadd(i-1,m,2,0)
     &                   + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                   * (bpcpamdq(i,m) - bpcmamdq(i,m))
                hadd(i-1,m,1,0)  = hadd(i-1,m,1,0)
     &                   + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                   * (bmcpamdq(i,m) - bmcmamdq(i,m))


                hadd(i-1,m,2,1)  = hadd(i-1,m,2,1)
     &                           - (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                           * bpcpamdq(i,m)
                hadd(i-1,m,1,1)  = hadd(i-1,m,1,1)
     &                           - (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                           * bmcpamdq(i,m)
                hadd(i-1,m,2,-1) = hadd(i-1,m,2,-1)
     &                           + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                           * bpcmamdq(i,m)
                hadd(i-1,m,1,-1) = hadd(i-1,m,1,-1)
     &                           + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                           * bmcmamdq(i,m)
c
            end if
c
c           # Transverse propagation of the correction wave between
c           # cells sharing faces. This gives CAAu_{xxy}.
c
            if(m3.lt.2) go to 200
               hadd(i,m,2,0)   = hadd(i,m,2,0)
     &                         + dtdx1d(i-1)*cpcqxxp(i,m)
               hadd(i,m,1,0)   = hadd(i,m,1,0)
     &                         + dtdx1d(i-1)*cmcqxxp(i,m)
               hadd(i-1,m,2,0) = hadd(i-1,m,2,0)
     &                         - dtdx1d(i-1)*cpcqxxm(i,m)
               hadd(i-1,m,1,0) = hadd(i-1,m,1,0)
     &                         - dtdx1d(i-1)*cmcqxxm(i,m)
c
  200    continue
c
      return
      end
      subroutine flux3(ixyz,maxm,meqn,mwaves,mbc,mx,
     &                 q1d,dtdx1d,dtdy,dtdz,aux1,aux2,aux3,maux,
     &                 method,mthlim,qadd,fadd,gadd,hadd,cfl1d,
     &                 wave,s,amdq,apdq,cqxx,
     &                 bmamdq,bmapdq,bpamdq,bpapdq,
     &                 cmamdq,cmapdq,cpamdq,cpapdq,
     &                 cmamdq2,cmapdq2,cpamdq2,cpapdq2,
     &                 bmcqxxp,bpcqxxp,bmcqxxm,bpcqxxm,
     &                 cmcqxxp,cpcqxxp,cmcqxxm,cpcqxxm,
     &                 bmcmamdq,bmcmapdq,bpcmamdq,bpcmapdq,
     &                 bmcpamdq,bmcpapdq,bpcpamdq,bpcpapdq,
     &                 rpn3,rpt3,rptt3)

c*********************************************************************72
c
cc FLUX3 computes modified fluxes F, G and H along a 1D slice of the 3D grid.
c
c  Discussion:
c
c     # Compute the modification to fluxes f, g and h that are generated by
c     # all interfaces along a 1D slice of the 3D grid.
c     #    ixyz = 1  if it is a slice in x
c     #           2  if it is a slice in y
c     #           3  if it is a slice in z
c     # This value is passed into the Riemann solvers. The flux modifications
c     # go into the arrays fadd, gadd and hadd.  The notation is written
c     # assuming we are solving along a 1D slice in the x-direction.
c
c     # fadd(i,.) modifies F to the left of cell i
c     # gadd(i,.,1,slice) modifies G below cell i (in the z-direction)
c     # gadd(i,.,2,slice) modifies G above cell i
c     #                   The G flux in the surrounding slices may
c     #                   also be updated.
c     #                   slice  =  -1     The slice below in y-direction
c     #                   slice  =   0     The slice used in the 2D method
c     #                   slice  =   1     The slice above in y-direction
c     # hadd(i,.,1,slice) modifies H below cell i (in the y-direction)
c     # hadd(i,.,2,slice) modifies H above cell i
c     #                   The H flux in the surrounding slices may
c     #                   also be updated.
c     #                   slice  =  -1     The slice below in z-direction
c     #                   slice  =   0     The slice used in the 2D method
c     #                   slice  =   1     The slice above in z-direction
c     #
c     # The method used is specified by method(2) and method(3):
c
c        method(2) = 1 No correction waves
c                  = 2 if second order correction terms are to be added, with
c                      a flux limiter as specified by mthlim.  No transverse
c                      propagation of these waves.
c
c         method(3) specify how the transverse wave propagation
c         of the increment wave and the correction wave are performed.
c         Note that method(3) is given by a two digit number, in
c         contrast to what is the case for claw2. It is convenient
c         to define the scheme using the pair (method(2),method(3)).
c
c         method(3) <  0 Gives dimensional splitting using Godunov
c                        splitting, i.e. formally first order
c                        accurate.
c                      0 Gives the Donor cell method. No transverse
c                        propagation of neither the increment wave
c                        nor the correction wave.
c                   = 10 Transverse propagation of the increment wave
c                        as in 2D. Note that method (2,10) is
c                        unconditionally unstable.
c                   = 11 Corner transport upwind of the increment
c                        wave. Note that method (2,11) also is
c                        unconditionally unstable.
c                   = 20 Both the increment wave and the correction
c                        wave propagate as in the 2D case. Only to
c                        be used with method(2) = 2.
c                   = 21 Corner transport upwind of the increment wave,
c                        and the correction wave propagates as in 2D.
c                        Only to be used with method(2) = 2.
c                   = 22 3D propagation of both the increment wave and
c                        the correction wave. Only to be used with
c                        method(2) = 2.
c
c         Recommended settings:   First order schemes:
c                                       (1,10) Stable for CFL < 1/2
c                                       (1,11) Stable for CFL < 1
c                                 Second order schemes:
c                                        (2,20) Stable for CFL < 1/2
c                                        (2,22) Stable for CFL < 1
c
c         WARNING! The schemes (2,10), (2,11) are unconditionally
c                  unstable.
c
c     Note that if method(6)=1 then the capa array comes into the second
c     order correction terms, and is already included in dtdx1d:
c     If ixyz = 1 then
c        dtdx1d(i) = dt/dx                      if method(6) = 0
c                  = dt/(dx*capa(i,jcom,kcom))  if method(6) = 1
c     If ixyz = 2 then
c        dtdx1d(j) = dt/dy                      if method(6) = 0
c                  = dt/(dy*capa(icom,j,kcom))  if method(6) = 1
c     If ixyz = 3 then
c        dtdx1d(k) = dt/dz                      if method(6) = 0
c                  = dt/(dz*capa(icom,jcom,k))  if method(6) = 1
c
c     Notation:
c        The jump in q (q1d(i,:)-q1d(i-1,:))  is split by rpn3 into
c            amdq =  the left-going flux difference  A^- Delta q
c            apdq = the right-going flux difference  A^+ Delta q
c        Each of these is split by rpt3 into
c            bmasdq = the down-going transverse flux difference B^- A^* Delta q
c            bpasdq =   the up-going transverse flux difference B^+ A^* Delta q
c        where A^* represents either A^- or A^+.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn3,rpt3, rptt3
      dimension     q1d(1-mbc:maxm+mbc, meqn)
      dimension    amdq(1-mbc:maxm+mbc, meqn)
      dimension    apdq(1-mbc:maxm+mbc, meqn)
      dimension  bmamdq(1-mbc:maxm+mbc, meqn)
      dimension  bmapdq(1-mbc:maxm+mbc, meqn)
      dimension  bpamdq(1-mbc:maxm+mbc, meqn)
      dimension  bpapdq(1-mbc:maxm+mbc, meqn)
      dimension   cqxx(1-mbc:maxm+mbc, meqn)
      dimension   qadd(1-mbc:maxm+mbc, meqn)
      dimension   fadd(1-mbc:maxm+mbc, meqn)
      dimension   gadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
      dimension   hadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
c
      dimension  cmamdq(1-mbc:maxm+mbc, meqn)
      dimension  cmapdq(1-mbc:maxm+mbc, meqn)
      dimension  cpamdq(1-mbc:maxm+mbc, meqn)
      dimension  cpapdq(1-mbc:maxm+mbc, meqn)
c
      dimension  cmamdq2(1-mbc:maxm+mbc, meqn)
      dimension  cmapdq2(1-mbc:maxm+mbc, meqn)
      dimension  cpamdq2(1-mbc:maxm+mbc, meqn)
      dimension  cpapdq2(1-mbc:maxm+mbc, meqn)
c
      dimension  bmcqxxm(1-mbc:maxm+mbc, meqn)
      dimension  bpcqxxm(1-mbc:maxm+mbc, meqn)
      dimension  cmcqxxm(1-mbc:maxm+mbc, meqn)
      dimension  cpcqxxm(1-mbc:maxm+mbc, meqn)
c
      dimension  bmcqxxp(1-mbc:maxm+mbc, meqn)
      dimension  bpcqxxp(1-mbc:maxm+mbc, meqn)
      dimension  cmcqxxp(1-mbc:maxm+mbc, meqn)
      dimension  cpcqxxp(1-mbc:maxm+mbc, meqn)
c
      dimension  bpcmamdq(1-mbc:maxm+mbc, meqn)
      dimension  bpcmapdq(1-mbc:maxm+mbc, meqn)
      dimension  bpcpamdq(1-mbc:maxm+mbc, meqn)
      dimension  bpcpapdq(1-mbc:maxm+mbc, meqn)
      dimension  bmcmamdq(1-mbc:maxm+mbc, meqn)
      dimension  bmcmapdq(1-mbc:maxm+mbc, meqn)
      dimension  bmcpamdq(1-mbc:maxm+mbc, meqn)
      dimension  bmcpapdq(1-mbc:maxm+mbc, meqn)
c
      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension aux1(1-mbc:maxm+mbc, maux, 3)
      dimension aux2(1-mbc:maxm+mbc, maux, 3)
      dimension aux3(1-mbc:maxm+mbc, maux, 3)
c
      dimension    s(1-mbc:maxm+mbc, mwaves)
      dimension  wave(1-mbc:maxm+mbc, meqn, mwaves)
c
      dimension method(7),mthlim(mwaves)
      logical limit, diff_flag
      common/comxyt/dtcom,dxcom,dycom,dzcom,tcom,icom,jcom,kcom

      common /step3_debug_common/ diff_flag

      limit = .false.
      do 5 mw=1,mwaves
         if (mthlim(mw) .gt. 0) limit = .true.
   5     continue
c
c     # initialize flux increments:
c
      do 10 m=1,meqn
         do 10 i = 1-mbc, mx+mbc
            qadd(i,m) = 0.d0
            fadd(i,m) = 0.d0
            gadd(i,m,1,-1) = 0.d0
            gadd(i,m,1, 0) = 0.d0
            gadd(i,m,1, 1) = 0.d0
            hadd(i,m,1,-1) = 0.d0
            hadd(i,m,1, 0) = 0.d0
            hadd(i,m,1, 1) = 0.d0
            gadd(i,m,2,-1) = 0.d0
            gadd(i,m,2, 0) = 0.d0
            gadd(i,m,2, 1) = 0.d0
            hadd(i,m,2,-1) = 0.d0
            hadd(i,m,2, 0) = 0.d0
            hadd(i,m,2, 1) = 0.d0
   10    continue
c
c     # local method parameters
c 6/3/02 : included check for negative method(3) (dimensional splitting)
      if (method(3) < 0) then
         m3 = -1
         m4 = 0
      else
         m3 = method(3)/10
         m4 = method(3) - 10*m3
      end if
c
c     # solve Riemann problem at each interface and compute Godunov flux F0
c
      call rpn3(ixyz,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &      aux2(1-mbc,1,2),aux2(1-mbc,1,2),
     &      maux,wave,s,amdq,apdq)
c
c     # Set qadd for the donor-cell upwind method (Godunov)
      do 40 i=1,mx+1
         do 40 m=1,meqn
            qadd(i,m) = qadd(i,m) - dtdx1d(i)*apdq(i,m)
            qadd(i-1,m) = qadd(i-1,m) - dtdx1d(i-1)*amdq(i,m)
   40    continue
c
c     # compute maximum wave speed for checking Courant number:
      cfl1d = 0.d0
      do 51 mw=1,mwaves
         do 50 i=1,mx+1
c          # if s>0 use dtdx1d(i) to compute CFL,
c          # if s<0 use dtdx1d(i-1) to compute CFL:
            cfl1d = dmax1(cfl1d, dtdx1d(i)*s(i,mw),
     &                          -dtdx1d(i-1)*s(i,mw))
   50       continue
   51    continue
c
      if (method(2).eq.1) go to 130
c
c     # modify F fluxes for second order q_{xx} correction terms:
c
c     # apply limiter to waves:
      if (limit) call limiter(maxm,meqn,mwaves,mbc,mx,wave,s,mthlim)
c
      do 120 i = 1, mx+1
c
c        # For correction terms below, need average of dtdx in cell
c        # i-1 and i.  Compute these and overwrite dtdx1d:
c
         dtdx1d(i-1) = 0.5d0 * (dtdx1d(i-1) + dtdx1d(i))
c
         do 120 m=1,meqn
            cqxx(i,m) = 0.d0
            do 119 mw=1,mwaves
               cqxx(i,m) = cqxx(i,m) + 0.5d0 * dabs(s(i,mw))
     &             * (1.d0 - dabs(s(i,mw))*dtdx1d(i-1)) * wave(i,m,mw)
  119          continue
            fadd(i,m) = fadd(i,m) + cqxx(i,m)
  120       continue
c
  130 continue
c
      if (m3 <= 0) return !! no transverse propagation
c
c     # modify G fluxes for transverse propagation
c
c     # split the left-going flux difference into down-going and up-going
c     # flux differences (in the y-direction).
c
      call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &          aux3,maux,1,amdq,bmamdq,bpamdq)
c
c     # split the right-going flux difference into down-going and up-going
c     # flux differences (in the y-direction).
c
      call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &          aux3,maux,2,apdq,bmapdq,bpapdq)
c
c     # split the left-going flux difference into down-going and up-going
c     # flux differences (in the z-direction).
c
      call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &          aux3,maux,1,amdq,cmamdq,cpamdq)
c
c     # split the right-going flux difference into down-going and up-going
c     # flux differences (in the y-direction).
c
      call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &          aux3,maux,2,apdq,cmapdq,cpapdq)
c
c     # Split the correction wave into transverse propagating waves
c     # in the y-direction and z-direction.
c
      if (m3.eq.2) then
         if (maux > 0) then
             !! DAC : icoor == 2 or 3 : indicates we are propagating in y or z
             !! direction

             !! Left of cell i, in the y direction
            call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &            aux1,aux2,aux3,maux,1,cqxx,bmcqxxm,bpcqxxm)


              !! Cell i, in the y direction
            call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &            aux1,aux2,aux3,maux,2,cqxx,bmcqxxp,bpcqxxp)

              !! Left of cell i, in the z direction
            call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &            aux1,aux2, aux3,maux,1,cqxx,cmcqxxm,cpcqxxm)

              !! Cell i, in the z direction.
           call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &            aux1,aux2,aux3,maux,2,cqxx,cmcqxxp,cpcqxxp)
           else
              !! aux arrays aren't being used, so we set imp = 0
              call rpt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &              aux1,aux2,aux3,maux,0,cqxx,bmcqxxm,bpcqxxm)

              call rpt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,
     &              aux1,aux2,aux3,maux,0,cqxx,cmcqxxm,cpcqxxm)
              do m = 1,meqn
                 do i = 0,mx+2
                    bmcqxxp(i,m) = bmcqxxm(i,m)
                    bpcqxxp(i,m) = bpcqxxm(i,m)
                    cmcqxxp(i,m) = cmcqxxm(i,m)
                    cpcqxxp(i,m) = cpcqxxm(i,m)
                 enddo
              enddo
           end if
        end if
c
c     # If the correction wave also propagates in a 3D sense, incorporate
c     # cpcqxx,... into cmamdq, cpamdq, ... so that it is split also.
c
      if(m4 .eq. 1)then
         do 145 m = 1, meqn
            do 145 i = 0, mx+2
               cpapdq2(i,m) = cpapdq(i,m)
               cpamdq2(i,m) = cpamdq(i,m)
               cmapdq2(i,m) = cmapdq(i,m)
               cmamdq2(i,m) = cmamdq(i,m)
 145         continue
      else if(m4 .eq. 2)then
         do 146 m = 1, meqn
            do 146 i = 0, mx+2
               cpapdq2(i,m) = cpapdq(i,m) - 3.d0*cpcqxxp(i,m)
               cpamdq2(i,m) = cpamdq(i,m) + 3.d0*cpcqxxm(i,m)
               cmapdq2(i,m) = cmapdq(i,m) - 3.d0*cmcqxxp(i,m)
               cmamdq2(i,m) = cmamdq(i,m) + 3.d0*cmcqxxm(i,m)
 146        continue
      end if
c
c     # The transverse flux differences in the z-direction are split
c     # into waves propagating in the y-direction. If m4 = 2,
c     # then the transverse propagating correction waves in the z-direction
c     # are also split. This yields terms of the form BCAu_{xzy} and
c     # BCAAu_{xxzy}.
c
      if( m4.gt.0 )then
          call rptt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,2,2,cpapdq2,bmcpapdq,bpcpapdq)
          call rptt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,1,2,cpamdq2,bmcpamdq,bpcpamdq)
          call rptt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,2,1,cmapdq2,bmcmapdq,bpcmapdq)
          call rptt3(ixyz,2,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,1,1,cmamdq2,bmcmamdq,bpcmamdq)
      end if
c
c     # The updates.
c
      do 180 m=1,meqn
         do 180 i = 1, mx+1
c
c           # Transverse propagation of the increment waves
c           # between cells sharing interfaces, i.e. the 2D approach.
c           # Yields BAu_{xy}.
c
            gadd(i-1,m,1,0) = gadd(i-1,m,1,0)
     &                      - 0.5d0*dtdx1d(i-1)*bmamdq(i,m)
            gadd(i-1,m,2,0) = gadd(i-1,m,2,0)
     &                      - 0.5d0*dtdx1d(i-1)*bpamdq(i,m)
            gadd(i,m,1,0)   = gadd(i,m,1,0)
     &                      - 0.5d0*dtdx1d(i-1)*bmapdq(i,m)
            gadd(i,m,2,0)   = gadd(i,m,2,0)
     &                      - 0.5d0*dtdx1d(i-1)*bpapdq(i,m)
c
c           # Transverse propagation of the increment wave (and the
c           # correction wave if m4=2) between cells
c           # only having a corner or edge in common. Yields terms of the
c           # BCAu_{xzy} and BCAAu_{xxzy}.
c
            if( m4.gt.0 )then
c

c DAC : 5/15/02 : Not clear why these things are being difference here
               if (diff_flag) then
                  gadd(i,m,2,0) = gadd(i,m,2,0)
     &                  + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                  * (bpcpapdq(i,m) - bpcmapdq(i,m))
                  gadd(i,m,1,0) = gadd(i,m,1,0)
     &                  + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                  * (bmcpapdq(i,m) - bmcmapdq(i,m))
               end if


                gadd(i,m,2,1) = gadd(i,m,2,1)
     &                          - (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bpcpapdq(i,m)
                gadd(i,m,1,1) = gadd(i,m,1,1)
     &                          - (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bmcpapdq(i,m)
                gadd(i,m,2,-1) = gadd(i,m,2,-1)
     &                          + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bpcmapdq(i,m)
                gadd(i,m,1,-1) = gadd(i,m,1,-1)
     &                          + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bmcmapdq(i,m)
c
                if (diff_flag) then
                   gadd(i-1,m,2,0) = gadd(i-1,m,2,0)
     &                   + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                   * (bpcpamdq(i,m) - bpcmamdq(i,m))
                   gadd(i-1,m,1,0) = gadd(i-1,m,1,0)
     &                   + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                   * (bmcpamdq(i,m) - bmcmamdq(i,m))
                end if


                gadd(i-1,m,2,1) = gadd(i-1,m,2,1)
     &                          - (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bpcpamdq(i,m)
                gadd(i-1,m,1,1) = gadd(i-1,m,1,1)
     &                          - (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bmcpamdq(i,m)
                gadd(i-1,m,2,-1) = gadd(i-1,m,2,-1)
     &                          + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bpcmamdq(i,m)
                gadd(i-1,m,1,-1) = gadd(i-1,m,1,-1)
     &                          + (1.d0/6.d0)*dtdx1d(i-1)*dtdz
     &                          * bmcmamdq(i,m)
c
            end if
c
c           # Transverse propagation of the correction wave between
c           # cells sharing faces. This gives BAAu_{xxy}.
c
            if(m3.lt.2) go to 180
               gadd(i,m,2,0)   = gadd(i,m,2,0)
     &                         + dtdx1d(i-1)*bpcqxxp(i,m)
               gadd(i,m,1,0)   = gadd(i,m,1,0)
     &                         + dtdx1d(i-1)*bmcqxxp(i,m)
               gadd(i-1,m,2,0) = gadd(i-1,m,2,0)
     &                         - dtdx1d(i-1)*bpcqxxm(i,m)
               gadd(i-1,m,1,0) = gadd(i-1,m,1,0)
     &                         - dtdx1d(i-1)*bmcqxxm(i,m)
c
  180       continue
c
c
c      # modify H fluxes
c
c     # If the correction wave also propagates in a 3D sense, incorporate
c     # cqxx into bmamdq, bpamdq, ... so that is is split also.
c
      if(m4 .eq. 2)then
         do 155 m = 1, meqn
            do 155 i = 0, mx+2
               bpapdq(i,m) = bpapdq(i,m) - 3.d0*bpcqxxp(i,m)
               bpamdq(i,m) = bpamdq(i,m) + 3.d0*bpcqxxm(i,m)
               bmapdq(i,m) = bmapdq(i,m) - 3.d0*bmcqxxp(i,m)
               bmamdq(i,m) = bmamdq(i,m) + 3.d0*bmcqxxm(i,m)
155         continue
      end if
c
c     # The transverse flux differences in the y-direction are split
c     # into waves propagating in the z-direction. If m4 = 2,
c     # then the transverse propagating correction waves in the y-direction
c     # are also split. This yields terms of the form BCAu_{xzy} and
c     # BCAAu_{xxzy}.
c
      if( m4.gt.0 )then
          call rptt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,2,2,bpapdq,bmcpapdq,bpcpapdq)
          call rptt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,1,2,bpamdq,bmcpamdq,bpcpamdq)
          call rptt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,2,1,bmapdq,bmcmapdq,bpcmapdq)
          call rptt3(ixyz,3,maxm,meqn,mwaves,mbc,mx,q1d,q1d,aux1,aux2,
     &              aux3,maux,1,1,bmamdq,bmcmamdq,bpcmamdq)
      end if
c
c     # The updates.
c
      do 200 m=1,meqn
         do 200 i = 1, mx+1
c
c           # Transverse propagation of the increment waves
c           # between cells sharing interfaces, i.e. the 2D approach.
c           # Yields CAu_{xy}.
c
            hadd(i-1,m,1,0) = hadd(i-1,m,1,0)
     &                      - 0.5d0*dtdx1d(i-1)*cmamdq(i,m)
            hadd(i-1,m,2,0) = hadd(i-1,m,2,0)
     &                      - 0.5d0*dtdx1d(i-1)*cpamdq(i,m)
            hadd(i,m,1,0)   = hadd(i,m,1,0)
     &                      - 0.5d0*dtdx1d(i-1)*cmapdq(i,m)
            hadd(i,m,2,0)   = hadd(i,m,2,0)
     &                      - 0.5d0*dtdx1d(i-1)*cpapdq(i,m)
c
c           # Transverse propagation of the increment wave (and the
c           # correction wave if m4=2) between cells
c           # only having a corner or edge in common. Yields terms of the
c           # CBAu_{xzy} and CBAAu_{xxzy}.
c
            if( m4.gt.0 )then
c
               if (diff_flag) then
                  hadd(i,m,2,0)  = hadd(i,m,2,0)
     &                  + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                  * (bpcpapdq(i,m) - bpcmapdq(i,m))
                  hadd(i,m,1,0)  = hadd(i,m,1,0)
     &                  + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                  * (bmcpapdq(i,m) - bmcmapdq(i,m))
               end if


                hadd(i,m,2,1)  = hadd(i,m,2,1)
     &                         - (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                         * bpcpapdq(i,m)
                hadd(i,m,1,1)  = hadd(i,m,1,1)
     &                         - (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                         * bmcpapdq(i,m)
                hadd(i,m,2,-1) = hadd(i,m,2,-1)
     &                         + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                         * bpcmapdq(i,m)
                hadd(i,m,1,-1) = hadd(i,m,1,-1)
     &                         + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                         * bmcmapdq(i,m)
c
                if (diff_flag) then
                   hadd(i-1,m,2,0)  = hadd(i-1,m,2,0)
     &                   + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                   * (bpcpamdq(i,m) - bpcmamdq(i,m))
                   hadd(i-1,m,1,0)  = hadd(i-1,m,1,0)
     &                   + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                   * (bmcpamdq(i,m) - bmcmamdq(i,m))
                end if


                hadd(i-1,m,2,1)  = hadd(i-1,m,2,1)
     &                           - (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                           * bpcpamdq(i,m)
                hadd(i-1,m,1,1)  = hadd(i-1,m,1,1)
     &                           - (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                           * bmcpamdq(i,m)
                hadd(i-1,m,2,-1) = hadd(i-1,m,2,-1)
     &                           + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                           * bpcmamdq(i,m)
                hadd(i-1,m,1,-1) = hadd(i-1,m,1,-1)
     &                           + (1.d0/6.d0)*dtdx1d(i-1)*dtdy
     &                           * bmcmamdq(i,m)
c
            end if
c
c           # Transverse propagation of the correction wave between
c           # cells sharing faces. This gives CAAu_{xxy}.
c
            if(m3.lt.2) go to 200
               hadd(i,m,2,0)   = hadd(i,m,2,0)
     &                         + dtdx1d(i-1)*cpcqxxp(i,m)
               hadd(i,m,1,0)   = hadd(i,m,1,0)
     &                         + dtdx1d(i-1)*cmcqxxp(i,m)
               hadd(i-1,m,2,0) = hadd(i-1,m,2,0)
     &                         - dtdx1d(i-1)*cpcqxxm(i,m)
               hadd(i-1,m,1,0) = hadd(i-1,m,1,0)
     &                         - dtdx1d(i-1)*cmcqxxm(i,m)
c
  200    continue
c
      return
      end
      function fss(s)

c*********************************************************************72
c
cc FSS computes fdisc at distance s between corners (xc0,yc0) and (xc1,yc1).
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      common/fsscorn/ xc0,yc0,xc1,yc1

      x = xc0 + s*(xc1-xc0)
      y = yc0 + s*(yc1-yc0)
      fss = fdisc(x,y)

      return
      end
      subroutine limiter(maxm,meqn,mwaves,mbc,mx,wave,s,mthlim)

c*********************************************************************72
c
cc LIMITER applies a limiter to the waves.  
c
c  Discussion:
c
c     # Version of December, 2002.
c     # Modified from the original CLAWPACK routine to eliminate calls 
c     # to philim.  Since philim was called for every wave at each cell
c     # interface, this was adding substantial overhead in some cases.
c
c     # The limiter is computed by comparing the 2-norm of each wave with
c     # the projection of the wave from the interface to the left or
c     # right onto the current wave.  For a linear system this would
c     # correspond to comparing the norms of the two waves.  For a 
c     # nonlinear problem the eigenvectors are not colinear and so the 
c     # projection is needed to provide more limiting in the case where the
c     # neighboring wave has large norm but points in a different direction
c     # in phase space.
c
c     # The specific limiter used in each family is determined by the
c     # value of the corresponding element of the array mthlim.
c     # Note that a different limiter may be used in each wave family.
c
c     # dotl and dotr denote the inner product of wave with the wave to
c     # the left or right.  The norm of the projections onto the wave are then
c     # given by dotl/wnorm2 and dotr/wnorm2, where wnorm2 is the 2-norm
c     # of wave.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension mthlim(mwaves)
      dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
      dimension    s(1-mbc:maxm+mbc, mwaves)
c
c
      do 200 mw=1,mwaves
         if (mthlim(mw) .eq. 0) go to 200
         dotr = 0.d0
         do 190 i = 0, mx+1
            wnorm2 = 0.d0
            dotl = dotr
            dotr = 0.d0
            do 5 m=1,meqn
               wnorm2 = wnorm2 + wave(i,m,mw)**2
               dotr = dotr + wave(i,m,mw)*wave(i+1,m,mw)
    5          continue
            if (i.eq.0) go to 190
            if (wnorm2.eq.0.d0) go to 190
c
            if (s(i,mw) .gt. 0.d0) then
                r = dotl / wnorm2
              else
                r = dotr / wnorm2
              end if
c
            go to (10,20,30,40,50) mthlim(mw)
c
   10       continue
c
c           # minmod
c
            wlimitr = dmax1(0.d0, dmin1(1.d0, r))
            go to 170
c
   20       continue
c
c           # superbee
c
            wlimitr = dmax1(0.d0, dmin1(1.d0, 2.d0*r), dmin1(2.d0, r))
            go to 170
c
   30       continue
c
c           # van Leer
c
            wlimitr = (r + dabs(r)) / (1.d0 + dabs(r))
            go to 170
c
   40       continue
c
c           # monotinized centered
c
            c = (1.d0 + r)/2.d0
            wlimitr = dmax1(0.d0, dmin1(c, 2.d0, 2.d0*r))
            go to 170
c
   50       continue
c
c           # Beam-Warming
c
            wlimitr = r
            go to 170
c
  170       continue
c
c           # apply limiter to waves:
c
            do 180 m=1,meqn
               wave(i,m,mw) = wlimitr * wave(i,m,mw)
  180          continue

  190       continue
  200    continue
c
      return
      end
      subroutine limiter_second (maxm,meqn,mwaves,mbc,mx,wave,s,mthlim)

c*********************************************************************72
c
cc LIMITER_SECOND applies a limiter to the waves.
c
c  Discussion:
c
c     # The limiter is computed by comparing the 2-norm of each wave with
c     # the projection of the wave from the interface to the left or
c     # right onto the current wave.  For a linear system this would
c     # correspond to comparing the norms of the two waves.  For a 
c     # nonlinear problem the eigenvectors are not colinear and so the 
c     # projection is needed to provide more limiting in the case where the
c     # neighboring wave has large norm but points in a different direction
c     # in phase space.
c
c     # The specific limiter used in each family is determined by the
c     # value of the corresponding element of the array mthlim, as used in
c     # the function philim.
c     # Note that a different limiter may be used in each wave family.
c
c     # dotl and dotr denote the inner product of wave with the wave to
c     # the left or right.  The norm of the projections onto the wave are then
c     # given by dotl/wnorm2 and dotr/wnorm2, where wnorm2 is the 2-norm
c     # of wave.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension mthlim(mwaves)
      dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
      dimension    s(1-mbc:maxm+mbc, mwaves)
c
c
      do 50 mw=1,mwaves
         if (mthlim(mw) .eq. 0) go to 50
         dotr = 0.d0
         do 40 i = 0, mx+1
            wnorm2 = 0.d0
            dotl = dotr
            dotr = 0.d0
            do 20 m=1,meqn
               wnorm2 = wnorm2 + wave(i,m,mw)**2
               dotr = dotr + wave(i,m,mw)*wave(i+1,m,mw)
   20          continue
            if (i.eq.0) go to 40
            if (wnorm2.eq.0.d0) go to 40
c
            if (s(i,mw) .gt. 0.d0) then
                wlimitr = philim(wnorm2, dotl, mthlim(mw))
              else
                wlimitr = philim(wnorm2, dotr, mthlim(mw))
              end if
c
            do 30 m=1,meqn
               wave(i,m,mw) = wlimitr * wave(i,m,mw)
   30          continue
   40       continue
   50    continue
c
      return
      end
      subroutine out1(maxmx,meqn,mbc,mx,xlower,dx,q,t,iframe)

c*********************************************************************72
c
cc OUT1 is an output routine for 1D.
c
c  Discussion:
c
c     # Write the results to the file fort.q<iframe>
c     # Use format required by matlab script  plotclaw1.m
c     # The same format is used by the amrclaw package.  
c     # Here it's adapted to output just the single grid.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, meqn)
      character*10 fname1, fname2
c
c     # first create the file name and open file
c
         fname1 = 'fort.qxxxx'
         fname2 = 'fort.txxxx'
         nstp = iframe
         do 55 ipos = 10, 7, -1
            idigit = mod(nstp,10)
            fname1(ipos:ipos) = char(ichar('0') + idigit)
            fname2(ipos:ipos) = char(ichar('0') + idigit)
            nstp = nstp / 10
 55      continue

         open(unit=50,file=fname1,status='unknown',form='formatted')
         open(unit=60,file=fname2,status='unknown',form='formatted')

c
c     # the following parameters are used in amrclaw where there are
c     # multiple grids.  Here they are all set to 1:
      ngrids = 1
      mptr = 1
      level = 1

      write(50,1001) mptr,level,mx
 1001 format(i5,'                 grid_number',/,
     &       i5,'                 AMR_level',/,
     &       i5,'                 mx')

      write(50,1002) xlower,dx
 1002 format(e18.8,'    xlow', /,
     &       e18.8,'    dx', /)
c
        do 10 i=1,mx
          do m=1,meqn
c            # exponents with more than 2 digits cause problems reading
c            # into matlab... reset tiny values to zero:
             if (dabs(q(i,m)) .lt. 1d-99) q(i,m) = 0.d0
             enddo
c
          write(50,1005) (q(i,m), m=1,meqn)
 1005     format(4e16.8)
c
 10       continue
        write(50,*) ' '
 20     continue
      write(50,*) ' '

      write(60,1000) t,meqn,ngrids
 1000 format(e18.8,'    time', /, 
     &       i5,'                 meqn'/,
     &       i5,'                 ngrids'/,/)
c

      close(unit=50)
      close(unit=60)

      return
      end
      subroutine out1_hdf (maxmx,meqn,mbc,mx,xlower,dx,q,t,iframe)

c*********************************************************************72
c
cc OUT1_HDF is an output routine for 1D.
c
c  Discussion:
c
c     # Output the results for a general system of conservation laws
c     # in 1 dimension to a hdf file as a scientific data set.
c     # See http://hdf.ncsa.uiuc.edu/ for more info on HDF.
c     # The results in this output file can be plotted in MATLAB
c     # using the "plotclaw1" script.
c
c     # Revised 2003 by Peter Blossey
c     # Adapted code written by Sorin Mitran to standard F77 CLAWPACK style
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      parameter   (nDim = 1)
      dimension    q(1-mbc:maxmx+mbc, meqn)
      character*14 fname
      character*13 qname2
      character*9  qname
c
c     # HDF: Declare variables that describe datasets and HDF files.
c
      integer    sd_id, sds_id, sds_rank
      integer    sds_dims, sds_start, sds_edges, sds_stride
      dimension  sds_dims(nDim), sds_start(nDim)
      dimension  sds_edges(nDim), sds_stride(nDim) 
      dimension  qbuf(21), qout(mx)
c
c     # HDF: Declare external HDF functions
c
      integer  sfstart, sfcreate, sfwdata, sfscompress, sfendacc, sfend
      external sfstart, sfcreate, sfwdata, sfscompress, sfendacc, sfend
c
c     # HDF: Set up HDF constants
c
      integer 	DFACC_READ, DFACC_WRITE, DFACC_CREATE 
      parameter(DFACC_READ = 1, DFACC_WRITE = 2, DFACC_CREATE = 4)

      integer   DFNT_FLOAT64, DFNT_INT32
      parameter(DFNT_FLOAT64 = 6, DFNT_INT32 = 24)

      integer   SUCCEED, FAIL
      parameter(SUCCEED = 0, FAIL = -1)
c
c     # HDF: Set up compression constants for HDF file.
c
      integer   COMP_CODE_DEFLATE, DEFLATE_LEVEL
      parameter (COMP_CODE_DEFLATE = 4, DEFLATE_LEVEL = 6)
c
c     # Write the results to the file fort.q<iframe>
c     # Use format required by matlab script  plotclaw2.m
c     # The same format is used by the amrclaw package.
c     # Here it's adapted to output just the single grid.
c
c     # first create the file name and open file
c
      fname = 'fort.q'
     &     // char(ichar('0') + mod(iframe/1000,10)) 
     &     // char(ichar('0') + mod(iframe/100,10)) 
     &     // char(ichar('0') + mod(iframe/10,10)) 
     &     // char(ichar('0') + mod(iframe,10))
     &     // '.hdf'
c
c     # Specify grid number and create a string which will describe this
c     # grid in HDF file.  This could be an input for simulations with
c     # multiple grids, as in AMRCLAW.  
c
      ngrids_out=1
      qname = 'grid_'
     &     // char(ichar('0') + mod(ngrids_out/1000,10)) 
     &     // char(ichar('0') + mod(ngrids_out/100,10)) 
     &     // char(ichar('0') + mod(ngrids_out/10,10)) 
     &     // char(ichar('0') + mod(ngrids_out,10))
c
c     # HDF: create hdf file.
c
      sd_id = sfstart(fname, DFACC_CREATE)
      if (sd_id.eq.FAIL) THEN
         WRITE(*,*) 'Failed to create HDF file (call to sfstart)'
         STOP
      end if
c
c     # HDF: create a data set for parameters describing q in HDF file.
c
      sds_rank = 1
      sds_dims(1) = 21
      
      sds_id = sfcreate(sd_id,qname,DFNT_FLOAT64,sds_rank,sds_dims)
      if (sds_id.eq.FAIL) THEN
         WRITE(*,*) 'Failed to create scientific data set in HDF file'
         STOP
      end if
c
c     # HDF: set up parameters describing data set.
c
      sds_start(1)  = 0
      sds_edges(1)  = sds_dims(1)
      sds_stride(1) = 1        
      qbuf(1) = ngrids_out
      qbuf(2) = nDim
      qbuf(3) = t
      qbuf(4) = meqn
      qbuf(5) = 1.
      qbuf(6) = mx
      qbuf(7) = 0.
      qbuf(8) = 0.
      qbuf(9) = 0.
      qbuf(10) = xlower
      qbuf(11) = 0.
      qbuf(12) = 0.
      qbuf(13) = 0.
      qbuf(14) = xlower+mx*dx
      qbuf(15) = 0.
      qbuf(16) = 0.
      qbuf(17) = 0.
      qbuf(18) = dx
      qbuf(19) = 0.
      qbuf(20) = 0.
      qbuf(21) = 0.
      istat = sfwdata(sds_id,sds_start,sds_stride,sds_edges,qbuf)  
      istat = sfendacc(sds_id)  
c
c     # Loop over fields in q
c
      do m = 1,meqn
c
c     # HDF: create a data set for parameters describing q in HDF file.
c
         qname2 = qname // '_'
     &        // char(ichar('0') + mod(m/100,10)) 
     &        // char(ichar('0') + mod(m/10,10)) 
     &        // char(ichar('0') + mod(m,10))
c
         sds_rank = nDim
         sds_dims(1) = mx
c      
         sds_id = sfcreate(sd_id,qname,DFNT_FLOAT64,sds_rank,sds_dims)
         if (sds_id.eq.FAIL) THEN
            WRITE(*,*) 'Failed to create data set in HDF file'
            STOP
         end if
c
c     # HDF: set up parameters describing data set.
c
         sds_start(1)  = 0
         sds_edges(1)  = sds_dims(1)
         sds_stride(1) = 1        

c     # Copy current field of q into qout.
c
         do i = 1,mx
            qout(i) = q(i,m)
         end do
c
c     # HDF: set compression mode and write data to hdf file.
c
         istat=sfscompress(sds_id,COMP_CODE_DEFLATE,DEFLATE_LEVEL)
         istat = sfwdata(sds_id,sds_start,sds_stride,sds_edges,qout)
         if (istat.eq.FAIL) then
            WRITE(*,*) 'Failed to write SDS (call to sfwdata)'
            STOP
         end if
c
c     # HDF: Close the data set
c
         istat = sfendacc(sds_id)  
         if (istat.eq.FAIL) then
            WRITE(*,*) 'Failed to close SDS (call to sfendacc)'
            STOP
         end if
      end do
c
c     # HDF: Close HDF file.
c
      istat = sfend(sd_id)
      if (istat.eq.FAIL) then
         WRITE(*,*) 'Failed to close ', fname, ' (call to sfend)'
         STOP
      end if

      return
      end
      subroutine out2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                 dx,dy,q,t,iframe)

c*********************************************************************72
c
cc OUT2 is an output routine for 2D.
c
c  Discussion:
c
c     # Write the results to the file fort.q<iframe>
c     # Use format required by matlab script  plotclaw2.m
c     # The same format is used by the amrclaw package.
c     # Here it's adapted to output just the single grid.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      character*10 fname1, fname2
c
c     # first create the file name and open file
c
         fname1 = 'fort.qxxxx'
         fname2 = 'fort.txxxx'
         nstp = iframe
         do 55 ipos = 10, 7, -1
            idigit = mod(nstp,10)
            fname1(ipos:ipos) = char(ichar('0') + idigit)
            fname2(ipos:ipos) = char(ichar('0') + idigit)
            nstp = nstp / 10
 55      continue

         open(unit=50,file=fname1,status='unknown',form='formatted')
         open(unit=60,file=fname2,status='unknown',form='formatted')

c
c     # the following parameters are used in amrclaw where there are
c     # multiple grids.  Here they are all set to 1:
      ngrids = 1
      mptr = 1
      level = 1

      write(50,1001) mptr,level,mx,my
 1001 format(i5,'                 grid_number',/,
     &       i5,'                 AMR_level',/,
     &       i5,'                 mx',/,
     &       i5,'                 my')

      write(50,1002) xlower,ylower,dx,dy
 1002 format(e26.16,'    xlow', /,
     &       e26.16,'    ylow', /,
     &       e26.16,'    dx', /,
     &       e26.16,'    dy',/)
c
      do 20 j=1,my
        do 10 i=1,mx
          do m=1,meqn
c            # exponents with more than 2 digits cause problems reading
c            # into matlab... reset tiny values to zero:
             if (dabs(q(i,j,m)) .lt. 1d-99) q(i,j,m) = 0.d0
             enddo
c
          write(50,1005) (q(i,j,m), m=1,meqn)
 1005     format(4e26.16)
c
 10       continue
        write(50,*) ' '
 20     continue
      write(50,*) ' '

      write(60,1000) t,meqn,ngrids
 1000 format(e26.16,'    time', /,
     &       i5,'                 meqn'/,
     &       i5,'                 ngrids'/,/)
c

      close(unit=50)
      close(unit=60)

      return
      end
      subroutine out2_hdf(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                 dx,dy,q,t,iframe)

c*********************************************************************72
c
cc OUT2_HDF is an output routine for 2D.
c
c  Discussion:
c
c     # Output the results for a general system of conservation laws
c     # in 2 dimensions to a hdf file as a scientific data set.
c     # See http://hdf.ncsa.uiuc.edu/ for more info on HDF.
c     # The results in this output file can be plotted in MATLAB
c     # using the "plotclaw2" script.
c
c     # Revised 2003 by Peter Blossey
c     # Adapted code written by Sorin Mitran to standard F77 CLAWPACK style
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      parameter   (nDim = 2)
      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      character*14 fname
      character*13 qname2
      character*9  qname
c
c     # HDF: Declare variables that describe datasets and HDF files.
c
      integer    sd_id, sds_id, sds_rank
      integer    sds_dims, sds_start, sds_edges, sds_stride
      dimension  sds_dims(nDim), sds_start(nDim)
      dimension  sds_edges(nDim), sds_stride(nDim) 

c     # HDF: x- and y- dimensions are reversed when output to HDF file.
      dimension  qbuf(21), qout(my,mx)
c
c     # HDF: Declare external HDF functions
c
      integer  sfstart, sfcreate, sfwdata, sfscompress, sfendacc, sfend
      external sfstart, sfcreate, sfwdata, sfscompress, sfendacc, sfend
c
c     # HDF: Set up HDF constants
c
      integer 	DFACC_READ, DFACC_WRITE, DFACC_CREATE 
      parameter(DFACC_READ = 1, DFACC_WRITE = 2, DFACC_CREATE = 4)

      integer   DFNT_FLOAT64, DFNT_INT32
      parameter(DFNT_FLOAT64 = 6, DFNT_INT32 = 24)

      integer   SUCCEED, FAIL
      parameter(SUCCEED = 0, FAIL = -1)
c
c     # HDF: Set up compression constants for HDF file.
c
      integer   COMP_CODE_DEFLATE, DEFLATE_LEVEL
      parameter (COMP_CODE_DEFLATE = 4, DEFLATE_LEVEL = 6)
c
c     # Write the results to the file fort.q<iframe>
c     # Use format required by matlab script  plotclaw2.m
c     # The same format is used by the amrclaw package.
c     # Here it's adapted to output just the single grid.
c
c     # first create the file name and open file
c
      fname = 'fort.q'
     &     // char(ichar('0') + mod(iframe/1000,10)) 
     &     // char(ichar('0') + mod(iframe/100,10)) 
     &     // char(ichar('0') + mod(iframe/10,10)) 
     &     // char(ichar('0') + mod(iframe,10))
     &     // '.hdf'
c
c     # Specify grid number and create a string which will describe this
c     # grid in HDF file.  This could be an input for simulations with
c     # multiple grids, as in AMRCLAW.  
c
      ngrids_out=1
      qname = 'grid_'
     &     // char(ichar('0') + mod(ngrids_out/1000,10)) 
     &     // char(ichar('0') + mod(ngrids_out/100,10)) 
     &     // char(ichar('0') + mod(ngrids_out/10,10)) 
     &     // char(ichar('0') + mod(ngrids_out,10))
c
c     # HDF: create hdf file.
c
      sd_id = sfstart(fname, DFACC_CREATE)
      if (sd_id.eq.FAIL) THEN
         WRITE(*,*) 'Failed to create HDF file (call to sfstart)'
         STOP
      end if
c
c     # HDF: create a data set for parameters describing q in HDF file.
c
      sds_rank = 1
      sds_dims(1) = 21
      
      sds_id = sfcreate(sd_id,qname,DFNT_FLOAT64,sds_rank,sds_dims)
      if (sds_id.eq.FAIL) THEN
         WRITE(*,*) 'Failed to create scientific data set in HDF file'
         STOP
      end if
c
c     # HDF: set up parameters describing data set.
c
      sds_start(1)  = 0
      sds_edges(1)  = sds_dims(1)
      sds_stride(1) = 1        
      qbuf(1) = ngrids_out
      qbuf(2) = nDim
      qbuf(3) = t
      qbuf(4) = meqn
      qbuf(5) = 1.
      qbuf(6) = mx
      qbuf(7) = my
      qbuf(8) = 0.
      qbuf(9) = 0.
      qbuf(10) = xlower
      qbuf(11) = ylower
      qbuf(12) = 0.
      qbuf(13) = 0.
      qbuf(14) = xlower+mx*dx
      qbuf(15) = ylower+my*dy
      qbuf(16) = 0.
      qbuf(17) = 0.
      qbuf(18) = dx
      qbuf(19) = dy
      qbuf(20) = 0.
      qbuf(21) = 0.
      istat = sfwdata(sds_id,sds_start,sds_stride,sds_edges,qbuf)  
      istat = sfendacc(sds_id)  
c
c     # Loop over fields in q
c
      do m = 1,meqn
c
c     # HDF: create a data set for parameters describing q in HDF file.
c
         qname2 = qname // '_'
     &        // char(ichar('0') + mod(m/100,10)) 
     &        // char(ichar('0') + mod(m/10,10)) 
     &        // char(ichar('0') + mod(m,10))
c
c     # HDF: Reverse dimensions when storing arrays because of different
c     #      conventions between c and FORTRAN as to which dimension should
c     #      be stored first.  Reversing the dimensions here will make
c     #      the x-direction first when reading into MATLAB.
c
         sds_rank = nDim
         sds_dims(1) = my
         sds_dims(2) = mx
c      
         sds_id = sfcreate(sd_id,qname,DFNT_FLOAT64,sds_rank,sds_dims)
         if (sds_id.eq.FAIL) THEN
            WRITE(*,*) 'Failed to create data set in HDF file'
            STOP
         end if
c
c     # HDF: set up parameters describing data set.
c
         sds_start(1)  = 0
         sds_edges(1)  = sds_dims(1)
         sds_stride(1) = 1        

         sds_start(2)  = 0
         sds_edges(2)  = sds_dims(2)
         sds_stride(2) = 1        

c     # Copy current field of q into qout.
c
         do j = 1,mx
            do i = 1,my
               qout(i,j) = q(j,i,m)
            end do
         end do
c
c     # HDF: set compression mode and write data to hdf file.
c
         istat=sfscompress(sds_id,COMP_CODE_DEFLATE,DEFLATE_LEVEL)
         istat = sfwdata(sds_id,sds_start,sds_stride,sds_edges,qout)
         if (istat.eq.FAIL) then
            WRITE(*,*) 'Failed to write SDS (call to sfwdata)'
            STOP
         end if
c
c     # HDF: Close the data set
c
         istat = sfendacc(sds_id)  
         if (istat.eq.FAIL) then
            WRITE(*,*) 'Failed to close SDS (call to sfendacc)'
            STOP
         end if
      end do
c
c     # HDF: Close HDF file.
c
      istat = sfend(sd_id)
      if (istat.eq.FAIL) then
         WRITE(*,*) 'Failed to close ', fname, ' (call to sfend)'
         STOP
      end if

      return
      end
      subroutine out3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,
     &                 xlower,ylower,zlower,dx,dy,dz,q,t,iframe)

c*********************************************************************72
c
cc OUT3 is an output routine for 3D.
c
c  Discussion:
c
c     # Write the results to the file fort.q<iframe>
c     # Use format required by matlab script  plotclaw2.m
c     # The same format is used by the amrclaw package.
c     # Here it's adapted to output just the single grid.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &               1-mbc:maxmz+mbc, meqn)
      character*10 fname1, fname2
c
c     # first create the file name and open file
c

         fname1 = 'fort.qxxxx'
         fname2 = 'fort.txxxx'
         nstp = iframe
         do 55 ipos = 10, 7, -1
            idigit = mod(nstp,10)
            fname1(ipos:ipos) = char(ichar('0') + idigit)
            fname2(ipos:ipos) = char(ichar('0') + idigit)
            nstp = nstp / 10
 55      continue

         open(unit=50,file=fname1,status='unknown',form='formatted')
         open(unit=60,file=fname2,status='unknown',form='formatted')

c
c     # the following parameters are used in amrclaw where there are
c     # multiple grids.  Here they are all set to 1:
      ngrids = 1
      mptr = 1
      level = 1

      write(50,1001) mptr,level,mx,my,mz
 1001 format(i5,'                 grid_number',/,
     &       i5,'                 AMR_level',/,
     &       i5,'                 mx',/,
     &       i5,'                 my',/,
     &       i5,'                 mz')

      write(50,1002) xlower,ylower,zlower,dx,dy,dz
 1002 format(e26.16,'    xlow', /,
     &       e26.16,'    ylow', /,
     &       e26.16,'    zlow', /,
     &       e26.16,'    dx', /,
     &       e26.16,'    dy', /,
     &       e26.16,'    dz',/)
c
      do 30 k=1,mz
         do 20 j=1,my
            do 10 i=1,mx
               do m=1,meqn
c                 # exponents with more than 2 digits cause problems reading
c                 # into matlab... reset tiny values to zero:
                  if (dabs(q(i,j,k,m)) .lt. 1d-99) q(i,j,k,m) = 0.d0
               enddo
c
               write(50,1005) (q(i,j,k,m), m=1,meqn)
 1005          format(5e26.16)
c
   10       continue
            write(50,*) ' '
   20    continue
         write(50,*) ' '
   30 continue
      write(50,*) ' '

      write(60,1000) t,meqn,ngrids
 1000 format(e26.16,'    time', /,
     &       i5,'                 meqn'/,
     &       i5,'                 ngrids'/,/)
c

      close(unit=50)
      close(unit=60)

      return
      end
      subroutine out3_hdf(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,
     &                 xlower,ylower,zlower,dx,dy,dz,q,t,iframe)

c*********************************************************************72
c
cc OUT3_HDF is an output routine for 3D.
c
c  Discussion:
c
c     # Output the results for a general system of conservation laws
c     # in 3 dimensions to a hdf file as a scientific data set.
c     # See http://hdf.ncsa.uiuc.edu/ for more info on HDF.
c     # The results in this output file can be plotted in MATLAB
c     # using the "plotclaw3" script.
c
c     # Revised 2003 by Peter Blossey
c     # Adapted code written by Sorin Mitran to standard F77 CLAWPACK style
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      parameter   (nDim = 3)
      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &               1-mbc:maxmz+mbc, meqn)
      character*14 fname
      character*13 qname2
      character*9  qname
c
c     # HDF: Declare variables that describe datasets and HDF files.
c
      integer    sd_id, sds_id, sds_rank
      integer    sds_dims, sds_start, sds_edges, sds_stride
      dimension  sds_dims(nDim), sds_start(nDim)
      dimension  sds_edges(nDim), sds_stride(nDim) 

c     # HDF: x- and y- dimensions are reversed when output to HDF file.
      dimension  qbuf(21), qout(mz,my,mx)
c
c     # HDF: Declare external HDF functions
c
      integer  sfstart, sfcreate, sfwdata, sfscompress, sfendacc, sfend
      external sfstart, sfcreate, sfwdata, sfscompress, sfendacc, sfend
c
c     # HDF: Set up HDF constants
c
      integer 	DFACC_READ, DFACC_WRITE, DFACC_CREATE 
      parameter(DFACC_READ = 1, DFACC_WRITE = 2, DFACC_CREATE = 4)

      integer   DFNT_FLOAT64, DFNT_INT32
      parameter(DFNT_FLOAT64 = 6, DFNT_INT32 = 24)

      integer   SUCCEED, FAIL
      parameter(SUCCEED = 0, FAIL = -1)
c
c     # HDF: Set up compression constants for HDF file.
c
      integer   COMP_CODE_DEFLATE, DEFLATE_LEVEL
      parameter (COMP_CODE_DEFLATE = 4, DEFLATE_LEVEL = 6)
c
c     # Write the results to the file fort.q<iframe>
c     # Use format required by matlab script  plotclaw2.m
c     # The same format is used by the amrclaw package.
c     # Here it's adapted to output just the single grid.
c
c     # first create the file name and open file
c
      fname = 'fort.q'
     &     // char(ichar('0') + mod(iframe/1000,10)) 
     &     // char(ichar('0') + mod(iframe/100,10)) 
     &     // char(ichar('0') + mod(iframe/10,10)) 
     &     // char(ichar('0') + mod(iframe,10))
     &     // '.hdf'
c
c     # Specify grid number and create a string which will describe this
c     # grid in HDF file.  This could be an input for simulations with
c     # multiple grids, as in AMRCLAW.  
c
      ngrids_out=1
      qname = 'grid_'
     &     // char(ichar('0') + mod(ngrids_out/1000,10)) 
     &     // char(ichar('0') + mod(ngrids_out/100,10)) 
     &     // char(ichar('0') + mod(ngrids_out/10,10)) 
     &     // char(ichar('0') + mod(ngrids_out,10))
c
c     # HDF: create hdf file.
c
      sd_id = sfstart(fname, DFACC_CREATE)
      if (sd_id.eq.FAIL) THEN
            WRITE(*,*) 'Failed to create ', fname,' (call to sfstart)'
         STOP
      end if
c
c     # HDF: create a data set for parameters describing q in HDF file.
c
      sds_rank = 1
      sds_dims(1) = 21
      
      sds_id = sfcreate(sd_id,qname,DFNT_FLOAT64,sds_rank,sds_dims)
      if (sds_id.eq.FAIL) THEN
         WRITE(*,*) 'Failed to create scientific data set in HDF file'
         STOP
      end if
c
c     # HDF: set up parameters describing data set.
c
      sds_start(1)  = 0
      sds_edges(1)  = sds_dims(1)
      sds_stride(1) = 1        
      qbuf(1) = ngrids_out
      qbuf(2) = nDim
      qbuf(3) = t
      qbuf(4) = meqn
      qbuf(5) = 1.
      qbuf(6) = mx
      qbuf(7) = my
      qbuf(8) = mz
      qbuf(9) = 0.
      qbuf(10) = xlower
      qbuf(11) = ylower
      qbuf(12) = zlower
      qbuf(13) = 0.
      qbuf(14) = xlower+mx*dx
      qbuf(15) = ylower+my*dy
      qbuf(16) = zlower+mz*dz
      qbuf(17) = 0.
      qbuf(18) = dx
      qbuf(19) = dy
      qbuf(20) = dz
      qbuf(21) = 0.
      istat = sfwdata(sds_id,sds_start,sds_stride,sds_edges,qbuf)  
      istat = sfendacc(sds_id)  
c
c     # Loop over fields in q
c
      do m = 1,meqn
c
c     # HDF: create a data set for parameters describing q in HDF file.
c
         qname2 = qname // '_'
     &        // char(ichar('0') + mod(m/100,10)) 
     &        // char(ichar('0') + mod(m/10,10)) 
     &        // char(ichar('0') + mod(m,10))
c
c     # HDF: Reverse dimensions when storing arrays because of different
c     #      conventions between c and FORTRAN as to which dimension should
c     #      be stored first.  Reversing the dimensions here will make
c     #      the x-direction first when reading into MATLAB.
c
         sds_rank = nDim
         sds_dims(1) = mz
         sds_dims(2) = my
         sds_dims(3) = mx
c      
         sds_id = sfcreate(sd_id,qname,DFNT_FLOAT64,sds_rank,sds_dims)
         if (sds_id.eq.FAIL) THEN
            WRITE(*,*) 'Failed to create data set in HDF file'
            STOP
         end if
c
c     # HDF: set up parameters describing data set.
c
         sds_start(1)  = 0
         sds_edges(1)  = sds_dims(1)
         sds_stride(1) = 1        

         sds_start(2)  = 0
         sds_edges(2)  = sds_dims(2)
         sds_stride(2) = 1        

         sds_start(3)  = 0
         sds_edges(3)  = sds_dims(3)
         sds_stride(3) = 1        
c
c     # Copy current field of q into qout.
c
         do k = 1,mx
            do j = 1,my
               do i = 1,mz
                  qout(i,j,k) = q(k,j,i,m)
               end do
            end do
         end do
c
c     # HDF: set compression mode and write data to hdf file.
c
         istat=sfscompress(sds_id,COMP_CODE_DEFLATE,DEFLATE_LEVEL)
         istat = sfwdata(sds_id,sds_start,sds_stride,sds_edges,qout)
         if (istat.eq.FAIL) then
            WRITE(*,*) 'Failed to write SDS (call to sfwdata)'
            STOP
         end if
c
c     # HDF: Close the data set
c
         istat = sfendacc(sds_id)  
         if (istat.eq.FAIL) then
            WRITE(*,*) 'Failed to close SDS (call to sfendacc)'
            STOP
         end if
      end do
c
c     # HDF: Close HDF file.
c
      istat = sfend(sd_id)
      if (istat.eq.FAIL) then
         WRITE(*,*) 'Failed to close ', fname, ' (call to sfend)'
         STOP
      end if

      return
      end
      double precision function philim(a,b,meth)

c*********************************************************************72
c
cc PHILIM computes a limiter based on wave strengths.
c
c  Discussion:
c
c    meth determines what limiter is used.
c    a is assumed to be nonzero.
c
c    NOTE: This routine is obsolete.  Instead of using limiter.f,
c    which calls philim.f for every wave, it is more efficient to 
c    use inlinelimiter.f, which eliminates all these function calls
c    to philim.  If you wish to change the limiter function and are
c    using inlinelimiter.f, the formulas must be changed in that routine.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      r = b/a
      go to (10,20,30,40,50) meth

   10 continue
c
c     # minmod
c
      philim = dmax1(0.d0, dmin1(1.d0, r))
      return
c
   20 continue
c
c     # superbee
c
      philim = dmax1(0.d0, dmin1(1.d0, 2.d0*r), dmin1(2.d0, r))
      return
c
   30 continue
c
c     # van Leer
c
      philim = (r + dabs(r)) / (1.d0 + dabs(r))
      return
c
   40 continue
c
c     # monotinized centered 
c
      c = (1.d0 + r)/2.d0
      philim = dmax1(0.d0, dmin1(c, 2.d0, 2.d0*r))
      return
c
   50 continue
c
c     # Beam-Warming
c
      philim = r

      return
      end
      subroutine restart_another (
     &  maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,
     &      xlower,ylower,zlower,dx,dy,dz,q)

c*********************************************************************72
c
cc RESTART_ANOTHER sets initial conditions for q.
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &      1-mbc:maxmz+mbc, meqn)
      character*10 fname1, fname2
      logical outt0
      common /restrt_block/ t0, iframe, outt0

      iunit = 16

c     # first create the file name and open file
      fname1 = 'fort.q'
     &     // char(ichar('0') + mod(iframe/1000,10)) 
     &     // char(ichar('0') + mod(iframe/100,10)) 
     &     // char(ichar('0') + mod(iframe/10,10)) 
     &     // char(ichar('0') + mod(iframe,10))
      open(iunit,file=fname1)

c     # Read grid parameters.
      read(iunit,*) igrid
      read(iunit,*) level
      read(iunit,*) mx_in
      read(iunit,*) my_in
      read(iunit,*) mz_in
      read(iunit,*) xlow_in
      read(iunit,*) ylow_in
      read(iunit,*) zlow_in
      read(iunit,*) dx_in
      read(iunit,*) dy_in
      read(iunit,*) dz_in

c     # Test for compatibility of grid resolution.
      if (mx_in .ne. mx .or. my_in .ne. my .or. mz_in .ne. mz) then
         stop 'rstart.f : data not compatible'
      end if

c     # Read variables in from old fort.qXXXX file.
      do k = 1,mz
         do j = 1,my
            do i = 1,mx
               read(iunit,*) (q(i,j,k,m),m=1,meqn)
            enddo
         enddo
      enddo
      close(iunit)

c     # Read initial time in from fort.tXXXX file.      
      fname2 = 'fort.t'
     &     // char(ichar('0') + mod(iframe/1000,10)) 
     &     // char(ichar('0') + mod(iframe/100,10)) 
     &     // char(ichar('0') + mod(iframe/10,10)) 
     &     // char(ichar('0') + mod(iframe,10))
      open(iunit,file=fname2)
      read(iunit,*) t0
      close(iunit)

c     # Turn off output from initial condition so that restart fort.qXXXX
c     # file will not be overwritten.
      outt0 = .false.

      write(*,*) 'Restarting from old output file ', fname1
      write(*,*) 'Simulation will be restarted at time t = ', t0
      write(*,*) 'Inital condition will not be output to a matlab ',
     &     'plot file'
      write(*,*) 

      return
      end
      subroutine restart(maxmx,maxmy,meqn,mbc,mx,my,
     &      xlower,ylower,dx,dy,q)

c*********************************************************************72
c
cc RESTART sets initial conditions for q.
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      character*10 fname1, fname2
      logical outt0
      common /restrt_block/ tinitial, iframe, outt0

      iunit = 16

c     # first create the file name and open file
      fname1 = 'fort.q'
     &     // char(ichar('0') + mod(iframe/1000,10)) 
     &     // char(ichar('0') + mod(iframe/100,10)) 
     &     // char(ichar('0') + mod(iframe/10,10)) 
     &     // char(ichar('0') + mod(iframe,10))
      open(iunit,file=fname1)

c     # Read grid parameters.
      read(iunit,*) igrid
      read(iunit,*) level
      read(iunit,*) mx_in
      read(iunit,*) my_in
      read(iunit,*) xlow_in
      read(iunit,*) ylow_in
      read(iunit,*) dx_in
      read(iunit,*) dy_in

c     # Test for compatibility of grid resolution.
      if (mx_in .ne. mx .or. my_in .ne. my) then
         stop 'rstart.f : data not compatible'
      end if

c     # Read variables in from old fort.qXXXX file.
      do j = 1,my
         do i = 1,mx
            read(iunit,*) (q(i,j,m),m=1,meqn)
         enddo
      enddo
      close(iunit)

c     # Read initial time in from fort.tXXXX file.      
      fname2 = 'fort.t'
     &     // char(ichar('0') + mod(iframe/1000,10)) 
     &     // char(ichar('0') + mod(iframe/100,10)) 
     &     // char(ichar('0') + mod(iframe/10,10)) 
     &     // char(ichar('0') + mod(iframe,10))
      open(iunit,file=fname2)
      read(iunit,*) tinitial
      close(iunit)

c     # Turn off output from initial condition so that restart fort.qXXXX
c     # file will not be overwritten.
      outt0 = .false.

      write(*,*) 'Restarting from old output file ', fname1
      write(*,*) 'Simulation will be restarted at time t = ', tinitial
      write(*,*) 'Inital condition will not be output to a matlab ',
     &     'plot file'
      write(*,*) 

      return
      end
      subroutine restart_hdf(maxmx,maxmy,meqn,mbc,mx,my,
     &     xlower,ylower,dx,dy,q)

c*********************************************************************72
c    
cc RESTART_HDF initializes Q using an old HDF output file.
c
c  Discussion:
c
c     # Initialize q using values from an old HDF output file.
c     # Copy the HDF output file to restart.data.hdf
c     # and call this routine from qinit.
c     
c     # See http://hdf.ncsa.uiuc.edu/ for more info on HDF.
c     
c     # Written 2003 by Peter Blossey 
c     
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      parameter   (nDim = 2)
      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      character*14 fname
c     
c     # HDF: Declare variables that describe datasets and HDF files.
c     
      integer    sd_id, sds_id, sds_start, sds_edges, sds_stride
      dimension  sds_start(nDim), sds_edges(nDim), sds_stride(nDim) 

c     # HDF: x- and y- dimensions are reversed when output to HDF file.
      dimension  qbuf(21), qout(my,mx)
c     
c     # HDF: Declare external HDF functions
c     
      integer  sfstart, sfselect, sfrdata, sfendacc, sfend
      external sfstart, sfselect, sfrdata, sfendacc, sfend
c     
c     # HDF: Set up HDF constants
c     
      integer 	DFACC_READ
      parameter(DFACC_READ = 1)

      integer   SUCCEED, FAIL
      parameter(SUCCEED = 0, FAIL = -1)
c
      logical   outt0
      common /restrt_block/ tinitial, iframe, outt0
c     
c     # first create the file name and open file
c     
      fname = 'fort.q'
     &     // char(ichar('0') + mod(iframe/1000,10)) 
     &     // char(ichar('0') + mod(iframe/100,10)) 
     &     // char(ichar('0') + mod(iframe/10,10)) 
     &     // char(ichar('0') + mod(iframe,10))
     &     // '.hdf'
      write(*,*) 'Restarting from ', fname
c     
c     # HDF: open hdf restart file.
c     
      sd_id = sfstart(fname,DFACC_READ)
      if (sd_id.eq.FAIL) THEN
         WRITE(*,*) 'Failed to open HDF file',
     &        ' (call to sfstart in restart_hdf.f)'
         STOP
      end if
c     
c     # HDF: Select grid parameter dataset in HDF file.
c     
      index = 0
      sds_id = sfselect(sd_id,index)
      if (sds_id.eq.FAIL) THEN
         WRITE(*,*) 'Failed to select data set for  variable ', index,
     &        ' in restart HDF file'
         WRITE(*,*) '(call to sfselect in restrt_hdf.f)'
         STOP
      end if
c     
c     # HDF: Set up dimensions for double vector.
c     
      sds_start(1) = 0
      sds_edges(1) = 21
      sds_stride(1) = 1
c     
c     # HDF: read double vector from hdf file.
c     
      istat = sfrdata(sds_id,sds_start,sds_stride,sds_edges,qbuf)  
      if (istat.eq.FAIL) THEN
         WRITE(*,*) 'Failed to read variable ', index,
     &        ' from restart HDF file'
         WRITE(*,*) '(call to sfrdata in restrt_hdf.f)'
         STOP
      end if
c     
c     # HDF: End access to double vector in HDF file.
c     
      istat = sfendacc(sds_id)  
      if (istat.eq.FAIL) THEN
         WRITE(*,*) 'Failed to end access to variable ', index,
     &        ' in restart HDF file'
         WRITE(*,*) '(call to sfendacc in restrt_hdf.f)'
         STOP
      end if
c     
c     # Check parameters against those read in from claw3ez.data
c     
      mx_in = qbuf(6)
      my_in = qbuf(7)
      if (mx_in .ne. mx .or. my_in .ne. my) then
         stop 'rstart.f : grid dimensions not compatible'
      end if
c     
      meqn_in = qbuf(4)
      if (meqn_in .ne. meqn) then
         stop 'rstart.f : meqn not compatible'
      end if
c
c     # Read in new starting time.
c
      tinitial = qbuf(3)
c
c     # Since we are starting from an old field, don't write out initial
c     # conditions since it will just be a copy of the old file.
c
      outt0 = .false.
      write(*,*) 'Restarting from file ', fname, ' at time ', tinitial
      write(*,*) 'Initial condition will not be written to a MATLAB ',
     &     'output file'
      write(*,*)
c     
c     # Loop over fields in q
c     
      do m = 1,meqn
c     
c     # HDF: Select dataset in HDF file.
c     
         index = index + 1
         sds_id = sfselect(sd_id,index)
         if (sds_id.eq.FAIL) THEN
            WRITE(*,*) 'Failed to select data set number ', index,
     &           ' in restart HDF file'
            WRITE(*,*) '(call to sfselect in restrt_hdf.f)'
            STOP
         end if
c     
c     # HDF: Set up dimensions for double array.
c     
         sds_start(1) = 0
         sds_start(2) = 0

         sds_edges(1) = my
         sds_edges(2) = mx

         sds_stride(1) = 1
         sds_stride(2) = 1
c     
c     # HDF: read double array from hdf file.
c     
         istat = sfrdata(sds_id,sds_start,sds_stride,sds_edges,qout)  
         if (istat.eq.FAIL) THEN
            WRITE(*,*) 'Failed to read variable ', index,
     &           ' from restart HDF file'
            WRITE(*,*) '(call to sfrdata in restrt_hdf.f)'
            STOP
         end if
c     
c     # HDF: End access to double array in HDF file.
c     
         istat = sfendacc(sds_id)  
         if (istat.eq.FAIL) THEN
            WRITE(*,*) 'Failed to end access to variable ', index,
     &           ' in restart HDF file'
            WRITE(*,*) '(call to sfendacc in restrt_hdf.f)'
            STOP
         end if
c     
c     # Put the data into q(:,:,m).
c
         do j = 1,my
            do i = 1,mx
               q(i,j,m) = qout(j,i)
            end do
         end do
c     
      end do
c     
c     # HDF: Close HDF file.
c
      istat = sfend(sd_id)
      if (istat.eq.FAIL) then
         WRITE(*,*) 'Failed to close SDS (call to sfend)'
         STOP
      end if

      return
      end
      subroutine restart_more (
     &  maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,
     &     xlower,ylower,zlower,dx,dy,dz,q)

c*********************************************************************72
c    
cc RESTART_MORE initializes Q using values from an old HDF output file.
c
c  Discussion:
c
c     # Copy the HDF output file to restart.data.hdf
c     # and call this routine from qinit.
c     
c     # See http://hdf.ncsa.uiuc.edu/ for more info on HDF.
c     
c     # Written 2003 by Peter Blossey 
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c     
      implicit double precision (a-h,o-z)

      parameter   (nDim = 3)
      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &     1-mbc:maxmz+mbc, meqn)
      character*14 fname
c     
c     # HDF: Declare variables that describe datasets and HDF files.
c     
      integer    sd_id, sds_id, sds_start, sds_edges, sds_stride
      dimension  sds_start(nDim), sds_edges(nDim), sds_stride(nDim) 

c     # HDF: x- and y- dimensions are reversed when output to HDF file.
      dimension  qbuf(21), qout(mz,my,mx)
c     
c     # HDF: Declare external HDF functions
c     
      integer  sfstart, sfselect, sfrdata, sfendacc, sfend
      external sfstart, sfselect, sfrdata, sfendacc, sfend
c     
c     # HDF: Set up HDF constants
c     
      integer 	DFACC_READ
      parameter(DFACC_READ = 1)

      integer   SUCCEED, FAIL
      parameter(SUCCEED = 0, FAIL = -1)
c
      logical   outt0
      common /restrt_block/ t0, iframe, outt0
c     
c     # first create the file name and open file
c     
      fname = 'fort.q'
     &     // char(ichar('0') + mod(iframe/1000,10)) 
     &     // char(ichar('0') + mod(iframe/100,10)) 
     &     // char(ichar('0') + mod(iframe/10,10)) 
     &     // char(ichar('0') + mod(iframe,10))
     &     // '.hdf'
      write(*,*) 'Restarting from ', fname
c     
c     # HDF: open hdf restart file.
c     
      sd_id = sfstart(fname,DFACC_READ)
      if (sd_id.eq.FAIL) THEN
         WRITE(*,*) 'Failed to open HDF file',
     &        ' (call to sfstart in restart_hdf.f)'
         STOP
      end if
c     
c     # HDF: Select grid parameter dataset in HDF file.
c     
      index = 0
      sds_id = sfselect(sd_id,index)
      if (sds_id.eq.FAIL) THEN
         WRITE(*,*) 'Failed to select data set for  variable ', index,
     &        ' in restart HDF file'
         WRITE(*,*) '(call to sfselect in restrt_hdf.f)'
         STOP
      end if
c     
c     # HDF: Set up dimensions for double vector.
c     
      sds_start(1) = 0
      sds_edges(1) = 21
      sds_stride(1) = 1
c     
c     # HDF: read double vector from hdf file.
c     
      istat = sfrdata(sds_id,sds_start,sds_stride,sds_edges,qbuf)  
      if (istat.eq.FAIL) THEN
         WRITE(*,*) 'Failed to read variable ', index,
     &        ' from restart HDF file'
         WRITE(*,*) '(call to sfrdata in restrt_hdf.f)'
         STOP
      end if
c     
c     # HDF: End access to double vector in HDF file.
c     
      istat = sfendacc(sds_id)  
      if (istat.eq.FAIL) THEN
         WRITE(*,*) 'Failed to end access to variable ', index,
     &        ' in restart HDF file'
         WRITE(*,*) '(call to sfendacc in restrt_hdf.f)'
         STOP
      end if
c     
c     # Check parameters against those read in from claw3ez.data
c     
      mx_in = qbuf(6)
      my_in = qbuf(7)
      mz_in = qbuf(8)
      if (mx_in .ne. mx .or. my_in .ne. my .or. mz_in .ne. mz) then
         stop 'rstart.f : grid dimensions not compatible'
      end if
c     
      meqn_in = qbuf(4)
      if (meqn_in .ne. meqn) then
         stop 'rstart.f : meqn not compatible'
      end if
c
c     # Read in new starting time.
c
      t0 = qbuf(3)
c
c     # Since we are starting from an old field, don't write out initial
c     # conditions since it will just be a copy of the old file.
c
      outt0 = .false.
      write(*,*) 'Restarting from file ', fname, ' at time ', t0
      write(*,*) 'Initial condition will not be written to a MATLAB ',
     &     'output file'
      write(*,*)
c     
c     # Loop over fields in q
c     
      do m = 1,meqn
c     
c     # HDF: Select dataset in HDF file.
c     
         index = index + 1
         sds_id = sfselect(sd_id,index)
         if (sds_id.eq.FAIL) THEN
            WRITE(*,*) 'Failed to select data set number ', index,
     &           ' in restart HDF file'
            WRITE(*,*) '(call to sfselect in restrt_hdf.f)'
            STOP
         end if
c     
c     # HDF: Set up dimensions for double array.
c     
         sds_start(1) = 0
         sds_start(2) = 0
         sds_start(3) = 0

         sds_edges(1) = mz
         sds_edges(2) = my
         sds_edges(3) = mx

         sds_stride(1) = 1
         sds_stride(2) = 1
         sds_stride(3) = 1
c     
c     # HDF: read double array from hdf file.
c     
         istat = sfrdata(sds_id,sds_start,sds_stride,sds_edges,qout)  
         if (istat.eq.FAIL) THEN
            WRITE(*,*) 'Failed to read variable ', index,
     &           ' from restart HDF file'
            WRITE(*,*) '(call to sfrdata in restrt_hdf.f)'
            STOP
         end if
c     
c     # HDF: End access to double array in HDF file.
c     
         istat = sfendacc(sds_id)  
         if (istat.eq.FAIL) THEN
            WRITE(*,*) 'Failed to end access to variable ', index,
     &           ' in restart HDF file'
            WRITE(*,*) '(call to sfendacc in restrt_hdf.f)'
            STOP
         end if
c     
c     # Put the data into q(:,:,:,m).
c
         do k = 1,mz
            do j = 1,my
               do i = 1,mx
                  q(i,j,k,m) = qout(k,j,i)
               end do
            end do
         end do
c     
      end do
c     
c     # HDF: Close HDF file.
c
      istat = sfend(sd_id)
      if (istat.eq.FAIL) then
         WRITE(*,*) 'Failed to close SDS (call to sfend)'
         STOP
      end if

      return
      end
      subroutine rstrt2(maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &           q,t,dx,dy,dtv,cflv,nv,method,mthlim,info)

c*********************************************************************72
c
cc RSTRT2 reads in the data needed for a restart
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension dtv(5),cflv(4),nv(2),method(7),mthlim(mwaves)
c
      read(99,*) mx,my
      read(99,*) dx,dy
      read(99,*) t
      read(99,*) dtv
      read(99,*) cflv
      read(99,*) nv
      read(99,*) method
      read(99,*) mthlim
      read(99,*) info
c
      do 12 m=1,meqn
         do 11 j=1,my
            do 10 i=1,mx
               read(99,*) q(i,j,m)
   10       continue
   11    continue
   12 continue
      return
      end
      subroutine setaux1(maxmx,mbc,mx,xlower,dx,maux,aux)

c*********************************************************************72
c
cc SETAUX1 sets auxiliary arrays for 1D.
c
c  Discussion:
c     
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension aux(1-mbc:maxmx+mbc, *)

      return
      end
      subroutine setaux2(maxmx,maxmy,mbc,mx,my,xlower,ylower,dx,dy,
     &                  maux,aux)

c*********************************************************************72
c
cc SETAUX2 sets auxiliary arrays for 2D.
c
c  Discussion:
c     
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

c     dimension aux(1-mbc:maxmx+mbc,1-mbc:maxmy+mbc, maux)
c
      return
      end
      subroutine setaux3(maxmx,maxmy,maxmz,mbc,mx,my,mz,xlower,ylower,
     &                  zlower,dx,dy,dz,maux,aux)

c*********************************************************************72
c
cc SETAUX3 sets auxiliary arrays for 3D.
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension  aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 
     &               1-mbc:maxmz+mbc, maux)
      return
      end
      subroutine setprob

c*********************************************************************72
c
cc SETPROB can be used to set up problem parameters or read other data.
c
c  Discussion:
c
c    The user may supply a SETPROB routine, which will then be used instead of
c    this one.  In that case, the user's SETPROB routine should be used to
c    initialize various problem parameters, perhaps by reading data from a file,
c    and transmitting that data to other routines, perhaps through a common block.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      return
      end
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)

c*********************************************************************72
c
cc SRC1 sets the source term for 1D.
c
c  Discussion:
c
c     do nothing... no source term
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, meqn)

      return
      end
      subroutine src2(maxmx,maxmy,meqn,mbc,mx,my,xlower,ylower,
     &                 dx,dy,q,maux,aux,t,dt)

c*********************************************************************72
c
cc SRC2 sets the source term for 2D.
c
c  Discussion:
c
c      # dummy subroutine for use when equation has no source term.
c      # If method(5)=0 then this routine is never called, but its
c      # existence may be required by some compilers.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
       implicit double precision (a-h,o-z)

       dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)

       return
       end
       subroutine src3(maxmx,maxmy,maxmz,meqn,mbc,mx,my,mz,
     &                 xlower,ylower,zlower,dx,dy,dz,q,maux,aux,t,dt)

c*********************************************************************72
c
cc SRC3 sets the source term for 3D.
c
c  Discussion:
c
c      # dummy subroutine for use when equation has no source term.
c      # If method(5)=0 then this routine is never called, but its
c      # existence may be required by some compilers.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension    q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, 
     &               1-mbc:maxmz+mbc, meqn)

      return
      end
      subroutine step1(maxmx,meqn,mwaves,mbc,mx,q,aux,dx,dt,
     &              method,mthlim,cfl,f,wave,s,amdq,apdq,dtdx,rp1)

c*********************************************************************72
c
cc STEP1 takes one time step, updating Q, in 1D.
c
c  Discussion:
c
c     method(1) = 1   ==>  Godunov method
c     method(1) = 2   ==>  Slope limiter method
c     mthlim(p)  controls what limiter is used in the pth family
c
c
c     amdq, apdq, wave, s, and f are used locally:
c
c     amdq(1-mbc:maxmx+mbc, meqn) = left-going flux-differences
c     apdq(1-mbc:maxmx+mbc, meqn) = right-going flux-differences
c        e.g. amdq(i,m) = m'th component of A^- \Delta q from i'th Riemann
c                         problem (between cells i-1 and i).
c
c     wave(1-mbc:maxmx+mbc, meqn, mwaves) = waves from solution of
c                                           Riemann problems,
c            wave(i,m,mw) = mth component of jump in q across
c                           wave in family mw in Riemann problem between
c                           states i-1 and i.
c
c     s(1-mbc:maxmx+mbc, mwaves) = wave speeds,
c            s(i,mw) = speed of wave in family mw in Riemann problem between
c                      states i-1 and i.
c
c     f(1-mbc:maxmx+mbc, meqn) = correction fluxes for second order method
c            f(i,m) = mth component of flux at left edge of ith cell 
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension    q(1-mbc:maxmx+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, *)
      dimension    f(1-mbc:maxmx+mbc, meqn)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)
      dimension dtdx(1-mbc:maxmx+mbc)
      dimension method(7),mthlim(mwaves)
      logical limit
c
c     # check if any limiters are used:
      limit = .false.
      do 5 mw=1,mwaves
         if (mthlim(mw) .gt. 0) limit = .true.
   5     continue
c
      mcapa = method(6)
      do 10 i=1-mbc,mx+mbc
         if (mcapa.gt.0) then
             if (aux(i,mcapa) .le. 0.d0) then
                write(6,*) 'Error -- capa must be positive'
                stop
                end if
             dtdx(i) = dt / (dx*aux(i,mcapa))
            else
             dtdx(i) = dt/dx
            end if
   10    continue
c
c
c
c     # solve Riemann problem at each interface 
c
c
      call rp1(maxmx,meqn,mwaves,mbc,mx,q,q,aux,aux,wave,s,amdq,apdq)
c
c     # Modify q for Godunov update:
c     # Note this may not correspond to a conservative flux-differencing
c     # for equations not in conservation form.  It is conservative if
c     # amdq + apdq = f(q(i)) - f(q(i-1)).
c
      do 40 i=1,mx+1
         do 40 m=1,meqn
            q(i,m) = q(i,m) - dtdx(i)*apdq(i,m)
            q(i-1,m) = q(i-1,m) - dtdx(i-1)*amdq(i,m)
   40       continue

c
c     # compute maximum wave speed:
      cfl = 0.d0
      do 50 mw=1,mwaves
         do 45 i=1,mx+1
c          # if s>0 use dtdx(i) to compute CFL,
c          # if s<0 use dtdx(i-1) to compute CFL:
           cfl = dmax1(cfl, dtdx(i)*s(i,mw), -dtdx(i-1)*s(i,mw))
   45      continue
   50    continue
c
      if (method(2) .eq. 1) go to 900
c
c     # compute correction fluxes for second order q_{xx} terms:
c
c
      do 100 m = 1, meqn
            do 100 i = 1-mbc, mx+mbc
               f(i,m) = 0.d0
  100          continue
c
c      # apply limiter to waves:
      if (limit) call limiter(maxmx,meqn,mwaves,mbc,mx,wave,s,mthlim)
c
      do 120 i=1,mx+1
         do 120 m=1,meqn
            do 110 mw=1,mwaves
               dtdxave = 0.5d0 * (dtdx(i-1) + dtdx(i))
               f(i,m) = f(i,m) + 0.5d0 * dabs(s(i,mw))
     &             * (1.d0 - dabs(s(i,mw))*dtdxave) * wave(i,m,mw)
  110          continue
  120       continue
c
c
  140 continue
c
c     # update q by differencing correction fluxes 
c
c
c     # (Note:  Godunov update has already been performed above)
c
      do 150 m=1,meqn
         do 150 i=1,mx
            q(i,m) = q(i,m) - dtdx(i) * (f(i+1,m) - f(i,m))
  150       continue
c
  900 continue
      return
      end
      subroutine step1fw(maxmx,meqn,mwaves,mbc,mx,q,aux,dx,dt,
     &              method,mthlim,cfl,f,fwave,s,amdq,apdq,dtdx,rp1)

c*********************************************************************72
c
cc STEP1FW takes one time step, updating Q, in 1D.
c
c  Discussion:
c
c     # step1fw is a modified version of step1 to use fwave instead of wave.
c     # A modified Riemann solver rp1 must be used in conjunction with this
c     # routine, which returns fwave's instead of wave's.
c     # See http://amath.washington.edu/~claw/fwave.html
c
c     # Limiters are applied to the fwave's, and the only significant
c     # modification of this code is in the "do 110" loop, for the 
c     # second order corrections.
c
c
c     method(1) = 1   ==>  Godunov method
c     method(1) = 2   ==>  Slope limiter method
c     mthlim(p)  controls what limiter is used in the pth family
c
c
c     amdq, apdq, fwave, s, and f are used locally:
c
c     amdq(1-mbc:maxmx+mbc, meqn) = left-going flux-differences
c     apdq(1-mbc:maxmx+mbc, meqn) = right-going flux-differences
c        e.g. amdq(i,m) = m'th component of A^- \Delta q from i'th Riemann
c                         problem (between cells i-1 and i).
c
c     fwave(1-mbc:maxmx+mbc, meqn, mwaves) = waves from solution of
c                                           Riemann problems,
c            fwave(i,m,mw) = mth component of jump in f across
c                           wave in family mw in Riemann problem between
c                           states i-1 and i.
c
c     s(1-mbc:maxmx+mbc, mwaves) = wave speeds,
c            s(i,mw) = speed of wave in family mw in Riemann problem between
c                      states i-1 and i.
c
c     f(1-mbc:maxmx+mbc, meqn) = correction fluxes for second order method
c            f(i,m) = mth component of flux at left edge of ith cell 
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension    q(1-mbc:maxmx+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, *)
      dimension    f(1-mbc:maxmx+mbc, meqn)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension fwave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)
      dimension dtdx(1-mbc:maxmx+mbc)
      dimension method(7),mthlim(mwaves)
      logical limit
c
c     # check if any limiters are used:
      limit = .false.
      do 5 mw=1,mwaves
	 if (mthlim(mw) .gt. 0) limit = .true.
   5     continue
c
      mcapa = method(6)
      do 10 i=1-mbc,mx+mbc
	 if (mcapa.gt.0) then
	     if (aux(i,mcapa) .le. 0.d0) then
		write(6,*) 'Error -- capa must be positive'
		stop
		end if
             dtdx(i) = dt / (dx*aux(i,mcapa))
	    else
             dtdx(i) = dt/dx
	    end if
   10	 continue
c
c
c
c     # solve Riemann problem at each interface 
c
c
      call rp1(maxmx,meqn,mwaves,mbc,mx,q,q,aux,aux,fwave,s,amdq,apdq)
c
c     # Modify q for Godunov update:
c     # Note this may not correspond to a conservative flux-differencing
c     # for equations not in conservation form.  It is conservative if
c     # amdq + apdq = f(q(i)) - f(q(i-1)).
c
      do 40 i=1,mx+1
         do 39 m=1,meqn
            q(i,m) = q(i,m) - dtdx(i)*apdq(i,m)
            q(i-1,m) = q(i-1,m) - dtdx(i-1)*amdq(i,m)
   39       continue
   40    continue

c
c     # compute maximum wave speed:
      cfl = 0.d0
      do 50 mw=1,mwaves
	 do 45 i=1,mx+1
c          # if s>0 use dtdx(i) to compute CFL,
c          # if s<0 use dtdx(i-1) to compute CFL:
	   cfl = dmax1(cfl, dtdx(i)*s(i,mw), -dtdx(i-1)*s(i,mw))
   45      continue
   50    continue
c
      if (method(2) .eq. 1) go to 900
c
c     # compute correction fluxes for second order q_{xx} terms:
c
c
      do 100 m = 1, meqn
            do 100 i = 1-mbc, mx+mbc
               f(i,m) = 0.d0
  100          continue
c
c     # apply limiter to waves:
      if (limit) then
         call limiter(maxmx,meqn,mwaves,mbc,mx,fwave,s,mthlim)
         end if

c
      do 120 i=1,mx+1
	 do 120 m=1,meqn
	    do 110 mw=1,mwaves
	       dtdxave = 0.5d0 * (dtdx(i-1) + dtdx(i))
	       f(i,m) = f(i,m) + 0.5d0 * dsign(1.d0,s(i,mw))
     &		   * (1.d0 - dabs(s(i,mw))*dtdxave) * fwave(i,m,mw)
c

  110          continue
  120       continue
c
c
  140 continue
c
c     # update q by differencing correction fluxes 
c
c
c     # (Note:  Godunov update has already been performed above)
c
      do 150 m=1,meqn
	 do 150 i=1,mx
	    q(i,m) = q(i,m) - dtdx(i) * (f(i+1,m) - f(i,m))
  150       continue
c
  900 continue
      return
      end
      subroutine step2ds(maxm,maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &               qold,qnew,aux,dx,dy,dt,method,mthlim,cfl,
     &               qadd,fadd,gadd,q1d,dtdx1d,dtdy1d,
     &                 aux1,aux2,aux3,work,mwork,rpn2,rpt2,ids)

c*********************************************************************72
c
cc STEP2DS takes one time step, updating Q, in 2D.
c
c  Discussion:
c
c     # On entry, qold and qnew should be identical and give the
c     #    initial data for this step
c     # On exit, qnew returns values at the end of the time step.
c     #    qold is unchanged.
c    
c     # qadd is used to return increments to q from flux2
c     # fadd and gadd are used to return flux increments from flux2.
c     # See the flux2 documentation for more information.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn2,rpt2
      dimension qold(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension qnew(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension  q1d(1-mbc:maxm+mbc, meqn)
      dimension qadd(1-mbc:maxm+mbc, meqn)
      dimension fadd(1-mbc:maxm+mbc, meqn)
      dimension gadd(1-mbc:maxm+mbc, meqn, 2)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, *)
      dimension aux1(1-mbc:maxm+mbc, *)
      dimension aux2(1-mbc:maxm+mbc, *)
      dimension aux3(1-mbc:maxm+mbc, *)

      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension dtdy1d(1-mbc:maxm+mbc)
      dimension method(7),mthlim(mwaves)
      dimension work(mwork)

      common /comxyt/ dtcom,dxcom,dycom,tcom,icom,jcom
c
c
c
c     # partition work array into pieces needed for local storage in
c     # flux2 routine.  Find starting index of each piece:
c
      i0wave = 1
      i0s = i0wave + (maxm+2*mbc)*meqn*mwaves
      i0amdq = i0s + (maxm+2*mbc)*mwaves
      i0apdq = i0amdq + (maxm+2*mbc)*meqn
      i0cqxx = i0apdq + (maxm+2*mbc)*meqn
      i0bmadq = i0cqxx + (maxm+2*mbc)*meqn
      i0bpadq = i0bmadq + (maxm+2*mbc)*meqn
      iused = i0bpadq + (maxm+2*mbc)*meqn - 1
c
      if (iused.gt.mwork) then
c        # This shouldn't happen due to checks in claw2
         write(6,*) '*** not enough work space in step2'
         write(6,*) '*** iused = ', iused, '   mwork =',mwork
         stop 
      end if
c
c
      mcapa = method(6)
      maux = method(7)
      cfl = 0.d0
      dtdx = dt/dx
      dtdy = dt/dy
c
      if (mcapa.eq.0) then
c        # no capa array:
         do 5 i=1-mbc,maxm+mbc
            dtdx1d(i) = dtdx
            dtdy1d(i) = dtdy
    5    continue
      end if
c
      if( ids.eq.1 )then
c
c     # perform x-sweeps
c
c
c     # note that for dimensional splitting we sweep over the rows of
c     # ghosts cells as well as the interior.  This updates the ghost
c     # cell values to the intermediate state as needed in the following 
c     # sweep in the y-direction.
c
      do 50 j = 1-mbc,my+mbc
c
c        # copy data along a slice into 1d arrays:
         do 21 m=1,meqn
            do 20 i = 1-mbc, mx+mbc
               q1d(i,m) = qold(i,j,m)
   20       continue
   21    continue
c
         if (mcapa.gt.0)  then
            do 22 i = 1-mbc, mx+mbc
               dtdx1d(i) = dtdx / aux(i,j,mcapa)
   22       continue
         end if
c
         if (maux .gt. 0)  then
             do 23 ma=1,maux
               do 23 i = 1-mbc, mx+mbc
                 aux2(i,ma) = aux(i,j  ,ma)
   23          continue
c
             if(j .ne. 1-mbc)then
                do 24 ma=1,maux
                   do 24 i = 1-mbc, mx+mbc
                      aux1(i,ma) = aux(i,j-1,ma)
   24              continue
                end if
c
             if(j .ne. my+mbc)then
                do 25 ma=1,maux
                   do 25 i = 1-mbc, mx+mbc
                      aux3(i,ma) = aux(i,j+1,ma)
   25              continue
                end if
c
             end if
c
c        # Store the value of j along this slice in the common block
c        # comxyt in case it is needed in the Riemann solver (for
c        # variable coefficient problems)
         jcom = j  
c           
c        # compute modifications fadd and gadd to fluxes along this slice:
         call flux2(1,maxm,meqn,mwaves,mbc,mx,
     &            q1d,dtdx1d,aux1,aux2,aux3,method,mthlim,
     &            qadd,fadd,gadd,cfl1d,
     &              work(i0wave),work(i0s),work(i0amdq),work(i0apdq),
     &              work(i0cqxx),work(i0bmadq),work(i0bpadq),rpn2,rpt2)
         cfl = dmax1(cfl,cfl1d)
c
c        # update qnew by flux differencing.
c        # (rather than maintaining arrays f and g for the total fluxes,
c        # the modifications are used immediately to update qnew
c        # in order to save storage.)
c
         if (mcapa.eq.0) then
c
c            # no capa array.  Standard flux differencing:
            do 31 m=1,meqn
               do 30 i=1,mx
                  qnew(i,j,m) = qnew(i,j,m) + qadd(i,m)
     &                 - dtdx * (fadd(i+1,m) - fadd(i,m))
   30          continue
   31       continue
c
         else
c
c            # with capa array.  
            do 41 m=1,meqn
               do 40 i=1,mx
                  qnew(i,j,m) = qnew(i,j,m) + qadd(i,m)
     &                        - dtdx * (fadd(i+1,m) - fadd(i,m))
     &                        / aux(i,j,mcapa)
   40          continue
   41       continue
         end if
   50 continue
c
      end if
c
      if( ids.eq.2 )then
c
c     # perform y sweeps

c
      do 100 i = 1-mbc, mx+mbc
c
c        # copy data along a slice into 1d arrays:
         do 71 m=1,meqn
            do 70 j = 1-mbc, my+mbc
               q1d(j,m) = qold(i,j,m)
   70       continue
   71    continue
c
         if (mcapa.gt.0)  then
            do 72 j = 1-mbc, my+mbc
               dtdy1d(j) = dtdy / aux(i,j,mcapa)
   72       continue
         end if
c
         if (maux .gt. 0)  then
c
             do 73 ma=1,maux
               do 73 j = 1-mbc, my+mbc
                 aux2(j,ma) = aux(i,j,ma)
   73          continue
c
             if(i .ne. 1-mbc)then
                do 74 ma=1,maux
                   do 74 j = 1-mbc, my+mbc
                      aux1(j,ma) = aux(i-1,j,ma)
   74              continue
                end if
c
             if(i .ne. mx+mbc)then
                do 75 ma=1,maux
                   do 75 j = 1-mbc, my+mbc
                      aux3(j,ma) = aux(i+1,j,ma)
   75              continue
                end if
c
             end if            
c
c     # Store the value of i along this slice in the common block
c        # comxyt in case it is needed in the Riemann solver (for
c        # variable coefficient problems)
         icom = i  
c           
c        # compute modifications fadd and gadd to fluxes along this slice:
         call flux2(2,maxm,meqn,mwaves,mbc,my,
     &            q1d,dtdy1d,aux1,aux2,aux3,method,mthlim,
     &            qadd,fadd,gadd,cfl1d,
     &              work(i0wave),work(i0s),work(i0amdq),work(i0apdq),
     &              work(i0cqxx),work(i0bmadq),work(i0bpadq),rpn2,rpt2)
c
         cfl = dmax1(cfl,cfl1d)
c
c        # update qnew by flux differencing.
c        # Note that the roles of fadd and gadd are reversed for
c        # the y-sweeps -- fadd is the modification to g-fluxes and
c        # gadd is the modification to f-fluxes to the left and right.
c
         if (mcapa.eq.0) then
c
c            # no capa array.  Standard flux differencing:
            do 81 m=1,meqn
               do 80 j=1,my
                  qnew(i,j,m) = qnew(i,j,m) + qadd(j,m)
     &                  - dtdy * (fadd(j+1,m) - fadd(j,m))
   80          continue
   81       continue
c
         else
c
c            # with capa array.  
            do 91 m=1,meqn
               do 90 j=1,my
                  qnew(i,j,m) = qnew(i,j,m) + qadd(j,m)
     &                  - dtdy * (fadd(j+1,m) - fadd(j,m))
     &                        / aux(i,j,mcapa)
   90          continue
   91       continue
         end if
  100 continue
c
      end if
c
c
      return
      end
      subroutine step2(maxm,maxmx,maxmy,meqn,mwaves,mbc,mx,my,
     &               qold,qnew,aux,dx,dy,dt,method,mthlim,cfl,
     &               qadd,fadd,gadd,q1d,dtdx1d,dtdy1d,
     &                 aux1,aux2,aux3,work,mwork,rpn2,rpt2)

c*********************************************************************72
c
cc STEP2 takes one time step, updating Q, in 2D.
c
c  Discussion:
c
c     # On entry, qold and qnew should be identical and give the
c     #    initial data for this step
c     # On exit, qnew returns values at the end of the time step.
c     #    qold is unchanged.
c    
c     # qadd is used to return increments to q from flux2
c     # fadd and gadd are used to return flux increments from flux2.
c     # See the flux2 documentation for more information.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn2,rpt2
      dimension qold(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension qnew(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      dimension  q1d(1-mbc:maxm+mbc, meqn)
      dimension qadd(1-mbc:maxm+mbc, meqn)
      dimension fadd(1-mbc:maxm+mbc, meqn)
      dimension gadd(1-mbc:maxm+mbc, meqn, 2)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, *)
      dimension aux1(1-mbc:maxm+mbc, *)
      dimension aux2(1-mbc:maxm+mbc, *)
      dimension aux3(1-mbc:maxm+mbc, *)

      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension dtdy1d(1-mbc:maxm+mbc)
      dimension method(7),mthlim(mwaves)
      dimension work(mwork)

      common /comxyt/ dtcom,dxcom,dycom,tcom,icom,jcom
c
c     # partition work array into pieces needed for local storage in
c     # flux2 routine.  Find starting index of each piece:
c
      i0wave = 1
      i0s = i0wave + (maxm+2*mbc)*meqn*mwaves
      i0amdq = i0s + (maxm+2*mbc)*mwaves
      i0apdq = i0amdq + (maxm+2*mbc)*meqn
      i0cqxx = i0apdq + (maxm+2*mbc)*meqn
      i0bmadq = i0cqxx + (maxm+2*mbc)*meqn
      i0bpadq = i0bmadq + (maxm+2*mbc)*meqn
      iused = i0bpadq + (maxm+2*mbc)*meqn - 1
c
      if (iused.gt.mwork) then
c        # This shouldn't happen due to checks in claw2
         write(6,*) '*** not enough work space in step2'
         write(6,*) '*** iused = ', iused, '   mwork =',mwork
         stop 
      end if
c
c
      mcapa = method(6)
      maux = method(7)
      cfl = 0.d0
      dtdx = dt/dx
      dtdy = dt/dy
c
      if (mcapa.eq.0) then
c        # no capa array:
         do 5 i=1-mbc,maxm+mbc
            dtdx1d(i) = dtdx
            dtdy1d(i) = dtdy
    5    continue
      end if
c
c
c     # perform x-sweeps
c
c
      do 50 j = 0,my+1
c
c        # copy data along a slice into 1d arrays:
         do 21 m=1,meqn
            do 20 i = 1-mbc, mx+mbc
               q1d(i,m) = qold(i,j,m)
   20       continue
   21    continue
c
         if (mcapa.gt.0)  then
            do 22 i = 1-mbc, mx+mbc
               dtdx1d(i) = dtdx / aux(i,j,mcapa)
   22       continue
         end if
c
         if (maux .gt. 0)  then
            do 25 ma=1,maux
               do 24 i = 1-mbc, mx+mbc
                  aux1(i,ma) = aux(i,j-1,ma)
                  aux2(i,ma) = aux(i,j  ,ma)
                  aux3(i,ma) = aux(i,j+1,ma)
   24          continue
   25       continue
         end if
c
c     # Store the value of j along this slice in the common block
c        # comxyt in case it is needed in the Riemann solver (for
c        # variable coefficient problems)
         jcom = j  
c           
c        # compute modifications fadd and gadd to fluxes along this slice:
         call flux2(1,maxm,meqn,mwaves,mbc,mx,
     &            q1d,dtdx1d,aux1,aux2,aux3,method,mthlim,
     &            qadd,fadd,gadd,cfl1d,
     &              work(i0wave),work(i0s),work(i0amdq),work(i0apdq),
     &              work(i0cqxx),work(i0bmadq),work(i0bpadq),rpn2,rpt2)
         cfl = dmax1(cfl,cfl1d)
c
c        # update qnew by flux differencing.
c        # (rather than maintaining arrays f and g for the total fluxes,
c        # the modifications are used immediately to update qnew
c        # in order to save storage.)
c
         if (mcapa.eq.0) then
c
c            # no capa array.  Standard flux differencing:
            do 31 m=1,meqn
               do 30 i=1,mx
                  qnew(i,j,m) = qnew(i,j,m) + qadd(i,m)
     &                 - dtdx * (fadd(i+1,m) - fadd(i,m))
     &                       - dtdy * (gadd(i,m,2) - gadd(i,m,1))
                  qnew(i,j-1,m) = qnew(i,j-1,m) - dtdy * gadd(i,m,1)
                  qnew(i,j+1,m) = qnew(i,j+1,m) + dtdy * gadd(i,m,2)
   30          continue
   31       continue
c
         else
c
c            # with capa array.  
            do 41 m=1,meqn
               do 40 i=1,mx
                  qnew(i,j,m) = qnew(i,j,m) + qadd(i,m)
     &                 - (dtdx * (fadd(i+1,m) - fadd(i,m))
     &                         +  dtdy * (gadd(i,m,2) - gadd(i,m,1)))
     &                       / aux(i,j,mcapa)
                 qnew(i,j-1,m) = qnew(i,j-1,m) - dtdy * gadd(i,m,1)
     &                       / aux(i,j-1,mcapa)
                 qnew(i,j+1,m) = qnew(i,j+1,m) + dtdy * gadd(i,m,2)
     &                       / aux(i,j+1,mcapa)
   40          continue
   41       continue
         end if
   50 continue
c
c
c
c     # perform y sweeps
c
      do 100 i = 0, mx+1
c
c        # copy data along a slice into 1d arrays:
         do 71 m=1,meqn
            do 70 j = 1-mbc, my+mbc
               q1d(j,m) = qold(i,j,m)
   70       continue
   71    continue
c
         if (mcapa.gt.0)  then
            do 72 j = 1-mbc, my+mbc
               dtdy1d(j) = dtdy / aux(i,j,mcapa)
   72       continue
         end if
c
         if (maux .gt. 0)  then
            do 75 ma=1,maux
               do 74 j = 1-mbc, my+mbc
                  aux1(j,ma) = aux(i-1,j,ma)
                  aux2(j,ma) = aux(i,  j,ma)
                  aux3(j,ma) = aux(i+1,j,ma)
   74          continue
   75       continue
         end if
c
c
c     # Store the value of i along this slice in the common block
c        # comxyt in case it is needed in the Riemann solver (for
c        # variable coefficient problems)
         icom = i  
c           
c        # compute modifications fadd and gadd to fluxes along this slice:
         call flux2(2,maxm,meqn,mwaves,mbc,my,
     &            q1d,dtdy1d,aux1,aux2,aux3,method,mthlim,
     &            qadd,fadd,gadd,cfl1d,
     &              work(i0wave),work(i0s),work(i0amdq),work(i0apdq),
     &              work(i0cqxx),work(i0bmadq),work(i0bpadq),rpn2,rpt2)
c
         cfl = dmax1(cfl,cfl1d)
c
c        # update qnew by flux differencing.
c        # Note that the roles of fadd and gadd are reversed for
c        # the y-sweeps -- fadd is the modification to g-fluxes and
c        # gadd is the modification to f-fluxes to the left and right.
c
         if (mcapa.eq.0) then
c
c            # no capa array.  Standard flux differencing:
            do 81 m=1,meqn
               do 80 j=1,my
                  qnew(i,j,m) = qnew(i,j,m) + (qadd(j,m)
     &                  - dtdy * (fadd(j+1,m) - fadd(j,m))
     &                  - dtdx * (gadd(j,m,2) - gadd(j,m,1)))
                  qnew(i-1,j,m) = qnew(i-1,j,m) - dtdx * gadd(j,m,1)
                  qnew(i+1,j,m) = qnew(i+1,j,m) + dtdx * gadd(j,m,2)
   80          continue
   81       continue
c
         else
c
c            # with capa array.  
            do 91 m=1,meqn
               do 90 j=1,my
                  qnew(i,j,m) = qnew(i,j,m) + qadd(j,m)
     &                  - (dtdy * (fadd(j+1,m) - fadd(j,m))
     &                    + dtdx * (gadd(j,m,2) - gadd(j,m,1)))
     &                       / aux(i,j,mcapa)
                  qnew(i-1,j,m) = qnew(i-1,j,m) - dtdx * gadd(j,m,1)
     &                       / aux(i-1,j,mcapa)
                  qnew(i+1,j,m) = qnew(i+1,j,m) + dtdx * gadd(j,m,2)
     &                       / aux(i+1,j,mcapa)
   90          continue
   91       continue
         end if
  100 continue
c
      return
      end
      subroutine step3ds(maxm,maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,
     &                   mz,qold,qnew,aux,dx,dy,dz,dt,method,mthlim,cfl,
     &                   qadd,fadd,gadd,hadd,q1d,dtdx1d,dtdy1d,dtdz1d,
     &                   aux1,aux2,aux3,maux,work,mwork,
     &                   rpn3,rpt3,rptt3,idir)

c*********************************************************************72
c
cc STEP3DS takes one time step, updating Q, in 3D.
c
c  Discussion:
c
c  To be used with dimensional splitting.
c     # On entry, qold and qnew should be identical and give the
c     #    initial data for this step.
c     # On exit, qnew returns values at the end of the time step.
c     #    qold is unchanged.
c
c     # This is a simplified version of the subroutine step3d
c     # Since only qadd and fadd is used in the flux updates,
c     # the solution updates below are considerably simplified
c     # compared to step3d.
c
c
c     # NOTE! Since dimensional splitting is used, it is possible
c     #       to reduce the memory requirement, i.e. the size of
c     #       the work array. It could be reduced with
c     #
c     #       (maxm + 2*mbc)*(37*meqn + 6*maux),
c     #
c     #       when also possible reductions in flux3 are included.
c     #       However, this term is small compared to the dominating
c     #       term (maxmx+2mbc)(maxmy+2mb)*(maxmz+2mbc).
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn3,rpt3,rptt3
      dimension qold(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &          1-mbc:maxmz+mbc, meqn)
      dimension qnew(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &          1-mbc:maxmz+mbc, meqn)
      dimension  q1d(1-mbc:maxm+mbc, meqn)
      dimension qadd(1-mbc:maxm+mbc, meqn)
      dimension fadd(1-mbc:maxm+mbc, meqn)
      dimension gadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
      dimension hadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &              1-mbc:maxmz+mbc, *)
      dimension aux1(1-mbc:maxm+mbc, maux, 3)
      dimension aux2(1-mbc:maxm+mbc, maux, 3)
      dimension aux3(1-mbc:maxm+mbc, maux, 3)
      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension dtdy1d(1-mbc:maxm+mbc)
      dimension dtdz1d(1-mbc:maxm+mbc)
      dimension method(7),mthlim(mwaves)
      dimension work(mwork)
      common /comxyzt/ dtcom,dxcom,dycom,dzcom,tcom,icom,jcom,kcom
c
c     # partition work array into pieces needed for local storage in
c     # flux2 routine.  Find starting index of each piece:
c
      i0wave     = 1
      i0s        = i0wave     + (maxm+2*mbc)*meqn*mwaves
      i0amdq     = i0s        + (maxm+2*mbc)*mwaves
      i0apdq     = i0amdq     + (maxm+2*mbc)*meqn
      i0cqxx     = i0apdq     + (maxm+2*mbc)*meqn
      i0bmamdq   = i0cqxx     + (maxm+2*mbc)*meqn
      i0bmapdq   = i0bmamdq   + (maxm+2*mbc)*meqn
      i0bpamdq   = i0bmapdq   + (maxm+2*mbc)*meqn
      i0bpapdq   = i0bpamdq   + (maxm+2*mbc)*meqn
      i0cmamdq   = i0bpapdq   + (maxm+2*mbc)*meqn
      i0cmapdq   = i0cmamdq   + (maxm+2*mbc)*meqn
      i0cpamdq   = i0cmapdq   + (maxm+2*mbc)*meqn
      i0cpapdq   = i0cpamdq   + (maxm+2*mbc)*meqn
      i0cmamdq2  = i0cpapdq   + (maxm+2*mbc)*meqn
      i0cmapdq2  = i0cmamdq2  + (maxm+2*mbc)*meqn
      i0cpamdq2  = i0cmapdq2  + (maxm+2*mbc)*meqn
      i0cpapdq2  = i0cpamdq2  + (maxm+2*mbc)*meqn
      i0bmcqxxp  = i0cpapdq2  + (maxm+2*mbc)*meqn
      i0bmcqxxm  = i0bmcqxxp  + (maxm+2*mbc)*meqn
      i0bpcqxxp  = i0bmcqxxm   + (maxm+2*mbc)*meqn
      i0bpcqxxm  = i0bpcqxxp   + (maxm+2*mbc)*meqn
      i0cmcqxxp  = i0bpcqxxm   + (maxm+2*mbc)*meqn
      i0cmcqxxm  = i0cmcqxxp   + (maxm+2*mbc)*meqn
      i0cpcqxxp  = i0cmcqxxm   + (maxm+2*mbc)*meqn
      i0cpcqxxm  = i0cpcqxxp   + (maxm+2*mbc)*meqn
      i0bmcmamdq = i0cpcqxxm   + (maxm+2*mbc)*meqn
      i0bmcmapdq = i0bmcmamdq + (maxm+2*mbc)*meqn
      i0bpcmamdq = i0bmcmapdq + (maxm+2*mbc)*meqn
      i0bpcmapdq = i0bpcmamdq + (maxm+2*mbc)*meqn
      i0bmcpamdq = i0bpcmapdq + (maxm+2*mbc)*meqn
      i0bmcpapdq = i0bmcpamdq + (maxm+2*mbc)*meqn
      i0bpcpamdq = i0bmcpapdq + (maxm+2*mbc)*meqn
      i0bpcpapdq = i0bpcpamdq + (maxm+2*mbc)*meqn
      iused      = i0bpcpapdq + (maxm+2*mbc)*meqn - 1
c
      if (iused.gt.mwork) then
c        # This shouldn't happen due to checks in claw2
         write(6,*) '*** not enough work space in step2'
         write(6,*) '*** iused = ', iused, '   mwork =',mwork
         stop
      end if
c
      mcapa = method(6)
      maux = method(7)
      cfl = 0.d0
      dtdx = dt/dx
      dtdy = dt/dy
      dtdz = dt/dz
c
      if (mcapa.eq.0) then
c        # no capa array:
         do 5 i=1-mbc,maxm+mbc
            dtdx1d(i) = dtdx
            dtdy1d(i) = dtdy
            dtdz1d(i) = dtdz
    5       continue
         end if
c
c
      if( idir .eq. 1)then
c
c     # perform x-sweeps
c
c
      do 50 k = 0,mz+1
         do 50 j = 0,my+1
c
            do 20 m=1,meqn
               do 20 i = 1-mbc, mx+mbc
c                 # copy data along a slice into 1d array:
                  q1d(i,m) = qold(i,j,k,m)
   20          continue
c
         if (mcapa.gt.0)  then
           do 23 i = 1-mbc, mx+mbc
               dtdx1d(i) = dtdx / aux(i,j,k,mcapa)
   23      continue
         end if
c
c        # Since dimensional splitting is used, only aux2 is needed.
c
         if (maux .gt. 0)  then
             do 22 ma=1,maux
               do 22 i = 1-mbc, mx+mbc
                 aux2(i,ma,1) = aux(i,j,k-1,ma)
                 aux2(i,ma,2) = aux(i,j,k,ma)
                 aux2(i,ma,3) = aux(i,j,k+1,ma)
   22          continue
           end if
c
c           # Store the value of j and k along this slice in the common block
c           # comxyt in case it is needed in the Riemann solver (for
c           # variable coefficient problems)
c
            jcom = j
            kcom = k
c
c           # compute modifications qadd and fadd along this slice
c
            call flux3(1,maxm,meqn,mwaves,mbc,mx,
     &                 q1d,dtdx1d,dtdy,dtdz,dummy1,aux2,dummy3,maux,
     &                 method,mthlim,qadd,fadd,gadd,hadd,cfl1d,
     &                 work(i0wave),work(i0s),work(i0amdq),
     &                 work(i0apdq),work(i0cqxx),
     &                 work(i0bmamdq),work(i0bmapdq),
     &                 work(i0bpamdq),work(i0bpapdq),
     &                 work(i0cmamdq),work(i0cmapdq),
     &                 work(i0cpamdq),work(i0cpapdq),
     &                 work(i0cmamdq2),work(i0cmapdq2),
     &                 work(i0cpamdq2),work(i0cpapdq2),
     &                 work(i0bmcqxxp),work(i0bpcqxxp),
     &                 work(i0bmcqxxm),work(i0bpcqxxm),
     &                 work(i0cmcqxxp),work(i0cpcqxxp),
     &                 work(i0cmcqxxm),work(i0cpcqxxm),
     &                 work(i0bmcmamdq),work(i0bmcmapdq),
     &                 work(i0bpcmamdq),work(i0bpcmapdq),
     &                 work(i0bmcpamdq),work(i0bmcpapdq),
     &                 work(i0bpcpamdq),work(i0bpcpapdq),
     &                 rpn3,rpt3,rptt3)
c
            cfl = dmax1(cfl,cfl1d)
c
c           # update qnew by flux differencing.
c           # (rather than maintaining arrays f, g and h for the total fluxes,
c           # the modifications are used immediately to update qnew
c           # in order to save storage.)
c
            if(mcapa. eq. 0)then
c              # no capa array.  Standard flux differencing:
               do 30 m=1,meqn
                  do 30 i=1,mx
                     qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(i,m)
     &                             - dtdx * (fadd(i+1,m) - fadd(i,m))
   30             continue
            else
c              # with capa array
               do 40 m=1,meqn
                  do 40 i=1,mx
                     qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(i,m)
     &                        - dtdx * (fadd(i+1,m) - fadd(i,m))
     &                        / aux(i,j,k,mcapa)
   40             continue
            end if
c
   50    continue
c
      else if( idir .eq. 2 )then
c
c     # perform y sweeps
c
c
      do 100 k = 0, mz+1
         do 100 i = 0, mx+1
c
            do 70 m=1,meqn
               do 70 j = 1-mbc, my+mbc
c                 # copy data along a slice into 1d array:
                  q1d(j,m) = qold(i,j,k,m)
   70          continue
c
         if (mcapa.gt.0)  then
           do 71 j = 1-mbc, my+mbc
               dtdy1d(j) = dtdy / aux(i,j,k,mcapa)
   71      continue
         end if
c
c        # Since dimensional splitting is used, only aux2 is needed.
c
         if (maux .gt. 0)  then
             do 72 ma=1,maux
               do 72 j = 1-mbc, my+mbc
                 aux2(j,ma,1) = aux(i-1,j,k,ma)
                 aux2(j,ma,2) = aux(i,j,k,ma)
                 aux2(j,ma,3) = aux(i+1,j,k,ma)
   72          continue
         end if
c
c           # Store the value of i and k along this slice in the common block
c           # comxyzt in case it is needed in the Riemann solver (for
c           # variable coefficient problems)
c
            icom = i
            kcom = k
c
c           # compute modifications qadd and fadd along this slice
c
            call flux3(2,maxm,meqn,mwaves,mbc,my,
     &                 q1d,dtdy1d,dtdz,dtdx,dummy1,aux2,dummy3,maux,
     &                 method,mthlim,qadd,fadd,gadd,hadd,cfl1d,
     &                 work(i0wave),work(i0s),work(i0amdq),
     &                 work(i0apdq),work(i0cqxx),
     &                 work(i0bmamdq),work(i0bmapdq),
     &                 work(i0bpamdq),work(i0bpapdq),
     &                 work(i0cmamdq),work(i0cmapdq),
     &                 work(i0cpamdq),work(i0cpapdq),
     &                 work(i0cmamdq2),work(i0cmapdq2),
     &                 work(i0cpamdq2),work(i0cpapdq2),
     &                 work(i0bmcqxxp),work(i0bpcqxxp),
     &                 work(i0bmcqxxm),work(i0bpcqxxm),
     &                 work(i0cmcqxxp),work(i0cpcqxxp),
     &                 work(i0cmcqxxm),work(i0cpcqxxm),
     &                 work(i0bmcmamdq),work(i0bmcmapdq),
     &                 work(i0bpcmamdq),work(i0bpcmapdq),
     &                 work(i0bmcpamdq),work(i0bmcpapdq),
     &                 work(i0bpcpamdq),work(i0bpcpapdq),
     &                 rpn3,rpt3,rptt3)
c
            cfl = dmax1(cfl,cfl1d)
c
c           # update qnew by flux differencing.
c           # Note that the roles of the flux updates are changed.
c           # fadd - modifies the g-fluxes
c
            if( mcapa.eq. 0)then
c               # no capa array.  Standard flux differencing:
                do 80 m=1,meqn
                   do 80 j=1,my
                      qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(j,m)
     &                              - dtdy * (fadd(j+1,m) - fadd(j,m))
   80              continue
             else
c              #with capa array.
                do 85 m=1,meqn
                   do 85 j=1,my
                      qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(j,m)
     &                        - dtdy * (fadd(j+1,m) - fadd(j,m))
     &                        / aux(i,j,k,mcapa)
   85              continue
            end if
c
  100    continue
c
      else
c
c     # perform z sweeps
c
c
c
      do 150 j = 0, my+1
         do 150 i = 0, mx+1
c
            do 110 m=1,meqn
               do 110 k = 1-mbc, mz+mbc
c                 # copy data along a slice into 1d array:
                  q1d(k,m) = qold(i,j,k,m)
 110           continue
c
         if (mcapa.gt.0)  then
           do 130 k = 1-mbc, mz+mbc
               dtdz1d(k) = dtdz / aux(i,j,k,mcapa)
 130       continue
         end if
c
c        # Since dimensional splitting is used, only aux2 is needed.
c
         if (maux .gt. 0)  then
             do 131 ma=1,maux
               do 131 k = 1-mbc, mz+mbc
                 aux2(k,ma,1) = aux(i,j-1,k,ma)
                 aux2(k,ma,2) = aux(i,j,k,ma)
                 aux2(k,ma,3) = aux(i,j+1,k,ma)
  131          continue
           end if
c
c           # Store the value of i and j along this slice in the common block
c           # comxyzt in case it is needed in the Riemann solver (for
c           # variable coefficient problems)
c
            icom = i
            jcom = j
c
c           # compute modifications qadd and fadd along this slice
c
            call flux3(3,maxm,meqn,mwaves,mbc,mz,
     &                 q1d,dtdz1d,dtdx,dtdy,dummy1,aux2,dummy3,maux,
     &                 method,mthlim,qadd,fadd,gadd,hadd,cfl1d,
     &                 work(i0wave),work(i0s),work(i0amdq),
     &                 work(i0apdq),work(i0cqxx),
     &                 work(i0bmamdq),work(i0bmapdq),
     &                 work(i0bpamdq),work(i0bpapdq),
     &                 work(i0cmamdq),work(i0cmapdq),
     &                 work(i0cpamdq),work(i0cpapdq),
     &                 work(i0cmamdq2),work(i0cmapdq2),
     &                 work(i0cpamdq2),work(i0cpapdq2),
     &                 work(i0bmcqxxp),work(i0bpcqxxp),
     &                 work(i0bmcqxxm),work(i0bpcqxxm),
     &                 work(i0cmcqxxp),work(i0cpcqxxp),
     &                 work(i0cmcqxxm),work(i0cpcqxxm),
     &                 work(i0bmcmamdq),work(i0bmcmapdq),
     &                 work(i0bpcmamdq),work(i0bpcmapdq),
     &                 work(i0bmcpamdq),work(i0bmcpapdq),
     &                 work(i0bpcpamdq),work(i0bpcpapdq),
     &                 rpn3,rpt3,rptt3)
c
            cfl = dmax1(cfl,cfl1d)
c
c           # update qnew by flux differencing.
c           # Note that the roles of the flux updates are changed.
c           # fadd - modifies the h-fluxes
c
            if(mcapa .eq. 0)then
c              #no capa array. Standard flux differencing:
               do 120 m=1,meqn
                  do 120 k=1,mz
                     qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(k,m)
     &                             - dtdz * (fadd(k+1,m) - fadd(k,m))
 120              continue
            else
c              # with capa array
               do 145 m=1,meqn
                  do 145 k=1,mz
                     qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(k,m)
     &                             - dtdz * (fadd(k+1,m) - fadd(k,m))
     &                             / aux(i,j,k,mcapa)
 145              continue
            end if
c
 150  continue
c
      end if
c
      return
      end
      subroutine step3(maxm,maxmx,maxmy,maxmz,meqn,mwaves,mbc,mx,my,
     &                 mz,qold,qnew,aux,dx,dy,dz,dt,method,mthlim,cfl,
     &                 qadd,fadd,gadd,hadd,q1d,dtdx1d,dtdy1d,dtdz1d,
     &                 aux1,aux2,aux3,maux,work,mwork,rpn3,rpt3,rptt3)

c*********************************************************************72
c
cc STEP3 takes one time step, updating Q, in 3D.
c
c  Discussion:
c
c     # On entry, qold and qnew should be identical and give the
c     #    initial data for this step
c     # On exit, qnew returns values at the end of the time step.
c     #    qold is unchanged.
c
c     # qadd is used to return increments to q from flux3.
c     # fadd, gadd and hadd are used to return flux increments from flux3.
c     # See the flux3 documentation for more information.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      external rpn3,rpt3,rptt3
      dimension qold(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &          1-mbc:maxmz+mbc, meqn)
      dimension qnew(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &          1-mbc:maxmz+mbc, meqn)
      dimension  q1d(1-mbc:maxm+mbc, meqn)
      dimension qadd(1-mbc:maxm+mbc, meqn)
      dimension fadd(1-mbc:maxm+mbc, meqn)
      dimension gadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
      dimension hadd(1-mbc:maxm+mbc, meqn, 2, -1:1)
      dimension aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc,
     &              1-mbc:maxmz+mbc, *)
      dimension aux1(1-mbc:maxm+mbc, maux, 3)
      dimension aux2(1-mbc:maxm+mbc, maux, 3)
      dimension aux3(1-mbc:maxm+mbc, maux, 3)
      dimension dtdx1d(1-mbc:maxm+mbc)
      dimension dtdy1d(1-mbc:maxm+mbc)
      dimension dtdz1d(1-mbc:maxm+mbc)
      dimension method(7),mthlim(mwaves)
      dimension work(mwork)


      common /comxyzt/ dtcom,dxcom,dycom,dzcom,tcom,icom,jcom,kcom

c
c
c     # partition work array into pieces needed for local storage in
c     # flux3 routine.  Find starting index of each piece:
c
      i0wave     = 1
      i0s        = i0wave     + (maxm+2*mbc)*meqn*mwaves
      i0amdq     = i0s        + (maxm+2*mbc)*mwaves
      i0apdq     = i0amdq     + (maxm+2*mbc)*meqn
      i0cqxx     = i0apdq     + (maxm+2*mbc)*meqn
      i0bmamdq   = i0cqxx     + (maxm+2*mbc)*meqn
      i0bmapdq   = i0bmamdq   + (maxm+2*mbc)*meqn
      i0bpamdq   = i0bmapdq   + (maxm+2*mbc)*meqn
      i0bpapdq   = i0bpamdq   + (maxm+2*mbc)*meqn
      i0cmamdq   = i0bpapdq   + (maxm+2*mbc)*meqn
      i0cmapdq   = i0cmamdq   + (maxm+2*mbc)*meqn
      i0cpamdq   = i0cmapdq   + (maxm+2*mbc)*meqn
      i0cpapdq   = i0cpamdq   + (maxm+2*mbc)*meqn
      i0cmamdq2  = i0cpapdq   + (maxm+2*mbc)*meqn
      i0cmapdq2  = i0cmamdq2  + (maxm+2*mbc)*meqn
      i0cpamdq2  = i0cmapdq2  + (maxm+2*mbc)*meqn
      i0cpapdq2  = i0cpamdq2  + (maxm+2*mbc)*meqn
      i0bmcqxxp  = i0cpapdq2  + (maxm+2*mbc)*meqn
      i0bmcqxxm  = i0bmcqxxp  + (maxm+2*mbc)*meqn
      i0bpcqxxp  = i0bmcqxxm   + (maxm+2*mbc)*meqn
      i0bpcqxxm  = i0bpcqxxp   + (maxm+2*mbc)*meqn
      i0cmcqxxp  = i0bpcqxxm   + (maxm+2*mbc)*meqn
      i0cmcqxxm  = i0cmcqxxp   + (maxm+2*mbc)*meqn
      i0cpcqxxp  = i0cmcqxxm   + (maxm+2*mbc)*meqn
      i0cpcqxxm  = i0cpcqxxp   + (maxm+2*mbc)*meqn
      i0bmcmamdq = i0cpcqxxm   + (maxm+2*mbc)*meqn
      i0bmcmapdq = i0bmcmamdq + (maxm+2*mbc)*meqn
      i0bpcmamdq = i0bmcmapdq + (maxm+2*mbc)*meqn
      i0bpcmapdq = i0bpcmamdq + (maxm+2*mbc)*meqn
      i0bmcpamdq = i0bpcmapdq + (maxm+2*mbc)*meqn
      i0bmcpapdq = i0bmcpamdq + (maxm+2*mbc)*meqn
      i0bpcpamdq = i0bmcpapdq + (maxm+2*mbc)*meqn
      i0bpcpapdq = i0bpcpamdq + (maxm+2*mbc)*meqn
      iused      = i0bpcpapdq + (maxm+2*mbc)*meqn - 1
c
      if (iused.gt.mwork) then
c        # This shouldn't happen due to checks in claw3
         write(6,*) '*** not enough work space in step3'
         write(6,*) '*** iused = ', iused, '   mwork =',mwork
         stop
      end if
c


      mcapa = method(6)
      maux = method(7)
      cfl = 0.d0
      dtdx = dt/dx
      dtdy = dt/dy
      dtdz = dt/dz
c
      if (mcapa.eq.0) then
c        # no capa array:
         do 5 i=1-mbc,maxm+mbc
            dtdx1d(i) = dtdx
            dtdy1d(i) = dtdy
            dtdz1d(i) = dtdz
    5       continue
         end if
c
c
c     # perform x-sweeps
c
c

      do 50 k = 0,mz+1
         do 50 j = 0,my+1

            do 20 m=1,meqn
               do 20 i = 1-mbc, mx+mbc
c                 # copy data along a slice into 1d array:
                  q1d(i,m) = qold(i,j,k,m)
   20          continue
c
         if (mcapa.gt.0)  then
           do 23 i = 1-mbc, mx+mbc
               dtdx1d(i) = dtdx / aux(i,j,k,mcapa)
   23      continue
         end if
c
         if (maux .gt. 0)  then
             do 22 ma=1,maux
               do 22 i = 1-mbc, mx+mbc
                 aux1(i,ma,1) = aux(i,j-1,k-1,ma)
                 aux1(i,ma,2) = aux(i,j-1,k,ma)
                 aux1(i,ma,3) = aux(i,j-1,k+1,ma)
                 aux2(i,ma,1) = aux(i,j,k-1,ma)
                 aux2(i,ma,2) = aux(i,j,k,ma)
                 aux2(i,ma,3) = aux(i,j,k+1,ma)
                 aux3(i,ma,1) = aux(i,j+1,k-1,ma)
                 aux3(i,ma,2) = aux(i,j+1,k,ma)
                 aux3(i,ma,3) = aux(i,j+1,k+1,ma)
   22          continue
           end if
c
c           # Store the value of j and k along this slice in the common block
c           # comxyt in case it is needed in the Riemann solver (for
c           # variable coefficient problems)
c
            jcom = j
            kcom = k
c
c           # compute modifications fadd, gadd and hadd to fluxes along
c           # this slice:
c
            call flux3(1,maxm,meqn,mwaves,mbc,mx,
     &                 q1d,dtdx1d,dtdy,dtdz,aux1,aux2,aux3,maux,
     &                 method,mthlim,qadd,fadd,gadd,hadd,cfl1d,
     &                 work(i0wave),work(i0s),work(i0amdq),
     &                 work(i0apdq),work(i0cqxx),
     &                 work(i0bmamdq),work(i0bmapdq),
     &                 work(i0bpamdq),work(i0bpapdq),
     &                 work(i0cmamdq),work(i0cmapdq),
     &                 work(i0cpamdq),work(i0cpapdq),
     &                 work(i0cmamdq2),work(i0cmapdq2),
     &                 work(i0cpamdq2),work(i0cpapdq2),
     &                 work(i0bmcqxxp),work(i0bpcqxxp),
     &                 work(i0bmcqxxm),work(i0bpcqxxm),
     &                 work(i0cmcqxxp),work(i0cpcqxxp),
     &                 work(i0cmcqxxm),work(i0cpcqxxm),
     &                 work(i0bmcmamdq),work(i0bmcmapdq),
     &                 work(i0bpcmamdq),work(i0bpcmapdq),
     &                 work(i0bmcpamdq),work(i0bmcpapdq),
     &                 work(i0bpcpamdq),work(i0bpcpapdq),
     &                 rpn3,rpt3,rptt3)
c
            cfl = dmax1(cfl,cfl1d)
c
c           # update qnew by flux differencing.
c           # (rather than maintaining arrays f, g and h for the total fluxes,
c           # the modifications are used immediately to update qnew
c           # in order to save storage.)
c
            if(mcapa. eq. 0)then
c
            do 30 m=1,meqn
               do 30 i=1,mx
                  qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(i,m)
     &                        - dtdx * (fadd(i+1,m) - fadd(i,m))
     &                        - dtdy * (gadd(i,m,2,0) - gadd(i,m,1,0))
     &                        - dtdz * (hadd(i,m,2,0) - hadd(i,m,1,0))
                  qnew(i,j-1,k,m)   = qnew(i,j-1,k,m)
     &                              - dtdy * gadd(i,m,1,0)
     &                              - dtdz * ( hadd(i,m,2,-1)
     &                                      -   hadd(i,m,1,-1) )
                  qnew(i,j-1,k-1,m) = qnew(i,j-1,k-1,m)
     &                              - dtdy * gadd(i,m,1,-1)
     &                              - dtdz * hadd(i,m,1,-1)
                  qnew(i,j,k-1,m)   = qnew(i,j,k-1,m)
     &                              - dtdy * ( gadd(i,m,2,-1)
     &                                     -   gadd(i,m,1,-1) )
     &                              - dtdz * hadd(i,m,1,0)
                  qnew(i,j+1,k-1,m) = qnew(i,j+1,k-1,m)
     &                              + dtdy * gadd(i,m,2,-1)
     &                              - dtdz * hadd(i,m,1,1)
                  qnew(i,j+1,k,m)   = qnew(i,j+1,k,m)
     &                              + dtdy * gadd(i,m,2,0)
     &                              - dtdz * ( hadd(i,m,2,1)
     &                                     -   hadd(i,m,1,1) )
                  qnew(i,j+1,k+1,m) = qnew(i,j+1,k+1,m)
     &                              + dtdy * gadd(i,m,2,1)
     &                              + dtdz * hadd(i,m,2,1)
                  qnew(i,j,k+1,m)   = qnew(i,j,k+1,m)
     &                              - dtdy * ( gadd(i,m,2,1)
     &                                     -   gadd(i,m,1,1) )
     &                              + dtdz * hadd(i,m,2,0)
                  qnew(i,j-1,k+1,m) = qnew(i,j-1,k+1,m)
     &                              - dtdy * gadd(i,m,1,1)
     &                              + dtdz * hadd(i,m,2,-1)
c
   30          continue
c
            else
c              # with capa array
               do 40 m=1,meqn
                  do 40 i=1,mx
                     qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(i,m)
     &                        - (dtdx * (fadd(i+1,m) - fadd(i,m))
     &                        +  dtdy * (gadd(i,m,2,0) - gadd(i,m,1,0))
     &                        +  dtdz * (hadd(i,m,2,0) - hadd(i,m,1,0)))
     &                        / aux(i,j,k,mcapa)

                     qnew(i,j-1,k,m)   = qnew(i,j-1,k,m)
     &                                 - (dtdy * gadd(i,m,1,0)
     &                                 +  dtdz * ( hadd(i,m,2,-1)
     &                                        -   hadd(i,m,1,-1) ))
     &                                 / aux(i,j-1,k,mcapa)
                     qnew(i,j-1,k-1,m) = qnew(i,j-1,k-1,m)
     &                                 - (dtdy * gadd(i,m,1,-1)
     &                                 +  dtdz * hadd(i,m,1,-1))
     &                                 / aux(i,j-1,k-1,mcapa)
                     qnew(i,j,k-1,m)   = qnew(i,j,k-1,m)
     &                                 - (dtdy * ( gadd(i,m,2,-1)
     &                                        -   gadd(i,m,1,-1) )
     &                                 +  dtdz * hadd(i,m,1,0))
     &                                 / aux(i,j,k-1,mcapa)
                     qnew(i,j+1,k-1,m) = qnew(i,j+1,k-1,m)
     &                                 + (dtdy * gadd(i,m,2,-1)
     &                                 -  dtdz * hadd(i,m,1,1))
     &                                 / aux(i,j+1,k-1,mcapa)
                     qnew(i,j+1,k,m)   = qnew(i,j+1,k,m)
     &                                 + (dtdy * gadd(i,m,2,0)
     &                                 - dtdz * ( hadd(i,m,2,1)
     &                                        -   hadd(i,m,1,1) ))
     &                                 / aux(i,j+1,k,mcapa)
                     qnew(i,j+1,k+1,m) = qnew(i,j+1,k+1,m)
     &                                 + (dtdy * gadd(i,m,2,1)
     &                                 +  dtdz * hadd(i,m,2,1))
     &                                 / aux(i,j+1,k+1,mcapa)
                     qnew(i,j,k+1,m)   = qnew(i,j,k+1,m)
     &                                 - (dtdy * ( gadd(i,m,2,1)
     &                                        -   gadd(i,m,1,1) )
     &                                 -  dtdz * hadd(i,m,2,0))
     &                                 / aux(i,j,k+1,mcapa)
                     qnew(i,j-1,k+1,m) = qnew(i,j-1,k+1,m)
     &                                 - (dtdy * gadd(i,m,1,1)
     &                                 -  dtdz * hadd(i,m,2,-1))
     &                                 / aux(i,j-1,k+1,mcapa)
c
c
   40          continue
c
            end if
c
   50    continue
   51    continue

c
c
c     # perform y sweeps
c
      do 100 k = 0, mz+1
         do 100 i = 0, mx+1
c
            do 70 m=1,meqn
               do 70 j = 1-mbc, my+mbc
c                 # copy data along a slice into 1d array:
                  q1d(j,m) = qold(i,j,k,m)
   70          continue
c
         if (mcapa.gt.0)  then
           do 71 j = 1-mbc, my+mbc
               dtdy1d(j) = dtdy / aux(i,j,k,mcapa)
   71      continue
         end if
c
         if (maux .gt. 0)  then
             do 72 ma=1,maux
               do 72 j = 1-mbc, my+mbc
                 aux1(j,ma,1) = aux(i-1,j,k-1,ma)
                 aux1(j,ma,2) = aux(i,j,k-1,ma)
                 aux1(j,ma,3) = aux(i+1,j,k-1,ma)
                 aux2(j,ma,1) = aux(i-1,j,k,ma)
                 aux2(j,ma,2) = aux(i,j,k,ma)
                 aux2(j,ma,3) = aux(i+1,j,k,ma)
                 aux3(j,ma,1) = aux(i-1,j,k+1,ma)
                 aux3(j,ma,2) = aux(i,j,k+1,ma)
                 aux3(j,ma,3) = aux(i+1,j,k+1,ma)
   72          continue
         end if
c
c           # Store the value of i and k along this slice in the common block
c           # comxyzt in case it is needed in the Riemann solver (for
c           # variable coefficient problems)
c
            icom = i
            kcom = k
c
c           # compute modifications fadd, gadd and hadd to fluxes along this
c           # slice:
c
            call flux3(2,maxm,meqn,mwaves,mbc,my,
     &                 q1d,dtdy1d,dtdz,dtdx,aux1,aux2,aux3,maux,
     &                 method,mthlim,qadd,fadd,gadd,hadd,cfl1d,
     &                 work(i0wave),work(i0s),work(i0amdq),
     &                 work(i0apdq),work(i0cqxx),
     &                 work(i0bmamdq),work(i0bmapdq),
     &                 work(i0bpamdq),work(i0bpapdq),
     &                 work(i0cmamdq),work(i0cmapdq),
     &                 work(i0cpamdq),work(i0cpapdq),
     &                 work(i0cmamdq2),work(i0cmapdq2),
     &                 work(i0cpamdq2),work(i0cpapdq2),
     &                 work(i0bmcqxxp),work(i0bpcqxxp),
     &                 work(i0bmcqxxm),work(i0bpcqxxm),
     &                 work(i0cmcqxxp),work(i0cpcqxxp),
     &                 work(i0cmcqxxm),work(i0cpcqxxm),
     &                 work(i0bmcmamdq),work(i0bmcmapdq),
     &                 work(i0bpcmamdq),work(i0bpcmapdq),
     &                 work(i0bmcpamdq),work(i0bmcpapdq),
     &                 work(i0bpcpamdq),work(i0bpcpapdq),
     &                 rpn3,rpt3,rptt3)
c
            cfl = dmax1(cfl,cfl1d)
c
c           # update qnew by flux differencing.
c           # Note that the roles of the flux updates are changed.
c           # fadd - modifies the g-fluxes
c           # gadd - modifies the h-fluxes
c           # hadd - modifies the f-fluxes
c
            if( mcapa.eq. 0)then
c               # no capa array.  Standard flux differencing:
            do 80 m=1,meqn
               do 80 j=1,my
                  qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(j,m)
     &                        - dtdy * (fadd(j+1,m) - fadd(j,m))
     &                        - dtdz * (gadd(j,m,2,0) - gadd(j,m,1,0))
     &                        - dtdx * (hadd(j,m,2,0) - hadd(j,m,1,0))
                  qnew(i,j,k+1,m)   = qnew(i,j,k+1,m)
     &                              + dtdz * gadd(j,m,2,0)
     &                              - dtdx * ( hadd(j,m,2,1)
     &                                     -   hadd(j,m,1,1) )
                  qnew(i+1,j,k+1,m) = qnew(i+1,j,k+1,m)
     &                              + dtdz * gadd(j,m,2,1)
     &                              +  dtdx * hadd(j,m,2,1)
                  qnew(i+1,j,k,m)   = qnew(i+1,j,k,m)
     &                              - dtdz * ( gadd(j,m,2,1)
     &                                     -   gadd(j,m,1,1) )
     &                              + dtdx * hadd(j,m,2,0)
                  qnew(i+1,j,k-1,m) = qnew(i+1,j,k-1,m)
     &                              - dtdz * gadd(j,m,1,1)
     &                              + dtdx * hadd(j,m,2,-1)
                  qnew(i,j,k-1,m)   = qnew(i,j,k-1,m)
     &                              - dtdz * gadd(j,m,1,0)
     &                              - dtdx * ( hadd(j,m,2,-1)
     &                                     -   hadd(j,m,1,-1) )
                  qnew(i-1,j,k-1,m) = qnew(i-1,j,k-1,m)
     &                              - dtdz * gadd(j,m,1,-1)
     &                              - dtdx * hadd(j,m,1,-1)
                  qnew(i-1,j,k,m)   = qnew(i-1,j,k,m)
     &                              - dtdz * ( gadd(j,m,2,-1)
     &                                     -   gadd(j,m,1,-1) )
     &                              - dtdx * hadd(j,m,1,0)
                  qnew(i-1,j,k+1,m) = qnew(i-1,j,k+1,m)
     &                              + dtdz * gadd(j,m,2,-1)
     &                              -  dtdx*hadd(j,m,1,1)
c
   80          continue
c
             else
c
c              #with capa array.
c
                do 85 m=1,meqn
                   do 85 j=1,my
                      qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(j,m)
     &                        - (dtdy * (fadd(j+1,m) - fadd(j,m))
     &                        +  dtdz * (gadd(j,m,2,0) - gadd(j,m,1,0))
     &                        +  dtdx * (hadd(j,m,2,0) - hadd(j,m,1,0)))
     &                        / aux(i,j,k,mcapa)
                      qnew(i,j,k+1,m)   = qnew(i,j,k+1,m)
     &                                 + (dtdz * gadd(j,m,2,0)
     &                                 -  dtdx * ( hadd(j,m,2,1)
     &                                        -   hadd(j,m,1,1) ))
     &                                 / aux(i,j,k+1,mcapa)
                      qnew(i+1,j,k+1,m) = qnew(i+1,j,k+1,m)
     &                                  + (dtdz * gadd(j,m,2,1)
     &                                  +  dtdx * hadd(j,m,2,1))
     &                                  / aux(i+1,j,k+1,mcapa)
                      qnew(i+1,j,k,m)   = qnew(i+1,j,k,m)
     &                                  - (dtdz * ( gadd(j,m,2,1)
     &                                          -   gadd(j,m,1,1) )
     &                                  -  dtdx * hadd(j,m,2,0) )
     &                                  / aux(i+1,j,k,mcapa)
                      qnew(i+1,j,k-1,m) = qnew(i+1,j,k-1,m)
     &                                  - (dtdz * gadd(j,m,1,1)
     &                                  -  dtdx * hadd(j,m,2,-1))
     &                                  / aux(i+1,j,k-1,mcapa)
                      qnew(i,j,k-1,m)   = qnew(i,j,k-1,m)
     &                                  - (dtdz * gadd(j,m,1,0)
     &                                  +  dtdx * ( hadd(j,m,2,-1)
     &                                         -   hadd(j,m,1,-1) ))
     &                                  / aux(i,j,k-1,mcapa)
                      qnew(i-1,j,k-1,m) = qnew(i-1,j,k-1,m)
     &                                  - (dtdz * gadd(j,m,1,-1)
     &                                  +  dtdx * hadd(j,m,1,-1))
     &                                  / aux(i-1,j,k-1,mcapa)
                      qnew(i-1,j,k,m)   = qnew(i-1,j,k,m)
     &                                  - (dtdz * ( gadd(j,m,2,-1)
     &                                         -   gadd(j,m,1,-1) )
     &                                  +  dtdx * hadd(j,m,1,0))
     &                                  / aux(i-1,j,k,mcapa)
                      qnew(i-1,j,k+1,m) = qnew(i-1,j,k+1,m)
     &                                  + (dtdz * gadd(j,m,2,-1)
     &                                  -  dtdx*hadd(j,m,1,1))
     &                                  / aux(i-1,j,k+1,mcapa)
c

   85              continue
c
            end if
c
  100    continue
  101    continue
c
c
c
c     # perform z sweeps
c
      do 150 j = 0, my+1
         do 150 i = 0, mx+1
c
            do 110 m=1,meqn
               do 110 k = 1-mbc, mz+mbc
c                 # copy data along a slice into 1d array:
                  q1d(k,m) = qold(i,j,k,m)
 110           continue
c
         if (mcapa.gt.0)  then
           do 130 k = 1-mbc, mz+mbc
               dtdz1d(k) = dtdz / aux(i,j,k,mcapa)
 130       continue
         end if
c
         if (maux .gt. 0)  then
             do 131 ma=1,maux
               do 131 k = 1-mbc, mz+mbc
                 aux1(k,ma,1) = aux(i-1,j-1,k,ma)
                 aux1(k,ma,2) = aux(i-1,j,k,ma)
                 aux1(k,ma,3) = aux(i-1,j+1,k,ma)
                 aux2(k,ma,1) = aux(i,j-1,k,ma)
                 aux2(k,ma,2) = aux(i,j,k,ma)
                 aux2(k,ma,3) = aux(i,j+1,k,ma)
                 aux3(k,ma,1) = aux(i+1,j-1,k,ma)
                 aux3(k,ma,2) = aux(i+1,j,k,ma)
                 aux3(k,ma,3) = aux(i+1,j+1,k,ma)
  131          continue
           end if
c
c           # Store the value of i and j along this slice in the common block
c           # comxyzt in case it is needed in the Riemann solver (for
c           # variable coefficient problems)
c
            icom = i
            jcom = j
c
c           # compute modifications fadd, gadd and hadd to fluxes along this
c           # slice:
c
            call flux3(3,maxm,meqn,mwaves,mbc,mz,
     &                 q1d,dtdz1d,dtdx,dtdy,aux1,aux2,aux3,maux,
     &                 method,mthlim,qadd,fadd,gadd,hadd,cfl1d,
     &                 work(i0wave),work(i0s),work(i0amdq),
     &                 work(i0apdq),work(i0cqxx),
     &                 work(i0bmamdq),work(i0bmapdq),
     &                 work(i0bpamdq),work(i0bpapdq),
     &                 work(i0cmamdq),work(i0cmapdq),
     &                 work(i0cpamdq),work(i0cpapdq),
     &                 work(i0cmamdq2),work(i0cmapdq2),
     &                 work(i0cpamdq2),work(i0cpapdq2),
     &                 work(i0bmcqxxp),work(i0bpcqxxp),
     &                 work(i0bmcqxxm),work(i0bpcqxxm),
     &                 work(i0cmcqxxp),work(i0cpcqxxp),
     &                 work(i0cmcqxxm),work(i0cpcqxxm),
     &                 work(i0bmcmamdq),work(i0bmcmapdq),
     &                 work(i0bpcmamdq),work(i0bpcmapdq),
     &                 work(i0bmcpamdq),work(i0bmcpapdq),
     &                 work(i0bpcpamdq),work(i0bpcpapdq),
     &                 rpn3,rpt3,rptt3)
c
            cfl = dmax1(cfl,cfl1d)
c
c           # update qnew by flux differencing.
c           # Note that the roles of the flux updates are changed.
c           # fadd - modifies the h-fluxes
c           # gadd - modifies the f-fluxes
c           # hadd - modifies the g-fluxes
c
            if(mcapa .eq. 0)then
c
c              #no capa array. Standard flux differencing:
c
            do 120 m=1,meqn
               do 120 k=1,mz
                  qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(k,m)
     &                        - dtdz * (fadd(k+1,m) - fadd(k,m))
     &                        - dtdx * (gadd(k,m,2,0) - gadd(k,m,1,0))
     &                        - dtdy * (hadd(k,m,2,0) - hadd(k,m,1,0))
                  qnew(i,j+1,k,m)   = qnew(i,j+1,k,m)
     &                              - dtdx * ( gadd(k,m,2,1)
     &                                     -   gadd(k,m,1,1) )
     &                              + dtdy * hadd(k,m,2,0)
                  qnew(i+1,j+1,k,m) = qnew(i+1,j+1,k,m)
     &                              + dtdx * gadd(k,m,2,1)
     &                              + dtdy * hadd(k,m,2,1)
                  qnew(i+1,j,k,m)   = qnew(i+1,j,k,m)
     &                              + dtdx * gadd(k,m,2,0)
     &                              - dtdy * ( hadd(k,m,2,1)
     &                                     -   hadd(k,m,1,1) )
                  qnew(i+1,j-1,k,m) = qnew(i+1,j-1,k,m)
     &                              + dtdx * gadd(k,m,2,-1)
     &                              - dtdy * hadd(k,m,1,1)
                  qnew(i,j-1,k,m)   = qnew(i,j-1,k,m)
     &                              - dtdx * ( gadd(k,m,2,-1)
     &                                     -   gadd(k,m,1,-1) )
     &                              - dtdy * hadd(k,m,1,0)
                  qnew(i-1,j-1,k,m) = qnew(i-1,j-1,k,m)
     &                              - dtdx * gadd(k,m,1,-1)
     &                              - dtdy * hadd(k,m,1,-1)
                  qnew(i-1,j,k,m)   = qnew(i-1,j,k,m)
     &                              - dtdx * gadd(k,m,1,0)
     &                              - dtdy * ( hadd(k,m,2,-1)
     &                                     -   hadd(k,m,1,-1) )
                  qnew(i-1,j+1,k,m) = qnew(i-1,j+1,k,m)
     &                              - dtdx * gadd(k,m,1,1)
     &                              + dtdy * hadd(k,m,2,-1)
c
 120           continue
c
            else
c
c              # with capa array
c
               do 145 m=1,meqn
                  do 145 k=1,mz
                     qnew(i,j,k,m) = qnew(i,j,k,m) + qadd(k,m)
     &                        - (dtdz * (fadd(k+1,m) - fadd(k,m))
     &                        +  dtdx * (gadd(k,m,2,0) - gadd(k,m,1,0))
     &                        +  dtdy * (hadd(k,m,2,0) - hadd(k,m,1,0)))
     &                        / aux(i,j,k,mcapa)
                     qnew(i,j+1,k,m) = qnew(i,j+1,k,m)
     &                               - (dtdx * ( gadd(k,m,2,1)
     &                                      -   gadd(k,m,1,1) )
     &                               -  dtdy * hadd(k,m,2,0))
     &                               / aux(i,j+1,k,mcapa)
                     qnew(i+1,j+1,k,m) = qnew(i+1,j+1,k,m)
     &                                 + (dtdx * gadd(k,m,2,1)
     &                                 +  dtdy * hadd(k,m,2,1))
     &                                 / aux(i+1,j+1,k,mcapa)
                     qnew(i+1,j,k,m)   = qnew(i+1,j,k,m)
     &                                 + (dtdx * gadd(k,m,2,0)
     &                                 - dtdy * ( hadd(k,m,2,1)
     &                                        -   hadd(k,m,1,1) ))
     &                                 / aux(i+1,j,k,mcapa)
                     qnew(i+1,j-1,k,m) = qnew(i+1,j-1,k,m)
     &                                 + (dtdx * gadd(k,m,2,-1)
     &                                 -  dtdy * hadd(k,m,1,1))
     &                                 / aux(i+1,j-1,k,mcapa)
                     qnew(i,j-1,k,m)   = qnew(i,j-1,k,m)
     &                                 - (dtdx * ( gadd(k,m,2,-1)
     &                                        -   gadd(k,m,1,-1) )
     &                                 +  dtdy * hadd(k,m,1,0))
     &                                 / aux(i,j-1,k,mcapa)
                     qnew(i-1,j-1,k,m) = qnew(i-1,j-1,k,m)
     &                                 - (dtdx * gadd(k,m,1,-1)
     &                                 +  dtdy * hadd(k,m,1,-1))
     &                                 / aux(i-1,j-1,k,mcapa)
                     qnew(i-1,j,k,m)   = qnew(i-1,j,k,m)
     &                                 - (dtdx * gadd(k,m,1,0)
     &                                 + dtdy * ( hadd(k,m,2,-1)
     &                                       -   hadd(k,m,1,-1) ))
     &                                 / aux(i-1,j,k,mcapa)
                     qnew(i-1,j+1,k,m) = qnew(i-1,j+1,k,m)
     &                                 - (dtdx * gadd(k,m,1,1)
     &                                 -  dtdy * hadd(k,m,2,-1))
     &                                 / aux(i-1,j+1,k,mcapa)
c
 145              continue
c
         end if
c
 150  continue
  151 continue
c
c
      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
c
c  Modified:
c
c    16 September 2005
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

      character ( len = 8 ) date
      character ( len = 10 ) time

      call date_and_time ( date, time )

      write ( *, '(a8,2x,a10)' ) date, time

      return
      end
      function zeroin ( ax, bx, f, tol )                                         

c*********************************************************************72
c
cc ZEROIN computes a zero of a function in an interval.
c
c  Discussion:
c         
c    This routine locates a zero of the function F(X) in the interval AX ,BX.
c
c    It is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
c    without  a  check.  zeroin  returns a zero  x  in the given interval 
c    ax,bx  to within a tolerance  4*macheps*dabs(x) + tol, where macheps
c    is the relative machine precision.   
c  
c  Modified:
c
c    07 April 2006
c
c  Reference:
c
c    Richard Brent,
c    Algorithms for Minimization without Derivatives,
c    Prentice-Hall, 1973.
c
c  Parameters:
c         
c    Input, real AX, BX, the left and right endpoints of the initial interval.
c    
c    Input, external F, the name of the function subprogram which evaluates F(X).
c    This function should have the form:
c      function f ( x )
c      real f
c      real x
c
c    Input, real TOL, a nonnegative tolerance on the size of the uncertainty \
c    interval.   
c               
c    Output, real ZEROIN, a point at which the value of F(X) is approximately zero.  
c             
      implicit double precision (a-h,o-z)

      external f
c         
c  compute eps, the relative machine precision    
c         
      eps = 1.d0     
   10 eps = eps/2.d0 
      tol1 = 1.d0 + eps        
      if (tol1 .gt. 1.d0) go to 10       
c         
c initialization    
c         
      a = ax        
      b = bx        
      fa = f(a)     
      fb = f(b)     
c         
c begin step        
c         
   20 c = a         
      fc = fa       
      d = b - a     
      e = d         
   30 if (dabs(fc) .ge. dabs(fb)) go to 40
      a = b         
      b = c         
      c = a         
      fa = fb       
      fb = fc       
      fc = fa       
c         
c convergence test  
c         
   40 tol1 = 2.d0*eps*dabs(b) + 0.5*tol   
      xm = .5*(c - b)         
      if (dabs(xm) .le. tol1) go to 90   
      if (fb .eq. 0.d0) go to 90         
c         
c is bisection necessary      
c         
      if (dabs(e) .lt. tol1) go to 70    
      if (dabs(fa) .le. dabs(fb)) go to 70
c         
c is quadratic interpolation possible   
c         
      if (a .ne. c) go to 50  
c         
c linear interpolation        
c         
      s = fb/fa     
      p = 2.d0*xm*s  
      q = 1.d0 - s   
      go to 60      
c         
c inverse quadratic interpolation       
c         
   50 q = fa/fc     
      r = fb/fc     
      s = fb/fa     
      p = s*(2.d0*xm*q*(q - r) - (b - a)*(r - 1.d0))
      q = (q - 1.d0)*(r - 1.d0)*(s - 1.d0) 
c         
c adjust signs      
c         
   60 if (p .gt. 0.d0) q = -q  
      p = dabs(p)    
c         
c is interpolation acceptable 
c         
      if ((2.d0*p) .ge. (3.d0*xm*q - dabs(tol1*q))) go to 70   
      if (p .ge. dabs(0.5*e*q)) go to 70 
      e = d         
      d = p/q       
      go to 80      
c         
c bisection         
c         
   70 d = xm        
      e = d         
c         
c complete step     
c         
   80 a = b         
      fa = fb       
      if (dabs(d) .gt. tol1) b = b + d   
      if (dabs(d) .le. tol1) b = b + dsign(tol1, xm)
      fb = f(b)     
      if ((fb*(fc/dabs(fc))) .gt. 0.d0) go to 20    
      go to 30      
c         
c done    
c         
   90 zeroin = b    
      return        
      end 
