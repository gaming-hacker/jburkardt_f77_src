      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS550_PRB.
c
c  Discussion:
c
c    TOMS550_PRB tests the TOMS550 library.
c
c    TOMS550_PRB reads a data file defining a solid, and calls PROPS
c    to compute the mass properties of the solid.
c
c    The solid is described by a set of surfaces which are
c    triangulated, and a four point Gaussian quadrature over
c    the triangles is used in calculating surface area,
c    volume, and mass properties.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    Adrian Messner, G Taylor
c
c  Reference:
c
c    Adrian Messner, G Taylor,
c    Algorithm 550:
c    Solid Polyhedron Measures,
c    ACM Transactions on Mathematical Software,
c    Volume 6, Number 1, March 1980, pages 121-130.
c
      implicit none

      integer maxkv
      parameter ( maxkv = 6000 )

      real a(10000)
      real area
      real cg(3)
      real dens
      logical error
      character*(80) heading
      real icg(3)
      real io(3)
      integer ka(maxkv)
      integer maxs
      integer maxv
      integer n1
      integer n2
      integer n3
      integer n4
      integer n5
      integer n6
      integer n7
      integer n8
      integer nerr
      real pr(3)
      real prcg(3)
      real vol
      real weight

      common / r4_common / maxs, maxv, area, weight, vol, dens,
     &  cg, io, icg, pr, prcg, error, nerr

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS550_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS550 library.'
      write ( *, '(a)' ) ' '
c
c  title and control cards are read for current case.
c
      read ( *, '(a)' ) heading
      write ( *, '(a)' ) heading
c
c  Read the maximum number of vertices, surfaces, and the density.
c
      read ( *, * ) maxv, maxs, dens

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Maximum number of vertices = ', maxv
      write ( *, '(a,i8)' ) '  Maximum number of surfaces = ', maxs
      write ( *, '(a,g14.6)' ) '  Density = ', dens
c
c  Array pointers are determined.
c
c  Pointer  Array
c
c    N1 --> VX
c    N2 --> VY
c    N3 --> VZ
c
c    N4 --> KFA
c    N5 --> AX
c    N6 --> AY
c    N7 --> AZ
c    N8 --> KV
c
      error = .false.

      n1 = 1
      n2 = n1 + maxv
      n3 = n2 + maxv

      n4 = 1
      n5 = n3 + maxv
      n6 = n5 + maxs
      n7 = n6 + maxs
      n8 = n4 + maxs
c
c  Read the vertex coordinates and face lists.
c
      call r4_faces ( ka(n8), ka(n4), a(n1), a(n2), a(n3), maxs, maxv,
     &  n8, maxkv, nerr, error )

      if ( error ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TOMS550_PRB:'
        write ( *, '(a,i8)' ) '  Error code = ', nerr
        stop
      end if
c
c  Calculate the mass properties.
c
      call r4_props ( ka(n8), ka(n4), a(n5), a(n6), a(n7),
     &  a(n1), a(n2), a(n3) )

      if ( error ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TOMS550_PRB:'
        write ( *, '(a,i8)' ) '  Error code = ', nerr
        stop
      end if
c
c  Print the mass properties.
c
      call r4_masspr ( dens, area, vol, weight, cg, io, icg,
     &  pr, prcg )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS550_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine r4_faces ( kv, kfa, vx, vy, vz, maxs, maxv,
     &  n8, maxkv, nerr, error )

c*********************************************************************72
c
cc R4_FACES reads vertex coordinates and face list data.
c
c  Discussion:
c
c    A face boundary is made up of a closed set of numbered
c    points (vertices) connected by straight lines.
c    A face list, enumerating the vertices of the face taken
c    in succession around the boundary, is the data
c    representation of the face.
c
c    A surface of the solid may be composed of more than one
c    face, an initial face and secondary face(s).  They are
c    linked together in storage by a pointer from one face
c    to another.
c
c    There are no practical restrictions on the number of
c    vertices per face.  The maximum number of surfaces is
c    controlled by the extent of available computer storage
c    and is also dependent on the total number of vertices.
c
c  Modified:
c
c    30 December 2006
c
c  Author:
c
c    Adrian Messner, G Taylor
c
c  Parameters:
c
c    Output, integer KV(*), an array containing face vertex information.
c
c    Output, integer KFA(MAXS), contains pointers to the initial face
c    list in KV for each surface, ordered by surface number.
c    If L1, L2, and L3 are the locations in KV of face lists for
c    surfaces 1, 2, 3, then the program would set
c      KFA(1) = L1
c      KFA(2) = L2
c      KFA(3) = L3
c    A pointer value of zero (KFA(I) = 0) indicates that surface I
c    was not utilized in defining the solid.  Hence gaps may
c    occur in the surface numbers.  Also, the order of data
c    entry of the face lists can be entirely arbitrary,
c    because of the afore-mentioned linkage mechanism.
c
c    Output, real VX(MAXV), VY(MAXV), VZ(MAXV), the vertex coordinates.
c
c    Input, integer MAXS, the number of surfaces.
c
c    Input, integer MAXV, the number of vertices.
c
c    Input, integer N8, ?
c
c    Input, integer MAXKV, ?
c
c    Output, integer NERR, ?
c
c    Output, logical ERROR, ?
c
      implicit none

      integer maxs
      integer maxv

      logical error
      integer i
      integer iend
      integer k
      integer kfa(maxs)
      integer kk
      integer kv(*)
      integer m
      integer m1
      integer m2
      integer m3
      integer maxkv
      integer n8
      integer nerr
      integer nf
      integer nv
      integer vertex
      integer vertex1
      real vx(maxv)
      real vy(maxv)
      real vz(maxv)
      real x
      real y
      real z
c
c  Read the vertex coordinates.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' Vertex         X            Y            Z'
      write ( *, '(a)' ) ' '

1     continue

      read ( *, '(i1,i4,5x,3f10.0)' ) iend, nv, x, y, z

      write ( *, '(i6,3f13.2)' ) nv, x, y, z

      if ( nv .le. 0 .or. nv .gt. maxv ) then
        nerr = 1
        error = .true.
        return
      end if

      vx(nv) = x
      vy(nv) = y
      vz(nv) = z

      if ( iend .eq. 0 ) then
        go to 1
      end if
c
c  Initialize the face pointer array.
c
      do i = 1, maxs
        kfa(i) = 0
      end do

      m1 = 1
c
c  Read the next card.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Face     Vertices'
      write ( *, '(a)' ) ' '

3     continue

      m2 = m1 + 2
      m3 = m1 + 16
c
c  Surface element definitions are input and stored.
c
      read ( *, '(i1,i4,15i5)' ) iend, kv(m1), ( kv(m), m = m2, m3 )

      nf = kv(m1)
      k = kfa(nf)
      kv(m1+1) = 0
c
c  Initial (or only) face definition.
c
      if ( k .eq. 0 ) then

        kfa(nf) = m1
c
c  Secondary face pointer is stored.
c
      else

4       continue

        kk = k
        k = kv(kk+1)

        if ( k .ne. 0 ) then
          go to 4
        end if

        k = kv(kk+1)

        if ( k .ne. 0 ) then
          go to 4
        end if

        kv(kk+1) = m1

      end if
c
c  Is the definition complete?
c
      vertex1 = kv(m2)
      m2 = m2 + 1

6     continue

      do i = m2, m3

        vertex = kv(i)

        if ( vertex .eq. vertex1 ) then
          go to 8
        end if

        if ( vertex .le. 0 .or. vertex .gt. maxv ) then
          go to 8
        end if

      end do
c
c  The definition is continued.
c
      m2 = m3 + 1
      m3 = m3 + 16
      read ( *, '(i1,i4,15i5)' ) iend, ( kv(m), m = m2, m3 )

      go to 6
c
c  Getting set for next surface, print current set of vertices.
c
8     continue

      m2 = m1 + 2
      m3 = i
      call r4_facvrt ( nf, kv, m2, m3 )
c
c  Is the vertex number illegal?
c
      if ( vertex .le. 0 .or. vertex .gt. maxv ) then
        nerr = 3
        error = .true.
        return
      end if

      m1 = i + 1

      if ( m1 + n8 .gt. maxkv ) then
        nerr = 4
        error = .true.
        return
      end if

      if ( iend .eq. 0 ) then
        go to 3
      end if

      return
      end
      subroutine r4_facvrt ( nf, kv, m2, m3 )

c*********************************************************************72
c
cc R4_FACVRT prints face vertices.
c
c  Modified:
c
c    02 January 2007
c
c  Author:
c
c    Adrian Messner, G Taylor
c
c  Parameters:
c
c    Input, integer NF, the index of the face.
c
c    Input, integer KV(*), an array containing face vertex information.
c
c    Input, integer M2, M3, the indices in KV of the first and last
c    vertices of the face.
c
      implicit none

      integer kv(*)
      integer m
      integer m2
      integer m3
      integer m4
      integer nf

      m4 = min0 ( m3, m2 + 9 )

      write ( *, '(i5,5x,10i5)' ) nf,( kv(m), m = m2, m4 )

      if ( m4 .eq. m3 ) then
        return
      end if

      m4 = m2 + 10
      write ( *, '(10x,10i5)' ) ( kv(m), m = m4, m3 )

      return
      end
      subroutine r4_masspr ( dens, area, vol, weight, cg, io, icg,
     &  pr, prcg )

c*********************************************************************72
c
cc R$_MASSPR prints mass properties of a solid.
c
c  Modified:
c
c    02 January 2007
c
c  Author:
c
c    Adrian Messner, G Taylor
c
c  Parameters:
c
c    Input, real DENS, the density.
c
c    Input, real AREA, the surface area.
c
c    Input, real VOL, the volume.
c
c    Input, real WEIGHT, the weight.
c
c    Input, real CG(3), the center of gravity.
c
c    Input, real IO(3), the mass moments with respect to the origin.
c
c    Input, real ICG, the mass moments with respect to the center of gravity.
c
c    Input, real PR(3), the product moments, with respect to the origin.
c
c    Input, real PRCG(3), the product moments, with respect to the
c    center of gravity.
c
      implicit none

      real area
      real cg(3)
      real dens
      integer i
      real icg(3)
      real io(3)
      real pr(3)
      real prcg(3)
      real vol
      real weight

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MASS PROPERTIES OF THE SOLID'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '          Density         Area          Volume         Weight'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,4f15.5)' ) dens, area, vol, weight
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '             Axes        X-comp         Y-comp         Z-comp'
      write ( *, '(a)' ) ' '
      write ( *, '(a,3f15.5)' ) '  cg       coord', ( cg(i), i = 1, 3 )
      write ( *, '(a,3f15.5)' ) 'moment     coord', ( io(i), i = 1, 3 )
      write ( *, '(a,3f15.5)' ) '              cg', ( icg(i), i = 1, 3 )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '             Axes        XY-comp        YZ-comp        ZX-comp'
      write ( *, '(a)' )
      write ( *, '(a,3f15.5)' ) 'prod mom   coord', ( pr(i), i = 1, 3 )
      write ( *, '(a,3f15.5)' )
     &  '              cg', ( prcg(i), i = 1, 3 )

      return
      end
