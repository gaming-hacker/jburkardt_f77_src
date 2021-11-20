      program main

c***********************************************************************
c
cc FSTREAM tests the NCARG STREAM routine for drawing streamlines.
c
c  Modified:
c
c    15 September 2006
c
c  Local parameters:
c
c    Local parameter, integer IDM is a dummy variable for STINIT and STREAM.
c
c    Local parameter, integer M, N, the number of rows and columns of data.
c
c    Local parameter, real U(M,N), V(M,N), the horizontal and vertical 
c    components of the vector quantity at each grid point.
c
      implicit none

      integer m
      integer n

      parameter ( m = 21 )
      parameter ( n = 25)

      integer idm
      integer ierrf
      integer iszdm
      integer iwkid
      integer iwtype
      integer lunit
      real u(m,n)
      real v(m,n)
      real wrk(2*m*n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FSTREAM:'
      write ( *, '(a)' ) '  Set up some vector data, and show'
      write ( *, '(a)' ) '  how the NCARG routine STREAM can create'
      write ( *, '(a)' ) '  a streamline plot.'
c
c  Define:
c    IERFF = the error file, 
c    IWKID = the workstation ID.
c    LUNIT = the FORTRAN unit number.
c
      ierrf = 6
      iwkid = 1
      lunit = 2
c
c  The workstation type IWTYPE will determine what kind of output is created
c  1: NCGM, an NCAR CGM file, called "gmeta" by default.
c  7: Display graphics in the current X window;
c  8: Display graphics in a new X window.
c  21: Color EPS File, Portrait
c  27: Color EPS File, Landscape
c
      iwtype = 21
c
c  Generate some data.
c
      call mkdat ( m, n, u, v )
c
c  Initialize GKS, open a workstation, active the workstation.
c
      call gopks ( ierrf, iszdm )
      call gopwk ( iwkid, lunit, iwtype )
      call gacwk ( iwkid )
c
c  Select the normalization transformation 0.
c  This means that user coordinates are the same as NDC coordinates.
c  This way, the title is drawn at the top of the plot.
c
      call gselnt ( 0 )
c
c  Write the plot title.
c
      call plchlq ( 0.5, 0.9765, 'Example Streamlines Plot',
     &  16.0, 0.0, 0.0 )
c
c  Define normalization transformation 1, and set up linear scaling.
c
      call set ( 0.1, 0.9, 0.1, 0.9, 1.0, 21.0, 1.0, 25.0, 1 )
c
c  Tell Streamlines that SET has been called, and
c  set the spacing of stream lines.
c
      call stseti ( 'SET -- Set Call Flag', 0 )
      call stsetr ( 'SSP -- Stream Spacing', 0.015 )
c
c  Initialize Streamlines.
c
      call stinit ( u, m, v, m, idm, idm, m, n, wrk, 2*m*n )
c
c  Draw the streamlines.
c
      call stream ( u, v, idm, idm, idm, wrk )
c
c  Close the frame
c
      call frame
c
c  Deactivate the workstation,
c  close the workstation,
c  close GKS.
c
      call gdawk ( iwkid )
      call gclwk ( iwkid )
      call gclks

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FSTREAM:'
      write ( *, '(a)' ) '  Normal end of execution.'

      stop
      end
      subroutine mkdat ( m, n, u, v )
c
c***********************************************************************
c
cc MKDAT sets up some vector data on a grid.
c
c  Modified:
c
c    15 September 2006
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of data.
c
c    Output, real U(M,N), V(M,N), the horizontal and vertical components
c    of the vector quantity at each grid point.
c
      implicit none

      integer m
      integer n

      real angle_i
      real angle_j
      integer i
      integer j
      real pi
      real u(m,n)
      real v(m,n)

      pi = 3.14159265
c
c  Specify horizontal and vertical vector components U and V on
c  the rectangular grid. 
c
      do i = 1, m
        angle_i = pi * real ( 2 * ( i - 1 ) ) / real ( m - 1 )
        do j = 1, n
          u(i,j) = sin ( angle_i ) 
        end do
      end do

      do j = 1, n
        angle_j = pi * real ( 2 * ( j - 1 ) ) / real ( n - 1 )
        do i = 1, m
          v(i,j) = sin ( angle_j )
        end do
      end do

      return
      end
