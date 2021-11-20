!  sesplot.f  02 July 1997
!
      subroutine plot ( num_indx, indx, pmin, pmax, labels, rbuf,
     &  rec_num, recbuf, npts)
!
!*******************************************************************
!
!  Module:   PLOT
!  Date:     7/10/89
!  Author:   Chris Whittington
!
!  Functional Description:
!
!     This routine plots data from an ODF file to the VERSATEC
!     printer, 4 plots to a page.  It uses DISSPLA graphics
!     routines to generate the plots.
!
!  Calling Sequence:
!
!      integer*4      num_indx  - Number of variable indexes in array indx
!      integer*4      indx      - Array of variable indexes to be plotted
!      real*4         pmin      - Array of plot mins
!      real*4         pmax      - Array of plot maxs
!      character*(*)  labels    - Array of plot labels
!      real*4         rbuf      - Array of data points
!      integer*4      rec_num   - Number of records for each set of data points
!      integer*4      npts      - Number of data points for each variable
!
!******************************************************************* 
!
      real epsilon
      real p_frametime
      integer p_max_nodfvar
      integer p_max_points
      integer p_maxrecord
      integer p_maxtime
      integer x_div
      integer y_div

      parameter ( p_frametime = .030 )    ! Smallest frame time to be used
      parameter ( p_max_nodfvar = 256 )   ! Maximum number of output variables
      parameter ( p_maxrecord = 100 )     ! Maximum number of records
      parameter ( p_maxtime = 300 )       ! Longest recording time (5 min.)
      parameter ( y_div = 2 )             ! Number of divisions on y-axis
      parameter ( x_div = 7 )             ! Number of divisions on x-axis
      parameter ( epsilon = 1.0e-10 )

      parameter ( 
     &  p_max_points = p_max_nodfvar * p_maxtime / p_frametime )
!
      integer*4 device_len
      character*6 device_type
      character*1 dummy
      character*1 esc
      integer*4 i
      integer ifind_nonblank
      integer ilen
      integer*4 indx(p_max_nodfvar)
      integer*4 istart
      integer*4 j
      integer*4 k
      character*(*) labels(p_max_nodfvar)
      integer*4 m
      integer*4 npts
      integer*4 num_indx
      integer*4 num_ticks
      character*60 plot_label(2)
      integer*4 plots_per_page
      real*4 pmax(p_max_nodfvar)
      real*4 pmin(p_max_nodfvar)
      real*4 rbuf(p_max_points)
      integer*4 rec_num
      integer*4 recbuf(p_maxrecord)
      real xdelta
      real xmax
      real xmin
      real xstart 
      real xstop
      real ydelta
      real ymax
      real ymin
      real ystart 
      real ystop
!
      common /plot_info/ plot_label 
!
      data device_type /'      '/
      data esc /27/
      data plots_per_page /0/
!
      call LIB$GET_SYMBOL('TMP_PLOT_DEVICE',device_type,device_len)
 
      if ((device_type(1:device_len) .eq. 'PRQ120') .or.
     &    (device_type(1:device_len) .eq. 'PRQ153') .or.
     &    (device_type(1:device_len) .eq. 'PRQ159')) then
        write(*,*) 'SES PLOT: VERSATEC DEVICE SELECTED...'
        call VRSTEC(1200,1,0)             ! Initialize Versatec Plotter
      else if (device_type(1:device_len) .eq. 'PRQ166') then
        write(*,*) 'SES PLOT: LN03 DEVICE SELECTED...'
        call LN03I
      else if (device_type(1:device_len) .eq. 'PRQ133') then
        write(*,*) 'SES PLOT: QMS DEVICE SELECTED...'
        call QMS2
      else
        write(*,*) ESC,'[?6L]'
        write(*,*) ESC,'[2J'
        write(*,*) '**************************************************'
        write(*,*) '* SES.OUTPUT: SES_PLOT_DEVICE not defined        *'
        write(*,*) '* or unknown device type.  Versatec is default   *'
        write(*,*) '* device.  Please define the SES_PLOT_DEVICE     *'
        write(*,*) '* logical to point to your output device.  The   *'
        write(*,*) '* following devices are currently supported:     *'
        write(*,*) '*                                                *'
        write(*,*) '*   PRQ120 - Versatec Printer (ASDC CLUSTER)     *'
        write(*,*) '*   PRQ133 - QMS Printer (ASDC CLUSTER)          *'
        write(*,*) '*   PRQ153 - Versatec Printer (ENG LOADS)        *'
        write(*,*) '*   PRQ159 - Versatec Printer (ENG HQ)           *'
        write(*,*) '*   PRQ166 - LN03 Printer (ROTORS MVII)          *'
        write(*,*) '*                                                *'
        write(*,*) '* Please call John Cluff (x3468) if your printer *'
        write(*,*) '* is not supported.                              *'
        write(*,*) '*                                                *'
        write(*,*) '* Example:                                       *'
        write(*,*) '* $ DEFINE SES_PLOT_DEVICE PRQ133                *'
        write(*,*) '**************************************************'
        write(*,*)' '
        write(*,*)' Hit RETURN to continue...'
!
!  Initialize the Versatec plotter:
!
        read (*,'(a1)') dummy
        call VRSTEC(1200,1,0)             
      end if
!
!  Turn off hardware scaling
!
      call HWSCAL('NONE')               
!
!  Turn off border.
! 
      call NOBRDR
 
      if (device_type(1:device_len) .eq. 'PRQ133') then
         call HWROT('MOVIE')
         call PHYSOR(0.5,1.0)              ! Establish physical origin
      else if (device_type(1:device_len) .eq. 'PRQ166') then
         call HWROT('COMIC')
         call PHYSOR(0.75,1.0)              ! Establish physical origin
      ELSE
         call HWROT('COMIC')
         call PHYSOR(0.55,0.5)              ! Establish physical origin
      end if
 
      call PAGE(8.5,11.)               ! Set page to portrait
 
      call SIMPLX                       ! Set character style
 
      call GRACE(0.)                    ! Keep curve inside plot border
 
      call NOCHEK                       ! Supress point messagees
 
      call INTAXS                       ! Make whole numbers appear as
                                        ! integers on X and Y axes
!
!  Autoscale the TIME axis if no plot window values were given
!
      if ( pmin(1) .eq. 0.0 .and. pmax(1) .eq. 0.0 ) then
 
          xmin = rbuf(1)
          xmax = rbuf(recbuf(1))
          m = 1
 
          do m = 2, rec_num
 
            istart = num_indx*npts*(m-1)
 
            if ( xmax .lt. rbuf(istart+recbuf(m)) ) then
              xmax = rbuf(istart+recbuf(m))
            end if
 
            if ( xmin .gt. rbuf(istart+1) ) then
              xmin = rbuf(istart+1)
            end if
 
          end do
 
          call get_plot_size (xmin,xmax,x_div,xstart,
     &                       xstop,xdelta,num_ticks)
 
      else
 
        xstart = pmin(1)
        xstop  = pmax(1)
        xdelta = abs ( xstop - xstart ) / x_div
 
      end if
!
!  Loop and plot each selected variable versus time.
!
      do i = 2, num_indx
 
          j = indx(i)
!
!  Define area of plot
!
          call AREA2D(7.,2.)       ! 7 in.along x-axis and 2 in. along y-axis
!
!  Put Variable name on Y-axis as its label
!
          ilen = IFIND_NONBLANK(labels(j),0)
 
          call yname(labels(j)(1:ilen), ilen)
!
!  Put "TIME" label only on bottom plot's X-axis; others have no label
!
          if (plots_per_page .eq. 0) then
            call XNAME('TIME',100)
          else
            call XNAME(' ',100)
          end if
!
!  Autoscale the Y-axis if no plot min or plot max values were given
!
          if ( pmin(j) .eq. 0.0 .and. pmax(j) .eq. 0.0 ) then
 
              istart = npts*(i - 1) + 1
              ymin = rbuf(istart)
              ymax = rbuf(istart)
 
              do m = 1, rec_num
                istart = num_indx*npts*(m-1) + npts*(i-1)
                do k = 2, recbuf(m)
                  if (rbuf(istart+k) .gt. ymax) ymax = rbuf(istart+k)
                  if (rbuf(istart+k) .lt. ymin) ymin = rbuf(istart+k)
                end do
              end do
 
              call get_plot_size (ymin,ymax,y_div,ystart,
     &                            ystop,ydelta,num_ticks)
 
!
!  Temporary fix which assumes that if ystart and ystop are really close
!  to ZERO, then hard code the Y-axis limits
!
              if (ystart .lt. epsilon .and. ystart .gt. 0.0 .and.
     &            ystop .lt. epsilon .and. ystop .gt. 0.0) then
                ystart = -1.0
                ystop = 1.0
                ydelta = 1.0
              end if
 
!
!  Else use user supplied plot min/max values for Y-axis scaling
!
          else
 
            ystart = pmin(j)
            ystop  = pmax(j)
            ydelta = abs(ystop - ystart) / y_div
 
          end if
 
!
!  Put the X and Y axis on the plot
!
          call HEIGHT(.12)
          call graf(xstart,xdelta,xstop,ystart,ydelta,ystop)
          call RESET('HEIGHT')
!
!  Draw the dashed grid
!
          call DASH
          call GRID(1,2)
          call RESET('DASH')
!
!  Draw the curve defined by the data points
!
          do m = 1, rec_num
            istart = num_indx*npts*(m-1) + npts*(i-1) + 1
            call CURVE(rbuf(1),rbuf(istart),recbuf(m),0)
          end do

          plots_per_page = plots_per_page + 1
!
!  If current plot page is full (4 plots allowed on a page) .or.
!     the last plot has been drawn,
!  Put plot labels at the top of the page and reset parameters
!  for the next page
!
          if (plots_per_page .eq. 4 .or. i .eq. num_indx) then
              call message (plot_label(1),100,2.,2.6)
              call message (plot_label(2),100,0.,2.2)
              call ENDPL(0)                     ! End current page of plots
              if (device_type(1:device_len) .eq. 'PRQ133') then
                 call PHYSOR(0.5,1.0)              ! Establish physical origin
              else if (device_type(1:device_len) .eq. 'PRQ166') then
                 call PHYSOR(0.75,1.0)              ! Establish physical origin
              else
                 call PHYSOR(0.55,0.5)              ! Establish physical origin
              end if
!
!  Reset plot counter
!
              plots_per_page = 0                
!
!  Else page is not full, so end the current subplot and move
!  The relative origin 2.5 inches up the page for the next subplot
!
          else

            call ENDGR(0)
            call OREL(0.,2.4)
 
          end if
 
      end do 
!
!  Close communication with output device
!
      call DONEPL
 
 100  FORMAT(' WARNING: Data values limited for variable #',
     &         I3,' - ',A31,
     &     /,' NEW LIMITS: max value = ',G12.6,2X,'min value = ',G12.6)
 
      return
      end
      subroutine get_plot_size(xmin,xmax,ndivs,pxmin,pxmax,
     &  pxstep,pxticks)
!
!**********************************************************************
!
!  get_plot_size
!
!  INPUTS:
!    xmin - Lower limit of data
!    xmax - Upper limit of data
!    ndivs - Number of divisions to allow on the axis
!
!  OUTPUTS:
!    pxmin - Minimum value for the plot axis
!    pxmax - Maximum value for the plot axis
!    pxstep - Step size for the plot axis
!    pxticks - Number of subdivisions per step on the axis
!             (for displaying tick marks)
!
!**********************************************************************
!
      integer n_steps
      parameter (n_steps=5)
c
      real best_merit
      real dif
      integer i
      real intlog
      real intlogdif
      integer itmp
      integer j
      real merit
      integer ndivs
      real norm_dif
      real pxmax
      real pxmax_temp
      real pxmin
      real pxmin_temp
      real pxstep
      real pxstep_temp
      integer pxticks
      real steps(5)
      real temp
      integer ticks(5)
      real xmax
      real xmin
c
      data (steps(i),i=1,5) /1., 2., 4., 5., 10./
      data (ticks(i),i=1,5) /5, 4, 4, 5, 5/
!
      if ( xmax .ne. xmin ) then
        norm_dif = (xmax - xmin)/max(abs(xmax),abs(xmin))
      else
        norm_dif = 0.0
      end if
 
      if ((xmax .eq. xmin) .or. (norm_dif .lt. 0.00001)) then

        if (xmax .eq. 0.0 ) then

          pxstep = 1.0

        else

          temp = log10(abs(xmax))
          intlog = int(temp)

          if (temp .lt. 0) then
            intlog = intlog - 1
          end if

          pxstep = 10.0**intlog

          if (pxstep .gt. 1.0) then
            pxstep = 1.0
          end if

        end if

        pxticks = 5
        itmp = ndivs/2
        pxmin = xmax - itmp * pxstep
        pxmax = xmax + (ndivs - itmp) * pxstep

      else

        best_merit = -999.0
        dif = xmax - xmin
        temp = log10(dif) - log10(float(ndivs))
        intlogdif = int(temp)

        if (temp .lt. 0.0) then
          intlogdif = intlogdif - 1.
        end if

        do j = 1, 2

          do i = 1, n_steps

            pxstep_temp = steps(i) * 10.0**intlogdif

            if (ndivs*pxstep_temp .gt. dif) then

              pxmin_temp = xmin - mod(xmin, pxstep_temp)

              if (xmin .lt. 0.0) then
                pxmin_temp = pxmin_temp - pxstep_temp
              end if

              pxmax_temp = pxmin_temp + ndivs * pxstep_temp

              if (pxmax_temp .gt. xmax) then

                merit = (xmax - xmin) / (pxmax_temp - pxmin_temp)

                if (merit .gt. best_merit) then
                  best_merit = merit
                  pxmax = pxmax_temp
                  pxmin = pxmin_temp
                  pxstep = pxstep_temp
                  pxticks = ticks(i)
                end if

              end if

            end if

          end do

          intlogdif = intlogdif + 1

        end do

      end if
 
      return
      end
