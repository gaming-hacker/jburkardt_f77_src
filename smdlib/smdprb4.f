!  smdprb4.f  13 May 1997
!
      program smdprb4
!
!***********************************************************************
!
!  This program displays all printable characters in a .FNT file
!  to provide a quick look at the characters in the font.
!
      real chr
      real gsxlcm
      real gsylcm
      integer ierr
      integer ifont
      character*32 label1
      character*32 label2
      character*32 label3
      integer lun
      character*1 orient
      character*32 title
      real xlen
      real ylen
 
      data label1/' !"#$%&''()*+,-./0123456789:;<=>?'/
 
      data label2/'@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'/
 
      data label3/' abcdefghijklmnopqrstuvwxyz{|}~ '/
 
      write(*,*)' '
      write(*,*)'SMDPRB4'
      write(*,*)'  Display all printable characters in a font.'
      write(*,*)' '
!
!  Define the graphics output device to be a PostScript file,
!  in portrait orientation, written to unit 7.
!
      lun=7
      orient = 'P'
 
      call postsc(lun,orient)
!
!  For each font
!
      do ifont=1,18
 
        write(title,'(''Font #'',I2)')ifont
        chr=0.0
        call mapsiz(0.,100.,0.,100.,chr)
        xlen=gsxlcm()
        ylen=gsylcm()
        call gswndo(0.,1.,0.,1.,0.5,0.5,xlen-1.,ylen-1.)
        call gscolr(1,ierr)
 
        call gsfont(ifont,ierr)
 
        if(ierr.ne.0)then
          write(*,*)' '
          write(*,*)'SMDPRB4 - Fatal error!'
          write(*,*)'  Font error ',ierr
          stop
        endif
 
        call gssetc(.025,0.0)
        call gsmove(0.25,0.90)
        call gspstr(title)
        call gsmove(0.025,0.75)
        call gspstr(label1)
        call gsmove(0.025,0.50)
        call gspstr(label2)
        call gsmove(0.025,0.25)
        call gspstr(label3)
 
        if(ifont.lt.18)call gsdrvr(2,0.0,0.0)
 
      end do
!
!  Terminate the plot.
!
      call stoplt
!
!  Terminate the graphics session.
!
      call finplt
 
      stop
      end
