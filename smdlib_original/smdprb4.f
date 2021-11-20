C  SMDPRB4.F  09 November 1992
C   
      program smdprb4
c
c***********************************************************************
c
C  This program displays all printable characters in a .FNT file 
C  to provide a quick look at the characters in the font.
C
      real chr
      real gsxlcm
      real gsylcm
      integer ierr
      integer ifont
      character*32 label1
      character*32 label2
      character*32 label3
      character*32 title
      real xlen
      real ylen
  
      data label1/' !"#$%&''()*+,-./0123456789:;<=>?'/
  
      data label2/'@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'/
    
      data label3/' abcdefghijklmnopqrstuvwxyz{|}~ '/   
 
      write(*,*)' '
      write(*,*)'SMDPRB4'
      write(*,*)'Display all printable characters in a font.'
      write(*,*)' '
c
c  Choose the output device
c
      call psccgm('cgmb','smdprb4.cgm')
c
c  For each font
c
      do 10 ifont=1,18
        write(title,'(''Font #'',I2)')ifont
        chr=0.0
        call mapsiz(0.,100.,0.,100.,chr)  
        xlen=gsxlcm() 
        ylen=gsylcm() 
        call gswndo(0.,1.,0.,1.,0.5,0.5,xlen-1.,ylen-1.)  
        call gscolr(1,ierr)   
        ierr=0
        call gsfont(ifont,ierr) 
C 
        if(ierr.ne.0)then
          write(*,*)'Font error ',ierr
          go to 10
        endif
C
        call gssetc(.025,0.0)  
        call gsmove(0.25,0.90)
        call gspstr(title)
        call gsmove(0.025,0.75)   
        call gspstr(label1)   
        call gsmove(0.025,0.50)   
        call gspstr(label2)   
        call gsmove(0.025,0.25)   
        call gspstr(label3)
C 
        if(ifont.lt.18)call gsdrvr(2,0.0,0.0)
10    continue
C
      call stoplt(0)
      call finplt
C
      stop
      end   
