c  robplot.f  21 May 1997
c
c
c  Maybe later add "overplots", where one variable gets
c  plotted repeatedly, from different records.
c
c
c  Need 3 skips:
c    Skip data records.
c    Skip read records.
c    Skip plot points.
c
c
c  You could cut down plot points by refusing to add a point to the
c  plot list if its distance from the previous plot point is too
c  small, say
c    abs(x2-x1) /(xmax-xmin) + abs(y2-y1)/(ymax-ymin) < eps
c
      program main

c*********************************************************************72
c
cc MAIN is the main program for ROBPLOT.
c
c  ROBPLOT ???
c
c  MAXPPT is the maximum number of points we will try to plot.
c  If the user gives us more than that, we will thin them out.
c
      integer MAXPPT
      parameter (MAXPPT=72*10)
c
c  MAXRECL is the maximum number of characters on a line of the
c  input data file.
c
      integer MAXRECL
      parameter (MAXRECL=512)
c
c  MAXVAL is the maximum number of values a given variable can have.
c  This is essentially the number of time values in one record of the
c  data file, except that we can use a "skipping" option to sample
c  fewer values.
c
      integer MAXVAL
      parameter (MAXVAL=500)
c
c  MAXVAR is the maximum number of variables.
c
      integer MAXVAR
      parameter (MAXVAR=30)
c
      character*80 command
      real data(MAXVAL,0:MAXVAR)
      real datamax(0:MAXVAR)
      real datamin(0:MAXVAR)
      logical echo
      logical existin
      logical existps
      character*(MAXRECL) first
      integer i
      integer ierror
      integer ihelp
      character*80 infile
      integer iplot(0:MAXVAR)
      integer ippt
      integer iquit
      integer irang(MAXVAR+1)
      character*80 item
      integer ival
      integer ix
      integer j
      integer jppt
      character*(MAXRECL) last
      integer lenc
      integer lenchr
      logical leqi
      character*(MAXRECL) line
      character*1 mark
      character*20 names(0:MAXVAR)
      logical none
      integer nplot
      integer nppt
      integer nrang
      integer nthin
      integer nval
      integer nval2
      integer nvar
      integer nxgrid
      integer nxgrid2
      integer ny
      integer nygrid
      integer nygrid2
      logical over
      character*80 psfile
      integer recl
      integer nskip
      character*80 string
      real temp
      character*80 title
      real xdel
      real xmax
      real xmaxr
      real xmin
      real xminr
      real xppt(MAXPPT)
      real ydel
      real ymax(0:MAXVAR)
      real ymaxr(0:MAXVAR)
      real ymaxx(0:MAXVAR)
      real ymin(0:MAXVAR)
      real yminr(0:MAXVAR)
      real yminx(0:MAXVAR)
      real yppt(MAXPPT)
c
c  Initialize the data.
c
      call init(data,datamax,datamin,echo,existin,existps,infile,
     &  iplot,ix,mark,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &  nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,string,title,
     &  xmax,xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  Say hello.
c
      call hello
c
c  Print a brief help reminder.
c
      ihelp=1
      call help(ihelp)
c
c  Request a command.
c
10    continue
 
      write(*,*)' '
      write(*,*)'Enter a command:'
c
c  Skip the input prompt if the previous command was a comment.
c
20    continue
c
c  Read a command.
c
      read(*,'(a)',end=100,err=100)command
 
      if (echo) then
        lenc=lenchr(command)
        write(*,'(1x,a)')command(1:lenc)
      end if
c
c  # is a comment.
c
      if ( leqi(command(1:1),'#') ) then
        go to 20
c
c  ECHO=T/F
c
      else if (leqi(command(1:4),'ECHO') ) then
 
        call chrdb1(command)
 
        if (command(5:5) .eq. '=' ) then
          if (leqi(command(6:6),'T')) then
            echo = .true.
          else if (leqi(command(6:6),'F')) then
            echo = .false.
          else
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your logical input could not be understood.'
            go to 10
          end if
        else
          write(*,*)' '
          write(*,*)'Enter a value for ECHO ("T" or "F"):'
          read(*,'(l1)',end=100,err=100) echo
        end if
 
        item = 'ECHO'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  H
c
      else if ( leqi(command,'H') ) then
 
        ihelp=2
        call help(ihelp)
c
c  HELP
c
      else if ( leqi(command,'HELP')) then
 
        ihelp=3
        call help(ihelp)
c
c  INFILE =
c
      else if ( leqi(command(1:6),'INFILE')) then

        command(1:6)=' '
        call flushl ( command )
 
        if ( infile .ne. ' ' ) then
          close(unit=1)
          infile = ' '
        end if
 
        if ( command(1:1) .eq. '=' ) then
          infile = command(2:)
        else
          write(*,*)' '
          write(*,*)'Enter the input file name.'
          read(*,'(a)',end=100,err=100) infile
        end if
 
        item = 'INFILE'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
 
        inquire ( file=infile, exist=existin )
 
        if ( .not. existin ) then
          write(*,*)' '
          write(*,*)'RobPlot - Error!'
          write(*,*)'  The input file you specified'
          write(*,*)'  does not exist!'
          infile = ' '
        end if
 
        if ( recl .eq. 0 ) then
          open(unit=1, file=infile, status='old', form='formatted',
     &      access='sequential', err=110)
        else
          open(unit=1, file=infile, status='old', form='formatted',
     &      recl=recl, access='sequential',err=110)
        end if
c
c  Read the first record, which should contain the data variable names.
c  However, skip any initial '#' comment lines.
c
30      continue

        read(1,'(a)',end=120,err=120)line
        if (line(1:1).eq.'#') go to 30
 
        nvar=0
c
c  Figure out the individual data names.
c
40      continue
 
        call chrup3(line,first,mark,last)
        line = last
 
        if ( first.ne.' ' ) then
          nvar=nvar+1
          if ( nvar.le.MAXVAR ) then
            names(nvar)=first
          end if
          go to 40
        end if
 
        if ( nvar .gt. MAXVAR ) then
          write(*,*)' '
          write(*,*)'The number of variables in the file is NVAR=',nvar
          write(*,*)'This program can only handle the first MAXVAR='
          write(*,*)MAXVAR,' variables.'
          write(*,*)' '
          write(*,*)'NVAR is being reset to ',MAXVAR
          nvar=MAXVAR
        end if
 
        item = 'NAMES'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  INIT
c
      else if ( leqi(command,'INIT') ) then
 
        call init(data,datamax,datamin,echo,existin,existps,infile,
     &    iplot,ix,mark,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,string,title,
     &    xmax,xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  MARK =
c
      else if (leqi(command(1:4),'MARK')) then
 
        call chrdb1(command)
 
        if (command(5:5) .eq. '=' ) then
          mark = command(6:6)
        else
          write(*,*)' '
          write(*,*)'Enter the character that separates'
          write(*,*)'data names, and data values.'
          write(*,*)' '
          write(*,*)'Typical values are a comma or blank.'
          read(*,'(a)',end=100,err=100) mark
        end if
 
        item = 'MARK'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  NTHIN=
c
      else if (leqi(command(1:5),'NTHIN') ) then
 
        call chrdb1(command)
 
        if (command(6:6) .eq. '=' ) then
          call chrcti(command(7:),nthin,ierror,lenc)
          if (ierror.ne.0) then
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your numeric input could not be understood.'
            go to 10
          end if
        else
          write(*,*)' '
          write(*,*)'Enter the value for NTHIN:'
          read(*,'(a)',end=100,err=100) nthin
        end if
 
        item = 'NTHIN'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  OVER=T/F
c
      else if (leqi(command(1:4),'OVER') ) then
 
        call chrdb1(command)
 
        if (command(5:5) .eq. '=' ) then
          if (leqi(command(6:6),'T')) then
            over = .true.
          else if (leqi(command(6:6),'F')) then
            over = .false.
          else
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your logical input could not be understood.'
            go to 10
          end if
        else
          write(*,*)' '
          write(*,*)'Enter a value for OVER ("T" or "F"):'
          read(*,'(l1)',end=100,err=100) over
        end if
 
        item = 'OVER'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  PLOT
c
      else if (leqi(command,'PLOT')) then
 
        if (infile .eq. ' ') then
          write(*,*)' '
          write(*,*)'RobPlot - Warning!'
          write(*,*)'  You must specify an input data file first!'
          write(*,*)'  (Use the INFILE=... command.)'
          go to 10
        end if
 
        if (nvar .le. 0 )then
          write(*,*)' '
          write(*,*)'RobPlot - Warning!'
          write(*,*)'  There are no variables to plot!'
          write(*,*)'  Maybe something is wrong with your data.'
          go to 10
        end if
 
        if (nval .le. 0 )then
          write(*,*)' '
          write(*,*)'RobPlot - Warning!'
          write(*,*)'  There is no data to plot.'
          write(*,*)'  Maybe something is wrong with your data,'
          write(*,*)'  or you have not issued the READ command.'
          go to 10
        end if
 
        if (psfile .eq. ' ') then
          write(*,*)' '
          write(*,*)'RobPlot - Warning!'
          write(*,*)'  You must specify a PostScript file first!'
          write(*,*)'  (Use the PSFILE=... command.)'
          go to 10
        end if
 
        if (ix.lt.0.or.ix.gt.nvar) then
          write(*,*)' '
          write(*,*)'RobPlot - Warning!'
          write(*,*)'  You have not specified a legal X plot variable.'
          write(*,*)'  (Use the X=... command.)'
          go to 10
        end if
 
        if (ny .le. 0) then
          write(*,*)' '
          write(*,*)'RobPlot - Warning!'
          write(*,*)'  You have not specified any Y plot variables.'
          write(*,*)'  (Use the Y=... command.)'
          go to 10
        end if
 
        do i=0,nvar
 
          if (iplot(i).eq.1)then
 
            nplot = nplot+1
c
c  Extract plotting data from user data.
c
            ippt=0
            jppt=0
            nppt=0
 
            do j=1,nval
 
              if (xmin.le.data(j,ix).and.data(j,ix).le.xmax)then
                ippt=ippt+1
                if(nppt.lt.maxppt)then
                  jppt=jppt+1
                  if (mod(ippt-1,nthin).eq.0) then
                    nppt=nppt+1
                    xppt(nppt)=data(j,ix)
                    yppt(nppt)=data(j,i)
                  end if
                end if
              end if
 
            end do
 
            write(*,*)' '
            write(*,*)nval,' data points from the input file;'
            write(*,*)ippt,' data points within [XMIN,XMAX];'
            write(*,*)jppt,' data points considered for plotting;'
            write(*,*)nppt,' thinned data points within [XMIN,XMAX].'
 
            if (nppt.gt.1)then
              call graf(nplot,nppt,nxgrid,nygrid,over,psfile,title,
     &          xmaxr,xminr,
     &          names(ix),xppt,ymaxr(i),
     &          yminr(i),names(i),yppt)
            end if
 
          end if
 
        end do
c
c  PSFILE =
c
      else if (leqi(command(1:6),'PSFILE')) then
 
        call chrdb1(command)
 
        if (command(7:7) .eq. '=' ) then
          psfile = command(8:)
        else
          write(*,*)' '
          write(*,*)'Enter the PostScript file name.'
          read(*,'(a)',end=100,err=100) psfile
        end if
 
        item = 'PSFILE'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
 
        inquire(file=psfile,exist=existps)
 
        if (existps) then
          write(*,*)' '
          write(*,*)'RobPlot - Warning!'
          write(*,*)'  The PostScript file you specified'
          write(*,*)'  already exists!  When you issue the'
          write(*,*)'  PLOT command, the old file may be'
          write(*,*)'  destroyed.'
        end if
c
c  QUIT
c
      else if (leqi(command(1:1),'Q')) then
 
        iquit=2
        call quit(iquit,nplot)
c
c  READ
c
      elseif (leqi(command,'READ')) then
 
        if (infile .eq. ' ') then
          write(*,*)' '
          write(*,*)'RobPlot - Warning!'
          write(*,*)'  Please specify an input file first,'
          write(*,*)'  using the command INFILE=...'
          go to 10
        end if
c
c  Skip NSKIP records.
c
        call skip(nskip,ierror)
 
        if (ierror.ne.0) then
          write(*,*)' '
          write(*,*)'RobPlot - Error!'
          write(*,*)'  There was an error or end-of-file condition'
          write(*,*)'  while trying to skip ',nskip,' data records.'
          write(*,*)' '
          write(*,*)'  The data file has been closed.'
          write(*,*)'  (Reopen it with the INFILE=... command.)'
          close(unit=1)
          infile=' '
          go to 10
        end if
c
c  Now read the data associated with the current data record.
c
        nval=0
        nval2=0
 
50      continue
 
        if ( nval2+1.le.MAXVAL)then
          read(1,*,end=60,err=60)(data(nval2+1,j),j=1,nvar)
          nval=nval2+1
        else
          read(1,*,end=60,err=60)(temp,j=1,nvar)
        end if
 
        nval2 = nval2+1
 
        go to 50
 
60      continue
 
        write(*,*)' '
        write(*,*)nval,' values were read.'
        if(nval2.ne.nval)then
          write(*,*)nval2,' values were in the file.'
        end if
c
c  Fill in the array index column.
c
        do i=1,nval
          data(i,0)=i
        end do
c
c  Compute the minimum and maximum of each data item.
c
        do i=0,nvar
          call rrange(nval,data(1,i),datamax(i),datamin(i))
        end do
c
c  Initialize data and plot minima and maxima
c
        if (0 .le. ix .and. ix .le. nvar)then
 
          xmin=datamin(ix)
          xmax=datamax(ix)
 
          call niceax(xmin,xmax,nxgrid,xminr,xmaxr,xdel,nxgrid2)
 
        end if
 
        do i=0,nvar
          ymax(i)=datamax(i)
          ymin(i)=datamin(i)
        end do
 
        do i=0,nvar
          ymaxx(i)=datamax(i)
          yminx(i)=datamin(i)
        end do
 
        do i=0,nvar
          call niceax(ymin(i),ymax(i),nygrid,yminr(i),ymaxr(i),ydel,
     &      nygrid2)
        end do
 
        item = 'DATAMIN'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  RECL=
c
      else if (leqi(command(1:4),'RECL')) then
 
        call chrdb1(command)
 
        if (command(5:5) .eq. '=' ) then
          call chrcti(command(6:),recl,ierror,lenc)
          if (ierror.ne.0) then
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your numeric input could not be understood.'
            go to 10
          end if
        else
          write(*,*)' '
          write(*,*)'Enter the record length of your data input file'
          write(*,*)'  (or 0 for no explicit record length).'
          read(*,'(a)',end=100,err=100) recl
        end if
 
        if (recl .lt.0 .or. recl .gt. maxrecl) then
          write(*,*)' '
          write(*,*)'RobPlot - Error!'
          write(*,*)'  RECL must be between 0 and ',maxrecl
          write(*,*)'  but your value was ',recl
          go to 10
        end if
 
        item = 'RECL'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  SHOW
c
      else if (leqi(command(1:4),'SHOW')) then
 
        if (leqi(command,'SHOW')) then
          write(*,*)' '
          write(*,*)'Show WHAT?  Enter a name or ? for a list.'
          read(*,'(a)')string
        else if (index(command,'=').ne.0) then
          i = index(command,'=')
          string=command(i+1:)
        else
          string=command(5:)
        end if
 
70      continue
 
        call wrdnex2(string,item,last)
        string = last
 
        if (item .ne. ' ') then
 
          call show(datamax,datamin,echo,infile,iplot,item,ix,
     &      mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &      nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &      xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
 
          go to 70
 
        end if
 
c
c  SKIP=
c
      else if (leqi(command(1:4),'SKIP')) then
 
        call chrdb1(command)
 
        if (command(5:5) .eq. '=' ) then
          call chrcti(command(6:),nskip,ierror,lenc)
          if (ierror.ne.0) then
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your numeric input could not be understood.'
            go to 10
          end if
        else
          write(*,*)' '
          write(*,*)'Enter the number of data records to skip.'
          read(*,'(a)',end=100,err=100)nskip
        end if
 
        if (nskip .lt.0 ) then
          write(*,*)' '
          write(*,*)'RobPlot - Error!'
          write(*,*)'  SKIP must not be negative'
          write(*,*)'  but your value was ',nskip
          nskip=0
          go to 10
        end if
 
        item = 'NSKIP'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  TITLE =
c
      else if (leqi(command(1:5),'TITLE')) then
 
        if (command(6:6) .eq. '=' ) then
          title = command(7:)
        else
          write(*,*)' '
          write(*,*)'Enter the plot title.'
          read(*,'(a)',end=100,err=100) title
        end if
 
        item = 'TITLE'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  X=
c
      else if (leqi(command,'X') .or.
     &         leqi(command(1:2),'X=')) then
 
        if (command(2:2) .eq. '=' ) then
          string = command(3:)
        else
          write(*,*)' '
          write(*,*)'Enter the NAME or INDEX of the X variable'
          read(*,'(a)',end=100,err=100) string
        end if
 
        ix = -1
 
        do i=0,nvar
          if ( leqi(string,names(i)) ) then
            ix=i
            go to 80
          end if
        end do
 
        if (ix.eq.-1) then
          call chrcti(string,ix,ierror,lenc)
          if (ierror.ne.0)then
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your X specification did not make sense.'
            go to 10
          end if
        end if
 
80      continue
 
        if (ix .lt.0 .or. ix .gt. nvar) then
          write(*,*)' '
          write(*,*)'RobPlot - Error!'
          write(*,*)'  The index of X must be between 0 and ',nvar
          write(*,*)'  but your value was ',ix
          go to 10
        end if
 
        xmin=datamin(ix)
        xmax=datamax(ix)
 
        if (ix.ne.0) then
          call niceax(xmin,xmax,nxgrid,xminr,xmaxr,xdel,nxgrid2)
        else
          xminr=xmin
          xmaxr=xmax
        end if
 
        do i=0,nvar
          ymaxx(i)=datamax(i)
          yminx(i)=datamin(i)
        end do
 
        item = 'XMAX'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  XMAX =
c
      else if (leqi(command(1:4),'XMAX') ) then
 
        call chrdb1(command)
 
        if (command(5:5) .eq. '=' ) then
          call chrctr(command(6:),xmax,ierror,lenc)
          if (ierror.ne.0) then
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your numeric input could not be understood.'
            go to 10
          end if
        else
          write(*,*)' '
          write(*,*)'Enter the maximum for the X variable'
          read(*,'(a)',end=100,err=100) xmax
        end if
 
        if (ix.ne.0)then
          call niceax(xmin,xmax,nxgrid,xminr,xmaxr,xdel,nxgrid2)
        else
          xminr=xmin
          xmaxr=xmax
        end if
 
        if (0.le.ix.and.ix.le.nvar) then
 
          do i=1,nvar
            call rrange2( nval, data(1,ix), xmax, xmin, data(1,i),
     &        ymaxx(i), yminx(i), none)
          end do
 
        end if
 
        item = 'XMAX'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  XMAXR =
c
      else if (leqi(command(1:5),'XMAXR') ) then
 
        call chrdb1(command)
 
        if (command(6:6) .eq. '=' ) then
          call chrctr(command(7:),xmaxr,ierror,lenc)
          if (ierror.ne.0) then
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your numeric input could not be understood.'
            go to 10
          end if
        else
          write(*,*)' '
          write(*,*)'Enter the rounded plot maximum for the X variable'
          read(*,'(a)',end=100,err=100) xmaxr
        end if
 
        item = 'XMAXR'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  XMIN =
c
      else if (leqi(command(1:4),'XMIN') ) then
 
        call chrdb1(command)
 
        if (command(5:5) .eq. '=' ) then
          call chrctr(command(6:),xmin,ierror,lenc)
          if (ierror.ne.0) then
            write(*,*)' '
            write(*,*)'RobPlot - Warning!'
            write(*,*)'  Your numeric input could not be understood.'
            go to 10
          end if
        else
          write(*,*)' '
          write(*,*)'Enter the minimum for the X variable'
          read(*,'(a)',end=100,err=100) xmin
        end if
 
        if (ix.ne.0)then
          call niceax(xmin,xmax,nxgrid,xminr,xmaxr,xdel,nxgrid2)
        else
          xminr=xmin
          xmaxr=xmax
        end if
 
        if (0.le.ix.and.ix.le.nvar) then
 
          do i=1,nvar
            call rrange2( nval, data(1,ix), xmax, xmin, data(1,i),
     &        ymaxx(i), yminx(i), none)
          end do
 
        end if
 
        item = 'XMIN'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  XMINR =
c
      else if (leqi(command(1:5),'XMINR') ) then
 
        call chrdb1(command)
 
        if (command(6:6) .eq. '=' ) then
          call chrctr(command(7:),xminr,ierror,lenc)
          if (ierror.ne.0) then
            write(*,*)' '
            write(*,*)'RobPlot - Warning!'
            write(*,*)'  Your numeric input could not be understood.'
            go to 10
          end if
        else
          write(*,*)' '
          write(*,*)'Enter the rounded plot minimum for the X variable'
          read(*,'(a)',end=100,err=100) xminr
        end if
 
        item = 'XMINR'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  Y
c  Y = name
c  Y = index_range
c
      else if (leqi(command,'Y') .or.
     &         leqi(command(1:2),'Y=')) then
 
        if (command(2:2) .eq. '=' ) then
          string = command(3:)
        else
          write(*,*)' '
          write(*,*)'Enter one NAME, or an INDEX_RANGE for the '//
     &      'Y variables'
          read(*,'(a)',end=100,err=100) string
        end if
 
        ny=0
 
        do i=0,nvar
          if ( leqi(string,names(i)) ) then
            iplot(i)=1
            ny=ny+1
          else
            iplot(i)=0
          end if
        end do
 
        if (ny.eq.0)then
 
          if (string.eq.'*')then
 
            ny=1
            do i=0,nvar
              if (i.ne.ix.and.i.ne.0) then
                iplot(i)=1
              else
                iplot(i)=0
              end if
            end do
 
          else
 
            call ranger(string,maxvar+1,nrang,irang)
 
            if (nrang.eq.0)then
              write(*,*)' '
              write(*,*)'RobPLot - Error!'
              write(*,*)'  Your Y specification did not make sense.'
              go to 10
            end if
 
            do i=0,nvar
              iplot(i)=0
            end do
 
            ny=0
            do i=1,nrang
              j=irang(i)
              if (0.le.j.and.j.le.nvar)then
                iplot(j)=1
                ny=ny+1
              end if
            end do
 
          end if
 
        end if
 
        item = 'Y'
 
        call show(datamax,datamin,echo,infile,iplot,item,ix,
     &    mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,
     &    nval,nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,
     &    xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c  YMAX
c  YMAX = value  [prompt for index or name]
c  YMAX(index)=value
c
      else if (leqi(command(1:4),'YMAX') ) then
 
        call chrdb1(command)
c
c  YMAX = value
c
        if (command(5:5) .eq. '=' ) then
 
          call chrctr(command(6:),temp,ierror,lenc)
          if (ierror.ne.0) then
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your numeric input could not be understood.'
            go to 10
          end if
 
          write(*,*)' '
          write(*,*)'To what variable does this limit apply?'
          write(*,*)'Enter a variable NAME or else an INDEX:'
 
          read(*,'(a)',end=100,err=100) string
 
          ival=-1
 
          do i=0,nvar
            if (leqi(string,names(i))) then
              ival=i
            end if
          end do
 
          if (ival.eq.-1) then
            call chrcti(string,ival,ierror,lenc)
            if (ierror.ne.0) then
              write(*,*)' '
              write(*,*)'RobPlot - Error!'
              write(*,*)'  Your input could not be understood.'
              go to 10
            end if
          end if
 
        else
c
c  YMAX(...
c
          if(command(5:5).eq.'(')then
 
            call chrup3(command(6:),first,')',last)
            command = last
 
            call chrcti(first,ival,ierror,lenc)
            if (ierror.ne.0) then
              write(*,*)' '
              write(*,*)'RobPlot - Error!'
              write(*,*)'  Your input could not be understood.'
              go to 10
            end if
 
            if(command(1:1).eq.'=')then
              call chrctr(command(2:),temp,ierror,lenc)
              if (ierror.ne.0) then
                write(*,*)' '
                write(*,*)'RobPlot - Error!'
                write(*,*)'  Your input could not be understood.'
                go to 10
              end if
            end if
c
c  Just YMAX
c
          else
 
 
            write(*,*)' '
            write(*,*)'To what variable does this limit apply?'
            write(*,*)'Enter a variable NAME or else an INDEX:'
 
            read(*,'(a)',end=100,err=100) string
 
            ival=-1
 
            do i=0,nvar
              if (leqi(string,names(i))) then
                ival=i
              end if
            end do
 
            if (ival.eq.-1) then
              call chrcti(string,ival,ierror,lenc)
              if (ierror.ne.0) then
                write(*,*)' '
                write(*,*)'RobPlot - Error!'
                write(*,*)'  Your input could not be understood.'
                go to 10
              end if
            end if
 
            write(*,*)' '
            write(*,*)'What is the maximum plot value to use?'
 
            read(*,'(a)',end=100,err=100) string
            call chrctr(string,temp,ierror,lenc)
            if (ierror.ne.0) then
              write(*,*)' '
              write(*,*)'RobPlot - Error!'
              write(*,*)'  Your input could not be understood.'
              go to 10
            end if
 
          end if
 
        end if
 
        if (ival.ge.0.and.ival.le.nvar)then
          ymax(ival)=temp
        else
          write(*,*)' '
          write(*,*)'RobPlot - Error!'
          write(*,*)'  The variable index ',ival,' is out of range!'
          go to 10
        end if
 
        write(*,*)' '
        write(*,*)'Maximum for ',names(ival),' index ',ival,
     &    ' was set to ',ymax(ival)
 
        call niceax(ymin(ival),ymax(ival),nygrid,yminr(ival),
     &    ymaxr(ival),ydel,nygrid2)
c
c  YMIN
c  YMIN = value  [prompt for index or name]
c  YMIN(index)=value
c
      else if (leqi(command(1:4),'YMIN') ) then
 
        call chrdb1(command)
c
c  YMIN = value
c
        if (command(5:5) .eq. '=' ) then
 
          call chrctr(command(6:),temp,ierror,lenc)
          if (ierror.ne.0) then
            write(*,*)' '
            write(*,*)'RobPlot - Error!'
            write(*,*)'  Your numeric input could not be understood.'
            go to 10
          end if
 
          write(*,*)' '
          write(*,*)'To what variable does this limit apply?'
          write(*,*)'Enter a variable NAME or else an INDEX:'
 
          read(*,'(a)',end=100,err=100) string
 
          ival=-1
 
          do i=0,nvar
            if (leqi(string,names(i))) then
              ival=i
            end if
          end do
 
          if (ival.eq.-1) then
            call chrcti(string,ival,ierror,lenc)
            if (ierror.ne.0) then
              write(*,*)' '
              write(*,*)'RobPlot - Error!'
              write(*,*)'  Your input could not be understood.'
              go to 10
            end if
          end if
 
        else
c
c  YMIN(...
c
          if(command(5:5).eq.'(')then
 
            call chrup3(command(6:),first,')',last)
            command = last
 
            call chrcti(first,ival,ierror,lenc)
            if (ierror.ne.0) then
              write(*,*)' '
              write(*,*)'RobPlot - Error!'
              write(*,*)'  Your input could not be understood.'
              go to 10
            end if
 
            if(command(1:1).eq.'=')then
              call chrctr(command(2:),temp,ierror,lenc)
              if (ierror.ne.0) then
                write(*,*)' '
                write(*,*)'RobPlot - Error!'
                write(*,*)'  Your input could not be understood.'
                go to 10
              end if
            end if
c
c  Just YMIN
c
          else
 
 
            write(*,*)' '
            write(*,*)'To what variable does this limit apply?'
            write(*,*)'Enter a variable NAME or else an INDEX:'
 
            read(*,'(a)',end=100,err=100) string
 
            ival=-1
 
            do i=0,nvar
              if (leqi(string,names(i))) then
                ival=i
              end if
            end do
 
            if (ival.eq.-1) then
              call chrcti(string,ival,ierror,lenc)
              if (ierror.ne.0) then
                write(*,*)' '
                write(*,*)'RobPlot - Error!'
                write(*,*)'  Your input could not be understood.'
                go to 10
              end if
            end if
 
            write(*,*)' '
            write(*,*)'What is the minimum plot value to use?'
 
            read(*,'(a)',end=100,err=100) string
            call chrctr(string,temp,ierror,lenc)
            if (ierror.ne.0) then
              write(*,*)' '
              write(*,*)'RobPlot - Error!'
              write(*,*)'  Your input could not be understood.'
              go to 10
            end if
 
          end if
 
        end if
 
        if (ival.ge.0.and.ival.le.nvar)then
          ymin(ival)=temp
        else
          write(*,*)' '
          write(*,*)'RobPlot - Error!'
          write(*,*)'  The variable index ',ival,' is out of range!'
          go to 10
        end if
 
        write(*,*)' '
        write(*,*)'Minimum for ',names(ival),' index ',ival,
     &    ' was set to ',ymin(ival)
 
        call niceax(ymin(ival),ymax(ival),nygrid,yminr(ival),
     &    ymaxr(ival),ydel,nygrid2)
c
c  Unrecognized command.
c
      else
        write(*,*)' '
        write(*,*)'RobPlot - Warning:'
        write(*,*)'  I could not follow the command:'
        write(*,'(1x,a)')command
      end if
 
      go to 10
c
c  An error occurred.  Maybe we should quit.
c
100   continue
      iquit=1
      call quit(iquit,nplot)
c
c  An error occurred while opening the input file.
c
110   continue
      write(*,*)' '
      write(*,*)'RobPlot - Error!'
      write(*,*)'  An error occurred while opening the'
      write(*,*)'  input data file.'
      go to 10
c
c  An error occurred while trying to read the data names record.
c
120   continue
      write(*,*)' '
      write(*,*)'RobPlot - Error!'
      write(*,*)'  An error occurred while trying to read the '
      write(*,*)'  data names record of the input data file.'
      close(unit=1)
      infile=' '
      go to 10
      end
      subroutine capchr(string)
c
c***********************************************************************
c
c  CAPCHR accepts a STRING of characters and replaces any lowercase
c  letters by uppercase ones.
c
c
c  Input/output, CHARACTER*(*) STRING, is the string of characters to
c  be transformed.
c
      integer i
      integer itemp
      integer nchar
      character*(*) string
c
      intrinsic char
      intrinsic ichar
      intrinsic len
c
      nchar = len(string)
 
      do i=1, nchar
        itemp=ichar(string(i:i))
        if ( 97 .le. itemp .and. itemp .le. 122 ) then
          string(i:i) = char(itemp-32)
        end if
      end do
 
      return
      end
      subroutine chrcti(string,intval,ierror,lchar)
c
c***********************************************************************
c
c  CHRCTI accepts a STRING of characters and reads an integer
c  from STRING into INTVAL.  The STRING must begin with an integer
c  but that may be followed by other information.
c  CHRCTI will read as many characters as possible until it reaches
c  the end of the STRING, or encounters a character which cannot be
c  part of the number.
c
c  Legal input is
c
c    blanks,
c    initial sign,
c    blanks,
c    integer part,
c    blanks,
c    final comma or semicolon,
c
c  with most quantities optional.
c
c
c  Input, CHARACTER*(*) STRING, the string containing the
c  data to be read.  Reading will begin at position 1 and
c  terminate at the end of the string, or when no more
c  characters can be read to form a legal integer.  Blanks,
c  commas, or other nonnumeric data will, in particular,
c  cause the conversion to halt.
c
c  Sample results:
c
c  STRING            INTVAL
c
c  '1'               1
c  '     1   '       1
c  '1A'              1
c  '12,34,56'        12
c  '  34 7'          34
c  '-1E2ABCD'        -100
c  '-1X2ABCD'        -1
c  ' 2E-1'           0
c  '23.45'           23
c
c
c  Output, INTEGER INTVAL, the integer read from the string.
c
c  Output, INTEGER IERROR, error flag.
c  0 if no errors,
c  Value of IHAVE when error occurred otherwise.
c
c  Output, INTEGER LCHAR, number of characters read from
c  STRING to form the number.
c
      character*1 chrtmp
      integer ierror
      integer ihave
      integer intval
      integer isgn
      integer iterm
      integer itop
      integer lchar
      integer nchar
      integer ndig
      character*1 null
      character*(*) string
c
      intrinsic char
      intrinsic len
      intrinsic lge
      intrinsic lle
c
      nchar=len(string)
 
      ierror=0
      intval=0
      lchar=-1
      isgn=1
      itop=0
      ihave=1
      iterm=0
      null=char(0)
 
10    continue
 
      lchar=lchar+1
      chrtmp=string(lchar+1:lchar+1)
c
c  Blank.
c
      if (chrtmp .eq. ' ' .or.
     &   chrtmp .eq. null) then
 
        if (ihave .eq. 2) then
 
        else if (ihave .eq. 3) then
          ihave=11
        end if
c
c  Comma.
c
      else if (chrtmp .eq. ',' .or.
     &  chrtmp .eq. ';') then
 
        if (ihave .ne. 1) then
          iterm=1
          ihave=12
          lchar=lchar+1
        end if
c
c  Minus sign.
c
      else if (chrtmp .eq. '-') then
 
        if (ihave .eq. 1) then
          ihave=2
          isgn=-1
        else
          iterm=1
        end if
c
c  Plus sign.
c
      else if (chrtmp .eq. '+') then
 
        if (ihave .eq. 1) then
          ihave=2
        else
          iterm=1
        end if
c
c  Digit.
c
      else if (ihave .lt. 11 .and.
     &       lge(chrtmp,'0') .and.
     &       lle(chrtmp,'9')) then
 
        ihave=3
        read(chrtmp,'(i1)')ndig
        itop=10*itop+ndig
c
c  Anything else is regarded as a terminator.
c
      else
        iterm=1
      end if
 
      if (iterm .ne. 1 .and. lchar+1 .lt. nchar)go to 10
      if (iterm .ne. 1 .and. lchar+1 .eq. nchar)lchar=nchar
c
c  Number seems to have terminated.  Have we got a legal number?
c
      if (ihave .eq. 1 .or. ihave .eq. 2) then
        ierror=ihave
        write(*,*)' '
        write(*,*)'CHRCTI - Serious error!'
        write(*,*)'  IERROR=',ierror
        write(*,*)'  Illegal or nonnumeric input:'
        write(*,'(1x,a)')string
        return
      end if
c
c  Number seems OK.  Form it.
c
      intval=isgn*itop
      return
      end
      subroutine chrctr(string,rval,ierror,lchar)
c
c***********************************************************************
c
c  CHRCTR accepts a string of characters, and tries to extract a
c  real number from the initial part of the string.
c
c  CHRCTR will read as many characters as possible until it reaches
c  the end of the string, or encounters a character which cannot be
c  part of the real number.
c
c  Legal input is:
c
c     1 blanks,
c     2 '+' or '-' sign,
c     2.5 spaces
c     3 integer part,
c     4 decimal point,
c     5 fraction part,
c     6 'E' or 'e' or 'D' or 'd', exponent marker,
c     7 exponent sign,
c     8 exponent integer part,
c     9 exponent decimal point,
c    10 exponent fraction part,
c    11 blanks,
c    12 final comma or semicolon.
c
c  with most quantities optional.
c
c  Examples:
c
c    STRING            RVAL
c
c    '1'               1.0
c    '     1   '       1.0
c    '1A'              1.0
c    '12,34,56'        12.0
c    '  34 7'          34.0
c    '-1E2ABCD'        -100.0
c    '-1X2ABCD'        -1.0
c    ' 2E-1'           0.2
c    '23.45'           23.45
c    '-4.2E+2'         -420.0
c    '17d2'            1700.0
c    '-14e-2'         -0.14
c    'e2'              100.0
c    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
c
c
c  Input, CHARACTER*(*) STRING, the string containing the
c  data to be read.  Reading will begin at position 1 and
c  terminate at the end of the string, or when no more
c  characters can be read to form a legal real.  Blanks,
c  commas, or other nonnumeric data will, in particular,
c  cause the conversion to halt.
c
c  Output, REAL RVAL, the real value that was read from the string.
c
c  Output, INTEGER IERROR, error flag.
c
c  0, no errors occurred.
c
c  1, 2, 6 or 7, the input number was garbled.  The
c  value of IERROR is the last type of input successfully
c  read.  For instance, 1 means initial blanks, 2 means
c  a plus or minus sign, and so on.
c
c  Output, INTEGER LCHAR, the number of characters read from
c  STRING to form the number, including any terminating
c  characters such as a trailing comma or blanks.
c
      character*1 chrtmp
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer lchar
      logical leqi
      integer nchar
      integer ndig
      character*1 null
      real rbot
      real rexp
      real rtop
      real rval
      character*(*) string
c
      intrinsic char
      intrinsic len
      external leqi
      intrinsic lge
      intrinsic lle
      intrinsic real
c
      nchar=len(string)
      ierror=0
      rval=0.0
      lchar=-1
      isgn=1
      rtop=0
      rbot=1
      jsgn=1
      jtop=0
      jbot=1
      ihave=1
      iterm=0
      null=char(0)
10    continue
      lchar=lchar+1
      chrtmp=string(lchar+1:lchar+1)
c
c  Blank character.
c
      if (chrtmp .eq. ' ' .or.
     &   chrtmp .eq. null) then
c
c  20 November 1993
c
c  I would like to allow input like "+ 2", where there is a space
c  between the plus and the number.  So I am going to comment out
c  this line, because I think that's all that's keeping me from
c  doing this.
c
c       if (ihave .eq. 2 .or.
c    &     ihave .eq. 6 .or.
c    &     ihave .eq. 7) then
        if (ihave .eq. 2) then
        else if ((ihave .eq. 6) .or.
     &     (ihave .eq. 7)) then
          iterm=1
        else if (ihave .gt. 1) then
          ihave=11
        end if
c
c  Comma.
c
      else if (chrtmp .eq. ',' .or.
     &  chrtmp .eq. ';') then
        if (ihave .ne. 1) then
          iterm=1
          ihave=12
          lchar=lchar+1
        end if
c
c  Minus sign.
c
      else if (chrtmp .eq. '-') then
        if (ihave .eq. 1) then
          ihave=2
          isgn=-1
        else if (ihave .eq. 6) then
          ihave=7
          jsgn=-1
        else
          iterm=1
        end if
c
c  Plus sign.
c
      else if (chrtmp .eq. '+') then
        if (ihave .eq. 1) then
          ihave=2
        else if (ihave .eq. 6) then
          ihave=7
        else
          iterm=1
        end if
c
c  Decimal point.
c
      else if (chrtmp .eq. '.') then
        if (ihave .lt. 4) then
          ihave=4
        else if (ihave .ge. 6 .and. ihave .le. 8) then
          ihave=9
        else
          iterm=1
        end if
c
c  Exponent marker.
c
      else if (leqi(chrtmp,'e') .or.
     &       leqi(chrtmp,'d') ) then
        if (ihave .lt. 6) then
          ihave=6
        else
          iterm=1
        end if
c
c  Digit.
c
      else if (ihave .lt. 11 .and.
     &       lge(chrtmp,'0') .and.
     &       lle(chrtmp,'9') ) then
        if (ihave .le. 2) then
          ihave=3
        else if (ihave .eq. 4) then
          ihave=5
        else if (ihave .eq. 6 .or. ihave .eq. 7) then
          ihave=8
        else if (ihave .eq. 9) then
          ihave=10
        end if
        read(chrtmp,'(i1)')ndig
        if (ihave .eq. 3) then
          rtop=10*rtop+ndig
        else if (ihave .eq. 5) then
          rtop=10*rtop+ndig
          rbot=10*rbot
        else if (ihave .eq. 8) then
          jtop=10*jtop+ndig
        else if (ihave .eq. 10) then
          jtop=10*jtop+ndig
          jbot=10*jbot
        end if
c
c  Anything else is regarded as a terminator.
c
      else
        iterm=1
      end if
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
      if (iterm .ne. 1 .and. lchar+1 .lt. nchar)go to 10
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LCHAR is equal to NCHAR.
c
      if (iterm .ne. 1 .and. lchar+1 .eq. nchar)lchar=nchar
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7!
c
      if (
     &  ihave .eq. 1 .or.
     &  ihave .eq. 2 .or.
     &  ihave .eq. 6 .or.
     &  ihave .eq. 7) then
        ierror=ihave
        write(*,*)' '
        write(*,*)'CHRCTR - Serious error!'
        write(*,*)'  Illegal or nonnumeric input:'
        write(*,'(1x,a)')string
        return
      end if
c
c  Number seems OK.  Form it.
c
      if (jtop .eq. 0) then
 
        rexp=1.0
 
      else
 
        if (jbot .eq. 1) then
          rexp=10.0**(jsgn*jtop)
        else
          rexp=jsgn*jtop
          rexp=rexp/jbot
          rexp=10.0**rexp
        end if
 
      end if
 
      rval=isgn*rexp*rtop/rbot
 
      return
      end
      subroutine chrdb1(string)
c
c***********************************************************************
c
c  CHRDB1 accepts a string of characters and removes all
c  blanks, left justifying the remainder and padding with
c  blanks.
c
c  Compare CHRDB0 which pads with nulls.
c
c
c  Input/output, CHARACTER*(*) STRING, the string to be transformed.
c
      character*1 chrtmp
      integer i
      integer j
      integer nchar
      character*(*) string
c
      intrinsic char
      intrinsic len
c
      nchar=len(string)
      j=0
 
      do i=1,nchar
 
        chrtmp=string(i:i)
        string(i:i)=' '
 
        if (chrtmp .ne. ' ') then
          j=j+1
          string(j:j)=chrtmp
        end if
 
      end do
 
      return
      end
      subroutine chrdb2(string)
c
c***********************************************************************
c
c  CHRDB2 accepts a string of characters, replaces consecutive blanks
c  by a single blank, left justifies the remainder and right pads with
c  blanks.
c
c  Compare CHRBD0 and CHRDB1.
c
c
c  Input/output, CHARACTER*(*) STRING, the string to be transformed.
c
      integer i
      integer j
      integer nchar
      character*1 newchr
      character*1 oldchr
      character*(*) string
c
      intrinsic char
      intrinsic len
c
      nchar=len(string)
 
      j=0
      newchr=' '
 
      do i=1,nchar
 
        oldchr=newchr
        newchr=string(i:i)
        string(i:i)=' '
 
        if (oldchr .ne. ' ' .or. newchr .ne. ' ') then
          j=j+1
          string(j:j)=newchr
        end if
 
      end do
 
      return
      end
      subroutine chrup3(string,first,middle,last)
c
c***********************************************************************
c
c  CHRUP3 divides a string into three parts:
c  *  The FIRST part, up to the first occurrence of MIDDLE,
c  *  the MIDDLE part,
c  *  the LAST part.
c
c  For instance, if, on input,
c
c    STRING = 'aBCdEfgh'
c    MIDDLE = 'eF'
c
c  then the output will be:
c
c    FIRST = 'aBCd'
c    LAST =  'gh'
c
c
c  Input, CHARACTER*(*) STRING, the string to be analyzed.
c
c  Output, CHARACTER*(*) FIRST, the entries in STRING, up
c  to, but not including, the first occurrence, if any,
c  of MIDDLE.  If MIDDLE occurs immediately, then FIRST=' '.
c  If FIRST is not long enough, trailing entries will be lost.
c  Unless FIRST is all blank, FIRST will have no leading blanks.
c
c  Input, CHARACTER*(*) MIDDLE, the string to be searched
c  for in STRING.  Trailing blanks in MIDDLE are ignored,
c  unless MIDDLE is entirely blank, in which case it will
c  be treated as a single blank.  The check for the occurrence
c  of MIDDLE in STRING ignores case.
c
c  Output, CHARACTER*(*) LAST, any characters of STRING which
c  occur after the first occurrence of MIDDLE.  If MIDDLE
c  occurs at the end of STRING, then LAST=' '.  If LAST is not
c  long enough, then trailing entries will be lost.
c  Unless LAST is all blank, LAST will have no leading blanks.
c
      character*(*) first
      integer i
      integer indexi
      character*(*) last
      integer lenm
      integer lens
      character*(*) middle
      character*(*) string
c
      intrinsic len
c
      if (middle .eq. ' ') then
        call chrdb2(string)
      end if
c
      lens=len(string)
      lenm=len(middle)
      i = indexi(string,middle)
 
      if ( i .eq. 0 ) then
        first=string
        last=' '
      else if ( i .eq. 1) then
        first=' '
        last=string(lenm+1:)
      else if (i+lenm .gt. lens) then
        first = string
        last = ' '
      else
        first = string(1:i-1)
        last = string(i+lenm:)
      end if
c
c  Force the FIRST and LAST strings to be flush left,
c  that is, to have no leading blanks.
c
      call flushl(first)
      call flushl(last)
 
      return
      end
      subroutine flushl(string)
c
c***********************************************************************
c
c  FLUSHL flushes a string left.
c
c  For instance:
c
c    Input             Output
c
c    '     Hello'      'Hello     '
c    ' Hi there!  '    'Hi there!   '
c
c
c  Input/output, CHARACTER*(*) STRING.
c
c  On input, STRING is a string of characters.
c
c  On output, any initial blank characters in STRING
c  have been cut, and pasted back onto the end.
c
      integer i
      integer lchar
      integer lenchr
      integer nonb
      character*1 null
      character*(*) string
c
      intrinsic char
      external lenchr
c
c  Check the length of the string to the last nonblank.
c  If nonpositive, return.
c
      lchar=lenchr(string)
      if (lchar .le. 0)return
      null=char(0)
c
c  Find the occurrence of the first nonblank.
c
      do i=1,lchar
        nonb=i
        if (string(i:i) .ne. ' ' .and.
     &     string(i:i) .ne. null)go to 10
      end do
 
      return
 
10    continue
c
c  Shift the string left.
c
      do i=1,lchar+1-nonb
        string(i:i)=string(i+nonb-1:i+nonb-1)
      end do
c
c  Blank out the end of the string.
c
      do i=lchar+2-nonb,lchar
        string(i:i)=' '
      end do
 
      return
      end
      subroutine graf(nplot,nval,nxgrid,nygrid,over,psfile,title,xmaxr,
     &  xminr,xname,xval,ymaxri,yminri,yname,yval)
c
c***********************************************************************
c
      integer nclip
      parameter (nclip=4)
c
      integer MAXVAL
      parameter (MAXVAL=1000)
c
      integer nval
c
      real angle
      character*9 chdate
      character*8 chtime
      integer i
      integer idash
      integer mjus
      integer ndec
      integer njus
      integer nplot
      integer nxgrid
      integer nygrid
      logical over
      logical portrait
      character*80 psfile
      real px
      real pxclip(nclip)
      real pxmax
      real pxmin
      real pxvec(MAXVAL)
      real py
      real pyclip(nclip)
      real pymax
      real pymin
      real pyvec(MAXVAL)
      real size
      character*80 string
      real thick
      character*80 title
      real x
      real xmaxr
      real xminr
      character*20 xname
      real xval(nval)
      real y
      real ymaxri
      real yminri
      character*20 yname
      real yval(nval)
c
c  Special operations before the very first plot.
c
      if (nplot .eq. 1) then
 
        call newdev(psfile)
 
        portrait=.true.
        call psinit(portrait)
 
        thick=0.005
        call setthk(thick)
 
        idash=10
        call setdash(idash)
c
c  ...or before the 5th, 9th, 13th plot.
c
      else if (mod(nplot,4) .eq. 1) then
        call chopit
      end if
c
c  On every (4n+1)-th plot, print date, time, and title on the new page.
c
      if (mod(nplot,4) .eq. 1) then
 
        px=7.5
        py=10.25
        size=0.14
        call date(chdate)
        call time(chtime)
        string=chtime//' '//chdate
        angle = 0.0
        mjus=2
 
        call keksymc(px,py,size,string,angle,mjus)
 
        if (title.ne.' ') then
          size=0.14
          px=0.50
          py=9.75
          string=title
          angle=0.0
          mjus=0
          call keksymc(px,py,size,string,angle,mjus)
        end if
 
      end if
c
c  Draw a box around the plotting area.
c
      pxmin=0.5
      pxmax=7.5
 
      if (mod(nplot,4) .eq. 1) then
        pymin=0.75
        pymax=2.75
      else if (mod(nplot,4) .eq. 2) then
        pymin=3.0
        pymax=5.0
      else if (mod(nplot,4) .eq. 3) then
        pymin=5.25
        pymax=7.25
      else
        pymin=7.5
        pymax=9.5
      end if
 
      call sldlin(pxmin,pymin,pxmin,pymax)
      call sldlin(pxmin,pymax,pxmax,pymax)
      call sldlin(pxmax,pymax,pxmax,pymin)
      call sldlin(pxmax,pymin,pxmin,pymin)
c
c  Vertical grid lines:
c    XMIN2+DX, ... XMIN2+6 DX
c
      do i=2,nxgrid-1
        px=( (nxgrid+1-i)*pxmin + (i-1)*pxmax ) / real(nxgrid)
        call dshlin(px,pymin,px,pymax)
      end do
c
c  Vertical grid line labels:
c    XMINR, XMINR+DX, ..., XMINR+6*DX, XMAXR
c
      size=0.07
      py = pymin-1.5*size
      angle = 0.0
      ndec=-1
      njus=1
 
      do i=1,nxgrid+1
        px= ( (nxgrid+1-i)*pxmin + (i-1)*pxmax ) / real(nxgrid)
        x = ( (nxgrid+1-i)*xminr + (i-1)*xmaxr) / real(nxgrid)
        call kekflt(px,py,size,x,angle,ndec,njus)
      end do
c
c  Horizontal grid lines:
c    YMIN+DY, YMIN+2 DY, YMIN+3 DY
c
      do i=2,nygrid
        py=( (nygrid+1-i)*pymin + (i-1)*pymax ) / real(nygrid)
        call dshlin(pxmin,py,pxmax,py)
      end do
c
c  Horizontal grid line labels.
c    YMIN, YMIN+DY, YMIN+2 DY, YMIN+3DY, YMAX
c
      size=0.07
      px = pxmin-1.5*size
      angle = 0.0
      ndec=3
      njus=2
 
      do i=1,nygrid+1,nygrid
        py= ( (nygrid+1-i)*pymin + (i-1)*pymax ) / real(nygrid)
        y = ( (nygrid+1-i)* yminri + (i-1)*ymaxri) / real(nygrid)
        call kekexp(px,py,size,y,angle,ndec,njus)
      end do
c
c  Y axis title.
c
      size=0.14
      px = 0.0
      py = (pymin+pymax)/2.0
      string=yname
      angle=90.0
      mjus=1
 
      call keksymc(px,py,size,string,angle,mjus)
c
c  If either line Y=0 or X=0 occurs within the plotting range, draw it.
c
      if ( yminri .lt. 0.0 .and. 0.0 .lt. ymaxri ) then
        py = (ymaxri*pymin-yminri*pymax) / (ymaxri-yminri)
        call sldlin(pxmin,py,pxmax,py)
      end if
 
      if ( xminr .lt. 0.0 .and. 0.0 .lt. xmaxr ) then
        px = (xmaxr*pxmin-xminr*pxmax) / (xmaxr-xminr)
        call sldlin(px,pymin,px,pymax)
      end if
c
c  Rescale the X, Y data.
c
      if (nval.gt.MAXVAL) then
        write(*,*)' '
        write(*,*)'GRAF - Warning!'
        write(*,*)'  There is more input data than GRAF can handle.'
        write(*,*)'  Input NVAL = ',nval
        write(*,*)'  Internal MAXVAL = ',MAXVAL
        write(*,*)'  Excess data will not be plotted.'
      end if
 
      do i=1,nval
        if (i.le.MAXVAL) then
          if (xmaxr-xminr.ne.0.0) then
            pxvec(i)=((xmaxr-xval(i))*pxmin+(xval(i)-xminr)*pxmax)
     &        / (xmaxr-xminr)
          else
            pxvec(i)=(pxmin+pxmax)/2.0
          end if
        end if
      end do
 
      do i=1,nval
        if (i.le.MAXVAL) then
          if (ymaxri-yminri.ne.0.0) then
            pyvec(i)=((ymaxri-yval(i))*pymin+(yval(i)-yminri)*pymax)
     &        / (ymaxri-yminri)
          else
            pyvec(i)=(pymin+pymax)/2.0
          end if
        end if
      end do
c
c  Thicken up the line for the data curve.
c
      thick=0.020
      call setthk(thick)
c
c  Save the plotting environment.
c
      call gsave
c
c  Set a clipping box that fits inside the desired plot box.
c
      pxclip(1)=pxmin
      pyclip(1)=pymin
      pxclip(2)=pxmax
      pyclip(2)=pymin
      pxclip(3)=pxmax
      pyclip(3)=pymax
      pxclip(4)=pxmin
      pyclip(4)=pymax
 
      call clipbox(pxclip,pyclip,nclip)
c
c  Draw the curve.
c
      call sldcrv(pxvec,pyvec,nval)
c
c  Restore the plotting environment.
c
      call grestore
c
c  X variable on bottom of page.
c
      thick=0.0075
      call setthk(thick)
 
      if (mod(nplot,4) .eq. 1) then
        size=0.14
        px=4.0
        py=0.25
        string=xname
        angle=0.0
        mjus=1
 
        call keksymc(px,py,size,string,angle,mjus)
 
      end if
 
      return
      end
      subroutine hello
c
c***********************************************************************
c
      write(*,*)' '
      write(*,*)'HELLO!'
      write(*,*)' '
      write(*,*)'  RobPlot'
      write(*,*)' '
      write(*,*)'  Revised on 21 May 1997'
      write(*,*)' '
      write(*,*)'  An interactive program which:'
      write(*,*)'  * Reads a data file;'
      write(*,*)'  * Accepts plot parameters;'
      write(*,*)'  * Creates a PostScript plot.'
      write(*,*)' '
      write(*,*)'  Latest improvements:'
      write(*,*)' '
      write(*,*)'  * XMINR= and XMAXR= added.'
      write(*,*)'  * X=, Y= handle name or index.'
      write(*,*)'  * Axis limits are rounded off.'
      write(*,*)'  * IPLOT= ...accepts a range of data.'
      write(*,*)'  * Any data Y(I) can be plotted versus I.'
      write(*,*)'  * ECHO command allows input files to be echoed.'
      write(*,*)'  * Data is clipped to fit within plot box.'
      write(*,*)'  * Input comments allowed, beginning with "#".'
      write(*,*)'  * Data curve thickened.'
 
      return
      end
      subroutine help(ihelp)
c
c***********************************************************************
c
      integer ihelp
c
      if (ihelp .eq. 1) then
        write(*,*)' '
        write(*,*)'For a little help, type "H";'
        write(*,*)'For lots of help, type "HELP".'
      else if (ihelp .eq. 2) then
        write(*,*)' '
        write(*,*)'Control commands: '
        write(*,*)'  H, Help, Init, Plot, Quit, Read, Show'
        write(*,*)'Data commands:'
        write(*,*)'  ECHO='
        write(*,*)'  INFILE= '
        write(*,*)'  MARK= RECL= SKIP='
        write(*,*)'  NTHIN='
        write(*,*)'  OVER='
        write(*,*)'  PSFILE= '
        write(*,*)'  X= XMAX= XMAXR= XMIN= XMINR='
        write(*,*)'  Y= YMAX(...)= YMIN(...)='
      else if (ihelp .eq. 3) then
        write(*,*)' '
        write(*,*)'Available commands are:'
        write(*,*)' '
        write(*,*)'ECHO=       Set echo of input data, (T/F).'
        write(*,*)'H           Print a brief list of commands.'
        write(*,*)'Help        Print command explanations.'
        write(*,*)'INFILE=     Specify the input data file.'
        write(*,*)'Init        Wipe out all internal data.'
        write(*,*)'IPLOT=      Specify items to plot by index.'
        write(*,*)'MARK=       Specify data separator;'
        write(*,*)'NTHIN=      Set plot point thinning factor.'
        write(*,*)'OVER=       T/F, make over plots.'
        write(*,*)'Plot        Make a plot of X versus Y.'
        write(*,*)'PSFILE=     Specify the PostScript output file.'
        write(*,*)'Quit        Quit the program.'
        write(*,*)'Read        Read the input data file.'
        write(*,*)'RECL=       Specify the input data file record '//
     &    'length.'
        write(*,*)'Show name   Print the value of a variable.'
        write(*,*)'Show *      Print all variables.'
        write(*,*)'SKIP=       Skip SKIP data records before a READ.'
        write(*,*)'TITLE=      Set the plot title.'
        write(*,*)'X=          Set the X plot variable by name.'
        write(*,*)'XMAX=       Set the plot maximum for X.'
        write(*,*)'XMAXR=      Set rounded plot maximum for X.'
        write(*,*)'XMIN=       Set the plot minimum for X.'
        write(*,*)'XMINR=      Set rounded plot minimum for X.'
        write(*,*)'Y=          Set one Y plot variable by name.'
        write(*,*)'YMAX(...)=  Set the plot maximum for Y(...).'
        write(*,*)'YMIN(...)=  Set the plot minimum for Y(...).'
      end if
 
      return
      end
      function indexi(string,sub)
c
c***********************************************************************
c
c  INDEXI is a case-insensitive INDEX function.  It returns the
c  location in STRING at which the substring SUB is first found,
c  or 0 if the substring does not occur at all.
c
c  INDEXI is also trailing blank insensitive.  This is very
c  important for those cases where you have stored information in
c  larger variables.  If STRING is of length 80, and SUB is of
c  length 80, then if STRING='FRED' and SUB='RED', a match would
c  not be reported by the standard FORTRAN INDEX, because it treats
c  both variables as being 80 characters long!  INDEXI assumes that
c  trailing blanks represent garbage!
c
c  Because of the suppression of trailing blanks, INDEXI cannot be
c  used to find, say, the first occurrence of the two-character
c  string 'A '.  However, INDEXI treats as a special case the
c  occurrence where STRING or SUB is entirely blank.  Thus you can
c  use INDEXI to search for occurrences of double or triple blanks
c  in a string, for example, although INDEX itself would be just as
c  suitable for that problem.
c
c
c  Input, CHARACTER*(*) STRING, the string to be searched.
c
c  Input, CHARACTER*(*) SUB, the substring to search for.
c
c  Output, INTEGER INDEXI.  0 if SUB does not occur in
c  STRING.  Otherwise STRING(INDEXI:INDEXI+LENS-1) = SUB,
c  where LENS is the length of SUB, and is the first place
c  this happens.  However, note that INDEXI ignores case,
c  unlike the standard FORTRAN INDEX function.
c
      integer i
      integer indexi
      integer lenchr
      logical leqi
      integer llen1
      integer llen2
      character*(*) string
      character*(*) sub
c
      intrinsic len
      external lenchr
      external leqi
c
      indexi=0
 
      llen1=lenchr(string)
      llen2=lenchr(sub)
c
c  In case STRING or SUB is blanks, use LEN.
c
      if (llen1 .eq. 0) then
        llen1=len(string)
      end if
 
      if (llen2 .eq. 0) then
        llen2=len(sub)
      end if
 
      if (llen2 .gt. llen1) then
        return
      end if
 
      do i=1,llen1+1-llen2
 
        if (leqi(string(i:i+llen2-1),sub)) then
          indexi=i
          return
        end if
 
      end do
 
      return
      end
      subroutine init(data,datamax,datamin,echo,existin,existps,infile,
     &  iplot,ix,mark,MAXVAL,MAXVAR,names,nplot,nskip,nthin,nval,
     &  nvar,nxgrid,ny,nygrid,over,psfile,recl,string,title,
     &  xmax,xmaxr,xmin,xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c***********************************************************************
c
      integer MAXVAL
      integer MAXVAR
c
      real data(MAXVAL,0:MAXVAR)
      real datamax(0:MAXVAR)
      real datamin(0:MAXVAR)
      logical echo
      logical existin
      logical existps
      integer i
      character*80 infile
      integer iplot(0:MAXVAR)
      integer ix
      integer j
      character*1 mark
      character*20 names(0:MAXVAR)
      integer nplot
      integer nskip
      integer nthin
      integer nval
      integer nvar
      integer nxgrid
      integer ny
      integer nygrid
      logical over
      character*80 psfile
      integer recl
      character*80 string
      character*80 title
      real xmax
      real xmaxr
      real xmin
      real xminr
      real ymax(0:MAXVAR)
      real ymaxr(0:MAXVAR)
      real ymaxx(0:MAXVAR)
      real ymin(0:MAXVAR)
      real yminr(0:MAXVAR)
      real yminx(0:MAXVAR)
c
      do i=1,MAXVAL
        do j=0,MAXVAR
          data(i,j)=0.0
        end do
      end do
 
      do i=0,MAXVAR
        datamax(i)=0.0
      end do
 
      do i=0,MAXVAR
        datamin(i)=0.0
      end do
 
      echo=.false.
      existin=.false.
      existps=.true.
      infile = ' '
 
      do i=0,MAXVAR
        iplot(i)=0
      end do
 
      ix = -1
      mark = ','
 
      do i=0,MAXVAR
        names(i)=' '
      end do
      names(0)='INDEX'
 
      nplot = 0
      nskip = 0
      nthin = 1
      nval = 0
      nvar = 0
      nxgrid = 7
      ny = 0
      nygrid = 4
      over = .false.
      psfile = 'robplot.ps'
      recl = 0
      string = ' '
      title = ' '
      xmax = 0.0
      xmaxr = 0.0
      xmin = 0.0
      xminr = 0.0
 
      do i=0,MAXVAR
        ymax(i) = 0.0
        ymaxr(i) = 0.0
        ymaxx(i) = 0.0
        ymin(i) = 0.0
        yminr(i) = 0.0
        yminx(i) = 0.0
      end do
 
      write(*,*)' '
      write(*,*)'INIT - Note:'
      write(*,*)'  The program data has been initialized.'
 
      return
      end
      function lenchr(string)
c
c***********************************************************************
c
c  LENCHR returns the length of STRING up to the last nonblank
c  character.
c
c
c  Input, CHARACTER*(*) STRING, the string to be measured.
c
c  Output, INTEGER LENCHR, the location of the last nonblank
c  character in STRING.
c
      integer i
      integer lenchr
      character*(*) string
c
      intrinsic char
      intrinsic len
c
      do i=len(string),1,-1
        if (string(i:i) .ne. ' ' .and.
     &     string(i:i) .ne. char(0)) then
          lenchr=i
          return
        end if
      end do
 
      lenchr=0
 
      return
      end
      function leqi(strng1,strng2)
c
c***********************************************************************
c
c  LEQI is a case insensitive comparison of two strings for
c  equality.  Thus, LEQI('Anjana','ANJANA') is .TRUE.
c
c
c  Input, CHARACTER*(*) STRNG1, STRNG2, the strings to compare.
c
c  Output, LOGICAL LEQI, the result of the comparison.
c
      integer i
      integer len1
      integer len2
      integer lenc
      logical leqi
      character*1 null
      character*1 s1
      character*1 s2
      character*(*) strng1
      character*(*) strng2
c
      intrinsic char
      intrinsic len
      intrinsic min
c
      len1=len(strng1)
      len2=len(strng2)
      lenc=min(len1,len2)
      leqi=.false.
 
      do i=1,lenc
        s1=strng1(i:i)
        s2=strng2(i:i)
        call capchr(s1)
        call capchr(s2)
        if (s1 .ne. s2)return
      end do
 
      null=char(0)
 
      do i=lenc+1,len1
        if (strng1(i:i) .ne. ' ' .and.
     &     strng1(i:i) .ne. null)return
      end do
 
      do i=lenc+1,len2
        if (strng2(i:i) .ne. ' ' .and.
     &     strng2(i:i) .ne. null)return
      end do
 
      leqi=.true.
      return
      end
      subroutine niceax ( xmin, xmax, ndivs, pxmin, pxmax, pxdiv, 
     &  nticks )
c
c***********************************************************************
c
c  NICEAX is given information about the range of a variable, and
c  the number of divisions desired, and returns suggestions for
c  labeling a plotting axis for the variable, including the
c  starting and ending points, the length of a single division,
c  and a suggested tick marking for the axis.
c
c
c  Input, real XMIN, XMAX, the lower and upper values that must be
c  included on the axis.
c
c  Input, integer NDIVS, the number of divisions desired along
c  the axis.
c
c  Output, real PXMIN, PXMAX, the recommended lower and upper axis
c  bounds.  It will be the case that PXMIN <= XMIN < XMAX <= PXMAX.
c
c  Output, real PXDIV, the recommended size of a single division.
c
c  Output, integer NTICKS, a suggested number of ticks to use,
c  if subdividing each of the NDIVS divisions of the axis.
c
      integer nsteps
      parameter ( nsteps = 5 )
c
      real best
      real good
      integer i
      integer intlog
      integer iticks(5)
      integer j
      integer ndivs
      integer nticks
      real pxmax
      real pxmax2
      real pxmin
      real pxmin2
      real pxdiv
      real pxdiv2
      real reldif
      real steps(nsteps)
      real xmax
      real xmin
c
      steps(1) =  1.0
      steps(2) =  2.0
      steps(3) =  4.0
      steps(4) =  5.0
      steps(5) = 10.0
 
      iticks(1) = 5
      iticks(2) = 4
      iticks(3) = 4
      iticks(4) = 5
      iticks(5) = 5
c
c  RELDIF = the X difference divided by the maximum X.
c
      if ( xmax .ne. xmin ) then
        reldif = (xmax - xmin) / max ( abs(xmax), abs(xmin) )
      else
        reldif = 0.0
      end if
c
c  Code for when XMIN and XMAX are relatively extremely close.
c
      if ( reldif .lt. 0.00001 ) then
 
        if ( xmax .eq. 0.0 ) then
 
          pxdiv = 1.0
 
        else
 
          intlog = int ( log10 ( abs ( xmax ) ) )
 
          if ( intlog .lt. 0 ) then
            intlog = intlog - 1
          end if
 
          pxdiv = 10.0**intlog
 
          if ( pxdiv .gt. 1.0 ) then
            pxdiv = 1.0
          end if
 
        end if
 
        nticks = 5
        pxmin = xmax - real ( ndivs/2 ) * pxdiv
        pxmax = xmax + real ( ndivs - ndivs/2 ) * pxdiv
c
c  Code for when XMIN and XMAX are relatively separated.
c
      else
 
        best = -999.0
c
c  On second loop, increase INTLOG by 1.
c
        do j = 1, 2
 
          intlog = int ( log10 ( (xmax-xmin) / real(ndivs) ) ) + (j-1)
 
          if ( xmax-xmin  .lt. real ( ndivs ) ) then
            intlog = intlog - 1
          end if
 
          do i = 1, nsteps
 
            pxdiv2 = steps(i) * 10.0**intlog
c
c  Don't even consider the steps unless they take us from
c  XMIN to XMAX!
c
            if ( ndivs*pxdiv2 .gt. xmax-xmin ) then
c
c  Start the axis at PXMIN2, to the left of XMIN, and
c  representing a whole number of steps of size PXDIV2.
c
              if ( xmin .ge. 0.0 ) then
                pxmin2 = real ( int ( xmin / pxdiv2 ) ) * pxdiv2
              else
                pxmin2 = real ( int ( xmin / pxdiv2 ) - 1 ) * pxdiv2
              end if
c
c  PXMAX2 is, of course, NDIVS steps above PXMIN2.
c
              pxmax2 = pxmin2 + ndivs * pxdiv2
c
c  Don't even consider going on unless PXMAX2 is at least XMAX.
c
c  If you consider this grid, then judge it by the relative amount
c  of wasted axis length.
c
              if ( pxmax2 .ge. xmax ) then
 
                good = ( xmax - xmin ) / ( pxmax2 - pxmin2 )
 
                if ( good .gt. best ) then
                  best = good
                  pxmax = pxmax2
                  pxmin = pxmin2
                  pxdiv = pxdiv2
                  nticks = iticks(i)
                end if
 
              end if
 
            end if
 
          end do
 
        end do
 
      end if
 
      return
      end
      subroutine quit(iquit,nplot)
c
c***********************************************************************
c
      integer iquit
      character*1 isay
      logical leqi
      integer nplot
c
      if (iquit .eq. 1) then
        write(*,*)' '
        write(*,*)'QUIT - Warning!'
        write(*,*)'  Please type "Y" to confirm that'
        write(*,*)'  you want to quit the program!'
        read(*,'(a)',end=10,err=10)isay
 
        if ( .not. leqi(isay,'Y')) then
          write(*,*)' '
          write(*,*)'The QUIT command is cancelled.'
          return
        end if
 
      end if
 
      write(*,*)' '
      write(*,*)'QUIT - Note:'
      write(*,*)'  The RobPlot program is halting.'
c
c  If NPLOT > 0, then plots have been made, and the plot file is open.
c  Close it.
c
      if (nplot.gt.0) then
        call plotnd
        write(*,*)' '
        write(*,*)'The PostScript output file has been closed.'
      end if
 
      stop
 
10    continue
      write(*,*)' '
      write(*,*)'QUIT - Error!'
      write(*,*)'  Problem reading user confirmation.'
      write(*,*)'  Error halt.'
      stop
      end
      subroutine ranger(string,maxval,nval,ival)
c
c***********************************************************************
c
c  RANGER reads a string of the form
c
c    '4:8 10 2 14:20'
c
c  or, (commas are optional)
c
c    '4:8,10,  2, 14:20'
c
c  and return the values
c
c    4, 5, 6, 7, 8, 10, 2, 14, 15, 16, 17, 18, 19, 20
c
c  0 and negative integers are acceptable.  So are pairs
c  of values that are equal, as in '4:4', which just represents
c  4, and pairs that represent descending sequences, as
c  in '4:-2'.
c
c
c  Input, CHARACTER*(*) STRING, contains a string of integers,
c  representing themselves, and pairs of integers representing
c  themselves and all integers between them.
c
c  Input, INTEGER MAXVAL, the dimension of the IVAL vector,
c  which represents the maximum number of integers that may
c  be read from the string.
c
c  Output, INTEGER NVAL, the number of integers read from the
c  string.
c
c  Output, INTEGER IVAL(MAXVAL).  The first NVAL entries of
c  IVAL contain the integers read from the string.
c
      integer maxval
c
      integer i
      integer ierror
      integer ilo
      integer inc
      integer intval
      integer ival(maxval)
      integer lchar
      integer lenchr
      integer lens
      integer next
      integer nval
      character*(*) string
c
      nval = 0
c
c  Replace commas by blanks.
c
      lens = lenchr(string)
 
      do i=1,lens
        if (string(i:i).eq.',') then
          string(i:i) = ' '
        end if
      end do
c
c  Remove all double blanks.
c
      call chrdb2(string)
c
c  Get the length of the string to the last nonblank.
c
      lens = lenchr(string)
c
c  Set a pointer to the next location in STRING to be examined.
c
      next=1
 
10    continue
c
c  Return if we have exhausted the string.
c
      if (next .gt. lens) then
        return
      end if
c
c  Find the next integer in the string.
c
      call chrcti(string(next:),intval,ierror,lchar)
 
      if (ierror.ne.0) then
        return
      end if
c
c  Move the pointer.
c
      next=next+lchar
c
c  If there's room, add the value to the list.
c
      if (nval .lt. maxval) then
        nval = nval+1
        ival(nval) = intval
      else
        return
      end if
c
c  Have we reached the end of the string?
c
      if (next .gt. lens) then
        return
      end if
c
c  Skip past the next character if it is a space.
c
      if (string(next:next) .eq. ' ') then
        next=next+1
        if (next .gt. lens) then
          return
        end if
      end if
c
c  Is the next character a colon?
c
      if (string(next:next).ne.':') then
        go to 10
      end if
c
c  Increase the pointer past the colon.
c
      next=next+1
 
      if (next .gt. lens) then
        return
      end if
c
c  Find the next integer in the string.
c
      call chrcti(string(next:),intval,ierror,lchar)
 
      if (ierror.ne.0) then
        return
      end if
c
c  Move the pointer.
c
      next = next+lchar
c
c  Generate integers between the two values.
c
      ilo=ival(nval)
 
      if (intval.ge.ilo) then
        inc=+1
      else
        inc=-1
      end if
 
      do i = ilo+inc,intval,inc
        if (nval.lt.maxval) then
          nval=nval+1
          ival(nval)=i
        else
          return
        end if
      end do
 
      go to 10
      end
      subroutine rrange(nval,x,xmax,xmin)
c
c***********************************************************************
c
c  RRANGE computes the range of a real array.
c
c
c  Input, INTEGER NVAL, the number of entries in the array.
c
c  Input, REAL X(NVAL), the array.
c
c  Output, REAL XMAX, XMIN, the largest and smallest entries in the 
c  array.
c
      integer nval
c
      integer i
      real x(nval)
      real xmax
      real xmin
c
      intrinsic max
      intrinsic min
c
      xmax=x(1)
      xmin=x(1)
 
      do i=2,nval
        xmax=max(xmax,x(i))
        xmin=min(xmin,x(i))
      end do
 
      return
      end
      subroutine rrange2(nval,x,xmax,xmin,y,ymax,ymin,none)
c
c***********************************************************************
c
c  RRANGE2 is given a set of pairs of points (X,Y), and a range
c  XMIN to XMAX of valid X values.  Over this range, it seeks
c  YMIN and YMAX, the minimum and maximum values of Y for
c  valid X's.
c
c
c  Input, INTEGER NVAL, the number of entries in the array.
c
c  Input, REAL X(NVAL), the X array.
c
c  Input, REAL XMAX, XMIN, the largest and smallest entries valid
c  values of X.
c
c  Input, REAL Y(NVAL), the Y array.
c
c  Output, REAL YMAX, YMIN, the largest and smallest entries of
c  Y that occur paired with an entry of X between XMIN and XMAX.
c
c  Output, LOGICAL NONE, is TRUE if no values of X were valid.
c  In this case, YMIN=YMAX=0.0, but these values are meaningless.
c
      integer nval
c
      logical none
      integer i
      real x(nval)
      real xmax
      real xmin
      real y(nval)
      real ymax
      real ymin
c
      intrinsic max
      intrinsic min
c
      none = .true.
 
      ymin=0.0
      ymax=0.0
 
      do i=1,nval
        if (xmin .le. x(i) .and. x(i) .le. xmax) then
 
          if (none) then
            ymin = y(i)
            ymax = y(i)
            none = .false.
          else
            ymin = min( ymin, y(i))
            ymax = max( ymax, y(i))
          end if
 
        end if
      end do
 
      return
      end
      subroutine show(datamax,datamin,echo,infile,iplot,item,ix,
     &  mark,MAXRECL,MAXVAL,MAXVAR,names,nplot,nskip,nthin,nval,
     &  nvar,nxgrid,ny,nygrid,over,psfile,recl,title,xmax,xmaxr,xmin,
     &  xminr,ymax,ymaxr,ymaxx,ymin,yminr,yminx)
c
c***********************************************************************
c
      integer MAXRECL
      integer MAXVAL
      integer MAXVAR
c
      real datamax(0:MAXVAR)
      real datamin(0:MAXVAR)
      logical echo
      integer i
      character*80 infile
      integer iplot(0:MAXVAR)
      character*80 item
      integer ix
      integer lenc
      integer lenchr
      logical leqi
      character*1 mark
      character*20 names(0:MAXVAR)
      integer nplot
      integer nskip
      integer nthin
      integer nval
      integer nvar
      integer nxgrid
      integer ny
      integer nygrid
      logical over
      character*80 psfile
      integer recl
      character*80 title
      real xmax
      real xmaxr
      real xmin
      real xminr
      real ymax(0:MAXVAR)
      real ymaxr(0:MAXVAR)
      real ymaxx(0:MAXVAR)
      real ymin(0:MAXVAR)
      real yminr(0:MAXVAR)
      real yminx(0:MAXVAR)
c
      if ( item .eq. '?') then
        write(*,*)' '
        write(*,*)'The following data may be shown:'
        write(*,*)' '
        write(*,*)'Program flags:'
        write(*,*)' '
        write(*,*)'  ECHO'
        write(*,*)' '
        write(*,*)'Internal limits:'
        write(*,*)' '
        write(*,*)'  MAXRECL'
        write(*,*)'  MAXVAL'
        write(*,*)'  MAXVAR'
        write(*,*)' '
        write(*,*)'User data:'
        write(*,*)' '
        write(*,*)'  INFILE'
        write(*,*)'  DATAMAX'
        write(*,*)'  DATAMIN'
        write(*,*)'  MARK'
        write(*,*)'  NAMES'
        write(*,*)'  NVAL'
        write(*,*)'  NVAR'
        write(*,*)'  RECL'
        write(*,*)'  SKIP'
        write(*,*)' '
        write(*,*)'Plotting data:'
        write(*,*)' '
        write(*,*)'  NPLOT'
        write(*,*)'  NTHIN'
        write(*,*)'  NXGRID'
        write(*,*)'  NY'
        write(*,*)'  NYGRID'
        write(*,*)'  OVER'
        write(*,*)'  PSFILE'
        write(*,*)'  TITLE'
        write(*,*)'  X, XMAX, XMAXR, XMIN, XMINR'
        write(*,*)'  Y, YMAX, YMAXX, YMIN, YMINX.'
        write(*,*)' '
        write(*,*)'? means print this list.'
        write(*,*)'* means print everything.'
        return
      end if
 
      if(item.eq.'*')then
        write(*,*)' '
        write(*,*)'Program flags:'
        write(*,*)' '
      end if
 
      if (leqi(item,'ECHO').or.item.eq.'*') then
        write(*,*)' '
        write(*,*)'The flag controlling input echo is ECHO=',echo
      end if
c
      if (item.eq.'*')then
        write(*,*)' '
        write(*,*)'Internal limits:'
        write(*,*)' '
      end if
 
      if (leqi(item,'MAXRECL').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'The maximum number of characters on a line of the '
     &    //'input data file is MAXRECL=',MAXRECL
      end if
 
      if (leqi(item,'MAXVAL').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'The maximum number of data values is MAXVAL=',MAXVAL
      end if
 
      if (leqi(item,'MAXVAR').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'The maximum number of data variables is MAXVAR=',
     &    MAXVAR
      end if
c
c  User data:
c
      if (item.eq.'*')then
        write(*,*)' '
        write(*,*)'User data:'
        write(*,*)' '
      end if
 
      if (leqi(item,'NAMES').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'The data names are:'
        write(*,*)' '
        do i=0,nvar
          write(*,'(1x,i8,2x,a,2x,2g14.6)')i,names(i)
        end do
        write(*,*)' '
      end if
 
      if (leqi(item,'DATAMAX').or.leqi(item,'DATAMIN') .or.
     &  item .eq. '*') then
        write(*,*)' '
        write(*,*)'The data minima and maxima are:'
        write(*,*)' '
        do i=0,nvar
          write(*,'(1x,i8,2x,a,2x,2g14.6)')i,names(i),datamin(i),
     &      datamax(i)
        end do
        write(*,*)' '
      end if
 
      if (leqi(item,'INFILE').or.item .eq. '*') then
        write(*,*)' '
        if (infile .eq. ' ') then
          write(*,*)' '
          write(*,*)'The input file has not been specified yet.'
          write(*,*)'  (Use the INFILE=... command.)'
        else
          lenc=lenchr(infile)
          write(*,*)'The input file is INFILE="'//infile(1:lenc)//'"'
        end if
      end if
 
      if (leqi(item,'MARK').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'Input data is separated by the character "'
     &    //mark//'"'
      end if
 
      if (leqi(item,'NVAL').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'The number of data values, NVAL, is ',nval
      end if
 
      if (leqi(item,'NVAR').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'The number of data variables, NVAR, is ',nvar
      end if
 
      if (leqi(item,'RECL').or.item .eq. '*') then
        write(*,*)' '
        if (recl .eq. 0) then
          write(*,*)'RECL=0; the input data file will be opened '
          write(*,*)'without any explicit RECL value.'
        else
          write(*,*)'The input data file will be opened '
          write(*,*)'with RECL = ',recl
        end if
      end if
 
      if (leqi(item,'SKIP').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'Before each READ command, NSKIP=, ',nskip,
     &    ' data records are skipped.'
      end if
c
c  Plotting data.
c
      if (item.eq.'*')then
        write(*,*)' '
        write(*,*)'Plotting data:'
        write(*,*)' '
      end if
 
      if (leqi(item,'NPLOT').or.item .eq. '*') then
        write(*,*)' '
        if (nplot .eq. 0) then
          write(*,*)'No plots have been made.'
        else
          write(*,*)'NPLOT=',nplot,' plots have been made.'
        end if
      end if
 
      if (leqi(item,'NTHIN').or.item .eq. '*') then
        write(*,*)' '
        if (nthin .eq. 1) then
          write(*,*)'No data thinning has been requested, NTHIN=',nthin
        else
          write(*,*)'Data may be thinned by a factor of NTHIN=',nthin
        end if
      end if
 
      if (leqi(item,'NXGRID').or.item .eq. '*') then
        write(*,*)'The number of grid lines used along the '//
     &    'X direction is NXGRID=',nxgrid
      end if
 
      if (leqi(item,'NYGRID').or.item .eq. '*') then
        write(*,*)'The number of grid lines used along the '//
     &    'Y direction is NYGRID=',nygrid
      end if
 
      if (leqi(item,'NY').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'The number of Y plot variables is NY=',ny
      end if
 
      if (leqi(item,'OVER').or.item .eq. '*') then
        write(*,*)' '
        write(*,*)'Make overplots? OVER = ',over
      end if
 
      if (leqi(item,'PSFILE').or.item .eq. '*') then
        write(*,*)' '
        if (psfile .eq. ' ') then
          write(*,*)'The PostScript file has not been specified yet.'
          write(*,*)'  (Use the PSFILE=... command.)'
        else
          lenc=lenchr(psfile)
          write(*,*)'The PostScript file is PSFILE="'//psfile(1:lenc)
     &      //'"'
        end if
      end if
 
      if (leqi(item,'TITLE').or.item .eq. '*') then
        write(*,*)' '
        if (title .eq. ' ') then
          write(*,*)'No plot title has been specified.'
          write(*,*)'  (Use the TITLE=... command.)'
        else
          lenc=lenchr(title)
          write(*,*)'The plot title is TITLE="'//title(1:lenc)//'"'
        end if
      end if
 
      if ( leqi(item,'X') ) then
        write(*,*)' '
        if (ix .lt. 0 .or. ix .gt. nvar) then
          write(*,*)'The X plot variable has not been specified yet.'
          write(*,*)'  (Use the X=... command)'
        else
          lenc=lenchr(names(ix))
          write(*,*)'The X plot variable, named ',names(ix)(1:lenc),
     &      ' is index ',ix
        end if
      end if
 
      if (leqi(item,'XMAX').or.leqi(item,'XMAXR').or.
     &    leqi(item,'XMIN').or.leqi(item,'XMINR').or.
     &  item .eq. '*') then
        if (ix .lt.0 .or. ix.gt.nvar ) then
          if (leqi(item,'XMAX')) then
            write(*,*)'No X plot variable has been defined yet!'
            write(*,*)'  (Use the X=... command!).'
          end if
        else
          write(*,*)' '
          lenc=lenchr(names(ix))
          write(*,*)'The X plot variable, named ',names(ix)(1:lenc),
     &      ' is index ',ix
          write(*,*)'  Raw X range:              ',
     &      datamin(ix),datamax(ix)
          write(*,*)'  User X limits:            ',
     &      xmin,xmax
          write(*,*)'  Rounded X limits:         ',
     &      xminr,xmaxr
          write(*,*)' '
        end if
      end if
 
      if ( leqi(item,'Y') ) then
 
        if (ny.ne.0)then
          write(*,*)' '
          write(*,*)'The Y variables to be plotted are:'
          write(*,*)' '
          do i=0,nvar
            if (iplot(i).eq.1) then
              write(*,'(1x,i3,2x,a)')i,names(i)
            end if
          end do
        else
          write(*,*)' '
          write(*,*)'No Y variables have been chosen.'
          write(*,*)'(Use the Y=... command!)'
        end if
 
      end if
 
      if (leqi(item,'YMAX').or.leqi(item,'YMAXX').or.
     &  leqi(item,'YMAX').or.leqi(item,'YMAXX').or.
     &  item .eq. '*') then
        if (ny .eq. 0) then
          if (leqi(item,'YMAX').or.leqi(item,'YMAXX').or.
     &        leqi(item,'YMIN').or.leqi(item,'YMINX') ) then
            write(*,*)'No Y plot variables have been defined yet!'
            write(*,*)'  (Use the Y=... command!).'
          end if
        else
          write(*,*)' '
          write(*,*)'Y variable indices, names, and ranges:'
          do i=0,nvar
            if (iplot(i).eq.1)then
              write(*,*)' '
              write(*,'(1x,i3,2x,a)')i,names(i)
              write(*,*)'  Raw Y range:              ',
     &          datamin(i),datamax(i)
              write(*,*)'  Y range in user X range:  ',
     &          yminx(i),ymaxx(i)
              write(*,*)'  User Y limits:            ',
     &          ymin(i),ymax(i)
              write(*,*)'  Rounded Y limits:         ',
     &          yminr(i),ymaxr(i)
            end if
          end do
        end if
      end if
 
 
      return
      end
      subroutine skip(nskip,ierror)
c
c***********************************************************************
c
c  SKIP skips NSKIP "data records", where a "data record" is defined as 
c  a sequence of zero or more FORTRAN sequential records, terminated by
c  a record beginning with the text 'RECORD', or by an end-of-file.
c
      integer i
      integer ierror
      logical leqi
      integer nskip
      character*6 string
c
      ierror=0
 
      do i=1,nskip
 
10      continue
 
        read(1,'(a)',end=20,err=20)string
        if (.not.leqi(string(1:6),'RECORD')) go to 10
 
      end do
 
      return
c
c  Unexpected error or end of file.
c
20    continue
      ierror=1
      return
      end
      subroutine wrdnex2(string,first,last)
c
c***********************************************************************
c
c  WRDNEX2 accepts a STRING containing words, separated by commas
c  and blanks.  At least one comma or blank is required to separate
c  words.
c
c  WRDNEX2 returns:
c  * FIRST, the first string of nonblank, noncomma characters;
c  * LAST, the characters of STRING that occur after FIRST and
c    the commas and blanks.
c
c
c  Input/output, CHARACTER*(*) STRING is the string of words to be 
c  analyzed.
c
c  Output, CHARACTER*(*) FIRST, the next word in the string.
c
c  Output, CHARACTER*(*) LAST, the text of the remaining words in the
c  string.
c
      character*1 c
      character*(*) first
      integer i
      integer ido
      integer ifirst
      integer ilast
      character*(*) last
      integer lenchr
      integer lenf
      integer lenl
      integer lens
      character*(*) string
c
      first = ' '
      last = ' '
 
      ifirst = 0
      ilast = 0
 
      lens=lenchr(string)
      lenf=len(first)
      lenl=len(last)
 
      ido = 0
 
      do i=1,lens
 
        c = string(i:i)
 
        if (ido .eq. 0) then
          if ( c .ne. ' ' .and. c .ne. ',') then
            ido = 1
          end if
        end if
 
        if ( ido .eq. 1) then
          if ( c .ne. ' ' .and. c .ne. ',') then
            ifirst = ifirst+1
            if (ifirst .le. lenf) then
              first(ifirst:ifirst)=c
            end if
          else
            ido = 2
          end if
        end if
 
        if ( ido .eq. 2 ) then
          if ( c .ne. ' ' .and. c .ne. ',') then
            ido = 3
          end if
        end if
 
        if ( ido .eq. 3 ) then
          ilast = ilast+1
          if (ilast .le. lenl) then
            last(ilast:ilast) = c
          end if
        end if
 
      end do
 
      return
      end
