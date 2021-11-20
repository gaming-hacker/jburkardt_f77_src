c  RANPRB.F  05 April 1995
c
      program ranprb
c
c***********************************************************************
c
      write(*,*)' '
      write(*,*)'RANPRB'
      write(*,*)'Tests for the random number routines of RANPACK.'
      write(*,*)' '
c
      call test01
      call test02
      call test03
      call test04
      call test05
      call test06
      call test07
      call test08
      call test09
      call test10
      call test11
      call test12
      call test13
      call test14
      call test15
      call test16
      call test17
      stop
      end
      subroutine test01
c
c***********************************************************************
c
c  Test ability of RANGET/RANSET to get and set the random number 
c  seed.
c
      integer i
      integer iseed1
      integer iseed2
      real ranf
      real x
c
      write(*,*)' '
      write(*,*)'TEST01'
      write(*,*)'Test RANF, RANGET and RANSET routines.'
c
c  Get the current seed
c
      call ranget(iseed1)
      write(*,*)' '
      write(*,*)'RANGET reports current seed is ',iseed1
      write(*,*)'Generate 10 random numbers with RANF:'
      write(*,*)' '

      do i=1,10
        x=ranf()
        write(*,*)x
      enddo
      call ranget(iseed2)

      write(*,*)' '
      write(*,*)'RANGET reports current seed is ',iseed2
      write(*,*)'Now have RANSET reset the seed to ',iseed1

      call ranset(iseed1)

      write(*,*)' '
      write(*,*)'Now generate 20 random numbers.  The first 10'
      write(*,*)'should match the previous sequence:'

      do i=1,20
        x=ranf()
        write(*,*)x
      enddo

      write(*,*)' '
      write(*,*)'Now use RANSET to reset the seed to ',iseed2
      write(*,*)'This should allow us to resume the first sequence'
      write(*,*)'where we left off.  The next 10 numbers should'
      write(*,*)'match the last 10 of the set of 20.'
      write(*,*)' '

      call ranset(iseed2)

      do i=1,10
        x=ranf()
        write(*,*)x
      enddo

      return
      end
      subroutine test02
c
c***********************************************************************
c
c  Test the average of 10000 random numbers.
c
      integer ntest
      parameter(ntest=32)

      integer isd09
      parameter (isd09=10)
c
      integer npts
      parameter(npts=10000)
c
      real aray20(npts)
      real aray25(npts)
      real ave
      real average(ntest)
      real halton
      integer i
      integer isd10
      integer isd14
      integer isd15
      integer isd16
      integer isd17
      integer isd18
      integer isd21
      integer isd22
      integer isd23
      integer isd26
      integer isd27
      integer isd28
      integer isdhal
      integer iseed
      integer iseed12(4)
      integer istart
      integer itest
      integer j
      integer jhi
      integer jsd23
      integer ksd23
      character name(ntest)*6
      integer nsob
      real off
      real ran0
      real ran1
      real ran10
      real ran11
      real ran12
      real ran13
      real ran14
      real ran15
      real ran16
      real ran17
      real ran18
      real ran19
      real ran2
      real ran21
      real ran22
      real ran22s
      real ran23
      real ran24
      real ran26
      real ran27
      real ran28
      real ran29
      real ran3
      real ran4
      real ran5
      real ran6
      real ran7
      real ran8
      real ran9
      logical reset
      real sd29
      real t11
      real t8
      real t9(isd09+1)
      real temp
      real temp25(64)

      real xsob(1)
c
      write(*,*)' '
      write(*,*)'TEST02'
      write(*,*)'Compute the average of 10000 successive random'
      write(*,*)'values, to see how far from 0.5 the answer is.'
      write(*,*)' '
c
      name(1)='ran0'
      name(2)='ran1'
      name(3)='ran2'
      name(4)='ran3'
      name(5)='ran4'
      name(6)='ran5'
      name(7)='ran6'
      name(8)='ran7'
      name(9)='ran8'
      name(10)='ran9'
      name(11)='ran10'
      name(12)='ran11'
      name(13)='ran12'
      name(14)='ran13'
      name(15)='ran14'
      name(16)='ran15'
      name(17)='ran16'
      name(18)='ran17'
      name(19)='ran18'
      name(20)='ran19'
      name(21)='ran20'
      name(22)='ran21'
      name(23)='ran22'
      name(24)='ran23'
      name(25)='ran24'
      name(26)='ran25'
      name(27)='ran26'
      name(28)='ran27'
      name(29)='ran28'
      name(30)='ran29'
      name(ntest-1)='halton'
      name(ntest)='sobol1'
c
      do i=1,ntest
        average(i)=0.0
      enddo
c
c  Set initial seed for each generator that needs one.
c
      t8=0.0
      t9(1)=0.0
      isd10=0
      t11=0.0
      iseed12(1)=istart()
      iseed12(2)=88
      iseed12(3)=102
      iseed12(4)=79
      reset=.true.
      isd14=istart()
      isd15=istart()
      isd16=istart()
      isd17=istart()
      isd18=istart()
      call ran20(aray20,npts)
      isd21=istart()
      isd22=istart()
      temp=ran22s(isd22)
      isd23=istart()
      jsd23=istart()
      ksd23=istart()
      call ran23s(isd23,jsd23,ksd23)
      isd23=istart()
      jsd23=istart()
      call ran24s(isd23,jsd23)
      call ran25(0,temp25)
      do i=1,npts,64
        call ran25(64,temp25)
        jhi=min(i+63,npts)
        do j=i,jhi
          aray25(j)=temp25(j+1-i)
        enddo
      enddo
      isd26=istart()
      temp=ran26(isd26)
      isd26=istart()
      isd27=istart()
      isd28=istart()
      sd29=0.123456789
c
      isdhal=istart()
      nsob=-1
      call sobol1(nsob,xsob)
      nsob=1
c
      itest=1
      iseed=istart()
      ave=0.0
      do i=1,npts
        ave=ave+ran0(iseed)
      enddo
      ave=ave/real(npts)
      off=ave-0.5
      write(*,*)name(itest),' average=',ave,' average-0.5=',off

      itest=2
      iseed=istart()
      ave=0.0
      do i=1,npts
c       ave=ave+ran1(iseed)
      enddo
      ave=ave/real(npts)
      off=ave-0.5
      write(*,*)name(itest),' average=',ave,' average-0.5=',off

      itest=3
      iseed=istart()
      ave=0.0
      do i=1,npts
        ave=ave+ran2(iseed)
      enddo
      ave=ave/real(npts)
      off=ave-0.5
      write(*,*)name(itest),' average=',ave,' average-0.5=',off

      itest=4
      iseed=istart()
      ave=0.0
      do i=1,npts
        ave=ave+ran3(iseed)
      enddo
      ave=ave/real(npts)
      off=ave-0.5
      write(*,*)name(itest),' average=',ave,' average-0.5=',off

      itest=5
      ave=0.0
      do i=1,npts
        ave=ave+ran4()
      enddo
      ave=ave/real(npts)
      off=ave-0.5
      write(*,*)name(itest),' average=',ave,' average-0.5=',off

      itest=6
      ave=0.0
      do i=1,npts
        ave=ave+ran5()
      enddo
      ave=ave/real(npts)
      off=ave-0.5
      write(*,*)name(itest),' average=',ave,' average-0.5=',off

      itest=6
      iseed=istart()
      ave=0.0
      do i=1,npts
        ave=ave+ran6(iseed)
      enddo
      ave=ave/real(npts)
      off=ave-0.5
      write(*,*)name(itest),' average=',ave,' average-0.5=',off

      itest=7
      ave=0.0
      do i=1,npts
        ave=ave+ran7()
      enddo
      ave=ave/real(npts)
      off=ave-0.5
      write(*,*)name(itest),' average=',ave,' average-0.5=',off

      do i=1,npts
        average(9)=average(9)+ran8(t8)
        average(10)=average(10)+ran9(t9,isd09)
        average(11)=average(11)+ran10(isd10)
        average(12)=average(12)+ran11(t11)
        average(13)=average(13)+ran12(iseed12)
        average(14)=average(14)+ran13(reset)
        average(15)=average(15)+ran14(isd14)
        average(16)=average(16)+ran15(isd15)
        average(17)=average(17)+ran16(isd16)
        average(18)=average(18)+ran17(isd17)
        average(19)=average(19)+ran18(isd18)
        average(20)=average(20)+ran19()
        average(21)=average(21)+aray20(i)
        average(22)=average(22)+ran21(isd21)
        average(23)=average(23)+ran22()
        average(24)=average(24)+ran23()
        average(25)=average(25)+ran24()
        average(26)=average(26)+aray25(i)
        average(27)=average(27)+ran26(isd26)
        average(28)=average(28)+ran27(isd27)
        average(29)=average(29)+ran28(isd28)
        average(30)=average(30)+ran29(sd29)
        average(ntest-1)=average(ntest-1)+halton(isdhal)
        call sobol1(nsob,xsob)
        average(ntest)=average(ntest)+xsob(1)
      enddo
c
      write(*,*)' '

      do i=7,ntest
        average(i)=average(i)/real(npts)
        off=average(i)-0.5
        write(*,*)name(i),' average=',average(i),' average-0.5=',off
      enddo

      return
      end
      subroutine test03
c
c***********************************************************************
c
c  Test the distribution of 10000 random numbers.
c
      integer ntest
      parameter(ntest=32)
c
      integer npts
      parameter(npts=10000)
c
      integer isd09
      parameter(isd09=10)
c
      real aray20(npts)
      real aray25(npts)
      real halton
      integer i
      integer ibin(ntest,10)
      integer ihi
      integer isd00
      integer isd02
      integer isd03
      integer isd06
      integer isd10
      integer isd14
      integer isd15
      integer isd16
      integer isd17
      integer isd18
      integer isd21
      integer isd22
      integer isd23
      integer isd26
      integer isd27
      integer isd28
      integer isdhal
      integer iseed(4)
      integer istart
      integer itest
      integer j
      integer jhi
      integer jsd23
      integer ksd23
      character name(ntest)*6
      integer nsob
      real ran0
      real ran1
      real ran10
      real ran11
      real ran12
      real ran13
      real ran14
      real ran15
      real ran16
      real ran17
      real ran18
      real ran19
      real ran2
      real ran21
      real ran22
      real ran22s
      real ran23
      real ran24
      real ran26
      real ran27
      real ran28
      real ran29
      real ran3
      real ran4
      real ran5
      real ran6
      real ran7
      real ran8
      real ran9
      logical reset
      real sd29
      real t11
      real t8
      real t9(isd09+1)
      real temp
      real temp25(64)
      real xhi
      real xlo
      real xsob(1)
c
      write(*,*)' '
      write(*,*)'TEST03'
      write(*,*)'Test distribution of 10000 successive random values.'
      write(*,*)'Compare the ranges 0<x<0.1,... 0.9<x<1.0.'
      write(*,*)' '
c
      name(1)='ran0'
      name(2)='ran1'
      name(3)='ran2'
      name(4)='ran3'
      name(5)='ran4'
      name(6)='ran5'
      name(7)='ran6'
      name(8)='ran7'
      name(9)='ran8'
      name(10)='ran9'
      name(11)='ran10'
      name(12)='ran11'
      name(13)='ran12'
      name(14)='ran13'
      name(15)='ran14'
      name(16)='ran15'
      name(17)='ran16'
      name(18)='ran17'
      name(19)='ran18'
      name(20)='ran19'
      name(21)='ran20'
      name(22)='ran21'
      name(23)='ran22'
      name(24)='ran23'
      name(25)='ran24'
      name(26)='ran25'
      name(27)='ran26'
      name(28)='ran27'
      name(29)='ran28'
      name(30)='ran29'
      name(ntest-1)='halton'
      name(ntest)='sobol1'
c
      do i=1,ntest
        do j=1,10
          ibin(i,j)=0
        enddo
      enddo
c
c  Set the seed for each generator that needs one.
c
      isd00=-123
      isd01=istart()
      isd02=istart()
      isd03=istart()
      isd06=istart()
      t8=0.0
      t9(1)=0.0
      isd10=istart()
      t11=0.0
      iseed(1)=istart()
      iseed(2)=88
      iseed(3)=102
      iseed(4)=79
      reset=.true.
      isd14=istart()
      isd15=istart()
      isd16=istart()
      isd17=istart()
      isd18=istart()
      call ran20(aray20,npts)
      isd21=istart()
      isd22=istart()
      temp=ran22s(isd22)
      isd23=istart()
      jsd23=istart()
      ksd23=istart()
      call ran23s(isd23,jsd23,ksd23)
      isd23=istart()
      jsd23=istart()
      call ran24s(isd23,jsd23)
c
      call ran25(0,temp25)

      do i=1,npts,64
        call ran25(64,temp25)
        jhi=min(i+63,npts)
        do j=i,jhi
          aray25(j)=temp25(j+1-i)
        enddo
      enddo

      isd26=istart()
      temp=ran26(isd26)
      isd26=istart()
      isd27=istart()
      isd28=istart()
      sd29=0.123456789
c
      isdhal=istart()
      nsob=-1
      call sobol1(nsob,xsob)
      nsob=1
c
      do i=1,npts
        j=int(10.0*ran0(isd00))+1
        ibin(1,j)=ibin(1,j)+1
c       j=int(10.0*ran1(isd01))+1
        ibin(2,j)=ibin(2,j)+1
        j=int(10.0*ran2(isd02))+1
        ibin(3,j)=ibin(3,j)+1
        j=int(10.0*ran3(isd03))+1
        ibin(4,j)=ibin(4,j)+1
        j=int(10.0*ran4())+1
        ibin(5,j)=ibin(5,j)+1
        j=int(10.0*ran5())+1
        ibin(6,j)=ibin(6,j)+1
        j=int(10.0*ran6(isd06))+1
        ibin(7,j)=ibin(7,j)+1
        j=int(10.0*ran7())+1
        ibin(8,j)=ibin(8,j)+1
        j=int(10.0*ran8(t8))+1
        ibin(9,j)=ibin(9,j)+1
        j=int(10.0*ran9(t9,isd09))+1
        ibin(10,j)=ibin(10,j)+1
        j=int(10.0*ran10(isd10))+1
        ibin(11,j)=ibin(11,j)+1
        j=int(10.0*ran11(t11))+1
        ibin(12,j)=ibin(12,j)+1
        j=int(10.0*ran12(iseed))+1
        ibin(13,j)=ibin(13,j)+1
        j=int(10.0*ran13(reset))+1
        ibin(14,j)=ibin(14,j)+1
        j=int(10.0*ran14(isd14))+1
        if(j.ge.1.and.j.le.10)then
          ibin(15,j)=ibin(15,j)+1
        endif
        j=int(10.0*ran15(isd15))+1
        ibin(16,j)=ibin(16,j)+1
        j=int(10.0*ran16(isd16))+1
        ibin(17,j)=ibin(17,j)+1
        j=int(10.0*ran17(isd17))+1
        ibin(18,j)=ibin(18,j)+1
        j=int(10.0*ran18(isd18))+1
        ibin(19,j)=ibin(19,j)+1
        j=int(10.0*ran19())+1
        ibin(20,j)=ibin(20,j)+1
        j=int(10.0*aray20(i))+1
        ibin(21,j)=ibin(21,j)+1
        j=int(10.0*ran21(isd21))+1
        ibin(22,j)=ibin(22,j)+1
        j=int(10.0*ran22())+1
        ibin(23,j)=ibin(23,j)+1
        j=int(10.0*ran23())+1
        ibin(24,j)=ibin(24,j)+1
        j=int(10.0*ran24())+1
        ibin(25,j)=ibin(25,j)+1
        j=int(10.0*aray25(i))+1
        ibin(26,j)=ibin(26,j)+1
        j=int(10.0*ran26(isd26))+1
        ibin(27,j)=ibin(27,j)+1
        j=int(10.0*ran27(isd27))+1
        ibin(28,j)=ibin(28,j)+1
        j=int(10.0*ran28(isd28))+1
        ibin(29,j)=ibin(29,j)+1
        j=int(10.0*ran29(sd29))+1
        ibin(30,j)=ibin(30,j)+1
        j=int(10.0*halton(isdhal))+1
        ibin(ntest-1,j)=ibin(ntest-1,j)+1
        call sobol1(nsob,xsob)
        j=int(10.0*xsob(1))+1
        ibin(ntest,j)=ibin(ntest,j)+1
      enddo
c
      do itest=1,ntest, 5
        ihi=min(itest+4,ntest)
        write(*,*)' '
        write(*,'('' Range'',8x,7(1x,a6,1x))')(name(i),i=itest,ihi)
        write(*,*)' '
        do j=1,10
          xlo=real(j-1)/10.0
          xhi=real(j)/10.0
          write(*,'(1x,2f5.2,5i8)')xlo,xhi,(ibin(i,j),i=itest,ihi)
        enddo
      enddo

      return
      end
      subroutine test04
c
c***********************************************************************
c
c  Test the distribution of 10000 normally distributed random numbers.
c
      integer ntest
      parameter(ntest=13)
c
      integer nbin
      parameter(nbin=16)
c
      integer npts
      parameter(npts=10000)
c
      real dnorm6
      real gau1
      real gau10
      real gau11
      real gau12
      real gau2
      real gau3
      real gau4
      real gau5
      real gau6
      real gau7
      real gau8
      real gau9
      real gau9s
      real gmean
      real gvar
      integer i
      integer ibin(ntest,nbin)
      integer iseed
      integer istart
      integer j
      character name(ntest)*5
      real sd12
      real temp
      real v11(npts)
      real v5(npts)
      real v6(npts)
      real xhi
      real xlo
c
      write(*,*)' '
      write(*,*)'TEST04'
      write(*,*)'Test distribution of 10000 successive normally'
      write(*,*)'distributed random values.'
      write(*,*)' '
c
      name(1)='Gau1'
      name(2)='Gau2'
      name(3)='Gau3'
      name(4)='Gau4'
      name(5)='Gau5'
      name(6)='Gau6'
      name(7)='Gau7'
      name(8)='Gau8'
      name(9)='Gau9'
      name(10)='Gau10'
      name(11)='Gau11'
      name(12)='Gau12'
      name(13)='Exact'
c
      do i=1,ntest
        do j=1,nbin
          ibin(i,j)=0
        enddo
      enddo
c
      gmean=0.0
      gvar=1.0

      iseed=istart()
      itest=1
      do i=1,npts
        j=int(2*gau1(gmean,gvar,iseed)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin)ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      iseed=istart()
      itest=2
      do i=1,npts
        j=int(2*gau2(gmean,gvar,iseed)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      iseed=istart()
      itest=3
      do i=1,npts
        j=int(2*gau3(iseed)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      iseed=istart()
      itest=4
      do i=1,npts
        j=int(2*gau4(gmean,gvar,iseed)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      call gau5(v5,npts)
      itest=5
      do i=1,npts
        j=int(2*v5(i)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      call gau6(v6,npts)
      itest=6
      do i=1,npts
        j=int(2*v6(i)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      iseed=istart()
      itest=7
      do i=1,npts
        j=int(2*gau7(iseed)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      iseed=istart()
      itest=8
      do i=1,npts
        j=int(2*gau8(iseed)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      iseed=istart()
      temp=gau9s(iseed)
      itest=9
      do i=1,npts
        j=int(2*gau9()+real(nbin/2)+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      iseed=istart()
      itest=10
      do i=1,npts
        j=int(2*gau10(gmean,gvar,iseed)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      call gau11(v11,npts)
      itest=11
      do i=1,npts
        j=int(2*v11(i)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin) ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      sd12=0.123456789
      itest=12
      do i=1,npts
        j=int(2*gau12(sd12)+real(nbin)/2+1)
        if(j.ge.1.and.j.le.nbin)ibin(itest,j)=ibin(itest,j)+1
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      itest=13
      do j=1,nbin
        xlo=real(j-1-nbin/2)/2.0
        xhi=real(j-nbin/2)/2.0
        ibin(itest,j)=npts*(dnorm6(xhi)-dnorm6(xlo))
      enddo
      write(*,'(a5,8i6)')name(itest),(ibin(itest,j),j=1,8)
      write(*,'(5x,8i6)')            (ibin(itest,j),j=9,16)

      return
      end
      subroutine test05
c
c***********************************************************************
c
c  Time uniform random value routines.
c
      integer nget
      parameter(nget=10000)
c
      real gau1
      real gau10
      real gau11
      real gau12
      real gau2
      real gau3
      real gau4
      real gau5
      real gau6
      real gau7
      real gau8
      real gau9
      real gau9s
      real gdev
      real gmean
      real gvar
      integer i
      integer iseed
      integer istart
      real ranf
      real sd12
      real temp
      real time
      real time1
      real time2
      real vector(nget)
      real x
c
      write(*,*)' '
      write(*,*)'TEST05'
      write(*,*)'Compare the speed of normal random value routines.'
      write(*,*)' '
      write(*,*)'Gau1, Gau2, Gau3, Gau4, Gau7, Gau8 are scalar codes.'
      write(*,*)'Gau5, Gau6 and Gau11 are vectorized.'
      write(*,*)' '
      write(*,*)'Gau3, Gau4, Gau5 and Gau11 return true normal'
      write(*,*)'random values, while Gau1, Gau2, Gau6, and Gau12'
      write(*,*)'return approximate normal random values,and'
      write(*,*)'Gau7 uses a sort of interpolation scheme.'
      write(*,*)' '
      write(*,*)'We time the computation of ',nget,' values.'
      write(*,*)' '
c
c  Try GAU1, scalar, approximate
c
      gmean=0.0
      gvar=1.0
      iseed=istart()
      call second(time1)
      do i=1,nget
        vector(i)=gau1(gmean,gvar,iseed)
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'Gau1 required ',time,' seconds.'
c
c  Try GAU2, scalar, approximate
c
      gmean=0.0
      gvar=1.0
      iseed=istart()
      call second(time1)
      do i=1,nget
        vector(i)=gau2(gmean,gvar,iseed)
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'Gau2 required ',time,' seconds.'
c
c  Try GAU3, scalar, Box-Muller
c
      iseed=istart()
      call second(time1)
      do i=1,nget
        vector(i)=gau3(iseed)
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'Gau3 required ',time,' seconds.'
c
c  Try GAU4, scalar, Box-Muller
c
      iseed=istart()
      call second(time1)
      do i=1,nget
        vector(i)=gau4(gmean,gvar,iseed)
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'Gau4 required ',time,' seconds.'
c
c  Try GAU5, buffered, vectorized, Box-Muller normal random values.
c
      call second(time1)
      call gau5(vector,nget)
      call second(time2)
      time=time2-time1
      write(*,*)'Gau5 required ',time,' seconds.'
c
c  Try GAU6, buffered, vectorized, approximate normal random values.
c
      call second(time1)
      call gau6(vector,nget)
      call second(time2)
      time=time2-time1
      write(*,*)'Gau6 required ',time,' seconds.'
c
c  Try GAU7, scalar, interpolated values.
c
      iseed=istart()
      call second(time1)
      do i=1,nget
        x=gau7(iseed)
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'Gau7 required ',time,' seconds.'
c
c  Try GAU8, scalar
c
      iseed=istart()
      call second(time1)
      do i=1,nget
c       x=gau8(iseed)
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'Gau8 required ',time,' seconds.'
c
c  Try GAU9, scalar
c
      iseed=istart()
      temp=gau9s(iseed)
      call second(time1)
      do i=1,nget
        x=gau9()
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'Gau9 required ',time,' seconds.'
c
c  Try GAU10, scalar, interpolated values.
c
      call second(time1)
      gmean=0.0
      gdev=1.0
      iseed=istart()
      do i=1,nget
        x=gau10(gmean,gdev,iseed)
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'Gau10 required ',time,' seconds.'
c
c  Try GAU11, unbuffered, vectorized, Box-Muller normal random values.
c
      call second(time1)
      call gau11(vector,nget)
      call second(time2)
      time=time2-time1
      write(*,*)'Gau11 required ',time,' seconds.'
c
c  Try GAU12, scalar, approximate
c
      sd12=0.123456789
      call second(time1)
      do i=1,nget
        vector(i)=gau12(sd12)
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'Gau12 required ',time,' seconds.'
c
c  For comparison,call RANF for uniform random numbers
c
      call second(time1)
      do i=1,nget
        vector(i)=ranf()
      enddo
      call second(time2)
      time=time2-time1
      write(*,*)'uniforms:     ',time,' seconds.'

      return
      end
      subroutine test06
c
c***********************************************************************
c
c  Test production of random bits.
c
      integer n
      parameter(n=10)
c
      integer i
      integer ia(n)
      integer nbits
c
      write(*,*)' '
      write(*,*)'TEST06'
      write(*,*)'Demonstrate the use of IRAN0'
      write(*,*)'random integer production.'
      write(*,*)'To run the same code on VMS and UNICOS, we pretend'
      write(*,*)'that we only have 32 bits in a word.'
      write(*,*)' '

      nbits=32
      call iran0(0,ia,nbits)
      call iran0(n,ia,nbits)

      write(*,*)'Random integers:'
      write(*,*)' '

      do i=1,n
        write(*,*)ia(i)
      enddo

      return
      end
      subroutine test07
c
c***********************************************************************
c
c  Test RNOISE
c
      real anoise
      integer i
      real rnoise
      real temp
c
      write(*,*)' '
      write(*,*)'TEST07'
      write(*,*)'Demonstrate the output of RNOISE'
      write(*,*)' '
      anoise=-1.0
      temp=rnoise(anoise)
      anoise=0.001
      do i=1,20
        write(*,*)rnoise(anoise)
      enddo

      return
      end
      subroutine test08
c
c***********************************************************************
c
c  Test exponential random deviates.
c
      integer npts
      parameter(npts=10000)
c
      real ee
      parameter(ee=2.718281828)
c
      real exp1
      real exp2
      real expect
      integer i
      integer isd1
      integer isd2
      integer istart
      integer j
      real total1
      real total2
      real trig(21)
      real x1(21)
      real x2(21)
      real y1
      real y2
c
      write(*,*)' '
      write(*,*)'TEST08'
      write(*,*)'Test exponential random deviate routines:'
      write(*,*)' '
c
      do i=1,21
        trig(i)=(i-1)/20.0
        x1(i)=0.0
        x2(i)=0.0
      enddo
c
      isd1=istart()
      isd2=istart()
      do i=1,npts
        y1=exp1(isd1)
        y2=exp2(isd2)
        do j=2,21
          if((y1.lt.trig(j)).and.(y1.gt.trig(j-1))) x1(j)=x1(j)+1.0
          if((y2.lt.trig(j)).and.(y2.gt.trig(j-1))) x2(j)=x2(j)+1.0
        enddo
      enddo

      total1=0.0
      total2=0.0
      do i=2,21
        total1=total1+x1(i)
        total2=total2+x2(i)
      enddo
c
      write(*,'(1x,a,i6,a)') 'Exponential distribution with',npts,
     &  ' points:'
      write(*,'(1x,t5,a,t19,a,t31,a,t41,a)') 'Interval','  Exp1  ',
     &  '  Exp2  ','Expected'

      do i=2,21
        x1(i)=x1(i)/total1
        x2(i)=x2(i)/total2
        expect=exp(-(trig(i-1)+trig(i))/2.0)
        expect=expect*0.05*ee/(ee-1)
        write(*,'(1x,2f6.2,3f12.4)') trig(i-1),trig(i),x1(i),x2(i),
     &    expect
      enddo

      return
      end
      subroutine test09
c
c***********************************************************************
c
c  Compare Monte-Carlo,Halton and Sobol methods for 1 dimensional
c  integration.
c
      real fun
      real halton
      integer i
      integer ihi
      integer iseed
      integer istart
      integer itemp
      integer k
      integer n
      integer nmax
      integer nstep
      real quad
      real quad1
      real quad2
      real quado
      real ran2
      real tol
      real x
      real xsob(1)
c
      write(*,*)' '
      write(*,*)'TEST09'
      write(*,*)
     &  'Compare behavior of a typical random number generator'
      write(*,*)
     &  'Ran2 with the Halton sequence generated by Halton and'
      write(*,*)'the Sobol sequence generated by Sobol1 for '
      write(*,*)'Monte Carlo estimation of the integral of x**3'
      write(*,*)'from 0 to 1.  The correct value is 0.25.'
      write(*,*)' '
      write(*,*)'The approximation is iterative, doubling the number'
      write(*,*)'of points used until the integral does not change'
      write(*,*)'by more than a tolerance.'
      write(*,*)' '
c
      do k=1,3
        iseed=istart()
        nmax=15
        tol=0.001
c
        if(k.eq.1) then
          write(*,*)' '
          write(*,*)'Using Ran2'
          x=ran2(iseed)
        elseif(k.eq.2) then
          write(*,*)' '
          write(*,*)'Using Halton'
          x=halton(iseed)
        else
          write(*,*)' '
          write(*,*)'Using Sobol1'
          n=-1
          call sobol1(n,xsob)
          n=1
          call sobol1(n,xsob)
          x=xsob(1)
        endif

        quad=fun(x)
        quad1=quad
        write(*,*)' '
        write(*,*)'Approximation=',quad

        do nstep=0,nmax

          quad2=0.0
          ihi=2**nstep

          do i=1,ihi

            if(k.eq.1) then
              x=ran2(iseed)
            elseif(k.eq.2) then
              x=halton(iseed)
            else
              call sobol1(n,xsob)
              x=xsob(1)
            endif

            quad2=quad2+fun(x)

          enddo

          quad2=quad2/real(ihi)
          quado=quad
          quad=0.5*(quad1+quad2)
          write(*,*)'Approximation=',quad

          if(abs(quado-quad).le.tol) then
            write(*,*)'tolerance achieved on step nstep=',nstep
            itemp=2**(nstep+1)
            write(*,*)'Total points required=',itemp
            go to 10
          endif

          quad1=quad2

        enddo

        write(*,*)'Tolerance not achieved.'

10      continue

      enddo

      return
      end
      function fun(x)
c
c***********************************************************************
c
      real fun
      real x
c
      fun=x*x*x
      return
      end
      subroutine test10
c
c***********************************************************************
c
c  Test normal distribution routines.
c
      integer nval
      parameter(nval=17)
c
      real dnorm1
      real dnorm2
      real dnorm3
      real dnorm6
      real dval(nval)
      integer i
      integer nz
      real p4
      real p5
      real pdf4
      real pdf5
      real q4
      real q5
      real xnorm1
      real xnorm2
      real xnorm3
      real xnorm4
      real xval1
      real xval2
      real xval3
      real xval4
      real z
c
      write(*,*)' '
      write(*,*)'TEST10'
      write(*,*)
     &  'Demonstrate use of routines to compute the cumulative'
      write(*,*)'Normal distribution function.'
      write(*,*)' '
      write(*,*)'   X        Dnorm1       Dnorm2       Dnorm3'
      write(*,*)'            Dnorm4       Dnorm5       Dnorm6'
      write(*,*)' '
c
      nz=0
c
      do i=0,nval-1
        z=i/4.0
        dval(i+1)=dnorm1(z)
        write(*,'(1x,4g14.6)') z,dnorm1(z),dnorm2(z,1,nz),dnorm3(z,
     &    .false.)
        call dnorm4(z,p4,q4,pdf4)
        call dnorm5(z,p5,q5,pdf5)
        write(*,'(1x,14x,2g14.6)') p4,p5,dnorm6(z)
      enddo

      write(*,*)' '
      write(*,*)'Now try to find X values for given probability:'
      write(*,*)' '
      write(*,*)
     &  '   Dnorm1     Xnorm1      Xnorm2      Xnorm3    Xnorm4'
      do i=0,nval-1
        xval1=xnorm1(dval(i+1))
        xval2=xnorm2(dval(i+1))
        xval3=xnorm3(dval(i+1))
        xval4=xnorm4(dval(i+1))
        write(*,'(1x,5g14.6)') dval(i+1),xval1,xval2,xval3,xval4
      enddo

      return
      end
      subroutine test11
c
c***********************************************************************
c
c  Test Chi-square routines.
c
      integer n
      parameter(n=6)
c
      integer i
      integer ierror
      integer j
      integer nfree
      integer nz
      real pchi1
      real pchi2
      real prob(n)
      real temp1(n)
      real temp2(n)
      real xchi1
      real xchi2
      real xval(n)
c
      prob(1)=0.500
      prob(2)=0.100
      prob(3)=0.050
      prob(4)=0.010
      prob(5)=0.005
      prob(6)=0.001
c
      xval(1)=1.000
      xval(2)=2.000
      xval(3)=5.000
      xval(4)=10.00
      xval(5)=20.00
      xval(6)=30.00
c
      write(*,*)' '
      write(*,*)'TEST11'
      write(*,*)'Test Xchi1, Xchi2, Pchi1 and Pchi2,'
      write(*,*)'chi-square probability routines.'
      write(*,*)' '
      write(*,*)'Given Nfree and probability, find X:'
      write(*,*)' '
      write(*,*)'Nfree down, probabilities across'
      write(*,*)' '
      write(*,'(4x,6f8.3)') (prob(i),i=1,n)
      write(*,*)' '
      ierror=0
      nz=0
      do i=1,30
        nfree=i
        do j=1,n
          temp1(j)=xchi1(prob(j),nfree)
          temp2(j)=xchi2(prob(j),real(nfree),ierror)
        enddo
        write(*,*)' '
        write(*,'(1x,i3,6f8.3)') nfree,(temp1(j),j=1,n)
        write(*,'(1x,3x,6f8.3)') (temp2(j),j=1,n)
      enddo
c
      write(*,*)' '
      write(*,*)'Given Nfree and X, find probability:'
      write(*,*)' '
      write(*,*)'Nfree down, X values across'
      write(*,*)' '
      write(*,'(4x,6f8.3)') (xval(i),i=1,n)
      write(*,*)' '

      do i=1,30
        nfree=i
        do j=1,n
          temp1(j)=pchi1(xval(j),nfree)
          temp2(j)=pchi2(xval(j),nfree,1,nz)
        enddo
        write(*,*)' '
        write(*,'(1x,i3,6f8.3)') nfree,(temp1(j),j=1,n)
        write(*,'(1x,3x,6f8.3)') (temp2(j),j=1,n)
      enddo

      return
      end
      subroutine test12
c
c***********************************************************************
c
c  Test routines for computing LOG(GAMMA(X)).
c
      integer n
      parameter(n=13)
c
      real gamln1
      real gamln2
      real gamln3
      real gamln4
      integer i
      integer ierror
      real gx(n)
      real x(n)
      real xx
c
      ierror=0

      x(1)=0.2
      x(2)=0.4
      x(3)=0.6
      x(4)=0.8
      x(5)=1.0
      x(6)=1.2
      x(7)=1.4
      x(8)=1.6
      x(9)=1.8
      x(10)=2.0
      x(11)=10.0
      x(12)=20.0
      x(13)=30.0
c
      gx(1)=4.590845
      gx(2)=2.218160
      gx(3)=1.489192
      gx(4)=1.164230
      gx(5)=1.000000
      gx(6)=0.918169
      gx(7)=0.887264
      gx(8)=0.893515
      gx(9)=0.931384
      gx(10)=1.000000
      gx(11)=3.6288000e+05
      gx(12)=1.2164510e+17
      gx(13)=8.8417620e+30
c
      write(*,*)' '
      write(*,*)'TEST12'
      write(*,*)'Test routines for computing Log(Gamma(X))'
      write(*,*)' '
c
      write(*,*)'Log of gamma function:'
      write(*,*)' '
      write(*,*)'X, Log(Gamma(X)), Gamln1, Gamln2, Gamln3'
      write(*,*)'                  Gamln4'
      write(*,*)' '
      do i=1,n
        xx=x(i)
        write(*,'(1x,5g14.6)') xx,log(gx(i)),gamln1(xx),gamln2(xx),
     &    gamln3(xx)
        write(*,'(1x,28x,g14.6)') gamln4(xx,ierror)
      enddo

      return
      end
      subroutine test13
c
c***********************************************************************
c
c  Computation of incomplete gamma function.
c
      integer nval
      parameter(nval=20)
c
      real gamic1
      real gamic2
      real gamic3
      real gamic4
      real ginc(nval)
      integer i
      integer ierror
      real p(nval)
      real pp
      real y(nval)
      real yy
c
      data p / 0.1000000,0.1000000,0.1000000,0.5000000,0.5000000,
     &  0.5000000,1.000000,1.000000,1.000000,1.100000,1.100000,1.100000,
     &  2.000000,2.000000,2.000000,6.000000,6.000000,11.00000,26.00000,
     &  41.00000 /
c
      data y / 3.1622779e-02,0.3162278,1.581139,7.0710681e-02,0.7071068,
     &  3.535534,0.1000000,1.000000,5.000000,0.1048809,1.048809,5.244044
     &  ,0.1414214,1.414214,7.071068,2.449490,12.24745,16.58312,25.49510
     &  ,44.82187 /
c
      data ginc / 0.7420263,0.9119753,0.9898955,0.2931279,0.7656418,
     &  0.9921661,9.5162600e-02,0.6321206,0.9932621,7.5747102e-02,
     &  0.6076457,0.9933425,9.1054002e-03,0.4130643,0.9931450,3.8731799e
     &  -02,0.9825937,0.9404267,0.4863866,0.7359709 /
c
      write(*,*)' '
      write(*,*)'TEST13'
      write(*,*)'Test routines for incomplete gamma function.'
      write(*,*)' '
c
      write(*,*)'Y, P, Actual, Gamic1, Gamic2'
      write(*,*)'              Gamic3, Gamic4'
      write(*,*)' '
      ierror=0

      do i=1,nval
        yy=y(i)
        pp=p(i)
        write(*,'(1x,5g14.6)') yy,pp,ginc(i),gamic1(yy,pp,ierror),
     &    gamic2(yy,pp)
        write(*,'(43x,2g14.6)') gamic3(yy,pp,ierror),gamic4(yy,pp,
     &    ierror)
      enddo

      return
      end
      subroutine test14
c
c***********************************************************************
c
c  Student's T distribution
c
      real cnt1
      real ct1
      real d
      integer i
      integer idf
      integer ierror
      integer j
      real t
      real temp1(4)
      real temp2(4)
c
      write(*,*)' '
      write(*,*)'TEST14'
      write(*,*)'Test CT1 and CNT1 for Student''s T distribution.'
      write(*,*)' '
      write(*,*)'   T, idf=1, 2, 3, 4'
      write(*,*)' '
      d=0.0
      ierror=0
      do j=1,15
        t=real(j)/10.0
        do i=1,4
          idf=i
          temp1(i)=ct1(t,idf,ierror)
          temp2(i)=cnt1(t,idf,d,ierror)
        enddo
        write(*,*)' '
        write(*,'(1x,5g14.6)') t,(temp1(i),i=1,4)
        write(*,'(1x,14x,4g14.6)') (temp2(i),i=1,4)
      enddo

      return
      end
      subroutine test15
c
c***********************************************************************
c
c  Computation of Owen's integral.
c
      integer nval
      parameter(nval=20)
c
      real a(nval)
      real aa
      real h(nval)
      real hh
      integer i
      real tha1
      real tha2
c
      data h / 0.1000000,0.1000000,0.1000000,0.5000000,0.5000000,
     &  0.5000000,1.000000,1.000000,1.000000,1.100000,1.100000,1.100000,
     &  2.000000,2.000000,2.000000,6.000000,6.000000,11.00000,26.00000,
     &  41.00000 /
c
      data a / 3.1622779e-02,0.3162278,1.581139,7.0710681e-02,0.7071068,
     &  3.535534,0.1000000,1.000000,5.000000,0.1048809,1.048809,5.244044
     &  ,0.1414214,1.414214,7.071068,2.449490,12.24745,16.58312,25.49510
     &  ,44.82187 /
c
      write(*,*)' '
      write(*,*)'TEST15'
      write(*,*)'Test routines for Owen''s integral.'
      write(*,*)' '
c
      write(*,*)'H, A, Tha1(h,a), Tha2(h,a)'
      write(*,*)' '
      do i=1,nval
        hh=h(i)
        aa=a(i)
        write(*,'(1x,5g14.6)') hh,aa,tha1(hh,aa),tha2(hh,1.0,aa,1.0)
      enddo

      return
      end
      subroutine test16
c
c***********************************************************************
c
      integer maxn
      parameter(maxn=21)
c
      integer i
      integer ibino1
      integer ibino2
      integer igeo1
      integer iseed1
      integer iseed2
      integer iseed3
      integer istart
      integer itemp
      integer ival1(maxn)
      integer ival2(maxn)
      integer jmax
      integer n
      integer ntest
      real p
c
      p=0.50
      iseed1=istart()
      iseed2=istart()
      iseed3=istart()
      ntest=1000
      n=maxn-1
c
      write(*,*)' '
      write(*,*)'TEST16'
      write(*,*)'Test two routines for binomial distributions.'
      write(*,*)' '
      write(*,*)'Set the probability of a head to ',p
      do i=1,maxn
        ival1(i)=0
        ival2(i)=0
      enddo

      do i=1,ntest
        itemp=ibino1(p,n,iseed1)
        ival1(itemp+1)=ival1(itemp+1)+1
        itemp=ibino2(p,n,iseed3)
        ival2(itemp+1)=ival2(itemp+1)+1
      enddo

      write(*,*)' '
      write(*,*)'Results from Ibino1 and Ibino2:'
      write(*,*)'Number of heads in ',n,' tosses'
      write(*,*)'carried out ',ntest,' times.'
      write(*,*)' '
      do i=1,maxn
        write(*,*)i-1,ival1(i),ival2(i)
      enddo
c
      jmax=1
      do i=1,maxn
        ival1(i)=0
      enddo

      do i=1,ntest
        itemp=igeo1(p,iseed2)
        if(itemp+1.le.20) then
          jmax=max(jmax,itemp+1)
          ival1(itemp+1)=ival1(itemp+1)+1
        else
          write(*,*)'TEST16 - Anomalous output value!'
        endif
      enddo

      write(*,*)' '
      write(*,*)'Results from Igeo1'
      write(*,*)'Number of heads before first tail'
      write(*,*)'Results for ',ntest,' trials:'
      write(*,*)' '
      do i=1,jmax
        write(*,*)i-1,ival1(i)
      enddo

      return
      end
      subroutine test17
c
c***********************************************************************
c
      integer maxn
      parameter(maxn=10)
c
      integer i
      integer n
      real twonrm
      real x(maxn)
      real xnrm
c
      write(*,*)' '
      write(*,*)'TEST17'
      write(*,*)'Test UniSph'
      write(*,*)'Compute some points on an N sphere.'
      write(*,*)' '
      n=3
      call unisph(n,x)
      xnrm=twonrm(n,x)
      write(*,*)'N=',n
      write(*,*)(x(i),i=1,3)
      write(*,*)'Radius=',xnrm
c
      n=10
      call unisph(n,x)
      xnrm=twonrm(n,x)
      write(*,*)' '
      write(*,*)'N=',n
      write(*,*)'Radius=',xnrm
      return
      end
      function twonrm(n,x)
c
c***********************************************************************
c
      integer n
c
      integer i
      real twonrm
      real x(n)
c
      twonrm=0.0
      do i=1,n
        twonrm=twonrm+x(i)*x(i)
      enddo

      twonrm=sqrt(twonrm)

      return
      end
      subroutine second(time)
c
c***********************************************************************
c
      real tarray(2)
      real time
c
      call etime(tarray)
      time=tarray(1)+tarray(2)
      return
      end
      function istart()
c
c***********************************************************************
c
      integer istart
c
      istart=1234567

      return
      end
