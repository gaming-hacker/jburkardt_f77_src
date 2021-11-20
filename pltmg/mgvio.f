c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine vutl(ncolor,red,green,blue,isock,fname)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          vioctr,viodtr,vioutl
            double precision
     +          red(*),green(*),blue(*)
            character*4
     +          type
            character*8
     +          keywrd
            character*80
     +          host,host0,fname,fname0
            common /gph0/id,keywrd
c
        if(ncolor.le.0) then
            call viostr(id,'putl',4)
            call vioint(id,ncolor,1)
            ii=vioutl(id,'c')
            ii=viodtr(id)
            call viostp()
            return
        else
c
            keywrd='bhdouble'
c
            call viosta()
            if(isock.lt.0) then
                call fstr(fname0,lenf,fname,0)
                type='FILE'
                host='localhost'
            else
                call sint(fname0,lenf,isock)
                type='INET'
                host=fname
            endif
            call fstr(host0,lenh,host,0)
            id=vioctr(type,'XDR',host0,lenh,fname0,lenf,'w')
            ii=vioutl(id,'o')
            call viostr(id,keywrd,8)
            call viostr(id,'putl',4)
            call vioint(id,ncolor,1)
            if(keywrd(3:3).eq.'d') then
                call viodbl(id,red,ncolor)
                call viodbl(id,green,ncolor)
                call viodbl(id,blue,ncolor)
            else
                call vioflt(id,red,ncolor)
                call vioflt(id,green,ncolor)
                call vioflt(id,blue,ncolor)
            endif
        endif
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine vframe(iframe)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            character*8
     +          keywrd
            common /gph0/id,keywrd
c
        call viostr(id,'list',4)
        call vioint(id,iframe,1)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine vline(x,y,z,n,icolor)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            double precision
     +          x(n),y(n),z(n)
            character*8
     +          keywrd
            common /gph0/id,keywrd
c
        call viostr(id,'line',4)
        call vioint(id,icolor,1)
        call vioint(id,n,1)
        if(keywrd(3:3).eq.'d') then
            call viodbl(id,x,n)
            call viodbl(id,y,n)
            call viodbl(id,z,n)
        else
            call vioflt(id,x,n)
            call vioflt(id,y,n)
            call vioflt(id,z,n)
        endif
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine vfill(x,y,z,n,icolor)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            double precision
     +          x(n),y(n),z(n)
            character*8
     +          keywrd
            common /gph0/id,keywrd
c
        call viostr(id,'fill',4)
        call vioint(id,icolor,1)
        call vioint(id,n,1)
        if(keywrd(3:3).eq.'d') then
            call viodbl(id,x,n)
            call viodbl(id,y,n)
            call viodbl(id,z,n)
        else
            call vioflt(id,x,n)
            call vioflt(id,y,n)
            call vioflt(id,z,n)
        endif
        return
        end

