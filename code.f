* Case for non-uniform grids shape factor calculation

        call location
        call cordinate
        call self_vf
      stop
      end
*===================================*
      subroutine location
      include 'input.in'
        dx1=
        dx2=
        dx3=
        dy1=
        dy2=
        dy3=

***** Four edge co-ordinates of chips
        cx1=x4
        cy1=y1+y2
        cx2=cx1
        cy2=cy1+y6

        cx3=cx1
        cy3=cy2+y7
        cx4=cx1
        cy4=cy3+y6

        cx5=cx1
        cy5=cy4+y7
        cx6=cx1
        cy6=cy5+y6

        cx7=cx1
        cy7=cy6+y7
        cx8=cx1
        cy8=cy7+y6

***** Total number of corner points 20; noc=20
***** M0,
***** 20 th point is not a corner point. last one 
***** Only for two chip case
        M0=anint(y1/dy1)+1
        noc(1)=M0+anint(y2/dy2)
        noc(2)=noc(1)+anint(x4/dx1)
        noc(3)=noc(2)+anint(y6/dy2)
        noc(4)=noc(3)+anint(x4/dx1)

        noc(5)=noc(4)+anint(y7/dy2)
        noc(6)=noc(5)+anint(x4/dx1)
        noc(7)=noc(6)+anint(y6/dy2)
        noc(8)=noc(7)+anint(x4/dx1)

        noc(9)=noc(8)+anint(y7/dy2)
        noc(10)=noc(9)+anint(x4/dx1)
        noc(11)=noc(10)+anint(y6/dy2)
        noc(12)=noc(11)+anint(x4/dx1)

        noc(13)=noc(12)+anint(y7/dy2)
        noc(14)=noc(13)+anint(x4/dx1)
        noc(15)=noc(14)+anint(y6/dy2)
        noc(16)=noc(15)+anint(x4/dx1)

        M1=noc(16)+anint(y4/dy2)

        noc(17)=M1+anint(y5/dy3)
        M2=noc(17)+anint(x1/dx1)
        M3=M2+anint(x2/dx2)
        noc(18)=M3+anint(x3/dx3)
        M4=noc(18)+anint(y5/dy3)
        M5=M4+anint((y4+y3+y2)/dy2)
        noc(19)=M5+anint(y1/dy1)
        M6=noc(19)+anint(x3/dx3)
        M7=M6+anint(x2/dx2)
        noc(20)=M7-1+anint(x1/dx1)

      return
      end
*===================================*
      subroutine cordinate
      include 'input.in'
* Surface1
        do i=1,noc(1)-1
          if (i.eq.1)then
            ds(i)=(dx1+dy1)/2.0
            fy(i)=dy1/2.0
            bx(i)=dx1/2.0
            by(i)=0.0
          elseif ((i.gt.1).and.(i.lt.M0))then
            ds(i)=dy1
            fy(i)=fy(i-1)+dy1
          elseif (i.eq.M0)then
            ds(i)=(dy1+dy2)/2.0
            fy(i)=fy(i-1)+(dy1+dy2)/2.0
          elseif (i.gt.M0)then
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=0.0
        enddo
* Surface2
        do i=noc(1),noc(2)-1
          if (i.eq.noc(1))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=dx1/2.0
          elseif (i.gt.noc(1))then
            ds(i)=dx1
            fx(i)=fx(i-1)+dx1
          endif
          fy(i)=cy1
        enddo
* Surface3
        do i=noc(2),noc(3)-1
          if (i.eq.noc(2))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          elseif (i.gt.noc(2))then
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=x4
        enddo
* Surface4
        do i=noc(3),noc(4)-1
          if (i.eq.noc(3))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=fx(i-1)-dx1/2.0
          elseif (i.gt.noc(3))then
            ds(i)=dx1
            fx(i)=fx(i-1)-dx1
          endif
          fy(i)=cy2
        enddo
* Surface5
        do i=noc(4),noc(5)-1
          if (i.eq.noc(4))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          elseif (i.gt.noc(4))then
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=0.0
        enddo
* Surface6
        do i=noc(5),noc(6)-1
          if (i.eq.noc(5))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=fx(i-1)+dx1/2.0
          elseif (i.gt.noc(5))then
            ds(i)=dx1
            fx(i)=fx(i-1)+dx1
          endif
          fy(i)=cy3
        enddo
* Surface7
        do i=noc(6),noc(7)-1
          if (i.eq.noc(6))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          elseif (i.gt.noc(6))then 
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=x4
        enddo
* Surface8
        do i=noc(7),noc(8)-1
          if (i.eq.noc(7))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=fx(i-1)-dx1/2.0
          elseif (i.gt.noc(7))then
            ds(i)=dx1
            fx(i)=fx(i-1)-dx1
          endif
          fy(i)=cy4
        enddo
* Surface9
        do i=noc(8),noc(9)-1
          if (i.eq.noc(8))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          elseif (i.gt.noc(8))then
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=0.0
        enddo
* Surface10
        do i=noc(9),noc(10)-1
          if (i.eq.noc(9))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=fx(i-1)+dx1/2.0
          elseif (i.gt.noc(9))then
            ds(i)=dx1
            fx(i)=fx(i-1)+dx1
          endif
          fy(i)=cy5
        enddo
* Surface11
        do i=noc(10),noc(11)-1
          if (i.eq.noc(10))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          elseif (i.gt.noc(10))then
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=x4
        enddo
* Surface12
        do i=noc(11),noc(12)-1
          if (i.eq.noc(11))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=fx(i-1)-dx1/2.0
          elseif (i.gt.noc(11))then
            ds(i)=dx1
            fx(i)=fx(i-1)-dx1
          endif
          fy(i)=cy6
        enddo
* Surface13
        do i=noc(12),noc(13)-1
          if (i.eq.noc(12))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          elseif (i.gt.noc(12))then
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=0.0
        enddo
* Surface14
        do i=noc(13),noc(14)-1
          if (i.eq.noc(13))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=fx(i-1)+dx1/2.0
          elseif (i.gt.noc(13))then
            ds(i)=dx1
            fx(i)=fx(i-1)+dx1
          endif
          fy(i)=cy7
        enddo
* Surface15
        do i=noc(14),noc(15)-1
          if (i.eq.noc(14))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          elseif (i.gt.noc(14))then
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=x4
        enddo
* Surface16
        do i=noc(15),noc(16)-1
          if (i.eq.noc(15))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=fx(i-1)-dx1/2.0
          elseif (i.gt.noc(15))then
            ds(i)=dx1
            fx(i)=fx(i-1)-dx1
          endif
          fy(i)=cy8
        enddo
* Surface17
        do i=noc(16),noc(17)-1
          if (i.eq.noc(16))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          elseif ((i.gt.noc(16)).and.(i.lt.M3))then
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          elseif (i.eq.M3)then
            ds(i)=(dy2+dy3)/2.0
            fy(i)=fy(i-1)+(dy2+dy3)/2.0
          elseif (i.gt.M3)then
            ds(i)=dy3
            fy(i)=fy(i-1)+dy3
          endif
          fx(i)=0.0
        enddo
* Surface18
        do i=noc(17),noc(18)-1
          if (i.eq.noc(17))then
            ds(i)=(dx1+dy3)/2.0
            fx(i)=dx1/2.0
          elseif ((i.gt.noc(17)).and.(i.lt.M2))then
            ds(i)=dx1
            fx(i)=fx(i-1)+dx1
          elseif (i.eq.M2)then
            ds(i)=(dx1+dx2)/2.0
            fx(i)=fx(i-1)+(dx1+dx2)/2.0
          elseif ((i.gt.M2).and.(i.lt.M3))then
            ds(i)=dx2
            fx(i)=fx(i-1)+dx2
          elseif (i.eq.M3)then
            ds(i)=(dx2+dx3)/2.0
            fx(i)=fx(i-1)+(dx2+dx3)/2.0
          elseif (i.gt.M3)then
            ds(i)=dx3
            fx(i)=fx(i-1)+dx3
          endif
          fy(i)=y1+y2+y3+y4+y5
        enddo
* Surface19
        do i=noc(18),noc(19)-1
          if (i.eq.noc(18))then
            ds(i)=(dx3+dy3)/2.0
            fy(i)=fy(i-1)-dy3/2.0
          elseif ((i.gt.noc(18)).and.(i.lt.M4))then
            ds(i)=dy3
            fy(i)=fy(i-1)-dy3
          elseif (i.eq.M4)then
            ds(i)=(dy3+dy2)/2.0
            fy(i)=fy(i-1)-(dy3+dy2)/2.0
          elseif ((i.gt.M4).and.(i.lt.M5))then
            ds(i)=dy3
            fy(i)=fy(i-1)-dy2
          elseif (i.eq.M5)then
            ds(i)=(dy2+dy1)/2.0
            fy(i)=fy(i-1)-(dy2+dy1)/2.0
          elseif (i.gt.M5)then
            ds(i)=dy1
            fy(i)=fy(i-1)-dy1
          endif
          fx(i)=x1+x2+x3
        enddo
* Surface20
        do i=noc(19),noc(20)
          if (i.eq.noc(19))then
            ds(i)=(dx3+dy3)/2.0
            fx(i)=fx(i-1)-dx3/2.0
          elseif ((i.gt.noc(19)).and.(i.lt.M6))then
            ds(i)=dx3
            fx(i)=fx(i-1)-dx3
          elseif (i.eq.M6)then
            ds(i)=(dx3+dx2)/2.0
            fx(i)=fx(i-1)-(dx3+dx2)/2.0
          elseif ((i.gt.M6).and.(i.lt.M7))then
            ds(i)=dx2
            fx(i)=fx(i-1)-dx2
          elseif (i.eq.M7)then
            ds(i)=(dx2+dx1)/2.0
            fx(i)=fx(i-1)-(dx2+dx1)/2.0
          elseif (i.gt.M7)then
            ds(i)=dx1
            fx(i)=fx(i-1)-dx1
          endif
          fy(i)=0.0
        enddo
      return
      end
