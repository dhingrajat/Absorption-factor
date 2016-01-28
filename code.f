* Case for non-uniform grids shape factor calculation

        call location
        call cordinate
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
        x1=xcp
        y1=ycp1
        x2=xcp
        y2=ycp2

        x3=xcp
        y3=ycp3
        x4=xcp
        y4=ycp3+ycp2-ycp1

        x5=xcp
        y5=2.d0*ycp3-ycp1
        x6=xcp
        y6=2.d0*ycp3+ycp2-2.d0*ycp1

        x7=xcp
        y7=3.d0*ycp3-2.d0*ycp1
        x8=xcp
        y8=3.d0*ycp3+ycp2-3.d0*ycp1

***** Total number of corner points 20; noc=20
***** M0, 
***** 20 th point is not a corner point. last one 
***** Only for two chip case
        M0=anint(y1/dy1)+1
        noc(1)=M0+anint(ycp1-y1/dy2)
        noc(2)=noc(1)+anint(xcp/dx1)
        noc(3)=noc(2)+anint((ycp2-ycp1)/dy2)
        noc(4)=noc(3)+anint(xcp/dx1)

        noc(5)=noc(4)+anint((ycp3-ycp2)/dy2)
        noc(6)=noc(5)+anint(xcp/dx1)
        noc(7)=noc(6)+anint((ycp2-ycp1)/dy2)
        noc(8)=noc(7)+anint(xcp/dx1)

        noc(9)=noc(8)+anint((ycp3-ycp2)/dy2)
        noc(10)=noc(9)+anint(xcp/dx1)
        noc(11)=noc(10)+anint((ycp2-ycp1)/dy2)
        noc(12)=noc(11)+anint(xcp/dx1)

        noc(13)=noc(12)+anint((ycp3-ycp2)/dy2)
        noc(14)=noc(13)+anint(xcp/dx1)
        noc(15)=noc(14)+anint((ycp2-ycp1)/dy2)
        noc(16)=noc(15)+anint(xcp/dx1)
	  
        M1=noc(16)+anint(y2/dy2)

        noc(17)=M1+anint(y3/dy3)
        M2=noc(17)+anint(x1/dx1)
        M3=M2+anint(x2/dx2)
        noc(18)=M3+anint(x3/dx3)
        M4=noc(18)+anint(y3/dy3)
        M5=M4+anint(y4/dy2)
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
          else
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
          else
            ds(i)=dx1
            fx(i)=fx(i-1)+dx1
          endif
          fy(i)=ycp1
        enddo
* Surface3
        do i=noc(2),noc(3)-1
          if (i.eq.noc(2))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          else
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=xcp
        enddo
* Surface4
        do i=noc(3),noc(4)-1
          if (i.eq.noc(3))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=fx(i-1)-dx1/2.0
          else
            ds(i)=dx1
            fx(i)=fx(i-1)-dx1
          endif
          fy(i)=ycp2
        enddo
* Surface5
        do i=noc(4),noc(5)-1
          if (i.eq.noc(4))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          else
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
          else
            ds(i)=dx1
            fx(i)=fx(i-1)+dx1
          endif
          fy(i)=ycp3
        enddo
* Surface7
        do i=noc(6),noc(7)-1
          if (i.eq.noc(6))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          else
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=xcp
        enddo
* Surface8
        do i=noc(7),noc(8)-1
          if (i.eq.noc(7))then
            ds(i)=(dx1+dy2)/2.0
            fx(i)=fx(i-1)-dx1/2.0
          else
            ds(i)=dx1
            fx(i)=fx(i-1)-dx1
          endif
          fy(i)=ycp3+ycp2-ycp1
        enddo
* Surface9
        do i=noc(8),noc(9)-1
          if (i.eq.noc(8))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          else
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
          else
            ds(i)=dx1
            fx(i)=fx(i-1)+dx1
          endif
          fy(i)=2ycp3-ycp1
        enddo
* Surface11
        do i=noc(10),noc(11)-1
          if (i.eq.noc(10))then
            ds(i)=(dx1+dy2)/2.0
            fy(i)=fy(i-1)+dy2/2.0
          else
            ds(i)=dy2
            fy(i)=fy(i-1)+dy2
          endif
          fx(i)=xcp
        enddo
* Surface12
* Surface13
* Surface14
* Surface15
      return
      end

