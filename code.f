* Case for non-uniform grids shape factor calculation

        call location
        call cordinate
        call self_vf
* Complete line w/o first and last point on it
        call surface1
        call surface2
        call surface3
        call surface4
        call surface5
        call surface6
        call surface7
        call surface8
        call surface9
        call surface10
        call surface11
        call surface12
        call surface13
        call surface14
        call surface15
        call surface16
        call surface17
        call surface18
        call surface19
        call surface20

        call corner1
        call corner2
        call corner3
        call corner4
        call corner5
        call corner6
        call corner7
        call corner8
        call corner9
        call corner10
        call corner11
        call corner12
        call corner13
        call corner14
        call corner15
        call corner16
        call corner17
        call corner18
        call corner19
        call corner20

        call output
        call ab_factor
      stop
      end
*===================================*
      subroutine location
      include 'input.in'
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

* Total number of corner points 20; noc=20
* M0,
* 20 th point is not a corner point. last one
* Only for two chip case
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
* Surface1-to-20
        do i=2,noc(20)
          bx(i)=fx(i-1)
          by(i)=fy(i-1)
        enddo
      return
      end
*===================================*
      subroutine self_vf
      include 'input.in'
        do i=1,noc(20)
        do j=1,noc(20)
* self pending for corner
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*===================================*
      subroutine surface1
      include 'input.in'
* Surface 1 with corner point1
        do i=2,noc(1)-1
           j=1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 1 with surface 2
        do i=2,noc(1)-1
        do j=noc(1),noc(2)-1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 1 with corner noc(2)
        do i=2,noc(1)-1
           j=noc(2)
          cs1=sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 1 with surface 3-to-17 and 18-(partly)
        do i=2,noc(1)-1
        do j=noc(2)+1,noc(17)+noc(2)-noc(1)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 1 with noc(17)+noc(2)-noc(1)+1 to noc(18)-1
        do i=2,noc(1)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(18)-1
          cond=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy1-cx1*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
              uc1=uc1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
                cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
                cs2=cs2+sqrt((cy1-by(j))**2+(cx1-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 1 with surface 19
        do i=2,noc(1)-1
        do j=noc(18),noc(19)-1
          cond=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy1-cx1*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
              uc1=uc1+sqrt((cy1-fy(i))**2+(fx(i)-cx1)**2)
              cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
                cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0) then
                cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
                cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 1 with surface 20
        do i=2,noc(1)-1
        do j=noc(19),noc(20)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
      return
      end
*===================================*
      subroutine surface2
      include 'input.in'
* Surface 2 with Surface 1 includes both corners
        do i=noc(1)+1,noc(2)-1
        do j=1,noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 2 with surface 3-18 and 19 partly
        do i=noc(1)+1,noc(2)-1
        do j=noc(2),noc(19)-noc(1)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 2 with surface 19 point number noc(19)-noc(1)+1
        do i=noc(1)+1,noc(2)-1
           j=noc(19)-noc(1)+1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+ds(i))/ds(i)
        enddo
* Surface 2 with surface 19 till 20 from noc(19)-noc(1)+2
        do i=noc(1)+1,noc(2)-1
        do j=noc(19)-noc(1)+2,noc(20)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface3
      include 'input.in'
* Surface 3 with surface 1-2
        do i=noc(2)+1,noc(3)-1
        do j=1,noc(2)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 3 with surface 4-17 and 18 partly
        do i=noc(2)+1,noc(3)-1
        do j=noc(3),noc(17)+noc(2)-noc(1)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 3 with surface 18 point number noc(17)+noc(2)-noc(1)
        do i=noc(2)+1,noc(3)-1
           j=noc(17)+noc(2)-noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+ds(i))/ds(i)
        enddo
* Surface 3 with surface 18 from noc(17)+noc(2)-noc(1)+1 to surface 20 
        do i=noc(2)+1,noc(3)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(20)-noc(2)+noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 3 with point number noc(20)-noc(2)+noc(1)+1
        do i=noc(2)+1,noc(3)-1
           j=noc(20)-noc(2)+noc(1)+1
          cs1=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+ds(i))/ds(i)
        enddo
* Surface 3 with corner 20 from noc(20)-noc(2)+noc(1)+2
        do i=noc(2)+1,noc(3)-1
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface4
      include 'input.in'
* Surface 4 with surface 1-3
        do i=noc(3)+1,noc(4)-1
        do j=1,noc(3)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 4 with corner noc(4) to Surface 6
        do i=noc(3)+1,noc(4)-1
        do j=noc(4),noc(6)-1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 4 with corner noc(6)
        do i=noc(3)+1,noc(4)-1
           j=noc(6)
          cs1=sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 4 with surface 7-18 till point noc(17)+noc(2)-noc(1)-1
        do i=noc(3)+1,noc(4)-1
        do j=noc(6)+1,noc(17)+noc(2)-noc(1)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 4 with point number noc(17)+noc(2)-noc(1) on surface 18
        do i=noc(3)+1,noc(4)-1
           j=noc(17)+noc(2)-noc(1)
          cond=cy3-cx3*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.lt.0.0)then
              cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
              cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
*co---Strsight line with obstruction for same chip width case
*     till point cx3,cy3 because cancelling of two sides
            endif
            uc1=sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
            cs2=sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 4 with from noc(17)+noc(2)-noc(1)+1 to noc(19)-noc(1)-noc(3)+noc(2)
        do i=noc(3)+1,noc(4)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(19)-noc(1)-noc(3)+noc(2)
          cond=cy3-cx3*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy3-cx3*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
              uc1=uc1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
              cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
                cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy3-cx3*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
                cs2=cs2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 4 with point j=noc(19)-noc(1)-noc(3)+noc(2)+1
        do i=noc(3)+1,noc(4)-1
           j=noc(19)-noc(1)-noc(3)+noc(2)+1
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs2-uc1+ds(i))/ds(i)
        enddo
* Surface 4 from point (19)-noc(1)-noc(3)+noc(2)+2 till noc(20)
        do i=noc(3)+1,noc(4)-1
        do j=noc(19)-noc(1)-noc(3)+noc(2)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface5
      include 'input.in'
* Surface 5 with surface 1-2-3
        do i=noc(4)+1,noc(5)-1
        do j=1,noc(3)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 5 from corner noc(3)
        do i=noc(4)+1,noc(5)-1
           j=noc(3)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
          uc1=sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 5 with surface 4
        do i=noc(4)+1,noc(5)-1
        do j=noc(3)+1,noc(4)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
        enddo
* Surface 5 from corner noc(4)
        do i=noc(4)+1,noc(5)-1
           j=noc(4)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs2-uc1+ds(i))/ds(i)
        enddo
* Surface 5 with surface 6
        do i=noc(4)+1,noc(5)-1
        do j=noc(5),noc(6)-1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 5 with corner noc(6)
        do i=noc(4)+1,noc(5)-1
           j=noc(6)
          cs1=sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 5 with surface 7 to 18 till noc(17)+noc(2)-noc(1)
        do i=noc(4)+1,noc(5)-1
        do j=noc(6)+1,noc(17)+noc(2)-noc(1)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 5 with surface 18 and partly 19=noc(19)+noc(2)+noc(4)-noc(5)-noc(3)-noc(1)+1
        do i=noc(4)+1,noc(5)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(19)-noc(1)-(noc(3)-noc(2))-
     &  (noc(5)-noc(4))+1
          cond=cy3-cx3*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy3-cx3*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy3)**2+(fx(i)-cx3)**2)
              uc1=uc1+sqrt((cy3-by(j))**2+(cx3-bx(j))**2)
              cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
                cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy3-cx3*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
                cs2=cs2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 5 with Surface 19 parallel points
        do i=noc(4)+1,noc(5)-1
        do j=noc(19)-noc(1)-(noc(3)-noc(2))-(noc(5)-noc(4))+2,
     &  noc(19)-noc(1)-(noc(3)-noc(2))
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo 
* Surface 5 with surface 19-20 till noc(20)-noc(2)+noc(1)+1
        do i=noc(4)+1,noc(5)-1
        do j=noc(19)-noc(1)-(noc(3)-noc(2))+1,noc(20)-(noc(2)-noc(1))+1
          cond=cy2-cx2*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy2-cx2*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
              uc2=uc2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
              cond2=cy2-cx2*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
                cs1=cs1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
                cs2=cs2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 5 to surface 20 from noc(20)-noc(2)+noc(1)+2
        do i=noc(4)+1,noc(5)-1
        do j=noc(20)-(noc(2)-noc(1))+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface6
      include 'input.in'
* Surface 6 with surface 1-3
        do i=noc(5)+1,noc(6)-1
        do j=1,noc(3)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 6 with corner noc(3)
        do i=noc(5)+1,noc(6)-1
           j=noc(3)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
          uc1=sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 6 with surface 4-5
        do i=noc(5)+1,noc(6)-1
        do j=noc(3)+1,noc(5)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 6 with surface 7-18 and partly 19
        do i=noc(5)+1,noc(6)-1
        do j=noc(6),noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 6 with point number noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+1 on surface 19
        do i=noc(5)+1,noc(6)-1
           j=noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1-uc2+ds(i))/ds(i)
        enddo
* Surface 6 from noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+2 to partial 20
        do i=noc(5)+1,noc(6)-1
        do j=noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+2,noc(20)-
     &  noc(2)+noc(1)
          cond=cy2-cx2*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy2-cx2*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
              uc2=uc2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
              cond2=cy2-cx2*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
                cs1=cs1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
                cs2=cs2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 6 with point noc(20)-noc(2)+noc(1)+1
        do i=noc(5)+1,noc(6)-1
           j=noc(20)-noc(2)+noc(1)+1
          cond=cy2-cx2*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
            cond1=cond1+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            else
              cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
              cs2=cs2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
            endif
          uc2=sqrt((by(i)-cy2)**2+(bx(i)-cx2)**2)
          cs1=sqrt((fy(i)-cy2)**2+(fx(i)-cx2)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 6 till noc(12) from noc(20)-noc(2)+noc(1)+2 till noc(20)
        do i=noc(5)+1,noc(6)-1
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface7
      include 'input.in'
* Surface 7 with surface 1-6
        do i=noc(6)+1,noc(7)-1
        do j=1,noc(6)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 7 with surface 8-17 and 18 till some point
        do i=noc(6)+1,noc(7)-1
        do j=noc(7),noc(17)+noc(2)-noc(1)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 7 with surface 18 point number noc(17)+noc(2)-noc(1)
        do i=noc(6)+1,noc(7)-1
           j=noc(17)+noc(2)-noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+ds(i))/ds(i)
        enddo
* Surface 7 with surface 18 half to Surface 20 half
        do i=noc(6)+1,noc(7)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(20)-noc(2)+noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 7 with point number noc(20)-noc(2)+noc(1)+1
        do i=noc(6)+1,noc(7)-1
           j=noc(20)-noc(2)+noc(1)+1
          cs1=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+ds(i))/ds(i)
        enddo
* Surface 7 with surface 20 from noc(20)-noc(2)+noc(1)+2
        do i=noc(6)+1,noc(7)-1
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface8
      include 'input.in'
* Surface 8 with surface 1-7
        do i=noc(7)+1,noc(8)-1
        do j=1,noc(7)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 8 with corner noc(8) to Surface 6
        do i=noc(7)+1,noc(8)-1
        do j=noc(8),noc(10)-1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 8 with corner noc(10)
        do i=noc(7)+1,noc(8)-1
           j=noc(10)
          cs1=sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 8 with surface 11-18 till point noc(17)+noc(2)-noc(1)-1
        do i=noc(7)+1,noc(8)-1
        do j=noc(10)+1,noc(17)+noc(2)-noc(1)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 8 with point number noc(17)+noc(2)-noc(1) on surface 18
        do i=noc(7)+1,noc(8)-1
           j=noc(17)+noc(2)-noc(1)
          cond=cy5-cx5*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond.le.0.0)then
              v_f(i,j)=0.0
            else
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
                cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
*co---Strsight line with obstruction for same chip width case
*     till point cx5,cy5 because cancelling of two sides
              endif
            uc1=sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
            cs2=sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 8 with from noc(17)+noc(2)-noc(1)+1 to partially Surface 19
        do i=noc(7)+1,noc(8)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(19)-noc(5)-noc(3)-noc(1)+
     &   noc(4)+noc(2)+noc(6)-noc(7)
          cond=cy5-cx5*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy5-cx5*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
              uc1=uc1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
              cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
                cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy5-cx5*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
                cs2=cs2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 8 with point j=noc(19)-noc(5)-noc(3)-noc(1)+noc(4)+noc(2)+noc(6)-noc(7)
        do i=noc(7)+1,noc(8)-1
           j=noc(19)-noc(5)-noc(3)-noc(1)+noc(4)+noc(2)+noc(6)-noc(7)+1
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs2-uc1+ds(i))/ds(i)
        enddo
* Surface 8 from point (19)-noc(1)-noc(3)+noc(2)+2 till noc(20)
        do i=noc(7)+1,noc(8)-1
        do j=noc(19)-noc(5)-noc(3)-noc(1)+noc(4)+noc(2)+noc(6)
     &    -noc(7)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface9
      include 'input.in'
* Surface 9 with surface 1-to-7
        do i=noc(8)+1,noc(9)-1
        do j=1,noc(7)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 9 from corner noc(7)
        do i=noc(8)+1,noc(9)-1
           j=noc(7)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
          uc1=sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 9 from surface 8
        do i=noc(8)+1,noc(9)-1
        do j=noc(7)+1,noc(8)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
        enddo
* Surface 9 with corner noc(8)
        do i=noc(8)+1,noc(9)-1
           j=noc(8)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs2-uc1+ds(i))/ds(i)
        enddo
* Surface 9 with surface 10
        do i=noc(8)+1,noc(9)-1
        do j=noc(9),noc(10)-1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 9 with corner noc(10)
        do i=noc(8)+1,noc(9)-1
           j=noc(10)
          cs1=sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 9 with surface 11 to 18 till noc(17)+noc(2)-noc(1)
        do i=noc(8)+1,noc(9)-1
        do j=noc(9)+1,noc(17)+noc(2)-noc(1)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 9 with surface 18 and partly 19=noc(19)-noc(1)-2*(noc(3)-noc(2))-2*(noc(5)-noc(4))+1
        do i=noc(8)+1,noc(9)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(19)-noc(1)-2*(noc(3)-noc(2))-
     &  2*(noc(5)-noc(4))+1
          cond=cy5-cx5*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy5-cx5*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
              uc1=uc1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
              cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
                cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy5-cx5*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
                cs2=cs2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 9 with Surface 19 parallel points
        do i=noc(8)+1,noc(9)-1
        do j=noc(19)-noc(1)-2*(noc(3)-noc(2))-2*(noc(5)-noc(4))+2,
     &  noc(19)-noc(1)-2*(noc(3)-noc(2))-(noc(5)-noc(4))
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo 
* Surface 9 with surface 19-20 till noc(20)-noc(2)+noc(1)+1
        do i=noc(8)+1,noc(9)-1
        do j=noc(19)-noc(1)-2*(noc(3)-noc(2))-(noc(5)-noc(4))+1,
     &  noc(20)-(noc(2)-noc(1))+1
          cond=cy4-cx4*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy4-cx4*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
              uc2=uc2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
              cond2=cy4-cx4*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
                cs1=cs1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
                cs2=cs2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 9 to surface 20 from noc(20)-(noc(2)-noc(1))+2
        do i=noc(8)+1,noc(9)-1
        do j=noc(20)-(noc(2)-noc(1))+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface10
      include 'input.in'
* Surface 10 with surface 1-7
        do i=noc(9)+1,noc(10)-1
        do j=1,noc(7)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 10 with corner noc(7)
        do i=noc(9)+1,noc(10)-1
           j=noc(7)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
          uc1=sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 10 with surface 8-9
        do i=noc(9)+1,noc(10)-1
        do j=noc(7)+1,noc(9)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 10 with surface 11-18 and partly 19
        do i=noc(9)+1,noc(10)-1
        do j=noc(10),noc(19)-noc(1)-2*(noc(3)-noc(2))-2*(noc(5)-noc(4))
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 10 with point noc(19)-noc(1)-2*(noc(3)-noc(2))-2*(noc(5)-noc(4))+1
        do i=noc(9)+1,noc(10)-1
           j=noc(19)-noc(1)-2*(noc(3)-noc(2))-2*(noc(5)-noc(4))+1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1-uc2+ds(i))/ds(i)
        enddo
* Surface 10 from noc(19)-noc(1)-2*(noc(3)-noc(2))-2*(noc(5)-noc(4))+2 to partial 20
        do i=noc(9)+1,noc(10)-1
        do j=noc(19)-noc(1)-2*(noc(3)-noc(2))-2*(noc(5)-noc(4))+2,
     &  noc(20)-noc(2)+noc(1)
          cond=cy4-cx4*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy4-cx4*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
              uc2=uc2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
              cond2=cy4-cx4*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
                cs1=cs1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
                cs2=cs2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 10 with point noc(20)-noc(2)+noc(1)+1
        do i=noc(9)+1,noc(10)-1
           j=noc(20)-noc(2)+noc(1)+1
          cond=cy4-cx4*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
            cond1=cond1+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            else
              cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
              cs2=cs2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
            endif
          uc2=sqrt((by(i)-cy4)**2+(bx(i)-cx4)**2)
          cs1=sqrt((fy(i)-cy4)**2+(fx(i)-cx4)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 10 till noc(12) from noc(20)-noc(2)+noc(1)+2 till noc(20)
        do i=noc(9)+1,noc(10)-1
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface11
      include 'input.in'
* Surface 11 with surface 1-10
        do i=noc(10)+1,noc(11)-1
        do j=1,noc(10)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 11 with surface 12-17 and 18 till some point
        do i=noc(10)+1,noc(11)-1
        do j=noc(11),noc(17)+noc(2)-noc(1)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 11 with surface 18 point number noc(17)+noc(2)-noc(1)
        do i=noc(10)+1,noc(11)-1
           j=noc(17)+noc(2)-noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+ds(i))/ds(i)
        enddo
* Surface 11 with surface 18 half to Surface 20 half
        do i=noc(10)+1,noc(11)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(20)-noc(2)+noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 11 with point number noc(20)-noc(2)+noc(1)+1
        do i=noc(10)+1,noc(11)-1
           j=noc(20)-noc(2)+noc(1)+1
          cs1=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+ds(i))/ds(i)
        enddo
* Surface 11 with surface 20 from noc(20)-noc(2)+noc(1)+2
        do i=noc(10)+1,noc(11)-1
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface12
      include 'input.in'
* Surface 12 with surface 1-11
        do i=noc(11)+1,noc(12)-1
        do j=1,noc(11)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 12 with corner noc(12) to surface 14
      do i=noc(11)+1,noc(12)-1
      do j=noc(12),noc(14)-1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 12 with corner noc(14)
        do i=noc(11)+1,noc(12)-1
           j=noc(14)
          cs1=sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 12 with surface surface 18
        do i=noc(11)+1,noc(12)-1
        do j=noc(14)+1,noc(17)+noc(2)-noc(1)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 12 with from noc(17)+noc(2)-noc(1)+1 on surface 18
        do i=noc(11)+1,noc(12)-1
           j=noc(17)+noc(2)-noc(1)
          cond=cy7-cx7*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.lt.0.0)then
              cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
              cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
*co---Strsight line with obstruction for same chip width case
*     till point cx7,cy7 because cancelling of two sides
            endif
            uc1=sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
            cs2=sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 12 with from noc(17)+noc(2)-noc(1)+1 to noc(19)-noc(1)-3
*(noc(3)-noc(2))-2*(noc(5)-noc(4))
        do i=noc(11)+1,noc(12)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(19)-noc(1)-3*(noc(3)-
     &  noc(2))-2*(noc(5)-noc(4))
          cond=cy7-cx7*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy7-cx7*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
              uc1=uc1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
              cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
                cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy7-cx7*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
                cs2=cs2+sqrt((cy-by(i))**2+(cx3-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 12 with point noc(19)-noc(1)-3*(noc(3)-noc(2))-2
*(noc(5)-noc(4))+1
        do i=noc(11)+1,noc(12)-1
           j=noc(19)-noc(1)-3*(noc(3)-noc(2))-2*(noc(5)-noc(4))+1
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs2-uc1+ds(i))/ds(i)
        enddo
* Surface 12 from noc(19)-noc(1)-3*(noc(3)-noc(2))-2
*(noc(5)-noc(4))+2 till noc(20)
        do i=noc(11)+1,noc(12)-1
        do j=noc(19)-noc(1)-3*(noc(3)-noc(2))-2*(noc(5)-noc(4))+2,
     &  noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface13
      include 'input.in'
* Surface 13 with surface 1-to-11
        do i=noc(12)+1,noc(13)-1
        do j=1,noc(11)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 13 from corner noc(11)
        do i=noc(12)+1,noc(13)-1
           j=noc(11)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
          uc1=sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 13 with surface 12
        do i=noc(12)+1,noc(13)-1
        do j=noc(11)+1,noc(12)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
        enddo
* Surface 13 from corner noc(12)
        do i=noc(12)+1,noc(13)-1
           j=noc(12)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs2-uc1+ds(i))/ds(i)
        enddo
* Surface 13 with surface 14
        do i=noc(12)+1,noc(13)-1
        do j=noc(13),noc(14)-1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 13 with corner noc(14)
        do i=noc(12)+1,noc(13)-1
           j=noc(14)
          cs1=sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 13 with surface 15 to 18 till noc(17)+noc(2)-noc(1)
        do i=noc(12)+1,noc(13)-1
        do j=noc(14)+1,noc(17)+noc(2)-noc(1)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 13 with surface 19 and partly 19=noc(19)-noc(1)-2*(noc(3)-noc(2))-2*(noc(5)-noc(4))+1
        do i=noc(12)+1,noc(13)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(19)-noc(1)-3*(noc(3)-noc(2))-
     &  3*(noc(5)-noc(4))+1
          cond=cy7-cx7*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy7-cx7*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
              uc1=uc1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
              cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
                cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy7-cx7*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
                cs2=cs2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 13 with Surface 19 parallel points
        do i=noc(12)+1,noc(13)-1
        do j=noc(19)-noc(1)-3*(noc(3)-noc(2))-3*(noc(5)-noc(4))+2,
     &  noc(19)-noc(1)-3*(noc(3)-noc(2))-2*(noc(5)-noc(4))
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo 
* Surface 13 to surface 19-20 till noc(20)-noc(2)+noc(1)+1
        do i=noc(12)+1,noc(13)-1
        do j=noc(19)-noc(1)-3*(noc(3)-noc(2))-2*(noc(5)-noc(4))+1,
     &  noc(20)-(noc(2)-noc(1))+1
          cond=cy6-cx6*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy6-cx6*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
              uc2=uc2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
              cond2=cy6-cx6*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
                cs1=cs1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
                cs2=cs2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 13 to surface 20 from noc(20)-(noc(2)-noc(1))+2
        do i=noc(12)+1,noc(13)-1
        do j=noc(20)-(noc(2)-noc(1))+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface14
      include 'input.in'
* Surface 14 with surface 1-11
        do i=noc(13)+1,noc(14)-1
        do j=1,noc(11)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 14 with corner noc(11)
        do i=noc(13)+1,noc(14)-1
           j=noc(11)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
          uc1=sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 14 with surface 12-13
        do i=noc(13)+1,noc(14)-1
        do j=noc(11)+1,noc(13)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 14 with surface 15-18 and partly 19
        do i=noc(13)+1,noc(14)-1
        do j=noc(14),noc(19)-noc(1)-3*(noc(3)-noc(2))-3*
     &  (noc(5)-noc(4))
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 14 with point noc(19)-noc(1)-3*(noc(3)-noc(2))-3*(noc(5)-noc(4))
        do i=noc(13)+1,noc(14)-1
           j=noc(19)-noc(1)-3*(noc(3)-noc(2))-3*(noc(5)-noc(4))+1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1-uc2+ds(i))/ds(i)
        enddo
* Surface 14 from noc(19)-noc(1)-3*(noc(3)-noc(2))-3*(noc(5)-noc(4))+2 to
* noc(20)-noc(2)+noc(1)
        do i=noc(13)+1,noc(14)-1
        do j=noc(19)-noc(1)-3*(noc(3)-noc(2))-3*(noc(5)-noc(4))+2,
     &  noc(20)-noc(2)+noc(1)
          cond=cy6-cx6*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy6-cx6*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
              uc2=uc2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
              cond2=cy6-cx6*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
                cs1=cs1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
                cs2=cs2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 14 with point noc(20)-noc(2)+noc(1)+1
        do i=noc(13)+1,noc(14)-1
           j=noc(20)-noc(2)+noc(1)+1
          cond=cy6-cx6*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
            cond1=cond1+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            else
              cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
              cs2=cs2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
            endif
          uc2=sqrt((by(i)-cy6)**2+(bx(i)-cx6)**2)
          cs1=sqrt((fy(i)-cy6)**2+(fx(i)-cx6)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 14 till noc(12) from noc(20)-noc(2)+noc(1)+2 till noc(20)
        do i=noc(13)+1,noc(14)-1
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface15
      include 'input.in'
* Surface 15 with surface 1-14
        do i=noc(14)+1,noc(15)-1
        do j=1,noc(14)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 15 with surface 16-17 and 18 till some point
        do i=noc(14)+1,noc(15)-1
        do j=noc(15),noc(17)+noc(2)-noc(1)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 15 with surface 18 point number noc(17)+noc(2)-noc(1)
        do i=noc(14)+1,noc(15)-1
           j=noc(17)+noc(2)-noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+ds(i))/ds(i)
        enddo
* Surface 15 with surface 18 half to Surface 20 half
        do i=noc(14)+1,noc(15)-1
        do j=noc(17)+noc(2)-noc(1)+1,noc(20)-noc(2)+noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 15 with point number noc(20)-noc(2)+noc(1)+1
        do i=noc(14)+1,noc(15)-1
           j=noc(20)-noc(2)+noc(1)+1
          cs1=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+ds(i))/ds(i)
        enddo
* Surface 15 with surface 20 from noc(20)-noc(2)+noc(1)+2
        do i=noc(14)+1,noc(15)-1
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface16
      include 'input.in'
* Surface 16 with surface 1-to-15
        do i=noc(15)+1,noc(16)-1
        do j=1,noc(15)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 16 with surface 17
        do i=noc(15)+1,noc(16)-1
        do j=noc(16),noc(18)+noc(17)-noc(16)-1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 16 with point noc(18)+noc(17)-noc(16)
        do i=noc(15)+1,noc(16)-1
           j=noc(18)+noc(17)-noc(16)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs2-uc1+ds(i))/ds(i)
        enddo
* Surface 16 from point noc(18)+noc(17)-noc(16)+1 till noc(20)
        do i=noc(15)+1,noc(16)-1
        do j=noc(18)+noc(17)-noc(16)+1,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface17
      include 'input.in'
* Surface 17 with surface 1-to-15
        do i=noc(16)+1,noc(17)-1
        do j=1,noc(15)-1
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 17 from corner noc(15)
        do i=noc(16)+1,noc(17)-1
           j=noc(15)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          uc1=sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 17 with surface 16
        do i=noc(16)+1,noc(17)-1
        do j=noc(15)+1,noc(16)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
        enddo
* Surface 17 with corner noc(16)
        do i=noc(16)+1,noc(17)-1
           j=noc(15)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs2-uc1+ds(i))/ds(i)
        enddo
* Surface 17 with surface 18 and partly 19
        do i=noc(16)+1,noc(17)-1
        do j=noc(17),noc(18)+noc(17)-noc(16)-1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
* Surface 17 to surface 19-20 till noc(20)-noc(2)+noc(1)+1
        do i=noc(16)+1,noc(17)-1
        do j=noc(18)+noc(17)-noc(16),noc(20)-noc(2)+noc(1)+1
          cond=cy8-cx8*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
              uc2=uc2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
              cond2=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
                cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
                cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 17 to surface 20 from noc(20)-(noc(2)-noc(1))+2
        do i=noc(16)+1,noc(17)-1
        do j=noc(20)-(noc(2)-noc(1))+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface18
      include 'input.in'
* Surface 18 with surface 1
        do i=noc(17)+1,noc(18)-1
        do j=1,noc(17)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
        enddo
* Surface 18 with corners 1
        do i=noc(17)+1,noc(17)+noc(2)-noc(1)
           j=1
          v_f(i,j)=0.0
        enddo
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=1
          cond=cy1-cx1*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((by(i)-cy1)**2+(bx(i)-cx1)**2)
              uc2=uc2+sqrt((cy1-fy(j))**2+(cx1-fx(j))**2)
              cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
                cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
                cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 18 till noc(17)+noc(2)-noc(1)-1 with corners
* noc(1),noc(2),noc(3),noc(4),noc(5),noc(6),noc(7)
* noc(8),noc(9),noc(10),noc(11),noc(12),noc(13),noc(14)
        do i=noc(17)+1,noc(17)+noc(2)-noc(1)-1
        do j1=1,14
           j=noc(j1)
          v_f(i,j)=0.0
        enddo
        enddo
* Surface 18 from noc(17)+noc(2)-noc(1) with corner noc(1)
        do i=noc(17)+noc(2)-noc(1),noc(18)-1
           j=noc(1)
          v_f(i,j)=0.0
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with corner noc(2)
           i=noc(17)+noc(2)-noc(1)
           j=noc(2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+0.5*ds(j))/ds(i)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
          uc1=sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with corner noc(3)
           i=noc(17)+noc(2)-noc(1)
           j=noc(3)
          cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
          cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 18 from noc(17)+noc(2)-noc(1)+1 till noc(18)-1 with noc(3)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(3)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          cond=cy3-cx3*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.lt.0.0)then
            uc2=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            uc2=uc2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          endif
          cond1=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond1=cond1+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond1.lt.0.0)then
            cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with noc(4)
           i=noc(17)+noc(2)-noc(1)
           j=noc(4)
          cond=cy3-cx3*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
            cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.lt.0.0)then
              cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
              cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
            uc2=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
* Surface 18 from noc(17)+noc(2)-noc(1)+1 to noc(18)-1 with noc(4)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(4)
          cond=cy3-cx3*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
            cond1=cy3-cx3*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
              uc2=uc2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
              cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
                cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy3-cx3*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
                cs2=cs2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 18 with corner noc(5)
        do i=noc(17)+1,noc(18)-1
           j=noc(5)
          v_f(i,j)=0.0
        enddo       
* Surface 18 point noc(17)+noc(2)-noc(1) with corner noc(6)
           i=noc(17)+noc(2)-noc(1)
           j=noc(6)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+0.5*ds(j))/ds(i)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(6)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
          uc1=sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with corner noc(7)
           i=noc(17)+noc(2)-noc(1)
           j=noc(7)
          cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
          cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 18 point noc(17)+noc(2)-noc(1)+1 with corner noc(7)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(7)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          cond=cy5-cx5*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.lt.0.0)then
            uc2=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            uc2=uc2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          endif
          cond1=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond1=cond1+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond1.lt.0.0)then
            cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with noc(8)
           i=noc(17)+noc(2)-noc(1)
           j=noc(8)
          cond=cy5-cx5*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
            cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.lt.0.0)then
              cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
              cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
            uc2=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
* Surface 18 from noc(17)+noc(2)-noc(1)+1 to noc(18)-1 with noc(8)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(8)
          cond=cy5-cx5*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
            cond1=cy5-cx5*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
              uc2=uc2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
              cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
                cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy5-cx5*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
                cs2=cs2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 18 with corner noc(9)
        do i=noc(17)+1,noc(18)-1
           j=noc(9)
          v_f(i,j)=0.0
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with corner noc(10)
           i=noc(17)+noc(2)-noc(1)
           j=noc(10)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+0.5*ds(j))/ds(i)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(10)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
          uc1=sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with corner noc(11)
           i=noc(17)+noc(2)-noc(1)
           j=noc(11)
          cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
          cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 18 from noc(17)+noc(2)-noc(1)+1 to noc(18)-1 with noc(11)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(11)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          cond=cy7-cx7*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.lt.0.0)then
            uc2=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            uc2=uc2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          endif
          cond1=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond1=cond1+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond1.lt.0.0)then
            cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with noc(12)
           i=noc(17)+noc(2)-noc(1)
           j=noc(12)
          cond=cy7-cx7*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
            cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.lt.0.0)then
              cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
              cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
            uc2=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
* Surface 18 point noc(17)+noc(2)-noc(1)+1 to noc(18)-1 with noc(12)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(12)
          cond=cy7-cx7*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
            cond1=cy7-cx7*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
              uc2=uc2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
              cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
                cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy7-cx7*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
                cs2=cs2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 18 with corner noc(13)
        do i=noc(17)+1,noc(18)-1
           j=noc(13)
          v_f(i,j)=0.0
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with corner noc(14)
           i=noc(17)+noc(2)-noc(1)
           j=noc(14)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          v_f(i,j)=0.5*(cs1-uc1+0.5*ds(j))/ds(i)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(14)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
          uc1=sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 till noc(17)+noc(2)-noc(1)-1 with corner noc(15)
        do i=noc(17)+1,noc(17)+noc(2)-noc(1)-1
           j=noc(15)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          uc1=sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 point noc(17)+noc(2)-noc(1) with corner noc(15)
           i=noc(17)+noc(2)-noc(1)
           j=noc(15)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2+0.5*ds(j))/ds(i)
* Surface 18 from noc(17)+noc(2)-noc(1)+1 with corner noc(15)
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(15)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 with corner noc(16)
        do i=noc(17)+1,noc(18)-1
           j=noc(16)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 with corner noc(17)
        do i=noc(17)+1,noc(18)-1
           j=noc(17)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 with corner noc(18)
        do i=noc(17)+1,noc(18)-1
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 18 with surface 19
        do i=noc(17)+1,noc(18)-1
        do j=noc(18)+1,noc(19)
          cond=cy8-cx8*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
              uc2=uc2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
              cond2=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
                cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
                cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5d0*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
* Surface 18 with Surface 20
        do i=noc(17)+1,noc(17)+noc(2)-noc(1)-1
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          v_f(i,j)=0.0
        enddo
        enddo
        do i=noc(17)+1,noc(17)+noc(2)-noc(1)-1
           j=noc(20)-noc(2)+noc(1)+1
          cond=cy8-cx8*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.ge.0.0)then
              cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
              cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            uc2=sqrt((by(i)-cy8)**2+(bx(i)-cx8)**2)
            cs1=sqrt((fy(i)-cy8)**2+(fx(i)-cx8)**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
        do i=noc(17)+1,noc(17)+noc(2)-noc(1)-1
        do j=noc(19)+1,noc(20)-(noc(2)-noc(1))
          cond=cy8-cx8*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
              uc2=uc2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
              cond2=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
                cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
                cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5d0*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
           i=noc(17)+noc(2)-noc(1)
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          cond=cy1-cx1*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
              uc2=uc2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
                cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
                cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
           i=noc(17)+noc(2)-noc(1)
           j=noc(20)-noc(2)+noc(1)+1
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          uc2=uc2+sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
          uc2=uc2+sqrt((cy8-cy1)**2+(cx8-cx1)**2)
          cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
            cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
            cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
           i=noc(17)+noc(2)-noc(1)
        do j=noc(19)+1,noc(20)-noc(2)+noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cond1=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond1.le.0.0)then
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          else
            uc2=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
            uc2=uc2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          endif
          cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
            cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
        do j=noc(20)-noc(2)+noc(1)+2,noc(20)
          cond=cy1-cx1*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((by(i)-cy1)**2+(bx(i)-cx1)**2)
              uc2=uc2+sqrt((cy1-fy(j))**2+(cx1-fx(j))**2)
              cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
                cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
                cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        enddo
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
           j=noc(20)-noc(2)+noc(1)+1
          cond=cy1-cx1*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
              uc2=uc2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
                cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
                cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
        do i=noc(17)+noc(2)-noc(1)+1,noc(18)-1
        do j=noc(19)+1,noc(20)-noc(2)+noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo

      return
      end
*====================================*
      subroutine surface19
      include 'input.in'
* Surface 19 with surfaces
        do i=noc(18)+1,noc(19)-1
        do j=1,noc(18)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
        enddo
* Surface 19 with corner i=1
        do i=noc(18)+1,noc(19)-1
           j=1
          cond=cy1-cx1*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
              uc2=uc2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
                cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
                cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 19 till noc(19)-noc(1) with corner noc(1)
        do i=noc(18)+1,noc(19)-noc(1)
           j=noc(1)
          cond=cy1-cx1*(fy(i)-by(j))/(fx(i)-bx(j))
          cond = cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            uc2=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
            uc2=uc2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
            cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
            cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
            cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.lt.0.0)then
              cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
              cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 19 point noc(19)-noc(1)+1 with corner noc(1)
           i=noc(19)-noc(1)+1
           j=noc(1)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
          uc2=uc2+sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
          cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.lt.0.0)then
            cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
            cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with noc(19)-noc(1)+2 with corner noc(1)
        do i=noc(19)-noc(1)+2,noc(19)-1
           j=noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(19)-noc(1) with corner noc(2)
        do i=noc(18)+1,noc(19)-noc(1)
           j=noc(2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(19)-noc(1)+1 with corner noc(2)
           i=noc(19)-noc(1)+1
           j=noc(2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cs2=sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
          cs2=cs2+sqrt((cy1-by(j))**2+(cx1-bx(j))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with noc(19)-noc(1)+2 with corner noc(2)
        do i=noc(19)-noc(1)+2,noc(19)-1
           j=noc(2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(19)-noc(1)-noc(3)+noc(2) with corner noc(3)
        do i=noc(18)+1,noc(19)-noc(1)-noc(3)+noc(2)
           j=noc(3)
          cond1=cy3-cx3*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
          if (cond1.lt.0.0)then
            uc1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            uc1=uc1+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
          else
            uc1=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          endif
          cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(j)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(j)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc2=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(19)-noc(1)-noc(3)+noc(2)+1 with corner noc(3)
           i=noc(19)-noc(1)-noc(3)+noc(2)+1
           j=noc(3)
          cs1=sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
          cs1=cs1+sqrt((cy2-fy(j))**2+(cx2-fx(j))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with noc(19)-noc(1)-noc(3)+noc(2)+2 with corner noc(3)
        do i=noc(19)-noc(1)-noc(3)+noc(2)+2,noc(19)-1
           j=noc(3)
          cs1=sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(19)-noc(1)-noc(3)+noc(2) with corner noc(4)
        do i=noc(18)+1,noc(19)-noc(1)-noc(3)+noc(2)
           j=noc(4)
          cond=cy3-cx3*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
            cond1=cy3-cx3*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
              uc2=uc2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
              cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
                cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy3-cx3*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
                cs2=cs2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 19 point noc(19)-noc(1)-noc(3)+noc(2)+1 with corner noc(4)
           i=noc(19)-noc(1)-noc(3)+noc(2)+1
           j=noc(4)
          cond2=cy2-cx2*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.gt.0.0)then
            cs1=sqrt((fy(j)-cy2)**2.0+(fx(j)-cx2)**2.0)
            cs1=cs1+sqrt((cy2-fy(i))**2.0+(cx2-fx(i))**2.0)
          else
            cs1=sqrt((fy(j)-fy(i))**2.0+(fx(j)-fx(i))**2.0)
          endif
          uc1=sqrt((cy2-fy(i))**2.0+(cx2-fx(i))**2.0)
          uc1=uc1+sqrt((by(j)-cy2)**2.0+(bx(j)-cx2)**2.0)
          cs2=sqrt((by(j)-by(i))**2.0+(bx(j)-bx(i))**2.0)
          uc2=sqrt((fy(j)-by(i))**2.0+(fx(j)-bx(i))**2.0)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with noc(19)-noc(1)-noc(3)+noc(2)+2 with corner noc(4)
        do i=noc(19)-noc(1)-noc(3)+noc(2)+2,noc(19)-1
           j=noc(4)
          cond=cy2-cx2*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(i)-fy(j))**2+(bx(i)-fx(j))**2)
            uc2=sqrt((fy(i)-cy2)**2+(fx(i)-cx2)**2)
            uc2=uc2+sqrt((cy2-by(j))**2+(cx2-bx(j))**2)
            cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
            cs2=cs2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
            cond1=cy2-cx2*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond1=cond1+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond1.gt.0.0)then
              cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
              cs1=cs1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 19 to noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1) corner noc(5)
        do i=noc(18)+1,noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)
           j=noc(5)
          cond=cy3-cx3*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            uc2=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            uc2=uc2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
            cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
            cond3=cy3-cx3*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.lt.0.0)then
              cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
              cs2=cs2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 19 point noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+1 with noc(5)
           i=noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+1
           j=noc(5)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
          uc2=uc2+sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
          cond3=cy3-cx3*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.lt.0.0)then
            cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
            cs2=cs2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 from noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+2 with noc(5)
        do i=noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+2,noc(19)-1
           j=noc(5)
          cond=cy2-cx2*(by(i)-fy(j))/(bx(i)-fx(j))
          cond=cond+bx(i)*(by(i)-fy(j))/(bx(i)-fx(j))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy2-cx2*(fy(i)-by(j))/(fx(i)-bx(j))
            cond1=cond1+fx(i)*(fy(i)-by(j))/(fx(i)-bx(j))-fy(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy2)**2+(fx(i)-cx2)**2)
              uc1=uc1+sqrt((cy2-by(j))**2+(cx2-bx(j))**2)
              cond2=cy2-cx2*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
                cs1=cs1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
                cs2=cs2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 19 point noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1) with noc(6)
        do i=noc(18)+1,noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)
           j=noc(6)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+1 with corner noc(6)
           i=noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+1
           j=noc(6)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cs2=sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
          cs2=cs2+sqrt((cy3-by(j))**2+(cx3-bx(j))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+2 with corner noc(6)
        do i=noc(19)+noc(4)+noc(2)-noc(5)-noc(3)-noc(1)+2,noc(19)-1
           j=noc(6)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
            cs2=cs2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          cond1=cy2-cx2*(fy(i)-by(j))/(fx(i)-bx(j))
          cond1=cond1+fx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-fy(j)
          if (cond1.gt.0.0)then
            uc1=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
            uc1=uc1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(19)-noc(7)-noc(5)-noc(3)-noc(1)+noc(6)+noc(4)+noc(2) with corner noc(7)
        do i=noc(18)+1,noc(19)-noc(7)-noc(5)-noc(3)-noc(1)+noc(6)+
     &       noc(4)+noc(2)
           j=noc(7)
          cond1=cy5-cx5*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
          if (cond1.lt.0.0)then
            uc1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            uc1=uc1+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
          else
            uc1=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          endif
          cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(j)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(j)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc2=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point i1+1 (calculated above) with corner noc(7)
           i=noc(19)-noc(7)-noc(5)-noc(3)-noc(1)+noc(6)+
     &       noc(4)+noc(2)+1
           j=noc(7)
          cs1=sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
          cs1=cs1+sqrt((cy4-fy(j))**2+(cx4-fx(j))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with i1+2 with corner noc(7)
        do i=noc(19)-noc(7)-noc(5)-noc(3)-noc(1)+noc(6)+
     &     noc(4)+noc(2)+2,noc(19)-1
           j=noc(7)
          cs1=sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(19)-noc(7)-noc(5)-noc(3)-noc(1)+noc(6)+noc(4)+noc(2) with corner noc(8)
        do i=noc(18)+1,noc(19)-noc(7)-noc(5)-noc(3)-noc(1)+noc(6)+
     &	   noc(4)+noc(2)
           j=noc(8)
          cond=cy5-cx5*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
            cond1=cy5-cx5*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
              uc2=uc2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
              cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
                cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy5-cx5*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
                cs2=cs2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 19 point i1+1 with corner noc(8)
           i=noc(19)-noc(7)-noc(5)-noc(3)-noc(1)+noc(6)+
     &	   noc(4)+noc(2)+1
           j=noc(8)
          cond2=cy4-cx4*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.gt.0.0)then
            cs1=sqrt((fy(j)-cy4)**2.0+(fx(j)-cx4)**2.0)
            cs1=cs1+sqrt((cy4-fy(i))**2.0+(cx4-fx(i))**2.0)
          else
            cs1=sqrt((fy(j)-fy(i))**2.0+(fx(j)-fx(i))**2.0)
          endif
          uc1=sqrt((cy4-fy(i))**2.0+(cx4-fx(i))**2.0)
          uc1=uc1+sqrt((by(j)-cy4)**2.0+(bx(j)-cx4)**2.0)
          cs2=sqrt((by(j)-by(i))**2.0+(bx(j)-bx(i))**2.0)
          uc2=sqrt((fy(j)-by(i))**2.0+(fx(j)-bx(i))**2.0)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with i1+2 with corner noc(8)
        do i=noc(19)-noc(7)-noc(5)-noc(3)-noc(1)+noc(6)+
     &	   noc(4)+noc(2)+2,noc(19)-1
           j=noc(8)
          cond=cy4-cx4*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(i)-fy(j))**2+(bx(i)-fx(j))**2)
            uc2=sqrt((fy(i)-cy4)**2+(fx(i)-cx4)**2)
            uc2=uc2+sqrt((cy4-by(j))**2+(cx4-bx(j))**2)
            cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
            cs2=cs2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
            cond1=cy4-cx4*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond1=cond1+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond1.gt.0.0)then
              cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
              cs1=cs1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 19 to noc(18)+noc(17)+noc(15)+noc(13)+noc(11)
* -noc(16)-noc(14)-noc(12)-noc(10)-1 corner noc(9)
        do i=noc(18)+1,noc(18)+noc(17)+noc(15)+noc(13)+noc(11)-
     &     noc(16)-noc(14)-noc(12)-noc(10)-1
           j=noc(9)
          cond=cy5-cx5*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            uc2=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            uc2=uc2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
            cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
            cond3=cy5-cx5*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.lt.0.0)then
              cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
              cs2=cs2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 19 point i1+1 with noc(9)
           i=noc(18)+noc(17)+noc(15)+noc(13)+noc(11)-noc(16)-
     &     noc(14)-noc(12)-noc(10)
           j=noc(9)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
          uc2=uc2+sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
          cond3=cy5-cx5*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.lt.0.0)then
            cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
            cs2=cs2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 from i1+2 with noc(9)
        do i=noc(18)+noc(17)+noc(15)+noc(13)+noc(11)-noc(16)-
     &     noc(14)-noc(12)-noc(10)+1,noc(19)-1
           j=noc(9)
          cond=cy4-cx4*(by(i)-fy(j))/(bx(i)-fx(j))
          cond=cond+bx(i)*(by(i)-fy(j))/(bx(i)-fx(j))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy4-cx4*(fy(i)-by(j))/(fx(i)-bx(j))
            cond1=cond1+fx(i)*(fy(i)-by(j))/(fx(i)-bx(j))-fy(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy4)**2+(fx(i)-cx4)**2)
              uc1=uc1+sqrt((cy4-by(j))**2+(cx4-bx(j))**2)
              cond2=cy4-cx4*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
                cs1=cs1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
                cs2=cs2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 19 noc(18)+noc(17)+noc(15)+noc(13)+noc(11)
* -noc(16)-noc(14)-noc(12)-noc(10)-1 with noc(10)
        do i=noc(18)+1,noc(18)+noc(17)+noc(15)+noc(13)+noc(11)-
     &     noc(16)-noc(14)-noc(12)-noc(10)-1
           j=noc(10)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point i1+1 with corner noc(10)
           i=noc(18)+noc(17)+noc(15)+noc(13)+noc(11)-
     &     noc(16)-noc(14)-noc(12)-noc(10)
           j=noc(10)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cs2=sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
          cs2=cs2+sqrt((cy5-by(j))**2+(cx5-bx(j))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with i1+2 with corner noc(10)
        do i=noc(18)+noc(17)+noc(15)+noc(13)+noc(11)-
     &     noc(16)-noc(14)-noc(12)-noc(10)+1,noc(19)-1
           j=noc(10)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
            cs2=cs2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          cond1=cy4-cx4*(fy(i)-by(j))/(fx(i)-bx(j))
          cond1=cond1+fx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-fy(j)
          if (cond1.gt.0.0)then
            uc1=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
            uc1=uc1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(18)+noc(17)+noc(15)+noc(13)-noc(16)-noc(14)-noc(12)
* with corner noc(11)
        do i=noc(18)+1,noc(18)+noc(17)+noc(15)+noc(13)-noc(16)-
     &     noc(14)-noc(12)-1
           j=noc(11)
          cond1=cy7-cx7*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
          if (cond1.lt.0.0)then
            uc1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            uc1=uc1+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
          else
            uc1=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          endif
          cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(j)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(j)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc2=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point i1+1 (calculated above) with corner noc(11)
           i=noc(18)+noc(17)+noc(15)+noc(13)-noc(16)-
     &     noc(14)-noc(12)
           j=noc(11)
          cs1=sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
          cs1=cs1+sqrt((cy6-fy(j))**2+(cx6-fx(j))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with i1+2 with corner noc(11)
        do i=noc(18)+noc(17)+noc(15)+noc(13)-noc(16)-
     &     noc(14)-noc(12)+1,noc(19)-1
           j=noc(11)
          cs1=sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(18)+noc(17)+noc(15)+noc(13)-noc(16)-noc(14)-noc(12)
* with corner noc(12)
        do i=noc(18)+1,noc(18)+noc(17)+noc(15)+noc(13)-noc(16)-
     &     noc(14)-noc(12)-1
           j=noc(12)
          cond=cy7-cx7*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
            cond1=cy7-cx7*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+fx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-fy(j)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
              uc2=uc2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
              cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
                cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy7-cx7*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
                cs2=cs2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 19 point i1+1 with corner noc(12)
           i=noc(18)+noc(17)+noc(15)+noc(13)-noc(16)-
     &     noc(14)-noc(12)
           j=noc(12)
          cond2=cy6-cx6*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.gt.0.0)then
            cs1=sqrt((fy(j)-cy6)**2.0+(fx(j)-cx6)**2.0)
            cs1=cs1+sqrt((cy6-fy(i))**2.0+(cx6-fx(i))**2.0)
          else
            cs1=sqrt((fy(j)-fy(i))**2.0+(fx(j)-fx(i))**2.0)
          endif
          uc1=sqrt((cy6-fy(i))**2.0+(cx6-fx(i))**2.0)
          uc1=uc1+sqrt((by(j)-cy6)**2.0+(bx(j)-cx6)**2.0)
          cs2=sqrt((by(j)-by(i))**2.0+(bx(j)-bx(i))**2.0)
          uc2=sqrt((fy(j)-by(i))**2.0+(fx(j)-bx(i))**2.0)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with i1+2 with corner noc(12)
        do i=noc(18)+noc(17)+noc(15)+noc(13)-noc(16)-
     &     noc(14)-noc(12)+1,noc(19)-1
           j=noc(12)
          cond=cy6-cx6*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(i)-fy(j))**2+(bx(i)-fx(j))**2)
            uc2=sqrt((fy(i)-cy6)**2+(fx(i)-cx6)**2)
            uc2=uc2+sqrt((cy6-by(j))**2+(cx6-bx(j))**2)
            cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
            cs2=cs2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
            cond1=cy6-cx6*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond1=cond1+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond1.gt.0.0)then
              cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
              cs1=cs1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 19 to noc(18)+noc(17)+noc(15)-noc(16)-noc(14)-1 corner noc(13)
        do i=noc(18)+1,noc(18)+noc(17)+noc(15)-noc(16)-noc(14)-1
           j=noc(13)
          cond=cy7-cx7*(fy(i)-by(j))/(fx(i)-bx(j))
          cond=cond+bx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-by(j)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            uc2=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            uc2=uc2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
            cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
            cond3=cy7-cx7*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.lt.0.0)then
              cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
              cs2=cs2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 19 point i1+1 with noc(13)
           i=noc(18)+noc(17)+noc(15)-noc(16)-noc(14)
           j=noc(13)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
          uc2=uc2+sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
          cond3=cy7-cx7*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.lt.0.0)then
            cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
            cs2=cs2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 from i1+2 with noc(13)
        do i=noc(18)+noc(17)+noc(15)-noc(16)-noc(14)+1,noc(19)-1
           j=noc(13)
          cond=cy6-cx6*(by(i)-fy(j))/(bx(i)-fx(j))
          cond=cond+bx(i)*(by(i)-fy(j))/(bx(i)-fx(j))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy6-cx6*(fy(i)-by(j))/(fx(i)-bx(j))
            cond1=cond1+fx(i)*(fy(i)-by(j))/(fx(i)-bx(j))-fy(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy6)**2+(fx(i)-cx6)**2)
              uc1=uc1+sqrt((cy6-by(j))**2+(cx6-bx(j))**2)
              cond2=cy6-cx6*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
                cs1=cs1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
                cs2=cs2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 19 point noc(18)+noc(17)+noc(15)-noc(16)-noc(14)-1 with noc(14)
        do i=noc(18)+1,noc(18)+noc(17)+noc(15)-noc(16)-noc(14)-1
           j=noc(14)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point i1+1 with corner noc(14)
           i=noc(18)+noc(17)+noc(15)-noc(16)-noc(14)
           j=noc(14)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cs2=sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
          cs2=cs2+sqrt((cy7-by(j))**2+(cx7-bx(j))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with i1+2 with corner noc(14)
        do i=noc(18)+noc(17)+noc(15)-noc(16)-noc(14)+1,noc(19)-1
           j=noc(14)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
            cs2=cs2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          cond1=cy6-cx6*(fy(i)-by(j))/(fx(i)-bx(j))
          cond1=cond1+fx(j)*(fy(i)-by(j))/(fx(i)-bx(j))-fy(j)
          if (cond1.gt.0.0)then
            uc1=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
            uc1=uc1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(18)+noc(17)-noc(16)-1 with corner noc(15)
        do i=noc(18)+1,noc(18)+noc(17)-noc(16)-1
           j=noc(15)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(18)+noc(17)-noc(16) with corner noc(15)
           i=noc(18)+noc(17)-noc(16)
           j=noc(15)
          cs1=sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
          cs1=cs1+sqrt((cy8-fy(j))**2+(cx8-fx(j))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with noc(18)+noc(17)-noc(16)+1 with corner noc(15)
        do i=noc(18)+noc(17)-noc(16)+1,noc(19)-1
           j=noc(15)
          cs1=sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(18)+noc(17)-noc(16)-1 with corner noc(16)
        do i=noc(18)+1,noc(18)+noc(17)-noc(16)-1
           j=noc(16)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 point noc(18)+noc(17)-noc(16) with i=noc(16)
           i=noc(18)+noc(17)-noc(16)
           j=noc(16)
          uc1=sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
          uc1=uc1+sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond2=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.gt.0.0)then
            cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
            cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 19 with noc(18)+noc(17)-noc(16)+1 with corner noc(16)
        do i=noc(18)+noc(17)-noc(16)+1,noc(19)-1
           j=noc(16)
          cond=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(i)-fy(j))**2+(bx(i)-fx(j))**2)
            uc2=sqrt((fy(i)-cy8)**2+(fx(i)-cx8)**2)
            uc2=uc2+sqrt((cy8-by(j))**2+(cx8-bx(j))**2)
            cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
            cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
            cond1=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond1=cond1+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond1.gt.0.0)then
              cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
              cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 19 with corner noc(17)
        do i=noc(18)+1,noc(19)-1
           j=noc(17)
          cond=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy8-cx8*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
              uc1=uc1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
              cond2=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
                cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
                cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 19 with noc(18)
        do i=noc(18)+1,noc(19)-1
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 19 with noc(19)
        do i=noc(18)+1,noc(19)-1
        do j=noc(19),noc(20)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
        enddo
      return
      end
*====================================*
      subroutine surface20
      include 'input.in'
* Surface 20 with surfaces
        do i=noc(19)+1,noc(20)
        do j=1,noc(19)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
        enddo
* Surface 20 with corner i=1
        do i=noc(19)+1,noc(20)
           j=1
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 20 with noc(1)
        do i=noc(19)+1,noc(20)
           j=noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 20 to noc(20)-noc(2)+noc(1) with i=noc(2)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(2)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 20 point noc(20)-noc(2)+noc(1)+1 with corner noc(2)
           i=noc(20)-noc(2)+noc(1)+1
           j=noc(2)
          cs1=sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          cs1=cs1+sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 20 from noc(20)-noc(2)+noc(1)+2 with corner noc(2)
        do i=noc(20)-noc(2)+noc(1)+2,noc(20)
           j=noc(2)
          cs1=sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy1-by(i))**2+(cy1-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 20 to noc(20)-noc(2)+noc(1) with i=noc(3)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(3)
          cs1=sqrt((fy(i)-cy2)**2+(fx(i)-cx2)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 20 point noc(20)-noc(2)+noc(1)+1 with corner noc(3)
           i=noc(20)-noc(2)+noc(1)+1
           j=noc(3)
          cs1=sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          cs1=cs1+sqrt((cy2-cy1)**2+(cx2-cx1)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
          uc1=uc1+sqrt((fy(i)-cy1)**2+(fx(i)-cx1)**2)
          uc2=sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 20 from noc(20)-noc(2)+noc(1)+2 with corner noc(3)
        do i=noc(20)-noc(2)+noc(1)+2,noc(20)
           j=noc(3)
          v_f(i,j)=0.0
        enddo
* Surface 20 point noc(20)-noc(2)+noc(1) with corner noc(4)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(4)
          cond=cy2-cx2*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond2=cy2-cx2*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.gt.0.0)then
              cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
              cs1=cs1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            uc1=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
            uc1=uc1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
            cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
            cs2=cs2+sqrt((by(i)-cy2)**2+(bx(i)-cx2)**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 20 from noc(20)-noc(2)+noc(1)+1 with corner noc(4)
        do i=noc(20)-noc(2)+noc(1)+1,noc(20)
           j=noc(4)
          v_f(i,j)=0.0
        enddo
* Surface 20 with corner noc(5)
        do i=noc(19)+1,noc(20)
           j=noc(5)
          cond=cy2-cx2*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy2-cx2*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+bx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-by(j)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy2)**2+(fx(i)-cx2)**2)
              uc1=uc1+sqrt((cy2-by(j))**2+(cx2-bx(j))**2)
              cond2=cy2-cx2*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
                cs1=cs1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
                cs2=cs2+sqrt((by(i)-cy2)**2+(bx(i)-cx2)**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 20 to noc(20)-noc(2)+noc(1) with corner noc(6)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(6)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond1=cy2-cx2*(by(j)-fy(i))/(bx(j)-fx(i))
          cond1=cond1+bx(j)*(by(j)-fy(i))/(bx(j)-fx(i))-by(j)
          if (cond1.gt.0.0)then
            uc1=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
            uc1=uc1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          endif
          cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
            cs2=cs2+sqrt((by(i)-cy2)**2+(bx(i)-cx2)**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
      enddo
* Surface 20 point noc(20)-noc(2)+noc(1)+1 with corner noc(6)
           i=noc(20)-noc(2)+noc(1)+1
           j=noc(6)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
          cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
          cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
            cs2=cs2+sqrt((by(i)-cy2)**2+(bx(i)-cx2)**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 20 from noc(20)-noc(2)+noc(1)+2 with corner noc(6)
        do i=noc(20)-noc(2)+noc(1)+2,noc(20)
           j=noc(6)
          v_f(i,j)=0.0
        enddo
* Surface 20 to noc(20)-noc(2)+noc(1) with corner noc(7)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(7)
          cs1=sqrt((fy(i)-cy4)**2+(fx(i)-cx4)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 20 point noc(20)-noc(2)+noc(1)+1 with corner noc(7)
           i=noc(20)-noc(2)+noc(1)+1
           j=noc(7)
          cs1=sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          cs1=cs1+sqrt((cy4-cy1)**2+(cx4-cx1)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
          uc1=uc1+sqrt((fy(i)-cy1)**2+(fx(i)-cx1)**2)
          uc2=sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 20 from noc(20)-noc(2)+noc(1)+2 with corner noc(7)
        do i=noc(20)-noc(2)+noc(1)+2,noc(20)
           j=noc(7)
          v_f(i,j)=0.0
        enddo
* Surface 20 point noc(20)-noc(2)+noc(1) with corner noc(8)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(8)
          cond=cy4-cx4*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond2=cy4-cx4*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.gt.0.0)then
              cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
              cs1=cs1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            uc1=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
            uc1=uc1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
            cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
            cs2=cs2+sqrt((by(i)-cy4)**2+(bx(i)-cx4)**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 20 from noc(20)-noc(2)+noc(1)+1 with corner noc(8)
        do i=noc(20)-noc(2)+noc(1)+1,noc(20)
           j=noc(8)
          v_f(i,j)=0.0
        enddo
* Surface 20 with corner noc(9)
        do i=noc(19)+1,noc(20)
           j=noc(9)
          cond=cy4-cx4*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy4-cx4*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+bx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-by(j)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy4)**2+(fx(i)-cx4)**2)
              uc1=uc1+sqrt((cy4-by(j))**2+(cx4-bx(j))**2)
              cond2=cy4-cx4*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
                cs1=cs1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
                cs2=cs2+sqrt((by(i)-cy4)**2+(bx(i)-cx4)**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 20 to noc(20)-noc(2)+noc(1) with corner noc(10)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(10)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond1=cy4-cx4*(by(j)-fy(i))/(bx(j)-fx(i))
          cond1=cond1+bx(j)*(by(j)-fy(i))/(bx(j)-fx(i))-by(j)
          if (cond1.gt.0.0)then
            uc1=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
            uc1=uc1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          endif
          cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
            cs2=cs2+sqrt((by(i)-cy4)**2+(bx(i)-cx4)**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
      enddo
* Surface 20 point noc(20)-noc(2)+noc(1)+1 with corner noc(10)
           i=noc(20)-noc(2)+noc(1)+1
           j=noc(10)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
          cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
          cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
            cs2=cs2+sqrt((by(i)-cy4)**2+(bx(i)-cx4)**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 20 from noc(20)-noc(2)+noc(1)+2 with corner noc(10)
        do i=noc(20)-noc(2)+noc(1)+2,noc(20)
           j=noc(10)
          v_f(i,j)=0.0
        enddo
* Surface 20 to noc(20)-noc(2)+noc(1) with corner noc(11)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(11)
          cs1=sqrt((fy(i)-cy6)**2+(fx(i)-cx6)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 20 point noc(20)-noc(2)+noc(1)+1 with corner noc(11)
           i=noc(20)-noc(2)+noc(1)+1
           j=noc(11)
          cs1=sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          cs1=cs1+sqrt((cy6-cy1)**2+(cx6-cx1)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
          uc1=uc1+sqrt((fy(i)-cy1)**2+(fx(i)-cx1)**2)
          uc2=sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 20 from noc(20)-noc(2)+noc(1)+2 with corner noc(11)
        do i=noc(20)-noc(2)+noc(1)+2,noc(20)
           j=noc(11)
          v_f(i,j)=0.0
        enddo
* Surface 20 point noc(20)-noc(2)+noc(1) with corner noc(12)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(12)
          cond=cy6-cx6*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond2=cy6-cx6*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.gt.0.0)then
              cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
              cs1=cs1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            uc1=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
            uc1=uc1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
            cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
            cs2=cs2+sqrt((by(i)-cy6)**2+(bx(i)-cx6)**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 20 from noc(20)-noc(2)+noc(1)+1 with corner noc(12)
        do i=noc(20)-noc(2)+noc(1)+1,noc(20)
           j=noc(12)
          v_f(i,j)=0.0
        enddo
* Surface 20 with corner noc(13)
        do i=noc(19)+1,noc(20)
           j=noc(13)
          cond=cy6-cx6*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy6-cx6*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+bx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-by(j)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy6)**2+(fx(i)-cx6)**2)
              uc1=uc1+sqrt((cy6-by(j))**2+(cx6-bx(j))**2)
              cond2=cy6-cx6*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
                cs1=cs1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
                cs2=cs2+sqrt((by(i)-cy6)**2+(bx(i)-cx6)**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 20 to noc(20)-noc(2)+noc(1) with corner noc(14)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(14)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond1=cy6-cx6*(by(j)-fy(i))/(bx(j)-fx(i))
          cond1=cond1+bx(j)*(by(j)-fy(i))/(bx(j)-fx(i))-by(j)
          if (cond1.gt.0.0)then
            uc1=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
            uc1=uc1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          endif
          cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
            cs2=cs2+sqrt((by(i)-cy6)**2+(bx(i)-cx6)**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
      enddo
* Surface 20 point noc(20)-noc(2)+noc(1)+1 with corner noc(14)
           i=noc(20)-noc(2)+noc(1)+1
           j=noc(14)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
          cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
          cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
          cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
          if (cond3.gt.0.0)then
            cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
            cs2=cs2+sqrt((by(i)-cy6)**2+(bx(i)-cx6)**2)
          else
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 20 from noc(20)-noc(2)+noc(1)+2 with corner noc(14)
        do i=noc(20)-noc(2)+noc(1)+2,noc(20)
           j=noc(14)
          v_f(i,j)=0.0
        enddo
* Surface 20 to noc(20)-noc(2)+noc(1) with corner noc(15)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(15)
          cs1=sqrt((fy(i)-cy8)**2+(fx(i)-cx8)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 20 point noc(20)-noc(2)+noc(1)+1 with corner noc(15)
           i=noc(20)-noc(2)+noc(1)+1
           j=noc(15)
          cs1=sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
          cs1=cs1+sqrt((cy8-cy1)**2+(cx8-cx1)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
          uc1=uc1+sqrt((fy(i)-cy1)**2+(fx(i)-cx1)**2)
          uc2=sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Surface 20 from noc(20)-noc(2)+noc(1)+2 with corner noc(15)
        do i=noc(20)-noc(2)+noc(1)+2,noc(20)
           j=noc(15)
          v_f(i,j)=0.0
        enddo
* Surface 20 point noc(20)-noc(2)+noc(1) with corner noc(16)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)
           j=noc(16)
          cond=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond2=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.gt.0.0)then
              cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
              cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            uc1=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
            uc1=uc1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
            cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
            cs2=cs2+sqrt((by(i)-cy8)**2+(bx(i)-cx8)**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        enddo
* Surface 20 from noc(20)-noc(2)+noc(1)+1 with corner noc(16)
        do i=noc(20)-noc(2)+noc(1)+1,noc(20)
           j=noc(16)
          v_f(i,j)=0.0
        enddo
* Surface 20 with corner noc(17)
        do i=noc(19)+1,noc(20)
           j=noc(17)
          cond=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy8-cx8*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+bx(j)*(fy(j)-by(i))/(fx(j)-bx(i))-by(j)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy8)**2+(fx(i)-cx8)**2)
              uc1=uc1+sqrt((cy8-by(j))**2+(cx8-bx(j))**2)
              cond2=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
                cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
                cs2=cs2+sqrt((by(i)-cy8)**2+(bx(i)-cx8)**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 20 till noc(20)-noc(2)+noc(1)+1 corner noc(18)
        do i=noc(19)+1,noc(20)-noc(2)+noc(1)+1
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo
* Surface 20 from noc(20)-noc(2)+noc(1)+2 with corner noc(18)
        do i=noc(20)-noc(2)+noc(1)+2,noc(20)
           j=noc(18)
          cond=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy1-cx1*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+bx(j)*(by(j)-fy(i))/(bx(j)-fx(i))-by(j)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((fy(i)-by(j))**2+(fx(i)-bx(j))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy1)**2+(fx(i)-cx1)**2)
              uc1=uc1+sqrt((cy1-by(j))**2+(cx1-bx(j))**2)
              cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
                cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
                cs2=cs2+sqrt((by(i)-cy1)**2+(bx(i)-cx1)**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
        enddo
* Surface 20 with noc(19)
        do i=noc(19)+1,noc(20)
           j=noc(19)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
        enddo

      return
      end
*====================================*
      subroutine corner1
      include 'input.in'
* Corner 1 with surface 1
           i=1
        do j=2,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* Corner 1 with i=noc(1)
           i=1
           j=noc(1)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Corner 1 with i=noc(2)
           i=1
           j=noc(2)
          cs1=sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Corner 1 with surface 3 to 17
           i=1
        do j1=3,17
           j=noc(j1)
          v_f(i,j)=0.0
        enddo
* Corner 1 with corner noc(18)
           i=1
           j=noc(18)
          cond=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy1-cx1*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((fy(i)-cy1)**2+(fx(i)-cx1)**2)
              uc1=uc1+sqrt((cy1-by(j))**2+(cx1-bx(j))**2)
              cond2=cy1-cx1*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
                cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
                cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
* Corner 1 with i=noc(19)
           i=1
           j=noc(19)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Corner 1 with i=1
           i=1
           j=1
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
      return
      end
*====================================*
      subroutine corner2
      include 'input.in'
* Corner 2 with surface 1
           i=noc(1)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* Corner 2 with corner 2
           i=noc(1)
           j=noc(2)
          cs1=sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Corner 2 till noc(17)
           i=noc(1)
        do j=noc(2)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* Corner 2 with corner(18)
           i=noc(1)
           j=noc(18)
          cond=cy1-cx1*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond3=cy1-cx1*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.lt.0.0)then
              cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
              cs2=cs2+sqrt((cy1-by(i))**2+(cx1-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            uc1=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
            uc1=uc1+sqrt((cy1-fy(i))**2+(fx(i)-cx1)**2)
            cs1=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
            cs1=cs1+sqrt((cy1-fy(i))**2+(cx1-fx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
* Corner 2 with corner noc(19)
           i=noc(1)
           j=noc(19)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* Corner 2 with corner 2
           i=noc(1)
           j=noc(1)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
      return
      end
*====================================*
      subroutine corner3
      include 'input.in'
* i=noc(2) with i=1
           i=noc(2)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(2) with surface 3 till noc(17)
           i=noc(2)
        do j=noc(2)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(2) with i=noc(18)
           i=noc(2)
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy1)**2+(bx(j)-cx1)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy1)**2+(fx(j)-cx1)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(2) with i=noc(19)
           i=noc(2)
           j=noc(19)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
      return
      end
*====================================*
      subroutine corner4
      include 'input.in'
* i=noc(3) till noc(20)
           i=noc(3)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(3) with i=noc(4)
           i=noc(3)
           j=noc(4)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(3) with i=noc(5)
           i=noc(3)
           j=noc(5)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(3) with i=noc(6)
           i=noc(3)
           j=noc(6)
          cs1=sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          cs1=cs1+sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
          cs2=sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
          cs2=cs2+sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(3) with surface 7-to-17
           i=noc(3)
        do j=noc(6)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(3) with i=noc(18)
           i=noc(3)
           j=noc(18)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          cond1=cy3-cx3*(by(j)-fy(i))/(bx(j)-fx(i))
          cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond1.lt.0.0)then
            uc1=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
            uc1=uc1+sqrt((cy3-fy(i))**2.d0+(cx3-fx(i))**2)
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(3) with i=noc(19)
           i=noc(3)
           j=noc(19)
          cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
      return
      end
*====================================*
      subroutine corner5
      include 'input.in'
* i=noc(4) with till noc(3)-1
           i=noc(4)
        do j=1,noc(3)-1
          v_f(i,j)=0.0
        enddo
* i=noc(4) with surface 4
           i=noc(4)
        do j=noc(3),noc(4)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(4) with surface 5 to all
           i=noc(4)
        do j=noc(4)+1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(4) with i=noc(5)
           i=noc(4)
           j=noc(5)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(4) with i=noc(6)
           i=noc(4)
           j=noc(6)
          cs1=sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(4) with surface 7-to-17
           i=noc(4)
        do j=noc(6)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(4) with i=noc(18)
           i=noc(4)
           j=noc(18)
          cond=cy3-cx3*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy3-cx3*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
              uc1=uc1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
              cond2=cy3-cx3*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
                cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy3-cx3*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
                cs2=cs2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
* i=noc(4) with i=noc(19)
           i=noc(4)
           j=noc(19)
          cond=cy2-cx2*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
        if (cond.ge.0.0)then
          v_f(i,j)=0.0
        else
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cond1=cy2-cx2*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond1.le.0.0)then
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          else
            uc2=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
            uc2=uc2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
            cond2=cy2-cx2*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.gt.0.0)then
              cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
              cs1=cs1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.gt.0.0)then
              cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
              cs2=cs2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        endif
* i=noc(4) with i=noc(4)
           i=noc(4)
           j=noc(4)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
      return
      end
*====================================*
      subroutine corner6
      include 'input.in'
* i=noc(5) with till noc(3)-1
           i=noc(5)
        do j=1,noc(3)-1
          v_f(i,j)=0.0
        enddo
* i=noc(5) with corner noc(3)
           i=noc(5)
        do j=noc(3),noc(5)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(5) with i=noc(5)
           i=noc(5)
           j=noc(5)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
* i=noc(5) with surface 6
           i=noc(5)
        do j=noc(5)+1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(5) with i=noc(6)
           i=noc(5)
           j=noc(6)
          cs1=sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(5) with surface 7-to-17
           i=noc(5)
        do j=noc(6)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(5) with i=noc(18)
           i=noc(5)
           j=noc(18)
          cond=cy3-cx3*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond3=cy3-cx3*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.lt.0.0)then
              cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
              cs2=cs2+sqrt((cy3-by(i))**2+(cx3-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            uc1=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
            uc1=uc1+sqrt((cy3-fy(i))**2+(fx(i)-cx3)**2)
            cs1=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
            cs1=cs1+sqrt((cy3-fy(i))**2+(cx3-fx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
* i=noc(5) with i=noc(19)
           i=noc(5)
           j=noc(19)
          cond=cy2-cx2*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy2-cx2*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
              uc2=uc2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
              cond2=cy2-cx2*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
                cs1=cs1+sqrt((cy2-fy(i))**2+(cx2-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
                cs2=cs2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
* i=noc(5) with i=noc(5)
           i=noc(5)
           j=noc(5)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
      return
      end
*====================================*
      subroutine corner7
      include 'input.in'
* i=noc(6) with till noc(3)-1
           i=noc(6)
        do j=1,noc(3)-1
          v_f(i,j)=0.0
        enddo
* i=noc(6) with i=noc(3) to noc(6)-1
           i=noc(6)
        do j=noc(3),noc(6)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(6) with i=noc(6)
           i=noc(6)
           j=noc(6)
          v_f(i,j)=0.0
* i=noc(6) with surface 7-to-17
           i=noc(6)
        do j=noc(6)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(6) with surface 17 to noc(20)
           i=noc(6)
        do j=noc(17)+1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(6) with i=noc(18)
           i=noc(6)
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy3)**2+(bx(j)-cx3)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy3)**2+(fx(j)-cx3)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(6) with i=noc(19)
           i=noc(6)
           j=noc(19)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cond1=cy2-cx2*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond1.le.0.0)then
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          else
            uc2=sqrt((fy(j)-cy2)**2+(fx(j)-cx2)**2)
            uc2=uc2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
            cond3=cy2-cx2*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.gt.0.0)then
              cs2=sqrt((by(j)-cy2)**2+(bx(j)-cx2)**2)
              cs2=cs2+sqrt((cy2-by(i))**2+(cx2-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
      return
      end
*====================================*
      subroutine corner8
      include 'input.in'
* i=noc(7) till noc(7)-1
           i=noc(7)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(7) with i=noc(8)
           i=noc(7)
           j=noc(8)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(7) with i=noc(9)
           i=noc(7)
           j=noc(9)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(7) with i=noc(10)
           i=noc(7)
           j=noc(10)
          cs1=sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          cs1=cs1+sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
          cs2=sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
          cs2=cs2+sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(7) with surface 11-to-17
           i=noc(7)
        do j=noc(10)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(7) with i=noc(18)
           i=noc(7)
           j=noc(18)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          cond1=cy5-cx5*(by(j)-fy(i))/(bx(j)-fx(i))
          cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond1.lt.0.0)then
            uc1=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
            uc1=uc1+sqrt((cy5-fy(i))**2.d0+(cx5-fx(i))**2)
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(7) with i=noc(19)
           i=noc(7)
           j=noc(19)
          cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
      return
      end
*====================================*
      subroutine corner9
      include 'input.in'
* i=noc(8) with till noc(7)-1
           i=noc(8)
        do j=1,noc(7)-1
          v_f(i,j)=0.0
        enddo
* i=noc(8) with surface 8
           i=noc(8)
        do j=noc(7),noc(8)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(8) with i=noc(8)
           i=noc(8)
           j=noc(8)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
* i=noc(8) with surface 9 to all
           i=noc(8)
        do j=noc(8)+1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(8) with i=noc(9)
           i=noc(8)
           j=noc(9)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(8) with i=noc(10)
           i=noc(8)
           j=noc(10)
          cs1=sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(8) with surface 11-to-17
           i=noc(8)
        do j=noc(10)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(8) with i=noc(18)
           i=noc(8)
           j=noc(18)
          cond=cy5-cx5*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy5-cx5*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
              uc1=uc1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
              cond2=cy5-cx5*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
                cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy5-cx5*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
                cs2=cs2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
* i=noc(8) with i=noc(19)
           i=noc(8)
           j=noc(19)
          cond=cy4-cx4*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
        if (cond.ge.0.0)then
          v_f(i,j)=0.0
        else
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cond1=cy4-cx4*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond1.le.0.0)then
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          else
            uc2=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
            uc2=uc2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
            cond2=cy4-cx4*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.gt.0.0)then
              cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
              cs1=cs1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.gt.0.0)then
              cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
              cs2=cs2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        endif
      return
      end
*====================================*
      subroutine corner10
      include 'input.in'
* i=noc(9) with till noc(7)-1
           i=noc(9)
        do j=1,noc(7)-1
          v_f(i,j)=0.0
        enddo
* i=noc(9) with corner i=noc(7)
           i=noc(9)
        do j=noc(7),noc(9)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(9) with i=noc(9)
           i=noc(9)
           j=noc(9)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
* i=noc(9) with surface 10
           i=noc(9)
        do j=noc(9)+1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(9) with i=noc(10)
           i=noc(9)
           j=noc(10)
          cs1=sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(9) with surface 11-to-17
          i=noc(9)
        do j=noc(10)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(9) with i=noc(18)
           i=noc(9)
           j=noc(18)
          cond=cy5-cx5*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond3=cy5-cx5*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.lt.0.0)then
              cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
              cs2=cs2+sqrt((cy5-by(i))**2+(cx5-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            uc1=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
            uc1=uc1+sqrt((cy5-fy(i))**2+(fx(i)-cx5)**2)
            cs1=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
            cs1=cs1+sqrt((cy5-fy(i))**2+(cx5-fx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
* i=noc(9) with i=noc(19)
           i=noc(9)
           j=noc(19)
          cond=cy4-cx4*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy4-cx4*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
              uc2=uc2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
              cond2=cy4-cx4*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
                cs1=cs1+sqrt((cy4-fy(i))**2+(cx4-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
                cs2=cs2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
      return
      end
*====================================*
      subroutine corner11
      include 'input.in'
* i=noc(10) with till noc(7)-1
           i=noc(10)
        do j=1,noc(7)-1
          v_f(i,j)=0.0
        enddo
* i=noc(10) with i=noc(7) to noc(10)-1
           i=noc(10)
        do j=noc(7),noc(10)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(10) with i=noc(10)
           i=noc(10)
           j=noc(10)
          v_f(i,j)=0.0
* i=noc(10) with surface 11-to-17
           i=noc(10)
        do j=noc(10)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(10) with surface 17 to noc(20)
           i=noc(10)
        do j=noc(17)+1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(10) with i=noc(18)
           i=noc(10)
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy5)**2+(bx(j)-cx5)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy5)**2+(fx(j)-cx5)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(10) with i=noc(19)
           i=noc(10)
           j=noc(19)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cond1=cy4-cx4*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond1.le.0.0)then
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          else
            uc2=sqrt((fy(j)-cy4)**2+(fx(j)-cx4)**2)
            uc2=uc2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
            cond3=cy4-cx4*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.gt.0.0)then
              cs2=sqrt((by(j)-cy4)**2+(bx(j)-cx4)**2)
              cs2=cs2+sqrt((cy4-by(i))**2+(cx4-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
      return
      end
*====================================*
      subroutine corner12
      include 'input.in'
* i=noc(11) with other surfaces
           i=noc(11)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(11) with i=noc(12)
           i=noc(11)
           j=noc(12)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(11) with i=noc(13)
           i=noc(11)
           j=noc(13)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(11) with i=noc(14)
           i=noc(11)
           j=noc(14)
          cs1=sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          cs1=cs1+sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
          cs2=sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
          cs2=cs2+sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(11) with surface 15-to-17
           i=noc(11)
        do j=noc(14)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(11) with i=noc(18)
           i=noc(11)
           j=noc(18)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
          cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
          if (cond2.lt.0.0)then
            cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          else
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          endif
          cond1=cy7-cx7*(by(j)-fy(i))/(bx(j)-fx(i))
          cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond1.lt.0.0)then
            uc1=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
            uc1=uc1+sqrt((cy7-fy(i))**2.d0+(cx7-fx(i))**2)
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          endif
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(11) with i=noc(19)
           i=noc(11)
           j=noc(19)
          cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
      return
      end
*====================================*
      subroutine corner13
      include 'input.in'
* i=noc(12) with till noc(11)-1
           i=noc(12)
        do j=1,noc(11)-1
          v_f(i,j)=0.0
        enddo
* i=noc(12) with surface 12
           i=noc(12)
        do j=noc(11),noc(12)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(12) with i=noc(12)
           i=noc(12)
           j=noc(12)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
* i=noc(12) with surface 13 to all
           i=noc(12)
        do j=noc(12)+1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(12) with i=noc(13)
           i=noc(12)
           j=noc(13)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(12) with i=noc(14)
           i=noc(12)
           j=noc(14)
          cs1=sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(12) with surface 11-to-17
           i=noc(12)
        do j=noc(14)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(12) with i=noc(18)
           i=noc(12)
           j=noc(18)
          cond=cy7-cx7*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond1=cy7-cx7*(by(j)-fy(i))/(bx(j)-fx(i))
            cond1=cond1+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
            if (cond1.ge.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc1=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
              uc1=uc1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
              cond2=cy7-cx7*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.lt.0.0)then
                cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
                cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy7-cx7*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.lt.0.0)then
                cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
                cs2=cs2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
* i=noc(12) with i=noc(19)
           i=noc(12)
           j=noc(19)
          cond=cy6-cx6*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
        if (cond.ge.0.0)then
          v_f(i,j)=0.0
        else
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cond1=cy6-cx6*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond1.le.0.0)then
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          else
            uc2=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
            uc2=uc2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
            cond2=cy6-cx6*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.gt.0.0)then
              cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
              cs1=cs1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.gt.0.0)then
              cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
              cs2=cs2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        endif
      return
      end
*====================================*
      subroutine corner14
      include 'input.in'
* i=noc(13) with till noc(11)-1
           i=noc(13)
        do j=1,noc(11)-1
          v_f(i,j)=0.0
        enddo
* i=noc(13) with i=noc(11)
           i=noc(13)
        do j=noc(11),noc(13)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(13) with i=noc(13)
           i=noc(13)
           j=noc(13)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
* i=noc(13) with surface 14
           i=noc(13)
        do j=noc(13)+1,noc(14)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(13) with i=noc(14)
           i=noc(13)
           j=noc(14)
          cs1=sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(13) with surface 15-to-17
           i=noc(13)
        do j=noc(14)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(13) with i=noc(18)
           i=noc(13)
           j=noc(18)
          cond=cy7-cx7*(fy(j)-by(i))/(fx(j)-bx(i))
          cond=cond+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond.le.0.0)then
            v_f(i,j)=0.0
          else
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            cond3=cy7-cx7*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.lt.0.0)then
              cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
              cs2=cs2+sqrt((cy7-by(i))**2+(cx7-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            uc1=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
            uc1=uc1+sqrt((cy7-fy(i))**2+(fx(i)-cx7)**2)
            cs1=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
            cs1=cs1+sqrt((cy7-fy(i))**2+(cx7-fx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
* i=noc(13) with i=noc(19)
           i=noc(13)
           j=noc(19)
          cond=cy6-cx6*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy6-cx6*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
              uc2=uc2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
              cond2=cy6-cx6*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
                cs1=cs1+sqrt((cy6-fy(i))**2+(cx6-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
                cs2=cs2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
      return
      end
*====================================*
      subroutine corner15
      include 'input.in'
* i=noc(14) with till noc(11)-1
           i=noc(14)
        do j=1,noc(11)-1
          v_f(i,j)=0.0
        enddo
* i=noc(14) with i=noc(11) to noc(14)-1
           i=noc(14)
        do j=noc(11),noc(14)-1
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(14) with i=noc(14)
           i=noc(14)
           j=noc(14)
          v_f(i,j)=0.0
* i=noc(14) with surface 15-to-17
           i=noc(14)
        do j=noc(14)+1,noc(17)
          v_f(i,j)=0.0
        enddo
* i=noc(14) with surface 17 to noc(20)
           i=noc(14)
        do j=noc(17)+1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(14) with i=noc(18)
           i=noc(14)
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy7)**2+(bx(j)-cx7)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy7)**2+(fx(j)-cx7)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(14) with i=noc(19)
           i=noc(14)
           j=noc(19)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cond1=cy6-cx6*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond1.le.0.0)then
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          else
            uc2=sqrt((fy(j)-cy6)**2+(fx(j)-cx6)**2)
            uc2=uc2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
            cond3=cy6-cx6*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.gt.0.0)then
              cs2=sqrt((by(j)-cy6)**2+(bx(j)-cx6)**2)
              cs2=cs2+sqrt((cy6-by(i))**2+(cx6-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
      return
      end
*====================================*
      subroutine corner16
      include 'input.in'
* i=noc(15) with other surfaces
           i=noc(15)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(15) with i=noc(16)
           i=noc(15)
           j=noc(16)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(15) with i=noc(16)
           i=noc(15)
           j=noc(16)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(15) with i=noc(17)
           i=noc(15)
           j=noc(17)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(15) with i=noc(18)
           i=noc(15)
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(15) with i=noc(19)
           i=noc(15)
           j=noc(19)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
      return
      end
*====================================*
      subroutine corner17
      include 'input.in'
* i=noc(16) with all surfaces
           i=noc(16)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(16) with i=noc(16)
           i=noc(16)
           j=noc(16)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
* i=noc(16) with i=noc(17)
           i=noc(16)
           j=noc(17)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(16) with i=noc(18)
           i=noc(16)
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(16) with i=noc(19)
           i=noc(16)
           j=noc(19)
          cond=cy8-cx8*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
        if (cond.ge.0.0)then
          v_f(i,j)=0.0
        else
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          cond1=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
          cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
          if (cond1.le.0.0)then
            cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          else
            uc2=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
            uc2=uc2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
            cond2=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
            cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
            if (cond2.gt.0.0)then
              cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
              cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
            else
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
            endif
            cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
            cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
            if (cond3.gt.0.0)then
              cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
              cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
            else
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
            endif
            v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
          endif
        endif
      return
      end
*====================================*
      subroutine corner18
      include 'input.in'
* i=noc(17) with till noc(20)
           i=noc(17)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(17) with i=noc(17)
           i=noc(17)
           j=noc(17)
          cs1=sqrt((fy(i)-by(i))**2+(fx(i)-bx(i))**2)
          v_f(i,j)=1.0-(cs1/ds(i))
* i=noc(17) with i=noc(18)
           i=noc(17)
           j=noc(18)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
* i=noc(17) with i=noc(19)
           i=noc(17)
           j=noc(19)
          cond=cy8-cx8*(by(j)-fy(i))/(bx(j)-fx(i))
          cond=cond+fx(i)*(by(j)-fy(i))/(bx(j)-fx(i))-fy(i)
          if (cond.ge.0.0)then
            v_f(i,j)=0.0
          else
            uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
            cond1=cy8-cx8*(fy(j)-by(i))/(fx(j)-bx(i))
            cond1=cond1+bx(i)*(fy(j)-by(i))/(fx(j)-bx(i))-by(i)
            if (cond1.le.0.0)then
              cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            else
              uc2=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
              uc2=uc2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
              cond2=cy8-cx8*(fy(j)-fy(i))/(fx(j)-fx(i))
              cond2=cond2+fx(i)*(fy(j)-fy(i))/(fx(j)-fx(i))-fy(i)
              if (cond2.gt.0.0)then
                cs1=sqrt((fy(j)-cy8)**2+(fx(j)-cx8)**2)
                cs1=cs1+sqrt((cy8-fy(i))**2+(cx8-fx(i))**2)
              else
                cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
              endif
              cond3=cy8-cx8*(by(j)-by(i))/(bx(j)-bx(i))
              cond3=cond3+bx(i)*(by(j)-by(i))/(bx(j)-bx(i))-by(i)
              if (cond3.gt.0.0)then
                cs2=sqrt((by(j)-cy8)**2+(bx(j)-cx8)**2)
                cs2=cs2+sqrt((cy8-by(i))**2+(cx8-bx(i))**2)
              else
                cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
              endif
              v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
            endif
          endif
      return
      end
*====================================*
      subroutine corner19
      include 'input.in'
* i=noc(18) with till noc(20)
           i=noc(18)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
* i=noc(18) with j=noc(19)
           i=noc(18)
           j=noc(19)
          cs1=sqrt((fy(j)-fy(i))**2+(fx(j)-fx(i))**2)
          cs2=sqrt((by(j)-by(i))**2+(bx(j)-bx(i))**2)
          uc1=sqrt((by(j)-fy(i))**2+(bx(j)-fx(i))**2)
          uc2=sqrt((fy(j)-by(i))**2+(fx(j)-bx(i))**2)
          v_f(i,j)=0.5*(cs1+cs2-uc1-uc2)/ds(i)
      return
      end
*====================================*
      subroutine corner20
      include 'input.in'
* i=noc(19) with all surfaces
           i=noc(19)
        do j=1,noc(20)
          v_f(i,j)=v_f(j,i)*ds(j)/ds(i)
        enddo
      return
      end
*====================================*
      subroutine output
      include 'input.in'

      open(2,file='vf_short.dat')
        write(2,20)
20      format(15x,'fixed_vf')

        do i=1,noc(20)
        do j=1,noc(20)
          add(i)=add(i)+v_f(i,j)
        enddo
        enddo

        do i=noc(19)+1,noc(20)
        do j=1,noc(20)
          write(2,30)i,j,v_f(i,j),add(i)
30        format(1x,i4,3x,i4,4x,d16.8,4x,d16.8)
        enddo
        enddo
      close(2)

      open(3,file='full_vf.dat')
        write(3,100)
100     format(15x,'shape factor sums')
        do i=1,noc(20)
        do j=1,noc(20)
          add(i)=add(i)+v_f(i,j)
        enddo
        enddo

        do i=1,noc(20)
        do j=1,noc(20)
          write(3,102)i,j,v_f(i,j),add(i)
102       format(1x,i4,3x,i4,4x,d16.8,4x,d16.8)
        enddo
        enddo
      close(3)

      open(4,file='short_vf.dat')
        write(4,103)
103     format(15x,'summation of view factors')
        do i=1,noc(20)
          write (4,104)i,add(i)
104       format(1x,i4,4x,d16.8)
        enddo
      close(4)

      return
      end
*====================================*
      subroutine ab_factor
      include 'input.in'
* Calculation for absorption factor Knowing viewfactor
* Defining values of emissivity
        do i=1,noc(20)
          if (i.gt.noc(1).and.(i.lt.noc(4)))then
            em(i)=roc
          elseif (i.gt.noc(5).and.(i.lt.noc(8)))then
            em(i)=roc
          elseif (i.gt.noc(9).and.(i.lt.noc(12)))then
            em(i)=roc
          elseif (i.gt.noc(13).and.(i.lt.noc(16)))then
            em(i)=roc
          elseif (i.gt.noc(17).and.(i.lt.noc(18)))then
            em(i)=roe
          elseif (i.gt.noc(19))then
            em(i)=roe
          else
            em(i)=rob
          endif
        enddo
		
        do i=1,noc(20)
          if (i.eq.1)then
            em(i)=(rob*dx1+dy1*roe)/(2.0*ds(i))
          elseif (i.eq.noc(1))then
            em(i)=(rob*dx1+dy2*roc)/(2.0*ds(i))
          elseif (i.eq.noc(4))then
            em(i)=(rob*dx1+dy2*roc)/(2.0*ds(i))
          elseif (i.eq.noc(5))then
            em(i)=(rob*dx1+dy2*roc)/(2.0*ds(i))
          elseif (i.eq.noc(8))then
            em(i)=(rob*dx1+dy2*roc)/(2.0*ds(i))
          elseif (i.eq.noc(9))then
            em(i)=(rob*dx1+dy2*roc)/(2.0*ds(i))
          elseif (i.eq.noc(12))then
            em(i)=(rob*dx1+dy2*roc)/(2.0*ds(i))
          elseif (i.eq.noc(13))then
            em(i)=(rob*dx1+dy2*roc)/(2.0*ds(i))
          elseif (i.eq.noc(16))then
            em(i)=(rob*dx1+dy2*roc)/(2.0*ds(i))
          elseif (i.eq.noc(17))then
            em(i)=(roe*dx1+dy3*rob)/(2.0*ds(i))
          elseif (i.eq.noc(18))then
            em(i)=(roe*dx3+dy3*rob)/(2.0*ds(i))
          elseif (i.eq.noc(19))then
            em(i)=(roe*dx3+dy1*rob)/(2.0*ds(i))
          endif
        enddo

* Defining values of reflectivity
        do i=1,noc(20)
          ro(i)=1-em(i)
        enddo

* Defining number of times to call gauss-siedel matrix
        do i1=1,noc(20)

* Defining cofficient of matrix
          do i=1,noc(20)
          do j=1,noc(20)+1
            if ((j.ne.noc(20)+1).and.(i1.eq.1))then
              if (i.ne.j) then
                a(i,j)=v_f(i,j)*ro(j)
              elseif (i.eq.j) then
                a(i,j)=v_f(i,j)*ro(j)-1.0
              endif
            elseif (j.eq.noc(20)+1) then
              a(i,j)=-v_f(i,i1)*em(i1)
            endif
          enddo
          enddo
* Defining old and new absorption factor
          do i=1,noc(20)
            a_fo(i,i1)=0.0
            a_f(i,i1)=0.0
          enddo
          iteration=0
700       iteration=iteration+1

          do i=1,noc(20)
            a_f(i,i1)=a(i,noc(20)+1)
            do j=1,noc(20)
              if (i.ne.j) then
                a_f(i,i1)=a_f(i,i1)-a(i,j)*a_f(j,i1)
              endif
            enddo
            a_f(i,i1)=a_f(i,i1)/a(i,i)
          enddo

          do i=1,noc(20)
            if (abs(a_f(i,i1)-a_fo(i,i1)).gt.0.0) then
* Giving absorption factor new values
              do j=1,noc(20)
                a_fo(j,i1)=a_f(j,i1)
              enddo
            goto 700
            endif
          enddo

        enddo

* Output file for result on absoroption factor

        do i=1,noc(20)
          ad(i)=0.0
        enddo

        open(4,file='z_absorption-factor.dat')
          do i=1,noc(20)
          do j=1,noc(20)
            ad(i)=ad(i)+a_f(i,j)
          enddo
          enddo

        write(4,120)
120     format(15x,'absorption factor sums')
          do i=1,noc(20)
*      do j=1,noc(20)
*      write(4,122)i,j,a_f(i,j),ad(i)
*122   format(1x,i4,3x,i4,4x,d16.8,4x,d16.8)
*      enddo
          write(4,122)i,ad(i)
122       format(1x,i4,4x,d16.8)

          enddo
        close(4)

        open(5,file='z_ro_em.dat')
        write(5,124)
124     format(4x,'reflectivity and emissivity')
          do j=1,noc(20)
          write(5,126)j,ro(j),em(j)
126       format(1x,i4,4x,d16.8,4x,d16.8)
          enddo
        close(5)

        open (7,file='a_f.dat')
          do i=1,noc(20)
          do j=1,noc(20)
            write(7,150)a_f(i,j)
150         format(d16.8)
          enddo
          enddo
        close (7)

      return
      end
*====================================*
