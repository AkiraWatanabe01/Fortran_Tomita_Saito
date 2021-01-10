! Page 140  8-3
! Runge Kutta
! The latest version (2016/11/4)

! ------------Subroutine--------------
subroutine rungekutta (p,q,r)
implicit none

 real(8)::d1,d2,d3,d4,f
 real,parameter::h=1.0e-3
 real(8),intent(in)::p,q
 real(8),intent(out)::r

 d1=0
 d2=0
 d3=0
 d4=0
 r=0

 d1=f(p,q)
 d2=f(p+h/2,q+d1/2)
 d3=f(p+h/2,q+d2/2)
 d4=f(p+h/2,q+d3)

 r=h*(d1+2*d2+2*d3+d4)/6

end subroutine rungekutta
! --------------------------------------

! -------------Function-----------------
function f(a,b) result (c)
implicit none

 real(8),intent(in)::a,b
 real(8)::c

 c=-a/b                                  ! f(x,y)=-x/y

end function f
! ---------------------------------------

! -----------------Main------------------ 
program practice
implicit none

 integer::x,y,i
 real(8)::x0,y0,x1,y1,inc
 real,parameter::hp=1.0e-3

 write(*,'(a)',advance='no')"input x :"
 read(*,*)x
 write(*,'(a)',advance='no')"input y :"
 read(*,*)y

 x0=dble(x)
 y0=dble(y)
 y1=y0

 write(*,'(F10.6,1X,F10.6)')x0,y0

 do i=1,10000
    x1=x0+i*hp
  
    call rungekutta(x1,y1,inc)
  
    y1=y1+inc 
    write(*,'(F10.6,1X,F10.6)')x1,y1
 end do


end program practice
