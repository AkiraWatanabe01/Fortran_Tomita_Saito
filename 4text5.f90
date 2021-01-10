! p42 4-5
!
! The latest version (2016/10/14)

program practice
implicit none
 
 real::x,y,r,num1,num2,num3,num4   
 complex::z

 write(*,*)'Input two real numbers x and y.'
 write(*,*)'x = ?'
 read(*,*)x
 write(*,*)'y = ?'
 read(*,*)y
 
 z=cmplx(x,y)
 r=sqrt(x**2+y**2)

 num1=x**2-y**2
 num2=2*x*y
 num3=x/r**2
 num4=-y/r**2

 write(*,*)'Z**2 =',z**2
 write(*,*)'1/z =',1/z
 write(*,*)'(x**2-y**2,2*x*y) = (',num1,',',num2,')'
 write(*,*)'(x/r**2,-y/r**2) = (',num3,',',num4,')'

end program practice
