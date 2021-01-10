! Page 140  8-2
! Cross product
! The latest version (2017/11/16)

! ------------Subroutine--------------
subroutine cross_product(p,q,r)
implicit none

 real,dimension(1:3),intent(in)::p,q
 real,dimension(1:3),intent(out)::r

 r(1)=p(2)*q(3)-p(3)*q(2)
 r(2)=p(3)*q(1)-p(1)*q(3)
 r(3)=p(1)*q(2)-p(2)*q(1)

end subroutine cross_product
! --------------------------------------

! -------------Function-----------------
function func_cross(x,y) result (z)
implicit none

 real,dimension(1:3),intent(in)::x,y
 real,dimension(1:3)::z                     ! 'intent' is not necessary.

 z(1)=x(2)*y(3)-x(3)*y(2)
 z(2)=x(3)*y(1)-x(1)*y(3)
 z(3)=x(1)*y(2)-x(2)*y(1)

end function func_cross
! ---------------------------------------

! -----------------Main------------------ 
program practice
implicit none

 real,dimension(1:3)::a,b,sub_cross

 interface                                  ! To use the array
   function func_cross(x,y) result(z)
     real,dimension(1:3),intent(in)::x,y
     real,dimension(1:3)::z
   end function
 end interface

 write(*,'(a)',advance='no')"input a :"
 read(*,*)a
 write(*,'(a)',advance='no')"input b :"
 read(*,*)b

 call cross_product(a,b,sub_cross)          ! Subroutine should be called when you use it.               
 write(*,*)'The cross product of a and b is ',sub_cross
 write(*,*)'The cross product of a and b is ',func_cross(a,b)

end program practice








