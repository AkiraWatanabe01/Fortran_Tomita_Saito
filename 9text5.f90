! 9-5
!making 2017/2/24
!upgrade

module vector_operator

 implicit none
 type vector
      real::x,y,z
 end type

 interface operator(.cross.)
    module procedure cross_product
 end interface

contains
  
 function cross_product(u,v) result(w)
 
  implicit none
  type(vector),intent(in)::u,v
  type(vector)::w
  
  w%x=u%y*v%z-u%z*v%y
  w%y=u%z*v%x-u%x*v%z
  w%z=u%x*v%y-u%y*v%x

 end function cross_product

end module vector_operator

program practice

 use vector_operator,only:vector,operator(.cross.)
 implicit none
 type(vector)::v1,v2,sol

 write(*,*)"Input a vector"
 read(*,*)v1 
 write(*,*)"Input the other vector"
 read(*,*)v2 

 sol=v1.cross.v2

 write(*,'(1x,a,3f7.2,a)')"Solution Component = (",sol,")"

end program practice
