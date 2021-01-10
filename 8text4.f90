! Page 140  8-4
! Nibunho
! The latest version (2017/11/17)

! ------------Subroutine--------------
recursive subroutine nibun (p,q,r,s)
implicit none

 real,intent(in)::s
 real(8)::mid,f
 real(8),intent(in)::p,q
 real(8),intent(out)::r

 r=(p+q)/2

 if (abs(f(r,s))<1.0d-6) return          ! To stop this subroutine 

 if (f(p,s)*f(r,s)>0) then
    call nibun((p+q)/2,q,r,s)

 else if (f(r,s)*f(q,s)>0) then
    call nibun(p,(p+q)/2,r,s)

 end if 

end subroutine nibun 
! --------------------------------------

! -------------Function-----------------
function f(a,b) result (c)
implicit none
 real,intent(in)::b
 real(8),intent(in)::a
 real(8)::c

 c=a**2-b                                  ! f(x)=x**2-??

end function f
! ---------------------------------------

! -----------------Main------------------ 
program practice
implicit none
 
 real::num
 real(8)::x0,ini1,ini2,f

 write(*,'(a)',advance='no')"I should calculate the square root of "
 read(*,*)num
 if (num<=0.e0) stop 'Input a positive number!' 

 write(*,'(a)',advance='no')"Input two initial values (left) :"
 read(*,*)ini1
 write(*,'(a)',advance='no')"Input two initial values (right) :"
 read(*,*)ini2

 if (f(ini1,num)>0.and.f(ini2,num)>0) stop 'One of them should be negative.'
 if (f(ini1,num)<0.and.f(ini2,num)<0) stop 'One of them should be positive.'

 call nibun(ini1,ini2,x0,num)
  
 write(*,'(a,F12.8)')'The answer is ',x0

end program practice
