! p42 4-3
!
! The latest version (2016/10/14)

program practice
implicit none

 integer::n 
 real::spr
 real(8)::dpr,dspr

 write(*,*)'Input integer n.'
 read(*,*)n
 
 spr=sqrt(real(n))
 dpr=sqrt(real(n))
 dspr=spr**2                         ! Change single into double

 write(*,*)spr
 write(*,*)dpr
 write(*,*)dspr


end program practice
