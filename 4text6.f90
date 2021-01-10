! p42 4-6
!
! The latest version (2016/10/19)

program practice
implicit none
 
 real::r1,r2,i1,i2  
 complex::z1,z2
 logical::h

 write(*,*)'Input a complex number z1.'
 write(*,*)'r1 = ?'
 read(*,*)r1
 write(*,*)'i1 = ?'
 read(*,*)i1

 write(*,*)'Input a complex number z2.'
 write(*,*)'r2 = ?'
 read(*,*)r2
 write(*,*)'i2 = ?'
 read(*,*)i2
 
 z1=cmplx(r1,i1)
 z2=cmplx(r2,i2)
 h=(abs(z1+z2)<=abs(z1)+abs(z2))

 write(*,*)'True (T) or False (F) ?'
 write(*,*)h

end program practice
