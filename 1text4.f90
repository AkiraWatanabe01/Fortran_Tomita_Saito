! page8 1-4
!
! The latest version (2016/10/6) 

program practice
implicit none

 real::r,a,n,s

 write(*,*)"Input the annual interest rate [%]."
 read(*,*)r

 write(*,*)"Input your capital [yen]."
 read(*,*)a

 write(*,*)"How long do you deposit it [year]?"
 read(*,*)n

 s=a*(1+r/100.0e0)**n

 write(*,*)s

end program practice
