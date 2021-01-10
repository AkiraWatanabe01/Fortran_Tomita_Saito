! page8 1-5
!
! The latest version (2016/10/7) 

program practice
implicit none

 real::L,T,pi,g

 pi=3.1415926e0       ! Define pi
 g=9.8e0              ! Define g

 write(*,*)'Input the length of the string [m].'
 read(*,*)L

 T=2*pi*(L/g)**0.5e0

 write(*,*)'The period is',T,'s'   ! 'words',variable,'words'

end program practice
