! p28 3-5
!
! The latest version (2016/10/10)

program practice
implicit none
 
 real::x,dis

 write(*,*)'Input real number x'
 read(*,*)x

  if (x>=0) then
     dis=x-aint(x)
     write(*,*)dis

  else
     dis=1.0-(abs(x)-aint(abs(x)))
     write(*,*)dis

  end if



end program practice
