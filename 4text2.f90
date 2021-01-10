! p42 4-2
!
! The latest version (2016/10/14)

program practice
implicit none
 
 real,parameter::sng=0.1e0         ! single precision
 real(8),parameter::dbl=0.1d0      ! double precision 
 real(8)::dsng

 dsng=sng                          ! Change single into double

 write(*,*)dsng
 write(*,*)dbl


end program practice
