! p42 4-2
! 4text2.f90
! The latest version (2024/12/31)

program practice

	implicit none
	real, parameter :: sng = 0.1e0                            ! single precision
	real(8), parameter :: dbl = 0.1d0                         ! double precision 
	real(8) :: double_sng
 
	double_sng = sng                                          ! Change single into double
	write(*,*)"[from single to double] 0.1e0 = ", double_sng
	write(*,*)"[double               ] 0.1d0 = ", dbl

end program practice
