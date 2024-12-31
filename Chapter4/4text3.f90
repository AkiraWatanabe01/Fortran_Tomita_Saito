! p42 4-3
! 4text3.f90
! The latest version (2024/12/31)

program practice

	implicit none
	integer :: n 
	real :: single_precision_root
	real(8) :: double_precision_root, doubled_single_precision_root

	write(*,*)"Input integer n."
	read(*,*)n

	single_precision_root = sqrt(real(n))
	double_precision_root = sqrt(real(n))
	doubled_single_precision_root = single_precision_root ** 2d0            ! Change single into double

	write(*,*)"single_precision_root = ", single_precision_root
	write(*,*)"double_precision_root = ", double_precision_root
	write(*,*)"doubled_single_precision_root = ", doubled_single_precision_root

end program practice
