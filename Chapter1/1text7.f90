! page8 1-7
! 1text7.f90
! The latest version (2024/9/16) 

program practice
implicit none

	real::x, y

	write(*,*)'Input the value of x.'
	read(*,*)x

	! Calculation
	y = 1 + x * (1 + x * (2 + x * (3 + x * (4 + 5 * x)))) 

	write(*,*)'y = ', y

end program practice
