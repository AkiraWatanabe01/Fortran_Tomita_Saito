! p16 2-3
! 2text3.f90
! the latest version (2024/9/20)

program practice
implicit none

	real::A,V,n

	A = 6.02e+23
	V = 22.4e0
	n = A / V * 1.0e-3   

	write(*,*)n

end program practice
