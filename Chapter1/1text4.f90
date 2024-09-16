! page8 1-4
! 1text4.f90
! The latest version (2024/9/16) 

program practice
implicit none

	real::r, a, n, s

	write(*,*)"Input the annual interest rate [%]."
	read(*,*)r

	write(*,*)"Input your capital [yen]."
	read(*,*)a

	write(*,*)"How long do you deposit it [year]?"
	read(*,*)n

	! Calculation
	s = a * (1 + r / 100.0) ** n     ! 100e0 is better

	write(*,*)s

end program practice
