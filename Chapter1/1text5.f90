! page8 1-5
! 1text5.f90
! The latest version (2024/11/25) 

program practice

	implicit none
	real :: L, T, pi, g

	pi = 3.1415926       ! Define pi (3.1415926e0)
	g = 9.8              ! Define g  (9.8e0)

	write(*,*)"Input the length of the string [m]."
	read(*,*)L

	! Calculation
	T = 2 * pi * (L / g) ** 0.5    ! 0.5e0 is better.

	write(*,*)"The period is", T, "s"   ! "words", variable, "words"

end program practice
