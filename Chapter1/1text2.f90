! page8 1-2
! 1text2.f90
! The latest version (2024/9/16)

program practice
implicit none

	real::C,F               

	write(*,*)'Input Celsius degree.' 
	read(*,*)C     

	! Calculation  
	! Just 1.8 is OK, but 1.8e0 is better than 1.8. The same is true for 32e0.
	F = 1.8 * C + 32.0 

	write(*,*)'Fahrenheit : ',F     

end program practice
