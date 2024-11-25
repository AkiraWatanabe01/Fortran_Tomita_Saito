! page8 1-3
! 1text3.f90
! The latest version (2024/11/25)

program practice

	implicit none
	real :: w, h, BMI    ! weight, height, BMI               

	write(*,*)"Input your weight[kg]." 
	read(*,*)w

	write(*,*)"Input your height[m]."
	read(*,*)h

	! Calculation  
	! BMI=w/h**2.0e0 is also OK.
	BMI = w / (h * h)

	write(*,*)"Your BMI is ", BMI     

end program practice
