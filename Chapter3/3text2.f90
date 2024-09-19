! p28 3-2
! 3text2.f90
! The latest version (2024/9/20)

program practice
implicit none

	real::height, weight, BMI

	write(*,*)'What is your height [m]?'
	read(*,*)height
 
	write(*,*)'What is your weight [kg]?'
	read(*,*)weight

	! Calculation
	BMI = weight / (height * height)

	write(*,*)'Your BMI is', BMI, '.'
	if (BMI <= 18.5e0) then
		write(*,*)'So, you are thin.'
	else if ((BMI > 18.5e0).and.(BMI < 25e0)) then 
		write(*,*)'So, you are normal.'
	else
		write(*,*)'So, you are overweight.'
	end if

end program practice
