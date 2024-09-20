! p28 3-5
! 3text5.f90
! The latest version (2024/9/21)

program practice
implicit none
 
	real::x, decimal_part

	write(*,*)'Input real number x'
 	read(*,*)x

	if (x >= 0) then
		decimal_part = x - aint(x)
	else
		decimal_part = 1.0 - (abs(x) - aint(abs(x)))
	end if

	write(*,*)decimal_part

end program practice
