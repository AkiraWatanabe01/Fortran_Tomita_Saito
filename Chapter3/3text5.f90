! p28 3-5
! 3text5.f90
! The latest version (2024/9/21)

program practice
implicit none
 
	real::x, decimal_part

	write(*,*)'Input real number x'
 	read(*,*)x

	decimal_part = abs(x) - aint(abs(x))
	if ((x < 0e0).and.(decimal_part /= 0e0)) then
		decimal_part = 1e0 - decimal_part
	end if

	write(*,*)decimal_part

end program practice
