! Page56 5-2
! 5text2.f90
! The latest version (2025/1/4)

program practice

	implicit none
	integer :: n
	integer :: x_n, x_one = 1, x_two = 1		! x_one = x_{n-1}, x_two = x_{n-2}
	real(8) :: ratio				! The ratio of x_n and x_n-1

	do n = 2, 45
 		! Fibonacci's sequence
		!  1 + 1 = 2
   		!      1 + 2 = 3
                !          2 + 3 = 5 
		!        :  
		x_n = x_one + x_two
		ratio = real(x_n) / real(x_one)
		x_two = x_one
		x_one = x_n

		! 2 integers, 1 blank, 10 integers, 1 blank, and 18 real numbers(16th decimal place)
		write(*, "(I2, 1X, I10, 1X, F18.16)")n, x_n, ratio
	end do

end program practice
