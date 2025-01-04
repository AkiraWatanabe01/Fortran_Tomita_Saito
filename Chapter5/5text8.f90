! Page56 5-8
! 5text8.f90
! The latest version (2025/1/4)

program practice

	implicit none
	integer :: i
 	integer :: num, sum

	do
		write(*,*)"Input a positive integer or 0 to stop :"
		read(*,*)num
  		if (num < 0) stop "Input a POSITIVE integer."
		if (num == 0) exit

		! The sum of divisors
		sum = 1 + num
		do i = 2, num - 1
			if (mod(num, i) /= 0) cycle
			sum = sum + i
		end do

		write(*,*)"The sum of divisors of ", num, "is ", sum
	end do

end program practice
