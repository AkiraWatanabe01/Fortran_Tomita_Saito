! Page57 5-7
! 5text7.f90
! The latest version (2025/1/4)

program practice

	implicit none
	integer :: n, m
	integer :: num, remainder, counter

	write(*,*)"Input a positive integer."
	read(*,*)num

	if (num == 1) then
		write(*,*)1
	end if

	do n = 2, num
		remainder = mod(num, n)
		if (remainder /= 0) cycle
		counter = 0

		do while (mod(num, n) == 0)
			num = num / n
			counter = counter + 1
		end do

		write(*,*)(n, m = 1, counter)
	end do

end program practice
