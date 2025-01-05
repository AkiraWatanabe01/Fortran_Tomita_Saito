! Page 85 6-6
! 6text6.f90 
! The latest version (2025/1/5)

program practice

	implicit none
	integer :: i, j
 	integer :: num
	integer :: interval(1:20) = (/ (0, i = 1, 20) /)
	real(8) :: x
	real(8) :: point(0:20)

	do i = 0, 20
		point(i) = 1e0 / 20 * i
	end do

	write(*,*)"Input an initial value (0 < x < 1):"
	read(*,*)x

	do i = 1, 500
		x = 4e0 * x * (1e0 - x)

		do j = 1, 20
			if ((point(j - 1) <= x) .and. (x < point(j))) then
				interval(j) = interval(j) + 1
			else if (x == 1e0) then
				interval(20) = interval(20) + 1
			end if
		end do
	end do

	do i = 1, 20
		num = interval(i) / 10
		write(*, '(1X, F4.2, 1X, "|", 50A1)')point(i), ('*', j = 1, num)
	end do

end program practice
