! Page87 6-13
! 6text13.f90 
! The latest version (2025/1/12)

program practice

	implicit none
	character(len = 1) :: symbol(0:1) = (/' ', '#'/)
	integer :: i, j, k
	integer, parameter :: n = 32
	integer :: top, bottom, left, right
	integer :: determinant
	integer, allocatable :: a(:, :)
	integer, allocatable :: temp(:, :)

	allocate(a(-n:n, -n:n))
	allocate(temp(-n:n, -n:n))

	! Initial value
	a(:, :) = 0
	a(0, 0) = 1

	! Calculation
	do k = 1, (n - 2)
		temp(:, :) = a(:, :)
		do j = -n, n
			left = j - 1
			right = j + 1
			if (j == -n) then
				left = n
			end if 
			if (j == n) then
				right = -n
			end if 
			do i = -n, n
				top = i - 1
				bottom = i + 1
				if (i == -n) then
					top = n
				end if 
				if (i == n) then
					bottom = -n
				end if 
				determinant = a(top, left) + a(i, left) + a(bottom, left) &
					+ a(top, j) + a(bottom, j) &
					+ a(top, right) + a(i, right) + a(bottom, right)
				if (determinant == 1) then
					temp(i, j) = 1
				end if
			end do
		end do
		a(:, :) = temp(:, :)
	end do

	do i = -n, n
		write(*,*)(symbol(a(i, j)), j = -n, n)
	end do

end program practice
