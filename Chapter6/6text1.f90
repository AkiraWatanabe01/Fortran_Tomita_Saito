! Page 85 6-1
! 6text1.f90
! The latest version (2025/1/5)

program practice

	implicit none
	integer :: i, j, k
	integer :: timestep
	integer, dimension(-100:100) :: a_scale
	real, dimension(-100:100) :: a, a_minus, a_plus
	real :: dt = 1.0e0 / 16.0e0

	! The initial condition
	a(:) = (/(0.0e0, i = -100, 100)/)
	a(0) = 10000.0e0

	write(*,*)"How many steps do you have ?"
	read(*,*)timestep

	do j = 1, timestep
		a_minus(:) = cshift(a(:), -1)
		a_plus(:) = cshift(a(:), 1)

		! Boundary condition
  		a_minus(-100) = 0
		a_minus(100) = 0
		a_plus(-100) = 0
		a_plus(100) = 0

		! Update
		a(:) = (1.0e0 - 2.0e0 * dt) * a(:) + dt * (a_minus(:) + a_plus(:))

		! Drawing histograms to see the process (This is an original way.)   
		do k = -100, 100
			a_scale(k) = int(a(k)) / 100
			write(*, '(1X, I4, "|", 100A1)')k, ('*', i = 1, a_scale(k))
		end do
	end do

end program practice
