! Page57 5-6
! 5text6.f90
! The latest version (2025/1/4)

program practice

	implicit none
	integer :: x, y, z, k, m, n
	real :: z0, v

	! x should be smaller than 71 
	do x = 1, 70
		do y = (x + 1), 99
			z0 = sqrt(real(x) ** 2.0E0 + real(y) **  2.0E0)
			if (z0 > 100) cycle
   
			v = z0 - int(z0)
			if (v /= 0) cycle

			! Euclidean algorithm
			m = x
			n = y
			do
				k = mod(m, n)
				if (k == 0) exit
				m = n
				n = k
			end do

			! Display the Pythagoras number 
			if (n /= 1) cycle
			z = int(z0)
			write(*,*)x, y, z
		end do
	end do

end program practice
