! Page86 6-10
! 6text10.f90  
! The latest version (2025/1/5)

program practice

	implicit none
	integer :: i, k, sum
	integer :: n, sum_p, sum_q
	integer :: sigma(1:100)
	integer :: p(0:50), q(0:50)

	do k = 1, 100
		sum = 0
		do i = 1, k
			if (mod(k, i)/=0) cycle
			sum = sum + i
		end do
		sigma(k) = sum
	end do

	n = 0
	p(0) = 1
	q(0) = 1
	write(*, '(1X, A3, I2, A5, I2, A6, I8, A1, I8, A1)')"(p(", n, "), q(", n, ")) = (", p(n), ",", q(n), ")"    
	do n = 1, 50
		sum_p = 0
		sum_q = 0
		do k = 1, n
			sum_p = sum_p + sigma(k) * p(n - k)
			sum_q = sum_q + (sigma(2 * k) - 2 * sigma(k)) * q(n - k)
		end do
		p(n) = sum_p / n
		q(n) = sum_q / n
		write(*,'(1X, A3, I2, A5, I2, A6, I8, A1, I8, A1)')"(p(", n, "), q(", n, ")) = (", p(n), ",", q(n), ")"    
	end do

end program practice
