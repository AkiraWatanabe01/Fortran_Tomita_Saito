! Page56 5-5
! 5text5.f90
! The latest version (2024/9/28)

program practice
implicit none
 
	integer::k, m, n
	real::S, x, h

	write(*,*)"The ratio of the circumference of a circle and its diameter"

	do k = 1, 12
		n = 2 ** k
		h = 1.0e0 / n

		! The initial value of S
        	! S = h*0.5e0*f(x_0)+h*0.5e0*f(x_n) 
                !   = h*0.5e0*(f(0)+f(1))
                !   = h*3.0e0   
		S=h*3.0e0

		do m = 1, (n - 1)
			x = h * m
			S = S + 4.0e0 / (1.0e0 + x ** 2.0e0)
		end do
  		S = S * h
		write(*,'(1x, 3I, 1x, f8.6)')k, S
	end do

end program practice
