! Page58 5-9
! 5text9.f90
! Approximation 
! The latest version (2024/10/2)

program practice
implicit none
 
	integer::i, j, h
	real::x, f
 	real::x_init, x_left, x_right
	real,parameter::dx = 0.1e0

	! Drawing a graph
	do i = -30, 20
		x = i * dx
		f = x ** 3 + x ** 2 - 5.0e0 * x -1.0e0

		h = int(f) + 6
		write(*,'(1x, f5.1, 1x, "|", 50a1, a1)')x, (" ", j = 1, (h - 1)), '*'
	end do

	! The Main part
	write(*,*)'Input one initial value:'
	read(*,*)x_init
 	x = x_init
	f = x ** 3 + x ** 2 - 5.0e0 * x - 1.0e0

	if ((f < 0).and.(x_init < -1.0e0)) then
 		x_left = x_init
   		x_right = -1.0e0
		do while (abs(f) > 1.0e-4)
  			x = (x_left + x_right) * 0.5e0
     			f = x ** 3 + x ** 2 - 5.0e0 * x - 1.0e0
         		if (f < 0.0e0) then
            			x_left = x
         		else
            			x_right = x
         		end if
      		end do
	end if

	if ((f > 0).and.(x_init < 1.0e0)) then
 		x_left = x_init
   		x_right = 1.0e0
		do while (abs(f) > 1.0e-4)
  			x = (x_left + x_right) * 0.5e0
     			f = x ** 3 + x ** 2 - 5.0e0 * x - 1.0e0
         		if (f < 0.0e0) then
            			x_right = x
         		else
            			x_left = x
         		end if
      		end do
	end if

	if ((f < 0).and.(x_init < 2.0e0)) then
 		x_left = x_init
   		x_right = 2.0e0
		do while (abs(f) > 1.0e-4)
  			x = (x_left + x_right) * 0.5e0
     			f = x ** 3 + x ** 2 - 5.0e0 * x - 1.0e0
         		if (f < 0.0e0) then
            			x_left = x
         		else
            			x_right = x
         		end if
      		end do
	end if

	if ((f > 0).and.(1.0e0 < x_init)) then
 		x_left = 1.0e0
   		x_right = x_init
		do while (abs(f) > 1.0e-4)
  			x = (x_left + x_right) * 0.5e0
     			f = x ** 3 + x ** 2 - 5.0e0 * x - 1.0e0
         		if (f < 0.0e0) then
            			x_left = x
         		else
            			x_right = x
         		end if
      		end do
	end if

	write(*,*)"The solution is x = ", x 

end program practice
