! Page 105 7-2
! 7text2.f90
! The latest version (2024/10/13)

program practice
implicit none

	character(len = 39)::axis_maximum, axis_minimum, axis_zero, axis, line
	integer::i, j
	integer::index 
	real::radian                                                                   ! -1 <= value <= 1
	real::value_sine, value_cosine
	real::y_value(-25:25)
	real, parameter::degree_to_radian = 3.1415927e0 / 180e0

	! axis
	axis_maximum = "+1-------------------------------------"
	axis_minimum = "-1-------------------------------------"
	axis_zero    = " 0-------------------------------------"
	axis         = " |                  |                  "

	do i = 25, -25, -1
		y_value(i) = i * 4e-2
	end do 

	do i = 25, -25, -1
		! axis
		select case (i)
		case (25)
			line(:) = axis_maximum(:)
		case (0)
			line(:) = axis_zero(:)
		case (-25)
			line(:) = axis_minimum(:)
		case default
			line(:) = axis(:)
			if (mod(i , 5) == 0) then
				line(2:2) = '-'
			end if
		end select

		do j = 3, 39
			radian = ((j - 3) * 10 - 180) * degree_to_radian
			value_sine = sin(radian)
			value_cosine = cos(radian)

			value_sine = anint(value_sine * 1e5) * 1e-5
			value_cosine = anint(value_cosine * 1e5) * 1e-5

			if (i > -25) then
				if ((y_value(i - 1) < value_sine).and.(value_sine <= y_value(i))) then
					line(j:j) = 's'
				end if

				if ((y_value(i - 1) < value_cosine).and.(value_cosine <= y_value(i))) then
					line(j:j) = 'c'
				end if
			end if

			if (i == -25) then
				if (value_sine <= y_value(i)) then
					line(j:j) = 's'
				end if

				if (value_cosine <= y_value(i)) then
					line(j:j) = 'c'
				end if
			end if
		end do

		! Display
		write(*,'(A39)')line(:)
	end do

end program practice
