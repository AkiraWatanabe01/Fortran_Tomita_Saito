! Page 105 7-2
! 7text2.f90
! The latest version (2024/10/13)

program practice
implicit none

	character(len = 39)::axis_maximum, axis_minimum, axis_zero, axis, line
	integer::i, j
	integer::row_sine, row_cosine 
	real::radian 
	real::value_sine, value_cosine
	real, parameter::degree_to_radian = 3.1415927e0 / 180e0

	! axis
	axis_maximum = "+1-------------------------------------"
	axis_minimum = "-1-------------------------------------"
	axis_zero    = " 0-------------------------------------"
	axis         = " |                  |                  "

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

			row_sine = nint(value_sine * 25.0e0)
			if (i == row_sine) then
				line(j:j) = 's'
			end if

			row_cosine = nint(value_cosine * 25.0e0)
			if (i == row_cosine) then
				line(j:j) = 'c'
			end if
		end do

		! Display
		write(*,'(A39)')line(:)
	end do

end program practice
