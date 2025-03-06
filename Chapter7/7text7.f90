! Page 105 7-7
! 7text7.f90 
! The latest version (2025/3/6)

program practice

	implicit none
	integer :: i
	integer :: month, day_of_week
	integer :: end_of_month, last_day
	character(len = 1) :: leap_year = ' '
	character(len = 3) :: week(1:7) = (/"SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"/)
	character(len = 3) :: day(1:37) = "   "
	character(len = 3) :: num_day(1:31) = (/"  1", "  2", "  3", "  4", "  5", "  6", "  7", &
					      "  8", "  9", " 10", " 11", " 12", " 13", " 14", &
					      " 15", " 16", " 17", " 18", " 19", " 20", " 21", &
					      " 22", " 23", " 24", " 25", " 26", " 27", " 28", &
					      " 29", " 30", " 31"/)

	! Month
	write(*,*)"Input a month (1 - 12)"
	read(*,*)month
	if ((month < 1) .or. (12 < month)) stop "Invalid month."

	! Day of week
	write(*,*)"Input the day of the week of the 1st day of the month."
	write(*,*)"(Sun:1, Mon:2, Tue:3, Wed:4, Thu:5, Fri:6, Sat:7)"
	read(*,*)day_of_week
	if ((day_of_week < 1) .or. (7 < day_of_week)) stop "Invalid number."

	! Leap year
	do while ((leap_year /= 'y') .and. (leap_year /= 'n'))
		write(*,*)"Leap year? (y/n)"
		read(*,*)leap_year 
	end do
	
	! Last day of the month
	select case (month)
	case (1)
		last_day = 31
	case (2)
		last_day = 28
		if (leap_year == 'y') then
			last_day = 29
		end if
	case (3)
		last_day = 31
	case (4)
		last_day = 30
	case (5)
		last_day = 31
	case (6)
		last_day = 30
	case (7)
		last_day = 31
	case (8)
		last_day = 31
	case (9)
		last_day = 30
	case (10)
		last_day = 31
	case (11)
		last_day = 30
	case (12)
		last_day = 31
	end select

	! Store values in the array
	end_of_month = day_of_week + last_day
	day(day_of_week:end_of_month) = num_day(1:last_day)
	
	! Output
	write(*, '(7(A3, 1X))')(week(i), i = 1, 7)
	write(*, '(7(A3, 1X))')(day(i), i = 1, 7)
	write(*, '(7(A3, 1X))')(day(i), i = 8, 14)
	write(*, '(7(A3, 1X))')(day(i), i = 15, 21)
	write(*, '(7(A3, 1X))')(day(i), i = 22, 28)
	write(*, '(7(A3, 1X))')(day(i), i = 29, 35)
	if (day(i) /= "   ") then
		write(*, '(2(A3, 1X))')(day(i), i = 36, 37)
	end if

end program practice
